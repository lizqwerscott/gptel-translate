;;; gptel-translate.el --- Translate with gptel      -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (templatel "0.1.6") (gptel "0.9.9.5"))
;; Keywords: tools
;; URL: https://github.com/lizqwerscott/gptel-translate

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use `gptel-translate-buffer' translate buffer/region content using gptel.

;;; Code:

(require 'gptel-request)
(require 'templatel)

(require 'gptel-translate-org)

;;; Customization

(defgroup gptel-translate nil
  "Options for buffer translation with gptel."
  :group 'gptel
  :prefix "gptel-translate-")

(defcustom gptel-translate-backend nil
  "Backend name for gptel translate requests.
If nil, uses `gptel-backend'."
  :type '(choice (string :tag "Backend name")
                 (const :tag "Use default" nil))
  :group 'gptel-translate)

(defcustom gptel-translate-model nil
  "Model symbol for gptel translate requests.
If nil, uses `gptel-model'."
  :type '(choice (symbol :tag "Model symbol")
                 (const :tag "Use default" nil))
  :group 'gptel-translate)

(defcustom gptel-translate-context-window nil
  "Maximum context window size for translation, in units of 1k tokens.

- If nil, automatically detect from the gptel model's
  `:context-window' property.
- If a string like \"8k\", \"256k\", \"1M\", parse it into the
  equivalent in 1k units.
- If a number, use it directly, unit is k."
  :type '(choice (const :tag "Auto-detect from model" nil)
                 (string :tag "Human-readable count (e.g. \"8k\", \"256k\", \"1M\")")
                 (integer :tag "Exact count in 1k units"))
  :group 'gptel-translate)

(defcustom gptel-translate-streamp t
  "Non-nil means stream the LLM output during translation.
If nil, wait for the entire response before displaying it."
  :type 'boolean
  :group 'gptel-translate)

(defcustom gptel-translate-target-language "Chinese"
  "Target language for translation.
This is used in the prompt sent to the LLM."
  :type 'string
  :group 'gptel-translate)

(defcustom gptel-translate-system-prompt (expand-file-name
                                          "./prompts/translate.md"
                                          (file-name-directory
                                           (or load-file-name (buffer-file-name))))
  "System prompt template used by LLM."
  :group 'gptel-translate
  :type 'string)

(defcustom gptel-translate-user-prompt "Please translate the following text into {{to}}. The text contains formatting marker lines that start with `[--PARA_` and end with `--]`. These markers are structural separators.
Your tasks:
- You MUST preserve EVERY marker line exactly as it appears. Do not modify, translate, or remove them.
- For each marker line, translate ONLY the text that follows it (until the next marker or the end of the input) into {{to}}.
- If the text following a marker is code (e.g., a code block), keep it exactly as is, without translation. The marker line before the code block MUST still be present in your output.
- Keep the original paragraph separations (blank lines) as they are.
- Your response must contain exactly the same number of marker lines as the input, in the same order.
- Do not output any extra explanations, notes, or content.\n
{{input}}\n"
  "User prompt template used by LLM."
  :group 'gptel-translate
  :type 'string)

(defcustom gptel-translate-followp t
  "Non-nil means follow the output during streaming translation.
If nil, stay at the current position in the buffer."
  :type 'boolean
  :group 'gptel-translate)

;;; Faces
(defface gptel-translate-header-desc-face '((t :inherit font-lock-variable-name-face))
  "Used in the buffer's `header-line-format' for description."
  :group 'gptel-translate)

(defface gptel-translate-original-face
  '((((background light)) :background "#fdf6e3" :foreground "#586e75" :slant italic)
    (((background dark)) :background "#1e2329" :foreground "#839496" :slant italic)
    (t :inherit shadow))
  "Face for the source text in the translation buffer.
In light themes it's a warm paper-like background; in dark themes
a subdued dark tone. Italic helps separate it from the translation."
  :group 'gptel-translate)

(defface gptel-translate-translation-face
  '((((background light)) :background "#f0f4f8" :foreground "#1a1a1a" :extend t)
    (((background dark)) :background "#1c2833" :foreground "#e0e0e0" :extend t)
    (t :inherit default))
  "Face for the translated text.
A clean, slightly cool light-gray background in light themes,
and a deep blue-gray in dark themes. High readability."
  :group 'gptel-translate)

(defface gptel-translate-status-idle-face
  '((t :inherit shadow))
  "Face for `idle' status indicator in header-line."
  :group 'gptel-translate)

(defface gptel-translate-status-waiting-face
  '((t :inherit font-lock-builtin-face))
  "Face for `waiting' status indicator in header-line."
  :group 'gptel-translate)

(defface gptel-translate-status-translating-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for `translating' status indicator in header-line."
  :group 'gptel-translate)

(defface gptel-translate-status-complete-face
  '((t :inherit success))
  "Face for `complete' status indicator in header-line."
  :group 'gptel-translate)

(defface gptel-translate-status-aborted-face
  '((t :inherit warning))
  "Face for `aborted' status indicator in header-line."
  :group 'gptel-translate)

(defface gptel-translate-status-error-face
  '((t :inherit error))
  "Face for `error' status indicator in header-line."
  :group 'gptel-translate)

;;; VAR

(defvar-local gptel-translate-orig-buffer-name ""
  "Original buffer name.")

(defvar-local gptel-translate-orig-buffer nil
  "Original buffer.")

(defvar-local gptel-translate-progress 0
  "Translate progress.")

(defvar-local gptel-translate-failed 0
  "Translate failed.")

(defvar-local gptel-translate-paragraph-number 0
  "Translate total paragraph number.")

(defvar-local gptel-translate--current-pos nil
  "Marker tracking the insertion point for the next original paragraph.
In streaming mode, after each original paragraph is inserted, this
marker is updated to point to the end of that paragraph, allowing
follow-up insertions to be placed correctly.")

(defvar-local gptel-translate--stream-state nil
  "Stream parser state for translation.
A plist with keys:
- :buffer  - accumulated response text
- :pos     - current scan position in :buffer
This variable is buffer-local to the result buffer.")

(defvar-local gptel-translate--status 'idle
  "Current translation status.
One of `idle', `waiting', `translating', `complete', `aborted', `error'.")

(defvar-local gptel-translate--backend-name ""
  "Backend name used for translation, stored for header-line display.")

(defvar-local gptel-translate--model-name ""
  "Model name used for translation, stored for header-line display.")

(defvar-local gptel-translate--scope-beg nil
  "Beginning position of the translation scope in the original buffer.
If nil, the scope is the entire buffer.  Set by `gptel-translate-buffer',
`gptel-translate-at-point', etc.  Used by `gptel-translate-retranslate'
to re-collect only the originally-scoped content.")

(defvar-local gptel-translate--scope-end nil
  "End position of the translation scope in the original buffer.
If nil, the scope is the entire buffer.  See `gptel-translate--scope-beg'.")

;;; Internal helpers

(defconst gptel-translate--para-marker-re
  (rx (opt "\n") "[--PARA_" (group (+ digit)) "--]" (opt "\n"))
  "Regexp matching [--PARA_N--] markers.

Group 1 is the paragraph number.")

(defun gptel-translate--stream-init ()
  "Initialize stream parser state for N paragraphs."
  (setq gptel-translate--stream-state
        (list :buffer "" :pos 0 :index -1)))

(defun gptel-translate--stream-chunk (chunk orig-buffer orig-paras result-buf)
  "Process one streaming CHUNK of translation response.

ORIG-BUFFER is source buffer.
ORIG-PARAS is source paras.
RESULT-BUF is result buffer.

Accumulates CHUNK into an internal buffer and scans for markers of the form
`[--PARA_N--]'. When a marker is found: 1. The text preceding the marker is
inserted as the translation of paragraph N (the one that was started by the
previous marker). 2. The index advances to paragraph N+1. 3. The original text
of paragraph N+1 is inserted into the result buffer.

This ensures original paragraphs and their translations appear interleaved in
the result buffer as the stream arrives."
  (with-current-buffer result-buf
    (let ((state gptel-translate--stream-state)
          (inhibit-read-only t))
      (setf (plist-get state :buffer)
            (concat (plist-get state :buffer) chunk))
      (let ((buf-str (plist-get state :buffer))
            (pos (plist-get state :pos))
            (current-idx (plist-get state :index))
            (marker-re gptel-translate--para-marker-re))
        (while (string-match marker-re buf-str pos)
          (let* ((end-of-marker (match-end 0))
                 (before-marker (substring buf-str pos (match-beginning 0))))
            ;; First, flush the previous paragraph's text (before this marker)
            (when (and (>= current-idx 0) (< current-idx (length orig-paras))
                       (not (string-blank-p before-marker)))
              (let* ((orig-para (nth current-idx orig-paras))
                     (orig-text (car orig-para))
                     (headingp (get-text-property 0 :headingp orig-text))
                     (level (get-text-property 0 :subtree-level orig-text)))
                (gptel-translate--insert-translate
                 result-buf (cons orig-buffer (cdr orig-para))
                 (string-trim before-marker) headingp level)))
            ;; If this paragraph's orig text hasn't been inserted yet, do it now
            (incf current-idx)
            (when (< current-idx (length orig-paras))
              (let* ((orig-para (nth current-idx orig-paras))
                     (orig (cons orig-buffer (cdr orig-para)))
                     (orig-text (car orig-para)))
                (gptel-translate--insert-orig result-buf orig orig-text)))
            (setq pos end-of-marker)))
        (setf (plist-get state :pos) pos)
        (setf (plist-get state :index) current-idx)
        (setq gptel-translate-progress current-idx)))))

(defun gptel-translate--stream-flush (orig-buffer orig-paras result-buf)
  "Finalize the streaming state: insert the last pending translation.

ORIG-BUFFER is source buffer.
ORIG-PARAS is source paras.
RESULT-BUF is result buffer.

When the stream completes, any text remaining in the buffer after
the last marker belongs to the last detected paragraph."
  (with-current-buffer result-buf
    (let* ((state gptel-translate--stream-state)
           (buf-str (plist-get state :buffer))
           (pos (plist-get state :pos))
           (current-idx (plist-get state :index)))
      ;; Insert any remaining text after the last marker
      (when (and (< pos (length buf-str))
                 (> (length (substring buf-str pos)) 0))
        (let ((remaining (substring buf-str pos)))
          (when (not (string-blank-p remaining))
            (let* ((orig-para (nth current-idx orig-paras))
                   (orig-text (car orig-para))
                   (headingp (get-text-property 0 :headingp orig-text))
                   (level (get-text-property 0 :subtree-level orig-text)))
              (gptel-translate--insert-translate
               result-buf (cons orig-buffer (cdr orig-para))
               (string-trim remaining) headingp level))))))))

(defun gptel-translate--format-status ()
  "Return propertized status string for header-line."
  (pcase gptel-translate--status
    ('idle        (propertize " ● Idle"        'face 'gptel-translate-status-idle-face))
    ('waiting     (propertize " ● Waiting"     'face 'gptel-translate-status-waiting-face))
    ('translating (propertize " ▶ Translating" 'face 'gptel-translate-status-translating-face))
    ('complete    (propertize " ✔ Complete"    'face 'gptel-translate-status-complete-face))
    ('aborted     (propertize " ⊘ Aborted"     'face 'gptel-translate-status-aborted-face))
    ('error       (propertize " ✗ Error"       'face 'gptel-translate-status-error-face))))

(defun gptel-translate--format-model-backend ()
  "Return propertized backend:model string for header-line right side."
  (when (and (not (string-empty-p gptel-translate--backend-name))
             (not (string-empty-p gptel-translate--model-name)))
    (propertize (format "%s:%s"
                        gptel-translate--backend-name
                        gptel-translate--model-name)
                'face 'shadow)))

(defun gptel-translate--resolve-system-prompt (replaces)
  "Return the rendered system prompt.
If `gptel-translate-system-prompt' is an existing file path, render it with
`templatel-render-file'. Otherwise treat it as a template literal and render
with `templatel-render-string'.
REPLACES is an alist passed to the templatel renderer."
  (let ((prompt gptel-translate-system-prompt))
    (unless (stringp prompt)
      (error "gptel-translate-system-prompt must be a string, got: %S" prompt))
    (if-let* ((file (expand-file-name prompt))
              ((file-exists-p file)))
        (templatel-render-file file replaces)
      (templatel-render-string prompt replaces))))

(defun gptel-translate--resolve-backend ()
  "Return the backend to use for translation requests."
  (or (and gptel-translate-backend
           (gptel-get-backend gptel-translate-backend))
      gptel-backend))

(defun gptel-translate--resolve-model ()
  "Return the model symbol to use for translation requests."
  (or gptel-translate-model gptel-model))

(defun gptel-translate--parse-context-window (str)
  "Parse STR like \"8k\", \"256k\", \"1M\" into an integer (in units of 1k).
Returns nil if STR cannot be parsed."
  (when (stringp str)
    (cond
     ((string-match (rx string-start (+ digit) (or "k" "K") string-end) str)
      (string-to-number (substring str 0 -1)))
     ((string-match (rx string-start (+ digit) (or "m" "M") string-end) str)
      (* 1000 (string-to-number (substring str 0 -1))))
     ((string-match (rx string-start (+ digit) string-end) str)
      (string-to-number str))
     (t nil))))

(defun gptel-translate--model-context-window ()
  "Return the context-window (in units of 1k) of the current gptel model.
Looks at the model plist property `:context-window' from the selected
backend and model."
  (when-let* ((_ (gptel-translate--resolve-backend))
              (model (gptel-translate--resolve-model)))
    (get model :context-window)))

(defun gptel-translate--resolve-context-window ()
  "Return the effective context window value (in 1k units)."
  (cond
   ((null gptel-translate-context-window)
    (or (gptel-translate--model-context-window)
        8))
   ((stringp gptel-translate-context-window)
    (or (gptel-translate--parse-context-window gptel-translate-context-window)
        (user-error "Cannot parse context window string: %s" gptel-translate-context-window)))
   ((numberp gptel-translate-context-window) gptel-translate-context-window)
   (t (user-error "Invalid context window value: %s" gptel-translate-context-window))))

(defun gptel-translate--resolve-max-tokens ()
  "Return the maximum number of tokens allowed for the input."
  (* 1000 (gptel-translate--resolve-context-window) 0.6 0.5))

(defun gptel-translate--collect-paragraphs (&optional beg end)
  "Collect paragraphs from buffer (or region BEG to END).
Returns a list of (STRING . POSITION) cons cells in order,
where POSITION is the buffer position of the paragraph start."
  (let (paragraphs)
    (save-excursion
      (save-restriction
        (when (and beg end) (narrow-to-region beg end))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((start (point)))
            (forward-paragraph)
            (when (> (point) start)
              (push (cons (buffer-substring-no-properties start (point))
                          start)
                    paragraphs))
            (skip-chars-forward "\n\t ")))))
    (nreverse paragraphs)))

(defun gptel-translate--merge-paragraphs (paragraphs)
  "Merge PARAGRAPHS into batches that fit within the context window.

PARAGRAPHS is a list of (STRING . POSITION) cons cells as returned by
`gptel-translate--collect-paragraphs'.

Returns a list of (BATCH . ORIG-PARAS) cons cells. BATCH is a single string
containing one or more paragraphs joined with PARA markers , and ORIG-PARAS is
the list of original (STRING . POSITION) cons cells for that batch.

The merging is performed greedily: paragraphs are added to the current
batch until doing so would exceed the available token budget, which is
60% of the resolved context window (measured in characters divided by 3
as a rough token estimate)."
  (let ((result nil)
        (current-para nil)
        (orig-para nil)
        (current-length 0)
        (max-token (gptel-translate--resolve-max-tokens)))
    (dolist (para paragraphs)
      (let ((para-data (car para)))
        (if (or (not current-para) (<= (/ (+ current-length (length para-data)) 3) max-token))
            (progn
              (push para-data current-para)
              (push para orig-para)
              (incf current-length (length para-data)))
          (let ((idx -1))
            (push (cons (string-join (mapcar (lambda (item)
                                               (format "[--PARA_%s--]\n%s" (incf idx) item))
                                             (reverse current-para))
                                     "")
                        (reverse orig-para))
                  result))
          (setq current-para nil
                current-length 0
                orig-para nil))))
    (when current-para
      (let ((idx -1))
        (push (cons (string-join (mapcar (lambda (item)
                                           (format "[--PARA_%s--]\n%s" (incf idx) item))
                                         (reverse current-para))
                                 "")
                    (reverse orig-para))
              result)))
    (reverse result)))

(defun gptel-translate--parse-paragraphs (response)
  "Parse RESPONSE string by splitting on PARA markers.

RESPONSE is the raw text returned from the LLM, which may contain
markers like \n[--PARA_0--]\n to delimit individual paragraph
translations.

Returns a list of strings, each being the translated text for one
paragraph.  Leading/trailing empty strings resulting from markers at
the very beginning or end are dropped."
  (let ((pos 0)
        (regex gptel-translate--para-marker-re)
        (translations '())
        (results))
    (while (string-match regex response pos)
      (let ((content-start (match-end 0))
            (next-pos (match-beginning 0)))
        (when (> content-start pos)
          (push (substring response pos next-pos) translations))
        (setq pos content-start)))
    (when (< pos (length response))
      (push (substring response pos) translations))
    (setq results (nreverse translations))
    (if (string-empty-p (car results))
        (cdr results)
      results)))

(defun gptel-translate--make-result-buffer (orig-name orig-buffer paragraphs &optional suffix)
  "Create and return a new buffer for translation results.
ORIG-NAME is the source buffer name.
ORIG-BUFFER is the source buffer object.
PARAGRAPHS is a list of (STRING . POSITION) cons cells.
SUFFIX, if non-nil, is appended to the result buffer name to
  distinguish scoped translations (e.g. \"*translate foo - heading*\").

If a result buffer with the same name already exists, it is killed first.

Each result entry has a text property `gptel-translate-orig-pos' whose
value is the buffer position in ORIG-BUFFER where the original paragraph
starts.  Returns the buffer."
  (let* ((buf-name (if suffix
                       (format "*translate %s - %s*" orig-name suffix)
                     (format "*translate %s*" orig-name)))
         (buf (get-buffer buf-name)))
    (when buf (kill-buffer buf))
    (setq buf (generate-new-buffer buf-name))
    (with-current-buffer buf
      (goto-char (point-min))
      (gptel-translate-result-mode)
      (setq gptel-translate-orig-buffer-name orig-name)
      (setq gptel-translate-orig-buffer orig-buffer)
      (setq gptel-translate-paragraph-number (length paragraphs))
      (setq gptel-translate-progress 0)
      (setq gptel-translate--status 'idle)
      (setq gptel-translate--current-pos (make-marker))
      (set-marker gptel-translate--current-pos (point))
      (gptel-translate--stream-init))
    buf))

(defun gptel-translate--insert-orig (result-buf orig orig-para)
  "Insert ORIG-PARA into RESULT-BUF as the original text paragraph.

RESULT-BUF is the translation result buffer.
ORIG is a cons cell (BUFFER . POSITION) identifying the source
of the original text in the source buffer.
ORIG-PARA is the original paragraph string.

The text is inserted with `gptel-translate-original-face' (or
`org-level-N' if ORIG-PARA has a `:headingp' text property) and
carries the `gptel-translate-orig' text property set to ORIG.
Point is advanced past the insertion."
  (with-current-buffer result-buf
    (let ((inhibit-read-only t)
          (pos (marker-position gptel-translate--current-pos)))
      (save-excursion
        (goto-char pos)
        (insert
         (propertize orig-para
                     'face (if (get-text-property 0 :headingp orig-para)
                               (intern (format "org-level-%d"
                                               (or (get-text-property 0 :subtree-level orig-para) 1)))
                             'gptel-translate-original-face)
                     'gptel-translate-orig orig))
        (insert "\n")
        (set-marker gptel-translate--current-pos (point))))))

(defun gptel-translate--insert-translate (result-buf orig translate &optional headingp subtree-level)
  "Insert TRANSLATE into RESULT-BUF as the translated text paragraph.

RESULT-BUF is the translation result buffer.
ORIG is a cons cell (BUFFER . POSITION) identifying the source
of the corresponding original text in the source buffer.
TRANSLATE is the translated paragraph string.
HEADINGP and SUBTREE-LEVEL, when provided, indicate this translation
corresponds to an Org heading; the text uses an `org-level-N' face
instead of `gptel-translate-translation-face'.

The text is inserted with the appropriate face and
carries the `gptel-translate-orig' text property set to ORIG.
Point is advanced past the insertion, and a blank line is added
after the translation to separate it from the next pair."
  (with-current-buffer result-buf
    (let ((inhibit-read-only t)
          (pos (marker-position gptel-translate--current-pos)))
      (save-excursion
        (goto-char pos)
        (insert (propertize translate
                            'face (if headingp
                                      (intern (format "org-level-%d" (or subtree-level 1)))
                                    'gptel-translate-translation-face)
                            'gptel-translate-translation-block t
                            'gptel-translate-orig orig
                            :headingp headingp
                            :subtree-level subtree-level))
        (insert "\n\n")
        (set-marker gptel-translate--current-pos (point))))))

(defun gptel-translate--apply-parsed-response (response orig-buffer orig-paras result-buf)
  "Parse RESPONSE string from a merged translation request and insert translations.

ORIG-BUFFER is the source buffer.
ORIG-PARAS is a list of (STRING . POS) cons cells.
RESULT-BUF is the translation result buffer.

Returns the number of original paragraphs that were paired with
a translation."
  (let* ((parsed (gptel-translate--parse-paragraphs response))
         (parsed-count (length parsed))
         (orig-count (length orig-paras)))
    (cl-loop for i from 0 below orig-count
             for orig-para = (nth i orig-paras)
             for orig-text = (car orig-para)
             for orig-pos = (cdr orig-para)
             for orig = (cons orig-buffer orig-pos)
             for headingp = (get-text-property 0 :headingp orig-text)
             for level = (get-text-property 0 :subtree-level orig-text)
             for translate = (if (< i parsed-count)
                                 (nth i parsed)
                               "<MISSING>")
             do (gptel-translate--insert-orig result-buf orig orig-text)
             do (gptel-translate--insert-translate result-buf orig translate headingp level))
    orig-count))

(defun gptel-translate--send-requests (result-buf orig-buffer merge-batches total)
  "Send translation requests sequentially through the LLM.

RESULT-BUF is the result buffer.
ORIG-BUFFER is the source buffer.
MERGE-BATCHES is a list of (MERGED-TEXT . ORIG-PARAS) cons cells.
TOTAL is the total number of paragraphs being translated."
  (let ((done 0)
        (failures 0))
    (with-current-buffer result-buf
      (cl-labels ((send-merged (merge-idx)
                    (if (or (>= merge-idx (length merge-batches)))
                        (progn
                          (setq gptel-translate--status
                                (if (> failures 0) 'error 'complete))
                          (message "Translation complete: %d ok, %d failed, %d total"
                                   done failures total))
                      (setq gptel-translate--status 'waiting)
                      (let* ((merge-pair (nth merge-idx merge-batches))
                             (merged-text (car merge-pair))
                             (orig-paras (cdr merge-pair)))
                        (gptel-request (templatel-render-string gptel-translate-user-prompt
                                                                `(("to" . ,gptel-translate-target-language)
                                                                  ("input" . ,merged-text)))
                          :buffer result-buf
                          :system (gptel-translate--resolve-system-prompt
                                   `(("to" . ,gptel-translate-target-language)))
                          :stream gptel-translate-streamp
                          :callback
                          (lambda (response _info)
                            (with-current-buffer result-buf
                              (cond ((eq response 'abort)
                                     (setq gptel-translate--status 'aborted)
                                     (message "Translation abort: %d ok, %d failed, %d total"
                                              done failures total))
                                    ((and (stringp response)
                                          (not (string-empty-p response)))
                                     (when gptel-translate-streamp
                                       (setq gptel-translate--status 'translating))
                                     (if gptel-translate-streamp
                                         (gptel-translate--stream-chunk
                                          response orig-buffer orig-paras result-buf)
                                       (cl-incf done (gptel-translate--apply-parsed-response
                                                      response orig-buffer orig-paras result-buf))
                                       (setq-local gptel-translate-failed failures)
                                       (setq-local gptel-translate-progress done)
                                       (send-merged (1+ merge-idx))))
                                    ((and gptel-translate-streamp (eq response t))
                                     (gptel-translate--stream-flush
                                      orig-buffer orig-paras result-buf)
                                     (cl-incf done (length orig-paras))
                                     (setq-local gptel-translate-failed failures)
                                     (setq-local gptel-translate-progress done)
                                     (send-merged (1+ merge-idx)))
                                    ((consp response))
                                    (t (progn
                                         (cl-incf failures)
                                         (setq-local gptel-translate-failed failures)
                                         (send-merged (1+ merge-idx))))))
                            ))))))
        (send-merged 0)))))

(defun gptel-translate--setup-and-send (paragraphs total merge-batches &optional suffix scope-beg scope-end)
  "Set up translation environment and send PARAGRAPHS to the LLM.

TOTAL is the total number of original paragraphs.
MERGE-BATCHES is the list of (MERGED-TEXT . ORIG-PARAS) ready for
translation.  Optional SUFFIX is appended to the result buffer name.
Optional SCOPE-BEG and SCOPE-END delimit the translation scope."
  (let* ((gptel-backend (gptel-translate--resolve-backend))
         (gptel-model (gptel-translate--resolve-model))
         (gptel-tools nil)
         (gptel-use-tools nil)
         (orig-name (buffer-name))
         (orig-buffer (current-buffer))
         (result-buf (gptel-translate--make-result-buffer
                      orig-name orig-buffer paragraphs suffix)))
    (display-buffer result-buf)
    (with-current-buffer result-buf
      (setq gptel-translate--backend-name (gptel-backend-name gptel-backend))
      (setq gptel-translate--model-name (gptel--model-name gptel-model))
      (setq gptel-translate--scope-beg scope-beg
            gptel-translate--scope-end scope-end))
    (gptel-translate--send-requests
     result-buf orig-buffer merge-batches total)))

;;; Commands

;;;###autoload
(defun gptel-translate-buffer (&optional beg end)
  "Translate buffer (or region BEG to END) paragraph-by-paragraph.

Show original text and translation side-by-side in a new buffer.

In Org mode, collects content by subtree so headings and their body
paragraphs are kept together.  In all other modes, uses paragraph-based
collection."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((paragraphs (if (eq major-mode 'org-mode)
                         (gptel-translate--collect-org-items beg end)
                       (gptel-translate--collect-paragraphs beg end)))
         (total (length paragraphs)))
    (if (zerop total)
        (message "Nothing to translate")
      (let ((merge-batches (if (eq major-mode 'org-mode)
                               (gptel-translate--merge-org-items paragraphs
                                                                 (* (gptel-translate--resolve-max-tokens) 3))
                             (gptel-translate--merge-paragraphs paragraphs))))
        (gptel-translate--setup-and-send paragraphs total merge-batches)))))

(defun gptel-translate--current-paragraph-bounds ()
  "Return (BEG . END) of the current paragraph at point.
Uses `backward-paragraph' / `forward-paragraph' to detect boundaries.
Returns nil if the paragraph is empty."
  (save-excursion
    (let ((beg (progn (skip-chars-forward "\n\t ")
                      (backward-paragraph)
                      (point)))
          (end (progn (forward-paragraph)
                      (point))))
      (when (> end beg)
        (cons beg end)))))

;;;###autoload
(defun gptel-translate-at-point ()
"Translate content at point.

In Org mode, translates the current subtree (heading + all children).
In non-Org modes, translates the current paragraph only.

Result buffer name reflects the scope:
- Non-Org:  *translate <buffer> - paragraph*
- Org:      *translate <buffer> - <heading>*

Repeated calls on the same scope reuse the same result buffer (replacing its
content)."
(interactive)
(if (eq major-mode 'org-mode)
    ;; --- Org mode: translate current subtree ---
    (if-let* ((bounds (gptel-translate--org-current-subtree-bounds)))
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (heading (save-excursion
                          (goto-char beg)
                          (org-get-heading t t t t)))
               (suffix (if (and heading (not (string-blank-p heading)))
                           (truncate-string-to-width heading 40 nil nil t)
                         "subtree")))
          (let* ((paragraphs (gptel-translate--collect-org-items beg end))
                 (total (length paragraphs)))
            (if (zerop total)
                (message "Nothing to translate")
              (let ((merge-batches (gptel-translate--merge-org-items
                                    paragraphs
                                    (* (gptel-translate--resolve-max-tokens) 3))))
                (gptel-translate--setup-and-send paragraphs total merge-batches
                                                 suffix beg end)))))
      (user-error "Not in an org subtree"))
  ;; --- Non-org mode: translate current paragraph ---
  (if-let* ((bounds (gptel-translate--current-paragraph-bounds)))
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (suffix "paragraph"))
        (let* ((paragraphs (gptel-translate--collect-paragraphs beg end))
               (total (length paragraphs)))
          (if (zerop total)
              (message "Nothing to translate")
            (let ((merge-batches (gptel-translate--merge-paragraphs paragraphs)))
              (gptel-translate--setup-and-send paragraphs total merge-batches
                                               suffix beg end)))))
    (user-error "No paragraph at point"))))

;;;###autoload
(defun gptel-translate-retranslate ()
  "Re-translate all content in the current result buffer.

Clears all existing translations and re-sends all paragraphs
to the LLM.  The original text is re-collected from the source
buffer to pick up any changes.

This command is only available in `gptel-translate-result-mode'."
  (interactive)
  (let ((orig-buffer gptel-translate-orig-buffer))
    (unless (buffer-live-p orig-buffer)
      (user-error "Original buffer \"%s\" is no longer alive"
                  gptel-translate-orig-buffer-name))
    ;; Abort any in-progress translation
    (when (memq gptel-translate--status '(waiting translating))
      (gptel-translate-abort))
    ;; Re-collect paragraphs from source buffer, respecting original scope
    (let* ((scope-beg gptel-translate--scope-beg)
           (scope-end gptel-translate--scope-end)
           (paragraphs
            (with-current-buffer orig-buffer
              (if (eq major-mode 'org-mode)
                  (gptel-translate--collect-org-items scope-beg scope-end)
                (gptel-translate--collect-paragraphs scope-beg scope-end))))
           (total (length paragraphs)))
      (if (zerop total)
          (message "Nothing to translate")
        ;; Resolve backend/model with current settings
        (let* ((gptel-backend (gptel-translate--resolve-backend))
               (gptel-model (gptel-translate--resolve-model))
               (gptel-tools nil)
               (gptel-use-tools nil)
               (merge-batches
                (with-current-buffer orig-buffer
                  (if (eq major-mode 'org-mode)
                      (gptel-translate--merge-org-items
                       paragraphs
                       (* (gptel-translate--resolve-max-tokens) 3))
                    (gptel-translate--merge-paragraphs paragraphs)))))
          ;; Clear result buffer and reset state
          (let ((inhibit-read-only t))
            (erase-buffer)
            (setq gptel-translate-paragraph-number total)
            (setq gptel-translate-progress 0)
            (setq gptel-translate-failed 0)
            (setq gptel-translate--status 'idle)
            (setq gptel-translate--backend-name
                  (gptel-backend-name gptel-backend))
            (setq gptel-translate--model-name
                  (gptel--model-name gptel-model))
            (set-marker gptel-translate--current-pos (point-min))
            (gptel-translate--stream-init))
          ;; Launch request chain
          (gptel-translate--send-requests
           (current-buffer) orig-buffer merge-batches total))))))

;;; Mode

(defvar gptel-translate-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gptel-translate-jump-to-original)

    (define-key map (kbd "TAB") #'gptel-translate-next-paragraph)
    (define-key map (kbd "n") #'gptel-translate-next-paragraph)

    (define-key map (kbd "<backtab>") #'gptel-translate-previous-paragraph)
    (define-key map (kbd "p") #'gptel-translate-previous-paragraph)
    (define-key map (kbd "g") #'gptel-translate-retranslate)
    (define-key map (kbd "C-g") #'gptel-translate-abort)
    map)
  "Keymap for `gptel-translate-result-mode'.")

(defun gptel-translate--orig-at-point ()
  "Return the original buffer position at point, or nil."
  (get-text-property (or (if (eobp) (1- (point)) (point))
                         (point))
                     'gptel-translate-orig))

(defun gptel-translate--goto-orig-pos ()
  "Jump to the original source position from the result buffer.
Return non-nil on success."
  (pcase-let* ((`(,buf . ,pos) (gptel-translate--orig-at-point)))
    (when (and pos (buffer-live-p buf))
      (switch-to-buffer-other-window buf)
      (goto-char pos)
      (recenter)
      (when (equal major-mode 'org-mode)
        (org-reveal))
      t)))

(defun gptel-translate-jump-to-original ()
  "Jump to the original source paragraph in its buffer.
Requires the source buffer to still be alive."
  (interactive)
  (unless (gptel-translate--goto-orig-pos)
    (message "No original source location for this paragraph")))

(defun gptel-translate-source-buffer-jump ()
  "Jump to the original source position in its window."
  (interactive)
  (pcase-let* ((`(,buf . ,pos) (gptel-translate--orig-at-point)))
    (when (and pos (buffer-live-p buf))
      (if-let* ((win (get-buffer-window buf)))
          (with-selected-window win
            (goto-char pos)
            (recenter)
            (when (equal major-mode 'org-mode)
              (org-reveal)))))))

(defun gptel-translate--find-paragraph-boundaries (&optional backward)
  "Move to the next/previous original paragraph boundary.
If BACKWARD is non-nil, search backward.  Return non-nil if moved.
Puts point at the start of the original text."
  (let ((fn (if backward #'previous-single-property-change
              #'next-single-property-change)))
    (let ((pos (funcall fn (point) 'face)))
      (while (and pos (not (get-text-property pos 'gptel-translate-translation-block)))
        (setq pos (funcall fn pos 'face)))
      (when pos
        (goto-char pos)
        (recenter)
        (when gptel-translate-followp
          (gptel-translate-source-buffer-jump))))))

(defun gptel-translate-next-paragraph ()
  "Move to the next original paragraph in the result buffer."
  (interactive)
  (unless (gptel-translate--find-paragraph-boundaries)
    (message "No next paragraph")))

(defun gptel-translate-previous-paragraph ()
  "Move to the previous original paragraph in the result buffer."
  (interactive)
  (unless (gptel-translate--find-paragraph-boundaries 'backward)
    (message "No previous paragraph")))

(defun gptel-translate-abort ()
  "Stop translate."
  (interactive)
  (message "Translate abort start.")
  (call-interactively #'gptel-abort))

;;;###autoload
(define-derived-mode gptel-translate-result-mode special-mode "GPTel-Translate"
  "Major mode for viewing translation results.

Provides syntax highlighting for original text and translated text,
and a read-only view of the side-by-side translation buffer.  Press RET
on an original paragraph to jump to its location in the source buffer.
TAB and S-TAB move between original paragraphs.

\\{gptel-translate-result-mode-map}"
  :group 'gptel-translate
  (setq header-line-format
        `(" "
          (:eval (gptel-translate--format-status))
          " "
          ,(propertize "Translation of " 'face 'gptel-translate-header-desc-face)
          (:eval (propertize gptel-translate-orig-buffer-name 'face 'font-lock-keyword-face))
          " "
          ,(propertize "success: [" 'face 'gptel-translate-header-desc-face)
          (:eval (propertize (format "%d/%d" gptel-translate-progress gptel-translate-paragraph-number)
                             'face 'font-lock-keyword-face))
          ,(propertize "]" 'face 'gptel-translate-header-desc-face)
          (:eval
           (unless (= gptel-translate-failed 0)
             (list (propertize " failed: " 'face 'gptel-translate-header-desc-face)
                   (propertize (format "%d" gptel-translate-failed)
                               'face 'font-lock-keyword-face))))
          (:eval
           (when-let ((mb (gptel-translate--format-model-backend)))
             (concat
              (propertize " " 'display
                          (if (fboundp 'string-pixel-width)
                              `(space :align-to (- right (,(string-pixel-width mb))))
                            `(space :align-to (- right ,(+ 5 (string-width mb))))))
              (propertize mb 'face 'font-lock-keyword-face)))))))

(provide 'gptel-translate)
;;; gptel-translate.el ends here
