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
- :ready   - vector of booleans, t when paragraph's orig text has been inserted
This variable is buffer-local to the result buffer.")

(defvar-local gptel-translate-abortp nil
  "Is abort translate.")

;;; Internal helpers
(defun gptel-translate--stream-init (n)
  "Initialize stream parser state for N paragraphs."
  (setq gptel-translate--stream-state
        (list :buffer "" :pos 0 :index nil
              :ready (make-vector n nil))))

(defun gptel-translate--stream-chunk (chunk orig-buffer orig-paras result-buf)
  "Process one streaming CHUNK of translation response.

ORIG-BUFFER is source buffer.
ORIG-PARAS is source paras.
RESULT-BUF is result buffer.

When a marker `[--PARA_N--]' is detected in the stream, insert the
original text of paragraph N (if not already inserted) and place a
marker for streaming translation updates."
  (with-current-buffer result-buf
    (let ((state gptel-translate--stream-state)
          (inhibit-read-only t))
      (setf (plist-get state :buffer)
            (concat (plist-get state :buffer) chunk))
      (let ((buf-str (plist-get state :buffer))
            (pos (plist-get state :pos))
            (current-idx (plist-get state :index))
            (marker-re "\\n?\\[--PARA_\\([0-9]+\\)--\\]\\n?"))
        (while (string-match marker-re buf-str pos)
          (let* ((end-of-marker (match-end 0))
                 (marker-idx (string-to-number (match-string 1 buf-str)))
                 (ready-vec (plist-get state :ready))
                 (before-marker (substring buf-str pos (match-beginning 0))))
            ;; First, flush the previous paragraph's text (before this marker)
            (when (and current-idx (>= current-idx 0) (< current-idx (length orig-paras))
                       (not (string-blank-p before-marker)))
              (gptel-translate--insert-translate result-buf (cons orig-buffer (cdr (nth current-idx orig-paras))) (string-trim before-marker)))
            ;; If this paragraph's orig text hasn't been inserted yet, do it now
            (unless (aref ready-vec marker-idx)
              (when (< marker-idx (length orig-paras))
                (let* ((orig-para (nth marker-idx orig-paras))
                       (orig (cons orig-buffer (cdr orig-para)))
                       (orig-text (car orig-para)))
                  (gptel-translate--insert-orig result-buf orig orig-text)))
              (aset ready-vec marker-idx t))
            (setq pos end-of-marker)
            (setq current-idx marker-idx)))
        (setf (plist-get state :pos) pos)
        (setf (plist-get state :index) current-idx)
        ;; Update progress in header based on how many slots are ready
        (let ((n-ready (cl-count-if #'identity (plist-get state :ready))))
          (setq gptel-translate-progress n-ready))))))

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
           (pos (plist-get state :pos)))
      ;; find the last detected index by scanning :ready vector
      (let ((last-idx -1))
        (dotimes (i (length (plist-get state :ready)))
          (when (aref (plist-get state :ready) i)
            (setq last-idx i)))
        (when (>= last-idx 0)
          ;; Insert any remaining text after the last marker
          (when (and (< pos (length buf-str))
                     (> (length (substring buf-str pos)) 0))
            (let ((remaining (substring buf-str pos)))
              (when (not (string-blank-p remaining))
                (gptel-translate--insert-translate result-buf (cons orig-buffer (cdr (nth last-idx orig-paras))) (string-trim remaining))))))))))

(defun gptel-translate--resolve-system-prompt (replaces)
  "Return the rendered system prompt.
If `gptel-translate-system-prompt' is an existing file path, render it
with `templatel-render-file'.  Otherwise treat it as a template literal
and render with `templatel-render-string'.
REPLACES is an alist passed to the templatel renderer."
  (let ((prompt gptel-translate-system-prompt))
    (if (and (stringp prompt)
             (file-exists-p (expand-file-name prompt)))
        (templatel-render-file (expand-file-name prompt) replaces)
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
        (max-token (* 1000 (gptel-translate--resolve-context-window) 0.6)))
    (dolist (para paragraphs)
      (let ((para-data (car para)))
        (if (<= (/ (+ current-length (length para-data)) 3) max-token)
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
                                           (format "\n[--PARA_%s--]\n%s" (incf idx) item))
                                         (reverse current-para))
                                 "\n")
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
        (regex "\n?\\[--PARA_\\([0-9]+\\)--\\]\n?")
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

(defun gptel-translate--make-result-buffer (orig-name orig-buffer paragraphs)
  "Create and return a new buffer for translation results.
ORIG-NAME is the source buffer name.
ORIG-BUFFER is the source buffer object.
PARAGRAPHS is a list of (STRING . POSITION) cons cells.

Each result entry has a text property `gptel-translate-orig-pos' whose
value is the buffer position in ORIG-BUFFER where the original paragraph
starts.  Returns the buffer and a list of slot markers."
  (let ((buf (generate-new-buffer
              (format "*translate %s*" orig-name))))
    (with-current-buffer buf
      (goto-char (point-min))
      (gptel-translate-result-mode)
      (setq gptel-translate-abortp nil)
      (setq gptel-translate-orig-buffer-name orig-name)
      (setq gptel-translate-orig-buffer orig-buffer)
      (setq gptel-translate-paragraph-number (length paragraphs))
      (setq gptel-translate-progress 0)
      (setq gptel-translate--current-pos (make-marker))
      (set-marker gptel-translate--current-pos (point))
      (gptel-translate--stream-init (length paragraphs)))
    buf))

(defun gptel-translate--insert-orig (result-buf orig orig-para)
  "Insert ORIG-PARA into RESULT-BUF as the original text paragraph.

RESULT-BUF is the translation result buffer.
ORIG is a cons cell (BUFFER . POSITION) identifying the source
of the original text in the source buffer.
ORIG-PARA is the original paragraph string.

The text is inserted with `gptel-translate-original-face' and
carries the `gptel-translate-orig' text property set to ORIG.
Point is advanced past the insertion."
  (with-current-buffer result-buf
    (let ((inhibit-read-only t)
          (pos (marker-position gptel-translate--current-pos)))
      (save-excursion
        (goto-char pos)
        (insert
         (propertize orig-para
                     'face 'gptel-translate-original-face
                     'gptel-translate-orig orig))
        (insert "\n")
        (set-marker gptel-translate--current-pos (point))))))

(defun gptel-translate--insert-translate (result-buf orig translate)
  "Insert TRANSLATE into RESULT-BUF as the translated text paragraph.

RESULT-BUF is the translation result buffer.
ORIG is a cons cell (BUFFER . POSITION) identifying the source
of the corresponding original text in the source buffer.
TRANSLATE is the translated paragraph string.

The text is inserted with `gptel-translate-translation-face' and
carries the `gptel-translate-orig' text property set to ORIG.
Point is advanced past the insertion, and a blank line is added
after the translation to separate it from the next pair."
  (with-current-buffer result-buf
    (let ((inhibit-read-only t)
          (pos (marker-position gptel-translate--current-pos)))
      (save-excursion
        (goto-char pos)
        (insert (propertize translate 'face 'gptel-translate-translation-face
                            'gptel-translate-orig orig))
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
             for orig-pos = (cdr orig-para)
             for orig = (cons orig-buffer orig-pos)
             for translate = (if (< i parsed-count)
                                 (nth i parsed)
                               "<MISSING>")
             do (gptel-translate--insert-orig result-buf orig (car orig-para))
             do (gptel-translate--insert-translate result-buf orig translate))
    orig-count))

;;; Commands

;;;###autoload
(defun gptel-translate-buffer (&optional beg end)
  "Translate buffer (or region BEG to END) paragraph-by-paragraph.

Show original text and translation side-by-side in a new buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((paragraphs (gptel-translate--collect-paragraphs beg end))
         (total (length paragraphs)))
    (if (zerop total)
        (message "Nothing to translate")
      (let* ((gptel-backend (gptel-translate--resolve-backend))
             (gptel-model (gptel-translate--resolve-model))
             (gptel-tools nil)
             (gptel-use-tools nil)
             (orig-name (buffer-name))
             (orig-buffer (current-buffer))
             (merge-parapgraphs (gptel-translate--merge-paragraphs paragraphs))
             (result-buf (gptel-translate--make-result-buffer orig-name orig-buffer paragraphs))
             (done 0)
             (failures 0))
        (display-buffer result-buf)
        ;; Send requests sequentially via recursive callback chain
        (cl-labels ((send-merged (merge-idx)
                      (if (>= merge-idx (length merge-parapgraphs))
                          (message "Translation complete: %d ok, %d failed, %d total"
                                   done failures total)
                        (let* ((merge-pair (nth merge-idx merge-parapgraphs))
                               (merged-text (car merge-pair))
                               (orig-paras (cdr merge-pair))) ; list of (STRING . POS)
                          (gptel-request (templatel-render-string gptel-translate-user-prompt
                                                                  `(("to" . ,gptel-translate-target-language)
                                                                    ("input" . ,merged-text)))
                            :system (gptel-translate--resolve-system-prompt
                                     `(("to" . ,gptel-translate-target-language)))
                            :stream gptel-translate-streamp
                            :callback
                            (lambda (response _info)
                              (with-current-buffer result-buf
                                (cond (gptel-translate-abortp
                                       (message "Translation abort.")
                                       (call-interactively #'gptel-abort))
                                      ((and (stringp response)
                                            (not (string-empty-p response)))
                                       (if gptel-translate-streamp
                                           (gptel-translate--stream-chunk
                                            response orig-buffer orig-paras result-buf)
                                         (cl-incf done (gptel-translate--apply-parsed-response
                                                        response orig-buffer orig-paras result-buf))
                                         (with-current-buffer result-buf
                                           (setq-local gptel-translate-failed failures)
                                           (setq-local gptel-translate-progress done))
                                         (send-merged (1+ merge-idx))))
                                      ((and gptel-translate-streamp (eq response t))
                                       (gptel-translate--stream-flush
                                        orig-buffer orig-paras result-buf)
                                       (cl-incf done (length orig-paras))
                                       (with-current-buffer result-buf
                                         (setq-local gptel-translate-failed failures)
                                         (setq-local gptel-translate-progress done))
                                       (send-merged (1+ merge-idx)))
                                      ((consp response)) ; streaming intermediate, ignore
                                      (t (progn
                                           (cl-incf failures)
                                           (with-current-buffer result-buf
                                             (setq-local gptel-translate-failed failures))
                                           (send-merged (1+ merge-idx))))))
                              ))))))
          (send-merged 0))))))

;;; Mode

(defvar gptel-translate-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gptel-translate-jump-to-original)

    (define-key map (kbd "TAB") #'gptel-translate-next-paragraph)
    (define-key map (kbd "n") #'gptel-translate-next-paragraph)

    (define-key map (kbd "<backtab>") #'gptel-translate-previous-paragraph)
    (define-key map (kbd "p") #'gptel-translate-previous-paragraph)
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
            (recenter))))))

(defun gptel-translate--find-paragraph-boundaries (&optional backward)
  "Move to the next/previous original paragraph boundary.
If BACKWARD is non-nil, search backward.  Return non-nil if moved.
Puts point at the start of the original text."
  (let ((fn (if backward #'previous-single-property-change
              #'next-single-property-change)))
    (let ((pos (funcall fn (point) 'face)))
      (while (and pos (not (memq (get-text-property pos 'face) '(gptel-translate-translation-face))))
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
  (call-interactively #'gptel-abort)
  (setq gptel-translate-abortp t))

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
                               'face 'font-lock-keyword-face)))))))

(provide 'gptel-translate)
;;; gptel-translate.el ends here
