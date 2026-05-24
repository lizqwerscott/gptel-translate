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
  "System prompt used by LLM."
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

;;; Internal helpers

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

(defun gptel-translate--make-result-buffer (orig-name orig-buffer paragraphs)
  "Create and return a new buffer for translation results.
ORIG-NAME is the source buffer name.
ORIG-BUFFER is the source buffer object.
PARAGRAPHS is a list of (STRING . POSITION) cons cells.

Each result entry has a text property `gptel-translate-orig-pos' whose
value is the buffer position in ORIG-BUFFER where the original paragraph
starts.  Returns the buffer and a list of slot markers."
  (let ((buf (generate-new-buffer
              (format "*translate %s*" orig-name)))
        markers)
    (with-current-buffer buf
      (cl-loop for (para . pos) in paragraphs
               for n from 1
               for slot = (progn
                            (when (> n 1)
                              (insert "\n"))
                            (insert
                             (propertize para
                                         'face 'gptel-translate-original-face
                                         'gptel-translate-orig (cons orig-buffer pos)))
                            (insert "\n")
                            (point-marker))
               do (push slot markers)
               do (insert "\n"))
      (setq markers (nreverse markers))
      (goto-char (point-min))
      (gptel-translate-result-mode)
      (setq gptel-translate-orig-buffer-name orig-name)
      (setq gptel-translate-orig-buffer orig-buffer)
      (setq gptel-translate-paragraph-number (length paragraphs))
      (setq gptel-translate-progress 0))
    (cons buf markers)))

(defun gptel-translate--set-translation-status (orig result-buf slot insertp status)
  "In RESULT-BUF at SLOT (a marker), insert STATUS text.
STATUS can be a translated string, nil meaning \"translating...\",
or an error string.  SLOT always points to the start of the
translation insertion area.

ORIG is a cons cell (ORIG-BUFFER . POS) identifying the original
source paragraph: ORIG-BUFFER is the buffer where the paragraph
resides, and POS is its character position in that buffer."
  (with-current-buffer result-buf
    (let ((inhibit-read-only t)
          (pos (marker-position slot)))   ; remember the original start
      (save-excursion
        (goto-char pos)
        ;; Delete previous content from pos to next blank line or buffer end
        (unless insertp
          (let ((end (save-excursion
                       (if (search-forward "\n\n" nil t)
                           (match-beginning 0)
                         (point-max)))))
            (delete-region pos end)))
        ;; Insert new content
        (insert (cond
                 ((null status) "<translating...>")
                 ((stringp status)
                  (propertize status 'face 'gptel-translate-translation-face
                              'gptel-translate-orig orig))
                 (t (format "<%s>" status))))
        (when insertp
          (set-marker slot (point)))))))

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
             (result-and-slots (gptel-translate--make-result-buffer orig-name orig-buffer paragraphs))
             (result-buf (car result-and-slots))
             (slots (cdr result-and-slots))
             (done 0)
             (failures 0))
        (display-buffer result-buf)
        ;; Send requests sequentially via recursive callback chain
        (cl-labels ((send-one (idx)
                      (if (>= idx total)
                          (message "Translation complete: %d ok, %d failed, %d total"
                                   done failures total)
                        (let* ((para (car (nth idx paragraphs)))
                               (orig-pos (cdr (nth idx paragraphs)))
                               (orig (cons orig-buffer orig-pos))
                               (slot (nth idx slots)))
                          (unless gptel-translate-streamp
                            (gptel-translate--set-translation-status orig result-buf slot nil nil))
                          (gptel-request (format "Translate to %s: %s\n" gptel-translate-target-language para)
                            :system (gptel-translate--resolve-system-prompt
                                     `(("to" . ,gptel-translate-target-language)))
                            :stream gptel-translate-streamp
                            :callback
                            (lambda (response _info)
                              (cond ((and (stringp response)
                                          (not (string-empty-p response)))
                                     (gptel-translate--set-translation-status orig result-buf slot gptel-translate-streamp response)
                                     (unless gptel-translate-streamp
                                       (cl-incf done)
                                       (with-current-buffer result-buf
                                         (setq-local gptel-translate-failed failures)
                                         (setq-local gptel-translate-progress done))
                                       (send-one (1+ idx))))
                                    ((consp response))
                                    ((and gptel-translate-streamp (eq response t))
                                     (cl-incf done)
                                     (with-current-buffer result-buf
                                       (setq-local gptel-translate-failed failures)
                                       (setq-local gptel-translate-progress done))
                                     (send-one (1+ idx)))
                                    (t (progn
                                         (cl-incf failures)
                                         (with-current-buffer result-buf
                                           (setq-local gptel-translate-failed failures))
                                         (gptel-translate--set-translation-status
                                          orig
                                          result-buf slot gptel-translate-streamp
                                          (format "<FAILED: %s>"
                                                  (if (null response)
                                                      "no response"
                                                    "error")))
                                         (send-one (1+ idx)))))))))))
          (send-one 0))))))

;;; Mode

(defvar gptel-translate-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gptel-translate-jump-to-original)

    (define-key map (kbd "TAB") #'gptel-translate-next-paragraph)
    (define-key map (kbd "n") #'gptel-translate-next-paragraph)

    (define-key map (kbd "<backtab>") #'gptel-translate-previous-paragraph)
    (define-key map (kbd "p") #'gptel-translate-previous-paragraph)
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
