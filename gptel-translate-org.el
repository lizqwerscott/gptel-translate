;;; gptel-translate-org.el --- gptel translate for org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

;;

;;; Code:

(require 'org-element)

(defun gptel-translate--collect-org-subtrees (&optional beg end)
  "Parse Org document structure using org-element, extracting content by subtree.

BEG and END are optional, specifying the region range.

Returns a list, each element is a plist:
  (:heading HEADING :body BODY :pos POS :level LEVEL :children CHILDREN)

HEADING is the full headline line (with asterisks), e.g. \"* Introduction\".
BODY is the body text between the headline and the first child headline,
split by paragraph into a list of (STRING . POS).
POS is the position of the headline in the buffer.
CHILDREN is a list of recursively parsed child nodes (same format)."
  (save-excursion
    (save-restriction
      (when (and beg end) (narrow-to-region beg end))
      (goto-char (point-min))
      (let ((tree (ignore-errors
                    (org-element-parse-buffer 'headline))))
        (when tree
          (gptel-translate--org-extract-nodes
           (org-element-contents tree)))))))

(defun gptel-translate--org-extract-nodes (contents)
  "Recursively extract subtree nodes from an org-element CONTENTS list.

CONTENTS is the list of parsed org-element contents (e.g., the :contents
property value). Returns a list of (:heading :body :pos :level :children)
plists, where :body is a list of (STRING . POS).

Only elements of type headline are extracted; non-headline elements are skipped."
  (let (result)
    (dolist (elem contents)
      (when (eq (car elem) 'headline)
        (let* ((level (org-element-property :level elem))
               (raw (org-element-property :raw-value elem))
               (stars (make-string level ?*))
               (heading (concat stars " " raw))
               (pos (org-element-property :begin elem))
               (cbeg (org-element-property :contents-begin elem))
               (cend (org-element-property :contents-end elem))
               ;; Collect all child headlines within CONTENTS
               (child-headlines
                (cl-remove-if-not
                 (lambda (c) (eq (car c) 'headline))
                 (org-element-contents elem)))
               body-end body children)
          ;; Compute body range: from contents-begin to the first child headline's begin
          ;; If there are no child headlines, use contents-end instead
          (if child-headlines
              (setq body-end (org-element-property :begin (car child-headlines)))
            (setq body-end cend))
          ;; Extract body text, split by forward-paragraph into (STRING . POS) list.
          ;; Skip property drawers and source code blocks.
          (setq body (when (and cbeg body-end (> body-end cbeg))
                       (save-excursion
                         (goto-char cbeg)
                         (skip-chars-forward "\n")
                         (let (paras)
                           (while (< (point) body-end)
                             (skip-chars-forward "[:blank:\n\r]" body-end)
                             (let* ((elem (org-element-at-point))
                                    (elem-type (car elem))
                                    (elem-end (org-element-property :end elem)))
                               (if (memq elem-type '(property-drawer src-block))
                                   (goto-char (min elem-end body-end))
                                 (let ((para-start (point)))
                                   (forward-paragraph)
                                   (when (> (point) body-end)
                                     (goto-char body-end))
                                   (let ((para-text
                                          (buffer-substring-no-properties
                                           para-start (point))))
                                     (unless (string-blank-p para-text)
                                       (push (cons para-text para-start) paras)))))))
                           (nreverse paras)))))
          (setq children
                (gptel-translate--org-extract-nodes
                 (org-element-contents elem)))
          (push (list :heading heading :body body :pos pos :level level :children children)
                result))))
    (nreverse result)))

(defun gptel-translate--collect-org-items (&optional beg end)
  "Collect Org buffer content into (STRING . POS) items.

Each item is either a heading line or an individual body paragraph.
Collects recursively in subtree mode: headings and body paragraphs
are collected depth-first, keeping parent and children together.

Each STRING carries the following text properties for subtree context:
- `:subtree-level` (integer): the heading level (1-based) of the owning subtree.

BEG and END specify the region to collect from (default: entire buffer).

Return a list of (STRING . POS) cons cells where:
- STRING is the text of a heading line or a single body paragraph
  (with `:subtree-level` properties).
- POS is the buffer position of the item's start.

All items are in document order (top-to-bottom, depth-first for subtree)."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (let* ((trees (gptel-translate--collect-org-subtrees beg end))
           (items))
      ;; Flatten tree structure into items, carrying subtree metadata
      (cl-labels
          ((flatten-tree
             (tree)
             (when tree
               (let ((heading (plist-get tree :heading))
                     (body    (plist-get tree :body))       ; list of (STRING . POS)
                     (pos     (plist-get tree :pos))
                     (level   (plist-get tree :level))
                     (children (plist-get tree :children))) ; list of trees
                 ;; Annotate a STRING with subtree-level and subtree-id text properties
                 (cl-labels
                     ((annotate (str &optional headingp)
                        (let ((len (length str)))
                          (put-text-property 0 len :headingp headingp str)
                          (put-text-property 0 len :subtree-level level str))
                        str))
                   ;; Add heading as an item with subtree metadata
                   (when heading
                     (push (cons (annotate heading t) pos) items))
                   ;; Add each body paragraph as an item with subtree metadata
                   (dolist (para body)
                     (push (cons (annotate (car para)) (cdr para)) items)))
                 ;; Process children recursively (subtree mode)
                 (dolist (child children)
                   (flatten-tree child))))))
        (dolist (tree trees)
          (flatten-tree tree)))
      (nreverse items))))

(defun gptel-translate--merge-org-items (items max-chars)
  "Merge ITEMS into batches preserving Org subtree structure.

ITEMS is a list of (STRING . POS) cons cells with :subtree-level
and :headingp text properties, as returned by
`gptel-translate--collect-org-items'.

MAX-CHARS is the maximum total characters allowed per batch.

Returns a list of (BATCH . ORIG-ITEMS) cons cells where BATCH is a
single string with [--PARA_N--] markers and ORIG-ITEMS is the list
of original (STRING . POS) cons cells for that batch.

Subtree structure is preserved: headings and their immediate body
paragraphs form an inseparable unit.  Splitting only occurs at
heading boundaries, keeping child subtrees with their parent when
space allows."
  (when items
    (cl-labels
        ((finalize (units)
           ;; Turn a list of units into a (BATCH . ORIG-ITEMS) cons.
           ;; Each unit is (ITEMS . TOTAL-CHARS).
           (let* ((all-items (cl-loop for u in units append (car u)))
                  (idx 0))
             (cons (string-join
                    (mapcar (lambda (it)
                              (format "[--PARA_%s--]\n%s"
                                      (prog1 idx (cl-incf idx))
                                      (car it)))
                            all-items)
                    "")
                   (mapcar (lambda (it) (cons (car it) (cdr it)))
                           all-items)))))
      ;; Step 1: Group items into heading-anchored units.
      ;; Each unit = a heading + its immediate body paragraphs
      ;; (non-heading items up to the next heading).
      (let ((units nil)
            (cur nil)
            (cur-len 0))
        (dolist (item items)
          (let* ((str (car item))
                 (headingp (get-text-property 0 :headingp str)))
            (if headingp
                (progn
                  (when cur
                    (push (cons (nreverse cur) cur-len) units))
                  (setq cur (list item)
                        cur-len (length str)))
              (push item cur)
              (cl-incf cur-len (length str)))))
        (when cur
          (push (cons (nreverse cur) cur-len) units))
        (setq units (nreverse units))

        ;; Step 2: Greedily merge units into batches.
        (let ((result nil)
              (batch nil)
              (batch-len 0))
          (dolist (unit units)
            (let ((unit-len (cdr unit)))
              (if (and batch
                       (<= (+ batch-len unit-len) max-chars))
                  (progn
                    (push unit batch)
                    (cl-incf batch-len unit-len))
                (when batch
                  (push (finalize (nreverse batch)) result))
                (setq batch (list unit)
                      batch-len unit-len))))
          (when batch
            (push (finalize (nreverse batch)) result))
          (nreverse result))))))


(provide 'gptel-translate-org)
;;; gptel-translate-org.el ends here
