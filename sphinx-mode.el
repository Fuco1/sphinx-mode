;;; sphinx-mode.el --- Minor mode providing sphinx support.

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 11th September 2016
;; Keywords: languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'rst)

(require 'dash)
(require 'f)
(require 's)
(require 'sphinx-src)

(defgroup sphinx ()
  "Sphinx group."
  :group 'editing
  :prefix "sphinx-")

(defface sphinx-code-block-face
  '((t (:inherit fixed-pitch)))
  "Face used for code blocks.")

(defvar sphinx-code-block-default-language "python"
  "The default language used for highlighting code blocks.")

(defun sphinx--fontify-code-blocks (limit)
  "Fontify code blocks from point to LIMIT."
  (save-excursion
    ;; This is for the ‘.. code-block::’ syntax.
    (while (re-search-forward "\.\. code-block::" limit t)
      (let* ((language
              (s-trim (buffer-substring-no-properties (point) (line-end-position))))
             (end (sphinx--fontify-block (point) language)))
        (goto-char end))))
  (save-excursion
    ;; This is for the ‘Here is some code::’ syntax.
    (while (re-search-forward "::$" limit t)
      (let ((line-is-rst-directive
             (save-excursion
               (back-to-indentation)
               (looking-at-p "\.\. ")))
            end)
        (unless line-is-rst-directive
          (setq end (sphinx--fontify-block (point)))
          (goto-char end))))))

(defun sphinx--fontify-block (beg &optional language)
  "Fontify indented block starting after BEG using LANGUAGE.

BEG should be on the line that ‘announces’ the block, e.g. the line
containing ‘.. code-block::’ or ending with ‘::’."
  (when (or (null language) (string-equal language ""))
    (setq language sphinx-code-block-default-language))
  (save-excursion
    (goto-char beg)
    (let* ((indent
            (progn
              (back-to-indentation)
              (+ 2 (current-column))))
           (highlight-beg
            (progn
              (forward-line)
              (while (and (looking-at-p "$") (not (eobp)))
                (forward-line))
              (point)))
           (highlight-end
            (progn
              (unless (rst-forward-indented-block indent)
                (goto-char (point-max)))
              (point))))
      (condition-case nil
          (sphinx-src-font-lock-fontify-block
           language highlight-beg highlight-end)
        (error nil))
      (add-face-text-property
       highlight-beg highlight-end 'sphinx-code-block-face 'append)
      highlight-end)))

(defun sphinx--get-refs-from-buffer (&optional buffer)
  "Get all refs from BUFFER.

If BUFFER is not given use the `current-buffer'."
  (setq buffer (current-buffer))
  (let (re)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward "^.. _\\(.*\\):\\s-*$" nil t)
            (push (list :name (match-string-no-properties 1)
                        :file (buffer-file-name)
                        :point (point)) re)))))
    (nreverse re)))

;; TODO: add caching
(defun sphinx--get-refs ()
  "Get all available refs in the project."
  (let* ((root (locate-dominating-file (buffer-file-name) "conf.py"))
         (sources (f-entries root (lambda (file) (f-ext-p file "rst"))))
         (re))
    (-each sources
      (lambda (source)
        (when (file-exists-p source)
          (if (get-file-buffer source)
              (with-current-buffer (find-file-noselect source)
                (push (sphinx--get-refs-from-buffer) re))
            (with-temp-buffer
              (insert-file-contents-literally source)
              (push (sphinx--get-refs-from-buffer) re))))))
    (apply '-concat (nreverse re))))

(defun sphinx-insert-ref (ref &optional title)
  "Insert a REF with a TITLE."
  (interactive
   (let ((ref (completing-read
               "Ref: " (-map (lambda (r)
                               (plist-get r :name))
                             (sphinx--get-refs)))))
     (list ref (read-from-minibuffer "Title: " nil nil nil nil ref))))
  (insert (if (and (stringp title)
                   (not (equal title "")))
              (format ":ref:`%s<%s>`" title ref)
            (format ":ref:`%s`" ref))))

;; TODO: add better default
(defun sphinx-goto-ref (ref)
  "Go to reference REF."
  (interactive
   (let ((ref (completing-read
               (format "Ref [default %s]: "
                       (symbol-at-point))
               (-map (lambda (r)
                       (plist-get r :name))
                     (sphinx--get-refs))
               nil nil nil nil (symbol-at-point))))
     (list ref)))
  (-when-let (target (--first (equal (plist-get it :name) ref) (sphinx--get-refs)))
    (find-file (plist-get target :file))
    (goto-char (plist-get target :point))))

(defvar sphinx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-'") 'sphinx-goto-ref)
    (define-key map (kbd "C-c TAB") 'sphinx-insert-ref)
    map)
  "Sphinx-mode keymap.")

;;;###autoload
(define-minor-mode sphinx-mode
  "Sphinx minor mode."
  :init-value nil
  :lighter "sphinx "
  :keymap 'sphinx-mode-map
  ;; add native fontification support
  (if sphinx-mode
      (font-lock-add-keywords nil '((sphinx--fontify-code-blocks)))
    (font-lock-remove-keywords nil '((sphinx--fontify-code-blocks)))))

(provide 'sphinx-mode)
;;; sphinx-mode.el ends here
