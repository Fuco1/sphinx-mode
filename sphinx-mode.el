;;; sphinx-mode.el --- Minor mode providing sphinx support.

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 11th September 2016
;; Keywords: languages
;; Package-requires: ((dash "2.14.0") (f "0.20"))


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

(require 'f)
(require 'dash)
(require 'sphinx-src)
(require 'subr-x)

(defgroup sphinx ()
  "Sphinx group."
  :group 'editing
  :prefix "sphinx-")

(defface sphinx-code-block-face
  '((t (:inherit fixed-pitch)))
  "Face used for code blocks.")

(defun sphinx-fontify-code-block (limit)
  "Fontify code blocks from point to LIMIT."
  (condition-case nil
      (while (re-search-forward "\.\. code-block:: \\(.*?\\)\n\n\\( +\\)" limit t)
        (let* ((block-start (match-end 0))
               (block-highlight-start (match-beginning 2))
               (lang (match-string 1))
               (prefix (match-string 2))
               (prefix-search (concat "^" prefix))
               block-end)
          (while (progn
                   (forward-line)
                   (looking-at prefix-search)))
          (setq block-end (point))
          (sphinx-src-font-lock-fontify-block lang block-start block-end)
          (add-face-text-property
           block-highlight-start block-end
           'sphinx-code-block-face 'append)))
    (error nil)))


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

(defun sphinx-compile-find-makefile-dir () ; adapted from rst-compile-find-conf
  "Look for the Sphinx Makefile in parents of the current path and returns the directory which contains it as a string."
  (interactive)
  (let ((file-name "Makefile")
        (buffer-file (buffer-file-name)))
    ;; Move up in the dir hierarchy till we find Makefile.
    (let* ((dir (file-name-directory buffer-file))
	   (prevdir nil))
      (while (and (or (not (string= dir prevdir))
		      (setq dir nil)
		      nil)
                  (not (file-exists-p (concat dir file-name))))
        ;; Move up to the parent dir and try again.
	(setq prevdir dir)
        (setq dir (expand-file-name (file-name-directory
                                     (directory-file-name
				      (file-name-directory dir))))))
      (or (and dir (concat dir "")) nil))))

;; TODO: replace this with sphinx-compile and sphinx-compile-and-view
(defun sphinx-view-build-html ()
  "Runs 'make html' in project root directory and then views compiled HTML of current source file."
  (interactive)
    (let
      (
       (buffer-name-base (file-name-base (buffer-name))) ; reads name of current bufer (presumably a .rst file), minus extension, into buffer-name-base
       (project-root-dir (sphinx-compile-find-makefile-dir)) ; finds parent directory containing makefile and reads into project-root-dir
       )
      (let
	  (
	   (file-rel-path (string-remove-prefix project-root-dir (file-name-directory buffer-file-name)))
	   )
        (shell-command (concat "make -C " project-root-dir " html"))
	(browse-url (concat (concat (concat project-root-dir "_build/html/" file-rel-path) buffer-name-base) ".html"))
	)
    )
  )

(defvar sphinx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-'") 'sphinx-goto-ref)
    (define-key map (kbd "C-c TAB") 'sphinx-insert-ref)
    (define-key map (kbd "C-c C-c C-v") 'sphinx-view-build-html)
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
      (font-lock-add-keywords nil '((sphinx-fontify-code-block)))
    (font-lock-remove-keywords nil '((sphinx-fontify-code-block)))))

(provide 'sphinx-mode)
;;; sphinx-mode.el ends here
