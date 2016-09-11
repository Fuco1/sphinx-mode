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

(require 'sphinx-src)

(defgroup sphinx ()
  "Sphinx group."
  :group 'editing
  :prefix "sphinx-")

(defun sphinx-fontify-code-block (limit)
  "Fontify code blocks from point to LIMIT."
  (condition-case nil
      (while (re-search-forward "^\.\. code-block:: \\(.*?\\)\n\n\\( +\\)" limit t)
        (let* ((block-start (match-end 0))
               (block-highlight-start (match-beginning 2))
               (lang (match-string 1))
               (prefix (match-string 2))
               (prefix-search (concat "^" prefix))
               block-end)
          (while (progn
                   (forward-line)
                   (looking-at prefix-search)))
          (setq block-end (progn (forward-line) (point)))
          (sphinx-src-font-lock-fontify-block lang block-start block-end)
          (add-face-text-property
           block-highlight-start block-end
           '(:inherit fixed-pitch :background "#232a2b") 'append)))
    (error nil)))

;;;###autoload
(define-minor-mode sphinx-mode
  "Sphinx minor mode."
  :init-value nil
  :lighter "sphinx "
  ;; add native fontification support
  (if sphinx-mode
      (font-lock-add-keywords nil '((sphinx-fontify-code-block)))
    (font-lock-remove-keywords nil '((sphinx-fontify-code-block)))))

(provide 'sphinx-mode)
;;; sphinx-mode.el ends here
