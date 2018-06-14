;;; rstdoc.el --- help generate documentation from docstrings -*-lexical-binding: t-*-

;; Copyright (C) 2018 David Bremner

;; Author: David Bremner <david@tethera.net>
;; Created: 26 May 2018
;; Keywords: emacs lisp, documentation
;; Homepage: https://notmuchmail.org

;; This file is not part of GNU Emacs.

;; rstdoc.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; rstdoc.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with rstdoc.el.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;

;; Rstdoc provides a facility to extract all of the docstrings defined in
;; an elisp source file. Usage:
;;
;; emacs -Q --batch -L . -l rstdoc -f rstdoc-batch-extract foo.el foo.rsti

;;; Code:

(provide 'rstdoc)

(defun rstdoc-batch-extract ()
  "Extract docstrings to and from the files on the command line"
  (apply #'rstdoc-extract command-line-args-left))

(defun rstdoc-extract (in-file out-file)
  "Write docstrings from IN-FILE to OUT-FILE"
  (load-file in-file)
  (let* ((definitions (cdr (assoc (expand-file-name in-file) load-history)))
	 (doc-hash (make-hash-table :test 'eq)))
    (mapc
     (lambda (elt)
       (let ((pair
	      (pcase elt
		(`(defun . ,name) (cons name (documentation name)))
		(`(,_ . ,_)  nil)
		(sym (cons sym (get sym 'variable-documentation))))))
	 (when (and pair (cdr pair))
	   (puthash (car pair) (cdr pair) doc-hash))))
     definitions)
    (with-temp-buffer
      (maphash
       (lambda (key val)
	 (rstdoc--insert-docstring key val))
       doc-hash)
      (write-region (point-min) (point-max) out-file))))

(defun rstdoc--insert-docstring (symbol docstring)
  (insert (format "\n.. |docstring::%s| replace::\n" symbol))
  (insert (replace-regexp-in-string "^" "    " (rstdoc--rst-quote-string docstring)))
  (insert "\n"))

(defvar rst--escape-alist
  '( ("\\\\='" . "\\\\'")
     ("\\([^\\]\\)'" . "\\1`")
     ("^[[:space:]\t]*$" . "|br|")
     ("^[[:space:]\t]" . "|indent| "))
    "list of (regex . replacement) pairs")

(defun rstdoc--rst-quote-string (str)
  (with-temp-buffer
    (insert str)
    (dolist (pair rst--escape-alist)
      (goto-char (point-min))
      (while (re-search-forward (car pair) nil t)
	(replace-match (cdr pair))))
    (buffer-substring (point-min) (point-max))))

;;; rstdoc.el ends here
