;;; autoloads-gen.el --- help generate autoloads  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Pengji Zhang
;;
;; This file is part of Notmuch.
;;
;; Notmuch is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Notmuch is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Notmuch.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Helpers for generating a `notmuch-autoloads.el' file.
;;
;; This file is written specifically for the Notmuch project. Some
;; design choices here perhaps only make sense for Notmuch.
;;
;; An alternative way is to directly call `package-generate-autoloads'
;; on our source directory. It was not chosen because we could not
;; easily exclude files. Besides, that function is for packages, so
;; using it on a non-package directory feels a bit hacky.

;;; Code:

(require 'seq) ; `seq-uniq', `seq-difference' (both in Emacs >= 25)

(defvar generated-autoload-file)
(defvar autoload-excludes) ; from obsolete autoloads.el

(defun autoloads-gen-batch ()
  "Generate autoloads on the command line.
First argument is the output file, and the rest are source files."
  (let ((output-file (car command-line-args-left))
	(sources (cdr command-line-args-left)))
    (setq command-line-args-left nil)
    (autoloads-gen (expand-file-name output-file)
		   (mapcar #'expand-file-name sources))))

(defun autoloads-gen (output-file sources)
  "Generate autoloads for SOURCES and write them to OUTPUT-FILE.
All filenames should be absolute.

Note that this function always generate OUTPUT-FILE anew, instead
of just updating added or changed autoloads."
  ;; Here we always generate a new file to avoid potential troubles
  ;; when switching Emacs versions, and also to update the timestamp
  ;; of the output file reliably.
  (let* ((dirs (seq-uniq (mapcar #'file-name-directory sources)))
	 (excludes (mapcan (lambda (dir)
			     (seq-difference (directory-files dir t)
					     sources))
			   dirs)))
    ;; NOTE: The generated file does not contain the additional
    ;; expression to modify `load-path', as is done by `package.el',
    ;; because it is tedious to do for Emacs <= 29. Besides, this file
    ;; is intended to be installed to some directory that is already
    ;; in `load-path'.
    (if (fboundp 'loaddefs-generate)
	(loaddefs-generate dirs output-file excludes nil nil t)
      ;; In Emacs >= 29, we have the new `loaddefs-gen' library, used
      ;; above, and that superseded the now obsolete `autoload'
      ;; library, used below.
      (when (file-exists-p output-file)
	(delete-file output-file))
      (let ((generated-autoload-file output-file)
	    (autoload-excludes excludes)
	    (backup-inhibited t))
	(mapc #'update-directory-autoloads dirs)))))

;;; autoloads-gen.el ends here
