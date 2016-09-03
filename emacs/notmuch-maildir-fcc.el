;;; notmuch-maildir-fcc.el ---

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use this as the fcc handler for message-mode,
;; customize the notmuch-fcc-dirs variable

;;; Code:

(eval-when-compile (require 'cl))
(require 'message)

(require 'notmuch-lib)

(defvar notmuch-maildir-fcc-count 0)

(defcustom notmuch-fcc-dirs "sent"
 "Determines the maildir directory in which to save outgoing mail.

Three types of values are permitted:

- nil: no Fcc header is added,

- a string: the value of `notmuch-fcc-dirs' is the name of the
  folder to use,

- a list: the folder is chosen based on the From address of the
  current message using a list of regular expressions and
  corresponding folders:

     ((\"Sebastian@SSpaeth.de\" . \"privat\")
      (\"spaetz@sspaeth.de\" . \"OUTBOX.OSS\")
      (\".*\" . \"defaultinbox\"))

  If none of the regular expressions match the From address, no
  Fcc header will be added.

In all cases, a relative FCC directory will be understood to
specify a directory within the notmuch mail store, (as set by
the database.path option in the notmuch configuration file).

You will be prompted to create the directory if it does not exist
yet when sending a mail."

 :type '(choice
	 (const :tag "No FCC header" nil)
	 (string :tag "A single folder")
	 (repeat :tag "A folder based on the From header"
		 (cons regexp (string :tag "Folder"))))
 :require 'notmuch-fcc-initialization
 :group 'notmuch-send)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions which set up the fcc header in the message buffer.

(defun notmuch-fcc-header-setup ()
  "Add an Fcc header to the current message buffer.

Sets the Fcc header based on the values of `notmuch-fcc-dirs'.

Originally intended to be use a hook function, but now called directly
by notmuch-mua-mail"

  (let ((subdir
	 (cond
	  ((or (not notmuch-fcc-dirs)
	       (message-field-value "Fcc"))
	   ;; Nothing set or an existing header.
	   nil)

	  ((stringp notmuch-fcc-dirs)
	   notmuch-fcc-dirs)

	  ((and (listp notmuch-fcc-dirs)
		(stringp (car notmuch-fcc-dirs)))
	   ;; Old style - no longer works.
	   (error "Invalid `notmuch-fcc-dirs' setting (old style)"))

	  ((listp notmuch-fcc-dirs)
	   (let* ((from (message-field-value "From"))
		  (match
		   (catch 'first-match
		     (dolist (re-folder notmuch-fcc-dirs)
		       (when (string-match-p (car re-folder) from)
			 (throw 'first-match re-folder))))))
	     (if match
		 (cdr match)
	       (message "No Fcc header added.")
	       nil)))

	  (t
	   (error "Invalid `notmuch-fcc-dirs' setting (neither string nor list)")))))

    (when subdir
      (notmuch-maildir-add-file-style-fcc-header subdir))))

(defun notmuch-maildir-add-file-style-fcc-header (subdir)
  (message-add-header
   (concat "Fcc: "
	   (file-truename
	    ;; If the resulting directory is not an absolute path,
	    ;; prepend the standard notmuch database path.
	    (if (= (elt subdir 0) ?/)
		subdir
	      (concat (notmuch-database-path) "/" subdir))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for saving a message either using notmuch insert or file
;; fcc. First functions common to the two cases.

(defun notmuch-maildir-message-do-fcc ()
  "Process Fcc headers in the current buffer.

This is a direct copy from message-mode's message-do-fcc."
  (let ((case-fold-search t)
	(buf (current-buffer))
	list file
	(mml-externalize-attachments message-fcc-externalize-attachments))
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(setq file (message-fetch-field "fcc" t)))
      (when file
	(set-buffer (get-buffer-create " *message temp*"))
	(erase-buffer)
	(insert-buffer-substring buf)
	(message-encode-message-body)
	(save-restriction
	  (message-narrow-to-headers)
	  (while (setq file (message-fetch-field "fcc" t))
	    (push file list)
	    (message-remove-header "fcc" nil t))
	  (let ((mail-parse-charset message-default-charset)
		(rfc2047-header-encoding-alist
		 (cons '("Newsgroups" . default)
		       rfc2047-header-encoding-alist)))
	    (mail-encode-encoded-word-buffer)))
	(goto-char (point-min))
	(when (re-search-forward
	       (concat "^" (regexp-quote mail-header-separator) "$")
	       nil t)
	  (replace-match "" t t ))
	;; Process FCC operations.
	(while list
	  (setq file (pop list))
	  (if (string-match "^[ \t]*|[ \t]*\\(.*\\)[ \t]*$" file)
	      ;; Pipe the article to the program in question.
	      (call-process-region (point-min) (point-max) shell-file-name
				   nil nil nil shell-command-switch
				   (match-string 1 file))
	    ;; Save the article.
	    (setq file (expand-file-name file))
	    (unless (file-exists-p (file-name-directory file))
	      (make-directory (file-name-directory file) t))
	    (if (and message-fcc-handler-function
		     (not (eq message-fcc-handler-function 'rmail-output)))
		(funcall message-fcc-handler-function file)
	      ;; FIXME this option, rmail-output (also used if
	      ;; message-fcc-handler-function is nil) is not
	      ;; documented anywhere AFAICS.  It should work in Emacs
	      ;; 23; I suspect it does not work in Emacs 22.
	      ;; FIXME I don't see the need for the two different cases here.
	      ;; mail-use-rfc822 makes no difference (in Emacs 23),and
	      ;; the third argument just controls \"Wrote file\" message.
	      (if (and (file-readable-p file) (mail-file-babyl-p file))
		  (rmail-output file 1 nil t)
		(let ((mail-use-rfc822 t))
		  (rmail-output file 1 t t))))))
	(kill-buffer (current-buffer))))))

(defun notmuch-fcc-handler (fcc-header)
  "Store message with file fcc."
  (notmuch-maildir-fcc-file-fcc fcc-header))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for saving a message using file fcc.

(defun notmuch-maildir-fcc-host-fixer (hostname)
  (replace-regexp-in-string "/\\|:"
			    (lambda (s)
			      (cond ((string-equal s "/") "\\057")
				    ((string-equal s ":") "\\072")
				    (t s)))
			    hostname
			    t
			    t))

(defun notmuch-maildir-fcc-make-uniq-maildir-id ()
   (let* ((ftime (float-time))
	  (microseconds (mod (* 1000000 ftime) 1000000))
	  (hostname (notmuch-maildir-fcc-host-fixer system-name)))
     (setq notmuch-maildir-fcc-count (+ notmuch-maildir-fcc-count 1))
     (format "%d.%d_%d_%d.%s"
	     ftime
	     (emacs-pid)
	     microseconds
	     notmuch-maildir-fcc-count
	     hostname)))

(defun notmuch-maildir-fcc-dir-is-maildir-p (dir)
  (and (file-exists-p (concat dir "/cur/"))
       (file-exists-p (concat dir "/new/"))
       (file-exists-p (concat dir "/tmp/"))))

(defun notmuch-maildir-fcc-create-maildir (path)
  (cond ((or (not (file-exists-p path)) (file-directory-p path))
	 (make-directory (concat path "/cur/") t)
	 (make-directory (concat path "/new/") t)
	 (make-directory (concat path "/tmp/") t))
	((file-regular-p path)
	 (error "%s is a file. Can't create maildir." path))
	(t
	 (error "I don't know how to create a maildir here"))))

(defun notmuch-maildir-fcc-save-buffer-to-tmp (destdir)
  "Returns the msg id of the message written to the temp directory
if successful, nil if not."
  (let ((msg-id (notmuch-maildir-fcc-make-uniq-maildir-id)))
    (while (file-exists-p (concat destdir "/tmp/" msg-id))
      (setq msg-id (notmuch-maildir-fcc-make-uniq-maildir-id)))
    (cond ((notmuch-maildir-fcc-dir-is-maildir-p destdir)
	   (write-file (concat destdir "/tmp/" msg-id))
	   msg-id)
	  (t
	   (error (format "Can't write to %s. Not a maildir."
			  destdir))
	   nil))))

(defun notmuch-maildir-fcc-move-tmp-to-new (destdir msg-id)
  (add-name-to-file
   (concat destdir "/tmp/" msg-id)
   (concat destdir "/new/" msg-id ":2,")))

(defun notmuch-maildir-fcc-move-tmp-to-cur (destdir msg-id &optional mark-seen)
  (add-name-to-file
   (concat destdir "/tmp/" msg-id)
   (concat destdir "/cur/" msg-id ":2," (when mark-seen "S"))))

(defun notmuch-maildir-fcc-file-fcc (fcc-header)
  "Write the message to the file specified by FCC-HEADER.

It offers the user a chance to correct the header, or filesystem,
if needed."
  (if (notmuch-maildir-fcc-dir-is-maildir-p fcc-header)
      (notmuch-maildir-fcc-write-buffer-to-maildir fcc-header 't)
    ;; The fcc-header is not a valid maildir see if the user wants to
    ;; fix it in some way.
    (let* ((prompt (format "Fcc %s is not a maildir: (r)etry, (c)reate folder, (i)gnore, or  (e)dit the header? "
			   fcc-header))
	    (response (read-char-choice prompt '(?r ?c ?i ?e))))
	 (case response
	       (?r (notmuch-maildir-fcc-file-fcc fcc-header))
	       (?c (if (file-writable-p fcc-header)
		       (notmuch-maildir-fcc-create-maildir fcc-header)
		     (message "No permission to create %s." fcc-header)
		     (sit-for 2))
		   (notmuch-maildir-fcc-file-fcc fcc-header))
	       (?i 't)
	       (?e (notmuch-maildir-fcc-file-fcc
		    (read-from-minibuffer "Fcc header: " fcc-header)))))))

(defun notmuch-maildir-fcc-write-buffer-to-maildir (destdir &optional mark-seen)
  "Writes the current buffer to maildir destdir. If mark-seen is
non-nil, it will write it to cur/, and mark it as read. It should
return t if successful, and nil otherwise."
  (let ((orig-buffer (buffer-name)))
    (with-temp-buffer
      (insert-buffer-substring orig-buffer)
      (catch 'link-error
	(let ((msg-id (notmuch-maildir-fcc-save-buffer-to-tmp destdir)))
	  (when msg-id
	    (cond (mark-seen
		   (condition-case err
		       (notmuch-maildir-fcc-move-tmp-to-cur destdir msg-id t)
		     (file-already-exists
		      (throw 'link-error nil))))
		  (t
		   (condition-case err
		       (notmuch-maildir-fcc-move-tmp-to-new destdir msg-id)
		     (file-already-exists
		      (throw 'link-error nil))))))
	  (delete-file (concat destdir "/tmp/" msg-id))))
      t)))

(provide 'notmuch-maildir-fcc)

;;; notmuch-maildir-fcc.el ends here
