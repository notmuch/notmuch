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
;;
;; To use this as the fcc handler for message-mode,
;; customize the notmuch-fcc-dirs variable

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

(defun notmuch-fcc-initialization ()
  "If notmuch-fcc-directories is set,
   hook them into the message-fcc-handler-function"
    ;; Set up the message-fcc-handler to move mails to the maildir in Fcc
    ;; The parameter is set to mark messages as "seen"
    (setq message-fcc-handler-function
          (lambda (destdir)
	    (notmuch-maildir-fcc-write-buffer-to-maildir destdir t)))
    ;; add a hook to actually insert the Fcc header when sending
    (add-hook 'message-header-setup-hook 'notmuch-fcc-header-setup))

(defun notmuch-fcc-header-setup ()
  "Add an Fcc header to the current message buffer.

Can be added to `message-send-hook' and will set the Fcc header
based on the values of `notmuch-fcc-dirs'. An existing Fcc header
will NOT be removed or replaced."

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
      (message-add-header
       (concat "Fcc: "
	       (file-truename
		;; If the resulting directory is not an absolute path,
		;; prepend the standard notmuch database path.
		(if (= (elt subdir 0) ?/)
		    subdir
		  (concat (notmuch-database-path) "/" subdir)))))
      
      ;; finally test if fcc points to a valid maildir
      (let ((fcc-header (message-field-value "Fcc")))
	(unless (notmuch-maildir-fcc-dir-is-maildir-p fcc-header)
	  (cond ((not (file-writable-p fcc-header))
		 (error (format "No permission to create %s, which does not exist"
				fcc-header)))
		((y-or-n-p (format "%s is not a maildir. Create it? "
				   fcc-header))
		 (notmuch-maildir-fcc-create-maildir fcc-header))
		(t
		 (error "Message not sent"))))))))
 
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

(notmuch-fcc-initialization)
(provide 'notmuch-maildir-fcc)

