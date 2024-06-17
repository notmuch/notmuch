;;; notmuch-maildir-fcc.el --- inserting using a fcc handler  -*- lexical-binding: t -*-

;; Copyright © Jesse Rosenthal
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
;;
;; Authors: Jesse Rosenthal <jrosenthal@jhu.edu>

;;; Code:

(require 'seq)

(require 'message)

(require 'notmuch-lib)

(defvar notmuch-maildir-fcc-count 0)

;;; Options

(defcustom notmuch-fcc-dirs "sent"
  "Determines the Fcc Header which says where to save outgoing mail.

Three types of values are permitted:

- nil: no Fcc header is added,

- a string: the value of `notmuch-fcc-dirs' is the Fcc header to
  be used.

- an alist: the folder is chosen based on the From address of
  the current message according to an alist mapping regular
  expressions to folders or nil:

     ((\"Sebastian@SSpaeth.de\" . \"privat\")
      (\"spaetz@sspaeth.de\" . \"OUTBOX.OSS\")
      (\".*\" . \"defaultinbox\"))

  If none of the regular expressions match the From address, or
  if the cdr of the matching entry is nil, then no Fcc header
  will be added.

If `notmuch-maildir-use-notmuch-insert' is set (the default) then
the header should be of the form \"folder +tag1 -tag2\" where
folder is the folder (relative to the notmuch mailstore) to store
the message in, and tag1 and tag2 are tag changes to apply to the
stored message. This string is split using `split-string-and-unquote',
so a folder name containing spaces can be specified by
quoting each space with an immediately preceding backslash
or surrounding the entire folder name in double quotes.

If `notmuch-maildir-use-notmuch-insert' is nil then the Fcc
header should be the directory where the message should be
saved. A relative directory will be understood to specify a
directory within the notmuch mail store, (as set by the
database.path option in the notmuch configuration file).

In all cases you will be prompted to create the folder or
directory if it does not exist yet when sending a mail."

  :type '(choice
	  (const :tag "No FCC header" nil)
	  (string :tag "A single folder")
	  (repeat :tag "A folder based on the From header"
		  (cons regexp (choice (const :tag "No FCC header" nil)
				       (string :tag "Folder")))))
  :require 'notmuch-fcc-initialization
  :group 'notmuch-send)

(defcustom notmuch-maildir-use-notmuch-insert t
  "Should fcc use notmuch insert instead of simple fcc."
  :type '(choice :tag "Fcc Method"
		 (const :tag "Use notmuch insert" t)
		 (const :tag "Use simple fcc" nil))
  :group 'notmuch-send)

;;; Functions which set up the fcc header in the message buffer.

(defun notmuch-fcc-header-setup ()
  "Add an Fcc header to the current message buffer.

If the Fcc header is already set, then keep it as-is.
Otherwise set it according to `notmuch-fcc-dirs'."
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
	   (if-let ((match (seq-some (let ((from (message-field-value "From")))
				       (pcase-lambda (`(,regexp . ,folder))
					 (and (string-match-p regexp from)
					      (cons t folder))))
				     notmuch-fcc-dirs)))
               (cdr match)
             (message "No Fcc header added.")
	     nil))
	  (t
	   (error "Invalid `notmuch-fcc-dirs' setting (neither string nor list)")))))
    (when subdir
      (if notmuch-maildir-use-notmuch-insert
	  (notmuch-maildir-add-notmuch-insert-style-fcc-header subdir)
	(notmuch-maildir-add-file-style-fcc-header subdir)))))

(defun notmuch-maildir-add-notmuch-insert-style-fcc-header (subdir)
  ;; Notmuch insert does not accept absolute paths, so check the user
  ;; really want this header inserted.
  (when (or (not (= (elt subdir 0) ?/))
	    (y-or-n-p (format "Fcc header %s is an absolute path %s %s" subdir
			      "and notmuch insert is requested."
			      "Insert header anyway? ")))
    (message-add-header (concat "Fcc: " subdir))))

(defun notmuch-maildir-add-file-style-fcc-header (subdir)
  (message-add-header
   (concat "Fcc: "
	   (file-truename
	    ;; If the resulting directory is not an absolute path,
	    ;; prepend the standard notmuch database path.
	    (if (= (elt subdir 0) ?/)
		subdir
	      (concat (notmuch-database-path) "/" subdir))))))

;;; Functions for saving a message using either method.

(defmacro with-temporary-notmuch-message-buffer (&rest body)
  "Set-up a temporary copy of the current message-mode buffer."
  `(save-restriction
     (widen)
     (let ((case-fold-search t)
	   (buf (current-buffer))
	   (mml-externalize-attachments message-fcc-externalize-attachments))
       (with-current-buffer (get-buffer-create " *message temp*")
	 (message-clone-locals buf) ;; for message-encoded-mail-cache
	 (erase-buffer)
	 (insert-buffer-substring buf)
	 ,@body))))

(defun notmuch-maildir-setup-message-for-saving ()
  "Setup message for saving.

This should be called on a temporary copy.
This is taken from the function message-do-fcc."
  (if (not message-encoded-mail-cache)
      (message-encode-message-body)
    (erase-buffer)
    (insert message-encoded-mail-cache))
  (save-restriction
    (message-narrow-to-headers)
    (mail-encode-encoded-word-buffer))
  (goto-char (point-min))
  (when (re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "$")
	 nil t)
    (replace-match "" t t )))

(defun notmuch-maildir-message-do-fcc ()
  "Process Fcc headers in the current buffer.

This is a rearranged version of message mode's message-do-fcc."
  (let (files file)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(setq file (message-fetch-field "fcc" t)))
      (when file
	(with-temporary-notmuch-message-buffer
	 (notmuch-maildir-setup-message-for-saving)
	 (save-restriction
	   (message-narrow-to-headers)
	   (while (setq file (message-fetch-field "fcc" t))
	     (push file files)
	     (message-remove-header "fcc" nil t)))
	 ;; Process FCC operations.
	 (mapc #'notmuch-fcc-handler files)
	 (kill-buffer (current-buffer)))))))

(defun notmuch-fcc-handler (fcc-header)
  "Store message with notmuch insert or normal (file) fcc.

If `notmuch-maildir-use-notmuch-insert' is set then store the
message using notmuch insert. Otherwise store the message using
normal fcc."
  (message "Doing Fcc...")
  (if notmuch-maildir-use-notmuch-insert
      (notmuch-maildir-fcc-with-notmuch-insert fcc-header)
    (notmuch-maildir-fcc-file-fcc fcc-header))
  (message "Doing Fcc...done"))

;;; Functions for saving a message using notmuch insert.

(defun notmuch-maildir-notmuch-insert-current-buffer (folder &optional create tags)
  "Use notmuch insert to put the current buffer in the database.

This inserts the current buffer as a message into the notmuch
database in folder FOLDER. If CREATE is non-nil it will supply
the --create-folder flag to create the folder if necessary. TAGS
should be a list of tag changes to apply to the inserted message."
  (apply 'notmuch-call-notmuch-process
	 :stdin-string (buffer-string) "insert"
	 (append (and create (list "--create-folder"))
		 (list (concat "--folder=" folder))
		 tags)))

(defun notmuch-maildir-fcc-with-notmuch-insert (fcc-header &optional create)
  "Store message with notmuch insert.

The fcc-header should be of the form \"folder +tag1 -tag2\" where
folder is the folder (relative to the notmuch mailstore) to store
the message in, and tag1 and tag2 are tag changes to apply to the
stored message. This string is split using `split-string-and-unquote',
so a folder name containing spaces can be specified by
quoting each space with an immediately preceding backslash
or surrounding the entire folder name in double quotes.

If CREATE is non-nil then create the folder if necessary."
  (pcase-let ((`(,folder . ,tags)
	       (split-string-and-unquote fcc-header)))
    (condition-case nil
	(notmuch-maildir-notmuch-insert-current-buffer folder create tags)
      ;; Since there are many reasons notmuch insert could fail, e.g.,
      ;; locked database, non-existent folder (which could be due to a
      ;; typo, or just the user want a new folder, let the user decide
      ;; how to deal with it.
      (error
       (let ((response (read-char-choice "Insert failed: \
\(r)etry, (c)reate folder, (i)gnore, or (e)dit the header? " '(?r ?c ?i ?e))))
	 (cl-case response
	   (?r (notmuch-maildir-fcc-with-notmuch-insert fcc-header))
	   (?c (notmuch-maildir-fcc-with-notmuch-insert fcc-header t))
	   (?i t)
	   (?e (notmuch-maildir-fcc-with-notmuch-insert
		(read-from-minibuffer "Fcc header: " fcc-header)))))))))

;;; Functions for saving a message using file fcc.

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
	 (hostname (notmuch-maildir-fcc-host-fixer (system-name))))
    (cl-incf notmuch-maildir-fcc-count)
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
	   (error "Can't write to %s. Not a maildir." destdir)))))

(defun notmuch-maildir-fcc-move-tmp-to-new (destdir msg-id)
  (add-name-to-file
   (concat destdir "/tmp/" msg-id)
   (concat destdir "/new/" msg-id ":2,")))

(defun notmuch-maildir-fcc-move-tmp-to-cur (destdir msg-id &optional mark-seen)
  (add-name-to-file
   (concat destdir "/tmp/" msg-id)
   (concat destdir "/cur/" msg-id ":2," (and mark-seen "S"))))

(defun notmuch-maildir-fcc-file-fcc (fcc-header)
  "Write the message to the file specified by FCC-HEADER.

If that fails, then offer the user a chance to correct the header
or filesystem."
  (if (notmuch-maildir-fcc-dir-is-maildir-p fcc-header)
      (notmuch-maildir-fcc-write-buffer-to-maildir fcc-header t)
    ;; The fcc-header is not a valid maildir see if the user wants to
    ;; fix it in some way.
    (let* ((prompt (format "Fcc %s is not a maildir: \
\(r)etry, (c)reate folder, (i)gnore, or (e)dit the header? " fcc-header))
	   (response (read-char-choice prompt '(?r ?c ?i ?e))))
      (cl-case response
	(?r (notmuch-maildir-fcc-file-fcc fcc-header))
	(?c (if (file-writable-p fcc-header)
		(notmuch-maildir-fcc-create-maildir fcc-header)
	      (message "No permission to create %s." fcc-header)
	      (sit-for 2))
	    (notmuch-maildir-fcc-file-fcc fcc-header))
	(?i t)
	(?e (notmuch-maildir-fcc-file-fcc
	     (read-from-minibuffer "Fcc header: " fcc-header)))))))

(defun notmuch-maildir-fcc-write-buffer-to-maildir (destdir &optional mark-seen)
  "Write the current buffer to maildir destdir.

If mark-seen is non-nil, then write it to \"cur/\", and mark it
as read, otherwise write it to \"new/\". Return t if successful,
and nil otherwise."
  (let ((orig-buffer (buffer-name)))
    (with-temp-buffer
      (insert-buffer-substring orig-buffer)
      (catch 'link-error
	(let ((msg-id (notmuch-maildir-fcc-save-buffer-to-tmp destdir)))
	  (when msg-id
	    (condition-case nil
		(if mark-seen
		    (notmuch-maildir-fcc-move-tmp-to-cur destdir msg-id t)
		  (notmuch-maildir-fcc-move-tmp-to-new destdir msg-id))
	      (file-already-exists
	       (throw 'link-error nil))))
	  (delete-file (concat destdir "/tmp/" msg-id))))
      t)))

;;; _

(provide 'notmuch-maildir-fcc)

;;; notmuch-maildir-fcc.el ends here
