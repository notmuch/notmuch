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

(require 'message)

(defvar notmuch-maildir-fcc-count 0)

(defcustom notmuch-fcc-dirs nil
 "If set to non-nil, this will cause message mode to file (fcc) your mail in the specified directory, depending on your From address.

 The first entry (a list) is used as a default fallback
 when nothing matches. So in the easiest case notmuch-fcc-dirs is
 just something like ((\"INBOX.Sent\"))

 If you need a more fancy setup, where you want different Outboxes depending
 on your From address, you use something like this:

     (   (\"defaultinbox\")
         (\"Sebastian Spaeth <Sebastian@SSpaeth.de>\" . \"privat\")
         (\"Sebastian Spaeth <SSpaeth@ethz.ch>\" . \"uni\")
     )

 This will constructs a path, concatenating the content of the
 variable 'message-directory' (a message mode variable
 customizable via m-x
 customize-variable<RET>message-directory<RET>) and the second
 part in the alist."
 :require 'notmuch-fcc-initialization
 :group 'notmuch
)

(defun notmuch-fcc-initialization ()
  "If notmuch-fcc-directories is set,
   hook them into the message-fcc-handler-function"
    ;Set up the message-fcc-handler to move mails to the maildir in Fcc
    ;The parameter is hardcoded to mark messages as "seen"
    (setq message-fcc-handler-function
          '(lambda (destdir)
             (notmuch-maildir-fcc-write-buffer-to-maildir destdir t)))
    ;add a hook to actually insert the Fcc header when sending
    ;(preferrably we would use message-header-setup-up, but notmuch-reply
    ; munges headers after that is run, so it won't work for replies within
    ; notmuch)
    (add-hook 'message-send-hook 'notmuch-fcc-header-setup))

(defun notmuch-fcc-header-setup ()
  "Can be added to message-send-hook and will set the FCC header
      based on the values of notmuch-fcc-directories (see the
      variable customization there for examples). It uses the
      first entry as default fallback if no From address
      matches."
  ; only do something if notmuch-fcc-dirs is set
  (if notmuch-fcc-dirs
   (let ((subdir
          (cdr (assoc-string (message-fetch-field "from") notmuch-fcc-dirs t))))
     (if (eq subdir nil) (setq subdir (car (car notmuch-fcc-dirs))))
     (unless (message-fetch-field "fcc")
       (message-add-header (concat "Fcc: "
				   (file-name-as-directory message-directory)
				   subdir)))
     (let ((fcc-header (message-fetch-field "fcc")))
     (unless (notmuch-maildir-fcc-dir-is-maildir-p fcc-header)
       (cond ((not (file-writable-p fcc-header))
	      (error (format "%s is not a maildir, but you don't have permission to create one." fcc-header)))
	     ((y-or-n-p (format "%s is not a maildir. Create it? "
				 fcc-header))
	      (notmuch-maildir-fcc-create-maildir fcc-header))
	     (t
	      (error "Not sending message."))))))))

(defun notmuch-maildir-fcc-host-fixer (hostname)
  (replace-regexp-in-string "/\\|:"
			    '(lambda (s)
                               (cond ((string-equal s "/") "\\057")
                                     ((string-equal s ":") "\\072")
                                     (t s)))
			    hostname
			    t
			    t))

(defun notmuch-maildir-fcc-make-uniq-maildir-id ()
   (let* ((ct (current-time))
	  (timeid (+ (* (car ct) 65536) (cadr ct)))
	  (microseconds (car (cdr (cdr ct))))
	  (hostname (notmuch-maildir-fcc-host-fixer system-name)))
     (setq notmuch-maildir-fcc-count (+ notmuch-maildir-fcc-count 1))
     (format "%d.%d_%d_%d.%s"
	     timeid
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
	 (error "%s is a file. Can't creat maildir." path))
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
