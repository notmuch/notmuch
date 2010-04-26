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

;; Commentary:
;;
;; This is the beginning of a solution for storing sent mail in a
;; maildir in emacs message mode, presented because some people might
;; find it useful. It is *not* fully tested, it *may* overwrite files,
;; and any directories you point this at may no longer be there
;; afterwards. Use at your own risk.
;;
;; To use this as the fcc handler for message-mode, put
;; one of the following in your init file:
;;
;; if you want Fcc'd messages to be marked as read:
;;
;;     (setq message-fcc-handler-function
;;          '(lambda (destdir)
;;	     (notmuch-maildir-fcc-write-buffer-to-maildir destdir t)))
;;
;; if you want Fcc'd messages to be marked as new:
;;
;;     (setq message-fcc-handler-function
;;          '(lambda (destdir)
;;	     (notmuch-maildir-fcc-write-buffer-to-maildir destdir nil)))


(defvar notmuch-maildir-fcc-count 0)

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
	  (microseconds (caddr ct))
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
	   (message (format "Can't write to %s. Not a maildir."
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
      (insert-buffer orig-buffer)
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
