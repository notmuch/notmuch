;;; notmuch-crypto.el --- functions for handling display of cryptographic metadata.
;;
;; Copyright © Jameson Rollins
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
;; Authors: Jameson Rollins <jrollins@finestructure.net>

;;; Code:

(require 'epg)
(require 'notmuch-lib)

(defcustom notmuch-crypto-process-mime t
  "Should cryptographic MIME parts be processed?

If this variable is non-nil signatures in multipart/signed
messages will be verified and multipart/encrypted parts will be
decrypted.  The result of the crypto operation will be displayed
in a specially colored header button at the top of the processed
part.  Signed parts will have variously colored headers depending
on the success or failure of the verification process and on the
validity of user ID of the signer.

The effect of setting this variable can be seen temporarily by
providing a prefix when viewing a signed or encrypted message, or
by providing a prefix when reloading the message in notmuch-show
mode."
  :type 'boolean
  :package-version '(notmuch . "0.25")
  :group 'notmuch-crypto)

(defcustom notmuch-crypto-get-keys-asynchronously t
  "Retrieve gpg keys asynchronously."
  :type 'boolean
  :group 'notmuch-crypto)

(defcustom notmuch-crypto-gpg-program epg-gpg-program
  "The gpg executable."
  :type 'string
  :group 'notmuch-crypto)

(defface notmuch-crypto-part-header
  '((((class color)
      (background dark))
     (:foreground "LightBlue1"))
    (((class color)
      (background light))
     (:foreground "blue")))
  "Face used for crypto parts headers."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-signature-good
  '((t (:background "green" :foreground "black")))
  "Face used for good signatures."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-signature-good-key
  '((t (:background "orange" :foreground "black")))
  "Face used for good signatures."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-signature-bad
  '((t (:background "red" :foreground "black")))
  "Face used for bad signatures."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-signature-unknown
  '((t (:background "red" :foreground "black")))
  "Face used for signatures of unknown status."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-decryption
  '((t (:background "purple" :foreground "black")))
  "Face used for encryption/decryption status messages."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(define-button-type 'notmuch-crypto-status-button-type
  'action (lambda (button) (message (button-get button 'help-echo)))
  'follow-link t
  'help-echo "Set notmuch-crypto-process-mime to process cryptographic mime parts."
  :supertype 'notmuch-button-type)

(defun notmuch-crypto-insert-sigstatus-button (sigstatus from)
  "Insert a button describing the signature status SIGSTATUS sent
by user FROM."
  (let* ((status (plist-get sigstatus :status))
	 (show-button t)
	 (face 'notmuch-crypto-signature-unknown)
	 (button-action (lambda (button) (message (button-get button 'help-echo))))
	 (keyid (concat "0x" (plist-get sigstatus :keyid)))
	 label help-msg)
    (cond
     ((string= status "good")
      (let ((fingerprint (concat "0x" (plist-get sigstatus :fingerprint)))
	    (userid (plist-get sigstatus :userid)))
	;; If userid is present it has full or greater validity.
	(if userid
	    (setq label (concat "Good signature by: " userid)
		  face 'notmuch-crypto-signature-good)
	  (setq label (concat "Good signature by key: " fingerprint)
		face 'notmuch-crypto-signature-good-key))
	(setq button-action 'notmuch-crypto-sigstatus-good-callback
	      help-msg (concat "Click to list key ID 0x" fingerprint "."))))

     ((string= status "error")
      (setq label (concat "Unknown key ID " keyid " or unsupported algorithm")
	    button-action 'notmuch-crypto-sigstatus-error-callback
	    help-msg (concat "Click to retrieve key ID " keyid
			     " from keyserver.")))

     ((string= status "bad")
      (setq label (concat "Bad signature (claimed key ID " keyid ")")
	    face 'notmuch-crypto-signature-bad))

     (status
      (setq label (concat "Unknown signature status: " status)))
     (t
      (setq show-button nil)))
    (when show-button
      (insert-button
       (concat "[ " label " ]")
       :type 'notmuch-crypto-status-button-type
       'help-echo help-msg
       'face face
       'mouse-face face
       'action button-action
       :notmuch-sigstatus sigstatus
       :notmuch-from from)
      (insert "\n"))))

(defun notmuch-crypto-sigstatus-good-callback (button)
  (let* ((id (notmuch-show-get-message-id))
	 (sigstatus (button-get button :notmuch-sigstatus))
	 (fingerprint (concat "0x" (plist-get sigstatus :fingerprint)))
	 (buffer (get-buffer-create "*notmuch-crypto-gpg-out*"))
	 (window (display-buffer buffer)))
    (with-selected-window window
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert (format "-- Key %s in message %s:\n"
			fingerprint id))
	(call-process notmuch-crypto-gpg-program nil t t "--batch" "--no-tty" "--list-keys" fingerprint))
      (recenter -1))))

(declare-function notmuch-show-refresh-view "notmuch-show" (&optional reset-state))
(declare-function notmuch-show-get-message-id "notmuch-show" (&optional bare))

(defun notmuch-crypto--async-key-sentinel (process event)
  "When the user asks for a GPG key to be retrieved
asynchronously, handle completion of that task.

If the retrieval is successful, the thread where the retrieval
was initiated is still displayed and the cursor has not moved,
redisplay the thread."
  (let ((status (process-status process))
	(exit-status (process-exit-status process))
	(keyid (process-get process :gpg-key-id)))
    (when (memq status '(exit signal))
      (message "Getting the GPG key %s asynchronously...%s."
	       keyid
	       (if (= exit-status 0)
		   "completed"
		 "failed"))
      ;; If the original buffer is still alive and point didn't move
      ;; (i.e. the user didn't move on or away), refresh the buffer to
      ;; show the updated signature status.
      (let ((show-buffer (process-get process :notmuch-show-buffer))
	    (show-point (process-get process :notmuch-show-point)))
	(when (and (bufferp show-buffer)
		   (buffer-live-p show-buffer)
		   (= show-point
		      (with-current-buffer show-buffer
			(point))))
	  (with-current-buffer show-buffer
	    (notmuch-show-refresh-view)))))))

(defun notmuch-crypto--set-button-label (button label)
  "Set the text displayed in BUTTON to LABEL."
  (save-excursion
    (let ((inhibit-read-only t))
      ;; This knows rather too much about how we typically format
      ;; buttons.
      (goto-char (button-start button))
      (forward-char 2)
      (delete-region (point) (- (button-end button) 2))
      (insert label))))

(defun notmuch-crypto-sigstatus-error-callback (button)
  "When signature validation has failed, try to retrieve the
corresponding key when the status button is pressed."
  (let* ((sigstatus (button-get button :notmuch-sigstatus))
	 (keyid (concat "0x" (plist-get sigstatus :keyid)))
	 (buffer (get-buffer-create "*notmuch-crypto-gpg-out*")))
    (if notmuch-crypto-get-keys-asynchronously
	(progn
	  (notmuch-crypto--set-button-label
	   button (format "Retrieving key %s asynchronously..." keyid))
	  (with-current-buffer buffer
	    (goto-char (point-max))
	    (insert (format "--- Retrieving key %s:\n" keyid)))
	  (let ((p (make-process :name "notmuch GPG key retrieval"
				 :connection-type 'pipe
				 :buffer buffer
				 :stderr buffer
				 :command (list notmuch-crypto-gpg-program "--recv-keys" keyid)
				 :sentinel #'notmuch-crypto--async-key-sentinel)))
	    (process-put p :gpg-key-id keyid)
	    (process-put p :notmuch-show-buffer (current-buffer))
	    (process-put p :notmuch-show-point (point))
	    (message "Getting the GPG key %s asynchronously..." keyid)))

      (let ((window (display-buffer buffer)))
	(with-selected-window window
	  (with-current-buffer buffer
	    (goto-char (point-max))
	    (insert (format "--- Retrieving key %s:\n" keyid))
	    (call-process notmuch-crypto-gpg-program nil t t "--recv-keys" keyid)
	    (insert "\n")
	    (call-process notmuch-crypto-gpg-program nil t t "--list-keys" keyid))
	  (recenter -1))
	(notmuch-show-refresh-view)))))

(defun notmuch-crypto-insert-encstatus-button (encstatus)
  "Insert a button describing the encryption status ENCSTATUS."
  (insert-button
   (concat "[ "
	   (let ((status (plist-get encstatus :status)))
	     (cond
	      ((string= status "good")
	       "Decryption successful")
	      ((string= status "bad")
	       "Decryption error")
	      (t
	       (concat "Unknown encryption status"
		       (if status (concat ": " status))))))
	   " ]")
   :type 'notmuch-crypto-status-button-type
   'face 'notmuch-crypto-decryption
   'mouse-face 'notmuch-crypto-decryption)
  (insert "\n"))

;;

(provide 'notmuch-crypto)

;;; notmuch-crypto.el ends here
