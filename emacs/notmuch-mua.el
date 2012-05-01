;; notmuch-mua.el --- emacs style mail-user-agent
;;
;; Copyright © David Edmondson
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
;; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Authors: David Edmondson <dme@dme.org>

(require 'json)
(require 'message)
(require 'mm-view)
(require 'format-spec)

(require 'notmuch-lib)
(require 'notmuch-address)

(eval-when-compile (require 'cl))

;;

(defcustom notmuch-mua-send-hook '(notmuch-mua-message-send-hook)
  "Hook run before sending messages."
  :type 'hook
  :group 'notmuch-send
  :group 'notmuch-hooks)

(defcustom notmuch-mua-compose-in 'current-window
  (concat
   "Where to create the mail buffer used to compose a new message.
Possible values are `current-window' (default), `new-window' and
`new-frame'. If set to `current-window', the mail buffer will be
displayed in the current window, so the old buffer will be
restored when the mail buffer is killed. If set to `new-window'
or `new-frame', the mail buffer will be displayed in a new
window/frame that will be destroyed when the buffer is killed.
You may want to customize `message-kill-buffer-on-exit'
accordingly."
   (when (< emacs-major-version 24)
           " Due to a known bug in Emacs 23, you should not set
this to `new-window' if `message-kill-buffer-on-exit' is
disabled: this would result in an incorrect behavior."))
  :group 'notmuch-send
  :type '(choice (const :tag "Compose in the current window" current-window)
		 (const :tag "Compose mail in a new window"  new-window)
		 (const :tag "Compose mail in a new frame"   new-frame)))

(defcustom notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full
  "Function used to generate a `User-Agent:' string. If this is
`nil' then no `User-Agent:' will be generated."
  :type '(choice (const :tag "No user agent string" nil)
		 (const :tag "Full" notmuch-mua-user-agent-full)
		 (const :tag "Notmuch" notmuch-mua-user-agent-notmuch)
		 (const :tag "Emacs" notmuch-mua-user-agent-emacs)
		 (function :tag "Custom user agent function"
			   :value notmuch-mua-user-agent-full))
  :group 'notmuch-send)

(defcustom notmuch-mua-hidden-headers '("^User-Agent:")
  "Headers that are added to the `message-mode' hidden headers
list."
  :type '(repeat string)
  :group 'notmuch-send)

;;

(defun notmuch-mua-get-switch-function ()
  "Get a switch function according to `notmuch-mua-compose-in'."
  (cond ((eq notmuch-mua-compose-in 'current-window)
	 'switch-to-buffer)
	((eq notmuch-mua-compose-in 'new-window)
	 'switch-to-buffer-other-window)
	((eq notmuch-mua-compose-in 'new-frame)
	 'switch-to-buffer-other-frame)
	(t (error "Invalid value for `notmuch-mua-compose-in'"))))

(defun notmuch-mua-maybe-set-window-dedicated ()
  "Set the selected window as dedicated according to
`notmuch-mua-compose-in'."
  (when (or (eq notmuch-mua-compose-in 'new-frame)
	    (eq notmuch-mua-compose-in 'new-window))
    (set-window-dedicated-p (selected-window) t)))

(defun notmuch-mua-user-agent-full ()
  "Generate a `User-Agent:' string suitable for notmuch."
  (concat (notmuch-mua-user-agent-notmuch)
	  " "
	  (notmuch-mua-user-agent-emacs)))

(defun notmuch-mua-user-agent-notmuch ()
  "Generate a `User-Agent:' string suitable for notmuch."
  (concat "Notmuch/" (notmuch-version) " (http://notmuchmail.org)"))

(defun notmuch-mua-user-agent-emacs ()
  "Generate a `User-Agent:' string suitable for notmuch."
  (concat "Emacs/" emacs-version " (" system-configuration ")"))

(defun notmuch-mua-add-more-hidden-headers ()
  "Add some headers to the list that are hidden by default."
  (mapc (lambda (header)
	  (when (not (member header message-hidden-headers))
	    (push header message-hidden-headers)))
	notmuch-mua-hidden-headers))

(defun notmuch-mua-get-quotable-parts (parts)
  (loop for part in parts
	if (notmuch-match-content-type (plist-get part :content-type) "multipart/alternative")
	  collect (let* ((subparts (plist-get part :content))
			(types (mapcar (lambda (part) (plist-get part :content-type)) subparts))
			(chosen-type (car (notmuch-multipart/alternative-choose types))))
		   (loop for part in (reverse subparts)
			 if (notmuch-match-content-type (plist-get part :content-type) chosen-type)
			 return part))
	else if (notmuch-match-content-type (plist-get part :content-type) "multipart/*")
	  append (notmuch-mua-get-quotable-parts (plist-get part :content))
	else if (notmuch-match-content-type (plist-get part :content-type) "text/*")
	  collect part))

(defun notmuch-mua-insert-quotable-part (message part)
  (save-restriction
    (narrow-to-region (point) (point))
    (notmuch-mm-display-part-inline message part (plist-get part :id)
				    (plist-get part :content-type)
				    notmuch-show-process-crypto)
    (goto-char (point-max))))

;; There is a bug in emacs 23's message.el that results in a newline
;; not being inserted after the References header, so the next header
;; is concatenated to the end of it. This function fixes the problem,
;; while guarding against the possibility that some current or future
;; version of emacs has the bug fixed.
(defun notmuch-mua-insert-references (original-func header references)
  (funcall original-func header references)
  (unless (bolp) (insert "\n")))

(defun notmuch-mua-reply (query-string &optional sender reply-all)
  (let ((args '("reply" "--format=json"))
	reply
	original)
    (when notmuch-show-process-crypto
      (setq args (append args '("--decrypt"))))

    (if reply-all
	(setq args (append args '("--reply-to=all")))
      (setq args (append args '("--reply-to=sender"))))
    (setq args (append args (list query-string)))

    ;; Get the reply object as JSON, and parse it into an elisp object.
    (with-temp-buffer
      (apply 'call-process (append (list notmuch-command nil (list t nil) nil) args))
      (goto-char (point-min))
      (let ((json-object-type 'plist)
	    (json-array-type 'list)
	    (json-false 'nil))
	(setq reply (json-read))))

    ;; Extract the original message to simplify the following code.
    (setq original (plist-get reply :original))

    ;; Extract the headers of both the reply and the original message.
    (let* ((original-headers (plist-get original :headers))
	   (reply-headers (plist-get reply :reply-headers)))

      ;; If sender is non-nil, set the From: header to its value.
      (when sender
	(plist-put reply-headers :From sender))
      (let
	  ;; Overlay the composition window on that being used to read
	  ;; the original message.
	  ((same-window-regexps '("\\*mail .*")))

	;; We modify message-header-format-alist to get around a bug in message.el.
	;; See the comment above on notmuch-mua-insert-references.
	(let ((message-header-format-alist
	       (loop for pair in message-header-format-alist
		     if (eq (car pair) 'References)
		     collect (cons 'References
				   (apply-partially
				    'notmuch-mua-insert-references
				    (cdr pair)))
		     else
		     collect pair)))
	  (notmuch-mua-mail (plist-get reply-headers :To)
			    (plist-get reply-headers :Subject)
			    (notmuch-headers-plist-to-alist reply-headers)
			    nil (notmuch-mua-get-switch-function))))

      ;; Insert the message body - but put it in front of the signature
      ;; if one is present
      (goto-char (point-max))
      (if (re-search-backward message-signature-separator nil t)
	  (forward-line -1)
	(goto-char (point-max)))

      (let ((from (plist-get original-headers :From))
	    (date (plist-get original-headers :Date))
	    (start (point)))

	;; message-cite-original constructs a citation line based on the From and Date
	;; headers of the original message, which are assumed to be in the buffer.
	(insert "From: " from "\n")
	(insert "Date: " date "\n\n")

	;; Get the parts of the original message that should be quoted; this includes
	;; all the text parts, except the non-preferred ones in a multipart/alternative.
	(let ((quotable-parts (notmuch-mua-get-quotable-parts (plist-get original :body))))
	  (mapc (apply-partially 'notmuch-mua-insert-quotable-part original) quotable-parts))

	(set-mark (point))
	(goto-char start)
	;; Quote the original message according to the user's configured style.
	(message-cite-original))))

  (goto-char (point-max))
  (push-mark)
  (message-goto-body)
  (set-buffer-modified-p nil))

(defun notmuch-mua-forward-message ()
  (funcall (notmuch-mua-get-switch-function) (current-buffer))
  (message-forward)

  (when notmuch-mua-user-agent-function
    (let ((user-agent (funcall notmuch-mua-user-agent-function)))
      (when (not (string= "" user-agent))
	(message-add-header (format "User-Agent: %s" user-agent)))))
  (message-sort-headers)
  (message-hide-headers)
  (set-buffer-modified-p nil)
  (notmuch-mua-maybe-set-window-dedicated)

  (message-goto-to))

(defun notmuch-mua-mail (&optional to subject other-headers &rest other-args)
  "Invoke the notmuch mail composition window.

OTHER-ARGS are passed through to `message-mail'."
  (interactive)

  (when notmuch-mua-user-agent-function
    (let ((user-agent (funcall notmuch-mua-user-agent-function)))
      (when (not (string= "" user-agent))
	(push (cons 'User-Agent user-agent) other-headers))))

  (unless (assq 'From other-headers)
    (push (cons 'From (concat
		       (notmuch-user-name) " <" (notmuch-user-primary-email) ">")) other-headers))

  (apply #'message-mail to subject other-headers other-args)
  (message-sort-headers)
  (message-hide-headers)
  (set-buffer-modified-p nil)
  (notmuch-mua-maybe-set-window-dedicated)

  (message-goto-to))

(defcustom notmuch-identities nil
  "Identities that can be used as the From: address when composing a new message.

If this variable is left unset, then a list will be constructed from the
name and addresses configured in the notmuch configuration file."
  :type '(repeat string)
  :group 'notmuch-send)

(defcustom notmuch-always-prompt-for-sender nil
  "Always prompt for the From: address when composing or forwarding a message.

This is not taken into account when replying to a message, because in that case
the From: header is already filled in by notmuch."
  :type 'boolean
  :group 'notmuch-send)

(defvar notmuch-mua-sender-history nil)

(defun notmuch-mua-prompt-for-sender ()
  (interactive)
  (let (name addresses one-name-only)
    ;; If notmuch-identities is non-nil, check if there is a fixed user name.
    (if notmuch-identities
	(let ((components (mapcar 'mail-extract-address-components notmuch-identities)))
	  (setq name          (caar components)
		addresses     (mapcar 'cadr components)
		one-name-only (eval
			       (cons 'and
				     (mapcar (lambda (identity)
					       (string-equal name (car identity)))
					     components)))))
      ;; If notmuch-identities is nil, use values from the notmuch configuration file.
      (setq name          (notmuch-user-name)
	    addresses     (cons (notmuch-user-primary-email) (notmuch-user-other-email))
	    one-name-only t))
    ;; Now prompt the user, either for an email address only or for a full identity.
    (if one-name-only
	(let ((address
	       (ido-completing-read (concat "Sender address for " name ": ") addresses
				    nil nil nil 'notmuch-mua-sender-history (car addresses))))
	  (concat name " <" address ">"))
      (ido-completing-read "Send mail From: " notmuch-identities
			   nil nil nil 'notmuch-mua-sender-history (car notmuch-identities)))))

(defun notmuch-mua-new-mail (&optional prompt-for-sender)
  "Invoke the notmuch mail composition window.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first."
  (interactive "P")
  (let ((other-headers
	 (when (or prompt-for-sender notmuch-always-prompt-for-sender)
	   (list (cons 'From (notmuch-mua-prompt-for-sender))))))
    (notmuch-mua-mail nil nil other-headers nil (notmuch-mua-get-switch-function))))

(defun notmuch-mua-new-forward-message (&optional prompt-for-sender)
  "Invoke the notmuch message forwarding window.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first."
  (interactive "P")
  (if (or prompt-for-sender notmuch-always-prompt-for-sender)
      (let* ((sender (notmuch-mua-prompt-for-sender))
	     (address-components (mail-extract-address-components sender))
	     (user-full-name (car address-components))
	     (user-mail-address (cadr address-components)))
	(notmuch-mua-forward-message))
    (notmuch-mua-forward-message)))

(defun notmuch-mua-new-reply (query-string &optional prompt-for-sender reply-all)
  "Invoke the notmuch reply window."
  (interactive "P")
  (let ((sender
	 (when prompt-for-sender
	   (notmuch-mua-prompt-for-sender))))
    (notmuch-mua-reply query-string sender reply-all)))

(defun notmuch-mua-send-and-exit (&optional arg)
  (interactive "P")
  (message-send-and-exit arg))

(defun notmuch-mua-kill-buffer ()
  (interactive)
  (message-kill-buffer))

(defun notmuch-mua-message-send-hook ()
  "The default function used for `notmuch-mua-send-hook', this
simply runs the corresponding `message-mode' hook functions."
  (run-hooks 'message-send-hook))

;;

(define-mail-user-agent 'notmuch-user-agent
  'notmuch-mua-mail 'notmuch-mua-send-and-exit
  'notmuch-mua-kill-buffer 'notmuch-mua-send-hook)

;; Add some more headers to the list that `message-mode' hides when
;; composing a message.
(notmuch-mua-add-more-hidden-headers)

;;

(provide 'notmuch-mua)
