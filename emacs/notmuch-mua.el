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

(require 'message)
(require 'mm-view)
(require 'format-spec)

(require 'notmuch-lib)
(require 'notmuch-address)

(eval-when-compile (require 'cl))

(declare-function notmuch-show-insert-bodypart "notmuch-show" (msg part depth &optional hide))

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

(defgroup notmuch-reply nil
  "Replying to messages in notmuch"
  :group 'notmuch)

(defcustom notmuch-mua-cite-function 'message-cite-original
  "*Function for citing an original message.
Predefined functions include `message-cite-original' and
`message-cite-original-without-signature'.
Note that these functions use `mail-citation-hook' if that is non-nil."
  :type '(radio (function-item message-cite-original)
		(function-item message-cite-original-without-signature)
		(function-item sc-cite-original)
		(function :tag "Other"))
  :link '(custom-manual "(message)Insertion Variables")
  :group 'notmuch-reply)

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

(defun notmuch-mua-reply-crypto (parts)
  "Add mml sign-encrypt flag if any part of original message is encrypted."
  (loop for part in parts
	if (notmuch-match-content-type (plist-get part :content-type) "multipart/encrypted")
	  do (mml-secure-message-sign-encrypt)
	else if (notmuch-match-content-type (plist-get part :content-type) "multipart/*")
	  do (notmuch-mua-reply-crypto (plist-get part :content))))

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
  ;; We don't want text properties leaking from the show renderer into
  ;; the reply so we use a temp buffer. Also we don't want hooks, such
  ;; as notmuch-wash-*, to be run on the quotable part so we set
  ;; notmuch-show-insert-text/plain-hook to nil.
  (insert (with-temp-buffer
	    (let ((notmuch-show-insert-text/plain-hook nil))
	      ;; Show the part but do not add buttons.
	      (notmuch-show-insert-bodypart message part 0 'no-buttons))
	    (buffer-substring-no-properties (point-min) (point-max)))))

;; There is a bug in emacs 23's message.el that results in a newline
;; not being inserted after the References header, so the next header
;; is concatenated to the end of it. This function fixes the problem,
;; while guarding against the possibility that some current or future
;; version of emacs has the bug fixed.
(defun notmuch-mua-insert-references (original-func header references)
  (funcall original-func header references)
  (unless (bolp) (insert "\n")))

(defun notmuch-mua-reply (query-string &optional sender reply-all)
  (let ((args '("reply" "--format=sexp" "--format-version=1"))
	(process-crypto notmuch-show-process-crypto)
	reply
	original)
    (when process-crypto
      (setq args (append args '("--decrypt"))))

    (if reply-all
	(setq args (append args '("--reply-to=all")))
      (setq args (append args '("--reply-to=sender"))))
    (setq args (append args (list query-string)))

    ;; Get the reply object as SEXP, and parse it into an elisp object.
    (setq reply (apply #'notmuch-call-notmuch-sexp args))

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
      ;; if one is present, and after any other content
      ;; message*setup-hooks may have added to the message body already.
      (save-restriction
	(message-goto-body)
	(narrow-to-region (point) (point-max))
	(goto-char (point-max))
	(if (re-search-backward message-signature-separator nil t)
	    (if message-signature-insert-empty-line
		(forward-line -1))
	  (goto-char (point-max))))

      (let ((from (plist-get original-headers :From))
	    (date (plist-get original-headers :Date))
	    (start (point)))

	;; notmuch-mua-cite-function constructs a citation line based
	;; on the From and Date headers of the original message, which
	;; are assumed to be in the buffer.
	(insert "From: " from "\n")
	(insert "Date: " date "\n\n")

	;; Get the parts of the original message that should be quoted; this includes
	;; all the text parts, except the non-preferred ones in a multipart/alternative.
	(let ((quotable-parts (notmuch-mua-get-quotable-parts (plist-get original :body))))
	  (mapc (apply-partially 'notmuch-mua-insert-quotable-part original) quotable-parts))

	(set-mark (point))
	(goto-char start)
	;; Quote the original message according to the user's configured style.
	(funcall notmuch-mua-cite-function)))

    ;; Crypto processing based crypto content of the original message
    (when process-crypto
      (notmuch-mua-reply-crypto (plist-get original :body))))

  ;; Push mark right before signature, if any.
  (message-goto-signature)
  (unless (eobp)
    (end-of-line -1))
  (push-mark)

  (message-goto-body)
  (set-buffer-modified-p nil))

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

;; Workaround: Running `ido-completing-read' in emacs 23.1, 23.2 and 23.3
;; without some explicit initialization fill freeze the operation.
;; Hence, we advice `ido-completing-read' to ensure required initialization
;; is done.
(if (and (= emacs-major-version 23) (< emacs-minor-version 4))
    (defadvice ido-completing-read (before notmuch-ido-mode-init activate)
      (ido-init-completion-maps)
      (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
      (add-hook 'choose-completion-string-functions
		'ido-choose-completion-string)
      (ad-disable-advice 'ido-completing-read 'before 'notmuch-ido-mode-init)
      (ad-activate 'ido-completing-read)))

(defun notmuch-mua-prompt-for-sender ()
  "Prompt for a sender from the user's configured identities."
  (if notmuch-identities
      (ido-completing-read "Send mail from: " notmuch-identities
			   nil nil nil 'notmuch-mua-sender-history
			   (car notmuch-identities))
    (let* ((name (notmuch-user-name))
	   (addrs (cons (notmuch-user-primary-email)
			(notmuch-user-other-email)))
	   (address
	    (ido-completing-read (concat "Sender address for " name ": ") addrs
				 nil nil nil 'notmuch-mua-sender-history
				 (car addrs))))
      (concat name " <" address ">"))))

(put 'notmuch-mua-new-mail 'notmuch-prefix-doc "... and prompt for sender")
(defun notmuch-mua-new-mail (&optional prompt-for-sender)
  "Compose new mail.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first."
  (interactive "P")
  (let ((other-headers
	 (when (or prompt-for-sender notmuch-always-prompt-for-sender)
	   (list (cons 'From (notmuch-mua-prompt-for-sender))))))
    (notmuch-mua-mail nil nil other-headers nil (notmuch-mua-get-switch-function))))

(defun notmuch-mua-new-forward-message (&optional prompt-for-sender)
  "Invoke the notmuch message forwarding window.

The current buffer must contain an RFC2822 message to forward.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first."
  (let* ((cur (current-buffer))
	 (message-forward-decoded-p nil)
	 (subject (message-make-forward-subject))
	 (other-headers
	  (when (or prompt-for-sender notmuch-always-prompt-for-sender)
	    (list (cons 'From (notmuch-mua-prompt-for-sender))))))
    (notmuch-mua-mail nil subject other-headers nil (notmuch-mua-get-switch-function))
    (message-forward-make-body cur)
    ;; `message-forward-make-body' shows the User-agent header.  Hide
    ;; it again.
    (message-hide-headers)
    (set-buffer-modified-p nil)))

(defun notmuch-mua-new-reply (query-string &optional prompt-for-sender reply-all)
  "Compose a reply to the message identified by QUERY-STRING.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first.  If REPLY-ALL is non-nil, the message
will be addressed to all recipients of the source message."

;; In current emacs (24.3) select-active-regions is set to t by
;; default. The reply insertion code sets the region to the quoted
;; message to make it easy to delete (kill-region or C-w). These two
;; things combine to put the quoted message in the primary selection.
;;
;; This is not what the user wanted and is a privacy risk (accidental
;; pasting of the quoted message). We can avoid some of the problems
;; by let-binding select-active-regions to nil. This fixes if the
;; primary selection was previously in a non-emacs window but not if
;; it was in an emacs window. To avoid the problem in the latter case
;; we deactivate mark.

  (let ((sender
	 (when prompt-for-sender
	   (notmuch-mua-prompt-for-sender)))
	(select-active-regions nil))
    (notmuch-mua-reply query-string sender reply-all)
    (deactivate-mark)))

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
