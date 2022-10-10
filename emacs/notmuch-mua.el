;;; notmuch-mua.el --- emacs style mail-user-agent  -*- lexical-binding: t -*-
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
;; along with Notmuch.  If not, see <https://www.gnu.org/licenses/>.
;;
;; Authors: David Edmondson <dme@dme.org>

;;; Code:

(eval-when-compile (require 'subr-x))

(require 'message)
(require 'gmm-utils)
(require 'mm-view)
(require 'format-spec)

(require 'notmuch-lib)
(require 'notmuch-address)
(require 'notmuch-draft)
(require 'notmuch-message)

(declare-function notmuch-show-insert-body "notmuch-show" (msg body depth))
(declare-function notmuch-fcc-header-setup "notmuch-maildir-fcc" ())
(declare-function notmuch-maildir-message-do-fcc "notmuch-maildir-fcc" ())
(declare-function notmuch-draft-postpone "notmuch-draft" ())
(declare-function notmuch-draft-save "notmuch-draft" ())

(defvar notmuch-show-indent-multipart)
(defvar notmuch-show-insert-header-p-function)
(defvar notmuch-show-max-text-part-size)
(defvar notmuch-show-insert-text/plain-hook)

;;; Options

(defcustom notmuch-mua-send-hook nil
  "Hook run before sending messages."
  :type 'hook
  :group 'notmuch-send
  :group 'notmuch-hooks)

(defcustom notmuch-mua-compose-in 'current-window
  "Where to create the mail buffer used to compose a new message.
Possible values are `current-window' (default), `new-window' and
`new-frame'. If set to `current-window', the mail buffer will be
displayed in the current window, so the old buffer will be
restored when the mail buffer is killed. If set to `new-window'
or `new-frame', the mail buffer will be displayed in a new
window/frame that will be destroyed when the buffer is killed.
You may want to customize `message-kill-buffer-on-exit'
accordingly."
  :group 'notmuch-send
  :type '(choice (const :tag "Compose in the current window" current-window)
		 (const :tag "Compose mail in a new window"  new-window)
		 (const :tag "Compose mail in a new frame"   new-frame)))

(defcustom notmuch-mua-user-agent-function nil
  "Function used to generate a `User-Agent:' string.
If this is `nil' then no `User-Agent:' will be generated."
  :type '(choice (const :tag "No user agent string" nil)
		 (const :tag "Full" notmuch-mua-user-agent-full)
		 (const :tag "Notmuch" notmuch-mua-user-agent-notmuch)
		 (const :tag "Emacs" notmuch-mua-user-agent-emacs)
		 (function :tag "Custom user agent function"
			   :value notmuch-mua-user-agent-full))
  :group 'notmuch-send)

(defcustom notmuch-mua-hidden-headers nil
  "Headers that are added to the `message-mode' hidden headers list."
  :type '(repeat string)
  :group 'notmuch-send)

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

(defgroup notmuch-reply nil
  "Replying to messages in notmuch."
  :group 'notmuch)

(defcustom notmuch-mua-cite-function 'message-cite-original
  "Function for citing an original message.

Predefined functions include `message-cite-original' and
`message-cite-original-without-signature'.  Note that these
functions use `mail-citation-hook' if that is non-nil."
  :type '(radio (function-item message-cite-original)
		(function-item message-cite-original-without-signature)
		(function-item sc-cite-original)
		(function :tag "Other"))
  :link '(custom-manual "(message)Insertion Variables")
  :group 'notmuch-reply)

(defcustom notmuch-mua-reply-insert-header-p-function
  'notmuch-show-reply-insert-header-p-never
  "Function to decide which parts get a header when replying.

This function specifies which parts of a mime message with
multiple parts get a header."
  :type '(radio (const :tag "No part headers"
		       notmuch-show-reply-insert-header-p-never)
		(const :tag "All except multipart/* and hidden parts"
		       notmuch-show-reply-insert-header-p-trimmed)
		(const :tag "Only for included text parts"
		       notmuch-show-reply-insert-header-p-minimal)
		(const :tag "Exactly as in show view"
		       notmuch-show-insert-header-p)
		(function :tag "Other"))
  :group 'notmuch-reply)

(defcustom notmuch-mua-attachment-regexp
  "\\b\\(attache\?ment\\|attached\\|attach\\|pi[èe]ce\s+jointe?\\)\\b"
  "Message body text indicating that an attachment is expected.

This is not used unless `notmuch-mua-attachment-check' is added
to `notmuch-mua-send-hook'."
  :type 'regexp
  :group 'notmuch-send)

;;; Various functions

(defun notmuch-mua-attachment-check ()
  "Signal an error an attachement is expected but missing.

Signal an error if the message text indicates that an attachment
is expected but no MML referencing an attachment is found.

Typically this is added to `notmuch-mua-send-hook'."
  (when (and
	 ;; When the message mentions attachment...
	 (save-excursion
	   (message-goto-body)
	   ;; Limit search from reaching other possible parts of the message
	   (let ((search-limit (search-forward "\n<#" nil t)))
	     (message-goto-body)
	     (cl-loop while (re-search-forward notmuch-mua-attachment-regexp
					       search-limit t)
		      ;; For every instance of the "attachment" string
		      ;; found, examine the text properties. If the text
		      ;; has either a `face' or `syntax-table' property
		      ;; then it is quoted text and should *not* cause the
		      ;; user to be asked about a missing attachment.
		      if (let ((props (text-properties-at (match-beginning 0))))
			   (not (or (memq 'syntax-table props)
				    (memq 'face props))))
		      return t
		      finally return nil)))
	 ;; ...but doesn't have a part with a filename...
	 (save-excursion
	   (message-goto-body)
	   (not (re-search-forward "^<#part [^>]*filename=" nil t)))
	 ;; ...and that's not okay...
	 (not (y-or-n-p "Attachment mentioned, but no attachment - is that okay?")))
    ;; ...signal an error.
    (error "Missing attachment")))

(defun notmuch-mua-get-switch-function ()
  "Get a switch function according to `notmuch-mua-compose-in'."
  (pcase notmuch-mua-compose-in
    ('current-window 'switch-to-buffer)
    ('new-window     'switch-to-buffer-other-window)
    ('new-frame      'switch-to-buffer-other-frame)
    (_ (error "Invalid value for `notmuch-mua-compose-in'"))))

(defun notmuch-mua-maybe-set-window-dedicated ()
  "Set the selected window as dedicated according to `notmuch-mua-compose-in'."
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
  (let ((notmuch-version (if (string= notmuch-emacs-version "unknown")
			     (notmuch-cli-version)
			   notmuch-emacs-version)))
    (concat "Notmuch/" notmuch-version " (https://notmuchmail.org)")))

(defun notmuch-mua-user-agent-emacs ()
  "Generate a `User-Agent:' string suitable for notmuch."
  (concat "Emacs/" emacs-version " (" system-configuration ")"))

(defun notmuch-mua-add-more-hidden-headers ()
  "Add some headers to the list that are hidden by default."
  (mapc (lambda (header)
	  (unless (member header message-hidden-headers)
	    (push header message-hidden-headers)))
	notmuch-mua-hidden-headers))

(defun notmuch-mua-reply-crypto (parts)
  "Add mml sign-encrypt flag if any part of original message is encrypted."
  (cl-loop for part in parts
	   for type = (plist-get part :content-type)
	   if (notmuch-match-content-type type "multipart/encrypted")
	   do (mml-secure-message-sign-encrypt)
	   else if (notmuch-match-content-type type "multipart/*")
	   do (notmuch-mua-reply-crypto (plist-get part :content))))

;; There is a bug in Emacs' message.el that results in a newline
;; not being inserted after the References header, so the next header
;; is concatenated to the end of it. This function fixes the problem,
;; while guarding against the possibility that some current or future
;; version of emacs has the bug fixed.
(defun notmuch-mua-insert-references (original-func header references)
  (funcall original-func header references)
  (unless (bolp) (insert "\n")))

;;; Mua reply

(defun notmuch-mua-reply (query-string &optional sender reply-all duplicate)
  (let* ((duparg (and duplicate (list (format "--duplicate=%d" duplicate))))
	 (args `("reply" "--format=sexp" "--format-version=5" ,@duparg))
	 (process-crypto notmuch-show-process-crypto)
	 reply
	 original)
    (when process-crypto
      (setq args (append args '("--decrypt=true"))))
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
	;; We modify message-header-format-alist to get around
	;; a bug in message.el.  See the comment above on
	;; notmuch-mua-insert-references.
	(let ((message-header-format-alist
	       (cl-loop for pair in message-header-format-alist
			if (eq (car pair) 'References)
			collect (cons 'References
				      (apply-partially
				       'notmuch-mua-insert-references
				       (cdr pair)))
			else
			collect pair)))
	  (notmuch-mua-mail (plist-get reply-headers :To)
			    (notmuch-sanitize (plist-get reply-headers :Subject))
			    (notmuch-headers-plist-to-alist reply-headers)
			    nil (notmuch-mua-get-switch-function))))
      ;; Create a buffer-local queue for tag changes triggered when
      ;; sending the reply.
      (when notmuch-message-replied-tags
	(setq notmuch-message-queued-tag-changes
	      (list (cons query-string notmuch-message-replied-tags))))
      ;; Insert the message body - but put it in front of the signature
      ;; if one is present, and after any other content
      ;; message*setup-hooks may have added to the message body already.
      (save-restriction
	(message-goto-body)
	(narrow-to-region (point) (point-max))
	(goto-char (point-max))
	(if (re-search-backward message-signature-separator nil t)
	    (when message-signature-insert-empty-line
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
	(insert
	 (with-temp-buffer
	   (let
	       ;; Don't attempt to clean up messages, excerpt
	       ;; citations, etc. in the original message before
	       ;; quoting.
	       ((notmuch-show-insert-text/plain-hook nil)
		;; Don't omit long parts.
		(notmuch-show-max-text-part-size 0)
		;; Insert headers for parts as appropriate for replying.
		(notmuch-show-insert-header-p-function
		 notmuch-mua-reply-insert-header-p-function)
		;; Ensure that any encrypted parts are
		;; decrypted during the generation of the reply
		;; text.
		(notmuch-show-process-crypto process-crypto)
		;; Don't indent multipart sub-parts.
		(notmuch-show-indent-multipart nil)
		;; Stop certain mime types from being inlined
		(mm-inline-override-types (notmuch--inline-override-types)))
	     ;; We don't want sigstatus buttons (an information leak and usually wrong anyway).
	     (cl-letf (((symbol-function 'notmuch-crypto-insert-sigstatus-button) #'ignore)
		       ((symbol-function 'notmuch-crypto-insert-encstatus-button) #'ignore))
	       (notmuch-show-insert-body original (plist-get original :body) 0)
	       (buffer-substring-no-properties (point-min) (point-max))))))
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

;;; Mode and keymap

(defvar notmuch-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap message-send-and-exit] #'notmuch-mua-send-and-exit)
    (define-key map [remap message-send] #'notmuch-mua-send)
    (define-key map (kbd "C-c C-p") #'notmuch-draft-postpone)
    (define-key map (kbd "C-x C-s") #'notmuch-draft-save)
    map)
  "Keymap for `notmuch-message-mode'.")

(define-derived-mode notmuch-message-mode message-mode "Message[Notmuch]"
  "Notmuch message composition mode. Mostly like `message-mode'."
  (notmuch-address-setup))

(put 'notmuch-message-mode 'flyspell-mode-predicate 'mail-mode-flyspell-verify)

;;; New messages

(defun notmuch-mua-pop-to-buffer (name switch-function)
  "Pop to buffer NAME, and warn if it already exists and is modified.
Like `message-pop-to-buffer' but enable `notmuch-message-mode'
instead of `message-mode' and SWITCH-FUNCTION is mandatory."
  (let ((buffer (get-buffer name)))
    (if (and buffer
	     (buffer-name buffer))
	(let ((window (get-buffer-window buffer 0)))
	  (if window
	      ;; Raise the frame already displaying the message buffer.
	      (progn
		(select-frame-set-input-focus (window-frame window))
		(select-window window))
	    (funcall switch-function buffer)
	    (set-buffer buffer))
	  (when (buffer-modified-p)
	    (if (y-or-n-p "Message already being composed; erase? ")
		(message nil)
	      (error "Message being composed"))))
      (funcall switch-function name)
      (set-buffer name))
    (erase-buffer)
    (notmuch-message-mode)))

(defun notmuch-mua--remove-dont-reply-to-names ()
  (when-let* ((nr (if (functionp message-dont-reply-to-names)
		      message-dont-reply-to-names
		    (gmm-regexp-concat message-dont-reply-to-names)))
	      (nr-filter
	       (if (functionp nr)
		   (lambda (mail) (and (not (funcall nr mail)) mail))
		 (lambda (mail) (and (not (string-match-p nr mail)) mail)))))
    (dolist (header '("To" "Cc"))
      (when-let ((v (message-fetch-field header)))
	(let* ((tokens (mapcar #'string-trim (message-tokenize-header v)))
	       (good-tokens (delq nil (mapcar nr-filter tokens)))
	       (addr (and good-tokens (mapconcat #'identity good-tokens ", "))))
	  (message-replace-header header addr))))))

(defun notmuch-mua-mail (&optional to subject other-headers _continue
				   switch-function yank-action send-actions
				   return-action &rest ignored)
  "Invoke the notmuch mail composition window.

The position of point when the function returns differs depending
on the values of TO and SUBJECT.  If both are non-nil, point is
moved to the message's body.  If SUBJECT is nil but TO isn't,
point is moved to the \"Subject:\" header.  Otherwise, point is
moved to the \"To:\" header."
  (interactive)
  (when notmuch-mua-user-agent-function
    (let ((user-agent (funcall notmuch-mua-user-agent-function)))
      (unless (string-empty-p user-agent)
	(push (cons 'User-Agent user-agent) other-headers))))
  (unless (assq 'From other-headers)
    (push (cons 'From (message-make-from
		       (notmuch-user-name)
		       (notmuch-user-primary-email)))
	  other-headers))
  (notmuch-mua-pop-to-buffer (message-buffer-name "mail" to)
			     (or switch-function
				 (notmuch-mua-get-switch-function)))
  (let ((headers
	 (append
	  ;; The following is copied from `message-mail'
	  `((To . ,(or to "")) (Subject . ,(or subject "")))
	  ;; C-h f compose-mail says that headers should be specified as
	  ;; (string . value); however all the rest of message expects
	  ;; headers to be symbols, not strings (eg message-header-format-alist).
	  ;; https://lists.gnu.org/archive/html/emacs-devel/2011-01/msg00337.html
	  ;; We need to convert any string input, eg from rmail-start-mail.
	  (dolist (h other-headers other-headers)
	    (when (stringp (car h))
	      (setcar h (intern (capitalize (car h))))))))
	;; Cause `message-setup-1' to do things relevant for mail,
	;; such as observe `message-default-mail-headers'.
	(message-this-is-mail t))
    (message-setup-1 headers yank-action send-actions return-action))
  (notmuch-fcc-header-setup)
  (notmuch-mua--remove-dont-reply-to-names)
  (message-sort-headers)
  (message-hide-headers)
  (set-buffer-modified-p nil)
  (notmuch-mua-maybe-set-window-dedicated)
  (cond
   ((and to subject) (message-goto-body))
   (to (message-goto-subject))
   (t (message-goto-to))))

(defvar notmuch-mua-sender-history nil)

(defun notmuch-mua-prompt-for-sender ()
  "Prompt for a sender from the user's configured identities."
  (if notmuch-identities
      (completing-read "Send mail from: " notmuch-identities
		       nil nil nil 'notmuch-mua-sender-history
		       (car notmuch-identities))
    (let* ((name (notmuch-user-name))
	   (addrs (cons (notmuch-user-primary-email)
			(notmuch-user-other-email)))
	   (address
	    (completing-read (concat "Sender address for " name ": ") addrs
			     nil nil nil 'notmuch-mua-sender-history
			     (car addrs))))
      (message-make-from name address))))

(put 'notmuch-mua-new-mail 'notmuch-prefix-doc "... and prompt for sender")
(defun notmuch-mua-new-mail (&optional prompt-for-sender)
  "Compose new mail.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first."
  (interactive "P")
  (let ((other-headers
	 (and (or prompt-for-sender notmuch-always-prompt-for-sender)
	      (list (cons 'From (notmuch-mua-prompt-for-sender))))))
    (notmuch-mua-mail nil nil other-headers nil (notmuch-mua-get-switch-function))))

(defun notmuch-mua-new-forward-messages (messages &optional prompt-for-sender)
  "Compose a new message forwarding MESSAGES.

If PROMPT-FOR-SENDER is non-nil, the user will be prompteed for
the From: address."
  (let* ((other-headers
	  (and (or prompt-for-sender notmuch-always-prompt-for-sender)
	       (list (cons 'From (notmuch-mua-prompt-for-sender)))))
	 ;; Comes from the first message and is applied later.
	 forward-subject
	 ;; List of accumulated message-references of forwarded messages.
	 forward-references
	 ;; List of corresponding message-query.
	 forward-queries)
    ;; Generate the template for the outgoing message.
    (notmuch-mua-mail nil "" other-headers nil (notmuch-mua-get-switch-function))
    (save-excursion
      ;; Insert all of the forwarded messages.
      (mapc (lambda (id)
	      (let ((temp-buffer (get-buffer-create
				  (concat "*notmuch-fwd-raw-" id "*"))))
		;; Get the raw version of this message in the buffer.
		(with-current-buffer temp-buffer
		  (erase-buffer)
		  (let ((coding-system-for-read 'no-conversion))
		    (notmuch--call-process notmuch-command nil t nil
				  "show" "--format=raw" id))
		  ;; Because we process the messages in reverse order,
		  ;; always generate a forwarded subject, then use the
		  ;; last (i.e. first) one.
		  (setq forward-subject (message-make-forward-subject))
		  (push (message-fetch-field "Message-ID") forward-references)
		  (push id forward-queries))
		;; Make a copy ready to be forwarded in the
		;; composition buffer.
		(message-forward-make-body temp-buffer)
		;; Kill the temporary buffer.
		(kill-buffer temp-buffer)))
	    ;; `message-forward-make-body' always puts the message at
	    ;; the top, so do them in reverse order.
	    (reverse messages))
      ;; Add in the appropriate subject.
      (save-restriction
	(message-narrow-to-headers)
	(message-remove-header "Subject")
	(message-add-header (concat "Subject: " forward-subject))
	(message-remove-header "References")
	(message-add-header (concat "References: "
				    (mapconcat 'identity forward-references " "))))
      ;; Create a buffer-local queue for tag changes triggered when
      ;; sending the message.
      (when notmuch-message-forwarded-tags
	(setq notmuch-message-queued-tag-changes
	      (cl-loop for id in forward-queries
		       collect
		       (cons id notmuch-message-forwarded-tags))))
      ;; `message-forward-make-body' shows the User-agent header.  Hide
      ;; it again.
      (message-hide-headers)
      (set-buffer-modified-p nil))))

(defun notmuch-mua-new-reply (query-string &optional prompt-for-sender reply-all duplicate)
  "Compose a reply to the message identified by QUERY-STRING.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first.  If REPLY-ALL is non-nil, the message
will be addressed to all recipients of the source message.  If
DUPLICATE is non-nil, based the reply on that duplicate file"
  ;; `select-active-regions' is t by default. The reply insertion code
  ;; sets the region to the quoted message to make it easy to delete
  ;; (kill-region or C-w). These two things combine to put the quoted
  ;; message in the primary selection.
  ;;
  ;; This is not what the user wanted and is a privacy risk (accidental
  ;; pasting of the quoted message). We can avoid some of the problems
  ;; by let-binding select-active-regions to nil. This fixes if the
  ;; primary selection was previously in a non-emacs window but not if
  ;; it was in an emacs window. To avoid the problem in the latter case
  ;; we deactivate mark.
  (let ((sender (and prompt-for-sender
		     (notmuch-mua-prompt-for-sender)))
	(select-active-regions nil))
    (notmuch-mua-reply query-string sender reply-all duplicate)
    (deactivate-mark)))

;;; Checks

(defun notmuch-mua-check-no-misplaced-secure-tag ()
  "Query user if there is a misplaced secure mml tag.

Emacs message-send will (probably) ignore a secure mml tag unless
it is at the start of the body. Returns t if there is no such
tag, or the user confirms they mean it."
  (save-excursion
    (let ((body-start (progn (message-goto-body) (point))))
      (goto-char (point-max))
      (or
       ;; We are always fine if there is no secure tag.
       (not (search-backward "<#secure" nil t))
       ;; There is a secure tag, so it must be at the start of the
       ;; body, with no secure tag earlier (i.e., in the headers).
       (and (= (point) body-start)
	    (not (search-backward "<#secure" nil t)))
       ;; The user confirms they means it.
       (yes-or-no-p "\
There is a <#secure> tag not at the start of the body. It is
likely that the message will be sent unsigned and unencrypted.
Really send? ")))))

(defun notmuch-mua-check-secure-tag-has-newline ()
  "Query if the secure mml tag has a newline following it.

Emacs message-send will (probably) ignore a correctly placed
secure mml tag unless it is followed by a newline. Returns t if
any secure tag is followed by a newline, or the user confirms
they mean it."
  (save-excursion
    (message-goto-body)
    (or
     ;; There is no (correctly placed) secure tag.
     (not (looking-at "<#secure"))
     ;; The secure tag is followed by a newline.
     (looking-at "<#secure[^\n>]*>\n")
     ;; The user confirms they means it.
     (yes-or-no-p "\
The <#secure> tag at the start of the body is not followed by a
newline. It is likely that the message will be sent unsigned and
unencrypted.  Really send? "))))

;;; Finishing commands

(defun notmuch-mua-send-common (arg &optional exit)
  (interactive "P")
  (run-hooks 'notmuch-mua-send-hook)
  (when (and (notmuch-mua-check-no-misplaced-secure-tag)
	     (notmuch-mua-check-secure-tag-has-newline))
    (cl-letf (((symbol-function 'message-do-fcc)
	       #'notmuch-maildir-message-do-fcc))
      (if exit
	  (message-send-and-exit arg)
	(message-send arg)))))

(defun notmuch-mua-send-and-exit (&optional arg)
  (interactive "P")
  (notmuch-mua-send-common arg t))

(defun notmuch-mua-send (&optional arg)
  (interactive "P")
  (notmuch-mua-send-common arg))

(defun notmuch-mua-kill-buffer ()
  (interactive)
  (message-kill-buffer))

;;; _

(define-mail-user-agent 'notmuch-user-agent
  'notmuch-mua-mail
  'notmuch-mua-send-and-exit
  'notmuch-mua-kill-buffer
  'notmuch-mua-send-hook)

;; Add some more headers to the list that `message-mode' hides when
;; composing a message.
(notmuch-mua-add-more-hidden-headers)

(provide 'notmuch-mua)

;;; notmuch-mua.el ends here
