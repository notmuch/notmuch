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

(require 'notmuch-lib)
(require 'notmuch-address)

;;

(defcustom notmuch-mua-send-hook '(notmuch-mua-message-send-hook)
  "Hook run before sending messages."
  :type 'hook
  :group 'notmuch-send
  :group 'notmuch-hooks)

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

(defun notmuch-mua-reply (query-string &optional sender reply-all)
  (let (headers
	body
	(args '("reply")))
    (if notmuch-show-process-crypto
	(setq args (append args '("--decrypt"))))
    (if reply-all
	(setq args (append args '("--reply-to=all")))
      (setq args (append args '("--reply-to=sender"))))
    (setq args (append args (list query-string)))
    ;; This make assumptions about the output of `notmuch reply', but
    ;; really only that the headers come first followed by a blank
    ;; line and then the body.
    (with-temp-buffer
      (apply 'call-process (append (list notmuch-command nil (list t t) nil) args))
      (goto-char (point-min))
      (if (re-search-forward "^$" nil t)
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (point-min) (point))
	      (goto-char (point-min))
	      (setq headers (mail-header-extract)))))
      (forward-line 1)
      ;; Original message may contain (malicious) MML tags. We must
      ;; properly quote them in the reply.
      (mml-quote-region (point) (point-max))
      (setq body (buffer-substring (point) (point-max))))
    ;; If sender is non-nil, set the From: header to its value.
    (when sender
      (mail-header-set 'from sender headers))
    (let
	;; Overlay the composition window on that being used to read
	;; the original message.
	((same-window-regexps '("\\*mail .*")))
      (notmuch-mua-mail (mail-header 'to headers)
			(mail-header 'subject headers)
			(message-headers-to-generate headers t '(to subject))))
    ;; insert the message body - but put it in front of the signature
    ;; if one is present
    (goto-char (point-max))
    (if (re-search-backward message-signature-separator nil t)
	  (forward-line -1)
      (goto-char (point-max)))
    (insert body)
    (push-mark))
  (set-buffer-modified-p nil)

  (message-goto-body))

(defun notmuch-mua-forward-message ()
  (message-forward)

  (when notmuch-mua-user-agent-function
    (let ((user-agent (funcall notmuch-mua-user-agent-function)))
      (when (not (string= "" user-agent))
	(message-add-header (format "User-Agent: %s" user-agent)))))
  (message-sort-headers)
  (message-hide-headers)
  (set-buffer-modified-p nil)

  (message-goto-to))

(defun notmuch-mua-mail (&optional to subject other-headers &rest other-args)
  "Invoke the notmuch mail composition window.

OTHER-ARGS are passed through to `message-mail'."
  (interactive)

  (when notmuch-mua-user-agent-function
    (let ((user-agent (funcall notmuch-mua-user-agent-function)))
      (when (not (string= "" user-agent))
	(push (cons "User-Agent" user-agent) other-headers))))

  (unless (mail-header 'from other-headers)
    (push (cons "From" (concat
			(notmuch-user-name) " <" (notmuch-user-primary-email) ">")) other-headers))

  (apply #'message-mail to subject other-headers other-args)
  (message-sort-headers)
  (message-hide-headers)
  (set-buffer-modified-p nil)

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
	   (list (cons 'from (notmuch-mua-prompt-for-sender))))))
    (notmuch-mua-mail nil nil other-headers)))

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
