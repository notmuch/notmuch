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
  :group 'notmuch
  :type 'hook)

(defcustom notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full
  "Function used to generate a `User-Agent:' string. If this is
`nil' then no `User-Agent:' will be generated."
  :group 'notmuch
  :type 'function
  :options '(notmuch-mua-user-agent-full
	     notmuch-mua-user-agent-notmuch
	     notmuch-mua-user-agent-emacs))

(defcustom notmuch-mua-hidden-headers '("^User-Agent:")
  "Headers that are added to the `message-mode' hidden headers
list."
  :group 'notmuch
  :type '(repeat string))

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
	  (when (not (member header 'message-hidden-headers))
	    (push header message-hidden-headers)))
	notmuch-mua-hidden-headers))

(defun notmuch-mua-reply (query-string)
  (let (headers body)
    ;; This make assumptions about the output of `notmuch reply', but
    ;; really only that the headers come first followed by a blank
    ;; line and then the body.
    (with-temp-buffer
      (call-process notmuch-command nil t nil "reply" query-string)
      (goto-char (point-min))
      (if (re-search-forward "^$" nil t)
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (point-min) (point))
	      (goto-char (point-min))
	      (setq headers (mail-header-extract)))))
      (forward-line 1)
      (setq body (buffer-substring (point) (point-max))))
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
    (insert body))
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

(defun notmuch-mua-mail (&optional to subject other-headers continue
				   switch-function yank-action send-actions)
  (interactive)

  (when notmuch-mua-user-agent-function
    (let ((user-agent (funcall notmuch-mua-user-agent-function)))
      (when (not (string= "" user-agent))
	(push (cons "User-Agent" user-agent) other-headers))))

  (message-mail to subject other-headers continue
		switch-function yank-action send-actions)
  (message-sort-headers)
  (message-hide-headers)
  (set-buffer-modified-p nil)

  (message-goto-to))

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
