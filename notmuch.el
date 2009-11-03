; notmuch.el --- run notmuch within emacs
;
; Copyright © Carl Worth
;
; This file is part of Notmuch.
;
; Notmuch is free software: you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Notmuch is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Carl Worth <cworth@cworth.org>
;
; Much of notmuch.el was written by looking at the implementation of
; compile.el from the emacs distribution source which has the
; following copyright and authorsip (and the identical license as
; above):
;
; Copyright (C) 1985, 1986, 1987, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;   2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;   Free Software Foundation, Inc.

; Authors: Roland McGrath <roland@gnu.org>,
;	    Daniel Pfeiffer <occitan@esperanto.org>

(defvar notmuch-show-mode-map
  (let ((map (make-sparse-keymap)))
    ; I don't actually want all of these toggle commands occupying
    ; keybindings. They steal valuable key-binding space, are hard
    ; to remember, and act globally rather than locally.
    ;
    ; Will be much preferable to switch to direct manipulation for
    ; toggling visibility of these components. Probably using
    ; overlays-at to query and manipulate the current overlay.
    (define-key map "b" 'notmuch-show-toggle-body-read-visible)
    (define-key map "c" 'notmuch-show-toggle-citations-visible)
    (define-key map "h" 'notmuch-show-toggle-headers-visible)
    (define-key map "n" 'notmuch-show-next-message)
    (define-key map "p" 'notmuch-show-previous-message)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "s" 'notmuch-show-toggle-signatures-visible)
    (define-key map "x" 'kill-this-buffer)
    (define-key map "+" 'notmuch-show-add-tag)
    (define-key map "-" 'notmuch-show-remove-tag)
    map)
  "Keymap for \"notmuch show\" buffers.")
(fset 'notmuch-show-mode-map notmuch-show-mode-map)

(defvar notmuch-show-message-begin-regexp    "message{")
(defvar notmuch-show-message-end-regexp      "message}")
(defvar notmuch-show-header-begin-regexp     "header{")
(defvar notmuch-show-header-end-regexp       "header}")
(defvar notmuch-show-body-begin-regexp       "body{")
(defvar notmuch-show-body-end-regexp         "body}")
(defvar notmuch-show-attachment-begin-regexp "attachment{")
(defvar notmuch-show-attachment-end-regexp   "attachment}")
(defvar notmuch-show-part-begin-regexp       "part{")
(defvar notmuch-show-part-end-regexp         "part}")
(defvar notmuch-show-marker-regexp "\\(message\\|header\\|body\\|attachment\\|part\\)[{}].*$")

(defvar notmuch-show-id-regexp "ID: \\([^ ]*\\)")
(defvar notmuch-show-tags-regexp "(\\([^)]*\\))$")

(defun notmuch-show-get-message-id ()
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at notmuch-show-message-begin-regexp))
	(re-search-backward notmuch-show-message-begin-regexp))
    (re-search-forward notmuch-show-id-regexp)
    (buffer-substring (match-beginning 1) (match-end 1))))

(defun notmuch-show-set-tags (tags)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at notmuch-show-message-begin-regexp))
	(re-search-backward notmuch-show-message-begin-regexp))
    (re-search-forward notmuch-show-tags-regexp)
    (let ((inhibit-read-only t)
	  (beg (match-beginning 1))
	  (end (match-end 1)))
      (delete-region beg end)
      (goto-char beg)
      (insert (mapconcat 'identity tags " ")))))

(defun notmuch-show-get-tags ()
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at notmuch-show-message-begin-regexp))
	(re-search-backward notmuch-show-message-begin-regexp))
    (re-search-forward notmuch-show-tags-regexp)
    (split-string (buffer-substring (match-beginning 1) (match-end 1)))))

(defun notmuch-show-add-tag (tag)
  (interactive "sTag to add: ")
  (notmuch-call-notmuch-process "tag" (concat "+" tag) (concat "id:" (notmuch-show-get-message-id)))
  (notmuch-show-set-tags (delete-dups (sort (cons tag (notmuch-show-get-tags)) 'string<))))

(defun notmuch-show-remove-tag (tag)
  (interactive "sTag to remove: ")
  (notmuch-call-notmuch-process "tag" (concat "-" tag) (concat "id:" (notmuch-show-get-message-id)))
  (notmuch-show-set-tags (delete tag (notmuch-show-get-tags))))

(defun notmuch-show-next-message ()
  "Advance point to the beginning of the next message in the buffer.

Before moving, also remove the \"unread\" tag from the current message."
  (interactive)
  (notmuch-show-remove-tag "unread")
  ; First, ensure we get off the current message marker
  (if (not (eobp))
      (forward-char))
  (re-search-forward notmuch-show-message-begin-regexp nil t)
  ; This dance might look pointless, but it's important. I originally
  ; just had (beginning-of-line) here which looked right on the
  ; display but actually put point all the way back to the first
  ; character of the first invisible line. That is, it put point into
  ; the closing markers of the previous message rather than at the
  ; beginning of the current message. And that in turn meant that
  ; looking up the current message-ID would actually return the
  ; previous message ID.
  ;
  ; So this dance ensures that we're actually on the current message
  ; when it looks like we are.
  (end-of-visible-line)
  (beginning-of-line)
  (recenter 0))

(defun notmuch-show-previous-message ()
  "Advance point to the beginning of the previous message in the buffer."
  (interactive)
  ; First, ensure we get off the current message marker
  (if (not (bobp))
      (previous-line))
  (re-search-backward notmuch-show-message-begin-regexp nil t)
  ; This dance might look pointless, but it's important. I originally
  ; just had (beginning-of-line) here which looked right on the
  ; display but actually put point all the way back to the first
  ; character of the first invisible line. That is, it put point into
  ; the closing markers of the previous message rather than at the
  ; beginning of the current message. And that in turn meant that
  ; looking up the current message-ID would actually return the
  ; previous message ID.
  ;
  ; So this dance ensures that we're actually on the current message
  ; when it looks like we are.
  (end-of-visible-line)
  (beginning-of-line)
  (recenter 0))

(defun notmuch-show-markup-citations-region (beg end)
  (goto-char beg)
  (beginning-of-line)
  (while (< (point) end)
    (let ((beg-sub (point)))
      (if (looking-at ">")
	  (progn
	    (while (looking-at ">")
	      (next-line))
	    (let ((overlay (make-overlay beg-sub (point))))
	      (overlay-put overlay 'invisible 'notmuch-show-citation)
	      (overlay-put overlay 'before-string
			   (concat "[" (number-to-string (count-lines beg-sub (point)))
				   " quoted lines.]")))))
      (if (looking-at "--[ ]?$")
	  (let ((overlay (make-overlay beg-sub end)))
	    (overlay-put overlay 'invisible 'notmuch-show-signature)
	    (overlay-put overlay 'before-string
			 (concat "[" (number-to-string (count-lines beg-sub (point)))
				 "-line signature.]"))
	    (goto-char end)))
      (next-line))))

(defun notmuch-show-markup-body (unread)
  (re-search-forward notmuch-show-body-begin-regexp)
  (next-line 1)
  (beginning-of-line)
  (let ((beg (point)))
    (re-search-forward notmuch-show-body-end-regexp)
    (if (not unread)
	(overlay-put (make-overlay beg (match-beginning 0))
		     'invisible 'notmuch-show-body-read))
    (notmuch-show-markup-citations-region beg (point))
    ))

(defun notmuch-show-markup-header ()
  (re-search-forward notmuch-show-header-begin-regexp)
  (next-line 2)
  (beginning-of-line)
  (let ((beg (point)))
    (re-search-forward notmuch-show-header-end-regexp)
    (overlay-put (make-overlay beg (match-beginning 0))
		 'invisible 'notmuch-show-header)))

(defun notmuch-show-markup-message ()
  (if (re-search-forward notmuch-show-message-begin-regexp nil t)
      (progn
	(let ((unread (looking-at ".*unread$")))
	  (notmuch-show-markup-header)
	  (notmuch-show-markup-body unread)))
    (goto-char (point-max))))

(defun notmuch-show-hide-markers ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (re-search-forward notmuch-show-marker-regexp nil t)
	  (progn
	    (overlay-put (make-overlay (match-beginning 0) (+ (match-end 0) 1))
			 'invisible 'notmuch-show-marker))
	(goto-char (point-max))))))

(defun notmuch-show-markup-messages ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (notmuch-show-markup-message)))
  (notmuch-show-hide-markers))

(defun notmuch-show-toggle-citations-visible ()
  "Toggle visibility of citations"
  (interactive)
  (if notmuch-show-citations-visible
      (add-to-invisibility-spec 'notmuch-show-citation)
    (remove-from-invisibility-spec 'notmuch-show-citation))
  (set 'notmuch-show-citations-visible (not notmuch-show-citations-visible))
  ; Need to force the redisplay for some reason
  (force-window-update)
  (redisplay t))

(defun notmuch-show-toggle-signatures-visible ()
  "Toggle visibility of signatures"
  (interactive)
  (if notmuch-show-signatures-visible
      (add-to-invisibility-spec 'notmuch-show-signature)
    (remove-from-invisibility-spec 'notmuch-show-signature))
  (set 'notmuch-show-signatures-visible (not notmuch-show-signatures-visible))
  ; Need to force the redisplay for some reason
  (force-window-update)
  (redisplay t))

(defun notmuch-show-toggle-headers-visible ()
  "Toggle visibility of header fields"
  (interactive)
  (if notmuch-show-headers-visible
      (add-to-invisibility-spec 'notmuch-show-header)
    (remove-from-invisibility-spec 'notmuch-show-header))
  (set 'notmuch-show-headers-visible (not notmuch-show-headers-visible))
  ; Need to force the redisplay for some reason
  (force-window-update)
  (redisplay t))

(defun notmuch-show-toggle-body-read-visible ()
  "Toggle visibility of message bodies of read messages"
  (interactive)
  (if notmuch-show-body-read-visible
      (add-to-invisibility-spec 'notmuch-show-body-read)
    (remove-from-invisibility-spec 'notmuch-show-body-read))
  (set 'notmuch-show-body-read-visible (not notmuch-show-body-read-visible))
  ; Need to force the redisplay for some reason
  (force-window-update)
  (redisplay t))

;;;###autoload
(defun notmuch-show-mode ()
  "Major mode for handling the output of \"notmuch show\""
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'notmuch-show-headers-visible) t)
  (notmuch-show-toggle-headers-visible)
  (set (make-local-variable 'notmuch-show-body-read-visible) t)
  (notmuch-show-toggle-body-read-visible)
  (set (make-local-variable 'notmuch-show-citations-visible) t)
  (notmuch-show-toggle-citations-visible)
  (set (make-local-variable 'notmuch-show-signatures-visible) t)
  (notmuch-show-toggle-signatures-visible)
  (add-to-invisibility-spec 'notmuch-show-marker)
  (use-local-map notmuch-show-mode-map)
  (setq major-mode 'notmuch-show-mode
	mode-name "notmuch-show")
  (setq buffer-read-only t))

(defun notmuch-show (thread-id)
  "Run \"notmuch show\" with the given thread ID and display results."
  (interactive "sNotmuch show: ")
  (let ((buffer (get-buffer-create (concat "*notmuch-show-" thread-id "*"))))
    (switch-to-buffer buffer)
    (notmuch-show-mode)
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (if proc
	  (error "notmuch search process already running for query `%s'" query)
	)
      (erase-buffer)
      (goto-char (point-min))
      (save-excursion
	(call-process "notmuch" nil t nil "show" thread-id)
	(notmuch-show-markup-messages)
	)
      )))

(defvar notmuch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'notmuch-search-archive-thread)
    (define-key map "f" 'notmuch-search-filter)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "s" 'notmuch-search)
    (define-key map "x" 'kill-this-buffer)
    (define-key map "\r" 'notmuch-search-show-thread)
    (define-key map "+" 'notmuch-search-add-tag)
    (define-key map "-" 'notmuch-search-remove-tag)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'notmuch-search-goto-last-thread)
    (define-key map "=" 'notmuch-search-refresh-view)
    (define-key map "\M->" 'notmuch-search-goto-last-thread)
    map)
  "Keymap for \"notmuch search\" buffers.")
(fset 'notmuch-search-mode-map notmuch-search-mode-map)

(defun notmuch-search-goto-last-thread (&optional arg)
  "Move point to the last thread in the buffer."
  (interactive "^P")
  (end-of-buffer arg)
  (beginning-of-line))

;;;###autoload
(defun notmuch-search-mode ()
  "Major mode for handling the output of \"notmuch search\""
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'notmuch-search-query-string)
  (use-local-map notmuch-search-mode-map)
  (setq major-mode 'notmuch-search-mode
	mode-name "notmuch-search")
  (setq buffer-read-only t))

(defun notmuch-search-find-thread-id ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (re-search-forward "[a-fA-F0-9]*")
      (filter-buffer-substring beg (point)))))

(defun notmuch-search-markup-this-thread-id ()
  (beginning-of-line)
  (let ((beg (point)))
    (re-search-forward "[a-fA-F0-9]*")
    (forward-char)
    (overlay-put (make-overlay beg (point)) 'invisible 'notmuch-search)))

(defun notmuch-search-markup-thread-ids ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (notmuch-search-markup-this-thread-id)
      (next-line))))

(defun notmuch-search-hide-thread-ids ()
  (interactive)
  (add-to-invisibility-spec 'notmuch-search))

(defun notmuch-search-show-thread-ids ()
  (interactive)
  (remove-from-invisibility-spec 'notmuch-search))

(defun notmuch-search-show-thread ()
  (interactive)
  (notmuch-show (notmuch-search-find-thread-id)))

(defun notmuch-call-notmuch-process (&rest args)
  (let ((error-buffer (get-buffer-create "*Notmuch errors*")))
    (with-current-buffer error-buffer
	(erase-buffer))
    (if (eq (apply 'call-process "notmuch" nil error-buffer nil args) 0)
	(point)
      (progn
	(with-current-buffer error-buffer
	  (let ((beg (point-min))
		(end (- (point-max) 1)))
	    (error (buffer-substring beg end))
	    ))))))

(defun notmuch-search-set-tags (tags)
  (save-excursion
    (end-of-line)
    (re-search-backward "(")
    (forward-char)
    (let ((beg (point))
	  (inhibit-read-only t))
      (re-search-forward ")")
      (backward-char)
      (let ((end (point)))
	(delete-region beg end)
	(insert (mapconcat  'identity tags " "))))))

(defun notmuch-search-get-tags ()
  (save-excursion
    (end-of-line)
    (re-search-backward "(")
    (let ((beg (+ (point) 1)))
      (re-search-forward ")")
      (let ((end (- (point) 1)))
	(split-string (buffer-substring beg end))))))

(defun notmuch-search-add-tag (tag)
  (interactive "sTag to add: ")
  (notmuch-call-notmuch-process "tag" (concat "+" tag) (concat "thread:" (notmuch-search-find-thread-id)))
  (notmuch-search-set-tags (delete-dups (sort (cons tag (notmuch-search-get-tags)) 'string<))))

(defun notmuch-search-remove-tag (tag)
  (interactive "sTag to remove: ")
  (notmuch-call-notmuch-process "tag" (concat "-" tag) (concat "thread:" (notmuch-search-find-thread-id)))
  (notmuch-search-set-tags (delete tag (notmuch-search-get-tags))))

(defun notmuch-search-archive-thread ()
  (interactive)
  (notmuch-search-remove-tag "inbox"))

(defun notmuch-search (query)
  "Run \"notmuch search\" with the given query string and display results."
  (interactive "sNotmuch search: ")
  (let ((buffer (get-buffer-create (concat "*notmuch-search-" query "*"))))
    (switch-to-buffer buffer)
    (notmuch-search-mode)
    (set 'notmuch-search-query-string query)
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (if proc
	  (error "notmuch search process already running for query `%s'" query)
	)
      (erase-buffer)
      (goto-char (point-min))
      (save-excursion
	(call-process "notmuch" nil t nil "search" query)
	(notmuch-search-markup-thread-ids)
        ; A well-behaved program ends its output with a newline, but we
        ; don't actually want the blank line at the end of the file.
	(goto-char (point-max))
	(if (looking-at "^$")
	    (delete-backward-char 1)
	  )
	))))

(defun notmuch-search-refresh-view ()
  "Refresh the current view.

Kills the current buffer and runs a new search with the same
query string as the current search. If the current thread is in
the new search results, then point will be placed on the same
thread. Otherwise, point will be moved to attempt to be in the
same relative position within the new buffer."
  (interactive)
  (let ((here (point))
	(thread (notmuch-search-find-thread-id))
	(query notmuch-search-query-string))
    (kill-this-buffer)
    (notmuch-search query)
    (goto-char (point-min))
    (if (re-search-forward (concat "^" thread) nil t)
	(beginning-of-line)
      (goto-char here))))

(defun notmuch-search-filter (query)
  "Run \"notmuch search\" to refine the current search results.

A search string will be constructed by appending QUERY to the
current search string, and the results of \"notmuch search\" for
the combined query will be displayed."
  (interactive "sFilter search: ")
  (notmuch-search (concat notmuch-search-query-string " and " query)))

(defun notmuch ()
  "Run notmuch to display all mail with tag of 'inbox'"
  (interactive)
  (notmuch-search "tag:inbox"))

(provide 'notmuch)
