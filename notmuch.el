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

(load "cl-seq")

(defvar notmuch-show-mode-map
  (let ((map (make-sparse-keymap)))
    ; I don't actually want all of these toggle commands occupying
    ; keybindings. They steal valuable key-binding space, are hard
    ; to remember, and act globally rather than locally.
    ;
    ; Will be much preferable to switch to direct manipulation for
    ; toggling visibility of these components. Probably using
    ; overlays-at to query and manipulate the current overlay.
    (define-key map "a" 'notmuch-show-archive-thread)
    (define-key map "A" 'notmuch-show-mark-read-then-archive-thread)
    (define-key map "b" 'notmuch-show-toggle-body-read-visible)
    (define-key map "c" 'notmuch-show-toggle-citations-visible)
    (define-key map "h" 'notmuch-show-toggle-headers-visible)
    (define-key map "n" 'notmuch-show-next-message)
    (define-key map "N" 'notmuch-show-mark-read-then-next-open-message)
    (define-key map "p" 'notmuch-show-previous-message)
    (define-key map (kbd "C-n") 'notmuch-show-next-line)
    (define-key map (kbd "C-p") 'notmuch-show-previous-line)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "s" 'notmuch-show-toggle-signatures-visible)
    (define-key map "w" 'notmuch-show-view-raw-message)
    (define-key map "x" 'kill-this-buffer)
    (define-key map "+" 'notmuch-show-add-tag)
    (define-key map "-" 'notmuch-show-remove-tag)
    (define-key map (kbd "DEL") 'notmuch-show-rewind)
    (define-key map " " 'notmuch-show-advance-marking-read-and-archiving)
    (define-key map "|" 'notmuch-show-pipe-message)
    map)
  "Keymap for \"notmuch show\" buffers.")
(fset 'notmuch-show-mode-map notmuch-show-mode-map)

(defvar notmuch-show-signature-lines-max 6
  "Maximum length of signature that will be hidden by default.")

(set 'notmuch-show-message-begin-regexp    "message{")
(set 'notmuch-show-message-end-regexp      "message}")
(set 'notmuch-show-header-begin-regexp     "header{")
(set 'notmuch-show-header-end-regexp       "header}")
(set 'notmuch-show-body-begin-regexp       "body{")
(set 'notmuch-show-body-end-regexp         "body}")
(set 'notmuch-show-attachment-begin-regexp "attachment{")
(set 'notmuch-show-attachment-end-regexp   "attachment}")
(set 'notmuch-show-part-begin-regexp       "part{")
(set 'notmuch-show-part-end-regexp         "part}")
(set 'notmuch-show-marker-regexp "\\(message\\|header\\|body\\|attachment\\|part\\)[{}].*$")

(set 'notmuch-show-id-regexp "id:\\([^ ]*\\)")
(set 'notmuch-show-filename-regexp "filename:\\(.*\\)$")
(set 'notmuch-show-tags-regexp "(\\([^)]*\\))$")

; XXX: This should be a generic function in emacs somewhere, not here
(defun point-invisible-p ()
  "Return whether the character at point is invisible.

Here visibility is determined by `buffer-invisibility-spec' and
the invisible property of any overlays for point. It doesn't have
anything to do with whether point is currently being displayed
within the current window."
  (let ((prop (get-char-property (point) 'invisible)))
    (if (eq buffer-invisibility-spec t)
	prop
      (or (memq prop buffer-invisibility-spec)
	  (assq prop buffer-invisibility-spec)))))

(defun notmuch-show-next-line ()
  "Like builtin `next-line' but ensuring we end on a visible character.

By advancing forward until reaching a visible character.

Unlike builtin `next-line' this version accepts no arguments."
  (interactive)
  (set 'this-command 'next-line)
  (call-interactively 'next-line)
  (while (point-invisible-p)
    (forward-char)))

(defun notmuch-show-previous-line ()
  "Like builtin `previous-line' but ensuring we end on a visible character.

By advancing forward until reaching a visible character.

Unlike builtin `next-line' this version accepts no arguments."
  (interactive)
  (set 'this-command 'previous-line)
  (call-interactively 'previous-line)
  (while (point-invisible-p)
    (forward-char)))

(defun notmuch-show-get-message-id ()
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at notmuch-show-message-begin-regexp))
	(re-search-backward notmuch-show-message-begin-regexp))
    (re-search-forward notmuch-show-id-regexp)
    (buffer-substring (match-beginning 1) (match-end 1))))

(defun notmuch-show-get-filename ()
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at notmuch-show-message-begin-regexp))
	(re-search-backward notmuch-show-message-begin-regexp))
    (re-search-forward notmuch-show-filename-regexp)
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

(defun notmuch-show-add-tag (&rest toadd)
  "Add a tag to the current message."
  (interactive "sTag to add: ")
  (apply 'notmuch-call-notmuch-process
	 (append (cons "tag"
		       (mapcar (lambda (s) (concat "+" s)) toadd))
		 (cons (concat "id:" (notmuch-show-get-message-id)) nil)))
  (notmuch-show-set-tags (sort (union toadd (notmuch-show-get-tags) :test 'string=) 'string<)))

(defun notmuch-show-remove-tag (&rest toremove)
  "Remove a tag from the current message."
  (interactive "sTag to remove: ")
  (let ((tags (notmuch-show-get-tags)))
    (if (intersection tags toremove :test 'string=)
	(progn
	  (apply 'notmuch-call-notmuch-process
		 (append (cons "tag"
			       (mapcar (lambda (s) (concat "-" s)) toremove))
			 (cons (concat "id:" (notmuch-show-get-message-id)) nil)))
	  (notmuch-show-set-tags (sort (set-difference tags toremove :test 'string=) 'string<))))))

(defun notmuch-show-archive-thread-maybe-mark-read (markread)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if markread
	  (notmuch-show-remove-tag "unread" "inbox")
	(notmuch-show-remove-tag "inbox"))
      (if (not (eobp))
	  (forward-char))
      (if (not (re-search-forward notmuch-show-message-begin-regexp nil t))
	  (goto-char (point-max)))))
  (let ((parent-buffer notmuch-show-parent-buffer))
    (kill-this-buffer)
    (if parent-buffer
	(progn
	  (switch-to-buffer parent-buffer)
	  (forward-line)
	  (notmuch-search-show-thread)))))

(defun notmuch-show-mark-read-then-archive-thread ()
  "Remove \"unread\" tag from each message, then archive and show next thread.

Archive each message currrently shown by removing the \"unread\"
and \"inbox\" tag from each. Then kill this buffer and show the
next thread from the search from which this thread was originally
shown.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not archive the
entire thread, but only the messages shown in the current
buffer."
  (interactive)
  (notmuch-show-archive-thread-maybe-mark-read t))

(defun notmuch-show-archive-thread ()
  "Archive each message in thread, and show next thread from search.

Archive each message currrently shown by removing the \"inbox\"
tag from each. Then kill this buffer and show the next thread
from the search from which this thread was originally shown.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not archive the
entire thread, but only the messages shown in the current
buffer."
  (interactive)
  (notmuch-show-archive-thread-maybe-mark-read nil))

(defun notmuch-show-view-raw-message ()
  "View the raw email of the current message."
  (interactive)
  (view-file (notmuch-show-get-filename)))

(defun notmuch-show-pipe-message (command)
  "Pipe the contents of the current message to the given command.

The given command will be executed with the raw contents of the
current email message as stdin. Anything printed by the command
to stdout or stderr will appear in the *Messages* buffer."
  (interactive "sPipe message to command: ")
  (apply 'start-process-shell-command "notmuch-pipe-command" "*notmuch-pipe*" (split-string (concat command " < " (notmuch-show-get-filename)))))

(defun notmuch-show-move-to-current-message-summary-line ()
  "Move to the beginning of the one-line summary of the current message.

This gives us a stable place to move to and work from since the
summary line is always visible. This is important since moving to
an invisible location is unreliable, (the main command loop moves
point either forward or backward to the next visible character
when a command ends with point on an invisible character).

Emits an error if point is not within a valid message, (that is
not pattern of `notmuch-show-message-begin-regexp' could be found
by searching backward)."
  (beginning-of-line)
  (if (not (looking-at notmuch-show-message-begin-regexp))
      (if (re-search-backward notmuch-show-message-begin-regexp nil t)
	  (forward-line 2)
	(error "Not within a valid message."))
    (forward-line 2)))

(defun notmuch-show-last-message-p ()
  "Predicate testing whether point is within the last message."
  (save-window-excursion
    (save-excursion
      (notmuch-show-move-to-current-message-summary-line)
      (not (re-search-forward notmuch-show-message-begin-regexp nil t)))))

(defun notmuch-show-message-unread-p ()
  "Preficate testing whether current message is unread."
  (member "unread" (notmuch-show-get-tags)))

(defun notmuch-show-next-message ()
  "Advance to the beginning of the next message in the buffer.

Moves to the last visible character of the current message if
already on the last message in the buffer."
  (interactive)
  (notmuch-show-move-to-current-message-summary-line)
  (if (re-search-forward notmuch-show-message-begin-regexp nil t)
      (notmuch-show-move-to-current-message-summary-line)
    (goto-char (- (point-max) 1))
    (while (point-invisible-p)
      (backward-char)))
  (recenter 0))

(defun notmuch-show-find-next-message ()
  "Returns the position of the next message in the buffer.

Or the position of the last visible character of the current
message if already within the last message in the buffer."
  ; save-excursion doesn't save our window position
  ; save-window-excursion doesn't save point
  ; Looks like we have to use both.
  (save-excursion
    (save-window-excursion
      (notmuch-show-next-message)
      (point))))

(defun notmuch-show-next-unread-message ()
  "Advance to the beginning of the next unread message in the buffer.

Moves to the last visible character of the current message if
there are no more unread messages past the current point."
  (notmuch-show-next-message)
  (while (and (not (notmuch-show-last-message-p))
	      (not (notmuch-show-message-unread-p)))
    (notmuch-show-next-message))
  (if (not (notmuch-show-message-unread-p))
      (notmuch-show-next-message)))

(defun notmuch-show-next-open-message ()
  "Advance to the the next message which is not hidden.

If read messages are currently hidden, advance to the next unread
message. Otherwise, advance to the next message."
  (if (or (memq 'notmuch-show-body-read buffer-invisibility-spec)
	  (assq 'notmuch-show-body-read buffer-invisibility-spec))
      (notmuch-show-next-unread-message)
    (notmuch-show-next-message)))

(defun notmuch-show-previous-message ()
  "Backup to the beginning of the previous message in the buffer.

If within a message rather than at the beginning of it, then
simply move to the beginning of the current message."
  (interactive)
  (let ((start (point)))
    (notmuch-show-move-to-current-message-summary-line)
    (if (not (< (point) start))
	; Go backward twice to skip the current message's marker
	(progn
	  (re-search-backward notmuch-show-message-begin-regexp nil t)
	  (re-search-backward notmuch-show-message-begin-regexp nil t)
	  (notmuch-show-move-to-current-message-summary-line)
	  ))
    (recenter 0)))

(defun notmuch-show-find-previous-message ()
  "Returns the position of the previous message in the buffer.

Or the position of the beginning of the current message if point
is originally within the message rather than at the beginning of
it."
  ; save-excursion doesn't save our window position
  ; save-window-excursion doesn't save point
  ; Looks like we have to use both.
  (save-excursion
    (save-window-excursion
      (notmuch-show-previous-message)
      (point))))

(defun notmuch-show-mark-read-then-next-open-message ()
  "Remove unread tag from current message, then advance to next unread message."
  (interactive)
  (notmuch-show-remove-tag "unread")
  (notmuch-show-next-open-message))

(defun notmuch-show-rewind ()
  "Do reverse scrolling compared to `notmuch-show-advance-marking-read-and-archiving'

Specifically, if the beginning of the previous email is fewer
than `window-height' lines from the current point, move to it
just like `notmuch-show-previous-message'.

Otherwise, just scroll down a screenful of the current message.

This command does not modify any message tags, (it does not undo
any effects from previous calls to
`notmuch-show-advance-marking-read-and-archiving'."
  (interactive)
  (let ((previous (notmuch-show-find-previous-message)))
    (if (> (count-lines previous (point)) (- (window-height) next-screen-context-lines))
	(progn
	  (condition-case nil
	      (scroll-down nil)
	    ((beginning-of-buffer) nil))
	  (goto-char (window-start)))
      (notmuch-show-previous-message))))

(defun notmuch-show-advance-marking-read-and-archiving ()
  "Advance through buffer, marking read and archiving.

This command is intended to be one of the simplest ways to
process a thread of email. It does the following:

If the current message in the thread is not yet fully visible,
scroll by a near screenful to read more of the message.

Otherwise, (the end of the current message is already within the
current window), remove the \"unread\" tag (if present) from the
current message and advance to the next open message.

Finally, if there is no further message to advance to, and this
last message is already read, then archive the entire current
thread, (remove the \"inbox\" tag from each message). Also kill
this buffer, and display the next thread from the search from
which this thread was originally shown."
  (interactive)
  (let ((next (notmuch-show-find-next-message))
	(unread (notmuch-show-message-unread-p)))
    (if (> next (window-end))
	(scroll-up nil)
      (let ((last (notmuch-show-last-message-p)))
	(notmuch-show-mark-read-then-next-open-message)
	(if last
	    (notmuch-show-archive-thread))))))

(defun notmuch-show-markup-citations-region (beg end)
  (goto-char beg)
  (beginning-of-line)
  (while (< (point) end)
    (let ((beg-sub (point)))
      (if (looking-at ">")
	  (progn
	    (while (looking-at ">")
	      (forward-line))
	    (let ((overlay (make-overlay beg-sub (point))))
	      (overlay-put overlay 'invisible 'notmuch-show-citation)
	      (overlay-put overlay 'before-string
			   (concat "[" (number-to-string (count-lines beg-sub (point)))
				   "-line citation. Press 'c' to show.]\n")))))
      (if (looking-at "--[ ]?$")
	  (let ((sig-lines (count-lines beg-sub end)))
	    (if (<= sig-lines notmuch-show-signature-lines-max)
		(progn
		  (overlay-put (make-overlay beg-sub (+ beg-sub 1))
			       'before-string
			       (concat "[" (number-to-string sig-lines)
				       "-line signature. Press 's' to show.]"))
		  (overlay-put (make-overlay (+ beg-sub 2) end)
			       'invisible 'notmuch-show-signature)
		  (goto-char end)))))
      (forward-line))))

(defun notmuch-show-markup-body ()
  (re-search-forward notmuch-show-body-begin-regexp)
  (next-line 1)
  (beginning-of-line)
  (let ((beg (point)))
    (re-search-forward notmuch-show-body-end-regexp)
    (let ((end (match-beginning 0)))
      (notmuch-show-markup-citations-region beg end)
      (if (not (notmuch-show-message-unread-p))
	  (overlay-put (make-overlay beg end)
		       'invisible 'notmuch-show-body-read)))))

(defun notmuch-show-markup-header ()
  (re-search-forward notmuch-show-header-begin-regexp)
  (forward-line 1)
  (beginning-of-line)
  (let ((beg (point)))
    (end-of-line)
    ; Inverse video for subject
    (overlay-put (make-overlay beg (point)) 'face '((cons :inverse-video t)))
    (beginning-of-line)
    (forward-line 2)
    (set 'beg (point))
    (re-search-forward notmuch-show-header-end-regexp)
    (overlay-put (make-overlay beg (match-beginning 0))
		 'invisible 'notmuch-show-header)))

(defun notmuch-show-markup-message ()
  (if (re-search-forward notmuch-show-message-begin-regexp nil t)
      (progn
	(notmuch-show-markup-header)
	(notmuch-show-markup-body))
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
  "Major mode for viewing a thread with notmuch.

This buffer contains the results of the \"notmuch show\" command
for displaying a single thread of email from your email archives.

By default, various components of email messages, (citations,
signatures, already-read messages), are invisible to help you
focus on the most important things, (new text from unread
messages). See the various commands below for toggling the
visibility of hidden components.

The `notmuch-show-next-message' and
`notmuch-show-previous-message' commands, (bound to 'n' and 'p by
default), allow you to navigate to the next and previous
messages. Each time you navigate away from a message with
`notmuch-show-next-message' the current message will have its
\"unread\" tag removed.

You can add or remove tags from the current message with '+' and
'-'.  You can also archive all messages in the current
view, (remove the \"inbox\" tag from each), with
`notmuch-show-archive-thread' (bound to 'a' by default).

\\{notmuch-show-mode-map}"
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

(defun notmuch-show (thread-id &optional parent-buffer)
  "Run \"notmuch show\" with the given thread ID and display results.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the next
thread from that buffer can be show when done with this one)."
  (interactive "sNotmuch show: ")
  (let ((buffer (get-buffer-create (concat "*notmuch-show-" thread-id "*"))))
    (switch-to-buffer buffer)
    (notmuch-show-mode)
    (set (make-local-variable 'notmuch-show-parent-buffer) parent-buffer)
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
      ; Move straight to the first unread message
      (if (not (notmuch-show-message-unread-p))
	  (progn
	    (notmuch-show-next-unread-message)
	    ; But if there are no unread messages, go back to the
	    ; beginning of the buffer, and open up the bodies of all
	    ; read message.
	    (if (not (notmuch-show-message-unread-p))
		(progn
		  (goto-char (point-min))
		  (notmuch-show-toggle-body-read-visible)))))
      )))

(defvar notmuch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'notmuch-search-archive-thread)
    (define-key map "b" 'notmuch-search-scroll-down)
    (define-key map "f" 'notmuch-search-filter)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "s" 'notmuch-search)
    (define-key map "t" 'notmuch-search-filter-by-tag)
    (define-key map "x" 'kill-this-buffer)
    (define-key map (kbd "RET") 'notmuch-search-show-thread)
    (define-key map "+" 'notmuch-search-add-tag)
    (define-key map "-" 'notmuch-search-remove-tag)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'notmuch-search-goto-last-thread)
    (define-key map "=" 'notmuch-search-refresh-view)
    (define-key map "\M->" 'notmuch-search-goto-last-thread)
    (define-key map " " 'notmuch-search-scroll-up)
    (define-key map (kbd "<DEL>") 'notmuch-search-scroll-down)
    map)
  "Keymap for \"notmuch search\" buffers.")
(fset 'notmuch-search-mode-map notmuch-search-mode-map)

(defun notmuch-search-scroll-up ()
  "Scroll up, moving point to last message in thread if at end."
  (interactive)
  (condition-case nil
      (scroll-up nil)
    ((end-of-buffer) (notmuch-search-goto-last-thread))))

(defun notmuch-search-scroll-down ()
  "Scroll down, moving point to first message in thread if at beginning."
  (interactive)
  ; I don't know why scroll-down doesn't signal beginning-of-buffer
  ; the way that scroll-up signals end-of-buffer, but c'est la vie.
  ;
  ; So instead of trapping a signal we instead check whether the
  ; window begins on the first line of the buffer and if so, move
  ; directly to that position. (We have to count lines since the
  ; window-start position is not the same as point-min due to the
  ; invisible thread-ID characters on the first line.
  (if (equal (count-lines (point-min) (window-start)) 1)
      (goto-char (window-start))
    (scroll-down nil)))

(defun notmuch-search-goto-last-thread (&optional arg)
  "Move point to the last thread in the buffer."
  (interactive "^P")
  (end-of-buffer arg)
  (forward-line -1))

;;;###autoload
(defun notmuch-search-mode ()
  "Major mode for searching mail with notmuch.

This buffer contains the results of a \"notmuch search\" of your
email archives. Each line in the buffer represents a single
thread giving a relative date for the thread and a subject.

Pressing RET on any line displays that thread. The '+' and '-'
keys can be used to add or remove tags from a thread. The 'a' key
is a convenience key for archiving a thread (removing the
\"inbox\" tag).

Other useful commands are `notmuch-search-filter' for filtering
the current search based on an additional query string,
`notmuch-search-filter-by-tag' for filtering to include only
messages with a given tag, and `notmuch-search' to execute a new,
global search.

\\{notmuch-search-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'notmuch-search-query-string)
  (set (make-local-variable 'scroll-preserve-screen-position) t)
  (add-to-invisibility-spec 'notmuch-search)
  (use-local-map notmuch-search-mode-map)
  (setq major-mode 'notmuch-search-mode
	mode-name "notmuch-search")
  (setq buffer-read-only t))

(defun notmuch-search-find-thread-id ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (re-search-forward "thread:[a-fA-F0-9]*" nil t)
      (filter-buffer-substring beg (point)))))

(defun notmuch-search-markup-this-thread-id ()
  (beginning-of-line)
  (let ((beg (point)))
    (if (re-search-forward "thread:[a-fA-F0-9]*" nil t)
	(progn
	  (forward-char)
	  (overlay-put (make-overlay beg (point)) 'invisible 'notmuch-search)))))

(defun notmuch-search-markup-thread-ids ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (notmuch-search-markup-this-thread-id)
      (next-line))))

(defun notmuch-search-hide-thread-ids ()
  (interactive)
  (add-to-invisibility-spec 'notmuch-search)
  (force-window-update)
  (redisplay t))

(defun notmuch-search-show-thread-ids ()
  (interactive)
  (remove-from-invisibility-spec 'notmuch-search)
  (force-window-update)
  (redisplay t))

(defun notmuch-search-show-thread ()
  (interactive)
  (let ((thread-id (notmuch-search-find-thread-id)))
    (if (> (length thread-id) 0)
	(notmuch-show thread-id (current-buffer))
      (error "End of search results"))))

(defun notmuch-call-notmuch-process (&rest args)
  "Synchronously invoke \"notmuch\" with the given list of arguments.

Output from the process will be presented to the user as an error
and will also appear in a buffer named \"*Notmuch errors*\"."
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
  (notmuch-call-notmuch-process "tag" (concat "+" tag) (notmuch-search-find-thread-id))
  (notmuch-search-set-tags (delete-dups (sort (cons tag (notmuch-search-get-tags)) 'string<))))

(defun notmuch-search-remove-tag (tag)
  (interactive "sTag to remove: ")
  (notmuch-call-notmuch-process "tag" (concat "-" tag) (notmuch-search-find-thread-id))
  (notmuch-search-set-tags (delete tag (notmuch-search-get-tags))))

(defun notmuch-search-archive-thread ()
  "Archive the current thread (remove its \"inbox\" tag).

This function advances point to the next line when finished."
  (interactive)
  (notmuch-search-remove-tag "inbox")
  (forward-line))

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
  "Filter the current search results based on an additional query string.

Runs a new search matching only messages that match both the
current search results AND the additional query string provided."
  (interactive "sFilter search: ")
  (notmuch-search (concat notmuch-search-query-string " and " query)))

(defun notmuch-search-filter-by-tag (tag)
  "Filter the current search results based on a single tag.

Runs a new search matching only messages that match both the
current search results AND that are tagged with the given tag."
  (interactive "sFilter by tag: ")
  (notmuch-search (concat notmuch-search-query-string " and tag:" tag)))

(defun notmuch ()
  "Run notmuch to display all mail with tag of 'inbox'"
  (interactive)
  (notmuch-search "tag:inbox"))

(provide 'notmuch)
