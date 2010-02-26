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

; This is an emacs-based interface to the notmuch mail system.
;
; You will first need to have the notmuch program installed and have a
; notmuch database built in order to use this. See
; http://notmuchmail.org for details.
;
; To install this software, copy it to a directory that is on the
; `load-path' variable within emacs (a good candidate is
; /usr/local/share/emacs/site-lisp). If you are viewing this from the
; notmuch source distribution then you can simply run:
;
;	sudo make install-emacs
;
; to install it.
;
; Then, to actually run it, add:
;
;	(require 'notmuch)
;
; to your ~/.emacs file, and then run "M-x notmuch" from within emacs,
; or run:
;
;	emacs -f notmuch
;
; Have fun, and let us know if you have any comment, questions, or
; kudos: Notmuch list <notmuch@notmuchmail.org> (subscription is not
; required, but is available from http://notmuchmail.org).

(require 'cl)
(require 'mm-view)
(require 'message)

(defvar notmuch-show-stash-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'notmuch-show-stash-cc)
    (define-key map "d" 'notmuch-show-stash-date)
    (define-key map "F" 'notmuch-show-stash-filename)
    (define-key map "f" 'notmuch-show-stash-from)
    (define-key map "i" 'notmuch-show-stash-message-id)
    (define-key map "s" 'notmuch-show-stash-subject)
    (define-key map "T" 'notmuch-show-stash-tags)
    (define-key map "t" 'notmuch-show-stash-to)
    map)
  "Submap for stash commands"
  )

(fset 'notmuch-show-stash-map notmuch-show-stash-map)

(defvar notmuch-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'notmuch-help)
    (define-key map "q" 'kill-this-buffer)
    (define-key map (kbd "C-p") 'notmuch-show-previous-line)
    (define-key map (kbd "C-n") 'notmuch-show-next-line)
    (define-key map (kbd "M-TAB") 'notmuch-show-previous-button)
    (define-key map (kbd "TAB") 'notmuch-show-next-button)
    (define-key map "s" 'notmuch-search)
    (define-key map "m" 'message-mail)
    (define-key map "f" 'notmuch-show-forward-current)
    (define-key map "r" 'notmuch-show-reply)
    (define-key map "|" 'notmuch-show-pipe-message)
    (define-key map "w" 'notmuch-show-save-attachments)
    (define-key map "V" 'notmuch-show-view-raw-message)
    (define-key map "v" 'notmuch-show-view-all-mime-parts)
    (define-key map "c" 'notmuch-show-stash-map)
    (define-key map "b" 'notmuch-show-toggle-current-body)
    (define-key map "h" 'notmuch-show-toggle-current-header)
    (define-key map "-" 'notmuch-show-remove-tag)
    (define-key map "+" 'notmuch-show-add-tag)
    (define-key map "x" 'notmuch-show-archive-thread-then-exit)
    (define-key map "a" 'notmuch-show-archive-thread)
    (define-key map "P" 'notmuch-show-previous-message)
    (define-key map "N" 'notmuch-show-next-message)
    (define-key map "p" 'notmuch-show-previous-open-message)
    (define-key map "n" 'notmuch-show-next-open-message)
    (define-key map (kbd "DEL") 'notmuch-show-rewind)
    (define-key map " " 'notmuch-show-advance-and-archive)
    map)
  "Keymap for \"notmuch show\" buffers.")
(fset 'notmuch-show-mode-map notmuch-show-mode-map)

(defvar notmuch-show-signature-regexp "\\(-- ?\\|_+\\)$"
  "Pattern to match a line that separates content from signature.

The regexp can (and should) include $ to match the end of the
line, but should not include ^ to match the beginning of the
line. This is because notmuch may have inserted additional space
for indentation at the beginning of the line. But notmuch will
move past the indentation when testing this pattern, (so that the
pattern can still test against the entire line).")

(defvar notmuch-show-signature-button-format
  "[ %d-line signature. Click/Enter to toggle visibility. ]"
  "String used to construct button text for hidden signatures

Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-show-citation-button-format
  "[ %d more citation lines. Click/Enter to toggle visibility. ]"
  "String used to construct button text for hidden citations.

Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-show-signature-lines-max 12
  "Maximum length of signature that will be hidden by default.")

(defvar notmuch-show-citation-lines-prefix 4
  "Always show at least this many lines of a citation.

If there is one more line, show that, otherwise collapse
remaining lines into a button.")

(defvar notmuch-command "notmuch"
  "Command to run the notmuch binary.")

(defvar notmuch-show-message-begin-regexp    "\fmessage{")
(defvar notmuch-show-message-end-regexp      "\fmessage}")
(defvar notmuch-show-header-begin-regexp     "\fheader{")
(defvar notmuch-show-header-end-regexp       "\fheader}")
(defvar notmuch-show-body-begin-regexp       "\fbody{")
(defvar notmuch-show-body-end-regexp         "\fbody}")
(defvar notmuch-show-attachment-begin-regexp "\fattachment{")
(defvar notmuch-show-attachment-end-regexp   "\fattachment}")
(defvar notmuch-show-part-begin-regexp       "\fpart{")
(defvar notmuch-show-part-end-regexp         "\fpart}")
(defvar notmuch-show-marker-regexp "\f\\(message\\|header\\|body\\|attachment\\|part\\)[{}].*$")

(defvar notmuch-show-id-regexp "\\(id:[^ ]*\\)")
(defvar notmuch-show-depth-match-regexp " depth:\\([0-9]*\\).*match:\\([01]\\) ")
(defvar notmuch-show-filename-regexp "filename:\\(.*\\)$")
(defvar notmuch-show-contentype-regexp "Content-type: \\(.*\\)")

(defvar notmuch-show-tags-regexp "(\\([^)]*\\))$")

(defvar notmuch-show-parent-buffer nil)
(defvar notmuch-show-body-read-visible nil)
(defvar notmuch-show-citations-visible nil)
(defvar notmuch-show-signatures-visible nil)
(defvar notmuch-show-headers-visible nil)

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

(defun notmuch-select-tag-with-completion (prompt &rest search-terms)
  (let ((tag-list
	 (with-output-to-string
	   (with-current-buffer standard-output
	     (apply 'call-process notmuch-command nil t nil "search-tags" search-terms)))))
    (completing-read prompt (split-string tag-list "\n+" t) nil nil nil)))

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

Unlike builtin `previous-line' this version accepts no arguments."
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
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun notmuch-show-get-filename ()
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at notmuch-show-message-begin-regexp))
	(re-search-backward notmuch-show-message-begin-regexp))
    (re-search-forward notmuch-show-filename-regexp)
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

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

(defun notmuch-show-get-bcc ()
  "Return BCC address(es) of current message"
  (notmuch-show-get-header-field 'bcc))

(defun notmuch-show-get-cc ()
  "Return CC address(es) of current message"
  (notmuch-show-get-header-field 'cc))

(defun notmuch-show-get-date ()
  "Return Date of current message"
  (notmuch-show-get-header-field 'date))

(defun notmuch-show-get-from ()
  "Return From address of current message"
  (notmuch-show-get-header-field 'from))

(defun notmuch-show-get-subject ()
  "Return Subject of current message"
  (notmuch-show-get-header-field 'subject))

(defun notmuch-show-get-to ()
  "Return To address(es) of current message"
  (notmuch-show-get-header-field 'to))

(defun notmuch-show-get-header-field (name)
  "Retrieve the header field NAME from the current message.
NAME should be a symbol, in lower case, as returned by
mail-header-extract-no-properties"
  (let* ((result (assoc name (notmuch-show-get-header)))
        (val (and result (cdr result))))
    val))

(defun notmuch-show-get-header ()
  "Retrieve and parse the header from the current message. Returns an alist with of (header . value)
where header is a symbol and value is a string.  The summary from notmuch-show is returned as the
pseudoheader summary"
  (require 'mailheader)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at notmuch-show-message-begin-regexp))
	(re-search-backward notmuch-show-message-begin-regexp))
    (re-search-forward (concat notmuch-show-header-begin-regexp "\n[[:space:]]*\\(.*\\)\n"))
    (let* ((summary (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	  (beg (point)))
      (re-search-forward notmuch-show-header-end-regexp)
      (let ((text (buffer-substring beg (match-beginning 0))))
	(with-temp-buffer
	  (insert text)
	  (goto-char (point-min))
	  (while (looking-at "\\([[:space:]]*\\)[A-Za-z][-A-Za-z0-9]*:")
	    (delete-region (match-beginning 1) (match-end 1))
	    (forward-line)
	    )
	  (goto-char (point-min))
	  (cons (cons 'summary summary) (mail-header-extract-no-properties)))))))

(defun notmuch-show-add-tag (&rest toadd)
  "Add a tag to the current message."
  (interactive
   (list (notmuch-select-tag-with-completion "Tag to add: ")))
  (apply 'notmuch-call-notmuch-process
	 (append (cons "tag"
		       (mapcar (lambda (s) (concat "+" s)) toadd))
		 (cons (notmuch-show-get-message-id) nil)))
  (notmuch-show-set-tags (sort (union toadd (notmuch-show-get-tags) :test 'string=) 'string<)))

(defun notmuch-show-remove-tag (&rest toremove)
  "Remove a tag from the current message."
  (interactive
   (list (notmuch-select-tag-with-completion "Tag to remove: " (notmuch-show-get-message-id))))
  (let ((tags (notmuch-show-get-tags)))
    (if (intersection tags toremove :test 'string=)
	(progn
	  (apply 'notmuch-call-notmuch-process
		 (append (cons "tag"
			       (mapcar (lambda (s) (concat "-" s)) toremove))
			 (cons (notmuch-show-get-message-id) nil)))
	  (notmuch-show-set-tags (sort (set-difference tags toremove :test 'string=) 'string<))))))

(defun notmuch-show-archive-thread ()
  "Archive each message in thread, then show next thread from search.

Archive each message currently shown by removing the \"inbox\"
tag from each. Then kill this buffer and show the next thread
from the search from which this thread was originally shown.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not archive the
entire thread, but only the messages shown in the current
buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (notmuch-show-remove-tag "inbox")
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

(defun notmuch-show-archive-thread-then-exit ()
  "Archive each message in thread, then exit back to search results."
  (interactive)
  (notmuch-show-archive-thread)
  (kill-this-buffer))

(defun notmuch-show-view-raw-message ()
  "View the raw email of the current message."
  (interactive)
  (view-file (notmuch-show-get-filename)))

(defmacro with-current-notmuch-show-message (&rest body)
  "Evaluate body with current buffer set to the text of current message"
  `(save-excursion
     (let ((filename (notmuch-show-get-filename)))
       (let ((buf (generate-new-buffer (concat "*notmuch-msg-" filename "*"))))
         (with-current-buffer buf
           (insert-file-contents filename nil nil nil t)
           ,@body)
	 (kill-buffer buf)))))

(defun notmuch-show-view-all-mime-parts ()
  "Use external viewers to view all attachments from the current message."
  (interactive)
  (with-current-notmuch-show-message
   ; We ovverride the mm-inline-media-tests to indicate which message
   ; parts are already sufficiently handled by the original
   ; presentation of the message in notmuch-show mode. These parts
   ; will be inserted directly into the temporary buffer of
   ; with-current-notmuch-show-message and silently discarded.
   ;
   ; Any MIME part not explicitly mentioned here will be handled by an
   ; external viewer as configured in the various mailcap files.
   (let ((mm-inline-media-tests '(
				  ("text/.*" ignore identity)
				  ("application/pgp-signature" ignore identity)
				  ("multipart/alternative" ignore identity)
				  ("multipart/mixed" ignore identity)
				  ("multipart/related" ignore identity)
				 )))
     (mm-display-parts (mm-dissect-buffer)))))

(defun notmuch-foreach-mime-part (function mm-handle)
  (cond ((stringp (car mm-handle))
         (dolist (part (cdr mm-handle))
           (notmuch-foreach-mime-part function part)))
        ((bufferp (car mm-handle))
         (funcall function mm-handle))
        (t (dolist (part mm-handle)
             (notmuch-foreach-mime-part function part)))))

(defun notmuch-count-attachments (mm-handle)
  (let ((count 0))
    (notmuch-foreach-mime-part
     (lambda (p)
       (let ((disposition (mm-handle-disposition p)))
         (and (listp disposition)
              (or (equal (car disposition) "attachment")
                  (and (equal (car disposition) "inline")
                       (assq 'filename disposition)))
              (incf count))))
     mm-handle)
    count))

(defun notmuch-save-attachments (mm-handle &optional queryp)
  (notmuch-foreach-mime-part
   (lambda (p)
     (let ((disposition (mm-handle-disposition p)))
       (and (listp disposition)
            (or (equal (car disposition) "attachment")
                (and (equal (car disposition) "inline")
                     (assq 'filename disposition)))
            (or (not queryp)
                (y-or-n-p
                 (concat "Save '" (cdr (assq 'filename disposition)) "' ")))
            (mm-save-part p))))
   mm-handle))

(defun notmuch-show-save-attachments ()
  "Save all attachments from the current message."
  (interactive)
  (with-current-notmuch-show-message
   (let ((mm-handle (mm-dissect-buffer)))
     (notmuch-save-attachments
      mm-handle (> (notmuch-count-attachments mm-handle) 1))))
  (message "Done"))

(defun notmuch-reply (query-string)
  (switch-to-buffer (generate-new-buffer "notmuch-draft"))
  (call-process notmuch-command nil t nil "reply" query-string)
  (message-insert-signature)
  (goto-char (point-min))
  (if (re-search-forward "^$" nil t)
      (progn
	(insert "--text follows this line--")
	(forward-line)))
  (message-mode))

(defun notmuch-show-reply ()
  "Begin composing a reply to the current message in a new buffer."
  (interactive)
  (let ((message-id (notmuch-show-get-message-id)))
    (notmuch-reply message-id)))

(defun notmuch-show-forward-current ()
  "Forward the current message."
  (interactive)
  (with-current-notmuch-show-message
   (message-forward)))

(defun notmuch-show-pipe-message (command)
  "Pipe the contents of the current message to the given command.

The given command will be executed with the raw contents of the
current email message as stdin. Anything printed by the command
to stdout or stderr will appear in the *Messages* buffer."
  (interactive "sPipe message to command: ")
  (apply 'start-process-shell-command "notmuch-pipe-command" "*notmuch-pipe*"
	 (list command " < " (shell-quote-argument (notmuch-show-get-filename)))))

(defun notmuch-show-move-to-current-message-summary-line ()
  "Move to the beginning of the one-line summary of the current message.

This gives us a stable place to move to and work from since the
summary line is always visible. This is important since moving to
an invisible location is unreliable, (the main command loop moves
point either forward or backward to the next visible character
when a command ends with point on an invisible character).

Emits an error if point is not within a valid message, (that is
no pattern of `notmuch-show-message-begin-regexp' could be found
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
  "Predicate testing whether current message is unread."
  (member "unread" (notmuch-show-get-tags)))

(defun notmuch-show-message-open-p ()
  "Predicate testing whether current message is open (body is visible)."
  (let ((btn (previous-button (point) t)))
    (while (not (button-has-type-p btn 'notmuch-button-body-toggle-type))
      (setq btn (previous-button (button-start btn))))
    (not (invisible-p (button-get btn 'invisibility-spec)))))

(defun notmuch-show-next-message-without-marking-read ()
  "Advance to the beginning of the next message in the buffer.

Moves to the last visible character of the current message if
already on the last message in the buffer.

Returns nil if already on the last message in the buffer."
  (notmuch-show-move-to-current-message-summary-line)
  (if (re-search-forward notmuch-show-message-begin-regexp nil t)
      (progn
	(notmuch-show-move-to-current-message-summary-line)
	(recenter 0)
	t)
    (goto-char (- (point-max) 1))
    (while (point-invisible-p)
      (backward-char))
    (recenter 0)
    nil))

(defun notmuch-show-next-message ()
  "Advance to the next message (whether open or closed)
and remove the unread tag from that message.

Moves to the last visible character of the current message if
already on the last message in the buffer.

Returns nil if already on the last message in the buffer."
  (interactive)
  (notmuch-show-next-message-without-marking-read)
  (notmuch-show-mark-read))

(defun notmuch-show-find-next-message ()
  "Returns the position of the next message in the buffer.

Or the position of the last visible character of the current
message if already within the last message in the buffer."
  ; save-excursion doesn't save our window position
  ; save-window-excursion doesn't save point
  ; Looks like we have to use both.
  (save-excursion
    (save-window-excursion
      (notmuch-show-next-message-without-marking-read)
      (point))))

(defun notmuch-show-next-unread-message ()
  "Advance to the next unread message.

Moves to the last visible character of the current message if
there are no more unread messages past the current point."
  (notmuch-show-next-message-without-marking-read)
  (while (and (not (notmuch-show-last-message-p))
	      (not (notmuch-show-message-unread-p)))
    (notmuch-show-next-message-without-marking-read))
  (if (not (notmuch-show-message-unread-p))
      (notmuch-show-next-message-without-marking-read))
  (notmuch-show-mark-read))

(defun notmuch-show-next-open-message ()
  "Advance to the next open message (that is, body is visible).

Moves to the last visible character of the final message in the buffer
if there are no more open messages."
  (interactive)
  (while (and (notmuch-show-next-message-without-marking-read)
	      (not (notmuch-show-message-open-p))))
  (notmuch-show-mark-read))

(defun notmuch-show-previous-message-without-marking-read ()
  "Backup to the beginning of the previous message in the buffer.

If within a message rather than at the beginning of it, then
simply move to the beginning of the current message.

Returns nil if already on the first message in the buffer."
  (let ((start (point)))
    (notmuch-show-move-to-current-message-summary-line)
    (if (not (< (point) start))
	; Go backward twice to skip the current message's marker
	(progn
	  (re-search-backward notmuch-show-message-begin-regexp nil t)
	  (re-search-backward notmuch-show-message-begin-regexp nil t)
	  (notmuch-show-move-to-current-message-summary-line)
	  (recenter 0)
	  (if (= (point) start)
	      nil
	    t))
      (recenter 0)
      (nil))))

(defun notmuch-show-previous-message ()
  "Backup to the previous message (whether open or closed)
and remove the unread tag from that message.

If within a message rather than at the beginning of it, then
simply move to the beginning of the current message."
  (interactive)
  (notmuch-show-previous-message-without-marking-read)
  (notmuch-show-mark-read))

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
      (notmuch-show-previous-message-without-marking-read)
      (point))))

(defun notmuch-show-previous-open-message ()
  "Backup to previous open message (that is, body is visible).

Moves to the first message in the buffer if there are no previous
open messages."
  (interactive)
  (while (and (notmuch-show-previous-message-without-marking-read)
	      (not (notmuch-show-message-open-p))))
  (notmuch-show-mark-read))

(defun notmuch-show-rewind ()
  "Backup through the thread, (reverse scrolling compared to \\[notmuch-show-advance-and-archive]).

Specifically, if the beginning of the previous email is fewer
than `window-height' lines from the current point, move to it
just like `notmuch-show-previous-message'.

Otherwise, just scroll down a screenful of the current message.

This command does not modify any message tags, (it does not undo
any effects from previous calls to
`notmuch-show-advance-and-archive'."
  (interactive)
  (let ((previous (notmuch-show-find-previous-message)))
    (if (> (count-lines previous (point)) (- (window-height) next-screen-context-lines))
	(progn
	  (condition-case nil
	      (scroll-down nil)
	    ((beginning-of-buffer) nil))
	  (goto-char (window-start)))
      (notmuch-show-previous-message))))

(defun notmuch-show-advance-and-archive ()
  "Advance through thread and archive.

This command is intended to be one of the simplest ways to
process a thread of email. It does the following:

If the current message in the thread is not yet fully visible,
scroll by a near screenful to read more of the message.

Otherwise, (the end of the current message is already within the
current window), advance to the next open message.

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
	(notmuch-show-next-open-message)
	(if last
	    (notmuch-show-archive-thread))))))

(defun notmuch-show-next-button ()
  "Advance point to the next button in the buffer."
  (interactive)
  (forward-button 1))

(defun notmuch-show-previous-button ()
  "Move point back to the previous button in the buffer."
  (interactive)
  (backward-button 1))

(defun notmuch-toggle-invisible-action (cite-button)
  (let ((invis-spec (button-get cite-button 'invisibility-spec)))
        (if (invisible-p invis-spec)
            (remove-from-invisibility-spec invis-spec)
          (add-to-invisibility-spec invis-spec)
          ))
  (force-window-update)
  (redisplay t))

(defun notmuch-show-toggle-current-body ()
  "Toggle the display of the current message body."
  (interactive)
  (save-excursion
    (notmuch-show-move-to-current-message-summary-line)
    (unless (button-at (point))
      (notmuch-show-next-button))
    (push-button))
  )

(defun notmuch-show-toggle-current-header ()
  "Toggle the display of the current message header."
  (interactive)
  (save-excursion
    (notmuch-show-move-to-current-message-summary-line)
    (forward-line)
    (unless (button-at (point))
      (notmuch-show-next-button))
    (push-button))
  )

(define-button-type 'notmuch-button-invisibility-toggle-type
  'action 'notmuch-toggle-invisible-action
  'follow-link t
  'face 'font-lock-comment-face)
(define-button-type 'notmuch-button-citation-toggle-type 'help-echo "mouse-1, RET: Show citation"
  :supertype 'notmuch-button-invisibility-toggle-type)
(define-button-type 'notmuch-button-signature-toggle-type 'help-echo "mouse-1, RET: Show signature"
  :supertype 'notmuch-button-invisibility-toggle-type)
(define-button-type 'notmuch-button-headers-toggle-type 'help-echo "mouse-1, RET: Show headers"
  :supertype 'notmuch-button-invisibility-toggle-type)
(define-button-type 'notmuch-button-body-toggle-type
  'help-echo "mouse-1, RET: Show message"
  'face 'notmuch-message-summary-face
  :supertype 'notmuch-button-invisibility-toggle-type)

(defun notmuch-show-citation-regexp (depth)
  "Build a regexp for matching citations at a given DEPTH (indent)"
  (let ((line-regexp (format "[[:space:]]\\{%d\\}>.*\n" depth)))
    (concat "\\(?:^" line-regexp
	    "\\(?:[[:space:]]*\n" line-regexp
	    "\\)?\\)+")))

(defun notmuch-show-region-to-button (beg end type prefix button-text)
  "Auxilary function to do the actual making of overlays and buttons

BEG and END are buffer locations. TYPE should a string, either
\"citation\" or \"signature\". PREFIX is some arbitrary text to
insert before the button, probably for indentation.  BUTTON-TEXT
is what to put on the button."

;; This uses some slightly tricky conversions between strings and
;; symbols because of the way the button code works. Note that
;; replacing intern-soft with make-symbol will cause this to fail,
;; since the newly created symbol has no plist.

  (let ((overlay (make-overlay beg end))
	(invis-spec (make-symbol (concat "notmuch-" type "-region")))
	(button-type (intern-soft (concat "notmuch-button-"
					  type "-toggle-type"))))
    (add-to-invisibility-spec invis-spec)
    (overlay-put overlay 'invisible invis-spec)
    (goto-char (1+ end))
    (save-excursion
      (goto-char (1- beg))
      (insert prefix)
      (insert-button button-text
		     'invisibility-spec invis-spec
		     :type button-type)
      )))


(defun notmuch-show-markup-citations-region (beg end depth)
  "Markup citations, and up to one signature in the given region"
  ;; it would be nice if the untabify was not required, but
  ;; that would require notmuch to indent with spaces.
  (untabify beg end)
  (let ((citation-regexp (notmuch-show-citation-regexp depth))
	(signature-regexp (concat (format "^[[:space:]]\\{%d\\}" depth)
				  notmuch-show-signature-regexp))
	(indent (concat "\n" (make-string depth ? ))))
    (goto-char beg)
    (beginning-of-line)
    (while (and (< (point) end)
		(re-search-forward citation-regexp end t))
      (let* ((cite-start (match-beginning 0))
	     (cite-end 	(match-end 0))
	     (cite-lines (count-lines cite-start cite-end)))
	(when (> cite-lines (1+ notmuch-show-citation-lines-prefix))
	  (goto-char cite-start)
	  (forward-line notmuch-show-citation-lines-prefix)
	  (notmuch-show-region-to-button
	   (point) cite-end
	   "citation"
	   indent
	   (format notmuch-show-citation-button-format
		   (- cite-lines notmuch-show-citation-lines-prefix))
	   ))))
    (if (and (< (point) end)
	     (re-search-forward signature-regexp end t))
	(let* ((sig-start (match-beginning 0))
	       (sig-end (match-end 0))
	       (sig-lines (1- (count-lines sig-start end))))
	  (if (<= sig-lines notmuch-show-signature-lines-max)
	      (notmuch-show-region-to-button
	       sig-start
	       end
	       "signature"
	       indent
	       (format notmuch-show-signature-button-format sig-lines)
	       ))))))

(defun notmuch-show-markup-part (beg end depth)
  (if (re-search-forward notmuch-show-part-begin-regexp nil t)
      (progn
        (let (mime-message mime-type)
          (save-excursion
            (re-search-forward notmuch-show-contentype-regexp end t)
            (setq mime-type (car (split-string (buffer-substring
                                                (match-beginning 1) (match-end 1))))))

          (if (equal mime-type "text/html")
              (let ((filename (notmuch-show-get-filename)))
                (with-temp-buffer
                  (insert-file-contents filename nil nil nil t)
                  (setq mime-message (mm-dissect-buffer)))))
          (forward-line)
          (let ((beg (point-marker)))
            (re-search-forward notmuch-show-part-end-regexp)
            (let ((end (copy-marker (match-beginning 0))))
              (goto-char end)
              (if (not (bolp))
                  (insert "\n"))
              (indent-rigidly beg end depth)
              (if (not (eq mime-message nil))
                  (save-excursion
                    (goto-char beg)
                    (forward-line -1)
                    (let ((handle-type (mm-handle-type mime-message))
                          mime-type)
                      (if (sequencep (car handle-type))
                          (setq mime-type (car handle-type))
                        (setq mime-type (car (car (cdr handle-type))))
                        )
                      (if (equal mime-type "text/html")
                          (mm-display-part mime-message))))
                )
              (notmuch-show-markup-citations-region beg end depth)
              ; Advance to the next part (if any) (so the outer loop can
              ; determine whether we've left the current message.
              (if (re-search-forward notmuch-show-part-begin-regexp nil t)
                  (beginning-of-line)))))
        (goto-char end))
    (goto-char end)))

(defun notmuch-show-markup-parts-region (beg end depth)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (notmuch-show-markup-part beg end depth))))

(defun notmuch-show-markup-body (depth match btn)
  "Markup a message body, (indenting, buttonizing citations,
etc.), and hiding the body itself if the message does not match
the current search.

DEPTH specifies the depth at which this message appears in the
tree of the current thread, (the top-level messages have depth 0
and each reply increases depth by 1). MATCH indicates whether
this message is regarded as matching the current search. BTN is
the button which is used to toggle the visibility of this
message.

When this function is called, point must be within the message, but
before the delimiter marking the beginning of the body."
  (re-search-forward notmuch-show-body-begin-regexp)
  (forward-line)
  (let ((beg (point-marker)))
    (re-search-forward notmuch-show-body-end-regexp)
    (let ((end (copy-marker (match-beginning 0))))
      (notmuch-show-markup-parts-region beg end depth)
      (let ((invis-spec (make-symbol "notmuch-show-body-read")))
        (overlay-put (make-overlay beg end)
                     'invisible invis-spec)
        (button-put btn 'invisibility-spec invis-spec)
        (if (not match)
            (add-to-invisibility-spec invis-spec)))
      (set-marker beg nil)
      (set-marker end nil)
      )))

(defun notmuch-fontify-headers ()
  (while (looking-at "[[:space:]]")
    (forward-char))
  (if (looking-at "[Tt]o:")
      (progn
	(overlay-put (make-overlay (point) (re-search-forward ":"))
		     'face 'message-header-name)
	(overlay-put (make-overlay (point) (re-search-forward ".*$"))
		     'face 'message-header-to))
    (if (looking-at "[B]?[Cc][Cc]:")
	(progn
	  (overlay-put (make-overlay (point) (re-search-forward ":"))
		       'face 'message-header-name)
	  (overlay-put (make-overlay (point) (re-search-forward ".*$"))
		       'face 'message-header-cc))
      (if (looking-at "[Ss]ubject:")
	  (progn
	    (overlay-put (make-overlay (point) (re-search-forward ":"))
			 'face 'message-header-name)
	    (overlay-put (make-overlay (point) (re-search-forward ".*$"))
			 'face 'message-header-subject))
	(if (looking-at "[Ff]rom:")
	    (progn
	      (overlay-put (make-overlay (point) (re-search-forward ":"))
			   'face 'message-header-name)
	      (overlay-put (make-overlay (point) (re-search-forward ".*$"))
			   'face 'message-header-other)))))))

(defun notmuch-show-markup-header (message-begin depth)
  "Buttonize and decorate faces in a message header.

MESSAGE-BEGIN is the position of the absolute first character in
the message (including all delimiters that will end up being
invisible etc.). This is to allow a button to reliably extend to
the beginning of the message even if point is positioned at an
invisible character (such as the beginning of the buffer).

DEPTH specifies the depth at which this message appears in the
tree of the current thread, (the top-level messages have depth 0
and each reply increases depth by 1)."
  (re-search-forward notmuch-show-header-begin-regexp)
  (forward-line)
  (let ((beg (point-marker))
	(summary-end (copy-marker (line-beginning-position 2)))
	(subject-end (copy-marker (line-end-position 2)))
	(invis-spec (make-symbol "notmuch-show-header"))
        (btn nil))
    (re-search-forward notmuch-show-header-end-regexp)
    (beginning-of-line)
    (let ((end (point-marker)))
      (indent-rigidly beg end depth)
      (goto-char beg)
      (setq btn (make-button message-begin summary-end :type 'notmuch-button-body-toggle-type))
      (forward-line)
      (add-to-invisibility-spec invis-spec)
      (overlay-put (make-overlay subject-end end)
		   'invisible invis-spec)
      (make-button (line-beginning-position) subject-end
		   'invisibility-spec invis-spec
		   :type 'notmuch-button-headers-toggle-type)
      (while (looking-at "[[:space:]]*[A-Za-z][-A-Za-z0-9]*:")
	(beginning-of-line)
	(notmuch-fontify-headers)
	(forward-line)
	)
      (goto-char end)
      (insert "\n")
      (set-marker beg nil)
      (set-marker summary-end nil)
      (set-marker subject-end nil)
      (set-marker end nil)
      )
  btn))

(defun notmuch-show-markup-message ()
  (if (re-search-forward notmuch-show-message-begin-regexp nil t)
      (let ((message-begin (match-beginning 0)))
	(re-search-forward notmuch-show-depth-match-regexp)
	(let ((depth (string-to-number (buffer-substring (match-beginning 1) (match-end 1))))
	      (match (string= "1" (buffer-substring (match-beginning 2) (match-end 2))))
              (btn nil))
	  (setq btn (notmuch-show-markup-header message-begin depth))
	  (notmuch-show-markup-body depth match btn)))
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

(defun notmuch-documentation-first-line (symbol)
  "Return the first line of the documentation string for SYMBOL."
  (let ((doc (documentation symbol)))
    (if doc
	(with-temp-buffer
	  (insert (documentation symbol t))
	  (goto-char (point-min))
	  (let ((beg (point)))
	    (end-of-line)
	    (buffer-substring beg (point))))
      "")))

(defun notmuch-prefix-key-description (key)
  "Given a prefix key code, return a human-readable string representation.

This is basically just `format-kbd-macro' but we also convert ESC to M-."
  (let ((desc (format-kbd-macro (vector key))))
    (if (string= desc "ESC")
	"M-"
      (concat desc " "))))

; I would think that emacs would have code handy for walking a keymap
; and generating strings for each key, and I would prefer to just call
; that. But I couldn't find any (could be all implemented in C I
; suppose), so I wrote my own here.
(defun notmuch-substitute-one-command-key-with-prefix (prefix binding)
  "For a key binding, return a string showing a human-readable
representation of the prefixed key as well as the first line of
documentation from the bound function.

For a mouse binding, return nil."
  (let ((key (car binding))
	(action (cdr binding)))
    (if (mouse-event-p key)
	nil
      (if (keymapp action)
	  (let ((substitute (apply-partially 'notmuch-substitute-one-command-key-with-prefix (notmuch-prefix-key-description key)))
		(as-list))
	    (map-keymap (lambda (a b)
			  (push (cons a b) as-list))
			action)
	    (mapconcat substitute as-list "\n"))
	(concat prefix (format-kbd-macro (vector key))
		"\t"
		(notmuch-documentation-first-line action))))))

(defalias 'notmuch-substitute-one-command-key
  (apply-partially 'notmuch-substitute-one-command-key-with-prefix nil))

(defun notmuch-substitute-command-keys (doc)
  "Like `substitute-command-keys' but with documentation, not function names."
  (let ((beg 0))
    (while (string-match "\\\\{\\([^}[:space:]]*\\)}" doc beg)
      (let ((map (substring doc (match-beginning 1) (match-end 1))))
	(setq doc (replace-match (mapconcat 'notmuch-substitute-one-command-key
					    (cdr (symbol-value (intern map))) "\n") 1 1 doc)))
      (setq beg (match-end 0)))
    doc))

(defun notmuch-help ()
  "Display help for the current notmuch mode."
  (interactive)
  (let* ((mode major-mode)
	 (doc (substitute-command-keys (notmuch-substitute-command-keys (documentation mode t)))))
    (with-current-buffer (generate-new-buffer "*notmuch-help*")
      (insert doc)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (view-buffer (current-buffer) 'kill-buffer-if-not-modified))))

;;;###autoload
(defun notmuch-show-mode ()
  "Major mode for viewing a thread with notmuch.

This buffer contains the results of the \"notmuch show\" command
for displaying a single thread of email from your email archives.

By default, various components of email messages, (citations,
signatures, already-read messages), are hidden. You can make
these parts visible by clicking with the mouse button or by
pressing RET after positioning the cursor on a hidden part, (for
which \\[notmuch-show-next-button] and \\[notmuch-show-previous-button] are helpful).

Reading the thread sequentially is well-supported by pressing
\\[notmuch-show-advance-and-archive]. This will
scroll the current message (if necessary), advance to the next
message, or advance to the next thread (if already on the last
message of a thread).

Other commands are available to read or manipulate the thread more
selectively, (such as '\\[notmuch-show-next-message]' and '\\[notmuch-show-previous-message]' to advance to messages without
removing any tags, and '\\[notmuch-show-archive-thread]' to archive an entire thread without
scrolling through with \\[notmuch-show-advance-and-archive]).

You can add or remove arbitary tags from the current message with
'\\[notmuch-show-add-tag]' or '\\[notmuch-show-remove-tag]'.

All currently available key bindings:

\\{notmuch-show-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (add-to-invisibility-spec 'notmuch-show-marker)
  (use-local-map notmuch-show-mode-map)
  (setq major-mode 'notmuch-show-mode
	mode-name "notmuch-show")
  (setq buffer-read-only t))

(defgroup notmuch nil
  "Notmuch mail reader for Emacs."
  :group 'mail)

(defcustom notmuch-show-hook nil
  "List of functions to call when notmuch displays a message."
  :type 'hook
  :options '(goto-address)
  :group 'notmuch)

(defcustom notmuch-search-hook nil
  "List of functions to call when notmuch displays the search results."
  :type 'hook
  :options '(hl-line-mode)
  :group 'notmuch)

(defun notmuch-show-do-stash (text)
    (kill-new text)
    (message (concat "Saved: " text)))

(defun notmuch-show-stash-cc ()
  "Copy CC field of current message to kill-ring."
  (interactive)
  (notmuch-show-do-stash (notmuch-show-get-cc)))

(defun notmuch-show-stash-date ()
  "Copy date of current message to kill-ring."
  (interactive)
  (notmuch-show-do-stash (notmuch-show-get-date)))

(defun notmuch-show-stash-filename ()
  "Copy filename of current message to kill-ring."
  (interactive)
  (notmuch-show-do-stash (notmuch-show-get-filename)))

(defun notmuch-show-stash-from ()
  "Copy From address of current message to kill-ring."
  (interactive)
  (notmuch-show-do-stash (notmuch-show-get-from)))

(defun notmuch-show-stash-message-id ()
  "Copy message ID of current message to kill-ring."
  (interactive)
  (notmuch-show-do-stash (notmuch-show-get-message-id)))

(defun notmuch-show-stash-subject ()
  "Copy Subject field of current message to kill-ring."
  (interactive)
  (notmuch-show-do-stash (notmuch-show-get-subject)))

(defun notmuch-show-stash-tags ()
  "Copy tags of current message to kill-ring as a comma separated list."
  (interactive)
  (notmuch-show-do-stash (mapconcat 'identity (notmuch-show-get-tags) ",")))

(defun notmuch-show-stash-to ()
  "Copy To address of current message to kill-ring."
  (interactive)
  (notmuch-show-do-stash (notmuch-show-get-to)))

; Make show mode a bit prettier, highlighting URLs and using word wrap

(defun notmuch-show-mark-read ()
  (notmuch-show-remove-tag "unread"))

(defun notmuch-show-pretty-hook ()
  (goto-address-mode 1)
  (visual-line-mode))

(add-hook 'notmuch-show-hook 'notmuch-show-mark-read)
(add-hook 'notmuch-show-hook 'notmuch-show-pretty-hook)
(add-hook 'notmuch-search-hook
	  (lambda()
	    (hl-line-mode 1) ))

(defun notmuch-show (thread-id &optional parent-buffer query-context)
  "Run \"notmuch show\" with the given thread ID and display results.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the next
thread from that buffer can be show when done with this one).

The optional QUERY-CONTEXT is a notmuch search term. Only messages from the thread
matching this search term are shown if non-nil. "
  (interactive "sNotmuch show: ")
  (let ((buffer (get-buffer-create (concat "*notmuch-show-" thread-id "*"))))
    (switch-to-buffer buffer)
    (notmuch-show-mode)
    (set (make-local-variable 'notmuch-show-parent-buffer) parent-buffer)
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (if proc
	  (error "notmuch search process already running for query `%s'" thread-id)
	)
      (erase-buffer)
      (goto-char (point-min))
      (save-excursion
	(let* ((basic-args (list notmuch-command nil t nil "show" "--entire-thread" thread-id))
		(args (if query-context (append basic-args (list "and (" query-context ")")) basic-args)))
	  (apply 'call-process args)
	  (when (and (eq (buffer-size) 0) query-context)
	    (apply 'call-process basic-args)))
	(notmuch-show-markup-messages)
	)
      (run-hooks 'notmuch-show-hook)
      ; Move straight to the first open message
      (if (not (notmuch-show-message-open-p))
	  (notmuch-show-next-open-message))
      )))

(defvar notmuch-search-authors-width 40
  "Number of columns to use to display authors in a notmuch-search buffer.")

(defvar notmuch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'notmuch-help)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "x" 'kill-this-buffer)
    (define-key map (kbd "<DEL>") 'notmuch-search-scroll-down)
    (define-key map "b" 'notmuch-search-scroll-down)
    (define-key map " " 'notmuch-search-scroll-up)
    (define-key map "<" 'notmuch-search-first-thread)
    (define-key map ">" 'notmuch-search-last-thread)
    (define-key map "p" 'notmuch-search-previous-thread)
    (define-key map "n" 'notmuch-search-next-thread)
    (define-key map "r" 'notmuch-search-reply-to-thread)
    (define-key map "m" 'message-mail)
    (define-key map "s" 'notmuch-search)
    (define-key map "o" 'notmuch-search-toggle-order)
    (define-key map "=" 'notmuch-search-refresh-view)
    (define-key map "t" 'notmuch-search-filter-by-tag)
    (define-key map "f" 'notmuch-search-filter)
    (define-key map [mouse-1] 'notmuch-search-show-thread)
    (define-key map "*" 'notmuch-search-operate-all)
    (define-key map "a" 'notmuch-search-archive-thread)
    (define-key map "-" 'notmuch-search-remove-tag)
    (define-key map "+" 'notmuch-search-add-tag)
    (define-key map (kbd "RET") 'notmuch-search-show-thread)
    map)
  "Keymap for \"notmuch search\" buffers.")
(fset 'notmuch-search-mode-map notmuch-search-mode-map)

(defvar notmuch-search-query-string)
(defvar notmuch-search-oldest-first t
  "Show the oldest mail first in the search-mode")

(defvar notmuch-search-disjunctive-regexp      "\\<[oO][rR]\\>")

(defun notmuch-search-scroll-up ()
  "Move forward through search results by one window's worth."
  (interactive)
  (condition-case nil
      (scroll-up nil)
    ((end-of-buffer) (notmuch-search-last-thread))))

(defun notmuch-search-scroll-down ()
  "Move backward through the search results by one window's worth."
  (interactive)
  ; I don't know why scroll-down doesn't signal beginning-of-buffer
  ; the way that scroll-up signals end-of-buffer, but c'est la vie.
  ;
  ; So instead of trapping a signal we instead check whether the
  ; window begins on the first line of the buffer and if so, move
  ; directly to that position. (We have to count lines since the
  ; window-start position is not the same as point-min due to the
  ; invisible thread-ID characters on the first line.
  (if (equal (count-lines (point-min) (window-start)) 0)
      (goto-char (point-min))
    (scroll-down nil)))

(defun notmuch-search-next-thread ()
  "Select the next thread in the search results."
  (interactive)
  (forward-line 1))

(defun notmuch-search-previous-thread ()
  "Select the previous thread in the search results."
  (interactive)
  (forward-line -1))

(defun notmuch-search-last-thread ()
  "Select the last thread in the search results."
  (interactive)
  (goto-char (point-max))
  (forward-line -2))

(defun notmuch-search-first-thread ()
  "Select the first thread in the search results."
  (interactive)
  (goto-char (point-min)))

(defface notmuch-message-summary-face
 '((((class color) (background light)) (:background "#f0f0f0"))
   (((class color) (background dark)) (:background "#303030")))
 "Face for the single-line message summary in notmuch-show-mode."
 :group 'notmuch)

(defface notmuch-tag-face
  '((((class color)
      (background dark))
     (:foreground "OliveDrab1"))
    (((class color)
      (background light))
     (:foreground "navy blue" :bold t))
    (t
     (:bold t)))
  "Notmuch search mode face used to highligh tags."
  :group 'notmuch)

(defvar notmuch-tag-face-alist nil
  "List containing the tag list that need to be highlighed")

(defvar notmuch-search-font-lock-keywords  nil)

;;;###autoload
(defun notmuch-search-mode ()
  "Major mode displaying results of a notmuch search.

This buffer contains the results of a \"notmuch search\" of your
email archives. Each line in the buffer represents a single
thread giving a summary of the thread (a relative date, the
number of matched messages and total messages in the thread,
participants in the thread, a representative subject line, and
any tags).

Pressing \\[notmuch-search-show-thread] on any line displays that thread. The '\\[notmuch-search-add-tag]' and '\\[notmuch-search-remove-tag]'
keys can be used to add or remove tags from a thread. The '\\[notmuch-search-archive-thread]' key
is a convenience for archiving a thread (removing the \"inbox\"
tag). The '\\[notmuch-search-operate-all]' key can be used to add or remove a tag from all
threads in the current buffer.

Other useful commands are '\\[notmuch-search-filter]' for filtering the current search
based on an additional query string, '\\[notmuch-search-filter-by-tag]' for filtering to include
only messages with a given tag, and '\\[notmuch-search]' to execute a new, global
search.

Complete list of currently available key bindings:

\\{notmuch-search-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'notmuch-search-query-string)
  (make-local-variable 'notmuch-search-oldest-first)
  (set (make-local-variable 'scroll-preserve-screen-position) t)
  (add-to-invisibility-spec 'notmuch-search)
  (use-local-map notmuch-search-mode-map)
  (setq truncate-lines t)
  (setq major-mode 'notmuch-search-mode
	mode-name "notmuch-search")
  (setq buffer-read-only t)
  (if (not notmuch-tag-face-alist)
      (add-to-list 'notmuch-search-font-lock-keywords (list
		"(\\([^)]*\\))$" '(1  'notmuch-tag-face)))
    (let ((notmuch-search-tags (mapcar 'car notmuch-tag-face-alist)))
      (loop for notmuch-search-tag  in notmuch-search-tags
	    do (add-to-list 'notmuch-search-font-lock-keywords (list
			(concat "([^)]*\\(" notmuch-search-tag "\\)[^)]*)$")
			`(1  ,(cdr (assoc notmuch-search-tag notmuch-tag-face-alist))))))))
  (set (make-local-variable 'font-lock-defaults)
         '(notmuch-search-font-lock-keywords t)))

(defun notmuch-search-find-thread-id ()
  "Return the thread for the current thread"
  (get-text-property (point) 'notmuch-search-thread-id))

(defun notmuch-search-find-authors ()
  "Return the authors for the current thread"
  (get-text-property (point) 'notmuch-search-authors))

(defun notmuch-search-find-subject ()
  "Return the subject for the current thread"
  (get-text-property (point) 'notmuch-search-subject))

(defun notmuch-search-show-thread ()
  "Display the currently selected thread."
  (interactive)
  (let ((thread-id (notmuch-search-find-thread-id)))
    (if (> (length thread-id) 0)
	(notmuch-show thread-id (current-buffer) notmuch-search-query-string)
      (error "End of search results"))))

(defun notmuch-search-reply-to-thread ()
  "Begin composing a reply to the entire current thread in a new buffer."
  (interactive)
  (let ((message-id (notmuch-search-find-thread-id)))
    (notmuch-reply message-id)))

(defun notmuch-call-notmuch-process (&rest args)
  "Synchronously invoke \"notmuch\" with the given list of arguments.

Output from the process will be presented to the user as an error
and will also appear in a buffer named \"*Notmuch errors*\"."
  (let ((error-buffer (get-buffer-create "*Notmuch errors*")))
    (with-current-buffer error-buffer
	(erase-buffer))
    (if (eq (apply 'call-process notmuch-command nil error-buffer nil args) 0)
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
  "Add a tag to the currently selected thread.

The tag is added to messages in the currently selected thread
which match the current search terms."
  (interactive
   (list (notmuch-select-tag-with-completion "Tag to add: ")))
  (notmuch-call-notmuch-process "tag" (concat "+" tag) (notmuch-search-find-thread-id))
  (notmuch-search-set-tags (delete-dups (sort (cons tag (notmuch-search-get-tags)) 'string<))))

(defun notmuch-search-remove-tag (tag)
  "Remove a tag from the currently selected thread.

The tag is removed from messages in the currently selected thread
which match the current search terms."
  (interactive
   (list (notmuch-select-tag-with-completion "Tag to remove: " (notmuch-search-find-thread-id))))
  (notmuch-call-notmuch-process "tag" (concat "-" tag) (notmuch-search-find-thread-id))
  (notmuch-search-set-tags (delete tag (notmuch-search-get-tags))))

(defun notmuch-search-archive-thread ()
  "Archive the currently selected thread (remove its \"inbox\" tag).

This function advances the next thread when finished."
  (interactive)
  (notmuch-search-remove-tag "inbox")
  (forward-line))

(defun notmuch-search-process-sentinel (proc msg)
  "Add a message to let user know when \"notmuch search\" exits"
  (let ((buffer (process-buffer proc))
	(status (process-status proc))
	(exit-status (process-exit-status proc)))
    (if (memq status '(exit signal))
	(if (buffer-live-p buffer)
	    (with-current-buffer buffer
	      (save-excursion
		(let ((inhibit-read-only t))
		  (goto-char (point-max))
		  (if (eq status 'signal)
		      (insert "Incomplete search results (search process was killed).\n"))
		  (if (eq status 'exit)
		      (progn
			(insert "End of search results.")
			(if (not (= exit-status 0))
			    (insert (format " (process returned %d)" exit-status)))
			(insert "\n"))))))))))

(defun notmuch-search-process-filter (proc string)
  "Process and filter the output of \"notmuch search\""
  (let ((buffer (process-buffer proc)))
    (if (buffer-live-p buffer)
	(with-current-buffer buffer
	  (save-excursion
	    (let ((line 0)
		  (more t)
		  (inhibit-read-only t))
	      (while more
		(if (string-match "^\\(thread:[0-9A-Fa-f]*\\) \\(.*\\) \\(\\[[0-9/]*\\]\\) \\([^;]*\\); \\(.*\\) (\\([^()]*\\))$" string line)
		    (let* ((thread-id (match-string 1 string))
			   (date (match-string 2 string))
			   (count (match-string 3 string))
			   (authors (match-string 4 string))
			   (authors-length (length authors))
			   (subject (match-string 5 string))
			   (tags (match-string 6 string)))
		      (if (> authors-length 40)
			  (set 'authors (concat (substring authors 0 (- 40 3)) "...")))
		      (goto-char (point-max))
		      (let ((beg (point-marker)))
			(insert (format "%s %-7s %-40s %s (%s)\n" date count authors subject tags))
			(put-text-property beg (point-marker) 'notmuch-search-thread-id thread-id)
			(put-text-property beg (point-marker) 'notmuch-search-authors authors)
			(put-text-property beg (point-marker) 'notmuch-search-subject subject))
		      (set 'line (match-end 0)))
		  (set 'more nil))))))
      (delete-process proc))))

(defun notmuch-search-operate-all (action)
  "Add/remove tags from all matching messages.

Tis command adds or removes tags from all messages matching the
current search terms. When called interactively, this command
will prompt for tags to be added or removed. Tags prefixed with
'+' will be added and tags prefixed with '-' will be removed.

Each character of the tag name may consist of alphanumeric
characters as well as `_.+-'.
"
  (interactive "sOperation (+add -drop): notmuch tag ")
  (let ((action-split (split-string action " +")))
    ;; Perform some validation
    (let ((words action-split))
      (when (null words) (error "No operation given"))
      (while words
	(unless (string-match-p "^[-+][-+_.[:word:]]+$" (car words))
	  (error "Action must be of the form `+thistag -that_tag'"))
	(setq words (cdr words))))
    (apply 'notmuch-call-notmuch-process "tag"
	   (append action-split (list notmuch-search-query-string) nil))))

;;;###autoload
(defun notmuch-search (query &optional oldest-first)
  "Run \"notmuch search\" with the given query string and display results."
  (interactive "sNotmuch search: ")
  (let ((buffer (get-buffer-create (concat "*notmuch-search-" query "*"))))
    (switch-to-buffer buffer)
    (notmuch-search-mode)
    (set 'notmuch-search-query-string query)
    (set 'notmuch-search-oldest-first oldest-first)
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (if proc
	  (error "notmuch search process already running for query `%s'" query)
	)
      (erase-buffer)
      (goto-char (point-min))
      (save-excursion
	(let ((proc (start-process-shell-command
		     "notmuch-search" buffer notmuch-command "search"
		     (if oldest-first "--sort=oldest-first" "--sort=newest-first")
		     (shell-quote-argument query))))
	  (set-process-sentinel proc 'notmuch-search-process-sentinel)
	  (set-process-filter proc 'notmuch-search-process-filter))))
    (run-hooks 'notmuch-search-hook)))

(defun notmuch-search-refresh-view ()
  "Refresh the current view.

Kills the current buffer and runs a new search with the same
query string as the current search. If the current thread is in
the new search results, then point will be placed on the same
thread. Otherwise, point will be moved to attempt to be in the
same relative position within the new buffer."
  (interactive)
  (let ((here (point))
	(oldest-first notmuch-search-oldest-first)
	(thread (notmuch-search-find-thread-id))
	(query notmuch-search-query-string))
    (kill-this-buffer)
    (notmuch-search query oldest-first)
    (goto-char (point-min))
    (if (re-search-forward (concat "^" thread) nil t)
	(beginning-of-line)
      (goto-char here))))

(defun notmuch-search-toggle-order ()
  "Toggle the current search order.

By default, the \"inbox\" view created by `notmuch' is displayed
in chronological order (oldest thread at the beginning of the
buffer), while any global searches created by `notmuch-search'
are displayed in reverse-chronological order (newest thread at
the beginning of the buffer).

This command toggles the sort order for the current search.

Note that any filtered searches created by
`notmuch-search-filter' retain the search order of the parent
search."
  (interactive)
  (set 'notmuch-search-oldest-first (not notmuch-search-oldest-first))
  (notmuch-search-refresh-view))

(defun notmuch-search-filter (query)
  "Filter the current search results based on an additional query string.

Runs a new search matching only messages that match both the
current search results AND the additional query string provided."
  (interactive "sFilter search: ")
  (let ((grouped-query (if (string-match-p notmuch-search-disjunctive-regexp query) (concat "( " query " )") query)))
    (notmuch-search (concat notmuch-search-query-string " and " grouped-query) notmuch-search-oldest-first)))

(defun notmuch-search-filter-by-tag (tag)
  "Filter the current search results based on a single tag.

Runs a new search matching only messages that match both the
current search results AND that are tagged with the given tag."
  (interactive
   (list (notmuch-select-tag-with-completion "Filter by tag: ")))
  (notmuch-search (concat notmuch-search-query-string " and tag:" tag) notmuch-search-oldest-first))


;;;###autoload
(defun notmuch ()
  "Run notmuch to display all mail with tag of 'inbox'"
  (interactive)
  (notmuch-search "tag:inbox" notmuch-search-oldest-first))

(setq mail-user-agent 'message-user-agent)

(defvar notmuch-folder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'notmuch-help)
    (define-key map "x" 'kill-this-buffer)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "m" 'message-mail)
    (define-key map "e" 'notmuch-folder-show-empty-toggle)
    (define-key map ">" 'notmuch-folder-last)
    (define-key map "<" 'notmuch-folder-first)
    (define-key map "=" 'notmuch-folder)
    (define-key map "s" 'notmuch-search)
    (define-key map [mouse-1] 'notmuch-folder-show-search)
    (define-key map (kbd "RET") 'notmuch-folder-show-search)
    (define-key map " " 'notmuch-folder-show-search)
    (define-key map "p" 'notmuch-folder-previous)
    (define-key map "n" 'notmuch-folder-next)
    map)
  "Keymap for \"notmuch folder\" buffers.")

(fset 'notmuch-folder-mode-map notmuch-folder-mode-map)

(defcustom notmuch-folders (quote (("inbox" . "tag:inbox") ("unread" . "tag:unread")))
  "List of searches for the notmuch folder view"
  :type '(alist :key-type (string) :value-type (string))
  :group 'notmuch)

(defun notmuch-folder-mode ()
  "Major mode for showing notmuch 'folders'.

This buffer contains a list of message counts returned by a
customizable set of searches of your email archives. Each line in
the buffer shows the name of a saved search and the resulting
message count.

Pressing RET on any line opens a search window containing the
results for the saved search on that line.

Here is an example of how the search list could be
customized, (the following text would be placed in your ~/.emacs
file):

(setq notmuch-folders '((\"inbox\" . \"tag:inbox\")
                        (\"unread\" . \"tag:inbox AND tag:unread\")
                        (\"notmuch\" . \"tag:inbox AND to:notmuchmail.org\")))

Of course, you can have any number of folders, each configured
with any supported search terms (see \"notmuch help search-terms\").

Currently available key bindings:

\\{notmuch-folder-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map 'notmuch-folder-mode-map)
  (setq truncate-lines t)
  (hl-line-mode 1)
  (setq major-mode 'notmuch-folder-mode
	mode-name "notmuch-folder")
  (setq buffer-read-only t))

(defun notmuch-folder-next ()
  "Select the next folder in the list."
  (interactive)
  (forward-line 1)
  (if (eobp)
      (forward-line -1)))

(defun notmuch-folder-previous ()
  "Select the previous folder in the list."
  (interactive)
  (forward-line -1))

(defun notmuch-folder-first ()
  "Select the first folder in the list."
  (interactive)
  (goto-char (point-min)))

(defun notmuch-folder-last ()
  "Select the last folder in the list."
  (interactive)
  (goto-char (point-max))
  (forward-line -1))

(defun notmuch-folder-count (search)
  (car (process-lines notmuch-command "count" search)))

(setq notmuch-folder-show-empty t)

(defun notmuch-folder-show-empty-toggle ()
  "Toggle the listing of empty folders"
  (interactive)
  (setq notmuch-folder-show-empty (not notmuch-folder-show-empty))
  (notmuch-folder))

(defun notmuch-folder-add (folders)
  (if folders
      (let* ((name (car (car folders)))
	    (inhibit-read-only t)
	    (search (cdr (car folders)))
	    (count (notmuch-folder-count search)))
	(if (or notmuch-folder-show-empty
		(not (equal count "0")))
	    (progn
	      (insert name)
	      (indent-to 16 1)
	      (insert count)
	      (insert "\n")
	      )
	  )
	(notmuch-folder-add (cdr folders)))))

(defun notmuch-folder-find-name ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (re-search-forward "\\([ \t]*[^ \t]+\\)")
      (filter-buffer-substring (match-beginning 1) (match-end 1)))))

(defun notmuch-folder-show-search (&optional folder)
  "Show a search window for the search related to the specified folder."
  (interactive)
  (if (null folder)
      (setq folder (notmuch-folder-find-name)))
  (let ((search (assoc folder notmuch-folders)))
    (if search
	(notmuch-search (cdr search) notmuch-search-oldest-first))))

;;;###autoload
(defun notmuch-folder ()
  "Show the notmuch folder view and update the displayed counts."
  (interactive)
  (let ((buffer (get-buffer-create "*notmuch-folders*")))
    (switch-to-buffer buffer)
    (let ((inhibit-read-only t)
	  (n (line-number-at-pos)))
      (erase-buffer)
      (notmuch-folder-mode)
      (notmuch-folder-add notmuch-folders)
      (goto-char (point-min))
      (goto-line n))))

(provide 'notmuch)
