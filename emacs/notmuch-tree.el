;;; notmuch-tree.el --- displaying notmuch forests  -*- lexical-binding: t -*-
;;
;; Copyright © Carl Worth
;; Copyright © David Edmondson
;; Copyright © Mark Walters
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
;;          Mark Walters <markwalters1009@gmail.com>

;;; Code:

(require 'mail-parse)

(require 'notmuch-lib)
(require 'notmuch-show)
(require 'notmuch-tag)
(require 'notmuch-parser)
(require 'notmuch-jump)

(declare-function notmuch-search "notmuch"
		  (&optional query oldest-first target-thread target-line
			     no-display))
(declare-function notmuch-call-notmuch-process "notmuch-lib" (&rest args))
(declare-function notmuch-read-query "notmuch" (prompt))
(declare-function notmuch-search-find-thread-id "notmuch" (&optional bare))
(declare-function notmuch-search-find-subject "notmuch" ())

;; For `notmuch-tree-next-thread-from-search'.
(declare-function notmuch-search-next-thread "notmuch" ())
(declare-function notmuch-search-previous-thread "notmuch" ())
(declare-function notmuch-tree-from-search-thread "notmuch" ())

;; this variable distinguishes the unthreaded display from the normal tree display
(defvar-local notmuch-tree-unthreaded nil
  "A buffer local copy of argument unthreaded to the function notmuch-tree.")

;;; Options

(defgroup notmuch-tree nil
  "Showing message and thread structure."
  :group 'notmuch)

(defcustom notmuch-tree-show-out nil
  "View selected messages in new window rather than split-pane."
  :type 'boolean
  :group 'notmuch-tree)

(defcustom notmuch-unthreaded-show-out t
  "View selected messages in new window rather than split-pane."
  :type 'boolean
  :group 'notmuch-tree)

(defun notmuch-tree-show-out ()
  (if notmuch-tree-unthreaded
      notmuch-unthreaded-show-out
    notmuch-tree-show-out))

(defcustom notmuch-tree-thread-symbols
  '((prefix . " ")
    (top . "─")
    (top-tee . "┬")
    (vertical . "│")
    (vertical-tee . "├")
    (bottom . "╰")
    (arrow . "►"))
  "Strings used to draw trees in notmuch tree results.
Symbol keys denote where the corresponding string value is used:
`prefix' is used at the top of the tree, followed by `top' if it
has no children or `top-tee' if it does; `vertical' is a bar
connecting with a response down the list skipping the current
one, while `vertical-tee' marks the current message as a reply to
the previous one; `bottom' is used at the bottom of threads.
Finally, the `arrrow' string in the list is used as a pointer to
every message.

Common customizations include setting `prefix' to \"-\", to see
equal-length prefixes, and `arrow' to an empty string or to a
different kind of arrow point."
  :type '(alist :key-type symbol :value-type string)
  :group 'notmuch-tree)

(defconst notmuch-tree--field-names
  '(choice :tag "Field"
	   (const :tag "Date" "date")
	   (const :tag "Authors" "authors")
	   (const :tag "Subject" "subject")
	   (const :tag "Tree" "tree")
	   (const :tag "Tags" "tags")
	   (function)))

(defcustom notmuch-tree-result-format
  `(("date" . "%12s  ")
    ("authors" . "%-20s")
    ((("tree" . "%s")
      ("subject" . "%s"))
     . " %-54s ")
    ("tags" . "(%s)"))
  "Result formatting for tree view.

List of pairs of (field . format-string).  Supported field
strings are: \"date\", \"authors\", \"subject\", \"tree\",
\"tags\".  It is also supported to pass a function in place of a
field-name. In this case the function is passed the thread
object (plist) and format string.

Tree means the thread tree box graphics. The field may
also be a list in which case the formatting rules are
applied recursively and then the output of all the fields
in the list is inserted according to format-string.

Note that the author string should not contain whitespace
\(put it in the neighbouring fields instead)."

  :type `(alist :key-type (choice ,notmuch-tree--field-names
				  (alist :key-type ,notmuch-tree--field-names
					 :value-type (string :tag "Format")))
		:value-type (string :tag "Format"))
  :group 'notmuch-tree)

(defcustom notmuch-unthreaded-result-format
  `(("date" . "%12s  ")
    ("authors" . "%-20s")
    ((("subject" . "%s")) ." %-54s ")
    ("tags" . "(%s)"))
  "Result formatting for unthreaded tree view.

List of pairs of (field . format-string).  Supported field
strings are: \"date\", \"authors\", \"subject\", \"tree\",
\"tags\".  It is also supported to pass a function in place of a
field-name. In this case the function is passed the thread
object (plist) and format string.

Tree means the thread tree box graphics. The field may
also be a list in which case the formatting rules are
applied recursively and then the output of all the fields
in the list is inserted according to format-string.

Note that the author string should not contain whitespace
\(put it in the neighbouring fields instead)."

  :type `(alist :key-type (choice ,notmuch-tree--field-names
				  (alist :key-type ,notmuch-tree--field-names
					 :value-type (string :tag "Format")))
		:value-type (string :tag "Format"))
  :group 'notmuch-tree)

(defun notmuch-tree-result-format ()
  (if notmuch-tree-unthreaded
      notmuch-unthreaded-result-format
    notmuch-tree-result-format))

;;; Faces
;;;; Faces for messages that match the query

(defface notmuch-tree-match-face
  '((t :inherit default))
  "Default face used in tree mode face for matching messages"
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-match-date-face
  nil
  "Face used in tree mode for the date in messages matching the query."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-match-author-face
  '((((class color)
      (background dark))
     (:foreground "OliveDrab1"))
    (((class color)
      (background light))
     (:foreground "dark blue"))
    (t
     (:bold t)))
  "Face used in tree mode for the author in messages matching the query."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-match-subject-face
  nil
  "Face used in tree mode for the subject in messages matching the query."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-match-tree-face
  nil
  "Face used in tree mode for the thread tree block graphics in messages matching the query."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-match-tag-face
  '((((class color)
      (background dark))
     (:foreground "OliveDrab1"))
    (((class color)
      (background light))
     (:foreground "navy blue" :bold t))
    (t
     (:bold t)))
  "Face used in tree mode for tags in messages matching the query."
  :group 'notmuch-tree
  :group 'notmuch-faces)

;;;; Faces for messages that do not match the query

(defface notmuch-tree-no-match-face
  '((t (:foreground "gray")))
  "Default face used in tree mode face for non-matching messages."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-no-match-date-face
  nil
  "Face used in tree mode for non-matching dates."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-no-match-subject-face
  nil
  "Face used in tree mode for non-matching subjects."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-no-match-tree-face
  nil
  "Face used in tree mode for the thread tree block graphics in messages matching the query."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-no-match-author-face
  nil
  "Face used in tree mode for non-matching authors."
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defface notmuch-tree-no-match-tag-face
  nil
  "Face used in tree mode face for non-matching tags."
  :group 'notmuch-tree
  :group 'notmuch-faces)

;;; Variables

(defvar-local notmuch-tree-previous-subject
  "The subject of the most recent result shown during the async display.")

(defvar-local notmuch-tree-basic-query nil
  "A buffer local copy of argument query to the function notmuch-tree.")

(defvar-local notmuch-tree-query-context nil
  "A buffer local copy of argument query-context to the function notmuch-tree.")

(defvar-local notmuch-tree-target-msg nil
  "A buffer local copy of argument target to the function notmuch-tree.")

(defvar-local notmuch-tree-open-target nil
  "A buffer local copy of argument open-target to the function notmuch-tree.")

(defvar-local notmuch-tree-parent-buffer nil)

(defvar-local notmuch-tree-message-window nil
  "The window of the message pane.

It is set in both the tree buffer and the child show buffer. It
is used to try and close the message pane when quitting tree view
or the child show buffer.")
(put 'notmuch-tree-message-window 'permanent-local t)

(defvar-local notmuch-tree-message-buffer nil
  "The buffer name of the show buffer in the message pane.

This is used to try and make sure we don't close the message pane
if the user has loaded a different buffer in that window.")
(put 'notmuch-tree-message-buffer 'permanent-local t)

;;; Tree wrapper commands

(defmacro notmuch-tree--define-do-in-message-window (name cmd)
  "Define NAME as a command that calls CMD interactively in the message window.
If the message pane is closed then this command does nothing.
Avoid using this macro in new code; it will be removed."
  `(defun ,name ()
     ,(concat "(In message window) " (documentation cmd t))
     (interactive)
     (when (window-live-p notmuch-tree-message-window)
       (with-selected-window notmuch-tree-message-window
	 (call-interactively #',cmd)))))

(notmuch-tree--define-do-in-message-window
 notmuch-tree-previous-message-button
 notmuch-show-previous-button)
(notmuch-tree--define-do-in-message-window
 notmuch-tree-next-message-button
 notmuch-show-next-button)
(notmuch-tree--define-do-in-message-window
 notmuch-tree-toggle-message-process-crypto
 notmuch-show-toggle-process-crypto)

(defun notmuch-tree--message-process-crypto ()
  "Return value of `notmuch-show-process-crypto' in the message window.
If that window isn't alive, then return the current value.
Avoid using this function in new code; it will be removed."
  (if (window-live-p notmuch-tree-message-window)
      (with-selected-window notmuch-tree-message-window
	notmuch-show-process-crypto)
    notmuch-show-process-crypto))

(defmacro notmuch-tree--define-close-message-window-and (name cmd)
  "Define NAME as a variant of CMD.

NAME determines the value of `notmuch-show-process-crypto' in the
message window, closes the window, and then call CMD interactively
with that value let-bound.  If the message window does not exist,
then NAME behaves like CMD."
  `(defun ,name ()
     ,(concat "(Close message pane and) " (documentation cmd t))
     (interactive)
     (let ((notmuch-show-process-crypto
	    (notmuch-tree--message-process-crypto)))
       (notmuch-tree-close-message-window)
       (call-interactively #',cmd))))

(notmuch-tree--define-close-message-window-and
 notmuch-tree-help
 notmuch-help)
(notmuch-tree--define-close-message-window-and
 notmuch-tree-new-mail
 notmuch-mua-new-mail)
(notmuch-tree--define-close-message-window-and
 notmuch-tree-jump-search
 notmuch-jump-search)
(notmuch-tree--define-close-message-window-and
 notmuch-tree-forward-message
 notmuch-show-forward-message)
(notmuch-tree--define-close-message-window-and
 notmuch-tree-reply-sender
 notmuch-show-reply-sender)
(notmuch-tree--define-close-message-window-and
 notmuch-tree-reply
 notmuch-show-reply)
(notmuch-tree--define-close-message-window-and
 notmuch-tree-view-raw-message
 notmuch-show-view-raw-message)

;;; Keymap

(defvar notmuch-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map notmuch-common-keymap)
    ;; These bindings shadow common bindings with variants
    ;; that additionally close the message window.
    (define-key map [remap notmuch-bury-or-kill-this-buffer] 'notmuch-tree-quit)
    (define-key map [remap notmuch-search]        'notmuch-tree-to-search)
    (define-key map [remap notmuch-help]          'notmuch-tree-help)
    (define-key map [remap notmuch-mua-new-mail]  'notmuch-tree-new-mail)
    (define-key map [remap notmuch-jump-search]   'notmuch-tree-jump-search)

    (define-key map "o" 'notmuch-tree-toggle-order)
    (define-key map "S" 'notmuch-search-from-tree-current-query)
    (define-key map "U" 'notmuch-unthreaded-from-tree-current-query)
    (define-key map "Z" 'notmuch-tree-from-unthreaded-current-query)

    ;; these use notmuch-show functions directly
    (define-key map "|" 'notmuch-show-pipe-message)
    (define-key map "w" 'notmuch-show-save-attachments)
    (define-key map "v" 'notmuch-show-view-all-mime-parts)
    (define-key map "c" 'notmuch-show-stash-map)
    (define-key map "b" 'notmuch-show-resend-message)

    ;; these apply to the message pane
    (define-key map (kbd "M-TAB")     'notmuch-tree-previous-message-button)
    (define-key map (kbd "<backtab>") 'notmuch-tree-previous-message-button)
    (define-key map (kbd "TAB")       'notmuch-tree-next-message-button)
    (define-key map "$" 'notmuch-tree-toggle-message-process-crypto)

    ;; bindings from show (or elsewhere) but we close the message pane first.
    (define-key map "f" 'notmuch-tree-forward-message)
    (define-key map "r" 'notmuch-tree-reply-sender)
    (define-key map "R" 'notmuch-tree-reply)
    (define-key map "V" 'notmuch-tree-view-raw-message)
    (define-key map "l" 'notmuch-tree-filter)
    (define-key map "t" 'notmuch-tree-filter-by-tag)
    (define-key map "E" 'notmuch-tree-edit-search)

    ;; The main tree view bindings
    (define-key map (kbd "RET") 'notmuch-tree-show-message)
    (define-key map [mouse-1] 'notmuch-tree-show-message)
    (define-key map "x" 'notmuch-tree-archive-message-then-next-or-exit)
    (define-key map "X" 'notmuch-tree-archive-thread-then-exit)
    (define-key map "A" 'notmuch-tree-archive-thread-then-next)
    (define-key map "a" 'notmuch-tree-archive-message-then-next)
    (define-key map "z" 'notmuch-tree-to-tree)
    (define-key map "n" 'notmuch-tree-next-matching-message)
    (define-key map "p" 'notmuch-tree-prev-matching-message)
    (define-key map "N" 'notmuch-tree-next-message)
    (define-key map "P" 'notmuch-tree-prev-message)
    (define-key map (kbd "M-p") 'notmuch-tree-prev-thread)
    (define-key map (kbd "M-n") 'notmuch-tree-next-thread)
    (define-key map "k" 'notmuch-tag-jump)
    (define-key map "-" 'notmuch-tree-remove-tag)
    (define-key map "+" 'notmuch-tree-add-tag)
    (define-key map "*" 'notmuch-tree-tag-thread)
    (define-key map " " 'notmuch-tree-scroll-or-next)
    (define-key map (kbd "DEL") 'notmuch-tree-scroll-message-window-back)
    (define-key map "e" 'notmuch-tree-resume-message)
    map)
  "Keymap for \"notmuch tree\" buffers.")

;;; Message properties

(defun notmuch-tree-get-message-properties ()
  "Return the properties of the current message as a plist.

Some useful entries are:
:headers - Property list containing the headers :Date, :Subject, :From, etc.
:tags - Tags for this message."
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) :notmuch-message-properties)))

(defun notmuch-tree-set-message-properties (props)
  (save-excursion
    (beginning-of-line)
    (put-text-property (point)
		       (+ (point) 1)
		       :notmuch-message-properties props)))

(defun notmuch-tree-set-prop (prop val &optional props)
  (let ((inhibit-read-only t)
	(props (or props
		   (notmuch-tree-get-message-properties))))
    (plist-put props prop val)
    (notmuch-tree-set-message-properties props)))

(defun notmuch-tree-get-prop (prop &optional props)
  (plist-get (or props (notmuch-tree-get-message-properties))
	     prop))

(defun notmuch-tree-set-tags (tags)
  "Set the tags of the current message."
  (notmuch-tree-set-prop :tags tags))

(defun notmuch-tree-get-tags ()
  "Return the tags of the current message."
  (notmuch-tree-get-prop :tags))

(defun notmuch-tree-get-message-id (&optional bare)
  "Return the message id of the current message."
  (let ((id (notmuch-tree-get-prop :id)))
    (if id
	(if bare
	    id
	  (notmuch-id-to-query id))
      nil)))

(defun notmuch-tree-get-match ()
  "Return whether the current message is a match."
  (notmuch-tree-get-prop :match))

;;; Update display

(defun notmuch-tree-refresh-result ()
  "Redisplay the current message line.

This redisplays the current line based on the messages
properties (as they are now). This is used when tags are
updated."
  (let ((init-point (point))
	(end (line-end-position))
	(msg (notmuch-tree-get-message-properties))
	(inhibit-read-only t))
    (beginning-of-line)
    ;; This is a little tricky: we override
    ;; notmuch-tree-previous-subject to get the decision between
    ;; ... and a subject right and it stops notmuch-tree-insert-msg
    ;; from overwriting the buffer local copy of
    ;; notmuch-tree-previous-subject if this is called while the
    ;; buffer is displaying.
    (let ((notmuch-tree-previous-subject
	   (notmuch-tree-get-prop :previous-subject)))
      (delete-region (point) (1+ (line-end-position)))
      (notmuch-tree-insert-msg msg))
    (let ((new-end (line-end-position)))
      (goto-char (if (= init-point end)
		     new-end
		   (min init-point (- new-end 1)))))))

(defun notmuch-tree-tag-update-display (&optional tag-changes)
  "Update display for TAG-CHANGES to current message.

Updates the message in the message pane if appropriate, but does
NOT change the database."
  (let* ((current-tags (notmuch-tree-get-tags))
	 (new-tags (notmuch-update-tags current-tags tag-changes))
	 (tree-msg-id (notmuch-tree-get-message-id)))
    (unless (equal current-tags new-tags)
      (notmuch-tree-set-tags new-tags)
      (notmuch-tree-refresh-result)
      (when (window-live-p notmuch-tree-message-window)
	(with-selected-window notmuch-tree-message-window
	  (when (string= tree-msg-id (notmuch-show-get-message-id))
	    (notmuch-show-update-tags new-tags)))))))

;;; Commands (and some helper functions used by them)

(defun notmuch-tree-tag (tag-changes)
  "Change tags for the current message."
  (interactive
   (list (notmuch-read-tag-changes (notmuch-tree-get-tags) "Tag message")))
  (notmuch-tag (notmuch-tree-get-message-id) tag-changes)
  (notmuch-tree-tag-update-display tag-changes))

(defun notmuch-tree-add-tag (tag-changes)
  "Same as `notmuch-tree-tag' but sets initial input to '+'."
  (interactive
   (list (notmuch-read-tag-changes (notmuch-tree-get-tags) "Tag message" "+")))
  (notmuch-tree-tag tag-changes))

(defun notmuch-tree-remove-tag (tag-changes)
  "Same as `notmuch-tree-tag' but sets initial input to '-'."
  (interactive
   (list (notmuch-read-tag-changes (notmuch-tree-get-tags) "Tag message" "-")))
  (notmuch-tree-tag tag-changes))

(defun notmuch-tree-resume-message ()
  "Resume EDITING the current draft message."
  (interactive)
  (notmuch-tree-close-message-window)
  (let ((id (notmuch-tree-get-message-id)))
    (if id
	(notmuch-draft-resume id)
      (message "No message to resume!"))))

;; The next two functions close the message window before calling
;; notmuch-search or notmuch-tree but they do so after the user has
;; entered the query (in case the user was basing the query on
;; something in the message window).

(defun notmuch-tree-to-search ()
  "Run \"notmuch search\" with the given `query' and display results."
  (interactive)
  (let ((query (notmuch-read-query "Notmuch search: ")))
    (notmuch-tree-close-message-window)
    (notmuch-search query)))

(defun notmuch-tree-to-tree ()
  "Run a query and display results in tree view."
  (interactive)
  (let ((query (notmuch-read-query "Notmuch tree view search: ")))
    (notmuch-tree-close-message-window)
    (notmuch-tree query)))

(defun notmuch-tree-archive-thread-then-next ()
  "Archive all messages in the current buffer, then show next thread from search."
  (interactive)
  (notmuch-tree-archive-thread)
  (notmuch-tree-next-thread))

(defun notmuch-unthreaded-from-tree-current-query ()
  "Switch from tree view to unthreaded view."
  (interactive)
  (unless notmuch-tree-unthreaded
    (notmuch-tree-refresh-view 'unthreaded)))

(defun notmuch-tree-from-unthreaded-current-query ()
  "Switch from unthreaded view to tree view."
  (interactive)
  (when notmuch-tree-unthreaded
    (notmuch-tree-refresh-view 'tree)))

(defun notmuch-search-from-tree-current-query ()
  "Call notmuch search with the current query."
  (interactive)
  (notmuch-tree-close-message-window)
  (notmuch-search (notmuch-tree-get-query)))

(defun notmuch-tree-message-window-kill-hook ()
  "Close the message pane when exiting the show buffer."
  (let ((buffer (current-buffer)))
    (when (and (window-live-p notmuch-tree-message-window)
	       (eq (window-buffer notmuch-tree-message-window) buffer))
      ;; We could check whether this is the only window in its frame,
      ;; but simply ignoring the error that is thrown otherwise is
      ;; what we had to do for Emacs 24 and we stick to that because
      ;; it is still the simplest approach.
      (ignore-errors
	(delete-window notmuch-tree-message-window)))))

(defun notmuch-tree-command-hook ()
  (when (eq major-mode 'notmuch-tree-mode)
    ;; We just run the notmuch-show-command-hook on the message pane.
    (when (buffer-live-p notmuch-tree-message-buffer)
      (with-current-buffer notmuch-tree-message-buffer
	(notmuch-show-command-hook)))))

(defun notmuch-tree-show-message-in ()
  "Show the current message (in split-pane)."
  (interactive)
  (let ((id (notmuch-tree-get-message-id))
	(inhibit-read-only t)
	buffer)
    (when id
      ;; We close and reopen the window to kill off un-needed buffers
      ;; this might cause flickering but seems ok.
      (notmuch-tree-close-message-window)
      (setq notmuch-tree-message-window
	    (split-window-vertically (/ (window-height) 4)))
      (with-selected-window notmuch-tree-message-window
	(let (;; Since we are only displaying one message do not indent.
	      (notmuch-show-indent-messages-width 0)
	      (notmuch-show-single-message t)
	      ;; Ensure that `pop-to-buffer-same-window' uses the
	      ;; window we want it to use.
	      (display-buffer-overriding-action
		 '((display-buffer-same-window)
		   (inhibit-same-window . nil))))
	  (setq buffer (notmuch-show id))))
      ;; We need the `let' as notmuch-tree-message-window is buffer local.
      (let ((window notmuch-tree-message-window))
	(with-current-buffer buffer
	  (setq notmuch-tree-message-window window)
	  (add-hook 'kill-buffer-hook 'notmuch-tree-message-window-kill-hook)))
      (when notmuch-show-mark-read-tags
	(notmuch-tree-tag-update-display notmuch-show-mark-read-tags))
      (setq notmuch-tree-message-buffer buffer))))

(defun notmuch-tree-show-message-out ()
  "Show the current message (in whole window)."
  (interactive)
  (let ((id (notmuch-tree-get-message-id))
	(inhibit-read-only t))
    (when id
      ;; We close the window to kill off un-needed buffers.
      (notmuch-tree-close-message-window)
      ;; n-s-s-m is buffer local, so use inner let.
      (let ((notmuch-show-single-message t))
	(notmuch-show id)))))

(defun notmuch-tree-show-message (arg)
  "Show the current message.

Shows in split pane or whole window according to value of
`notmuch-tree-show-out'. A prefix argument reverses the choice."
  (interactive "P")
  (if (or (and (notmuch-tree-show-out) (not arg))
	  (and (not (notmuch-tree-show-out)) arg))
      (notmuch-tree-show-message-out)
    (notmuch-tree-show-message-in)))

(defun notmuch-tree-scroll-message-window ()
  "Scroll the message window (if it exists)."
  (interactive)
  (when (window-live-p notmuch-tree-message-window)
    (with-selected-window notmuch-tree-message-window
      (if (pos-visible-in-window-p (point-max))
	  t
	(scroll-up)))))

(defun notmuch-tree-scroll-message-window-back ()
  "Scroll the message window back (if it exists)."
  (interactive)
  (when (window-live-p notmuch-tree-message-window)
    (with-selected-window notmuch-tree-message-window
      (if (pos-visible-in-window-p (point-min))
	  t
	(scroll-down)))))

(defun notmuch-tree-scroll-or-next ()
  "Scroll the message window.
If it at end go to next message."
  (interactive)
  (when (notmuch-tree-scroll-message-window)
    (notmuch-tree-next-matching-message)))

(defun notmuch-tree-quit (&optional kill-both)
  "Close the split view or exit tree."
  (interactive "P")
  (when (or (not (notmuch-tree-close-message-window)) kill-both)
    (kill-buffer (current-buffer))))

(defun notmuch-tree-close-message-window ()
  "Close the message-window. Return t if close succeeds."
  (interactive)
  (when (and (window-live-p notmuch-tree-message-window)
	     (eq (window-buffer notmuch-tree-message-window)
		 notmuch-tree-message-buffer))
    (delete-window notmuch-tree-message-window)
    (unless (get-buffer-window-list notmuch-tree-message-buffer)
      (kill-buffer notmuch-tree-message-buffer))
    t))

(defun notmuch-tree-archive-message (&optional unarchive)
  "Archive the current message.

Archive the current message by applying the tag changes in
`notmuch-archive-tags' to it. If a prefix argument is given, the
message will be \"unarchived\", i.e. the tag changes in
`notmuch-archive-tags' will be reversed."
  (interactive "P")
  (when notmuch-archive-tags
    (notmuch-tree-tag
     (notmuch-tag-change-list notmuch-archive-tags unarchive))))

(defun notmuch-tree-archive-message-then-next (&optional unarchive)
  "Archive the current message and move to next matching message."
  (interactive "P")
  (notmuch-tree-archive-message unarchive)
  (notmuch-tree-next-matching-message))

(defun notmuch-tree-archive-thread-then-exit ()
  "Archive all messages in the current buffer, then exit notmuch-tree."
  (interactive)
  (notmuch-tree-archive-thread)
  (notmuch-tree-quit t))

(defun notmuch-tree-archive-message-then-next-or-exit ()
  "Archive current message, then show next open message in current thread.

If at the last open message in the current thread, then exit back
to search results."
  (interactive)
  (notmuch-tree-archive-message)
  (notmuch-tree-next-matching-message t))

(defun notmuch-tree-next-message ()
  "Move to next message."
  (interactive)
  (forward-line)
  (when (window-live-p notmuch-tree-message-window)
    (notmuch-tree-show-message-in)))

(defun notmuch-tree-prev-message ()
  "Move to previous message."
  (interactive)
  (forward-line -1)
  (when (window-live-p notmuch-tree-message-window)
    (notmuch-tree-show-message-in)))

(defun notmuch-tree-goto-matching-message (&optional prev)
  "Move to the next or previous matching message.

Returns t if there was a next matching message in the thread to show,
nil otherwise."
  (let ((dir (if prev -1 nil))
	(eobfn (if prev #'bobp #'eobp)))
    (while (and (not (funcall eobfn))
		(not (notmuch-tree-get-match)))
      (forward-line dir))
    (not (funcall eobfn))))

(defun notmuch-tree-matching-message (&optional prev pop-at-end)
  "Move to the next or previous matching message."
  (interactive "P")
  (forward-line (if prev -1 nil))
  (if (and (not (notmuch-tree-goto-matching-message prev)) pop-at-end)
      (notmuch-tree-quit pop-at-end)
    (when (window-live-p notmuch-tree-message-window)
      (notmuch-tree-show-message-in))))

(defun notmuch-tree-prev-matching-message (&optional pop-at-end)
  "Move to previous matching message."
  (interactive "P")
  (notmuch-tree-matching-message t pop-at-end))

(defun notmuch-tree-next-matching-message (&optional pop-at-end)
  "Move to next matching message."
  (interactive "P")
  (notmuch-tree-matching-message nil pop-at-end))

(defun notmuch-tree-refresh-view (&optional view)
  "Refresh view."
  (interactive)
  (when (get-buffer-process (current-buffer))
    (error "notmuch tree process already running for current buffer"))
  (let ((inhibit-read-only t)
	(basic-query notmuch-tree-basic-query)
	(unthreaded (cond ((eq view 'unthreaded) t)
			  ((eq view 'tree) nil)
			  (t notmuch-tree-unthreaded)))
	(query-context notmuch-tree-query-context)
	(target (notmuch-tree-get-message-id)))
    (erase-buffer)
    (notmuch-tree-worker basic-query
			 query-context
			 target
			 nil
			 unthreaded
			 notmuch-search-oldest-first)))

(defun notmuch-tree-thread-top ()
  (when (notmuch-tree-get-message-properties)
    (while (not (or (notmuch-tree-get-prop :first) (eobp)))
      (forward-line -1))))

(defun notmuch-tree-prev-thread-in-tree ()
  "Move to the previous thread in the current tree"
  (interactive)
  (forward-line -1)
  (notmuch-tree-thread-top)
  (not (bobp)))

(defun notmuch-tree-next-thread-in-tree ()
  "Get the next thread in the current tree. Returns t if a thread was
found or nil if not."
  (interactive)
  (forward-line 1)
  (while (not (or (notmuch-tree-get-prop :first) (eobp)))
    (forward-line 1))
  (not (eobp)))

(defun notmuch-tree-next-thread-from-search (&optional previous)
  "Move to the next thread in the parent search results, if any.

If PREVIOUS is non-nil, move to the previous item in the
search results instead."
  (interactive "P")
  (let ((parent-buffer notmuch-tree-parent-buffer))
    (notmuch-tree-quit t)
    (when (buffer-live-p parent-buffer)
      (switch-to-buffer parent-buffer)
      (if previous
	  (notmuch-search-previous-thread)
	(notmuch-search-next-thread))
      (notmuch-tree-from-search-thread))))

(defun notmuch-tree-next-thread (&optional previous)
  "Move to the next thread in the current tree or parent search results.

If PREVIOUS is non-nil, move to the previous thread in the tree or
search results instead."
  (interactive)
  (unless (if previous (notmuch-tree-prev-thread-in-tree)
	    (notmuch-tree-next-thread-in-tree))
    (notmuch-tree-next-thread-from-search previous)))

(defun notmuch-tree-prev-thread ()
  "Move to the previous thread in the current tree or parent search results."
  (interactive)
  (notmuch-tree-next-thread t))

(defun notmuch-tree-thread-mapcar (function)
  "Call FUNCTION for each message in the current thread.
FUNCTION is called for side effects only."
  (save-excursion
    (notmuch-tree-thread-top)
    (cl-loop collect (funcall function)
	     do (forward-line)
	     while (and (notmuch-tree-get-message-properties)
			(not (notmuch-tree-get-prop :first))))))

(defun notmuch-tree-get-messages-ids-thread-search ()
  "Return a search string for all message ids of messages in the current thread."
  (mapconcat 'identity
	     (notmuch-tree-thread-mapcar 'notmuch-tree-get-message-id)
	     " or "))

(defun notmuch-tree-tag-thread (tag-changes)
  "Tag all messages in the current thread."
  (interactive
   (let ((tags (apply #'append (notmuch-tree-thread-mapcar
				(lambda () (notmuch-tree-get-tags))))))
     (list (notmuch-read-tag-changes tags "Tag thread"))))
  (when (notmuch-tree-get-message-properties)
    (notmuch-tag (notmuch-tree-get-messages-ids-thread-search) tag-changes)
    (notmuch-tree-thread-mapcar
     (lambda () (notmuch-tree-tag-update-display tag-changes)))))

(defun notmuch-tree-archive-thread (&optional unarchive)
  "Archive each message in thread.

Archive each message currently shown by applying the tag changes
in `notmuch-archive-tags' to each. If a prefix argument is given,
the messages will be \"unarchived\", i.e. the tag changes in
`notmuch-archive-tags' will be reversed.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not archive the
entire thread, but only the messages shown in the current
buffer."
  (interactive "P")
  (when notmuch-archive-tags
    (notmuch-tree-tag-thread
     (notmuch-tag-change-list notmuch-archive-tags unarchive))))

;;; Functions for displaying the tree buffer itself

(defun notmuch-tree-clean-address (address)
  "Try to clean a single email ADDRESS for display. Return
AUTHOR_NAME if present, otherwise return AUTHOR_EMAIL. Return
unchanged ADDRESS if parsing fails."
  (let* ((clean-address (notmuch-clean-address address))
	 (p-address (car clean-address))
	 (p-name (cdr clean-address)))

    ;; If we have a name return that otherwise return the address.
    (or p-name p-address)))

(defun notmuch-tree-format-field (field format-string msg)
  "Format a FIELD of MSG according to FORMAT-STRING and return string."
  (let* ((headers (plist-get msg :headers))
	 (match (plist-get msg :match)))
    (cond
     ((listp field)
      (format format-string (notmuch-tree-format-field-list field msg)))

     ((functionp field)
      (funcall field format-string msg))

     ((string-equal field "date")
      (let ((face (if match
		      'notmuch-tree-match-date-face
		    'notmuch-tree-no-match-date-face)))
	(propertize (format format-string (plist-get msg :date_relative))
		    'face face)))

     ((string-equal field "tree")
      (let ((tree-status (plist-get msg :tree-status))
	    (face (if match
		      'notmuch-tree-match-tree-face
		    'notmuch-tree-no-match-tree-face)))

	(propertize (format format-string
			    (mapconcat #'identity (reverse tree-status) ""))
		    'face face)))

     ((string-equal field "subject")
      (let ((bare-subject (notmuch-show-strip-re (plist-get headers :Subject)))
	    (previous-subject notmuch-tree-previous-subject)
	    (face (if match
		      'notmuch-tree-match-subject-face
		    'notmuch-tree-no-match-subject-face)))

	(setq notmuch-tree-previous-subject bare-subject)
	(propertize (format format-string
			    (if (string= previous-subject bare-subject)
				" ..."
			      bare-subject))
		    'face face)))

     ((string-equal field "authors")
      (let ((author (notmuch-tree-clean-address (plist-get headers :From)))
	    (len (length (format format-string "")))
	    (face (if match
		      'notmuch-tree-match-author-face
		    'notmuch-tree-no-match-author-face)))
	(when (> (length author) len)
	  (setq author (substring author 0 len)))
	(propertize (format format-string author) 'face face)))

     ((string-equal field "tags")
      (let ((tags (plist-get msg :tags))
	    (orig-tags (plist-get msg :orig-tags))
	    (face (if match
		      'notmuch-tree-match-tag-face
		    'notmuch-tree-no-match-tag-face)))
	(format format-string (notmuch-tag-format-tags tags orig-tags face)))))))

(defun notmuch-tree-format-field-list (field-list msg)
  "Format fields of MSG according to FIELD-LIST and return string."
  (let ((face (if (plist-get msg :match)
		  'notmuch-tree-match-face
		'notmuch-tree-no-match-face))
	(result-string))
    (dolist (spec field-list result-string)
      (let ((field-string (notmuch-tree-format-field (car spec) (cdr spec) msg)))
	(setq result-string (concat result-string field-string))))
    (notmuch-apply-face result-string face t)))

(defun notmuch-tree-insert-msg (msg)
  "Insert the message MSG according to notmuch-tree-result-format."
  ;; We need to save the previous subject as it will get overwritten
  ;; by the insert-field calls.
  (let ((previous-subject notmuch-tree-previous-subject))
    (insert (notmuch-tree-format-field-list (notmuch-tree-result-format) msg))
    (notmuch-tree-set-message-properties msg)
    (notmuch-tree-set-prop :previous-subject previous-subject)
    (insert "\n")))

(defun notmuch-tree-goto-and-insert-msg (msg)
  "Insert msg at the end of the buffer. Move point to msg if it is the target."
  (save-excursion
    (goto-char (point-max))
    (notmuch-tree-insert-msg msg))
  (let ((msg-id (notmuch-id-to-query (plist-get msg :id)))
	(target notmuch-tree-target-msg))
    (when (or (and (not target) (plist-get msg :match))
	      (string= msg-id target))
      (setq notmuch-tree-target-msg "found")
      (goto-char (point-max))
      (forward-line -1)
      (when notmuch-tree-open-target
	(notmuch-tree-show-message-in)
	(notmuch-tree-command-hook)))))

(defun notmuch-tree-insert-tree (tree depth tree-status first last)
  "Insert the message tree TREE at depth DEPTH in the current thread.

A message tree is another name for a single sub-thread: i.e., a
message together with all its descendents."
  (let ((msg (car tree))
	(replies (cadr tree))
	;; outline level, computed from the message's depth and
	;; whether or not it's the first message in the tree.
	(level (1+ (if (and (eq 0 depth) (not first)) 1 depth))))
    (cond
     ((and (< 0 depth) (not last))
      (push (alist-get 'vertical-tee  notmuch-tree-thread-symbols) tree-status))
     ((and (< 0 depth) last)
      (push (alist-get 'bottom notmuch-tree-thread-symbols) tree-status))
     ((and (eq 0 depth) first last)
      (push (alist-get 'prefix notmuch-tree-thread-symbols) tree-status))
     ((and (eq 0 depth) first (not last))
      (push (alist-get 'top-tee notmuch-tree-thread-symbols) tree-status))
     ((and (eq 0 depth) (not first) last)
      (push (alist-get 'bottom notmuch-tree-thread-symbols) tree-status))
     ((and (eq 0 depth) (not first) (not last))
      (push (alist-get 'vertical-tee notmuch-tree-thread-symbols) tree-status)))
    (push (concat (alist-get (if replies 'top-tee 'top) notmuch-tree-thread-symbols)
		  (alist-get 'arrow notmuch-tree-thread-symbols))
	  tree-status)
    (setq msg (plist-put msg :first (and first (eq 0 depth))))
    (setq msg (plist-put msg :tree-status tree-status))
    (setq msg (plist-put msg :orig-tags (plist-get msg :tags)))
    (setq msg (plist-put msg :level level))
    (notmuch-tree-goto-and-insert-msg msg)
    (pop tree-status)
    (pop tree-status)
    (if last
	(push " " tree-status)
      (push (alist-get 'vertical notmuch-tree-thread-symbols) tree-status))
    (notmuch-tree-insert-thread replies (1+ depth) tree-status)))

(defun notmuch-tree-insert-thread (thread depth tree-status)
  "Insert the collection of sibling sub-threads THREAD at depth DEPTH in the current forest."
  (let ((n (length thread)))
    (cl-loop for tree in thread
	     for count from 1 to n
	     do (notmuch-tree-insert-tree tree depth tree-status
					  (eq count 1)
					  (eq count n)))))

(defun notmuch-tree-insert-forest-thread (forest-thread)
  "Insert a single complete thread."
  ;; Reset at the start of each main thread.
  (setq notmuch-tree-previous-subject nil)
  (notmuch-tree-insert-thread forest-thread 0 nil))

(defun notmuch-tree-insert-forest (forest)
  "Insert a forest of threads.

This function inserts a collection of several complete threads as
passed to it by notmuch-tree-process-filter."
  (mapc 'notmuch-tree-insert-forest-thread forest))

(define-derived-mode notmuch-tree-mode fundamental-mode "notmuch-tree"
  "Major mode displaying messages (as opposed to threads) of a notmuch search.

This buffer contains the results of a \"notmuch tree\" of your
email archives. Each line in the buffer represents a single
message giving the relative date, the author, subject, and any
tags.

Pressing \\[notmuch-tree-show-message] on any line displays that message.

Complete list of currently available key bindings:

\\{notmuch-tree-mode-map}"
  (setq notmuch-buffer-refresh-function #'notmuch-tree-refresh-view)
  (hl-line-mode 1)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (when notmuch-tree-outline-enabled (notmuch-tree-outline-mode 1)))

(defvar notmuch-tree-process-exit-functions nil
  "Functions called when the process inserting a tree of results finishes.

Functions in this list are called with one argument, the process
object, and with the tree results buffer as the current buffer.")

(defun notmuch-tree-process-sentinel (proc _msg)
  "Add a message to let user know when \"notmuch tree\" exits."
  (let ((buffer (process-buffer proc))
	(status (process-status proc))
	(exit-status (process-exit-status proc)))
    (when (memq status '(exit signal))
      (kill-buffer (process-get proc 'parse-buf))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (save-excursion
	    (let ((inhibit-read-only t))
	      (goto-char (point-max))
	      (when (eq status 'signal)
		(insert "Incomplete search results (tree view process was killed).\n"))
	      (when (eq status 'exit)
		(insert "End of search results.")
		(unless (= exit-status 0)
		  (insert (format " (process returned %d)" exit-status)))
		(insert "\n"))))
	  (run-hook-with-args 'notmuch-tree-process-exit-functions proc))))))

(defun notmuch-tree-process-filter (proc string)
  "Process and filter the output of \"notmuch show\" for tree view."
  (let ((results-buf (process-buffer proc))
	(parse-buf (process-get proc 'parse-buf))
	(inhibit-read-only t))
    (if (not (buffer-live-p results-buf))
	(delete-process proc)
      (with-current-buffer parse-buf
	;; Insert new data
	(save-excursion
	  (goto-char (point-max))
	  (insert string))
	(notmuch-sexp-parse-partial-list 'notmuch-tree-insert-forest-thread
					 results-buf)))))

(defun notmuch-tree-worker (basic-query &optional query-context target
					open-target unthreaded oldest-first)
  "Insert the tree view of the search in the current buffer.

This is is a helper function for notmuch-tree. The arguments are
the same as for the function notmuch-tree."
  (interactive)
  (notmuch-tree-mode)
  (add-hook 'post-command-hook #'notmuch-tree-command-hook t t)
  (setq notmuch-search-oldest-first oldest-first)
  (setq notmuch-tree-unthreaded unthreaded)
  (setq notmuch-tree-basic-query basic-query)
  (setq notmuch-tree-query-context (if (or (string= query-context "")
					   (string= query-context "*"))
				       nil
				     query-context))
  (setq notmuch-tree-target-msg target)
  (setq notmuch-tree-open-target open-target)
  ;; Set the default value for `notmuch-show-process-crypto' in this
  ;; buffer. Although we don't use this some of the functions we call
  ;; (such as reply) do. It is a buffer local variable so setting it
  ;; will not affect genuine show buffers.
  (setq notmuch-show-process-crypto notmuch-crypto-process-mime)
  (erase-buffer)
  (goto-char (point-min))
  (let* ((search-args (concat basic-query
			      (and query-context
				   (concat " and (" query-context ")"))))
	 (sort-arg (if oldest-first "--sort=oldest-first" "--sort=newest-first"))
	 (message-arg (if unthreaded "--unthreaded" "--entire-thread")))
    (when (equal (car (notmuch--process-lines notmuch-command "count" search-args)) "0")
      (setq search-args basic-query))
    (notmuch-tag-clear-cache)
    (let ((proc (notmuch-start-notmuch
		 "notmuch-tree" (current-buffer) #'notmuch-tree-process-sentinel
		 "show" "--body=false" "--format=sexp" "--format-version=5"
		 sort-arg message-arg search-args))
	  ;; Use a scratch buffer to accumulate partial output.
	  ;; This buffer will be killed by the sentinel, which
	  ;; should be called no matter how the process dies.
	  (parse-buf (generate-new-buffer " *notmuch tree parse*")))
      (process-put proc 'parse-buf parse-buf)
      (set-process-filter proc 'notmuch-tree-process-filter)
      (set-process-query-on-exit-flag proc nil))))

(defun notmuch-tree-get-query ()
  "Return the current query in this tree buffer."
  (if notmuch-tree-query-context
      (concat notmuch-tree-basic-query
	      " and ("
	      notmuch-tree-query-context
	      ")")
    notmuch-tree-basic-query))

(defun notmuch-tree-toggle-order ()
  "Toggle the current search order.

This command toggles the sort order for the current search. The
default sort order is defined by `notmuch-search-oldest-first'."
  (interactive)
  (setq notmuch-search-oldest-first (not notmuch-search-oldest-first))
  (notmuch-tree-refresh-view))

(defun notmuch-tree (&optional query query-context target buffer-name
			       open-target unthreaded parent-buffer oldest-first)
  "Display threads matching QUERY in tree view.

The arguments are:
  QUERY: the main query. This can be any query but in many cases will be
      a single thread. If nil this is read interactively from the minibuffer.
  QUERY-CONTEXT: is an additional term for the query. The query used
      is QUERY and QUERY-CONTEXT unless that does not match any messages
      in which case we fall back to just QUERY.
  TARGET: A message ID (with the id: prefix) that will be made
      current if it appears in the tree view results.
  BUFFER-NAME: the name of the buffer to display the tree view. If
      it is nil \"*notmuch-tree\" followed by QUERY is used.
  OPEN-TARGET: If TRUE open the target message in the message pane.
  UNTHREADED: If TRUE only show matching messages in an unthreaded view."
  (interactive)
  (unless query
    (setq query (notmuch-read-query (concat "Notmuch "
					    (if unthreaded "unthreaded " "tree ")
					    "view search: "))))
  (let* ((name
	  (or buffer-name
	      (notmuch-search-buffer-title query
					   (if unthreaded "unthreaded" "tree"))))
	 (buffer (get-buffer-create (generate-new-buffer-name name)))
	(inhibit-read-only t))
    (pop-to-buffer-same-window buffer))
  ;; Don't track undo information for this buffer
  (setq buffer-undo-list t)
  (notmuch-tree-worker query query-context target open-target unthreaded oldest-first)
  (setq notmuch-tree-parent-buffer parent-buffer)
  (setq truncate-lines t))

(defun notmuch-unthreaded (&optional query query-context target buffer-name
				     open-target)
  "Display threads matching QUERY in unthreaded view.

See function NOTMUCH-TREE for documentation of the arguments"
  (interactive)
  (notmuch-tree query query-context target buffer-name open-target t))

(defun notmuch-tree-filter (query)
  "Filter or LIMIT the current search results based on an additional query string.

Runs a new tree search matching only messages that match both the
current search results AND the additional query string provided."
  (interactive (list (notmuch-read-query "Filter search: ")))
  (let ((notmuch-show-process-crypto (notmuch-tree--message-process-crypto))
	(grouped-query (notmuch-group-disjunctive-query-string query))
	(grouped-original-query (notmuch-group-disjunctive-query-string
				 (notmuch-tree-get-query))))
    (notmuch-tree-close-message-window)
    (notmuch-tree (if (string= grouped-original-query "*")
		      grouped-query
		    (concat grouped-original-query " and " grouped-query)))))

(defun notmuch-tree-filter-by-tag (tag)
  "Filter the current search results based on a single TAG.

Run a new search matching only messages that match the current
search results and that are also tagged with the given TAG."
  (interactive
   (list (notmuch-select-tag-with-completion "Filter by tag: "
					     notmuch-tree-basic-query)))
  (let ((notmuch-show-process-crypto (notmuch-tree--message-process-crypto)))
    (notmuch-tree-close-message-window)
    (notmuch-tree (concat notmuch-tree-basic-query " and tag:" tag)
		  notmuch-tree-query-context
		  nil
		  nil
		  nil
		  notmuch-tree-unthreaded
		  nil
		  notmuch-search-oldest-first)))

(defun notmuch-tree-edit-search (query)
  "Edit the current search"
  (interactive (list (read-from-minibuffer "Edit search: "
					   notmuch-tree-basic-query)))
  (let ((notmuch-show-process-crypto (notmuch-tree--message-process-crypto)))
    (notmuch-tree-close-message-window)
    (notmuch-tree query
		  notmuch-tree-query-context
		  nil
		  nil
		  nil
		  notmuch-tree-unthreaded
		  nil
		  notmuch-search-oldest-first)))

;;; Tree outline mode
;;;; Custom variables
(defcustom notmuch-tree-outline-enabled nil
  "Whether to automatically activate `notmuch-tree-outline-mode' in tree views."
  :type 'boolean)

(defcustom notmuch-tree-outline-visibility 'hide-others
  "Default state of the forest outline for `notmuch-tree-outline-mode'.

This variable controls the state of a forest initially and after
a movement command.  If set to nil, all trees are displayed while
the symbol hide-all indicates that all trees in the forest should
be folded and hide-other that only the first one should be
unfolded."
  :type '(choice (const :tag "Show all" nil)
		 (const :tag "Hide others" hide-others)
		 (const :tag "Hide all" hide-all)))

(defcustom notmuch-tree-outline-auto-close nil
  "Close message and tree windows when moving past the last message."
  :type 'boolean)

(defcustom notmuch-tree-outline-open-on-next nil
  "Open new messages under point if they are closed when moving to next one.

When this flag is set, using the command
`notmuch-tree-outline-next' with point on a header for a new
message that is not shown will open its `notmuch-show' buffer
instead of moving point to next matching message."
  :type 'boolean)

;;;; Helper functions
(defsubst notmuch-tree-outline--pop-at-end (pop-at-end)
  (if notmuch-tree-outline-auto-close (not pop-at-end) pop-at-end))

(defun notmuch-tree-outline--set-visibility ()
  (when (and notmuch-tree-outline-mode (> (point-max) (point-min)))
    (cl-case notmuch-tree-outline-visibility
      (hide-others (notmuch-tree-outline-hide-others))
      (hide-all (outline-hide-body)))))

(defun notmuch-tree-outline--on-exit (proc)
  (when (eq (process-status proc) 'exit)
    (notmuch-tree-outline--set-visibility)))

(add-hook 'notmuch-tree-process-exit-functions #'notmuch-tree-outline--on-exit)

(defsubst notmuch-tree-outline--level (&optional props)
  (or (plist-get (or props (notmuch-tree-get-message-properties)) :level) 0))

(defsubst notmuch-tree-outline--message-open-p ()
  (and (buffer-live-p notmuch-tree-message-buffer)
       (get-buffer-window notmuch-tree-message-buffer)
       (let ((id (notmuch-tree-get-message-id)))
	 (and id
	      (with-current-buffer notmuch-tree-message-buffer
		(string= (notmuch-show-get-message-id) id))))))

(defsubst notmuch-tree-outline--at-original-match-p ()
  (and (notmuch-tree-get-prop :match)
       (equal (notmuch-tree-get-prop :orig-tags)
              (notmuch-tree-get-prop :tags))))

(defun notmuch-tree-outline--next (prev thread pop-at-end &optional open-new)
  (cond (thread
	 (notmuch-tree-thread-top)
	 (if prev
	     (outline-backward-same-level 1)
	   (outline-forward-same-level 1))
	 (when (> (notmuch-tree-outline--level) 0) (outline-show-branches))
	 (notmuch-tree-outline--next nil nil pop-at-end t))
	((and (or open-new notmuch-tree-outline-open-on-next)
	      (notmuch-tree-outline--at-original-match-p)
	      (not (notmuch-tree-outline--message-open-p)))
	 (notmuch-tree-outline-hide-others t))
	(t (outline-next-visible-heading (if prev -1 1))
	   (unless (notmuch-tree-get-prop :match)
	     (notmuch-tree-matching-message prev pop-at-end))
	   (notmuch-tree-outline-hide-others t))))

;;;; User commands
(defun notmuch-tree-outline-hide-others (&optional and-show)
  "Fold all threads except the one around point.
If AND-SHOW is t, make the current message visible if it's not."
  (interactive)
  (save-excursion
    (while (and (not (bobp)) (> (notmuch-tree-outline--level) 1))
      (outline-previous-heading))
    (outline-hide-sublevels 1))
  (when (> (notmuch-tree-outline--level) 0)
    (outline-show-subtree)
    (when and-show (notmuch-tree-show-message nil))))

(defun notmuch-tree-outline-next (&optional pop-at-end)
  "Next matching message in a forest, taking care of thread visibility.
A prefix argument reverses the meaning of `notmuch-tree-outline-auto-close'."
  (interactive "P")
  (let ((pop (notmuch-tree-outline--pop-at-end pop-at-end)))
    (if (null notmuch-tree-outline-visibility)
	(notmuch-tree-matching-message nil pop)
      (notmuch-tree-outline--next nil nil pop))))

(defun notmuch-tree-outline-previous (&optional pop-at-end)
  "Previous matching message in forest, taking care of thread visibility.
With prefix, quit the tree view if there is no previous message."
  (interactive "P")
  (if (null notmuch-tree-outline-visibility)
      (notmuch-tree-prev-matching-message pop-at-end)
    (notmuch-tree-outline--next t nil pop-at-end)))

(defun notmuch-tree-outline-next-thread ()
  "Next matching thread in forest, taking care of thread visibility."
  (interactive)
  (if (null notmuch-tree-outline-visibility)
      (notmuch-tree-next-thread)
    (notmuch-tree-outline--next nil t nil)))

(defun notmuch-tree-outline-previous-thread ()
  "Previous matching thread in forest, taking care of thread visibility."
  (interactive)
  (if (null notmuch-tree-outline-visibility)
      (notmuch-tree-prev-thread)
    (notmuch-tree-outline--next t t nil)))

;;;; Mode definition
(defvar notmuch-tree-outline-mode-lighter nil
  "The lighter mark for notmuch-tree-outline mode.
Usually empty since outline-minor-mode's lighter will be active.")

(define-minor-mode notmuch-tree-outline-mode
  "Minor mode allowing message trees to be folded as outlines.

When this mode is set, each thread and subthread in the results
list is treated as a foldable section, with its first message as
its header.

The mode just makes available in the tree buffer all the
keybindings in `outline-minor-mode', and binds the following
additional keys:

\\{notmuch-tree-outline-mode-map}

The customizable variable `notmuch-tree-outline-visibility'
controls how navigation in the buffer is affected by this mode:

  - If it is set to nil, `notmuch-tree-outline-previous',
    `notmuch-tree-outline-next', and their thread counterparts
    behave just as the corresponding notmuch-tree navigation keys
    when this mode is not enabled.

  - If, on the other hand, `notmuch-tree-outline-visibility' is
    set to a non-nil value, these commands hiding the outlines of
    the trees you are not reading as you move to new messages.

To enable notmuch-tree-outline-mode by default in all
notmuch-tree buffers, just set
`notmuch-tree-outline-mode-enabled' to t."
  :lighter notmuch-tree-outline-mode-lighter
  :keymap `((,(kbd "TAB") . outline-cycle)
	    (,(kbd "M-TAB") . outline-cycle-buffer)
	    ("n" . notmuch-tree-outline-next)
	    ("p" . notmuch-tree-outline-previous)
	    (,(kbd "M-n") . notmuch-tree-outline-next-thread)
	    (,(kbd "M-p") . notmuch-tree-outline-previous-thread))
  (outline-minor-mode notmuch-tree-outline-mode)
  (unless (derived-mode-p 'notmuch-tree-mode)
    (user-error "notmuch-tree-outline-mode is only meaningful for notmuch trees!"))
  (if notmuch-tree-outline-mode
      (progn (setq-local outline-regexp "^[^\n]+")
	     (setq-local outline-level #'notmuch-tree-outline--level)
	     (notmuch-tree-outline--set-visibility))
    (setq-local outline-regexp (default-value 'outline-regexp))
    (setq-local	outline-level (default-value 'outline-level))))

;;; _

(provide 'notmuch-tree)

;;; notmuch-tree.el ends here
