;; notmuch-pick.el --- displaying notmuch forests.
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
;; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Authors: David Edmondson <dme@dme.org>
;;          Mark Walters <markwalters1009@gmail.com>

(require 'mail-parse)

(require 'notmuch-lib)
(require 'notmuch-query)
(require 'notmuch-show)
(require 'notmuch) ;; XXX ATM, as notmuch-search-mode-map is defined here

(eval-when-compile (require 'cl))

(declare-function notmuch-call-notmuch-process "notmuch" (&rest args))
(declare-function notmuch-show "notmuch-show" (&rest args))
(declare-function notmuch-tag "notmuch" (query &rest tags))
(declare-function notmuch-show-strip-re "notmuch-show" (subject))
(declare-function notmuch-show-spaces-n "notmuch-show" (n))
(declare-function notmuch-read-query "notmuch" (prompt))
(declare-function notmuch-read-tag-changes "notmuch" (&optional initial-input &rest search-terms))
(declare-function notmuch-update-tags "notmuch" (current-tags tag-changes))
(declare-function notmuch-hello-trim "notmuch-hello" (search))
(declare-function notmuch-search-find-thread-id "notmuch" ())
(declare-function notmuch-search-find-subject "notmuch" ())

;; the following variable is defined in notmuch.el
(defvar notmuch-search-query-string)

(defgroup notmuch-pick nil
  "Showing message and thread structure."
  :group 'notmuch)

;; This is ugly. We can't run setup-show-out until it has been defined
;; which needs the keymap to be defined. So we defer setting up to
;; notmuch-pick-init.
(defcustom notmuch-pick-show-out nil
  "View selected messages in new window rather than split-pane."
  :type 'boolean
  :group 'notmuch-pick
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (when (fboundp 'notmuch-pick-setup-show-out)
	   (notmuch-pick-setup-show-out))))

(defcustom notmuch-pick-result-format
  `(("date" . "%12s  ")
    ("authors" . "%-20s")
    ("subject" . " %-54s ")
    ("tags" . "(%s)"))
  "Result formatting for Pick. Supported fields are: date,
        authors, subject, tags Note: subject includes the tree
        structure graphics, and the author string should not
        contain whitespace (put it in the neighbouring fields
        instead).  For example:
        (setq notmuch-pick-result-format \(\(\"authors\" . \"%-40s\"\)
                                             \(\"subject\" . \"%s\"\)\)\)"
  :type '(alist :key-type (string) :value-type (string))
  :group 'notmuch-pick)

(defcustom notmuch-pick-asynchronous-parser t
  "Use the asynchronous parser."
  :type 'boolean
  :group 'notmuch-pick)

;; Faces for messages that match the query.
(defface notmuch-pick-match-date-face
  '((t :inherit default))
  "Face used in pick mode for the date in messages matching the query."
  :group 'notmuch-pick
  :group 'notmuch-faces)

(defface notmuch-pick-match-author-face
  '((((class color)
      (background dark))
     (:foreground "OliveDrab1"))
    (((class color)
      (background light))
     (:foreground "dark blue"))
    (t
     (:bold t)))
  "Face used in pick mode for the date in messages matching the query."
  :group 'notmuch-pick
  :group 'notmuch-faces)

(defface notmuch-pick-match-subject-face
  '((t :inherit default))
  "Face used in pick mode for the subject in messages matching the query."
  :group 'notmuch-pick
  :group 'notmuch-faces)

(defface notmuch-pick-match-tag-face
  '((((class color)
      (background dark))
     (:foreground "OliveDrab1"))
    (((class color)
      (background light))
     (:foreground "navy blue" :bold t))
    (t
     (:bold t)))
  "Face used in pick mode for tags in messages matching the query."
  :group 'notmuch-pick
  :group 'notmuch-faces)

;; Faces for messages that do not match the query.
(defface notmuch-pick-no-match-date-face
  '((t (:foreground "gray")))
  "Face used in pick mode for non-matching dates."
  :group 'notmuch-pick
  :group 'notmuch-faces)

(defface notmuch-pick-no-match-subject-face
  '((t (:foreground "gray")))
  "Face used in pick mode for non-matching subjects."
  :group 'notmuch-pick
  :group 'notmuch-faces)

(defface notmuch-pick-no-match-author-face
  '((t (:foreground "gray")))
  "Face used in pick mode for the date in messages matching the query."
  :group 'notmuch-pick
  :group 'notmuch-faces)

(defface notmuch-pick-no-match-tag-face
  '((t (:foreground "gray")))
  "Face used in pick mode face for non-matching tags."
  :group 'notmuch-pick
  :group 'notmuch-faces)

(defvar notmuch-pick-previous-subject "")
(make-variable-buffer-local 'notmuch-pick-previous-subject)

;; The basic query i.e. the key part of the search request.
(defvar notmuch-pick-basic-query nil)
(make-variable-buffer-local 'notmuch-pick-basic-query)
;; The context of the search: i.e., useful but can be dropped.
(defvar notmuch-pick-query-context nil)
(make-variable-buffer-local 'notmuch-pick-query-context)
(defvar notmuch-pick-target-msg nil)
(make-variable-buffer-local 'notmuch-pick-target-msg)
(defvar notmuch-pick-buffer-name nil)
(make-variable-buffer-local 'notmuch-pick-buffer-name)
;; This variable is the window used for the message pane. It is set
;; in both the parent pick buffer and the child show buffer. It is
;; used to try and close the message pane when quitting pick or the
;; child show buffer.
(defvar notmuch-pick-message-window nil)
(make-variable-buffer-local 'notmuch-pick-message-window)
(put 'notmuch-pick-message-window 'permanent-local t)
(defvar notmuch-pick-message-buffer nil)
(make-variable-buffer-local 'notmuch-pick-message-buffer-name)
(put 'notmuch-pick-message-buffer-name 'permanent-local t)
(defvar notmuch-pick-process-state nil
  "Parsing state of the search process filter.")


(defvar notmuch-pick-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'notmuch-pick-show-message)
    (define-key map "q" 'notmuch-pick-quit)
    (define-key map "x" 'notmuch-pick-quit)
    (define-key map "?" 'notmuch-help)
    (define-key map "a" 'notmuch-pick-archive-message-then-next)
    (define-key map "=" 'notmuch-pick-refresh-view)
    (define-key map "s" 'notmuch-pick-to-search)
    (define-key map "z" 'notmuch-pick-to-pick)
    (define-key map "m" 'notmuch-pick-new-mail)
    (define-key map "f" 'notmuch-pick-forward-message)
    (define-key map "r" 'notmuch-pick-reply-sender)
    (define-key map "R" 'notmuch-pick-reply)
    (define-key map "n" 'notmuch-pick-next-matching-message)
    (define-key map "p" 'notmuch-pick-prev-matching-message)
    (define-key map "N" 'notmuch-pick-next-message)
    (define-key map "P" 'notmuch-pick-prev-message)
    (define-key map "|" 'notmuch-pick-pipe-message)
    (define-key map "-" 'notmuch-pick-remove-tag)
    (define-key map "+" 'notmuch-pick-add-tag)
    (define-key map " " 'notmuch-pick-scroll-or-next)
    (define-key map "b" 'notmuch-pick-scroll-message-window-back)
    map))
(fset 'notmuch-pick-mode-map notmuch-pick-mode-map)

(defun notmuch-pick-setup-show-out ()
  (let ((map notmuch-pick-mode-map))
    (if notmuch-pick-show-out
	(progn
	  (define-key map (kbd "M-RET") 'notmuch-pick-show-message)
	  (define-key map (kbd "RET") 'notmuch-pick-show-message-out))
      (progn
	(define-key map (kbd "RET") 'notmuch-pick-show-message)
	(define-key map (kbd "M-RET") 'notmuch-pick-show-message-out)))))

(defun notmuch-pick-get-message-properties ()
  "Return the properties of the current message as a plist.

Some useful entries are:
:headers - Property list containing the headers :Date, :Subject, :From, etc.
:tags - Tags for this message"
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) :notmuch-message-properties)))

(defun notmuch-pick-set-message-properties (props)
  (save-excursion
    (beginning-of-line)
    (put-text-property (point) (+ (point) 1) :notmuch-message-properties props)))

(defun notmuch-pick-set-prop (prop val &optional props)
  (let ((inhibit-read-only t)
	(props (or props
		   (notmuch-pick-get-message-properties))))
    (plist-put props prop val)
    (notmuch-pick-set-message-properties props)))

(defun notmuch-pick-get-prop (prop &optional props)
  (let ((props (or props
		   (notmuch-pick-get-message-properties))))
    (plist-get props prop)))

(defun notmuch-pick-set-tags (tags)
  "Set the tags of the current message."
  (notmuch-pick-set-prop :tags tags))

(defun notmuch-pick-get-tags ()
  "Return the tags of the current message."
  (notmuch-pick-get-prop :tags))

(defun notmuch-pick-get-message-id ()
  "Return the message id of the current message."
  (let ((id (notmuch-pick-get-prop :id)))
    (if id
	(notmuch-id-to-query id)
      nil)))

(defun notmuch-pick-get-match ()
  "Return whether the current message is a match."
  (interactive)
  (notmuch-pick-get-prop :match))

(defun notmuch-pick-refresh-result ()
  (let ((init-point (point))
	(end (line-end-position))
	(msg (notmuch-pick-get-message-properties))
	(inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (1+ (line-end-position)))
    (notmuch-pick-insert-msg msg)
    (let ((new-end (line-end-position)))
      (goto-char (if (= init-point end)
		     new-end
		   (min init-point (- new-end 1)))))))

(defun notmuch-pick-tag-update-display (&optional tag-changes)
  "Update display for TAG-CHANGES to current message.

Does NOT change the database."
  (let* ((current-tags (notmuch-pick-get-tags))
	 (new-tags (notmuch-update-tags current-tags tag-changes)))
    (unless (equal current-tags new-tags)
      (notmuch-pick-set-tags new-tags)
      (notmuch-pick-refresh-result))))

(defun notmuch-pick-tag (&optional tag-changes)
  "Change tags for the current message"
  (interactive)
  (setq tag-changes (notmuch-tag (notmuch-pick-get-message-id) tag-changes))
  (notmuch-pick-tag-update-display tag-changes))

(defun notmuch-pick-add-tag ()
  "Same as `notmuch-pick-tag' but sets initial input to '+'."
  (interactive)
  (notmuch-pick-tag "+"))

(defun notmuch-pick-remove-tag ()
  "Same as `notmuch-pick-tag' but sets initial input to '-'."
  (interactive)
  (notmuch-pick-tag "-"))

;; The next two functions close the message window before searching or
;; picking but they do so after the user has entered the query (in
;; case the user was basing the query on something in the message
;; window).

(defun notmuch-pick-to-search ()
  "Run \"notmuch search\" with the given `query' and display results."
  (interactive)
  (let ((query (notmuch-read-query "Notmuch search: ")))
    (notmuch-pick-close-message-window)
    (notmuch-search query)))

(defun notmuch-pick-to-pick ()
  "Run a query and display results in experimental notmuch-pick mode"
  (interactive)
  (let ((query (notmuch-read-query "Notmuch pick: ")))
    (notmuch-pick-close-message-window)
    (notmuch-pick query)))

;; This function should be in notmuch-hello.el but we are trying to
;; minimise impact on the rest of the codebase.
(defun notmuch-pick-from-hello (&optional search)
  "Run a query and display results in experimental notmuch-pick mode"
  (interactive)
  (unless (null search)
    (setq search (notmuch-hello-trim search))
    (let ((history-delete-duplicates t))
      (add-to-history 'notmuch-search-history search)))
  (notmuch-pick search))

;; This function should be in notmuch-show.el but be we trying to
;; minimise impact on the rest of the codebase.
(defun notmuch-pick-from-show-current-query ()
  "Call notmuch pick with the current query"
  (interactive)
  (notmuch-pick notmuch-show-thread-id
		notmuch-show-query-context
		(notmuch-show-get-message-id)))

;; This function should be in notmuch.el but be we trying to minimise
;; impact on the rest of the codebase.
(defun notmuch-pick-from-search-current-query ()
  "Call notmuch pick with the current query"
  (interactive)
  (notmuch-pick notmuch-search-query-string))

;; This function should be in notmuch.el but be we trying to minimise
;; impact on the rest of the codebase.
(defun notmuch-pick-from-search-thread ()
  "Show the selected thread with notmuch-pick"
  (interactive)
  (notmuch-pick (notmuch-search-find-thread-id)
                notmuch-search-query-string
		nil
                (notmuch-prettify-subject (notmuch-search-find-subject)))
  (notmuch-pick-show-match-message-with-wait))

(defun notmuch-pick-message-window-kill-hook ()
  (let ((buffer (current-buffer)))
    (when (and (window-live-p notmuch-pick-message-window)
	       (eq (window-buffer notmuch-pick-message-window) buffer))
      ;; We do not want an error if this is the sole window in the
      ;; frame and I do not know how to test for that in emacs pre
      ;; 24. Hence we just ignore-errors.
      (ignore-errors
	(delete-window notmuch-pick-message-window)))))

(defun notmuch-pick-show-message ()
  "Show the current message (in split-pane)."
  (interactive)
  (let ((id (notmuch-pick-get-message-id))
	(inhibit-read-only t)
	buffer)
    (when id
      ;; We close and reopen the window to kill off un-needed buffers
      ;; this might cause flickering but seems ok.
      (notmuch-pick-close-message-window)
      (setq notmuch-pick-message-window
	    (split-window-vertically (/ (window-height) 4)))
      (with-selected-window notmuch-pick-message-window
	;; Since we are only displaying one message do not indent.
	(let ((notmuch-show-indent-messages-width 0)
	      (notmuch-show-only-matching-messages t))
	  (setq buffer (notmuch-show id nil nil nil))))
      ;; We need the `let' as notmuch-pick-message-window is buffer local.
      (let ((window notmuch-pick-message-window))
	(with-current-buffer buffer
	  (setq notmuch-pick-message-window window)
	  (add-hook 'kill-buffer-hook 'notmuch-pick-message-window-kill-hook)))
      (when notmuch-show-mark-read-tags
	(notmuch-pick-tag-update-display notmuch-show-mark-read-tags))
      (setq notmuch-pick-message-buffer buffer))))

(defun notmuch-pick-show-message-out ()
  "Show the current message (in whole window)."
  (interactive)
  (let ((id (notmuch-pick-get-message-id))
	(inhibit-read-only t)
	buffer)
    (when id
      ;; We close the window to kill off un-needed buffers.
      (notmuch-pick-close-message-window)
      (notmuch-show id nil nil nil))))

(defun notmuch-pick-scroll-message-window ()
  "Scroll the message window (if it exists)"
  (interactive)
  (when (window-live-p notmuch-pick-message-window)
    (with-selected-window notmuch-pick-message-window
      (if (pos-visible-in-window-p (point-max))
	  t
	(scroll-up)))))

(defun notmuch-pick-scroll-message-window-back ()
  "Scroll the message window back(if it exists)"
  (interactive)
  (when (window-live-p notmuch-pick-message-window)
    (with-selected-window notmuch-pick-message-window
      (if (pos-visible-in-window-p (point-min))
	  t
	(scroll-down)))))

(defun notmuch-pick-scroll-or-next ()
  "Scroll the message window. If it at end go to next message."
  (interactive)
  (when (notmuch-pick-scroll-message-window)
    (notmuch-pick-next-matching-message)))

(defun notmuch-pick-quit ()
  "Close the split view or exit pick."
  (interactive)
  (unless (notmuch-pick-close-message-window)
    (kill-buffer (current-buffer))))

(defun notmuch-pick-close-message-window ()
  "Close the message-window. Return t if close succeeds."
  (interactive)
  (when (and (window-live-p notmuch-pick-message-window)
	     (eq (window-buffer notmuch-pick-message-window) notmuch-pick-message-buffer))
    (delete-window notmuch-pick-message-window)
    (unless (get-buffer-window-list notmuch-pick-message-buffer)
      (kill-buffer notmuch-pick-message-buffer))
    t))

(defun notmuch-pick-archive-message (&optional unarchive)
  "Archive the current message.

Archive the current message by applying the tag changes in
`notmuch-archive-tags' to it (remove the \"inbox\" tag by
default). If a prefix argument is given, the message will be
\"unarchived\", i.e. the tag changes in `notmuch-archive-tags'
will be reversed."
  (interactive "P")
  (when notmuch-archive-tags
    (apply 'notmuch-pick-tag
	   (notmuch-tag-change-list notmuch-archive-tags unarchive))))

(defun notmuch-pick-archive-message-then-next (&optional unarchive)
  "Archive the current message and move to next matching message."
  (interactive "P")
  (notmuch-pick-archive-message unarchive)
  (notmuch-pick-next-matching-message))

(defun notmuch-pick-next-message ()
  "Move to next message."
  (interactive)
  (forward-line)
  (when (window-live-p notmuch-pick-message-window)
    (notmuch-pick-show-message)))

(defun notmuch-pick-prev-message ()
  "Move to previous message."
  (interactive)
  (forward-line -1)
  (when (window-live-p notmuch-pick-message-window)
    (notmuch-pick-show-message)))

(defun notmuch-pick-prev-matching-message ()
  "Move to previous matching message."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp)) (not (notmuch-pick-get-match)))
    (forward-line -1))
  (when (window-live-p notmuch-pick-message-window)
    (notmuch-pick-show-message)))

(defun notmuch-pick-next-matching-message ()
  "Move to next matching message."
  (interactive)
  (forward-line)
  (while (and (not (eobp)) (not (notmuch-pick-get-match)))
    (forward-line))
  (when (window-live-p notmuch-pick-message-window)
    (notmuch-pick-show-message)))

(defun notmuch-pick-show-match-message-with-wait ()
  "Show the first matching message but wait for it to appear or search to finish."
  (interactive)
  (unless (notmuch-pick-get-match)
    (notmuch-pick-next-matching-message))
  (while (and (not (notmuch-pick-get-match))
	      (get-buffer-process (current-buffer)))
    (message "waiting for message")
    (sit-for 0.1)
    (goto-char (point-min))
    (unless (notmuch-pick-get-match)
      (notmuch-pick-next-matching-message)))
  (message nil)
  (when (notmuch-pick-get-match)
    (notmuch-pick-show-message)))

(defun notmuch-pick-refresh-view ()
  "Refresh view."
  (interactive)
  (let ((inhibit-read-only t)
	(basic-query notmuch-pick-basic-query)
	(query-context notmuch-pick-query-context)
	(target (notmuch-pick-get-message-id))
	(buffer-name notmuch-pick-buffer-name))
    (erase-buffer)
    (notmuch-pick-worker basic-query
			 query-context
			 target
			 (get-buffer buffer-name))))

(defmacro with-current-notmuch-pick-message (&rest body)
  "Evaluate body with current buffer set to the text of current message"
  `(save-excursion
     (let ((id (notmuch-pick-get-message-id)))
       (let ((buf (generate-new-buffer (concat "*notmuch-msg-" id "*"))))
         (with-current-buffer buf
	    (call-process notmuch-command nil t nil "show" "--format=raw" id)
           ,@body)
	 (kill-buffer buf)))))

(defun notmuch-pick-new-mail (&optional prompt-for-sender)
  "Compose new mail."
  (interactive "P")
  (notmuch-pick-close-message-window)
  (notmuch-mua-new-mail prompt-for-sender ))

(defun notmuch-pick-forward-message (&optional prompt-for-sender)
  "Forward the current message."
  (interactive "P")
  (notmuch-pick-close-message-window)
  (with-current-notmuch-pick-message
   (notmuch-mua-new-forward-message prompt-for-sender)))

(defun notmuch-pick-reply (&optional prompt-for-sender)
  "Reply to the sender and all recipients of the current message."
  (interactive "P")
  (notmuch-pick-close-message-window)
  (notmuch-mua-new-reply (notmuch-pick-get-message-id) prompt-for-sender t))

(defun notmuch-pick-reply-sender (&optional prompt-for-sender)
  "Reply to the sender of the current message."
  (interactive "P")
  (notmuch-pick-close-message-window)
  (notmuch-mua-new-reply (notmuch-pick-get-message-id) prompt-for-sender nil))

;; Shamelessly stolen from notmuch-show.el: maybe should be unified.
(defun notmuch-pick-pipe-message (command)
  "Pipe the contents of the current message to the given command.

The given command will be executed with the raw contents of the
current email message as stdin. Anything printed by the command
to stdout or stderr will appear in the *notmuch-pipe* buffer.

When invoked with a prefix argument, the command will receive all
open messages in the current thread (formatted as an mbox) rather
than only the current message."
  (interactive "sPipe message to command: ")
  (let ((shell-command
	 (concat notmuch-command " show --format=raw "
		 (shell-quote-argument (notmuch-pick-get-message-id)) " | " command))
	 (buf (get-buffer-create (concat "*notmuch-pipe*"))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((exit-code (call-process-shell-command shell-command nil buf)))
	(goto-char (point-max))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(unless (zerop exit-code)
	  (switch-to-buffer-other-window buf)
	  (message (format "Command '%s' exited abnormally with code %d"
			   shell-command exit-code)))))))

(defun notmuch-pick-clean-address (address)
  "Try to clean a single email ADDRESS for display. Return
AUTHOR_NAME if present, otherwise return AUTHOR_EMAIL. Return
unchanged ADDRESS if parsing fails."
  (let* ((clean-address (notmuch-clean-address address))
	 (p-address (car clean-address))
	 (p-name (cdr clean-address)))

    ;; If we have a name return that otherwise return the address.
    (or p-name p-address)))

(defun notmuch-pick-insert-field (field format-string msg)
  (let* ((headers (plist-get msg :headers))
	(match (plist-get msg :match)))
    (cond
     ((string-equal field "date")
      (let ((face (if match
		      'notmuch-pick-match-date-face
		    'notmuch-pick-no-match-date-face)))
	(insert (propertize (format format-string (plist-get msg :date_relative))
			    'face face))))

     ((string-equal field "subject")
      (let ((tree-status (plist-get msg :tree-status))
	    (bare-subject (notmuch-show-strip-re (plist-get headers :Subject)))
	    (face (if match
		      'notmuch-pick-match-subject-face
		    'notmuch-pick-no-match-subject-face)))
	(insert (propertize (format format-string
				    (concat
				     (mapconcat #'identity (reverse tree-status) "")
				     (if (string= notmuch-pick-previous-subject bare-subject)
					 " ..."
				       bare-subject)))
			    'face face))
	(setq notmuch-pick-previous-subject bare-subject)))

     ((string-equal field "authors")
      (let ((author (notmuch-pick-clean-address (plist-get headers :From)))
	    (len (length (format format-string "")))
	    (face (if match
		      'notmuch-pick-match-author-face
		    'notmuch-pick-no-match-author-face)))
	(when (> (length author) len)
	  (setq author (substring author 0 len)))
	(insert (propertize (format format-string author)
			    'face face))))

     ((string-equal field "tags")
      (let ((tags (plist-get msg :tags))
	    (face (if match
			  'notmuch-pick-match-tag-face
			'notmuch-pick-no-match-tag-face)))
	(when tags
	  (insert (propertize (format format-string
				      (mapconcat #'identity tags ", "))
			      'face face))))))))

(defun notmuch-pick-insert-msg (msg)
  "Insert the message MSG according to notmuch-pick-result-format"
  (dolist (spec notmuch-pick-result-format)
    (notmuch-pick-insert-field (car spec) (cdr spec) msg))
  (notmuch-pick-set-message-properties msg)
  (insert "\n"))

(defun notmuch-pick-goto-and-insert-msg (msg)
  "Insert msg at the end of the buffer. Move point to msg if it is the target"
  (save-excursion
    (goto-char (point-max))
    (notmuch-pick-insert-msg msg))
  (let ((msg-id (notmuch-id-to-query (plist-get msg :id))))
    (when (string= msg-id notmuch-pick-target-msg)
      (setq notmuch-pick-target-msg "found")
      (goto-char (point-max))
      (forward-line -1))))

(defun notmuch-pick-insert-tree (tree depth tree-status first last)
  "Insert the message tree TREE at depth DEPTH in the current thread."
  (let ((msg (car tree))
	(replies (cadr tree)))

      (cond
       ((and (< 0 depth) (not last))
	(push "├" tree-status))
       ((and (< 0 depth) last)
	(push "╰" tree-status))
       ((and (eq 0 depth) first last)
;;	  (push "─" tree-status)) choice between this and next line is matter of taste.
	(push " " tree-status))
       ((and (eq 0 depth) first (not last))
	  (push "┬" tree-status))
       ((and (eq 0 depth) (not first) last)
	(push "╰" tree-status))
       ((and (eq 0 depth) (not first) (not last))
	(push "├" tree-status)))

      (push (concat (if replies "┬" "─") "►") tree-status)
      (notmuch-pick-goto-and-insert-msg (plist-put msg :tree-status tree-status))
      (pop tree-status)
      (pop tree-status)

      (if last
	  (push " " tree-status)
	(push "│" tree-status))

    (notmuch-pick-insert-thread replies (1+ depth) tree-status)))

(defun notmuch-pick-insert-thread (thread depth tree-status)
  "Insert the thread THREAD at depth DEPTH >= 1 in the current forest."
  (let ((n (length thread)))
    (loop for tree in thread
	  for count from 1 to n

	  do (notmuch-pick-insert-tree tree depth tree-status (eq count 1) (eq count n)))))

(defun notmuch-pick-insert-forest-thread (forest-thread)
  (let (tree-status)
    ;; Reset at the start of each main thread.
    (setq notmuch-pick-previous-subject nil)
    (notmuch-pick-insert-thread forest-thread 0 tree-status)))

(defun notmuch-pick-insert-forest (forest)
  (mapc 'notmuch-pick-insert-forest-thread forest))

(defun notmuch-pick-mode ()
  "Major mode displaying messages (as opposed to threads) of of a notmuch search.

This buffer contains the results of a \"notmuch pick\" of your
email archives. Each line in the buffer represents a single
message giving the relative date, the author, subject, and any
tags.

Pressing \\[notmuch-pick-show-message] on any line displays that message.

Complete list of currently available key bindings:

\\{notmuch-pick-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (use-local-map notmuch-pick-mode-map)
  (setq major-mode 'notmuch-pick-mode
	mode-name "notmuch-pick")
  (hl-line-mode 1)
  (setq buffer-read-only t
	truncate-lines t))

(defun notmuch-pick-process-sentinel (proc msg)
  "Add a message to let user know when \"notmuch pick\" exits"
  (let ((buffer (process-buffer proc))
	(status (process-status proc))
	(exit-status (process-exit-status proc))
	(never-found-target-thread nil))
    (when (memq status '(exit signal))
        (kill-buffer (process-get proc 'parse-buf))
	(if (buffer-live-p buffer)
	    (with-current-buffer buffer
	      (save-excursion
		(let ((inhibit-read-only t)
		      (atbob (bobp)))
		  (goto-char (point-max))
		  (if (eq status 'signal)
		      (insert "Incomplete search results (pick process was killed).\n"))
		  (when (eq status 'exit)
		    (insert "End of search results.")
		    (unless (= exit-status 0)
		      (insert (format " (process returned %d)" exit-status)))
		    (insert "\n")))))))))


(defun notmuch-pick-show-error (string &rest objects)
  (save-excursion
    (goto-char (point-max))
    (insert "Error: Unexpected output from notmuch search:\n")
    (insert (apply #'format string objects))
    (insert "\n")))


(defun notmuch-pick-process-filter (proc string)
  "Process and filter the output of \"notmuch show\" (for pick)"
  (let ((results-buf (process-buffer proc))
        (parse-buf (process-get proc 'parse-buf))
        (inhibit-read-only t)
        done)
    (if (not (buffer-live-p results-buf))
        (delete-process proc)
      (with-current-buffer parse-buf
        ;; Insert new data
        (save-excursion
          (goto-char (point-max))
          (insert string))
	(notmuch-sexp-parse-partial-list 'notmuch-pick-insert-forest-thread
					 results-buf)))))

(defun notmuch-pick-worker (basic-query &optional query-context target buffer)
  (interactive)
  (notmuch-pick-mode)
  (setq notmuch-pick-basic-query basic-query)
  (setq notmuch-pick-query-context query-context)
  (setq notmuch-pick-buffer-name (buffer-name buffer))
  (setq notmuch-pick-target-msg target)

  (erase-buffer)
  (goto-char (point-min))
  (let* ((search-args (concat basic-query
		       (if query-context (concat " and (" query-context ")"))
		       ))
	 (message-arg "--entire-thread"))
    (if (equal (car (process-lines notmuch-command "count" search-args)) "0")
	(setq search-args basic-query))
    (if notmuch-pick-asynchronous-parser
	(let ((proc (start-process
		     "notmuch-pick" buffer
		     notmuch-command "show" "--body=false" "--format=sexp"
		     message-arg search-args))
	      ;; Use a scratch buffer to accumulate partial output.
              ;; This buffer will be killed by the sentinel, which
              ;; should be called no matter how the process dies.
              (parse-buf (generate-new-buffer " *notmuch pick parse*")))
          (process-put proc 'parse-buf parse-buf)
	  (set-process-sentinel proc 'notmuch-pick-process-sentinel)
	  (set-process-filter proc 'notmuch-pick-process-filter)
	  (set-process-query-on-exit-flag proc nil))
      (progn
	(notmuch-pick-insert-forest
	 (notmuch-query-get-threads
	  (list "--body=false" message-arg search-args)))
	(save-excursion
	  (goto-char (point-max))
	  (insert "End of search results.\n"))))))


(defun notmuch-pick (&optional query query-context target buffer-name show-first-match)
  "Run notmuch pick with the given `query' and display the results"
  (interactive "sNotmuch pick: ")
  (if (null query)
      (setq query (notmuch-read-query "Notmuch pick: ")))
  (let ((buffer (get-buffer-create (generate-new-buffer-name
				    (or buffer-name
					(concat "*notmuch-pick-" query "*")))))
	(inhibit-read-only t))

    (switch-to-buffer buffer)
    ;; Don't track undo information for this buffer
    (set 'buffer-undo-list t)

    (notmuch-pick-worker query query-context target buffer)

    (setq truncate-lines t)
    (when show-first-match
      (notmuch-pick-show-match-message-with-wait))))


;; Set up key bindings from the rest of notmuch.
(define-key 'notmuch-search-mode-map "z" 'notmuch-pick)
(define-key 'notmuch-search-mode-map "Z" 'notmuch-pick-from-search-current-query)
(define-key 'notmuch-search-mode-map (kbd "M-RET") 'notmuch-pick-from-search-thread)
(define-key 'notmuch-hello-mode-map "z" 'notmuch-pick-from-hello)
(define-key 'notmuch-show-mode-map "z" 'notmuch-pick)
(define-key 'notmuch-show-mode-map "Z" 'notmuch-pick-from-show-current-query)
(notmuch-pick-setup-show-out)
(message "Initialised notmuch-pick")

(provide 'notmuch-pick)
