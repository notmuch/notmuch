;; notmuch-show.el --- displaying notmuch forests.
;;
;; Copyright © Carl Worth
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
;; Authors: Carl Worth <cworth@cworth.org>
;;          David Edmondson <dme@dme.org>

(eval-when-compile (require 'cl))
(require 'mm-view)
(require 'message)
(require 'mm-decode)
(require 'mailcap)
(require 'icalendar)
(require 'goto-addr)

(require 'notmuch-lib)
(require 'notmuch-tag)
(require 'notmuch-query)
(require 'notmuch-wash)
(require 'notmuch-mua)
(require 'notmuch-crypto)
(require 'notmuch-print)

(declare-function notmuch-call-notmuch-process "notmuch" (&rest args))
(declare-function notmuch-fontify-headers "notmuch" nil)
(declare-function notmuch-search-next-thread "notmuch" nil)
(declare-function notmuch-search-show-thread "notmuch" nil)

(defcustom notmuch-message-headers '("Subject" "To" "Cc" "Date")
  "Headers that should be shown in a message, in this order.

For an open message, all of these headers will be made visible
according to `notmuch-message-headers-visible' or can be toggled
with `notmuch-show-toggle-visibility-headers'. For a closed message,
only the first header in the list will be visible."
  :type '(repeat string)
  :group 'notmuch-show)

(defcustom notmuch-message-headers-visible t
  "Should the headers be visible by default?

If this value is non-nil, then all of the headers defined in
`notmuch-message-headers' will be visible by default in the display
of each message. Otherwise, these headers will be hidden and
`notmuch-show-toggle-visibility-headers' can be used to make them
visible for any given message."
  :type 'boolean
  :group 'notmuch-show)

(defcustom notmuch-show-relative-dates t
  "Display relative dates in the message summary line."
  :type 'boolean
  :group 'notmuch-show)

(defvar notmuch-show-markup-headers-hook '(notmuch-show-colour-headers)
  "A list of functions called to decorate the headers listed in
`notmuch-message-headers'.")

(defcustom notmuch-show-hook '(notmuch-show-turn-on-visual-line-mode)
  "Functions called after populating a `notmuch-show' buffer."
  :type 'hook
  :options '(notmuch-show-turn-on-visual-line-mode)
  :group 'notmuch-show
  :group 'notmuch-hooks)

(defcustom notmuch-show-insert-text/plain-hook '(notmuch-wash-wrap-long-lines
						 notmuch-wash-tidy-citations
						 notmuch-wash-elide-blank-lines
						 notmuch-wash-excerpt-citations)
  "Functions used to improve the display of text/plain parts."
  :type 'hook
  :options '(notmuch-wash-convert-inline-patch-to-part
	     notmuch-wash-wrap-long-lines
	     notmuch-wash-tidy-citations
	     notmuch-wash-elide-blank-lines
	     notmuch-wash-excerpt-citations)
  :group 'notmuch-show
  :group 'notmuch-hooks)

;; Mostly useful for debugging.
(defcustom notmuch-show-all-multipart/alternative-parts t
  "Should all parts of multipart/alternative parts be shown?"
  :type 'boolean
  :group 'notmuch-show)

(defcustom notmuch-show-indent-messages-width 1
  "Width of message indentation in threads.

Messages are shown indented according to their depth in a thread.
This variable determines the width of this indentation measured
in number of blanks.  Defaults to `1', choose `0' to disable
indentation."
  :type 'integer
  :group 'notmuch-show)

(defcustom notmuch-show-indent-multipart nil
  "Should the sub-parts of a multipart/* part be indented?"
  ;; dme: Not sure which is a good default.
  :type 'boolean
  :group 'notmuch-show)

(defcustom notmuch-show-part-button-default-action 'notmuch-show-save-part
  "Default part header button action (on ENTER or mouse click)."
  :group 'notmuch-show
  :type '(choice (const :tag "Save part"
			notmuch-show-save-part)
		 (const :tag "View part"
			notmuch-show-view-part)
		 (const :tag "View interactively"
			notmuch-show-interactively-view-part)))

(defcustom notmuch-show-only-matching-messages nil
  "Only matching messages are shown by default."
  :type 'boolean
  :group 'notmuch-show)

(defvar notmuch-show-thread-id nil)
(make-variable-buffer-local 'notmuch-show-thread-id)
(put 'notmuch-show-thread-id 'permanent-local t)

(defvar notmuch-show-parent-buffer nil)
(make-variable-buffer-local 'notmuch-show-parent-buffer)
(put 'notmuch-show-parent-buffer 'permanent-local t)

(defvar notmuch-show-query-context nil)
(make-variable-buffer-local 'notmuch-show-query-context)
(put 'notmuch-show-query-context 'permanent-local t)

(defvar notmuch-show-process-crypto nil)
(make-variable-buffer-local 'notmuch-show-process-crypto)
(put 'notmuch-show-process-crypto 'permanent-local t)

(defvar notmuch-show-elide-non-matching-messages nil)
(make-variable-buffer-local 'notmuch-show-elide-non-matching-messages)
(put 'notmuch-show-elide-non-matching-messages 'permanent-local t)

(defvar notmuch-show-indent-content t)
(make-variable-buffer-local 'notmuch-show-indent-content)
(put 'notmuch-show-indent-content 'permanent-local t)

(defcustom notmuch-show-stash-mlarchive-link-alist
  '(("Gmane" . "http://mid.gmane.org/")
    ("MARC" . "http://marc.info/?i=")
    ("Mail Archive, The" . "http://mail-archive.com/search?l=mid&q=")
    ;; FIXME: can these services be searched by `Message-Id' ?
    ;; ("MarkMail" . "http://markmail.org/")
    ;; ("Nabble" . "http://nabble.com/")
    ;; ("opensubscriber" . "http://opensubscriber.com/")
    )
  "List of Mailing List Archives to use when stashing links.

These URIs are concatenated with the current message's
Message-Id in `notmuch-show-stash-mlarchive-link'."
  :type '(alist :key-type (string :tag "Name")
		:value-type (string :tag "URL"))
  :group 'notmuch-show)

(defcustom notmuch-show-stash-mlarchive-link-default "Gmane"
  "Default Mailing List Archive to use when stashing links.

This is used when `notmuch-show-stash-mlarchive-link' isn't
provided with an MLA argument nor `completing-read' input."
  :type `(choice
	  ,@(mapcar
	     (lambda (mla)
	       (list 'const :tag (car mla) :value (car mla)))
	     notmuch-show-stash-mlarchive-link-alist))
  :group 'notmuch-show)

(defcustom notmuch-show-mark-read-tags '("-unread")
  "List of tag changes to apply to a message when it is marked as read.

Tags starting with \"+\" (or not starting with either \"+\" or
\"-\") in the list will be added, and tags starting with \"-\"
will be removed from the message being marked as read.

For example, if you wanted to remove an \"unread\" tag and add a
\"read\" tag (which would make little sense), you would set:
    (\"-unread\" \"+read\")"
  :type '(repeat string)
  :group 'notmuch-show)


(defmacro with-current-notmuch-show-message (&rest body)
  "Evaluate body with current buffer set to the text of current message"
  `(save-excursion
     (let ((id (notmuch-show-get-message-id)))
       (let ((buf (generate-new-buffer (concat "*notmuch-msg-" id "*"))))
         (with-current-buffer buf
	    (call-process notmuch-command nil t nil "show" "--format=raw" id)
           ,@body)
	 (kill-buffer buf)))))

(defun notmuch-show-turn-on-visual-line-mode ()
  "Enable Visual Line mode."
  (visual-line-mode t))

(defun notmuch-show-view-all-mime-parts ()
  "Use external viewers to view all attachments from the current message."
  (interactive)
  (with-current-notmuch-show-message
   ;; We override the mm-inline-media-tests to indicate which message
   ;; parts are already sufficiently handled by the original
   ;; presentation of the message in notmuch-show mode. These parts
   ;; will be inserted directly into the temporary buffer of
   ;; with-current-notmuch-show-message and silently discarded.
   ;;
   ;; Any MIME part not explicitly mentioned here will be handled by an
   ;; external viewer as configured in the various mailcap files.
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

(defun notmuch-show-with-message-as-text (fn)
  "Apply FN to a text representation of the current message.

FN is called with one argument, the message properties. It should
operation on the contents of the current buffer."

  ;; Remake the header to ensure that all information is available.
  (let* ((to (notmuch-show-get-to))
	 (cc (notmuch-show-get-cc))
	 (from (notmuch-show-get-from))
	 (subject (notmuch-show-get-subject))
	 (date (notmuch-show-get-date))
	 (tags (notmuch-show-get-tags))
	 (depth (notmuch-show-get-depth))

	 (header (concat
		  "Subject: " subject "\n"
		  "To: " to "\n"
		  (if (not (string= cc ""))
		      (concat "Cc: " cc "\n")
		    "")
		  "From: " from "\n"
		  "Date: " date "\n"
		  (if tags
		      (concat "Tags: "
			      (mapconcat #'identity tags ", ") "\n")
		    "")))
	 (all (buffer-substring (notmuch-show-message-top)
				(notmuch-show-message-bottom)))

	 (props (notmuch-show-get-message-properties))
	 (indenting notmuch-show-indent-content))
    (with-temp-buffer
      (insert all)
      (if indenting
	  (indent-rigidly (point-min) (point-max) (- depth)))
      ;; Remove the original header.
      (goto-char (point-min))
      (re-search-forward "^$" (point-max) nil)
      (delete-region (point-min) (point))
      (insert header)
      (funcall fn props))))

(defun notmuch-show-print-message ()
  "Print the current message."
  (interactive)
  (notmuch-show-with-message-as-text 'notmuch-print-message))

(defun notmuch-show-fontify-header ()
  (let ((face (cond
	       ((looking-at "[Tt]o:")
		'message-header-to)
	       ((looking-at "[Bb]?[Cc][Cc]:")
		'message-header-cc)
	       ((looking-at "[Ss]ubject:")
		'message-header-subject)
	       ((looking-at "[Ff]rom:")
		'message-header-from)
	       (t
		'message-header-other))))

    (overlay-put (make-overlay (point) (re-search-forward ":"))
		 'face 'message-header-name)
    (overlay-put (make-overlay (point) (re-search-forward ".*$"))
		 'face face)))

(defun notmuch-show-colour-headers ()
  "Apply some colouring to the current headers."
  (goto-char (point-min))
  (while (looking-at "^[A-Za-z][-A-Za-z0-9]*:")
    (notmuch-show-fontify-header)
    (forward-line)))

(defun notmuch-show-spaces-n (n)
  "Return a string comprised of `n' spaces."
  (make-string n ? ))

(defun notmuch-show-update-tags (tags)
  "Update the displayed tags of the current message."
  (save-excursion
    (goto-char (notmuch-show-message-top))
    (if (re-search-forward "(\\([^()]*\\))$" (line-end-position) t)
	(let ((inhibit-read-only t))
	  (replace-match (concat "("
				 (propertize (mapconcat 'identity tags " ")
					     'face 'notmuch-tag-face)
				 ")"))))))

(defun notmuch-show-clean-address (address)
  "Try to clean a single email ADDRESS for display.  Return
unchanged ADDRESS if parsing fails."
  (condition-case nil
    (let (p-name p-address)
      ;; It would be convenient to use `mail-header-parse-address',
      ;; but that expects un-decoded mailbox parts, whereas our
      ;; mailbox parts are already decoded (and hence may contain
      ;; UTF-8). Given that notmuch should handle most of the awkward
      ;; cases, some simple string deconstruction should be sufficient
      ;; here.
      (cond
       ;; "User <user@dom.ain>" style.
       ((string-match "\\(.*\\) <\\(.*\\)>" address)
	(setq p-name (match-string 1 address)
	      p-address (match-string 2 address)))

       ;; "<user@dom.ain>" style.
       ((string-match "<\\(.*\\)>" address)
	(setq p-address (match-string 1 address)))

       ;; Everything else.
       (t
	(setq p-address address)))

      (when p-name
	;; Remove elements of the mailbox part that are not relevant for
	;; display, even if they are required during transport:
	;;
	;; Backslashes.
	(setq p-name (replace-regexp-in-string "\\\\" "" p-name))

	;; Outer single and double quotes, which might be nested.
	(loop
	 with start-of-loop
	 do (setq start-of-loop p-name)

	 when (string-match "^\"\\(.*\\)\"$" p-name)
	 do (setq p-name (match-string 1 p-name))

	 when (string-match "^'\\(.*\\)'$" p-name)
	 do (setq p-name (match-string 1 p-name))

	 until (string= start-of-loop p-name)))

      ;; If the address is 'foo@bar.com <foo@bar.com>' then show just
      ;; 'foo@bar.com'.
      (when (string= p-name p-address)
	(setq p-name nil))

      ;; If no name results, return just the address.
      (if (not p-name)
	  p-address
	;; Otherwise format the name and address together.
	(concat p-name " <" p-address ">")))
    (error address)))

(defun notmuch-show-insert-headerline (headers date tags depth)
  "Insert a notmuch style headerline based on HEADERS for a
message at DEPTH in the current thread."
  (let ((start (point)))
    (insert (notmuch-show-spaces-n (* notmuch-show-indent-messages-width depth))
	    (notmuch-show-clean-address (plist-get headers :From))
	    " ("
	    date
	    ") ("
	    (propertize (mapconcat 'identity tags " ")
			'face 'notmuch-tag-face)
	    ")\n")
    (overlay-put (make-overlay start (point)) 'face 'notmuch-message-summary-face)))

(defun notmuch-show-insert-header (header header-value)
  "Insert a single header."
  (insert header ": " header-value "\n"))

(defun notmuch-show-insert-headers (headers)
  "Insert the headers of the current message."
  (let ((start (point)))
    (mapc (lambda (header)
	    (let* ((header-symbol (intern (concat ":" header)))
		   (header-value (plist-get headers header-symbol)))
	      (if (and header-value
		       (not (string-equal "" header-value)))
		  (notmuch-show-insert-header header header-value))))
	  notmuch-message-headers)
    (save-excursion
      (save-restriction
	(narrow-to-region start (point-max))
	(run-hooks 'notmuch-show-markup-headers-hook)))))

(define-button-type 'notmuch-show-part-button-type
  'action 'notmuch-show-part-button-default
  'keymap 'notmuch-show-part-button-map
  'follow-link t
  'face 'message-mml)

(defvar notmuch-show-part-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "s" 'notmuch-show-part-button-save)
    (define-key map "v" 'notmuch-show-part-button-view)
    (define-key map "o" 'notmuch-show-part-button-interactively-view)
    (define-key map "|" 'notmuch-show-part-button-pipe)
    map)
  "Submap for button commands")
(fset 'notmuch-show-part-button-map notmuch-show-part-button-map)

(defun notmuch-show-insert-part-header (nth content-type declared-type &optional name comment)
  (let ((button))
    (setq button
	  (insert-button
	   (concat "[ "
		   (if name (concat name ": ") "")
		   declared-type
		   (if (not (string-equal declared-type content-type))
		       (concat " (as " content-type ")")
		     "")
		   (or comment "")
		   " ]")
	   :type 'notmuch-show-part-button-type
	   :notmuch-part nth
	   :notmuch-filename name
	   :notmuch-content-type content-type))
    (insert "\n")
    ;; return button
    button))

;; Functions handling particular MIME parts.

(defmacro notmuch-with-temp-part-buffer (message-id nth &rest body)
  (declare (indent 2))
  (let ((process-crypto (make-symbol "process-crypto")))
    `(let ((,process-crypto notmuch-show-process-crypto))
       (with-temp-buffer
	 (setq notmuch-show-process-crypto ,process-crypto)
	 ;; Always acquires the part via `notmuch part', even if it is
	 ;; available in the JSON output.
	 (insert (notmuch-get-bodypart-internal ,message-id ,nth notmuch-show-process-crypto))
	 ,@body))))

(defun notmuch-show-save-part (message-id nth &optional filename content-type)
  (notmuch-with-temp-part-buffer message-id nth
    (let ((file (read-file-name
		 "Filename to save as: "
		 (or mailcap-download-directory "~/")
		 nil nil
		 filename)))
      ;; Don't re-compress .gz & al.  Arguably we should make
      ;; `file-name-handler-alist' nil, but that would chop
      ;; ange-ftp, which is reasonable to use here.
      (mm-write-region (point-min) (point-max) file nil nil nil 'no-conversion t))))

(defun notmuch-show-view-part (message-id nth &optional filename content-type )
  (notmuch-with-temp-part-buffer message-id nth
    ;; set mm-inlined-types to nil to force an external viewer
    (let ((handle (mm-make-handle (current-buffer) (list content-type)))
	  (mm-inlined-types nil))
      ;; We override mm-save-part as notmuch-show-save-part is better
      ;; since it offers the filename. We need to lexically bind
      ;; everything we need for notmuch-show-save-part to prevent
      ;; potential dynamic shadowing.
      (lexical-let ((message-id message-id)
		    (nth nth)
		    (filename filename)
		    (content-type content-type))
	(flet ((mm-save-part (&rest args) (notmuch-show-save-part
					   message-id nth filename content-type)))
	  (mm-display-part handle))))))

(defun notmuch-show-interactively-view-part (message-id nth &optional filename content-type)
  (notmuch-with-temp-part-buffer message-id nth
    (let ((handle (mm-make-handle (current-buffer) (list content-type))))
      (mm-interactively-view-part handle))))

(defun notmuch-show-pipe-part (message-id nth &optional filename content-type)
  (notmuch-with-temp-part-buffer message-id nth
    (let ((handle (mm-make-handle (current-buffer) (list content-type))))
      (mm-pipe-part handle))))

(defun notmuch-show-multipart/*-to-list (part)
  (mapcar (lambda (inner-part) (plist-get inner-part :content-type))
	  (plist-get part :content)))

(defun notmuch-show-insert-part-multipart/alternative (msg part content-type nth depth declared-type)
  (notmuch-show-insert-part-header nth declared-type content-type nil)
  (let ((chosen-type (car (notmuch-multipart/alternative-choose (notmuch-show-multipart/*-to-list part))))
	(inner-parts (plist-get part :content))
	(start (point)))
    ;; This inserts all parts of the chosen type rather than just one,
    ;; but it's not clear that this is the wrong thing to do - which
    ;; should be chosen if there are more than one that match?
    (mapc (lambda (inner-part)
	    (let ((inner-type (plist-get inner-part :content-type)))
	      (if (or notmuch-show-all-multipart/alternative-parts
		      (string= chosen-type inner-type))
		  (notmuch-show-insert-bodypart msg inner-part depth)
		(notmuch-show-insert-part-header (plist-get inner-part :id) inner-type inner-type nil " (not shown)"))))
	  inner-parts)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-setup-w3m ()
  "Instruct w3m how to retrieve content from a \"related\" part of a message."
  (interactive)
  (if (boundp 'w3m-cid-retrieve-function-alist)
    (unless (assq 'notmuch-show-mode w3m-cid-retrieve-function-alist)
      (push (cons 'notmuch-show-mode 'notmuch-show-w3m-cid-retrieve)
	    w3m-cid-retrieve-function-alist)))
  (setq mm-inline-text-html-with-images t))

(defvar w3m-current-buffer) ;; From `w3m.el'.
(defvar notmuch-show-w3m-cid-store nil)
(make-variable-buffer-local 'notmuch-show-w3m-cid-store)

(defun notmuch-show-w3m-cid-store-internal (content-id
					    message-id
					    part-number
					    content-type
					    content)
  (push (list content-id
	      message-id
	      part-number
	      content-type
	      content)
	notmuch-show-w3m-cid-store))

(defun notmuch-show-w3m-cid-store (msg part)
  (let ((content-id (plist-get part :content-id)))
    (when content-id
      (notmuch-show-w3m-cid-store-internal (concat "cid:" content-id)
					   (plist-get msg :id)
					   (plist-get part :id)
					   (plist-get part :content-type)
					   nil))))

(defun notmuch-show-w3m-cid-retrieve (url &rest args)
  (let ((matching-part (with-current-buffer w3m-current-buffer
			 (assoc url notmuch-show-w3m-cid-store))))
    (if matching-part
	(let ((message-id (nth 1 matching-part))
	      (part-number (nth 2 matching-part))
	      (content-type (nth 3 matching-part))
	      (content (nth 4 matching-part)))
	  ;; If we don't already have the content, get it and cache
	  ;; it, as some messages reference the same cid: part many
	  ;; times (hundreds!), which results in many calls to
	  ;; `notmuch part'.
	  (unless content
	    (setq content (notmuch-get-bodypart-internal (notmuch-id-to-query message-id)
							      part-number notmuch-show-process-crypto))
	    (with-current-buffer w3m-current-buffer
	      (notmuch-show-w3m-cid-store-internal url
						   message-id
						   part-number
						   content-type
						   content)))
	  (insert content)
	  content-type)
      nil)))

(defun notmuch-show-insert-part-multipart/related (msg part content-type nth depth declared-type)
  (notmuch-show-insert-part-header nth declared-type content-type nil)
  (let ((inner-parts (plist-get part :content))
	(start (point)))

    ;; We assume that the first part is text/html and the remainder
    ;; things that it references.

    ;; Stash the non-primary parts.
    (mapc (lambda (part)
	    (notmuch-show-w3m-cid-store msg part))
	  (cdr inner-parts))

    ;; Render the primary part.
    (notmuch-show-insert-bodypart msg (car inner-parts) depth)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-multipart/signed (msg part content-type nth depth declared-type)
  (let ((button (notmuch-show-insert-part-header nth declared-type content-type nil)))
    (button-put button 'face 'notmuch-crypto-part-header)
    ;; add signature status button if sigstatus provided
    (if (plist-member part :sigstatus)
	(let* ((from (notmuch-show-get-header :From msg))
	       (sigstatus (car (plist-get part :sigstatus))))
	  (notmuch-crypto-insert-sigstatus-button sigstatus from))
      ;; if we're not adding sigstatus, tell the user how they can get it
      (button-put button 'help-echo "Set notmuch-crypto-process-mime to process cryptographic MIME parts.")))

  (let ((inner-parts (plist-get part :content))
	(start (point)))
    ;; Show all of the parts.
    (mapc (lambda (inner-part)
	    (notmuch-show-insert-bodypart msg inner-part depth))
	  inner-parts)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-multipart/encrypted (msg part content-type nth depth declared-type)
  (let ((button (notmuch-show-insert-part-header nth declared-type content-type nil)))
    (button-put button 'face 'notmuch-crypto-part-header)
    ;; add encryption status button if encstatus specified
    (if (plist-member part :encstatus)
	(let ((encstatus (car (plist-get part :encstatus))))
	  (notmuch-crypto-insert-encstatus-button encstatus)
	  ;; add signature status button if sigstatus specified
	  (if (plist-member part :sigstatus)
	      (let* ((from (notmuch-show-get-header :From msg))
		     (sigstatus (car (plist-get part :sigstatus))))
		(notmuch-crypto-insert-sigstatus-button sigstatus from))))
      ;; if we're not adding encstatus, tell the user how they can get it
      (button-put button 'help-echo "Set notmuch-crypto-process-mime to process cryptographic MIME parts.")))

  (let ((inner-parts (plist-get part :content))
	(start (point)))
    ;; Show all of the parts.
    (mapc (lambda (inner-part)
	    (notmuch-show-insert-bodypart msg inner-part depth))
	  inner-parts)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-multipart/* (msg part content-type nth depth declared-type)
  (notmuch-show-insert-part-header nth declared-type content-type nil)
  (let ((inner-parts (plist-get part :content))
	(start (point)))
    ;; Show all of the parts.
    (mapc (lambda (inner-part)
	    (notmuch-show-insert-bodypart msg inner-part depth))
	  inner-parts)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-message/rfc822 (msg part content-type nth depth declared-type)
  (notmuch-show-insert-part-header nth declared-type content-type nil)
  (let* ((message (car (plist-get part :content)))
	 (body (car (plist-get message :body)))
	 (start (point)))

    ;; Override `notmuch-message-headers' to force `From' to be
    ;; displayed.
    (let ((notmuch-message-headers '("From" "Subject" "To" "Cc" "Date")))
      (notmuch-show-insert-headers (plist-get message :headers)))

    ;; Blank line after headers to be compatible with the normal
    ;; message display.
    (insert "\n")

    ;; Show the body
    (notmuch-show-insert-bodypart msg body depth)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-text/plain (msg part content-type nth depth declared-type)
  (let ((start (point)))
    ;; If this text/plain part is not the first part in the message,
    ;; insert a header to make this clear.
    (if (> nth 1)
	(notmuch-show-insert-part-header nth declared-type content-type (plist-get part :filename)))
    (insert (notmuch-get-bodypart-content msg part nth notmuch-show-process-crypto))
    (save-excursion
      (save-restriction
	(narrow-to-region start (point-max))
	(run-hook-with-args 'notmuch-show-insert-text/plain-hook msg depth))))
  t)

(defun notmuch-show-insert-part-text/calendar (msg part content-type nth depth declared-type)
  (notmuch-show-insert-part-header nth declared-type content-type (plist-get part :filename))
  (insert (with-temp-buffer
	    (insert (notmuch-get-bodypart-content msg part nth notmuch-show-process-crypto))
	    (goto-char (point-min))
	    (let ((file (make-temp-file "notmuch-ical"))
		  result)
	      (icalendar--convert-ical-to-diary
	       (icalendar--read-element nil nil)
	       file t)
	      (set-buffer (get-file-buffer file))
	      (setq result (buffer-substring (point-min) (point-max)))
	      (set-buffer-modified-p nil)
	      (kill-buffer (current-buffer))
	      (delete-file file)
	      result)))
  t)

;; For backwards compatibility.
(defun notmuch-show-insert-part-text/x-vcalendar (msg part content-type nth depth declared-type)
  (notmuch-show-insert-part-text/calendar msg part content-type nth depth declared-type))

(defun notmuch-show-insert-part-application/octet-stream (msg part content-type nth depth declared-type)
  ;; If we can deduce a MIME type from the filename of the attachment,
  ;; do so and pass it on to the handler for that type.
  (if (plist-get part :filename)
      (let ((extension (file-name-extension (plist-get part :filename)))
	    mime-type)
	(if extension
	    (progn
	      (mailcap-parse-mimetypes)
	      (setq mime-type (mailcap-extension-to-mime extension))
	      (if (and mime-type
		       (not (string-equal mime-type "application/octet-stream")))
		  (notmuch-show-insert-bodypart-internal msg part mime-type nth depth content-type)
		nil))
	  nil))))

;; Handler for wash generated inline patch fake parts.
(defun notmuch-show-insert-part-inline-patch-fake-part (msg part content-type nth depth declared-type)
  (notmuch-show-insert-part-*/* msg part "text/x-diff" nth depth "inline patch"))

(defun notmuch-show-insert-part-*/* (msg part content-type nth depth declared-type)
  ;; This handler _must_ succeed - it is the handler of last resort.
  (notmuch-show-insert-part-header nth content-type declared-type (plist-get part :filename))
  (notmuch-mm-display-part-inline msg part nth content-type notmuch-show-process-crypto)
  t)

;; Functions for determining how to handle MIME parts.

(defun notmuch-show-handlers-for (content-type)
  "Return a list of content handlers for a part of type CONTENT-TYPE."
  (let (result)
    (mapc (lambda (func)
	    (if (functionp func)
		(push func result)))
	  ;; Reverse order of prefrence.
	  (list (intern (concat "notmuch-show-insert-part-*/*"))
		(intern (concat
			 "notmuch-show-insert-part-"
			 (car (notmuch-split-content-type content-type))
			 "/*"))
		(intern (concat "notmuch-show-insert-part-" content-type))))
    result))

;; 

(defun notmuch-show-insert-bodypart-internal (msg part content-type nth depth declared-type)
  (let ((handlers (notmuch-show-handlers-for content-type)))
    ;; Run the content handlers until one of them returns a non-nil
    ;; value.
    (while (and handlers
		(not (funcall (car handlers) msg part content-type nth depth declared-type)))
      (setq handlers (cdr handlers))))
  t)

(defun notmuch-show-insert-bodypart (msg part depth)
  "Insert the body part PART at depth DEPTH in the current thread."
  (let ((content-type (downcase (plist-get part :content-type)))
	(nth (plist-get part :id)))
    (notmuch-show-insert-bodypart-internal msg part content-type nth depth content-type))
  ;; Some of the body part handlers leave point somewhere up in the
  ;; part, so we make sure that we're down at the end.
  (goto-char (point-max))
  ;; Ensure that the part ends with a carriage return.
  (unless (bolp)
    (insert "\n")))

(defun notmuch-show-insert-body (msg body depth)
  "Insert the body BODY at depth DEPTH in the current thread."
  (mapc (lambda (part) (notmuch-show-insert-bodypart msg part depth)) body))

(defun notmuch-show-make-symbol (type)
  (make-symbol (concat "notmuch-show-" type)))

(defun notmuch-show-strip-re (string)
  (replace-regexp-in-string "^\\([Rr]e: *\\)+" "" string))

(defvar notmuch-show-previous-subject "")
(make-variable-buffer-local 'notmuch-show-previous-subject)

(defun notmuch-show-insert-msg (msg depth)
  "Insert the message MSG at depth DEPTH in the current thread."
  (let* ((headers (plist-get msg :headers))
	 ;; Indentation causes the buffer offset of the start/end
	 ;; points to move, so we must use markers.
	 message-start message-end
	 content-start content-end
	 headers-start headers-end
	 body-start body-end
	 (headers-invis-spec (notmuch-show-make-symbol "header"))
	 (message-invis-spec (notmuch-show-make-symbol "message"))
	 (bare-subject (notmuch-show-strip-re (plist-get headers :Subject))))

    ;; Set `buffer-invisibility-spec' to `nil' (a list), otherwise
    ;; removing items from `buffer-invisibility-spec' (which is what
    ;; `notmuch-show-headers-visible' and
    ;; `notmuch-show-message-visible' do) is a no-op and has no
    ;; effect. This caused threads with only matching messages to have
    ;; those messages hidden initially because
    ;; `buffer-invisibility-spec' stayed `t'.
    ;;
    ;; This needs to be set here (rather than just above the call to
    ;; `notmuch-show-headers-visible') because some of the part
    ;; rendering or body washing functions
    ;; (e.g. `notmuch-wash-text/plain-citations') manipulate
    ;; `buffer-invisibility-spec').
    (when (eq buffer-invisibility-spec t)
      (setq buffer-invisibility-spec nil))

    (setq message-start (point-marker))

    (notmuch-show-insert-headerline headers
				    (or (if notmuch-show-relative-dates
					    (plist-get msg :date_relative)
					  nil)
					(plist-get headers :Date))
				    (plist-get msg :tags) depth)

    (setq content-start (point-marker))

    (plist-put msg :headers-invis-spec headers-invis-spec)
    (plist-put msg :message-invis-spec message-invis-spec)

    ;; Set `headers-start' to point after the 'Subject:' header to be
    ;; compatible with the existing implementation. This just sets it
    ;; to after the first header.
    (notmuch-show-insert-headers headers)
    (save-excursion
      (goto-char content-start)
      ;; If the subject of this message is the same as that of the
      ;; previous message, don't display it when this message is
      ;; collapsed.
      (when (not (string= notmuch-show-previous-subject
			  bare-subject))
	(forward-line 1))
      (setq headers-start (point-marker)))
    (setq headers-end (point-marker))

    (setq notmuch-show-previous-subject bare-subject)

    (setq body-start (point-marker))
    ;; A blank line between the headers and the body.
    (insert "\n")
    (notmuch-show-insert-body msg (plist-get msg :body)
			      (if notmuch-show-indent-content depth 0))
    ;; Ensure that the body ends with a newline.
    (unless (bolp)
      (insert "\n"))
    (setq body-end (point-marker))
    (setq content-end (point-marker))

    ;; Indent according to the depth in the thread.
    (if notmuch-show-indent-content
	(indent-rigidly content-start content-end (* notmuch-show-indent-messages-width depth)))

    (setq message-end (point-max-marker))

    ;; Save the extents of this message over the whole text of the
    ;; message.
    (put-text-property message-start message-end :notmuch-message-extent (cons message-start message-end))

    (let ((headers-overlay (make-overlay headers-start headers-end))
          (invis-specs (list headers-invis-spec message-invis-spec)))
      (overlay-put headers-overlay 'invisible invis-specs)
      (overlay-put headers-overlay 'priority 10))
    (overlay-put (make-overlay body-start body-end) 'invisible message-invis-spec)

    (plist-put msg :depth depth)

    ;; Save the properties for this message. Currently this saves the
    ;; entire message (augmented it with other stuff), which seems
    ;; like overkill. We might save a reduced subset (for example, not
    ;; the content).
    (notmuch-show-set-message-properties msg)

    ;; Set header visibility.
    (notmuch-show-headers-visible msg notmuch-message-headers-visible)

    ;; Message visibility depends on whether it matched the search
    ;; criteria.
    (notmuch-show-message-visible msg (and (plist-get msg :match)
					   (not (plist-get msg :excluded))))))

(defun notmuch-show-toggle-process-crypto ()
  "Toggle the processing of cryptographic MIME parts."
  (interactive)
  (setq notmuch-show-process-crypto (not notmuch-show-process-crypto))
  (message (if notmuch-show-process-crypto
	       "Processing cryptographic MIME parts."
	     "Not processing cryptographic MIME parts."))
  (notmuch-show-refresh-view))

(defun notmuch-show-toggle-elide-non-matching ()
  "Toggle the display of non-matching messages."
  (interactive)
  (setq notmuch-show-elide-non-matching-messages (not notmuch-show-elide-non-matching-messages))
  (message (if notmuch-show-elide-non-matching-messages
	       "Showing matching messages only."
	     "Showing all messages."))
  (notmuch-show-refresh-view))

(defun notmuch-show-toggle-thread-indentation ()
  "Toggle the indentation of threads."
  (interactive)
  (setq notmuch-show-indent-content (not notmuch-show-indent-content))
  (message (if notmuch-show-indent-content
	       "Content is indented."
	     "Content is not indented."))
  (notmuch-show-refresh-view))

(defun notmuch-show-insert-tree (tree depth)
  "Insert the message tree TREE at depth DEPTH in the current thread."
  (let ((msg (car tree))
	(replies (cadr tree)))
    ;; We test whether there is a message or just some replies.
    (when msg
      (notmuch-show-insert-msg msg depth))
    (notmuch-show-insert-thread replies (1+ depth))))

(defun notmuch-show-insert-thread (thread depth)
  "Insert the thread THREAD at depth DEPTH in the current forest."
  (mapc (lambda (tree) (notmuch-show-insert-tree tree depth)) thread))

(defun notmuch-show-insert-forest (forest)
  "Insert the forest of threads FOREST."
  (mapc (lambda (thread) (notmuch-show-insert-thread thread 0)) forest))

(defun notmuch-show-buttonise-links (start end)
  "Buttonise URLs and mail addresses between START and END.

This also turns id:\"<message id>\"-parts into buttons for
a corresponding notmuch search."
  (goto-address-fontify-region start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward "id:\\(\"?\\)[^[:space:]\"]+\\1" end t)
      ;; remove the overlay created by goto-address-mode
      (remove-overlays (match-beginning 0) (match-end 0) 'goto-address t)
      (make-text-button (match-beginning 0) (match-end 0)
			'action `(lambda (arg)
				   (notmuch-show ,(match-string-no-properties 0)))
			'follow-link t
			'help-echo "Mouse-1, RET: search for this message"
			'face goto-address-mail-face))))

;;;###autoload
(defun notmuch-show (thread-id &optional parent-buffer query-context buffer-name)
  "Run \"notmuch show\" with the given thread ID and display results.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).

The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.

The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used."
  (interactive "sNotmuch show: ")
  (let ((buffer-name (generate-new-buffer-name
		      (or buffer-name
			  (concat "*notmuch-" thread-id "*")))))
    (switch-to-buffer (get-buffer-create buffer-name))
    ;; Set the default value for `notmuch-show-process-crypto' in this
    ;; buffer.
    (setq notmuch-show-process-crypto notmuch-crypto-process-mime)
    ;; Set the default value for
    ;; `notmuch-show-elide-non-matching-messages' in this buffer. If
    ;; there is a prefix argument, invert the default.
    (setq notmuch-show-elide-non-matching-messages notmuch-show-only-matching-messages)
    (if current-prefix-arg
	(setq notmuch-show-elide-non-matching-messages (not notmuch-show-elide-non-matching-messages)))

    (setq notmuch-show-thread-id thread-id
	  notmuch-show-parent-buffer parent-buffer
	  notmuch-show-query-context query-context)
    (notmuch-show-build-buffer)
    (notmuch-show-goto-first-wanted-message)
    (current-buffer)))

(defun notmuch-show-build-buffer ()
  (let ((inhibit-read-only t))

    (notmuch-show-mode)
    ;; Don't track undo information for this buffer
    (set 'buffer-undo-list t)

    (erase-buffer)
    (goto-char (point-min))
    (save-excursion
      (let* ((basic-args (list notmuch-show-thread-id))
	     (args (if notmuch-show-query-context
		       (append (list "\'") basic-args
			       (list "and (" notmuch-show-query-context ")\'"))
		     (append (list "\'") basic-args (list "\'"))))
	     (cli-args (cons "--exclude=false"
			     (when notmuch-show-elide-non-matching-messages
			       (list "--entire-thread=false")))))

	(notmuch-show-insert-forest (notmuch-query-get-threads (append cli-args args)))
	;; If the query context reduced the results to nothing, run
	;; the basic query.
	(when (and (eq (buffer-size) 0)
		   notmuch-show-query-context)
	  (notmuch-show-insert-forest
	   (notmuch-query-get-threads (append cli-args basic-args)))))

      (jit-lock-register #'notmuch-show-buttonise-links)

      ;; Set the header line to the subject of the first message.
      (setq header-line-format (notmuch-show-strip-re (notmuch-show-get-subject)))

      (run-hooks 'notmuch-show-hook))))

(defun notmuch-show-capture-state ()
  "Capture the state of the current buffer.

This includes:
 - the list of open messages,
 - the current message."
  (list (notmuch-show-get-message-id) (notmuch-show-get-message-ids-for-open-messages)))

(defun notmuch-show-apply-state (state)
  "Apply STATE to the current buffer.

This includes:
 - opening the messages previously opened,
 - closing all other messages,
 - moving to the correct current message."
  (let ((current (car state))
	(open (cadr state)))

    ;; Open those that were open.
    (goto-char (point-min))
    (loop do (notmuch-show-message-visible (notmuch-show-get-message-properties)
					   (member (notmuch-show-get-message-id) open))
	  until (not (notmuch-show-goto-message-next)))

    ;; Go to the previously open message.
    (goto-char (point-min))
    (unless (loop if (string= current (notmuch-show-get-message-id))
		  return t
		  until (not (notmuch-show-goto-message-next)))
      (goto-char (point-min))
      (message "Previously current message not found."))
    (notmuch-show-message-adjust)))

(defun notmuch-show-refresh-view (&optional reset-state)
  "Refresh the current view.

Refreshes the current view, observing changes in display
preferences. If invoked with a prefix argument (or RESET-STATE is
non-nil) then the state of the buffer (open/closed messages) is
reset based on the original query."
  (interactive "P")
  (let ((inhibit-read-only t)
	(state (unless reset-state
		 (notmuch-show-capture-state))))
    (erase-buffer)
    (notmuch-show-build-buffer)
    (if state
	(notmuch-show-apply-state state)
      ;; We're resetting state, so navigate to the first open message
      ;; and mark it read, just like opening a new show buffer.
      (notmuch-show-goto-first-wanted-message))))

(defvar notmuch-show-stash-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'notmuch-show-stash-cc)
    (define-key map "d" 'notmuch-show-stash-date)
    (define-key map "F" 'notmuch-show-stash-filename)
    (define-key map "f" 'notmuch-show-stash-from)
    (define-key map "i" 'notmuch-show-stash-message-id)
    (define-key map "I" 'notmuch-show-stash-message-id-stripped)
    (define-key map "s" 'notmuch-show-stash-subject)
    (define-key map "T" 'notmuch-show-stash-tags)
    (define-key map "t" 'notmuch-show-stash-to)
    (define-key map "l" 'notmuch-show-stash-mlarchive-link)
    (define-key map "L" 'notmuch-show-stash-mlarchive-link-and-go)
    map)
  "Submap for stash commands")
(fset 'notmuch-show-stash-map notmuch-show-stash-map)

(defvar notmuch-show-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map "?" 'notmuch-help)
	(define-key map "q" 'notmuch-kill-this-buffer)
	(define-key map (kbd "<C-tab>") 'widget-backward)
	(define-key map (kbd "M-TAB") 'notmuch-show-previous-button)
	(define-key map (kbd "<backtab>") 'notmuch-show-previous-button)
	(define-key map (kbd "TAB") 'notmuch-show-next-button)
	(define-key map "s" 'notmuch-search)
	(define-key map "m" 'notmuch-mua-new-mail)
	(define-key map "f" 'notmuch-show-forward-message)
	(define-key map "r" 'notmuch-show-reply-sender)
	(define-key map "R" 'notmuch-show-reply)
	(define-key map "|" 'notmuch-show-pipe-message)
	(define-key map "w" 'notmuch-show-save-attachments)
	(define-key map "V" 'notmuch-show-view-raw-message)
	(define-key map "v" 'notmuch-show-view-all-mime-parts)
	(define-key map "c" 'notmuch-show-stash-map)
	(define-key map "=" 'notmuch-show-refresh-view)
	(define-key map "h" 'notmuch-show-toggle-visibility-headers)
	(define-key map "*" 'notmuch-show-tag-all)
	(define-key map "-" 'notmuch-show-remove-tag)
	(define-key map "+" 'notmuch-show-add-tag)
	(define-key map "X" 'notmuch-show-archive-thread-then-exit)
	(define-key map "x" 'notmuch-show-archive-message-then-next-or-exit)
	(define-key map "A" 'notmuch-show-archive-thread-then-next)
	(define-key map "a" 'notmuch-show-archive-message-then-next-or-next-thread)
	(define-key map "N" 'notmuch-show-next-message)
	(define-key map "P" 'notmuch-show-previous-message)
	(define-key map "n" 'notmuch-show-next-open-message)
	(define-key map "p" 'notmuch-show-previous-open-message)
	(define-key map (kbd "DEL") 'notmuch-show-rewind)
	(define-key map " " 'notmuch-show-advance-and-archive)
	(define-key map (kbd "M-RET") 'notmuch-show-open-or-close-all)
	(define-key map (kbd "RET") 'notmuch-show-toggle-message)
	(define-key map "#" 'notmuch-show-print-message)
	(define-key map "!" 'notmuch-show-toggle-elide-non-matching)
	(define-key map "$" 'notmuch-show-toggle-process-crypto)
	(define-key map "<" 'notmuch-show-toggle-thread-indentation)
	(define-key map "t" 'toggle-truncate-lines)
	map)
      "Keymap for \"notmuch show\" buffers.")
(fset 'notmuch-show-mode-map notmuch-show-mode-map)

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
\\[notmuch-show-advance-and-archive]. This will scroll the current message (if necessary), advance
to the next message, or advance to the next thread (if already on
the last message of a thread).

Other commands are available to read or manipulate the thread
more selectively, (such as '\\[notmuch-show-next-message]' and '\\[notmuch-show-previous-message]' to advance to messages
without removing any tags, and '\\[notmuch-show-archive-thread]' to archive an entire thread
without scrolling through with \\[notmuch-show-advance-and-archive]).

You can add or remove arbitrary tags from the current message with
'\\[notmuch-show-add-tag]' or '\\[notmuch-show-remove-tag]'.

All currently available key bindings:

\\{notmuch-show-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map notmuch-show-mode-map)
  (setq major-mode 'notmuch-show-mode
	mode-name "notmuch-show")
  (setq buffer-read-only t
	truncate-lines t))

(defun notmuch-show-move-to-message-top ()
  (goto-char (notmuch-show-message-top)))

(defun notmuch-show-move-to-message-bottom ()
  (goto-char (notmuch-show-message-bottom)))

(defun notmuch-show-message-adjust ()
  (recenter 0))

;; Movement related functions.

;; There's some strangeness here where a text property applied to a
;; region a->b is not found when point is at b. We walk backwards
;; until finding the property.
(defun notmuch-show-message-extent ()
  (let (r)
    (save-excursion
      (while (not (setq r (get-text-property (point) :notmuch-message-extent)))
	(backward-char)))
    r))

(defun notmuch-show-message-top ()
  (car (notmuch-show-message-extent)))

(defun notmuch-show-message-bottom ()
  (cdr (notmuch-show-message-extent)))

(defun notmuch-show-goto-message-next ()
  (let ((start (point)))
    (notmuch-show-move-to-message-bottom)
    (if (not (eobp))
	t
      (goto-char start)
      nil)))

(defun notmuch-show-goto-message-previous ()
  (notmuch-show-move-to-message-top)
  (if (bobp)
      nil
    (backward-char)
    (notmuch-show-move-to-message-top)
    t))

(defun notmuch-show-mapc (function)
  "Iterate through all messages in the current thread with
`notmuch-show-goto-message-next' and call FUNCTION for side
effects."
  (save-excursion
    (goto-char (point-min))
    (loop do (funcall function)
	  while (notmuch-show-goto-message-next))))

;; Functions relating to the visibility of messages and their
;; components.

(defun notmuch-show-element-visible (props visible-p spec-property)
  (let ((spec (plist-get props spec-property)))
    (if visible-p
	(remove-from-invisibility-spec spec)
      (add-to-invisibility-spec spec))))

(defun notmuch-show-message-visible (props visible-p)
  (notmuch-show-element-visible props visible-p :message-invis-spec)
  (notmuch-show-set-prop :message-visible visible-p props))

(defun notmuch-show-headers-visible (props visible-p)
  (notmuch-show-element-visible props visible-p :headers-invis-spec)
  (notmuch-show-set-prop :headers-visible visible-p props))

;; Functions for setting and getting attributes of the current
;; message.

(defun notmuch-show-set-message-properties (props)
  (save-excursion
    (notmuch-show-move-to-message-top)
    (put-text-property (point) (+ (point) 1) :notmuch-message-properties props)))

(defun notmuch-show-get-message-properties ()
  "Return the properties of the current message as a plist.

Some useful entries are:
:headers - Property list containing the headers :Date, :Subject, :From, etc.
:body - Body of the message
:tags - Tags for this message"
  (save-excursion
    (notmuch-show-move-to-message-top)
    (get-text-property (point) :notmuch-message-properties)))

(defun notmuch-show-set-prop (prop val &optional props)
  (let ((inhibit-read-only t)
	(props (or props
		   (notmuch-show-get-message-properties))))
    (plist-put props prop val)
    (notmuch-show-set-message-properties props)))

(defun notmuch-show-get-prop (prop &optional props)
  (let ((props (or props
		   (notmuch-show-get-message-properties))))
    (plist-get props prop)))

(defun notmuch-show-get-message-id (&optional bare)
  "Return an id: query for the Message-Id of the current message.

If optional argument BARE is non-nil, return
the Message-Id without id: prefix and escaping."
  (if bare
      (notmuch-show-get-prop :id)
    (notmuch-id-to-query (notmuch-show-get-prop :id))))

(defun notmuch-show-get-messages-ids ()
  "Return all id: queries of messages in the current thread."
  (let ((message-ids))
    (notmuch-show-mapc
     (lambda () (push (notmuch-show-get-message-id) message-ids)))
    message-ids))

(defun notmuch-show-get-messages-ids-search ()
  "Return a search string for all message ids of messages in the
current thread."
  (mapconcat 'identity (notmuch-show-get-messages-ids) " or "))

;; dme: Would it make sense to use a macro for many of these?

(defun notmuch-show-get-filename ()
  "Return the filename of the current message."
  (notmuch-show-get-prop :filename))

(defun notmuch-show-get-header (header &optional props)
  "Return the named header of the current message, if any."
  (plist-get (notmuch-show-get-prop :headers props) header))

(defun notmuch-show-get-cc ()
  (notmuch-show-get-header :Cc))

(defun notmuch-show-get-date ()
  (notmuch-show-get-header :Date))

(defun notmuch-show-get-from ()
  (notmuch-show-get-header :From))

(defun notmuch-show-get-subject ()
  (notmuch-show-get-header :Subject))

(defun notmuch-show-get-to ()
  (notmuch-show-get-header :To))

(defun notmuch-show-get-depth ()
  (notmuch-show-get-prop :depth))

(defun notmuch-show-set-tags (tags)
  "Set the tags of the current message."
  (notmuch-show-set-prop :tags tags)
  (notmuch-show-update-tags tags))

(defun notmuch-show-get-tags ()
  "Return the tags of the current message."
  (notmuch-show-get-prop :tags))

(defun notmuch-show-message-visible-p ()
  "Is the current message visible?"
  (notmuch-show-get-prop :message-visible))

(defun notmuch-show-headers-visible-p ()
  "Are the headers of the current message visible?"
  (notmuch-show-get-prop :headers-visible))

(defun notmuch-show-mark-read (&optional unread)
  "Mark the current message as read.

Mark the current message as read by applying the tag changes in
`notmuch-show-mark-read-tags' to it (remove the \"unread\" tag by
default). If a prefix argument is given, the message will be
marked as unread, i.e. the tag changes in
`notmuch-show-mark-read-tags' will be reversed."
  (interactive "P")
  (when notmuch-show-mark-read-tags
    (apply 'notmuch-show-tag-message
	   (notmuch-tag-change-list notmuch-show-mark-read-tags unread))))

;; Functions for getting attributes of several messages in the current
;; thread.

(defun notmuch-show-get-message-ids-for-open-messages ()
  "Return a list of all id: queries for open messages in the current thread."
  (save-excursion
    (let (message-ids done)
      (goto-char (point-min))
      (while (not done)
	(if (notmuch-show-message-visible-p)
	    (setq message-ids (append message-ids (list (notmuch-show-get-message-id)))))
	(setq done (not (notmuch-show-goto-message-next)))
	)
      message-ids
      )))

;; Commands typically bound to keys.

(defun notmuch-show-advance ()
  "Advance through thread.

If the current message in the thread is not yet fully visible,
scroll by a near screenful to read more of the message.

Otherwise, (the end of the current message is already within the
current window), advance to the next open message."
  (interactive)
  (let* ((end-of-this-message (notmuch-show-message-bottom))
	 (visible-end-of-this-message (1- end-of-this-message))
	 (ret nil))
    (while (invisible-p visible-end-of-this-message)
      (setq visible-end-of-this-message
	    (max (point-min)
		 (1- (previous-single-char-property-change
		      visible-end-of-this-message 'invisible)))))
    (cond
     ;; Ideally we would test `end-of-this-message' against the result
     ;; of `window-end', but that doesn't account for the fact that
     ;; the end of the message might be hidden.
     ((and visible-end-of-this-message
	   (> visible-end-of-this-message (window-end)))
      ;; The bottom of this message is not visible - scroll.
      (scroll-up nil))

     ((not (= end-of-this-message (point-max)))
      ;; This is not the last message - move to the next visible one.
      (notmuch-show-next-open-message))

     ((not (= (point) (point-max)))
      ;; This is the last message, but the cursor is not at the end of
      ;; the buffer. Move it there.
      (goto-char (point-max)))

     (t
      ;; This is the last message - change the return value
      (setq ret t)))
    ret))

(defun notmuch-show-advance-and-archive ()
  "Advance through thread and archive.

This command is intended to be one of the simplest ways to
process a thread of email. It works exactly like
notmuch-show-advance, in that it scrolls through messages in a
show buffer, except that when it gets to the end of the buffer it
archives the entire current thread, (remove the \"inbox\" tag
from each message), kills the buffer, and displays the next
thread from the search from which this thread was originally
shown."
  (interactive)
  (if (notmuch-show-advance)
      (notmuch-show-archive-thread-then-next)))

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
  (let ((start-of-message (notmuch-show-message-top))
	(start-of-window (window-start)))
    (cond
      ;; Either this message is properly aligned with the start of the
      ;; window or the start of this message is not visible on the
      ;; screen - scroll.
     ((or (= start-of-message start-of-window)
	  (< start-of-message start-of-window))
      (scroll-down)
      ;; If a small number of lines from the previous message are
      ;; visible, realign so that the top of the current message is at
      ;; the top of the screen.
      (when (<= (count-screen-lines (window-start) start-of-message)
		next-screen-context-lines)
	(goto-char (notmuch-show-message-top))
	(notmuch-show-message-adjust))
      ;; Move to the top left of the window.
      (goto-char (window-start)))
     (t
      ;; Move to the previous message.
      (notmuch-show-previous-message)))))

(defun notmuch-show-reply (&optional prompt-for-sender)
  "Reply to the sender and all recipients of the current message."
  (interactive "P")
  (notmuch-mua-new-reply (notmuch-show-get-message-id) prompt-for-sender t))

(defun notmuch-show-reply-sender (&optional prompt-for-sender)
  "Reply to the sender of the current message."
  (interactive "P")
  (notmuch-mua-new-reply (notmuch-show-get-message-id) prompt-for-sender nil))

(defun notmuch-show-forward-message (&optional prompt-for-sender)
  "Forward the current message."
  (interactive "P")
  (with-current-notmuch-show-message
   (notmuch-mua-new-forward-message prompt-for-sender)))

(defun notmuch-show-next-message (&optional pop-at-end)
  "Show the next message.

If a prefix argument is given and this is the last message in the
thread, navigate to the next thread in the parent search buffer."
  (interactive "P")
  (if (notmuch-show-goto-message-next)
      (progn
	(notmuch-show-mark-read)
	(notmuch-show-message-adjust))
    (if pop-at-end
	(notmuch-show-next-thread)
      (goto-char (point-max)))))

(defun notmuch-show-previous-message ()
  "Show the previous message or the start of the current message."
  (interactive)
  (if (= (point) (notmuch-show-message-top))
      (notmuch-show-goto-message-previous)
    (notmuch-show-move-to-message-top))
  (notmuch-show-mark-read)
  (notmuch-show-message-adjust))

(defun notmuch-show-next-open-message (&optional pop-at-end)
  "Show the next open message.

If a prefix argument is given and this is the last open message
in the thread, navigate to the next thread in the parent search
buffer. Return t if there was a next open message in the thread
to show, nil otherwise."
  (interactive "P")
  (let (r)
    (while (and (setq r (notmuch-show-goto-message-next))
		(not (notmuch-show-message-visible-p))))
    (if r
	(progn
	  (notmuch-show-mark-read)
	  (notmuch-show-message-adjust))
      (if pop-at-end
	  (notmuch-show-next-thread)
	(goto-char (point-max))))
    r))

(defun notmuch-show-next-matching-message ()
  "Show the next matching message."
  (interactive)
  (let (r)
    (while (and (setq r (notmuch-show-goto-message-next))
		(not (notmuch-show-get-prop :match))))
    (if r
	(progn
	  (notmuch-show-mark-read)
	  (notmuch-show-message-adjust))
      (goto-char (point-max)))))

(defun notmuch-show-open-if-matched ()
  "Open a message if it is matched (whether or not excluded)."
  (let ((props (notmuch-show-get-message-properties)))
    (notmuch-show-message-visible props (plist-get props :match))))

(defun notmuch-show-goto-first-wanted-message ()
  "Move to the first open message and mark it read"
  (goto-char (point-min))
  (if (notmuch-show-message-visible-p)
      (notmuch-show-mark-read)
    (notmuch-show-next-open-message))
  (when (eobp)
    ;; There are no matched non-excluded messages so open all matched
    ;; (necessarily excluded) messages and go to the first.
    (notmuch-show-mapc 'notmuch-show-open-if-matched)
    (force-window-update)
    (goto-char (point-min))
    (if (notmuch-show-message-visible-p)
	(notmuch-show-mark-read)
      (notmuch-show-next-open-message))))

(defun notmuch-show-previous-open-message ()
  "Show the previous open message."
  (interactive)
  (while (and (if (= (point) (notmuch-show-message-top))
		  (notmuch-show-goto-message-previous)
		(notmuch-show-move-to-message-top))
	      (not (notmuch-show-message-visible-p))))
  (notmuch-show-mark-read)
  (notmuch-show-message-adjust))

(defun notmuch-show-view-raw-message ()
  "View the file holding the current message."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
	 (buf (get-buffer-create (concat "*notmuch-raw-" id "*"))))
    (call-process notmuch-command nil buf nil "show" "--format=raw" id)
    (switch-to-buffer buf)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (view-buffer buf 'kill-buffer-if-not-modified)))

(defun notmuch-show-pipe-message (entire-thread command)
  "Pipe the contents of the current message (or thread) to the given command.

The given command will be executed with the raw contents of the
current email message as stdin. Anything printed by the command
to stdout or stderr will appear in the *notmuch-pipe* buffer.

When invoked with a prefix argument, the command will receive all
open messages in the current thread (formatted as an mbox) rather
than only the current message."
  (interactive "P\nsPipe message to command: ")
  (let (shell-command)
    (if entire-thread
	(setq shell-command
	      (concat notmuch-command " show --format=mbox --exclude=false "
		      (shell-quote-argument
		       (mapconcat 'identity (notmuch-show-get-message-ids-for-open-messages) " OR "))
		      " | " command))
      (setq shell-command
	    (concat notmuch-command " show --format=raw "
		    (shell-quote-argument (notmuch-show-get-message-id)) " | " command)))
    (let ((buf (get-buffer-create (concat "*notmuch-pipe*"))))
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
			     shell-command exit-code))))))))

(defun notmuch-show-tag-message (&rest tag-changes)
  "Change tags for the current message.

TAG-CHANGES is a list of tag operations for `notmuch-tag'."
  (let* ((current-tags (notmuch-show-get-tags))
	 (new-tags (notmuch-update-tags current-tags tag-changes)))
    (unless (equal current-tags new-tags)
      (funcall 'notmuch-tag (notmuch-show-get-message-id) tag-changes)
      (notmuch-show-set-tags new-tags))))

(defun notmuch-show-tag (&optional tag-changes)
  "Change tags for the current message.

See `notmuch-tag' for information on the format of TAG-CHANGES."
  (interactive)
  (setq tag-changes (funcall 'notmuch-tag (notmuch-show-get-message-id) tag-changes))
  (let* ((current-tags (notmuch-show-get-tags))
	 (new-tags (notmuch-update-tags current-tags tag-changes)))
    (unless (equal current-tags new-tags)
      (notmuch-show-set-tags new-tags))))

(defun notmuch-show-tag-all (&optional tag-changes)
  "Change tags for all messages in the current show buffer.

See `notmuch-tag' for information on the format of TAG-CHANGES."
  (interactive)
  (setq tag-changes (funcall 'notmuch-tag (notmuch-show-get-messages-ids-search) tag-changes))
  (notmuch-show-mapc
   (lambda ()
     (let* ((current-tags (notmuch-show-get-tags))
	    (new-tags (notmuch-update-tags current-tags tag-changes)))
       (unless (equal current-tags new-tags)
	 (notmuch-show-set-tags new-tags))))))

(defun notmuch-show-add-tag ()
  "Same as `notmuch-show-tag' but sets initial input to '+'."
  (interactive)
  (notmuch-show-tag "+"))

(defun notmuch-show-remove-tag ()
  "Same as `notmuch-show-tag' but sets initial input to '-'."
  (interactive)
  (notmuch-show-tag "-"))

(defun notmuch-show-toggle-visibility-headers ()
  "Toggle the visibility of the current message headers."
  (interactive)
  (let ((props (notmuch-show-get-message-properties)))
    (notmuch-show-headers-visible
     props
     (not (plist-get props :headers-visible))))
  (force-window-update))

(defun notmuch-show-toggle-message ()
  "Toggle the visibility of the current message."
  (interactive)
  (let ((props (notmuch-show-get-message-properties)))
    (notmuch-show-message-visible
     props
     (not (plist-get props :message-visible))))
  (force-window-update))

(defun notmuch-show-open-or-close-all ()
  "Set the visibility all of the messages in the current thread.
By default make all of the messages visible. With a prefix
argument, hide all of the messages."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (loop do (notmuch-show-message-visible (notmuch-show-get-message-properties)
					   (not current-prefix-arg))
	  until (not (notmuch-show-goto-message-next))))
  (force-window-update))

(defun notmuch-show-next-button ()
  "Advance point to the next button in the buffer."
  (interactive)
  (forward-button 1))

(defun notmuch-show-previous-button ()
  "Move point back to the previous button in the buffer."
  (interactive)
  (backward-button 1))

(defun notmuch-show-next-thread (&optional show-next)
  "Move to the next item in the search results, if any."
  (interactive "P")
  (let ((parent-buffer notmuch-show-parent-buffer))
    (notmuch-kill-this-buffer)
    (when (buffer-live-p parent-buffer)
      (switch-to-buffer parent-buffer)
      (notmuch-search-next-thread)
      (if show-next
	  (notmuch-search-show-thread)))))

(defun notmuch-show-archive-thread (&optional unarchive)
  "Archive each message in thread.

Archive each message currently shown by applying the tag changes
in `notmuch-archive-tags' to each (remove the \"inbox\" tag by
default). If a prefix argument is given, the messages will be
\"unarchived\", i.e. the tag changes in `notmuch-archive-tags'
will be reversed.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not archive the
entire thread, but only the messages shown in the current
buffer."
  (interactive "P")
  (when notmuch-archive-tags
    (notmuch-show-tag-all
     (notmuch-tag-change-list notmuch-archive-tags unarchive))))

(defun notmuch-show-archive-thread-then-next ()
  "Archive all messages in the current buffer, then show next thread from search."
  (interactive)
  (notmuch-show-archive-thread)
  (notmuch-show-next-thread t))

(defun notmuch-show-archive-thread-then-exit ()
  "Archive all messages in the current buffer, then exit back to search results."
  (interactive)
  (notmuch-show-archive-thread)
  (notmuch-show-next-thread))

(defun notmuch-show-archive-message (&optional unarchive)
  "Archive the current message.

Archive the current message by applying the tag changes in
`notmuch-archive-tags' to it (remove the \"inbox\" tag by
default). If a prefix argument is given, the message will be
\"unarchived\", i.e. the tag changes in `notmuch-archive-tags'
will be reversed."
  (interactive "P")
  (when notmuch-archive-tags
    (apply 'notmuch-show-tag-message
	   (notmuch-tag-change-list notmuch-archive-tags unarchive))))

(defun notmuch-show-archive-message-then-next-or-exit ()
  "Archive the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then exit back
to search results."
  (interactive)
  (notmuch-show-archive-message)
  (notmuch-show-next-open-message t))

(defun notmuch-show-archive-message-then-next-or-next-thread ()
  "Archive the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then show next
thread from search."
  (interactive)
  (notmuch-show-archive-message)
  (unless (notmuch-show-next-open-message)
    (notmuch-show-next-thread t)))

(defun notmuch-show-stash-cc ()
  "Copy CC field of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-cc)))

(defun notmuch-show-stash-date ()
  "Copy date of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-date)))

(defun notmuch-show-stash-filename ()
  "Copy filename of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-filename)))

(defun notmuch-show-stash-from ()
  "Copy From address of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-from)))

(defun notmuch-show-stash-message-id ()
  "Copy id: query matching the current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-message-id)))

(defun notmuch-show-stash-message-id-stripped ()
  "Copy message ID of current message (sans `id:' prefix) to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-message-id t)))

(defun notmuch-show-stash-subject ()
  "Copy Subject field of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-subject)))

(defun notmuch-show-stash-tags ()
  "Copy tags of current message to kill-ring as a comma separated list."
  (interactive)
  (notmuch-common-do-stash (mapconcat 'identity (notmuch-show-get-tags) ",")))

(defun notmuch-show-stash-to ()
  "Copy To address of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-to)))

(defun notmuch-show-stash-mlarchive-link (&optional mla)
  "Copy an ML Archive URI for the current message to the kill-ring.

This presumes that the message is available at the selected Mailing List Archive.

If optional argument MLA is non-nil, use the provided key instead of prompting
the user (see `notmuch-show-stash-mlarchive-link-alist')."
  (interactive)
  (notmuch-common-do-stash
   (concat (cdr (assoc
		 (or mla
		     (let ((completion-ignore-case t))
		       (completing-read
			"Mailing List Archive: "
			notmuch-show-stash-mlarchive-link-alist
			nil t nil nil notmuch-show-stash-mlarchive-link-default)))
		 notmuch-show-stash-mlarchive-link-alist))
	   (notmuch-show-get-message-id t))))

(defun notmuch-show-stash-mlarchive-link-and-go (&optional mla)
  "Copy an ML Archive URI for the current message to the kill-ring and visit it.

This presumes that the message is available at the selected Mailing List Archive.

If optional argument MLA is non-nil, use the provided key instead of prompting
the user (see `notmuch-show-stash-mlarchive-link-alist')."
  (interactive)
  (notmuch-show-stash-mlarchive-link mla)
  (browse-url (current-kill 0 t)))

;; Commands typically bound to buttons.

(defun notmuch-show-part-button-default (&optional button)
  (interactive)
  (notmuch-show-part-button-internal button notmuch-show-part-button-default-action))

(defun notmuch-show-part-button-save (&optional button)
  (interactive)
  (notmuch-show-part-button-internal button #'notmuch-show-save-part))

(defun notmuch-show-part-button-view (&optional button)
  (interactive)
  (notmuch-show-part-button-internal button #'notmuch-show-view-part))

(defun notmuch-show-part-button-interactively-view (&optional button)
  (interactive)
  (notmuch-show-part-button-internal button #'notmuch-show-interactively-view-part))

(defun notmuch-show-part-button-pipe (&optional button)
  (interactive)
  (notmuch-show-part-button-internal button #'notmuch-show-pipe-part))

(defun notmuch-show-part-button-internal (button handler)
  (let ((button (or button (button-at (point)))))
    (if button
	(let ((nth (button-get button :notmuch-part)))
	  (if nth
	      (funcall handler (notmuch-show-get-message-id) nth
		       (button-get button :notmuch-filename)
		       (button-get button :notmuch-content-type)))))))

;;

(provide 'notmuch-show)
