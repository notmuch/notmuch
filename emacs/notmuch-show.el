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

(require 'cl)
(require 'mm-view)
(require 'message)
(require 'mm-decode)
(require 'mailcap)

(require 'notmuch-lib)
(require 'notmuch-query)
(require 'notmuch-wash)
(require 'notmuch-mua)

(declare-function notmuch-call-notmuch-process "notmuch" (&rest args))
(declare-function notmuch-fontify-headers "notmuch" nil)
(declare-function notmuch-select-tag-with-completion "notmuch" (prompt &rest search-terms))
(declare-function notmuch-search-show-thread "notmuch" nil)

(defcustom notmuch-message-headers '("Subject" "To" "Cc" "Date")
  "Headers that should be shown in a message, in this order.

For an open message, all of these headers will be made visible
according to `notmuch-message-headers-visible' or can be toggled
with `notmuch-show-toggle-headers'. For a closed message, only
the first header in the list will be visible."
  :group 'notmuch
  :type '(repeat string))

(defcustom notmuch-message-headers-visible t
  "Should the headers be visible by default?

If this value is non-nil, then all of the headers defined in
`notmuch-message-headers' will be visible by default in the display
of each message. Otherwise, these headers will be hidden and
`notmuch-show-toggle-headers' can be used to make the visible for
any given message."
  :group 'notmuch
  :type 'boolean)

(defvar notmuch-show-markup-headers-hook '(notmuch-show-colour-headers)
  "A list of functions called to decorate the headers listed in
`notmuch-message-headers'.")

(defcustom notmuch-show-hook nil
  "Functions called after populating a `notmuch-show' buffer."
  :group 'notmuch
  :type 'hook)

(defcustom notmuch-show-insert-text/plain-hook '(notmuch-wash-excerpt-citations)
  "Functions used to improve the display of text/plain parts."
  :group 'notmuch
  :type 'hook
  :options '(notmuch-wash-convert-inline-patch-to-part
	     notmuch-wash-wrap-long-lines
	     notmuch-wash-tidy-citations
	     notmuch-wash-elide-blank-lines
	     notmuch-wash-excerpt-citations))

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
				 (mapconcat 'identity tags " ")
				 ")"))))))

(defun notmuch-show-insert-headerline (headers date tags depth)
  "Insert a notmuch style headerline based on HEADERS for a
message at DEPTH in the current thread."
  (let ((start (point)))
    (insert (notmuch-show-spaces-n depth)
	    (plist-get headers :From)
	    " ("
	    date
	    ") ("
	    (mapconcat 'identity tags " ")
	    ")\n")
    (overlay-put (make-overlay start (point)) 'face 'notmuch-message-summary-face)))

(defun notmuch-show-insert-header (header header-value)
  "Insert a single header."
  (insert header ": " header-value "\n"))

(defun notmuch-show-insert-headers (headers)
  "Insert the headers of the current message."
  (let ((start (point)))
    (mapc '(lambda (header)
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
  'action 'notmuch-show-part-button-action
  'follow-link t
  'face 'message-mml)

(defun notmuch-show-insert-part-header (nth content-type declared-type &optional name)
  (insert-button
   (concat "[ "
	   (if name (concat name ": ") "")
	   declared-type
	   (if (not (string-equal declared-type content-type))
	       (concat " (as " content-type ")")
	     "")
	   " ]\n")
   :type 'notmuch-show-part-button-type
   :notmuch-part nth
   :notmuch-filename name))

;; Functions handling particular MIME parts.

(defun notmuch-show-save-part (message-id nth &optional filename)
  (with-temp-buffer
    ;; Always acquires the part via `notmuch part', even if it is
    ;; available in the JSON output.
    (insert (notmuch-show-get-bodypart-internal message-id nth))
    (let ((file (read-file-name
		 "Filename to save as: "
		 (or mailcap-download-directory "~/")
		 nil nil
		 filename))
	  (require-final-newline nil)
          (coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) file))))

(defun notmuch-show-mm-display-part-inline (msg part content-type content)
  "Use the mm-decode/mm-view functions to display a part in the
current buffer, if possible."
  (let ((display-buffer (current-buffer)))
    (with-temp-buffer
      (insert content)
      (let ((handle (mm-make-handle (current-buffer) (list content-type))))
	(set-buffer display-buffer)
	(if (and (mm-inlinable-p handle)
		 (mm-inlined-p handle))
	    (progn
	      (mm-display-part handle)
	      t)
	  nil)))))

(defun notmuch-show-insert-part-text/plain (msg part content-type nth depth declared-type)
  (let ((start (point)))
    ;; If this text/plain part is not the first part in the message,
    ;; insert a header to make this clear.
    (if (> nth 1)
	(notmuch-show-insert-part-header nth declared-type content-type (plist-get part :filename)))
    (insert (notmuch-show-get-bodypart-content msg part nth))
    (save-excursion
      (save-restriction
	(narrow-to-region start (point-max))
	(run-hook-with-args 'notmuch-show-insert-text/plain-hook depth))))
  t)

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

(defun notmuch-show-insert-part-*/* (msg part content-type nth depth declared-type)
  ;; This handler _must_ succeed - it is the handler of last resort.
  (notmuch-show-insert-part-header nth content-type declared-type (plist-get part :filename))
  (let ((content (notmuch-show-get-bodypart-content msg part nth)))
    (if content
	(notmuch-show-mm-display-part-inline msg part content-type content)))
  t)

;; Functions for determining how to handle MIME parts.

(defun notmuch-show-split-content-type (content-type)
  (split-string content-type "/"))

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
			 (car (notmuch-show-split-content-type content-type))
			 "/*"))
		(intern (concat "notmuch-show-insert-part-" content-type))))
    result))

;; Helper for parts which are generally not included in the default
;; JSON output.

(defun notmuch-show-get-bodypart-internal (message-id part-number)
  (with-temp-buffer
    (let ((coding-system-for-read 'no-conversion))
      (call-process notmuch-command nil t nil
		    "part" (format "--part=%s" part-number) message-id)
      (buffer-string))))

(defun notmuch-show-get-bodypart-content (msg part nth)
  (or (plist-get part :content)
      (notmuch-show-get-bodypart-internal (concat "id:" (plist-get msg :id)) nth)))

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
  (if (not (bolp))
      (insert "\n")))

(defun notmuch-show-insert-body (msg body depth)
  "Insert the body BODY at depth DEPTH in the current thread."
  (mapc '(lambda (part) (notmuch-show-insert-bodypart msg part depth)) body))

(defun notmuch-show-make-symbol (type)
  (make-symbol (concat "notmuch-show-" type)))

(defun notmuch-show-insert-msg (msg depth)
  "Insert the message MSG at depth DEPTH in the current thread."
  (let ((headers (plist-get msg :headers))
	;; Indentation causes the buffer offset of the start/end
	;; points to move, so we must use markers.
	message-start message-end
	content-start content-end
	headers-start headers-end
	body-start body-end
	(headers-invis-spec (notmuch-show-make-symbol "header"))
	(message-invis-spec (notmuch-show-make-symbol "message")))

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
				    (or (plist-get msg :date_relative)
					(plist-get headers :Date))
				    (plist-get msg :tags) depth)

    (setq content-start (point-marker))

    ;; Set `headers-start' to point after the 'Subject:' header to be
    ;; compatible with the existing implementation. This just sets it
    ;; to after the first header.
    (notmuch-show-insert-headers headers)
    ;; Headers should include a blank line (backwards compatibility).
    (insert "\n")
    (save-excursion
      (goto-char content-start)
      (forward-line 1)
      (setq headers-start (point-marker)))
    (setq headers-end (point-marker))

    (setq body-start (point-marker))
    (notmuch-show-insert-body msg (plist-get msg :body) depth)
    ;; Ensure that the body ends with a newline.
    (if (not (bolp))
	(insert "\n"))
    (setq body-end (point-marker))
    (setq content-end (point-marker))

    ;; Indent according to the depth in the thread.
    (indent-rigidly content-start content-end depth)

    (setq message-end (point-max-marker))

    ;; Save the extents of this message over the whole text of the
    ;; message.
    (put-text-property message-start message-end :notmuch-message-extent (cons message-start message-end))

    (plist-put msg :headers-invis-spec headers-invis-spec)
    (overlay-put (make-overlay headers-start headers-end) 'invisible headers-invis-spec)

    (plist-put msg :message-invis-spec message-invis-spec)
    (overlay-put (make-overlay body-start body-end) 'invisible message-invis-spec)

    ;; Save the properties for this message. Currently this saves the
    ;; entire message (augmented it with other stuff), which seems
    ;; like overkill. We might save a reduced subset (for example, not
    ;; the content).
    (notmuch-show-set-message-properties msg)

    ;; Set header visibility.
    (notmuch-show-headers-visible msg notmuch-message-headers-visible)

    ;; Message visibility depends on whether it matched the search
    ;; criteria.
    (notmuch-show-message-visible msg (plist-get msg :match))))

(defun notmuch-show-insert-tree (tree depth)
  "Insert the message tree TREE at depth DEPTH in the current thread."
  (let ((msg (car tree))
	(replies (cadr tree)))
    (notmuch-show-insert-msg msg depth)
    (notmuch-show-insert-thread replies (1+ depth))))

(defun notmuch-show-insert-thread (thread depth)
  "Insert the thread THREAD at depth DEPTH in the current forest."
  (mapc '(lambda (tree) (notmuch-show-insert-tree tree depth)) thread))

(defun notmuch-show-insert-forest (forest)
  "Insert the forest of threads FOREST."
  (mapc '(lambda (thread) (notmuch-show-insert-thread thread 0)) forest))

(defvar notmuch-show-parent-buffer nil)

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

The optional BUFFER-NAME provides the neame of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used. "
  (interactive "sNotmuch show: ")
  (let ((buffer (get-buffer-create (generate-new-buffer-name
				    (or buffer-name
					(concat "*notmuch-" thread-id "*")))))
	(inhibit-read-only t))
    (switch-to-buffer buffer)
    (notmuch-show-mode)
    (set (make-local-variable 'notmuch-show-parent-buffer) parent-buffer)
    (erase-buffer)
    (goto-char (point-min))
    (save-excursion
      (let* ((basic-args (list thread-id))
	     (args (if query-context
		       (append (list "\'") basic-args (list "and (" query-context ")\'"))
		     (append (list "\'") basic-args (list "\'")))))
	(notmuch-show-insert-forest (notmuch-query-get-threads args))
	;; If the query context reduced the results to nothing, run
	;; the basic query.
	(when (and (eq (buffer-size) 0)
		   query-context)
	  (notmuch-show-insert-forest
	   (notmuch-query-get-threads basic-args))))

      ;; Enable buttonisation of URLs and email addresses in the
      ;; buffer.
      (goto-address-mode t)
      ;; Act on visual lines rather than logical lines.
      (visual-line-mode t)

      (run-hooks 'notmuch-show-hook))

    ;; Move straight to the first open message
    (if (not (notmuch-show-message-visible-p))
	(notmuch-show-next-open-message))
    (notmuch-show-mark-read)))

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
  "Submap for stash commands")
(fset 'notmuch-show-stash-map notmuch-show-stash-map)

(defvar notmuch-show-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map "?" 'notmuch-help)
	(define-key map "q" 'kill-this-buffer)
	(define-key map (kbd "M-TAB") 'notmuch-show-previous-button)
	(define-key map (kbd "TAB") 'notmuch-show-next-button)
	(define-key map "s" 'notmuch-search)
	(define-key map "m" 'notmuch-mua-mail)
	(define-key map "f" 'notmuch-show-forward-message)
	(define-key map "r" 'notmuch-show-reply)
	(define-key map "|" 'notmuch-show-pipe-message)
	(define-key map "w" 'notmuch-show-save-attachments)
	(define-key map "V" 'notmuch-show-view-raw-message)
	(define-key map "v" 'notmuch-show-view-all-mime-parts)
	(define-key map "c" 'notmuch-show-stash-map)
	(define-key map "h" 'notmuch-show-toggle-headers)
	(define-key map "-" 'notmuch-show-remove-tag)
	(define-key map "+" 'notmuch-show-add-tag)
	(define-key map "x" 'notmuch-show-archive-thread-then-exit)
	(define-key map "a" 'notmuch-show-archive-thread)
	(define-key map "N" 'notmuch-show-next-message)
	(define-key map "P" 'notmuch-show-previous-message)
	(define-key map "n" 'notmuch-show-next-open-message)
	(define-key map "p" 'notmuch-show-previous-open-message)
	(define-key map (kbd "DEL") 'notmuch-show-rewind)
	(define-key map " " 'notmuch-show-advance-and-archive)
	(define-key map (kbd "M-RET") 'notmuch-show-open-or-close-all)
	(define-key map (kbd "RET") 'notmuch-show-toggle-message)
	map)
      "Keymap for \"notmuch show\" buffers.")
(fset 'notmuch-show-mode-map notmuch-show-mode-map)

;;;###autoload
(defun notmuch-show-mode ()
  "Major mode for viewing a thread with notmuch.

This buffer contains the results of the \"notmuch show\" command
for displaying a single thread of email from your email archives.

By default, various components of email messages, (citations,
signatures, already-read messages), are hidden. You can make
these parts visible by clicking with the mouse button or by
pressing RET after positioning the cursor on a hidden part, (for
which \\[notmuch-show-next-button] and
\\[notmuch-show-previous-button] are helpful).

Reading the thread sequentially is well-supported by pressing
\\[notmuch-show-advance-and-archive]. This will scroll the
current message (if necessary), advance to the next message, or
advance to the next thread (if already on the last message of a
thread).

Other commands are available to read or manipulate the thread
more selectively, (such as '\\[notmuch-show-next-message]' and
'\\[notmuch-show-previous-message]' to advance to messages
without removing any tags, and '\\[notmuch-show-archive-thread]'
to archive an entire thread without scrolling through with
\\[notmuch-show-advance-and-archive]).

You can add or remove arbitary tags from the current message with
'\\[notmuch-show-add-tag]' or '\\[notmuch-show-remove-tag]'.

All currently available key bindings:

\\{notmuch-show-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map notmuch-show-mode-map)
  (setq major-mode 'notmuch-show-mode
	mode-name "notmuch-show")
  (setq buffer-read-only t))

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

(defun notmuch-show-move-past-invisible-forward ()
  (while (point-invisible-p)
    (forward-char)))

(defun notmuch-show-move-past-invisible-backward ()
  (while (point-invisible-p)
    (backward-char)))

;; Functions relating to the visibility of messages and their
;; components.

(defun notmuch-show-element-visible (props visible-p spec-property)
  (let ((spec (plist-get props spec-property)))
    (if visible-p
	(remove-from-invisibility-spec spec)
      (add-to-invisibility-spec spec))))

(defun notmuch-show-message-visible (props visible-p)
  (if visible-p
      ;; When making the message visible, the headers may or not be
      ;; visible. So we check that property separately.
      (let ((headers-visible (plist-get props :headers-visible)))
	(notmuch-show-element-visible props headers-visible :headers-invis-spec)
	(notmuch-show-element-visible props t :message-invis-spec))
    (notmuch-show-element-visible props nil :headers-invis-spec)
    (notmuch-show-element-visible props nil :message-invis-spec))

  (notmuch-show-set-prop :message-visible visible-p props))

(defun notmuch-show-headers-visible (props visible-p)
  (if (plist-get props :message-visible)
      (notmuch-show-element-visible props visible-p :headers-invis-spec))
  (notmuch-show-set-prop :headers-visible visible-p props))

;; Functions for setting and getting attributes of the current
;; message.

(defun notmuch-show-set-message-properties (props)
  (save-excursion
    (notmuch-show-move-to-message-top)
    (put-text-property (point) (+ (point) 1) :notmuch-message-properties props)))

(defun notmuch-show-get-message-properties ()
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

(defun notmuch-show-get-message-id ()
  "Return the message id of the current message."
  (concat "id:" (notmuch-show-get-prop :id)))

;; dme: Would it make sense to use a macro for many of these?

(defun notmuch-show-get-filename ()
  "Return the filename of the current message."
  (notmuch-show-get-prop :filename))

(defun notmuch-show-get-header (header)
  "Return the named header of the current message, if any."
  (plist-get (notmuch-show-get-prop :headers) header))

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

(defun notmuch-show-mark-read ()
  "Mark the current message as read."
  (notmuch-show-remove-tag "unread"))

;; Commands typically bound to keys.

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
  (let ((end-of-this-message (notmuch-show-message-bottom)))
    (cond
     ;; Ideally we would test `end-of-this-message' against the result
     ;; of `window-end', but that doesn't account for the fact that
     ;; the end of the message might be hidden, so we have to actually
     ;; go to the end, walk back over invisible text and then see if
     ;; point is visible.
     ((save-excursion
	(goto-char (- end-of-this-message 1))
	(notmuch-show-move-past-invisible-backward)
	(> (point) (window-end)))
      ;; The bottom of this message is not visible - scroll.
      (scroll-up nil))

     ((not (= end-of-this-message (point-max)))
      ;; This is not the last message - move to the next visible one.
      (notmuch-show-next-open-message))

     (t
      ;; This is the last message - archive the thread.
      (notmuch-show-archive-thread)))))

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
      (if (<= (count-screen-lines (window-start) start-of-message)
	      next-screen-context-lines)
	  (progn
	    (goto-char (notmuch-show-message-top))
	    (notmuch-show-message-adjust)))
      ;; Move to the top left of the window.
      (goto-char (window-start)))
     (t
      ;; Move to the previous message.
      (notmuch-show-previous-message)))))

(defun notmuch-show-reply ()
  "Reply to the current message."
  (interactive)
  (notmuch-mua-reply (notmuch-show-get-message-id)))

(defun notmuch-show-forward-message ()
  "Forward the current message."
  (interactive)
  (with-current-notmuch-show-message
   (notmuch-mua-forward-message)))

(defun notmuch-show-next-message ()
  "Show the next message."
  (interactive)
  (if (notmuch-show-goto-message-next)
      (progn
	(notmuch-show-mark-read)
	(notmuch-show-message-adjust))
    (goto-char (point-max))))

(defun notmuch-show-previous-message ()
  "Show the previous message."
  (interactive)
  (notmuch-show-goto-message-previous)
  (notmuch-show-mark-read)
  (notmuch-show-message-adjust))

(defun notmuch-show-next-open-message ()
  "Show the next message."
  (interactive)
  (let (r)
    (while (and (setq r (notmuch-show-goto-message-next))
		(not (notmuch-show-message-visible-p))))
    (if r
	(progn
	  (notmuch-show-mark-read)
	  (notmuch-show-message-adjust))
      (goto-char (point-max)))))

(defun notmuch-show-previous-open-message ()
  "Show the previous message."
  (interactive)
  (while (and (notmuch-show-goto-message-previous)
	      (not (notmuch-show-message-visible-p))))
  (notmuch-show-mark-read)
  (notmuch-show-message-adjust))

(defun notmuch-show-view-raw-message ()
  "View the file holding the current message."
  (interactive)
  (view-file (notmuch-show-get-filename)))

(defun notmuch-show-pipe-message (command)
  "Pipe the contents of the current message to the given command.

The given command will be executed with the raw contents of the
current email message as stdin. Anything printed by the command
to stdout or stderr will appear in the *Messages* buffer."
  (interactive "sPipe message to command: ")
  (apply 'start-process-shell-command "notmuch-pipe-command" "*notmuch-pipe*"
	 (list command " < "
	       (shell-quote-argument (notmuch-show-get-filename)))))

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
   (list (notmuch-select-tag-with-completion
	  "Tag to remove: " (notmuch-show-get-message-id))))
  (let ((tags (notmuch-show-get-tags)))
    (if (intersection tags toremove :test 'string=)
	(progn
	  (apply 'notmuch-call-notmuch-process
		 (append (cons "tag"
			       (mapcar (lambda (s) (concat "-" s)) toremove))
			 (cons (notmuch-show-get-message-id) nil)))
	  (notmuch-show-set-tags (sort (set-difference tags toremove :test 'string=) 'string<))))))

(defun notmuch-show-toggle-headers ()
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

(defun notmuch-show-archive-thread-internal (show-next)
  ;; Remove the tag from the current set of messages.
  (goto-char (point-min))
  (loop do (notmuch-show-remove-tag "inbox")
	until (not (notmuch-show-goto-message-next)))
  ;; Move to the next item in the search results, if any.
  (let ((parent-buffer notmuch-show-parent-buffer))
    (kill-this-buffer)
    (if parent-buffer
	(progn
	  (switch-to-buffer parent-buffer)
	  (forward-line)
	  (if show-next
	      (notmuch-search-show-thread))))))

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
  (notmuch-show-archive-thread-internal t))

(defun notmuch-show-archive-thread-then-exit ()
  "Archive each message in thread, then exit back to search results."
  (interactive)
  (notmuch-show-archive-thread-internal nil))

(defun notmuch-show-do-stash (text)
  (kill-new text)
  (message "Saved: %s" text))

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

;; Commands typically bound to buttons.

(defun notmuch-show-part-button-action (button)
  (let ((nth (button-get button :notmuch-part)))
    (if nth
	(notmuch-show-save-part (notmuch-show-get-message-id) nth
				(button-get button :notmuch-filename))
      (message "Not a valid part (is it a fake part?)."))))

;;

(provide 'notmuch-show)
