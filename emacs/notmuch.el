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

(require 'notmuch-lib)
(require 'notmuch-show)
(require 'notmuch-mua)
(require 'notmuch-hello)
(require 'notmuch-maildir-fcc)
(require 'notmuch-message)

(defcustom notmuch-search-result-format
  `(("date" . "%s ")
    ("count" . "%-7s ")
    ("authors" . "%-20s ")
    ("subject" . "%s ")
    ("tags" . "(%s)"))
  "Search result formating. Supported fields are:
	date, count, authors, subject, tags
For example:
	(setq notmuch-search-result-format \(\(\"authors\" . \"%-40s\"\)
					     \(\"subject\" . \"%s\"\)\)\)"
  :type '(alist :key-type (string) :value-type (string))
  :group 'notmuch)

(defun notmuch-select-tag-with-completion (prompt &rest search-terms)
  (let ((tag-list
	 (with-output-to-string
	   (with-current-buffer standard-output
	     (apply 'call-process notmuch-command nil t nil "search-tags" search-terms)))))
    (completing-read prompt (split-string tag-list "\n+" t) nil nil nil)))

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

(defcustom notmuch-search-hook '(hl-line-mode)
  "List of functions to call when notmuch displays the search results."
  :type 'hook
  :options '(hl-line-mode)
  :group 'notmuch)

(defvar notmuch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'notmuch-help)
    (define-key map "q" 'notmuch-search-quit)
    (define-key map "x" 'notmuch-search-quit)
    (define-key map (kbd "<DEL>") 'notmuch-search-scroll-down)
    (define-key map "b" 'notmuch-search-scroll-down)
    (define-key map " " 'notmuch-search-scroll-up)
    (define-key map "<" 'notmuch-search-first-thread)
    (define-key map ">" 'notmuch-search-last-thread)
    (define-key map "p" 'notmuch-search-previous-thread)
    (define-key map "n" 'notmuch-search-next-thread)
    (define-key map "r" 'notmuch-search-reply-to-thread)
    (define-key map "m" 'notmuch-mua-mail)
    (define-key map "s" 'notmuch-search)
    (define-key map "o" 'notmuch-search-toggle-order)
    (define-key map "=" 'notmuch-search-refresh-view)
    (define-key map "G" 'notmuch-search-poll-and-refresh-view)
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
(defvar notmuch-search-target-thread)
(defvar notmuch-search-target-line)
(defvar notmuch-search-continuation)

(defvar notmuch-search-disjunctive-regexp      "\\<[oO][rR]\\>")

(defun notmuch-search-quit ()
  "Exit the search buffer, calling any defined continuation function."
  (interactive)
  (let ((continuation notmuch-search-continuation))
    (kill-this-buffer)
    (when continuation
      (funcall continuation))))

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

(defface notmuch-search-non-matching-authors
  '((((class color)
      (background dark))
     (:foreground "grey30"))
    (((class color)
      (background light))
     (:foreground "grey60"))
    (t (:italic t)))
  "Face used in search mode for authors not matching the query."
  :group 'notmuch)

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
  (make-local-variable 'notmuch-search-target-thread)
  (make-local-variable 'notmuch-search-target-line)
  (set (make-local-variable 'notmuch-search-continuation) nil)
  (set (make-local-variable 'scroll-preserve-screen-position) t)
  (add-to-invisibility-spec 'notmuch-search)
  (use-local-map notmuch-search-mode-map)
  (setq truncate-lines t)
  (setq major-mode 'notmuch-search-mode
	mode-name "notmuch-search")
  (setq buffer-read-only t))

(defun notmuch-search-properties-in-region (property beg end)
  (save-excursion
    (let ((output nil)
	  (last-line (line-number-at-pos end))
	  (max-line (- (line-number-at-pos (point-max)) 2)))
      (goto-char beg)
      (beginning-of-line)
      (while (<= (line-number-at-pos) (min last-line max-line))
	(setq output (cons (get-text-property (point) property) output))
	(forward-line 1))
      output)))

(defun notmuch-search-find-thread-id ()
  "Return the thread for the current thread"
  (get-text-property (point) 'notmuch-search-thread-id))

(defun notmuch-search-find-thread-id-region (beg end)
  "Return a list of threads for the current region"
  (notmuch-search-properties-in-region 'notmuch-search-thread-id beg end))

(defun notmuch-search-find-authors ()
  "Return the authors for the current thread"
  (get-text-property (point) 'notmuch-search-authors))

(defun notmuch-search-find-authors-region (beg end)
  "Return a list of authors for the current region"
  (notmuch-search-properties-in-region 'notmuch-search-authors beg end))

(defun notmuch-search-find-subject ()
  "Return the subject for the current thread"
  (get-text-property (point) 'notmuch-search-subject))

(defun notmuch-search-find-subject-region (beg end)
  "Return a list of authors for the current region"
  (notmuch-search-properties-in-region 'notmuch-search-subject beg end))

(defun notmuch-search-show-thread ()
  "Display the currently selected thread."
  (interactive)
  (let ((thread-id (notmuch-search-find-thread-id))
	(subject (notmuch-search-find-subject)))
    (if (> (length thread-id) 0)
	(notmuch-show thread-id
		      (current-buffer)
		      notmuch-search-query-string
		      ;; name the buffer based on notmuch-search-find-subject
		      (if (string-match "^[ \t]*$" subject)
			  "[No Subject]"
			(truncate-string-to-width
			 (concat "*"
				 (truncate-string-to-width subject 32 nil nil t)
				 "*")
			 32 nil nil t)))
      (error "End of search results"))))

(defun notmuch-search-reply-to-thread ()
  "Begin composing a reply to the entire current thread in a new buffer."
  (interactive)
  (let ((message-id (notmuch-search-find-thread-id)))
    (notmuch-mua-reply message-id)))

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
	(insert (propertize (mapconcat  'identity tags " ")
			    'font-lock-face 'notmuch-tag-face))))))

(defun notmuch-search-get-tags ()
  (save-excursion
    (end-of-line)
    (re-search-backward "(")
    (let ((beg (+ (point) 1)))
      (re-search-forward ")")
      (let ((end (- (point) 1)))
	(split-string (buffer-substring beg end))))))

(defun notmuch-search-get-tags-region (beg end)
  (save-excursion
    (let ((output nil)
	  (last-line (line-number-at-pos end))
	  (max-line (- (line-number-at-pos (point-max)) 2)))
      (goto-char beg)
      (while (<= (line-number-at-pos) (min last-line max-line))
	(setq output (append output (notmuch-search-get-tags)))
	(forward-line 1))
      output)))

(defun notmuch-search-add-tag-thread (tag)
  (notmuch-search-add-tag-region tag (point) (point)))

(defun notmuch-search-add-tag-region (tag beg end)
  (let ((search-id-string (mapconcat 'identity (notmuch-search-find-thread-id-region beg end) " or ")))
    (notmuch-call-notmuch-process "tag" (concat "+" tag) search-id-string)
    (save-excursion
      (let ((last-line (line-number-at-pos end))
	    (max-line (- (line-number-at-pos (point-max)) 2)))
	(goto-char beg)
	(while (<= (line-number-at-pos) (min last-line max-line))
	  (notmuch-search-set-tags (delete-dups (sort (cons tag (notmuch-search-get-tags)) 'string<)))
	  (forward-line))))))

(defun notmuch-search-remove-tag-thread (tag)
  (notmuch-search-remove-tag-region tag (point) (point)))

(defun notmuch-search-remove-tag-region (tag beg end)
  (let ((search-id-string (mapconcat 'identity (notmuch-search-find-thread-id-region beg end) " or ")))
    (notmuch-call-notmuch-process "tag" (concat "-" tag) search-id-string)
    (save-excursion
      (let ((last-line (line-number-at-pos end))
	    (max-line (- (line-number-at-pos (point-max)) 2)))
	(goto-char beg)
	(while (<= (line-number-at-pos) (min last-line max-line))
	  (notmuch-search-set-tags (delete tag (notmuch-search-get-tags)))
	  (forward-line))))))

(defun notmuch-search-add-tag (tag)
  "Add a tag to the currently selected thread or region.

The tag is added to all messages in the currently selected thread
or threads in the current region."
  (interactive
   (list (notmuch-select-tag-with-completion "Tag to add: ")))
  (save-excursion
    (if (region-active-p)
	(let* ((beg (region-beginning))
	       (end (region-end)))
	  (notmuch-search-add-tag-region tag beg end))
      (notmuch-search-add-tag-thread tag))))

(defun notmuch-search-remove-tag (tag)
  "Remove a tag from the currently selected thread or region.

The tag is removed from all messages in the currently selected
thread or threads in the current region."
  (interactive
   (list (notmuch-select-tag-with-completion
	  "Tag to remove: "
	  (if (region-active-p)
	      (mapconcat 'identity
			 (notmuch-search-find-thread-id-region (region-beginning) (region-end))
			 " ")
	    (notmuch-search-find-thread-id)))))
  (save-excursion
    (if (region-active-p)
	(let* ((beg (region-beginning))
	       (end (region-end)))
	  (notmuch-search-remove-tag-region tag beg end))
      (notmuch-search-remove-tag-thread tag))))

(defun notmuch-search-archive-thread ()
  "Archive the currently selected thread (remove its \"inbox\" tag).

This function advances the next thread when finished."
  (interactive)
  (notmuch-search-remove-tag-thread "inbox")
  (forward-line))

(defun notmuch-search-process-sentinel (proc msg)
  "Add a message to let user know when \"notmuch search\" exits"
  (let ((buffer (process-buffer proc))
	(status (process-status proc))
	(exit-status (process-exit-status proc))
	(never-found-target-thread nil))
    (if (memq status '(exit signal))
	(if (buffer-live-p buffer)
	    (with-current-buffer buffer
	      (save-excursion
		(let ((inhibit-read-only t)
		      (atbob (bobp)))
		  (goto-char (point-max))
		  (if (eq status 'signal)
		      (insert "Incomplete search results (search process was killed).\n"))
		  (if (eq status 'exit)
		      (progn
			(insert "End of search results.")
			(if (not (= exit-status 0))
			    (insert (format " (process returned %d)" exit-status)))
			(insert "\n")
			(if (and atbob
				 (not (string= notmuch-search-target-thread "found")))
			    (set 'never-found-target-thread t))))))
	      (if (and never-found-target-thread
		       notmuch-search-target-line)
		  (goto-line notmuch-search-target-line)))))))

(defcustom notmuch-search-line-faces nil
  "Tag/face mapping for line highlighting in notmuch-search.

Here is an example of how to color search results based on tags.
(the following text would be placed in your ~/.emacs file):

(setq notmuch-search-line-faces '((\"delete\" . '(:foreground \"red\"))
                                 (\"unread\" . '(:foreground \"green\"))))

Order matters: for lines with multiple tags, the the first
matching will be applied."
  :type '(alist :key-type (string) :value-type (list))
  :group 'notmuch)

(defun notmuch-search-color-line (start end line-tag-list)
  "Colorize lines in notmuch-show based on tags"
  (if notmuch-search-line-faces
      (let ((overlay (make-overlay start end))
	    (tags-faces (copy-alist notmuch-search-line-faces)))
	(while tags-faces
	  (let* ((tag-face (car tags-faces))
		 (tag (car tag-face))
		 (face (cdr tag-face)))
	    (cond ((member tag line-tag-list)
		   (overlay-put overlay 'face face)
		   (setq tags-faces nil))
		  (t
		   (setq tags-faces (cdr tags-faces)))))))))

(defun notmuch-search-insert-authors (format-string authors)
  (insert (let* ((formatted-sample (format format-string ""))
		 (formatted-authors (format format-string authors))
		 (truncated-string
		  (if (> (length formatted-authors)
			 (length formatted-sample))
		      (concat (substring authors 0 (- (length formatted-sample) 4)) "... ")
		    formatted-authors)))
	    ;; Need to save the match data to avoid interfering with
	    ;; `notmuch-search-process-filter'.
	    (save-match-data
	      (if (string-match "\\(.*\\)|\\(..*\\)" truncated-string)
		  (concat (match-string 1 truncated-string) ","
			  (propertize (match-string 2 truncated-string)
				      'face 'notmuch-search-non-matching-authors))
		truncated-string)))))

(defun notmuch-search-insert-field (field date count authors subject tags)
  (cond
   ((string-equal field "date")
    (insert (format (cdr (assoc field notmuch-search-result-format)) date)))
   ((string-equal field "count")
    (insert (format (cdr (assoc field notmuch-search-result-format)) count)))
   ((string-equal field "authors")
    (notmuch-search-insert-authors (cdr (assoc field notmuch-search-result-format)) authors))
   ((string-equal field "subject")
    (insert (format (cdr (assoc field notmuch-search-result-format)) subject)))
   ((string-equal field "tags")
    (insert (concat "(" (propertize tags 'font-lock-face 'notmuch-tag-face) ")")))))

(defun notmuch-search-show-result (date count authors subject tags)
  (let ((fields) (field))
    (setq fields (mapcar 'car notmuch-search-result-format))
    (loop for field in fields
	  do (notmuch-search-insert-field field date count authors subject tags)))
  (insert "\n"))

(defun notmuch-search-process-filter (proc string)
  "Process and filter the output of \"notmuch search\""
  (let ((buffer (process-buffer proc))
	(found-target nil))
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
			   (subject (match-string 5 string))
			   (tags (match-string 6 string))
			   (tag-list (if tags (save-match-data (split-string tags)))))
		      (goto-char (point-max))
		      (let ((beg (point-marker)))
			(notmuch-search-show-result date count authors subject tags)
			(notmuch-search-color-line beg (point-marker) tag-list)
			(put-text-property beg (point-marker) 'notmuch-search-thread-id thread-id)
			(put-text-property beg (point-marker) 'notmuch-search-authors authors)
			(put-text-property beg (point-marker) 'notmuch-search-subject subject)
			(if (string= thread-id notmuch-search-target-thread)
			    (progn
			      (set 'found-target beg)
			      (set 'notmuch-search-target-thread "found"))))
		      (set 'line (match-end 0)))
		  (set 'more nil)))))
	  (if found-target
	      (goto-char found-target)))
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

(defun notmuch-search-buffer-title (query)
  "Returns the title for a buffer with notmuch search results."
  (let* ((saved-search (rassoc-if (lambda (key)
				    (string-match (concat "^" (regexp-quote key))
						  query))
				  (reverse (notmuch-saved-searches))))
	 (saved-search-name (car saved-search))
	 (saved-search-query (cdr saved-search)))
    (cond ((and saved-search (equal saved-search-query query))
	   ;; Query is the same as saved search (ignoring case)
	   (concat "*notmuch-saved-search-" saved-search-name "*"))
	  (saved-search
	   (concat "*notmuch-search-"
		   (replace-regexp-in-string (concat "^" (regexp-quote saved-search-query))
					     (concat "[ " saved-search-name " ]")
					     query)
		   "*"))
	  (t
	   (concat "*notmuch-search-" query "*"))
	  )))

;;;###autoload
(defun notmuch-search (query &optional oldest-first target-thread target-line continuation)
  "Run \"notmuch search\" with the given query string and display results.

The optional parameters are used as follows:

  oldest-first: A Boolean controlling the sort order of returned threads
  target-thread: A thread ID (with the thread: prefix) that will be made
                 current if it appears in the search results.
  target-line: The line number to move to if the target thread does not
               appear in the search results."
  (interactive "sNotmuch search: ")
  (let ((buffer (get-buffer-create (notmuch-search-buffer-title query))))
    (switch-to-buffer buffer)
    (notmuch-search-mode)
    (set 'notmuch-search-query-string query)
    (set 'notmuch-search-oldest-first oldest-first)
    (set 'notmuch-search-target-thread target-thread)
    (set 'notmuch-search-target-line target-line)
    (set 'notmuch-search-continuation continuation)
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
  (let ((target-line (line-number-at-pos))
	(oldest-first notmuch-search-oldest-first)
	(target-thread (notmuch-search-find-thread-id))
	(query notmuch-search-query-string)
	(continuation notmuch-search-continuation))
    (kill-this-buffer)
    (notmuch-search query oldest-first target-thread target-line continuation)
    (goto-char (point-min))))

(defcustom notmuch-poll-script ""
  "An external script to incorporate new mail into the notmuch database.

If this variable is non empty, then it should name a script to be
invoked by `notmuch-search-poll-and-refresh-view' and
`notmuch-hello-poll-and-update' (each have a default keybinding
of 'G'). The script could do any of the following depending on
the user's needs:

1. Invoke a program to transfer mail to the local mail store
2. Invoke \"notmuch new\" to incorporate the new mail
3. Invoke one or more \"notmuch tag\" commands to classify the mail"
  :type 'string
  :group 'notmuch)

(defun notmuch-poll ()
  "Run external script to import mail.

Invokes `notmuch-poll-script' if it is not set to an empty string."
  (interactive)
  (if (not (string= notmuch-poll-script ""))
      (call-process notmuch-poll-script nil nil)))

(defun notmuch-search-poll-and-refresh-view ()
  "Invoke `notmuch-poll' to import mail, then refresh the current view."
  (interactive)
  (notmuch-poll)
  (notmuch-search-refresh-view))

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
  (let ((grouped-query (if (string-match-p notmuch-search-disjunctive-regexp query)
			   (concat "( " query " )")
			 query)))
    (notmuch-search (if (string= notmuch-search-query-string "*")
			grouped-query
		      (concat notmuch-search-query-string " and " grouped-query)) notmuch-search-oldest-first)))

(defun notmuch-search-filter-by-tag (tag)
  "Filter the current search results based on a single tag.

Runs a new search matching only messages that match both the
current search results AND that are tagged with the given tag."
  (interactive
   (list (notmuch-select-tag-with-completion "Filter by tag: ")))
  (notmuch-search (concat notmuch-search-query-string " and tag:" tag) notmuch-search-oldest-first))

;;;###autoload
(defun notmuch ()
  "Run notmuch and display saved searches, known tags, etc."
  (interactive)
  (notmuch-hello))

(setq mail-user-agent 'notmuch-user-agent)

(provide 'notmuch)
