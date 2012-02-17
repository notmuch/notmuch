;; notmuch-wash.el --- cleaning up message bodies
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

(require 'coolj)

(declare-function notmuch-show-insert-bodypart "notmuch-show" (msg part depth))

;;

(defvar notmuch-wash-signature-regexp
  "^\\(-- ?\\|_+\\)$"
  "Pattern to match a line that separates content from signature.")

(defvar notmuch-wash-citation-regexp
  "\\(^[[:space:]]*>.*\n\\)+"
  "Pattern to match citation lines.")

(defvar notmuch-wash-original-regexp "^\\(--+\s?[oO]riginal [mM]essage\s?--+\\)$"
  "Pattern to match a line that separates original message from reply in top-posted message.")

(defvar notmuch-wash-button-signature-hidden-format
  "[ %d-line signature. Click/Enter to show. ]"
  "String used to construct button text for hidden signatures.
Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-wash-button-signature-visible-format
  "[ %d-line signature. Click/Enter to hide. ]"
  "String used to construct button text for visible signatures.
Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-wash-button-citation-hidden-format
  "[ %d more citation lines. Click/Enter to show. ]"
  "String used to construct button text for hidden citations.
Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-wash-button-citation-visible-format
  "[ %d more citation lines. Click/Enter to hide. ]"
  "String used to construct button text for visible citations.
Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-wash-button-original-hidden-format
  "[ %d-line hidden original message. Click/Enter to show. ]"
  "String used to construct button text for hidden citations.
Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-wash-button-original-visible-format
  "[ %d-line original message. Click/Enter to hide. ]"
  "String used to construct button text for visible citations.
Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-wash-signature-lines-max 12
  "Maximum length of signature that will be hidden by default.")

(defvar notmuch-wash-citation-lines-prefix 3
  "Always show at least this many lines from the start of a citation.

If there is one more line than the sum of
`notmuch-wash-citation-lines-prefix' and
`notmuch-wash-citation-lines-suffix', show that, otherwise
collapse the remaining lines into a button.")

(defvar notmuch-wash-citation-lines-suffix 3
  "Always show at least this many lines from the end of a citation.

If there is one more line than the sum of
`notmuch-wash-citation-lines-prefix' and
`notmuch-wash-citation-lines-suffix', show that, otherwise
collapse the remaining lines into a button.")

(defvar notmuch-wash-wrap-lines-length nil
  "Wrap line after at most this many characters.

If this is nil, lines in messages will be wrapped to fit in the
current window. If this is a number, lines will be wrapped after
this many characters or at the window width (whichever one is
lower).")

(defun notmuch-wash-toggle-invisible-action (cite-button)
  (let ((invis-spec (button-get cite-button 'invisibility-spec)))
    (if (invisible-p invis-spec)
	(remove-from-invisibility-spec invis-spec)
      (add-to-invisibility-spec invis-spec)))
  (let* ((new-start (button-start cite-button))
	 (overlay (button-get cite-button 'overlay))
	 (button-label (notmuch-wash-button-label overlay))
	 (old-point (point))
	 (inhibit-read-only t))
    (goto-char new-start)
    (insert button-label)
    (let ((old-end (button-end cite-button)))
      (move-overlay cite-button new-start (point))
      (delete-region (point) old-end))
    (goto-char (min old-point (1- (button-end cite-button)))))
  (force-window-update)
  (redisplay t))

(define-button-type 'notmuch-wash-button-invisibility-toggle-type
  'action 'notmuch-wash-toggle-invisible-action
  'follow-link t
  'face 'font-lock-comment-face)

(define-button-type 'notmuch-wash-button-citation-toggle-type
  'help-echo "mouse-1, RET: Show citation"
  :supertype 'notmuch-wash-button-invisibility-toggle-type)

(define-button-type 'notmuch-wash-button-signature-toggle-type
  'help-echo "mouse-1, RET: Show signature"
  :supertype 'notmuch-wash-button-invisibility-toggle-type)

(define-button-type 'notmuch-wash-button-original-toggle-type
  'help-echo "mouse-1, RET: Show original message"
  :supertype 'notmuch-wash-button-invisibility-toggle-type)

(defun notmuch-wash-region-isearch-show (overlay)
  (dolist (invis-spec (overlay-get overlay 'invisible))
    (remove-from-invisibility-spec invis-spec)))

(defun notmuch-wash-button-label (overlay)
  (let* ((type (overlay-get overlay 'type))
	 (invis-spec (overlay-get overlay 'invisible))
	 (state (if (invisible-p invis-spec) "hidden" "visible"))
	 (label-format (symbol-value (intern-soft (concat "notmuch-wash-button-"
							  type "-" state "-format"))))
	 (lines-count (count-lines (overlay-start overlay) (overlay-end overlay))))
    (format label-format lines-count)))

(defun notmuch-wash-region-to-button (msg beg end type &optional prefix)
  "Auxiliary function to do the actual making of overlays and buttons

BEG and END are buffer locations. TYPE should a string, either
\"citation\" or \"signature\". Optional PREFIX is some arbitrary
text to insert before the button, probably for indentation.  Note
that PREFIX should not include a newline."

  ;; This uses some slightly tricky conversions between strings and
  ;; symbols because of the way the button code works. Note that
  ;; replacing intern-soft with make-symbol will cause this to fail,
  ;; since the newly created symbol has no plist.

  (let ((overlay (make-overlay beg end))
	(message-invis-spec (plist-get msg :message-invis-spec))
	(invis-spec (make-symbol (concat "notmuch-" type "-region")))
	(button-type (intern-soft (concat "notmuch-wash-button-"
					  type "-toggle-type"))))
    (add-to-invisibility-spec invis-spec)
    (overlay-put overlay 'invisible (list invis-spec message-invis-spec))
    (overlay-put overlay 'isearch-open-invisible #'notmuch-wash-region-isearch-show)
    (overlay-put overlay 'priority 10)
    (overlay-put overlay 'type type)
    (goto-char (1+ end))
    (save-excursion
      (goto-char beg)
      (if prefix
	  (insert-before-markers prefix))
      (let ((button-beg (point)))
	(insert-before-markers (notmuch-wash-button-label overlay) "\n")
	(make-button button-beg (1- (point))
		     'invisibility-spec invis-spec
		     'overlay overlay
		     :type button-type)))))

(defun notmuch-wash-excerpt-citations (msg depth)
  "Excerpt citations and up to one signature."
  (goto-char (point-min))
  (beginning-of-line)
  (if (and (< (point) (point-max))
	   (re-search-forward notmuch-wash-original-regexp nil t))
      (let* ((msg-start (match-beginning 0))
	     (msg-end (point-max))
	     (msg-lines (count-lines msg-start msg-end)))
	(notmuch-wash-region-to-button
	 msg msg-start msg-end "original")))
  (while (and (< (point) (point-max))
	      (re-search-forward notmuch-wash-citation-regexp nil t))
    (let* ((cite-start (match-beginning 0))
	   (cite-end (match-end 0))
	   (cite-lines (count-lines cite-start cite-end)))
      (overlay-put (make-overlay cite-start cite-end) 'face 'message-cited-text)
      (when (> cite-lines (+ notmuch-wash-citation-lines-prefix
			     notmuch-wash-citation-lines-suffix
			     1))
	(goto-char cite-start)
	(forward-line notmuch-wash-citation-lines-prefix)
	(let ((hidden-start (point-marker)))
	  (goto-char cite-end)
	  (forward-line (- notmuch-wash-citation-lines-suffix))
	  (notmuch-wash-region-to-button
	   msg hidden-start (point-marker)
	   "citation")))))
  (if (and (not (eobp))
	   (re-search-forward notmuch-wash-signature-regexp nil t))
      (let* ((sig-start (match-beginning 0))
	     (sig-end (match-end 0))
	     (sig-lines (count-lines sig-start (point-max))))
	(if (<= sig-lines notmuch-wash-signature-lines-max)
	    (let ((sig-start-marker (make-marker))
		  (sig-end-marker (make-marker)))
	      (set-marker sig-start-marker sig-start)
	      (set-marker sig-end-marker (point-max))
	      (overlay-put (make-overlay sig-start-marker sig-end-marker) 'face 'message-cited-text)
	      (notmuch-wash-region-to-button
	       msg sig-start-marker sig-end-marker
	       "signature"))))))

;;

(defun notmuch-wash-elide-blank-lines (msg depth)
  "Elide leading, trailing and successive blank lines."

  ;; Algorithm derived from `article-strip-multiple-blank-lines' in
  ;; `gnus-art.el'.

  ;; Make all blank lines empty.
  (goto-char (point-min))
  (while (re-search-forward "^[[:space:]\t]+$" nil t)
    (replace-match "" nil t))

  ;; Replace multiple empty lines with a single empty line.
  (goto-char (point-min))
  (while (re-search-forward "^\n\\(\n+\\)" nil t)
    (delete-region (match-beginning 1) (match-end 1)))

  ;; Remove a leading blank line.
  (goto-char (point-min))
  (if (looking-at "\n")
      (delete-region (match-beginning 0) (match-end 0)))

  ;; Remove a trailing blank line.
  (goto-char (point-max))
  (if (looking-at "\n")
      (delete-region (match-beginning 0) (match-end 0))))

;;

(defun notmuch-wash-tidy-citations (msg depth)
  "Improve the display of cited regions of a message.

Perform several transformations on the message body:

- Remove lines of repeated citation leaders with no other
  content,
- Remove citation leaders standing alone before a block of cited
  text,
- Remove citation trailers standing alone after a block of cited
  text."

  ;; Remove lines of repeated citation leaders with no other content.
  (goto-char (point-min))
  (while (re-search-forward "\\(^>[> ]*\n\\)\\{2,\\}" nil t)
    (replace-match "\\1"))

  ;; Remove citation leaders standing alone before a block of cited
  ;; text.
  (goto-char (point-min))
  (while (re-search-forward "\\(\n\\|^[^>].*\\)\n\\(^>[> ]*\n\\)" nil t)
    (replace-match "\\1\n"))

  ;; Remove citation trailers standing alone after a block of cited
  ;; text.
  (goto-char (point-min))
  (while (re-search-forward "\\(^>[> ]*\n\\)\\(^$\\|^[^>].*\\)" nil t)
    (replace-match "\\2")))

;;

(defun notmuch-wash-wrap-long-lines (msg depth)
  "Wrap long lines in the message.

If `notmuch-wash-wrap-lines-length' is a number, this will wrap
the message lines to the minimum of the width of the window or
its value. Otherwise, this function will wrap long lines in the
message at the window width. When doing so, citation leaders in
the wrapped text are maintained."

  (let* ((coolj-wrap-follows-window-size nil)
	 (limit (if (numberp notmuch-wash-wrap-lines-length)
		    (min notmuch-wash-wrap-lines-length
			 (window-width))
		  (window-width)))
	 (fill-column (- limit
			 depth
			 ;; 2 to avoid poor interaction with
			 ;; `word-wrap'.
			 2)))
    (coolj-wrap-region (point-min) (point-max))))

;;

(require 'diff-mode)

(defvar diff-file-header-re) ; From `diff-mode.el'.

(defun notmuch-wash-subject-to-filename (subject &optional maxlen)
  "Convert a mail SUBJECT into a filename.

The resulting filename is similar to the names generated by \"git
format-patch\", without the leading patch sequence number
\"0001-\" and \".patch\" extension. Any leading \"[PREFIX]\"
style strings are removed prior to conversion.

Optional argument MAXLEN is the maximum length of the resulting
filename, before trimming any trailing . and - characters."
  (let* ((s (replace-regexp-in-string "^ *\\(\\[[^]]*\\] *\\)*" "" subject))
	 (s (replace-regexp-in-string "[^A-Za-z0-9._]+" "-" s))
	 (s (replace-regexp-in-string "\\.+" "." s))
	 (s (if maxlen (substring s 0 (min (length s) maxlen)) s))
	 (s (replace-regexp-in-string "[.-]*$" "" s)))
    s))

(defun notmuch-wash-subject-to-patch-sequence-number (subject)
  "Convert a patch mail SUBJECT into a patch sequence number.

Return the patch sequence number N from the last \"[PATCH N/M]\"
style prefix in SUBJECT, or nil if such a prefix can't be found."
  (when (string-match
	 "^ *\\(\\[[^]]*\\] *\\)*\\[[^]]*?\\([0-9]+\\)/[0-9]+[^]]*\\].*"
	 subject)
      (string-to-number (substring subject (match-beginning 2) (match-end 2)))))

(defun notmuch-wash-subject-to-patch-filename (subject)
  "Convert a patch mail SUBJECT into a filename.

The resulting filename is similar to the names generated by \"git
format-patch\". If the patch mail was generated and sent using
\"git format-patch/send-email\", this should re-create the
original filename the sender had."
  (format "%04d-%s.patch"
	  (or (notmuch-wash-subject-to-patch-sequence-number subject) 1)
	  (notmuch-wash-subject-to-filename subject 52)))

(defun notmuch-wash-convert-inline-patch-to-part (msg depth)
  "Convert an inline patch into a fake 'text/x-diff' attachment.

Given that this function guesses whether a buffer includes a
patch and then guesses the extent of the patch, there is scope
for error."

  (goto-char (point-min))
  (when (re-search-forward diff-file-header-re nil t)
    (beginning-of-line -1)
    (let ((patch-start (point))
	  (patch-end (point-max))
	  part)
      (goto-char patch-start)
      (if (or
	   ;; Patch ends with signature.
	   (re-search-forward notmuch-wash-signature-regexp nil t)
	   ;; Patch ends with bugtraq comment.
	   (re-search-forward "^\\*\\*\\* " nil t))
	  (setq patch-end (match-beginning 0)))
      (save-restriction
	(narrow-to-region patch-start patch-end)
	(setq part (plist-put part :content-type "inline-patch-fake-part"))
	(setq part (plist-put part :content (buffer-string)))
	(setq part (plist-put part :id -1))
	(setq part (plist-put part :filename
			      (notmuch-wash-subject-to-patch-filename
			       (plist-get
				(plist-get msg :headers) :Subject))))
	(delete-region (point-min) (point-max))
	(notmuch-show-insert-bodypart nil part depth)))))

;;

;; Temporary workaround for Emacs bug #8721
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=8721

(defun notmuch-isearch-range-invisible (beg end)
  "Same as `isearch-range-invisible' but with fixed Emacs bug #8721."
  (when (/= beg end)
    ;; Check that invisibility runs up to END.
    (save-excursion
      (goto-char beg)
      (let (;; can-be-opened keeps track if we can open some overlays.
	    (can-be-opened (eq search-invisible 'open))
	    ;; the list of overlays that could be opened
	    (crt-overlays nil))
	(when (and can-be-opened isearch-hide-immediately)
	  (isearch-close-unnecessary-overlays beg end))
	;; If the following character is currently invisible,
	;; skip all characters with that same `invisible' property value.
	;; Do that over and over.
	(while (and (< (point) end) (invisible-p (point)))
	  (if (invisible-p (get-text-property (point) 'invisible))
	      (progn
		(goto-char (next-single-property-change (point) 'invisible
							nil end))
		;; if text is hidden by an `invisible' text property
		;; we cannot open it at all.
		(setq can-be-opened nil))
	    (when can-be-opened
	      (let ((overlays (overlays-at (point)))
		    ov-list
		    o
		    invis-prop)
		(while overlays
		  (setq o (car overlays)
			invis-prop (overlay-get o 'invisible))
		  (if (invisible-p invis-prop)
		      (if (overlay-get o 'isearch-open-invisible)
			  (setq ov-list (cons o ov-list))
			;; We found one overlay that cannot be
			;; opened, that means the whole chunk
			;; cannot be opened.
			(setq can-be-opened nil)))
		  (setq overlays (cdr overlays)))
		(if can-be-opened
		    ;; It makes sense to append to the open
		    ;; overlays list only if we know that this is
		    ;; t.
		    (setq crt-overlays (append ov-list crt-overlays)))))
	    (goto-char (next-overlay-change (point)))))
	;; See if invisibility reaches up thru END.
	(if (>= (point) end)
	    (if (and can-be-opened (consp crt-overlays))
		(progn
		  (setq isearch-opened-overlays
			(append isearch-opened-overlays crt-overlays))
		  (mapc 'isearch-open-overlay-temporary crt-overlays)
		  nil)
	      (setq isearch-hidden t)))))))

(defadvice isearch-range-invisible (around notmuch-isearch-range-invisible-advice activate)
  "Call `notmuch-isearch-range-invisible' instead of the original
`isearch-range-invisible' when in `notmuch-show-mode' mode."
  (if (eq major-mode 'notmuch-show-mode)
      (setq ad-return-value (notmuch-isearch-range-invisible beg end))
    ad-do-it))

;;

(provide 'notmuch-wash)
