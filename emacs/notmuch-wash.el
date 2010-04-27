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

(defvar notmuch-wash-signature-button-format
  "[ %d-line signature. Click/Enter to toggle visibility. ]"
  "String used to construct button text for hidden signatures.
Can use up to one integer format parameter, i.e. %d")

(defvar notmuch-wash-citation-button-format
  "[ %d more citation lines. Click/Enter to toggle visibility. ]"
  "String used to construct button text for hidden citations.
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

(defun notmuch-wash-toggle-invisible-action (cite-button)
  (let ((invis-spec (button-get cite-button 'invisibility-spec)))
    (if (invisible-p invis-spec)
	(remove-from-invisibility-spec invis-spec)
      (add-to-invisibility-spec invis-spec)))
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

(defun notmuch-wash-region-isearch-show (overlay)
  (remove-from-invisibility-spec (overlay-get overlay 'invisible)))

(defun notmuch-wash-region-to-button (beg end type prefix button-text)
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
	(button-type (intern-soft (concat "notmuch-wash-button-"
					  type "-toggle-type"))))
    (add-to-invisibility-spec invis-spec)
    (overlay-put overlay 'invisible invis-spec)
    (overlay-put overlay 'isearch-open-invisible #'notmuch-wash-region-isearch-show)
    (goto-char (1+ end))
    (save-excursion
      (goto-char (1- beg))
      (insert prefix)
      (insert-button button-text
		     'invisibility-spec invis-spec
		     :type button-type))))

(defun notmuch-wash-excerpt-citations (depth)
  "Excerpt citations and up to one signature."
  (goto-char (point-min))
  (beginning-of-line)
  (while (and (< (point) (point-max))
	      (re-search-forward notmuch-wash-citation-regexp nil t))
    (let* ((cite-start (match-beginning 0))
	   (cite-end (match-end 0))
	   (cite-lines (count-lines cite-start cite-end)))
      (overlay-put (make-overlay cite-start cite-end) 'face 'message-cited-text-face)
      (when (> cite-lines (+ notmuch-wash-citation-lines-prefix
			     notmuch-wash-citation-lines-suffix
			     1))
	(goto-char cite-start)
	(forward-line notmuch-wash-citation-lines-prefix)
	(let ((hidden-start (point-marker)))
	  (goto-char cite-end)
	  (forward-line (- notmuch-wash-citation-lines-suffix))
	  (notmuch-wash-region-to-button
	   hidden-start (point-marker)
	   "citation" "\n"
	   (format notmuch-wash-citation-button-format
		   (- cite-lines
		      notmuch-wash-citation-lines-prefix
		      notmuch-wash-citation-lines-suffix)))))))
  (if (and (not (eobp))
	   (re-search-forward notmuch-wash-signature-regexp nil t))
      (let* ((sig-start (match-beginning 0))
	     (sig-end (match-end 0))
	     (sig-lines (1- (count-lines sig-start (point-max)))))
	(if (<= sig-lines notmuch-wash-signature-lines-max)
	    (let ((sig-start-marker (make-marker))
		  (sig-end-marker (make-marker)))
	      (set-marker sig-start-marker sig-start)
	      (set-marker sig-end-marker (point-max))
	      (overlay-put (make-overlay sig-start-marker sig-end-marker) 'face 'message-cited-text-face)
	      (notmuch-wash-region-to-button
	       sig-start-marker sig-end-marker
	       "signature" "\n"
	       (format notmuch-wash-signature-button-format sig-lines)))))))

;;

(defun notmuch-wash-elide-blank-lines (depth)
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

(defun notmuch-wash-tidy-citations (depth)
  "Improve the display of cited regions of a message.

Perform four transformations on the message body:

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

(defun notmuch-wash-wrap-long-lines (depth)
  "Wrap any long lines in the message to the width of the window.

When doing so, maintaining citation leaders in the wrapped text."

  (let ((coolj-wrap-follows-window-size nil)
	(fill-column (- (window-width)
			depth
			;; 2 to avoid poor interaction with
			;; `word-wrap'.
			2)))
    (coolj-wrap-region (point-min) (point-max))))

;;

(require 'diff-mode)

(defvar diff-file-header-re) ; From `diff-mode.el'.

(defun notmuch-wash-convert-inline-patch-to-part (depth)
  "Convert an inline patch into a fake 'text/x-diff' attachment.

Given that this function guesses whether a buffer includes a
patch and then guesses the extent of the patch, there is scope
for error."

  (goto-char (point-min))
  (if (re-search-forward diff-file-header-re nil t)
      (progn
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
	    (setq part (plist-put part :content-type "text/x-diff"))
	    (setq part (plist-put part :content (buffer-string)))
	    (setq part (plist-put part :id -1))
	    (setq part (plist-put part :filename "inline patch"))
	    (delete-region (point-min) (point-max))
	    (notmuch-show-insert-bodypart nil part depth))))))

;;

(provide 'notmuch-wash)
