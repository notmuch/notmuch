;; notmuch-wash.el --- cleaning up message bodies
;;
;; Copyright © Carl Worth
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
    (goto-char (1+ end))
    (save-excursion
      (goto-char (1- beg))
      (insert prefix)
      (insert-button button-text
		     'invisibility-spec invis-spec
		     :type button-type))))

(defun notmuch-wash-text/plain-citations (depth)
  "Markup citations, and up to one signature in the buffer."
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

(provide 'notmuch-wash)
