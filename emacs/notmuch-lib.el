;; notmuch-lib.el --- common variables, functions and function declarations
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

;; This is an part of an emacs-based interface to the notmuch mail system.

(defvar notmuch-command "notmuch"
  "Command to run the notmuch binary.")

(declare-function notmuch-toggle-invisible-action "notmuch" (cite-button))

(define-button-type 'notmuch-button-invisibility-toggle-type
  'action 'notmuch-toggle-invisible-action
  'follow-link t
  'face 'font-lock-comment-face)

(define-button-type 'notmuch-button-headers-toggle-type
  'help-echo "mouse-1, RET: Show headers"
  :supertype 'notmuch-button-invisibility-toggle-type)

;; XXX: This should be a generic function in emacs somewhere, not
;; here.
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

(provide 'notmuch-lib)
