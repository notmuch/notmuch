;; notmuch-message.el --- message-mode functions specific to notmuch
;;
;; Copyright © Jesse Rosenthal
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
;; Authors: Jesse Rosenthal <jrosenthal@jhu.edu>

(require 'message)
(require 'notmuch-mua)

(defcustom notmuch-message-replied-tags '("replied")
  "Tags to be automatically added to or removed from a message when it is replied to.
Any tag in the list will be added to a replied message or,
if it is prefaced with a \"-\", removed.

For example, if you wanted to add a \"replied\" tag and remove
the \"inbox\" and \"todo\", you would set
    (\"replied\" \"-inbox\" \"-todo\"\)"
  :type 'list
  :group 'notmuch)

(defun notmuch-message-mark-replied ()
  ;; get the in-reply-to header and parse it for the message id.
  (let ((rep (mail-header-parse-addresses (message-field-value "In-Reply-To"))))
    (when (and notmuch-message-replied-tags rep)
      ;; add a "+" to any tag that is doesn't already begin with a "+"
      ;; or "-"
      (let ((tags (mapcar '(lambda (str)
			     (if (not (string-match "^[+-]" str))
				 (concat "+" str)
			       str))
			  notmuch-message-replied-tags)))
	(apply 'notmuch-call-notmuch-process "tag"
	       (append tags (list (concat "id:" (car (car rep)))) nil))))))

(add-hook 'message-send-hook 'notmuch-message-mark-replied)

(provide 'notmuch-message)
