;;; notmuch-message.el --- message-mode functions specific to notmuch  -*- lexical-binding: t -*-
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
;; along with Notmuch.  If not, see <https://www.gnu.org/licenses/>.
;;
;; Authors: Jesse Rosenthal <jrosenthal@jhu.edu>

;;; Code:

(require 'message)
(require 'notmuch-tag)

(defcustom notmuch-message-replied-tags '("+replied")
  "List of tag changes to apply to a message when it has been replied to.

Tags starting with \"+\" (or not starting with either \"+\" or
\"-\") in the list will be added, and tags starting with \"-\"
will be removed from the message being replied to.

For example, if you wanted to add a \"replied\" tag and remove
the \"inbox\" and \"todo\" tags, you would set:
    (\"+replied\" \"-inbox\" \"-todo\")"
  :type '(repeat string)
  :group 'notmuch-send)

(defcustom notmuch-message-forwarded-tags '("+forwarded")
  "List of tag changes to apply to a message when it has been forwarded.

Tags starting with \"+\" (or not starting with either \"+\" or
\"-\") in the list will be added, and tags starting with \"-\"
will be removed from the message being forwarded.

For example, if you wanted to add a \"forwarded\" tag and remove
the \"inbox\" tag, you would set:
    (\"+forwarded\" \"-inbox\")"
  :type '(repeat string)
  :group 'notmuch-send)

(defvar-local notmuch-message-queued-tag-changes nil
  "List of tag changes to be applied when sending a message.

A list of queries and tag changes that are to be applied to them
when the message that was composed in the current buffer is being
send.  Each item in this list is a list of strings, where the
first is a notmuch query and the rest are the tag changes to be
applied to the matching messages.")

(defun notmuch-message-apply-queued-tag-changes ()
  ;; Apply the tag changes queued in the buffer-local variable
  ;; notmuch-message-queued-tag-changes.
  (pcase-dolist (`(,query . ,tags) notmuch-message-queued-tag-changes)
    (notmuch-tag query tags)))

(add-hook 'message-send-hook 'notmuch-message-apply-queued-tag-changes)

(provide 'notmuch-message)

;;; notmuch-message.el ends here
