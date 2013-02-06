;; notmuch-tag.el --- tag messages within emacs
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

(eval-when-compile (require 'cl))
(require 'crm)
(require 'notmuch-lib)

(defcustom notmuch-before-tag-hook nil
  "Hooks that are run before tags of a message are modified.

'tags' will contain the tags that are about to be added or removed as
a list of strings of the form \"+TAG\" or \"-TAG\".
'query' will be a string containing the search query that determines
the messages that are about to be tagged"

  :type 'hook
  :options '(notmuch-hl-line-mode)
  :group 'notmuch-hooks)

(defcustom notmuch-after-tag-hook nil
  "Hooks that are run after tags of a message are modified.

'tags' will contain the tags that were added or removed as
a list of strings of the form \"+TAG\" or \"-TAG\".
'query' will be a string containing the search query that determines
the messages that were tagged"
  :type 'hook
  :options '(notmuch-hl-line-mode)
  :group 'notmuch-hooks)

(defvar notmuch-select-tag-history nil
  "Variable to store minibuffer history for
`notmuch-select-tag-with-completion' function.")

(defvar notmuch-read-tag-changes-history nil
  "Variable to store minibuffer history for
`notmuch-read-tag-changes' function.")

(defun notmuch-tag-completions (&optional search-terms)
  (if (null search-terms)
      (setq search-terms (list "*")))
  (split-string
   (with-output-to-string
     (with-current-buffer standard-output
       (apply 'call-process notmuch-command nil t
	      nil "search" "--output=tags" "--exclude=false" search-terms)))
   "\n+" t))

(defun notmuch-select-tag-with-completion (prompt &rest search-terms)
  (let ((tag-list (notmuch-tag-completions search-terms)))
    (completing-read prompt tag-list nil nil nil 'notmuch-select-tag-history)))

(defun notmuch-read-tag-changes (&optional initial-input &rest search-terms)
  (let* ((all-tag-list (notmuch-tag-completions))
	 (add-tag-list (mapcar (apply-partially 'concat "+") all-tag-list))
	 (remove-tag-list (mapcar (apply-partially 'concat "-")
				  (if (null search-terms)
				      all-tag-list
				    (notmuch-tag-completions search-terms))))
	 (tag-list (append add-tag-list remove-tag-list))
	 (crm-separator " ")
	 ;; By default, space is bound to "complete word" function.
	 ;; Re-bind it to insert a space instead.  Note that <tab>
	 ;; still does the completion.
	 (crm-local-completion-map
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-parent map crm-local-completion-map)
	    (define-key map " " 'self-insert-command)
	    map)))
    (delete "" (completing-read-multiple "Tags (+add -drop): "
		tag-list nil nil initial-input
		'notmuch-read-tag-changes-history))))

(defun notmuch-update-tags (tags tag-changes)
  "Return a copy of TAGS with additions and removals from TAG-CHANGES.

TAG-CHANGES must be a list of tags names, each prefixed with
either a \"+\" to indicate the tag should be added to TAGS if not
present or a \"-\" to indicate that the tag should be removed
from TAGS if present."
  (let ((result-tags (copy-sequence tags)))
    (dolist (tag-change tag-changes)
      (let ((op (string-to-char tag-change))
	    (tag (unless (string= tag-change "") (substring tag-change 1))))
	(case op
	  (?+ (unless (member tag result-tags)
		(push tag result-tags)))
	  (?- (setq result-tags (delete tag result-tags)))
	  (otherwise
	   (error "Changed tag must be of the form `+this_tag' or `-that_tag'")))))
    (sort result-tags 'string<)))

(defun notmuch-tag (query &optional tag-changes)
  "Add/remove tags in TAG-CHANGES to messages matching QUERY.

QUERY should be a string containing the search-terms.
TAG-CHANGES can take multiple forms.  If TAG-CHANGES is a list of
strings of the form \"+tag\" or \"-tag\" then those are the tag
changes applied.  If TAG-CHANGES is a string then it is
interpreted as a single tag change.  If TAG-CHANGES is the string
\"-\" or \"+\", or null, then the user is prompted to enter the
tag changes.

Note: Other code should always use this function alter tags of
messages instead of running (notmuch-call-notmuch-process \"tag\" ..)
directly, so that hooks specified in notmuch-before-tag-hook and
notmuch-after-tag-hook will be run."
  ;; Perform some validation
  (if (string-or-null-p tag-changes)
      (if (or (string= tag-changes "-") (string= tag-changes "+") (null tag-changes))
	  (setq tag-changes (notmuch-read-tag-changes tag-changes query))
	(setq tag-changes (list tag-changes))))
  (mapc (lambda (tag-change)
	  (unless (string-match-p "^[-+]\\S-+$" tag-change)
	    (error "Tag must be of the form `+this_tag' or `-that_tag'")))
	tag-changes)
  (unless (null tag-changes)
    (run-hooks 'notmuch-before-tag-hook)
    (apply 'notmuch-call-notmuch-process "tag"
	   (append tag-changes (list "--" query)))
    (run-hooks 'notmuch-after-tag-hook))
  ;; in all cases we return tag-changes as a list
  tag-changes)

(defun notmuch-tag-change-list (tags &optional reverse)
  "Convert TAGS into a list of tag changes.

Add a \"+\" prefix to any tag in TAGS list that doesn't already
begin with a \"+\" or a \"-\". If REVERSE is non-nil, replace all
\"+\" prefixes with \"-\" and vice versa in the result."
  (mapcar (lambda (str)
	    (let ((s (if (string-match "^[+-]" str) str (concat "+" str))))
	      (if reverse
		  (concat (if (= (string-to-char s) ?-) "+" "-")
			  (substring s 1))
		s)))
	  tags))


;;

(provide 'notmuch-tag)
