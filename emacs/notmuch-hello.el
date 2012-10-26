;; notmuch-hello.el --- welcome to notmuch, a frontend
;;
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
;; Authors: David Edmondson <dme@dme.org>

(eval-when-compile (require 'cl))
(require 'widget)
(require 'wid-edit) ; For `widget-forward'.

(require 'notmuch-lib)
(require 'notmuch-mua)

(declare-function notmuch-search "notmuch" (query &optional oldest-first target-thread target-line continuation))
(declare-function notmuch-poll "notmuch" ())

(defcustom notmuch-hello-recent-searches-max 10
  "The number of recent searches to display."
  :type 'integer
  :group 'notmuch-hello)

(defcustom notmuch-show-empty-saved-searches nil
  "Should saved searches with no messages be listed?"
  :type 'boolean
  :group 'notmuch-hello)

(defun notmuch-sort-saved-searches (alist)
  "Generate an alphabetically sorted saved searches alist."
  (sort (copy-sequence alist) (lambda (a b) (string< (car a) (car b)))))

(defcustom notmuch-saved-search-sort-function nil
  "Function used to sort the saved searches for the notmuch-hello view.

This variable controls how saved searches should be sorted. No
sorting (nil) displays the saved searches in the order they are
stored in `notmuch-saved-searches'. Sort alphabetically sorts the
saved searches in alphabetical order. Custom sort function should
be a function or a lambda expression that takes the saved
searches alist as a parameter, and returns a new saved searches
alist to be used."
  :type '(choice (const :tag "No sorting" nil)
		 (const :tag "Sort alphabetically" notmuch-sort-saved-searches)
		 (function :tag "Custom sort function"
			   :value notmuch-sort-saved-searches))
  :group 'notmuch-hello)

(defvar notmuch-hello-indent 4
  "How much to indent non-headers.")

(defcustom notmuch-show-logo t
  "Should the notmuch logo be shown?"
  :type 'boolean
  :group 'notmuch-hello)

(defcustom notmuch-show-all-tags-list nil
  "Should all tags be shown in the notmuch-hello view?"
  :type 'boolean
  :group 'notmuch-hello)

(defcustom notmuch-hello-tag-list-make-query nil
  "Function or string to generate queries for the all tags list.

This variable controls which query results are shown for each tag
in the \"all tags\" list. If nil, it will use all messages with
that tag. If this is set to a string, it is used as a filter for
messages having that tag (equivalent to \"tag:TAG and (THIS-VARIABLE)\").
Finally this can be a function that will be called for each tag and
should return a filter for that tag, or nil to hide the tag."
  :type '(choice (const :tag "All messages" nil)
		 (const :tag "Unread messages" "tag:unread")
		 (string :tag "Custom filter"
			 :value "tag:unread")
		 (function :tag "Custom filter function"))
  :group 'notmuch-hello)

(defcustom notmuch-hello-hide-tags nil
  "List of tags to be hidden in the \"all tags\"-section."
  :type '(repeat string)
  :group 'notmuch-hello)

(defface notmuch-hello-logo-background
  '((((class color)
      (background dark))
     (:background "#5f5f5f"))
    (((class color)
      (background light))
     (:background "white")))
  "Background colour for the notmuch logo."
  :group 'notmuch-hello
  :group 'notmuch-faces)

(defcustom notmuch-column-control t
  "Controls the number of columns for saved searches/tags in notmuch view.

This variable has three potential sets of values:

- t: automatically calculate the number of columns possible based
  on the tags to be shown and the window width,
- an integer: a lower bound on the number of characters that will
  be used to display each column,
- a float: a fraction of the window width that is the lower bound
  on the number of characters that should be used for each
  column.

So:
- if you would like two columns of tags, set this to 0.5.
- if you would like a single column of tags, set this to 1.0.
- if you would like tags to be 30 characters wide, set this to
  30.
- if you don't want to worry about all of this nonsense, leave
  this set to `t'."
  :type '(choice
	  (const :tag "Automatically calculated" t)
	  (integer :tag "Number of characters")
	  (float :tag "Fraction of window"))
  :group 'notmuch-hello)

(defcustom notmuch-hello-thousands-separator " "
  "The string used as a thousands separator.

Typically \",\" in the US and UK and \".\" or \" \" in Europe.
The latter is recommended in the SI/ISO 31-0 standard and by the
International Bureau of Weights and Measures."
  :type 'string
  :group 'notmuch-hello)

(defcustom notmuch-hello-mode-hook nil
  "Functions called after entering `notmuch-hello-mode'."
  :type 'hook
  :group 'notmuch-hello
  :group 'notmuch-hooks)

(defcustom notmuch-hello-refresh-hook nil
  "Functions called after updating a `notmuch-hello' buffer."
  :type 'hook
  :group 'notmuch-hello
  :group 'notmuch-hooks)

(defvar notmuch-hello-url "http://notmuchmail.org"
  "The `notmuch' web site.")

(defvar notmuch-hello-custom-section-options
  '((:filter (string :tag "Filter for each tag"))
    (:filter-count (string :tag "Different filter to generate message counts"))
    (:initially-hidden (const :tag "Hide this section on startup" t))
    (:show-empty-searches (const :tag "Show queries with no matching messages" t))
    (:hide-if-empty (const :tag "Hide this section if all queries are empty
\(and not shown by show-empty-searches)" t)))
  "Various customization-options for notmuch-hello-tags/query-section.")

(define-widget 'notmuch-hello-tags-section 'lazy
  "Customize-type for notmuch-hello tag-list sections."
  :tag "Customized tag-list section (see docstring for details)"
  :type
  `(list :tag ""
	 (const :tag "" notmuch-hello-insert-tags-section)
	 (string :tag "Title for this section")
	 (plist
	  :inline t
	  :options
	  ,(append notmuch-hello-custom-section-options
		   '((:hide-tags (repeat :tag "Tags that will be hidden"
					 string)))))))

(define-widget 'notmuch-hello-query-section 'lazy
  "Customize-type for custom saved-search-like sections"
  :tag "Customized queries section (see docstring for details)"
  :type
  `(list :tag ""
	 (const :tag "" notmuch-hello-insert-searches)
	 (string :tag "Title for this section")
	 (repeat :tag "Queries"
		 (cons (string :tag "Name") (string :tag "Query")))
	 (plist :inline t :options ,notmuch-hello-custom-section-options)))

(defcustom notmuch-hello-sections
  (list #'notmuch-hello-insert-header
	#'notmuch-hello-insert-saved-searches
	#'notmuch-hello-insert-search
	#'notmuch-hello-insert-recent-searches
	#'notmuch-hello-insert-alltags
	#'notmuch-hello-insert-footer)
  "Sections for notmuch-hello.

The list contains functions which are used to construct sections in
notmuch-hello buffer.  When notmuch-hello buffer is constructed,
these functions are run in the order they appear in this list.  Each
function produces a section simply by adding content to the current
buffer.  A section should not end with an empty line, because a
newline will be inserted after each section by `notmuch-hello'.

Each function should take no arguments. The return value is
ignored.

For convenience an element can also be a list of the form (FUNC ARG1
ARG2 .. ARGN) in which case FUNC will be applied to the rest of the
list.

A \"Customized tag-list section\" item in the customize-interface
displays a list of all tags, optionally hiding some of them. It
is also possible to filter the list of messages matching each tag
by an additional filter query. Similarly, the count of messages
displayed next to the buttons can be generated by applying a
different filter to the tag query. These filters are also
supported for \"Customized queries section\" items."
  :group 'notmuch-hello
  :type
  '(repeat
    (choice (function-item notmuch-hello-insert-header)
	    (function-item notmuch-hello-insert-saved-searches)
	    (function-item notmuch-hello-insert-search)
	    (function-item notmuch-hello-insert-recent-searches)
	    (function-item notmuch-hello-insert-alltags)
	    (function-item notmuch-hello-insert-footer)
	    (function-item notmuch-hello-insert-inbox)
	    notmuch-hello-tags-section
	    notmuch-hello-query-section
	    (function :tag "Custom section"))))

(defvar notmuch-hello-hidden-sections nil
  "List of sections titles whose contents are hidden")

(defvar notmuch-hello-first-run t
  "True if `notmuch-hello' is run for the first time, set to nil
afterwards.")

(defun notmuch-hello-nice-number (n)
  (let (result)
    (while (> n 0)
      (push (% n 1000) result)
      (setq n (/ n 1000)))
    (setq result (or result '(0)))
    (apply #'concat
     (number-to-string (car result))
     (mapcar (lambda (elem)
	      (format "%s%03d" notmuch-hello-thousands-separator elem))
	     (cdr result)))))

(defun notmuch-hello-trim (search)
  "Trim whitespace."
  (if (string-match "^[[:space:]]*\\(.*[^[:space:]]\\)[[:space:]]*$" search)
      (match-string 1 search)
    search))

(defun notmuch-hello-search (&optional search)
  (interactive)
  (unless (null search)
    (setq search (notmuch-hello-trim search))
    (let ((history-delete-duplicates t))
      (add-to-history 'notmuch-search-history search)))
  (notmuch-search search notmuch-search-oldest-first nil nil
		  #'notmuch-hello-search-continuation))

(defun notmuch-hello-add-saved-search (widget)
  (interactive)
  (let ((search (widget-value
		 (symbol-value
		  (widget-get widget :notmuch-saved-search-widget))))
	(name (completing-read "Name for saved search: "
			       notmuch-saved-searches)))
    ;; If an existing saved search with this name exists, remove it.
    (setq notmuch-saved-searches
	  (loop for elem in notmuch-saved-searches
		if (not (equal name
			       (car elem)))
		collect elem))
    ;; Add the new one.
    (customize-save-variable 'notmuch-saved-searches
			     (add-to-list 'notmuch-saved-searches
					  (cons name search) t))
    (message "Saved '%s' as '%s'." search name)
    (notmuch-hello-update)))

(defun notmuch-hello-longest-label (searches-alist)
  (or (loop for elem in searches-alist
	    maximize (length (car elem)))
      0))

(defun notmuch-hello-reflect-generate-row (ncols nrows row list)
  (let ((len (length list)))
    (loop for col from 0 to (- ncols 1)
	  collect (let ((offset (+ (* nrows col) row)))
		    (if (< offset len)
			(nth offset list)
		      ;; Don't forget to insert an empty slot in the
		      ;; output matrix if there is no corresponding
		      ;; value in the input matrix.
		      nil)))))

(defun notmuch-hello-reflect (list ncols)
  "Reflect a `ncols' wide matrix represented by `list' along the
diagonal."
  ;; Not very lispy...
  (let ((nrows (ceiling (length list) ncols)))
    (loop for row from 0 to (- nrows 1)
	  append (notmuch-hello-reflect-generate-row ncols nrows row list))))

(defun notmuch-hello-widget-search (widget &rest ignore)
  (notmuch-search (widget-get widget
			      :notmuch-search-terms)
		  notmuch-search-oldest-first
		  nil nil #'notmuch-hello-search-continuation))

(defun notmuch-saved-search-count (search)
  (car (process-lines notmuch-command "count" search)))

(defun notmuch-hello-tags-per-line (widest)
  "Determine how many tags to show per line and how wide they
should be. Returns a cons cell `(tags-per-line width)'."
  (let ((tags-per-line
	 (cond
	  ((integerp notmuch-column-control)
	   (max 1
		(/ (- (window-width) notmuch-hello-indent)
		   ;; Count is 9 wide (8 digits plus space), 1 for the space
		   ;; after the name.
		   (+ 9 1 (max notmuch-column-control widest)))))

	  ((floatp notmuch-column-control)
	   (let* ((available-width (- (window-width) notmuch-hello-indent))
		  (proposed-width (max (* available-width notmuch-column-control) widest)))
	     (floor available-width proposed-width)))

	  (t
	   (max 1
		(/ (- (window-width) notmuch-hello-indent)
		   ;; Count is 9 wide (8 digits plus space), 1 for the space
		   ;; after the name.
		   (+ 9 1 widest)))))))

    (cons tags-per-line (/ (max 1
				(- (window-width) notmuch-hello-indent
				   ;; Count is 9 wide (8 digits plus
				   ;; space), 1 for the space after the
				   ;; name.
				   (* tags-per-line (+ 9 1))))
			   tags-per-line))))

(defun notmuch-hello-filtered-query (query filter)
  "Constructs a query to search all messages matching QUERY and FILTER.

If FILTER is a string, it is directly used in the returned query.

If FILTER is a function, it is called with QUERY as a parameter and
the string it returns is used as the query. If nil is returned,
the entry is hidden.

Otherwise, FILTER is ignored.
"
  (cond
   ((functionp filter) (funcall filter query))
   ((stringp filter)
    (concat "(" query ") and (" filter ")"))
   (t query)))

(defun notmuch-hello-query-counts (query-alist &rest options)
  "Compute list of counts of matched messages from QUERY-ALIST.

QUERY-ALIST must be a list containing elements of the form (NAME . QUERY)
or (NAME QUERY COUNT-QUERY). If the latter form is used,
COUNT-QUERY specifies an alternate query to be used to generate
the count for the associated query.

The result is the list of elements of the form (NAME QUERY COUNT).

The values :show-empty-searches, :filter and :filter-count from
options will be handled as specified for
`notmuch-hello-insert-searches'."
  (notmuch-remove-if-not
   #'identity
   (mapcar
    (lambda (elem)
      (let* ((name (car elem))
	     (query-and-count (if (consp (cdr elem))
				  ;; do we have a different query for the message count?
				  (cons (second elem) (third elem))
				(cons (cdr elem) (cdr elem))))
	     (message-count
	      (string-to-number
	       (notmuch-saved-search-count
		(notmuch-hello-filtered-query (cdr query-and-count)
					      (or (plist-get options :filter-count)
						 (plist-get options :filter)))))))
	(and (or (plist-get options :show-empty-searches) (> message-count 0))
	     (list name (notmuch-hello-filtered-query
			 (car query-and-count) (plist-get options :filter))
		   message-count))))
    query-alist)))

(defun notmuch-hello-insert-buttons (searches)
  "Insert buttons for SEARCHES.

SEARCHES must be a list containing lists of the form (NAME QUERY COUNT), where
QUERY is the query to start when the button for the corresponding entry is
activated. COUNT should be the number of messages matching the query.
Such a list can be computed with `notmuch-hello-query-counts'."
  (let* ((widest (notmuch-hello-longest-label searches))
	 (tags-and-width (notmuch-hello-tags-per-line widest))
	 (tags-per-line (car tags-and-width))
	 (column-width (cdr tags-and-width))
	 (column-indent 0)
	 (count 0)
	 (reordered-list (notmuch-hello-reflect searches tags-per-line))
	 ;; Hack the display of the buttons used.
	 (widget-push-button-prefix "")
	 (widget-push-button-suffix ""))
    ;; dme: It feels as though there should be a better way to
    ;; implement this loop than using an incrementing counter.
    (mapc (lambda (elem)
	    ;; (not elem) indicates an empty slot in the matrix.
	    (when elem
	      (if (> column-indent 0)
		  (widget-insert (make-string column-indent ? )))
	      (let* ((name (first elem))
		     (query (second elem))
		     (msg-count (third elem)))
		(widget-insert (format "%8s "
				       (notmuch-hello-nice-number msg-count)))
		(widget-create 'push-button
			       :notify #'notmuch-hello-widget-search
			       :notmuch-search-terms query
			       name)
		(setq column-indent
		      (1+ (max 0 (- column-width (length name)))))))
	    (setq count (1+ count))
	    (when (eq (% count tags-per-line) 0)
	      (setq column-indent 0)
	      (widget-insert "\n")))
	  reordered-list)

    ;; If the last line was not full (and hence did not include a
    ;; carriage return), insert one now.
    (unless (eq (% count tags-per-line) 0)
      (widget-insert "\n"))))

(defimage notmuch-hello-logo ((:type png :file "notmuch-logo.png")))

(defun notmuch-hello-search-continuation()
  (notmuch-hello-update t))

(defun notmuch-hello-update (&optional no-display)
  "Update the current notmuch view."
  ;; Lazy - rebuild everything.
  (interactive)
  (notmuch-hello no-display))

(defun notmuch-hello-poll-and-update ()
  "Invoke `notmuch-poll' to import mail, then refresh the current view."
  (interactive)
  (notmuch-poll)
  (notmuch-hello-update))


(defvar notmuch-hello-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "v" (lambda () "Display the notmuch version" (interactive)
			  (message "notmuch version %s" (notmuch-version))))
    (define-key map "?" 'notmuch-help)
    (define-key map "q" 'notmuch-kill-this-buffer)
    (define-key map "=" 'notmuch-hello-update)
    (define-key map "G" 'notmuch-hello-poll-and-update)
    (define-key map (kbd "<C-tab>") 'widget-backward)
    (define-key map "m" 'notmuch-mua-new-mail)
    (define-key map "s" 'notmuch-hello-search)
    map)
  "Keymap for \"notmuch hello\" buffers.")
(fset 'notmuch-hello-mode-map notmuch-hello-mode-map)

(defun notmuch-hello-mode ()
 "Major mode for convenient notmuch navigation. This is your entry portal into notmuch.

Complete list of currently available key bindings:

\\{notmuch-hello-mode-map}"
 (interactive)
 (kill-all-local-variables)
 (use-local-map notmuch-hello-mode-map)
 (setq major-mode 'notmuch-hello-mode
	mode-name "notmuch-hello")
 (run-mode-hooks 'notmuch-hello-mode-hook)
 ;;(setq buffer-read-only t)
)

(defun notmuch-hello-generate-tag-alist (&optional hide-tags)
  "Return an alist from tags to queries to display in the all-tags section."
  (mapcar (lambda (tag)
	    (cons tag (concat "tag:" (notmuch-escape-boolean-term tag))))
	  (notmuch-remove-if-not
	   (lambda (tag)
	     (not (member tag hide-tags)))
	   (process-lines notmuch-command "search-tags"))))

(defun notmuch-hello-insert-header ()
  "Insert the default notmuch-hello header."
  (when notmuch-show-logo
    (let ((image notmuch-hello-logo))
      ;; The notmuch logo uses transparency. That can display poorly
      ;; when inserting the image into an emacs buffer (black logo on
      ;; a black background), so force the background colour of the
      ;; image. We use a face to represent the colour so that
      ;; `defface' can be used to declare the different possible
      ;; colours, which depend on whether the frame has a light or
      ;; dark background.
      (setq image (cons 'image
			(append (cdr image)
				(list :background (face-background 'notmuch-hello-logo-background)))))
      (insert-image image))
    (widget-insert "  "))

  (widget-insert "Welcome to ")
  ;; Hack the display of the links used.
  (let ((widget-link-prefix "")
	(widget-link-suffix ""))
    (widget-create 'link
		   :notify (lambda (&rest ignore)
			     (browse-url notmuch-hello-url))
		   :help-echo "Visit the notmuch website."
		   "notmuch")
    (widget-insert ". ")
    (widget-insert "You have ")
    (widget-create 'link
		   :notify (lambda (&rest ignore)
			     (notmuch-hello-update))
		   :help-echo "Refresh"
		   (notmuch-hello-nice-number
		    (string-to-number (car (process-lines notmuch-command "count")))))
    (widget-insert " messages.\n")))


(defun notmuch-hello-insert-saved-searches ()
  "Insert the saved-searches section."
  (let ((searches (notmuch-hello-query-counts
		   (if notmuch-saved-search-sort-function
		       (funcall notmuch-saved-search-sort-function
				notmuch-saved-searches)
		     notmuch-saved-searches)
		   :show-empty-searches notmuch-show-empty-saved-searches)))
    (when searches
      (widget-insert "Saved searches: ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (customize-variable 'notmuch-saved-searches))
		     "edit")
      (widget-insert "\n\n")
      (let ((start (point)))
	(notmuch-hello-insert-buttons searches)
	(indent-rigidly start (point) notmuch-hello-indent)))))

(defun notmuch-hello-insert-search ()
  "Insert a search widget."
  (widget-insert "Search: ")
  (widget-create 'editable-field
		 ;; Leave some space at the start and end of the
		 ;; search boxes.
		 :size (max 8 (- (window-width) notmuch-hello-indent
				 (length "Search: ")))
		 :action (lambda (widget &rest ignore)
			   (notmuch-hello-search (widget-value widget))))
  ;; Add an invisible dot to make `widget-end-of-line' ignore
  ;; trailing spaces in the search widget field.  A dot is used
  ;; instead of a space to make `show-trailing-whitespace'
  ;; happy, i.e. avoid it marking the whole line as trailing
  ;; spaces.
  (widget-insert ".")
  (put-text-property (1- (point)) (point) 'invisible t)
  (widget-insert "\n"))

(defun notmuch-hello-insert-recent-searches ()
  "Insert recent searches."
  (when notmuch-search-history
    (widget-insert "Recent searches: ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq notmuch-search-history nil)
			     (notmuch-hello-update))
		   "clear")
    (widget-insert "\n\n")
    (let ((start (point)))
      (loop for i from 1 to notmuch-hello-recent-searches-max
	    for search in notmuch-search-history do
	    (let ((widget-symbol (intern (format "notmuch-hello-search-%d" i))))
	      (set widget-symbol
		   (widget-create 'editable-field
				  ;; Don't let the search boxes be
				  ;; less than 8 characters wide.
				  :size (max 8
					     (- (window-width)
						;; Leave some space
						;; at the start and
						;; end of the
						;; boxes.
						(* 2 notmuch-hello-indent)
						;; 1 for the space
						;; before the
						;; `[save]' button. 6
						;; for the `[save]'
						;; button.
						1 6))
				  :action (lambda (widget &rest ignore)
					    (notmuch-hello-search (widget-value widget)))
				  search))
	      (widget-insert " ")
	      (widget-create 'push-button
			     :notify (lambda (widget &rest ignore)
				       (notmuch-hello-add-saved-search widget))
			     :notmuch-saved-search-widget widget-symbol
			     "save"))
	    (widget-insert "\n"))
      (indent-rigidly start (point) notmuch-hello-indent))
    nil))

(defun notmuch-hello-insert-searches (title query-alist &rest options)
  "Insert a section with TITLE showing a list of buttons made from QUERY-ALIST.

QUERY-ALIST must be a list containing elements of the form (NAME . QUERY)
or (NAME QUERY COUNT-QUERY). If the latter form is used,
COUNT-QUERY specifies an alternate query to be used to generate
the count for the associated item.

Supports the following entries in OPTIONS as a plist:
:initially-hidden - if non-nil, section will be hidden on startup
:show-empty-searches - show buttons with no matching messages
:hide-if-empty - hide if no buttons would be shown
   (only makes sense without :show-empty-searches)
:filter - This can be a function that takes the search query as its argument and
   returns a filter to be used in conjuction with the query for that search or nil
   to hide the element. This can also be a string that is used as a combined with
   each query using \"and\".
:filter-count - Separate filter to generate the count displayed each search. Accepts
   the same values as :filter. If :filter and :filter-count are specified, this
   will be used instead of :filter, not in conjunction with it."
  (widget-insert title ": ")
  (if (and notmuch-hello-first-run (plist-get options :initially-hidden))
      (add-to-list 'notmuch-hello-hidden-sections title))
  (let ((is-hidden (member title notmuch-hello-hidden-sections))
	(start (point)))
    (if is-hidden
	(widget-create 'push-button
		       :notify `(lambda (widget &rest ignore)
				  (setq notmuch-hello-hidden-sections
					(delete ,title notmuch-hello-hidden-sections))
				  (notmuch-hello-update))
		       "show")
      (widget-create 'push-button
		     :notify `(lambda (widget &rest ignore)
				(add-to-list 'notmuch-hello-hidden-sections
					     ,title)
				(notmuch-hello-update))
		     "hide"))
    (widget-insert "\n")
    (when (not is-hidden)
      (let ((searches (apply 'notmuch-hello-query-counts query-alist options)))
	(when (or (not (plist-get options :hide-if-empty))
		  searches)
	  (widget-insert "\n")
	  (notmuch-hello-insert-buttons searches)
	  (indent-rigidly start (point) notmuch-hello-indent))))))

(defun notmuch-hello-insert-tags-section (&optional title &rest options)
  "Insert a section displaying all tags with message counts.

TITLE defaults to \"All tags\".
Allowed options are those accepted by `notmuch-hello-insert-searches' and the
following:

:hide-tags - List of tags that should be excluded."
  (apply 'notmuch-hello-insert-searches
	 (or title "All tags")
	 (notmuch-hello-generate-tag-alist (plist-get options :hide-tags))
	 options))

(defun notmuch-hello-insert-inbox ()
  "Show an entry for each saved search and inboxed messages for each tag"
  (notmuch-hello-insert-searches "What's in your inbox"
				 (append
				  (notmuch-saved-searches)
				  (notmuch-hello-generate-tag-alist))
				 :filter "tag:inbox"))

(defun notmuch-hello-insert-alltags ()
  "Insert a section displaying all tags and associated message counts"
  (notmuch-hello-insert-tags-section
   nil
   :initially-hidden (not notmuch-show-all-tags-list)
   :hide-tags notmuch-hello-hide-tags
   :filter notmuch-hello-tag-list-make-query))

(defun notmuch-hello-insert-footer ()
  "Insert the notmuch-hello footer."
  (let ((start (point)))
    (widget-insert "Type a search query and hit RET to view matching threads.\n")
    (when notmuch-search-history
      (widget-insert "Hit RET to re-submit a previous search. Edit it first if you like.\n")
      (widget-insert "Save recent searches with the `save' button.\n"))
    (when notmuch-saved-searches
      (widget-insert "Edit saved searches with the `edit' button.\n"))
    (widget-insert "Hit RET or click on a saved search or tag name to view matching threads.\n")
    (widget-insert "`=' to refresh this screen. `s' to search messages. `q' to quit.\n")
    (widget-create 'link
		   :notify (lambda (&rest ignore)
			     (customize-variable 'notmuch-hello-sections))
		   :button-prefix "" :button-suffix ""
		   "Customize")
    (widget-insert " this page.")
    (let ((fill-column (- (window-width) notmuch-hello-indent)))
      (center-region start (point)))))

;;;###autoload
(defun notmuch-hello (&optional no-display)
  "Run notmuch and display saved searches, known tags, etc."
  (interactive)

  ;; Jump through a hoop to get this value from the deprecated variable
  ;; name (`notmuch-folders') or from the default value.
  (unless notmuch-saved-searches
    (setq notmuch-saved-searches (notmuch-saved-searches)))

  (if no-display
      (set-buffer "*notmuch-hello*")
    (switch-to-buffer "*notmuch-hello*"))

  (let ((target-line (line-number-at-pos))
	(target-column (current-column))
	(inhibit-read-only t))

    ;; Delete all editable widget fields.  Editable widget fields are
    ;; tracked in a buffer local variable `widget-field-list' (and
    ;; others).  If we do `erase-buffer' without properly deleting the
    ;; widgets, some widget-related functions are confused later.
    (mapc 'widget-delete widget-field-list)

    (erase-buffer)

    (unless (eq major-mode 'notmuch-hello-mode)
      (notmuch-hello-mode))

    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapc 'delete-overlay (car all))
      (mapc 'delete-overlay (cdr all)))

    (mapc
     (lambda (section)
       (let ((point-before (point)))
	 (if (functionp section)
	     (funcall section)
	   (apply (car section) (cdr section)))
	 ;; don't insert a newline when the previous section didn't
	 ;; show anything.
	 (unless (eq (point) point-before)
	   (widget-insert "\n"))))
     notmuch-hello-sections)
    (widget-setup)

    ;; Move point back to where it was before refresh. Use line and
    ;; column instead of point directly to be insensitive to additions
    ;; and removals of text within earlier lines.
    (goto-char (point-min))
    (forward-line (1- target-line))
    (move-to-column target-column))
  (run-hooks 'notmuch-hello-refresh-hook)
  (setq notmuch-hello-first-run nil))

(defun notmuch-folder ()
  "Deprecated function for invoking notmuch---calling `notmuch' is preferred now."
  (interactive)
  (notmuch-hello))

;;

(provide 'notmuch-hello)
