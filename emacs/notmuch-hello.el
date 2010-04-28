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

(require 'widget)
(require 'wid-edit) ; For `widget-forward'.
(require 'cl)

(require 'notmuch-lib)
(require 'notmuch-mua)

(declare-function notmuch-search "notmuch" (query &optional oldest-first target-thread target-line continuation))
(declare-function notmuch-poll "notmuch" ())

(defvar notmuch-hello-search-bar-marker nil
  "The position of the search bar within the notmuch-hello buffer.")

(defcustom notmuch-recent-searches-max 10
  "The number of recent searches to store and display."
  :type 'integer
  :group 'notmuch)

(defcustom notmuch-show-empty-saved-searches nil
  "Should saved searches with no messages be listed?"
  :type 'boolean
  :group 'notmuch)

(defvar notmuch-hello-indent 4
  "How much to indent non-headers.")

(defcustom notmuch-show-logo t
  "Should the notmuch logo be shown?"
  :type 'boolean
  :group 'notmuch)

(defcustom notmuch-show-all-tags-list nil
  "Should all tags be shown in the notmuch-hello view?"
  :type 'boolean
  :group 'notmuch)

(defface notmuch-hello-logo-background
  '((((class color)
      (background dark))
     (:background "#5f5f5f"))
    (((class color)
      (background light))
     (:background "white")))
  "Background colour for the notmuch logo."
  :group 'notmuch)

(defvar notmuch-hello-url "http://notmuchmail.org"
  "The `notmuch' web site.")

(defvar notmuch-hello-recent-searches nil)

(defun notmuch-hello-remember-search (search)
  (if (not (member search notmuch-hello-recent-searches))
      (push search notmuch-hello-recent-searches))
  (if (> (length notmuch-hello-recent-searches)
	 notmuch-recent-searches-max)
      (setq notmuch-hello-recent-searches (butlast notmuch-hello-recent-searches))))

(defun notmuch-hello-trim (search)
  "Trim whitespace."
  (if (string-match "^[[:space:]]*\\(.*[^[:space:]]\\)[[:space:]]*$" search)
      (match-string 1 search)
    search))

(defun notmuch-hello-search (search)
  (let ((search (notmuch-hello-trim search)))
    (notmuch-hello-remember-search search)
    (notmuch-search search notmuch-search-oldest-first nil nil #'notmuch-hello-search-continuation)))

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
			     (push (cons name search)
				   notmuch-saved-searches))
    (message "Saved '%s' as '%s'." search name)
    (notmuch-hello-update)))

(defun notmuch-hello-longest-label (tag-alist)
  (or (loop for elem in tag-alist
	    maximize (length (car elem)))
      0))

(defun notmuch-hello-roundup (dividend divisor)
  "Return the rounded up value of dividing `dividend' by `divisor'."
  (+ (/ dividend divisor)
     (if (> (% dividend divisor) 0) 1 0)))

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
  (let ((nrows (notmuch-hello-roundup (length list) ncols)))
    (loop for row from 0 to (- nrows 1)
	  append (notmuch-hello-reflect-generate-row ncols nrows row list))))

(defun notmuch-hello-widget-search (widget &rest ignore)
  (notmuch-search (widget-get widget
			      :notmuch-search-terms)
		  notmuch-search-oldest-first
		  nil nil #'notmuch-hello-search-continuation))

(defun notmuch-saved-search-count (search)
  (car (process-lines notmuch-command "count" search)))

(defun notmuch-hello-insert-tags (tag-alist widest target)
  (let* ((tags-per-line (max 1
			     (/ (- (window-width) notmuch-hello-indent)
				;; Count is 7 wide (6 digits plus
				;; space), 1 for the space after the
				;; name.
				(+ 7 1 widest))))
	 (count 0)
	 (reordered-list (notmuch-hello-reflect tag-alist tags-per-line))
	 ;; Hack the display of the buttons used.
	 (widget-push-button-prefix "")
	 (widget-push-button-suffix "")
	 (found-target-pos nil))
    ;; dme: It feels as though there should be a better way to
    ;; implement this loop than using an incrementing counter.
    (mapc (lambda (elem)
	    ;; (not elem) indicates an empty slot in the matrix.
	    (when elem
	      (let* ((name (car elem))
		     (query (cdr elem))
		     (formatted-name (format "%s " name)))
		(widget-insert (format "%6s " (notmuch-saved-search-count query)))
		(if (string= formatted-name target)
		    (setq found-target-pos (point-marker)))
		(widget-create 'push-button
			       :notify #'notmuch-hello-widget-search
			       :notmuch-search-terms query
			       formatted-name)
		;; Insert enough space to consume the rest of the
		;; column.  Because the button for the name is `(1+
		;; (length name))' long (due to the trailing space) we
		;; can just insert `(- widest (length name))' spaces -
		;; the column separator is included in the button if
		;; `(equal widest (length name)'.
		(widget-insert (make-string (- widest (length name)) ? ))))
	    (setq count (1+ count))
	    (if (eq (% count tags-per-line) 0)
		(widget-insert "\n")))
	  reordered-list)

    ;; If the last line was not full (and hence did not include a
    ;; carriage return), insert one now.
    (if (not (eq (% count tags-per-line) 0))
	(widget-insert "\n"))
    found-target-pos))

(defun notmuch-hello-goto-search ()
  "Put point inside the `search' widget."
  (interactive)
  (goto-char notmuch-hello-search-bar-marker))

(defimage notmuch-hello-logo ((:type png :file "notmuch-logo.png")))

(defun notmuch-hello-search-continuation()
  (notmuch-hello-update t))

(defun notmuch-hello-update (&optional no-display)
  ;; Lazy - rebuild everything.
  (interactive)
  (notmuch-hello no-display))

(defun notmuch-hello-poll-and-update ()
  "Invoke `notmuch-poll' to import mail, then refresh the current view."
  (interactive)
  (notmuch-poll)
  (notmuch-hello-update))

(defun notmuch-hello (&optional no-display)
  (interactive)

  ; Jump through a hoop to get this value from the deprecated variable
  ; name (`notmuch-folders') or from the default value.
  (if (not notmuch-saved-searches)
    (setq notmuch-saved-searches (notmuch-saved-searches)))

  (if no-display
      (set-buffer "*notmuch-hello*")
    (switch-to-buffer "*notmuch-hello*"))

  (let ((target (if (widget-at)
		   (widget-value (widget-at))
		 (condition-case nil
		     (progn
		       (widget-forward 1)
		       (widget-value (widget-at)))
		   (error nil)))))

    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))

    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapc 'delete-overlay (car all))
      (mapc 'delete-overlay (cdr all)))

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
		     (car (process-lines notmuch-command "count")))
      (widget-insert " messages (that's not much mail).\n"))

    (let ((found-target-pos nil)
	  (final-target-pos nil))
      (let* ((saved-alist
	      ;; Filter out empty saved seaches if required.
	      (if notmuch-show-empty-saved-searches
		  notmuch-saved-searches
		(loop for elem in notmuch-saved-searches
		      if (> (string-to-number (notmuch-saved-search-count (cdr elem))) 0)
		      collect elem)))
	     (saved-widest (notmuch-hello-longest-label saved-alist))
	     (alltags-alist (if notmuch-show-all-tags-list
				(mapcar '(lambda (tag) (cons tag (concat "tag:" tag)))
					(process-lines notmuch-command "search-tags"))))
	     (alltags-widest (notmuch-hello-longest-label alltags-alist))
	     (widest (max saved-widest alltags-widest)))

	(when saved-alist
	  (widget-insert "\nSaved searches: ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (customize-variable 'notmuch-saved-searches))
			 "edit")
	  (widget-insert "\n\n")
	  (setq final-target-pos (point-marker))
	  (let ((start (point)))
	    (setq found-target-pos (notmuch-hello-insert-tags saved-alist widest target))
	    (if found-target-pos
		(setq final-target-pos found-target-pos))
	    (indent-rigidly start (point) notmuch-hello-indent)))

	(widget-insert "\nSearch: ")
	(setq notmuch-hello-search-bar-marker (point-marker))
	(widget-create 'editable-field
		       ;; Leave some space at the start and end of the
		       ;; search boxes.
		       :size (max 8 (- (window-width) notmuch-hello-indent
				       (length "Search: ")))
		       :action (lambda (widget &rest ignore)
				 (notmuch-hello-search (widget-value widget))))
	(widget-insert "\n")

	(when notmuch-hello-recent-searches
	  (widget-insert "\nRecent searches: ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (setq notmuch-hello-recent-searches nil)
				   (notmuch-hello-update))
			 "clear")
	  (widget-insert "\n\n")
	  (let ((start (point))
		(nth 0))
	    (mapc '(lambda (search)
		     (let ((widget-symbol (intern (format "notmuch-hello-search-%d" nth))))
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
		     (widget-insert "\n")
		     (setq nth (1+ nth)))
		  notmuch-hello-recent-searches)
	    (indent-rigidly start (point) notmuch-hello-indent)))

	(when alltags-alist
	  (widget-insert "\nAll tags: ")
	  (widget-create 'push-button
			 :notify (lambda (widget &rest ignore)
				   (setq notmuch-show-all-tags-list nil)
				   (notmuch-hello-update))
			 "hide")
	  (widget-insert "\n\n")
	  (let ((start (point)))
	    (setq found-target-pos (notmuch-hello-insert-tags alltags-alist widest target))
	    (if (not final-target-pos)
		(setq final-target-pos found-target-pos))
	    (indent-rigidly start (point) notmuch-hello-indent)))

	(widget-insert "\n")

	(if (not notmuch-show-all-tags-list)
	    (widget-create 'push-button
			   :notify (lambda (widget &rest ignore)
				     (setq notmuch-show-all-tags-list t)
				     (notmuch-hello-update))
			   "Show all tags")))

      (let ((start (point)))
	(widget-insert "\n\n")
	(widget-insert "Type a search query and hit RET to view matching threads.\n")
	(when notmuch-hello-recent-searches
	  (widget-insert "Hit RET to re-submit a previous search. Edit it first if you like.\n")
	  (widget-insert "Save recent searches with the `save' button.\n"))
	(when notmuch-saved-searches
	  (widget-insert "Edit saved searches with the `edit' button.\n"))
	(widget-insert "Hit RET or click on a saved search or tag name to view matching threads.\n")
	(widget-insert "`=' refreshes this screen. `s' jumps to the search box. `q' to quit.\n")
	(let ((fill-column (- (window-width) notmuch-hello-indent)))
	  (center-region start (point))))

      (use-local-map widget-keymap)
      (local-set-key "=" 'notmuch-hello-update)
      (local-set-key "G" 'notmuch-hello-poll-and-update)
      (local-set-key "m" 'notmuch-mua-mail)
      (local-set-key "q" '(lambda () (interactive) (kill-buffer (current-buffer))))
      (local-set-key "s" 'notmuch-hello-goto-search)
      (local-set-key "v" '(lambda () (interactive)
			    (message "notmuch version %s" (notmuch-version))))

      (widget-setup)

      (when final-target-pos
	(goto-char final-target-pos)
	(unless (widget-at)
	  (widget-forward 1)))

      (unless (widget-at)
	(notmuch-hello-goto-search)))))

;;;###autoload
(defun notmuch-folder ()
  "Deprecated function for invoking notmuch---calling `notmuch' is preferred now."
  (interactive)
  (notmuch-hello))

;;

(provide 'notmuch-hello)
