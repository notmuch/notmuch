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
(declare-function notmuch-folder-count "notmuch" (search))

(defcustom notmuch-hello-recent-searches-max 10
  "The number of recent searches to store and display."
  :type 'integer
  :group 'notmuch)

(defcustom notmuch-hello-show-empty-saved-searches nil
  "Should saved searches with no messages be listed?"
  :type 'boolean
  :group 'notmuch)

(defcustom notmuch-hello-indent 4
  "How much to indent non-headers."
  :type 'integer
  :group 'notmuch)

(defcustom notmuch-hello-saved-searches notmuch-folders
  "A list of saved searches to display."
  :type '(alist :key-type string :value-type string)
  :group 'notmuch)

(defcustom notmuch-hello-show-logo t
  "Should the notmuch logo be shown?"
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

(defcustom notmuch-hello-jump-to-search nil
  "Whether `notmuch-hello' should always jump to the search
field."
  :type 'boolean
  :group 'notmuch)

(defvar notmuch-hello-url "http://notmuchmail.org"
  "The `notmuch' web site.")

(defvar notmuch-hello-recent-searches nil)

(defun notmuch-hello-remember-search (search)
  (if (not (member search notmuch-hello-recent-searches))
      (push search notmuch-hello-recent-searches))
  (if (> (length notmuch-hello-recent-searches)
	 notmuch-hello-recent-searches-max)
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
			       notmuch-hello-saved-searches)))
    ;; If an existing saved search with this name exists, remove it.
    (setq notmuch-hello-saved-searches
	  (loop for elem in notmuch-hello-saved-searches
		if (not (equal name
			       (car elem)))
		collect elem))
    ;; Add the new one.
    (customize-save-variable 'notmuch-hello-saved-searches
			     (push (cons name search)
				   notmuch-hello-saved-searches))
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

(defun notmuch-hello-reflect (list width)
  "Reflect a `width' wide matrix represented by `list' along the
diagonal."
  ;; Not very lispy...
  (let* ((len (length list))
	 (nrows (notmuch-hello-roundup len width)))
    (loop for row from 0 to (- nrows 1)
	  append (loop for col from 0 to (- width 1)
		       ;; How could we calculate the offset just once
		       ;; per inner-loop?
		       if (< (+ (* nrows col) row) len)
		       collect (nth (+ (* nrows col) row) list)
		       else
		       ;; Don't forget to insert an empty slot in the
		       ;; output matrix if there is no corresponding
		       ;; value in the input matrix.
		       collect nil))))

(defun notmuch-hello-widget-search (widget &rest ignore)
  (notmuch-search (widget-get widget
			      :notmuch-search-terms)
		  notmuch-search-oldest-first
		  nil nil #'notmuch-hello-search-continuation))

(defun notmuch-hello-insert-tags (tag-alist widest target)
  (let* ((tags-per-line (max 1
			     (/ (- (window-width) notmuch-hello-indent)
				;; Count is 7 wide, 1 for the space
				;; after the name.
				(+ 7 1 widest))))
	 (count 0)
	 (reordered-list (notmuch-hello-reflect tag-alist tags-per-line))
	 ;; Hack the display of the buttons used.
	 (widget-push-button-prefix "")
	 (widget-push-button-suffix "")
	 (found-target-pos nil))
    ;; dme: It feels as though there should be a better way to
    ;; implement this loop than using an incrementing counter.
    (loop for elem in reordered-list
	  do (progn
	       ;; (not elem) indicates an empty slot in the matrix.
	       (when elem
		 (widget-insert (format "%6s " (notmuch-folder-count (cdr elem))))
		 (if (string= (car elem) target)
		     (progn
		       (setq found-target-pos (point-marker))))
		 (widget-create 'push-button
				:notify #'notmuch-hello-widget-search
				:notmuch-search-terms (cdr elem)
				(car elem))
		 (insert (make-string (- widest (length (car elem))) ? )))
	       (setq count (1+ count))
	       (if (eq (% count tags-per-line) 0)
		   (widget-insert "\n"))))

    ;; If the last line was not full (and hence did not include a
    ;; carriage return), insert one now.
    (if (not (eq (% count tags-per-line) 0))
	(widget-insert "\n"))
    found-target-pos))

(defun notmuch-hello-goto-search ()
  "Put point inside the `search' widget, which we know is first."
  (interactive)
  (goto-char (point-min))
  (widget-forward 3))

(defimage notmuch-hello-logo ((:type png :file "notmuch-logo.png")))

(defun notmuch-hello-search-continuation()
  (notmuch-hello t))

(defun notmuch-hello-update (&optional no-display)
  ;; Lazy - rebuild everything.
  (interactive)
  (let ((target (if (widget-at)
		   (widget-value (widget-at))
		 (progn
		   (widget-forward 1)
		   (widget-value (widget-at))))))
    (notmuch-hello no-display target)))

(defun notmuch-hello (&optional no-display target)
  (interactive)

  (if no-display
      (set-buffer "*notmuch-hello*")
    (switch-to-buffer "*notmuch-hello*"))

  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))

  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all)))

  (when notmuch-hello-show-logo
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
    (widget-insert " messages (that's not much mail).\n\n"))

  (let ((start (point)))
    (widget-insert "Search: ")
    (widget-create 'editable-field
		   ;; Leave some space at the start and end of the
		   ;; search boxes.
		   :size (max 8 (- (window-width) (* 2 notmuch-hello-indent)
				   (length "Search: ")))
		   :action (lambda (widget &rest ignore)
			     (notmuch-hello-search (widget-value widget))))
    (widget-insert "\n")
    (indent-rigidly start (point) notmuch-hello-indent))

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

  (let ((found-target-pos nil)
	(final-target-pos nil))
    (let* ((saved-alist
	    ;; Filter out empty saved seaches if required.
	    (if notmuch-hello-show-empty-saved-searches
		notmuch-hello-saved-searches
	      (loop for elem in notmuch-hello-saved-searches
		    if (> (string-to-number (notmuch-folder-count (cdr elem))) 0)
		    collect elem)))
	   (saved-widest (notmuch-hello-longest-label saved-alist))
	   (alltags-alist (mapcar '(lambda (tag) (cons tag (concat "tag:" tag)))
				  (process-lines notmuch-command "search-tags")))
	   (alltags-widest (notmuch-hello-longest-label alltags-alist))
	   (widest (max saved-widest alltags-widest)))

      (when saved-alist
	(widget-insert "\nSaved searches: ")
	(widget-create 'push-button
		       :notify (lambda (&rest ignore)
				 (customize-variable 'notmuch-hello-saved-searches))
		       "edit")
	(widget-insert "\n\n")
	(let ((start (point)))
	  (setq found-target-pos (notmuch-hello-insert-tags saved-alist widest target))
	  (if (not final-target-pos)
	      (setq final-target-pos found-target-pos))
	  (indent-rigidly start (point) notmuch-hello-indent)))

      (when alltags-alist
	(widget-insert "\nAll tags:\n\n")
	(let ((start (point)))
	  (setq found-target-pos (notmuch-hello-insert-tags alltags-alist widest target))
	  (if (not final-target-pos)
	      (setq final-target-pos found-target-pos))
	  (indent-rigidly start (point) notmuch-hello-indent))))

    (let ((start (point)))
      (widget-insert "\n\n")
      (widget-insert "Type a search query and hit RET to view matching threads.\n")
      (when notmuch-hello-recent-searches
	(widget-insert "Hit RET to re-submit a previous search. Edit it first if you like.\n")
	(widget-insert "Save recent searches with the `save' button.\n"))
      (when notmuch-hello-saved-searches
	(widget-insert "Edit saved searches with the `edit' button.\n"))
      (widget-insert "Hit RET or click on a saved search or tag name to view matching threads.\n")
      (widget-insert "`=' refreshes this screen. `s' jumps to the search box. `q' to quit.\n")
      (let ((fill-column (- (window-width) notmuch-hello-indent)))
	(center-region start (point))))

    (use-local-map widget-keymap)
    (local-set-key "=" 'notmuch-hello-update)
    (local-set-key "m" 'notmuch-mua-mail)
    (local-set-key "q" '(lambda () (interactive) (kill-buffer (current-buffer))))
    (local-set-key "s" 'notmuch-hello-goto-search)
    (local-set-key "v" '(lambda () (interactive)
			  (message "notmuch version %s" (notmuch-version))))

    (widget-setup)

    (if final-target-pos
	(goto-char final-target-pos)
      (if notmuch-hello-jump-to-search
	  (notmuch-hello-goto-search)
	(goto-char (point-min))))))

;;

(provide 'notmuch-hello)
