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

(require 'mm-view)
(require 'mm-decode)
(eval-when-compile (require 'cl))

(defvar notmuch-command "notmuch"
  "Command to run the notmuch binary.")

(defgroup notmuch nil
  "Notmuch mail reader for Emacs."
  :group 'mail)

(defgroup notmuch-hello nil
  "Overview of saved searches, tags, etc."
  :group 'notmuch)

(defgroup notmuch-search nil
  "Searching and sorting mail."
  :group 'notmuch)

(defgroup notmuch-show nil
  "Showing messages and threads."
  :group 'notmuch)

(defgroup notmuch-send nil
  "Sending messages from Notmuch."
  :group 'notmuch)

(custom-add-to-group 'notmuch-send 'message 'custom-group)

(defgroup notmuch-crypto nil
  "Processing and display of cryptographic MIME parts."
  :group 'notmuch)

(defgroup notmuch-hooks nil
  "Running custom code on well-defined occasions."
  :group 'notmuch)

(defgroup notmuch-external nil
  "Running external commands from within Notmuch."
  :group 'notmuch)

(defgroup notmuch-faces nil
  "Graphical attributes for displaying text"
  :group 'notmuch)

(defcustom notmuch-search-oldest-first t
  "Show the oldest mail first when searching."
  :type 'boolean
  :group 'notmuch-search)

;;

(defvar notmuch-search-history nil
  "Variable to store notmuch searches history.")

(defcustom notmuch-saved-searches nil
  "A list of saved searches to display."
  :type '(alist :key-type string :value-type string)
  :group 'notmuch-hello)

(defvar notmuch-folders nil
  "Deprecated name for what is now known as `notmuch-saved-searches'.")

(defun notmuch-saved-searches ()
  "Common function for querying the notmuch-saved-searches variable.

We do this as a function to support the old name of the
variable (`notmuch-folders') as well as for the default value if
the user hasn't set this variable with the old or new value."
  (if notmuch-saved-searches
      notmuch-saved-searches
    (if notmuch-folders
	notmuch-folders
      '(("inbox" . "tag:inbox")
	("unread" . "tag:unread")))))

(defun notmuch-version ()
  "Return a string with the notmuch version number."
  (let ((long-string
	 ;; Trim off the trailing newline.
	 (substring (shell-command-to-string
		     (concat notmuch-command " --version"))
		    0 -1)))
    (if (string-match "^notmuch\\( version\\)? \\(.*\\)$"
		      long-string)
	(match-string 2 long-string)
      "unknown")))

(defun notmuch-config-get (item)
  "Return a value from the notmuch configuration."
  ;; Trim off the trailing newline
  (substring (shell-command-to-string
	      (concat notmuch-command " config get " item))
	      0 -1))

(defun notmuch-database-path ()
  "Return the database.path value from the notmuch configuration."
  (notmuch-config-get "database.path"))

(defun notmuch-user-name ()
  "Return the user.name value from the notmuch configuration."
  (notmuch-config-get "user.name"))

(defun notmuch-user-primary-email ()
  "Return the user.primary_email value from the notmuch configuration."
  (notmuch-config-get "user.primary_email"))

(defun notmuch-user-other-email ()
  "Return the user.other_email value (as a list) from the notmuch configuration."
  (split-string (notmuch-config-get "user.other_email") "\n"))

(defun notmuch-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun notmuch-prettify-subject (subject)
  ;; This function is used by `notmuch-search-process-filter' which
  ;; requires that we not disrupt its' matching state.
  (save-match-data
    (if (and subject
	     (string-match "^[ \t]*$" subject))
	"[No Subject]"
      subject)))

(defun notmuch-id-to-query (id)
  "Return a query that matches the message with id ID."
  (concat "id:\"" (replace-regexp-in-string "\"" "\"\"" id t t) "\""))

;;

(defun notmuch-common-do-stash (text)
  "Common function to stash text in kill ring, and display in minibuffer."
  (kill-new text)
  (message "Stashed: %s" text))

;;

(defun notmuch-remove-if-not (predicate list)
  "Return a copy of LIST with all items not satisfying PREDICATE removed."
  (let (out)
    (while list
      (when (funcall predicate (car list))
        (push (car list) out))
      (setq list (cdr list)))
    (nreverse out)))

;; This lets us avoid compiling these replacement functions when emacs
;; is sufficiently new enough to supply them alone. We do the macro
;; treatment rather than just wrapping our defun calls in a when form
;; specifically so that the compiler never sees the code on new emacs,
;; (since the code is triggering warnings that we don't know how to get
;; rid of.
;;
;; A more clever macro here would accept a condition and a list of forms.
(defmacro compile-on-emacs-prior-to-23 (form)
  "Conditionally evaluate form only on emacs < emacs-23."
  (list 'when (< emacs-major-version 23)
	form))

(defun notmuch-split-content-type (content-type)
  "Split content/type into 'content' and 'type'"
  (split-string content-type "/"))

(defun notmuch-match-content-type (t1 t2)
  "Return t if t1 and t2 are matching content types, taking wildcards into account"
  (let ((st1 (notmuch-split-content-type t1))
	(st2 (notmuch-split-content-type t2)))
    (if (or (string= (cadr st1) "*")
	    (string= (cadr st2) "*"))
	;; Comparison of content types should be case insensitive.
	(string= (downcase (car st1)) (downcase (car st2)))
      (string= (downcase t1) (downcase t2)))))

(defvar notmuch-multipart/alternative-discouraged
  '(
    ;; Avoid HTML parts.
    "text/html"
    ;; multipart/related usually contain a text/html part and some associated graphics.
    "multipart/related"
    ))

(defun notmuch-multipart/alternative-choose (types)
  "Return a list of preferred types from the given list of types"
  ;; Based on `mm-preferred-alternative-precedence'.
  (let ((seq types))
    (dolist (pref (reverse notmuch-multipart/alternative-discouraged))
      (dolist (elem (copy-sequence seq))
	(when (string-match pref elem)
	  (setq seq (nconc (delete elem seq) (list elem))))))
    seq))

(defun notmuch-parts-filter-by-type (parts type)
  "Given a list of message parts, return a list containing the ones matching
the given type."
  (remove-if-not
   (lambda (part) (notmuch-match-content-type (plist-get part :content-type) type))
   parts))

;; Helper for parts which are generally not included in the default
;; JSON output.
(defun notmuch-get-bodypart-internal (query part-number process-crypto)
  (let ((args '("show" "--format=raw"))
	(part-arg (format "--part=%s" part-number)))
    (setq args (append args (list part-arg)))
    (if process-crypto
	(setq args (append args '("--decrypt"))))
    (setq args (append args (list query)))
    (with-temp-buffer
      (let ((coding-system-for-read 'no-conversion))
	(progn
	  (apply 'call-process (append (list notmuch-command nil (list t nil) nil) args))
	  (buffer-string))))))

(defun notmuch-get-bodypart-content (msg part nth process-crypto)
  (or (plist-get part :content)
      (notmuch-get-bodypart-internal (notmuch-id-to-query (plist-get msg :id)) nth process-crypto)))

(defun notmuch-mm-display-part-inline (msg part nth content-type process-crypto)
  "Use the mm-decode/mm-view functions to display a part in the
current buffer, if possible."
  (let ((display-buffer (current-buffer)))
    (with-temp-buffer
      ;; In case there is :content, the content string is already converted
      ;; into emacs internal format. `gnus-decoded' is a fake charset,
      ;; which means no further decoding (to be done by mm- functions).
      (let* ((charset (if (plist-member part :content)
			  'gnus-decoded
			(plist-get part :content-charset)))
	     (handle (mm-make-handle (current-buffer) `(,content-type (charset . ,charset)))))
	;; If the user wants the part inlined, insert the content and
	;; test whether we are able to inline it (which includes both
	;; capability and suitability tests).
	(when (mm-inlined-p handle)
	  (insert (notmuch-get-bodypart-content msg part nth process-crypto))
	  (when (mm-inlinable-p handle)
	    (set-buffer display-buffer)
	    (mm-display-part handle)
	    t))))))

;; Converts a plist of headers to an alist of headers. The input plist should
;; have symbols of the form :Header as keys, and the resulting alist will have
;; symbols of the form 'Header as keys.
(defun notmuch-headers-plist-to-alist (plist)
  (loop for (key value . rest) on plist by #'cddr
	collect (cons (intern (substring (symbol-name key) 1)) value)))

;; Compatibility functions for versions of emacs before emacs 23.
;;
;; Both functions here were copied from emacs 23 with the following copyright:
;;
;; Copyright (C) 1985, 1986, 1992, 1994, 1995, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
;;
;; and under the GPL version 3 (or later) exactly as notmuch itself.
(compile-on-emacs-prior-to-23
 (defun apply-partially (fun &rest args)
   "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called."
   (lexical-let ((fun fun) (args1 args))
     (lambda (&rest args2) (apply fun (append args1 args2))))))

(compile-on-emacs-prior-to-23
 (defun mouse-event-p (object)
   "Return non-nil if OBJECT is a mouse click event."
   (memq (event-basic-type object) '(mouse-1 mouse-2 mouse-3 mouse-movement))))

;; This variable is used only buffer local, but it needs to be
;; declared globally first to avoid compiler warnings.
(defvar notmuch-show-process-crypto nil)
(make-variable-buffer-local 'notmuch-show-process-crypto)

(provide 'notmuch-lib)

