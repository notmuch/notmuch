;;; notmuch-address.el --- address completion with notmuch
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

;;; Code:

(require 'message)
(require 'notmuch-parser)
(require 'notmuch-lib)
(require 'notmuch-company)
;;
(declare-function company-manual-begin "company")

(defcustom notmuch-address-command 'internal
  "The command which generates possible addresses. It must take a
single argument and output a list of possible matches, one per
line. The default value of `internal' uses built-in address
completion."
  :type '(radio
	  (const :tag "Use internal address completion" internal)
	  (const :tag "Disable address completion" nil)
	  (string :tag "Use external completion command" "notmuch-addresses"))
  :group 'notmuch-send
  :group 'notmuch-external)

(defcustom notmuch-address-selection-function 'notmuch-address-selection-function
  "The function to select address from given list. The function is
called with PROMPT, COLLECTION, and INITIAL-INPUT as arguments
(subset of what `completing-read' can be called with).
While executed the value of `completion-ignore-case' is t.
See documentation of function `notmuch-address-selection-function'
to know how address selection is made by default."
  :type 'function
  :group 'notmuch-send
  :group 'notmuch-external)

(defvar notmuch-address-last-harvest 0
  "Time of last address harvest")

(defvar notmuch-address-completions (make-hash-table :test 'equal)
  "Hash of email addresses for completion during email composition.
  This variable is set by calling `notmuch-address-harvest'.")

(defvar notmuch-address-full-harvest-finished nil
  "t indicates that full completion address harvesting has been
finished")

(defun notmuch-address-selection-function (prompt collection initial-input)
  "Call (`completing-read'
      PROMPT COLLECTION nil nil INITIAL-INPUT 'notmuch-address-history)"
  (completing-read
   prompt collection nil nil initial-input 'notmuch-address-history))

(defvar notmuch-address-completion-headers-regexp
  "^\\(Resent-\\)?\\(To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):")

(defvar notmuch-address-history nil)

(defun notmuch-address-message-insinuate ()
  (message "calling notmuch-address-message-insinuate is no longer needed"))

(defcustom notmuch-address-use-company t
  "If available, use company mode for address completion"
  :type 'boolean
  :group 'notmuch-send)

(defun notmuch-address-setup ()
  (let* ((use-company (and notmuch-address-use-company
			   (eq notmuch-address-command 'internal)
			   (require 'company nil t)))
	 (pair (cons notmuch-address-completion-headers-regexp
		     (if use-company
			 #'company-manual-begin
		       #'notmuch-address-expand-name))))
      (when use-company
	(notmuch-company-setup))
      (unless (memq pair message-completion-alist)
	(setq message-completion-alist
	      (push pair message-completion-alist)))))

(defun notmuch-address-matching (substring)
  "Returns a list of completion candidates matching SUBSTRING.
The candidates are taken from `notmuch-address-completions'."
  (let ((candidates)
	(re (regexp-quote substring)))
    (maphash (lambda (key val)
	       (when (string-match re key)
		 (push key candidates)))
	     notmuch-address-completions)
    candidates))

(defun notmuch-address-options (original)
  "Returns a list of completion candidates. Uses either
elisp-based implementation or older implementation requiring
external commands."
  (cond
   ((eq notmuch-address-command 'internal)
    (when (not notmuch-address-full-harvest-finished)
      ;; First, run quick synchronous harvest based on what the user
      ;; entered so far
      (notmuch-address-harvest (format "to:%s*" original) t))
    (prog1 (notmuch-address-matching original)
      ;; Then start the (potentially long-running) full asynchronous harvest if necessary
      (notmuch-address-harvest-trigger)))
   (t
    (process-lines notmuch-address-command original))))

(defun notmuch-address-expand-name ()
  (when notmuch-address-command
    (let* ((end (point))
	   (beg (save-excursion
		  (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
		  (goto-char (match-end 0))
		  (point)))
	   (orig (buffer-substring-no-properties beg end))
	   (completion-ignore-case t)
	   (options (with-temp-message "Looking for completion candidates..."
		      (notmuch-address-options orig)))
	   (num-options (length options))
	   (chosen (cond
		    ((eq num-options 0)
		     nil)
		    ((eq num-options 1)
		     (car options))
		    (t
		     (funcall notmuch-address-selection-function
			      (format "Address (%s matches): " num-options)
			      (cdr options) (car options))))))
      (if chosen
	  (progn
	    (push chosen notmuch-address-history)
	    (delete-region beg end)
	    (insert chosen))
	(message "No matches.")
	(ding)))))

;; Copied from `w3m-which-command'.
(defun notmuch-address-locate-command (command)
  "Return non-nil if `command' is an executable either on
`exec-path' or an absolute pathname."
  (when (stringp command)
    (if (and (file-name-absolute-p command)
	     (file-executable-p command))
	command
      (setq command (file-name-nondirectory command))
      (catch 'found-command
	(let (bin)
	  (dolist (dir exec-path)
	    (setq bin (expand-file-name command dir))
	    (when (or (and (file-executable-p bin)
			   (not (file-directory-p bin)))
		      (and (file-executable-p (setq bin (concat bin ".exe")))
			   (not (file-directory-p bin))))
	      (throw 'found-command bin))))))))

(defun notmuch-address-harvest-addr (result)
  (let ((name-addr (plist-get result :name-addr)))
    (puthash name-addr t notmuch-address-completions)))

(defun notmuch-address-harvest-handle-result (obj)
  (notmuch-address-harvest-addr obj))

(defun notmuch-address-harvest-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
	(goto-char (point-max))
	(insert string))
      (notmuch-sexp-parse-partial-list
       'notmuch-address-harvest-handle-result (process-buffer proc)))))

(defvar notmuch-address-harvest-procs '(nil . nil)
  "The currently running harvests.

The car is a partial harvest, and the cdr is a full harvest")

(defun notmuch-address-harvest (&optional filter-query synchronous callback)
  "Collect addresses completion candidates. It queries the
notmuch database for all messages sent by the user optionally
matching FILTER-QUERY (if not nil). It collects the destination
addresses from those messages and stores them in
`notmuch-address-completions'. Address harvesting may take some
time so the address collection runs asynchronously unless
SYNCHRONOUS is t. In case of asynchronous execution, CALLBACK is
called when harvesting finishes."
  (let* ((from-me-query (mapconcat (lambda (x) (concat "from:" x)) (notmuch-user-emails) " or "))
	 (query (if filter-query
		    (format "(%s) and (%s)" from-me-query filter-query)
		  from-me-query))
	 (args `("address" "--format=sexp" "--format-version=2"
		 "--output=recipients"
		 "--deduplicate=address"
		 ,query)))
    (if synchronous
	(mapc #'notmuch-address-harvest-addr
				   (apply 'notmuch-call-notmuch-sexp args))
      ;; Asynchronous
      (let* ((current-proc (if filter-query
			       (car notmuch-address-harvest-procs)
			     (cdr notmuch-address-harvest-procs)))
	     (proc-name (format "notmuch-address-%s-harvest"
				(if filter-query "partial" "full")))
	     (proc-buf (concat " *" proc-name "*")))
	;; Kill any existing process
	(when current-proc
	  (kill-buffer (process-buffer current-proc))) ; this also kills the process

	(setq current-proc
	      (apply 'notmuch-start-notmuch proc-name proc-buf
		     callback				; process sentinel
		     args))
	(set-process-filter current-proc 'notmuch-address-harvest-filter)
	(set-process-query-on-exit-flag current-proc nil)
	(if filter-query
	    (setcar notmuch-address-harvest-procs current-proc)
	  (setcdr notmuch-address-harvest-procs current-proc)))))
  ;; return value
  nil)

(defun notmuch-address-harvest-trigger ()
  (let ((now (float-time)))
    (when (> (- now notmuch-address-last-harvest) 86400)
      (setq notmuch-address-last-harvest now)
      (notmuch-address-harvest nil nil
			       (lambda (proc event)
				 ;; If harvest fails, we want to try
				 ;; again when the trigger is next
				 ;; called
				 (if (string= event "finished\n")
				     (setq notmuch-address-full-harvest-finished t)
				   (setq notmuch-address-last-harvest 0)))))))

;;

(provide 'notmuch-address)

;;; notmuch-address.el ends here
