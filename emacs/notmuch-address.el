;; notmuch-address.el --- address completion with notmuch
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

(require 'message)

;;

(defcustom notmuch-address-command "notmuch-addresses"
  "The command which generates possible addresses. It must take a
single argument and output a list of possible matches, one per
line."
  :type 'string
  :group 'notmuch-send
  :group 'notmuch-external)

(defvar notmuch-address-message-alist-member
  '("^\\(Resent-\\)?\\(To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):"
	      . notmuch-address-expand-name))

(defvar notmuch-address-history nil)

(defun notmuch-address-message-insinuate ()
  (unless (memq notmuch-address-message-alist-member message-completion-alist)
    (setq message-completion-alist
	  (push notmuch-address-message-alist-member message-completion-alist))))

(defun notmuch-address-options (original)
  (process-lines notmuch-address-command original))

(defun notmuch-address-expand-name ()
  (let* ((end (point))
	 (beg (save-excursion
		(re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
		(goto-char (match-end 0))
		(point)))
	 (orig (buffer-substring-no-properties beg end))
	 (completion-ignore-case t)
	 (options (notmuch-address-options orig))
	 (num-options (length options))
	 (chosen (cond
		  ((eq num-options 0)
		   nil)
		  ((eq num-options 1)
		   (car options))
		  (t
		   (completing-read (format "Address (%s matches): " num-options)
				    (cdr options) nil nil (car options)
				    'notmuch-address-history)))))
    (if chosen
	(progn
	  (push chosen notmuch-address-history)
	  (delete-region beg end)
	  (insert chosen))
      (message "No matches.")
      (ding))))

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

;; If we can find the program specified by `notmuch-address-command',
;; insinuate ourselves into `message-mode'.
(when (notmuch-address-locate-command notmuch-address-command)
  (notmuch-address-message-insinuate))

;;

(provide 'notmuch-address)
