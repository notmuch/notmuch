; notmuch.el --- run notmuch within emacs
;
; Copyright © Carl Worth
;
; This file is part of Notmuch.
;
; Notmuch is free software: you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Notmuch is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Carl Worth <cworth@cworth.org>
;
; Much of notmuch.el was written by looking at the implementation of
; compile.el from the emacs distribution source which has the
; following copyright and authorsip (and the identical license as
; above):
;
; Copyright (C) 1985, 1986, 1987, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;   2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;   Free Software Foundation, Inc.

; Authors: Roland McGrath <roland@gnu.org>,
;	    Daniel Pfeiffer <occitan@esperanto.org>

(defvar notmuch-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'notmuch-show-next-message)
    (define-key map "p" 'notmuch-show-previous-message)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "x" 'kill-this-buffer)
    map)
  "Keymap for \"notmuch show\" buffers.")
(fset 'notmuch-show-mode-map notmuch-show-mode-map)

(defvar notmuch-show-message-begin-regexp "%message{")

(defun notmuch-show-next-message ()
  "Advance point to the beginning of the next message in the buffer."
  (interactive)
  ; First, ensure we get off the current message marker
  (if (not (eobp))
      (forward-char))
  (if (not (re-search-forward notmuch-show-message-begin-regexp nil t))
      (goto-char (point-max)))
  (beginning-of-line))

(defun notmuch-show-previous-message ()
  "Advance point to the beginning of the previous message in the buffer."
  (interactive)
  ; First, ensure we get off the current message marker
  (if (not (eobp))
      (forward-char))
  (if (not (re-search-backward notmuch-show-message-begin-regexp nil t))
      (progn
	(goto-char (point-min))
	(beginning-of-line))))

;;;###autoload
(defun notmuch-show-mode ()
  "Major mode for handling the output of \"notmuch show\""
  (interactive)
  (kill-all-local-variables)
  (use-local-map notmuch-show-mode-map)
  (setq major-mode 'notmuch-show-mode
	mode-name "notmuch-show")
  (setq buffer-read-only t))

(defun notmuch-show (thread-id)
  "Run \"notmuch show\" with the given thread ID and display results."
  (interactive "sNotmuch show: ")
  (let ((buffer (get-buffer-create (concat "*notmuch-show-" thread-id "*"))))
    (switch-to-buffer buffer)
    (notmuch-show-mode)
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (if proc
	  (error "notmuch search process already running for query `%s'" query)
	)
      (erase-buffer)
      (goto-char (point-min))
      (save-excursion
	(call-process "notmuch" nil t nil "show" thread-id)
	)
      )))

(defvar notmuch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'notmuch-search-archive-thread)
    (define-key map "f" 'notmuch-search-filter)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "s" 'notmuch-search)
    (define-key map "x" 'kill-this-buffer)
    (define-key map "\r" 'notmuch-search-show-thread)
    (define-key map "+" 'notmuch-search-add-tag)
    (define-key map "-" 'notmuch-search-remove-tag)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'notmuch-search-goto-last-thread)
    (define-key map "\M->" 'notmuch-search-goto-last-thread)
    map)
  "Keymap for \"notmuch search\" buffers.")
(fset 'notmuch-search-mode-map notmuch-search-mode-map)

(defun notmuch-search-goto-last-thread (&optional arg)
  "Move point to the last thread in the buffer."
  (interactive "^P")
  (end-of-buffer arg)
  (beginning-of-line))

;;;###autoload
(defun notmuch-search-mode ()
  "Major mode for handling the output of \"notmuch search\""
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'notmuch-search-query-string)
  (use-local-map notmuch-search-mode-map)
  (setq major-mode 'notmuch-search-mode
	mode-name "notmuch-search")
  (setq buffer-read-only t))

(defun notmuch-search-find-thread-id ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (re-search-forward "[a-fA-F0-9]*")
      (filter-buffer-substring beg (point)))))

(defun notmuch-search-markup-this-thread-id ()
  (beginning-of-line)
  (let ((beg (point)))
    (re-search-forward "[a-fA-F0-9]*")
    (forward-char)
    (overlay-put (make-overlay beg (point)) 'invisible 'notmuch-search)))

(defun notmuch-search-markup-thread-ids ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (notmuch-search-markup-this-thread-id)
      (next-line))))

(defun notmuch-search-hide-thread-ids ()
  (interactive)
  (add-to-invisibility-spec 'notmuch-search))

(defun notmuch-search-show-thread-ids ()
  (interactive)
  (remove-from-invisibility-spec 'notmuch-search))

(defun notmuch-search-show-thread ()
  (interactive)
  (notmuch-show (notmuch-search-find-thread-id)))

(defun notmuch-search-call-notmuch-process (&rest args)
  (let ((error-buffer (get-buffer-create "*Notmuch errors*")))
    (with-current-buffer error-buffer
	(erase-buffer))
    (if (eq (apply 'call-process "notmuch" nil error-buffer nil args) 0)
	(point)
      (progn
	(with-current-buffer error-buffer
	  (let ((beg (point-min))
		(end (- (point-max) 1)))
	    (error (buffer-substring beg end))
	    ))))))

(defun notmuch-search-set-tags (tags)
  (save-excursion
    (end-of-line)
    (re-search-backward "(")
    (forward-char)
    (let ((beg (point))
	  (inhibit-read-only t))
      (re-search-forward ")")
      (backward-char)
      (let ((end (point)))
	(delete-region beg end)
	(insert (mapconcat  'identity tags " "))))))

(defun notmuch-search-get-tags ()
  (save-excursion
    (end-of-line)
    (re-search-backward "(")
    (let ((beg (+ (point) 1)))
      (re-search-forward ")")
      (let ((end (- (point) 1)))
	(split-string (buffer-substring beg end))))))

(defun notmuch-search-add-tag (tag)
  (interactive "sTag to add: ")
  (notmuch-search-call-notmuch-process "tag" (concat "+" tag) (concat "thread:" (notmuch-search-find-thread-id)))
  (notmuch-search-set-tags (delete-dups (sort (cons tag (notmuch-search-get-tags)) 'string<))))

(defun notmuch-search-remove-tag (tag)
  (interactive "sTag to remove: ")
  (notmuch-search-call-notmuch-process "tag" (concat "-" tag) (concat "thread:" (notmuch-search-find-thread-id)))
  (notmuch-search-set-tags (delete tag (notmuch-search-get-tags))))

(defun notmuch-search-archive-thread ()
  (interactive)
  (notmuch-search-remove-tag "inbox"))

(defun notmuch-search (query)
  "Run \"notmuch search\" with the given query string and display results."
  (interactive "sNotmuch search: ")
  (let ((buffer (get-buffer-create (concat "*notmuch-search-" query "*"))))
    (switch-to-buffer buffer)
    (notmuch-search-mode)
    (set 'notmuch-search-query-string query)
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (if proc
	  (error "notmuch search process already running for query `%s'" query)
	)
      (erase-buffer)
      (goto-char (point-min))
      (save-excursion
	(call-process "notmuch" nil t nil "search" query)
	(notmuch-search-markup-thread-ids)
        ; A well-behaved program ends its output with a newline, but we
        ; don't actually want the blank line at the end of the file.
	(goto-char (point-max))
	(if (looking-at "^$")
	    (delete-backward-char 1)
	  )
	))))

(defun notmuch-search-filter (query)
  "Run \"notmuch search\" to refine the current search results.

A search string will be constructed by appending QUERY to the
current search string, and the results of \"notmuch search\" for
the combined query will be displayed."
  (interactive "sFilter search: ")
  (notmuch-search (concat notmuch-search-query-string " and " query)))

(defun notmuch ()
  "Run notmuch to display all mail with tag of 'inbox'"
  (interactive)
  (notmuch-search "tag:inbox"))

(provide 'notmuch)
