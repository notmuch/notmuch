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

(defvar notmuch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Keymap for \"notmuch search\" buffers.")
(fset 'notmuch-search-mode-map notmuch-search-mode-map)

;;;###autoload
(defun notmuch-search-mode ()
  "Major mode for handling the output of notmuch search"
  (interactive)
  (kill-all-local-variables)
  (use-local-map notmuch-search-mode-map)
  (setq major-mode 'notmuch-search-mode
	mode-name "notmuch-search")
  (setq buffer-read-only t))

(defun notmuch-search (query)
  "Run \"notmuch search\" with the given query string and display results."
  (interactive "sNotmuch search: ")
  (let ((buffer (get-buffer-create (concat "*notmuch-search-" query))))
    (switch-to-buffer buffer)
    (notmuch-search-mode)
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (if proc
	  (error "notmuch search process already running for query `%s'" query)
	)
      (erase-buffer)
      (beginning-of-buffer)
      (save-excursion
	(call-process "notmuch" nil t nil "search" query)
	)
      )))

(defun notmuch ()
  "Run notmuch to display all mail with tag of 'inbox'"
  (interactive)
  (notmuch-search "tag:inbox"))

(provide 'notmuch)
