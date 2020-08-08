;;; notmuch-compat.el --- compatibility functions for earlier versions of emacs
;;
;; The functions in this file are copied from more modern versions of
;; emacs and are Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2017
;; Free Software Foundation, Inc.
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

;;; Code:

;; emacs master has a bugfix for folding long headers when sending
;; messages. Include the fix for earlier versions of emacs. To avoid
;; interfering with gnus we only run the hook when called from
;; notmuch-message-mode.

(declare-function mail-header-fold-field "mail-parse" nil)

(defun notmuch-message--fold-long-headers ()
  (when (eq major-mode 'notmuch-message-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (when (and (looking-at "[^:]+:")
		 (> (- (line-end-position) (point)) 998))
	(mail-header-fold-field))
      (forward-line 1))))

(unless (fboundp 'message--fold-long-headers)
  (add-hook 'message-header-hook 'notmuch-message--fold-long-headers))

;; End of compatibility functions

(provide 'notmuch-compat)

;;; notmuch-compat.el ends here
