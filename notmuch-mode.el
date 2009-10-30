; A mode for running notmuch within emacs

;;;###autoload
(defun notmuch-search-mode ()
  "Major mode for handling the output of notmuch search"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'notmuch-search-mode
	mode-name "notmuch-search")
  (setq buffer-read-only t))

(defun notmuch ()
  "Run notmuch to display all mail with tag of 'inbox'"
  (interactive)
  (require 'compile)
  (compilation-start "notmuch search tag:inbox" 'notmuch-search-mode))
