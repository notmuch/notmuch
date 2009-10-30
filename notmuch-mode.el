; A mode for running notmuch within emacs

(defun notmuch ()
  "Run notmuch to display all mail with tag of 'inbox'"
  (interactive)
  (require 'compile)
  (compilation-start "notmuch search tag:inbox"))
