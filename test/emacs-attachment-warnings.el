(require 'notmuch-mua)

(defun attachment-check-test (&optional fn)
  "Test `notmuch-mua-attachment-check' using a message where optional FN is evaluated.

Return `t' if the message would be sent, otherwise `nil'"
  (notmuch-mua-mail)
  (message-goto-body)
  (when fn
    (funcall fn))
  (prog1
      (condition-case nil
	  ;; Force `y-or-n-p' to always return `nil', as if the user
	  ;; pressed "n".
	  (letf (((symbol-function 'y-or-n-p) (lambda (&rest args) nil)))
	    (notmuch-mua-attachment-check)
	    t)
	('error nil))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(defvar attachment-check-tests
  '(
    ;; These are all okay:
    (t)
    (t . (lambda () (insert "Nothing is a-tt-a-ch-ed!\n")))
    (t . (lambda ()
	   (insert "Here is an attachment:\n")
	   (insert "<#part filename=\"foo\" />\n")))
    (t . (lambda () (insert "<#part filename=\"foo\" />\n")))
    (t . (lambda ()
	   ;; "attachment" is only mentioned in a quoted section.
	   (insert "> I sent you an attachment!\n")
	   ;; Code in `notmuch-mua-attachment-check' avoids matching on
	   ;; "attachment" in a quoted section of the message by looking at
	   ;; fontification properties. For fontification to happen we need to
	   ;; allow some time for redisplay.
	   (sit-for 0.01)))

    ;; These should not be okay:
    (nil . (lambda () (insert "Here is an attachment:\n")))
    (nil . (lambda ()
	     ;; "attachment" is mentioned in both a quoted section and
	     ;; outside of it.
	     (insert "> I sent you an attachment!\n")
	     (insert "The attachment was missing!\n")
	     ;; Code in `notmuch-mua-attachment-check' avoids matching
	     ;; on "attachment" in a quoted section of the message by
	     ;; looking at fontification properties. For fontification
	     ;; to happen we need to allow some time for redisplay.
	     (sit-for 0.01)))
    ))

(defun notmuch-test-attachment-warning-1 ()
  (let (output expected)
    (mapcar (lambda (test)
	      (let* ((expect (car test))
		     (body (cdr test))
		     (result (attachment-check-test body)))
		(push expect expected)
		(push (if (eq result expect)
			  result
			;; In the case of a failure, include the test
			;; details to make it simpler to debug.
			(format "%S <-- %S" result body))
		      output)))
	    attachment-check-tests)
    (notmuch-test-expect-equal output expected)))
