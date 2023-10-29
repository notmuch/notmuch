(require 'cl-lib)
(require 'notmuch-mua)

(defun subject-check-test (&optional fn)
  "Test `notmuch-mua-subject-check'.
Optionally, evaluate FN before doing the test.

Return t if the message would be sent, and nil otherwise."
  (notmuch-mua-mail)
  (message-goto-subject)
  (when fn
    (funcall fn))
  (prog1
      (condition-case nil
	  ;; Force `y-or-n-p' to always return `nil', as if the user
	  ;; pressed "n".
	  (cl-letf (((symbol-function 'y-or-n-p)
		     (lambda (&rest args) nil)))
	    (notmuch-mua-subject-check)
	    t)
	('error nil))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(defvar subject-check-tests
  '(;; These are okay.
    (t . (lambda () (insert "something")))
    ;; These should not be okay.
    (nil)
    (nil . (lambda () (insert "         ")))
    (nil . (lambda () (insert "		  	")))
    (nil . (lambda () (insert " ")))	; NON-BREAKING SPACE
    ))

(defun notmuch-test-subject-warning-1 ()
  (let (output expected)
    (dolist (test subject-check-tests)
      (let* ((expect (car test))
	     (body (cdr test))
	     (result (subject-check-test body)))
	(push expect expected)
	(push (if (eq result expect)
		  result
		;; In the case of a failure, include the test
		;; details to make it simpler to debug.
		(format "%S <-- %S" result body))
	      output)))
    (notmuch-test-expect-equal output expected)))
