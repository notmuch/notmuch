;;; test-lib.el --- auxiliary stuff for Notmuch Emacs tests
;;
;; Copyright © Carl Worth
;; Copyright © David Edmondson
;;
;; This file is part of Notmuch test suit.
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
;;
;; Authors: Dmitry Kurochkin <dmitry.kurochkin@gmail.com>

;;; Code:

(require 'cl-lib)

;; Ensure that the dynamic variables that are defined by this library
;; are defined by the time that we let-bind them.  This is needed
;; because starting with Emacs 27 undeclared variables in evaluated
;; interactive code (such as our tests) use lexical scope.
(require 'smtpmail)

;; `read-file-name' by default uses `completing-read' function to read
;; user input.  It does not respect `standard-input' variable which we
;; use in tests to provide user input.  So replace it with a plain
;; `read' call.
(setq read-file-name-function (lambda (&rest _) (read)))

(defun notmuch-test-wait ()
  "Wait for process completion."
  (while (get-buffer-process (current-buffer))
    (accept-process-output nil 0.1)))

(defun test-output (&optional filename)
  "Save current buffer to file FILENAME.  Default FILENAME is OUTPUT."
  (notmuch-post-command)
  (write-region (point-min) (point-max) (or filename "OUTPUT")))

(defun test-visible-output (&optional filename)
  "Save visible text in current buffer to file FILENAME.  Default
FILENAME is OUTPUT."
  (notmuch-post-command)
  (let ((text (visible-buffer-string))
	;; Tests expect output in UTF-8 encoding
	(coding-system-for-write 'utf-8))
    (with-temp-file (or filename "OUTPUT") (insert text))))

(defun visible-buffer-string ()
  "Same as `buffer-string', but excludes invisible text and
removes any text properties."
  (visible-buffer-substring (point-min) (point-max)))

(defun visible-buffer-substring (start end)
  "Same as `buffer-substring-no-properties', but excludes
invisible text."
  (let (str)
    (while (< start end)
      (let ((next-pos (next-char-property-change start end)))
	(unless (invisible-p start)
	  (setq str (concat str (buffer-substring-no-properties
				 start next-pos))))
	(setq start next-pos)))
    str))

;; process-attributes is not defined everywhere, so define an
;; alternate way to test if a process still exists.

(defun test-process-running (pid)
  (= 0
   (signal-process pid 0)))

(defun orphan-watchdog-check (pid)
  "Periodically check that the process with id PID is still
running, quit if it terminated."
  (unless (test-process-running pid)
    (kill-emacs)))

(defun orphan-watchdog (pid)
  "Initiate orphan watchdog check."
  (run-at-time 60 60 'orphan-watchdog-check pid))

(defvar notmuch-hello-mode-hook-counter -100
  "Tests that care about this counter must let-bind it to 0.")
(add-hook 'notmuch-hello-mode-hook
	  (lambda () (cl-incf notmuch-hello-mode-hook-counter)))

(defvar notmuch-hello-refresh-hook-counter -100
  "Tests that care about this counter must let-bind it to 0.")
(add-hook 'notmuch-hello-refresh-hook
	  (lambda () (cl-incf notmuch-hello-refresh-hook-counter)))

(defvar notmuch-test-tag-hook-output nil)
(defun notmuch-test-tag-hook () (push (cons query tag-changes) notmuch-test-tag-hook-output))

(defun notmuch-test-mark-links ()
  "Enclose links in the current buffer with << and >>."
  ;; Links are often created by jit-lock functions
  (jit-lock-fontify-now)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (let ((button))
	(while (setq button (next-button (point)))
	  (goto-char (button-start button))
	  (insert "<<")
	  (goto-char (button-end button))
	  (insert ">>"))))))

(defmacro notmuch-test-run (&rest body)
  "Evaluate a BODY of test expressions and output the result."
  `(with-temp-buffer
     (let ((buffer (current-buffer))
	   (result (progn ,@body)))
       (switch-to-buffer buffer)
       (insert (if (stringp result)
		   result
		 (prin1-to-string result)))
       (test-output))))

(defun notmuch-test-report-unexpected (output expected)
  "Report that the OUTPUT does not match the EXPECTED result."
  (concat "Expect:\t" (prin1-to-string expected) "\n"
	  "Output:\t" (prin1-to-string output) "\n"))

(defun notmuch-test-expect-equal (output expected)
  "Compare OUTPUT with EXPECTED. Report any discrepancies."
  (cond
   ((equal output expected)
    t)
   ((and (listp output)
	 (listp expected))
    ;; Reporting the difference between two lists is done by
    ;; reporting differing elements of OUTPUT and EXPECTED
    ;; pairwise. This is expected to make analysis of failures
    ;; simpler.
    (apply #'concat (cl-loop for o in output
			     for e in expected
			     if (not (equal o e))
			     collect (notmuch-test-report-unexpected o e))))
   (t
    (notmuch-test-report-unexpected output expected))))

(defun notmuch-post-command ()
  (run-hooks 'post-command-hook))

(defmacro notmuch-test-progn (&rest body)
  (cons 'progn
	(mapcar
	 (lambda (x) `(prog1 ,x (notmuch-post-command)))
	 body)))

;; For historical reasons, we hide deleted tags by default in the test
;; suite
(setq notmuch-tag-deleted-formats
      '((".*" nil)))

;; Also for historical reasons, we set the fcc handler to file not
;; insert.

(setq notmuch-maildir-use-notmuch-insert nil)

;; force a common html renderer, to avoid test variations between
;; environments

(setq mm-text-html-renderer 'html2text)
