;; test-lib.el --- auxiliary stuff for Notmuch Emacs tests.
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
;; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Authors: Dmitry Kurochkin <dmitry.kurochkin@gmail.com>

;; avoid crazy 10-column default of --batch
(set-frame-width (window-frame (get-buffer-window)) 80)

(defun notmuch-test-wait ()
  "Wait for process completion."
  (while (get-buffer-process (current-buffer))
    (sleep-for 0.1)))

(defun test-output (&optional filename)
  "Save current buffer to file FILENAME.  Default FILENAME is OUTPUT."
  (write-region (point-min) (point-max) (or filename "OUTPUT")))

(defun test-visible-output (&optional filename)
  "Save visible text in current buffer to file FILENAME.  Default
FILENAME is OUTPUT."
  (let ((text (visible-buffer-string)))
    (with-temp-file (or filename "OUTPUT") (insert text))))

(defun visible-buffer-string ()
  "Same as `buffer-string', but excludes invisible text."
  (visible-buffer-substring (point-min) (point-max)))

(defun visible-buffer-substring (start end)
  "Same as `buffer-substring', but excludes invisible text."
  (let (str)
    (while (< start end)
      (let ((next-pos (next-char-property-change start end)))
	(when (not (invisible-p start))
	  (setq str (concat str (buffer-substring start next-pos))))
	(setq start next-pos)))
    str))
