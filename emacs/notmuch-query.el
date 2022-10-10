;;; notmuch-query.el --- provide an emacs api to query notmuch  -*- lexical-binding: t -*-
;;
;; Copyright © David Bremner
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
;;
;; Authors: David Bremner <david@tethera.net>

;;; Code:

(require 'notmuch-lib)

;;; Basic query function

(define-obsolete-function-alias
  'notmuch-query-get-threads
  #'notmuch--run-show
  "notmuch 0.37")

;;; Mapping functions across collections of messages

(defun notmuch-query-map-aux  (mapper function seq)
  "Private function to do the actual mapping and flattening."
  (cl-mapcan (lambda (tree)
	       (funcall mapper function tree))
	     seq))

(defun notmuch-query-map-threads (fn threads)
  "Apply function FN to every thread in THREADS.
Flatten results to a list.  See the function
`notmuch-query-get-threads' for more information."
  (notmuch-query-map-aux 'notmuch-query-map-forest fn threads))

(defun notmuch-query-map-forest (fn forest)
  "Apply function FN to every message in FOREST.
Flatten results to a list.  See the function
`notmuch-query-get-threads' for more information."
  (notmuch-query-map-aux 'notmuch-query-map-tree fn forest))

(defun notmuch-query-map-tree (fn tree)
  "Apply function FN to every message in TREE.
Flatten results to a list.  See the function
`notmuch--run-show' for more information."
  (cons (funcall fn (car tree))
	(notmuch-query-map-forest fn (cadr tree))))

;;; Predefined queries

(defun notmuch-query-get-message-ids (&rest search-terms)
  "Return a list of message-ids of messages that match SEARCH-TERMS."
  (notmuch-query-map-threads
   (lambda (msg) (plist-get msg :id))
   (notmuch--run-show search-terms)))

;;; Everything in this library is obsolete
(dolist (fun '(map-aux map-threads map-forest map-tree get-message-ids))
  (make-obsolete (intern (format "notmuch-query-%s" fun)) nil "notmuch 0.37"))

(provide 'notmuch-query)

;;; notmuch-query.el ends here
