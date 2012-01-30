(defun notmuch-test-address-cleaning-1 ()
  (notmuch-test-expect-equal (notmuch-show-clean-address "dme@dme.org")
			"dme@dme.org"))

(defun notmuch-test-address-cleaning-2 ()
  (let* ((input '("foo@bar.com"
		  "<foo@bar.com>"
		  "Foo Bar <foo@bar.com>"
		  "foo@bar.com <foo@bar.com>"
		  "\"Foo Bar\" <foo@bar.com>"))
	 (expected '("foo@bar.com"
		     "foo@bar.com"
		     "Foo Bar <foo@bar.com>"
		     "foo@bar.com"
		     "Foo Bar <foo@bar.com>"))
	 (output (mapcar #'notmuch-show-clean-address input)))
    (notmuch-test-expect-equal output expected)))

(defun notmuch-test-address-cleaning-3 ()
  (let* ((input '("ДБ <db-uknot@stop.me.uk>"
		  "foo (at home) <foo@bar.com>"
		  "foo [at home] <foo@bar.com>"
		  "Foo Bar"
		  "'Foo Bar' <foo@bar.com>"
		  "\"'Foo Bar'\" <foo@bar.com>"
		  "'\"Foo Bar\"' <foo@bar.com>"
		  "'\"'Foo Bar'\"' <foo@bar.com>"
		  "Fred Dibna \\[extraordinaire\\] <fred@dibna.com>"))
	 (expected '("ДБ <db-uknot@stop.me.uk>"
		     "foo (at home) <foo@bar.com>"
		     "foo [at home] <foo@bar.com>"
		     "Foo Bar"
		     "Foo Bar <foo@bar.com>"
		     "Foo Bar <foo@bar.com>"
		     "Foo Bar <foo@bar.com>"
		     "Foo Bar <foo@bar.com>"
		     "Fred Dibna [extraordinaire] <fred@dibna.com>"))
	 (output (mapcar #'notmuch-show-clean-address input)))
    (notmuch-test-expect-equal output expected)))
