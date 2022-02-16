#
# Copyright (c) 2010-2020 Notmuch Developers
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see https://www.gnu.org/licenses/ .

test_require_emacs () {
    local ret=0
    test_require_external_prereq "$TEST_EMACS" || ret=1
    test_require_external_prereq "$TEST_EMACSCLIENT" || ret=1
    test_require_external_prereq dtach || ret=1
    return $ret
}

# Deliver a message with emacs and add it to the database
#
# Uses emacs to generate and deliver a message to the mail store.
# Accepts arbitrary extra emacs/elisp functions to modify the message
# before sending, which is useful to doing things like attaching files
# to the message and encrypting/signing.
emacs_deliver_message () {
    local subject body smtp_dummy_pid smtp_dummy_port
    subject="$1"
    body="$2"
    shift 2
    # before we can send a message, we have to prepare the FCC maildir
    mkdir -p "$MAIL_DIR"/sent/{cur,new,tmp}
    # eval'ing smtp-dummy --background will set smtp_dummy_pid and -_port
    smtp_dummy_pid= smtp_dummy_port=
    eval `$TEST_DIRECTORY/smtp-dummy --background sent_message`
    test -n "$smtp_dummy_pid" || return 1
    test -n "$smtp_dummy_port" || return 1

    test_emacs \
	"(let ((message-send-mail-function 'message-smtpmail-send-it)
	       (mail-host-address \"example.com\")
	       (smtpmail-smtp-server \"localhost\")
	       (smtpmail-smtp-service \"${smtp_dummy_port}\"))
	   (notmuch-mua-mail)
	   (message-goto-to)
	   (insert \"test_suite@notmuchmail.org\nDate: 01 Jan 2000 12:00:00 -0000\")
	   (message-goto-subject)
	   (insert \"${subject}\")
	   (message-goto-body)
	   (insert \"${body}\")
	   $*
	   (let ((mml-secure-smime-sign-with-sender t)
		 (mml-secure-openpgp-sign-with-sender t))
	     (notmuch-mua-send-and-exit)))"
    # In case message was sent properly, client waits for confirmation
    # before exiting and resuming control here; therefore making sure
    # that server exits by sending (KILL) signal to it is safe.
    kill -9 $smtp_dummy_pid
    notmuch new >/dev/null
}

# Pretend to deliver a message with emacs. Really save it to a file
# and add it to the database
#
# Uses emacs to generate and deliver a message to the mail store.
# Accepts arbitrary extra emacs/elisp functions to modify the message
# before sending, which is useful to doing things like attaching files
# to the message and encrypting/signing.
#
# If any GNU-style long-arguments (like --quiet or --decrypt=true) are
# at the head of the argument list, they are sent directly to "notmuch
# new" after message delivery
emacs_fcc_message () {
    local nmn_args subject body
    nmn_args=''
    while [[ "$1" =~ ^-- ]]; do
	nmn_args="$nmn_args $1"
	shift
    done
    subject="$1"
    body="$2"
    shift 2
    # before we can send a message, we have to prepare the FCC maildir
    mkdir -p "$MAIL_DIR"/sent/{cur,new,tmp}

    test_emacs \
	"(let ((message-send-mail-function (lambda () t))
	       (mail-host-address \"example.com\"))
	   (notmuch-mua-mail)
	   (message-goto-to)
	   (insert \"test_suite@notmuchmail.org\nDate: 01 Jan 2000 12:00:00 -0000\")
	   (message-goto-subject)
	   (insert \"${subject}\")
	   (message-goto-body)
	   (insert \"${body}\")
	   $*
	   (let ((mml-secure-smime-sign-with-sender t)
		 (mml-secure-openpgp-sign-with-sender t))
	     (notmuch-mua-send-and-exit)))" || return 1
    notmuch new $nmn_args >/dev/null
}

test_emacs_expect_t () {
	local result
	test "$#" = 1 ||
	error "bug in the test script: not 1 parameter to test_emacs_expect_t"
	if [ -z "$inside_subtest" ]; then
		error "bug in the test script: test_emacs_expect_t without test_begin_subtest"
	fi

	# Run the test.
	if ! test_skip "$test_subtest_name"
	then
		test_emacs "(notmuch-test-run $1)" >/dev/null

		# Restore state after the test.
		exec 1>&6 2>&7		# Restore stdout and stderr
		inside_subtest=

		# test_emacs may update missing external prerequisites
		test_check_missing_external_prereqs_ "$test_subtest_name" && return

		# Report success/failure.
		result=$(cat OUTPUT)
		if [ "$result" = t ]
		then
			test_ok_
		else
			test_failure_ "${result}"
		fi
	else
		# Restore state after the (non) test.
		exec 1>&6 2>&7		# Restore stdout and stderr
		inside_subtest=
	fi
}

emacs_generate_script () {
	# Construct a little test script here for the benefit of the user,
	# (who can easily run "run_emacs" to get the same emacs environment
	# for investigating any failures).
	cat <<EOF >"$TMP_DIRECTORY/run_emacs"
#!/bin/sh
export PATH=$PATH
export NOTMUCH_CONFIG=$NOTMUCH_CONFIG

# Here's what we are using here:
#
# --quick		Use minimal customization. This implies --no-init-file,
#			--no-site-file and (emacs 24) --no-site-lisp
#
# --directory		Ensure that the local elisp sources are found
#
# --load		Force loading of notmuch.el and test-lib.el

exec ${TEST_EMACS} --quick \
	--directory "$NOTMUCH_BUILDDIR/emacs" --load notmuch.el \
	--directory "$NOTMUCH_SRCDIR/test" --load test-lib.el \
	"\$@"
EOF
	chmod a+x "$TMP_DIRECTORY/run_emacs"
}

test_emacs () {
	# test dependencies beforehand to avoid the waiting loop below
	test_require_emacs || return

	if [ -z "$EMACS_SERVER" ]; then
		emacs_tests="$NOTMUCH_SRCDIR/test/${this_test_bare}.el"
		if [ -f "$emacs_tests" ]; then
			load_emacs_tests="--eval '(load \"$emacs_tests\")'"
		else
			load_emacs_tests=
		fi
		server_name="notmuch-test-suite-$$"
		# start a detached session with an emacs server
		# user's TERM (or 'vt100' in case user's TERM is known dumb
		# or unknown) is given to dtach which assumes a minimally
		# VT100-compatible terminal -- and emacs inherits that
		TERM=$SMART_TERM dtach -n "$TEST_TMPDIR/emacs-dtach-socket.$$" \
			sh -c "stty rows 24 cols 80; exec '$TMP_DIRECTORY/run_emacs' \
				--no-window-system \
				$load_emacs_tests \
				--eval '(setq server-name \"$server_name\")' \
				--eval '(server-start)' \
				--eval '(orphan-watchdog $$)'" || return
		EMACS_SERVER="$server_name"
		# wait until the emacs server is up
		until test_emacs '()' >/dev/null 2>/dev/null; do
			sleep 1
		done
	fi

	# Clear test-output output file.  Most Emacs tests end with a
	# call to (test-output).  If the test code fails with an
	# exception before this call, the output file won't get
	# updated.  Since we don't want to compare against an output
	# file from another test, so start out with an empty file.
	rm -f OUTPUT
	touch OUTPUT

	${TEST_EMACSCLIENT} --socket-name="$EMACS_SERVER" --eval "(notmuch-test-progn $*)"
}

emacs_generate_script
