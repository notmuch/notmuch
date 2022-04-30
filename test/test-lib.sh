#
# Copyright (c) 2005 Junio C Hamano
# Copyright (c) 2010 Notmuch Developers
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

if [ ${BASH_VERSINFO[0]} -lt 4 ]; then
    echo "Error: The notmuch test suite requires a bash version >= 4.0"
    echo "due to use of associative arrays within the test suite."
    echo "Please try again with a newer bash (or help us fix the"
    echo "test suite to be more portable). Thanks."
    exit 1
fi

# Make sure echo builtin does not expand backslash-escape sequences by default.
shopt -u xpg_echo

# Ensure NOTMUCH_SRCDIR and NOTMUCH_BUILDDIR are set.
. $(dirname "$0")/export-dirs.sh || exit 1

# It appears that people try to run tests without building...
if [[ ! -x "$NOTMUCH_BUILDDIR/notmuch" ]]; then
	echo >&2 'You do not seem to have built notmuch yet.'
	exit 1
fi

this_test=${0##*/}
this_test=${this_test%.sh}
this_test_bare=${this_test#T[0-9][0-9][0-9]-}

# if --tee was passed, write the output not only to the terminal, but
# additionally to the file test-results/$BASENAME.out, too.
case "$GIT_TEST_TEE_STARTED, $* " in
done,*)
	# do not redirect again
	;;
*' --tee '*|*' --va'*)
	mkdir -p test-results
	BASE=test-results/$this_test
	(GIT_TEST_TEE_STARTED=done "$BASH" "$0" "$@" 2>&1;
	 echo $? > $BASE.exit) | tee $BASE.out
	test "$(cat $BASE.exit)" = 0
	exit
	;;
esac

# STDIN from /dev/null. EOF for readers (and ENOTTY for tty related ioctls).
exec </dev/null

# Save STDOUT to fd 6 and STDERR to fd 7.
exec 6>&1 7>&2
# Make xtrace debugging (when used) use redirected STDERR, with verbose lead:
BASH_XTRACEFD=7
export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'

. "$NOTMUCH_SRCDIR/test/test-vars.sh" || exit 1

add_gnupg_home () {
    [ -e "${GNUPGHOME}/gpg.conf" ] && return
    _gnupg_exit () { gpgconf --kill all 2>/dev/null || true; }
    at_exit_function _gnupg_exit
    mkdir -p -m 0700 "$GNUPGHOME"
    gpg --no-tty --import <$NOTMUCH_SRCDIR/test/gnupg-secret-key.asc >"$GNUPGHOME"/import.log 2>&1
    test_debug "cat $GNUPGHOME/import.log"
    if (gpg --quick-random --version >/dev/null 2>&1) ; then
	echo quick-random >> "$GNUPGHOME"/gpg.conf
    elif (gpg --debug-quick-random --version >/dev/null 2>&1) ; then
	echo debug-quick-random >> "$GNUPGHOME"/gpg.conf
    fi
    echo no-emit-version >> "$GNUPGHOME"/gpg.conf

    # Change this if we ship a new test key
    FINGERPRINT="5AEAB11F5E33DCE875DDB75B6D92612D94E46381"
    SELF_USERID="Notmuch Test Suite <test_suite@notmuchmail.org> (INSECURE!)"
    SELF_EMAIL="test_suite@notmuchmail.org"
    printf '%s:6:\n' "$FINGERPRINT" | gpg --quiet --batch --no-tty --import-ownertrust
}

add_gpgsm_home () {
    test_require_external_prereq openssl

    local fpr
    [ -e "$GNUPGHOME/gpgsm.conf" ] && return
    _gnupg_exit () { gpgconf --kill all 2>/dev/null || true; }
    at_exit_function _gnupg_exit
    mkdir -p -m 0700 "$GNUPGHOME"
    gpgsm --batch --no-tty --no-common-certs-import --pinentry-mode=loopback --passphrase-fd 3 \
	  --disable-dirmngr --import  >"$GNUPGHOME"/import.log 2>&1 3<<<'' <$NOTMUCH_SRCDIR/test/smime/0xE0972A47.p12
    fpr=$(gpgsm --batch --with-colons --list-key test_suite@notmuchmail.org | awk -F: '/^fpr/ {print $10}')
    echo "$fpr S relax" >> "$GNUPGHOME/trustlist.txt"
    gpgsm --quiet --batch --no-tty --no-common-certs-import --disable-dirmngr --import < $NOTMUCH_SRCDIR/test/smime/ca.crt
    echo "4D:E0:FF:63:C0:E9:EC:01:29:11:C8:7A:EE:DA:3A:9A:7F:6E:C1:0D S" >> "$GNUPGHOME/trustlist.txt"
    printf '%s::1\n' include-certs disable-crl-checks | gpgconf --output /dev/null --change-options gpgsm
    gpgsm --batch --no-tty --no-common-certs-import --pinentry-mode=loopback --passphrase-fd 3 \
	      --disable-dirmngr --import "$NOTMUCH_SRCDIR/test/smime/bob.p12" >>"$GNUPGHOME"/import.log 2>&1 3<<<''
    test_debug "cat $GNUPGHOME/import.log"
}

# Each test should start with something like this, after copyright notices:
#
# test_description='Description of this test...
# This test checks if command xyzzy does the right thing...
# '
# . ./test-lib.sh || exit 1

color=maybe

while test "$#" -ne 0
do
	case "$1" in
	-d|--debug)
		debug=t; shift ;;
	-i|--immediate)
		immediate=t; shift ;;
	-h|--help)
		help=t; shift ;;
	-v|--verbose)
		verbose=t; shift ;;
	-q|--quiet)
		quiet=t; shift ;;
	--with-dashes)
		with_dashes=t; shift ;;
	--no-color)
		color=; shift ;;
	--no-python)
		# noop now...
		shift ;;
	--valgrind)
		valgrind=t; verbose=t; shift ;;
	--tee)
		shift ;; # was handled already
	*)
		echo "error: unknown test option '$1'" >&2; exit 1 ;;
	esac
done

if test -n "$debug"; then
	fmt_subtest () {
		printf -v $1 " %-4s" "[$((test_count - 1))]"
	}
else
	fmt_subtest () {
		printf -v $1 ''
	}
fi

test -n "$COLORS_WITHOUT_TTY" || [ -t 1 ] || color=

if [ -n "$color" ] && [ "$ORIGINAL_TERM" != 'dumb' ] &&
	tput -T "$ORIGINAL_TERM" -S <<<$'bold\nsetaf\nsgr0\n' >/dev/null 2>&1
then
	color=t
else
	color=
fi

if test -n "$color"
then
	# _tput run in subshell (``) only
	_tput () { exec tput -T "$ORIGINAL_TERM" "$@"; }
	unset BOLD RED GREEN BROWN SGR0
	say_color () {
		case "$1" in
			error)	b=${BOLD=`_tput bold`}
				c=${RED=`_tput setaf 1`}   ;; # bold red
			skip)	b=${BOLD=`_tput bold`}
				c=${GREEN=`_tput setaf 2`} ;; # bold green
			pass)	b= c=${GREEN=`_tput setaf 2`} ;; # green
			info)	b= c=${BROWN=`_tput setaf 3`} ;; # brown
			*) b= c=; test -n "$quiet" && return ;;
		esac
		f=$2
		shift 2
		sgr0=${SGR0=`_tput sgr0`}
		fmt_subtest st
		printf " ${b}${c}${f}${sgr0}${st}" "$@"
	}
else
	say_color() {
		test -z "$1" && test -n "$quiet" && return
		f=$2
		shift 2
		fmt_subtest st
		printf " ${f}${st}" "$@"
	}
fi

error () {
	say_color error "error: $*\n"
	GIT_EXIT_OK=t
	exit 1
}

say () {
	say_color info "$*"
}

test "${test_description}" != "" ||
error "Test script did not set test_description."

if test "$help" = "t"
then
	echo "Tests ${test_description}"
	exit 0
fi

test_description_printed=
print_test_description () {
	test -z "$test_description_printed" || return 0
	echo
	echo $this_test: "Testing ${test_description}"
	test_description_printed=1
}
if [ -z "$NOTMUCH_TEST_QUIET" ]
then
	print_test_description
fi

test_failure=0
test_count=0
test_fixed=0
test_broken=0
test_success=0

declare -a _exit_functions=()

at_exit_function () {
	_exit_functions=($1 ${_exit_functions[@]/$1})
}

rm_exit_function () {
	_exit_functions=(${_exit_functions[@]/$1})
}

_exit_common () {
	code=$?
	trap - EXIT
	set +ex
	for _fn in ${_exit_functions[@]}; do $_fn; done
	rm -rf "$TEST_TMPDIR"
}

trap_exit () {
	_exit_common
	if test -n "$GIT_EXIT_OK"
	then
		exit $code
	else
		exec >&6
		say_color error '%-6s' FATAL
		echo " $test_subtest_name"
		echo
		echo "Unexpected exit while executing $0. Exit code $code."
		exit 1
	fi
}

trap_signal () {
	_exit_common
	echo >&6 "FATAL: $0: interrupted by signal" $((code - 128))
	exit $code
}

die () {
	_exit_common
	exec >&6
	say_color error '%-6s' FATAL
	echo " $*"
	echo
	echo "Unexpected exit while executing $0."
	exit 1
}

trap 'trap_exit' EXIT
trap 'trap_signal' HUP INT TERM

# Add an existing, fixed corpus of email to the database.
#
# $1 is the corpus dir under corpora to add, using "default" if unset.
#
# The default corpus is based on about 50 messages from early in the
# history of the notmuch mailing list, which allows for reliably
# testing commands that need to operate on a not-totally-trivial
# number of messages.
add_email_corpus () {
    local corpus
    corpus=${1:-default}

    rm -rf ${MAIL_DIR}
    cp -a $NOTMUCH_SRCDIR/test/corpora/$corpus ${MAIL_DIR}
    notmuch new >/dev/null || die "'notmuch new' failed while adding email corpus"
}

test_begin_subtest () {
    if [ -n "$inside_subtest" ]; then
	exec 1>&6 2>&7		# Restore stdout and stderr
	error "bug in test script: Missing test_expect_equal in ${BASH_SOURCE[1]}:${BASH_LINENO[0]}"
    fi
    test_subtest_name="$1"
    test_reset_state_
    # Redirect test output to the previously prepared file descriptors
    # 3 and 4 (see below)
    if test "$verbose" != "t"; then exec 4>test.output 3>&4; fi
    exec >&3 2>&4
    inside_subtest=t
}

# Pass test if two arguments match
#
# Note: Unlike all other test_expect_* functions, this function does
# not accept a test name. Instead, the caller should call
# test_begin_subtest before calling this function in order to set the
# name.
test_expect_equal () {
	local output expected testname
	exec 1>&6 2>&7		# Restore stdout and stderr
	if [ -z "$inside_subtest" ]; then
		error "bug in the test script: test_expect_equal without test_begin_subtest"
	fi
	inside_subtest=
	test "$#" = 2 ||
	error "bug in the test script: not 2 parameters to test_expect_equal"

	output="$1"
	expected="$2"
	if ! test_skip "$test_subtest_name"
	then
		if [ "$output" = "$expected" ]; then
			test_ok_
		else
			testname=$this_test.$test_count
			echo "$expected" > $testname.expected
			echo "$output" > $testname.output
			test_failure_ "$(diff -u $testname.expected $testname.output)"
		fi
    fi
}

test_diff_file_ () {
    local file1 file2 testname basename1 basename2
    file1="$1"
    file2="$2"
    if ! test_skip "$test_subtest_name"
    then
	if diff -q "$file1" "$file2" >/dev/null ; then
	    test_ok_
	else
	    testname=$this_test.$test_count
	    basename1=`basename "$file1"`
	    basename2=`basename "$file2"`
	    cp "$file1" "$testname.$basename1"
	    cp "$file2" "$testname.$basename2"
	    test_failure_ "$(diff -u "$testname.$basename1" "$testname.$basename2")"
	fi
    fi
}

# Like test_expect_equal, but takes two filenames.
test_expect_equal_file () {
    exec 1>&6 2>&7		# Restore stdout and stderr
    if [ -z "$inside_subtest" ]; then
	error "bug in the test script: test_expect_equal_file without test_begin_subtest"
    fi
    inside_subtest=
    test "$#" = 2 ||
	error "bug in the test script: not 2 parameters to test_expect_equal_file"

    test_diff_file_ "$1" "$2"
}

# Like test_expect_equal_file, but compare the part of the two files after the first blank line
test_expect_equal_message_body () {
    exec 1>&6 2>&7		# Restore stdout and stderr
    if [ -z "$inside_subtest" ]; then
	error "bug in the test script: test_expect_equal_file without test_begin_subtest"
    fi
    test "$#" = 2 ||
	error "bug in the test script: not 2 parameters to test_expect_equal_file"

    expected=$(sed '1,/^$/d' "$1")
    output=$(sed '1,/^$/d' "$2")
    test_expect_equal "$expected" "$output"
}

# Like test_expect_equal, but takes two filenames. Fails if either is empty
test_expect_equal_file_nonempty () {
    exec 1>&6 2>&7		# Restore stdout and stderr
    if [ -z "$inside_subtest" ]; then
	error "bug in the test script: test_expect_equal_file_nonempty without test_begin_subtest"
    fi
    inside_subtest=
    test "$#" = 2 ||
	error "bug in the test script: not 2 parameters to test_expect_equal_file_nonempty"

    for file in "$1" "$2"; do
	if [ ! -s "$file" ]; then
	    test_failure_ "Missing or zero length file: $file"
	    return $?
	fi
    done

    test_diff_file_ "$1" "$2"
}

# Like test_expect_equal, but arguments are JSON expressions to be
# canonicalized before diff'ing.  If an argument cannot be parsed, it
# is used unchanged so that there's something to diff against.
test_expect_equal_json () {
    local script output expected
    # The test suite forces LC_ALL=C, but this causes Python 3 to
    # decode stdin as ASCII.  We need to read JSON in UTF-8, so
    # override Python's stdio encoding defaults.
    script='import json, sys; json.dump(json.load(sys.stdin), sys.stdout, sort_keys=True, indent=4)'
    output=$(echo "$1" | PYTHONIOENCODING=utf-8 $NOTMUCH_PYTHON -c "$script" \
	|| echo "$1")
    expected=$(echo "$2" | PYTHONIOENCODING=utf-8 $NOTMUCH_PYTHON -c "$script" \
	|| echo "$2")
    shift 2
    test_expect_equal "$output" "$expected" "$@"
}

# Ensure that the argument is valid JSON data.
test_valid_json () {
    PYTHONIOENCODING=utf-8 $NOTMUCH_PYTHON -c "import sys, json; json.load(sys.stdin)" <<<"$1"
    test_expect_equal "$?" 0
}

# Sort the top-level list of JSON data from stdin.
test_sort_json () {
    PYTHONIOENCODING=utf-8 $NOTMUCH_PYTHON -c \
	"import sys, json; json.dump(sorted(json.load(sys.stdin)),sys.stdout)"
}

# test for json objects:
# read the source of test/json_check_nodes.py (or the output when
# invoking it without arguments) for an explanation of the syntax.
test_json_nodes () {
	local output
	exec 1>&6 2>&7		# Restore stdout and stderr
	if [ -z "$inside_subtest" ]; then
		error "bug in the test script: test_json_eval without test_begin_subtest"
	fi
	inside_subtest=
	test "$#" > 0 ||
	    error "bug in the test script: test_json_nodes needs at least 1 parameter"

	if ! test_skip "$test_subtest_name"
	then
	    output=$(PYTHONIOENCODING=utf-8 $NOTMUCH_PYTHON -B "$NOTMUCH_SRCDIR"/test/json_check_nodes.py "$@")
		if [ "$?" = 0 ]
		then
			test_ok_
		else
			test_failure_ "$output"
		fi
	fi
}

NOTMUCH_NEW () {
    notmuch new "${@}" | grep -v -E -e '^Processed [0-9]*( total)? file|Found [0-9]* total file'
}

NOTMUCH_DUMP_TAGS () {
    # this relies on the default format being batch-tag, otherwise some tests will break
    notmuch dump --include=tags "${@}" | sed '/^#/d' | sort
}

notmuch_drop_mail_headers () {
    $NOTMUCH_PYTHON -c '
import email, sys
msg = email.message_from_file(sys.stdin)
for hdr in sys.argv[1:]: del msg[hdr]
print(msg.as_string(False))
' "$@"
}

notmuch_debug_sanitize () {
    grep -v '^D.:'
}

notmuch_exception_sanitize () {
    perl -pe 's,(A Xapian exception occurred at) .*?([^/]*[.]cc?):([0-9]*),\1 \2:XXX,'
}

notmuch_search_sanitize () {
    notmuch_debug_sanitize | perl -pe 's/("?thread"?: ?)("?)................("?)/\1\2XXX\3/'
}

notmuch_search_files_sanitize () {
    notmuch_dir_sanitize |  sed 's/msg-[0-9][0-9][0-9]/msg-XXX/'
}

notmuch_dir_sanitize () {
    sed -e "s,$MAIL_DIR,MAIL_DIR," -e "s,${PWD},CWD,g" "$@"
}

NOTMUCH_SHOW_FILENAME_SQUELCH='s,filename:.*/mail,filename:/XXX/mail,'
notmuch_show_sanitize () {
    sed -e "$NOTMUCH_SHOW_FILENAME_SQUELCH"
}
notmuch_show_sanitize_all () {
    notmuch_debug_sanitize | \
    sed \
	-e 's| filename:.*| filename:XXXXX|' \
	-e 's| id:[^ ]* | id:XXXXX |' | \
	notmuch_date_sanitize
}

notmuch_json_show_sanitize () {
    sed \
	-e 's|"id": "[^"]*",|"id": "XXXXX",|g' \
	-e 's|"Date": "Fri, 05 Jan 2001 [^"]*0000"|"Date": "GENERATED_DATE"|g' \
	-e 's|"filename": "signature.asc",||g' \
	-e 's|"filename": \["/[^"]*"\],|"filename": \["YYYYY"\],|g' \
	-e 's|"timestamp": 97.......|"timestamp": 42|g' \
	-e 's|"content-length": [1-9][0-9]*|"content-length": "NONZERO"|g'
}

notmuch_emacs_error_sanitize () {
    local command
    command=$1
    shift
    for file in "$@"; do
	echo "=== $file ==="
	notmuch_debug_sanitize < "$file"
    done | sed \
	-e '/^$/d' \
	-e '/^\[.*\]$/d' \
	-e "s|^\(command: \)\{0,1\}/.*/$command|\1YYY/$command|"
}

notmuch_date_sanitize () {
    sed \
	-e 's/^Date: Fri, 05 Jan 2001 .*0000/Date: GENERATED_DATE/'
}

notmuch_uuid_sanitize () {
    sed 's/[0-9a-f]\{8\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{12\}/UUID/g'
}

notmuch_built_with_sanitize () {
    sed 's/^built_with[.]\(.*\)=.*$/built_with.\1=something/'
}

notmuch_config_sanitize () {
    notmuch_dir_sanitize | notmuch_built_with_sanitize
}

notmuch_show_part () {
    awk '/^\014part}/{ f=0 }; { if (f) { print $0 } } /^\014part{ ID: '"$1"'/{ f=1 }'
}

# End of notmuch helper functions

# Use test_set_prereq to tell that a particular prerequisite is available.
#
# The prerequisite can later be checked for by using test_have_prereq.
#
# The single parameter is the prerequisite tag (a simple word, in all
# capital letters by convention).

test_set_prereq () {
	satisfied="$satisfied$1 "
}
satisfied=" "

test_have_prereq () {
	case $satisfied in
	*" $1 "*)
		: yes, have it ;;
	*)
		! : nope ;;
	esac
}

declare -A test_missing_external_prereq_
declare -A test_subtest_missing_external_prereq_

# declare prerequisite for the given external binary
test_declare_external_prereq () {
	local binary
	binary="$1"
	test "$#" = 2 && name=$2 || name="$binary(1)"

	if ! hash $binary 2>/dev/null; then
		test_missing_external_prereq_["${binary}"]=t
		eval "
$binary () {
	test_subtest_missing_external_prereq_[\"${name}\"]=t
	false
}"
	fi
}

# You are not expected to call test_ok_ and test_failure_ directly, use
# the text_expect_* functions instead.

test_ok_ () {
	if test "$test_subtest_known_broken_" = "t"; then
		test_known_broken_ok_
		return
	fi
	test_success=$(($test_success + 1))
	if test -n "$NOTMUCH_TEST_QUIET"; then
		return 0
	fi
	say_color pass "%-6s" "PASS"
	echo " $test_subtest_name"
}

test_failure_ () {
	print_test_description
	if test "$test_subtest_known_broken_" = "t"; then
		test_known_broken_failure_ "$@"
		return
	fi
	test_failure=$(($test_failure + 1))
	test_failure_message_ "FAIL" "$test_subtest_name" "$@"
	test "$immediate" = "" || { GIT_EXIT_OK=t; exit 1; }
	return 1
}

test_failure_message_ () {
	say_color error "%-6s" "$1"
	echo " $2"
	shift 2
	if [ "$#" != "0" ]; then
		echo "$@" | sed -e 's/^/	/'
	fi
	if test "$verbose" != "t"; then cat test.output; fi
}

test_known_broken_ok_ () {
	test_reset_state_
	test_fixed=$(($test_fixed+1))
	say_color pass "%-6s" "FIXED"
	echo " $test_subtest_name"
}

test_known_broken_failure_ () {
	test_reset_state_
	test_broken=$(($test_broken+1))
	if [ -z "$NOTMUCH_TEST_QUIET" ]; then
		test_failure_message_ "BROKEN" "$test_subtest_name" "$@"
	else
		test_failure_message_ "BROKEN" "$test_subtest_name"
	fi
	return 1
}

test_debug () {
	test "$debug" = "" || eval "$1"
}

test_run_ () {
	test_cleanup=:
	if test "$verbose" != "t"; then exec 4>test.output 3>&4; fi
	eval >&3 2>&4 "$1"
	eval_ret=$?
	eval >&3 2>&4 "$test_cleanup"
	return 0
}

test_skip () {
	test_count=$(($test_count+1))
	to_skip=
	for skp in $NOTMUCH_SKIP_TESTS
	do
		case $this_test.$test_count in
		$skp)
			to_skip=t
			break
		esac
		case $this_test_bare.$test_count in
		$skp)
			to_skip=t
			break
		esac
	done
	case "$to_skip" in
	t)
		test_report_skip_ "$@"
		;;
	*)
		test_check_missing_external_prereqs_ "$@"
		;;
	esac
}

test_check_missing_external_prereqs_ () {
	if [[ ${#test_subtest_missing_external_prereq_[@]} != 0 ]]; then
		say_color skip >&1 "missing prerequisites: "
		echo ${!test_subtest_missing_external_prereq_[@]} >&1
		test_report_skip_ "$@"
	else
		false
	fi
}

test_report_skip_ () {
	test_reset_state_
	say_color skip >&3 "skipping test:"
	echo " $@" >&3
	say_color skip "%-6s" "SKIP"
	echo " $1"
}

test_subtest_known_broken () {
	test_subtest_known_broken_=t
}

test_expect_success () {
	exec 1>&6 2>&7		# Restore stdout and stderr
	if [ -z "$inside_subtest" ]; then
		error "bug in the test script: test_expect_success without test_begin_subtest"
	fi
	inside_subtest=
	test "$#" = 1 ||
	error "bug in the test script: not 1 parameters to test_expect_success"

	if ! test_skip "$test_subtest_name"
	then
		test_run_ "$1"
		run_ret="$?"
		# test_run_ may update missing external prerequisites
		test_check_missing_external_prereqs_ "$test_subtest_name" ||
		if [ "$run_ret" = 0 -a "$eval_ret" = 0 ]
		then
			test_ok_
		else
			test_failure_ "$1"
		fi
	fi
}

test_expect_code () {
	exec 1>&6 2>&7		# Restore stdout and stderr
	if [ -z "$inside_subtest" ]; then
		error "bug in the test script: test_expect_code without test_begin_subtest"
	fi
	inside_subtest=
	test "$#" = 2 ||
	error "bug in the test script: not 2 parameters to test_expect_code"

	if ! test_skip "$test_subtest_name"
	then
		test_run_ "$2"
		run_ret="$?"
		# test_run_ may update missing external prerequisites,
		test_check_missing_external_prereqs_ "$test_subtest_name" ||
		if [ "$run_ret" = 0 -a "$eval_ret" = "$1" ]
		then
			test_ok_
		else
			test_failure_ "exit code $eval_ret, expected $1" "$2"
		fi
	fi
}

# This is not among top-level (test_expect_success)
# but is a prefix that can be used in the test script, like:
#
#	test_expect_success 'complain and die' '
#	    do something &&
#	    do something else &&
#	    test_must_fail git checkout ../outerspace
#	'
#
# Writing this as "! git checkout ../outerspace" is wrong, because
# the failure could be due to a segv.  We want a controlled failure.

test_must_fail () {
	"$@"
	test $? -gt 0 -a $? -le 129 -o $? -gt 192
}

# test_cmp is a helper function to compare actual and expected output.
# You can use it like:
#
#	test_expect_success 'foo works' '
#		echo expected >expected &&
#		foo >actual &&
#		test_cmp expected actual
#	'
#
# This could be written as either "cmp" or "diff -u", but:
# - cmp's output is not nearly as easy to read as diff -u
# - not all diff versions understand "-u"

test_cmp () {
	$GIT_TEST_CMP "$@"
}

# This function can be used to schedule some commands to be run
# unconditionally at the end of the test to restore sanity:
#
#	test_expect_success 'test core.capslock' '
#		git config core.capslock true &&
#		test_when_finished "git config --unset core.capslock" &&
#		hello world
#	'
#
# That would be roughly equivalent to
#
#	test_expect_success 'test core.capslock' '
#		git config core.capslock true &&
#		hello world
#		git config --unset core.capslock
#	'
#
# except that the greeting and config --unset must both succeed for
# the test to pass.

test_when_finished () {
	test_cleanup="{ $*
		} && (exit \"\$eval_ret\"); eval_ret=\$?; $test_cleanup"
}

test_done () {
	GIT_EXIT_OK=t
	test_results_dir="$TEST_DIRECTORY/test-results"
	mkdir -p "$test_results_dir"
	test_results_path="$test_results_dir/$this_test"

	printf %s\\n \
		"success $test_success" \
		"fixed $test_fixed" \
		"broken $test_broken" \
		"failed $test_failure" \
		"total $test_count" \
	    > $test_results_path

	[ -n "$EMACS_SERVER" ] && test_emacs '(kill-emacs)'

	if [ "$test_failure" = "0" ]; then
	    if [ "$test_broken" = "0" ]; then
		rm -rf "$remove_tmp"
	    fi
	    exit 0
	else
	    exit 1
	fi
}

test_python () {
    # Note: if there is need to print debug information from python program,
    # use stdout = os.fdopen(6, 'w') or stderr = os.fdopen(7, 'w')
    PYTHONPATH="$NOTMUCH_BUILDDIR/bindings/python-cffi/build/stage:$NOTMUCH_SRCDIR/bindings/python${PYTHONPATH:+:$PYTHONPATH}" \
	$NOTMUCH_PYTHON -B - > OUTPUT
}

test_C () {
    local exec_file test_file
    exec_file="test${test_count}"
    test_file="${exec_file}.c"
    cat > ${test_file}
    ${TEST_CC} ${TEST_CFLAGS} -I${NOTMUCH_SRCDIR}/test -I${NOTMUCH_SRCDIR}/lib -o ${exec_file} ${test_file} -L${NOTMUCH_BUILDDIR}/lib/ -lnotmuch -ltalloc
    echo "== stdout ==" > OUTPUT.stdout
    echo "== stderr ==" > OUTPUT.stderr
    ./${exec_file} "$@" 1>>OUTPUT.stdout 2>>OUTPUT.stderr
    notmuch_dir_sanitize OUTPUT.stdout OUTPUT.stderr | notmuch_exception_sanitize | notmuch_debug_sanitize > OUTPUT
}

make_shim () {
    local base_name test_file shim_file
    base_name="$1"
    test_file="${base_name}.c"
    shim_file="${base_name}.so"
    cat > ${test_file}
    ${TEST_CC} ${TEST_CFLAGS} ${TEST_SHIM_CFLAGS} -I${NOTMUCH_SRCDIR}/test -I${NOTMUCH_SRCDIR}/lib -o ${shim_file} ${test_file} ${TEST_SHIM_LDFLAGS}
}

notmuch_with_shim () {
    local base_name shim_file
    base_name="$1"
    shift
    shim_file="${base_name}.so"
    LD_PRELOAD=${LD_PRELOAD:+:$LD_PRELOAD}:./${shim_file} notmuch-shared "$@"
}

# Creates a script that counts how much time it is executed and calls
# notmuch.  $notmuch_counter_command is set to the path to the
# generated script.  Use notmuch_counter_value() function to get the
# current counter value.
notmuch_counter_reset () {
	notmuch_counter_command="$TMP_DIRECTORY/notmuch_counter"
	if [ ! -x "$notmuch_counter_command" ]; then
		notmuch_counter_state_path="$TMP_DIRECTORY/notmuch_counter.state"
		cat >"$notmuch_counter_command" <<EOF || return
#!/bin/sh

read count < "$notmuch_counter_state_path"
echo \$((count + 1)) > "$notmuch_counter_state_path"

exec notmuch "\$@"
EOF
		chmod +x "$notmuch_counter_command" || return
	fi

	echo 0 > "$notmuch_counter_state_path"
}

# Returns the current notmuch counter value.
notmuch_counter_value () {
	if [ -r "$notmuch_counter_state_path" ]; then
		read count < "$notmuch_counter_state_path"
	else
		count=0
	fi
	echo $count
}

test_reset_state_ () {
	test -z "$test_init_done_" && test_init_

	test_subtest_known_broken_=
	test_subtest_missing_external_prereq_=()
}

# called once before the first subtest
test_init_ () {
	test_init_done_=t

	# skip all tests if there were external prerequisites missing during init
	test_check_missing_external_prereqs_ "all tests in $this_test" && test_done
}


# Where to run the tests
TEST_DIRECTORY=$NOTMUCH_BUILDDIR/test

. "$NOTMUCH_SRCDIR/test/test-lib-common.sh" || exit 1

# Use -P to resolve symlinks in our working directory so that the cwd
# in subprocesses like git equals our $PWD (for pathname comparisons).
cd -P "$TMP_DIRECTORY" || error "Cannot set up test environment"

if test "$verbose" = "t"
then
	exec 4>&2 3>&1
else
	exec 4>test.output 3>&4
fi

for skp in $NOTMUCH_SKIP_TESTS
do
	to_skip=
	for skp in $NOTMUCH_SKIP_TESTS
	do
		case "$this_test" in
		$skp)
			to_skip=t
			break
		esac
		case "$this_test_bare" in
		$skp)
			to_skip=t
			break
		esac
	done
	case "$to_skip" in
	t)
		say_color skip >&3 "skipping test $this_test altogether"
		say_color skip "skip all tests in $this_test"
		test_done
	esac
done

# Provide an implementation of the 'yes' utility
yes () {
	if test $# = 0
	then
		y=y
	else
		y="$*"
	fi

	while echo "$y"
	do
		:
	done
}

# Fix some commands on Windows
case $(uname -s) in
*MINGW*)
	# Windows has its own (incompatible) sort and find
	sort () {
		/usr/bin/sort "$@"
	}
	find () {
		/usr/bin/find "$@"
	}
	sum () {
		md5sum "$@"
	}
	# git sees Windows-style pwd
	pwd () {
		builtin pwd -W
	}
	# no POSIX permissions
	# backslashes in pathspec are converted to '/'
	# exec does not inherit the PID
	;;
*)
	test_set_prereq POSIXPERM
	test_set_prereq BSLASHPSPEC
	test_set_prereq EXECKEEPSPID
	;;
esac

test -z "$NO_PERL" && test_set_prereq PERL
test -z "$NO_PYTHON" && test_set_prereq PYTHON

# test whether the filesystem supports symbolic links
ln -s x y 2>/dev/null && test -h y 2>/dev/null && test_set_prereq SYMLINKS
rm -f y

# declare prerequisites for external binaries used in tests
test_declare_external_prereq dtach
test_declare_external_prereq emacs
test_declare_external_prereq ${TEST_EMACSCLIENT}
test_declare_external_prereq ${TEST_GDB}
test_declare_external_prereq gpg
test_declare_external_prereq openssl
test_declare_external_prereq gpgsm
test_declare_external_prereq ${NOTMUCH_PYTHON}
test_declare_external_prereq xapian-metadata
test_declare_external_prereq xapian-delve
