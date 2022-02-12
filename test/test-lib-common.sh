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

# This file contains common code to be used by both the regular
# (correctness) tests and the performance tests.

# test-lib.sh defines die() which echoes to nonstandard fd where
# output was redirected earlier in that file. If test-lib.sh is not
# loaded, neither this redirection nor die() function were defined.
#
type die >/dev/null 2>&1 || die () { echo "$@" >&2; exit 1; }

if [[ -z "$NOTMUCH_SRCDIR" ]] || [[ -z "$NOTMUCH_BUILDDIR" ]]; then
	echo "internal: srcdir or builddir not set" >&2
	exit 1
fi

# Explicitly require external prerequisite.  Useful when binary is
# called indirectly (e.g. from emacs).
# Returns success if dependency is available, failure otherwise.
test_require_external_prereq () {
	local binary
	binary="$1"
	if [[ ${test_missing_external_prereq_["${binary}"]} == t ]]; then
		# dependency is missing, call the replacement function to note it
		eval "$binary"
	else
		true
	fi
}

backup_database () {
    test_name=$(basename $0 .sh)
    rm -rf $TMP_DIRECTORY/notmuch-dir-backup."$test_name"
    cp -pR ${MAIL_DIR}/.notmuch $TMP_DIRECTORY/notmuch-dir-backup."${test_name}"
}

restore_database () {
    test_name=$(basename $0 .sh)
    rm -rf ${MAIL_DIR}/.notmuch
    cp -pR $TMP_DIRECTORY/notmuch-dir-backup."${test_name}" ${MAIL_DIR}/.notmuch
}

# Prepend $TEST_DIRECTORY/../lib to LD_LIBRARY_PATH, to make tests work
# on systems where ../notmuch depends on LD_LIBRARY_PATH.
LD_LIBRARY_PATH=${TEST_DIRECTORY%/*}/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
export LD_LIBRARY_PATH

# configure output
. "$NOTMUCH_BUILDDIR/sh.config" || exit 1

# load OS specifics
if [[ -e "$NOTMUCH_SRCDIR/test/test-lib-$PLATFORM.sh" ]]; then
    . "$NOTMUCH_SRCDIR/test/test-lib-$PLATFORM.sh" || exit 1
fi

# Generate a new message in the mail directory, with a unique message
# ID and subject. The message is not added to the index.
#
# After this function returns, the filename of the generated message
# is available as $gen_msg_filename and the message ID is available as
# $gen_msg_id .
#
# This function supports named parameters with the bash syntax for
# assigning a value to an associative array ([name]=value). The
# supported parameters are:
#
#  [dir]=directory/of/choice
#
#	Generate the message in directory 'directory/of/choice' within
#	the mail store. The directory will be created if necessary.
#
#  [filename]=name
#
#	Store the message in file 'name'. The default is to store it
#	in 'msg-<count>', where <count> is three-digit number of the
#	message.
#
#  [body]=text
#
#	Text to use as the body of the email message
#
#  '[from]="Some User <user@example.com>"'
#  '[to]="Some User <user@example.com>"'
#  '[subject]="Subject of email message"'
#  '[date]="RFC 822 Date"'
#
#	Values for email headers. If not provided, default values will
#	be generated instead.
#
#  '[cc]="Some User <user@example.com>"'
#  [reply-to]=some-address
#  [in-reply-to]=<message-id>
#  [references]=<message-id>
#  [content-type]=content-type-specification
#  '[header]=full header line, including keyword'
#
#	Additional values for email headers. If these are not provided
#	then the relevant headers will simply not appear in the
#	message.
#
#  '[id]=message-id'
#
#	Controls the message-id of the created message.
gen_msg_cnt=0
gen_msg_filename=""
gen_msg_id=""
generate_message () {
    # This is our (bash-specific) magic for doing named parameters
    local -A template="($@)"
    local additional_headers

    gen_msg_cnt=$((gen_msg_cnt + 1))
    if [ -z "${template[filename]}" ]; then
	gen_msg_name="msg-$(printf "%03d" $gen_msg_cnt)"
    else
	gen_msg_name=${template[filename]}
    fi

    if [ -z "${template[id]}" ]; then
	gen_msg_id="${gen_msg_name%:2,*}@notmuch-test-suite"
    else
	gen_msg_id="${template[id]}"
    fi

    if [ -z "${template[dir]}" ]; then
	gen_msg_filename="${MAIL_DIR}/$gen_msg_name"
    else
	gen_msg_filename="${MAIL_DIR}/${template[dir]}/$gen_msg_name"
	mkdir -p "$(dirname "$gen_msg_filename")"
    fi

    if [ -z "${template[body]}" ]; then
	template[body]="This is just a test message (#${gen_msg_cnt})"
    fi

    if [ -z "${template[from]}" ]; then
	template[from]="Notmuch Test Suite <test_suite@notmuchmail.org>"
    fi

    if [ -z "${template[to]}" ]; then
	template[to]="Notmuch Test Suite <test_suite@notmuchmail.org>"
    fi

    if [ -z "${template[subject]}" ]; then
	if [ -n "$test_subtest_name" ]; then
	    template[subject]="$test_subtest_name"
	else
	    template[subject]="Test message #${gen_msg_cnt}"
	fi
    elif [ "${template[subject]}" = "@FORCE_EMPTY" ]; then
	template[subject]=""
    fi

    if [ -z "${template[date]}" ]; then
	# we use decreasing timestamps here for historical reasons;
	# the existing test suite when we converted to unique timestamps just
	# happened to have signicantly fewer failures with that choice.
	local date_secs=$((978709437 - gen_msg_cnt))
	# printf %(..)T is bash 4.2+ feature. use perl fallback if needed...
	TZ=UTC printf -v template[date] "%(%a, %d %b %Y %T %z)T" $date_secs 2>/dev/null ||
	    template[date]=`perl -le 'use POSIX "strftime";
				@time = gmtime '"$date_secs"';
				print strftime "%a, %d %b %Y %T +0000", @time'`
    fi

    additional_headers=""
    if [ ! -z "${template[header]}" ]; then
	additional_headers="${template[header]}
${additional_headers}"
    fi

    if [ ! -z "${template[reply-to]}" ]; then
	additional_headers="Reply-To: ${template[reply-to]}
${additional_headers}"
    fi

    if [ ! -z "${template[in-reply-to]}" ]; then
	additional_headers="In-Reply-To: ${template[in-reply-to]}
${additional_headers}"
    fi

    if [ ! -z "${template[cc]}" ]; then
	additional_headers="Cc: ${template[cc]}
${additional_headers}"
    fi

    if [ ! -z "${template[bcc]}" ]; then
	additional_headers="Bcc: ${template[bcc]}
${additional_headers}"
    fi

    if [ ! -z "${template[references]}" ]; then
	additional_headers="References: ${template[references]}
${additional_headers}"
    fi

    if [ ! -z "${template[content-type]}" ]; then
	additional_headers="Content-Type: ${template[content-type]}
${additional_headers}"
    fi

    if [ ! -z "${template[content-transfer-encoding]}" ]; then
	additional_headers="Content-Transfer-Encoding: ${template[content-transfer-encoding]}
${additional_headers}"
    fi

    # Note that in the way we're setting it above and using it below,
    # `additional_headers' will also serve as the header / body separator
    # (empty line in between).

    cat <<EOF >"$gen_msg_filename"
From: ${template[from]}
To: ${template[to]}
Message-Id: <${gen_msg_id}>
Subject: ${template[subject]}
Date: ${template[date]}
${additional_headers}
${template[body]}
EOF
}

# Generate a new message and add it to the database.
#
# All of the arguments and return values supported by generate_message
# are also supported here, so see that function for details.
add_message () {
    generate_message "$@" &&
    notmuch new > /dev/null
}

if test -n "$valgrind"
then
	make_symlink () {
		test -h "$2" &&
		test "$1" = "$(readlink "$2")" || {
			# be super paranoid
			if mkdir "$2".lock
			then
				rm -f "$2" &&
				ln -s "$1" "$2" &&
				rm -r "$2".lock
			else
				while test -d "$2".lock
				do
					say "Waiting for lock on $2."
					sleep 1
				done
			fi
		}
	}

	make_valgrind_symlink () {
		# handle only executables
		test -x "$1" || return

		base=$(basename "$1")
		symlink_target=$TEST_DIRECTORY/../$base
		# do not override scripts
		if test -x "$symlink_target" &&
		    test ! -d "$symlink_target" &&
		    test "#!" != "$(head -c 2 < "$symlink_target")"
		then
			symlink_target=$TEST_DIRECTORY/valgrind.sh
		fi
		case "$base" in
		*.sh|*.perl)
			symlink_target=$TEST_DIRECTORY/unprocessed-script
		esac
		# create the link, or replace it if it is out of date
		make_symlink "$symlink_target" "$GIT_VALGRIND/bin/$base" || exit
	}

	# override notmuch executable in TEST_DIRECTORY/..
	GIT_VALGRIND=$TEST_DIRECTORY/valgrind
	mkdir -p "$GIT_VALGRIND"/bin
	make_valgrind_symlink $TEST_DIRECTORY/../notmuch
	OLDIFS=$IFS
	IFS=:
	for path in $PATH
	do
		ls "$path"/notmuch 2> /dev/null |
		while read file
		do
			make_valgrind_symlink "$file"
		done
	done
	IFS=$OLDIFS
	PATH=$GIT_VALGRIND/bin:$PATH
	GIT_EXEC_PATH=$GIT_VALGRIND/bin
	export GIT_VALGRIND
	test -n "$NOTMUCH_BUILDDIR" && MANPATH="$NOTMUCH_BUILDDIR/doc/_build/man"
else # normal case
	if test -n "$NOTMUCH_BUILDDIR"
		then
			PATH="$NOTMUCH_BUILDDIR:$PATH"
			MANPATH="$NOTMUCH_BUILDDIR/doc/_build/man"
		fi
fi
export PATH MANPATH

# Test repository
test="tmp.$(basename "$0" .sh)"
TMP_DIRECTORY="$TEST_DIRECTORY/$test"
test ! -z "$debug" || remove_tmp=$TMP_DIRECTORY
rm -rf "$TMP_DIRECTORY" || {
	GIT_EXIT_OK=t
	echo >&6 "FATAL: Cannot prepare test area"
	exit 1
}

# A temporary home directory is needed by at least:
# - emacs/"Sending a message via (fake) SMTP"
# - emacs/"Reply within emacs"
# - crypto/emacs_deliver_message
export HOME="${TMP_DIRECTORY}/home"
mkdir -p "${HOME}"

MAIL_DIR="${TMP_DIRECTORY}/mail"
export NOTMUCH_CONFIG="${TMP_DIRECTORY}/notmuch-config"

mkdir -p "${MAIL_DIR}"

cat <<EOF >"${NOTMUCH_CONFIG}"
[database]
path=${MAIL_DIR}

[user]
name=Notmuch Test Suite
primary_email=test_suite@notmuchmail.org
other_email=test_suite_other@notmuchmail.org;test_suite@otherdomain.org
EOF
