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

find_notmuch_path ()
{
    dir="$1"

    while [ -n "$dir" ]; do
	bin="$dir/notmuch"
	if [ -x "$bin" ]; then
	    echo "$dir"
	    return
	fi
	dir="$(dirname "$dir")"
	if [ "$dir" = "/" ]; then
	    break
	fi
    done
}

backup_database () {
    test_name=$(basename $0 .sh)
    rm -rf notmuch-dir-backup."$test_name"
    cp -pR ${MAIL_DIR}/.notmuch notmuch-dir-backup."${test_name}"
}

restore_database () {
    test_name=$(basename $0 .sh)
    rm -rf ${MAIL_DIR}/.notmuch
    cp -pR notmuch-dir-backup."${test_name}" ${MAIL_DIR}/.notmuch
}

# Test the binaries we have just built.  The tests are kept in
# test/ subdirectory and are run in 'trash directory' subdirectory.
TEST_DIRECTORY=$(pwd -P)
notmuch_path=`find_notmuch_path "$TEST_DIRECTORY"`

# Prepend $TEST_DIRECTORY/../lib to LD_LIBRARY_PATH, to make tests work
# on systems where ../notmuch depends on LD_LIBRARY_PATH.
LD_LIBRARY_PATH=${TEST_DIRECTORY%/*}/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
export LD_LIBRARY_PATH

# configure output
. $notmuch_path/sh.config || exit 1

# load OS specifics
if [ -e ./test-lib-$PLATFORM.sh ]; then
	. ./test-lib-$PLATFORM.sh || exit 1
fi

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
	test -n "$notmuch_path" && MANPATH="$notmuch_path/doc/_build/man"
else # normal case
	if test -n "$notmuch_path"
		then
			PATH="$notmuch_path:$PATH"
			MANPATH="$notmuch_path/doc/_build/man"
		fi
fi
export PATH MANPATH

# Test repository
test="tmp.$(basename "$0" .sh)"
test -n "$root" && test="$root/$test"
case "$test" in
/*) TMP_DIRECTORY="$test" ;;
 *) TMP_DIRECTORY="$TEST_DIRECTORY/$test" ;;
esac
test ! -z "$debug" || remove_tmp=$TMP_DIRECTORY
rm -fr "$test" || {
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

mkdir -p "${test}"
mkdir -p "${MAIL_DIR}"

cat <<EOF >"${NOTMUCH_CONFIG}"
[database]
path=${MAIL_DIR}

[user]
name=Notmuch Test Suite
primary_email=test_suite@notmuchmail.org
other_email=test_suite_other@notmuchmail.org;test_suite@otherdomain.org
EOF
