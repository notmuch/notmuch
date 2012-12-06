. ./version.sh

. ../test/test-lib-common.sh

set -e

if ! test -x ../notmuch
then
	echo >&2 'You do not seem to have built notmuch yet.'
	exit 1
fi

add_email_corpus ()
{
    rm -rf ${MAIL_DIR}

    case "$1" in
	--small)
	    arg="mail/enron/bailey-s"
	    ;;
	--medium)
	    arg="mail/notmuch-archive"
	    ;;
	*)
	    arg=mail
    esac

    if command -v pixz > /dev/null; then
	XZ=pixz
    else
	XZ=xz
    fi

    printf "Unpacking corpus\n"
    tar --checkpoint=.5000 --extract --strip-components=1 \
	--directory ${TMP_DIRECTORY} \
	--use-compress-program ${XZ} \
	--file ../download/notmuch-email-corpus-${PERFTEST_VERSION}.tar.xz \
	notmuch-email-corpus/"$arg"

    printf "\n"
}

print_header () {
    printf "[v%4s]               Wall(s)\tUsr(s)\tSys(s)\tRes(K)\tIn(512B)\tOut(512B)\n" \
	   ${PERFTEST_VERSION}
}

time_run () {
    printf "%-22s" "$1"
    if test "$verbose" != "t"; then exec 4>test.output 3>&4; fi
    if ! eval >&3 "/usr/bin/time -f '%e\t%U\t%S\t%M\t%I\t%O' $2" ; then
	test_failure=$(($test_failure + 1))
	return 1
    fi
    return 0
}

time_done () {
    if [ "$test_failure" = "0" ]; then
	rm -rf "$remove_tmp"
	exit 0
    else
	exit 1
    fi
}

cd -P "$test" || error "Cannot setup test environment"
test_failure=0
