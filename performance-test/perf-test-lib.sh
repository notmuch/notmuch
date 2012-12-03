. ./version.sh

corpus_size=large

while test "$#" -ne 0
do
	case "$1" in
	-d|--debug)
		debug=t;
		shift
		;;
	-s|--small)
		corpus_size=small;
		shift
		;;
	-m|--medium)
		corpus_size=medium;
		shift
		;;
	-l|--large)
		corpus_size=large;
		shift
		;;
	*)
		echo "error: unknown performance test option '$1'" >&2; exit 1 ;;
	esac
done
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

    case "$corpus_size" in
	small)
	    mail_subdir="mail/enron/bailey-s"
	    check_for="${TEST_DIRECTORY}/corpus/$mail_subdir"
	    ;;
	medium)
	    mail_subdir="mail/notmuch-archive"
	    check_for="${TEST_DIRECTORY}/corpus/$mail_subdir"
	    ;;
	*)
	    mail_subdir=mail
	    check_for="${TEST_DIRECTORY}/corpus/$mail_subdir/enron/wolfe-j"
    esac

    MAIL_CORPUS="${TEST_DIRECTORY}/corpus/$mail_subdir"
    args=()
    if [ ! -d "$check_for" ] ; then
	args+=("notmuch-email-corpus/$mail_subdir")
    fi

    if [[ ${#args[@]} > 0 ]]; then
	if command -v pixz > /dev/null; then
	    XZ=pixz
	else
	    XZ=xz
	fi

	printf "Unpacking corpus\n"
	mkdir -p "${TEST_DIRECTORY}/corpus"

	tar --checkpoint=.5000 --extract --strip-components=1 \
	    --directory ${TEST_DIRECTORY}/corpus \
	    --use-compress-program ${XZ} \
	    --file ../download/notmuch-email-corpus-${PERFTEST_VERSION}.tar.xz \
	    "${args[@]}"

	printf "\n"

    fi

    cp -lr $MAIL_CORPUS $MAIL_DIR
}


print_header () {
    printf "[v%4s %6s]          Wall(s)\tUsr(s)\tSys(s)\tRes(K)\tIn/Out(512B)\n" \
	   ${PERFTEST_VERSION} ${corpus_size}
}

time_run () {
    printf "  %-22s" "$1"
    if test "$verbose" != "t"; then exec 4>test.output 3>&4; fi
    if ! eval >&3 "/usr/bin/time -f '%e\t%U\t%S\t%M\t%I/%O' $2" ; then
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

echo
echo $(basename "$0"): "Testing ${test_description:-notmuch performance}"
