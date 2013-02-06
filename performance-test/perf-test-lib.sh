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

DB_CACHE_DIR=${TEST_DIRECTORY}/notmuch.cache.$corpus_size

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
    TAG_CORPUS="${TEST_DIRECTORY}/corpus/tags"

    args=()
    if [ ! -d "$TAG_CORPUS" ] ; then
	args+=("notmuch-email-corpus/tags")
    fi

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

    cp -lr $TAG_CORPUS $TMP_DIRECTORY/corpus.tags
    cp -lr $MAIL_CORPUS $MAIL_DIR
}

notmuch_new_with_cache ()
{
    if [ -d $DB_CACHE_DIR ]; then
	cp -r $DB_CACHE_DIR ${MAIL_DIR}/.notmuch
    else
	"$1" 'Initial notmuch new' "notmuch new"
	cache_database
    fi
}

time_start ()
{
    add_email_corpus

    print_header

    notmuch_new_with_cache time_run
}

memory_start ()
{
    add_email_corpus

    local timestamp=$(date +%Y%m%dT%H%M%S)
    log_dir="${TEST_DIRECTORY}/log.$(basename $0)-$corpus_size-${timestamp}"
    mkdir -p ${log_dir}

    notmuch_new_with_cache memory_run
}

memory_run ()
{
    test_count=$(($test_count+1))

    log_file=$log_dir/$test_count.log
    talloc_log=$log_dir/$test_count.talloc

    printf "[ %d ]\t%s\n" $test_count "$1"

    NOTMUCH_TALLOC_REPORT="$talloc_log" valgrind --leak-check=full --log-file="$log_file" $2

    awk '/LEAK SUMMARY/,/suppressed/ { sub(/^==[0-9]*==/," "); print }' "$log_file"
    echo
    sed -n -e 's/.*[(]total *\([^)]*\)[)]/talloced at exit: \1/p' $talloc_log
    echo
}

memory_done ()
{
    time_done
}

cache_database ()
{
    if [ -d $MAIL_DIR/.notmuch ]; then
	cp -r $MAIL_DIR/.notmuch $DB_CACHE_DIR
    else
	echo "Warning: No database found to cache"
    fi
}

uncache_database ()
{
    rm -rf $DB_CACHE_DIR
}

print_header ()
{
    printf "\t\t\tWall(s)\tUsr(s)\tSys(s)\tRes(K)\tIn/Out(512B)\n"
}

time_run ()
{
    printf "  %-22s" "$1"
    test_count=$(($test_count+1))
    if test "$verbose" != "t"; then exec 4>test.output 3>&4; fi
    if ! eval >&3 "/usr/bin/time -f '%e\t%U\t%S\t%M\t%I/%O' $2" ; then
	test_failure=$(($test_failure + 1))
	return 1
    fi
    return 0
}

time_done ()
{
    if [ "$test_failure" = "0" ]; then
	rm -rf "$remove_tmp"
	exit 0
    else
	exit 1
    fi
}

cd -P "$test" || error "Cannot setup test environment"
test_failure=0
test_count=0

printf "\n%-55s [%s %s]\n"  \
    "$(basename "$0"): Testing ${test_description:-notmuch performance}" \
    "${PERFTEST_VERSION}"  "${corpus_size}"
