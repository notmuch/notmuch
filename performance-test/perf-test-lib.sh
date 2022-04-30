. $(dirname "$0")/version.sh || exit 1

debug=""
corpus_size=large
perf_callgraph=lbr
use_perf=0

while test "$#" -ne 0
do
	case "$1" in
	-d|--debug)
		debug=t;
		shift
		;;
	-p|--perf)
		use_perf=1;
		shift
		;;
	-c|--call-graph)
		shift
		perf_callgraph=$1
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

# Ensure NOTMUCH_SRCDIR and NOTMUCH_BUILDDIR are set.
. $(dirname "$0")/../test/export-dirs.sh || exit 1

. "$NOTMUCH_SRCDIR/test/test-vars.sh" || exit 1

# Where to run the tests
TEST_DIRECTORY=$NOTMUCH_BUILDDIR/performance-test

. "$NOTMUCH_SRCDIR/test/test-lib-common.sh" || exit 1

set -e

# It appears that people try to run tests without building...
if [[ ! -x "$NOTMUCH_BUILDDIR/notmuch" ]]; then
	echo >&2 'You do not seem to have built notmuch yet.'
	exit 1
fi

DB_CACHE_DIR=${TEST_DIRECTORY}/notmuch.cache.$corpus_size

add_email_corpus ()
{
    rm -rf ${MAIL_DIR}

    CORPUS_DIR=${TEST_DIRECTORY}/corpus
    mkdir -p "${CORPUS_DIR}"

    MAIL_CORPUS="${CORPUS_DIR}/mail.${corpus_size}"
    TAG_CORPUS="${CORPUS_DIR}/tags"

    if command -v pixz > /dev/null; then
	XZ=pixz
    else
	XZ=xz
    fi

    if [ ! -d "${CORPUS_DIR}/manifest" ]; then

	printf "Unpacking manifests\n"
	tar --extract --use-compress-program ${XZ} --strip-components=1 \
	    --directory ${TEST_DIRECTORY}/corpus \
	    --wildcards --file ../download/notmuch-email-corpus-${PERFTEST_VERSION}.tar.xz \
	    'notmuch-email-corpus/manifest/*'
    fi

    file_list=$(mktemp file_listXXXXXX)
    declare -a extract_dirs
    if [ ! -d "$TAG_CORPUS" ] ; then
	extract_dirs=("${extract_dirs[@]}" notmuch-email-corpus/tags)
    fi

    if [ ! -d "$MAIL_CORPUS" ] ; then
	if [[ "$corpus_size" != "large" ]]; then
	    sed s,^,notmuch-email-corpus/, < \
		${TEST_DIRECTORY}/corpus/manifest/MANIFEST.${corpus_size} >> $file_list
	else
	    extract_dirs=("${extract_dirs[@]}" notmuch-email-corpus/mail)
	fi
    fi

    if [[ -s $file_list || -n "${extract_dirs[*]}" ]]; then

	printf "Unpacking corpus\n"
	tar --checkpoint=.5000 --extract --strip-components=1 \
	    --directory ${TEST_DIRECTORY}/corpus \
	    --use-compress-program ${XZ} \
	    --file ../download/notmuch-email-corpus-${PERFTEST_VERSION}.tar.xz \
	    --anchored --recursion \
	    --files-from $file_list "${extract_dirs[@]}"

	printf "\n"

	if [[ ! -d ${MAIL_CORPUS} ]]; then
	    printf "creating link farm\n"

	    if [[ "$corpus_size" = large ]]; then
		cp -rl ${TEST_DIRECTORY}/corpus/mail ${MAIL_CORPUS}
	    else
		while read -r file; do
		    tdir=${MAIL_CORPUS}/$(dirname $file)
		    mkdir -p $tdir
		    ln ${TEST_DIRECTORY}/corpus/$file $tdir
		done <${TEST_DIRECTORY}/corpus/manifest/MANIFEST.${corpus_size}
	    fi
	fi

    fi

    rm $file_list
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

make_log_dir () {
    local timestamp=$(date +%Y%m%dT%H%M%S)
    log_dir=${TEST_DIRECTORY}/log.$(basename $0)-$corpus_size-${timestamp}
    mkdir -p "${log_dir}"
}

time_start ()
{
    add_email_corpus

    if [[ "$use_perf" = 1 ]]; then
	make_log_dir
    fi

    print_header

    notmuch_new_with_cache time_run
}

memory_start ()
{
    add_email_corpus

    make_log_dir

    notmuch_new_with_cache memory_run
}

memory_run ()
{
    test_count=$(($test_count+1))

    log_file=$log_dir/$test_count.log
    talloc_log=$log_dir/$test_count.talloc

    printf "[ %d ]\t%s\n" $test_count "$1"

    NOTMUCH_TALLOC_REPORT="$talloc_log" eval "valgrind --leak-check=full --log-file='$log_file' $2"

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

print_emacs_header ()
{
    printf "\t\t\tWall(s)\tGCs\tGC time(s)\n"
}

time_run ()
{
    printf "  %-22s" "$1"
    test_count=$(($test_count+1))
    if test "$verbose" != "t"; then exec 4>test.output 3>&4; else exec 3>&1; fi
    if [[ "$use_perf" = 1 ]]; then
	command_str="perf record --call-graph=${perf_callgraph} -o ${log_dir}/${test_count}.perf $2"
    else
	command_str="/usr/bin/time -f '%e\t%U\t%S\t%M\t%I/%O' $2"
    fi

    if ! eval >&3 "$command_str" ; then
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

cd -P "$test" || error "Cannot set up test environment"
test_failure=0
test_count=0

printf "\n%-55s [%s %s]\n"  \
    "$(basename "$0"): Testing ${test_description:-notmuch performance}" \
    "${PERFTEST_VERSION}"  "${corpus_size}"
