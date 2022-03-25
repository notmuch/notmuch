# Common variable settings for (correctness) tests and performance
# tests.

# Keep the original TERM for say_color and test_emacs
ORIGINAL_TERM=$TERM

# Set SMART_TERM to vt100 for known dumb/unknown terminal.
# Otherwise use whatever TERM is currently used so that
# users' actual TERM environments are being used in tests.
case ${TERM-} in
	'' | dumb | unknown )
		SMART_TERM=vt100 ;;
	*)
		SMART_TERM=$TERM ;;
esac

# For repeatability, reset the environment to known value.
LANG=C
LC_ALL=C
PAGER=cat
TZ=UTC
TERM=dumb
export LANG LC_ALL PAGER TERM TZ
GIT_TEST_CMP=${GIT_TEST_CMP:-diff -u}
if [[ ( -n "$TEST_EMACS" && -z "$TEST_EMACSCLIENT" ) || \
      ( -z "$TEST_EMACS" && -n "$TEST_EMACSCLIENT" ) ]]; then
    echo "error: must specify both or neither of TEST_EMACS and TEST_EMACSCLIENT" >&2
    exit 1
fi
TEST_EMACS=${TEST_EMACS:-${EMACS:-emacs}}
TEST_EMACSCLIENT=${TEST_EMACSCLIENT:-emacsclient}
TEST_GDB=${TEST_GDB:-gdb}
TEST_CC=${TEST_CC:-cc}
TEST_CFLAGS=${TEST_CFLAGS:-"-g -O0"}
TEST_SHIM_CFLAGS=${TEST_SHIM_CFLAGS:-"-fpic -shared"}
TEST_SHIM_LDFLAGS=${TEST_SHIM_LDFLAGS:-"-ldl"}

# Protect ourselves from common misconfiguration to export
# CDPATH into the environment
unset CDPATH

unset GREP_OPTIONS

# For lib/open.cc:_load_key_file
unset XDG_CONFIG_HOME

# for lib/open.cc:_choose_database_path
unset XDG_DATA_HOME
unset MAILDIR

# For emacsclient
unset ALTERNATE_EDITOR

# for reproducibility
unset EMAIL
unset NAME

GIT_EXIT_OK=
# Note: TEST_TMPDIR *NOT* exported!
TEST_TMPDIR=$(mktemp -d "${TMPDIR:-/tmp}/notmuch-test-$$.XXXXXX")
# Put GNUPGHOME in TMPDIR to avoid problems with long paths.
export GNUPGHOME="${TEST_TMPDIR}/gnupg"
