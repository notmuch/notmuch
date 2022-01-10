#!/usr/bin/env bash
test_description="python bindings (pytest)"
. $(dirname "$0")/test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_PYTHON3_CFFI -eq 0 -o $NOTMUCH_HAVE_PYTHON3_PYTEST -eq 0 ]; then
    test_done
fi


test_begin_subtest "python cffi tests (NOTMUCH_CONFIG set)"
pytest_dir=$NOTMUCH_BUILDDIR/bindings/python-cffi/build/stage
printf "[pytest]\nminversion = 3.0\naddopts = -ra\n" > $pytest_dir/pytest.ini
test_expect_success "(cd $pytest_dir && ${NOTMUCH_PYTHON} -m pytest --verbose --log-file=$TMP_DIRECTORY/test.output)"

test_begin_subtest "python cffi tests (NOTMUCH_CONFIG unset)"
pytest_dir=$NOTMUCH_BUILDDIR/bindings/python-cffi/build/stage
printf "[pytest]\nminversion = 3.0\naddopts = -ra\n" > $pytest_dir/pytest.ini
unset NOTMUCH_CONFIG
test_expect_success "(cd $pytest_dir && ${NOTMUCH_PYTHON} -m pytest --verbose --log-file=$TMP_DIRECTORY/test.output)"
test_done
