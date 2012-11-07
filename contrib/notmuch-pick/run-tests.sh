#!/usr/bin/env bash

set -eu

fail() {
    echo ERROR $1
    exit 1
}

TESTS="emacs-pick emacs-pick-sync"
TESTFILES="$TESTS pick.expected-output"

export PICK_DIR="`cd \`dirname "$0"\` && pwd`"
PICK_TEST_DIR="$PICK_DIR/test"


for f in $TESTFILES
do
    test -f "$PICK_TEST_DIR/$f" || test -d "$PICK_TEST_DIR/$f" || fail "$PICK_TEST_DIR/$f does not exist"
done

cd "$PICK_DIR/../../test"

test -x ../notmuch || fail "`cd .. && pwd`/notmuch has not been built"

for f in $TESTFILES
do
    if test -f "$f"
    then
	fail "$f exists"
    fi
done

trap "rm -f $TESTFILES" 0

for f in $TESTFILES
do
    ln -s "$PICK_TEST_DIR/$f" .
done

#don't exec -- traps would not run.
for f in $TESTS
do
    echo $f
    ./$f
done
