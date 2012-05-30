#!/usr/bin/env bash
test_description='atomicity'
. ./test-lib.sh

# This script tests the effects of killing and restarting "notmuch
# new" at arbitrary points.  If notmuch new is properly atomic, the
# final database contents should be the same regardless of when (or
# if) it is killed and restarted.

if test_require_external_prereq gdb; then

# Create a maildir structure to also stress flag synchronization
    mkdir $MAIL_DIR/cur
    mkdir $MAIL_DIR/new
    mkdir $MAIL_DIR/tmp
    mkdir $MAIL_DIR/.remove-dir

    # Prepare the initial database
    generate_message [subject]='Duplicate' [filename]='duplicate:2,' [dir]=cur
    generate_message [subject]='Remove' [filename]='remove:2,' [dir]=cur
    generate_message [subject]='"Remove duplicate"' [filename]='remove-duplicate:2,' [dir]=cur
    cp $MAIL_DIR/cur/remove-duplicate:2, $MAIL_DIR/cur/remove-duplicate-copy:2,
    generate_message [subject]='Rename' [filename]='rename:2,' [dir]=cur
    generate_message [subject]='"Rename duplicate"' [filename]='rename-duplicate:2,' [dir]=cur
    generate_message [subject]='"Move 1"' [filename]='move1:2,' [dir]=cur
    generate_message [subject]='"Move 2"' [filename]='move2:2,' [dir]=new
    generate_message [subject]='Flag' [filename]='flag:2,' [dir]=cur
    generate_message [subject]='"Flag duplicate"' [filename]='flag-duplicate:2,' [dir]=cur
    cp $MAIL_DIR/cur/flag-duplicate:2, $MAIL_DIR/cur/flag-duplicate-copy:2,F
    generate_message [subject]='"Remove directory"' [filename]='remove-directory:2,' [dir]=.remove-dir
    generate_message [subject]='"Remove directory duplicate"' [filename]='remove-directory-duplicate:2,' [dir]=.remove-dir
    cp $MAIL_DIR/.remove-dir/remove-directory-duplicate:2, $MAIL_DIR/cur/
    notmuch new > /dev/null

    # Make all maildir changes, but *don't* update the database
    generate_message [subject]='Added' [filename]='added:2,' [dir]=cur
    cp $MAIL_DIR/cur/duplicate:2, $MAIL_DIR/cur/duplicate-copy:2,
    generate_message [subject]='"Add duplicate"' [filename]='add-duplicate:2,' [dir]=cur
    generate_message [subject]='"Add duplicate copy"' [filename]='add-duplicate-copy:2,' [dir]=cur
    rm $MAIL_DIR/cur/remove:2,
    rm $MAIL_DIR/cur/remove-duplicate-copy:2,
    mv $MAIL_DIR/cur/rename:2, $MAIL_DIR/cur/renamed:2,
    mv $MAIL_DIR/cur/rename-duplicate:2, $MAIL_DIR/cur/renamed-duplicate:2,
    mv $MAIL_DIR/cur/move1:2, $MAIL_DIR/new/move1:2,
    mv $MAIL_DIR/new/move2:2, $MAIL_DIR/cur/move2:2,
    mv $MAIL_DIR/cur/flag:2, $MAIL_DIR/cur/flag:2,F
    rm $MAIL_DIR/cur/flag-duplicate-copy:2,F
    rm $MAIL_DIR/.remove-dir/remove-directory:2,
    rm $MAIL_DIR/.remove-dir/remove-directory-duplicate:2,
    rmdir $MAIL_DIR/.remove-dir

    # Prepare a snapshot of the updated maildir.  The gdb script will
    # update the database in this snapshot as it goes.
    cp -a $MAIL_DIR $MAIL_DIR.snap
    cp ${NOTMUCH_CONFIG} ${NOTMUCH_CONFIG}.snap
    NOTMUCH_CONFIG=${NOTMUCH_CONFIG}.snap notmuch config set database.path $MAIL_DIR.snap



    # Execute notmuch new and, at every call to rename, snapshot the
    # database, run notmuch new again on the snapshot, and capture the
    # results of search.
    #
    # -tty /dev/null works around a conflict between the 'timeout' wrapper
    # and gdb's attempt to control the TTY.
    export MAIL_DIR
    gdb -tty /dev/null -batch -x $TEST_DIRECTORY/atomicity.gdb notmuch >/dev/null 2>/dev/null

    # Get the final, golden output
    notmuch search '*' > expected

    # Check output against golden output
    outcount=$(cat outcount)
    echo -n > searchall
    echo -n > expectall
    for ((i = 0; i < $outcount; i++)); do
	if ! cmp -s search.$i expected; then
	    # Find the range of interruptions that match this output
	    for ((end = $i + 1 ; end < $outcount; end++)); do
		if ! cmp -s search.$i search.$end; then
		    break
		fi
	    done
	    echo "When interrupted after $test/backtrace.$(expr $i - 1) (abort points $i-$(expr $end - 1))" >> searchall
	    cat search.$i >> searchall
	    cat expected >> expectall
	    echo >> searchall
	    echo >> expectall

	    i=$(expr $end - 1)
	fi
    done
fi

test_begin_subtest '"notmuch new" is idempotent under arbitrary aborts'
test_expect_equal_file searchall expectall

test_expect_success "detected $outcount>10 abort points" "test $outcount -gt 10"

test_done
