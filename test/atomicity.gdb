# This gdb script runs notmuch new and simulates killing and
# restarting notmuch new after every Xapian commit.  To simulate this
# more efficiently, this script runs notmuch new and, immediately
# after every Xapian commit, it *pauses* the running notmuch new,
# copies the entire database and maildir to a snapshot directory, and
# executes a full notmuch new on that snapshot, comparing the final
# results with the expected output.  It can then resume the paused
# notmuch new, which is still running on the original maildir, and
# repeat this process.

set args new

# Make Xapian commit after every operation instead of batching
set environment XAPIAN_FLUSH_THRESHOLD = 1

# gdb can't keep track of a simple integer.  This is me weeping.
shell echo 0 > outcount

shell touch inodes

break rename
commands
# As an optimization, only consider snapshots after a Xapian commit.
# Xapian overwrites record.base? as the last step in the commit.
shell echo > gdbcmd
shell stat -c %i $MAIL_DIR/.notmuch/xapian/record.base* > inodes.new
shell if cmp inodes inodes.new; then echo cont > gdbcmd; fi
shell mv inodes.new inodes
source gdbcmd

# Save a backtrace in case the test does fail
set logging file backtrace
set logging on
backtrace
set logging off
shell mv backtrace backtrace.`cat outcount`

# Snapshot the database
shell rm -r $MAIL_DIR.snap/.notmuch
shell cp -r $MAIL_DIR/.notmuch $MAIL_DIR.snap/.notmuch
# Restore the mtime of $MAIL_DIR.snap, which we just changed
shell touch -r $MAIL_DIR $MAIL_DIR.snap
# Run notmuch new to completion on the snapshot
shell NOTMUCH_CONFIG=${NOTMUCH_CONFIG}.snap XAPIAN_FLUSH_THRESHOLD=1000 notmuch new > /dev/null
shell NOTMUCH_CONFIG=${NOTMUCH_CONFIG}.snap notmuch search '*' > search.`cat outcount` 2>&1
shell echo $(expr $(cat outcount) + 1) > outcount
cont
end

run
