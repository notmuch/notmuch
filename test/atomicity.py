# This gdb Python script runs notmuch new and simulates killing and
# restarting notmuch new after every Xapian commit.  To simulate this
# more efficiently, this script runs notmuch new and, immediately
# after every Xapian commit, it *pauses* the running notmuch new,
# copies the entire database and maildir to a snapshot directory, and
# executes a full notmuch new on that snapshot, comparing the final
# results with the expected output.  It can then resume the paused
# notmuch new, which is still running on the original maildir, and
# repeat this process.

import gdb
import os
import glob
import shutil
import subprocess

gdb.execute('set args new')

# Make Xapian commit after every operation instead of batching
gdb.execute('set environment XAPIAN_FLUSH_THRESHOLD = 1')

maildir = os.environ['MAIL_DIR']

# Trap calls to rename, which happens just before Xapian commits
class RenameBreakpoint(gdb.Breakpoint):
    def __init__(self, *args, **kwargs):
        super(RenameBreakpoint, self).__init__(*args, **kwargs)
        self.last_inodes = {}
        self.n = 0

    def stop(self):
        xapiandir = '%s/.notmuch/xapian' % maildir
        if os.path.isfile('%s/iamchert' % xapiandir):
            # As an optimization, only consider snapshots after a
            # Xapian has really committed.  The chert backend
            # overwrites record.base? as the last step in the commit,
            # so keep an eye on their inumbers.
            inodes = {}
            for path in glob.glob('%s/record.base*' % xapiandir):
                inodes[path] = os.stat(path).st_ino
            if inodes == self.last_inodes:
                # Continue
                return False
            self.last_inodes = inodes

        # Save a backtrace in case the test does fail
        backtrace = gdb.execute('backtrace', to_string=True)
        open('backtrace.%d' % self.n, 'w').write(backtrace)

        # Snapshot the database
        shutil.rmtree('%s.snap/.notmuch' % maildir)
        shutil.copytree('%s/.notmuch' % maildir, '%s.snap/.notmuch' % maildir)
        # Restore the mtime of $MAIL_DIR.snap/
        shutil.copystat('%s/.notmuch' % maildir, '%s.snap/.notmuch' % maildir)

        # Run notmuch new to completion on the snapshot
        env = os.environ.copy()
        env.update(NOTMUCH_CONFIG=os.environ['NOTMUCH_CONFIG'] + '.snap',
                   XAPIAN_FLUSH_THRESHOLD='1000')
        subprocess.check_call(
            ['notmuch', 'new'], env=env, stdout=open('/dev/null', 'w'))
        subprocess.check_call(
            ['notmuch', 'search', '*'], env=env,
            stdout=open('search.%d' % self.n, 'w'))

        # Tell the shell how far we've gotten
        open('outcount', 'w').write(str(self.n + 1))

        # Continue
        self.n += 1
        return False
RenameBreakpoint('rename')

try:
    gdb.execute('run')
except Exception:
    import traceback
    raise SystemExit(traceback.format_exc())
