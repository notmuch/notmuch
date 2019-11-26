import email.message
import mailbox
import pathlib
import shutil
import socket
import subprocess
import textwrap
import time
import os

import pytest


def pytest_report_header():
    which = shutil.which('notmuch')
    vers = subprocess.run(['notmuch', '--version'], stdout=subprocess.PIPE)
    return ['{} ({})'.format(vers.stdout.decode(errors='replace').strip(),which)]


@pytest.fixture(scope='function')
def tmppath(tmpdir):
    """The tmpdir fixture wrapped in pathlib.Path."""
    return pathlib.Path(str(tmpdir))


@pytest.fixture
def notmuch(maildir):
    """Return a function which runs notmuch commands on our test maildir.

    This uses the notmuch-config file created by the ``maildir``
    fixture.
    """
    def run(*args):
        """Run a notmuch comand.

        This function runs with a timeout error as many notmuch
        commands may block if multiple processes are trying to open
        the database in write-mode.  It is all too easy to
        accidentally do this in the unittests.
        """
        cfg_fname = maildir.path / 'notmuch-config'
        cmd = ['notmuch'] + list(args)
        env = os.environ.copy()
        env['NOTMUCH_CONFIG'] = str(cfg_fname)
        proc = subprocess.run(cmd,
                              timeout=5,
                              env=env)
        proc.check_returncode()
    return run


@pytest.fixture
def maildir(tmppath):
    """A basic test interface to a valid maildir directory.

    This creates a valid maildir and provides a simple mechanism to
    deliver test emails to it.  It also writes a notmuch-config file
    in the top of the maildir.
    """
    cur = tmppath / 'cur'
    cur.mkdir()
    new = tmppath / 'new'
    new.mkdir()
    tmp = tmppath / 'tmp'
    tmp.mkdir()
    cfg_fname = tmppath/'notmuch-config'
    with cfg_fname.open('w') as fp:
        fp.write(textwrap.dedent("""\
            [database]
            path={tmppath!s}
            [user]
            name=Some Hacker
            primary_email=dst@example.com
            [new]
            tags=unread;inbox;
            ignore=
            [search]
            exclude_tags=deleted;spam;
            [maildir]
            synchronize_flags=true
            [crypto]
            gpg_path=gpg
            """.format(tmppath=tmppath)))
    return MailDir(tmppath)


class MailDir:
    """An interface around a correct maildir."""

    def __init__(self, path):
        self._path = pathlib.Path(path)
        self.mailbox = mailbox.Maildir(str(path))
        self._idcount = 0

    @property
    def path(self):
        """The pathname of the maildir."""
        return self._path

    def _next_msgid(self):
        """Return a new unique message ID."""
        msgid = '{}@{}'.format(self._idcount, socket.getfqdn())
        self._idcount += 1
        return msgid

    def deliver(self,
                subject='Test mail',
                body='This is a test mail',
                to='dst@example.com',
                frm='src@example.com',
                headers=None,
                new=False,      # Move to new dir or cur dir?
                keywords=None,  # List of keywords or labels
                seen=False,     # Seen flag (cur dir only)
                replied=False,  # Replied flag (cur dir only)
                flagged=False):  # Flagged flag (cur dir only)
        """Deliver a new mail message in the mbox.

        This does only adds the message to maildir, does not insert it
        into the notmuch database.

        :returns: A tuple of (msgid, pathname).
        """
        msgid = self._next_msgid()
        when = time.time()
        msg = email.message.EmailMessage()
        msg.add_header('Received', 'by MailDir; {}'.format(time.ctime(when)))
        msg.add_header('Message-ID', '<{}>'.format(msgid))
        msg.add_header('Date', time.ctime(when))
        msg.add_header('From', frm)
        msg.add_header('To', to)
        msg.add_header('Subject', subject)
        if headers:
            for h, v in headers:
                msg.add_header(h, v)
        msg.set_content(body)
        mdmsg = mailbox.MaildirMessage(msg)
        if not new:
            mdmsg.set_subdir('cur')
        if flagged:
            mdmsg.add_flag('F')
        if replied:
            mdmsg.add_flag('R')
        if seen:
            mdmsg.add_flag('S')
        boxid = self.mailbox.add(mdmsg)
        basename = boxid
        if mdmsg.get_info():
            basename += mailbox.Maildir.colon + mdmsg.get_info()
        msgpath = self.path / mdmsg.get_subdir() / basename
        return (msgid, msgpath)
