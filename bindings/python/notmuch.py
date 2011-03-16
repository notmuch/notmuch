#!/usr/bin/env python
"""This is a notmuch implementation in python.
It's goal is to allow running the test suite on the cnotmuch python bindings.

This "binary" honors the NOTMUCH_CONFIG environmen variable for reading a user's
notmuch configuration (e.g. the database path).

   (c) 2010 by Sebastian Spaeth <Sebastian@SSpaeth.de>
               Jesse Rosenthal <jrosenthal@jhu.edu>
   This code is licensed under the GNU GPL v3+.
"""
import sys
import os

import re
import stat
import email

from notmuch import Database, Query, NotmuchError, STATUS
from ConfigParser import SafeConfigParser
from cStringIO import StringIO

PREFIX = re.compile('(\w+):(.*$)')

HELPTEXT = """The notmuch mail system.
Usage: notmuch <command> [args...]

Where <command> and [args...] are as follows:
	setup	Interactively setup notmuch for first use.
	new	[--verbose]
		Find and import new messages to the notmuch database.
	search	[options...] <search-terms> [...]
		Search for messages matching the given search terms.
	show	<search-terms> [...]
		Show all messages matching the search terms.
	count	<search-terms> [...]
		Count messages matching the search terms.
	reply	[options...] <search-terms> [...]
		Construct a reply template for a set of messages.
	tag	+<tag>|-<tag> [...] [--] <search-terms> [...]
		Add/remove tags for all messages matching the search terms.
	dump	[<filename>]
		Create a plain-text dump of the tags for each message.
	restore	<filename>
		Restore the tags from the given dump file (see 'dump').
	search-tags	[<search-terms> [...] ]
		List all tags found in the database or matching messages.
	help	[<command>]
		This message, or more detailed help for the named command.

Use "notmuch help <command>" for more details on each command.
And "notmuch help search-terms" for the common search-terms syntax.
"""

USAGE = """Notmuch is configured and appears to have a database. Excellent!

At this point you can start exploring the functionality of notmuch by
using commands such as:
	notmuch search tag:inbox
	notmuch search to:"%(fullname)s"
	notmuch search from:"%(mailaddress)s"
	notmuch search subject:"my favorite things"

See "notmuch help search" for more details.

You can also use "notmuch show" with any of the thread IDs resulting
from a search. Finally, you may want to explore using a more sophisticated
interface to notmuch such as the emacs interface implemented in notmuch.el
or any other interface described at http://notmuchmail.org

And don't forget to run "notmuch new" whenever new mail arrives.

Have fun, and may your inbox never have much mail.
"""

#-------------------------------------------------------------------------
def quote_query_line(argv):
    # mangle arguments wrapping terms with spaces in quotes
    for (num, item) in enumerate(argv):
        if item.find(' ') >= 0:
		# if we use prefix:termWithSpaces, put quotes around term
		match = PREFIX.match(item)
                if match:
			argv[num] = '%s:"%s"' %(match.group(1), match.group(2))
		else:
			argv[num] = '"%s"' % item
    return ' '.join(argv)

#-------------------------------------------------------------------------


class Notmuch(object):

    def __init__(self, configpath="~/.notmuch-config)"):
        self._config = None
        self._configpath = os.getenv('NOTMUCH_CONFIG',
            os.path.expanduser(configpath))

    def cmd_usage(self):
        """Print the usage text and exits"""
        data={}
        names = self.get_user_email_addresses()
        data['fullname'] = names[0] if names[0] else 'My Name'
        data['mailaddress'] = names[1] if names[1] else 'My@email.address'
        print USAGE % data

    def cmd_new(self):
        """Run 'notmuch new'"""
        #get the database directory
        db = Database(mode=Database.MODE.READ_WRITE)
        path = db.get_path()
        print self._add_new_files_recursively(path, db)

    def cmd_help(self, subcmd=None):
        """Print help text for 'notmuch help'"""
        if len(subcmd) > 1:
            print "Help for specific commands not implemented"
            return
        print HELPTEXT

    def _get_user_notmuch_config(self):
        """Returns the ConfigParser of the user's notmuch-config"""
	# return the cached config parser if we read it already
	if self._config:
            return self._config

	config = SafeConfigParser()
	config.read(self._configpath)
	self._config = config
	return config

    def _add_new_files_recursively(self, path, db):
        """:returns: (added, moved, removed)"""
        print "Enter add new files with path %s" % path

        try:
            #get the Directory() object for this path
            db_dir = db.get_directory(path)
            added = moved = removed = 0
        except NotmuchError:
            # Occurs if we have wrong absolute paths in the db, for example
            return (0,0,0)


        # for folder in subdirs:

        # TODO, retrieve dir mtime here and store it later
        # as long as Filenames() does not allow multiple iteration, we need to
        # use this kludgy way to get a sorted list of filenames
        # db_files is a list of subdirectories and filenames in this folder
        db_files = set()
        db_folders = set()
        for subdir in db_dir.get_child_directories():
            db_folders.add(subdir)
# file is a keyword (remove this ;))
        for mail in db_dir.get_child_files():
            db_files.add(mail)

        fs_files = set(os.listdir(db_dir.path))

        # list of files (and folders) on the fs, but not the db
        for fs_file in ((fs_files - db_files) - db_folders):
            absfile = os.path.normpath(os.path.join(db_dir.path, fs_file))
            statinfo = os.stat(absfile)

            if stat.S_ISDIR(statinfo.st_mode):
                # This is a directory
                if fs_file in ['.notmuch','tmp','.']:
                    continue
		print "%s %s" % (fs_file, db_folders)
                print "Directory not in db yet. Descending into %s" % absfile
                new = self._add_new_files_recursively(absfile, db)
                added += new[0]
                moved += new[1]
                removed += new[2]

            elif stat.S_ISLNK(statinfo.st_mode):
                print ("%s is a symbolic link (%d). FIXME!!!" %
                       (absfile, statinfo.st_mode))
                exit(1)

            else:
                # This is a regular file, not in the db yet. Add it.
                print "This file needs to be added %s" % (absfile)
                (msg, status) = db.add_message(absfile)
                # We increases 'added', even on dupe messages. If it is a moved
                # message, we will deduct it later and increase 'moved' instead
                added += 1

                if status == STATUS.DUPLICATE_MESSAGE_ID:
                    print "Added msg was in the db"
                else:
                    print "New message."

        # Finally a list of files (not folders) in the database,
        # but not the filesystem
        for db_file in (db_files - fs_files):
            absfile = os.path.normpath(os.path.join(db_dir.path, db_file))

            # remove a mail message from the db
            print ("%s is not on the fs anymore. Delete" % absfile)
            status = db.remove_message(absfile)

            if status == STATUS.SUCCESS:
                # we just deleted the last reference, so this was a remove
                removed += 1
                sys.stderr.write("SUCCESS %d %s %s.\n" %
                        (status, STATUS.status2str(status), absfile))
            elif status == STATUS.DUPLICATE_MESSAGE_ID:
                # The filename exists already somewhere else, so this is a move
                moved += 1
                added -= 1
                sys.stderr.write("DUPE %d %s %s.\n" %
                        (status, STATUS.status2str(status), absfile))
            else:
                # This should not occur
                sys.stderr.write("This should not occur %d %s %s.\n" %
                        (status, STATUS.status2str(status), absfile))

        # list of folders in the filesystem. Just descend into dirs
        for fs_file in fs_files:
            absfile = os.path.normpath(os.path.join(db_dir.path, fs_file))
            if os.path.isdir(absfile):
                # This is a directory. Remove it from the db_folder list. 
                # All remaining db_folders at the end will be not present
                # on the file system.
                db_folders.remove(fs_file)
                if fs_file in ['.notmuch','tmp','.']:
                    continue
                new = self._add_new_files_recursively(absfile, db)
                added += new[0]
                moved += new[0]
                removed += new[0]

        # we are not interested in anything but directories here
        #TODO: All remaining elements of db_folders are not in the filesystem
        #delete those

        return added, moved, removed
        #Read the mtime of a directory from the filesystem
        #
        #* Call :meth:`Database.add_message` for all mail files in
        #  the directory

        #* Call notmuch_directory_set_mtime with the mtime read from the 
        #  filesystem.  Then, when wanting to check for updates to the
        #  directory in the future, the client can call :meth:`get_mtime`
        #  and know that it only needs to add files if the mtime of the 
        #  directory and files are newer than the stored timestamp.

    def get_user_email_addresses(self):
        """ Reads a user's notmuch config and returns his email addresses as
	list (name, primary_address, other_address1,...)"""

	#read the config file
	config = self._get_user_notmuch_config()

        conf = {'name': '', 'primary_email': ''}
        for entry in conf:
            if config.has_option('user', entry):
                conf[entry] = config.get('user', entry)

	if config.has_option('user','other_email'):
            other = config.get('user','other_email')
            other = [mail.strip() for mail in other.split(';') if mail]
        else:
            other = []
        # for being compatible. It would be nicer to return a dict.
	return conf.keys() + other

    def quote_msg_body(self, oldbody ,date, from_address):
        """Transform a mail body into a quoted text,
        starting with On foo, bar wrote:

        :param body: a str with a mail body
        :returns: The new payload of the email.message()
        """

        # we get handed a string, wrap it in a file-like object
        oldbody = StringIO(oldbody)
        newbody = StringIO()

        newbody.write("On %s, %s wrote:\n" % (date, from_address))

        for line in oldbody:
            newbody.write("> " + line)

        return newbody.getvalue()

    def format_reply(self, msgs):
        """Gets handed Messages() and displays the reply to them

        This is pretty ugly and hacky. It tries to mimic the "real"
        notmuch output as much as it can to pass the test suite. It
        could deserve a healthy bit of love.  It is also buggy because
        it returns after the first message it has handled."""

        for msg in msgs:
            f = open(msg.get_filename(), "r")
            reply = email.message_from_file(f)

            # handle the easy non-multipart case:
            if not reply.is_multipart():
                reply.set_payload(self.quote_msg_body(reply.get_payload(),
                    reply['date'], reply['from']))
            else:
                # handle the tricky multipart case
                deleted = ""
                """A string describing which nontext attachements
                that have been deleted"""
                delpayloads = []
                """A list of payload indices to be deleted"""
                payloads = reply.get_payload()

                for (num, part) in enumerate(payloads):
                    mime_main = part.get_content_maintype()
                    if mime_main not in ['multipart', 'message', 'text']:
                        deleted += "Non-text part: %s\n" % (part.get_content_type())
                        payloads[num].set_payload("Non-text part: %s" %
                                (part.get_content_type()))
                        payloads[num].set_type('text/plain')
                        delpayloads.append(num)
                    elif mime_main == 'text':
                        payloads[num].set_payload(self.quote_msg_body(
                            payloads[num].get_payload(),
                            reply['date'], reply['from']))
                    else:
                        # TODO handle deeply nested multipart messages
                        sys.stderr.write ("FIXME: Ignoring multipart part. Handle me\n")
                # Delete those payloads that we don't need anymore
                for item in reversed(sorted(delpayloads)):
                    del payloads[item]

        # Back to single- and multipart handling
        my_addresses = self.get_user_email_addresses()
        used_address = None
        # filter our email addresses from all to: cc: and bcc: fields
        # if we find one of "my" addresses being used, 
        # it is stored in used_address
        for header in ['To', 'CC', 'Bcc']:
            if not header in reply:
                #only handle fields that exist
                continue
            addresses = email.utils.getaddresses(reply.get_all(header, []))
            purged_addr = []
            for (name, mail) in addresses:
                if mail in my_addresses[1:]:
                    used_address = email.utils.formataddr(
                            (my_addresses[0], mail))
                else:
                    purged_addr.append(email.utils.formataddr((name, mail)))

            if purged_addr:
                reply.replace_header(header, ", ".join(purged_addr))
            else:
                # we deleted all addresses, delete the header
                del reply[header]

        # Use our primary email address to the From
        # (save original from line, we still need it)
        new_to = reply['From']
        if used_address:
            reply['From'] = used_address
        else:
            email.utils.formataddr((my_addresses[0], my_addresses[1]))

        reply['Subject'] = 'Re: ' + reply['Subject']

        # Calculate our new To: field
        # add all remaining original 'To' addresses
        if 'To' in reply:
            new_to += ", " + reply['To']
        reply.add_header('To', new_to)

        # Add our primary email address to the BCC
        new_bcc = my_addresses[1]
        if 'Bcc' in reply:
            new_bcc += ', '  + reply['Bcc']
        reply['Bcc'] = new_bcc

        # Set replies 'In-Reply-To' header to original's Message-ID
        if 'Message-ID' in reply:
            reply['In-Reply-To'] = reply['Message-ID']

        #Add original's Message-ID to replies 'References' header.
        if 'References' in reply:
            reply['References'] =  ' '.join([reply['References'], reply['Message-ID']])
        else:
            reply['References'] = reply['Message-ID']

        # Delete the original Message-ID.
        del(reply['Message-ID'])

        # filter all existing headers but a few and delete them from 'reply'
        delheaders = filter(lambda x: x not in ['From', 'To', 'Subject', 'CC',
                                                'Bcc', 'In-Reply-To',
                                                'References', 'Content-Type'],
                            reply.keys())
        map(reply.__delitem__, delheaders)

        # TODO: OUCH, we return after the first msg we have handled rather than
        # handle all of them
        # return resulting message without Unixfrom
        return reply.as_string(False)


def main():
    # Handle command line options
    #------------------------------------
    # No option given, print USAGE and exit
    if len(sys.argv) == 1:
        Notmuch().cmd_usage()
    #------------------------------------
    elif sys.argv[1] == 'setup':
       """Interactively setup notmuch for first use."""
       exit("Not implemented.")
    #-------------------------------------
    elif sys.argv[1] == 'new':
        """Check for new and removed messages."""
        Notmuch().cmd_new()
    #-------------------------------------
    elif sys.argv[1] == 'help':
        """Print the help text"""
        Notmuch().cmd_help(sys.argv[1:])
    #-------------------------------------
    elif sys.argv[1] == 'part':
        part()
    #-------------------------------------
    elif sys.argv[1] == 'search':
        search()
    #-------------------------------------
    elif sys.argv[1] == 'show':
        show()
    #-------------------------------------
    elif sys.argv[1] == 'reply':
        db = Database()
        if len(sys.argv) == 2:
            # no search term. abort
            exit("Error: notmuch reply requires at least one search term.")
        # mangle arguments wrapping terms with spaces in quotes
        querystr = quote_query_line(sys.argv[2:])
        msgs = Query(db, querystr).search_messages()
        print Notmuch().format_reply(msgs)
    #-------------------------------------
    elif sys.argv[1] == 'count':
        if len(sys.argv) == 2:
            # no further search term, count all
            querystr = ''
        else:
            # mangle arguments wrapping terms with spaces in quotes
            querystr = quote_query_line(sys.argv[2:])
	print Database().create_query(querystr).count_messages()
    #-------------------------------------
    elif sys.argv[1] == 'tag':
        # build lists of tags to be added and removed
        add = []
        remove = []
        while not sys.argv[2] == '--' and \
                (sys.argv[2].startswith('+') or sys.argv[2].startswith('-')):
                    if sys.argv[2].startswith('+'):
                        # append to add list without initial +
                        add.append(sys.argv.pop(2)[1:])
                    else:
                        # append to remove list without initial -
                        remove.append(sys.argv.pop(2)[1:])
        # skip eventual '--'
        if sys.argv[2] == '--': sys.argv.pop(2)
        # the rest is search terms
        querystr = quote_query_line(sys.argv[2:])
        db = Database(mode=Database.MODE.READ_WRITE)
        msgs  = Query(db, querystr).search_messages()
        for msg in msgs:
            # actually add and remove all tags
            map(msg.add_tag, add)
            map(msg.remove_tag, remove)
    #-------------------------------------
    elif sys.argv[1] == 'search-tags':
        if len(sys.argv) == 2:
            # no further search term
            print "\n".join(Database().get_all_tags())
        else:
            # mangle arguments wrapping terms with spaces in quotes
            querystr = quote_query_line(sys.argv[2:])
            db = Database()
            msgs  = Query(db, querystr).search_messages()
            print "\n".join([t for t in msgs.collect_tags()])
    #-------------------------------------
    elif sys.argv[1] == 'dump':
        if len(sys.argv) == 2:
            f = sys.stdout
        else:
            f = open(sys.argv[2], "w")
        db = Database()
        query = Query(db, '')
        query.set_sort(Query.SORT.MESSAGE_ID)
        msgs = query.search_messages()
        for msg in msgs:
            f.write("%s (%s)\n" % (msg.get_message_id(), msg.get_tags()))
    #-------------------------------------
    elif sys.argv[1] == 'restore':
        if len(sys.argv) == 2:
            print("No filename given. Reading dump from stdin.")
            f = sys.stdin
        else:
            f = open(sys.argv[2], "r")

        # split the msg id and the tags
        MSGID_TAGS = re.compile("(\S+)\s\((.*)\)$")
        db = Database(mode=Database.MODE.READ_WRITE)

        #read each line of the dump file
        for line in f:
            msgs = MSGID_TAGS.match(line)
            if not msgs:
                sys.stderr.write("Warning: Ignoring invalid input line: %s" %
                        line)
                continue
            # split line in components and fetch message
            msg_id = msgs.group(1)
            new_tags = set(msgs.group(2).split())
            msg = db.find_message(msg_id)

            if msg == None:
                sys.stderr.write(
                        "Warning: Cannot apply tags to missing message: %s\n" % msg_id)
                continue

            # do nothing if the old set of tags is the same as the new one
            old_tags = set(msg.get_tags())
            if old_tags == new_tags: continue

            # set the new tags
            msg.freeze()
            # only remove tags if the new ones are not a superset anyway
            if not (new_tags > old_tags): msg.remove_all_tags()
            for tag in new_tags: msg.add_tag(tag)
            msg.thaw()
    #-------------------------------------
    else:
        # unknown command
        exit("Error: Unknown command '%s' (see \"notmuch help\")" % sys.argv[1])

def part():
    db = Database()
    query_string = ''
    part_num = 0
    first_search_term = 0
    for (num, arg) in enumerate(sys.argv[1:]):
        if arg.startswith('--part='):
            part_num_str = arg.split("=")[1]
            try:
                part_num = int(part_num_str)
            except ValueError:
                # just emulating behavior
                exit(1)
        elif not arg.startswith('--'):
            # save the position of the first sys.argv
            # that is a search term
            first_search_term = num + 1
    if first_search_term:
        # mangle arguments wrapping terms with spaces in quotes
        querystr = quote_query_line(sys.argv[first_search_term:])
    qry = Query(db,querystr)
    msgs = [msg for msg in qry.search_messages()]

    if not msgs:
        sys.exit(1)
    elif len(msgs) > 1:
        raise Exception("search term did not match precisely one message")
    else:
        msg = msgs[0]
        print msg.get_part(part_num)

def search():
    db = Database()
    query_string = ''
    sort_order = "newest-first"
    first_search_term = 0
    for (num, arg) in enumerate(sys.argv[1:]):
        if arg.startswith('--sort='):
            sort_order=arg.split("=")[1]
            if not sort_order in ("oldest-first", "newest-first"):
                raise Exception("unknown sort order")
        elif not arg.startswith('--'):
            # save the position of the first sys.argv that is a search term
            first_search_term = num + 1

    if first_search_term:
        # mangle arguments wrapping terms with spaces in quotes
        querystr = quote_query_line(sys.argv[first_search_term:])

    qry = Query(db, querystr)
    if sort_order == "oldest-first":
        qry.set_sort(Query.SORT.OLDEST_FIRST)
    else:
        qry.set_sort(Query.SORT.NEWEST_FIRST)
        threads = qry.search_threads()

    for thread in threads:
        print thread

def show():
    entire_thread = False
    db = Database()
    out_format = "text"
    querystr = ''
    first_search_term = None

    # ugly homegrown option parsing
    # TODO: use OptionParser
    for (num, arg) in enumerate(sys.argv[1:]):
        if arg == '--entire-thread':
            entire_thread = True
        elif arg.startswith("--format="):
            out_format = arg.split("=")[1]
            if out_format == 'json':
                # for compatibility use --entire-thread for json
                  entire_thread = True
            if not out_format in ("json", "text"):
                  raise Exception("unknown format")
        elif not arg.startswith('--'):
            # save the position of the first sys.argv that is a search term
            first_search_term = num + 1

    if first_search_term:
        # mangle arguments wrapping terms with spaces in quotes
        querystr = quote_query_line(sys.argv[first_search_term:])

    threads = Query(db, querystr).search_threads()
    first_toplevel = True
    if out_format == "json":
        sys.stdout.write("[")
    for thread in threads:
        msgs = thread.get_toplevel_messages()
        if not first_toplevel:
            if out_format == "json":
                sys.stdout.write(", ")
        first_toplevel = False
        msgs.print_messages(out_format, 0, entire_thread)

    if out_format == "json":
        sys.stdout.write("]")
    sys.stdout.write("\n")

if __name__ == '__main__':
    main()
