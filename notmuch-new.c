/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"

static void
tag_inbox_and_unread (notmuch_message_t *message)
{
    notmuch_message_add_tag (message, "inbox");
    notmuch_message_add_tag (message, "unread");
}

int
notmuch_new_command (unused (void *ctx),
		     unused (int argc), unused (char *argv[]))
{
    notmuch_database_t *notmuch;
    const char *mail_directory;
    add_files_state_t add_files_state;
    double elapsed;
    struct timeval tv_now;
    int ret = 0;

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    mail_directory = notmuch_database_get_path (notmuch);

    add_files_state.ignore_read_only_directories = TRUE;
    add_files_state.saw_read_only_directory = FALSE;
    add_files_state.total_files = 0;
    add_files_state.processed_files = 0;
    add_files_state.added_messages = 0;
    add_files_state.callback = tag_inbox_and_unread;
    gettimeofday (&add_files_state.tv_start, NULL);

    ret = add_files (notmuch, mail_directory, &add_files_state);

    gettimeofday (&tv_now, NULL);
    elapsed = notmuch_time_elapsed (add_files_state.tv_start,
				    tv_now);
    if (add_files_state.processed_files) {
	printf ("Processed %d %s in ", add_files_state.processed_files,
		add_files_state.processed_files == 1 ?
		"file" : "total files");
	notmuch_time_print_formatted_seconds (elapsed);
	if (elapsed > 1) {
	    printf (" (%d files/sec.).                 \n",
		    (int) (add_files_state.processed_files / elapsed));
	} else {
	    printf (".                    \n");
	}
    }
    if (add_files_state.added_messages) {
	printf ("Added %d new %s to the database (not much, really).\n",
		add_files_state.added_messages,
		add_files_state.added_messages == 1 ?
		"message" : "messages");
    } else {
	printf ("No new mail---and that's not much.\n");
    }

    if (elapsed > 1 && ! add_files_state.saw_read_only_directory) {
	printf ("\nTip: If you have any sub-directories that are archives (that is,\n"
		"they will never receive new mail), marking these directores as\n"
		"read-only (chmod u-w /path/to/dir) will make \"notmuch new\"\n"
		"much more efficient (it won't even look in those directories).\n");
    }

    if (ret) {
	printf ("\nNote: At least one error was encountered: %s\n",
		notmuch_status_to_string (ret));
    }

  DONE:
    if (notmuch)
	notmuch_database_close (notmuch);

    return ret;
}
