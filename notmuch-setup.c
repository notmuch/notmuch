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

static notmuch_status_t
add_all_files (notmuch_database_t *notmuch,
	       const char *mail_directory,
	       int num_files)
{
    add_files_state_t add_files_state;
    double elapsed;
    struct timeval tv_now;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    add_files_state.ignore_read_only_directories = FALSE;
    add_files_state.saw_read_only_directory = FALSE;
    add_files_state.total_files = num_files;
    add_files_state.processed_files = 0;
    add_files_state.added_messages = 0;
    add_files_state.callback = NULL;
    gettimeofday (&add_files_state.tv_start, NULL);

    ret = add_files (notmuch, mail_directory, &add_files_state);

    gettimeofday (&tv_now, NULL);
    elapsed = notmuch_time_elapsed (add_files_state.tv_start,
				    tv_now);
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
    if (add_files_state.added_messages) {
	printf ("Added %d %s to the database.\n\n",
		add_files_state.added_messages,
		add_files_state.added_messages == 1 ?
		"message" : "unique messages");
    }

    return ret;
}


/* XXX: This should be merged with the existing add_files function in
 * add-files.c. */
/* Recursively count all regular files in path and all sub-direcotries
 * of path.  The result is added to *count (which should be
 * initialized to zero by the top-level caller before calling
 * count_files). */
static void
count_files (const char *path, int *count)
{
    DIR *dir;
    struct dirent *e, *entry = NULL;
    int entry_length;
    int err;
    char *next;
    struct stat st;

    dir = opendir (path);

    if (dir == NULL) {
	fprintf (stderr, "Warning: failed to open directory %s: %s\n",
		 path, strerror (errno));
	goto DONE;
    }

    entry_length = offsetof (struct dirent, d_name) +
	pathconf (path, _PC_NAME_MAX) + 1;
    entry = malloc (entry_length);

    while (1) {
	err = readdir_r (dir, entry, &e);
	if (err) {
	    fprintf (stderr, "Error reading directory: %s\n",
		     strerror (errno));
	    free (entry);
	    goto DONE;
	}

	if (e == NULL)
	    break;

	/* Ignore special directories to avoid infinite recursion.
	 * Also ignore the .notmuch directory.
	 */
	/* XXX: Eventually we'll want more sophistication to let the
	 * user specify files to be ignored. */
	if (strcmp (entry->d_name, ".") == 0 ||
	    strcmp (entry->d_name, "..") == 0 ||
	    strcmp (entry->d_name, ".notmuch") == 0)
	{
	    continue;
	}

	if (asprintf (&next, "%s/%s", path, entry->d_name) == -1) {
	    next = NULL;
	    fprintf (stderr, "Error descending from %s to %s: Out of memory\n",
		     path, entry->d_name);
	    continue;
	}

	stat (next, &st);

	if (S_ISREG (st.st_mode)) {
	    *count = *count + 1;
	    if (*count % 1000 == 0) {
		printf ("Found %d files so far.\r", *count);
		fflush (stdout);
	    }
	} else if (S_ISDIR (st.st_mode)) {
	    count_files (next, count);
	}

	free (next);
    }

  DONE:
    if (entry)
	free (entry);

    closedir (dir);
}

static const char *
make_path_absolute (void *ctx, const char *path)
{
    char *cwd;

    if (*path == '/')
	return path;

    cwd = getcwd (NULL, 0);
    if (cwd == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return NULL;
    }

    path = talloc_asprintf (ctx, "%s/%s", cwd, path);
    if (path == NULL)
	fprintf (stderr, "Out of memory.\n");

    free (cwd);

    return path;
}

int
notmuch_setup_command (unused (void *ctx),
		       unused (int argc), unused (char *argv[]))
{
    char *response = NULL;
    size_t response_size;
    notmuch_config_t *config;
    char **old_other_emails;
    size_t old_other_emails_len;
    GPtrArray *other_emails;
    unsigned int i;

#define prompt(format, ...)				\
    do {						\
	printf (format, ##__VA_ARGS__);			\
	fflush (stdout);				\
	getline (&response, &response_size, stdin);	\
	chomp_newline (response);			\
    } while (0)

    config = notmuch_config_open (ctx, NULL);

    prompt ("Your full name [%s]: ", notmuch_config_get_user_name (config));
    if (strlen (response))
	notmuch_config_set_user_name (config, response);

    prompt ("Your primary email address [%s]: ",
	    notmuch_config_get_user_primary_email (config));
    if (strlen (response))
	notmuch_config_set_user_primary_email (config, response);

    other_emails = g_ptr_array_new ();

    old_other_emails = notmuch_config_get_user_other_email (config,
					     &old_other_emails_len);
    for (i = 0; i < old_other_emails_len; i++) {
	prompt ("Additional email address [%s]: ", old_other_emails[i]);
	if (strlen (response))
	    g_ptr_array_add (other_emails, talloc_strdup (ctx, response));
	else
	    g_ptr_array_add (other_emails, talloc_strdup (ctx,
							 old_other_emails[i]));
    }

    do {
	prompt ("Additional email address [Press 'Enter' if none]: ");
	if (strlen (response))
	    g_ptr_array_add (other_emails, talloc_strdup (ctx, response));
    } while (strlen (response));
    if (other_emails->len)
	notmuch_config_set_user_other_email (config,
					     (const char **)
					     other_emails->pdata,
					     other_emails->len);
    g_ptr_array_free (other_emails, TRUE);

    prompt ("Top-level directory of your email archive [%s]: ",
	    notmuch_config_get_database_path (config));
    if (strlen (response)) {
	const char *absolute_path;

	absolute_path = make_path_absolute (ctx, response);
	notmuch_config_set_database_path (config, absolute_path);
    }

    notmuch_config_save (config);

    return 0;
}
