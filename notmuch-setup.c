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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"

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

static void
welcome_message_pre_setup (void)
{
    printf (
	"Welcome to notmuch!\n\n"

	"The goal of notmuch is to help you manage and search your collection of\n"
	"email, and to efficiently keep up with the flow of email as it comes in.\n\n"

	"Notmuch needs to know a few things about you such as your name and email\n"
	"address, as well as the directory that contains your email. This is where\n"
	"you already have mail stored and where messages will be delivered in the\n"
	"future. This directory can contain any number of sub-directories. Regular\n"
	"files in these directories should be individual email messages. If there\n"
	"are other, non-email files (such as indexes maintained by other email\n"
	"programs) then notmuch will do its best to detect those and ignore them.\n\n"

	"If you already have your email being delivered to directories in either\n"
	"maildir or mh format, then that's perfect. Mail storage that uses mbox\n"
	"format, (where one mbox file contains many messages), will not work with\n"
	"notmuch. If that's how your mail is currently stored, we recommend you\n"
	"first convert it to maildir format with a utility such as mb2md. You can\n"
	"continue configuring notmuch now, but be sure to complete the conversion\n"
	"before you run \"notmuch new\" for the first time.\n\n");
}

static void
welcome_message_post_setup (void)
{
    printf ("\n"
	    "Notmuch is now configured, and the configuration settings are saved in\n"
	    "a file in your home directory named .notmuch-config . If you'd like to\n"
	    "change the configuration in the future, you can either edit that file\n"
	    "directly or run \"notmuch setup\".  To choose an alternate configuration\n"
	    "location, set ${NOTMUCH_CONFIG}.\n\n"

	    "The next step is to run \"notmuch new\" which will create a database\n"
	    "that indexes all of your mail. Depending on the amount of mail you have\n"
	    "the initial indexing process can take a long time, so expect that.\n"
	    "Also, the resulting database will require roughly the same amount of\n"
	    "storage space as your current collection of email. So please ensure you\n"
	    "have sufficient storage space available now.\n\n");
}

static void
print_tag_list (const char **tags, size_t tags_len)
{
    unsigned int i;

    for (i = 0; i < tags_len; i++) {
	if (i != 0)
	    printf (" ");
	printf ("%s", tags[i]);
    }
}

static GPtrArray *
parse_tag_list (void *ctx, char *response)
{
    GPtrArray *tags = g_ptr_array_new ();
    char *tag = response;
    char *space;

    while (tag && *tag) {
	space = strchr (tag, ' ');
	if (space)
	    g_ptr_array_add (tags, talloc_strndup (ctx, tag, space - tag));
	else
	    g_ptr_array_add (tags, talloc_strdup (ctx, tag));
	tag = space;
	while (tag && *tag == ' ')
	    tag++;
    }

    return tags;
}

int
notmuch_setup_command (notmuch_config_t *config,
		       unused(notmuch_database_t *notmuch),
		       int argc, char *argv[])
{
    char *response = NULL;
    size_t response_size = 0;
    const char **old_other_emails;
    size_t old_other_emails_len;
    GPtrArray *other_emails;
    unsigned int i;
    const char **new_tags;
    size_t new_tags_len;
    const char **search_exclude_tags;
    size_t search_exclude_tags_len;

#define prompt(format, ...)                                     \
    do {                                                        \
	printf (format, ##__VA_ARGS__);                         \
	fflush (stdout);                                        \
	if (getline (&response, &response_size, stdin) < 0) {   \
	    printf ("Exiting.\n");                              \
	    exit (EXIT_FAILURE);                                \
	}                                                       \
	chomp_newline (response);                               \
    } while (0)

    if (notmuch_minimal_options ("setup", argc, argv) < 0)
	return EXIT_FAILURE;

    if (notmuch_requested_db_uuid)
	fprintf (stderr, "Warning: ignoring --uuid=%s\n",
		 notmuch_requested_db_uuid);

    if (notmuch_config_is_new (config))
	welcome_message_pre_setup ();

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
	    g_ptr_array_add (other_emails, talloc_strdup (config, response));
	else
	    g_ptr_array_add (other_emails, talloc_strdup (config,
							  old_other_emails[i]));
    }

    do {
	prompt ("Additional email address [Press 'Enter' if none]: ");
	if (strlen (response))
	    g_ptr_array_add (other_emails, talloc_strdup (config, response));
    } while (strlen (response));
    if (other_emails->len)
	notmuch_config_set_user_other_email (config,
					     (const char **)
					     other_emails->pdata,
					     other_emails->len);
    g_ptr_array_free (other_emails, true);

    prompt ("Top-level directory of your email archive [%s]: ",
	    notmuch_config_get_database_path (config));
    if (strlen (response)) {
	const char *absolute_path;

	absolute_path = make_path_absolute (config, response);
	notmuch_config_set_database_path (config, absolute_path);
    }

    new_tags = notmuch_config_get_new_tags (config, &new_tags_len);

    printf ("Tags to apply to all new messages (separated by spaces) [");
    print_tag_list (new_tags, new_tags_len);
    prompt ("]: ");

    if (strlen (response)) {
	GPtrArray *tags = parse_tag_list (config, response);

	notmuch_config_set_new_tags (config, (const char **) tags->pdata,
				     tags->len);

	g_ptr_array_free (tags, true);
    }


    search_exclude_tags = notmuch_config_get_search_exclude_tags (config, &search_exclude_tags_len);

    printf ("Tags to exclude when searching messages (separated by spaces) [");
    print_tag_list (search_exclude_tags, search_exclude_tags_len);
    prompt ("]: ");

    if (strlen (response)) {
	GPtrArray *tags = parse_tag_list (config, response);

	notmuch_config_set_search_exclude_tags (config,
						(const char **) tags->pdata,
						tags->len);

	g_ptr_array_free (tags, true);
    }

    if (notmuch_config_save (config))
	return EXIT_FAILURE;

    if (notmuch_config_is_new (config))
	welcome_message_post_setup ();

    return EXIT_SUCCESS;
}
