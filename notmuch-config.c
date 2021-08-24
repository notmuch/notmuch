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

#include <pwd.h>
#include <netdb.h>
#include <assert.h>

#include "path-util.h"
#include "unicode-util.h"

static const char toplevel_config_comment[] =
    " .notmuch-config - Configuration file for the notmuch mail system\n"
    "\n"
    " For more information about notmuch, see https://notmuchmail.org";

static const struct config_group {
    const char *group_name;
    const char *comment;
} group_comment_table [] = {
    {
	"database",
	" Database configuration\n"
	"\n"
	" The only value supported here is 'path' which should be the top-level\n"
	" directory where your mail currently exists and to where mail will be\n"
	" delivered in the future. Files should be individual email messages.\n"
	" Notmuch will store its database within a sub-directory of the path\n"
	" configured here named \".notmuch\".\n"
    },
    {
	"user",
	" User configuration\n"
	"\n"
	" Here is where you can let notmuch know how you would like to be\n"
	" addressed. Valid settings are\n"
	"\n"
	"\tname		Your full name.\n"
	"\tprimary_email	Your primary email address.\n"
	"\tother_email	A list (separated by ';') of other email addresses\n"
	"\t		at which you receive email.\n"
	"\n"
	" Notmuch will use the various email addresses configured here when\n"
	" formatting replies. It will avoid including your own addresses in the\n"
	" recipient list of replies, and will set the From address based on the\n"
	" address to which the original email was addressed.\n"
    },
    {
	"new",
	" Configuration for \"notmuch new\"\n"
	"\n"
	" The following options are supported here:\n"
	"\n"
	"\ttags	A list (separated by ';') of the tags that will be\n"
	"\t	added to all messages incorporated by \"notmuch new\".\n"
	"\n"
	"\tignore	A list (separated by ';') of file and directory names\n"
	"\t	that will not be searched for messages by \"notmuch new\".\n"
	"\n"
	"\t	NOTE: *Every* file/directory that goes by one of those\n"
	"\t	names will be ignored, independent of its depth/location\n"
	"\t	in the mail store.\n"
    },
    {
	"search",
	" Search configuration\n"
	"\n"
	" The following option is supported here:\n"
	"\n"
	"\texclude_tags\n"
	"\t\tA ;-separated list of tags that will be excluded from\n"
	"\t\tsearch results by default.  Using an excluded tag in a\n"
	"\t\tquery will override that exclusion.\n"
    },
    {
	"maildir",
	" Maildir compatibility configuration\n"
	"\n"
	" The following option is supported here:\n"
	"\n"
	"\tsynchronize_flags      Valid values are true and false.\n"
	"\n"
	"\tIf true, then the following maildir flags (in message filenames)\n"
	"\twill be synchronized with the corresponding notmuch tags:\n"
	"\n"
	"\t\tFlag	Tag\n"
	"\t\t----	-------\n"
	"\t\tD	draft\n"
	"\t\tF	flagged\n"
	"\t\tP	passed\n"
	"\t\tR	replied\n"
	"\t\tS	unread (added when 'S' flag is not present)\n"
	"\n"
	"\tThe \"notmuch new\" command will notice flag changes in filenames\n"
	"\tand update tags, while the \"notmuch tag\" and \"notmuch restore\"\n"
	"\tcommands will notice tag changes and update flags in filenames\n"
    },
};

struct _notmuch_conffile {
    char *filename;
    GKeyFile *key_file;
    bool is_new;
};

static int
notmuch_conffile_destructor (notmuch_conffile_t *config)
{
    if (config->key_file)
	g_key_file_free (config->key_file);

    return 0;
}

static bool
get_config_from_file (notmuch_conffile_t *config, bool create_new)
{
    #define BUF_SIZE 4096
    char *config_str = NULL;
    int config_len = 0;
    int config_bufsize = BUF_SIZE;
    size_t len;
    GError *error = NULL;
    bool ret = false;

    FILE *fp = fopen (config->filename, "r");
    if (fp == NULL) {
	if (errno == ENOENT) {
	    /* If create_new is true, then the caller is prepared for a
	     * default configuration file in the case of FILE NOT FOUND.
	     */
	    if (create_new) {
		config->is_new = true;
		ret = true;
	    } else {
		fprintf (stderr, "Configuration file %s not found.\n"
			 "Try running 'notmuch setup' to create a configuration.\n",
			 config->filename);
	    }
	} else {
	    fprintf (stderr, "Error opening config file '%s': %s\n",
		     config->filename, strerror (errno));
	}
	goto out;
    }

    config_str = talloc_zero_array (config, char, config_bufsize);
    if (config_str == NULL) {
	fprintf (stderr, "Error reading '%s': Out of memory\n", config->filename);
	goto out;
    }

    while ((len = fread (config_str + config_len, 1,
			 config_bufsize - config_len, fp)) > 0) {
	config_len += len;
	if (config_len == config_bufsize) {
	    config_bufsize += BUF_SIZE;
	    config_str = talloc_realloc (config, config_str, char, config_bufsize);
	    if (config_str == NULL) {
		fprintf (stderr, "Error reading '%s': Failed to reallocate memory\n",
			 config->filename);
		goto out;
	    }
	}
    }

    if (ferror (fp)) {
	fprintf (stderr, "Error reading '%s': I/O error\n", config->filename);
	goto out;
    }

    if (g_key_file_load_from_data (config->key_file, config_str, config_len,
				   G_KEY_FILE_KEEP_COMMENTS, &error)) {
	ret = true;
	goto out;
    }

    fprintf (stderr, "Error parsing config file '%s': %s\n",
	     config->filename, error->message);

    g_error_free (error);

  out:
    if (fp)
	fclose (fp);

    if (config_str)
	talloc_free (config_str);

    return ret;
}

/* Open the named notmuch configuration file. If the filename is NULL,
 * the value of the environment variable $NOTMUCH_CONFIG will be used.
 * If $NOTMUCH_CONFIG is unset, the default configuration file
 * ($HOME/.notmuch-config) will be used.
 *
 * If any error occurs, (out of memory, or a permission-denied error,
 * etc.), this function will print a message to stderr and return
 * NULL.
 *
 * FILE NOT FOUND: When the specified configuration file (whether from
 * 'filename' or the $NOTMUCH_CONFIG environment variable) does not
 * exist, the behavior of this function depends on the 'is_new_ret'
 * variable.
 *
 *	If is_new_ret is NULL, then a "file not found" message will be
 *	printed to stderr and NULL will be returned.
 *
 *	If is_new_ret is non-NULL then a default configuration will be
 *	returned and *is_new_ret will be set to 1 on return so that
 *	the caller can recognize this case.
 *
 *	These default configuration settings are determined as
 *	follows:
 *
 *		database_path:		$MAILDIR, otherwise $HOME/mail
 *
 *		user_name:		$NAME variable if set, otherwise
 *					read from /etc/passwd
 *
 *		user_primary_mail:	$EMAIL variable if set, otherwise
 *					constructed from the username and
 *					hostname of the current machine.
 *
 *		user_other_email:	Not set.
 *
 *	The default configuration also contains comments to guide the
 *	user in editing the file directly.
 */
notmuch_conffile_t *
notmuch_conffile_open (notmuch_database_t *notmuch,
		       const char *filename,
		       bool create)
{
    char *notmuch_config_env = NULL;

    notmuch_conffile_t *config = talloc_zero (notmuch, notmuch_conffile_t);

    if (config == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return NULL;
    }

    talloc_set_destructor (config, notmuch_conffile_destructor);

    if (filename) {
	config->filename = talloc_strdup (config, filename);
    } else if ((notmuch_config_env = getenv ("NOTMUCH_CONFIG"))) {
	config->filename = talloc_strdup (config, notmuch_config_env);
    } else {
	config->filename = talloc_asprintf (config, "%s/.notmuch-config",
					    getenv ("HOME"));
    }

    config->key_file = g_key_file_new ();

    if (! get_config_from_file (config, create)) {
	talloc_free (config);
	return NULL;
    }

    if (config->is_new)
	g_key_file_set_comment (config->key_file, NULL, NULL,
				toplevel_config_comment, NULL);

    for (size_t i = 0; i < ARRAY_SIZE (group_comment_table); i++) {
	const char *name = group_comment_table[i].group_name;
	if (! g_key_file_has_group (config->key_file,  name)) {
	    /* Force group to exist before adding comment */
	    g_key_file_set_value (config->key_file, name, "dummy_key", "dummy_val");
	    g_key_file_remove_key (config->key_file, name, "dummy_key", NULL);
	    g_key_file_set_comment (config->key_file, name, NULL,
				    group_comment_table[i].comment, NULL);
	}
    }
    return config;
}

/* Close the given notmuch_conffile_t object, freeing all resources.
 *
 * Note: Any changes made to the configuration are *not* saved by this
 * function. To save changes, call notmuch_conffile_save before
 * notmuch_conffile_close.
 */
void
notmuch_conffile_close (notmuch_conffile_t *config)
{
    talloc_free (config);
}

/* Save any changes made to the notmuch configuration.
 *
 * Any comments originally in the file will be preserved.
 *
 * Returns 0 if successful, and 1 in case of any error, (after
 * printing a description of the error to stderr).
 */
int
notmuch_conffile_save (notmuch_conffile_t *config)
{
    size_t length;
    char *data, *filename;
    GError *error = NULL;

    data = g_key_file_to_data (config->key_file, &length, NULL);
    if (data == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    /* Try not to overwrite symlinks. */
    filename = notmuch_canonicalize_file_name (config->filename);
    if (! filename) {
	if (errno == ENOENT) {
	    filename = strdup (config->filename);
	    if (! filename) {
		fprintf (stderr, "Out of memory.\n");
		g_free (data);
		return 1;
	    }
	} else {
	    fprintf (stderr, "Error canonicalizing %s: %s\n", config->filename,
		     strerror (errno));
	    g_free (data);
	    return 1;
	}
    }

    if (! g_file_set_contents (filename, data, length, &error)) {
	if (strcmp (filename, config->filename) != 0) {
	    fprintf (stderr, "Error saving configuration to %s (-> %s): %s\n",
		     config->filename, filename, error->message);
	} else {
	    fprintf (stderr, "Error saving configuration to %s: %s\n",
		     filename, error->message);
	}
	g_error_free (error);
	free (filename);
	g_free (data);
	return 1;
    }

    free (filename);
    g_free (data);
    return 0;
}

bool
notmuch_conffile_is_new (notmuch_conffile_t *config)
{
    return config->is_new;
}

static void
_config_set (notmuch_conffile_t *config,
	     const char *group, const char *key, const char *value)
{
    g_key_file_set_string (config->key_file, group, key, value);
}

static void
_config_set_list (notmuch_conffile_t *config,
		  const char *group, const char *key,
		  const char *list[],
		  size_t length)
{
    g_key_file_set_string_list (config->key_file, group, key, list, length);
}

void
notmuch_conffile_set_database_path (notmuch_conffile_t *config,
				    const char *database_path)
{
    _config_set (config, "database", "path", database_path);
}

void
notmuch_conffile_set_user_name (notmuch_conffile_t *config,
				const char *user_name)
{
    _config_set (config, "user", "name", user_name);
}

void
notmuch_conffile_set_user_primary_email (notmuch_conffile_t *config,
					 const char *primary_email)
{
    _config_set (config, "user", "primary_email", primary_email);
}

void
notmuch_conffile_set_user_other_email (notmuch_conffile_t *config,
				       const char *list[],
				       size_t length)
{
    _config_set_list (config, "user", "other_email", list, length);
}

void
notmuch_conffile_set_new_tags (notmuch_conffile_t *config,
			       const char *list[],
			       size_t length)
{
    _config_set_list (config, "new", "tags", list, length);
}

void
notmuch_conffile_set_new_ignore (notmuch_conffile_t *config,
				 const char *list[],
				 size_t length)
{
    _config_set_list (config, "new", "ignore", list, length);
}

void
notmuch_conffile_set_search_exclude_tags (notmuch_conffile_t *config,
					  const char *list[],
					  size_t length)
{
    _config_set_list (config, "search", "exclude_tags", list, length);
}


/* Given a configuration item of the form <group>.<key> return the
 * component group and key. If any error occurs, print a message on
 * stderr and return 1. Otherwise, return 0.
 *
 * Note: This function modifies the original 'item' string.
 */
static int
_item_split (char *item, char **group, char **key)
{
    char *period;

    *group = item;

    period = strchr (item, '.');
    if (period == NULL || *(period + 1) == '\0') {
	fprintf (stderr,
		 "Invalid configuration name: %s\n"
		 "(Should be of the form <section>.<item>)\n", item);
	return 1;
    }

    *period = '\0';
    *key = period + 1;

    return 0;
}

/* These are more properly called Xapian fields, but the user facing
 * docs call them prefixes, so make the error message match */
static bool
validate_field_name (const char *str)
{
    const char *key;

    if (! g_utf8_validate (str, -1, NULL)) {
	fprintf (stderr, "Invalid utf8: %s\n", str);
	return false;
    }

    key = g_utf8_strrchr (str, -1, '.');
    if (! key ) {
	INTERNAL_ERROR ("Impossible code path on input: %s\n", str);
    }

    key++;

    if (! *key) {
	fprintf (stderr, "Empty prefix name: %s\n", str);
	return false;
    }

    if (! unicode_word_utf8 (key)) {
	fprintf (stderr, "Non-word character in prefix name: %s\n", key);
	return false;
    }

    if (key[0] >= 'a' && key[0] <= 'z') {
	fprintf (stderr, "Prefix names starting with lower case letters are reserved: %s\n", key);
	return false;
    }

    return true;
}

#define BUILT_WITH_PREFIX "built_with."

typedef struct config_key {
    const char *name;
    bool prefix;
    bool (*validate)(const char *);
} config_key_info_t;

static const struct config_key
    config_key_table[] = {
    { "index.decrypt",   false,  NULL },
    { "index.header.",   true,   validate_field_name },
    { "query.",          true,   NULL },
};

static const config_key_info_t *
_config_key_info (const char *item)
{
    for (size_t i = 0; i < ARRAY_SIZE (config_key_table); i++) {
	if (config_key_table[i].prefix &&
	    strncmp (item, config_key_table[i].name,
		     strlen (config_key_table[i].name)) == 0)
	    return config_key_table + i;
	if (strcmp (item, config_key_table[i].name) == 0)
	    return config_key_table + i;
    }
    return NULL;
}

static int
notmuch_config_command_get (notmuch_database_t *notmuch, char *item)
{
    notmuch_config_values_t *list;

    if (STRNCMP_LITERAL (item, BUILT_WITH_PREFIX) == 0) {
	if (notmuch_built_with (item + strlen (BUILT_WITH_PREFIX)))
	    puts ("true");
	else
	    puts ("false");
    } else {
	for (list = notmuch_config_get_values_string (notmuch, item);
	     notmuch_config_values_valid (list);
	     notmuch_config_values_move_to_next (list)) {
	    const char *val = notmuch_config_values_get (list);
	    puts (val);
	}
    }
    return EXIT_SUCCESS;
}

static int
_set_db_config (notmuch_database_t *notmuch, const char *key, int argc, char **argv)
{
    const char *val = "";

    if (argc > 1) {
	/* XXX handle lists? */
	fprintf (stderr, "notmuch config set: at most one value expected for %s\n", key);
	return EXIT_FAILURE;
    }

    if (argc > 0) {
	val = argv[0];
    }

    if (print_status_database ("notmuch config", notmuch,
			       notmuch_database_reopen (notmuch,
							NOTMUCH_DATABASE_MODE_READ_WRITE)))
	return EXIT_FAILURE;

    if (print_status_database ("notmuch config", notmuch,
			       notmuch_database_set_config (notmuch, key, val)))
	return EXIT_FAILURE;

    if (print_status_database ("notmuch config", notmuch,
			       notmuch_database_close (notmuch)))
	return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

static int
notmuch_config_command_set (notmuch_database_t *notmuch,
			    int argc, char *argv[])
{
    char *group, *key;
    const config_key_info_t *key_info;
    notmuch_conffile_t *config;
    bool update_database = false;
    int opt_index, ret;
    char *item;

    notmuch_opt_desc_t options[] = {
	{ .opt_bool = &update_database, .name = "database" },
	{ }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0)
	return EXIT_FAILURE;

    argc -= opt_index;
    argv += opt_index;

    if (argc < 1) {
	fprintf (stderr, "Error: notmuch config set requires at least "
		 "one argument.\n");
	return EXIT_FAILURE;
    }

    item = argv[0];
    argv++;
    argc--;

    if (STRNCMP_LITERAL (item, BUILT_WITH_PREFIX) == 0) {
	fprintf (stderr, "Error: read only option: %s\n", item);
	return 1;
    }

    key_info = _config_key_info (item);
    if (key_info && key_info->validate && (! key_info->validate (item)))
	return 1;

    if (update_database) {
	return _set_db_config (notmuch, item, argc, argv);
    }

    if (_item_split (item, &group, &key))
	return 1;

    config = notmuch_conffile_open (notmuch,
				    notmuch_config_path (notmuch), false);
    if (! config)
	return 1;

    /* With only the name of an item, we clear it from the
     * configuration file.
     *
     * With a single value, we set it as a string.
     *
     * With multiple values, we set them as a string list.
     */
    switch (argc) {
    case 0:
	g_key_file_remove_key (config->key_file, group, key, NULL);
	break;
    case 1:
	g_key_file_set_string (config->key_file, group, key, argv[0]);
	break;
    default:
	g_key_file_set_string_list (config->key_file, group, key,
				    (const gchar **) argv, argc);
	break;
    }

    ret = notmuch_conffile_save (config);

    notmuch_conffile_close (config);

    return ret;
}

static
void
_notmuch_config_list_built_with ()
{
    printf ("%scompact=%s\n",
	    BUILT_WITH_PREFIX,
	    notmuch_built_with ("compact") ? "true" : "false");
    printf ("%sfield_processor=%s\n",
	    BUILT_WITH_PREFIX,
	    notmuch_built_with ("field_processor") ? "true" : "false");
    printf ("%sretry_lock=%s\n",
	    BUILT_WITH_PREFIX,
	    notmuch_built_with ("retry_lock") ? "true" : "false");
}

static int
notmuch_config_command_list (notmuch_database_t *notmuch)
{
    notmuch_config_pairs_t *list;

    _notmuch_config_list_built_with ();
    for (list = notmuch_config_get_pairs (notmuch, "");
	 notmuch_config_pairs_valid (list);
	 notmuch_config_pairs_move_to_next (list)) {
	const char *value = notmuch_config_pairs_value (list);
	if (value)
	    printf ("%s=%s\n", notmuch_config_pairs_key (list), value);
    }
    notmuch_config_pairs_destroy (list);
    return EXIT_SUCCESS;
}

int
notmuch_config_command (notmuch_database_t *notmuch, int argc, char *argv[])
{
    int ret;
    int opt_index;

    opt_index = notmuch_minimal_options ("config", argc, argv);
    if (opt_index < 0)
	return EXIT_FAILURE;

    /* skip at least subcommand argument */
    argc -= opt_index;
    argv += opt_index;

    if (argc < 1) {
	fprintf (stderr, "Error: notmuch config requires at least one argument.\n");
	return EXIT_FAILURE;
    }

    if (strcmp (argv[0], "get") == 0) {
	if (argc != 2) {
	    fprintf (stderr, "Error: notmuch config get requires exactly "
		     "one argument.\n");
	    return EXIT_FAILURE;
	}
	ret = notmuch_config_command_get (notmuch, argv[1]);
    } else if (strcmp (argv[0], "set") == 0) {
	ret = notmuch_config_command_set (notmuch, argc, argv);
    } else if (strcmp (argv[0], "list") == 0) {
	ret = notmuch_config_command_list (notmuch);
    } else {
	fprintf (stderr, "Unrecognized argument for notmuch config: %s\n",
		 argv[0]);
	return EXIT_FAILURE;
    }

    return ret ? EXIT_FAILURE : EXIT_SUCCESS;

}

void
notmuch_conffile_set_maildir_synchronize_flags (notmuch_conffile_t *config,
						bool synchronize_flags)
{
    g_key_file_set_boolean (config->key_file,
			    "maildir", "synchronize_flags", synchronize_flags);
}
