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

#include <pwd.h>
#include <netdb.h>

static const char toplevel_config_comment[] =
    " .notmuch-config - Configuration file for the notmuch mail system\n"
    "\n"
    " For more information about notmuch, see http://notmuchmail.org";

static const char database_config_comment[] =
    " Database configuration\n"
    "\n"
    " The only value supported here is 'path' which should be the top-level\n"
    " directory where your mail currently exists and to where mail will be\n"
    " delivered in the future. Files should be individual email messages.\n"
    " Notmuch will store its database within a sub-directory of the path\n"
    " configured here named \".notmuch\".\n";

static const char user_config_comment[] =
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
    " address to which the original email was addressed.\n";

struct _notmuch_config {
    char *filename;
    GKeyFile *key_file;

    char *database_path;
    char *user_name;
    char *user_primary_email;
    char **user_other_email;
    size_t user_other_email_length;
};

static int
notmuch_config_destructor (notmuch_config_t *config)
{
    if (config->key_file)
	g_key_file_free (config->key_file);

    return 0;
}

static char *
get_name_from_passwd_file (void *ctx)
{
    long pw_buf_size = sysconf(_SC_GETPW_R_SIZE_MAX);
    char *pw_buf = talloc_zero_size (ctx, pw_buf_size);
    struct passwd passwd, *ignored;
    char *name;
    int e;

    if (pw_buf_size == -1) pw_buf_size = 64;

    while ((e = getpwuid_r (getuid (), &passwd, pw_buf,
                            pw_buf_size, &ignored)) == ERANGE) {
        pw_buf_size = pw_buf_size * 2;
        pw_buf = talloc_zero_size(ctx, pw_buf_size);
    }

    if (e == 0) {
	char *comma = strchr (passwd.pw_gecos, ',');
	if (comma)
	    name = talloc_strndup (ctx, passwd.pw_gecos,
				   comma - passwd.pw_gecos);
	else
	    name = talloc_strdup (ctx, passwd.pw_gecos);
    } else {
	name = talloc_strdup (ctx, "");
    }

    talloc_free (pw_buf);

    return name;
}

static char *
get_username_from_passwd_file (void *ctx)
{
    long pw_buf_size = sysconf(_SC_GETPW_R_SIZE_MAX);
    char *pw_buf = talloc_zero_size (ctx, pw_buf_size);
    struct passwd passwd, *ignored;
    char *name;
    int e;

    if (pw_buf_size == -1) pw_buf_size = 64;
    while ((e = getpwuid_r (getuid (), &passwd, pw_buf,
                            pw_buf_size, &ignored)) == ERANGE) {
        pw_buf_size = pw_buf_size * 2;
        pw_buf = talloc_zero_size(ctx, pw_buf_size);
    }

    if (e == 0)
	name = talloc_strdup (ctx, passwd.pw_name);
    else
	name = talloc_strdup (ctx, "");

    talloc_free (pw_buf);

    return name;
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
 * Note: It is *not* an error if the specified configuration file does
 * not exist. In this case, a default configuration will be created
 * and returned. Subsequently calling notmuch_config_save will cause
 * the configuration to be written to the filename specified at the
 * time of notmuch_config_open.
 *
 * The default configuration settings are determined as follows:
 *
 *	database_path:		$HOME/mail
 *
 *	user_name:		From /etc/passwd
 *
 *	user_primary_mail: 	$EMAIL variable if set, otherwise
 *				constructed from the username and
 *				hostname of the current machine.
 *
 *	user_other_email:	Not set.
 *
 * The default configuration also contains comments to guide the user
 * in editing the file directly.
 */
notmuch_config_t *
notmuch_config_open (void *ctx,
		     const char *filename,
		     notmuch_bool_t *is_new_ret)
{
    GError *error = NULL;
    int is_new = 0;
    char *notmuch_config_env = NULL;

    if (is_new_ret)
	*is_new_ret = 0;

    notmuch_config_t *config = talloc (ctx, notmuch_config_t);
    if (config == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return NULL;
    }
    
    talloc_set_destructor (config, notmuch_config_destructor);

    if (filename) {
	config->filename = talloc_strdup (config, filename);
    } else if ((notmuch_config_env = getenv ("NOTMUCH_CONFIG"))) {
	config->filename = talloc_strdup (config, notmuch_config_env);
	notmuch_config_env = NULL;
    } else {
	config->filename = talloc_asprintf (config, "%s/.notmuch-config",
					    getenv ("HOME"));
    }

    config->key_file = g_key_file_new ();

    config->database_path = NULL;
    config->user_name = NULL;
    config->user_primary_email = NULL;
    config->user_other_email = NULL;
    config->user_other_email_length = 0;

    if (! g_key_file_load_from_file (config->key_file,
				     config->filename,
				     G_KEY_FILE_KEEP_COMMENTS,
				     &error))
    {
	/* We are capable of dealing with a non-existent configuration
	 * file, so be silent about that. */
	if (!(error->domain == G_FILE_ERROR &&
	      error->code == G_FILE_ERROR_NOENT))
	{
	    fprintf (stderr, "Error reading configuration file %s: %s\n",
		     config->filename, error->message);
	    talloc_free (config);
	    g_error_free (error);
	    return NULL;
	}

	g_error_free (error);
	is_new = 1;
    }

    if (notmuch_config_get_database_path (config) == NULL) {
	char *path = talloc_asprintf (config, "%s/mail",
				      getenv ("HOME"));
	notmuch_config_set_database_path (config, path);
	talloc_free (path);
    }

    if (notmuch_config_get_user_name (config) == NULL) {
	char *name = get_name_from_passwd_file (config);
	notmuch_config_set_user_name (config, name);
	talloc_free (name);
    }

    if (notmuch_config_get_user_primary_email (config) == NULL) {
	char *email = getenv ("EMAIL");
	if (email) {
	    notmuch_config_set_user_primary_email (config, email);
	} else {
	    char hostname[256];
	    struct hostent *hostent;
	    const char *domainname;

	    char *username = get_username_from_passwd_file (config);

	    gethostname (hostname, 256);
	    hostname[255] = '\0';

	    hostent = gethostbyname (hostname);
	    if (hostent && (domainname = strchr (hostent->h_name, '.')))
		domainname += 1;
	    else
		domainname = "(none)";

	    email = talloc_asprintf (config, "%s@%s.%s",
				     username, hostname, domainname);

	    notmuch_config_set_user_primary_email (config, email);

	    talloc_free (username);
	    talloc_free (email);
	}
    }

    /* When we create a new configuration file here, we  add some
     * comments to help the user understand what can be done. */
    if (is_new) {
	g_key_file_set_comment (config->key_file, NULL, NULL,
				toplevel_config_comment, NULL);
	g_key_file_set_comment (config->key_file, "database", NULL,
				database_config_comment, NULL);
	g_key_file_set_comment (config->key_file, "user", NULL,
				user_config_comment, NULL);
    }

    if (is_new_ret)
	*is_new_ret = is_new;

    return config;
}

/* Close the given notmuch_config_t object, freeing all resources.
 * 
 * Note: Any changes made to the configuration are *not* saved by this
 * function. To save changes, call notmuch_config_save before
 * notmuch_config_close.
*/
void
notmuch_config_close (notmuch_config_t *config)
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
notmuch_config_save (notmuch_config_t *config)
{
    size_t length;
    char *data;
    GError *error = NULL;

    data = g_key_file_to_data (config->key_file, &length, NULL);
    if (data == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    if (! g_file_set_contents (config->filename, data, length, &error)) {
	fprintf (stderr, "Error saving configuration to %s: %s\n",
		 config->filename, error->message);
	g_error_free (error);
	return 1;
    }

    return 0;
}

const char *
notmuch_config_get_database_path (notmuch_config_t *config)
{
    char *path;

    if (config->database_path == NULL) {
	path = g_key_file_get_string (config->key_file,
				      "database", "path", NULL);
	if (path) {
	    config->database_path = talloc_strdup (config, path);
	    free (path);
	}
    }

    return config->database_path;
}

void
notmuch_config_set_database_path (notmuch_config_t *config,
				  const char *database_path)
{
    g_key_file_set_string (config->key_file,
			   "database", "path", database_path);

    talloc_free (config->database_path);
    config->database_path = NULL;
}

const char *
notmuch_config_get_user_name (notmuch_config_t *config)
{
    char *name;

    if (config->user_name == NULL) {
	name = g_key_file_get_string (config->key_file,
				      "user", "name", NULL);
	if (name) {
	    config->user_name = talloc_strdup (config, name);
	    free (name);
	}
    }

    return config->user_name;
}

void
notmuch_config_set_user_name (notmuch_config_t *config,
			      const char *user_name)
{
    g_key_file_set_string (config->key_file,
			   "user", "name", user_name);

    talloc_free (config->user_name);
    config->user_name = NULL;
}

const char *
notmuch_config_get_user_primary_email (notmuch_config_t *config)
{
    char *email;

    if (config->user_primary_email == NULL) {
	email = g_key_file_get_string (config->key_file,
				       "user", "primary_email", NULL);
	if (email) {
	    config->user_primary_email = talloc_strdup (config, email);
	    free (email);
	}
    }

    return config->user_primary_email;
}

void
notmuch_config_set_user_primary_email (notmuch_config_t *config,
				       const char *primary_email)
{
    g_key_file_set_string (config->key_file,
			   "user", "primary_email", primary_email);

    talloc_free (config->user_primary_email);
    config->user_primary_email = NULL;
}

char **
notmuch_config_get_user_other_email (notmuch_config_t *config,
				     size_t *length)
{
    char **emails;
    size_t emails_length;
    unsigned int i;

    if (config->user_other_email == NULL) {
	emails = g_key_file_get_string_list (config->key_file,
					     "user", "other_email",
					     &emails_length, NULL);
	if (emails) {
	    config->user_other_email = talloc_size (config,
						    sizeof (char *) *
						    (emails_length + 1));
	    for (i = 0; i < emails_length; i++)
		config->user_other_email[i] = talloc_strdup (config->user_other_email,
							     emails[i]);
	    config->user_other_email[i] = NULL;

	    g_strfreev (emails);

	    config->user_other_email_length = emails_length;
	}
    }

    *length = config->user_other_email_length;
    return config->user_other_email;
}

void
notmuch_config_set_user_other_email (notmuch_config_t *config,
				     const char *other_email[],
				     size_t length)
{
    g_key_file_set_string_list (config->key_file,
				"user", "other_email",
				other_email, length);

    talloc_free (config->user_other_email);
    config->user_other_email = NULL;
}
