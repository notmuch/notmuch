/* config.cc - API for database metadata
 *
 * Copyright © 2016 David Bremner
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
 * Author: David Bremner <david@tethera.net>
 */

#include "notmuch.h"
#include "notmuch-private.h"
#include "database-private.h"

#include <pwd.h>
#include <netdb.h>

static const std::string CONFIG_PREFIX = "C";

struct _notmuch_config_list {
    notmuch_database_t *notmuch;
    Xapian::TermIterator iterator;
    char *current_key;
    char *current_val;
};

struct _notmuch_config_values {
    const char *iterator;
    size_t tok_len;
    const char *string;
    void *children; /* talloc_context */
};

struct _notmuch_config_pairs {
    notmuch_string_map_iterator_t *iter;
};

static const char *_notmuch_config_key_to_string (notmuch_config_key_t key);

static int
_notmuch_config_list_destroy (notmuch_config_list_t *list)
{
    /* invoke destructor w/o deallocating memory */
    list->iterator.~TermIterator();
    return 0;
}

notmuch_status_t
notmuch_database_set_config (notmuch_database_t *notmuch,
			     const char *key,
			     const char *value)
{
    notmuch_status_t status;

    status = _notmuch_database_ensure_writable (notmuch);
    if (status)
	return status;

    if (! notmuch->config) {
	if ((status = _notmuch_config_load_from_database (notmuch)))
	    return status;
    }

    try {
	notmuch->writable_xapian_db->set_metadata (CONFIG_PREFIX + key, value);
    } catch (const Xapian::Error &error) {
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	notmuch->exception_reported = true;
	_notmuch_database_log (notmuch, "Error: A Xapian exception occurred setting metadata: %s\n",
			       error.get_msg ().c_str ());
    }

    if (status)
	return status;

    _notmuch_string_map_set (notmuch->config, key, value);

    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_metadata_value (notmuch_database_t *notmuch,
		 const char *key,
		 std::string &value)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    try {
	value = notmuch->xapian_db->get_metadata (CONFIG_PREFIX + key);
    } catch (const Xapian::Error &error) {
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	notmuch->exception_reported = true;
	_notmuch_database_log (notmuch, "Error: A Xapian exception occurred getting metadata: %s\n",
			       error.get_msg ().c_str ());
    }
    return status;
}

notmuch_status_t
notmuch_database_get_config (notmuch_database_t *notmuch,
			     const char *key,
			     char **value)
{
    const char *stored_val;
    notmuch_status_t status;

    if (! notmuch->config) {
	if ((status = _notmuch_config_load_from_database (notmuch)))
	    return status;
    }

    if (! value)
	return NOTMUCH_STATUS_NULL_POINTER;

    stored_val = _notmuch_string_map_get (notmuch->config, key);
    if (! stored_val) {
	/* XXX in principle this API should be fixed so empty string
	 * is distinguished from not found */
	*value = strdup ("");
    } else {
	*value = strdup (stored_val);
    }

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_database_get_config_list (notmuch_database_t *notmuch,
				  const char *prefix,
				  notmuch_config_list_t **out)
{
    notmuch_config_list_t *list = NULL;
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    list = talloc (notmuch, notmuch_config_list_t);
    if (! list) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    list->notmuch = notmuch;
    list->current_key = NULL;
    list->current_val = NULL;

    try {

	new(&(list->iterator)) Xapian::TermIterator (notmuch->xapian_db->metadata_keys_begin
							 (CONFIG_PREFIX + (prefix ? prefix : "")));
	talloc_set_destructor (list, _notmuch_config_list_destroy);

    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch,
			       "A Xapian exception occurred getting metadata iterator: %s.\n",
			       error.get_msg ().c_str ());
	notmuch->exception_reported = true;
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    *out = list;

  DONE:
    if (status) {
	if (list) {
	    talloc_free (list);
	    if (status != NOTMUCH_STATUS_XAPIAN_EXCEPTION)
		_notmuch_config_list_destroy (list);
	}
    } else {
	talloc_set_destructor (list, _notmuch_config_list_destroy);
    }

    return status;
}

notmuch_bool_t
notmuch_config_list_valid (notmuch_config_list_t *metadata)
{
    if (metadata->iterator == metadata->notmuch->xapian_db->metadata_keys_end ())
	return false;

    return true;
}

static inline char *
_key_from_iterator (notmuch_config_list_t *list)
{
    return talloc_strdup (list, (*list->iterator).c_str () + CONFIG_PREFIX.length ());
}

const char *
notmuch_config_list_key (notmuch_config_list_t *list)
{
    if (list->current_key)
	talloc_free (list->current_key);

    list->current_key = _key_from_iterator (list);

    return list->current_key;
}

const char *
notmuch_config_list_value (notmuch_config_list_t *list)
{
    std::string strval;
    notmuch_status_t status;
    char *key = _key_from_iterator (list);

    /* TODO: better error reporting?? */
    status = _metadata_value (list->notmuch, key, strval);
    if (status)
	return NULL;

    if (list->current_val)
	talloc_free (list->current_val);

    list->current_val = talloc_strdup (list, strval.c_str ());
    talloc_free (key);
    return list->current_val;
}

void
notmuch_config_list_move_to_next (notmuch_config_list_t *list)
{
    list->iterator++;
}

void
notmuch_config_list_destroy (notmuch_config_list_t *list)
{
    talloc_free (list);
}

notmuch_status_t
_notmuch_config_load_from_database (notmuch_database_t *notmuch)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    notmuch_config_list_t *list;

    if (notmuch->config == NULL)
	notmuch->config = _notmuch_string_map_create (notmuch);

    if (unlikely (notmuch->config == NULL))
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    status = notmuch_database_get_config_list (notmuch, "", &list);
    if (status)
	return status;

    for (; notmuch_config_list_valid (list); notmuch_config_list_move_to_next (list)) {
	_notmuch_string_map_append (notmuch->config,
				    notmuch_config_list_key (list),
				    notmuch_config_list_value (list));
    }

    return status;
}

notmuch_config_values_t *
notmuch_config_get_values (notmuch_database_t *notmuch, notmuch_config_key_t key)
{
    const char *key_str = _notmuch_config_key_to_string (key);

    if (! key_str)
	return NULL;

    return notmuch_config_get_values_string (notmuch, key_str);
}

notmuch_config_values_t *
notmuch_config_get_values_string (notmuch_database_t *notmuch, const char *key_str)
{
    notmuch_config_values_t *values = NULL;
    bool ok = false;

    values = talloc (notmuch, notmuch_config_values_t);
    if (unlikely (! values))
	goto DONE;

    values->children = talloc_new (values);

    values->string = _notmuch_string_map_get (notmuch->config, key_str);
    if (! values->string)
	goto DONE;

    values->iterator = strsplit_len (values->string, ';', &(values->tok_len));
    ok = true;

  DONE:
    if (! ok) {
	if (values)
	    talloc_free (values);
	return NULL;
    }
    return values;
}

notmuch_bool_t
notmuch_config_values_valid (notmuch_config_values_t *values)
{
    if (! values)
	return false;

    return (values->iterator != NULL);
}

const char *
notmuch_config_values_get (notmuch_config_values_t *values)
{
    return talloc_strndup (values, values->iterator, values->tok_len);
}

void
notmuch_config_values_start (notmuch_config_values_t *values)
{
    if (values == NULL)
	return;
    if (values->children) {
	talloc_free (values->children);
    }

    values->children = talloc_new (values);

    values->iterator = strsplit_len (values->string, ';', &(values->tok_len));
}

void
notmuch_config_values_move_to_next (notmuch_config_values_t *values)
{
    values->iterator += values->tok_len;
    values->iterator = strsplit_len (values->iterator, ';', &(values->tok_len));
}

void
notmuch_config_values_destroy (notmuch_config_values_t *values)
{
    talloc_free (values);
}

notmuch_config_pairs_t *
notmuch_config_get_pairs (notmuch_database_t *notmuch,
			  const char *prefix)
{
    notmuch_config_pairs_t *pairs = talloc (notmuch, notmuch_config_pairs_t);

    pairs->iter = _notmuch_string_map_iterator_create (notmuch->config, prefix, false);
    return pairs;
}

notmuch_bool_t
notmuch_config_pairs_valid (notmuch_config_pairs_t *pairs)
{
    return _notmuch_string_map_iterator_valid (pairs->iter);
}

void
notmuch_config_pairs_move_to_next (notmuch_config_pairs_t *pairs)
{
    _notmuch_string_map_iterator_move_to_next (pairs->iter);
}

const char *
notmuch_config_pairs_key (notmuch_config_pairs_t *pairs)
{
    return _notmuch_string_map_iterator_key (pairs->iter);
}

const char *
notmuch_config_pairs_value (notmuch_config_pairs_t *pairs)
{
    return _notmuch_string_map_iterator_value (pairs->iter);
}

void
notmuch_config_pairs_destroy (notmuch_config_pairs_t *pairs)
{
    _notmuch_string_map_iterator_destroy (pairs->iter);
    talloc_free (pairs);
}

notmuch_status_t
_notmuch_config_load_from_file (notmuch_database_t *notmuch,
				GKeyFile *file)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    gchar **groups = NULL, **keys, *val;

    if (notmuch->config == NULL)
	notmuch->config = _notmuch_string_map_create (notmuch);

    if (unlikely (notmuch->config == NULL)) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    groups = g_key_file_get_groups (file, NULL);
    for (gchar **grp = groups; *grp; grp++) {
	keys = g_key_file_get_keys (file, *grp, NULL, NULL);
	for (gchar **keys_p = keys; *keys_p; keys_p++) {
	    char *absolute_key = talloc_asprintf (notmuch, "%s.%s", *grp,  *keys_p);
	    val = g_key_file_get_value (file, *grp, *keys_p, NULL);
	    if (! val) {
		status = NOTMUCH_STATUS_FILE_ERROR;
		goto DONE;
	    }
	    _notmuch_string_map_set (notmuch->config, absolute_key, val);
	    g_free (val);
	    talloc_free (absolute_key);
	    if (status)
		goto DONE;
	}
	g_strfreev (keys);
    }

  DONE:
    if (groups)
	g_strfreev (groups);

    return status;
}

notmuch_status_t
notmuch_config_get_bool (notmuch_database_t *notmuch, notmuch_config_key_t key, notmuch_bool_t *val)
{
    const char *key_string, *val_string;

    key_string = _notmuch_config_key_to_string (key);
    if (! key_string) {
	return NOTMUCH_STATUS_ILLEGAL_ARGUMENT;
    }

    val_string = _notmuch_string_map_get (notmuch->config, key_string);
    if (! val_string) {
	*val = FALSE;
	return NOTMUCH_STATUS_SUCCESS;
    }

    if (strcase_equal (val_string, "false") || strcase_equal (val_string, "no"))
	*val = FALSE;
    else if (strcase_equal (val_string, "true") || strcase_equal (val_string, "yes"))
	*val = TRUE;
    else
	return NOTMUCH_STATUS_ILLEGAL_ARGUMENT;

    return NOTMUCH_STATUS_SUCCESS;
}

static const char *
_get_name_from_passwd_file (void *ctx)
{
    long pw_buf_size;
    char *pw_buf;
    struct passwd passwd, *ignored;
    const char *name;
    int e;

    pw_buf_size = sysconf (_SC_GETPW_R_SIZE_MAX);
    if (pw_buf_size == -1) pw_buf_size = 64;
    pw_buf = (char *) talloc_size (ctx, pw_buf_size);

    while ((e = getpwuid_r (getuid (), &passwd, pw_buf,
			    pw_buf_size, &ignored)) == ERANGE) {
	pw_buf_size = pw_buf_size * 2;
	pw_buf = (char *) talloc_zero_size (ctx, pw_buf_size);
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
_get_username_from_passwd_file (void *ctx)
{
    long pw_buf_size;
    char *pw_buf;
    struct passwd passwd, *ignored;
    char *name;
    int e;

    pw_buf_size = sysconf (_SC_GETPW_R_SIZE_MAX);
    if (pw_buf_size == -1) pw_buf_size = 64;
    pw_buf = (char *) talloc_zero_size (ctx, pw_buf_size);

    while ((e = getpwuid_r (getuid (), &passwd, pw_buf,
			    pw_buf_size, &ignored)) == ERANGE) {
	pw_buf_size = pw_buf_size * 2;
	pw_buf = (char *) talloc_zero_size (ctx, pw_buf_size);
    }

    if (e == 0)
	name = talloc_strdup (ctx, passwd.pw_name);
    else
	name = talloc_strdup (ctx, "");

    talloc_free (pw_buf);

    return name;
}

static const char *
_get_email_from_passwd_file (void *ctx)
{

    char hostname[256];
    struct hostent *hostent;
    const char *domainname;
    char *email;

    char *username = _get_username_from_passwd_file (ctx);

    gethostname (hostname, 256);
    hostname[255] = '\0';

    hostent = gethostbyname (hostname);
    if (hostent && (domainname = strchr (hostent->h_name, '.')))
	domainname += 1;
    else
	domainname = "(none)";

    email = talloc_asprintf (ctx, "%s@%s.%s",
			     username, hostname, domainname);

    talloc_free (username);
    return email;
}

static const char *
_notmuch_config_key_to_string (notmuch_config_key_t key)
{
    switch (key) {
    case NOTMUCH_CONFIG_DATABASE_PATH:
	return "database.path";
    case NOTMUCH_CONFIG_MAIL_ROOT:
	return "database.mail_root";
    case NOTMUCH_CONFIG_HOOK_DIR:
	return "database.hook_dir";
    case NOTMUCH_CONFIG_BACKUP_DIR:
	return "database.backup_dir";
    case NOTMUCH_CONFIG_EXCLUDE_TAGS:
	return "search.exclude_tags";
    case NOTMUCH_CONFIG_NEW_TAGS:
	return "new.tags";
    case NOTMUCH_CONFIG_NEW_IGNORE:
	return "new.ignore";
    case NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS:
	return "maildir.synchronize_flags";
    case NOTMUCH_CONFIG_PRIMARY_EMAIL:
	return "user.primary_email";
    case NOTMUCH_CONFIG_OTHER_EMAIL:
	return "user.other_email";
    case NOTMUCH_CONFIG_USER_NAME:
	return "user.name";
    default:
	return NULL;
    }
}

static const char *
_notmuch_config_default (notmuch_database_t *notmuch, notmuch_config_key_t key)
{
    char *path;
    const char *name, *email;

    switch (key) {
    case NOTMUCH_CONFIG_DATABASE_PATH:
	path = getenv ("MAILDIR");
	if (path)
	    path = talloc_strdup (notmuch, path);
	else
	    path = talloc_asprintf (notmuch, "%s/mail",
				    getenv ("HOME"));
	return path;
    case NOTMUCH_CONFIG_MAIL_ROOT:
	/* by default, mail root is the same as database path */
	return notmuch_database_get_path (notmuch);
    case NOTMUCH_CONFIG_EXCLUDE_TAGS:
	return "";
    case NOTMUCH_CONFIG_NEW_TAGS:
	return "unread;inbox";
    case NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS:
	return "true";
    case NOTMUCH_CONFIG_USER_NAME:
	name = getenv ("NAME");
	if (name)
	    name = talloc_strdup (notmuch, name);
	else
	    name = _get_name_from_passwd_file (notmuch);
	return name;
    case NOTMUCH_CONFIG_PRIMARY_EMAIL:
	email = getenv ("EMAIL");
	if (email)
	    email = talloc_strdup (notmuch, email);
	else
	    email = _get_email_from_passwd_file (notmuch);
	return email;
    case NOTMUCH_CONFIG_NEW_IGNORE:
	return "";
    case NOTMUCH_CONFIG_HOOK_DIR:
    case NOTMUCH_CONFIG_BACKUP_DIR:
    case NOTMUCH_CONFIG_OTHER_EMAIL:
	return NULL;
    default:
    case NOTMUCH_CONFIG_LAST:
	INTERNAL_ERROR ("illegal key enum %d", key);
    }
}

notmuch_status_t
_notmuch_config_load_defaults (notmuch_database_t *notmuch)
{
    notmuch_config_key_t key;

    for (key = NOTMUCH_CONFIG_FIRST;
	 key < NOTMUCH_CONFIG_LAST;
	 key = notmuch_config_key_t (key + 1)) {
	const char *val = notmuch_config_get (notmuch, key);
	const char *key_string = _notmuch_config_key_to_string (key);

	val = _notmuch_string_map_get (notmuch->config, key_string);
	if (! val) {
	    _notmuch_string_map_set (notmuch->config, key_string, _notmuch_config_default (notmuch,
											   key));
	}
    }
    return NOTMUCH_STATUS_SUCCESS;
}

const char *
notmuch_config_get (notmuch_database_t *notmuch, notmuch_config_key_t key)
{

    return _notmuch_string_map_get (notmuch->config, _notmuch_config_key_to_string (key));
}

const char *
notmuch_config_path (notmuch_database_t *notmuch)
{
    return notmuch->config_path;
}

notmuch_status_t
notmuch_config_set (notmuch_database_t *notmuch, notmuch_config_key_t key, const char *val)
{

    return notmuch_database_set_config (notmuch, _notmuch_config_key_to_string (key), val);
}

void
_notmuch_config_cache (notmuch_database_t *notmuch, notmuch_config_key_t key, const char *val)
{
    if (notmuch->config == NULL)
	notmuch->config = _notmuch_string_map_create (notmuch);

    _notmuch_string_map_set (notmuch->config, _notmuch_config_key_to_string (key), val);
}
