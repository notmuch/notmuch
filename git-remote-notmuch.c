/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2023 Felipe Contreras
 * Copyright © 2024 David Bremner
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
 * Authors: David Bremner <david@tethera.net>
 *	    Felipe Contreras (prototype in ruby)
 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <notmuch.h>
#include "notmuch-client.h"
#include "path-util.h"
#include "hex-escape.h"
#include "string-util.h"
#include "tag-util.h"

#define ASSERT(x) assert ((x))

/* File scope globals */
const char *debug_flags = NULL;
FILE *log_file = NULL;

/* For use with getline. */
char *buffer = NULL;
size_t buffer_len = 0;

/* message state for tracking e.g. deletions */
typedef enum {
    MSG_STATE_UNKNOWN=0,
    MSG_STATE_SEEN,
    MSG_STATE_MISSING,
    MSG_STATE_DELETED
} _message_state_t;

static _message_state_t
get_message_state (GHashTable *mid_state, const char *key)
{
    gpointer val = NULL;

    if (! g_hash_table_lookup_extended (mid_state, key, NULL,
					&val))
	return MSG_STATE_UNKNOWN;
    else
	return GPOINTER_TO_INT (val);
}

static bool
set_message_state (GHashTable *mid_state, const char *mid, _message_state_t state)
{
    return g_hash_table_replace (mid_state, g_strdup (mid),
				 GINT_TO_POINTER (state));
}

static inline bool
equal_lastmod (const char *uuid1, unsigned long counter1,
	       const char *uuid2, unsigned long counter2)
{
    return (strcmp_null (uuid1, uuid2) == 0) && (counter1 == counter2);
}

/* Error handling */
static void
ensure (bool condition, const char *format, ...)
{
    va_list va_args;

    if (! condition) {
	va_start (va_args, format);
	vfprintf (stderr, format, va_args);
	va_end (va_args);
	fprintf (stderr, "\n");
	exit (EXIT_FAILURE);
    }
}

/* It is a (protocol) error to call this at/after EOF */
static void
buffer_line (FILE *stream)
{
    ssize_t nread;

    nread = getline (&buffer, &buffer_len, stream);
    ensure (nread >= 0, "getline %s", strerror (errno));
    chomp_newline (buffer);
}

static GStrv
tokenize_buffer ()
{
    char *tok = buffer;
    size_t tok_len = 0;

    g_autoptr (GStrvBuilder) builder = g_strv_builder_new ();

    while ((tok = strtok_len (tok + tok_len, " \t\n", &tok_len))) {
	g_autofree char *null_terminated = g_strndup (tok, tok_len);
	g_strv_builder_add (builder, null_terminated);
    }

    return g_strv_builder_end (builder);
}

static void
flog (const char *format, ...)
{
    va_list va_args;

    if (log_file) {
	va_start (va_args, format);
	vfprintf (log_file, format, va_args);
	fflush (log_file);
	va_end (va_args);
    }
}

static const char *
gmessage (GError *err)
{
    if (err)
	return err->message;
    else
	return NULL;
}

static void
str2ul (const char *str, unsigned long int *num_p)
{
    gboolean ret;

    g_autoptr (GError) gerror = NULL;

    ret = g_ascii_string_to_unsigned (str, 10, 0, G_MAXUINT64, num_p, &gerror);
    ensure (ret, "converting %s to unsigned long: %s", str, gmessage (gerror));
}

static void
read_lastmod (const char *dir, char **uuid_out, unsigned long *counter_out)
{
    g_autoptr (GString) filename = g_string_new (dir);
    unsigned long num = 0;
    FILE *in;

    assert (uuid_out);
    assert (counter_out);

    g_string_append (filename, "/lastmod");

    in = fopen (filename->str, "r");
    if (! in) {
	ensure (errno == ENOENT, "error opening lastmod file");
	*uuid_out = NULL;
	*counter_out = 0;
    } else {
	g_auto (GStrv) tokens = NULL;
	buffer_line (in);

	tokens = tokenize_buffer ();

	*uuid_out = tokens[0];
	str2ul (tokens[1], &num);

	flog ("loaded uuid = %s\tlastmod = %zu\n", tokens[0], num);
    }

    *counter_out = num;

}

static void
store_lastmod (notmuch_database_t *notmuch, const char *dir)
{
    char *filename = NULL;
    FILE *out;
    unsigned long lastmod;
    const char *uuid;

    ASSERT (filename = talloc_asprintf (notmuch, "%s/lastmod", dir));

    out = fopen (filename, "w");
    ensure (out, "error opening %s for writing: %s", filename, strerror (errno));

    lastmod = notmuch_database_get_revision (notmuch, &uuid);
    ASSERT (fprintf (out, "%s\t%zu\n", uuid, lastmod) > 0);
}

static void
write_data (const char *data)
{
    printf ("data %zu\n", strlen (data));
    fputs (data, stdout);
}

static void
cmd_capabilities ()
{
    fputs ("import\nexport\nrefspec refs/heads/*:refs/notmuch/*\n\n", stdout);
    fflush (stdout);
}

static void
cmd_list (notmuch_database_t *db, const char *uuid, unsigned long lastmod)
{
    unsigned long db_lastmod;
    const char *db_uuid;

    db_lastmod = notmuch_database_get_revision (db, &db_uuid);

    printf ("? refs/heads/master%s\n\n",
	    equal_lastmod (uuid, lastmod, db_uuid, db_lastmod) ? " unchanged" : "");
}

static void
cmd_import (notmuch_database_t *notmuch,
	    const char *nm_dir,
	    const char *uuid,
	    unsigned long lastmod)
{
    const char *ident = NULL;
    const char *lastmod_str = NULL;
    notmuch_messages_t *messages;
    notmuch_status_t status;
    notmuch_query_t *query;
    char *mid_buf = NULL;
    size_t mid_buf_len = 0;

    ident = talloc_asprintf (notmuch, "%s <%s> %zu +0000",
			     notmuch_config_get (notmuch, NOTMUCH_CONFIG_USER_NAME),
			     notmuch_config_get (notmuch, NOTMUCH_CONFIG_PRIMARY_EMAIL),
			     time (NULL));


    printf ("feature done\ncommit refs/notmuch/master\nmark :1\ncommitter %s\n", ident);

    ASSERT (lastmod_str = talloc_asprintf (notmuch, "lastmod: %zu\n", lastmod));
    write_data (lastmod_str);
    if (uuid)
	puts ("from refs/notmuch/master^0");

    /* don't send deleteall here, as there may be other files in the
     * repo outside the database prefix */

    status = notmuch_query_create_with_syntax (notmuch,
					       "",
					       NOTMUCH_QUERY_SYNTAX_XAPIAN,
					       &query);

    if (print_status_database ("git-remote-nm", notmuch, status))
	exit (EXIT_FAILURE);

    if (debug_flags && strchr (debug_flags, 's'))
	notmuch_query_set_sort (query, NOTMUCH_SORT_NEWEST_FIRST);
    else
	notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    status = notmuch_query_search_messages (query, &messages);
    if (print_status_query ("git-remote-nm", query, status))
	exit (EXIT_FAILURE);

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages)) {
	const char *tag_buf = "";
	const char *mid;
	const char *hash;
	const char *prefix = notmuch_config_get (notmuch, NOTMUCH_CONFIG_GIT_METADATA_PREFIX);
	int ret;

	notmuch_message_t *message = notmuch_messages_get (messages);
	mid = notmuch_message_get_message_id (message);

	ret = hex_encode (notmuch, mid, &mid_buf, &mid_buf_len);
	ensure (ret == HEX_SUCCESS, "failed to hex-encode message-id %s\n", mid);

	/* we can't use _notmuch_sha1_from_string because we don't want
	 * to include the null terminator */
	g_autoptr (GChecksum) sha1 = NULL;
	sha1 = g_checksum_new (G_CHECKSUM_SHA1);
	g_checksum_update (sha1, (const guchar *) mid, strlen (mid));
	hash = g_checksum_get_string (sha1);
	printf ("M 644 inline %s/%2.2s/%2.2s/%s/tags\n", prefix, hash, hash + 2, mid_buf);

	for (notmuch_tags_t *tags = notmuch_message_get_tags (message);
	     notmuch_tags_valid (tags);
	     notmuch_tags_move_to_next (tags)) {
	    const char *tag_str = notmuch_tags_get (tags);
	    ASSERT (tag_buf = talloc_asprintf (message, "%s%s\n", tag_buf, tag_str));
	}
	write_data (tag_buf);
	notmuch_message_destroy (message);
    }
    puts ("");
    puts ("done");
    fflush (stdout);
    store_lastmod (notmuch, nm_dir);
}

static GString *
read_data ()
{
    ssize_t nread;
    size_t bytes;
    size_t data_size;

    g_auto (GStrv) tokens = NULL;

    ASSERT ((nread = getline (&buffer, &buffer_len, stdin) != -1));

    tokens = tokenize_buffer ();

    str2ul (tokens[1], &data_size);

    buffer = realloc (buffer, data_size + 1);
    bytes = fread (buffer, 1, data_size, stdin);
    ASSERT (bytes == data_size);

    buffer_len = data_size;

    return g_string_new_len (buffer, buffer_len);
}

static void
free_string (GString *str)
{
    g_string_free (str, true);
}

static bool
path_to_mid (notmuch_database_t *notmuch, const char *path, char **mid_p, size_t *mid_len_p)
{
    g_autofree char *basename = NULL;
    const char *prefix = notmuch_config_get (notmuch, NOTMUCH_CONFIG_GIT_METADATA_PREFIX);

    if (strncmp (prefix, path, strlen (prefix)))
	return false;

    basename = g_path_get_dirname (path + strlen (prefix) + 7);
    ASSERT (HEX_SUCCESS ==
	    hex_decode (notmuch, basename, mid_p, mid_len_p));
    return true;
}

/* In order to force a message to be deleted from the database, we
 * need to delete all of its filenames. XXX TODO Add to library
 * API? */
static notmuch_status_t
remove_message_all (notmuch_database_t *notmuch,
		    notmuch_message_t *message)
{
    notmuch_filenames_t *filenames = NULL;
    notmuch_status_t status;

    for (filenames = notmuch_message_get_filenames (message);
	 notmuch_filenames_valid (filenames);
	 notmuch_filenames_move_to_next (filenames)) {
	const char *filename =  notmuch_filenames_get (filenames);
	status = notmuch_database_remove_message (notmuch, filename);
	if (status != NOTMUCH_STATUS_SUCCESS &&
	    status != NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) {
	    fprintf (stderr, "failed to remove %s from database\n", filename);
	    return status;
	}
    }
    return NOTMUCH_STATUS_SUCCESS;
}

static void
mark_unseen (notmuch_database_t *notmuch,
	     GHashTable *mid_state)
{
    notmuch_status_t status;
    notmuch_messages_t *messages;
    notmuch_query_t *query;

    if (debug_flags && strchr (debug_flags, 'd')) {
	flog ("total mids = %d\n", g_hash_table_size (mid_state));
    }
    status = notmuch_query_create_with_syntax (notmuch,
					       "",
					       NOTMUCH_QUERY_SYNTAX_XAPIAN,
					       &query);

    if (print_status_database ("git-remote-nm", notmuch, status))
	exit (EXIT_FAILURE);

    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    status = notmuch_query_search_messages (query, &messages);
    if (print_status_query ("git-remote-nm", query, status))
	exit (EXIT_FAILURE);

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages)) {
	notmuch_message_t *message = notmuch_messages_get (messages);
	const char *mid = notmuch_message_get_message_id (message);

	switch (get_message_state (mid_state, mid)) {
	case MSG_STATE_SEEN:
	case MSG_STATE_DELETED:
	    break;
	case MSG_STATE_UNKNOWN:
	    set_message_state (mid_state, mid, MSG_STATE_DELETED);
	    break;
	case MSG_STATE_MISSING:
	    INTERNAL_ERROR ("found missing mid %s", mid);
	}
	notmuch_message_destroy (message);
    }
}

static void
purge_database (notmuch_database_t *notmuch, GHashTable *msg_state)
{
    gpointer key, value;

    GHashTableIter iter;
    int count = 0;

    if (debug_flags && strchr (debug_flags, 'd'))
	flog ("removing unseen messages from database\n");

    g_hash_table_iter_init (&iter, msg_state);
    while (g_hash_table_iter_next (&iter, &key, &value)) {
	notmuch_message_t *message;
	const char *mid = key;

	if (GPOINTER_TO_INT (value) != MSG_STATE_DELETED)
	    continue;

	ASSERT (NOTMUCH_STATUS_SUCCESS ==
		notmuch_database_find_message (notmuch,
					       mid, &message));
	/* If the message is in the database, clean up */
	if (message) {
	    remove_message_all (notmuch, message);
	    if (debug_flags && strchr (debug_flags, 'd'))
		flog ("removed from database %s\n", mid);
	    count++;
	}
    }
    if (debug_flags && strchr (debug_flags, 'd'))
	flog ("removed %d messages from database\n", count);
}

static void
check_missing (notmuch_database_t *notmuch, GHashTable *mid_state)
{
    notmuch_status_t status;
    notmuch_bool_t strict;
    gpointer key, value;

    GHashTableIter iter;
    int count = 0;

    status = notmuch_config_get_bool (notmuch,
				      NOTMUCH_CONFIG_GIT_FAIL_ON_MISSING,
				      &strict);
    if (print_status_database ("config_get_bool", notmuch,  status))
	exit (EXIT_FAILURE);

    g_hash_table_iter_init (&iter, mid_state);
    while (g_hash_table_iter_next (&iter, &key, &value)) {
	if (GPOINTER_TO_INT (value) != MSG_STATE_MISSING)
	    continue;

	fprintf (stderr, "%c: missing mid %s\n", strict ? 'E' : 'W', (const char *) key);
	flog ("%c: missing mid %s\n", strict ? 'E' : 'W', (const char *) key);
	count++;
    }

    if (count > 0) {
	if (strict) {
	    fprintf (stderr, "E: %d missing messages\n", count);
	    exit (1);
	} else {
	    flog ("I: ignoring missing messages\n");
	}
    }
}

static void
cmd_export (notmuch_database_t *notmuch, const char *nm_dir)
{
    ssize_t nread;

    int commit_count = 0;

    g_autoptr (GHashTable) blobs = NULL;
    g_autoptr (GHashTable) mid_state = NULL;

    /* Do not supply a function to free values, as we use the same
     * pointer for key and value */
    ASSERT (mid_state = g_hash_table_new_full ((GHashFunc) g_str_hash,
					       (GEqualFunc) g_str_equal,
					       g_free, NULL));

    ASSERT (blobs = g_hash_table_new_full ((GHashFunc) g_str_hash,
					   (GEqualFunc) g_str_equal,
					   g_free, (GDestroyNotify) free_string));

    while ((nread = getline (&buffer, &buffer_len, stdin)) != -1) {
	flog ("export %s\n", buffer);
	if (STRNCMP_LITERAL (buffer, "done") == 0) {
	    break;
	} else if (STRNCMP_LITERAL (buffer, "blob") == 0) {
	    GString *data;
	    g_auto (GStrv) tokens = NULL;


	    flog ("export blob\n");
	    buffer_line (stdin);

	    tokens = tokenize_buffer ();

	    data = read_data ();

	    flog ("\tmark%s\n", tokens[1]);
	    g_hash_table_insert (blobs, g_strdup (tokens[1]), data);
	    buffer_line (stdin);
	} else if (STRNCMP_LITERAL (buffer, "commit") == 0) {
	    char *mid = NULL;
	    size_t mid_len = 0;
	    bool process_this_commit = true;
	    g_autoptr (GString) commit_msg = NULL;
	    const char *commit_ref = buffer + strlen ("commit ");
	    const char *database_ref = notmuch_config_get (notmuch, NOTMUCH_CONFIG_GIT_REF);
	    chomp_newline (buffer);
	    if (strcmp (commit_ref, database_ref)) {
		process_this_commit = false;
		flog ("ignoring commit to ref %s\n", commit_ref);
	    }

	    if (process_this_commit) {
		commit_count++;
		flog ("export commit %d\n", commit_count);
	    }

	    /* mark for commit (ignored) */
	    buffer_line (stdin);
	    /* author (ignored) */
	    buffer_line (stdin);
	    /* committer (ignored) */
	    buffer_line (stdin);

	    /* commit message */
	    commit_msg = read_data ();
	    flog ("commit msg %s\n", commit_msg->str);
	    while (strlen (buffer) > 0) {
		g_autoptr (GString) mark = NULL;
		g_autoptr (GString) path = NULL;
		const GString *blob;
		notmuch_message_t *message;
		const char *tok;
		size_t tok_len;
		size_t max_tok_len;
		tag_op_list_t *tag_ops;
		g_auto (GStrv) tokens = NULL;

		buffer_line (stdin);
		if (strlen (buffer) == 0)
		    break;
		if (! process_this_commit)
		    break;

		tokens = tokenize_buffer ();
		if (STRNCMP_LITERAL (tokens[0], "D") == 0) {
		    if (path_to_mid (notmuch, tokens[1], &mid, &mid_len)) {
			flog ("marking message %s for deletion\n", mid);
			set_message_state (mid_state, mid, MSG_STATE_DELETED);
		    } else {
			if (debug_flags && strchr (debug_flags, 'd'))
			    flog ("ignoring non prefixed file %s\n", tokens[1]);
		    }
		} else if (STRNCMP_LITERAL (tokens[0], "M") == 0) {

		    ASSERT (blob = g_hash_table_lookup (blobs, tokens[2]));

		    if (! path_to_mid (notmuch, tokens[3], &mid, &mid_len)) {
			if (debug_flags)
			    flog ("ignoring non prefixed file %s\n", tokens[3]);
			continue;
		    }

		    if (debug_flags && strchr (debug_flags, 'd')) {
			flog ("marking mid seen: %s\n", mid);
		    }

		    ASSERT (NOTMUCH_STATUS_SUCCESS ==
			    notmuch_database_find_message (notmuch, mid, &message));
		    if (! message) {
			if (debug_flags && strchr (debug_flags, 'm')) {
			    flog ("marking mid missing: %s\n", mid);
			}
			set_message_state (mid_state, mid, MSG_STATE_MISSING);
		    } else {
			set_message_state (mid_state, mid, MSG_STATE_SEEN);
			ASSERT (NOTMUCH_STATUS_SUCCESS ==
				notmuch_message_freeze (message));

			tag_ops = tag_op_list_create (message);
			tok = blob->str;
			max_tok_len = blob->len;
			tok_len = 0;
			while ((tok_len < max_tok_len) &&
			       (tok = strsplit_len (tok + tok_len, '\n', &tok_len)) != NULL) {
			    const char *tag = talloc_strndup (message, tok, tok_len);
			    ASSERT (0 == tag_op_list_append (tag_ops, tag, false));
			}

			ASSERT (NOTMUCH_STATUS_SUCCESS ==
				tag_op_list_apply (message, tag_ops, TAG_FLAG_REMOVE_ALL));

			ASSERT (NOTMUCH_STATUS_SUCCESS ==
				notmuch_message_thaw (message));

			notmuch_message_destroy (message);

		    }
		} else {
		    flog ("export ignoring line %s\n", buffer);
		}
	    }
	    puts ("ok refs/heads/master");
	}
    }

    mark_unseen (notmuch, mid_state);

    if (commit_count > 0)
	purge_database (notmuch, mid_state);

    check_missing (notmuch, mid_state);

    store_lastmod (notmuch, nm_dir);
    puts ("");
}


/* stubs since we cannot link with notmuch.o */
const notmuch_opt_desc_t notmuch_shared_options[] = {
    { }
};

const char *notmuch_requested_db_uuid = NULL;

void
notmuch_process_shared_options (unused (notmuch_database_t *notmuch),
				unused (const char *dummy))
{
}

int
notmuch_minimal_options (unused (const char *subcommand),
			 unused (int argc),
			 unused (char **argv))
{
    return 0;
}

static notmuch_database_t *
open_database (const char *arg)
{
    notmuch_status_t status;
    notmuch_database_t *notmuch;
    const char *path = NULL;
    const char *config = NULL;
    const char *profile = NULL;
    const char *scheme = NULL;
    const char *uriquery = NULL;
    g_autofree char *status_string = NULL;

    g_autoptr (GUri) uri = NULL;
    g_autoptr (GHashTable) params = NULL;
    g_autoptr (GError) gerror = NULL;
    g_autoptr (GString) address = NULL;

    address = g_string_new (arg);

    scheme = g_uri_peek_scheme (address->str);
    if (! scheme || (strcmp (scheme, "notmuch") != 0)) {
	ASSERT (g_string_prepend (address, "notmuch://"));
    }

    uri = g_uri_parse (address->str, G_URI_FLAGS_ENCODED_QUERY, &gerror);
    ensure (uri, "unable to parse URL/address %s: %s\n", address->str, gmessage (gerror));

    uriquery = g_uri_get_query (uri);
    if (uriquery) {
	flog ("uriquery = %s\n", uriquery);
	params = g_uri_parse_params (uriquery, -1, "&", G_URI_PARAMS_NONE, &gerror);
	ensure (params,  "unable to parse parameters %s: %s\n", uriquery, gmessage (gerror));
    }

    if (strlen (g_uri_get_path (uri)) > 0) {
	path = g_uri_get_path (uri);
	config = "";
    }

    if (params) {
	if (! path)
	    path = g_hash_table_lookup (params, "path");
	config = g_hash_table_lookup (params, "config");
	profile = g_hash_table_lookup (params, "profile");
    }

    flog ("url = %s\npath = %s\nconfig = %s\nprofile = %s\n",
	  address->str, path, config, profile);

    status = notmuch_database_open_with_config (path,
						NOTMUCH_DATABASE_MODE_READ_WRITE,
						config,
						profile,
						&notmuch,
						&status_string);

    ensure (status == 0, "open database: %s", status_string);

    return notmuch;
}

int
main (int argc, char *argv[])
{
    notmuch_status_t status;
    notmuch_database_t *db;
    unsigned long lastmod = 0;
    char *uuid = NULL;
    const char *nm_dir = NULL;
    g_autofree char *status_string = NULL;
    const char *git_dir;
    ssize_t nread;
    const char *log_file_name;

    debug_flags = getenv ("GIT_REMOTE_NM_DEBUG");
    log_file_name = getenv ("GIT_REMOTE_NM_LOG");

    if (log_file_name)
	log_file = fopen (log_file_name, "w");

    ensure (argc >= 3, "usage: %s ALIAS URL\n", argv[0]);

    db = open_database (argv[2]);

    git_dir = getenv ("GIT_DIR");
    ensure (git_dir, "GIT_DIR not set");
    flog ("GIT_DIR=%s\n", git_dir);

    ASSERT (nm_dir = talloc_asprintf (db, "%s/%s", git_dir, "notmuch"));

    status = mkdir_recursive (db, nm_dir, 0700, &status_string);
    ensure (status == 0, "mkdir: %s", status_string);

    read_lastmod (nm_dir, &uuid, &lastmod);

    while ((nread = getline (&buffer, &buffer_len, stdin)) != -1) {
	char *s = buffer;
	flog ("command = %s\n", buffer);

	/* skip leading space */
	while (*s && isspace (*s)) s++;

	if (! *s)
	    break;

	if (STRNCMP_LITERAL (s, "capabilities") == 0)
	    cmd_capabilities ();
	else if (STRNCMP_LITERAL (s, "export") == 0)
	    cmd_export (db, nm_dir);
	else if (STRNCMP_LITERAL (s, "import") == 0)
	    cmd_import (db, nm_dir, uuid, lastmod);
	else if (STRNCMP_LITERAL (s, "list") == 0)
	    cmd_list (db, uuid, lastmod);

	fflush (stdout);
	flog ("finished command = %s\n", buffer);
    }
    flog ("finished loop\n");

    notmuch_database_destroy (db);
}
