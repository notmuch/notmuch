/* database.cc - The database interfaces of the notmuch mail library
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

#include "database-private.h"

#include <iostream>

#include <xapian.h>

#include <glib.h> /* g_strdup_printf, g_free, GPtrArray, GHashTable */

using namespace std;

const char *
notmuch_status_to_string (notmuch_status_t status)
{
    switch (status) {
    case NOTMUCH_STATUS_SUCCESS:
	return "No error occurred";
    case NOTMUCH_STATUS_XAPIAN_EXCEPTION:
	return "A Xapian exception occurred";
    case NOTMUCH_STATUS_FILE_ERROR:
	return "Something went wrong trying to read or write a file";
    case NOTMUCH_STATUS_FILE_NOT_EMAIL:
	return "File is not an email";
    case NOTMUCH_STATUS_NULL_POINTER:
	return "Erroneous NULL pointer";
    case NOTMUCH_STATUS_TAG_TOO_LONG:
	return "Tag value is too long";
    default:
    case NOTMUCH_STATUS_LAST_STATUS:
	return "Unknown error status value";
    }
}

/* XXX: We should drop this function and convert all callers to call
 * _notmuch_message_add_term instead. */
static void
add_term (Xapian::Document doc,
	  const char *prefix_name,
	  const char *value)
{
    const char *prefix;
    char *term;

    if (value == NULL)
	return;

    prefix = _find_prefix (prefix_name);

    term = g_strdup_printf ("%s%s", prefix, value);

    if (strlen (term) <= NOTMUCH_TERM_MAX)
	doc.add_term (term);

    g_free (term);
}

static void
find_messages_by_term (Xapian::Database *db,
		       const char *prefix_name,
		       const char *value,
		       Xapian::PostingIterator *begin,
		       Xapian::PostingIterator *end)
{
    Xapian::PostingIterator i;
    char *term;

    term = g_strdup_printf ("%s%s", _find_prefix (prefix_name), value);

    *begin = db->postlist_begin (term);

    if (end)
	*end = db->postlist_end (term);

    free (term);
}

Xapian::Document
find_message_by_docid (Xapian::Database *db, Xapian::docid docid)
{
    return db->get_document (docid);
}

static void
insert_thread_id (GHashTable *thread_ids, Xapian::Document doc)
{
    string value_string;
    const char *value, *id, *comma;

    value_string = doc.get_value (NOTMUCH_VALUE_THREAD);
    value = value_string.c_str();
    if (strlen (value)) {
	id = value;
	while (*id) {
	    comma = strchr (id, ',');
	    if (comma == NULL)
		comma = id + strlen (id);
	    g_hash_table_insert (thread_ids,
				 strndup (id, comma - id), NULL);
	    id = comma;
	    if (*id)
		id++;
	}
    }
}

notmuch_message_t *
notmuch_database_find_message (notmuch_database_t *notmuch,
			       const char *message_id)
{
    Xapian::PostingIterator i, end;

    find_messages_by_term (notmuch->xapian_db,
			   "msgid", message_id, &i, &end);

    if (i == end)
	return NULL;

    return _notmuch_message_create (notmuch, notmuch, *i);
}

/* Return one or more thread_ids, (as a GPtrArray of strings), for the
 * given message based on looking into the database for any messages
 * referenced in parents, and also for any messages in the database
 * referencing message_id.
 *
 * Caller should free all strings in the array and the array itself,
 * (g_ptr_array_free) when done. */
static GPtrArray *
find_thread_ids (notmuch_database_t *notmuch,
		 GPtrArray *parents,
		 const char *message_id)
{
    Xapian::WritableDatabase *db = notmuch->xapian_db;
    Xapian::PostingIterator child, children_end;
    Xapian::Document doc;
    GHashTable *thread_ids;
    GList *keys, *l;
    unsigned int i;
    const char *parent_message_id;
    GPtrArray *result;

    thread_ids = g_hash_table_new_full (g_str_hash, g_str_equal,
					free, NULL);

    find_messages_by_term (db, "ref", message_id, &child, &children_end);
    for ( ; child != children_end; child++) {
	doc = find_message_by_docid (db, *child);
	insert_thread_id (thread_ids, doc);
    }

    for (i = 0; i < parents->len; i++) {
	notmuch_message_t *parent;
	notmuch_thread_ids_t *ids;

	parent_message_id = (char *) g_ptr_array_index (parents, i);
	parent = notmuch_database_find_message (notmuch, parent_message_id);
	if (parent == NULL)
	    continue;

	for (ids = notmuch_message_get_thread_ids (parent);
	     notmuch_thread_ids_has_more (ids);
	     notmuch_thread_ids_advance (ids))
	{
	    const char *id;

	    id = notmuch_thread_ids_get (ids);
	    g_hash_table_insert (thread_ids, strdup (id), NULL);
	}

	notmuch_message_destroy (parent);
    }

    result = g_ptr_array_new ();

    keys = g_hash_table_get_keys (thread_ids);
    for (l = keys; l; l = l->next) {
	char *id = (char *) l->data;
	g_ptr_array_add (result, id);
    }
    g_list_free (keys);

    /* We're done with the hash table, but we've taken the pointers to
     * the allocated strings and put them into our result array, so
     * tell the hash not to free them on its way out. */
    g_hash_table_steal_all (thread_ids);
    g_hash_table_unref (thread_ids);

    return result;
}

/* Advance 'str' past any whitespace or RFC 822 comments. A comment is
 * a (potentially nested) parenthesized sequence with '\' used to
 * escape any character (including parentheses).
 *
 * If the sequence to be skipped continues to the end of the string,
 * then 'str' will be left pointing at the final terminating '\0'
 * character.
 */
static void
skip_space_and_comments (const char **str)
{
    const char *s;

    s = *str;
    while (*s && (isspace (*s) || *s == '(')) {
	while (*s && isspace (*s))
	    s++;
	if (*s == '(') {
	    int nesting = 1;
	    s++;
	    while (*s && nesting) {
		if (*s == '(')
		    nesting++;
		else if (*s == ')')
		    nesting--;
		else if (*s == '\\')
		    if (*(s+1))
			s++;
		s++;
	    }
	}
    }

    *str = s;
}

/* Parse an RFC 822 message-id, discarding whitespace, any RFC 822
 * comments, and the '<' and '>' delimeters.
 *
 * If not NULL, then *next will be made to point to the first character
 * not parsed, (possibly pointing to the final '\0' terminator.
 *
 * Returns a newly allocated string which the caller should free()
 * when done with it.
 *
 * Returns NULL if there is any error parsing the message-id. */
static char *
parse_message_id (const char *message_id, const char **next)
{
    const char *s, *end;
    char *result;

    if (message_id == NULL)
	return NULL;

    s = message_id;

    skip_space_and_comments (&s);

    /* Skip any unstructured text as well. */
    while (*s && *s != '<')
	s++;

    if (*s == '<') {
	s++;
    } else {
	if (next)
	    *next = s;
	return NULL;
    }

    skip_space_and_comments (&s);

    end = s;
    while (*end && *end != '>')
	end++;
    if (next) {
	if (*end)
	    *next = end + 1;
	else
	    *next = end;
    }

    if (end > s && *end == '>')
	end--;
    if (end <= s)
	return NULL;

    result = strndup (s, end - s + 1);

    /* Finally, collapse any whitespace that is within the message-id
     * itself. */
    {
	char *r;
	int len;

	for (r = result, len = strlen (r); *r; r++, len--)
	    if (*r == ' ' || *r == '\t')
		memmove (r, r+1, len);
    }

    return result;
}

/* Parse a References header value, putting a copy of each referenced
 * message-id into 'array'. */
static void
parse_references (GPtrArray *array,
		  const char *refs)
{
    char *ref;

    if (refs == NULL)
	return;

    while (*refs) {
	ref = parse_message_id (refs, &refs);

	if (ref)
	    g_ptr_array_add (array, ref);
    }
}

char *
notmuch_database_default_path (void)
{
    if (getenv ("NOTMUCH_BASE"))
	return strdup (getenv ("NOTMUCH_BASE"));

    return g_strdup_printf ("%s/mail", getenv ("HOME"));
}

notmuch_database_t *
notmuch_database_create (const char *path)
{
    notmuch_database_t *notmuch = NULL;
    char *notmuch_path = NULL;
    struct stat st;
    int err;
    char *local_path = NULL;

    if (path == NULL)
	path = local_path = notmuch_database_default_path ();

    err = stat (path, &st);
    if (err) {
	fprintf (stderr, "Error: Cannot create database at %s: %s.\n",
		 path, strerror (errno));
	goto DONE;
    }

    if (! S_ISDIR (st.st_mode)) {
	fprintf (stderr, "Error: Cannot create database at %s: Not a directory.\n",
		 path);
	goto DONE;
    }

    notmuch_path = g_strdup_printf ("%s/%s", path, ".notmuch");

    err = mkdir (notmuch_path, 0755);

    if (err) {
	fprintf (stderr, "Error: Cannot create directory %s: %s.\n",
		 notmuch_path, strerror (errno));
	goto DONE;
    }

    notmuch = notmuch_database_open (path);

  DONE:
    if (notmuch_path)
	free (notmuch_path);
    if (local_path)
	free (local_path);

    return notmuch;
}

notmuch_database_t *
notmuch_database_open (const char *path)
{
    notmuch_database_t *notmuch = NULL;
    char *notmuch_path = NULL, *xapian_path = NULL;
    struct stat st;
    int err;
    char *local_path = NULL;

    if (path == NULL)
	path = local_path = notmuch_database_default_path ();

    notmuch_path = g_strdup_printf ("%s/%s", path, ".notmuch");

    err = stat (notmuch_path, &st);
    if (err) {
	fprintf (stderr, "Error opening database at %s: %s\n",
		 notmuch_path, strerror (errno));
	goto DONE;
    }

    xapian_path = g_strdup_printf ("%s/%s", notmuch_path, "xapian");

    notmuch = talloc (NULL, notmuch_database_t);
    notmuch->path = talloc_strdup (notmuch, path);

    try {
	notmuch->xapian_db = new Xapian::WritableDatabase (xapian_path,
							   Xapian::DB_CREATE_OR_OPEN);
	notmuch->query_parser = new Xapian::QueryParser;
	notmuch->query_parser->set_default_op (Xapian::Query::OP_AND);
	notmuch->query_parser->set_database (*notmuch->xapian_db);
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred: %s\n",
		 error.get_msg().c_str());
    }
    
  DONE:
    if (local_path)
	free (local_path);
    if (notmuch_path)
	free (notmuch_path);
    if (xapian_path)
	free (xapian_path);

    return notmuch;
}

void
notmuch_database_close (notmuch_database_t *notmuch)
{
    delete notmuch->query_parser;
    delete notmuch->xapian_db;
    talloc_free (notmuch);
}

const char *
notmuch_database_get_path (notmuch_database_t *notmuch)
{
    return notmuch->path;
}

notmuch_status_t
notmuch_database_add_message (notmuch_database_t *notmuch,
			      const char *filename)
{
    Xapian::WritableDatabase *db = notmuch->xapian_db;
    Xapian::Document doc;
    notmuch_message_file_t *message_file;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    GPtrArray *parents, *thread_ids;

    const char *refs, *in_reply_to, *date, *header;
    const char *from, *to, *subject;
    char *message_id;

    time_t time_value;
    unsigned int i;

    message_file = notmuch_message_file_open (filename);
    if (message_file == NULL) {
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    notmuch_message_file_restrict_headers (message_file,
					   "date",
					   "from",
					   "in-reply-to",
					   "message-id",
					   "references",
					   "subject",
					   "to",
					   (char *) NULL);

    try {
	header = notmuch_message_file_get_header (message_file, "message-id");
	if (header) {
	    message_id = parse_message_id (header, NULL);
	    /* So the header value isn't RFC-compliant, but it's
	     * better than no message-id at all. */
	    if (message_id == NULL)
		message_id = xstrdup (header);
	} else {
	    /* No message-id at all, let's generate one by taking a
	     * hash over the file's contents. */
	    char *sha1 = notmuch_sha1_of_file (filename);

	    /* If that failed too, something is really wrong. Give up. */
	    if (sha1 == NULL) {
		ret = NOTMUCH_STATUS_FILE_ERROR;
		goto DONE;
	    }

	    message_id = g_strdup_printf ("notmuch-sha1-%s", sha1);
	    free (sha1);
	}

	doc.set_data (filename);

	add_term (doc, "type", "mail");

	parents = g_ptr_array_new ();

	refs = notmuch_message_file_get_header (message_file, "references");
	parse_references (parents, refs);

	in_reply_to = notmuch_message_file_get_header (message_file, "in-reply-to");
	parse_references (parents, in_reply_to);

	for (i = 0; i < parents->len; i++)
	    add_term (doc, "ref", (char *) g_ptr_array_index (parents, i));

	thread_ids = find_thread_ids (notmuch, parents, message_id);

	for (i = 0; i < parents->len; i++)
	    g_free (g_ptr_array_index (parents, i));
	g_ptr_array_free (parents, TRUE);

	add_term (doc, "msgid", message_id);
	doc.add_value (NOTMUCH_VALUE_MESSAGE_ID, message_id);

	free (message_id);

	if (thread_ids->len) {
	    unsigned int i;
	    GString *thread_id;
	    char *id;

	    for (i = 0; i < thread_ids->len; i++) {
		id = (char *) thread_ids->pdata[i];
		add_term (doc, "thread", id);
		if (i == 0)
		    thread_id = g_string_new (id);
		else
		    g_string_append_printf (thread_id, ",%s", id);

		free (id);
	    }
	    doc.add_value (NOTMUCH_VALUE_THREAD, thread_id->str);
	    g_string_free (thread_id, TRUE);
	} else {
	    /* If not part of any existing thread, generate a new thread_id. */
	    thread_id_t thread_id;

	    thread_id_generate (&thread_id);
	    add_term (doc, "thread", thread_id.str);
	    doc.add_value (NOTMUCH_VALUE_THREAD, thread_id.str);
	}

	g_ptr_array_free (thread_ids, TRUE);

	date = notmuch_message_file_get_header (message_file, "date");
	time_value = notmuch_parse_date (date, NULL);

	doc.add_value (NOTMUCH_VALUE_DATE,
		       Xapian::sortable_serialise (time_value));

	from = notmuch_message_file_get_header (message_file, "from");
	subject = notmuch_message_file_get_header (message_file, "subject");
	to = notmuch_message_file_get_header (message_file, "to");

	if (from == NULL &&
	    subject == NULL &&
	    to == NULL)
	{
	    ret = NOTMUCH_STATUS_FILE_NOT_EMAIL;
	    goto DONE;
	} else {
	    db->add_document (doc);
	}
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred: %s.\n",
		 error.get_msg().c_str());
	ret = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	goto DONE;
    }

  DONE:
    if (message_file)
	notmuch_message_file_close (message_file);

    return ret;
}
