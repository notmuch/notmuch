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

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

typedef struct {
    const char *name;
    const char *prefix;
} prefix_t;

/* Here's the current schema for our database:
 *
 * We currently have two different types of documents: mail and timestamps.
 *
 * Mail document
 * -------------
 * A mail document is associated with a particular email message file
 * on disk. It is indexed with the following prefixed terms:
 *
 *    Single terms of given prefix:
 *
 *	type:	mail
 *
 *	id:	Unique ID of mail, (from Message-ID header or generated
 *		as "notmuch-sha1-<sha1_sum_of_entire_file>.
 *
 *    Multiple terms of given prefix:
 *
 *	ref:	The message IDs from all In-Reply-To and References
 *		headers in the message.
 *
 *	tag:	Any tags associated with this message by the user.
 *
 *	thread:	The thread ID of all threads to which the mail belongs
 *
 *    A mail document also has two values:
 *
 *	TIMESTAMP:	The time_t value corresponding to the message's
 *			Date header.
 *
 *	MESSAGE_ID:	The unique ID of the mail mess (see "id" above)
 *
 * Timestamp document
 * ------------------
 * A timestamp document is used by a client of the notmuch library to
 * maintain data necessary to allow for efficient polling of mail
 * directories. The notmuch library does no interpretation of
 * timestamps, but merely allows the user to store and retrieve
 * timestamps as name/value pairs.
 *
 * The timestamp document is indexed with a single prefixed term:
 *
 *	timestamp:	The user's key value (likely a directory name)
 *
 * and has a single value:
 *
 *	TIMETAMPS:	The time_t value from the user.
 */

/* With these prefix values we follow the conventions published here:
 *
 * http://xapian.org/docs/omega/termprefixes.html
 *
 * as much as makes sense. Note that I took some liberty in matching
 * the reserved prefix values to notmuch concepts, (for example, 'G'
 * is documented as "newsGroup (or similar entity - e.g. a web forum
 * name)", for which I think the thread is the closest analogue in
 * notmuch. This in spite of the fact that we will eventually be
 * storing mailing-list messages where 'G' for "mailing list name"
 * might be even a closer analogue. I'm treating the single-character
 * prefixes preferentially for core notmuch concepts (which will be
 * nearly universal to all mail messages).
 */

prefix_t BOOLEAN_PREFIX_INTERNAL[] = {
    { "type", "T" },
    { "thread", "G" },
    { "ref", "XREFERENCE" },
    { "timestamp", "XTIMESTAMP" },
};

prefix_t BOOLEAN_PREFIX_EXTERNAL[] = {
    { "tag", "K" },
    { "id", "Q" }
};

const char *
_find_prefix (const char *name)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX_INTERNAL); i++)
	if (strcmp (name, BOOLEAN_PREFIX_INTERNAL[i].name) == 0)
	    return BOOLEAN_PREFIX_INTERNAL[i].prefix;

    for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX_EXTERNAL); i++)
	if (strcmp (name, BOOLEAN_PREFIX_EXTERNAL[i].name) == 0)
	    return BOOLEAN_PREFIX_EXTERNAL[i].prefix;

    INTERNAL_ERROR ("No prefix exists for '%s'\n", name);

    return "";
}

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
    case NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
	return "Message ID is identical to a message in database";
    case NOTMUCH_STATUS_NULL_POINTER:
	return "Erroneous NULL pointer";
    case NOTMUCH_STATUS_TAG_TOO_LONG:
	return "Tag value is too long (exceeds NOTMUCH_TAG_MAX)";
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
find_doc_ids (notmuch_database_t *notmuch,
	      const char *prefix_name,
	      const char *value,
	      Xapian::PostingIterator *begin,
	      Xapian::PostingIterator *end)
{
    Xapian::PostingIterator i;
    char *term;

    term = g_strdup_printf ("%s%s", _find_prefix (prefix_name), value);

    *begin = notmuch->xapian_db->postlist_begin (term);

    *end = notmuch->xapian_db->postlist_end (term);

    free (term);
}

static notmuch_private_status_t
find_unique_doc_id (notmuch_database_t *notmuch,
		    const char *prefix_name,
		    const char *value,
		    unsigned int *doc_id)
{
    Xapian::PostingIterator i, end;

    find_doc_ids (notmuch, prefix_name, value, &i, &end);

    if (i == end) {
	*doc_id = 0;
	return NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND;
    } else {
	*doc_id = *i;
	return NOTMUCH_PRIVATE_STATUS_SUCCESS;
    }
}

static Xapian::Document
find_document_for_doc_id (notmuch_database_t *notmuch, unsigned doc_id)
{
    return notmuch->xapian_db->get_document (doc_id);
}

static notmuch_private_status_t
find_unique_document (notmuch_database_t *notmuch,
		      const char *prefix_name,
		      const char *value,
		      Xapian::Document *document,
		      unsigned int *doc_id)
{
    notmuch_private_status_t status;

    status = find_unique_doc_id (notmuch, prefix_name, value, doc_id);

    if (status) {
	*document = Xapian::Document ();
	return status;
    }

    *document = find_document_for_doc_id (notmuch, *doc_id);
    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

/* XXX: Should rewrite this to accept a notmuch_message_t* instead of
 * a Xapian:Document and then we could just use
 * notmuch_message_get_thread_ids instead of duplicating its logic
 * here. */
static void
insert_thread_id (GHashTable *thread_ids, Xapian::Document doc)
{
    string value_string;
    Xapian::TermIterator i;
    const char *prefix_str = _find_prefix ("thread");
    char prefix;

    assert (strlen (prefix_str) == 1);

    prefix = *prefix_str;

    i = doc.termlist_begin ();
    i.skip_to (prefix_str);

    while (1) {
	if (i == doc.termlist_end ())
	    break;
	value_string = *i;
	if (value_string.empty () || value_string[0] != prefix)
	    break;
	g_hash_table_insert (thread_ids,
			     strdup (value_string.c_str () + 1), NULL);
	i++;
    }
}

notmuch_message_t *
notmuch_database_find_message (notmuch_database_t *notmuch,
			       const char *message_id)
{
    notmuch_private_status_t status;
    unsigned int doc_id;

    status = find_unique_doc_id (notmuch, "id", message_id, &doc_id);

    if (status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND)
	return NULL;

    return _notmuch_message_create (notmuch, notmuch, doc_id, NULL);
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
    Xapian::PostingIterator child, children_end;
    Xapian::Document doc;
    GHashTable *thread_ids;
    GList *keys, *l;
    unsigned int i;
    const char *parent_message_id;
    GPtrArray *result;

    thread_ids = g_hash_table_new_full (g_str_hash, g_str_equal,
					free, NULL);

    find_doc_ids (notmuch, "ref", message_id, &child, &children_end);
    for ( ; child != children_end; child++) {
	doc = find_document_for_doc_id (notmuch, *child);
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
    unsigned int i;

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

	for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX_EXTERNAL); i++) {
	    prefix_t *prefix = &BOOLEAN_PREFIX_EXTERNAL[i];
	    notmuch->query_parser->add_boolean_prefix (prefix->name,
						       prefix->prefix);
	}
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

notmuch_private_status_t
find_timestamp_document (notmuch_database_t *notmuch, const char *db_key,
			 Xapian::Document *doc, unsigned int *doc_id)
{
    return find_unique_document (notmuch, "timestamp", db_key, doc, doc_id);
}

/* We allow the user to use arbitrarily long keys for timestamps,
 * (they're for filesystem paths after all, which have no limit we
 * know about). But we have a term-length limit. So if we exceed that,
 * we'll use the SHA-1 of the user's key as the actual key for
 * constructing a database term.
 *
 * Caution: This function returns a newly allocated string which the
 * caller should free() when finished.
 */
static char *
timestamp_db_key (const char *key)
{
    int term_len = strlen (_find_prefix ("timestamp")) + strlen (key);

    if (term_len > NOTMUCH_TERM_MAX)
	return notmuch_sha1_of_string (key);
    else
	return strdup (key);
}

notmuch_status_t
notmuch_database_set_timestamp (notmuch_database_t *notmuch,
				const char *key, time_t timestamp)
{
    Xapian::Document doc;
    unsigned int doc_id;
    notmuch_private_status_t status;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    char *db_key = NULL;

    db_key = timestamp_db_key (key);

    try {
	status = find_timestamp_document (notmuch, db_key, &doc, &doc_id);

	doc.add_value (NOTMUCH_VALUE_TIMESTAMP,
		       Xapian::sortable_serialise (timestamp));

	if (status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND) {
	    char *term = talloc_asprintf (NULL, "%s%s",
					  _find_prefix ("timestamp"), db_key);
	    doc.add_term (term);
	    talloc_free (term);

	    notmuch->xapian_db->add_document (doc);
	} else {
	    notmuch->xapian_db->replace_document (doc_id, doc);
	}

    } catch (Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred: %s.\n",
		 error.get_msg().c_str());
	ret = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    if (db_key)
	free (db_key);

    return ret;
}

time_t
notmuch_database_get_timestamp (notmuch_database_t *notmuch, const char *key)
{
    Xapian::Document doc;
    unsigned int doc_id;
    notmuch_private_status_t status;
    char *db_key = NULL;
    time_t ret = 0;

    db_key = timestamp_db_key (key);

    try {
	status = find_timestamp_document (notmuch, db_key, &doc, &doc_id);

	if (status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND)
	    goto DONE;

	ret =  Xapian::sortable_unserialise (doc.get_value (NOTMUCH_VALUE_TIMESTAMP));
    } catch (Xapian::Error &error) {
	goto DONE;
    }

  DONE:
    if (db_key)
	free (db_key);

    return ret;
}

/* Given a (mostly empty) 'message' and its corresponding
 * 'message_file' link it to existing threads in the database.
 *
 * We first looke at 'message_file' and its link-relevant headers
 * (References and In-Reply-To) for message IDs. We also look in the
 * database for existing message that reference 'message'.p
 *
 * The end result is to call _notmuch_message_add_thread_id with one
 * or more thread IDs to which this message belongs, (including
 * generating a new thread ID if necessary if the message doesn't
 * connect to any existing threads).
 */
static notmuch_status_t
_notmuch_database_link_message (notmuch_database_t *notmuch,
				notmuch_message_t *message,
				notmuch_message_file_t *message_file)
{
    GPtrArray *parents, *thread_ids;
    const char *refs, *in_reply_to;
    const char *message_id = notmuch_message_get_message_id (message);
    unsigned int i;

    parents = g_ptr_array_new ();

    refs = notmuch_message_file_get_header (message_file, "references");
    parse_references (parents, refs);

    in_reply_to = notmuch_message_file_get_header (message_file, "in-reply-to");
    parse_references (parents, in_reply_to);

    for (i = 0; i < parents->len; i++)
	_notmuch_message_add_term (message, "ref",
				   (char *) g_ptr_array_index (parents, i));

    thread_ids = find_thread_ids (notmuch, parents, message_id);

    for (i = 0; i < parents->len; i++)
	g_free (g_ptr_array_index (parents, i));
    g_ptr_array_free (parents, TRUE);

    if (thread_ids->len) {
	GString *thread_id;
	char *id;

	for (i = 0; i < thread_ids->len; i++) {
	    id = (char *) thread_ids->pdata[i];
	    _notmuch_message_add_thread_id (message, id);
	    if (i == 0)
		thread_id = g_string_new (id);
	    else
		g_string_append_printf (thread_id, ",%s", id);

	    free (id);
	}
	g_string_free (thread_id, TRUE);
    } else {
	_notmuch_message_ensure_thread_id (message);
    }

    g_ptr_array_free (thread_ids, TRUE);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_database_add_message (notmuch_database_t *notmuch,
			      const char *filename)
{
    notmuch_message_file_t *message_file;
    notmuch_message_t *message;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    const char *date, *header;
    const char *from, *to, *subject, *old_filename;
    char *message_id;

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
	/* The first order of business is to find/create a message ID. */

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

	/* Now that we have a message ID, we get a message object,
	 * (which may or may not reference an existing document in the
	 * database). */

	/* Use NULL for owner since we want to free this locally. */
	message = _notmuch_message_create_for_message_id (NULL,
							  notmuch,
							  message_id,
							  &ret);
	free (message_id);

	if (message == NULL)
	    goto DONE;

	/* Has a message previously been added with the same ID? */
	old_filename = notmuch_message_get_filename (message);
	if (old_filename && strlen (old_filename)) {
	    ret = NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID;
	    goto DONE;
	} else {
	    _notmuch_message_set_filename (message, filename);
	    _notmuch_message_add_term (message, "type", "mail");
	}

	ret = _notmuch_database_link_message (notmuch, message, message_file);
	if (ret)
	    goto DONE;

	date = notmuch_message_file_get_header (message_file, "date");
	_notmuch_message_set_date (message, date);

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
	    _notmuch_message_sync (message);
	}
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred: %s.\n",
		 error.get_msg().c_str());
	ret = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	goto DONE;
    }

  DONE:
    if (message)
	notmuch_message_destroy (message);
    if (message_file)
	notmuch_message_file_close (message_file);

    return ret;
}
