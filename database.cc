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

#include "notmuch-private.h"

#include <iostream>

#include <xapian.h>

#include <glib.h> /* g_strdup_printf, g_free, GHashTable */

using namespace std;

struct _notmuch_database {
    char *path;
    Xapian::WritableDatabase *xapian_db;
    Xapian::TermGenerator *term_gen;
};

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

/* Xapian complains if we provide a term longer than this. */
#define NOTMUCH_MAX_TERM 245

/* These prefix values are specifically chosen to be compatible
 * with sup, (http://sup.rubyforge.org), written by
 * William Morgan <wmorgan-sup@masanjin.net>, and released
 * under the GNU GPL v2.
 */

typedef struct {
    const char *name;
    const char *prefix;
} prefix_t;

prefix_t NORMAL_PREFIX[] = {
    { "subject", "S" },
    { "body", "B" },
    { "from_name", "FN" },
    { "to_name", "TN" },
    { "name", "N" },
    { "attachment", "A" }
};

prefix_t BOOLEAN_PREFIX[] = {
    { "type", "K" },
    { "from_email", "FE" },
    { "to_email", "TE" },
    { "email", "E" },
    { "date", "D" },
    { "label", "L" },
    { "source_id", "I" },
    { "attachment_extension", "O" },
    { "msgid", "Q" },
    { "thread", "H" },
    { "ref", "R" }
};

/* Similarly, these value numbers are also chosen to be sup
 * compatible. */

typedef enum {
    NOTMUCH_VALUE_MESSAGE_ID = 0,
    NOTMUCH_VALUE_THREAD = 1,
    NOTMUCH_VALUE_DATE = 2
} notmuch_value_t;

static const char *
find_prefix (const char *name)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE (NORMAL_PREFIX); i++)
	if (strcmp (name, NORMAL_PREFIX[i].name) == 0)
	    return NORMAL_PREFIX[i].prefix;

    for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX); i++)
	if (strcmp (name, BOOLEAN_PREFIX[i].name) == 0)
	    return BOOLEAN_PREFIX[i].prefix;

    return "";
}

/* "128 bits of thread-id ought to be enough for anybody" */
#define NOTMUCH_THREAD_ID_BITS	 128
#define NOTMUCH_THREAD_ID_DIGITS (NOTMUCH_THREAD_ID_BITS / 4)
typedef struct _thread_id {
    char str[NOTMUCH_THREAD_ID_DIGITS + 1];
} thread_id_t;

static void
thread_id_generate (thread_id_t *thread_id)
{
    static int seeded = 0;
    FILE *dev_random;
    uint32_t value;
    char *s;
    int i;

    if (! seeded) {
	dev_random = fopen ("/dev/random", "r");
	if (dev_random == NULL) {
	    srand (time (NULL));
	} else {
	    fread ((void *) &value, sizeof (value), 1, dev_random);
	    srand (value);
	    fclose (dev_random);
	}
	seeded = 1;
    }

    s = thread_id->str;
    for (i = 0; i < NOTMUCH_THREAD_ID_DIGITS; i += 8) {
	value = rand ();
	sprintf (s, "%08x", value);
	s += 8;
    }
}

static void
add_term (Xapian::Document doc,
	  const char *prefix_name,
	  const char *value)
{
    const char *prefix;
    char *term;

    if (value == NULL)
	return;

    prefix = find_prefix (prefix_name);

    term = g_strdup_printf ("%s%s", prefix, value);

    if (strlen (term) <= NOTMUCH_MAX_TERM)
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

    term = g_strdup_printf ("%s%s", find_prefix (prefix_name), value);

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

Xapian::Document
find_message_by_message_id (Xapian::Database *db, const char *message_id)
{
    Xapian::PostingIterator i, end;

    find_messages_by_term (db, "msgid", message_id, &i, &end);

    if (i != end)
	return find_message_by_docid (db, *i);
    else
	return Xapian::Document ();
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

/* Return one or more thread_ids, (as a GPtrArray of strings), for the
 * given message based on looking into the database for any messages
 * referenced in parents, and also for any messages in the database
 * referencing message_id.
 *
 * Caller should free all strings in the array and the array itself,
 * (g_ptr_array_free) when done. */
static GPtrArray *
find_thread_ids (Xapian::Database *db,
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

    find_messages_by_term (db, "ref", message_id, &child, &children_end);
    for ( ; child != children_end; child++) {
	doc = find_message_by_docid (db, *child);
	insert_thread_id (thread_ids, doc);
    }

    for (i = 0; i < parents->len; i++) {
	parent_message_id = (char *) g_ptr_array_index (parents, i);
	doc = find_message_by_message_id (db, parent_message_id);
	insert_thread_id (thread_ids, doc);
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
    if (end > s)
	return strndup (s, end - s + 1);
    else
	return NULL;
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

    /* C++ is so nasty in requiring these casts. I'm almost tempted to
     * write a C wrapper for Xapian... */
    notmuch = (notmuch_database_t *) xmalloc (sizeof (notmuch_database_t));
    notmuch->path = xstrdup (path);

    try {
	notmuch->xapian_db = new Xapian::WritableDatabase (xapian_path,
							   Xapian::DB_CREATE_OR_OPEN);
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
    delete notmuch->xapian_db;
    free (notmuch->path);
    free (notmuch);
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
    notmuch_message_t *message;

    GPtrArray *parents, *thread_ids;

    const char *refs, *in_reply_to, *date, *header;
    const char *from, *to, *subject;
    char *message_id;

    time_t time_value;
    unsigned int i;

    message = notmuch_message_open (filename);

    notmuch_message_restrict_headers (message,
				      "date",
				      "from",
				      "in-reply-to",
				      "message-id",
				      "references",
				      "subject",
				      (char *) NULL);

    try {
	doc = Xapian::Document ();

	doc.set_data (filename);

	parents = g_ptr_array_new ();

	refs = notmuch_message_get_header (message, "references");
	parse_references (parents, refs);

	in_reply_to = notmuch_message_get_header (message, "in-reply-to");
	parse_references (parents, in_reply_to);

	for (i = 0; i < parents->len; i++)
	    add_term (doc, "ref", (char *) g_ptr_array_index (parents, i));

	header = notmuch_message_get_header (message, "message-id");
	if (header) {
	    message_id = parse_message_id (header, NULL);
	    /* So the header value isn't RFC-compliant, but it's
	     * better than no message-id at all. */
	    if (message_id == NULL)
		message_id = xstrdup (header);
	} else {
	    /* XXX: Should generate a message_id here, (such as a SHA1
	     * sum of the message itself) */
	    message_id = NULL;
	}

	thread_ids = find_thread_ids (db, parents, message_id);

	for (i = 0; i < parents->len; i++)
	    g_free (g_ptr_array_index (parents, i));
	g_ptr_array_free (parents, TRUE);
	if (message_id) {
	    add_term (doc, "msgid", message_id);
	    doc.add_value (NOTMUCH_VALUE_MESSAGE_ID, message_id);
	}

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
	    g_ptr_array_free (thread_ids, TRUE);
	    doc.add_value (NOTMUCH_VALUE_THREAD, thread_id->str);
	    g_string_free (thread_id, TRUE);
	} else if (message_id) {
	    /* If not part of any existing thread, generate a new thread_id. */
	    thread_id_t thread_id;

	    thread_id_generate (&thread_id);
	    add_term (doc, "thread", thread_id.str);
	    doc.add_value (NOTMUCH_VALUE_THREAD, thread_id.str);
	}

	free (message_id);

	date = notmuch_message_get_header (message, "date");
	time_value = notmuch_parse_date (date, NULL);

	doc.add_value (NOTMUCH_VALUE_DATE,
		       Xapian::sortable_serialise (time_value));

	from = notmuch_message_get_header (message, "from");
	subject = notmuch_message_get_header (message, "subject");
	to = notmuch_message_get_header (message, "to");

	if (from == NULL &&
	    subject == NULL &&
	    to == NULL)
	{
	    notmuch_message_close (message);
	    return NOTMUCH_STATUS_FILE_NOT_EMAIL;
	} else {
	    db->add_document (doc);
	}
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred: %s.\n",
		 error.get_msg().c_str());
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    notmuch_message_close (message);

    return NOTMUCH_STATUS_SUCCESS;
}
