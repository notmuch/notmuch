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
#include "parse-time-vrp.h"

#include <iostream>

#include <sys/time.h>
#include <signal.h>

#include <glib.h> /* g_free, GPtrArray, GHashTable */
#include <glib-object.h> /* g_type_init */

#include <gmime/gmime.h> /* g_mime_init */

using namespace std;

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

typedef struct {
    const char *name;
    const char *prefix;
} prefix_t;

#define NOTMUCH_DATABASE_VERSION 1

#define STRINGIFY(s) _SUB_STRINGIFY(s)
#define _SUB_STRINGIFY(s) #s

/* Here's the current schema for our database (for NOTMUCH_DATABASE_VERSION):
 *
 * We currently have two different types of documents (mail and
 * directory) and also some metadata.
 *
 * Mail document
 * -------------
 * A mail document is associated with a particular email message file
 * on disk. It is indexed with the following prefixed terms which the
 * database uses to construct threads, etc.:
 *
 *    Single terms of given prefix:
 *
 *	type:	mail
 *
 *	id:	Unique ID of mail. This is from the Message-ID header
 *		if present and not too long (see NOTMUCH_MESSAGE_ID_MAX).
 *		If it's present and too long, then we use
 *		"notmuch-sha1-<sha1_sum_of_message_id>".
 *              If this header is not present, we use
 *		"notmuch-sha1-<sha1_sum_of_entire_file>".
 *
 *	thread:	The ID of the thread to which the mail belongs
 *
 *	replyto: The ID from the In-Reply-To header of the mail (if any).
 *
 *    Multiple terms of given prefix:
 *
 *	reference: All message IDs from In-Reply-To and References
 *		   headers in the message.
 *
 *	tag:	   Any tags associated with this message by the user.
 *
 *	file-direntry:  A colon-separated pair of values
 *		        (INTEGER:STRING), where INTEGER is the
 *		        document ID of a directory document, and
 *		        STRING is the name of a file within that
 *		        directory for this mail message.
 *
 *    A mail document also has four values:
 *
 *	TIMESTAMP:	The time_t value corresponding to the message's
 *			Date header.
 *
 *	MESSAGE_ID:	The unique ID of the mail mess (see "id" above)
 *
 *	FROM:		The value of the "From" header
 *
 *	SUBJECT:	The value of the "Subject" header
 *
 * In addition, terms from the content of the message are added with
 * "from", "to", "attachment", and "subject" prefixes for use by the
 * user in searching. Similarly, terms from the path of the mail
 * message are added with a "folder" prefix. But the database doesn't
 * really care itself about any of these.
 *
 * The data portion of a mail document is empty.
 *
 * Directory document
 * ------------------
 * A directory document is used by a client of the notmuch library to
 * maintain data necessary to allow for efficient polling of mail
 * directories.
 *
 * All directory documents contain one term:
 *
 *	directory:	The directory path (relative to the database path)
 *			Or the SHA1 sum of the directory path (if the
 *			path itself is too long to fit in a Xapian
 *			term).
 *
 * And all directory documents for directories other than top-level
 * directories also contain the following term:
 *
 *	directory-direntry: A colon-separated pair of values
 *		            (INTEGER:STRING), where INTEGER is the
 *		            document ID of the parent directory
 *		            document, and STRING is the name of this
 *		            directory within that parent.
 *
 * All directory documents have a single value:
 *
 *	TIMESTAMP:	The mtime of the directory (at last scan)
 *
 * The data portion of a directory document contains the path of the
 * directory (relative to the database path).
 *
 * Database metadata
 * -----------------
 * Xapian allows us to store arbitrary name-value pairs as
 * "metadata". We currently use the following metadata names with the
 * given meanings:
 *
 *	version		The database schema version, (which is distinct
 *			from both the notmuch package version (see
 *			notmuch --version) and the libnotmuch library
 *			version. The version is stored as an base-10
 *			ASCII integer. The initial database version
 *			was 1, (though a schema existed before that
 *			were no "version" database value existed at
 *			all). Successive versions are allocated as
 *			changes are made to the database (such as by
 *			indexing new fields).
 *
 *	last_thread_id	The last thread ID generated. This is stored
 *			as a 16-byte hexadecimal ASCII representation
 *			of a 64-bit unsigned integer. The first ID
 *			generated is 1 and the value will be
 *			incremented for each thread ID.
 *
 *	thread_id_*	A pre-allocated thread ID for a particular
 *			message. This is actually an arbitrarily large
 *			family of metadata name. Any particular name is
 *			formed by concatenating "thread_id_" with a message
 *			ID (or the SHA1 sum of a message ID if it is very
 *			long---see description of 'id' in the mail
 *			document). The value stored is a thread ID.
 *
 *			These thread ID metadata values are stored
 *			whenever a message references a parent message
 *			that does not yet exist in the database. A
 *			thread ID will be allocated and stored, and if
 *			the message is later added, the stored thread
 *			ID will be used (and the metadata value will
 *			be cleared).
 *
 *			Even before a message is added, it's
 *			pre-allocated thread ID is useful so that all
 *			descendant messages that reference this common
 *			parent can be recognized as belonging to the
 *			same thread.
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

static prefix_t BOOLEAN_PREFIX_INTERNAL[] = {
    { "type",			"T" },
    { "reference",		"XREFERENCE" },
    { "replyto",		"XREPLYTO" },
    { "directory",		"XDIRECTORY" },
    { "file-direntry",		"XFDIRENTRY" },
    { "directory-direntry",	"XDDIRENTRY" },
};

static prefix_t BOOLEAN_PREFIX_EXTERNAL[] = {
    { "thread",			"G" },
    { "tag",			"K" },
    { "is",			"K" },
    { "id",			"Q" }
};

static prefix_t PROBABILISTIC_PREFIX[]= {
    { "from",			"XFROM" },
    { "to",			"XTO" },
    { "attachment",		"XATTACHMENT" },
    { "subject",		"XSUBJECT"},
    { "folder",			"XFOLDER"}
};

const char *
_find_prefix (const char *name)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX_INTERNAL); i++) {
	if (strcmp (name, BOOLEAN_PREFIX_INTERNAL[i].name) == 0)
	    return BOOLEAN_PREFIX_INTERNAL[i].prefix;
    }

    for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX_EXTERNAL); i++) {
	if (strcmp (name, BOOLEAN_PREFIX_EXTERNAL[i].name) == 0)
	    return BOOLEAN_PREFIX_EXTERNAL[i].prefix;
    }

    for (i = 0; i < ARRAY_SIZE (PROBABILISTIC_PREFIX); i++) {
	if (strcmp (name, PROBABILISTIC_PREFIX[i].name) == 0)
	    return PROBABILISTIC_PREFIX[i].prefix;
    }

    INTERNAL_ERROR ("No prefix exists for '%s'\n", name);

    return "";
}

const char *
notmuch_status_to_string (notmuch_status_t status)
{
    switch (status) {
    case NOTMUCH_STATUS_SUCCESS:
	return "No error occurred";
    case NOTMUCH_STATUS_OUT_OF_MEMORY:
	return "Out of memory";
    case NOTMUCH_STATUS_READ_ONLY_DATABASE:
	return "Attempt to write to a read-only database";
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
    case NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW:
	return "Unbalanced number of calls to notmuch_message_freeze/thaw";
    case NOTMUCH_STATUS_UNBALANCED_ATOMIC:
	return "Unbalanced number of calls to notmuch_database_begin_atomic/end_atomic";
    default:
    case NOTMUCH_STATUS_LAST_STATUS:
	return "Unknown error status value";
    }
}

static void
find_doc_ids_for_term (notmuch_database_t *notmuch,
		       const char *term,
		       Xapian::PostingIterator *begin,
		       Xapian::PostingIterator *end)
{
    *begin = notmuch->xapian_db->postlist_begin (term);

    *end = notmuch->xapian_db->postlist_end (term);
}

static void
find_doc_ids (notmuch_database_t *notmuch,
	      const char *prefix_name,
	      const char *value,
	      Xapian::PostingIterator *begin,
	      Xapian::PostingIterator *end)
{
    char *term;

    term = talloc_asprintf (notmuch, "%s%s",
			    _find_prefix (prefix_name), value);

    find_doc_ids_for_term (notmuch, term, begin, end);

    talloc_free (term);
}

notmuch_private_status_t
_notmuch_database_find_unique_doc_id (notmuch_database_t *notmuch,
				      const char *prefix_name,
				      const char *value,
				      unsigned int *doc_id)
{
    Xapian::PostingIterator i, end;

    find_doc_ids (notmuch, prefix_name, value, &i, &end);

    if (i == end) {
	*doc_id = 0;
	return NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND;
    }

    *doc_id = *i;

#if DEBUG_DATABASE_SANITY
    i++;

    if (i != end)
	INTERNAL_ERROR ("Term %s:%s is not unique as expected.\n",
			prefix_name, value);
#endif

    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

static Xapian::Document
find_document_for_doc_id (notmuch_database_t *notmuch, unsigned doc_id)
{
    return notmuch->xapian_db->get_document (doc_id);
}

/* Generate a compressed version of 'message_id' of the form:
 *
 *	notmuch-sha1-<sha1_sum_of_message_id>
 */
static char *
_message_id_compressed (void *ctx, const char *message_id)
{
    char *sha1, *compressed;

    sha1 = notmuch_sha1_of_string (message_id);

    compressed = talloc_asprintf (ctx, "notmuch-sha1-%s", sha1);
    free (sha1);

    return compressed;
}

notmuch_status_t
notmuch_database_find_message (notmuch_database_t *notmuch,
			       const char *message_id,
			       notmuch_message_t **message_ret)
{
    notmuch_private_status_t status;
    unsigned int doc_id;

    if (message_ret == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (strlen (message_id) > NOTMUCH_MESSAGE_ID_MAX)
	message_id = _message_id_compressed (notmuch, message_id);

    try {
	status = _notmuch_database_find_unique_doc_id (notmuch, "id",
						       message_id, &doc_id);

	if (status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND)
	    *message_ret = NULL;
	else {
	    *message_ret = _notmuch_message_create (notmuch, notmuch, doc_id,
						    NULL);
	    if (*message_ret == NULL)
		return NOTMUCH_STATUS_OUT_OF_MEMORY;
	}

	return NOTMUCH_STATUS_SUCCESS;
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred finding message: %s.\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	*message_ret = NULL;
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
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
		if (*s == '(') {
		    nesting++;
		} else if (*s == ')') {
		    nesting--;
		} else if (*s == '\\') {
		    if (*(s+1))
			s++;
		}
		s++;
	    }
	}
    }

    *str = s;
}

/* Parse an RFC 822 message-id, discarding whitespace, any RFC 822
 * comments, and the '<' and '>' delimiters.
 *
 * If not NULL, then *next will be made to point to the first character
 * not parsed, (possibly pointing to the final '\0' terminator.
 *
 * Returns a newly talloc'ed string belonging to 'ctx'.
 *
 * Returns NULL if there is any error parsing the message-id. */
static char *
_parse_message_id (void *ctx, const char *message_id, const char **next)
{
    const char *s, *end;
    char *result;

    if (message_id == NULL || *message_id == '\0')
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

    result = talloc_strndup (ctx, s, end - s + 1);

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

/* Parse a References header value, putting a (talloc'ed under 'ctx')
 * copy of each referenced message-id into 'hash'.
 *
 * We explicitly avoid including any reference identical to
 * 'message_id' in the result (to avoid mass confusion when a single
 * message references itself cyclically---and yes, mail messages are
 * not infrequent in the wild that do this---don't ask me why).
 *
 * Return the last reference parsed, if it is not equal to message_id.
 */
static char *
parse_references (void *ctx,
		  const char *message_id,
		  GHashTable *hash,
		  const char *refs)
{
    char *ref;

    if (refs == NULL || *refs == '\0')
	return NULL;

    while (*refs) {
	ref = _parse_message_id (ctx, refs, &refs);

	if (ref && strcmp (ref, message_id))
	    g_hash_table_insert (hash, ref, NULL);
    }

    /* The return value of this function is used to add a parent
     * reference to the database.  We should avoid making a message
     * its own parent, thus the following check.
     */

    if (ref && strcmp(ref, message_id)) {
	return ref;
    } else {
	return NULL;
    }
}

notmuch_status_t
notmuch_database_create (const char *path, notmuch_database_t **database)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    notmuch_database_t *notmuch = NULL;
    char *notmuch_path = NULL;
    struct stat st;
    int err;

    if (path == NULL) {
	fprintf (stderr, "Error: Cannot create a database for a NULL path.\n");
	status = NOTMUCH_STATUS_NULL_POINTER;
	goto DONE;
    }

    err = stat (path, &st);
    if (err) {
	fprintf (stderr, "Error: Cannot create database at %s: %s.\n",
		 path, strerror (errno));
	status = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    if (! S_ISDIR (st.st_mode)) {
	fprintf (stderr, "Error: Cannot create database at %s: Not a directory.\n",
		 path);
	status = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    notmuch_path = talloc_asprintf (NULL, "%s/%s", path, ".notmuch");

    err = mkdir (notmuch_path, 0755);

    if (err) {
	fprintf (stderr, "Error: Cannot create directory %s: %s.\n",
		 notmuch_path, strerror (errno));
	status = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    status = notmuch_database_open (path,
				    NOTMUCH_DATABASE_MODE_READ_WRITE,
				    &notmuch);
    if (status)
	goto DONE;
    status = notmuch_database_upgrade (notmuch, NULL, NULL);
    if (status) {
	notmuch_database_close(notmuch);
	notmuch = NULL;
    }

  DONE:
    if (notmuch_path)
	talloc_free (notmuch_path);

    if (database)
	*database = notmuch;
    else
	talloc_free (notmuch);
    return status;
}

notmuch_status_t
_notmuch_database_ensure_writable (notmuch_database_t *notmuch)
{
    if (notmuch->mode == NOTMUCH_DATABASE_MODE_READ_ONLY) {
	fprintf (stderr, "Cannot write to a read-only database.\n");
	return NOTMUCH_STATUS_READ_ONLY_DATABASE;
    }

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_database_open (const char *path,
		       notmuch_database_mode_t mode,
		       notmuch_database_t **database)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    void *local = talloc_new (NULL);
    notmuch_database_t *notmuch = NULL;
    char *notmuch_path, *xapian_path;
    struct stat st;
    int err;
    unsigned int i, version;
    static int initialized = 0;

    if (path == NULL) {
	fprintf (stderr, "Error: Cannot open a database for a NULL path.\n");
	status = NOTMUCH_STATUS_NULL_POINTER;
	goto DONE;
    }

    if (! (notmuch_path = talloc_asprintf (local, "%s/%s", path, ".notmuch"))) {
	fprintf (stderr, "Out of memory\n");
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    err = stat (notmuch_path, &st);
    if (err) {
	fprintf (stderr, "Error opening database at %s: %s\n",
		 notmuch_path, strerror (errno));
	status = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    if (! (xapian_path = talloc_asprintf (local, "%s/%s", notmuch_path, "xapian"))) {
	fprintf (stderr, "Out of memory\n");
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    /* Initialize the GLib type system and threads */
    g_type_init ();

    /* Initialize gmime */
    if (! initialized) {
	g_mime_init (0);
	initialized = 1;
    }

    notmuch = talloc_zero (NULL, notmuch_database_t);
    notmuch->exception_reported = FALSE;
    notmuch->path = talloc_strdup (notmuch, path);

    if (notmuch->path[strlen (notmuch->path) - 1] == '/')
	notmuch->path[strlen (notmuch->path) - 1] = '\0';

    notmuch->needs_upgrade = FALSE;
    notmuch->mode = mode;
    notmuch->atomic_nesting = 0;
    try {
	string last_thread_id;

	if (mode == NOTMUCH_DATABASE_MODE_READ_WRITE) {
	    notmuch->xapian_db = new Xapian::WritableDatabase (xapian_path,
							       Xapian::DB_CREATE_OR_OPEN);
	    version = notmuch_database_get_version (notmuch);

	    if (version > NOTMUCH_DATABASE_VERSION) {
		fprintf (stderr,
			 "Error: Notmuch database at %s\n"
			 "       has a newer database format version (%u) than supported by this\n"
			 "       version of notmuch (%u). Refusing to open this database in\n"
			 "       read-write mode.\n",
			 notmuch_path, version, NOTMUCH_DATABASE_VERSION);
		notmuch->mode = NOTMUCH_DATABASE_MODE_READ_ONLY;
		notmuch_database_destroy (notmuch);
		notmuch = NULL;
		status = NOTMUCH_STATUS_FILE_ERROR;
		goto DONE;
	    }

	    if (version < NOTMUCH_DATABASE_VERSION)
		notmuch->needs_upgrade = TRUE;
	} else {
	    notmuch->xapian_db = new Xapian::Database (xapian_path);
	    version = notmuch_database_get_version (notmuch);
	    if (version > NOTMUCH_DATABASE_VERSION)
	    {
		fprintf (stderr,
			 "Warning: Notmuch database at %s\n"
			 "         has a newer database format version (%u) than supported by this\n"
			 "         version of notmuch (%u). Some operations may behave incorrectly,\n"
			 "         (but the database will not be harmed since it is being opened\n"
			 "         in read-only mode).\n",
			 notmuch_path, version, NOTMUCH_DATABASE_VERSION);
	    }
	}

	notmuch->last_doc_id = notmuch->xapian_db->get_lastdocid ();
	last_thread_id = notmuch->xapian_db->get_metadata ("last_thread_id");
	if (last_thread_id.empty ()) {
	    notmuch->last_thread_id = 0;
	} else {
	    const char *str;
	    char *end;

	    str = last_thread_id.c_str ();
	    notmuch->last_thread_id = strtoull (str, &end, 16);
	    if (*end != '\0')
		INTERNAL_ERROR ("Malformed database last_thread_id: %s", str);
	}

	notmuch->query_parser = new Xapian::QueryParser;
	notmuch->term_gen = new Xapian::TermGenerator;
	notmuch->term_gen->set_stemmer (Xapian::Stem ("english"));
	notmuch->value_range_processor = new Xapian::NumberValueRangeProcessor (NOTMUCH_VALUE_TIMESTAMP);
	notmuch->date_range_processor = new ParseTimeValueRangeProcessor (NOTMUCH_VALUE_TIMESTAMP);

	notmuch->query_parser->set_default_op (Xapian::Query::OP_AND);
	notmuch->query_parser->set_database (*notmuch->xapian_db);
	notmuch->query_parser->set_stemmer (Xapian::Stem ("english"));
	notmuch->query_parser->set_stemming_strategy (Xapian::QueryParser::STEM_SOME);
	notmuch->query_parser->add_valuerangeprocessor (notmuch->value_range_processor);
	notmuch->query_parser->add_valuerangeprocessor (notmuch->date_range_processor);

	for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX_EXTERNAL); i++) {
	    prefix_t *prefix = &BOOLEAN_PREFIX_EXTERNAL[i];
	    notmuch->query_parser->add_boolean_prefix (prefix->name,
						       prefix->prefix);
	}

	for (i = 0; i < ARRAY_SIZE (PROBABILISTIC_PREFIX); i++) {
	    prefix_t *prefix = &PROBABILISTIC_PREFIX[i];
	    notmuch->query_parser->add_prefix (prefix->name, prefix->prefix);
	}
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred opening database: %s\n",
		 error.get_msg().c_str());
	notmuch_database_destroy (notmuch);
	notmuch = NULL;
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

  DONE:
    talloc_free (local);

    if (database)
	*database = notmuch;
    else
	talloc_free (notmuch);
    return status;
}

void
notmuch_database_close (notmuch_database_t *notmuch)
{
    try {
	if (notmuch->xapian_db != NULL &&
	    notmuch->mode == NOTMUCH_DATABASE_MODE_READ_WRITE)
	    (static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db))->flush ();
    } catch (const Xapian::Error &error) {
	if (! notmuch->exception_reported) {
	    fprintf (stderr, "Error: A Xapian exception occurred flushing database: %s\n",
		     error.get_msg().c_str());
	}
    }

    /* Many Xapian objects (and thus notmuch objects) hold references to
     * the database, so merely deleting the database may not suffice to
     * close it.  Thus, we explicitly close it here. */
    if (notmuch->xapian_db != NULL) {
	try {
	    notmuch->xapian_db->close();
	} catch (const Xapian::Error &error) {
	    /* do nothing */
	}
    }

    delete notmuch->term_gen;
    notmuch->term_gen = NULL;
    delete notmuch->query_parser;
    notmuch->query_parser = NULL;
    delete notmuch->xapian_db;
    notmuch->xapian_db = NULL;
    delete notmuch->value_range_processor;
    notmuch->value_range_processor = NULL;
    delete notmuch->date_range_processor;
    notmuch->date_range_processor = NULL;
}

void
notmuch_database_destroy (notmuch_database_t *notmuch)
{
    notmuch_database_close (notmuch);
    talloc_free (notmuch);
}

const char *
notmuch_database_get_path (notmuch_database_t *notmuch)
{
    return notmuch->path;
}

unsigned int
notmuch_database_get_version (notmuch_database_t *notmuch)
{
    unsigned int version;
    string version_string;
    const char *str;
    char *end;

    version_string = notmuch->xapian_db->get_metadata ("version");
    if (version_string.empty ())
	return 0;

    str = version_string.c_str ();
    if (str == NULL || *str == '\0')
	return 0;

    version = strtoul (str, &end, 10);
    if (*end != '\0')
	INTERNAL_ERROR ("Malformed database version: %s", str);

    return version;
}

notmuch_bool_t
notmuch_database_needs_upgrade (notmuch_database_t *notmuch)
{
    return notmuch->needs_upgrade;
}

static volatile sig_atomic_t do_progress_notify = 0;

static void
handle_sigalrm (unused (int signal))
{
    do_progress_notify = 1;
}

/* Upgrade the current database.
 *
 * After opening a database in read-write mode, the client should
 * check if an upgrade is needed (notmuch_database_needs_upgrade) and
 * if so, upgrade with this function before making any modifications.
 *
 * The optional progress_notify callback can be used by the caller to
 * provide progress indication to the user. If non-NULL it will be
 * called periodically with 'count' as the number of messages upgraded
 * so far and 'total' the overall number of messages that will be
 * converted.
 */
notmuch_status_t
notmuch_database_upgrade (notmuch_database_t *notmuch,
			  void (*progress_notify) (void *closure,
						   double progress),
			  void *closure)
{
    Xapian::WritableDatabase *db;
    struct sigaction action;
    struct itimerval timerval;
    notmuch_bool_t timer_is_active = FALSE;
    unsigned int version;
    notmuch_status_t status;
    unsigned int count = 0, total = 0;

    status = _notmuch_database_ensure_writable (notmuch);
    if (status)
	return status;

    db = static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db);

    version = notmuch_database_get_version (notmuch);

    if (version >= NOTMUCH_DATABASE_VERSION)
	return NOTMUCH_STATUS_SUCCESS;

    if (progress_notify) {
	/* Setup our handler for SIGALRM */
	memset (&action, 0, sizeof (struct sigaction));
	action.sa_handler = handle_sigalrm;
	sigemptyset (&action.sa_mask);
	action.sa_flags = SA_RESTART;
	sigaction (SIGALRM, &action, NULL);

	/* Then start a timer to send SIGALRM once per second. */
	timerval.it_interval.tv_sec = 1;
	timerval.it_interval.tv_usec = 0;
	timerval.it_value.tv_sec = 1;
	timerval.it_value.tv_usec = 0;
	setitimer (ITIMER_REAL, &timerval, NULL);

	timer_is_active = TRUE;
    }

    /* Before version 1, each message document had its filename in the
     * data field. Copy that into the new format by calling
     * notmuch_message_add_filename.
     */
    if (version < 1) {
	notmuch_query_t *query = notmuch_query_create (notmuch, "");
	notmuch_messages_t *messages;
	notmuch_message_t *message;
	char *filename;
	Xapian::TermIterator t, t_end;

	total = notmuch_query_count_messages (query);

	for (messages = notmuch_query_search_messages (query);
	     notmuch_messages_valid (messages);
	     notmuch_messages_move_to_next (messages))
	{
	    if (do_progress_notify) {
		progress_notify (closure, (double) count / total);
		do_progress_notify = 0;
	    }

	    message = notmuch_messages_get (messages);

	    filename = _notmuch_message_talloc_copy_data (message);
	    if (filename && *filename != '\0') {
		_notmuch_message_add_filename (message, filename);
		_notmuch_message_sync (message);
	    }
	    talloc_free (filename);

	    notmuch_message_destroy (message);

	    count++;
	}

	notmuch_query_destroy (query);

	/* Also, before version 1 we stored directory timestamps in
	 * XTIMESTAMP documents instead of the current XDIRECTORY
	 * documents. So copy those as well. */

	t_end = notmuch->xapian_db->allterms_end ("XTIMESTAMP");

	for (t = notmuch->xapian_db->allterms_begin ("XTIMESTAMP");
	     t != t_end;
	     t++)
	{
	    Xapian::PostingIterator p, p_end;
	    std::string term = *t;

	    p_end = notmuch->xapian_db->postlist_end (term);

	    for (p = notmuch->xapian_db->postlist_begin (term);
		 p != p_end;
		 p++)
	    {
		Xapian::Document document;
		time_t mtime;
		notmuch_directory_t *directory;

		if (do_progress_notify) {
		    progress_notify (closure, (double) count / total);
		    do_progress_notify = 0;
		}

		document = find_document_for_doc_id (notmuch, *p);
		mtime = Xapian::sortable_unserialise (
		    document.get_value (NOTMUCH_VALUE_TIMESTAMP));

		directory = _notmuch_directory_create (notmuch, term.c_str() + 10,
						       NOTMUCH_FIND_CREATE, &status);
		notmuch_directory_set_mtime (directory, mtime);
		notmuch_directory_destroy (directory);
	    }
	}
    }

    db->set_metadata ("version", STRINGIFY (NOTMUCH_DATABASE_VERSION));
    db->flush ();

    /* Now that the upgrade is complete we can remove the old data
     * and documents that are no longer needed. */
    if (version < 1) {
	notmuch_query_t *query = notmuch_query_create (notmuch, "");
	notmuch_messages_t *messages;
	notmuch_message_t *message;
	char *filename;

	for (messages = notmuch_query_search_messages (query);
	     notmuch_messages_valid (messages);
	     notmuch_messages_move_to_next (messages))
	{
	    if (do_progress_notify) {
		progress_notify (closure, (double) count / total);
		do_progress_notify = 0;
	    }

	    message = notmuch_messages_get (messages);

	    filename = _notmuch_message_talloc_copy_data (message);
	    if (filename && *filename != '\0') {
		_notmuch_message_clear_data (message);
		_notmuch_message_sync (message);
	    }
	    talloc_free (filename);

	    notmuch_message_destroy (message);
	}

	notmuch_query_destroy (query);
    }

    if (version < 1) {
	Xapian::TermIterator t, t_end;

	t_end = notmuch->xapian_db->allterms_end ("XTIMESTAMP");

	for (t = notmuch->xapian_db->allterms_begin ("XTIMESTAMP");
	     t != t_end;
	     t++)
	{
	    Xapian::PostingIterator p, p_end;
	    std::string term = *t;

	    p_end = notmuch->xapian_db->postlist_end (term);

	    for (p = notmuch->xapian_db->postlist_begin (term);
		 p != p_end;
		 p++)
	    {
		if (do_progress_notify) {
		    progress_notify (closure, (double) count / total);
		    do_progress_notify = 0;
		}

		db->delete_document (*p);
	    }
	}
    }

    if (timer_is_active) {
	/* Now stop the timer. */
	timerval.it_interval.tv_sec = 0;
	timerval.it_interval.tv_usec = 0;
	timerval.it_value.tv_sec = 0;
	timerval.it_value.tv_usec = 0;
	setitimer (ITIMER_REAL, &timerval, NULL);

	/* And disable the signal handler. */
	action.sa_handler = SIG_IGN;
	sigaction (SIGALRM, &action, NULL);
    }

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_database_begin_atomic (notmuch_database_t *notmuch)
{
    if (notmuch->mode == NOTMUCH_DATABASE_MODE_READ_ONLY ||
	notmuch->atomic_nesting > 0)
	goto DONE;

    try {
	(static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db))->begin_transaction (false);
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred beginning transaction: %s.\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

DONE:
    notmuch->atomic_nesting++;
    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_database_end_atomic (notmuch_database_t *notmuch)
{
    Xapian::WritableDatabase *db;

    if (notmuch->atomic_nesting == 0)
	return NOTMUCH_STATUS_UNBALANCED_ATOMIC;

    if (notmuch->mode == NOTMUCH_DATABASE_MODE_READ_ONLY ||
	notmuch->atomic_nesting > 1)
	goto DONE;

    db = static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db);
    try {
	db->commit_transaction ();

	/* This is a hack for testing.  Xapian never flushes on a
	 * non-flushed commit, even if the flush threshold is 1.
	 * However, we rely on flushing to test atomicity. */
	const char *thresh = getenv ("XAPIAN_FLUSH_THRESHOLD");
	if (thresh && atoi (thresh) == 1)
	    db->flush ();
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred committing transaction: %s.\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

DONE:
    notmuch->atomic_nesting--;
    return NOTMUCH_STATUS_SUCCESS;
}

/* We allow the user to use arbitrarily long paths for directories. But
 * we have a term-length limit. So if we exceed that, we'll use the
 * SHA-1 of the path for the database term.
 *
 * Note: This function may return the original value of 'path'. If it
 * does not, then the caller is responsible to free() the returned
 * value.
 */
const char *
_notmuch_database_get_directory_db_path (const char *path)
{
    int term_len = strlen (_find_prefix ("directory")) + strlen (path);

    if (term_len > NOTMUCH_TERM_MAX)
	return notmuch_sha1_of_string (path);
    else
	return path;
}

/* Given a path, split it into two parts: the directory part is all
 * components except for the last, and the basename is that last
 * component. Getting the return-value for either part is optional
 * (the caller can pass NULL).
 *
 * The original 'path' can represent either a regular file or a
 * directory---the splitting will be carried out in the same way in
 * either case. Trailing slashes on 'path' will be ignored, and any
 * cases of multiple '/' characters appearing in series will be
 * treated as a single '/'.
 *
 * Allocation (if any) will have 'ctx' as the talloc owner. But
 * pointers will be returned within the original path string whenever
 * possible.
 *
 * Note: If 'path' is non-empty and contains no non-trailing slash,
 * (that is, consists of a filename with no parent directory), then
 * the directory returned will be an empty string. However, if 'path'
 * is an empty string, then both directory and basename will be
 * returned as NULL.
 */
notmuch_status_t
_notmuch_database_split_path (void *ctx,
			      const char *path,
			      const char **directory,
			      const char **basename)
{
    const char *slash;

    if (path == NULL || *path == '\0') {
	if (directory)
	    *directory = NULL;
	if (basename)
	    *basename = NULL;
	return NOTMUCH_STATUS_SUCCESS;
    }

    /* Find the last slash (not counting a trailing slash), if any. */

    slash = path + strlen (path) - 1;

    /* First, skip trailing slashes. */
    while (slash != path) {
	if (*slash != '/')
	    break;

	--slash;
    }

    /* Then, find a slash. */
    while (slash != path) {
	if (*slash == '/')
	    break;

	if (basename)
	    *basename = slash;

	--slash;
    }

    /* Finally, skip multiple slashes. */
    while (slash != path) {
	if (*slash != '/')
	    break;

	--slash;
    }

    if (slash == path) {
	if (directory)
	    *directory = talloc_strdup (ctx, "");
	if (basename)
	    *basename = path;
    } else {
	if (directory)
	    *directory = talloc_strndup (ctx, path, slash - path + 1);
    }

    return NOTMUCH_STATUS_SUCCESS;
}

/* Find the document ID of the specified directory.
 *
 * If (flags & NOTMUCH_FIND_CREATE), a new directory document will be
 * created if one does not exist for 'path'.  Otherwise, if the
 * directory document does not exist, this sets *directory_id to
 * ((unsigned int)-1) and returns NOTMUCH_STATUS_SUCCESS.
 */
notmuch_status_t
_notmuch_database_find_directory_id (notmuch_database_t *notmuch,
				     const char *path,
				     notmuch_find_flags_t flags,
				     unsigned int *directory_id)
{
    notmuch_directory_t *directory;
    notmuch_status_t status;

    if (path == NULL) {
	*directory_id = 0;
	return NOTMUCH_STATUS_SUCCESS;
    }

    directory = _notmuch_directory_create (notmuch, path, flags, &status);
    if (status || !directory) {
	*directory_id = -1;
	return status;
    }

    *directory_id = _notmuch_directory_get_document_id (directory);

    notmuch_directory_destroy (directory);

    return NOTMUCH_STATUS_SUCCESS;
}

const char *
_notmuch_database_get_directory_path (void *ctx,
				      notmuch_database_t *notmuch,
				      unsigned int doc_id)
{
    Xapian::Document document;

    document = find_document_for_doc_id (notmuch, doc_id);

    return talloc_strdup (ctx, document.get_data ().c_str ());
}

/* Given a legal 'filename' for the database, (either relative to
 * database path or absolute with initial components identical to
 * database path), return a new string (with 'ctx' as the talloc
 * owner) suitable for use as a direntry term value.
 *
 * If (flags & NOTMUCH_FIND_CREATE), the necessary directory documents
 * will be created in the database as needed.  Otherwise, if the
 * necessary directory documents do not exist, this sets
 * *direntry to NULL and returns NOTMUCH_STATUS_SUCCESS.
 */
notmuch_status_t
_notmuch_database_filename_to_direntry (void *ctx,
					notmuch_database_t *notmuch,
					const char *filename,
					notmuch_find_flags_t flags,
					char **direntry)
{
    const char *relative, *directory, *basename;
    Xapian::docid directory_id;
    notmuch_status_t status;

    relative = _notmuch_database_relative_path (notmuch, filename);

    status = _notmuch_database_split_path (ctx, relative,
					   &directory, &basename);
    if (status)
	return status;

    status = _notmuch_database_find_directory_id (notmuch, directory, flags,
						  &directory_id);
    if (status || directory_id == (unsigned int)-1) {
	*direntry = NULL;
	return status;
    }

    *direntry = talloc_asprintf (ctx, "%u:%s", directory_id, basename);

    return NOTMUCH_STATUS_SUCCESS;
}

/* Given a legal 'path' for the database, return the relative path.
 *
 * The return value will be a pointer to the original path contents,
 * and will be either the original string (if 'path' was relative) or
 * a portion of the string (if path was absolute and begins with the
 * database path).
 */
const char *
_notmuch_database_relative_path (notmuch_database_t *notmuch,
				 const char *path)
{
    const char *db_path, *relative;
    unsigned int db_path_len;

    db_path = notmuch_database_get_path (notmuch);
    db_path_len = strlen (db_path);

    relative = path;

    if (*relative == '/') {
	while (*relative == '/' && *(relative+1) == '/')
	    relative++;

	if (strncmp (relative, db_path, db_path_len) == 0)
	{
	    relative += db_path_len;
	    while (*relative == '/')
		relative++;
	}
    }

    return relative;
}

notmuch_status_t
notmuch_database_get_directory (notmuch_database_t *notmuch,
				const char *path,
				notmuch_directory_t **directory)
{
    notmuch_status_t status;

    if (directory == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;
    *directory = NULL;

    try {
	*directory = _notmuch_directory_create (notmuch, path,
						NOTMUCH_FIND_LOOKUP, &status);
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred getting directory: %s.\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
    return status;
}

/* Allocate a document ID that satisfies the following criteria:
 *
 * 1. The ID does not exist for any document in the Xapian database
 *
 * 2. The ID was not previously returned from this function
 *
 * 3. The ID is the smallest integer satisfying (1) and (2)
 *
 * This function will trigger an internal error if these constraints
 * cannot all be satisfied, (that is, the pool of available document
 * IDs has been exhausted).
 */
unsigned int
_notmuch_database_generate_doc_id (notmuch_database_t *notmuch)
{
    assert (notmuch->last_doc_id >= notmuch->xapian_db->get_lastdocid ());

    notmuch->last_doc_id++;

    if (notmuch->last_doc_id == 0)
	INTERNAL_ERROR ("Xapian document IDs are exhausted.\n");	

    return notmuch->last_doc_id;
}

static const char *
_notmuch_database_generate_thread_id (notmuch_database_t *notmuch)
{
    /* 16 bytes (+ terminator) for hexadecimal representation of
     * a 64-bit integer. */
    static char thread_id[17];
    Xapian::WritableDatabase *db;

    db = static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db);

    notmuch->last_thread_id++;

    sprintf (thread_id, "%016" PRIx64, notmuch->last_thread_id);

    db->set_metadata ("last_thread_id", thread_id);

    return thread_id;
}

static char *
_get_metadata_thread_id_key (void *ctx, const char *message_id)
{
    if (strlen (message_id) > NOTMUCH_MESSAGE_ID_MAX)
	message_id = _message_id_compressed (ctx, message_id);

    return talloc_asprintf (ctx, NOTMUCH_METADATA_THREAD_ID_PREFIX "%s",
			    message_id);
}

/* Find the thread ID to which the message with 'message_id' belongs.
 *
 * Note: 'thread_id_ret' must not be NULL!
 * On success '*thread_id_ret' is set to a newly talloced string belonging to
 * 'ctx'.
 *
 * Note: If there is no message in the database with the given
 * 'message_id' then a new thread_id will be allocated for this
 * message and stored in the database metadata, (where this same
 * thread ID can be looked up if the message is added to the database
 * later).
 */
static notmuch_status_t
_resolve_message_id_to_thread_id (notmuch_database_t *notmuch,
				  void *ctx,
				  const char *message_id,
				  const char **thread_id_ret)
{
    notmuch_status_t status;
    notmuch_message_t *message;
    string thread_id_string;
    char *metadata_key;
    Xapian::WritableDatabase *db;

    status = notmuch_database_find_message (notmuch, message_id, &message);

    if (status)
	return status;

    if (message) {
	*thread_id_ret = talloc_steal (ctx,
				       notmuch_message_get_thread_id (message));

	notmuch_message_destroy (message);

	return NOTMUCH_STATUS_SUCCESS;
    }

    /* Message has not been seen yet.
     *
     * We may have seen a reference to it already, in which case, we
     * can return the thread ID stored in the metadata. Otherwise, we
     * generate a new thread ID and store it there.
     */
    db = static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db);
    metadata_key = _get_metadata_thread_id_key (ctx, message_id);
    thread_id_string = notmuch->xapian_db->get_metadata (metadata_key);

    if (thread_id_string.empty()) {
	*thread_id_ret = talloc_strdup (ctx,
					_notmuch_database_generate_thread_id (notmuch));
	db->set_metadata (metadata_key, *thread_id_ret);
    } else {
	*thread_id_ret = talloc_strdup (ctx, thread_id_string.c_str());
    }

    talloc_free (metadata_key);

    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_merge_threads (notmuch_database_t *notmuch,
		const char *winner_thread_id,
		const char *loser_thread_id)
{
    Xapian::PostingIterator loser, loser_end;
    notmuch_message_t *message = NULL;
    notmuch_private_status_t private_status;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    find_doc_ids (notmuch, "thread", loser_thread_id, &loser, &loser_end);

    for ( ; loser != loser_end; loser++) {
	message = _notmuch_message_create (notmuch, notmuch,
					   *loser, &private_status);
	if (message == NULL) {
	    ret = COERCE_STATUS (private_status,
				 "Cannot find document for doc_id from query");
	    goto DONE;
	}

	_notmuch_message_remove_term (message, "thread", loser_thread_id);
	_notmuch_message_add_term (message, "thread", winner_thread_id);
	_notmuch_message_sync (message);

	notmuch_message_destroy (message);
	message = NULL;
    }

  DONE:
    if (message)
	notmuch_message_destroy (message);

    return ret;
}

static void
_my_talloc_free_for_g_hash (void *ptr)
{
    talloc_free (ptr);
}

static notmuch_status_t
_notmuch_database_link_message_to_parents (notmuch_database_t *notmuch,
					   notmuch_message_t *message,
					   notmuch_message_file_t *message_file,
					   const char **thread_id)
{
    GHashTable *parents = NULL;
    const char *refs, *in_reply_to, *in_reply_to_message_id;
    const char *last_ref_message_id, *this_message_id;
    GList *l, *keys = NULL;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    parents = g_hash_table_new_full (g_str_hash, g_str_equal,
				     _my_talloc_free_for_g_hash, NULL);
    this_message_id = notmuch_message_get_message_id (message);

    refs = notmuch_message_file_get_header (message_file, "references");
    last_ref_message_id = parse_references (message,
					    this_message_id,
					    parents, refs);

    in_reply_to = notmuch_message_file_get_header (message_file, "in-reply-to");
    in_reply_to_message_id = parse_references (message,
					       this_message_id,
					       parents, in_reply_to);

    /* For the parent of this message, use the last message ID of the
     * References header, if available.  If not, fall back to the
     * first message ID in the In-Reply-To header. */
    if (last_ref_message_id) {
        _notmuch_message_add_term (message, "replyto",
                                   last_ref_message_id);
    } else if (in_reply_to_message_id) {
	_notmuch_message_add_term (message, "replyto",
			     in_reply_to_message_id);
    }

    keys = g_hash_table_get_keys (parents);
    for (l = keys; l; l = l->next) {
	char *parent_message_id;
	const char *parent_thread_id = NULL;

	parent_message_id = (char *) l->data;

	_notmuch_message_add_term (message, "reference",
				   parent_message_id);

	ret = _resolve_message_id_to_thread_id (notmuch,
						message,
						parent_message_id,
						&parent_thread_id);
	if (ret)
	    goto DONE;

	if (*thread_id == NULL) {
	    *thread_id = talloc_strdup (message, parent_thread_id);
	    _notmuch_message_add_term (message, "thread", *thread_id);
	} else if (strcmp (*thread_id, parent_thread_id)) {
	    ret = _merge_threads (notmuch, *thread_id, parent_thread_id);
	    if (ret)
		goto DONE;
	}
    }

  DONE:
    if (keys)
	g_list_free (keys);
    if (parents)
	g_hash_table_unref (parents);

    return ret;
}

static notmuch_status_t
_notmuch_database_link_message_to_children (notmuch_database_t *notmuch,
					    notmuch_message_t *message,
					    const char **thread_id)
{
    const char *message_id = notmuch_message_get_message_id (message);
    Xapian::PostingIterator child, children_end;
    notmuch_message_t *child_message = NULL;
    const char *child_thread_id;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    notmuch_private_status_t private_status;

    find_doc_ids (notmuch, "reference", message_id, &child, &children_end);

    for ( ; child != children_end; child++) {

	child_message = _notmuch_message_create (message, notmuch,
						 *child, &private_status);
	if (child_message == NULL) {
	    ret = COERCE_STATUS (private_status,
				 "Cannot find document for doc_id from query");
	    goto DONE;
	}

	child_thread_id = notmuch_message_get_thread_id (child_message);
	if (*thread_id == NULL) {
	    *thread_id = talloc_strdup (message, child_thread_id);
	    _notmuch_message_add_term (message, "thread", *thread_id);
	} else if (strcmp (*thread_id, child_thread_id)) {
	    _notmuch_message_remove_term (child_message, "reference",
					  message_id);
	    _notmuch_message_sync (child_message);
	    ret = _merge_threads (notmuch, *thread_id, child_thread_id);
	    if (ret)
		goto DONE;
	}

	notmuch_message_destroy (child_message);
	child_message = NULL;
    }

  DONE:
    if (child_message)
	notmuch_message_destroy (child_message);

    return ret;
}

/* Given a (mostly empty) 'message' and its corresponding
 * 'message_file' link it to existing threads in the database.
 *
 * The first check is in the metadata of the database to see if we
 * have pre-allocated a thread_id in advance for this message, (which
 * would have happened if a message was previously added that
 * referenced this one).
 *
 * Second, we look at 'message_file' and its link-relevant headers
 * (References and In-Reply-To) for message IDs.
 *
 * Finally, we look in the database for existing message that
 * reference 'message'.
 *
 * In all cases, we assign to the current message the first thread_id
 * found (through either parent or child). We will also merge any
 * existing, distinct threads where this message belongs to both,
 * (which is not uncommon when messages are processed out of order).
 *
 * Finally, if no thread ID has been found through parent or child, we
 * call _notmuch_message_generate_thread_id to generate a new thread
 * ID. This should only happen for new, top-level messages, (no
 * References or In-Reply-To header in this message, and no previously
 * added message refers to this message).
 */
static notmuch_status_t
_notmuch_database_link_message (notmuch_database_t *notmuch,
				notmuch_message_t *message,
				notmuch_message_file_t *message_file)
{
    notmuch_status_t status;
    const char *message_id, *thread_id = NULL;
    char *metadata_key;
    string stored_id;

    message_id = notmuch_message_get_message_id (message);
    metadata_key = _get_metadata_thread_id_key (message, message_id);

    /* Check if we have already seen related messages to this one.
     * If we have then use the thread_id that we stored at that time.
     */
    stored_id = notmuch->xapian_db->get_metadata (metadata_key);
    if (! stored_id.empty()) {
        Xapian::WritableDatabase *db;

	db = static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db);

	/* Clear the metadata for this message ID. We don't need it
	 * anymore. */
        db->set_metadata (metadata_key, "");
        thread_id = stored_id.c_str();

        _notmuch_message_add_term (message, "thread", thread_id);
    }
    talloc_free (metadata_key);

    status = _notmuch_database_link_message_to_parents (notmuch, message,
							message_file,
							&thread_id);
    if (status)
	return status;

    status = _notmuch_database_link_message_to_children (notmuch, message,
							 &thread_id);
    if (status)
	return status;

    /* If not part of any existing thread, generate a new thread ID. */
    if (thread_id == NULL) {
	thread_id = _notmuch_database_generate_thread_id (notmuch);

	_notmuch_message_add_term (message, "thread", thread_id);
    }

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_database_add_message (notmuch_database_t *notmuch,
			      const char *filename,
			      notmuch_message_t **message_ret)
{
    notmuch_message_file_t *message_file;
    notmuch_message_t *message = NULL;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS, ret2;
    notmuch_private_status_t private_status;

    const char *date, *header;
    const char *from, *to, *subject;
    char *message_id = NULL;

    if (message_ret)
	*message_ret = NULL;

    ret = _notmuch_database_ensure_writable (notmuch);
    if (ret)
	return ret;

    message_file = notmuch_message_file_open (filename);
    if (message_file == NULL)
	return NOTMUCH_STATUS_FILE_ERROR;

    /* Adding a message may change many documents.  Do this all
     * atomically. */
    ret = notmuch_database_begin_atomic (notmuch);
    if (ret)
	goto DONE;

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
	/* Before we do any real work, (especially before doing a
	 * potential SHA-1 computation on the entire file's contents),
	 * let's make sure that what we're looking at looks like an
	 * actual email message.
	 */
	from = notmuch_message_file_get_header (message_file, "from");
	subject = notmuch_message_file_get_header (message_file, "subject");
	to = notmuch_message_file_get_header (message_file, "to");

	if ((from == NULL || *from == '\0') &&
	    (subject == NULL || *subject == '\0') &&
	    (to == NULL || *to == '\0'))
	{
	    ret = NOTMUCH_STATUS_FILE_NOT_EMAIL;
	    goto DONE;
	}

	/* Now that we're sure it's mail, the first order of business
	 * is to find a message ID (or else create one ourselves). */

	header = notmuch_message_file_get_header (message_file, "message-id");
	if (header && *header != '\0') {
	    message_id = _parse_message_id (message_file, header, NULL);

	    /* So the header value isn't RFC-compliant, but it's
	     * better than no message-id at all. */
	    if (message_id == NULL)
		message_id = talloc_strdup (message_file, header);

	    /* If a message ID is too long, substitute its sha1 instead. */
	    if (message_id && strlen (message_id) > NOTMUCH_MESSAGE_ID_MAX) {
		char *compressed = _message_id_compressed (message_file,
							   message_id);
		talloc_free (message_id);
		message_id = compressed;
	    }
	}

	if (message_id == NULL ) {
	    /* No message-id at all, let's generate one by taking a
	     * hash over the file's contents. */
	    char *sha1 = notmuch_sha1_of_file (filename);

	    /* If that failed too, something is really wrong. Give up. */
	    if (sha1 == NULL) {
		ret = NOTMUCH_STATUS_FILE_ERROR;
		goto DONE;
	    }

	    message_id = talloc_asprintf (message_file,
					  "notmuch-sha1-%s", sha1);
	    free (sha1);
	}

	/* Now that we have a message ID, we get a message object,
	 * (which may or may not reference an existing document in the
	 * database). */

	message = _notmuch_message_create_for_message_id (notmuch,
							  message_id,
							  &private_status);

	talloc_free (message_id);

	if (message == NULL) {
	    ret = COERCE_STATUS (private_status,
				 "Unexpected status value from _notmuch_message_create_for_message_id");
	    goto DONE;
	}

	_notmuch_message_add_filename (message, filename);

	/* Is this a newly created message object? */
	if (private_status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND) {
	    _notmuch_message_add_term (message, "type", "mail");

	    ret = _notmuch_database_link_message (notmuch, message,
						  message_file);
	    if (ret)
		goto DONE;

	    date = notmuch_message_file_get_header (message_file, "date");
	    _notmuch_message_set_header_values (message, date, from, subject);

	    ret = _notmuch_message_index_file (message, filename);
	    if (ret)
		goto DONE;
	} else {
	    ret = NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID;
	}

	_notmuch_message_sync (message);
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred adding message: %s.\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	ret = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	goto DONE;
    }

  DONE:
    if (message) {
	if ((ret == NOTMUCH_STATUS_SUCCESS ||
	     ret == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) && message_ret)
	    *message_ret = message;
	else
	    notmuch_message_destroy (message);
    }

    if (message_file)
	notmuch_message_file_close (message_file);

    ret2 = notmuch_database_end_atomic (notmuch);
    if ((ret == NOTMUCH_STATUS_SUCCESS ||
	 ret == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) &&
	ret2 != NOTMUCH_STATUS_SUCCESS)
	ret = ret2;

    return ret;
}

notmuch_status_t
notmuch_database_remove_message (notmuch_database_t *notmuch,
				 const char *filename)
{
    notmuch_status_t status;
    notmuch_message_t *message;

    status = notmuch_database_find_message_by_filename (notmuch, filename,
							&message);

    if (status == NOTMUCH_STATUS_SUCCESS && message) {
	    status = _notmuch_message_remove_filename (message, filename);
	    if (status == NOTMUCH_STATUS_SUCCESS)
		_notmuch_message_delete (message);
	    else if (status == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID)
		_notmuch_message_sync (message);

	    notmuch_message_destroy (message);
    }

    return status;
}

notmuch_status_t
notmuch_database_find_message_by_filename (notmuch_database_t *notmuch,
					   const char *filename,
					   notmuch_message_t **message_ret)
{
    void *local;
    const char *prefix = _find_prefix ("file-direntry");
    char *direntry, *term;
    Xapian::PostingIterator i, end;
    notmuch_status_t status;

    if (message_ret == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    /* return NULL on any failure */
    *message_ret = NULL;

    local = talloc_new (notmuch);

    try {
	status = _notmuch_database_filename_to_direntry (
	    local, notmuch, filename, NOTMUCH_FIND_LOOKUP, &direntry);
	if (status || !direntry)
	    goto DONE;

	term = talloc_asprintf (local, "%s%s", prefix, direntry);

	find_doc_ids_for_term (notmuch, term, &i, &end);

	if (i != end) {
	    notmuch_private_status_t private_status;

	    *message_ret = _notmuch_message_create (notmuch, notmuch, *i,
						    &private_status);
	    if (*message_ret == NULL)
		status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	}
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "Error: A Xapian exception occurred finding message by filename: %s\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

  DONE:
    talloc_free (local);

    if (status && *message_ret) {
	notmuch_message_destroy (*message_ret);
	*message_ret = NULL;
    }
    return status;
}

notmuch_string_list_t *
_notmuch_database_get_terms_with_prefix (void *ctx, Xapian::TermIterator &i,
					 Xapian::TermIterator &end,
					 const char *prefix)
{
    int prefix_len = strlen (prefix);
    notmuch_string_list_t *list;

    list = _notmuch_string_list_create (ctx);
    if (unlikely (list == NULL))
	return NULL;

    for (i.skip_to (prefix); i != end; i++) {
	/* Terminate loop at first term without desired prefix. */
	if (strncmp ((*i).c_str (), prefix, prefix_len))
	    break;

	_notmuch_string_list_append (list, (*i).c_str () + prefix_len);
    }

    return list;
}

notmuch_tags_t *
notmuch_database_get_all_tags (notmuch_database_t *db)
{
    Xapian::TermIterator i, end;
    notmuch_string_list_t *tags;

    try {
	i = db->xapian_db->allterms_begin();
	end = db->xapian_db->allterms_end();
	tags = _notmuch_database_get_terms_with_prefix (db, i, end,
							_find_prefix ("tag"));
	_notmuch_string_list_sort (tags);
	return _notmuch_tags_create (db, tags);
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred getting tags: %s.\n",
		 error.get_msg().c_str());
	db->exception_reported = TRUE;
	return NULL;
    }
}
