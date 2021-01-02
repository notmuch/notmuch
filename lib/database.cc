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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "database-private.h"
#include "string-util.h"

#include <iostream>

#include <sys/time.h>
#include <sys/stat.h>
#include <signal.h>
#include <ftw.h>

#include <glib.h>               /* g_free, GPtrArray, GHashTable */
#include <glib-object.h>        /* g_type_init */

#include <gmime/gmime.h>        /* g_mime_init */

using namespace std;

typedef struct {
    const char *name;
    const char *prefix;
    notmuch_field_flag_t flags;
} prefix_t;

#define NOTMUCH_DATABASE_VERSION 3

#define STRINGIFY(s) _SUB_STRINGIFY (s)
#define _SUB_STRINGIFY(s) #s

#define LOG_XAPIAN_EXCEPTION(message, error) _log_xapian_exception (__location__, message, error)

static void
_log_xapian_exception (const char *where, notmuch_database_t *notmuch,  const Xapian::Error error) {
    _notmuch_database_log (notmuch,
			   "A Xapian exception occurred at %s: %s\n",
			   where,
			   error.get_msg ().c_str ());
    notmuch->exception_reported = true;
}

notmuch_database_mode_t
_notmuch_database_mode (notmuch_database_t *notmuch)
{
    if (notmuch->writable_xapian_db)
	return NOTMUCH_DATABASE_MODE_READ_WRITE;
    else
	return NOTMUCH_DATABASE_MODE_READ_ONLY;
}

/* Here's the current schema for our database (for NOTMUCH_DATABASE_VERSION):
 *
 * We currently have three different types of documents (mail, ghost,
 * and directory) and also some metadata.
 *
 * There are two kinds of prefixes used in notmuch. There are the
 * human friendly 'prefix names' like "thread:", which are also used
 * in the query parser, and the actual prefix terms in the database
 * (e.g. "G"). The correspondence is maintained in the file scope data
 * structure 'prefix_table'.
 *
 * Mail document
 * -------------
 * A mail document is associated with a particular email message. It
 * is stored in one or more files on disk and is uniquely identified
 * by its "id" field (which is generally the message ID). It is
 * indexed with the following prefixed terms which the database uses
 * to construct threads, etc.:
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
 *      property:       Has a property with key=value
 *                 FIXME: if no = is present, should match on any value
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
 *	LAST_MOD:	The revision number as of the last tag or
 *			filename change.
 *
 * The prefixed terms described above are also searchable without an
 * explicit field name, but as of notmuch 0.29 this is due to
 * query-parser setup, not extra terms in the database.  In addition,
 * terms from the content of the message are added without a prefix
 * for use by the user in searching. Note that the prefix name "body"
 * is used to refer to the empty prefix string in the database.
 *
 * The path of the containing folder is added with the "folder" prefix
 * (see _notmuch_message_add_folder_terms).  Sub-paths of the the path
 * of the mail message are added with the "path" prefix.
 *
 * The data portion of a mail document is empty.
 *
 * Ghost mail document [if NOTMUCH_FEATURE_GHOSTS]
 * -----------------------------------------------
 * A ghost mail document is like a mail document, but where we don't
 * have the message content.  These are used to track thread reference
 * information for messages we haven't received.
 *
 * A ghost mail document has type: ghost; id and thread fields that
 * are identical to the mail document fields; and a MESSAGE_ID value.
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
 *	features	The set of features supported by this
 *			database. This consists of a set of
 *			'\n'-separated lines, where each is a feature
 *			name, a '\t', and compatibility flags.  If the
 *			compatibility flags contain 'w', then the
 *			opener must support this feature to safely
 *			write this database.  If the compatibility
 *			flags contain 'r', then the opener must
 *			support this feature to read this database.
 *			Introduced in database version 3.
 *
 *	last_thread_id	The last thread ID generated. This is stored
 *			as a 16-byte hexadecimal ASCII representation
 *			of a 64-bit unsigned integer. The first ID
 *			generated is 1 and the value will be
 *			incremented for each thread ID.
 *
 *	C*		metadata keys starting with C indicate
 *			configuration data. It can be managed with the
 *			n_database_*config* API.  There is a convention
 *			of hierarchical keys separated by '.' (e.g.
 *			query.notmuch stores the value for the named
 *			query 'notmuch'), but it is not enforced by the
 *			API.
 *
 * Obsolete metadata
 * -----------------
 *
 * If ! NOTMUCH_FEATURE_GHOSTS, there are no ghost mail documents.
 * Instead, the database has the following additional database
 * metadata:
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


notmuch_string_map_iterator_t *
_notmuch_database_user_headers (notmuch_database_t *notmuch)
{
    return _notmuch_string_map_iterator_create (notmuch->user_header, "", false);
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
    case NOTMUCH_STATUS_UNSUPPORTED_OPERATION:
	return "Unsupported operation";
    case NOTMUCH_STATUS_UPGRADE_REQUIRED:
	return "Operation requires a database upgrade";
    case NOTMUCH_STATUS_PATH_ERROR:
	return "Path supplied is illegal for this function";
    case NOTMUCH_STATUS_MALFORMED_CRYPTO_PROTOCOL:
	return "Crypto protocol missing, malformed, or unintelligible";
    case NOTMUCH_STATUS_FAILED_CRYPTO_CONTEXT_CREATION:
	return "Crypto engine initialization failure";
    case NOTMUCH_STATUS_UNKNOWN_CRYPTO_PROTOCOL:
	return "Unknown crypto protocol";
    default:
    case NOTMUCH_STATUS_LAST_STATUS:
	return "Unknown error status value";
    }
}

void
_notmuch_database_log (notmuch_database_t *notmuch,
		       const char *format,
		       ...)
{
    va_list va_args;

    va_start (va_args, format);

    if (notmuch->status_string)
	talloc_free (notmuch->status_string);

    notmuch->status_string = talloc_vasprintf (notmuch, format, va_args);
    va_end (va_args);
}

void
_notmuch_database_log_append (notmuch_database_t *notmuch,
			      const char *format,
			      ...)
{
    va_list va_args;

    va_start (va_args, format);

    if (notmuch->status_string)
	notmuch->status_string = talloc_vasprintf_append (notmuch->status_string, format, va_args);
    else
	notmuch->status_string = talloc_vasprintf (notmuch, format, va_args);

    va_end (va_args);
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

void
_notmuch_database_find_doc_ids (notmuch_database_t *notmuch,
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

    _notmuch_database_find_doc_ids (notmuch, prefix_name, value, &i, &end);

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
char *
_notmuch_message_id_compressed (void *ctx, const char *message_id)
{
    char *sha1, *compressed;

    sha1 = _notmuch_sha1_of_string (message_id);

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
	message_id = _notmuch_message_id_compressed (notmuch, message_id);

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
	_notmuch_database_log (notmuch, "A Xapian exception occurred finding message: %s.\n",
			       error.get_msg ().c_str ());
	notmuch->exception_reported = true;
	*message_ret = NULL;
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
}

notmuch_status_t
_notmuch_database_ensure_writable (notmuch_database_t *notmuch)
{
    if (_notmuch_database_mode (notmuch) == NOTMUCH_DATABASE_MODE_READ_ONLY) {
	_notmuch_database_log (notmuch, "Cannot write to a read-only database.\n");
	return NOTMUCH_STATUS_READ_ONLY_DATABASE;
    }

    return NOTMUCH_STATUS_SUCCESS;
}

/* Allocate a revision number for the next change. */
unsigned long
_notmuch_database_new_revision (notmuch_database_t *notmuch)
{
    unsigned long new_revision = notmuch->revision + 1;

    /* If we're in an atomic section, hold off on updating the
     * committed revision number until we commit the atomic section.
     */
    if (notmuch->atomic_nesting)
	notmuch->atomic_dirty = true;
    else
	notmuch->revision = new_revision;

    return new_revision;
}

notmuch_status_t
notmuch_database_close (notmuch_database_t *notmuch)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    /* Many Xapian objects (and thus notmuch objects) hold references to
     * the database, so merely deleting the database may not suffice to
     * close it.  Thus, we explicitly close it here. */
    if (notmuch->open) {
	try {
	    /* If there's an outstanding transaction, it's unclear if
	     * closing the Xapian database commits everything up to
	     * that transaction, or may discard committed (but
	     * unflushed) transactions.  To be certain, explicitly
	     * cancel any outstanding transaction before closing. */
	    if (_notmuch_database_mode (notmuch) == NOTMUCH_DATABASE_MODE_READ_WRITE &&
		notmuch->atomic_nesting)
		notmuch->writable_xapian_db->cancel_transaction ();

	    /* Close the database.  This implicitly flushes
	     * outstanding changes. */
	    notmuch->xapian_db->close ();
	} catch (const Xapian::Error &error) {
	    status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	    if (! notmuch->exception_reported) {
		_notmuch_database_log (notmuch, "Error: A Xapian exception occurred closing database: %s\n",
				       error.get_msg ().c_str ());
	    }
	}
    }
    notmuch->open = false;
    return status;
}

notmuch_status_t
_notmuch_database_reopen (notmuch_database_t *notmuch)
{
    if (_notmuch_database_mode (notmuch) != NOTMUCH_DATABASE_MODE_READ_ONLY)
	return NOTMUCH_STATUS_UNSUPPORTED_OPERATION;

    try {
	notmuch->xapian_db->reopen ();
    } catch (const Xapian::Error &error) {
	if (! notmuch->exception_reported) {
	    _notmuch_database_log (notmuch, "Error: A Xapian exception reopening database: %s\n",
				   error.get_msg ().c_str ());
	    notmuch->exception_reported = true;
	}
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    notmuch->view++;

    return NOTMUCH_STATUS_SUCCESS;
}

static int
unlink_cb (const char *path,
	   unused (const struct stat *sb),
	   unused (int type),
	   unused (struct FTW *ftw))
{
    return remove (path);
}

static int
rmtree (const char *path)
{
    return nftw (path, unlink_cb, 64, FTW_DEPTH | FTW_PHYS);
}

class NotmuchCompactor : public Xapian::Compactor
{
    notmuch_compact_status_cb_t status_cb;
    void *status_closure;

public:
    NotmuchCompactor(notmuch_compact_status_cb_t cb, void *closure) :
	status_cb (cb), status_closure (closure)
    {
    }

    virtual void
    set_status (const std::string &table, const std::string &status)
    {
	char *msg;

	if (status_cb == NULL)
	    return;

	if (status.length () == 0)
	    msg = talloc_asprintf (NULL, "compacting table %s", table.c_str ());
	else
	    msg = talloc_asprintf (NULL, "     %s", status.c_str ());

	if (msg == NULL) {
	    return;
	}

	status_cb (msg, status_closure);
	talloc_free (msg);
    }
};

/* Compacts the given database, optionally saving the original database
 * in backup_path. Additionally, a callback function can be provided to
 * give the user feedback on the progress of the (likely long-lived)
 * compaction process.
 *
 * The backup path must point to a directory on the same volume as the
 * original database. Passing a NULL backup_path will result in the
 * uncompacted database being deleted after compaction has finished.
 * Note that the database write lock will be held during the
 * compaction process to protect data integrity.
 */
notmuch_status_t
notmuch_database_compact (const char *path,
			  const char *backup_path,
			  notmuch_compact_status_cb_t status_cb,
			  void *closure)
{
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    notmuch_database_t *notmuch = NULL;
    char *message = NULL;

    ret = notmuch_database_open_verbose (path,
					 NOTMUCH_DATABASE_MODE_READ_WRITE,
					 &notmuch,
					 &message);
    if (ret) {
	if (status_cb) status_cb (message, closure);
	return ret;
    }

    _notmuch_config_cache (notmuch, NOTMUCH_CONFIG_DATABASE_PATH, path);

    return notmuch_database_compact_db (notmuch,
					backup_path,
					status_cb,
					closure);
}

notmuch_status_t
notmuch_database_compact_db (notmuch_database_t *notmuch,
			     const char *backup_path,
			     notmuch_compact_status_cb_t status_cb,
			     void *closure) {
    void *local;
    char *notmuch_path, *xapian_path, *compact_xapian_path;
    const char* path;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    struct stat statbuf;
    bool keep_backup;

    ret = _notmuch_database_ensure_writable (notmuch);
    if (ret)
	return ret;

    path = notmuch_config_get (notmuch, NOTMUCH_CONFIG_DATABASE_PATH);
    if (! path)
	return NOTMUCH_STATUS_PATH_ERROR;

    local = talloc_new (NULL);
    if (! local)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    if (! (notmuch_path = talloc_asprintf (local, "%s/%s", path, ".notmuch"))) {
	ret = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    if (! (xapian_path = talloc_asprintf (local, "%s/%s", notmuch_path, "xapian"))) {
	ret = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    if (! (compact_xapian_path = talloc_asprintf (local, "%s.compact", xapian_path))) {
	ret = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    if (backup_path == NULL) {
	if (! (backup_path = talloc_asprintf (local, "%s.old", xapian_path))) {
	    ret = NOTMUCH_STATUS_OUT_OF_MEMORY;
	    goto DONE;
	}
	keep_backup = false;
    } else {
	keep_backup = true;
    }

    if (stat (backup_path, &statbuf) != -1) {
	_notmuch_database_log (notmuch, "Path already exists: %s\n", backup_path);
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }
    if (errno != ENOENT) {
	_notmuch_database_log (notmuch, "Unknown error while stat()ing path: %s\n",
			       strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    /* Unconditionally attempt to remove old work-in-progress database (if
     * any). This is "protected" by database lock. If this fails due to write
     * errors (etc), the following code will fail and provide error message.
     */
    (void) rmtree (compact_xapian_path);

    try {
	NotmuchCompactor compactor (status_cb, closure);
	notmuch->xapian_db->compact (compact_xapian_path, Xapian::DBCOMPACT_NO_RENUMBER, 0, compactor);
    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch, "Error while compacting: %s\n", error.get_msg ().c_str ());
	ret = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	goto DONE;
    }

    if (rename (xapian_path, backup_path)) {
	_notmuch_database_log (notmuch, "Error moving %s to %s: %s\n",
			       xapian_path, backup_path, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    if (rename (compact_xapian_path, xapian_path)) {
	_notmuch_database_log (notmuch, "Error moving %s to %s: %s\n",
			       compact_xapian_path, xapian_path, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    if (! keep_backup) {
	if (rmtree (backup_path)) {
	    _notmuch_database_log (notmuch, "Error removing old database %s: %s\n",
				   backup_path, strerror (errno));
	    ret = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}
    }

  DONE:
    if (notmuch) {
	notmuch_status_t ret2;

	const char *str = notmuch_database_status_string (notmuch);
	if (status_cb && str)
	    status_cb (str, closure);

	ret2 = notmuch_database_destroy (notmuch);

	/* don't clobber previous error status */
	if (ret == NOTMUCH_STATUS_SUCCESS && ret2 != NOTMUCH_STATUS_SUCCESS)
	    ret = ret2;
    }

    talloc_free (local);

    return ret;
}

notmuch_status_t
notmuch_database_destroy (notmuch_database_t *notmuch)
{
    notmuch_status_t status;

    status = notmuch_database_close (notmuch);

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
    delete notmuch->last_mod_range_processor;
    notmuch->last_mod_range_processor = NULL;

    talloc_free (notmuch);

    return status;
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

    try {
	version_string = notmuch->xapian_db->get_metadata ("version");
    } catch (const Xapian::Error &error) {
	LOG_XAPIAN_EXCEPTION (notmuch, error);
	return 0;
    }

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
    unsigned int version;

    if (_notmuch_database_mode (notmuch) != NOTMUCH_DATABASE_MODE_READ_WRITE)
	return FALSE;

    if (NOTMUCH_FEATURES_CURRENT & ~notmuch->features)
	return TRUE;

    version = notmuch_database_get_version (notmuch);

    return (version > 0 && version < NOTMUCH_DATABASE_VERSION);
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
    void *local = talloc_new (NULL);
    Xapian::TermIterator t, t_end;
    Xapian::WritableDatabase *db;
    struct sigaction action;
    struct itimerval timerval;
    bool timer_is_active = false;
    enum _notmuch_features target_features, new_features;
    notmuch_status_t status;
    notmuch_private_status_t private_status;
    notmuch_query_t *query = NULL;
    unsigned int count = 0, total = 0;

    status = _notmuch_database_ensure_writable (notmuch);
    if (status)
	return status;

    db = notmuch->writable_xapian_db;

    target_features = notmuch->features | NOTMUCH_FEATURES_CURRENT;
    new_features = NOTMUCH_FEATURES_CURRENT & ~notmuch->features;

    if (! notmuch_database_needs_upgrade (notmuch))
	return NOTMUCH_STATUS_SUCCESS;

    if (progress_notify) {
	/* Set up our handler for SIGALRM */
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

	timer_is_active = true;
    }

    /* Figure out how much total work we need to do. */
    if (new_features &
	(NOTMUCH_FEATURE_FILE_TERMS | NOTMUCH_FEATURE_BOOL_FOLDER |
	 NOTMUCH_FEATURE_LAST_MOD)) {
	query = notmuch_query_create (notmuch, "");
	unsigned msg_count;

	status = notmuch_query_count_messages (query, &msg_count);
	if (status)
	    goto DONE;

	total += msg_count;
	notmuch_query_destroy (query);
	query = NULL;
    }
    if (new_features & NOTMUCH_FEATURE_DIRECTORY_DOCS) {
	t_end = db->allterms_end ("XTIMESTAMP");
	for (t = db->allterms_begin ("XTIMESTAMP"); t != t_end; t++)
	    ++total;
    }
    if (new_features & NOTMUCH_FEATURE_GHOSTS) {
	/* The ghost message upgrade converts all thread_id_*
	 * metadata values into ghost message documents. */
	t_end = db->metadata_keys_end ("thread_id_");
	for (t = db->metadata_keys_begin ("thread_id_"); t != t_end; ++t)
	    ++total;
    }

    /* Perform the upgrade in a transaction. */
    db->begin_transaction (true);

    /* Set the target features so we write out changes in the desired
     * format. */
    notmuch->features = target_features;

    /* Perform per-message upgrades. */
    if (new_features &
	(NOTMUCH_FEATURE_FILE_TERMS | NOTMUCH_FEATURE_BOOL_FOLDER |
	 NOTMUCH_FEATURE_LAST_MOD)) {
	notmuch_messages_t *messages;
	notmuch_message_t *message;
	char *filename;

	query = notmuch_query_create (notmuch, "");

	status = notmuch_query_search_messages (query, &messages);
	if (status)
	    goto DONE;
	for (;
	     notmuch_messages_valid (messages);
	     notmuch_messages_move_to_next (messages)) {
	    if (do_progress_notify) {
		progress_notify (closure, (double) count / total);
		do_progress_notify = 0;
	    }

	    message = notmuch_messages_get (messages);

	    /* Before version 1, each message document had its
	     * filename in the data field. Copy that into the new
	     * format by calling notmuch_message_add_filename.
	     */
	    if (new_features & NOTMUCH_FEATURE_FILE_TERMS) {
		filename = _notmuch_message_talloc_copy_data (message);
		if (filename && *filename != '\0') {
		    _notmuch_message_add_filename (message, filename);
		    _notmuch_message_clear_data (message);
		}
		talloc_free (filename);
	    }

	    /* Prior to version 2, the "folder:" prefix was
	     * probabilistic and stemmed. Change it to the current
	     * boolean prefix. Add "path:" prefixes while at it.
	     */
	    if (new_features & NOTMUCH_FEATURE_BOOL_FOLDER)
		_notmuch_message_upgrade_folder (message);

	    /* Prior to NOTMUCH_FEATURE_LAST_MOD, messages did not
	     * track modification revisions.  Give all messages the
	     * next available revision; since we just started tracking
	     * revisions for this database, that will be 1.
	     */
	    if (new_features & NOTMUCH_FEATURE_LAST_MOD)
		_notmuch_message_upgrade_last_mod (message);

	    _notmuch_message_sync (message);

	    notmuch_message_destroy (message);

	    count++;
	}

	notmuch_query_destroy (query);
	query = NULL;
    }

    /* Perform per-directory upgrades. */

    /* Before version 1 we stored directory timestamps in
     * XTIMESTAMP documents instead of the current XDIRECTORY
     * documents. So copy those as well. */
    if (new_features & NOTMUCH_FEATURE_DIRECTORY_DOCS) {
	t_end = notmuch->xapian_db->allterms_end ("XTIMESTAMP");

	for (t = notmuch->xapian_db->allterms_begin ("XTIMESTAMP");
	     t != t_end;
	     t++) {
	    Xapian::PostingIterator p, p_end;
	    std::string term = *t;

	    p_end = notmuch->xapian_db->postlist_end (term);

	    for (p = notmuch->xapian_db->postlist_begin (term);
		 p != p_end;
		 p++) {
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

		directory = _notmuch_directory_find_or_create (notmuch, term.c_str () + 10,
							       NOTMUCH_FIND_CREATE, &status);
		notmuch_directory_set_mtime (directory, mtime);
		notmuch_directory_destroy (directory);

		db->delete_document (*p);
	    }

	    ++count;
	}
    }

    /* Perform metadata upgrades. */

    /* Prior to NOTMUCH_FEATURE_GHOSTS, thread IDs for missing
     * messages were stored as database metadata. Change these to
     * ghost messages.
     */
    if (new_features & NOTMUCH_FEATURE_GHOSTS) {
	notmuch_message_t *message;
	std::string message_id, thread_id;

	t_end = db->metadata_keys_end (NOTMUCH_METADATA_THREAD_ID_PREFIX);
	for (t = db->metadata_keys_begin (NOTMUCH_METADATA_THREAD_ID_PREFIX);
	     t != t_end; ++t) {
	    if (do_progress_notify) {
		progress_notify (closure, (double) count / total);
		do_progress_notify = 0;
	    }

	    message_id = (*t).substr (
		strlen (NOTMUCH_METADATA_THREAD_ID_PREFIX));
	    thread_id = db->get_metadata (*t);

	    /* Create ghost message */
	    message = _notmuch_message_create_for_message_id (
		notmuch, message_id.c_str (), &private_status);
	    if (private_status == NOTMUCH_PRIVATE_STATUS_SUCCESS) {
		/* Document already exists; ignore the stored thread ID */
	    } else if (private_status ==
		       NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND) {
		private_status = _notmuch_message_initialize_ghost (
		    message, thread_id.c_str ());
		if (! private_status)
		    _notmuch_message_sync (message);
	    }

	    if (private_status) {
		_notmuch_database_log (notmuch,
				       "Upgrade failed while creating ghost messages.\n");
		status = COERCE_STATUS (private_status, "Unexpected status from _notmuch_message_initialize_ghost");
		goto DONE;
	    }

	    /* Clear saved metadata thread ID */
	    db->set_metadata (*t, "");

	    ++count;
	}
    }

    status = NOTMUCH_STATUS_SUCCESS;
    db->set_metadata ("features", _notmuch_database_print_features (local, notmuch->features));
    db->set_metadata ("version", STRINGIFY (NOTMUCH_DATABASE_VERSION));

  DONE:
    if (status == NOTMUCH_STATUS_SUCCESS)
	db->commit_transaction ();
    else
	db->cancel_transaction ();

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

    if (query)
	notmuch_query_destroy (query);

    talloc_free (local);
    return status;
}

notmuch_status_t
notmuch_database_begin_atomic (notmuch_database_t *notmuch)
{
    if (_notmuch_database_mode (notmuch) == NOTMUCH_DATABASE_MODE_READ_ONLY ||
	notmuch->atomic_nesting > 0)
	goto DONE;

    if (notmuch_database_needs_upgrade (notmuch))
	return NOTMUCH_STATUS_UPGRADE_REQUIRED;

    try {
	notmuch->writable_xapian_db->begin_transaction (false);
    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch, "A Xapian exception occurred beginning transaction: %s.\n",
			       error.get_msg ().c_str ());
	notmuch->exception_reported = true;
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

    if (_notmuch_database_mode (notmuch) == NOTMUCH_DATABASE_MODE_READ_ONLY ||
	notmuch->atomic_nesting > 1)
	goto DONE;

    db = notmuch->writable_xapian_db;
    try {
	db->commit_transaction ();

	/* This is a hack for testing.  Xapian never flushes on a
	 * non-flushed commit, even if the flush threshold is 1.
	 * However, we rely on flushing to test atomicity. */
	const char *thresh = getenv ("XAPIAN_FLUSH_THRESHOLD");
	if (thresh && atoi (thresh) == 1)
	    db->commit ();
    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch, "A Xapian exception occurred committing transaction: %s.\n",
			       error.get_msg ().c_str ());
	notmuch->exception_reported = true;
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    if (notmuch->atomic_dirty) {
	++notmuch->revision;
	notmuch->atomic_dirty = false;
    }

  DONE:
    notmuch->atomic_nesting--;
    return NOTMUCH_STATUS_SUCCESS;
}

unsigned long
notmuch_database_get_revision (notmuch_database_t *notmuch,
			       const char **uuid)
{
    if (uuid)
	*uuid = notmuch->uuid;
    return notmuch->revision;
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
	return _notmuch_sha1_of_string (path);
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
    while (slash != path && *slash == '/')
	--slash;

    /* Then, find a slash. */
    while (slash != path && *slash != '/') {
	if (basename)
	    *basename = slash;

	--slash;
    }

    /* Finally, skip multiple slashes. */
    while (slash != path && *(slash - 1) == '/')
	--slash;

    if (slash == path) {
	if (directory)
	    *directory = talloc_strdup (ctx, "");
	if (basename)
	    *basename = path;
    } else {
	if (directory)
	    *directory = talloc_strndup (ctx, path, slash - path);
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

    directory = _notmuch_directory_find_or_create (notmuch, path, flags, &status);
    if (status || ! directory) {
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
    if (status || directory_id == (unsigned int) -1) {
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
	while (*relative == '/' && *(relative + 1) == '/')
	    relative++;

	if (strncmp (relative, db_path, db_path_len) == 0) {
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
	*directory = _notmuch_directory_find_or_create (notmuch, path,
							NOTMUCH_FIND_LOOKUP, &status);
    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch, "A Xapian exception occurred getting directory: %s.\n",
			       error.get_msg ().c_str ());
	notmuch->exception_reported = true;
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

    if (! (notmuch->features & NOTMUCH_FEATURE_FILE_TERMS))
	return NOTMUCH_STATUS_UPGRADE_REQUIRED;

    /* return NULL on any failure */
    *message_ret = NULL;

    local = talloc_new (notmuch);

    try {
	status = _notmuch_database_filename_to_direntry (
	    local, notmuch, filename, NOTMUCH_FIND_LOOKUP, &direntry);
	if (status || ! direntry)
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
	_notmuch_database_log (notmuch, "Error: A Xapian exception occurred finding message by filename: %s\n",
			       error.get_msg ().c_str ());
	notmuch->exception_reported = true;
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
	i = db->xapian_db->allterms_begin ();
	end = db->xapian_db->allterms_end ();
	tags = _notmuch_database_get_terms_with_prefix (db, i, end,
							_find_prefix ("tag"));
	_notmuch_string_list_sort (tags);
	return _notmuch_tags_create (db, tags);
    } catch (const Xapian::Error &error) {
	_notmuch_database_log (db, "A Xapian exception occurred getting tags: %s.\n",
			       error.get_msg ().c_str ());
	db->exception_reported = true;
	return NULL;
    }
}

const char *
notmuch_database_status_string (const notmuch_database_t *notmuch)
{
    return notmuch->status_string;
}
