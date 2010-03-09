/* message.cc - Results of message-based searches from a notmuch database
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
#include "database-private.h"

#include <stdint.h>

#include <gmime/gmime.h>

#include <xapian.h>

struct _notmuch_message {
    notmuch_database_t *notmuch;
    Xapian::docid doc_id;
    int frozen;
    char *message_id;
    char *thread_id;
    char *in_reply_to;
    char *filename;
    notmuch_message_file_t *message_file;
    notmuch_message_list_t *replies;
    unsigned long flags;

    Xapian::Document doc;
};

/* We end up having to call the destructor explicitly because we had
 * to use "placement new" in order to initialize C++ objects within a
 * block that we allocated with talloc. So C++ is making talloc
 * slightly less simple to use, (we wouldn't need
 * talloc_set_destructor at all otherwise).
 */
static int
_notmuch_message_destructor (notmuch_message_t *message)
{
    message->doc.~Document ();

    return 0;
}

/* Create a new notmuch_message_t object for an existing document in
 * the database.
 *
 * Here, 'talloc owner' is an optional talloc context to which the new
 * message will belong. This allows for the caller to not bother
 * calling notmuch_message_destroy on the message, and no that all
 * memory will be reclaimed with 'talloc_owner' is free. The caller
 * still can call notmuch_message_destroy when finished with the
 * message if desired.
 *
 * The 'talloc_owner' argument can also be NULL, in which case the
 * caller *is* responsible for calling notmuch_message_destroy.
 *
 * If no document exists in the database with document ID of 'doc_id'
 * then this function returns NULL and optionally sets *status to
 * NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND.
 *
 * This function can also fail to due lack of available memory,
 * returning NULL and optionally setting *status to
 * NOTMUCH_PRIVATE_STATUS_OUT_OF_MEMORY.
 *
 * The caller can pass NULL for status if uninterested in
 * distinguishing these two cases.
 */
notmuch_message_t *
_notmuch_message_create (const void *talloc_owner,
			 notmuch_database_t *notmuch,
			 unsigned int doc_id,
			 notmuch_private_status_t *status)
{
    notmuch_message_t *message;

    if (status)
	*status = NOTMUCH_PRIVATE_STATUS_SUCCESS;

    message = talloc (talloc_owner, notmuch_message_t);
    if (unlikely (message == NULL)) {
	if (status)
	    *status = NOTMUCH_PRIVATE_STATUS_OUT_OF_MEMORY;
	return NULL;
    }

    message->notmuch = notmuch;
    message->doc_id = doc_id;

    message->frozen = 0;
    message->flags = 0;

    /* Each of these will be lazily created as needed. */
    message->message_id = NULL;
    message->thread_id = NULL;
    message->in_reply_to = NULL;
    message->filename = NULL;
    message->message_file = NULL;

    message->replies = _notmuch_message_list_create (message);
    if (unlikely (message->replies == NULL)) {
	if (status)
	    *status = NOTMUCH_PRIVATE_STATUS_OUT_OF_MEMORY;
	return NULL;
    }

    /* This is C++'s creepy "placement new", which is really just an
     * ugly way to call a constructor for a pre-allocated object. So
     * it's really not an error to not be checking for OUT_OF_MEMORY
     * here, since this "new" isn't actually allocating memory. This
     * is language-design comedy of the wrong kind. */

    new (&message->doc) Xapian::Document;

    talloc_set_destructor (message, _notmuch_message_destructor);

    try {
	message->doc = notmuch->xapian_db->get_document (doc_id);
    } catch (const Xapian::DocNotFoundError &error) {
	talloc_free (message);
	if (status)
	    *status = NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND;
	return NULL;
    }

    return message;
}

/* Create a new notmuch_message_t object for a specific message ID,
 * (which may or may not already exist in the database).
 *
 * The 'notmuch' database will be the talloc owner of the returned
 * message.
 *
 * If there is already a document with message ID 'message_id' in the
 * database, then the returned message can be used to query/modify the
 * document. Otherwise, a new document will be inserted into the
 * database before this function returns, (and *status will be set
 * to NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND).
 *
 * If an error occurs, this function will return NULL and *status
 * will be set as appropriate. (The status pointer argument must
 * not be NULL.)
 */
notmuch_message_t *
_notmuch_message_create_for_message_id (notmuch_database_t *notmuch,
					const char *message_id,
					notmuch_private_status_t *status_ret)
{
    notmuch_message_t *message;
    Xapian::Document doc;
    Xapian::WritableDatabase *db;
    unsigned int doc_id;
    char *term;

    *status_ret = NOTMUCH_PRIVATE_STATUS_SUCCESS;

    message = notmuch_database_find_message (notmuch, message_id);
    if (message)
	return talloc_steal (notmuch, message);

    term = talloc_asprintf (NULL, "%s%s",
			    _find_prefix ("id"), message_id);
    if (term == NULL) {
	*status_ret = NOTMUCH_PRIVATE_STATUS_OUT_OF_MEMORY;
	return NULL;
    }

    if (notmuch->mode == NOTMUCH_DATABASE_MODE_READ_ONLY)
	INTERNAL_ERROR ("Failure to ensure database is writable.");

    db = static_cast<Xapian::WritableDatabase *> (notmuch->xapian_db);
    try {
	doc.add_term (term, 0);
	talloc_free (term);

	doc.add_value (NOTMUCH_VALUE_MESSAGE_ID, message_id);

	doc_id = db->add_document (doc);
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred creating message: %s\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	*status_ret = NOTMUCH_PRIVATE_STATUS_XAPIAN_EXCEPTION;
	return NULL;
    }

    message = _notmuch_message_create (notmuch, notmuch,
				       doc_id, status_ret);

    /* We want to inform the caller that we had to create a new
     * document. */
    if (*status_ret == NOTMUCH_PRIVATE_STATUS_SUCCESS)
	*status_ret = NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND;

    return message;
}

const char *
notmuch_message_get_message_id (notmuch_message_t *message)
{
    Xapian::TermIterator i;

    if (message->message_id)
	return message->message_id;

    i = message->doc.termlist_begin ();
    i.skip_to (_find_prefix ("id"));

    if (i == message->doc.termlist_end ())
	INTERNAL_ERROR ("Message with document ID of %d has no message ID.\n",
			message->doc_id);

    message->message_id = talloc_strdup (message, (*i).c_str () + 1);

#if DEBUG_DATABASE_SANITY
    i++;

    if (i != message->doc.termlist_end () &&
	strncmp ((*i).c_str (), _find_prefix ("id"),
		 strlen (_find_prefix ("id"))) == 0)
    {
	INTERNAL_ERROR ("Mail (doc_id: %d) has duplicate message IDs",
			message->doc_id);
    }
#endif

    return message->message_id;
}

static void
_notmuch_message_ensure_message_file (notmuch_message_t *message)
{
    const char *filename;

    if (message->message_file)
	return;

    filename = notmuch_message_get_filename (message);
    if (unlikely (filename == NULL))
	return;

    message->message_file = _notmuch_message_file_open_ctx (message, filename);
}

const char *
notmuch_message_get_header (notmuch_message_t *message, const char *header)
{
    _notmuch_message_ensure_message_file (message);
    if (message->message_file == NULL)
	return NULL;

    return notmuch_message_file_get_header (message->message_file, header);
}

/* Return the message ID from the In-Reply-To header of 'message'.
 *
 * Returns an empty string ("") if 'message' has no In-Reply-To
 * header.
 *
 * Returns NULL if any error occurs.
 */
const char *
_notmuch_message_get_in_reply_to (notmuch_message_t *message)
{
    const char *prefix = _find_prefix ("replyto");
    int prefix_len = strlen (prefix);
    Xapian::TermIterator i;
    std::string in_reply_to;

    if (message->in_reply_to)
	return message->in_reply_to;

    i = message->doc.termlist_begin ();
    i.skip_to (prefix);

    if (i != message->doc.termlist_end ())
	in_reply_to = *i;

    /* It's perfectly valid for a message to have no In-Reply-To
     * header. For these cases, we return an empty string. */
    if (i == message->doc.termlist_end () ||
	strncmp (in_reply_to.c_str (), prefix, prefix_len))
    {
	message->in_reply_to = talloc_strdup (message, "");
	return message->in_reply_to;
    }

    message->in_reply_to = talloc_strdup (message,
					  in_reply_to.c_str () + prefix_len);

#if DEBUG_DATABASE_SANITY
    i++;

    in_reply_to = *i;

    if (i != message->doc.termlist_end () &&
	strncmp ((*i).c_str (), prefix, prefix_len) == 0)
    {
       INTERNAL_ERROR ("Message %s has duplicate In-Reply-To IDs: %s and %s\n",
			notmuch_message_get_message_id (message),
			message->in_reply_to,
			(*i).c_str () + prefix_len);
    }
#endif

    return message->in_reply_to;
}

const char *
notmuch_message_get_thread_id (notmuch_message_t *message)
{
    const char *prefix = _find_prefix ("thread");
    Xapian::TermIterator i;
    std::string id;

    /* This code is written with the assumption that "thread" has a
     * single-character prefix. */
    assert (strlen (prefix) == 1);

    if (message->thread_id)
	return message->thread_id;

    i = message->doc.termlist_begin ();
    i.skip_to (prefix);

    if (i != message->doc.termlist_end ())
	id = *i;

    if (i == message->doc.termlist_end () || id[0] != *prefix)
	INTERNAL_ERROR ("Message with document ID of %d has no thread ID.\n",
			message->doc_id);

    message->thread_id = talloc_strdup (message, id.c_str () + 1);

#if DEBUG_DATABASE_SANITY
    i++;
    id = *i;

    if (i != message->doc.termlist_end () && id[0] == *prefix)
    {
	INTERNAL_ERROR ("Message %s has duplicate thread IDs: %s and %s\n",
			notmuch_message_get_message_id (message),
			message->thread_id,
			id.c_str () + 1);
    }
#endif

    return message->thread_id;
}

void
_notmuch_message_add_reply (notmuch_message_t *message,
			    notmuch_message_node_t *reply)
{
    _notmuch_message_list_append (message->replies, reply);
}

notmuch_messages_t *
notmuch_message_get_replies (notmuch_message_t *message)
{
    return _notmuch_messages_create (message->replies);
}

/* Add an additional 'filename' for 'message'.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_set_sync. */
notmuch_status_t
_notmuch_message_add_filename (notmuch_message_t *message,
			       const char *filename)
{
    notmuch_status_t status;
    void *local = talloc_new (message);
    char *direntry;

    if (message->filename) {
	talloc_free (message->filename);
	message->filename = NULL;
    }

    if (filename == NULL)
	INTERNAL_ERROR ("Message filename cannot be NULL.");

    status = _notmuch_database_filename_to_direntry (local,
						     message->notmuch,
						     filename, &direntry);
    if (status)
	return status;

    _notmuch_message_add_term (message, "file-direntry", direntry);

    talloc_free (local);

    return NOTMUCH_STATUS_SUCCESS;
}

char *
_notmuch_message_talloc_copy_data (notmuch_message_t *message)
{
    return talloc_strdup (message, message->doc.get_data ().c_str ());
}

void
_notmuch_message_clear_data (notmuch_message_t *message)
{
    message->doc.set_data ("");
}

const char *
notmuch_message_get_filename (notmuch_message_t *message)
{
    const char *prefix = _find_prefix ("file-direntry");
    int prefix_len = strlen (prefix);
    Xapian::TermIterator i;
    char *colon, *direntry = NULL;
    const char *db_path, *directory, *basename;
    unsigned int directory_id;
    void *local = talloc_new (message);

    if (message->filename)
	return message->filename;

    i = message->doc.termlist_begin ();
    i.skip_to (prefix);

    if (i != message->doc.termlist_end ())
	direntry = talloc_strdup (local, (*i).c_str ());

    if (i == message->doc.termlist_end () ||
	strncmp (direntry, prefix, prefix_len))
    {
	/* A message document created by an old version of notmuch
	 * (prior to rename support) will have the filename in the
	 * data of the document rather than as a file-direntry term.
	 *
	 * It would be nice to do the upgrade of the document directly
	 * here, but the database is likely open in read-only mode. */
	const char *data;

	data = message->doc.get_data ().c_str ();

	if (data == NULL)
	    INTERNAL_ERROR ("message with no filename");

	message->filename = talloc_strdup (message, data);

	return message->filename;
    }

    direntry += prefix_len;

    directory_id = strtol (direntry, &colon, 10);

    if (colon == NULL || *colon != ':')
	INTERNAL_ERROR ("malformed direntry");

    basename = colon + 1;

    *colon = '\0';

    db_path = notmuch_database_get_path (message->notmuch);

    directory = _notmuch_database_get_directory_path (local,
						      message->notmuch,
						      directory_id);

    if (strlen (directory))
	message->filename = talloc_asprintf (message, "%s/%s/%s",
					     db_path, directory, basename);
    else
	message->filename = talloc_asprintf (message, "%s/%s",
					     db_path, basename);
    talloc_free ((void *) directory);

    talloc_free (local);

    return message->filename;
}

notmuch_bool_t
notmuch_message_get_flag (notmuch_message_t *message,
			  notmuch_message_flag_t flag)
{
    return message->flags & (1 << flag);
}

void
notmuch_message_set_flag (notmuch_message_t *message,
			  notmuch_message_flag_t flag, notmuch_bool_t enable)
{
    if (enable)
	message->flags |= (1 << flag);
    else
	message->flags &= ~(1 << flag);
}

time_t
notmuch_message_get_date (notmuch_message_t *message)
{
    std::string value;

    try {
	value = message->doc.get_value (NOTMUCH_VALUE_TIMESTAMP);
    } catch (Xapian::Error &error) {
	INTERNAL_ERROR ("Failed to read timestamp value from document.");
	return 0;
    }

    return Xapian::sortable_unserialise (value);
}

notmuch_tags_t *
notmuch_message_get_tags (notmuch_message_t *message)
{
    Xapian::TermIterator i, end;
    i = message->doc.termlist_begin();
    end = message->doc.termlist_end();
    return _notmuch_convert_tags(message, i, end);
}

void
_notmuch_message_set_date (notmuch_message_t *message,
			   const char *date)
{
    time_t time_value;

    /* GMime really doesn't want to see a NULL date, so protect its
     * sensibilities. */
    if (date == NULL || *date == '\0')
	time_value = 0;
    else
	time_value = g_mime_utils_header_decode_date (date, NULL);

    message->doc.add_value (NOTMUCH_VALUE_TIMESTAMP,
			    Xapian::sortable_serialise (time_value));
}

/* Synchronize changes made to message->doc out into the database. */
void
_notmuch_message_sync (notmuch_message_t *message)
{
    Xapian::WritableDatabase *db;

    if (message->notmuch->mode == NOTMUCH_DATABASE_MODE_READ_ONLY)
	return;

    db = static_cast <Xapian::WritableDatabase *> (message->notmuch->xapian_db);
    db->replace_document (message->doc_id, message->doc);
}

/* Ensure that 'message' is not holding any file object open. Future
 * calls to various functions will still automatically open the
 * message file as needed.
 */
void
_notmuch_message_close (notmuch_message_t *message)
{
    if (message->message_file) {
	notmuch_message_file_close (message->message_file);
	message->message_file = NULL;
    }
}

/* Add a name:value term to 'message', (the actual term will be
 * encoded by prefixing the value with a short prefix). See
 * NORMAL_PREFIX and BOOLEAN_PREFIX arrays for the mapping of term
 * names to prefix values.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_set_sync. */
notmuch_private_status_t
_notmuch_message_add_term (notmuch_message_t *message,
			   const char *prefix_name,
			   const char *value)
{

    char *term;

    if (value == NULL)
	return NOTMUCH_PRIVATE_STATUS_NULL_POINTER;

    term = talloc_asprintf (message, "%s%s",
			    _find_prefix (prefix_name), value);

    if (strlen (term) > NOTMUCH_TERM_MAX)
	return NOTMUCH_PRIVATE_STATUS_TERM_TOO_LONG;

    message->doc.add_term (term, 0);

    talloc_free (term);

    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

/* Parse 'text' and add a term to 'message' for each parsed word. Each
 * term will be added both prefixed (if prefix_name is not NULL) and
 * also unprefixed). */
notmuch_private_status_t
_notmuch_message_gen_terms (notmuch_message_t *message,
			    const char *prefix_name,
			    const char *text)
{
    Xapian::TermGenerator *term_gen = message->notmuch->term_gen;

    if (text == NULL)
	return NOTMUCH_PRIVATE_STATUS_NULL_POINTER;

    term_gen->set_document (message->doc);

    if (prefix_name) {
	const char *prefix = _find_prefix (prefix_name);

	term_gen->index_text (text, 1, prefix);
    }

    term_gen->index_text (text);

    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

/* Remove a name:value term from 'message', (the actual term will be
 * encoded by prefixing the value with a short prefix). See
 * NORMAL_PREFIX and BOOLEAN_PREFIX arrays for the mapping of term
 * names to prefix values.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_set_sync. */
notmuch_private_status_t
_notmuch_message_remove_term (notmuch_message_t *message,
			      const char *prefix_name,
			      const char *value)
{
    char *term;

    if (value == NULL)
	return NOTMUCH_PRIVATE_STATUS_NULL_POINTER;

    term = talloc_asprintf (message, "%s%s",
			    _find_prefix (prefix_name), value);

    if (strlen (term) > NOTMUCH_TERM_MAX)
	return NOTMUCH_PRIVATE_STATUS_TERM_TOO_LONG;

    try {
	message->doc.remove_term (term);
    } catch (const Xapian::InvalidArgumentError) {
	/* We'll let the philosopher's try to wrestle with the
	 * question of whether failing to remove that which was not
	 * there in the first place is failure. For us, we'll silently
	 * consider it all good. */
    }

    talloc_free (term);

    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_add_tag (notmuch_message_t *message, const char *tag)
{
    notmuch_private_status_t private_status;
    notmuch_status_t status;

    status = _notmuch_database_ensure_writable (message->notmuch);
    if (status)
	return status;

    if (tag == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (strlen (tag) > NOTMUCH_TAG_MAX)
	return NOTMUCH_STATUS_TAG_TOO_LONG;

    private_status = _notmuch_message_add_term (message, "tag", tag);
    if (private_status) {
	INTERNAL_ERROR ("_notmuch_message_add_term return unexpected value: %d\n",
			private_status);
    }

    if (! message->frozen)
	_notmuch_message_sync (message);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_remove_tag (notmuch_message_t *message, const char *tag)
{
    notmuch_private_status_t private_status;
    notmuch_status_t status;

    status = _notmuch_database_ensure_writable (message->notmuch);
    if (status)
	return status;

    if (tag == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (strlen (tag) > NOTMUCH_TAG_MAX)
	return NOTMUCH_STATUS_TAG_TOO_LONG;

    private_status = _notmuch_message_remove_term (message, "tag", tag);
    if (private_status) {
	INTERNAL_ERROR ("_notmuch_message_remove_term return unexpected value: %d\n",
			private_status);
    }

    if (! message->frozen)
	_notmuch_message_sync (message);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_remove_all_tags (notmuch_message_t *message)
{
    notmuch_private_status_t private_status;
    notmuch_status_t status;
    notmuch_tags_t *tags;
    const char *tag;

    status = _notmuch_database_ensure_writable (message->notmuch);
    if (status)
	return status;

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags))
    {
	tag = notmuch_tags_get (tags);

	private_status = _notmuch_message_remove_term (message, "tag", tag);
	if (private_status) {
	    INTERNAL_ERROR ("_notmuch_message_remove_term return unexpected value: %d\n",
			    private_status);
	}
    }

    if (! message->frozen)
	_notmuch_message_sync (message);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_freeze (notmuch_message_t *message)
{
    notmuch_status_t status;

    status = _notmuch_database_ensure_writable (message->notmuch);
    if (status)
	return status;

    message->frozen++;

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_thaw (notmuch_message_t *message)
{
    notmuch_status_t status;

    status = _notmuch_database_ensure_writable (message->notmuch);
    if (status)
	return status;

    if (message->frozen > 0) {
	message->frozen--;
	if (message->frozen == 0)
	    _notmuch_message_sync (message);
	return NOTMUCH_STATUS_SUCCESS;
    } else {
	return NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW;
    }
}

void
notmuch_message_destroy (notmuch_message_t *message)
{
    talloc_free (message);
}
