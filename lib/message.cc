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

struct _notmuch_message {
    notmuch_database_t *notmuch;
    Xapian::docid doc_id;
    int frozen;
    char *message_id;
    char *thread_id;
    char *in_reply_to;
    notmuch_filename_list_t *filename_list;
    char *author;
    notmuch_message_file_t *message_file;
    notmuch_message_list_t *replies;
    unsigned long flags;

    Xapian::Document doc;
};

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

struct maildir_flag_tag {
    char flag;
    const char *tag;
    bool inverse;
};

/* ASCII ordered table of Maildir flags and associated tags */
struct maildir_flag_tag flag2tag[] = {
    { 'D', "draft",   false},
    { 'F', "flagged", false},
    { 'P', "passed",  false},
    { 'R', "replied", false},
    { 'S', "unread",  true }
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

static notmuch_message_t *
_notmuch_message_create_for_document (const void *talloc_owner,
				      notmuch_database_t *notmuch,
				      unsigned int doc_id,
				      Xapian::Document doc,
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
    message->filename_list = NULL;
    message->message_file = NULL;
    message->author = NULL;

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

    message->doc = doc;

    return message;
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
    Xapian::Document doc;

    try {
	doc = notmuch->xapian_db->get_document (doc_id);
    } catch (const Xapian::DocNotFoundError &error) {
	if (status)
	    *status = NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND;
	return NULL;
    }

    return _notmuch_message_create_for_document (talloc_owner, notmuch,
						 doc_id, doc, status);
}

/* Create a new notmuch_message_t object for a specific message ID,
 * (which may or may not already exist in the database).
 *
 * The 'notmuch' database will be the talloc owner of the returned
 * message.
 *
 * This function returns a valid notmuch_message_t whether or not
 * there is already a document in the database with the given message
 * ID. These two cases can be distinguished by the value of *status:
 *
 *
 *   NOTMUCH_PRIVATE_STATUS_SUCCESS:
 *
 *     There is already a document with message ID 'message_id' in the
 *     database. The returned message can be used to query/modify the
 *     document.
 *   NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND:
 *
 *     No document with 'message_id' exists in the database. The
 *     returned message contains a newly created document (not yet
 *     added to the database) and a document ID that is known not to
 *     exist in the database. The caller can modify the message, and a
 *     call to _notmuch_message_sync will add * the document to the
 *     database.
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

	doc_id = _notmuch_database_generate_doc_id (notmuch);
    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred creating message: %s\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	*status_ret = NOTMUCH_PRIVATE_STATUS_XAPIAN_EXCEPTION;
	return NULL;
    }

    message = _notmuch_message_create_for_document (notmuch, notmuch,
						    doc_id, doc, status_ret);

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
 * call to _notmuch_message_sync. */
notmuch_status_t
_notmuch_message_add_filename (notmuch_message_t *message,
			       const char *filename)
{
    notmuch_status_t status;
    void *local = talloc_new (message);
    char *direntry;

    if (message->filename_list) {
	_notmuch_filename_list_destroy (message->filename_list);
	message->filename_list = NULL;
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

static void
_notmuch_message_ensure_filename_list (notmuch_message_t *message)
{
    const char *prefix = _find_prefix ("file-direntry");
    int prefix_len = strlen (prefix);
    Xapian::TermIterator i;

    if (message->filename_list)
	return;

    message->filename_list = _notmuch_filename_list_create (message);

    i = message->doc.termlist_begin ();
    i.skip_to (prefix);

    if (i == message->doc.termlist_end () ||
	strncmp ((*i).c_str (), prefix, prefix_len))
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

	_notmuch_filename_list_add_filename (message->filename_list, data);

	return;
    }

    for (; i != message->doc.termlist_end (); i++) {
	void *local = talloc_new (message);
	const char *db_path, *directory, *basename, *filename;
	char *colon, *direntry = NULL;
	unsigned int directory_id;

	/* Terminate loop at first term without desired prefix. */
	if (strncmp ((*i).c_str (), prefix, prefix_len))
	    break;

	direntry = talloc_strdup (local, (*i).c_str ());

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
	    filename = talloc_asprintf (message, "%s/%s/%s",
					db_path, directory, basename);
	else
	    filename = talloc_asprintf (message, "%s/%s",
					db_path, basename);

	_notmuch_filename_list_add_filename (message->filename_list,
					     filename);

	talloc_free (local);
    }
}

const char *
notmuch_message_get_filename (notmuch_message_t *message)
{
    _notmuch_message_ensure_filename_list (message);

    if (message->filename_list == NULL)
	return NULL;

    if (message->filename_list->head == NULL ||
	message->filename_list->head->filename == NULL)
    {
	INTERNAL_ERROR ("message with no filename");
    }

    return message->filename_list->head->filename;
}

notmuch_filenames_t *
notmuch_message_get_filenames (notmuch_message_t *message)
{
    _notmuch_message_ensure_filename_list (message);

    return _notmuch_filenames_create (message, message->filename_list);
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

const char *
notmuch_message_get_author (notmuch_message_t *message)
{
    return message->author;
}

void
notmuch_message_set_author (notmuch_message_t *message,
			    const char *author)
{
    if (message->author)
	talloc_free(message->author);
    message->author = talloc_strdup(message, author);
    return;
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
 * call to _notmuch_message_sync. */
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
 * call to _notmuch_message_sync. */
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

/* Change the message filename stored in the database.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_sync.
 */
notmuch_status_t
_notmuch_message_rename (notmuch_message_t *message,
			 const char *new_filename)
{
    void *local = talloc_new (message);
    char *direntry;
    Xapian::PostingIterator i, end;
    Xapian::Document document;
    notmuch_private_status_t private_status;
    notmuch_status_t status;
    const char *old_filename;

    old_filename = notmuch_message_get_filename(message);
    old_filename = talloc_reference(local, old_filename);
    if (unlikely (! old_filename))
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    status = _notmuch_message_add_filename (message, new_filename);
    if (status)
	return status;

    status = _notmuch_database_filename_to_direntry (local, message->notmuch,
						     old_filename, &direntry);
    if (status)
	return status;

    private_status = _notmuch_message_remove_term (message, "file-direntry", direntry);
    status = COERCE_STATUS (private_status,
			    "Unexpected error from _notmuch_message_remove_term");

    talloc_free (local);

    return status;
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
notmuch_message_maildir_flags_to_tags (notmuch_message_t *message)
{
    const char *flags;
    notmuch_status_t status;
    notmuch_filenames_t *filenames;
    const char *filename;
    char *combined_flags = talloc_strdup (message, "");
    unsigned i;
    int seen_maildir_info = 0;

    for (filenames = notmuch_message_get_filenames (message);
	 notmuch_filenames_valid (filenames);
	 notmuch_filenames_move_to_next (filenames))
    {
	filename = notmuch_filenames_get (filenames);

	flags = strstr (filename, ":2,");
	if (! flags)
	    continue;

	seen_maildir_info = 1;
	flags += 3;

	combined_flags = talloc_strdup_append (combined_flags, flags);
    }

    /* If none of the filenames have any maildir info field (not even
     * an empty info with no flags set) then there's no information to
     * go on, so do nothing. */
    if (! seen_maildir_info)
	return NOTMUCH_STATUS_SUCCESS;

    status = notmuch_message_freeze (message);
    if (status)
	return status;

    for (i = 0; i < ARRAY_SIZE(flag2tag); i++) {
	if ((strchr (combined_flags, flag2tag[i].flag) != NULL)
	    ^ 
	    flag2tag[i].inverse)
	{
	    status = notmuch_message_add_tag (message, flag2tag[i].tag);
	} else {
	    status = notmuch_message_remove_tag (message, flag2tag[i].tag);
	}
	if (status)
	    return status;
    }
    status = notmuch_message_thaw (message);

    talloc_free (combined_flags);

    return status;
}

static void
maildir_get_new_flags(notmuch_message_t *message, char *flags)
{
    notmuch_tags_t *tags;
    const char *tag;
    unsigned i;
    char *p;

    for (i = 0; i < ARRAY_SIZE(flag2tag); i++)
	flags[i] = flag2tag[i].inverse ? flag2tag[i].flag : '\0';

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags))
    {
	tag = notmuch_tags_get (tags);
	for (i = 0; i < ARRAY_SIZE(flag2tag); i++) {
	    if (strcmp(tag, flag2tag[i].tag) == 0)
		flags[i] = flag2tag[i].inverse ? '\0' : flag2tag[i].flag;
	}
    }

    p = flags;
    for (i = 0; i < ARRAY_SIZE(flag2tag); i++) {
	if (flags[i])
	    *p++ = flags[i];
    }
    *p = '\0';
}

static char *
maildir_get_subdir (char *filename)
{
    char *p, *subdir = NULL;

    p = filename + strlen (filename) - 1;
    while (p > filename + 3 && *p != '/')
	p--;
    if (*p == '/') {
	subdir = p - 3;
	if (subdir > filename && *(subdir - 1) != '/')
	    subdir = NULL;
    }
    return subdir;
}

/* XXX: Needs to iterate over all filenames in the message
 *
 * XXX: Needs to ensure that existing, unsupported flags in the
 *      filename are left unchanged (which also needs a test in the
 *      test suite).
 */
notmuch_status_t
notmuch_message_tags_to_maildir_flags (notmuch_message_t *message)
{
    char flags[ARRAY_SIZE(flag2tag)+1];
    const char *filename, *p;
    char *filename_new, *subdir = NULL;
    int ret;

    maildir_get_new_flags (message, flags);

    filename = notmuch_message_get_filename (message);
    /* TODO: Iterate over all file names. */
    p = strstr(filename, ":2,");
    if ((p && strcmp (p+3, flags) == 0) ||
	(!p && flags[0] == '\0')) {
	// Return if flags are not to be changed - this suppresses
	// moving the message from new/ to cur/ during initial
	// tagging.
	return NOTMUCH_STATUS_SUCCESS;
    }
    if (!p)
	p = filename + strlen(filename);

    filename_new = (char*)talloc_size(message, (p-filename) + 3 + sizeof(flags));
    if (unlikely (filename_new == NULL))
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    memcpy(filename_new, filename, p-filename);
    filename_new[p-filename] = '\0';

    /* If message is in new/ move it under cur/. */
    subdir = maildir_get_subdir (filename_new);
    if (subdir && memcmp (subdir, "new/", 4) == 0)
	memcpy (subdir, "cur/", 4);

    strcpy (filename_new+(p-filename), ":2,");
    strcpy (filename_new+(p-filename)+3, flags);

    if (strcmp (filename, filename_new) != 0) {
	notmuch_status_t status;

	ret = rename (filename, filename_new);
	if (ret == -1) {
	    perror (talloc_asprintf (message, "rename of %s to %s failed",
				     filename, filename_new));
	    exit (1);
	}
	status = _notmuch_message_rename (message, filename_new);

	_notmuch_message_sync (message);

	return status;
    }
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
