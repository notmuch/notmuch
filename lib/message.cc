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

struct visible _notmuch_message {
    notmuch_database_t *notmuch;
    Xapian::docid doc_id;
    int frozen;
    char *message_id;
    char *thread_id;
    char *in_reply_to;
    notmuch_string_list_t *tag_list;
    notmuch_string_list_t *filename_term_list;
    notmuch_string_list_t *filename_list;
    char *author;
    notmuch_message_file_t *message_file;
    notmuch_message_list_t *replies;
    unsigned long flags;
    /* For flags that are initialized on-demand, lazy_flags indicates
     * if each flag has been initialized. */
    unsigned long lazy_flags;

    /* Message document modified since last sync */
    notmuch_bool_t modified;

    Xapian::Document doc;
    Xapian::termcount termpos;
};

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

struct maildir_flag_tag {
    char flag;
    const char *tag;
    notmuch_bool_t inverse;
};

/* ASCII ordered table of Maildir flags and associated tags */
static struct maildir_flag_tag flag2tag[] = {
    { 'D', "draft",   FALSE},
    { 'F', "flagged", FALSE},
    { 'P', "passed",  FALSE},
    { 'R', "replied", FALSE},
    { 'S', "unread",  TRUE }
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
    message->lazy_flags = 0;

    /* Each of these will be lazily created as needed. */
    message->message_id = NULL;
    message->thread_id = NULL;
    message->in_reply_to = NULL;
    message->tag_list = NULL;
    message->filename_term_list = NULL;
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
    message->termpos = 0;

    return message;
}

/* Create a new notmuch_message_t object for an existing document in
 * the database.
 *
 * Here, 'talloc owner' is an optional talloc context to which the new
 * message will belong. This allows for the caller to not bother
 * calling notmuch_message_destroy on the message, and know that all
 * memory will be reclaimed when 'talloc_owner' is freed. The caller
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
 *     document. The message may be a ghost message.
 *
 *   NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND:
 *
 *     No document with 'message_id' exists in the database. The
 *     returned message contains a newly created document (not yet
 *     added to the database) and a document ID that is known not to
 *     exist in the database.  This message is "blank"; that is, it
 *     contains only a message ID and no other metadata. The caller
 *     can modify the message, and a call to _notmuch_message_sync
 *     will add the document to the database.
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
    unsigned int doc_id;
    char *term;

    *status_ret = (notmuch_private_status_t) notmuch_database_find_message (notmuch,
									    message_id,
									    &message);
    if (message)
	return talloc_steal (notmuch, message);
    else if (*status_ret)
	return NULL;

    /* If the message ID is too long, substitute its sha1 instead. */
    if (strlen (message_id) > NOTMUCH_MESSAGE_ID_MAX)
	message_id = _notmuch_message_id_compressed (message, message_id);

    term = talloc_asprintf (NULL, "%s%s",
			    _find_prefix ("id"), message_id);
    if (term == NULL) {
	*status_ret = NOTMUCH_PRIVATE_STATUS_OUT_OF_MEMORY;
	return NULL;
    }

    if (notmuch->mode == NOTMUCH_DATABASE_MODE_READ_ONLY)
	INTERNAL_ERROR ("Failure to ensure database is writable.");

    try {
	doc.add_term (term, 0);
	talloc_free (term);

	doc.add_value (NOTMUCH_VALUE_MESSAGE_ID, message_id);

	doc_id = _notmuch_database_generate_doc_id (notmuch);
    } catch (const Xapian::Error &error) {
	_notmuch_database_log(_notmuch_message_database (message), "A Xapian exception occurred creating message: %s\n",
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

static char *
_notmuch_message_get_term (notmuch_message_t *message,
			   Xapian::TermIterator &i, Xapian::TermIterator &end,
			   const char *prefix)
{
    int prefix_len = strlen (prefix);
    char *value;

    i.skip_to (prefix);

    if (i == end)
	return NULL;

    const std::string &term = *i;
    if (strncmp (term.c_str(), prefix, prefix_len))
	return NULL;

    value = talloc_strdup (message, term.c_str() + prefix_len);

#if DEBUG_DATABASE_SANITY
    i++;

    if (i != end && strncmp ((*i).c_str (), prefix, prefix_len) == 0) {
	INTERNAL_ERROR ("Mail (doc_id: %d) has duplicate %s terms: %s and %s\n",
			message->doc_id, prefix, value,
			(*i).c_str () + prefix_len);
    }
#endif

    return value;
}

void
_notmuch_message_ensure_metadata (notmuch_message_t *message)
{
    Xapian::TermIterator i, end;
    const char *thread_prefix = _find_prefix ("thread"),
	*tag_prefix = _find_prefix ("tag"),
	*id_prefix = _find_prefix ("id"),
	*type_prefix = _find_prefix ("type"),
	*filename_prefix = _find_prefix ("file-direntry"),
	*replyto_prefix = _find_prefix ("replyto");

    /* We do this all in a single pass because Xapian decompresses the
     * term list every time you iterate over it.  Thus, while this is
     * slightly more costly than looking up individual fields if only
     * one field of the message object is actually used, it's a huge
     * win as more fields are used. */

    i = message->doc.termlist_begin ();
    end = message->doc.termlist_end ();

    /* Get thread */
    if (!message->thread_id)
	message->thread_id =
	    _notmuch_message_get_term (message, i, end, thread_prefix);

    /* Get tags */
    assert (strcmp (thread_prefix, tag_prefix) < 0);
    if (!message->tag_list) {
	message->tag_list =
	    _notmuch_database_get_terms_with_prefix (message, i, end,
						     tag_prefix);
	_notmuch_string_list_sort (message->tag_list);
    }

    /* Get id */
    assert (strcmp (tag_prefix, id_prefix) < 0);
    if (!message->message_id)
	message->message_id =
	    _notmuch_message_get_term (message, i, end, id_prefix);

    /* Get document type */
    assert (strcmp (id_prefix, type_prefix) < 0);
    if (! NOTMUCH_TEST_BIT (message->lazy_flags, NOTMUCH_MESSAGE_FLAG_GHOST)) {
	i.skip_to (type_prefix);
	/* "T" is the prefix "type" fields.  See
	 * BOOLEAN_PREFIX_INTERNAL. */
	if (*i == "Tmail")
	    NOTMUCH_CLEAR_BIT (&message->flags, NOTMUCH_MESSAGE_FLAG_GHOST);
	else if (*i == "Tghost")
	    NOTMUCH_SET_BIT (&message->flags, NOTMUCH_MESSAGE_FLAG_GHOST);
	else
	    INTERNAL_ERROR ("Message without type term");
	NOTMUCH_SET_BIT (&message->lazy_flags, NOTMUCH_MESSAGE_FLAG_GHOST);
    }

    /* Get filename list.  Here we get only the terms.  We lazily
     * expand them to full file names when needed in
     * _notmuch_message_ensure_filename_list. */
    assert (strcmp (type_prefix, filename_prefix) < 0);
    if (!message->filename_term_list && !message->filename_list)
	message->filename_term_list =
	    _notmuch_database_get_terms_with_prefix (message, i, end,
						     filename_prefix);

    /* Get reply to */
    assert (strcmp (filename_prefix, replyto_prefix) < 0);
    if (!message->in_reply_to)
	message->in_reply_to =
	    _notmuch_message_get_term (message, i, end, replyto_prefix);
    /* It's perfectly valid for a message to have no In-Reply-To
     * header. For these cases, we return an empty string. */
    if (!message->in_reply_to)
	message->in_reply_to = talloc_strdup (message, "");
}

static void
_notmuch_message_invalidate_metadata (notmuch_message_t *message,
				      const char *prefix_name)
{
    if (strcmp ("thread", prefix_name) == 0) {
	talloc_free (message->thread_id);
	message->thread_id = NULL;
    }

    if (strcmp ("tag", prefix_name) == 0) {
	talloc_unlink (message, message->tag_list);
	message->tag_list = NULL;
    }

    if (strcmp ("type", prefix_name) == 0) {
	NOTMUCH_CLEAR_BIT (&message->flags, NOTMUCH_MESSAGE_FLAG_GHOST);
	NOTMUCH_CLEAR_BIT (&message->lazy_flags, NOTMUCH_MESSAGE_FLAG_GHOST);
    }

    if (strcmp ("file-direntry", prefix_name) == 0) {
	talloc_free (message->filename_term_list);
	talloc_free (message->filename_list);
	message->filename_term_list = message->filename_list = NULL;
    }

    if (strcmp ("replyto", prefix_name) == 0) {
	talloc_free (message->in_reply_to);
	message->in_reply_to = NULL;
    }
}

unsigned int
_notmuch_message_get_doc_id (notmuch_message_t *message)
{
    return message->doc_id;
}

const char *
notmuch_message_get_message_id (notmuch_message_t *message)
{
    if (!message->message_id)
	_notmuch_message_ensure_metadata (message);
    if (!message->message_id)
	INTERNAL_ERROR ("Message with document ID of %u has no message ID.\n",
			message->doc_id);
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

    message->message_file = _notmuch_message_file_open_ctx (
	_notmuch_message_database (message), message, filename);
}

const char *
notmuch_message_get_header (notmuch_message_t *message, const char *header)
{
    Xapian::valueno slot = Xapian::BAD_VALUENO;

    /* Fetch header from the appropriate xapian value field if
     * available */
    if (strcasecmp (header, "from") == 0)
	slot = NOTMUCH_VALUE_FROM;
    else if (strcasecmp (header, "subject") == 0)
	slot = NOTMUCH_VALUE_SUBJECT;
    else if (strcasecmp (header, "message-id") == 0)
	slot = NOTMUCH_VALUE_MESSAGE_ID;

    if (slot != Xapian::BAD_VALUENO) {
	try {
	    std::string value = message->doc.get_value (slot);

	    /* If we have NOTMUCH_FEATURE_FROM_SUBJECT_ID_VALUES, then
	     * empty values indicate empty headers.  If we don't, then
	     * it could just mean we didn't record the header. */
	    if ((message->notmuch->features &
		 NOTMUCH_FEATURE_FROM_SUBJECT_ID_VALUES) ||
		! value.empty())
		return talloc_strdup (message, value.c_str ());

	} catch (Xapian::Error &error) {
	    _notmuch_database_log(_notmuch_message_database (message), "A Xapian exception occurred when reading header: %s\n",
		     error.get_msg().c_str());
	    message->notmuch->exception_reported = TRUE;
	    return NULL;
	}
    }

    /* Otherwise fall back to parsing the file */
    _notmuch_message_ensure_message_file (message);
    if (message->message_file == NULL)
	return NULL;

    return _notmuch_message_file_get_header (message->message_file, header);
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
    if (!message->in_reply_to)
	_notmuch_message_ensure_metadata (message);
    return message->in_reply_to;
}

const char *
notmuch_message_get_thread_id (notmuch_message_t *message)
{
    if (!message->thread_id)
	_notmuch_message_ensure_metadata (message);
    if (!message->thread_id)
	INTERNAL_ERROR ("Message with document ID of %u has no thread ID.\n",
			message->doc_id);
    return message->thread_id;
}

void
_notmuch_message_add_reply (notmuch_message_t *message,
			    notmuch_message_t *reply)
{
    _notmuch_message_list_add_message (message->replies, reply);
}

notmuch_messages_t *
notmuch_message_get_replies (notmuch_message_t *message)
{
    return _notmuch_messages_create (message->replies);
}

static void
_notmuch_message_remove_terms (notmuch_message_t *message, const char *prefix)
{
    Xapian::TermIterator i;
    size_t prefix_len = strlen (prefix);

    while (1) {
	i = message->doc.termlist_begin ();
	i.skip_to (prefix);

	/* Terminate loop when no terms remain with desired prefix. */
	if (i == message->doc.termlist_end () ||
	    strncmp ((*i).c_str (), prefix, prefix_len))
	    break;

	try {
	    message->doc.remove_term ((*i));
	    message->modified = TRUE;
	} catch (const Xapian::InvalidArgumentError) {
	    /* Ignore failure to remove non-existent term. */
	}
    }
}

/* Return true if p points at "new" or "cur". */
static bool is_maildir (const char *p)
{
    return strcmp (p, "cur") == 0 || strcmp (p, "new") == 0;
}

/* Add "folder:" term for directory. */
static notmuch_status_t
_notmuch_message_add_folder_terms (notmuch_message_t *message,
				   const char *directory)
{
    char *folder, *last;

    folder = talloc_strdup (NULL, directory);
    if (! folder)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    /*
     * If the message file is in a leaf directory named "new" or
     * "cur", presume maildir and index the parent directory. Thus a
     * "folder:" prefix search matches messages in the specified
     * maildir folder, i.e. in the specified directory and its "new"
     * and "cur" subdirectories.
     *
     * Note that this means the "folder:" prefix can't be used for
     * distinguishing between message files in "new" or "cur". The
     * "path:" prefix needs to be used for that.
     *
     * Note the deliberate difference to _filename_is_in_maildir(). We
     * don't want to index different things depending on the existence
     * or non-existence of all maildir sibling directories "new",
     * "cur", and "tmp". Doing so would be surprising, and difficult
     * for the user to fix in case all subdirectories were not in
     * place during indexing.
     */
    last = strrchr (folder, '/');
    if (last) {
	if (is_maildir (last + 1))
	    *last = '\0';
    } else if (is_maildir (folder)) {
	*folder = '\0';
    }

    _notmuch_message_add_term (message, "folder", folder);

    talloc_free (folder);

    return NOTMUCH_STATUS_SUCCESS;
}

#define RECURSIVE_SUFFIX "/**"

/* Add "path:" terms for directory. */
static notmuch_status_t
_notmuch_message_add_path_terms (notmuch_message_t *message,
				 const char *directory)
{
    /* Add exact "path:" term. */
    _notmuch_message_add_term (message, "path", directory);

    if (strlen (directory)) {
	char *path, *p;

	path = talloc_asprintf (NULL, "%s%s", directory, RECURSIVE_SUFFIX);
	if (! path)
	    return NOTMUCH_STATUS_OUT_OF_MEMORY;

	/* Add recursive "path:" terms for directory and all parents. */
	for (p = path + strlen (path) - 1; p > path; p--) {
	    if (*p == '/') {
		strcpy (p, RECURSIVE_SUFFIX);
		_notmuch_message_add_term (message, "path", path);
	    }
	}

	talloc_free (path);
    }

    /* Recursive all-matching path:** for consistency. */
    _notmuch_message_add_term (message, "path", "**");

    return NOTMUCH_STATUS_SUCCESS;
}

/* Add directory based terms for all filenames of the message. */
static notmuch_status_t
_notmuch_message_add_directory_terms (void *ctx, notmuch_message_t *message)
{
    const char *direntry_prefix = _find_prefix ("file-direntry");
    int direntry_prefix_len = strlen (direntry_prefix);
    Xapian::TermIterator i = message->doc.termlist_begin ();
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    for (i.skip_to (direntry_prefix); i != message->doc.termlist_end (); i++) {
	unsigned int directory_id;
	const char *direntry, *directory;
	char *colon;
	const std::string &term = *i;

	/* Terminate loop at first term without desired prefix. */
	if (strncmp (term.c_str (), direntry_prefix, direntry_prefix_len))
	    break;

	/* Indicate that there are filenames remaining. */
	status = NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID;

	direntry = term.c_str ();
	direntry += direntry_prefix_len;

	directory_id = strtol (direntry, &colon, 10);

	if (colon == NULL || *colon != ':')
	    INTERNAL_ERROR ("malformed direntry");

	directory = _notmuch_database_get_directory_path (ctx,
							  message->notmuch,
							  directory_id);

	_notmuch_message_add_folder_terms (message, directory);
	_notmuch_message_add_path_terms (message, directory);
    }

    return status;
}

/* Add an additional 'filename' for 'message'.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_sync. */
notmuch_status_t
_notmuch_message_add_filename (notmuch_message_t *message,
			       const char *filename)
{
    const char *relative, *directory;
    notmuch_status_t status;
    void *local = talloc_new (message);
    char *direntry;

    if (filename == NULL)
	INTERNAL_ERROR ("Message filename cannot be NULL.");

    if (! (message->notmuch->features & NOTMUCH_FEATURE_FILE_TERMS) ||
	! (message->notmuch->features & NOTMUCH_FEATURE_BOOL_FOLDER))
	return NOTMUCH_STATUS_UPGRADE_REQUIRED;

    relative = _notmuch_database_relative_path (message->notmuch, filename);

    status = _notmuch_database_split_path (local, relative, &directory, NULL);
    if (status)
	return status;

    status = _notmuch_database_filename_to_direntry (
	local, message->notmuch, filename, NOTMUCH_FIND_CREATE, &direntry);
    if (status)
	return status;

    /* New file-direntry allows navigating to this message with
     * notmuch_directory_get_child_files() . */
    _notmuch_message_add_term (message, "file-direntry", direntry);

    _notmuch_message_add_folder_terms (message, directory);
    _notmuch_message_add_path_terms (message, directory);

    talloc_free (local);

    return NOTMUCH_STATUS_SUCCESS;
}

/* Remove a particular 'filename' from 'message'.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_sync.
 *
 * If this message still has other filenames, returns
 * NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID.
 *
 * Note: This function does not remove a document from the database,
 * even if the specified filename is the only filename for this
 * message. For that functionality, see
 * _notmuch_database_remove_message. */
notmuch_status_t
_notmuch_message_remove_filename (notmuch_message_t *message,
				  const char *filename)
{
    void *local = talloc_new (message);
    char *direntry;
    notmuch_private_status_t private_status;
    notmuch_status_t status;

    if (! (message->notmuch->features & NOTMUCH_FEATURE_FILE_TERMS) ||
	! (message->notmuch->features & NOTMUCH_FEATURE_BOOL_FOLDER))
	return NOTMUCH_STATUS_UPGRADE_REQUIRED;

    status = _notmuch_database_filename_to_direntry (
	local, message->notmuch, filename, NOTMUCH_FIND_LOOKUP, &direntry);
    if (status || !direntry)
	return status;

    /* Unlink this file from its parent directory. */
    private_status = _notmuch_message_remove_term (message,
						   "file-direntry", direntry);
    status = COERCE_STATUS (private_status,
			    "Unexpected error from _notmuch_message_remove_term");
    if (status)
	return status;

    /* Re-synchronize "folder:" and "path:" terms for this message. */

    /* Remove all "folder:" terms. */
    _notmuch_message_remove_terms (message, _find_prefix ("folder"));

    /* Remove all "path:" terms. */
    _notmuch_message_remove_terms (message, _find_prefix ("path"));

    /* Add back terms for all remaining filenames of the message. */
    status = _notmuch_message_add_directory_terms (local, message);

    talloc_free (local);

    return status;
}

/* Upgrade the "folder:" prefix from V1 to V2. */
#define FOLDER_PREFIX_V1       "XFOLDER"
#define ZFOLDER_PREFIX_V1      "Z" FOLDER_PREFIX_V1
void
_notmuch_message_upgrade_folder (notmuch_message_t *message)
{
    /* Remove all old "folder:" terms. */
    _notmuch_message_remove_terms (message, FOLDER_PREFIX_V1);

    /* Remove all old "folder:" stemmed terms. */
    _notmuch_message_remove_terms (message, ZFOLDER_PREFIX_V1);

    /* Add new boolean "folder:" and "path:" terms. */
    _notmuch_message_add_directory_terms (message, message);
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
    message->modified = TRUE;
}

static void
_notmuch_message_ensure_filename_list (notmuch_message_t *message)
{
    notmuch_string_node_t *node;

    if (message->filename_list)
	return;

    if (!message->filename_term_list)
	_notmuch_message_ensure_metadata (message);

    message->filename_list = _notmuch_string_list_create (message);
    node = message->filename_term_list->head;

    if (!node) {
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

	_notmuch_string_list_append (message->filename_list, data);

	return;
    }

    for (; node; node = node->next) {
	void *local = talloc_new (message);
	const char *db_path, *directory, *basename, *filename;
	char *colon, *direntry = NULL;
	unsigned int directory_id;

	direntry = node->string;

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

	_notmuch_string_list_append (message->filename_list, filename);

	talloc_free (local);
    }

    talloc_free (message->filename_term_list);
    message->filename_term_list = NULL;
}

const char *
notmuch_message_get_filename (notmuch_message_t *message)
{
    _notmuch_message_ensure_filename_list (message);

    if (message->filename_list == NULL)
	return NULL;

    if (message->filename_list->head == NULL ||
	message->filename_list->head->string == NULL)
    {
	INTERNAL_ERROR ("message with no filename");
    }

    return message->filename_list->head->string;
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
    if (flag == NOTMUCH_MESSAGE_FLAG_GHOST &&
	! NOTMUCH_TEST_BIT (message->lazy_flags, flag))
	_notmuch_message_ensure_metadata (message);

    return NOTMUCH_TEST_BIT (message->flags, flag);
}

void
notmuch_message_set_flag (notmuch_message_t *message,
			  notmuch_message_flag_t flag, notmuch_bool_t enable)
{
    if (enable)
	NOTMUCH_SET_BIT (&message->flags, flag);
    else
	NOTMUCH_CLEAR_BIT (&message->flags, flag);
    NOTMUCH_SET_BIT (&message->lazy_flags, flag);
}

time_t
notmuch_message_get_date (notmuch_message_t *message)
{
    std::string value;

    try {
	value = message->doc.get_value (NOTMUCH_VALUE_TIMESTAMP);
    } catch (Xapian::Error &error) {
	_notmuch_database_log(_notmuch_message_database (message), "A Xapian exception occurred when reading date: %s\n",
		 error.get_msg().c_str());
	message->notmuch->exception_reported = TRUE;
	return 0;
    }

    if (value.empty ())
	/* sortable_unserialise is undefined on empty string */
	return 0;
    return Xapian::sortable_unserialise (value);
}

notmuch_tags_t *
notmuch_message_get_tags (notmuch_message_t *message)
{
    notmuch_tags_t *tags;

    if (!message->tag_list)
	_notmuch_message_ensure_metadata (message);

    tags = _notmuch_tags_create (message, message->tag_list);
    /* _notmuch_tags_create steals the reference to the tag_list, but
     * in this case it's still used by the message, so we add an
     * *additional* talloc reference to the list.  As a result, it's
     * possible to modify the message tags (which talloc_unlink's the
     * current list from the message) while still iterating because
     * the iterator will keep the current list alive. */
    if (!talloc_reference (message, message->tag_list))
	return NULL;

    return tags;
}

const char *
_notmuch_message_get_author (notmuch_message_t *message)
{
    return message->author;
}

void
_notmuch_message_set_author (notmuch_message_t *message,
			    const char *author)
{
    if (message->author)
	talloc_free(message->author);
    message->author = talloc_strdup(message, author);
    return;
}

void
_notmuch_message_set_header_values (notmuch_message_t *message,
				    const char *date,
				    const char *from,
				    const char *subject)
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
    message->doc.add_value (NOTMUCH_VALUE_FROM, from);
    message->doc.add_value (NOTMUCH_VALUE_SUBJECT, subject);
    message->modified = TRUE;
}

/* Upgrade a message to support NOTMUCH_FEATURE_LAST_MOD.  The caller
 * must call _notmuch_message_sync. */
void
_notmuch_message_upgrade_last_mod (notmuch_message_t *message)
{
    /* _notmuch_message_sync will update the last modification
     * revision; we just have to ask it to. */
    message->modified = TRUE;
}

/* Synchronize changes made to message->doc out into the database. */
void
_notmuch_message_sync (notmuch_message_t *message)
{
    Xapian::WritableDatabase *db;

    if (message->notmuch->mode == NOTMUCH_DATABASE_MODE_READ_ONLY)
	return;

    if (! message->modified)
	return;

    /* Update the last modification of this message. */
    if (message->notmuch->features & NOTMUCH_FEATURE_LAST_MOD)
	/* sortable_serialise gives a reasonably compact encoding,
	 * which directly translates to reduced IO when scanning the
	 * value stream.  Since it's built for doubles, we only get 53
	 * effective bits, but that's still enough for the database to
	 * last a few centuries at 1 million revisions per second. */
	message->doc.add_value (NOTMUCH_VALUE_LAST_MOD,
				Xapian::sortable_serialise (
				    _notmuch_database_new_revision (
					message->notmuch)));

    db = static_cast <Xapian::WritableDatabase *> (message->notmuch->xapian_db);
    db->replace_document (message->doc_id, message->doc);
    message->modified = FALSE;
}

/* Delete a message document from the database. */
notmuch_status_t
_notmuch_message_delete (notmuch_message_t *message)
{
    notmuch_status_t status;
    Xapian::WritableDatabase *db;

    status = _notmuch_database_ensure_writable (message->notmuch);
    if (status)
	return status;

    db = static_cast <Xapian::WritableDatabase *> (message->notmuch->xapian_db);
    db->delete_document (message->doc_id);
    return NOTMUCH_STATUS_SUCCESS;
}

/* Transform a blank message into a ghost message.  The caller must
 * _notmuch_message_sync the message. */
notmuch_private_status_t
_notmuch_message_initialize_ghost (notmuch_message_t *message,
				   const char *thread_id)
{
    notmuch_private_status_t status;

    status = _notmuch_message_add_term (message, "type", "ghost");
    if (status)
	return status;
    status = _notmuch_message_add_term (message, "thread", thread_id);
    if (status)
	return status;

    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

/* Ensure that 'message' is not holding any file object open. Future
 * calls to various functions will still automatically open the
 * message file as needed.
 */
void
_notmuch_message_close (notmuch_message_t *message)
{
    if (message->message_file) {
	_notmuch_message_file_close (message->message_file);
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
    message->modified = TRUE;

    talloc_free (term);

    _notmuch_message_invalidate_metadata (message, prefix_name);

    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

/* Parse 'text' and add a term to 'message' for each parsed word. Each
 * term will be added both prefixed (if prefix_name is not NULL) and
 * also non-prefixed). */
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

	term_gen->set_termpos (message->termpos);
	term_gen->index_text (text, 1, prefix);
	/* Create a gap between this an the next terms so they don't
	 * appear to be a phrase. */
	message->termpos = term_gen->get_termpos () + 100;

	_notmuch_message_invalidate_metadata (message, prefix_name);
    }

    term_gen->set_termpos (message->termpos);
    term_gen->index_text (text);
    /* Create a term gap, as above. */
    message->termpos = term_gen->get_termpos () + 100;

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
	message->modified = TRUE;
    } catch (const Xapian::InvalidArgumentError) {
	/* We'll let the philosopher's try to wrestle with the
	 * question of whether failing to remove that which was not
	 * there in the first place is failure. For us, we'll silently
	 * consider it all good. */
    }

    talloc_free (term);

    _notmuch_message_invalidate_metadata (message, prefix_name);

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

/* Is the given filename within a maildir directory?
 *
 * Specifically, is the final directory component of 'filename' either
 * "cur" or "new". If so, return a pointer to that final directory
 * component within 'filename'. If not, return NULL.
 *
 * A non-NULL return value is guaranteed to be a valid string pointer
 * pointing to the characters "new/" or "cur/", (but not
 * NUL-terminated).
 */
static const char *
_filename_is_in_maildir (const char *filename)
{
    const char *slash, *dir = NULL;

    /* Find the last '/' separating directory from filename. */
    slash = strrchr (filename, '/');
    if (slash == NULL)
	return NULL;

    /* Jump back 4 characters to where the previous '/' will be if the
     * directory is named "cur" or "new". */
    if (slash - filename < 4)
	return NULL;

    slash -= 4;

    if (*slash != '/')
	return NULL;

    dir = slash + 1;

    if (STRNCMP_LITERAL (dir, "cur/") == 0 ||
	STRNCMP_LITERAL (dir, "new/") == 0)
    {
	return dir;
    }

    return NULL;
}

notmuch_status_t
notmuch_message_maildir_flags_to_tags (notmuch_message_t *message)
{
    const char *flags;
    notmuch_status_t status;
    notmuch_filenames_t *filenames;
    const char *filename, *dir;
    char *combined_flags = talloc_strdup (message, "");
    unsigned i;
    int seen_maildir_info = 0;

    for (filenames = notmuch_message_get_filenames (message);
	 notmuch_filenames_valid (filenames);
	 notmuch_filenames_move_to_next (filenames))
    {
	filename = notmuch_filenames_get (filenames);
	dir = _filename_is_in_maildir (filename);

	if (! dir)
	    continue;

	flags = strstr (filename, ":2,");
	if (flags) {
	    seen_maildir_info = 1;
	    flags += 3;
	    combined_flags = talloc_strdup_append (combined_flags, flags);
	} else if (STRNCMP_LITERAL (dir, "new/") == 0) {
	    /* Messages are delivered to new/ with no "info" part, but
	     * they effectively have default maildir flags.  According
	     * to the spec, we should ignore the info part for
	     * messages in new/, but some MUAs (mutt) can set maildir
	     * flags on messages in new/, so we're liberal in what we
	     * accept. */
	    seen_maildir_info = 1;
	}
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

/* From the set of tags on 'message' and the flag2tag table, compute a
 * set of maildir-flag actions to be taken, (flags that should be
 * either set or cleared).
 *
 * The result is returned as two talloced strings: to_set, and to_clear
 */
static void
_get_maildir_flag_actions (notmuch_message_t *message,
			   char **to_set_ret,
			   char **to_clear_ret)
{
    char *to_set, *to_clear;
    notmuch_tags_t *tags;
    const char *tag;
    unsigned i;

    to_set = talloc_strdup (message, "");
    to_clear = talloc_strdup (message, "");

    /* First, find flags for all set tags. */
    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags))
    {
	tag = notmuch_tags_get (tags);

	for (i = 0; i < ARRAY_SIZE (flag2tag); i++) {
	    if (strcmp (tag, flag2tag[i].tag) == 0) {
		if (flag2tag[i].inverse)
		    to_clear = talloc_asprintf_append (to_clear,
						       "%c",
						       flag2tag[i].flag);
		else
		    to_set = talloc_asprintf_append (to_set,
						     "%c",
						     flag2tag[i].flag);
	    }
	}
    }

    /* Then, find the flags for all tags not present. */
    for (i = 0; i < ARRAY_SIZE (flag2tag); i++) {
	if (flag2tag[i].inverse) {
	    if (strchr (to_clear, flag2tag[i].flag) == NULL)
		to_set = talloc_asprintf_append (to_set, "%c", flag2tag[i].flag);
	} else {
	    if (strchr (to_set, flag2tag[i].flag) == NULL)
		to_clear = talloc_asprintf_append (to_clear, "%c", flag2tag[i].flag);
	}
    }

    *to_set_ret = to_set;
    *to_clear_ret = to_clear;
}

/* Given 'filename' and a set of maildir flags to set and to clear,
 * compute the new maildir filename.
 *
 * If the existing filename is in the directory "new", the new
 * filename will be in the directory "cur", except for the case when
 * no flags are changed and the existing filename does not contain
 * maildir info (starting with ",2:").
 *
 * After a sequence of ":2," in the filename, any subsequent
 * single-character flags will be added or removed according to the
 * characters in flags_to_set and flags_to_clear. Any existing flags
 * not mentioned in either string will remain. The final list of flags
 * will be in ASCII order.
 *
 * If the original flags seem invalid, (repeated characters or
 * non-ASCII ordering of flags), this function will return NULL
 * (meaning that renaming would not be safe and should not occur).
 */
static char*
_new_maildir_filename (void *ctx,
		       const char *filename,
		       const char *flags_to_set,
		       const char *flags_to_clear)
{
    const char *info, *flags;
    unsigned int flag, last_flag;
    char *filename_new, *dir;
    char flag_map[128];
    int flags_in_map = 0;
    notmuch_bool_t flags_changed = FALSE;
    unsigned int i;
    char *s;

    memset (flag_map, 0, sizeof (flag_map));

    info = strstr (filename, ":2,");

    if (info == NULL) {
	info = filename + strlen(filename);
    } else {
	/* Loop through existing flags in filename. */
	for (flags = info + 3, last_flag = 0;
	     *flags;
	     last_flag = flag, flags++)
	{
	    flag = *flags;

	    /* Original flags not in ASCII order. Abort. */
	    if (flag < last_flag)
		return NULL;

	    /* Non-ASCII flag. Abort. */
	    if (flag > sizeof(flag_map) - 1)
		return NULL;

	    /* Repeated flag value. Abort. */
	    if (flag_map[flag])
		return NULL;

	    flag_map[flag] = 1;
	    flags_in_map++;
	}
    }

    /* Then set and clear our flags from tags. */
    for (flags = flags_to_set; *flags; flags++) {
	flag = *flags;
	if (flag_map[flag] == 0) {
	    flag_map[flag] = 1;
	    flags_in_map++;
	    flags_changed = TRUE;
	}
    }

    for (flags = flags_to_clear; *flags; flags++) {
	flag = *flags;
	if (flag_map[flag]) {
	    flag_map[flag] = 0;
	    flags_in_map--;
	    flags_changed = TRUE;
	}
    }

    /* Messages in new/ without maildir info can be kept in new/ if no
     * flags have changed. */
    dir = (char *) _filename_is_in_maildir (filename);
    if (dir && STRNCMP_LITERAL (dir, "new/") == 0 && !*info && !flags_changed)
	return talloc_strdup (ctx, filename);

    filename_new = (char *) talloc_size (ctx,
					 info - filename +
					 strlen (":2,") + flags_in_map + 1);
    if (unlikely (filename_new == NULL))
	return NULL;

    strncpy (filename_new, filename, info - filename);
    filename_new[info - filename] = '\0';

    strcat (filename_new, ":2,");

    s = filename_new + strlen (filename_new);
    for (i = 0; i < sizeof (flag_map); i++)
    {
	if (flag_map[i]) {
	    *s = i;
	    s++;
	}
    }
    *s = '\0';

    /* If message is in new/ move it under cur/. */
    dir = (char *) _filename_is_in_maildir (filename_new);
    if (dir && STRNCMP_LITERAL (dir, "new/") == 0)
	memcpy (dir, "cur/", 4);

    return filename_new;
}

notmuch_status_t
notmuch_message_tags_to_maildir_flags (notmuch_message_t *message)
{
    notmuch_filenames_t *filenames;
    const char *filename;
    char *filename_new;
    char *to_set, *to_clear;
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    _get_maildir_flag_actions (message, &to_set, &to_clear);

    for (filenames = notmuch_message_get_filenames (message);
	 notmuch_filenames_valid (filenames);
	 notmuch_filenames_move_to_next (filenames))
    {
	filename = notmuch_filenames_get (filenames);

	if (! _filename_is_in_maildir (filename))
	    continue;

	filename_new = _new_maildir_filename (message, filename,
					      to_set, to_clear);
	if (filename_new == NULL)
	    continue;

	if (strcmp (filename, filename_new)) {
	    int err;
	    notmuch_status_t new_status;

	    err = rename (filename, filename_new);
	    if (err)
		continue;

	    new_status = _notmuch_message_remove_filename (message,
							   filename);
	    /* Hold on to only the first error. */
	    if (! status && new_status
		&& new_status != NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) {
		status = new_status;
		continue;
	    }

	    new_status = _notmuch_message_add_filename (message,
							filename_new);
	    /* Hold on to only the first error. */
	    if (! status && new_status) {
		status = new_status;
		continue;
	    }

	    _notmuch_message_sync (message);
	}

	talloc_free (filename_new);
    }

    talloc_free (to_set);
    talloc_free (to_clear);

    return status;
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

    talloc_free (tags);
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

notmuch_database_t *
_notmuch_message_database (notmuch_message_t *message)
{
    return message->notmuch;
}
