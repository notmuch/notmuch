#include "database-private.h"

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
    char *ref, *last_ref = NULL;

    if (refs == NULL || *refs == '\0')
	return NULL;

    while (*refs) {
	ref = _notmuch_message_id_parse (ctx, refs, &refs);

	if (ref && strcmp (ref, message_id)) {
	    g_hash_table_add (hash, ref);
	    last_ref = ref;
	}
    }

    /* The return value of this function is used to add a parent
     * reference to the database.  We should avoid making a message
     * its own parent, thus the above check.
     */
    return talloc_strdup (ctx, last_ref);
}

static const char *
_notmuch_database_generate_thread_id (notmuch_database_t *notmuch)
{
    /* 16 bytes (+ terminator) for hexadecimal representation of
     * a 64-bit integer. */
    static char thread_id[17];

    notmuch->last_thread_id++;

    sprintf (thread_id, "%016" PRIx64, notmuch->last_thread_id);

    notmuch->writable_xapian_db->set_metadata ("last_thread_id", thread_id);

    return thread_id;
}

static char *
_get_metadata_thread_id_key (void *ctx, const char *message_id)
{
    if (strlen (message_id) > NOTMUCH_MESSAGE_ID_MAX)
	message_id = _notmuch_message_id_compressed (ctx, message_id);

    return talloc_asprintf (ctx, NOTMUCH_METADATA_THREAD_ID_PREFIX "%s",
			    message_id);
}


static notmuch_status_t
_resolve_message_id_to_thread_id_old (notmuch_database_t *notmuch,
				      void *ctx,
				      const char *message_id,
				      const char **thread_id_ret);


/* Find the thread ID to which the message with 'message_id' belongs.
 *
 * Note: 'thread_id_ret' must not be NULL!
 * On success '*thread_id_ret' is set to a newly talloced string belonging to
 * 'ctx'.
 *
 * Note: If there is no message in the database with the given
 * 'message_id' then a new thread_id will be allocated for this
 * message ID and stored in the database metadata so that the
 * thread ID can be looked up if the message is added to the database
 * later.
 */
static notmuch_status_t
_resolve_message_id_to_thread_id (notmuch_database_t *notmuch,
				  void *ctx,
				  const char *message_id,
				  const char **thread_id_ret)
{
    notmuch_private_status_t status;
    notmuch_message_t *message;

    if (! (notmuch->features & NOTMUCH_FEATURE_GHOSTS))
	return _resolve_message_id_to_thread_id_old (notmuch, ctx, message_id,
						     thread_id_ret);

    /* Look for this message (regular or ghost) */
    message = _notmuch_message_create_for_message_id (
	notmuch, message_id, &status);
    if (status == NOTMUCH_PRIVATE_STATUS_SUCCESS) {
	/* Message exists */
	*thread_id_ret = talloc_steal (
	    ctx, notmuch_message_get_thread_id (message));
    } else if (status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND) {
	/* Message did not exist.  Give it a fresh thread ID and
	 * populate this message as a ghost message. */
	*thread_id_ret = talloc_strdup (
	    ctx, _notmuch_database_generate_thread_id (notmuch));
	if (! *thread_id_ret) {
	    status = NOTMUCH_PRIVATE_STATUS_OUT_OF_MEMORY;
	} else {
	    status = _notmuch_message_initialize_ghost (message, *thread_id_ret);
	    if (status == 0)
		/* Commit the new ghost message */
		_notmuch_message_sync (message);
	}
    } else {
	/* Create failed. Fall through. */
    }

    notmuch_message_destroy (message);

    return COERCE_STATUS (status, "Error creating ghost message");
}

/* Pre-ghost messages _resolve_message_id_to_thread_id */
static notmuch_status_t
_resolve_message_id_to_thread_id_old (notmuch_database_t *notmuch,
				      void *ctx,
				      const char *message_id,
				      const char **thread_id_ret)
{
    notmuch_status_t status;
    notmuch_message_t *message;
    std::string thread_id_string;
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
    db = notmuch->writable_xapian_db;
    metadata_key = _get_metadata_thread_id_key (ctx, message_id);
    thread_id_string = notmuch->xapian_db->get_metadata (metadata_key);

    if (thread_id_string.empty ()) {
	*thread_id_ret = talloc_strdup (ctx,
					_notmuch_database_generate_thread_id (notmuch));
	db->set_metadata (metadata_key, *thread_id_ret);
    } else {
	*thread_id_ret = talloc_strdup (ctx, thread_id_string.c_str ());
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

    _notmuch_database_find_doc_ids (notmuch, "thread", loser_thread_id, &loser, &loser_end);

    for (; loser != loser_end; loser++) {
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

notmuch_status_t
_notmuch_database_link_message_to_parents (notmuch_database_t *notmuch,
					   notmuch_message_t *message,
					   notmuch_message_file_t *message_file,
					   const char **thread_id)
{
    GHashTable *parents = NULL;
    const char *refs, *in_reply_to, *in_reply_to_message_id, *strict_message_id = NULL;
    const char *last_ref_message_id, *this_message_id;
    GList *l, *keys = NULL;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    parents = g_hash_table_new_full (g_str_hash, g_str_equal,
				     _my_talloc_free_for_g_hash, NULL);
    this_message_id = notmuch_message_get_message_id (message);

    refs = _notmuch_message_file_get_header (message_file, "references");
    last_ref_message_id = parse_references (message,
					    this_message_id,
					    parents, refs);

    in_reply_to = _notmuch_message_file_get_header (message_file, "in-reply-to");
    if (in_reply_to)
	strict_message_id = _notmuch_message_id_parse_strict (message,
							      in_reply_to);

    in_reply_to_message_id = parse_references (message,
					       this_message_id,
					       parents, in_reply_to);

    /* For the parent of this message, use
     * 1) the In-Reply-To header, if it looks sane, otherwise
     * 2) the last message ID of the References header, if available.
     * 3) Otherwise, fall back to the first message ID in
     * the In-Reply-To header.
     */

    if (strict_message_id) {
	_notmuch_message_add_term (message, "replyto", strict_message_id);
    } else if (last_ref_message_id) {
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

    _notmuch_database_find_doc_ids (notmuch, "reference", message_id, &child, &children_end);

    for (; child != children_end; child++) {

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

/* Fetch and clear the stored thread_id for message, or NULL if none. */
static char *
_consume_metadata_thread_id (void *ctx, notmuch_database_t *notmuch,
			     notmuch_message_t *message)
{
    const char *message_id;
    std::string stored_id;
    char *metadata_key;

    message_id = notmuch_message_get_message_id (message);
    metadata_key = _get_metadata_thread_id_key (ctx, message_id);

    /* Check if we have already seen related messages to this one.
     * If we have then use the thread_id that we stored at that time.
     */
    stored_id = notmuch->xapian_db->get_metadata (metadata_key);
    if (stored_id.empty ()) {
	return NULL;
    } else {
	/* Clear the metadata for this message ID. We don't need it
	 * anymore. */
	notmuch->writable_xapian_db->set_metadata (metadata_key, "");

	return talloc_strdup (ctx, stored_id.c_str ());
    }
}

/* Given a blank or ghost 'message' and its corresponding
 * 'message_file' link it to existing threads in the database.
 *
 * First, if is_ghost, this retrieves the thread ID already stored in
 * the message (which will be the case if a message was previously
 * added that referenced this one).  If the message is blank
 * (!is_ghost), it doesn't have a thread ID yet (we'll generate one
 * later in this function).  If the database does not support ghost
 * messages, this checks for a thread ID stored in database metadata
 * for this message ID.
 *
 * Second, we look at 'message_file' and its link-relevant headers
 * (References and In-Reply-To) for message IDs.
 *
 * Finally, we look in the database for existing message that
 * reference 'message'.
 *
 * In all cases, we assign to the current message the first thread ID
 * found. We will also merge any existing, distinct threads where this
 * message belongs to both, (which is not uncommon when messages are
 * processed out of order).
 *
 * Finally, if no thread ID has been found through referenced messages, we
 * call _notmuch_message_generate_thread_id to generate a new thread
 * ID. This should only happen for new, top-level messages, (no
 * References or In-Reply-To header in this message, and no previously
 * added message refers to this message).
 */
static notmuch_status_t
_notmuch_database_link_message (notmuch_database_t *notmuch,
				notmuch_message_t *message,
				notmuch_message_file_t *message_file,
				bool is_ghost)
{
    void *local = talloc_new (NULL);
    notmuch_status_t status;
    const char *thread_id = NULL;

    /* Check if the message already had a thread ID */
    if (notmuch->features & NOTMUCH_FEATURE_GHOSTS) {
	if (is_ghost)
	    thread_id = notmuch_message_get_thread_id (message);
    } else {
	thread_id = _consume_metadata_thread_id (local, notmuch, message);
	if (thread_id)
	    _notmuch_message_add_term (message, "thread", thread_id);
    }

    status = _notmuch_database_link_message_to_parents (notmuch, message,
							message_file,
							&thread_id);
    if (status)
	goto DONE;

    if (! (notmuch->features & NOTMUCH_FEATURE_GHOSTS)) {
	/* In general, it shouldn't be necessary to link children,
	 * since the earlier indexing of those children will have
	 * stored a thread ID for the missing parent.  However, prior
	 * to ghost messages, these stored thread IDs were NOT
	 * rewritten during thread merging (and there was no
	 * performant way to do so), so if indexed children were
	 * pulled into a different thread ID by a merge, it was
	 * necessary to pull them *back* into the stored thread ID of
	 * the parent.  With ghost messages, we just rewrite the
	 * stored thread IDs during merging, so this workaround isn't
	 * necessary. */
	status = _notmuch_database_link_message_to_children (notmuch, message,
							     &thread_id);
	if (status)
	    goto DONE;
    }

    /* If not part of any existing thread, generate a new thread ID. */
    if (thread_id == NULL) {
	thread_id = _notmuch_database_generate_thread_id (notmuch);

	_notmuch_message_add_term (message, "thread", thread_id);
    }

  DONE:
    talloc_free (local);

    return status;
}

notmuch_status_t
notmuch_database_index_file (notmuch_database_t *notmuch,
			     const char *filename,
			     notmuch_indexopts_t *indexopts,
			     notmuch_message_t **message_ret)
{
    notmuch_message_file_t *message_file;
    notmuch_message_t *message = NULL;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS, ret2;
    notmuch_private_status_t private_status;
    notmuch_bool_t is_ghost = false, is_new = false;
    notmuch_indexopts_t *def_indexopts = NULL;

    const char *date;
    const char *from, *to, *subject;
    char *message_id = NULL;

    if (message_ret)
	*message_ret = NULL;

    ret = _notmuch_database_ensure_writable (notmuch);
    if (ret)
	return ret;

    message_file = _notmuch_message_file_open (notmuch, filename);
    if (message_file == NULL)
	return NOTMUCH_STATUS_FILE_ERROR;

    /* Adding a message may change many documents.  Do this all
     * atomically. */
    ret = notmuch_database_begin_atomic (notmuch);
    if (ret)
	goto DONE;

    ret = _notmuch_message_file_get_headers (message_file,
					     &from, &subject, &to, &date,
					     &message_id);
    if (ret)
	goto DONE;

    try {
	/* Now that we have a message ID, we get a message object,
	 * (which may or may not reference an existing document in the
	 * database). */

	message = _notmuch_message_create_for_message_id (notmuch,
							  message_id,
							  &private_status);

	talloc_free (message_id);

	/* We cannot call notmuch_message_get_flag for a new message */
	switch (private_status) {
	case NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND:
	    is_ghost = false;
	    is_new = true;
	    break;
	case NOTMUCH_PRIVATE_STATUS_SUCCESS:
	    ret = notmuch_message_get_flag_st (message, NOTMUCH_MESSAGE_FLAG_GHOST, &is_ghost);
	    if (ret)
		goto DONE;
	    is_new = false;
	    break;
	default:
	    ret = COERCE_STATUS (private_status,
				 "Unexpected status value from _notmuch_message_create_for_message_id");
	    goto DONE;
	}

	ret = _notmuch_message_add_filename (message, filename);
	if (ret)
	    goto DONE;

	if (is_new || is_ghost) {
	    _notmuch_message_add_term (message, "type", "mail");
	    if (is_ghost)
		/* Convert ghost message to a regular message */
		_notmuch_message_remove_term (message, "type", "ghost");
	}

	ret = _notmuch_database_link_message (notmuch, message,
					      message_file, is_ghost);
	if (ret)
	    goto DONE;

	if (is_new || is_ghost)
	    _notmuch_message_set_header_values (message, date, from, subject);

	if (! indexopts) {
	    def_indexopts = notmuch_database_get_default_indexopts (notmuch);
	    indexopts = def_indexopts;
	}

	ret = _notmuch_message_index_file (message, indexopts, message_file);
	if (ret)
	    goto DONE;

	if (! is_new && ! is_ghost)
	    ret = NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID;

	_notmuch_message_sync (message);
    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch, "A Xapian exception occurred adding message: %s.\n",
			       error.get_msg ().c_str ());
	notmuch->exception_reported = true;
	ret = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	goto DONE;
    }

  DONE:
    if (def_indexopts)
	notmuch_indexopts_destroy (def_indexopts);

    if (message) {
	if ((ret == NOTMUCH_STATUS_SUCCESS ||
	     ret == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) && message_ret)
	    *message_ret = message;
	else
	    notmuch_message_destroy (message);
    }

    if (message_file)
	_notmuch_message_file_close (message_file);

    ret2 = notmuch_database_end_atomic (notmuch);
    if ((ret == NOTMUCH_STATUS_SUCCESS ||
	 ret == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) &&
	ret2 != NOTMUCH_STATUS_SUCCESS)
	ret = ret2;

    return ret;
}

notmuch_status_t
notmuch_database_add_message (notmuch_database_t *notmuch,
			      const char *filename,
			      notmuch_message_t **message_ret)
{
    return notmuch_database_index_file (notmuch, filename,
					NULL,
					message_ret);

}
