/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 * Copyright © 2009 Keith Packard
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
 * Authors: Carl Worth <cworth@cworth.org>
 *          Keith Packard <keithp@keithp.com>
 *          Austin Clements <aclements@csail.mit.edu>
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "notmuch-client.h"

/* Context that gets inherited from the root node. */
typedef struct mime_node_context {
    /* Per-message resources.  These are allocated internally and must
     * be destroyed. */
    GMimeStream *stream;
    GMimeParser *parser;
    GMimeMessage *mime_message;
    _notmuch_message_crypto_t *msg_crypto;

    /* repaired/unmangled parts that will need to be cleaned up */
    GSList *repaired_parts;

    /* Context provided by the caller. */
    _notmuch_crypto_t *crypto;
} mime_node_context_t;

static int
_mime_node_context_free (mime_node_context_t *res)
{
    if (res->mime_message)
	g_object_unref (res->mime_message);

    if (res->parser)
	g_object_unref (res->parser);

    if (res->stream)
	g_object_unref (res->stream);

    if (res->repaired_parts)
	g_slist_free_full (res->repaired_parts, g_object_unref);

    return 0;
}

/* keep track of objects that need to be destroyed when the mime node
 * context goes away. */
static void
_mime_node_context_track_repaired_part (mime_node_context_t *ctx, GMimeObject *part)
{
    if (part)
	ctx->repaired_parts = g_slist_prepend (ctx->repaired_parts, part);
}

const _notmuch_message_crypto_t *
mime_node_get_message_crypto_status (mime_node_t *node)
{
    return node->ctx->msg_crypto;
}

notmuch_status_t
mime_node_open (const void *ctx, notmuch_message_t *message,
		int duplicate,
		_notmuch_crypto_t *crypto, mime_node_t **root_out)
{
    const char *filename = notmuch_message_get_filename (message);
    mime_node_context_t *mctx;
    mime_node_t *root;
    notmuch_status_t status;
    int fd = -1;

    root = talloc_zero (ctx, mime_node_t);
    if (root == NULL) {
	fprintf (stderr, "Out of memory.\n");
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    /* Create the tree-wide context */
    mctx = talloc_zero (root, mime_node_context_t);
    if (mctx == NULL) {
	fprintf (stderr, "Out of memory.\n");
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }
    talloc_set_destructor (mctx, _mime_node_context_free);

    /* Fast path */
    if (duplicate <= 0)
	fd = open (filename, O_RDONLY);
    if (fd == -1) {
	/* Slow path - Either we are trying to open a specific file, or
	 * for some reason the first file in the list is
	 * not available anymore. The latter is clearly a problem in the
	 * database, but we are not going to let this problem be a
	 * show stopper */
	notmuch_filenames_t *filenames;
	int i = 1;

	for (filenames = notmuch_message_get_filenames (message);
	     notmuch_filenames_valid (filenames);
	     notmuch_filenames_move_to_next (filenames), i++) {
	    if (i >= duplicate) {
		filename = notmuch_filenames_get (filenames);
		fd = open (filename, O_RDONLY);
		if (fd != -1) {
		    break;
		} else {
		    if (duplicate > 0) {
			fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
			status = NOTMUCH_STATUS_FILE_ERROR;
			goto DONE;
		    }
		}
	    }
	}

	talloc_free (filenames);
	if (fd == -1) {
	    /* Give up */
	    fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
	    status = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}
    }

    mctx->stream = g_mime_stream_gzfile_new (fd);
    if (! mctx->stream) {
	fprintf (stderr, "Out of memory.\n");
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    mctx->parser = g_mime_parser_new_with_stream (mctx->stream);
    if (! mctx->parser) {
	fprintf (stderr, "Out of memory.\n");
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    mctx->mime_message = g_mime_parser_construct_message (mctx->parser, NULL);
    if (! mctx->mime_message) {
	fprintf (stderr, "Failed to parse %s\n", filename);
	status = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    mctx->msg_crypto = _notmuch_message_crypto_new (mctx);

    mctx->crypto = crypto;

    /* Create the root node */
    root->part = GMIME_OBJECT (mctx->mime_message);
    root->envelope_file = message;
    root->nchildren = 1;
    root->ctx = mctx;

    root->parent = NULL;
    root->part_num = 0;
    root->next_child = 0;
    root->next_part_num = 1;

    *root_out = root;
    return NOTMUCH_STATUS_SUCCESS;

  DONE:
    talloc_free (root);
    return status;
}

/* Signature list destructor */
static int
_signature_list_free (GMimeSignatureList **proxy)
{
    g_object_unref (*proxy);
    return 0;
}

/* Set up signature list destructor */
static void
set_signature_list_destructor (mime_node_t *node)
{
    GMimeSignatureList **proxy = talloc (node, GMimeSignatureList *);

    if (proxy) {
	*proxy = node->sig_list;
	talloc_set_destructor (proxy, _signature_list_free);
    }
}

/* Unwrapped MIME part destructor */
static int
_unwrapped_child_free (GMimeObject **proxy)
{
    g_object_unref (*proxy);
    return 0;
}

/* Set up unwrapped MIME part destructor */
static void
set_unwrapped_child_destructor (mime_node_t *node)
{
    GMimeObject **proxy = talloc (node, GMimeObject *);

    if (proxy) {
	*proxy = node->unwrapped_child;
	talloc_set_destructor (proxy, _unwrapped_child_free);
    }
}

/* Verify a signed mime node */
static void
node_verify (mime_node_t *node, GMimeObject *part)
{
    GError *err = NULL;
    notmuch_status_t status;

    node->verify_attempted = true;
    if (GMIME_IS_APPLICATION_PKCS7_MIME (part))
	node->sig_list = g_mime_application_pkcs7_mime_verify (
	    GMIME_APPLICATION_PKCS7_MIME (part), GMIME_VERIFY_NONE, &node->unwrapped_child, &err);
    else
	node->sig_list = g_mime_multipart_signed_verify (
	    GMIME_MULTIPART_SIGNED (part), GMIME_VERIFY_NONE, &err);

    if (node->unwrapped_child) {
	node->nchildren = 1;
	set_unwrapped_child_destructor (node);
    }

    if (node->sig_list)
	set_signature_list_destructor (node);
    else
	fprintf (stderr, "Failed to verify signed part: %s\n",
		 err ? err->message : "no error explanation given");

    if (err)
	g_error_free (err);

    status = _notmuch_message_crypto_potential_sig_list (node->ctx->msg_crypto, node->sig_list);
    if (status) /* this is a warning, not an error */
	fprintf (stderr, "Warning: failed to note signature status: %s.\n", notmuch_status_to_string (
		     status));
}

/* Decrypt and optionally verify an encrypted mime node */
static void
node_decrypt_and_verify (mime_node_t *node, GMimeObject *part)
{
    GError *err = NULL;
    GMimeDecryptResult *decrypt_result = NULL;
    notmuch_status_t status;
    notmuch_message_t *message = NULL;

    if (! node->unwrapped_child) {
	for (mime_node_t *parent = node; parent; parent = parent->parent)
	    if (parent->envelope_file) {
		message = parent->envelope_file;
		break;
	    }

	node->unwrapped_child = _notmuch_crypto_decrypt (&node->decrypt_attempted,
							 node->ctx->crypto->decrypt,
							 message,
							 part, &decrypt_result, &err);
	if (node->unwrapped_child)
	    set_unwrapped_child_destructor (node);
    }
    if (! node->unwrapped_child) {
	fprintf (stderr, "Failed to decrypt part: %s\n",
		 err ? err->message : "no error explanation given");
	goto DONE;
    }

    node->decrypt_success = true;
    status = _notmuch_message_crypto_successful_decryption (node->ctx->msg_crypto);
    if (status) /* this is a warning, not an error */
	fprintf (stderr, "Warning: failed to note decryption status: %s.\n",
		 notmuch_status_to_string (status));

    if (decrypt_result) {
	/* This may be NULL if the part is not signed. */
	node->sig_list = g_mime_decrypt_result_get_signatures (decrypt_result);
	if (node->sig_list) {
	    node->verify_attempted = true;
	    g_object_ref (node->sig_list);
	    set_signature_list_destructor (node);
	    status = _notmuch_message_crypto_potential_sig_list (node->ctx->msg_crypto,
								 node->sig_list);
	    if (status) /* this is a warning, not an error */
		fprintf (stderr, "Warning: failed to note signature status: %s.\n",
			 notmuch_status_to_string (status));
	}

	if (node->ctx->crypto->decrypt == NOTMUCH_DECRYPT_TRUE && message) {
	    notmuch_database_t *db = notmuch_message_get_database (message);
	    const char *session_key = g_mime_decrypt_result_get_session_key (decrypt_result);
	    if (db && session_key)
		print_status_message ("Failed to stash session key in the database",
				      message,
				      notmuch_message_add_property (message, "session-key",
								    session_key));
	}
	g_object_unref (decrypt_result);
    }

  DONE:
    if (err)
	g_error_free (err);
}

static bool
_mime_node_set_up_part (mime_node_t *node, GMimeObject *part, int numchild);

static mime_node_t *
_mime_node_create (mime_node_t *parent, GMimeObject *part, int numchild)
{
    mime_node_t *node = talloc_zero (parent, mime_node_t);

    /* Set basic node properties */
    node->ctx = parent->ctx;
    if (! talloc_reference (node, node->ctx)) {
	fprintf (stderr, "Out of memory.\n");
	talloc_free (node);
	return NULL;
    }
    node->parent = parent;
    node->part_num = node->next_part_num = -1;
    node->next_child = 0;

    if (_mime_node_set_up_part (node, part, numchild))
	return node;
    talloc_free (node);
    return NULL;
}

/* associate a MIME part with a node. */
static bool
_mime_node_set_up_part (mime_node_t *node, GMimeObject *part, int numchild)
{
    /* Deal with the different types of parts */
    if (GMIME_IS_PART (part)) {
	node->part = part;
	node->nchildren = 0;
    } else if (GMIME_IS_MULTIPART (part)) {
	GMimeObject *repaired_part = _notmuch_repair_mixed_up_mangled (part);
	if (repaired_part) {
	    /* This was likely "Mixed Up" in transit!  We replace it
	     * with the more likely-to-be-correct variant. */
	    _mime_node_context_track_repaired_part (node->ctx, repaired_part);
	    part = repaired_part;
	}
	node->part = part;
	node->nchildren = g_mime_multipart_get_count (GMIME_MULTIPART (part));
    } else if (GMIME_IS_MESSAGE_PART (part)) {
	/* Promote part to an envelope and open it */
	GMimeMessagePart *message_part = GMIME_MESSAGE_PART (part);
	GMimeMessage *message = g_mime_message_part_get_message (message_part);
	node->envelope_part = message_part;
	node->part = GMIME_OBJECT (message);
	node->nchildren = 1;
    } else {
	fprintf (stderr, "Warning: Unknown mime part type: %s.\n",
		 g_type_name (G_OBJECT_TYPE (part)));
	return false;
    }

    /* Handle PGP/MIME parts (by definition not cryptographic payload parts) */
    if (GMIME_IS_MULTIPART_ENCRYPTED (part) && (node->ctx->crypto->decrypt !=
						NOTMUCH_DECRYPT_FALSE)) {
	if (node->nchildren != 2) {
	    /* this violates RFC 3156 section 4, so we won't bother with it. */
	    fprintf (stderr, "Error: %d part(s) for a multipart/encrypted "
		     "message (must be exactly 2)\n",
		     node->nchildren);
	} else {
	    node_decrypt_and_verify (node, part);
	}
    } else if (GMIME_IS_MULTIPART_SIGNED (part) && node->ctx->crypto->verify) {
	if (node->nchildren != 2) {
	    /* this violates RFC 3156 section 5, so we won't bother with it. */
	    fprintf (stderr, "Error: %d part(s) for a multipart/signed message "
		     "(must be exactly 2)\n",
		     node->nchildren);
	} else {
	    node_verify (node, part);
	}
    } else if (GMIME_IS_APPLICATION_PKCS7_MIME (part) &&
	       GMIME_SECURE_MIME_TYPE_SIGNED_DATA == g_mime_application_pkcs7_mime_get_smime_type (
		   GMIME_APPLICATION_PKCS7_MIME (part))) {
	/* If node->ctx->crypto->verify is false, it would be better
	 * to just unwrap (instead of verifying), but
	 * https://github.com/jstedfast/gmime/issues/67 */
	node_verify (node, part);
    } else if (GMIME_IS_APPLICATION_PKCS7_MIME (part) &&
	       GMIME_SECURE_MIME_TYPE_ENVELOPED_DATA == g_mime_application_pkcs7_mime_get_smime_type (
		   GMIME_APPLICATION_PKCS7_MIME (part)) &&
	       (node->ctx->crypto->decrypt != NOTMUCH_DECRYPT_FALSE)) {
	node_decrypt_and_verify (node, part);
	if (node->unwrapped_child && node->nchildren == 0)
	    node->nchildren = 1;
    } else {
	if (_notmuch_message_crypto_potential_payload (node->ctx->msg_crypto, part, node->parent ?
						       node->parent->part : NULL, numchild) &&
	    node->ctx->msg_crypto->decryption_status == NOTMUCH_MESSAGE_DECRYPTED_FULL) {
	    GMimeObject *clean_payload = _notmuch_repair_crypto_payload_skip_legacy_display (part);
	    if (clean_payload != part) {
		/* only one layer of recursion is possible here
		 * because there can be only a single cryptographic
		 * payload: */
		return _mime_node_set_up_part (node, clean_payload, numchild);
	    }
	}
    }

    return true;
}

mime_node_t *
mime_node_child (mime_node_t *parent, int child)
{
    GMimeObject *sub;
    mime_node_t *node;

    if (! parent || ! parent->part || child < 0 || child >= parent->nchildren)
	return NULL;

    if (GMIME_IS_MULTIPART (parent->part)) {
	if (child == GMIME_MULTIPART_ENCRYPTED_CONTENT && parent->unwrapped_child)
	    sub = parent->unwrapped_child;
	else
	    sub = g_mime_multipart_get_part (
		GMIME_MULTIPART (parent->part), child);
    } else if (GMIME_IS_MESSAGE (parent->part)) {
	sub = g_mime_message_get_mime_part (GMIME_MESSAGE (parent->part));
    } else if (GMIME_IS_APPLICATION_PKCS7_MIME (parent->part) &&
	       parent->unwrapped_child &&
	       child == 0) {
	sub = parent->unwrapped_child;
    } else {
	/* This should have been caught by _mime_node_set_up_part */
	INTERNAL_ERROR ("Unexpected GMimeObject type: %s",
			g_type_name (G_OBJECT_TYPE (parent->part)));
    }
    node = _mime_node_create (parent, sub, child);

    if (child == parent->next_child && parent->next_part_num != -1) {
	/* We're traversing in depth-first order.  Record the child's
	 * depth-first numbering. */
	node->part_num = parent->next_part_num;
	node->next_part_num = node->part_num + 1;

	/* Prepare the parent for its next depth-first child. */
	parent->next_child++;
	parent->next_part_num = -1;

	if (node->nchildren == 0) {
	    /* We've reached a leaf, so find the parent that has more
	     * children and set it up to number its next child. */
	    mime_node_t *iter = node->parent;
	    while (iter && iter->next_child == iter->nchildren)
		iter = iter->parent;
	    if (iter)
		iter->next_part_num = node->part_num + 1;
	}
    }

    return node;
}

static mime_node_t *
_mime_node_seek_dfs_walk (mime_node_t *node, int *n)
{
    int i;

    if (*n == 0)
	return node;

    *n -= 1;
    for (i = 0; i < node->nchildren; i++) {
	mime_node_t *child = mime_node_child (node, i);
	mime_node_t *ret = _mime_node_seek_dfs_walk (child, n);
	if (ret)
	    return ret;

	talloc_free (child);
    }
    return NULL;
}

mime_node_t *
mime_node_seek_dfs (mime_node_t *node, int n)
{
    if (n < 0)
	return NULL;
    return _mime_node_seek_dfs_walk (node, &n);
}
