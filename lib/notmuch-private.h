/* notmuch-private.h - Internal interfaces for notmuch.
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

#ifndef NOTMUCH_PRIVATE_H
#define NOTMUCH_PRIVATE_H

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* For getline and asprintf */
#endif
#include <stdbool.h>
#include <stdio.h>

#include "compat.h"

#include "notmuch.h"

NOTMUCH_BEGIN_DECLS

#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#include <assert.h>

#include <talloc.h>

#include "gmime-extra.h"

#include "xutil.h"
#include "error_util.h"
#include "string-util.h"
#include "crypto.h"
#include "repair.h"

#ifdef DEBUG
# define DEBUG_DATABASE_SANITY 1
# define DEBUG_THREADING 1
# define DEBUG_QUERY 1
#endif

#define COMPILE_TIME_ASSERT(pred) ((void) sizeof (char[1 - 2 * ! (pred)]))

#define STRNCMP_LITERAL(var, literal) \
    strncmp ((var), (literal), sizeof (literal) - 1)

/* Robust bit test/set/reset macros */
#define _NOTMUCH_VALID_BIT(bit) \
    ((bit) >= 0 && ((unsigned long) bit) < CHAR_BIT * sizeof (unsigned long long))
#define NOTMUCH_TEST_BIT(val, bit) \
    (_NOTMUCH_VALID_BIT (bit) ? ! ! ((val) & (1ull << (bit))) : 0)
#define NOTMUCH_SET_BIT(valp, bit) \
    (_NOTMUCH_VALID_BIT (bit) ? (*(valp) |= (1ull << (bit))) : *(valp))
#define NOTMUCH_CLEAR_BIT(valp,  bit) \
    (_NOTMUCH_VALID_BIT (bit) ? (*(valp) &= ~(1ull << (bit))) : *(valp))

#define unused(x) x __attribute__ ((unused))

/* Thanks to Andrew Tridgell's (SAMBA's) talloc for this definition of
 * unlikely. The talloc source code comes to us via the GNU LGPL v. 3.
 */
/* these macros gain us a few percent of speed on gcc */
#if (__GNUC__ >= 3)
/* the strange !! is to ensure that __builtin_expect() takes either 0 or 1
 * as its first argument */
#ifndef likely
#define likely(x)   __builtin_expect (! ! (x), 1)
#endif
#ifndef unlikely
#define unlikely(x) __builtin_expect (! ! (x), 0)
#endif
#else
#ifndef likely
#define likely(x) (x)
#endif
#ifndef unlikely
#define unlikely(x) (x)
#endif
#endif

typedef enum {
    NOTMUCH_VALUE_TIMESTAMP = 0,
    NOTMUCH_VALUE_MESSAGE_ID,
    NOTMUCH_VALUE_FROM,
    NOTMUCH_VALUE_SUBJECT,
    NOTMUCH_VALUE_LAST_MOD,
} notmuch_value_t;

/* Xapian (with flint backend) complains if we provide a term longer
 * than this, but I haven't yet found a way to query the limit
 * programmatically. */
#define NOTMUCH_TERM_MAX 245

#define NOTMUCH_METADATA_THREAD_ID_PREFIX "thread_id_"

/* For message IDs we have to be even more restrictive. Beyond fitting
 * into the term limit, we also use message IDs to construct
 * metadata-key values. And the documentation says that these should
 * be restricted to about 200 characters. (The actual limit for the
 * chert backend at least is 252.)
 */
#define NOTMUCH_MESSAGE_ID_MAX (200 - sizeof (NOTMUCH_METADATA_THREAD_ID_PREFIX))

typedef enum _notmuch_private_status {
    /* First, copy all the public status values. */
    NOTMUCH_PRIVATE_STATUS_SUCCESS			= NOTMUCH_STATUS_SUCCESS,
    NOTMUCH_PRIVATE_STATUS_OUT_OF_MEMORY		= NOTMUCH_STATUS_OUT_OF_MEMORY,
    NOTMUCH_PRIVATE_STATUS_READ_ONLY_DATABASE		= NOTMUCH_STATUS_READ_ONLY_DATABASE,
    NOTMUCH_PRIVATE_STATUS_XAPIAN_EXCEPTION		= NOTMUCH_STATUS_XAPIAN_EXCEPTION,
    NOTMUCH_PRIVATE_STATUS_FILE_NOT_EMAIL		= NOTMUCH_STATUS_FILE_NOT_EMAIL,
    NOTMUCH_PRIVATE_STATUS_NULL_POINTER			= NOTMUCH_STATUS_NULL_POINTER,
    NOTMUCH_PRIVATE_STATUS_TAG_TOO_LONG			= NOTMUCH_STATUS_TAG_TOO_LONG,
    NOTMUCH_PRIVATE_STATUS_UNBALANCED_FREEZE_THAW	= NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW,

    /* Then add our own private values. */
    NOTMUCH_PRIVATE_STATUS_TERM_TOO_LONG		= NOTMUCH_STATUS_LAST_STATUS,
    NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND,
    NOTMUCH_PRIVATE_STATUS_BAD_PREFIX,

    NOTMUCH_PRIVATE_STATUS_LAST_STATUS
} notmuch_private_status_t;

/* Coerce a notmuch_private_status_t value to a notmuch_status_t
 * value, generating an internal error if the private value is equal
 * to or greater than NOTMUCH_STATUS_LAST_STATUS. (The idea here is
 * that the caller has previously handled any expected
 * notmuch_private_status_t values.)
 *
 * Note that the function _internal_error does not return. Evaluating
 * to NOTMUCH_STATUS_SUCCESS is done purely to appease the compiler.
 */
#define COERCE_STATUS(private_status, format, ...)                      \
    ((private_status >= (notmuch_private_status_t) NOTMUCH_STATUS_LAST_STATUS) \
     ?                                                                  \
     _internal_error (format " (%s).\n",                                \
		      ##__VA_ARGS__,                                    \
		      __location__),                                    \
     (notmuch_status_t) NOTMUCH_PRIVATE_STATUS_SUCCESS                  \
     :                                                                  \
     (notmuch_status_t) private_status)

/* Flags shared by various lookup functions. */
typedef enum _notmuch_find_flags {
    /* Lookup without creating any documents.  This is the default
     * behavior. */
    NOTMUCH_FIND_LOOKUP = 0,
    /* If set, create the necessary document (or documents) if they
     * are missing.  Requires a read/write database. */
    NOTMUCH_FIND_CREATE = 1 << 0,
} notmuch_find_flags_t;

typedef struct _notmuch_doc_id_set notmuch_doc_id_set_t;

/* database.cc */

/* Lookup a prefix value by name.
 *
 * XXX: This should really be static inside of message.cc, and we can
 * do that once we convert database.cc to use the
 * _notmuch_message_add/remove_term functions. */
const char *
_find_prefix (const char *name);

/* Lookup a prefix value by name, including possibly user defined prefixes
 */
const char *
_notmuch_database_prefix (notmuch_database_t *notmuch, const char *name);

char *
_notmuch_message_id_compressed (void *ctx, const char *message_id);

notmuch_status_t
_notmuch_database_ensure_writable (notmuch_database_t *notmuch);

notmuch_status_t
_notmuch_database_reopen (notmuch_database_t *notmuch);

void
_notmuch_database_log (notmuch_database_t *notmuch,
		       const char *format, ...);

void
_notmuch_database_log_append (notmuch_database_t *notmuch,
			      const char *format, ...);

unsigned long
_notmuch_database_new_revision (notmuch_database_t *notmuch);

const char *
_notmuch_database_relative_path (notmuch_database_t *notmuch,
				 const char *path);

notmuch_status_t
_notmuch_database_split_path (void *ctx,
			      const char *path,
			      const char **directory,
			      const char **basename);

const char *
_notmuch_database_get_directory_db_path (const char *path);

unsigned int
_notmuch_database_generate_doc_id (notmuch_database_t *notmuch);

notmuch_private_status_t
_notmuch_database_find_unique_doc_id (notmuch_database_t *notmuch,
				      const char *prefix_name,
				      const char *value,
				      unsigned int *doc_id);

notmuch_status_t
_notmuch_database_find_directory_id (notmuch_database_t *database,
				     const char *path,
				     notmuch_find_flags_t flags,
				     unsigned int *directory_id);

const char *
_notmuch_database_get_directory_path (void *ctx,
				      notmuch_database_t *notmuch,
				      unsigned int doc_id);

notmuch_status_t
_notmuch_database_filename_to_direntry (void *ctx,
					notmuch_database_t *notmuch,
					const char *filename,
					notmuch_find_flags_t flags,
					char **direntry);

/* directory.cc */

notmuch_directory_t *
_notmuch_directory_find_or_create (notmuch_database_t *notmuch,
				   const char *path,
				   notmuch_find_flags_t flags,
				   notmuch_status_t *status_ret);

unsigned int
_notmuch_directory_get_document_id (notmuch_directory_t *directory);

notmuch_database_mode_t
_notmuch_database_mode (notmuch_database_t *notmuch);

/* message.cc */

notmuch_message_t *
_notmuch_message_create (const void *talloc_owner,
			 notmuch_database_t *notmuch,
			 unsigned int doc_id,
			 notmuch_private_status_t *status);

notmuch_message_t *
_notmuch_message_create_for_message_id (notmuch_database_t *notmuch,
					const char *message_id,
					notmuch_private_status_t *status);

unsigned int
_notmuch_message_get_doc_id (notmuch_message_t *message);

const char *
_notmuch_message_get_in_reply_to (notmuch_message_t *message);

notmuch_private_status_t
_notmuch_message_add_term (notmuch_message_t *message,
			   const char *prefix_name,
			   const char *value);

notmuch_private_status_t
_notmuch_message_remove_term (notmuch_message_t *message,
			      const char *prefix_name,
			      const char *value);

notmuch_private_status_t
_notmuch_message_has_term (notmuch_message_t *message,
			   const char *prefix_name,
			   const char *value,
			   bool *result);

notmuch_private_status_t
_notmuch_message_gen_terms (notmuch_message_t *message,
			    const char *prefix_name,
			    const char *text);

void
_notmuch_message_upgrade_filename_storage (notmuch_message_t *message);

void
_notmuch_message_upgrade_folder (notmuch_message_t *message);

notmuch_status_t
_notmuch_message_add_filename (notmuch_message_t *message,
			       const char *filename);

notmuch_status_t
_notmuch_message_remove_filename (notmuch_message_t *message,
				  const char *filename);

notmuch_status_t
_notmuch_message_rename (notmuch_message_t *message,
			 const char *new_filename);

void
_notmuch_message_ensure_thread_id (notmuch_message_t *message);

void
_notmuch_message_set_header_values (notmuch_message_t *message,
				    const char *date,
				    const char *from,
				    const char *subject);

void
_notmuch_message_update_subject (notmuch_message_t *message,
				 const char *subject);

void
_notmuch_message_upgrade_last_mod (notmuch_message_t *message);

void
_notmuch_message_sync (notmuch_message_t *message);

notmuch_status_t
_notmuch_message_delete (notmuch_message_t *message);

notmuch_private_status_t
_notmuch_message_initialize_ghost (notmuch_message_t *message,
				   const char *thread_id);

void
_notmuch_message_close (notmuch_message_t *message);

/* Get a copy of the data in this message document.
 *
 * Caller should talloc_free the result when done.
 *
 * This function is intended to support database upgrade and really
 * shouldn't be used otherwise. */
char *
_notmuch_message_talloc_copy_data (notmuch_message_t *message);

/* Clear the data in this message document.
 *
 * This function is intended to support database upgrade and really
 * shouldn't be used otherwise. */
void
_notmuch_message_clear_data (notmuch_message_t *message);

/* Set the author member of 'message' - this is the representation used
 * when displaying the message */
void
_notmuch_message_set_author (notmuch_message_t *message, const char *author);

/* Get the author member of 'message' */
const char *
_notmuch_message_get_author (notmuch_message_t *message);

/* message-file.c */

/* XXX: I haven't decided yet whether these will actually get exported
 * into the public interface in notmuch.h
 */

typedef struct _notmuch_message_file notmuch_message_file_t;

/* Open a file containing a single email message.
 *
 * The caller should call notmuch_message_close when done with this.
 *
 * Returns NULL if any error occurs.
 */
notmuch_message_file_t *
_notmuch_message_file_open (notmuch_database_t *notmuch, const char *filename);

/* Like notmuch_message_file_open but with 'ctx' as the talloc owner. */
notmuch_message_file_t *
_notmuch_message_file_open_ctx (notmuch_database_t *notmuch,
				void *ctx, const char *filename);

/* Close a notmuch message previously opened with notmuch_message_open. */
void
_notmuch_message_file_close (notmuch_message_file_t *message);

/* Parse the message.
 *
 * This will be done automatically as necessary on other calls
 * depending on it, but an explicit call allows for better error
 * status reporting.
 */
notmuch_status_t
_notmuch_message_file_parse (notmuch_message_file_t *message);

/* Get the gmime message of a message file.
 *
 * The message file is parsed as necessary.
 *
 * The GMimeMessage* is set to *mime_message on success (which the
 * caller must not unref).
 *
 * XXX: Would be nice to not have to expose GMimeMessage here.
 */
notmuch_status_t
_notmuch_message_file_get_mime_message (notmuch_message_file_t *message,
					GMimeMessage **mime_message);

/* Get the value of the specified header from the message as a UTF-8 string.
 *
 * The message file is parsed as necessary.
 *
 * The header name is case insensitive.
 *
 * The Received: header is special - for it all Received: headers in
 * the message are concatenated
 *
 * The returned value is owned by the notmuch message and is valid
 * only until the message is closed. The caller should copy it if
 * needing to modify the value or to hold onto it for longer.
 *
 * Returns NULL on errors, empty string if the message does not
 * contain a header line matching 'header'.
 */
const char *
_notmuch_message_file_get_header (notmuch_message_file_t *message,
				  const char *header);

notmuch_status_t
_notmuch_message_file_get_headers (notmuch_message_file_t *message_file,
				   const char **from_out,
				   const char **subject_out,
				   const char **to_out,
				   const char **date_out,
				   char **message_id_out);

const char *
_notmuch_message_file_get_filename (notmuch_message_file_t *message);

/* add-message.cc */
notmuch_status_t
_notmuch_database_link_message_to_parents (notmuch_database_t *notmuch,
					   notmuch_message_t *message,
					   notmuch_message_file_t *message_file,
					   const char **thread_id);
/* index.cc */

notmuch_status_t
_notmuch_message_index_file (notmuch_message_t *message,
			     notmuch_indexopts_t *indexopts,
			     notmuch_message_file_t *message_file);

/* messages.c */

typedef struct _notmuch_message_node {
    notmuch_message_t *message;
    struct _notmuch_message_node *next;
} notmuch_message_node_t;

typedef struct _notmuch_message_list {
    notmuch_message_node_t *head;
    notmuch_message_node_t **tail;
} notmuch_message_list_t;

/* There's a rumor that there's an alternate struct _notmuch_messages
 * somewhere with some nasty C++ objects in it. We'll try to maintain
 * ignorance of that here. (See notmuch_mset_messages_t in query.cc)
 */
struct _notmuch_messages {
    bool is_of_list_type;
    notmuch_doc_id_set_t *excluded_doc_ids;
    notmuch_message_node_t *iterator;
};

notmuch_message_list_t *
_notmuch_message_list_create (const void *ctx);

bool
_notmuch_message_list_empty (notmuch_message_list_t *list);

void
_notmuch_message_list_add_message (notmuch_message_list_t *list,
				   notmuch_message_t *message);

notmuch_messages_t *
_notmuch_messages_create (notmuch_message_list_t *list);

bool
_notmuch_messages_has_next (notmuch_messages_t *messages);

/* query.cc */

bool
_notmuch_mset_messages_valid (notmuch_messages_t *messages);

notmuch_message_t *
_notmuch_mset_messages_get (notmuch_messages_t *messages);

void
_notmuch_mset_messages_move_to_next (notmuch_messages_t *messages);

bool
_notmuch_doc_id_set_contains (notmuch_doc_id_set_t *doc_ids,
			      unsigned int doc_id);

void
_notmuch_doc_id_set_remove (notmuch_doc_id_set_t *doc_ids,
			    unsigned int doc_id);

/* querying xapian documents by type (e.g. "mail" or "ghost"): */
notmuch_status_t
_notmuch_query_search_documents (notmuch_query_t *query,
				 const char *type,
				 notmuch_messages_t **out);

notmuch_status_t
_notmuch_query_count_documents (notmuch_query_t *query,
				const char *type,
				unsigned *count_out);
/* message-id.c */

/* Parse an RFC 822 message-id, discarding whitespace, any RFC 822
 * comments, and the '<' and '>' delimiters.
 *
 * If not NULL, then *next will be made to point to the first character
 * not parsed, (possibly pointing to the final '\0' terminator.
 *
 * Returns a newly talloc'ed string belonging to 'ctx'.
 *
 * Returns NULL if there is any error parsing the message-id. */
char *
_notmuch_message_id_parse (void *ctx, const char *message_id, const char **next);

/* Parse a message-id, discarding leading and trailing whitespace, and
 * '<' and '>' delimiters.
 *
 * Apply a probably-stricter-than RFC definition of what is allowed in
 * a message-id. In particular, forbid whitespace.
 *
 * Returns a newly talloc'ed string belonging to 'ctx'.
 *
 * Returns NULL if there is any error parsing the message-id.
 */

char *
_notmuch_message_id_parse_strict (void *ctx, const char *message_id);


/* message.cc */

void
_notmuch_message_add_reply (notmuch_message_t *message,
			    notmuch_message_t *reply);

void
_notmuch_message_remove_unprefixed_terms (notmuch_message_t *message);

const char *
_notmuch_message_get_thread_id_only (notmuch_message_t *message);

size_t _notmuch_message_get_thread_depth (notmuch_message_t *message);

void
_notmuch_message_label_depths (notmuch_message_t *message,
			       size_t depth);

notmuch_message_list_t *
_notmuch_message_sort_subtrees (void *ctx, notmuch_message_list_t *list);

/* sha1.c */

char *
_notmuch_sha1_of_string (const char *str);

char *
_notmuch_sha1_of_file (const char *filename);

/* string-list.c */

typedef struct _notmuch_string_node {
    char *string;
    struct _notmuch_string_node *next;
} notmuch_string_node_t;

typedef struct _notmuch_string_list {
    int length;
    notmuch_string_node_t *head;
    notmuch_string_node_t **tail;
} notmuch_string_list_t;

notmuch_string_list_t *
_notmuch_string_list_create (const void *ctx);

/*
 * return the number of strings in 'list'
 */
int
_notmuch_string_list_length (notmuch_string_list_t *list);

/* Add 'string' to 'list'.
 *
 * The list will create its own talloced copy of 'string'.
 */
void
_notmuch_string_list_append (notmuch_string_list_t *list,
			     const char *string);

void
_notmuch_string_list_sort (notmuch_string_list_t *list);

const notmuch_string_list_t *
_notmuch_message_get_references (notmuch_message_t *message);

/* string-map.c */
typedef struct _notmuch_string_map notmuch_string_map_t;
typedef struct _notmuch_string_map_iterator notmuch_string_map_iterator_t;
notmuch_string_map_t *
_notmuch_string_map_create (const void *ctx);

void
_notmuch_string_map_append (notmuch_string_map_t *map,
			    const char *key,
			    const char *value);

void
_notmuch_string_map_set (notmuch_string_map_t *map,
			 const char *key,
			 const char *value);

const char *
_notmuch_string_map_get (notmuch_string_map_t *map, const char *key);

notmuch_string_map_iterator_t *
_notmuch_string_map_iterator_create (notmuch_string_map_t *map, const char *key,
				     bool exact);

bool
_notmuch_string_map_iterator_valid (notmuch_string_map_iterator_t *iter);

void
_notmuch_string_map_iterator_move_to_next (notmuch_string_map_iterator_t *iter);

const char *
_notmuch_string_map_iterator_key (notmuch_string_map_iterator_t *iterator);

const char *
_notmuch_string_map_iterator_value (notmuch_string_map_iterator_t *iterator);

void
_notmuch_string_map_iterator_destroy (notmuch_string_map_iterator_t *iterator);

/* Create an iterator for user headers. Destroy with
 * _notmuch_string_map_iterator_destroy. Actually in database.cc*/
notmuch_string_map_iterator_t *
_notmuch_database_user_headers (notmuch_database_t *notmuch);

/* tags.c */

notmuch_tags_t *
_notmuch_tags_create (const void *ctx, notmuch_string_list_t *list);

/* filenames.c */

/* The notmuch_filenames_t iterates over a notmuch_string_list_t of
 * file names */
notmuch_filenames_t *
_notmuch_filenames_create (const void *ctx,
			   notmuch_string_list_t *list);

/* thread.cc */

notmuch_thread_t *
_notmuch_thread_create (void *ctx,
			notmuch_database_t *notmuch,
			unsigned int seed_doc_id,
			notmuch_doc_id_set_t *match_set,
			notmuch_string_list_t *excluded_terms,
			notmuch_exclude_t omit_exclude,
			notmuch_sort_t sort);

/* indexopts.c */

struct _notmuch_indexopts {
    _notmuch_crypto_t crypto;
};

#define CONFIG_HEADER_PREFIX "index.header."

#define EMPTY_STRING(s) ((s)[0] == '\0')

NOTMUCH_END_DECLS

#ifdef __cplusplus
/* Implicit typecast from 'void *' to 'T *' is okay in C, but not in
 * C++. In talloc_steal, an explicit cast is provided for type safety
 * in some GCC versions. Otherwise, a cast is required. Provide a
 * template function for this to maintain type safety, and redefine
 * talloc_steal to use it.
 */
#if ! (__GNUC__ >= 3)
template <class T> T *
_notmuch_talloc_steal (const void *new_ctx, const T *ptr)
{
    return static_cast<T *> (talloc_steal (new_ctx, ptr));
}
#undef talloc_steal
#define talloc_steal _notmuch_talloc_steal
#endif
#endif

#endif
