/* notmuch - Not much of an email program, (just index and search)
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

#ifndef NOTMUCH_CLIENT_H
#define NOTMUCH_CLIENT_H

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* for getline */
#endif
#include <stdio.h>

#include "compat.h"

#include <gmime/gmime.h>

/* GMIME_CHECK_VERSION in gmime 2.4 is not usable from the
 * preprocessor (it calls a runtime function). But since
 * GMIME_MAJOR_VERSION and friends were added in gmime 2.6, we can use
 * these to check the version number. */
#ifdef GMIME_MAJOR_VERSION
#define GMIME_ATLEAST_26
typedef GMimeCryptoContext notmuch_crypto_context_t;
#else
typedef GMimeCipherContext notmuch_crypto_context_t;
#endif

#include "notmuch.h"

/* This is separate from notmuch-private.h because we're trying to
 * keep notmuch.c from looking into any internals, (which helps us
 * develop notmuch.h into a plausible library interface).
 */
#include "xutil.h"

#include <stddef.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <signal.h>

#include "talloc-extra.h"

#define unused(x) x __attribute__ ((unused))

#define STRINGIFY(s) STRINGIFY_(s)
#define STRINGIFY_(s) #s

typedef struct mime_node mime_node_t;
struct sprinter;
struct notmuch_show_params;

typedef struct notmuch_show_format {
    struct sprinter *(*new_sprinter) (const void *ctx, FILE *stream);
    notmuch_status_t (*part) (const void *ctx, struct sprinter *sprinter,
			      struct mime_node *node, int indent,
			      const struct notmuch_show_params *params);
} notmuch_show_format_t;

typedef struct notmuch_crypto {
    notmuch_crypto_context_t* gpgctx;
    notmuch_bool_t verify;
    notmuch_bool_t decrypt;
} notmuch_crypto_t;

typedef struct notmuch_show_params {
    notmuch_bool_t entire_thread;
    notmuch_bool_t omit_excluded;
    notmuch_bool_t output_body;
    notmuch_bool_t raw;
    int part;
    notmuch_crypto_t crypto;
    notmuch_bool_t include_html;
} notmuch_show_params_t;

/* There's no point in continuing when we've detected that we've done
 * something wrong internally (as opposed to the user passing in a
 * bogus value).
 *
 * Note that __location__ comes from talloc.h.
 */
#define INTERNAL_ERROR(format, ...)			\
    do {						\
	fprintf(stderr,					\
		"Internal error: " format " (%s)\n",	\
		##__VA_ARGS__, __location__);		\
	exit (1);					\
    } while (0)

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

#define STRNCMP_LITERAL(var, literal) \
    strncmp ((var), (literal), sizeof (literal) - 1)

static inline void
chomp_newline (char *str)
{
    if (str && str[strlen(str)-1] == '\n')
	str[strlen(str)-1] = '\0';
}

/* Exit status code indicating the requested format version is too old
 * (support for that version has been dropped).  CLI code should use
 * notmuch_exit_if_unsupported_format rather than directly exiting
 * with this code.
 */
#define NOTMUCH_EXIT_FORMAT_TOO_OLD 20
/* Exit status code indicating the requested format version is newer
 * than the version supported by the CLI.  CLI code should use
 * notmuch_exit_if_unsupported_format rather than directly exiting
 * with this code.
 */
#define NOTMUCH_EXIT_FORMAT_TOO_NEW 21

/* The current structured output format version.  Requests for format
 * versions above this will return an error.  Backwards-incompatible
 * changes such as removing map fields, changing the meaning of map
 * fields, or changing the meanings of list elements should increase
 * this.  New (required) map fields can be added without increasing
 * this.
 */
#define NOTMUCH_FORMAT_CUR 2
/* The minimum supported structured output format version.  Requests
 * for format versions below this will return an error. */
#define NOTMUCH_FORMAT_MIN 1
/* The minimum non-deprecated structured output format version.
 * Requests for format versions below this will print a stern warning.
 * Must be between NOTMUCH_FORMAT_MIN and NOTMUCH_FORMAT_CUR,
 * inclusive.
 */
#define NOTMUCH_FORMAT_MIN_ACTIVE 1

/* The output format version requested by the caller on the command
 * line.  If no format version is requested, this will be set to
 * NOTMUCH_FORMAT_CUR.  Even though the command-line option is
 * per-command, this is global because commands can share structured
 * output code.
 */
extern int notmuch_format_version;

typedef struct _notmuch_config notmuch_config_t;

/* Commands that support structured output should support the
 * following argument
 *  { NOTMUCH_OPT_INT, &notmuch_format_version, "format-version", 0, 0 }
 * and should invoke notmuch_exit_if_unsupported_format to check the
 * requested version.  If notmuch_format_version is outside the
 * supported range, this will print a detailed diagnostic message for
 * the user and exit with NOTMUCH_EXIT_FORMAT_TOO_{OLD,NEW} to inform
 * the invoking program of the problem.
 */
void
notmuch_exit_if_unsupported_format (void);

notmuch_crypto_context_t *
notmuch_crypto_get_context (notmuch_crypto_t *crypto, const char *protocol);

int
notmuch_crypto_cleanup (notmuch_crypto_t *crypto);

int
notmuch_count_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_dump_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_new_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_insert_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_reply_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_restore_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_search_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_setup_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_show_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_tag_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_config_command (notmuch_config_t *config, int argc, char *argv[]);

int
notmuch_compact_command (notmuch_config_t *config, int argc, char *argv[]);

const char *
notmuch_time_relative_date (const void *ctx, time_t then);

void
notmuch_time_print_formatted_seconds (double seconds);

double
notmuch_time_elapsed (struct timeval start, struct timeval end);

char *
query_string_from_args (void *ctx, int argc, char *argv[]);

notmuch_status_t
show_one_part (const char *filename, int part);

void
format_part_sprinter (const void *ctx, struct sprinter *sp, mime_node_t *node,
		      notmuch_bool_t first, notmuch_bool_t output_body,
		      notmuch_bool_t include_html);

void
format_headers_sprinter (struct sprinter *sp, GMimeMessage *message,
			 notmuch_bool_t reply);

typedef enum {
    NOTMUCH_SHOW_TEXT_PART_REPLY = 1 << 0,
} notmuch_show_text_part_flags;

void
show_text_part_content (GMimeObject *part, GMimeStream *stream_out,
			notmuch_show_text_part_flags flags);

char *
json_quote_chararray (const void *ctx, const char *str, const size_t len);

char *
json_quote_str (const void *ctx, const char *str);

/* notmuch-config.c */

notmuch_config_t *
notmuch_config_open (void *ctx,
		     const char *filename,
		     notmuch_bool_t create_new);

void
notmuch_config_close (notmuch_config_t *config);

int
notmuch_config_save (notmuch_config_t *config);

notmuch_bool_t
notmuch_config_is_new (notmuch_config_t *config);

const char *
notmuch_config_get_database_path (notmuch_config_t *config);

void
notmuch_config_set_database_path (notmuch_config_t *config,
				  const char *database_path);

const char *
notmuch_config_get_user_name (notmuch_config_t *config);

void
notmuch_config_set_user_name (notmuch_config_t *config,
			      const char *user_name);

const char *
notmuch_config_get_user_primary_email (notmuch_config_t *config);

void
notmuch_config_set_user_primary_email (notmuch_config_t *config,
				       const char *primary_email);

const char **
notmuch_config_get_user_other_email (notmuch_config_t *config,
				     size_t *length);

void
notmuch_config_set_user_other_email (notmuch_config_t *config,
				     const char *other_email[],
				     size_t length);

const char **
notmuch_config_get_new_tags (notmuch_config_t *config,
			     size_t *length);
void
notmuch_config_set_new_tags (notmuch_config_t *config,
			     const char *new_tags[],
			     size_t length);

const char **
notmuch_config_get_new_ignore (notmuch_config_t *config,
			       size_t *length);

void
notmuch_config_set_new_ignore (notmuch_config_t *config,
			       const char *new_ignore[],
			       size_t length);

notmuch_bool_t
notmuch_config_get_maildir_synchronize_flags (notmuch_config_t *config);

void
notmuch_config_set_maildir_synchronize_flags (notmuch_config_t *config,
					      notmuch_bool_t synchronize_flags);

const char **
notmuch_config_get_search_exclude_tags (notmuch_config_t *config, size_t *length);

void
notmuch_config_set_search_exclude_tags (notmuch_config_t *config,
				      const char *list[],
				      size_t length);

int
notmuch_run_hook (const char *db_path, const char *hook);

notmuch_bool_t
debugger_is_active (void);

/* mime-node.c */

/* mime_node_t represents a single node in a MIME tree.  A MIME tree
 * abstracts the different ways of traversing different types of MIME
 * parts, allowing a MIME message to be viewed as a generic tree of
 * parts.  Message-type parts have one child, multipart-type parts
 * have multiple children, and leaf parts have zero children.
 */
struct mime_node {
    /* The MIME object of this part.  This will be a GMimeMessage,
     * GMimePart, GMimeMultipart, or a subclass of one of these.
     *
     * This will never be a GMimeMessagePart because GMimeMessagePart
     * is structurally redundant with GMimeMessage.  If this part is a
     * message (that is, 'part' is a GMimeMessage), then either
     * envelope_file will be set to a notmuch_message_t (for top-level
     * messages) or envelope_part will be set to a GMimeMessagePart
     * (for embedded message parts).
     */
    GMimeObject *part;

    /* If part is a GMimeMessage, these record the envelope of the
     * message: either a notmuch_message_t representing a top-level
     * message, or a GMimeMessagePart representing a MIME part
     * containing a message.
     */
    notmuch_message_t *envelope_file;
    GMimeMessagePart *envelope_part;

    /* The number of children of this part. */
    int nchildren;

    /* The parent of this node or NULL if this is the root node. */
    struct mime_node *parent;

    /* The depth-first part number of this child if the MIME tree is
     * being traversed in depth-first order, or -1 otherwise. */
    int part_num;

    /* True if decryption of this part was attempted. */
    notmuch_bool_t decrypt_attempted;
    /* True if decryption of this part's child succeeded.  In this
     * case, the decrypted part is substituted for the second child of
     * this part (which would usually be the encrypted data). */
    notmuch_bool_t decrypt_success;

    /* True if signature verification on this part was attempted. */
    notmuch_bool_t verify_attempted;
#ifdef GMIME_ATLEAST_26
    /* The list of signatures for signed or encrypted containers. If
     * there are no signatures, this will be NULL. */
    GMimeSignatureList* sig_list;
#else
    /* For signed or encrypted containers, the validity of the
     * signature.  May be NULL if signature verification failed.  If
     * there are simply no signatures, this will be non-NULL with an
     * empty signers list. */
    const GMimeSignatureValidity *sig_validity;
#endif

    /* Internal: Context inherited from the root iterator. */
    struct mime_node_context *ctx;

    /* Internal: For successfully decrypted multipart parts, the
     * decrypted part to substitute for the second child. */
    GMimeObject *decrypted_child;

    /* Internal: The next child for depth-first traversal and the part
     * number to assign it (or -1 if unknown). */
    int next_child;
    int next_part_num;
};

/* Construct a new MIME node pointing to the root message part of
 * message. If crypto->verify is true, signed child parts will be
 * verified. If crypto->decrypt is true, encrypted child parts will be
 * decrypted.  If crypto->gpgctx is NULL, it will be lazily
 * initialized.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Root node is returned in *node_out.
 *
 * NOTMUCH_STATUS_FILE_ERROR: Failed to open message file.
 *
 * NOTMUCH_STATUS_OUT_OF_MEMORY: Out of memory.
 */
notmuch_status_t
mime_node_open (const void *ctx, notmuch_message_t *message,
		notmuch_crypto_t *crypto, mime_node_t **node_out);

/* Return a new MIME node for the requested child part of parent.
 * parent will be used as the talloc context for the returned child
 * node.
 *
 * In case of any failure, this function returns NULL, (after printing
 * an error message on stderr).
 */
mime_node_t *
mime_node_child (mime_node_t *parent, int child);

/* Return the nth child of node in a depth-first traversal.  If n is
 * 0, returns node itself.  Returns NULL if there is no such part. */
mime_node_t *
mime_node_seek_dfs (mime_node_t *node, int n);

#include "command-line-arguments.h"
#endif
