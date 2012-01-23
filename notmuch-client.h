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

#include <talloc.h>

#define unused(x) x __attribute__ ((unused))

#define STRINGIFY(s) STRINGIFY_(s)
#define STRINGIFY_(s) #s

typedef struct notmuch_show_format {
    const char *message_set_start;
    const char *message_start;
    void (*message) (const void *ctx,
		     notmuch_message_t *message,
		     int indent);
    const char *header_start;
    void (*header) (const void *ctx,
		    notmuch_message_t *message);
    void (*header_message_part) (GMimeMessage *message);
    const char *header_end;
    const char *body_start;
    void (*part_start) (GMimeObject *part,
			int *part_count);
    void (*part_encstatus) (int status);
#ifdef GMIME_ATLEAST_26
    void (*part_sigstatus) (GMimeSignatureList* siglist);
#else
    void (*part_sigstatus) (const GMimeSignatureValidity* validity);
#endif
    void (*part_content) (GMimeObject *part);
    void (*part_end) (GMimeObject *part);
    const char *part_sep;
    const char *body_end;
    const char *message_end;
    const char *message_set_sep;
    const char *message_set_end;
} notmuch_show_format_t;

typedef struct notmuch_show_params {
    int entire_thread;
    int raw;
    int part;
#ifdef GMIME_ATLEAST_26
    GMimeCryptoContext* cryptoctx;
#else
    GMimeCipherContext* cryptoctx;
#endif
    int decrypt;
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

int
notmuch_count_command (void *ctx, int argc, char *argv[]);

int
notmuch_dump_command (void *ctx, int argc, char *argv[]);

int
notmuch_new_command (void *ctx, int argc, char *argv[]);

int
notmuch_reply_command (void *ctx, int argc, char *argv[]);

int
notmuch_restore_command (void *ctx, int argc, char *argv[]);

int
notmuch_search_command (void *ctx, int argc, char *argv[]);

int
notmuch_setup_command (void *ctx, int argc, char *argv[]);

int
notmuch_show_command (void *ctx, int argc, char *argv[]);

int
notmuch_tag_command (void *ctx, int argc, char *argv[]);

int
notmuch_search_tags_command (void *ctx, int argc, char *argv[]);

int
notmuch_cat_command (void *ctx, int argc, char *argv[]);

int
notmuch_config_command (void *ctx, int argc, char *argv[]);

const char *
notmuch_time_relative_date (const void *ctx, time_t then);

void
notmuch_time_print_formatted_seconds (double seconds);

double
notmuch_time_elapsed (struct timeval start, struct timeval end);

char *
query_string_from_args (void *ctx, int argc, char *argv[]);

notmuch_status_t
show_message_body (notmuch_message_t *message,
		   const notmuch_show_format_t *format,
		   notmuch_show_params_t *params);

notmuch_status_t
show_one_part (const char *filename, int part);

char *
json_quote_chararray (const void *ctx, const char *str, const size_t len);

char *
json_quote_str (const void *ctx, const char *str);

/* notmuch-config.c */

typedef struct _notmuch_config notmuch_config_t;

notmuch_config_t *
notmuch_config_open (void *ctx,
		     const char *filename,
		     notmuch_bool_t *is_new_ret);

void
notmuch_config_close (notmuch_config_t *config);

int
notmuch_config_save (notmuch_config_t *config);

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
typedef struct mime_node {
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
} mime_node_t;

/* Construct a new MIME node pointing to the root message part of
 * message.  If cryptoctx is non-NULL, it will be used to verify
 * signatures on any child parts.  If decrypt is true, then cryptoctx
 * will additionally be used to decrypt any encrypted child parts.
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
#ifdef GMIME_ATLEAST_26
		GMimeCryptoContext *cryptoctx,
#else
		GMimeCipherContext *cryptoctx,
#endif
		notmuch_bool_t decrypt, mime_node_t **node_out);

/* Return a new MIME node for the requested child part of parent.
 * parent will be used as the talloc context for the returned child
 * node.
 *
 * In case of any failure, this function returns NULL, (after printing
 * an error message on stderr).
 */
mime_node_t *
mime_node_child (const mime_node_t *parent, int child);

/* Return the nth child of node in a depth-first traversal.  If n is
 * 0, returns node itself.  Returns NULL if there is no such part. */
mime_node_t *
mime_node_seek_dfs (mime_node_t *node, int n);

#include "command-line-arguments.h"
#endif
