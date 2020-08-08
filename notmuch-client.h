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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#ifndef NOTMUCH_CLIENT_H
#define NOTMUCH_CLIENT_H

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* for getline */
#endif
#include <stdbool.h>
#include <stdio.h>
#include <sysexits.h>

#include "compat.h"

#include "gmime-extra.h"

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
#include <ctype.h>
#include <zlib.h>

#include "talloc-extra.h"
#include "crypto.h"
#include "repair.h"

#define unused(x) x ## _unused __attribute__ ((unused))

#define STRINGIFY(s) STRINGIFY_ (s)
#define STRINGIFY_(s) #s

typedef struct mime_node mime_node_t;
struct sprinter;
struct notmuch_show_params;

typedef struct notmuch_show_format {
    struct sprinter *(*new_sprinter)(const void *ctx, FILE *stream);
    notmuch_status_t (*part)(const void *ctx, struct sprinter *sprinter,
			     struct mime_node *node, int indent,
			     const struct notmuch_show_params *params);
} notmuch_show_format_t;

typedef struct notmuch_show_params {
    bool entire_thread;
    bool omit_excluded;
    bool output_body;
    int part;
    _notmuch_crypto_t crypto;
    bool include_html;
    GMimeStream *out_stream;
} notmuch_show_params_t;

/* There's no point in continuing when we've detected that we've done
 * something wrong internally (as opposed to the user passing in a
 * bogus value).
 *
 * Note that __location__ comes from talloc.h.
 */
#define INTERNAL_ERROR(format, ...)                     \
    do {                                                \
	fprintf (stderr,                                 \
		 "Internal error: " format " (%s)\n",    \
		 ##__VA_ARGS__, __location__);           \
	exit (1);                                       \
    } while (0)

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

#define STRNCMP_LITERAL(var, literal) \
    strncmp ((var), (literal), sizeof (literal) - 1)

static inline void
chomp_newline (char *str)
{
    if (str && str[strlen (str) - 1] == '\n')
	str[strlen (str) - 1] = '\0';
}

/* Exit status code indicating temporary failure; user is invited to
 * retry.
 *
 * For example, file(s) in the mail store were removed or renamed
 * after notmuch new scanned the directories but before indexing the
 * file(s). If the file was renamed, the indexing might not be
 * complete, and the user is advised to re-run notmuch new.
 */
#define NOTMUCH_EXIT_TEMPFAIL EX_TEMPFAIL

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
#define NOTMUCH_FORMAT_CUR 4
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

int
notmuch_count_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_dump_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_new_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_insert_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_reindex_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_reply_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_restore_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_search_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_address_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_setup_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_show_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_tag_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_config_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

int
notmuch_compact_command (notmuch_config_t *config, notmuch_database_t *notmuch, int argc, char *argv[]);

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
		      bool output_body,
		      bool include_html);

void
format_headers_sprinter (struct sprinter *sp, GMimeMessage *message,
			 bool reply, const _notmuch_message_crypto_t *msg_crypto);

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

typedef enum {
    NOTMUCH_COMMAND_CONFIG_OPEN		= 1 << 0,
    NOTMUCH_COMMAND_CONFIG_CREATE	= 1 << 1,
    NOTMUCH_COMMAND_DATABASE_EARLY	= 1 << 2,
    NOTMUCH_COMMAND_DATABASE_WRITE	= 1 << 3,
} notmuch_command_mode_t;

notmuch_config_t *
notmuch_config_open (void *ctx,
		     const char *filename,
		     notmuch_command_mode_t config_mode);

void
notmuch_config_close (notmuch_config_t *config);

int
notmuch_config_save (notmuch_config_t *config);

bool
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

bool
notmuch_config_get_maildir_synchronize_flags (notmuch_config_t *config);

void
notmuch_config_set_maildir_synchronize_flags (notmuch_config_t *config,
					      bool synchronize_flags);

const char **
notmuch_config_get_search_exclude_tags (notmuch_config_t *config, size_t *length);

void
notmuch_config_set_search_exclude_tags (notmuch_config_t *config,
					const char *list[],
					size_t length);
const char *
_notmuch_config_get_path (notmuch_config_t *config);

int
notmuch_run_hook (const char *db_path, const char *hook);

bool
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
    bool decrypt_attempted;
    /* True if decryption of this part's child succeeded.  In this
     * case, the decrypted part is substituted for the second child of
     * this part (which would usually be the encrypted data). */
    bool decrypt_success;

    /* True if signature verification on this part was attempted. */
    bool verify_attempted;

    /* The list of signatures for signed or encrypted containers. If
     * there are no signatures, this will be NULL. */
    GMimeSignatureList *sig_list;

    /* Internal: Context inherited from the root iterator. */
    struct mime_node_context *ctx;

    /* Internal: For successfully decrypted multipart parts, the
     * decrypted part to substitute for the second child; or, for
     * PKCS#7 parts, the part returned after removing/processing the
     * PKCS#7 transformation */
    GMimeObject *unwrapped_child;

    /* Internal: The next child for depth-first traversal and the part
     * number to assign it (or -1 if unknown). */
    int next_child;
    int next_part_num;
};

/* Construct a new MIME node pointing to the root message part of
 * message. If crypto->verify is true, signed child parts will be
 * verified. If crypto->decrypt is NOTMUCH_DECRYPT_TRUE, encrypted
 * child parts will be decrypted using either stored session keys or
 * asymmetric crypto.  If crypto->decrypt is NOTMUCH_DECRYPT_AUTO,
 * only session keys will be tried.  If the crypto contexts
 * (crypto->gpgctx or crypto->pkcs7) are NULL, they will be lazily
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
		_notmuch_crypto_t *crypto, mime_node_t **node_out);

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

const _notmuch_message_crypto_t *
mime_node_get_message_crypto_status (mime_node_t *node);

typedef enum dump_formats {
    DUMP_FORMAT_AUTO,
    DUMP_FORMAT_BATCH_TAG,
    DUMP_FORMAT_SUP
} dump_format_t;

typedef enum dump_includes {
    DUMP_INCLUDE_TAGS		= 1,
    DUMP_INCLUDE_CONFIG		= 2,
    DUMP_INCLUDE_PROPERTIES	= 4
} dump_include_t;

#define DUMP_INCLUDE_DEFAULT (DUMP_INCLUDE_TAGS | DUMP_INCLUDE_CONFIG | DUMP_INCLUDE_PROPERTIES)

#define NOTMUCH_DUMP_VERSION 3

int
notmuch_database_dump (notmuch_database_t *notmuch,
		       const char *output_file_name,
		       const char *query_str,
		       dump_format_t output_format,
		       dump_include_t include,
		       bool gzip_output);

/* If status indicates error print appropriate
 * messages to stderr.
 */

notmuch_status_t
print_status_query (const char *loc,
		    const notmuch_query_t *query,
		    notmuch_status_t status);

notmuch_status_t
print_status_message (const char *loc,
		      const notmuch_message_t *message,
		      notmuch_status_t status);

notmuch_status_t
print_status_database (const char *loc,
		       const notmuch_database_t *database,
		       notmuch_status_t status);

int
status_to_exit (notmuch_status_t status);

notmuch_status_t
print_status_gzbytes (const char *loc,
		      gzFile file,
		      int bytes);

/* the __location__ macro is defined in talloc.h */
#define ASSERT_GZBYTES(file, bytes) ((print_status_gzbytes (__location__, file, bytes)) ? exit (1) : 0)
#define GZPRINTF(file, fmt, ...) ASSERT_GZBYTES (file, gzprintf (file, fmt, ##__VA_ARGS__));
#define GZPUTS(file, str) ASSERT_GZBYTES(file, gzputs (file, str));

#include "command-line-arguments.h"

extern const char *notmuch_requested_db_uuid;
extern const notmuch_opt_desc_t notmuch_shared_options [];
void notmuch_exit_if_unmatched_db_uuid (notmuch_database_t *notmuch);

void notmuch_process_shared_options (const char *subcommand_name);
int notmuch_minimal_options (const char *subcommand_name,
			     int argc, char **argv);


/* the state chosen by the user invoking one of the notmuch
 * subcommands that does indexing */
struct _notmuch_client_indexing_cli_choices {
    int decrypt_policy;
    bool decrypt_policy_set;
    notmuch_indexopts_t *opts;
};
extern struct _notmuch_client_indexing_cli_choices indexing_cli_choices;
extern const notmuch_opt_desc_t notmuch_shared_indexing_options [];
notmuch_status_t
notmuch_process_shared_indexing_options (notmuch_database_t *notmuch);

#endif
