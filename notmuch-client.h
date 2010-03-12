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
notmuch_part_command (void *ctx, int argc, char *argv[]);

const char *
notmuch_time_relative_date (const void *ctx, time_t then);

void
notmuch_time_print_formatted_seconds (double seconds);

double
notmuch_time_elapsed (struct timeval start, struct timeval end);

char *
query_string_from_args (void *ctx, int argc, char *argv[]);

notmuch_status_t
show_message_body (const char *filename,
		   void (*show_part) (GMimeObject *part, int *part_count));

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

char **
notmuch_config_get_user_other_email (notmuch_config_t *config,
				     size_t *length);

void
notmuch_config_set_user_other_email (notmuch_config_t *config,
				     const char *other_email[],
				     size_t length);

char **
notmuch_config_get_new_tags (notmuch_config_t *config,
			     size_t *length);
void
notmuch_config_set_new_tags (notmuch_config_t *config,
			     const char *new_tags[],
			     size_t length);

notmuch_bool_t
debugger_is_active (void);

#endif
