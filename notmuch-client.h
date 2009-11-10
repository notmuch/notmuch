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

typedef int (*command_function_t) (void *ctx, int argc, char *argv[]);

typedef struct command {
    const char *name;
    command_function_t function;
    const char *summary;
    const char *documentation;
} command_t;

typedef void (*add_files_callback_t) (notmuch_message_t *message);

typedef struct {
    int ignore_read_only_directories;
    int saw_read_only_directory;

    int total_files;
    int processed_files;
    int added_messages;
    struct timeval tv_start;

    add_files_callback_t callback;
} add_files_state_t;

static inline void
chomp_newline (char *str)
{
    if (str && str[strlen(str)-1] == '\n')
	str[strlen(str)-1] = '\0';
}

int
notmuch_dump_command (void *ctx, int argc, char *argv[]);

int
notmuch_new_command (void *ctx, int argc, char *argv[]);

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

notmuch_status_t
add_files (notmuch_database_t *notmuch, const char *path,
	   add_files_state_t *state);

char *
query_string_from_args (void *ctx, int argc, char *argv[]);

const char *
notmuch_time_relative_date (void *ctx, time_t then);

void
notmuch_time_print_formatted_seconds (double seconds);

double
notmuch_time_elapsed (struct timeval start, struct timeval end);

#endif
