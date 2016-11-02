/* notmuch - Not much of an email library, (just index and search)
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

/* This header file defines functions that will only be conditionally
 * compiled for compatibility on systems that don't provide their own
 * implementations of the functions.
 */

#ifndef NOTMUCH_COMPAT_H
#define NOTMUCH_COMPAT_H

#ifdef __cplusplus
extern "C" {
#endif

#if !STD_GETPWUID
#define _POSIX_PTHREAD_SEMANTICS 1
#endif
#if !STD_ASCTIME
#define _POSIX_PTHREAD_SEMANTICS 1
#endif

#if !HAVE_CANONICALIZE_FILE_NAME
/* we only call this function from C, and this makes testing easier */
#ifndef __cplusplus
char *
canonicalize_file_name (const char *path);
#endif
#endif

#if !HAVE_GETLINE
#include <stdio.h>
#include <unistd.h>

ssize_t
getline (char **lineptr, size_t *n, FILE *stream);

ssize_t
getdelim (char **lineptr, size_t *n, int delimiter, FILE *fp);

#endif /* !HAVE_GETLINE */

#if !HAVE_STRCASESTR
char* strcasestr(const char *haystack, const char *needle);
#endif /* !HAVE_STRCASESTR */

#if !HAVE_STRSEP
char *strsep(char **stringp, const char *delim);
#endif /* !HAVE_STRSEP */

#if !HAVE_TIMEGM
#include <time.h>
time_t timegm (struct tm *tm);
#endif /* !HAVE_TIMEGM */

/* Silence gcc warnings about unused results.  These warnings exist
 * for a reason; any use of this needs to be justified. */
#ifdef __GNUC__
#define IGNORE_RESULT(x) ({ __typeof__(x) __z = (x); (void)(__z = __z); })
#else /* !__GNUC__ */
#define IGNORE_RESULT(x) x
#endif /* __GNUC__ */

#ifdef __cplusplus
}
#endif

#endif /* NOTMUCH_COMPAT_H */
