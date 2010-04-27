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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
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

#ifdef __cplusplus
}
#endif

#endif /* NOTMUCH_COMPAT_H */
