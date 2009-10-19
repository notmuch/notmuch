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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#ifndef NOTMUCH_PRIVATE_H
#define NOTMUCH_PRIVATE_H

#include "notmuch.h"

#include <stdlib.h>
#include <string.h>

NOTMUCH_BEGIN_DECLS

/* xutil.c */
void *
xcalloc (size_t nmemb, size_t size);

void *
xmalloc (size_t size);

void *
xrealloc (void *ptrr, size_t size);

char *
xstrdup (const char *s);

char *
xstrndup (const char *s, size_t n);

NOTMUCH_END_DECLS

#endif
