/* notmuch-private-cxx.h - Internal, C++ interfaces for notmuch.
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

#ifndef NOTMUCH_PRIVATE_CXX_H
#define NOTMUCH_PRIVATE_CXX_H

/* It's a bit annoying to have a separate C++ private header, but I
 * don't yet have C wrappers for types like Xapian::Document and
 * Xapian::PostingIterator. Maybe I'll write those to be able to fold
 * this stuff back into notmuch-private.h */

#include "notmuch-private.h"

#include <xapian.h>

/***********/
/* tags.cc */
/***********/

typedef struct _notmuch_terms {
    char prefix_char;
    Xapian::TermIterator iterator;
    Xapian::TermIterator iterator_end;
} notmuch_terms_t;

struct _notmuch_tags {
    notmuch_terms_t terms;
};

notmuch_terms_t *
_notmuch_terms_create (void *ctx,
		       Xapian::Document doc,
		       const char *prefix_name);

/* The assertion is to ensure that 'type' is a derivative of
 * notmuch_terms_t in that it contains a notmuch_terms_t as its first
 * member. We do this by name of 'terms' as opposed to type, because
 * that's as clever as I've been so far. */
#define _notmuch_terms_create_type(ctx, doc, prefix_name, type) \
    (COMPILE_TIME_ASSERT(offsetof(type, terms) == 0),		\
     (type *) _notmuch_terms_create (ctx, doc, prefix_name))

#endif
