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

notmuch_tags_t *
_notmuch_tags_create_terms (void *ctx,
			    Xapian::Document doc,
			    const char *prefix_name);

#endif
