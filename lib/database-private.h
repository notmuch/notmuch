/* database-private.h - For peeking into the internals of notmuch_database_t
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

#ifndef NOTMUCH_DATABASE_PRIVATE_H
#define NOTMUCH_DATABASE_PRIVATE_H

/* According to WG14/N1124, a C++ implementation won't provide us a
 * macro like PRIx64 (which gives a printf format string for
 * formatting a uint64_t as hexadecimal) unless we define
 * __STDC_FORMAT_MACROS before including inttypes.h. That's annoying,
 * but there it is.
 */
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include "notmuch-private.h"

#include <xapian.h>

#pragma GCC visibility push(hidden)

struct _notmuch_database {
    notmuch_bool_t exception_reported;

    char *path;

    notmuch_bool_t needs_upgrade;
    notmuch_database_mode_t mode;
    int atomic_nesting;
    Xapian::Database *xapian_db;

    unsigned int last_doc_id;
    uint64_t last_thread_id;

    Xapian::QueryParser *query_parser;
    Xapian::TermGenerator *term_gen;
    Xapian::ValueRangeProcessor *value_range_processor;
    Xapian::ValueRangeProcessor *date_range_processor;
};

/* Return the list of terms from the given iterator matching a prefix.
 * The prefix will be stripped from the strings in the returned list.
 * The list will be allocated using ctx as the talloc context.
 *
 * The function returns NULL on failure.
 */
notmuch_string_list_t *
_notmuch_database_get_terms_with_prefix (void *ctx, Xapian::TermIterator &i,
					 Xapian::TermIterator &end,
					 const char *prefix);

#pragma GCC visibility pop

#endif
