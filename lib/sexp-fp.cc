/* sexp-fp.cc - "sexp:" field processor glue
 *
 * This file is part of notmuch.
 *
 * Copyright © 2022 David Bremner
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
 * Author: David Bremner <david@tethera.net>
 */

#include "database-private.h"
#include "sexp-fp.h"
#include <iostream>

Xapian::Query
SexpFieldProcessor::operator() (const std::string & query_string)
{
    notmuch_status_t status;
    Xapian::Query output;

#if HAVE_SFSEXP
    status = _notmuch_sexp_string_to_xapian_query (notmuch, query_string.c_str (), output);
    if (status) {
	throw Xapian::QueryParserError ("error parsing " + query_string);
    }
#else
    throw Xapian::QueryParserError ("sexp query parser not available");
#endif

    return output;

}
