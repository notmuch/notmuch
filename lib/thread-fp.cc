/* thread-fp.cc - "thread:" field processor glue
 *
 * This file is part of notmuch.
 *
 * Copyright © 2018 David Bremner
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
#include "thread-fp.h"
#include <iostream>

Xapian::Query
ThreadFieldProcessor::operator() (const std::string & str)
{
    notmuch_status_t status;
    const char *thread_prefix = _find_prefix ("thread");

    if (str.at (0) == '{') {
	if (str.size () <= 1 || str.at (str.size () - 1) != '}') {
	    throw Xapian::QueryParserError ("missing } in '" + str + "'");
	} else {
	    Xapian::Query subquery;
	    Xapian::Query query;
	    std::string msg;
	    std::string subquery_str = str.substr (1, str.size () - 2);

	    status = _notmuch_query_string_to_xapian_query (notmuch, subquery_str, subquery, msg);
	    if (status)
		throw Xapian::QueryParserError (msg);

	    status = _notmuch_query_expand (notmuch, "thread", subquery, query, msg);
	    if (status)
		throw Xapian::QueryParserError (msg);

	    return query;
	}
    } else {
	/* literal thread id */
	std::string term = thread_prefix + str;
	return Xapian::Query (term);
    }

}
