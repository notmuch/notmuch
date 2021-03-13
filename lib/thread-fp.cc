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
	    std::string subquery_str = str.substr (1, str.size () - 2);
	    notmuch_query_t *subquery = notmuch_query_create (notmuch, subquery_str.c_str ());
	    notmuch_messages_t *messages;
	    std::set<std::string> terms;

	    if (! subquery)
		throw Xapian::QueryParserError ("failed to create subquery for '" + subquery_str +
						"'");

	    status = notmuch_query_search_messages (subquery, &messages);
	    if (status)
		throw Xapian::QueryParserError ("failed to search messages for '" + subquery_str +
						"'");

	    for (; notmuch_messages_valid (messages); notmuch_messages_move_to_next (messages)) {
		std::string term = thread_prefix;
		notmuch_message_t *message;
		message = notmuch_messages_get (messages);
		term += _notmuch_message_get_thread_id_only (message);
		terms.insert (term);
	    }
	    return Xapian::Query (Xapian::Query::OP_OR, terms.begin (), terms.end ());
	}
    } else {
	/* literal thread id */
	std::string term = thread_prefix + str;
	return Xapian::Query (term);
    }

}
