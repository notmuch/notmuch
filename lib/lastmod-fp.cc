/* lastmod-fp.cc - lastmod range query glue
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

notmuch_status_t
_notmuch_lastmod_strings_to_query (notmuch_database_t *notmuch,
				   const std::string &from, const std::string &to,
				   Xapian::Query &output, std::string &msg)
{
    long from_idx = 0L, to_idx = LONG_MAX;
    long current;
    std::string str;

    /* revision should not change, but for the avoidance of doubt,
     * grab for both ends of range, if needed*/
    current = notmuch_database_get_revision (notmuch, NULL);

    try {
	if (from.empty ())
	    from_idx = 0L;
	else
	    from_idx = std::stol (from);
    } catch (std::logic_error &e) {
	msg = "bad 'from' revision: '" + from + "'";
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    if (from_idx < 0)
	from_idx += current;

    try {
	if (EMPTY_STRING (to))
	    to_idx = LONG_MAX;
	else
	    to_idx = std::stol (to);
    } catch (std::logic_error &e) {
	msg = "bad 'to' revision: '" + to + "'";
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    if (to_idx < 0)
	to_idx += current;

    output = Xapian::Query (Xapian::Query::OP_VALUE_RANGE, NOTMUCH_VALUE_LAST_MOD,
			    Xapian::sortable_serialise (from_idx),
			    Xapian::sortable_serialise (to_idx));
    return NOTMUCH_STATUS_SUCCESS;
}
