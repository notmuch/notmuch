/* parse-time-vrp.cc - date range query glue
 *
 * This file is part of notmuch.
 *
 * Copyright © 2012 Jani Nikula
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
 * Author: Jani Nikula <jani@nikula.org>
 */

#include "database-private.h"
#include "parse-time-vrp.h"
#include "parse-time-string.h"

#define PREFIX "date:"

/* See *ValueRangeProcessor in xapian-core/api/valuerangeproc.cc */
Xapian::valueno
ParseTimeValueRangeProcessor::operator() (std::string &begin, std::string &end)
{
    time_t t, now;

    /* Require date: prefix in start of the range... */
    if (STRNCMP_LITERAL (begin.c_str (), PREFIX))
	return Xapian::BAD_VALUENO;

    /* ...and remove it. */
    begin.erase (0, sizeof (PREFIX) - 1);

    /* Use the same 'now' for begin and end. */
    if (time (&now) == (time_t) -1)
	return Xapian::BAD_VALUENO;

    if (!begin.empty ()) {
	if (parse_time_string (begin.c_str (), &t, &now, PARSE_TIME_ROUND_DOWN))
	    return Xapian::BAD_VALUENO;

	begin.assign (Xapian::sortable_serialise ((double) t));
    }

    if (!end.empty ()) {
	if (parse_time_string (end.c_str (), &t, &now, PARSE_TIME_ROUND_UP_INCLUSIVE))
	    return Xapian::BAD_VALUENO;

	end.assign (Xapian::sortable_serialise ((double) t));
    }

    return valno;
}
