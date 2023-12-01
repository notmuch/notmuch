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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Jani Nikula <jani@nikula.org>
 */

#include "database-private.h"
#include "parse-time-vrp.h"
#include "parse-time-string.h"

notmuch_status_t
_notmuch_date_strings_to_query (Xapian::valueno slot,
				const std::string &begin, const std::string &end,
				Xapian::Query &output, std::string &msg)
{
    double from = DBL_MIN, to = DBL_MAX;
    time_t parsed_time, now;
    std::string str;

    /* Use the same 'now' for begin and end. */
    if (time (&now) == (time_t) -1) {
	msg = "unable to get current time";
	return NOTMUCH_STATUS_ILLEGAL_ARGUMENT;
    }

    if (! begin.empty ()) {
	if (parse_time_string (begin.c_str (), &parsed_time, &now, PARSE_TIME_ROUND_DOWN)) {
	    msg = "Didn't understand date specification '" + begin + "'";
	    return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	}

	from = (double) parsed_time;
    }

    if (! end.empty ()) {
	if (end == "!" && ! begin.empty ())
	    str = begin;
	else
	    str = end;

	if (parse_time_string (str.c_str (), &parsed_time, &now, PARSE_TIME_ROUND_UP_INCLUSIVE)) {
	    msg = "Didn't understand date specification '" + str + "'";
	    return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	}
	to = (double) parsed_time;
    }

    output = Xapian::Query (Xapian::Query::OP_VALUE_RANGE, slot,
			    Xapian::sortable_serialise (from),
			    Xapian::sortable_serialise (to));
    return NOTMUCH_STATUS_SUCCESS;
}

Xapian::Query
ParseTimeRangeProcessor::operator() (const std::string &begin, const std::string &end)
{

    Xapian::Query output;
    std::string msg;

    if (_notmuch_date_strings_to_query (slot, begin, end, output, msg))
	throw Xapian::QueryParserError (msg);

    return output;
}

/* XXX TODO: is throwing an exception the right thing to do here? */
Xapian::Query
DateFieldProcessor::operator() (const std::string & str)
{
    double from = DBL_MIN, to = DBL_MAX;
    time_t parsed_time, now;

    /* Use the same 'now' for begin and end. */
    if (time (&now) == (time_t) -1)
	throw Xapian::QueryParserError ("Unable to get current time");

    if (parse_time_string (str.c_str (), &parsed_time, &now, PARSE_TIME_ROUND_DOWN))
	throw Xapian::QueryParserError ("Didn't understand date specification '" + str + "'");
    else
	from = (double) parsed_time;

    if (parse_time_string (str.c_str (), &parsed_time, &now, PARSE_TIME_ROUND_UP_INCLUSIVE))
	throw Xapian::QueryParserError ("Didn't understand date specification '" + str + "'");
    else
	to = (double) parsed_time;

    return Xapian::Query (Xapian::Query::OP_VALUE_RANGE, slot,
			  Xapian::sortable_serialise (from),
			  Xapian::sortable_serialise (to));
}
