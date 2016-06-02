/* query-fp.cc - "query:" field processor glue
 *
 * This file is part of notmuch.
 *
 * Copyright © 2016 David Bremner
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
#include "query-fp.h"
#include <iostream>

#if HAVE_XAPIAN_FIELD_PROCESSOR

Xapian::Query
QueryFieldProcessor::operator() (const std::string & name)
{
    std::string key = "query." + name;
    char *expansion;
    notmuch_status_t status;

    status = notmuch_database_get_config (notmuch, key.c_str (), &expansion);
    if (status) {
	throw Xapian::QueryParserError ("error looking up key" + name);
    }

    return parser.parse_query (expansion, NOTMUCH_QUERY_PARSER_FLAGS);
}
#endif
