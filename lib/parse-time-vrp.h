/* parse-time-vrp.h - date range query glue
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

#ifndef NOTMUCH_PARSE_TIME_VRP_H
#define NOTMUCH_PARSE_TIME_VRP_H

#include <xapian.h>

/* see *ValueRangeProcessor in xapian-core/include/xapian/queryparser.h */
class ParseTimeValueRangeProcessor : public Xapian::ValueRangeProcessor {
protected:
    Xapian::valueno valno;

public:
    ParseTimeValueRangeProcessor (Xapian::valueno slot_)
	: valno(slot_) { }

    Xapian::valueno operator() (std::string &begin, std::string &end);
};

#if HAVE_XAPIAN_FIELD_PROCESSOR
class DateFieldProcessor : public Xapian::FieldProcessor {
    Xapian::Query operator()(const std::string & str);
};
#endif
#endif /* NOTMUCH_PARSE_TIME_VRP_H */
