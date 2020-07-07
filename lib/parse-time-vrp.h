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
class ParseTimeRangeProcessor : public Xapian::RangeProcessor {

public:
    ParseTimeRangeProcessor (Xapian::valueno slot_, const std::string prefix_)
	:  Xapian::RangeProcessor(slot_, prefix_, 0) { }

    Xapian::Query operator() (const std::string &begin, const std::string &end);
};

class DateFieldProcessor : public Xapian::FieldProcessor {
private:
    Xapian::valueno slot;
public:
    DateFieldProcessor(Xapian::valueno slot_) : slot(slot_) { };
    Xapian::Query operator()(const std::string & str);
};

#endif /* NOTMUCH_PARSE_TIME_VRP_H */
