/* query-fp.h - query field processor glue
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

#ifndef NOTMUCH_QUERY_FP_H
#define NOTMUCH_QUERY_FP_H

#include <xapian.h>
#include "notmuch.h"

#if HAVE_XAPIAN_FIELD_PROCESSOR
class QueryFieldProcessor : public Xapian::FieldProcessor {
 protected:
    Xapian::QueryParser &parser;
    notmuch_database_t *notmuch;

 public:
    QueryFieldProcessor (Xapian::QueryParser &parser_, notmuch_database_t *notmuch_)
	: parser(parser_), notmuch(notmuch_) { };

    Xapian::Query operator()(const std::string & str);
};
#endif
#endif /* NOTMUCH_QUERY_FP_H */
