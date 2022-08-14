/* lastmod-fp.h - database revision query glue
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

#ifndef NOTMUCH_LASTMOD_FP_H
#define NOTMUCH_LASTMOD_FP_H

#include <xapian.h>

class LastModRangeProcessor : public Xapian::RangeProcessor {
protected:
    notmuch_database_t *notmuch;

public:
    LastModRangeProcessor (notmuch_database_t *notmuch_, const std::string prefix_)
	:  Xapian::RangeProcessor (NOTMUCH_VALUE_LAST_MOD, prefix_, 0), notmuch (notmuch_)
    {
    }

    Xapian::Query operator() (const std::string &begin, const std::string &end);
};

#endif /* NOTMUCH_LASTMOD_FP_H */
