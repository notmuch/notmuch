/* sexp-fp.h - sexp field processor glue
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

#ifndef NOTMUCH_SEXP_FP_H
#define NOTMUCH_SEXP_FP_H

#include <xapian.h>
#include "notmuch.h"

class SexpFieldProcessor : public Xapian::FieldProcessor {
protected:
    notmuch_database_t *notmuch;

public:
    SexpFieldProcessor (notmuch_database_t *notmuch_) : notmuch (notmuch_)
    {
    };

    Xapian::Query operator() (const std::string & query_string);
};

#endif /* NOTMUCH_SEXP_FP_H */
