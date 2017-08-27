/* regex-fields.h - xapian glue for semi-bruteforce regexp search
 *
 * This file is part of notmuch.
 *
 * Copyright © 2015 Austin Clements
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
 * Author: Austin Clements <aclements@csail.mit.edu>
 *                David Bremner <david@tethera.net>
 */

#ifndef NOTMUCH_REGEXP_FIELDS_H
#define NOTMUCH_REGEXP_FIELDS_H
#if HAVE_XAPIAN_FIELD_PROCESSOR
#include <sys/types.h>
#include <regex.h>
#include "database-private.h"
#include "notmuch-private.h"

/* A posting source that returns documents where a value matches a
 * regexp.
 */
class RegexpPostingSource : public Xapian::PostingSource
{
 protected:
    const Xapian::valueno slot_;
    regex_t regexp_;
    Xapian::Database db_;
    bool started_;
    Xapian::ValueIterator it_, end_;

/* No copying */
    RegexpPostingSource (const RegexpPostingSource &);
    RegexpPostingSource &operator= (const RegexpPostingSource &);

 public:
    RegexpPostingSource (Xapian::valueno slot, const std::string &regexp);
    ~RegexpPostingSource ();
    void init (const Xapian::Database &db);
    Xapian::doccount get_termfreq_min () const;
    Xapian::doccount get_termfreq_est () const;
    Xapian::doccount get_termfreq_max () const;
    Xapian::docid get_docid () const;
    bool at_end () const;
    void next (unused (double min_wt));
    void skip_to (Xapian::docid did, unused (double min_wt));
    bool check (Xapian::docid did, unused (double min_wt));
};


class RegexpFieldProcessor : public Xapian::FieldProcessor {
 protected:
    Xapian::valueno slot;
    std::string term_prefix;
    notmuch_field_flag_t options;
    Xapian::QueryParser &parser;
    notmuch_database_t *notmuch;

 public:
    RegexpFieldProcessor (std::string prefix, notmuch_field_flag_t options,
			  Xapian::QueryParser &parser_, notmuch_database_t *notmuch_);

    ~RegexpFieldProcessor () { };

    Xapian::Query operator()(const std::string & str);
};
#endif
#endif /* NOTMUCH_REGEXP_FIELDS_H */
