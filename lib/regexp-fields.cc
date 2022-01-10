/* regexp-fields.cc - field processor glue for regex supporting fields
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

#include "regexp-fields.h"
#include "notmuch-private.h"
#include "database-private.h"

notmuch_status_t
compile_regex (regex_t &regexp, const char *str, std::string &msg)
{
    int err = regcomp (&regexp, str, REG_EXTENDED | REG_NOSUB);

    if (err != 0) {
	size_t len = regerror (err, &regexp, NULL, 0);
	char *buffer = new char[len];
	msg = "Regexp error: ";
	(void) regerror (err, &regexp, buffer, len);
	msg.append (buffer, len);
	delete[] buffer;

	return NOTMUCH_STATUS_ILLEGAL_ARGUMENT;
    }
    return NOTMUCH_STATUS_SUCCESS;
}

RegexpPostingSource::RegexpPostingSource (Xapian::valueno slot, const std::string &regexp)
    : slot_ (slot)
{
    std::string msg;
    notmuch_status_t status = compile_regex (regexp_, regexp.c_str (), msg);

    if (status)
	throw Xapian::QueryParserError (msg);
}

RegexpPostingSource::~RegexpPostingSource ()
{
    regfree (&regexp_);
}

void
RegexpPostingSource::init (const Xapian::Database &db)
{
    db_ = db;
    it_ = db_.valuestream_begin (slot_);
    end_ = db.valuestream_end (slot_);
    started_ = false;
}

Xapian::doccount
RegexpPostingSource::get_termfreq_min () const
{
    return 0;
}

Xapian::doccount
RegexpPostingSource::get_termfreq_est () const
{
    return get_termfreq_max () / 2;
}

Xapian::doccount
RegexpPostingSource::get_termfreq_max () const
{
    return db_.get_value_freq (slot_);
}

Xapian::docid
RegexpPostingSource::get_docid () const
{
    return it_.get_docid ();
}

bool
RegexpPostingSource::at_end () const
{
    return it_ == end_;
}

void
RegexpPostingSource::next (unused (double min_wt))
{
    if (started_ && ! at_end ())
	++it_;
    started_ = true;

    for (; ! at_end (); ++it_) {
	std::string value = *it_;
	if (regexec (&regexp_, value.c_str (), 0, NULL, 0) == 0)
	    break;
    }
}

void
RegexpPostingSource::skip_to (Xapian::docid did, unused (double min_wt))
{
    started_ = true;
    it_.skip_to (did);
    for (; ! at_end (); ++it_) {
	std::string value = *it_;
	if (regexec (&regexp_, value.c_str (), 0, NULL, 0) == 0)
	    break;
    }
}

bool
RegexpPostingSource::check (Xapian::docid did, unused (double min_wt))
{
    started_ = true;
    if (! it_.check (did) || at_end ())
	return false;
    return (regexec (&regexp_, (*it_).c_str (), 0, NULL, 0) == 0);
}

static inline Xapian::valueno
_find_slot (std::string prefix)
{
    if (prefix == "from")
	return NOTMUCH_VALUE_FROM;
    else if (prefix == "subject")
	return NOTMUCH_VALUE_SUBJECT;
    else if (prefix == "mid")
	return NOTMUCH_VALUE_MESSAGE_ID;
    else
	return Xapian::BAD_VALUENO;
}

RegexpFieldProcessor::RegexpFieldProcessor (std::string field_,
					    notmuch_field_flag_t options_,
					    Xapian::QueryParser &parser_,
					    notmuch_database_t *notmuch_)
    : slot (_find_slot (field_)),
    field (field_),
    term_prefix (_find_prefix (field_.c_str ())),
    options (options_),
    parser (parser_),
    notmuch (notmuch_)
{
};

notmuch_status_t
_notmuch_regexp_to_query (notmuch_database_t *notmuch, Xapian::valueno slot, std::string field,
			  std::string regexp_str,
			  Xapian::Query &output, std::string &msg)
{
    regex_t regexp;
    notmuch_status_t status;

    status = compile_regex (regexp, regexp_str.c_str (), msg);
    if (status) {
	_notmuch_database_log_append (notmuch, "error compiling regex %s", msg.c_str ());
	return status;
    }

    if (slot == Xapian::BAD_VALUENO)
	slot = _find_slot (field);

    if (slot == Xapian::BAD_VALUENO) {
	std::string term_prefix = _find_prefix (field.c_str ());
	std::vector<std::string> terms;

	for (Xapian::TermIterator it = notmuch->xapian_db->allterms_begin (term_prefix);
	     it != notmuch->xapian_db->allterms_end (); ++it) {
	    if (regexec (&regexp, (*it).c_str () + term_prefix.size (),
			 0, NULL, 0) == 0)
		terms.push_back (*it);
	}
	output = Xapian::Query (Xapian::Query::OP_OR, terms.begin (), terms.end ());
    } else {
	RegexpPostingSource *postings = new RegexpPostingSource (slot, regexp_str);
	output = Xapian::Query (postings->release ());
    }
    return NOTMUCH_STATUS_SUCCESS;
}

Xapian::Query
RegexpFieldProcessor::operator() (const std::string & str)
{
    if (str.empty ()) {
	if (options & NOTMUCH_FIELD_PROBABILISTIC) {
	    return Xapian::Query (Xapian::Query::OP_AND_NOT,
				  Xapian::Query::MatchAll,
				  Xapian::Query (Xapian::Query::OP_WILDCARD, term_prefix));
	} else {
	    return Xapian::Query (term_prefix);
	}
    }

    if (str.at (0) == '/') {
	if (str.length () > 1 && str.at (str.size () - 1) == '/') {
	    Xapian::Query query;
	    std::string regexp_str = str.substr (1, str.size () - 2);
	    std::string msg;
	    notmuch_status_t status;

	    status = _notmuch_regexp_to_query (notmuch, slot, field, regexp_str, query, msg);
	    if (status)
		throw Xapian::QueryParserError (msg);
	    return query;
	} else {
	    throw Xapian::QueryParserError ("unmatched regex delimiter in '" + str + "'");
	}
    } else {
	if (options & NOTMUCH_FIELD_PROBABILISTIC) {
	    /* TODO replace this with a nicer API level triggering of
	     * phrase parsing, when possible */
	    std::string query_str;

	    if (*str.rbegin () != '*' || str.find (' ') != std::string::npos)
		query_str = '"' + str + '"';
	    else
		query_str = str;

	    return parser.parse_query (query_str, NOTMUCH_QUERY_PARSER_FLAGS, term_prefix);
	} else {
	    /* Boolean prefix */
	    std::string term = term_prefix + str;
	    return Xapian::Query (term);
	}
    }
}
