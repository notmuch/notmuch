#include "database-private.h"

#if HAVE_SFSEXP
#include "sexp.h"
#include "unicode-util.h"

/* _sexp is used for file scope symbols to avoid clashing with
 * definitions from sexp.h */

typedef enum {
    SEXP_FLAG_NONE	= 0,
    SEXP_FLAG_FIELD	= 1 << 0,
} _sexp_flag_t;

typedef struct  {
    const char *name;
    Xapian::Query::op xapian_op;
    Xapian::Query initial;
    _sexp_flag_t flags;
} _sexp_prefix_t;

static _sexp_prefix_t prefixes[] =
{
    { "and",            Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_NONE },
    { "not",            Xapian::Query::OP_AND_NOT,      Xapian::Query::MatchAll,
      SEXP_FLAG_NONE },
    { "or",             Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_NONE },
    { "subject",        Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD },
    { }
};

static notmuch_status_t _sexp_to_xapian_query (notmuch_database_t *notmuch,
					       const _sexp_prefix_t *parent,
					       const sexp_t *sx,
					       Xapian::Query &output);

static notmuch_status_t
_sexp_combine_query (notmuch_database_t *notmuch,
		     const _sexp_prefix_t *parent,
		     Xapian::Query::op operation,
		     Xapian::Query left,
		     const sexp_t *sx,
		     Xapian::Query &output)
{
    Xapian::Query subquery;

    notmuch_status_t status;

    /* if we run out elements, return accumulator */

    if (! sx) {
	output = left;
	return NOTMUCH_STATUS_SUCCESS;
    }

    status = _sexp_to_xapian_query (notmuch, parent, sx, subquery);
    if (status)
	return status;

    return _sexp_combine_query (notmuch,
				parent,
				operation,
				Xapian::Query (operation, left, subquery),
				sx->next, output);
}

static notmuch_status_t
_sexp_parse_phrase (std::string term_prefix, const char *phrase, Xapian::Query &output)
{
    Xapian::Utf8Iterator p (phrase);
    Xapian::Utf8Iterator end;
    std::vector<std::string> terms;

    while (p != end) {
	Xapian::Utf8Iterator start;
	while (p != end && ! Xapian::Unicode::is_wordchar (*p))
	    p++;

	if (p == end)
	    break;

	start = p;

	while (p != end && Xapian::Unicode::is_wordchar (*p))
	    p++;

	if (p != start) {
	    std::string word (start, p);
	    word = Xapian::Unicode::tolower (word);
	    terms.push_back (term_prefix + word);
	}
    }
    output = Xapian::Query (Xapian::Query::OP_PHRASE, terms.begin (), terms.end ());
    return NOTMUCH_STATUS_SUCCESS;
}

/* Here we expect the s-expression to be a proper list, with first
 * element defining and operation, or as a special case the empty
 * list */

static notmuch_status_t
_sexp_to_xapian_query (notmuch_database_t *notmuch, const _sexp_prefix_t *parent, const sexp_t *sx,
		       Xapian::Query &output)
{

    if (sx->ty == SEXP_VALUE) {
	std::string term = Xapian::Unicode::tolower (sx->val);
	Xapian::Stem stem = *(notmuch->stemmer);
	std::string term_prefix = parent ? _find_prefix (parent->name) : "";
	if (sx->aty == SEXP_BASIC && unicode_word_utf8 (sx->val)) {
	    output = Xapian::Query ("Z" + term_prefix + stem (term));
	    return NOTMUCH_STATUS_SUCCESS;
	} else {
	    return _sexp_parse_phrase (term_prefix, sx->val, output);
	}
    }

    /* Empty list */
    if (! sx->list) {
	output = Xapian::Query::MatchAll;
	return NOTMUCH_STATUS_SUCCESS;
    }

    if (sx->list->ty == SEXP_LIST) {
	_notmuch_database_log (notmuch, "unexpected list in field/operation position\n",
			       sx->list->val);
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    for (_sexp_prefix_t *prefix = prefixes; prefix && prefix->name; prefix++) {
	if (strcmp (prefix->name, sx->list->val) == 0) {
	    if (prefix->flags & SEXP_FLAG_FIELD) {
		if (parent) {
		    _notmuch_database_log (notmuch, "nested field: '%s' inside '%s'\n",
					   prefix->name, parent->name);
		    return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
		}
		parent = prefix;
	    }

	    return _sexp_combine_query (notmuch, parent, prefix->xapian_op, prefix->initial,
					sx->list->next, output);
	}
    }

    _notmuch_database_log (notmuch, "unknown prefix '%s'\n", sx->list->val);

    return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
}

notmuch_status_t
_notmuch_sexp_string_to_xapian_query (notmuch_database_t *notmuch, const char *querystr,
				      Xapian::Query &output)
{
    const sexp_t *sx = NULL;
    char *buf = talloc_strdup (notmuch, querystr);

    sx = parse_sexp (buf, strlen (querystr));
    if (! sx) {
	_notmuch_database_log (notmuch, "invalid s-expression: '%s'\n", querystr);
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    return _sexp_to_xapian_query (notmuch, NULL, sx, output);
}
#endif
