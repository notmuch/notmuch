#include "database-private.h"

#if HAVE_SFSEXP
#include "sexp.h"
#include "unicode-util.h"

/* _sexp is used for file scope symbols to avoid clashing with
 * definitions from sexp.h */

typedef enum {
    SEXP_FLAG_NONE	= 0,
    SEXP_FLAG_FIELD	= 1 << 0,
    SEXP_FLAG_BOOLEAN	= 1 << 1,
    SEXP_FLAG_SINGLE	= 1 << 2,
    SEXP_FLAG_WILDCARD	= 1 << 3,
    SEXP_FLAG_REGEX	= 1 << 4,
    SEXP_FLAG_DO_REGEX	= 1 << 5,
} _sexp_flag_t;

/*
 * define bitwise operators to hide casts */

inline _sexp_flag_t
operator| (_sexp_flag_t a, _sexp_flag_t b)
{
    return static_cast<_sexp_flag_t>(
	static_cast<unsigned>(a) | static_cast<unsigned>(b));
}

inline _sexp_flag_t
operator& (_sexp_flag_t a, _sexp_flag_t b)
{
    return static_cast<_sexp_flag_t>(
	static_cast<unsigned>(a) & static_cast<unsigned>(b));
}

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
    { "attachment",     Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD },
    { "body",           Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD },
    { "from",           Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "folder",         Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "id",             Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "is",             Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "mid",            Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "mimetype",       Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD },
    { "not",            Xapian::Query::OP_AND_NOT,      Xapian::Query::MatchAll,
      SEXP_FLAG_NONE },
    { "or",             Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_NONE },
    { "path",           Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "property",       Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "regex",          Xapian::Query::OP_INVALID,      Xapian::Query::MatchAll,
      SEXP_FLAG_SINGLE | SEXP_FLAG_DO_REGEX },
    { "rx",             Xapian::Query::OP_INVALID,      Xapian::Query::MatchAll,
      SEXP_FLAG_SINGLE | SEXP_FLAG_DO_REGEX },
    { "starts-with",    Xapian::Query::OP_WILDCARD,     Xapian::Query::MatchAll,
      SEXP_FLAG_SINGLE },
    { "subject",        Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "tag",            Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "thread",         Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "to",             Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD },
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

static notmuch_status_t
_sexp_parse_wildcard (notmuch_database_t *notmuch,
		      const _sexp_prefix_t *parent,
		      std::string match,
		      Xapian::Query &output)
{

    std::string term_prefix = parent ? _find_prefix (parent->name) : "";

    if (parent && ! (parent->flags & SEXP_FLAG_WILDCARD)) {
	_notmuch_database_log (notmuch, "'%s' does not support wildcard queries\n", parent->name);
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    output = Xapian::Query (Xapian::Query::OP_WILDCARD,
			    term_prefix + Xapian::Unicode::tolower (match));
    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_sexp_parse_one_term (notmuch_database_t *notmuch, std::string term_prefix, const sexp_t *sx,
		      Xapian::Query &output)
{
    Xapian::Stem stem = *(notmuch->stemmer);

    if (sx->aty == SEXP_BASIC && unicode_word_utf8 (sx->val)) {
	std::string term = Xapian::Unicode::tolower (sx->val);

	output = Xapian::Query ("Z" + term_prefix + stem (term));
	return NOTMUCH_STATUS_SUCCESS;
    } else {
	return _sexp_parse_phrase (term_prefix, sx->val, output);
    }

}

notmuch_status_t
_sexp_parse_regex (notmuch_database_t *notmuch,
		   const _sexp_prefix_t *prefix, const _sexp_prefix_t *parent,
		   std::string val, Xapian::Query &output)
{
    if (! parent) {
	_notmuch_database_log (notmuch, "illegal '%s' outside field\n",
			       prefix->name);
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    if (! (parent->flags & SEXP_FLAG_REGEX)) {
	_notmuch_database_log (notmuch, "'%s' not supported in field '%s'\n",
			       prefix->name, parent->name);
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    std::string msg; /* ignored */

    return _notmuch_regexp_to_query (notmuch, Xapian::BAD_VALUENO, parent->name,
				     val, output, msg);
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

	if (sx->aty == SEXP_BASIC && strcmp (sx->val, "*") == 0) {
	    return _sexp_parse_wildcard (notmuch, parent, "", output);
	}

	if (parent && (parent->flags & SEXP_FLAG_BOOLEAN)) {
	    output = Xapian::Query (term_prefix + sx->val);
	    return NOTMUCH_STATUS_SUCCESS;
	}
	if (parent) {
	    return _sexp_parse_one_term (notmuch, term_prefix, sx, output);
	} else {
	    Xapian::Query accumulator;
	    for (_sexp_prefix_t *prefix = prefixes; prefix->name; prefix++) {
		if (prefix->flags & SEXP_FLAG_FIELD) {
		    notmuch_status_t status;
		    Xapian::Query subquery;
		    term_prefix = _find_prefix (prefix->name);
		    status = _sexp_parse_one_term (notmuch, term_prefix, sx, subquery);
		    if (status)
			return status;
		    accumulator = Xapian::Query (Xapian::Query::OP_OR, accumulator, subquery);
		}
	    }
	    output = accumulator;
	    return NOTMUCH_STATUS_SUCCESS;
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

	    if ((prefix->flags & SEXP_FLAG_SINGLE) &&
		(! sx->list->next || sx->list->next->next || sx->list->next->ty != SEXP_VALUE)) {
		_notmuch_database_log (notmuch, "'%s' expects single atom as argument\n",
				       prefix->name);
		return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	    }

	    if (prefix->xapian_op == Xapian::Query::OP_WILDCARD)
		return _sexp_parse_wildcard (notmuch, parent, sx->list->next->val, output);

	    if (prefix->flags & SEXP_FLAG_DO_REGEX) {
		return _sexp_parse_regex (notmuch, prefix, parent, sx->list->next->val, output);
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
