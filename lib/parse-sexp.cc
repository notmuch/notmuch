#include "database-private.h"

#if HAVE_SFSEXP
#include "sexp.h"


/* _sexp is used for file scope symbols to avoid clashing with
 * definitions from sexp.h */

typedef enum {
    SEXP_FLAG_NONE = 0,
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
	if (sx->aty == SEXP_BASIC)
	    term = "Z" + stem (term);

	output = Xapian::Query (term);
	return NOTMUCH_STATUS_SUCCESS;
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
