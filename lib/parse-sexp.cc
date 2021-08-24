#include <xapian.h>
#include "notmuch-private.h"

#if HAVE_SFSEXP
#include "sexp.h"


/* _sexp is used for file scope symbols to avoid clashing with
 * definitions from sexp.h */

/* Here we expect the s-expression to be a proper list, with first
 * element defining and operation, or as a special case the empty
 * list */

static notmuch_status_t
_sexp_to_xapian_query (notmuch_database_t *notmuch, const sexp_t *sx,
		       Xapian::Query &output)
{

    if (sx->ty == SEXP_VALUE) {
	output = Xapian::Query (Xapian::Unicode::tolower (sx->val));
	return NOTMUCH_STATUS_SUCCESS;
    }

    /* Empty list */
    if (! sx->list) {
	output = Xapian::Query::MatchAll;
	return NOTMUCH_STATUS_SUCCESS;
    }

    if (sx->list->ty == SEXP_VALUE)
	_notmuch_database_log (notmuch, "unknown prefix '%s'\n", sx->list->val);
    else
	_notmuch_database_log (notmuch, "unexpected list in field/operation position\n",
			       sx->list->val);

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

    return _sexp_to_xapian_query (notmuch, sx, output);
}
#endif
