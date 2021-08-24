#include "database-private.h"

#if HAVE_SFSEXP
#include "sexp.h"
#include "unicode-util.h"

/* _sexp is used for file scope symbols to avoid clashing with
 * definitions from sexp.h */

/* sexp_binding structs attach name to a sexp and a defining
 * context. The latter allows lazy evaluation of parameters whose
 * definition contains other parameters.  Lazy evaluation is needed
 * because a primary goal of macros is to change the parent field for
 * a sexp.
 */

typedef struct sexp_binding {
    const char *name;
    const sexp_t *sx;
    const struct sexp_binding *context;
    const struct sexp_binding *next;
} _sexp_binding_t;

typedef enum {
    SEXP_FLAG_NONE	= 0,
    SEXP_FLAG_FIELD	= 1 << 0,
    SEXP_FLAG_BOOLEAN	= 1 << 1,
    SEXP_FLAG_SINGLE	= 1 << 2,
    SEXP_FLAG_WILDCARD	= 1 << 3,
    SEXP_FLAG_REGEX	= 1 << 4,
    SEXP_FLAG_DO_REGEX	= 1 << 5,
    SEXP_FLAG_EXPAND	= 1 << 6,
    SEXP_FLAG_DO_EXPAND = 1 << 7,
    SEXP_FLAG_ORPHAN	= 1 << 8,
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
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD | SEXP_FLAG_EXPAND },
    { "body",           Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD },
    { "from",           Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX | SEXP_FLAG_EXPAND },
    { "folder",         Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX | SEXP_FLAG_EXPAND },
    { "id",             Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "infix",          Xapian::Query::OP_INVALID,      Xapian::Query::MatchAll,
      SEXP_FLAG_SINGLE | SEXP_FLAG_ORPHAN },
    { "is",             Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX | SEXP_FLAG_EXPAND },
    { "matching",       Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_DO_EXPAND },
    { "mid",            Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "mimetype",       Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD | SEXP_FLAG_EXPAND },
    { "not",            Xapian::Query::OP_AND_NOT,      Xapian::Query::MatchAll,
      SEXP_FLAG_NONE },
    { "of",             Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_DO_EXPAND },
    { "or",             Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_NONE },
    { "path",           Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX },
    { "property",       Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX | SEXP_FLAG_EXPAND },
    { "query",          Xapian::Query::OP_INVALID,      Xapian::Query::MatchNothing,
      SEXP_FLAG_SINGLE | SEXP_FLAG_ORPHAN },
    { "regex",          Xapian::Query::OP_INVALID,      Xapian::Query::MatchAll,
      SEXP_FLAG_SINGLE | SEXP_FLAG_DO_REGEX },
    { "rx",             Xapian::Query::OP_INVALID,      Xapian::Query::MatchAll,
      SEXP_FLAG_SINGLE | SEXP_FLAG_DO_REGEX },
    { "starts-with",    Xapian::Query::OP_WILDCARD,     Xapian::Query::MatchAll,
      SEXP_FLAG_SINGLE },
    { "subject",        Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX | SEXP_FLAG_EXPAND },
    { "tag",            Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX | SEXP_FLAG_EXPAND },
    { "thread",         Xapian::Query::OP_OR,           Xapian::Query::MatchNothing,
      SEXP_FLAG_FIELD | SEXP_FLAG_BOOLEAN | SEXP_FLAG_WILDCARD | SEXP_FLAG_REGEX | SEXP_FLAG_EXPAND },
    { "to",             Xapian::Query::OP_AND,          Xapian::Query::MatchAll,
      SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD | SEXP_FLAG_EXPAND },
    { }
};

static notmuch_status_t _sexp_to_xapian_query (notmuch_database_t *notmuch,
					       const _sexp_prefix_t *parent,
					       const _sexp_binding_t *env,
					       const sexp_t *sx,
					       Xapian::Query &output);

static notmuch_status_t
_sexp_combine_query (notmuch_database_t *notmuch,
		     const _sexp_prefix_t *parent,
		     const _sexp_binding_t *env,
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

    status = _sexp_to_xapian_query (notmuch, parent, env, sx, subquery);
    if (status)
	return status;

    return _sexp_combine_query (notmuch,
				parent,
				env,
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
		      unused(const _sexp_binding_t *env),
		      std::string match,
		      Xapian::Query &output)
{

    std::string term_prefix = parent ? _notmuch_database_prefix (notmuch, parent->name) : "";

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
		   unused(const _sexp_binding_t *env),
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


static notmuch_status_t
_sexp_expand_query (notmuch_database_t *notmuch,
		    const _sexp_prefix_t *prefix, const _sexp_prefix_t *parent,
		    unused(const _sexp_binding_t *env), const sexp_t *sx, Xapian::Query &output)
{
    Xapian::Query subquery;
    notmuch_status_t status;
    std::string msg;

    if (! (parent->flags & SEXP_FLAG_EXPAND)) {
	_notmuch_database_log (notmuch, "'%s' unsupported inside '%s'\n", prefix->name, parent->name);
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    status = _sexp_combine_query (notmuch, NULL, NULL, prefix->xapian_op, prefix->initial, sx,
				  subquery);
    if (status)
	return status;

    status = _notmuch_query_expand (notmuch, parent->name, subquery, output, msg);
    if (status) {
	_notmuch_database_log (notmuch, "error expanding query %s\n", msg.c_str ());
    }
    return status;
}

static notmuch_status_t
_sexp_parse_infix (notmuch_database_t *notmuch, const sexp_t *sx, Xapian::Query &output)
{
    try {
	output = notmuch->query_parser->parse_query (sx->val, NOTMUCH_QUERY_PARSER_FLAGS);
    } catch (const Xapian::QueryParserError &error) {
	_notmuch_database_log (notmuch, "Syntax error in infix query: %s\n", sx->val);
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    } catch (const Xapian::Error &error) {
	if (! notmuch->exception_reported) {
	    _notmuch_database_log (notmuch,
				   "A Xapian exception occurred parsing query: %s\n",
				   error.get_msg ().c_str ());
	    _notmuch_database_log_append (notmuch,
					  "Query string was: %s\n",
					  sx->val);
	    notmuch->exception_reported = true;
	    return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	}
    }
    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_sexp_parse_header (notmuch_database_t *notmuch, const _sexp_prefix_t *parent,
		    const _sexp_binding_t *env, const sexp_t *sx, Xapian::Query &output)
{
    _sexp_prefix_t user_prefix;

    user_prefix.name = sx->list->val;
    user_prefix.flags = SEXP_FLAG_FIELD | SEXP_FLAG_WILDCARD;

    if (parent) {
	_notmuch_database_log (notmuch, "nested field: '%s' inside '%s'\n",
			       sx->list->val, parent->name);
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    parent = &user_prefix;

    return _sexp_combine_query (notmuch, parent, env, Xapian::Query::OP_AND, Xapian::Query::MatchAll,
				sx->list->next, output);
}

static _sexp_binding_t *
_sexp_bind (void *ctx, const _sexp_binding_t *env, const char *name, const sexp_t *sx, const
	    _sexp_binding_t *context)
{
    _sexp_binding_t *binding = talloc (ctx, _sexp_binding_t);

    binding->name = talloc_strdup (ctx, name);
    binding->sx = sx;
    binding->context = context;
    binding->next = env;
    return binding;
}

static notmuch_status_t
maybe_apply_macro (notmuch_database_t *notmuch, const _sexp_prefix_t *parent,
		   const _sexp_binding_t *env, const sexp_t *sx, const sexp_t *args,
		   Xapian::Query &output)
{
    const sexp_t *params, *param, *arg, *body;
    void *local = talloc_new (notmuch);
    _sexp_binding_t *new_env = NULL;
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    if (sx->list->ty != SEXP_VALUE || strcmp (sx->list->val, "macro") != 0) {
	status = NOTMUCH_STATUS_IGNORED;
	goto DONE;
    }

    params = sx->list->next;

    if (! params || (params->ty != SEXP_LIST)) {
	_notmuch_database_log (notmuch, "missing (possibly empty) list of arguments to macro\n");
	return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
    }

    body = params->next;

    if (! body) {
	_notmuch_database_log (notmuch, "missing body of macro\n");
	status = NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	goto DONE;
    }

    for (param = params->list, arg = args;
	 param && arg;
	 param = param->next, arg = arg->next) {
	if (param->ty != SEXP_VALUE || param->aty != SEXP_BASIC) {
	    _notmuch_database_log (notmuch, "macro parameters must be unquoted atoms\n");
	    status = NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	    goto DONE;
	}
	new_env = _sexp_bind (local, new_env, param->val, arg, env);
    }

    if (param && ! arg) {
	_notmuch_database_log (notmuch, "too few arguments to macro\n");
	status = NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	goto DONE;
    }

    if (! param && arg) {
	_notmuch_database_log (notmuch, "too many arguments to macro\n");
	status = NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	goto DONE;
    }

    status = _sexp_to_xapian_query (notmuch, parent, new_env, body, output);

  DONE:
    if (local)
	talloc_free (local);

    return status;
}

static notmuch_status_t
maybe_saved_squery (notmuch_database_t *notmuch, const _sexp_prefix_t *parent,
		    const _sexp_binding_t *env, const sexp_t *sx, Xapian::Query &output)
{
    char *key;
    char *expansion = NULL;
    notmuch_status_t status;
    sexp_t *saved_sexp;
    void *local = talloc_new (notmuch);
    char *buf;

    key = talloc_asprintf (local, "squery.%s", sx->list->val);
    if (! key) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    status = notmuch_database_get_config (notmuch, key, &expansion);
    if (status)
	goto DONE;
    if (EMPTY_STRING (expansion)) {
	status = NOTMUCH_STATUS_IGNORED;
	goto DONE;
    }

    buf = talloc_strdup (local, expansion);
    /* XXX TODO: free this memory */
    saved_sexp = parse_sexp (buf, strlen (expansion));
    if (! saved_sexp) {
	_notmuch_database_log (notmuch, "invalid saved s-expression query: '%s'\n", expansion);
	status = NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	goto DONE;
    }

    status = maybe_apply_macro (notmuch, parent, env, saved_sexp, sx->list->next, output);
    if (status == NOTMUCH_STATUS_IGNORED)
	status =  _sexp_to_xapian_query (notmuch, parent, env, saved_sexp, output);

  DONE:
    if (local)
	talloc_free (local);

    return status;
}

static notmuch_status_t
_sexp_expand_param (notmuch_database_t *notmuch, const _sexp_prefix_t *parent,
		    const _sexp_binding_t *env, const char *name,
		    Xapian::Query &output)
{
    for (; env; env = env->next) {
	if (strcmp (name, env->name) == 0) {
	    return _sexp_to_xapian_query (notmuch, parent, env->context, env->sx,
					  output);
	}
    }
    _notmuch_database_log (notmuch, "undefined parameter %s\n", name);
    return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
}

/* Here we expect the s-expression to be a proper list, with first
 * element defining and operation, or as a special case the empty
 * list */

static notmuch_status_t
_sexp_to_xapian_query (notmuch_database_t *notmuch, const _sexp_prefix_t *parent,
		       const _sexp_binding_t *env, const sexp_t *sx, Xapian::Query &output)
{
    notmuch_status_t status;

    if (sx->ty == SEXP_VALUE && sx->aty == SEXP_BASIC && sx->val[0] == ',') {
	return _sexp_expand_param (notmuch, parent, env, sx->val + 1, output);
    }

    if (sx->ty == SEXP_VALUE) {
	std::string term_prefix = parent ? _notmuch_database_prefix (notmuch, parent->name) : "";

	if (sx->aty == SEXP_BASIC && strcmp (sx->val, "*") == 0) {
	    return _sexp_parse_wildcard (notmuch, parent, env, "", output);
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
		    Xapian::Query subquery;
		    term_prefix = _notmuch_database_prefix (notmuch, prefix->name);
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

    status = maybe_saved_squery (notmuch, parent, env, sx, output);
    if (status != NOTMUCH_STATUS_IGNORED)
	return status;

    /* Check for user defined field */
    if (_notmuch_string_map_get (notmuch->user_prefix, sx->list->val)) {
	return _sexp_parse_header (notmuch, parent, env, sx, output);
    }

    if (strcmp (sx->list->val, "macro") == 0) {
	_notmuch_database_log (notmuch, "macro definition not permitted here\n");
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

	    if (parent && (prefix->flags & SEXP_FLAG_ORPHAN)) {
		_notmuch_database_log (notmuch, "'%s' not supported inside '%s'\n",
				       prefix->name, parent->name);
		return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	    }

	    if ((prefix->flags & SEXP_FLAG_SINGLE) &&
		(! sx->list->next || sx->list->next->next || sx->list->next->ty != SEXP_VALUE)) {
		_notmuch_database_log (notmuch, "'%s' expects single atom as argument\n",
				       prefix->name);
		return NOTMUCH_STATUS_BAD_QUERY_SYNTAX;
	    }

	    if (strcmp (prefix->name, "infix") == 0) {
		return _sexp_parse_infix (notmuch, sx->list->next, output);
	    }

	    if (strcmp (prefix->name, "query") == 0) {
		return _notmuch_query_name_to_query (notmuch, sx->list->next->val, output);
	    }

	    if (prefix->xapian_op == Xapian::Query::OP_WILDCARD)
		return _sexp_parse_wildcard (notmuch, parent, env, sx->list->next->val, output);

	    if (prefix->flags & SEXP_FLAG_DO_REGEX) {
		return _sexp_parse_regex (notmuch, prefix, parent, env, sx->list->next->val, output);
	    }

	    if (prefix->flags & SEXP_FLAG_DO_EXPAND) {
		return _sexp_expand_query (notmuch, prefix, parent, env, sx->list->next, output);
	    }

	    return _sexp_combine_query (notmuch, parent, env, prefix->xapian_op, prefix->initial,
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

    return _sexp_to_xapian_query (notmuch, NULL, NULL, sx, output);
}
#endif
