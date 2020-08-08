#include "database-private.h"
#include "query-fp.h"
#include "thread-fp.h"
#include "regexp-fields.h"
#include "parse-time-vrp.h"

typedef struct {
    const char *name;
    const char *prefix;
    notmuch_field_flag_t flags;
} prefix_t;

/* With these prefix values we follow the conventions published here:
 *
 * https://xapian.org/docs/omega/termprefixes.html
 *
 * as much as makes sense. Note that I took some liberty in matching
 * the reserved prefix values to notmuch concepts, (for example, 'G'
 * is documented as "newsGroup (or similar entity - e.g. a web forum
 * name)", for which I think the thread is the closest analogue in
 * notmuch. This in spite of the fact that we will eventually be
 * storing mailing-list messages where 'G' for "mailing list name"
 * might be even a closer analogue. I'm treating the single-character
 * prefixes preferentially for core notmuch concepts (which will be
 * nearly universal to all mail messages).
 */

static const
prefix_t prefix_table[] = {
    /* name			term prefix	flags */
    { "type",                   "T",            NOTMUCH_FIELD_NO_FLAGS },
    { "reference",              "XREFERENCE",   NOTMUCH_FIELD_NO_FLAGS },
    { "replyto",                "XREPLYTO",     NOTMUCH_FIELD_NO_FLAGS },
    { "directory",              "XDIRECTORY",   NOTMUCH_FIELD_NO_FLAGS },
    { "file-direntry",          "XFDIRENTRY",   NOTMUCH_FIELD_NO_FLAGS },
    { "directory-direntry",     "XDDIRENTRY",   NOTMUCH_FIELD_NO_FLAGS },
    { "body",                   "",             NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROBABILISTIC },
    { "thread",                 "G",            NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROCESSOR },
    { "tag",                    "K",            NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROCESSOR },
    { "is",                     "K",            NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROCESSOR },
    { "id",                     "Q",            NOTMUCH_FIELD_EXTERNAL },
    { "mid",                    "Q",            NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROCESSOR },
    { "path",                   "P",            NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROCESSOR },
    { "property",               "XPROPERTY",    NOTMUCH_FIELD_EXTERNAL },
    /*
     * Unconditionally add ':' to reduce potential ambiguity with
     * overlapping prefixes and/or terms that start with capital
     * letters. See Xapian document termprefixes.html for related
     * discussion.
     */
    { "folder",                 "XFOLDER:",     NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROCESSOR },
    { "date",                   NULL,           NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROCESSOR },
    { "query",                  NULL,           NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROCESSOR },
    { "from",                   "XFROM",        NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROBABILISTIC |
      NOTMUCH_FIELD_PROCESSOR },
    { "to",                     "XTO",          NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROBABILISTIC },
    { "attachment",             "XATTACHMENT",  NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROBABILISTIC },
    { "mimetype",               "XMIMETYPE",    NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROBABILISTIC },
    { "subject",                "XSUBJECT",     NOTMUCH_FIELD_EXTERNAL |
      NOTMUCH_FIELD_PROBABILISTIC |
      NOTMUCH_FIELD_PROCESSOR },
};

static const char *
_user_prefix (void *ctx, const char *name)
{
    return talloc_asprintf (ctx, "XU%s:", name);
}

const char *
_find_prefix (const char *name)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE (prefix_table); i++) {
	if (strcmp (name, prefix_table[i].name) == 0)
	    return prefix_table[i].prefix;
    }

    INTERNAL_ERROR ("No prefix exists for '%s'\n", name);

    return "";
}

/* Like find prefix, but include the possibility of user defined
 * prefixes specific to this database */

const char *
_notmuch_database_prefix (notmuch_database_t *notmuch, const char *name)
{
    unsigned int i;

    /*XXX TODO: reduce code duplication */
    for (i = 0; i < ARRAY_SIZE (prefix_table); i++) {
	if (strcmp (name, prefix_table[i].name) == 0)
	    return prefix_table[i].prefix;
    }

    if (notmuch->user_prefix)
	return _notmuch_string_map_get (notmuch->user_prefix, name);

    return NULL;
}

static void
_setup_query_field_default (const prefix_t *prefix, notmuch_database_t *notmuch)
{
    if (prefix->prefix)
	notmuch->query_parser->add_prefix ("", prefix->prefix);
    if (prefix->flags & NOTMUCH_FIELD_PROBABILISTIC)
	notmuch->query_parser->add_prefix (prefix->name, prefix->prefix);
    else
	notmuch->query_parser->add_boolean_prefix (prefix->name, prefix->prefix);
}

static void
_setup_query_field (const prefix_t *prefix, notmuch_database_t *notmuch)
{
    if (prefix->flags & NOTMUCH_FIELD_PROCESSOR) {
	Xapian::FieldProcessor *fp;

	if (STRNCMP_LITERAL (prefix->name, "date") == 0)
	    fp = (new DateFieldProcessor(NOTMUCH_VALUE_TIMESTAMP))->release ();
	else if (STRNCMP_LITERAL(prefix->name, "query") == 0)
	    fp = (new QueryFieldProcessor (*notmuch->query_parser, notmuch))->release ();
	else if (STRNCMP_LITERAL (prefix->name, "thread") == 0)
	    fp = (new ThreadFieldProcessor (*notmuch->query_parser, notmuch))->release ();
	else
	    fp = (new RegexpFieldProcessor (prefix->name, prefix->flags,
					    *notmuch->query_parser, notmuch))->release ();

	/* we treat all field-processor fields as boolean in order to get the raw input */
	if (prefix->prefix)
	    notmuch->query_parser->add_prefix ("", prefix->prefix);
	notmuch->query_parser->add_boolean_prefix (prefix->name, fp);
    } else {
	_setup_query_field_default (prefix, notmuch);
    }
}

notmuch_status_t
_notmuch_database_setup_standard_query_fields (notmuch_database_t *notmuch)
{
    for (unsigned int i = 0; i < ARRAY_SIZE (prefix_table); i++) {
	const prefix_t *prefix = &prefix_table[i];
	if (prefix->flags & NOTMUCH_FIELD_EXTERNAL) {
	    _setup_query_field (prefix, notmuch);
	}
    }
    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
_notmuch_database_setup_user_query_fields (notmuch_database_t *notmuch)
{
    notmuch_string_map_iterator_t *list;

    notmuch->user_prefix = _notmuch_string_map_create (notmuch);
    if (notmuch->user_prefix == NULL)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    notmuch->user_header = _notmuch_string_map_create (notmuch);
    if (notmuch->user_header == NULL)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    list = _notmuch_string_map_iterator_create (notmuch->config, CONFIG_HEADER_PREFIX, FALSE);
    if (! list)
	INTERNAL_ERROR ("unable to read headers from configuration");

    for (; _notmuch_string_map_iterator_valid (list);
	 _notmuch_string_map_iterator_move_to_next (list)) {

	prefix_t query_field;

	const char *key = _notmuch_string_map_iterator_key (list)
			  + sizeof (CONFIG_HEADER_PREFIX) - 1;

	_notmuch_string_map_append (notmuch->user_prefix,
				    key,
				    _user_prefix (notmuch, key));

	_notmuch_string_map_append (notmuch->user_header,
				    key,
				    _notmuch_string_map_iterator_value (list));

	query_field.name = talloc_strdup (notmuch, key);
	query_field.prefix = _user_prefix (notmuch, key);
	query_field.flags = NOTMUCH_FIELD_PROBABILISTIC
			    | NOTMUCH_FIELD_EXTERNAL;

	_setup_query_field_default (&query_field, notmuch);
    }

    _notmuch_string_map_iterator_destroy (list);

    return NOTMUCH_STATUS_SUCCESS;
}
