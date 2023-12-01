#include "database-private.h"

static const struct {
    /* NOTMUCH_FEATURE_* value. */
    _notmuch_features value;
    /* Feature name as it appears in the database.  This name should
     * be appropriate for displaying to the user if an older version
     * of notmuch doesn't support this feature. */
    const char *name;
    /* Compatibility flags when this feature is declared. */
    const char *flags;
} feature_names[] = {
    { NOTMUCH_FEATURE_FILE_TERMS,
      "multiple paths per message", "rw" },
    { NOTMUCH_FEATURE_DIRECTORY_DOCS,
      "relative directory paths", "rw" },
    /* Header values are not required for reading a database because a
     * reader can just refer to the message file. */
    { NOTMUCH_FEATURE_FROM_SUBJECT_ID_VALUES,
      "from/subject/message-ID in database", "w" },
    { NOTMUCH_FEATURE_BOOL_FOLDER,
      "exact folder:/path: search", "rw" },
    { NOTMUCH_FEATURE_GHOSTS,
      "mail documents for missing messages", "w" },
    /* Knowledge of the index mime-types are not required for reading
     * a database because a reader will just be unable to query
     * them. */
    { NOTMUCH_FEATURE_INDEXED_MIMETYPES,
      "indexed MIME types", "w" },
    { NOTMUCH_FEATURE_LAST_MOD,
      "modification tracking", "w" },
    /* Existing databases will work fine for all queries not involving
     * 'body:' */
    { NOTMUCH_FEATURE_UNPREFIX_BODY_ONLY,
      "index body and headers separately", "w" },
};

char *
_notmuch_database_print_features (const void *ctx, unsigned int features)
{
    unsigned int i;
    char *res = talloc_strdup (ctx, "");

    for (i = 0; i < ARRAY_SIZE (feature_names); ++i)
	if (features & feature_names[i].value)
	    res = talloc_asprintf_append_buffer (
		res, "%s\t%s\n", feature_names[i].name, feature_names[i].flags);

    return res;
}


/* Parse a database features string from the given database version.
 * Returns the feature bit set.
 *
 * For version < 3, this ignores the features string and returns a
 * hard-coded set of features.
 *
 * If there are unrecognized features that are required to open the
 * database in mode (which should be 'r' or 'w'), return a
 * comma-separated list of unrecognized but required features in
 * *incompat_out suitable for presenting to the user.  *incompat_out
 * will be allocated from ctx.
 */
_notmuch_features
_notmuch_database_parse_features (const void *ctx, const char *features, unsigned int version,
				  char mode, char **incompat_out)
{
    _notmuch_features res = static_cast<_notmuch_features>(0);
    unsigned int namelen, i;
    size_t llen = 0;
    const char *flags;

    /* Prior to database version 3, features were implied by the
     * version number. */
    if (version == 0)
	return NOTMUCH_FEATURES_V0;
    else if (version == 1)
	return NOTMUCH_FEATURES_V1;
    else if (version == 2)
	return NOTMUCH_FEATURES_V2;

    /* Parse the features string */
    while ((features = strtok_len_c (features + llen, "\n", &llen)) != NULL) {
	flags = strchr (features, '\t');
	if (! flags || flags > features + llen)
	    continue;
	namelen = flags - features;

	for (i = 0; i < ARRAY_SIZE (feature_names); ++i) {
	    if (strlen (feature_names[i].name) == namelen &&
		strncmp (feature_names[i].name, features, namelen) == 0) {
		res |= feature_names[i].value;
		break;
	    }
	}

	if (i == ARRAY_SIZE (feature_names) && incompat_out) {
	    /* Unrecognized feature */
	    const char *have = strchr (flags, mode);
	    if (have && have < features + llen) {
		/* This feature is required to access this database in
		 * 'mode', but we don't understand it. */
		if (! *incompat_out)
		    *incompat_out = talloc_strdup (ctx, "");
		*incompat_out = talloc_asprintf_append_buffer (
		    *incompat_out, "%s%.*s", **incompat_out ? ", " : "",
		    namelen, features);
	    }
	}
    }

    return res;
}
