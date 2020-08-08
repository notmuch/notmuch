#include <unistd.h>
#include "database-private.h"
#include "parse-time-vrp.h"

#if HAVE_XAPIAN_DB_RETRY_LOCK
#define DB_ACTION (Xapian::DB_CREATE_OR_OPEN | Xapian::DB_RETRY_LOCK)
#else
#define DB_ACTION Xapian::DB_CREATE_OR_OPEN
#endif

notmuch_status_t
notmuch_database_open (const char *path,
		       notmuch_database_mode_t mode,
		       notmuch_database_t **database)
{
    char *status_string = NULL;
    notmuch_status_t status;

    status = notmuch_database_open_verbose (path, mode, database,
					    &status_string);

    if (status_string) {
	fputs (status_string, stderr);
	free (status_string);
    }

    return status;
}

notmuch_status_t
notmuch_database_open_verbose (const char *path,
			       notmuch_database_mode_t mode,
			       notmuch_database_t **database,
			       char **status_string)
{
    return notmuch_database_open_with_config (path, mode, "", NULL,
					      database, status_string);
}

notmuch_status_t
notmuch_database_open_with_config (const char *database_path,
				   notmuch_database_mode_t mode,
				   const char *config_path,
				   unused(const char *profile),
				   notmuch_database_t **database,
				   char **status_string)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    void *local = talloc_new (NULL);
    notmuch_database_t *notmuch = NULL;
    char *notmuch_path, *xapian_path, *incompat_features;
    char *configured_database_path = NULL;
    char *message = NULL;
    struct stat st;
    int err;
    unsigned int version;
    GKeyFile *key_file = NULL;
    static int initialized = 0;

    /* XXX TODO: default locations for NULL case, handle profiles */
    if (config_path != NULL && ! EMPTY_STRING (config_path)) {
	key_file = g_key_file_new ();
	if (! g_key_file_load_from_file (key_file, config_path, G_KEY_FILE_NONE, NULL)) {
	    status = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}
	configured_database_path = g_key_file_get_value (key_file, "database", "path", NULL);
    }

    if (database_path == NULL)
	database_path = configured_database_path;

    if (database_path == NULL) {
	message = strdup ("Error: Cannot open a database for a NULL path.\n");
	status = NOTMUCH_STATUS_NULL_POINTER;
	goto DONE;
    }

    if (database_path[0] != '/') {
	message = strdup ("Error: Database path must be absolute.\n");
	status = NOTMUCH_STATUS_PATH_ERROR;
	goto DONE;
    }

    if (! (notmuch_path = talloc_asprintf (local, "%s/%s", database_path, ".notmuch"))) {
	message = strdup ("Out of memory\n");
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    err = stat (notmuch_path, &st);
    if (err) {
	IGNORE_RESULT (asprintf (&message, "Error opening database at %s: %s\n",
				 notmuch_path, strerror (errno)));
	status = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    if (! (xapian_path = talloc_asprintf (local, "%s/%s", notmuch_path, "xapian"))) {
	message = strdup ("Out of memory\n");
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    /* Initialize the GLib type system and threads */
#if ! GLIB_CHECK_VERSION (2, 35, 1)
    g_type_init ();
#endif

    /* Initialize gmime */
    if (! initialized) {
	g_mime_init ();
	initialized = 1;
    }

    notmuch = talloc_zero (NULL, notmuch_database_t);
    notmuch->exception_reported = false;
    notmuch->status_string = NULL;
    notmuch->path = talloc_strdup (notmuch, database_path);

    strip_trailing (notmuch->path, '/');

    notmuch->writable_xapian_db = NULL;
    notmuch->atomic_nesting = 0;
    notmuch->view = 1;
    try {
	std::string last_thread_id;
	std::string last_mod;

	if (mode == NOTMUCH_DATABASE_MODE_READ_WRITE) {
	    notmuch->writable_xapian_db = new Xapian::WritableDatabase (xapian_path,
									DB_ACTION);
	    notmuch->xapian_db = notmuch->writable_xapian_db;
	} else {
	    notmuch->xapian_db = new Xapian::Database (xapian_path);
	}

	/* Check version.  As of database version 3, we represent
	 * changes in terms of features, so assume a version bump
	 * means a dramatically incompatible change. */
	version = notmuch_database_get_version (notmuch);
	if (version > NOTMUCH_DATABASE_VERSION) {
	    IGNORE_RESULT (asprintf (&message,
				     "Error: Notmuch database at %s\n"
				     "       has a newer database format version (%u) than supported by this\n"
				     "       version of notmuch (%u).\n",
				     notmuch_path, version, NOTMUCH_DATABASE_VERSION));
	    notmuch_database_destroy (notmuch);
	    notmuch = NULL;
	    status = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}

	/* Check features. */
	incompat_features = NULL;
	notmuch->features = _notmuch_database_parse_features (
	    local, notmuch->xapian_db->get_metadata ("features").c_str (),
	    version, mode == NOTMUCH_DATABASE_MODE_READ_WRITE ? 'w' : 'r',
	    &incompat_features);
	if (incompat_features) {
	    IGNORE_RESULT (asprintf (&message,
				     "Error: Notmuch database at %s\n"
				     "       requires features (%s)\n"
				     "       not supported by this version of notmuch.\n",
				     notmuch_path, incompat_features));
	    notmuch_database_destroy (notmuch);
	    notmuch = NULL;
	    status = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}

	notmuch->last_doc_id = notmuch->xapian_db->get_lastdocid ();
	last_thread_id = notmuch->xapian_db->get_metadata ("last_thread_id");
	if (last_thread_id.empty ()) {
	    notmuch->last_thread_id = 0;
	} else {
	    const char *str;
	    char *end;

	    str = last_thread_id.c_str ();
	    notmuch->last_thread_id = strtoull (str, &end, 16);
	    if (*end != '\0')
		INTERNAL_ERROR ("Malformed database last_thread_id: %s", str);
	}

	/* Get current highest revision number. */
	last_mod = notmuch->xapian_db->get_value_upper_bound (
	    NOTMUCH_VALUE_LAST_MOD);
	if (last_mod.empty ())
	    notmuch->revision = 0;
	else
	    notmuch->revision = Xapian::sortable_unserialise (last_mod);
	notmuch->uuid = talloc_strdup (
	    notmuch, notmuch->xapian_db->get_uuid ().c_str ());

	notmuch->query_parser = new Xapian::QueryParser;
	notmuch->term_gen = new Xapian::TermGenerator;
	notmuch->term_gen->set_stemmer (Xapian::Stem ("english"));
	notmuch->value_range_processor = new Xapian::NumberRangeProcessor (NOTMUCH_VALUE_TIMESTAMP);
	notmuch->date_range_processor = new ParseTimeRangeProcessor (NOTMUCH_VALUE_TIMESTAMP, "date:");
	notmuch->last_mod_range_processor = new Xapian::NumberRangeProcessor (NOTMUCH_VALUE_LAST_MOD, "lastmod:");
	notmuch->query_parser->set_default_op (Xapian::Query::OP_AND);
	notmuch->query_parser->set_database (*notmuch->xapian_db);
	notmuch->query_parser->set_stemmer (Xapian::Stem ("english"));
	notmuch->query_parser->set_stemming_strategy (Xapian::QueryParser::STEM_SOME);
	notmuch->query_parser->add_rangeprocessor (notmuch->value_range_processor);
	notmuch->query_parser->add_rangeprocessor (notmuch->date_range_processor);
	notmuch->query_parser->add_rangeprocessor (notmuch->last_mod_range_processor);

	/* Configuration information is needed to set up query parser */
	status = _notmuch_config_load_from_database (notmuch);
	if (status)
	    goto DONE;

	if (key_file)
	    status = _notmuch_config_load_from_file (notmuch, key_file);
	if (status)
	    goto DONE;

	status = _notmuch_database_setup_standard_query_fields (notmuch);
	if (status)
	    goto DONE;

	status = _notmuch_database_setup_user_query_fields (notmuch);
	if (status)
	    goto DONE;

    } catch (const Xapian::Error &error) {
	IGNORE_RESULT (asprintf (&message, "A Xapian exception occurred opening database: %s\n",
				 error.get_msg ().c_str ()));
	notmuch_database_destroy (notmuch);
	notmuch = NULL;
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

  DONE:
    talloc_free (local);

    if (message) {
	if (status_string)
	    *status_string = message;
	else
	    free (message);
    }

    if (database)
	*database = notmuch;
    else
	talloc_free (notmuch);

    if (notmuch)
	notmuch->open = true;

    return status;
}
