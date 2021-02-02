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

static const char *
_xdg_dir (void *ctx,
	  const char *xdg_root_variable,
	  const char *xdg_prefix,
	  const char *profile_name)
{
    const char *xdg_root = getenv (xdg_root_variable);

    if (! xdg_root) {
	const char *home = getenv ("HOME");

	if (! home) return NULL;

	xdg_root = talloc_asprintf (ctx,
				    "%s/%s",
				    home,
				    xdg_prefix);
    }

    if (! profile_name)
	profile_name = getenv ("NOTMUCH_PROFILE");

    if (! profile_name)
	profile_name = "default";

    return talloc_asprintf (ctx,
			    "%s/notmuch/%s",
			    xdg_root,
			    profile_name);
}

static notmuch_status_t
_choose_hook_dir (notmuch_database_t *notmuch,
		  const char *profile,
		  char **message)
{
    const char *config;
    const char *hook_dir;
    struct stat st;
    int err;

    hook_dir = notmuch_config_get (notmuch, NOTMUCH_CONFIG_HOOK_DIR);

    if (hook_dir)
	return NOTMUCH_STATUS_SUCCESS;

    config = _xdg_dir (notmuch, "XDG_CONFIG_HOME", ".config", profile);
    if (! config)
	return NOTMUCH_STATUS_PATH_ERROR;

    hook_dir = talloc_asprintf (notmuch, "%s/hooks", config);

    err = stat (hook_dir, &st);
    if (err) {
	if (errno == ENOENT) {
	    const char *database_path = notmuch_database_get_path (notmuch);
	    hook_dir = talloc_asprintf (notmuch, "%s/.notmuch/hooks", database_path);
	} else {
	    IGNORE_RESULT (asprintf (message, "Error: Cannot stat %s: %s.\n",
				     hook_dir, strerror (errno)));
	    return NOTMUCH_STATUS_FILE_ERROR;
	}
    }

    _notmuch_config_cache (notmuch, NOTMUCH_CONFIG_HOOK_DIR, hook_dir);

    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_load_key_file (const char *path,
		const char *profile,
		GKeyFile **key_file)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    void *local = talloc_new (NULL);

    if (path && EMPTY_STRING (path))
	goto DONE;

    if (! path)
	path = getenv ("NOTMUCH_CONFIG");

    if (! path) {
	const char *dir = _xdg_dir (local, "XDG_CONFIG_HOME", ".config", profile);

	if (dir) {
	    path = talloc_asprintf (local, "%s/config", dir);
	    if (access (path, R_OK) != 0)
		path = NULL;
	}
    }

    if (! path) {
	const char *home = getenv ("HOME");

	path = talloc_asprintf (local, "%s/.notmuch-config", home);

	if (! profile)
	    profile = getenv ("NOTMUCH_PROFILE");

	if (profile)
	    path = talloc_asprintf (local, "%s.%s", path, profile);
    }

    *key_file = g_key_file_new ();
    if (! g_key_file_load_from_file (*key_file, path, G_KEY_FILE_NONE, NULL)) {
	status = NOTMUCH_STATUS_NO_CONFIG;
    }

  DONE:
    talloc_free (local);
    return status;
}

static notmuch_status_t
_choose_database_path (void *ctx,
		       const char *config_path,
		       const char *profile,
		       GKeyFile **key_file,
		       const char **database_path,
		       char **message)
{
    notmuch_status_t status;

    status = _load_key_file (config_path, profile, key_file);
    if (status) {
	*message = strdup ("Error: cannot load config file.\n");
	return status;
    }

    if (! *database_path) {
	*database_path = getenv ("NOTMUCH_DATABASE");
    }

    if (! *database_path && *key_file) {
	char *path = g_key_file_get_value (*key_file, "database", "path", NULL);
	if (path) {
	    *database_path = talloc_strdup (ctx, path);
	    g_free (path);
	}
    }

    if (*database_path == NULL) {
	*message = strdup ("Error: Cannot open a database for a NULL path.\n");
	return NOTMUCH_STATUS_NULL_POINTER;
    }

    if (*database_path[0] != '/') {
	*message = strdup ("Error: Database path must be absolute.\n");
	return NOTMUCH_STATUS_PATH_ERROR;
    }
    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_database_t *
_alloc_notmuch ()
{
    notmuch_database_t *notmuch;

    notmuch = talloc_zero (NULL, notmuch_database_t);
    if (! notmuch)
	return NULL;

    notmuch->exception_reported = false;
    notmuch->status_string = NULL;
    notmuch->writable_xapian_db = NULL;
    notmuch->atomic_nesting = 0;
    notmuch->view = 1;
    return notmuch;
}

static void
_set_database_path (notmuch_database_t *notmuch,
		    const char *database_path)
{
    char *path = talloc_strdup (notmuch, database_path);

    strip_trailing (path, '/');

    _notmuch_config_cache (notmuch, NOTMUCH_CONFIG_DATABASE_PATH, path);
}

notmuch_status_t
notmuch_database_open_with_config (const char *database_path,
				   notmuch_database_mode_t mode,
				   const char *config_path,
				   const char *profile,
				   notmuch_database_t **database,
				   char **status_string)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    void *local = talloc_new (NULL);
    notmuch_database_t *notmuch = NULL;
    char *notmuch_path, *incompat_features;
    char *message = NULL;
    struct stat st;
    int err;
    unsigned int version;
    GKeyFile *key_file = NULL;
    static int initialized = 0;

    notmuch = _alloc_notmuch ();
    if (! notmuch) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    if ((status = _choose_database_path (local, config_path, profile,
					 &key_file, &database_path, &message)))
	goto DONE;

    _set_database_path (notmuch, database_path);

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

    if (! (notmuch->xapian_path = talloc_asprintf (notmuch, "%s/%s", notmuch_path, "xapian"))) {
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

    try {
	std::string last_thread_id;
	std::string last_mod;

	if (mode == NOTMUCH_DATABASE_MODE_READ_WRITE) {
	    notmuch->writable_xapian_db = new Xapian::WritableDatabase (notmuch->xapian_path,
									DB_ACTION);
	    notmuch->xapian_db = notmuch->writable_xapian_db;
	} else {
	    notmuch->xapian_db = new Xapian::Database (notmuch->xapian_path);
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
	notmuch->date_range_processor = new ParseTimeRangeProcessor (NOTMUCH_VALUE_TIMESTAMP,
								     "date:");
	notmuch->last_mod_range_processor = new Xapian::NumberRangeProcessor (NOTMUCH_VALUE_LAST_MOD,
									      "lastmod:");
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

	status = _choose_hook_dir (notmuch, profile, &message);
	if (status)
	    goto DONE;

	status = _notmuch_config_load_defaults (notmuch);
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

    if (key_file)
	g_key_file_free (key_file);

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

notmuch_status_t
notmuch_database_create (const char *path, notmuch_database_t **database)
{
    char *status_string = NULL;
    notmuch_status_t status;

    status = notmuch_database_create_verbose (path, database,
					      &status_string);

    if (status_string) {
	fputs (status_string, stderr);
	free (status_string);
    }

    return status;
}

notmuch_status_t
notmuch_database_create_verbose (const char *path,
				 notmuch_database_t **database,
				 char **status_string)
{
    return notmuch_database_create_with_config (path, "", NULL, database, status_string);
}

notmuch_status_t
notmuch_database_create_with_config (const char *database_path,
				     const char *config_path,
				     const char *profile,
				     notmuch_database_t **database,
				     char **status_string)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    notmuch_database_t *notmuch = NULL;
    char *notmuch_path = NULL;
    char *message = NULL;
    GKeyFile *key_file = NULL;
    struct stat st;
    int err;
    void *local = talloc_new (NULL);

    if ((status = _choose_database_path (local, config_path, profile,
					 &key_file, &database_path, &message)))
	goto DONE;

    err = stat (database_path, &st);
    if (err) {
	IGNORE_RESULT (asprintf (&message, "Error: Cannot create database at %s: %s.\n",
				 database_path, strerror (errno)));
	status = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    if (! S_ISDIR (st.st_mode)) {
	IGNORE_RESULT (asprintf (&message, "Error: Cannot create database at %s: "
				 "Not a directory.\n",
				 database_path));
	status = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    notmuch_path = talloc_asprintf (local, "%s/%s", database_path, ".notmuch");

    err = mkdir (notmuch_path, 0755);
    if (err) {
	if (errno == EEXIST) {
	    status = NOTMUCH_STATUS_DATABASE_EXISTS;
	} else {
	    IGNORE_RESULT (asprintf (&message, "Error: Cannot create directory %s: %s.\n",
				     notmuch_path, strerror (errno)));
	    status = NOTMUCH_STATUS_FILE_ERROR;
	}
	goto DONE;
    }

    /* XXX this reads the config file twice, which is a bit wasteful */
    status = notmuch_database_open_with_config (database_path,
						NOTMUCH_DATABASE_MODE_READ_WRITE,
						config_path,
						profile,
						&notmuch, &message);
    if (status)
	goto DONE;

    /* Upgrade doesn't add these feature to existing databases, but
     * new databases have them. */
    notmuch->features |= NOTMUCH_FEATURE_FROM_SUBJECT_ID_VALUES;
    notmuch->features |= NOTMUCH_FEATURE_INDEXED_MIMETYPES;
    notmuch->features |= NOTMUCH_FEATURE_UNPREFIX_BODY_ONLY;

    status = notmuch_database_upgrade (notmuch, NULL, NULL);
    if (status) {
	notmuch_database_close (notmuch);
	notmuch = NULL;
    }

  DONE:
    talloc_free (local);

    if (key_file)
	g_key_file_free (key_file);

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
    return status;
}

notmuch_status_t
notmuch_database_reopen (notmuch_database_t *notmuch,
			 notmuch_database_mode_t new_mode)
{
    notmuch_database_mode_t cur_mode = _notmuch_database_mode (notmuch);

    if (notmuch->xapian_db == NULL) {
	_notmuch_database_log (notmuch, "Cannot reopen closed or nonexistent database\n");
	return NOTMUCH_STATUS_ILLEGAL_ARGUMENT;
    }

    try {
	if (cur_mode == new_mode &&
	    new_mode == NOTMUCH_DATABASE_MODE_READ_ONLY) {
	    notmuch->xapian_db->reopen ();
	} else {
	    notmuch->xapian_db->close ();

	    delete notmuch->xapian_db;
	    notmuch->xapian_db = NULL;
	    /* no need to free the same object twice */
	    notmuch->writable_xapian_db = NULL;

	    if (new_mode == NOTMUCH_DATABASE_MODE_READ_WRITE) {
		notmuch->writable_xapian_db = new Xapian::WritableDatabase (notmuch->xapian_path,
									    DB_ACTION);
		notmuch->xapian_db = notmuch->writable_xapian_db;
	    } else {
		notmuch->xapian_db = new Xapian::Database (notmuch->xapian_path,
							   DB_ACTION);
	    }
	}
    } catch (const Xapian::Error &error) {
	if (! notmuch->exception_reported) {
	    _notmuch_database_log (notmuch, "Error: A Xapian exception reopening database: %s\n",
				   error.get_msg ().c_str ());
	    notmuch->exception_reported = true;
	}
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    notmuch->view++;
    notmuch->open = true;
    return NOTMUCH_STATUS_SUCCESS;
}
