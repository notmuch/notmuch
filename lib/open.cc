#include <unistd.h>
#include <libgen.h>

#include "database-private.h"
#include "parse-time-vrp.h"
#include "path-util.h"

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

    status = notmuch_database_open_with_config (path, mode, "", NULL,
						database, &status_string);
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
_choose_dir (notmuch_database_t *notmuch,
	     const char *profile,
	     notmuch_config_key_t key,
	     const char *xdg_var,
	     const char *xdg_subdir,
	     const char *subdir,
	     char **message = NULL)
{
    const char *parent;
    const char *dir;
    struct stat st;
    int err;

    dir = notmuch_config_get (notmuch, key);

    if (dir)
	return NOTMUCH_STATUS_SUCCESS;

    parent = _xdg_dir (notmuch, xdg_var, xdg_subdir, profile);
    if (! parent)
	return NOTMUCH_STATUS_PATH_ERROR;

    dir = talloc_asprintf (notmuch, "%s/%s", parent, subdir);

    err = stat (dir, &st);
    if (err) {
	if (errno == ENOENT) {
	    char *notmuch_path = dirname (talloc_strdup (notmuch, notmuch->xapian_path));
	    dir = talloc_asprintf (notmuch, "%s/%s", notmuch_path, subdir);
	} else {
	    IGNORE_RESULT (asprintf (message, "Error: Cannot stat %s: %s.\n",
				     dir, strerror (errno)));
	    return NOTMUCH_STATUS_FILE_ERROR;
	}
    }

    _notmuch_config_cache (notmuch, key, dir);

    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_load_key_file (notmuch_database_t *notmuch,
		const char *path,
		const char *profile,
		GKeyFile **key_file)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    if (path && EMPTY_STRING (path))
	goto DONE;

    if (! path)
	path = getenv ("NOTMUCH_CONFIG");

    if (path)
	path = talloc_strdup (notmuch, path);
    else {
	const char *dir = _xdg_dir (notmuch, "XDG_CONFIG_HOME", ".config", profile);

	if (dir) {
	    path = talloc_asprintf (notmuch, "%s/config", dir);
	    if (access (path, R_OK) != 0)
		path = NULL;
	}
    }

    if (! path) {
	const char *home = getenv ("HOME");

	path = talloc_asprintf (notmuch, "%s/.notmuch-config", home);

	if (! profile)
	    profile = getenv ("NOTMUCH_PROFILE");

	if (profile)
	    path = talloc_asprintf (notmuch, "%s.%s", path, profile);
    }

    *key_file = g_key_file_new ();
    if (! g_key_file_load_from_file (*key_file, path, G_KEY_FILE_NONE, NULL)) {
	status = NOTMUCH_STATUS_NO_CONFIG;
    }

  DONE:
    if (path)
	notmuch->config_path = path;

    return status;
}

static notmuch_status_t
_db_dir_exists (const char *database_path, char **message)
{
    struct stat st;
    int err;

    err = stat (database_path, &st);
    if (err) {
	IGNORE_RESULT (asprintf (message, "Error: Cannot open database at %s: %s.\n",
				 database_path, strerror (errno)));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    if (! S_ISDIR (st.st_mode)) {
	IGNORE_RESULT (asprintf (message, "Error: Cannot open database at %s: "
				 "Not a directory.\n",
				 database_path));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_choose_database_path (notmuch_database_t *notmuch,
		       const char *profile,
		       GKeyFile *key_file,
		       const char **database_path,
		       char **message)
{
    if (! *database_path) {
	*database_path = getenv ("NOTMUCH_DATABASE");
    }

    if (! *database_path && key_file) {
	char *path = g_key_file_get_string (key_file, "database", "path", NULL);
	if (path) {
	    if (path[0] == '/')
		*database_path = talloc_strdup (notmuch, path);
	    else
		*database_path = talloc_asprintf (notmuch, "%s/%s", getenv ("HOME"), path);
	    g_free (path);
	}
    }
    if (! *database_path) {
	notmuch_status_t status;

	*database_path = _xdg_dir (notmuch, "XDG_DATA_HOME", ".local/share", profile);
	status = _db_dir_exists (*database_path, message);
	if (status) {
	    *database_path = NULL;
	} else {
	    notmuch->params |= NOTMUCH_PARAM_SPLIT;
	}
    }

    if (! *database_path) {
	*database_path = getenv ("MAILDIR");
    }

    if (! *database_path) {
	notmuch_status_t status;

	*database_path = talloc_asprintf (notmuch, "%s/mail", getenv ("HOME"));
	status = _db_dir_exists (*database_path, message);
	if (status) {
	    *database_path = NULL;
	}
    }

    if (*database_path == NULL) {
	*message = strdup ("Error: could not locate database.\n");
	return NOTMUCH_STATUS_NO_DATABASE;
    }

    if (*database_path[0] != '/') {
	*message = strdup ("Error: Database path must be absolute.\n");
	return NOTMUCH_STATUS_PATH_ERROR;
    }
    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_database_t *
_alloc_notmuch (const char *database_path, const char *config_path, const char *profile)
{
    notmuch_database_t *notmuch;

    notmuch = talloc_zero (NULL, notmuch_database_t);
    if (! notmuch)
	return NULL;

    notmuch->exception_reported = false;
    notmuch->status_string = NULL;
    notmuch->writable_xapian_db = NULL;
    notmuch->config_path = NULL;
    notmuch->atomic_nesting = 0;
    notmuch->transaction_count = 0;
    notmuch->transaction_threshold = 0;
    notmuch->view = 1;

    notmuch->params = NOTMUCH_PARAM_NONE;
    if (database_path)
	notmuch->params |= NOTMUCH_PARAM_DATABASE;
    if (config_path)
	notmuch->params |= NOTMUCH_PARAM_CONFIG;
    if (profile)
	notmuch->params |= NOTMUCH_PARAM_PROFILE;

    return notmuch;
}

static notmuch_status_t
_trial_open (const char *xapian_path, char **message_ptr)
{
    try {
	Xapian::Database db (xapian_path);
    } catch (const Xapian::DatabaseOpeningError &error) {
	IGNORE_RESULT (asprintf (message_ptr,
				 "Cannot open Xapian database at %s: %s\n",
				 xapian_path,
				 error.get_msg ().c_str ()));
	return NOTMUCH_STATUS_PATH_ERROR;
    } catch (const Xapian::Error &error) {
	IGNORE_RESULT (asprintf (message_ptr,
				 "A Xapian exception occurred opening database: %s\n",
				 error.get_msg ().c_str ()));
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
_notmuch_choose_xapian_path (void *ctx, const char *database_path,
			     const char **xapian_path, char **message_ptr)
{
    notmuch_status_t status;
    const char *trial_path, *notmuch_path;

    status = _db_dir_exists (database_path, message_ptr);
    if (status)
	goto DONE;

    trial_path = talloc_asprintf (ctx, "%s/xapian", database_path);
    status = _trial_open (trial_path, message_ptr);
    if (status != NOTMUCH_STATUS_PATH_ERROR)
	goto DONE;

    if (*message_ptr)
	free (*message_ptr);

    notmuch_path = talloc_asprintf (ctx, "%s/.notmuch", database_path);
    status = _db_dir_exists (notmuch_path, message_ptr);
    if (status)
	goto DONE;

    trial_path = talloc_asprintf (ctx, "%s/xapian", notmuch_path);
    status = _trial_open (trial_path, message_ptr);

  DONE:
    if (status == NOTMUCH_STATUS_SUCCESS)
	*xapian_path = trial_path;
    return status;
}

static void
_set_database_path (notmuch_database_t *notmuch,
		    const char *database_path)
{
    char *path = talloc_strdup (notmuch, database_path);

    strip_trailing (path, '/');

    _notmuch_config_cache (notmuch, NOTMUCH_CONFIG_DATABASE_PATH, path);
}

static void
_load_database_state (notmuch_database_t *notmuch)
{
    std::string last_thread_id;
    std::string last_mod;

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
}

static notmuch_status_t
_finish_open (notmuch_database_t *notmuch,
	      const char *profile,
	      notmuch_database_mode_t mode,
	      GKeyFile *key_file,
	      char **message_ptr)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    char *incompat_features;
    char *message = NULL;
    const char *autocommit_str;
    char *autocommit_end;
    unsigned int version;
    const char *database_path = notmuch_database_get_path (notmuch);

    try {

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
				     database_path, version, NOTMUCH_DATABASE_VERSION));
	    status = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}

	/* Check features. */
	incompat_features = NULL;
	notmuch->features = _notmuch_database_parse_features (
	    notmuch, notmuch->xapian_db->get_metadata ("features").c_str (),
	    version, mode == NOTMUCH_DATABASE_MODE_READ_WRITE ? 'w' : 'r',
	    &incompat_features);
	if (incompat_features) {
	    IGNORE_RESULT (asprintf (&message,
				     "Error: Notmuch database at %s\n"
				     "       requires features (%s)\n"
				     "       not supported by this version of notmuch.\n",
				     database_path, incompat_features));
	    status = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}

	_load_database_state (notmuch);

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
	notmuch->stemmer = new Xapian::Stem ("english");
	notmuch->query_parser->set_stemmer (*notmuch->stemmer);
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

	status = _choose_dir (notmuch, profile,
			      NOTMUCH_CONFIG_HOOK_DIR,
			      "XDG_CONFIG_HOME",
			      ".config",
			      "hooks",
			      &message);
	if (status)
	    goto DONE;

	status = _choose_dir (notmuch, profile,
			      NOTMUCH_CONFIG_BACKUP_DIR,
			      "XDG_DATA_HOME",
			      ".local/share",
			      "backups",
			      &message);
	if (status)
	    goto DONE;
	status = _notmuch_config_load_defaults (notmuch);
	if (status)
	    goto DONE;

	autocommit_str = notmuch_config_get (notmuch, NOTMUCH_CONFIG_AUTOCOMMIT);
	if (unlikely (! autocommit_str)) {
	    INTERNAL_ERROR ("missing configuration for autocommit");
	}
	notmuch->transaction_threshold = strtoul (autocommit_str, &autocommit_end, 10);
	if (*autocommit_end != '\0')
	    INTERNAL_ERROR ("Malformed database database.autocommit value: %s", autocommit_str);

	status = _notmuch_database_setup_standard_query_fields (notmuch);
	if (status)
	    goto DONE;

	status = _notmuch_database_setup_user_query_fields (notmuch);
	if (status)
	    goto DONE;

    } catch (const Xapian::Error &error) {
	IGNORE_RESULT (asprintf (&message, "A Xapian exception occurred opening database: %s\n",
				 error.get_msg ().c_str ()));
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
  DONE:
    if (message_ptr)
	*message_ptr = message;
    return status;
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
    notmuch_database_t *notmuch = NULL;
    char *message = NULL;
    GKeyFile *key_file = NULL;

    _notmuch_init ();

    notmuch = _alloc_notmuch (database_path, config_path, profile);
    if (! notmuch) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    status = _load_key_file (notmuch, config_path, profile, &key_file);
    if (status) {
	message = strdup ("Error: cannot load config file.\n");
	goto DONE;
    }

    if ((status = _choose_database_path (notmuch, profile, key_file,
					 &database_path,
					 &message)))
	goto DONE;

    status = _db_dir_exists (database_path, &message);
    if (status)
	goto DONE;

    _set_database_path (notmuch, database_path);

    status = _notmuch_choose_xapian_path (notmuch, database_path,
					  &notmuch->xapian_path, &message);
    if (status)
	goto DONE;

    status = _finish_open (notmuch, profile, mode, key_file, &message);

  DONE:
    if (key_file)
	g_key_file_free (key_file);

    if (message) {
	if (status_string)
	    *status_string = message;
	else
	    free (message);
    }

    if (status && notmuch) {
	notmuch_database_destroy (notmuch);
	notmuch = NULL;
    }

    if (database)
	*database = notmuch;

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
    const char *notmuch_path = NULL;
    char *message = NULL;
    GKeyFile *key_file = NULL;
    int err;

    _notmuch_init ();

    notmuch = _alloc_notmuch (database_path, config_path, profile);
    if (! notmuch) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    status = _load_key_file (notmuch, config_path, profile, &key_file);
    if (status) {
	message = strdup ("Error: cannot load config file.\n");
	goto DONE;
    }

    if ((status = _choose_database_path (notmuch, profile, key_file,
					 &database_path, &message)))
	goto DONE;

    status = _db_dir_exists (database_path, &message);
    if (status)
	goto DONE;

    _set_database_path (notmuch, database_path);

    if (key_file && ! (notmuch->params & NOTMUCH_PARAM_SPLIT)) {
	char *mail_root = notmuch_canonicalize_file_name (
	    g_key_file_get_string (key_file, "database", "mail_root", NULL));
	char *db_path = notmuch_canonicalize_file_name (database_path);

	if (mail_root && (0 != strcmp (mail_root, db_path)))
	    notmuch->params |= NOTMUCH_PARAM_SPLIT;

	free (mail_root);
	free (db_path);
    }

    if (notmuch->params & NOTMUCH_PARAM_SPLIT) {
	notmuch_path = database_path;
    } else {
	if (! (notmuch_path = talloc_asprintf (notmuch, "%s/%s", database_path, ".notmuch"))) {
	    status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	    goto DONE;
	}

	err = mkdir (notmuch_path, 0755);
	if (err) {
	    if (errno != EEXIST) {
		IGNORE_RESULT (asprintf (&message, "Error: Cannot create directory %s: %s.\n",
					 notmuch_path, strerror (errno)));
		status = NOTMUCH_STATUS_FILE_ERROR;
		goto DONE;
	    }
	}
    }

    if (! (notmuch->xapian_path = talloc_asprintf (notmuch, "%s/%s", notmuch_path, "xapian"))) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    status = _trial_open (notmuch->xapian_path, &message);
    if (status == NOTMUCH_STATUS_SUCCESS) {
	notmuch_database_destroy (notmuch);
	notmuch = NULL;
	status = NOTMUCH_STATUS_DATABASE_EXISTS;
	goto DONE;
    }

    if (message)
	free (message);

    status = _finish_open (notmuch,
			   profile,
			   NOTMUCH_DATABASE_MODE_READ_WRITE,
			   key_file,
			   &message);
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
    if (key_file)
	g_key_file_free (key_file);

    if (message) {
	if (status_string)
	    *status_string = message;
	else
	    free (message);
    }
    if (status && notmuch) {
	notmuch_database_destroy (notmuch);
	notmuch = NULL;
    }

    if (database)
	*database = notmuch;

    if (notmuch)
	notmuch->open = true;
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

	_load_database_state (notmuch);
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

static notmuch_status_t
_maybe_load_config_from_database (notmuch_database_t *notmuch,
				  GKeyFile *key_file,
				  const char *database_path,
				  const char *profile)
{
    char *message; /* ignored */

    if (_db_dir_exists (database_path, &message))
	return NOTMUCH_STATUS_NO_DATABASE;

    _set_database_path (notmuch, database_path);

    if (_notmuch_choose_xapian_path (notmuch, database_path, &notmuch->xapian_path, &message))
	return NOTMUCH_STATUS_NO_DATABASE;

    (void) _finish_open (notmuch, profile, NOTMUCH_DATABASE_MODE_READ_ONLY, key_file, &message);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_database_load_config (const char *database_path,
			      const char *config_path,
			      const char *profile,
			      notmuch_database_t **database,
			      char **status_string)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS, warning = NOTMUCH_STATUS_SUCCESS;
    notmuch_database_t *notmuch = NULL;
    char *message = NULL;
    GKeyFile *key_file = NULL;

    _notmuch_init ();

    notmuch = _alloc_notmuch (database_path, config_path, profile);
    if (! notmuch) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    status = _load_key_file (notmuch, config_path, profile, &key_file);
    switch (status) {
    case NOTMUCH_STATUS_SUCCESS:
	break;
    case NOTMUCH_STATUS_NO_CONFIG:
	warning = status;
	break;
    default:
	message = strdup ("Error: cannot load config file.\n");
	goto DONE;
    }

    status = _choose_database_path (notmuch, profile, key_file,
				    &database_path, &message);
    switch (status) {
    case NOTMUCH_STATUS_NO_DATABASE:
    case NOTMUCH_STATUS_SUCCESS:
	if (! warning)
	    warning = status;
	break;
    default:
	goto DONE;
    }


    if (database_path) {
	status = _maybe_load_config_from_database (notmuch, key_file, database_path, profile);
	switch (status) {
	case NOTMUCH_STATUS_NO_DATABASE:
	case NOTMUCH_STATUS_SUCCESS:
	    if (! warning)
		warning = status;
	    break;
	default:
	    goto DONE;
	}
    }

    if (key_file) {
	status = _notmuch_config_load_from_file (notmuch, key_file);
	if (status)
	    goto DONE;
    }
    status = _notmuch_config_load_defaults (notmuch);
    if (status)
	goto DONE;

  DONE:
    if (status_string)
	*status_string = message;

    if (status &&
	status != NOTMUCH_STATUS_NO_DATABASE
	&& status != NOTMUCH_STATUS_NO_CONFIG) {
	notmuch_database_destroy (notmuch);
	notmuch = NULL;
    }

    if (database)
	*database = notmuch;

    if (status)
	return status;
    else
	return warning;
}
