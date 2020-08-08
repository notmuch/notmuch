/* config.cc - API for database metadata
 *
 * Copyright © 2016 David Bremner
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: David Bremner <david@tethera.net>
 */

#include "notmuch.h"
#include "notmuch-private.h"
#include "database-private.h"

static const std::string CONFIG_PREFIX = "C";

struct _notmuch_config_list {
    notmuch_database_t *notmuch;
    Xapian::TermIterator iterator;
    char *current_key;
    char *current_val;
};

static int
_notmuch_config_list_destroy (notmuch_config_list_t *list)
{
    /* invoke destructor w/o deallocating memory */
    list->iterator.~TermIterator();
    return 0;
}

notmuch_status_t
notmuch_database_set_config (notmuch_database_t *notmuch,
			     const char *key,
			     const char *value)
{
    notmuch_status_t status;

    status = _notmuch_database_ensure_writable (notmuch);
    if (status)
	return status;

    if (! notmuch->config) {
	if ((status = _notmuch_config_load_from_database (notmuch)))
	    return status;
    }

    try {
	notmuch->writable_xapian_db->set_metadata (CONFIG_PREFIX + key, value);
    } catch (const Xapian::Error &error) {
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	notmuch->exception_reported = true;
	_notmuch_database_log (notmuch, "Error: A Xapian exception occurred setting metadata: %s\n",
			       error.get_msg ().c_str ());
    }

    if (status)
	return status;

    _notmuch_string_map_set (notmuch->config, key, value);

    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_metadata_value (notmuch_database_t *notmuch,
		 const char *key,
		 std::string &value)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    try {
	value = notmuch->xapian_db->get_metadata (CONFIG_PREFIX + key);
    } catch (const Xapian::Error &error) {
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	notmuch->exception_reported = true;
	_notmuch_database_log (notmuch, "Error: A Xapian exception occurred getting metadata: %s\n",
			       error.get_msg ().c_str ());
    }
    return status;
}

notmuch_status_t
notmuch_database_get_config (notmuch_database_t *notmuch,
			     const char *key,
			     char **value)
{
    const char* stored_val;
    notmuch_status_t status;

    if (! notmuch->config) {
	if ((status = _notmuch_config_load_from_database (notmuch)))
	    return status;
    }

    if (! value)
	return NOTMUCH_STATUS_NULL_POINTER;

    stored_val = _notmuch_string_map_get (notmuch->config, key);
    if (! stored_val) {
	/* XXX in principle this API should be fixed so empty string
	 * is distinguished from not found */
	*value = strdup("");
    } else {
	*value = strdup (stored_val);
    }

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_database_get_config_list (notmuch_database_t *notmuch,
				  const char *prefix,
				  notmuch_config_list_t **out)
{
    notmuch_config_list_t *list = NULL;
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;

    list = talloc (notmuch, notmuch_config_list_t);
    if (! list) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    list->notmuch = notmuch;
    list->current_key = NULL;
    list->current_val = NULL;

    try {

	new(&(list->iterator)) Xapian::TermIterator (notmuch->xapian_db->metadata_keys_begin
							 (CONFIG_PREFIX + (prefix ? prefix : "")));
	talloc_set_destructor (list, _notmuch_config_list_destroy);

    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch, "A Xapian exception occurred getting metadata iterator: %s.\n",
			       error.get_msg ().c_str ());
	notmuch->exception_reported = true;
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    *out = list;

  DONE:
    if (status) {
	if (list) {
	    talloc_free (list);
	    if (status != NOTMUCH_STATUS_XAPIAN_EXCEPTION)
		_notmuch_config_list_destroy (list);
	}
    }  else {
	talloc_set_destructor (list, _notmuch_config_list_destroy);
    }

    return status;
}

notmuch_bool_t
notmuch_config_list_valid (notmuch_config_list_t *metadata)
{
    if (metadata->iterator == metadata->notmuch->xapian_db->metadata_keys_end ())
	return false;

    return true;
}

static inline char * _key_from_iterator (notmuch_config_list_t *list) {
    return talloc_strdup (list, (*list->iterator).c_str () + CONFIG_PREFIX.length ());
}

const char *
notmuch_config_list_key (notmuch_config_list_t *list)
{
    if (list->current_key)
	talloc_free (list->current_key);

    list->current_key = _key_from_iterator (list);

    return list->current_key;
}

const char *
notmuch_config_list_value (notmuch_config_list_t *list)
{
    std::string strval;
    notmuch_status_t status;
    char *key = _key_from_iterator (list);

    /* TODO: better error reporting?? */
    status = _metadata_value (list->notmuch, key, strval);
    if (status)
	return NULL;

    if (list->current_val)
	talloc_free (list->current_val);

    list->current_val = talloc_strdup (list, strval.c_str ());
    talloc_free (key);
    return list->current_val;
}

void
notmuch_config_list_move_to_next (notmuch_config_list_t *list)
{
    list->iterator++;
}

void
notmuch_config_list_destroy (notmuch_config_list_t *list)
{
    talloc_free (list);
}

notmuch_status_t
_notmuch_config_load_from_database (notmuch_database_t *notmuch)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    notmuch_config_list_t *list;

    if (notmuch->config == NULL)
	notmuch->config = _notmuch_string_map_create (notmuch);

    if (unlikely(notmuch->config == NULL))
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    status = notmuch_database_get_config_list (notmuch, "", &list);
    if (status)
	return status;

    for (; notmuch_config_list_valid (list); notmuch_config_list_move_to_next (list)) {
	_notmuch_string_map_append (notmuch->config,
				    notmuch_config_list_key (list),
				    notmuch_config_list_value (list));
    }

    return status;
}

notmuch_status_t
_notmuch_config_load_from_file (notmuch_database_t *notmuch,
				GKeyFile *file)
{
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    gchar **groups,**keys, *val;

    if (notmuch->config == NULL)
	notmuch->config = _notmuch_string_map_create (notmuch);

    if (unlikely(notmuch->config == NULL)) {
	status = NOTMUCH_STATUS_OUT_OF_MEMORY;
	goto DONE;
    }

    for (groups = g_key_file_get_groups (file, NULL); *groups; groups++) {
	for (keys = g_key_file_get_keys (file, *groups, NULL, NULL); *keys; keys++) {
	    char *absolute_key = talloc_asprintf(notmuch, "%s.%s", *groups,  *keys);
	    val = g_key_file_get_value (file, *groups, *keys, NULL);
	    if (! val) {
		status = NOTMUCH_STATUS_FILE_ERROR;
		goto DONE;
	    }
	    _notmuch_string_map_set (notmuch->config, absolute_key, val);
	    talloc_free (absolute_key);
	    if (status)
		goto DONE;
	}
    }

 DONE:
    return status;
}
