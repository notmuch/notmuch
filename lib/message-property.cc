/* message-property.cc - Properties are like tags, but (key,value) pairs.
 * keys are allowed to repeat.
 *
 * This file is part of notmuch.
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

#include "notmuch-private.h"
#include "database-private.h"
#include "message-private.h"

#define LOG_XAPIAN_EXCEPTION(message, error) _log_xapian_exception (__location__, message, error)

static void
_log_xapian_exception (const char *where, notmuch_message_t *message,  const Xapian::Error error)
{
    notmuch_database_t *notmuch = notmuch_message_get_database (message);

    _notmuch_database_log (notmuch,
			   "A Xapian exception occurred at %s: %s\n",
			   where,
			   error.get_msg ().c_str ());
    notmuch->exception_reported = true;
}

notmuch_status_t
notmuch_message_get_property (notmuch_message_t *message, const char *key, const char **value)
{
    if (! value)
	return NOTMUCH_STATUS_NULL_POINTER;

    *value = _notmuch_string_map_get (_notmuch_message_property_map (message), key);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_count_properties (notmuch_message_t *message, const char *key, unsigned int *count)
{
    if (! count || ! key || ! message)
	return NOTMUCH_STATUS_NULL_POINTER;

    notmuch_string_map_t *map;

    map = _notmuch_message_property_map (message);
    if (! map)
	return NOTMUCH_STATUS_NULL_POINTER;

    notmuch_string_map_iterator_t *matcher = _notmuch_string_map_iterator_create (map, key, true);

    if (! matcher)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    *count = 0;
    while (_notmuch_string_map_iterator_valid (matcher)) {
	(*count)++;
	_notmuch_string_map_iterator_move_to_next (matcher);
    }

    _notmuch_string_map_iterator_destroy (matcher);
    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_notmuch_message_modify_property (notmuch_message_t *message, const char *key, const char *value,
				  bool delete_it)
{
    notmuch_private_status_t private_status;
    notmuch_status_t status;
    char *term = NULL;

    status = _notmuch_database_ensure_writable (notmuch_message_get_database (message));
    if (status)
	return status;

    if (key == NULL || value == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (strchr (key, '='))
	return NOTMUCH_STATUS_ILLEGAL_ARGUMENT;

    term = talloc_asprintf (message, "%s=%s", key, value);

    try {
	if (delete_it)
	    private_status = _notmuch_message_remove_term (message, "property", term);
	else
	    private_status = _notmuch_message_add_term (message, "property", term);
    } catch (Xapian::Error &error) {
	LOG_XAPIAN_EXCEPTION (message, error);
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    if (private_status)
	return COERCE_STATUS (private_status,
			      "Unhandled error modifying message property");
    if (! _notmuch_message_frozen (message))
	_notmuch_message_sync (message);

    if (term)
	talloc_free (term);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_add_property (notmuch_message_t *message, const char *key, const char *value)
{
    return _notmuch_message_modify_property (message, key, value, false);
}

notmuch_status_t
notmuch_message_remove_property (notmuch_message_t *message, const char *key, const char *value)
{
    return _notmuch_message_modify_property (message, key, value, true);
}

static
notmuch_status_t
_notmuch_message_remove_all_properties (notmuch_message_t *message, const char *key, bool prefix)
{
    notmuch_status_t status;
    const char *term_prefix;

    status = _notmuch_database_ensure_writable (notmuch_message_get_database (message));
    if (status)
	return status;

    if (key)
	term_prefix = talloc_asprintf (message, "%s%s%s", _find_prefix ("property"), key,
				       prefix ? "" : "=");
    else
	term_prefix = _find_prefix ("property");

    try {
	/* XXX better error reporting ? */
	_notmuch_message_remove_terms (message, term_prefix);
    } catch (Xapian::Error &error) {
	LOG_XAPIAN_EXCEPTION (message, error);
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    if (! _notmuch_message_frozen (message))
	_notmuch_message_sync (message);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_remove_all_properties (notmuch_message_t *message, const char *key)
{
    return _notmuch_message_remove_all_properties (message, key, false);
}

notmuch_status_t
notmuch_message_remove_all_properties_with_prefix (notmuch_message_t *message, const char *prefix)
{
    return _notmuch_message_remove_all_properties (message, prefix, true);
}

notmuch_message_properties_t *
notmuch_message_get_properties (notmuch_message_t *message, const char *key, notmuch_bool_t exact)
{
    notmuch_string_map_t *map;

    map = _notmuch_message_property_map (message);
    return _notmuch_string_map_iterator_create (map, key, exact);
}

notmuch_bool_t
notmuch_message_properties_valid (notmuch_message_properties_t *properties)
{
    return _notmuch_string_map_iterator_valid (properties);
}

void
notmuch_message_properties_move_to_next (notmuch_message_properties_t *properties)
{
    return _notmuch_string_map_iterator_move_to_next (properties);
}

const char *
notmuch_message_properties_key (notmuch_message_properties_t *properties)
{
    return _notmuch_string_map_iterator_key (properties);
}

const char *
notmuch_message_properties_value (notmuch_message_properties_t *properties)
{
    return _notmuch_string_map_iterator_value (properties);
}

void
notmuch_message_properties_destroy (notmuch_message_properties_t *properties)
{
    _notmuch_string_map_iterator_destroy (properties);
}
