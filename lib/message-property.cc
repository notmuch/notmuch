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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: David Bremner <david@tethera.net>
 */

#include "notmuch-private.h"
#include "database-private.h"
#include "message-private.h"

notmuch_status_t
notmuch_message_get_property (notmuch_message_t *message, const char *key, const char **value)
{
    if (! value)
	return NOTMUCH_STATUS_NULL_POINTER;

    *value = _notmuch_string_map_get (_notmuch_message_property_map (message), key);

    return NOTMUCH_STATUS_SUCCESS;
}

static notmuch_status_t
_notmuch_message_modify_property (notmuch_message_t *message, const char *key, const char *value,
				  notmuch_bool_t delete_it)
{
    notmuch_private_status_t private_status;
    notmuch_status_t status;
    char *term = NULL;

    status = _notmuch_database_ensure_writable (_notmuch_message_database (message));
    if (status)
	return status;

    if (key == NULL || value == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (strchr (key, '='))
	return NOTMUCH_STATUS_ILLEGAL_ARGUMENT;

    term = talloc_asprintf (message, "%s=%s", key, value);

    if (delete_it)
	private_status = _notmuch_message_remove_term (message, "property", term);
    else
	private_status = _notmuch_message_add_term (message, "property", term);

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
    return _notmuch_message_modify_property (message, key, value, FALSE);
}

notmuch_status_t
notmuch_message_remove_property (notmuch_message_t *message, const char *key, const char *value)
{
    return _notmuch_message_modify_property (message, key, value, TRUE);
}

notmuch_status_t
notmuch_message_remove_all_properties (notmuch_message_t *message, const char *key)
{
    notmuch_status_t status;
    const char * term_prefix;

    status = _notmuch_database_ensure_writable (_notmuch_message_database (message));
    if (status)
	return status;

    _notmuch_message_invalidate_metadata (message, "property");
    if (key)
	term_prefix = talloc_asprintf (message, "%s%s=", _find_prefix ("property"), key);
    else
	term_prefix = _find_prefix ("property");

    /* XXX better error reporting ? */
    _notmuch_message_remove_terms (message, term_prefix);

    return NOTMUCH_STATUS_SUCCESS;
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
