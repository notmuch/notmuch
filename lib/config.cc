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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: David Bremner <david@tethera.net>
 */

#include "notmuch.h"
#include "notmuch-private.h"
#include "database-private.h"

static const std::string CONFIG_PREFIX = "C";

notmuch_status_t
notmuch_database_set_config (notmuch_database_t *notmuch,
			     const char *key,
			     const char *value)
{
    notmuch_status_t status;
    Xapian::WritableDatabase *db;

    status = _notmuch_database_ensure_writable (notmuch);
    if (status)
	return status;

    try {
	db = static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db);
	db->set_metadata (CONFIG_PREFIX + key, value);
    } catch (const Xapian::Error &error) {
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
	notmuch->exception_reported = TRUE;
	_notmuch_database_log (notmuch, "Error: A Xapian exception occurred setting metadata: %s\n",
			       error.get_msg().c_str());
    }
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
	notmuch->exception_reported = TRUE;
	_notmuch_database_log (notmuch, "Error: A Xapian exception occurred getting metadata: %s\n",
			       error.get_msg().c_str());
    }
    return status;
}

notmuch_status_t
notmuch_database_get_config (notmuch_database_t *notmuch,
			     const char *key,
			     char **value)
{
    std::string strval;
    notmuch_status_t status;

    if (! value)
	return NOTMUCH_STATUS_NULL_POINTER;

    status = _metadata_value (notmuch, key, strval);
    if (status)
	return status;

    *value = strdup (strval.c_str ());

    return NOTMUCH_STATUS_SUCCESS;
}
