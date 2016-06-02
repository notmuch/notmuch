/*
 * Database routines intended only for testing, not exported from
 * library.
 *
 * Copyright (c) 2012 David Bremner
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
#include "database-test.h"

notmuch_status_t
notmuch_database_add_stub_message (notmuch_database_t *notmuch,
				   const char *message_id,
				   const char **tags)
{
    const char **tag;
    notmuch_status_t ret;
    notmuch_private_status_t private_status;
    notmuch_message_t *message;

    ret = _notmuch_database_ensure_writable (notmuch);
    if (ret)
	return ret;

    message = _notmuch_message_create_for_message_id (notmuch,
						      message_id,
						      &private_status);
    if (message == NULL) {
	return COERCE_STATUS (private_status,
			      "Unexpected status value from _notmuch_message_create_for_message_id");

    }

    if (private_status != NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND)
	return NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID;

    _notmuch_message_add_term (message, "type", "mail");

    if (tags) {
	ret = notmuch_message_freeze (message);
	if (ret)
	    return ret;

	for (tag = tags; *tag; tag++) {
	    ret = notmuch_message_add_tag (message, *tag);
	    if (ret)
		return ret;
	}

	ret = notmuch_message_thaw (message);
	if (ret)
	    return ret;
    }

    return NOTMUCH_STATUS_SUCCESS;
}
