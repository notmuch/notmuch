/* notmuch - Not much of an email program, (just index and search)
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

notmuch_bool_t
notmuch_built_with (const char *name)
{
    if (STRNCMP_LITERAL (name, "compact") == 0) {
	return HAVE_XAPIAN_COMPACT;
    } else if (STRNCMP_LITERAL (name, "field_processor") == 0) {
	return HAVE_XAPIAN_FIELD_PROCESSOR;
    } else if (STRNCMP_LITERAL (name, "retry_lock") == 0) {
	return HAVE_XAPIAN_DB_RETRY_LOCK;
    } else {
	return FALSE;
    }
}
