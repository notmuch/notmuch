/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
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
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"

/* Construct a single query string from the passed arguments, using
 * 'ctx' as the talloc owner for all allocations.
 *
 * Currently, the arguments are just connected with space characters,
 * but we might do more processing in the future, (such as inserting
 * any AND operators needed to work around Xapian QueryParser bugs).
 *
 * This function returns NULL in case of insufficient memory.
 */
char *
query_string_from_args (void *ctx, int argc, char *argv[])
{
    char *query_string;
    int i;

    query_string = talloc_strdup (ctx, "");
    if (query_string == NULL)
	return NULL;

    for (i = 0; i < argc; i++) {
	if (i != 0) {
	    query_string = talloc_strdup_append (query_string, " ");
	    if (query_string == NULL)
		return NULL;
	}

	query_string = talloc_strdup_append (query_string, argv[i]);
	if (query_string == NULL)
	    return NULL;
    }

    return query_string;
}

