/* filenames.c - Iterator for a list of filenames
 *
 * Copyright © 2010 Intel Corporation
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

#include "notmuch-private.h"

struct _notmuch_filenames {
    notmuch_string_node_t *iterator;
};

/* The notmuch_filenames_t iterates over a notmuch_string_list_t of
 * file names */
notmuch_filenames_t *
_notmuch_filenames_create (const void *ctx,
			   notmuch_string_list_t *list)
{
    notmuch_filenames_t *filenames;

    filenames = talloc (ctx, notmuch_filenames_t);
    if (unlikely (filenames == NULL))
	return NULL;

    filenames->iterator = list->head;
    (void) talloc_reference (filenames, list);

    return filenames;
}

notmuch_bool_t
notmuch_filenames_valid (notmuch_filenames_t *filenames)
{
    if (filenames == NULL)
	return FALSE;

    return (filenames->iterator != NULL);
}

const char *
notmuch_filenames_get (notmuch_filenames_t *filenames)
{
    if ((filenames == NULL) || (filenames->iterator == NULL))
	return NULL;

    return filenames->iterator->string;
}

void
notmuch_filenames_move_to_next (notmuch_filenames_t *filenames)
{
    if ((filenames == NULL) || (filenames->iterator == NULL))
	return;

    filenames->iterator = filenames->iterator->next;
}

void
notmuch_filenames_destroy (notmuch_filenames_t *filenames)
{
    talloc_free (filenames);
}
