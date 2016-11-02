/* tags.c - Iterator for tags returned from message or thread
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

#include "notmuch-private.h"

struct _notmuch_tags {
    notmuch_string_node_t *iterator;
};

/* Create a new notmuch_tags_t object, with 'ctx' as its talloc owner.
 * The returned iterator will talloc_steal the 'list', since the list
 * is almost always transient.
 *
 * This function can return NULL in case of out-of-memory.
 */
notmuch_tags_t *
_notmuch_tags_create (const void *ctx, notmuch_string_list_t *list)
{
    notmuch_tags_t *tags;

    tags = talloc (ctx, notmuch_tags_t);
    if (unlikely (tags == NULL))
	return NULL;

    tags->iterator = list->head;
    (void) talloc_steal (tags, list);

    return tags;
}

notmuch_bool_t
notmuch_tags_valid (notmuch_tags_t *tags)
{
    return tags->iterator != NULL;
}

const char *
notmuch_tags_get (notmuch_tags_t *tags)
{
    if (tags->iterator == NULL)
	return NULL;

    return (char *) tags->iterator->string;
}

void
notmuch_tags_move_to_next (notmuch_tags_t *tags)
{
    if (tags->iterator == NULL)
	return;

    tags->iterator = tags->iterator->next;
}

void
notmuch_tags_destroy (notmuch_tags_t *tags)
{
    talloc_free (tags);
}
