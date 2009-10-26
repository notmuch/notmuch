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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private.h"

#include <glib.h> /* GList */

struct _notmuch_tags {
    GList *tags;
    GList *iterator;
};

/* XXX: Should write some talloc-friendly list to avoid the need for
 * this. */
static int
_notmuch_tags_destructor (notmuch_tags_t *tags)
{
    g_list_free (tags->tags);

    return 0;
}

/* Create a new notmuch_tags_t object, with 'ctx' as its talloc owner.
 *
 * This function can return NULL in case of out-of-memory.
 */
notmuch_tags_t *
_notmuch_tags_create (void *ctx)
{
    notmuch_tags_t *tags;

    tags = talloc (ctx, notmuch_tags_t);
    if (unlikely (tags == NULL))
	return NULL;

    talloc_set_destructor (tags, _notmuch_tags_destructor);

    tags->tags = NULL;
    tags->iterator = NULL;

    return tags;
}

void
_notmuch_tags_add_tag (notmuch_tags_t *tags, const char *tag)
{
    tags->tags = g_list_prepend (tags->tags, talloc_strdup (tags, tag));
}

void
_notmuch_tags_sort (notmuch_tags_t *tags)
{
    tags->tags = g_list_sort (tags->tags, (GCompareFunc) strcmp);
}

void
_notmuch_tags_reset (notmuch_tags_t *tags)
{
    tags->iterator = tags->tags;
}

notmuch_bool_t
notmuch_tags_has_more (notmuch_tags_t *tags)
{
    return tags->iterator != NULL;
}

const char *
notmuch_tags_get (notmuch_tags_t *tags)
{
    if (tags->iterator)
	return (char *) tags->iterator->data;
    else
	return NULL;
}

void
notmuch_tags_advance (notmuch_tags_t *tags)
{
    tags->iterator = tags->iterator->next;
}

void
notmuch_tags_destroy (notmuch_tags_t *tags)
{
    talloc_free (tags);
}
