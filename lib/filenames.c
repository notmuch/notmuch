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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private.h"

typedef struct _notmuch_filenames_node {
    char *filename;
    struct _notmuch_filenames_node *next;
} notmuch_filenames_node_t;

struct _notmuch_filenames {
    notmuch_filenames_node_t *head;
    notmuch_filenames_node_t **tail;
    notmuch_filenames_node_t *iterator;
};

/* Create a new notmuch_filenames_t object, with 'ctx' as its
 * talloc owner.
 *
 * This function can return NULL in case of out-of-memory.
 */
notmuch_filenames_t *
_notmuch_filenames_create (const void *ctx)
{
    notmuch_filenames_t *filenames;

    filenames = talloc (ctx, notmuch_filenames_t);
    if (unlikely (filenames == NULL))
	return NULL;

    filenames->head = NULL;
    filenames->tail = &filenames->head;

    filenames->iterator = NULL;

    return filenames;
}

/* Append a single 'node' to the end of 'filenames'.
 */
void
_notmuch_filenames_add_filename (notmuch_filenames_t *filenames,
				 const char *filename)
{
    /* Create and initialize new node. */
    notmuch_filenames_node_t *node = talloc (filenames,
					     notmuch_filenames_node_t);

    node->filename = talloc_strdup (node, filename);
    node->next = NULL;

    /* Append the node to the list. */
    *(filenames->tail) = node;
    filenames->tail = &node->next;
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
    if (filenames->iterator == NULL)
	return NULL;

    return filenames->iterator->filename;
}

void
_notmuch_filenames_move_to_first (notmuch_filenames_t *filenames)
{
    filenames->iterator = filenames->head;
}

void
notmuch_filenames_move_to_next (notmuch_filenames_t *filenames)
{
    if (filenames->iterator == NULL)
	return;

    filenames->iterator = filenames->iterator->next;
}

void
notmuch_filenames_destroy (notmuch_filenames_t *filenames)
{
    talloc_free (filenames);
}
