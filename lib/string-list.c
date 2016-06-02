/* strings.c - Iterator for a list of strings
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
 *         Austin Clements <aclements@csail.mit.edu>
 */

#include "notmuch-private.h"

/* Create a new notmuch_string_list_t object, with 'ctx' as its
 * talloc owner.
 *
 * This function can return NULL in case of out-of-memory.
 */
notmuch_string_list_t *
_notmuch_string_list_create (const void *ctx)
{
    notmuch_string_list_t *list;

    list = talloc (ctx, notmuch_string_list_t);
    if (unlikely (list == NULL))
	return NULL;

    list->length = 0;
    list->head = NULL;
    list->tail = &list->head;

    return list;
}

void
_notmuch_string_list_append (notmuch_string_list_t *list,
			     const char *string)
{
    /* Create and initialize new node. */
    notmuch_string_node_t *node = talloc (list, notmuch_string_node_t);

    node->string = talloc_strdup (node, string);
    node->next = NULL;

    /* Append the node to the list. */
    *(list->tail) = node;
    list->tail = &node->next;
    list->length++;
}

static int
cmpnode (const void *pa, const void *pb)
{
    notmuch_string_node_t *a = *(notmuch_string_node_t * const *)pa;
    notmuch_string_node_t *b = *(notmuch_string_node_t * const *)pb;

    return strcmp (a->string, b->string);
}

void
_notmuch_string_list_sort (notmuch_string_list_t *list)
{
    notmuch_string_node_t **nodes, *node;
    int i;

    if (list->length == 0)
	return;

    nodes = talloc_array (list, notmuch_string_node_t *, list->length);
    if (unlikely (nodes == NULL))
	INTERNAL_ERROR ("Could not allocate memory for list sort");

    for (i = 0, node = list->head; node; i++, node = node->next)
	nodes[i] = node;

    qsort (nodes, list->length, sizeof (*nodes), cmpnode);

    for (i = 0; i < list->length - 1; ++i)
	nodes[i]->next = nodes[i+1];
    nodes[i]->next = NULL;
    list->head = nodes[0];
    list->tail = &nodes[i]->next;

    talloc_free (nodes);
}
