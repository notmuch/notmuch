/* messages.c - Iterator for a set of messages
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


struct _notmuch_messages {
    notmuch_message_node_t *iterator;
};

/* Create a new notmuch_message_list_t object, with 'ctx' as its
 * talloc owner.
 *
 * This function can return NULL in case of out-of-memory.
 */
notmuch_message_list_t *
_notmuch_message_list_create (const void *ctx)
{
    notmuch_message_list_t *list;

    list = talloc (ctx, notmuch_message_list_t);
    if (unlikely (list == NULL))
	return NULL;

    list->head = NULL;
    list->tail = &list->head;

    return list;
}

/* Append 'node' (which can of course point to an aribtrarily long
 * list of nodes) to the end of 'list'.
 */
void
_notmuch_message_list_append (notmuch_message_list_t *list,
			      notmuch_message_node_t *node)
{
    *(list->tail) = node;
    list->tail = &node->next;
}

/* Allocate a new node for 'message' and append it to the end of
 * 'list'.
 */
void
_notmuch_message_list_add_message (notmuch_message_list_t *list,
				   notmuch_message_t *message)
{
    notmuch_message_node_t *node = talloc (list, notmuch_message_node_t);

    node->message = message;
    node->next = NULL;

    _notmuch_message_list_append (list, node);
}

notmuch_messages_t *
_notmuch_messages_create (notmuch_message_list_t *list)
{
    notmuch_messages_t *messages;

    if (list->head == NULL)
	return NULL;

    messages = talloc (list, notmuch_messages_t);
    if (unlikely (messages == NULL))
	return NULL;

    messages->iterator = list->head;

    return messages;
}

notmuch_bool_t
notmuch_messages_has_more (notmuch_messages_t *messages)
{
    return (messages != NULL && messages->iterator != NULL);
}

notmuch_message_t *
notmuch_messages_get (notmuch_messages_t *messages)
{
    if (messages->iterator == NULL)
	return NULL;

    return messages->iterator->message;
}

void
notmuch_messages_advance (notmuch_messages_t *messages)
{
    if (messages->iterator == NULL)
	return;

    messages->iterator = messages->iterator->next;
}

void
notmuch_messages_destroy (notmuch_messages_t *messages)
{
    talloc_free (messages);
}
