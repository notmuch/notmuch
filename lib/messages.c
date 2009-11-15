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

typedef struct _message_list {
    notmuch_message_t *message;
    struct _message_list *next;
} message_list_t;

struct _notmuch_messages {
    message_list_t *head;
    message_list_t **tail;
};

/* Create a new notmuch_messages_t object, with 'ctx' as its talloc owner.
 *
 * This function can return NULL in case of out-of-memory.
 */
notmuch_messages_t *
_notmuch_messages_create (void *ctx)
{
    notmuch_messages_t *messages;

    messages = talloc (ctx, notmuch_messages_t);
    if (unlikely (messages == NULL))
	return NULL;

    messages->head = NULL;
    messages->tail = &messages->head;

    return messages;
}

/* Add a new message to 'messages'.
 */
void
_notmuch_messages_add_message (notmuch_messages_t *messages,
			       notmuch_message_t *message)
{
    message_list_t *new = talloc (messages, message_list_t);

    new->message = message;
    new->next = NULL;

    *(messages->tail) = new;
    messages->tail = &new->next;
}

notmuch_bool_t
notmuch_messages_has_more (notmuch_messages_t *messages)
{
    return messages->head != NULL;
}

notmuch_message_t *
notmuch_messages_get (notmuch_messages_t *messages)
{
    if (messages->head == NULL)
	return NULL;

    return messages->head->message;
}

void
notmuch_messages_advance (notmuch_messages_t *messages)
{
    if (messages->head == NULL)
	return;

    messages->head = messages->head->next;
}

void
notmuch_messages_destroy (notmuch_messages_t *messages)
{
    talloc_free (messages);
}
