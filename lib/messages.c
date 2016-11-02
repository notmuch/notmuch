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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private.h"

#include <glib.h>

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

/* Append 'message' to the end of 'list'. */
void
_notmuch_message_list_add_message (notmuch_message_list_t *list,
				   notmuch_message_t *message)
{
    notmuch_message_node_t *node = talloc (list, notmuch_message_node_t);

    node->message = message;
    node->next = NULL;

    *(list->tail) = node;
    list->tail = &node->next;
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

    messages->is_of_list_type = TRUE;
    messages->iterator = list->head;

    return messages;
}

/* We're using the "is_of_type_list" to conditionally defer to the
 * notmuch_mset_messages_t implementation of notmuch_messages_t in
 * query.cc. It's ugly that that's over in query.cc, and it's ugly
 * that we're not using a union here. Both of those uglies are due to
 * C++:
 *
 *	1. I didn't want to force a C++ header file onto
 *	   notmuch-private.h and suddenly subject all our code to a
 *	   C++ compiler and its rules.
 *
 *	2. C++ won't allow me to put C++ objects, (with non-trivial
 *	   constructors) into a union anyway. Even though I'd
 *	   carefully control object construction with placement new
 *	   anyway. *sigh*
 */
notmuch_bool_t
notmuch_messages_valid (notmuch_messages_t *messages)
{
    if (messages == NULL)
	return FALSE;

    if (! messages->is_of_list_type)
	return _notmuch_mset_messages_valid (messages);

    return (messages->iterator != NULL);
}

notmuch_message_t *
notmuch_messages_get (notmuch_messages_t *messages)
{
    if (! messages->is_of_list_type)
	return _notmuch_mset_messages_get (messages);

    if (messages->iterator == NULL)
	return NULL;

    return messages->iterator->message;
}

void
notmuch_messages_move_to_next (notmuch_messages_t *messages)
{
    if (! messages->is_of_list_type) {
	_notmuch_mset_messages_move_to_next (messages);
	return;
    }

    if (messages->iterator == NULL)
	return;

    messages->iterator = messages->iterator->next;
}

void
notmuch_messages_destroy (notmuch_messages_t *messages)
{
    talloc_free (messages);
}


notmuch_tags_t *
notmuch_messages_collect_tags (notmuch_messages_t *messages)
{
    notmuch_string_list_t *tags;
    notmuch_tags_t *msg_tags;
    notmuch_message_t *msg;
    GHashTable *htable;
    GList *keys, *l;
    const char *tag;

    tags = _notmuch_string_list_create (messages);
    if (tags == NULL) return NULL;

    htable = g_hash_table_new_full (g_str_hash, g_str_equal, free, NULL);

    while ((msg = notmuch_messages_get (messages))) {
	msg_tags = notmuch_message_get_tags (msg);
	while ((tag = notmuch_tags_get (msg_tags))) {
	    g_hash_table_insert (htable, xstrdup (tag), NULL);
	    notmuch_tags_move_to_next (msg_tags);
	}
	notmuch_tags_destroy (msg_tags);
	notmuch_message_destroy (msg);
	notmuch_messages_move_to_next (messages);
    }

    keys = g_hash_table_get_keys (htable);
    for (l = keys; l; l = l->next) {
	_notmuch_string_list_append (tags, (char *)l->data);
    }

    g_list_free (keys);
    g_hash_table_destroy (htable);

    _notmuch_string_list_sort (tags);
    return _notmuch_tags_create (messages, tags);
}
