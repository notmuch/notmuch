/* thread.cc - Results of thread-based searches from a notmuch database
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
#include "database-private.h"

#include <xapian.h>

#include <gmime/gmime.h>
#include <glib.h> /* GHashTable */

struct _notmuch_thread {
    notmuch_database_t *notmuch;
    char *thread_id;
    char *subject;
    GHashTable *authors_hash;
    char *authors;
    GHashTable *tags;

    int total_messages;
    int matched_messages;
    time_t oldest;
    time_t newest;
};

static int
_notmuch_thread_destructor (notmuch_thread_t *thread)
{
    g_hash_table_unref (thread->authors_hash);
    g_hash_table_unref (thread->tags);

    return 0;
}

static void
_thread_add_author (notmuch_thread_t *thread,
		    const char *author)
{
    if (author == NULL)
	return;

    if (g_hash_table_lookup_extended (thread->authors_hash,
				      author, NULL, NULL))
	return;

    g_hash_table_insert (thread->authors_hash, xstrdup (author), NULL);

    if (thread->authors)
	thread->authors = talloc_asprintf (thread, "%s, %s",
					   thread->authors,
					   author);
    else
	thread->authors = talloc_strdup (thread, author);
}

static void
_thread_add_message (notmuch_thread_t *thread,
		     notmuch_message_t *message)
{
    notmuch_tags_t *tags;
    const char *tag;
    time_t date;
    InternetAddressList *list;
    InternetAddress *address;
    const char *from, *author;

    from = notmuch_message_get_header (message, "from");
    list = internet_address_list_parse_string (from);
    if (list) {
	address = internet_address_list_get_address (list, 0);
	if (address) {
	    author = internet_address_get_name (address);
	    if (author == NULL) {
		InternetAddressMailbox *mailbox;
		mailbox = INTERNET_ADDRESS_MAILBOX (address);
		author = internet_address_mailbox_get_addr (mailbox);
	    }
	    _thread_add_author (thread, author);
	}
	g_object_unref (G_OBJECT (list));
    }

    if (! thread->subject) {
	const char *subject;
	subject = notmuch_message_get_header (message, "subject");
	thread->subject = talloc_strdup (thread, subject);
    }

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_has_more (tags);
	 notmuch_tags_advance (tags))
    {
	tag = notmuch_tags_get (tags);
	g_hash_table_insert (thread->tags, xstrdup (tag), NULL);
    }

    thread->total_messages++;
}

static void
_thread_add_matched_message (notmuch_thread_t *thread,
			     notmuch_message_t *message)
{
    time_t date;

    date = notmuch_message_get_date (message);

    if (date < thread->oldest || ! thread->matched_messages)
	thread->oldest = date;

    if (date > thread->newest || ! thread->matched_messages)
	thread->newest = date;

    thread->matched_messages++;
}

/* Create a new notmuch_thread_t object for the given thread ID,
 * treating any messages matching 'query_string' as "matched".
 *
 * Creating the thread will trigger two database searches. The first
 * is for all messages belonging to the thread, (to get the first
 * subject line, the total count of messages, and all authors). The
 * second search is for all messages that are in the thread and that
 * also match the given query_string. This is to allow for a separate
 * count of matched messages, and to allow a viewer to diplay these
 * messages differently.
 *
 * Here, 'ctx' is talloc context for the resulting thread object.
 *
 * This function returns NULL in the case of any error.
 */
notmuch_thread_t *
_notmuch_thread_create (void *ctx,
			notmuch_database_t *notmuch,
			const char *thread_id,
			const char *query_string)
{
    notmuch_thread_t *thread;
    const char *thread_id_query_string, *matched_query_string;
    notmuch_query_t *thread_id_query, *matched_query;
    notmuch_messages_t *messages;

    thread_id_query_string = talloc_asprintf (ctx, "thread:%s", thread_id);
    if (unlikely (query_string == NULL))
	return NULL;

    matched_query_string = talloc_asprintf (ctx, "%s AND (%s)",
					    thread_id_query_string,
					    query_string);
    if (unlikely (matched_query_string == NULL))
	return NULL;

    thread_id_query = notmuch_query_create (notmuch, thread_id_query_string);
    if (unlikely (thread_id_query == NULL))
	return NULL;

    matched_query = notmuch_query_create (notmuch, matched_query_string);
    if (unlikely (thread_id_query == NULL))
	return NULL;

    thread = talloc (ctx, notmuch_thread_t);
    if (unlikely (thread == NULL))
	return NULL;

    talloc_set_destructor (thread, _notmuch_thread_destructor);

    thread->notmuch = notmuch;
    thread->thread_id = talloc_strdup (thread, thread_id);
    thread->subject = NULL;
    thread->authors_hash = g_hash_table_new_full (g_str_hash, g_str_equal,
						  free, NULL);
    thread->authors = NULL;
    thread->tags = g_hash_table_new_full (g_str_hash, g_str_equal,
					  free, NULL);

    thread->total_messages = 0;
    thread->matched_messages = 0;
    thread->oldest = 0;
    thread->newest = 0;

    notmuch_query_set_sort (thread_id_query, NOTMUCH_SORT_DATE);

    for (messages = notmuch_query_search_messages (thread_id_query, 0, -1);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	_thread_add_message (thread, notmuch_messages_get (messages));
    }

    notmuch_query_destroy (thread_id_query);

    for (messages = notmuch_query_search_messages (matched_query, 0, -1);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	_thread_add_matched_message (thread, notmuch_messages_get (messages));
    }

    notmuch_query_destroy (matched_query);

    return thread;
}

const char *
notmuch_thread_get_thread_id (notmuch_thread_t *thread)
{
    return thread->thread_id;
}

int
notmuch_thread_get_total_messages (notmuch_thread_t *thread)
{
    return thread->total_messages;
}

int
notmuch_thread_get_matched_messages (notmuch_thread_t *thread)
{
    return thread->matched_messages;
}

const char *
notmuch_thread_get_authors (notmuch_thread_t *thread)
{
    return thread->authors;
}

const char *
notmuch_thread_get_subject (notmuch_thread_t *thread)
{
    return thread->subject;
}

time_t
notmuch_thread_get_oldest_date (notmuch_thread_t *thread)
{
    return thread->oldest;
}

time_t
notmuch_thread_get_newest_date (notmuch_thread_t *thread)
{
    return thread->newest;
}

notmuch_tags_t *
notmuch_thread_get_tags (notmuch_thread_t *thread)
{
    notmuch_tags_t *tags;
    GList *keys, *l;

    tags = _notmuch_tags_create (thread);
    if (unlikely (tags == NULL))
	return NULL;

    keys = g_hash_table_get_keys (thread->tags);

    for (l = keys; l; l = l->next)
	_notmuch_tags_add_tag (tags, (char *) l->data);

    g_list_free (keys);

    _notmuch_tags_prepare_iterator (tags);

    return tags;
}

void
notmuch_thread_destroy (notmuch_thread_t *thread)
{
    talloc_free (thread);
}
