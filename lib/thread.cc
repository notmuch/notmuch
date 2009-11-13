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
    char *authors;
    GHashTable *tags;

    notmuch_bool_t has_message;
    time_t oldest;
    time_t newest;
};

static int
_notmuch_thread_destructor (notmuch_thread_t *thread)
{
    g_hash_table_unref (thread->tags);

    return 0;
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
	    if (author) {
		if (thread->authors)
		    thread->authors = talloc_asprintf (thread, "%s, %s",
						       thread->authors,
						       author);
		else
		    thread->authors = talloc_strdup (thread, author);
	    }
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

    date = notmuch_message_get_date (message);

    if (date < thread->oldest || ! thread->has_message)
	thread->oldest = date;

    if (date > thread->newest || ! thread->has_message)
	thread->newest = date;

    thread->has_message = 1;
}

/* Create a new notmuch_thread_t object for the given thread ID.
 *
 * Creating the thread will trigger a database search for the messages
 * belonging to the thread so that the thread object can return some
 * details about them, (authors, subject, etc.).
 *
 * Here, 'talloc owner' is an optional talloc context to which the new
 * thread will belong. This allows for the caller to not bother
 * calling notmuch_thread_destroy on the thread, and know that all
 * memory will be reclaimed with 'talloc_owner' is freed. The caller
 * still can call notmuch_thread_destroy when finished with the
 * thread if desired.
 *
 * The 'talloc_owner' argument can also be NULL, in which case the
 * caller *is* responsible for calling notmuch_thread_destroy.
 *
 * This function returns NULL in the case of any error.
 */
notmuch_thread_t *
_notmuch_thread_create (const void *ctx,
			notmuch_database_t *notmuch,
			const char *thread_id)
{
    notmuch_thread_t *thread;
    const char *query_string;
    notmuch_query_t *query;
    notmuch_messages_t *messages;

    query_string = talloc_asprintf (ctx, "thread:%s", thread_id);
    if (unlikely (query_string == NULL))
	return NULL;

    query = notmuch_query_create (notmuch, query_string);
    if (unlikely (query == NULL))
	return NULL;

    thread = talloc (ctx, notmuch_thread_t);
    if (unlikely (thread == NULL))
	return NULL;

    talloc_set_destructor (thread, _notmuch_thread_destructor);

    thread->notmuch = notmuch;
    thread->thread_id = talloc_strdup (thread, thread_id);
    thread->subject = NULL;
    thread->authors = NULL;
    thread->tags = g_hash_table_new_full (g_str_hash, g_str_equal,
					  free, NULL);

    thread->has_message = 0;
    thread->oldest = 0;
    thread->newest = 0;

    notmuch_query_set_sort (query, NOTMUCH_SORT_DATE_OLDEST_FIRST);

    for (messages = notmuch_query_search_messages (query, 0, -1);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	_thread_add_message (thread, notmuch_messages_get (messages));
    }

    notmuch_query_destroy (query);

    return thread;
}

const char *
notmuch_thread_get_thread_id (notmuch_thread_t *thread)
{
    return thread->thread_id;
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
