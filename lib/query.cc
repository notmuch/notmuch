/* query.cc - Support for searching a notmuch database
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

#include <glib.h> /* GHashTable, GPtrArray */

#include <xapian.h>

struct _notmuch_query {
    notmuch_database_t *notmuch;
    const char *query_string;
    notmuch_sort_t sort;
};

typedef struct _notmuch_mset_messages {
    notmuch_messages_t base;
    notmuch_database_t *notmuch;
    Xapian::MSetIterator iterator;
    Xapian::MSetIterator iterator_end;
} notmuch_mset_messages_t;

struct _notmuch_threads {
    notmuch_query_t *query;
    GHashTable *threads;
    notmuch_messages_t *messages;

    /* This thread ID is our iterator state. */
    const char *thread_id;
};

notmuch_query_t *
notmuch_query_create (notmuch_database_t *notmuch,
		      const char *query_string)
{
    notmuch_query_t *query;

#ifdef DEBUG_QUERY
    fprintf (stderr, "Query string is:\n%s\n", query_string);
#endif

    query = talloc (NULL, notmuch_query_t);
    if (unlikely (query == NULL))
	return NULL;

    query->notmuch = notmuch;

    query->query_string = talloc_strdup (query, query_string);

    query->sort = NOTMUCH_SORT_NEWEST_FIRST;

    return query;
}

void
notmuch_query_set_sort (notmuch_query_t *query, notmuch_sort_t sort)
{
    query->sort = sort;
}

/* We end up having to call the destructors explicitly because we had
 * to use "placement new" in order to initialize C++ objects within a
 * block that we allocated with talloc. So C++ is making talloc
 * slightly less simple to use, (we wouldn't need
 * talloc_set_destructor at all otherwise).
 */
static int
_notmuch_messages_destructor (notmuch_mset_messages_t *messages)
{
    messages->iterator.~MSetIterator ();
    messages->iterator_end.~MSetIterator ();

    return 0;
}

notmuch_messages_t *
notmuch_query_search_messages (notmuch_query_t *query)
{
    notmuch_database_t *notmuch = query->notmuch;
    const char *query_string = query->query_string;
    notmuch_mset_messages_t *messages;

    messages = talloc (query, notmuch_mset_messages_t);
    if (unlikely (messages == NULL))
	return NULL;

    try {

	messages->base.is_of_list_type = FALSE;
	messages->base.iterator = NULL;
	messages->notmuch = notmuch;
	new (&messages->iterator) Xapian::MSetIterator ();
	new (&messages->iterator_end) Xapian::MSetIterator ();

	talloc_set_destructor (messages, _notmuch_messages_destructor);

	Xapian::Enquire enquire (*notmuch->xapian_db);
	Xapian::Query mail_query (talloc_asprintf (query, "%s%s",
						   _find_prefix ("type"),
						   "mail"));
	Xapian::Query string_query, final_query;
	Xapian::MSet mset;
	unsigned int flags = (Xapian::QueryParser::FLAG_BOOLEAN |
			      Xapian::QueryParser::FLAG_PHRASE |
			      Xapian::QueryParser::FLAG_LOVEHATE |
			      Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE |
			      Xapian::QueryParser::FLAG_WILDCARD |
			      Xapian::QueryParser::FLAG_PURE_NOT);

	if (strcmp (query_string, "") == 0) {
	    final_query = mail_query;
	} else {
	    string_query = notmuch->query_parser->
		parse_query (query_string, flags);
	    final_query = Xapian::Query (Xapian::Query::OP_AND,
					 mail_query, string_query);
	}

	enquire.set_weighting_scheme (Xapian::BoolWeight());

	switch (query->sort) {
	case NOTMUCH_SORT_OLDEST_FIRST:
	    enquire.set_sort_by_value (NOTMUCH_VALUE_TIMESTAMP, FALSE);
	    break;
	case NOTMUCH_SORT_NEWEST_FIRST:
	    enquire.set_sort_by_value (NOTMUCH_VALUE_TIMESTAMP, TRUE);
	    break;
	case NOTMUCH_SORT_MESSAGE_ID:
	    enquire.set_sort_by_value (NOTMUCH_VALUE_MESSAGE_ID, FALSE);
	    break;
	}

#if DEBUG_QUERY
	fprintf (stderr, "Final query is:\n%s\n", final_query.get_description().c_str());
#endif

	enquire.set_query (final_query);

	mset = enquire.get_mset (0, notmuch->xapian_db->get_doccount ());

	messages->iterator = mset.begin ();
	messages->iterator_end = mset.end ();

    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred performing query: %s\n",
		 error.get_msg().c_str());
	fprintf (stderr, "Query string was: %s\n", query->query_string);
	notmuch->exception_reported = TRUE;
    }

    return &messages->base;
}

notmuch_bool_t
_notmuch_mset_messages_has_more (notmuch_messages_t *messages)
{
    notmuch_mset_messages_t *mset_messages;

    mset_messages = (notmuch_mset_messages_t *) messages;

    return (mset_messages->iterator != mset_messages->iterator_end);
}

notmuch_message_t *
_notmuch_mset_messages_get (notmuch_messages_t *messages)
{
    notmuch_message_t *message;
    Xapian::docid doc_id;
    notmuch_private_status_t status;
    notmuch_mset_messages_t *mset_messages;

    mset_messages = (notmuch_mset_messages_t *) messages;

    if (! _notmuch_mset_messages_has_more (&mset_messages->base))
	return NULL;

    doc_id = *mset_messages->iterator;

    message = _notmuch_message_create (mset_messages,
				       mset_messages->notmuch, doc_id,
				       &status);

    if (message == NULL &&
       status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND)
    {
	INTERNAL_ERROR ("a messages iterator contains a non-existent document ID.\n");
    }

    return message;
}

void
_notmuch_mset_messages_advance (notmuch_messages_t *messages)
{
    notmuch_mset_messages_t *mset_messages;

    mset_messages = (notmuch_mset_messages_t *) messages;

    mset_messages->iterator++;
}

/* Glib objects force use to use a talloc destructor as well, (but not
 * nearly as ugly as the for messages due to C++ objects). At
 * this point, I'd really like to have some talloc-friendly
 * equivalents for the few pieces of glib that I'm using. */
static int
_notmuch_threads_destructor (notmuch_threads_t *threads)
{
    g_hash_table_unref (threads->threads);

    return 0;
}

notmuch_threads_t *
notmuch_query_search_threads (notmuch_query_t *query)
{
    notmuch_threads_t *threads;

    threads = talloc (query, notmuch_threads_t);
    if (threads == NULL)
	return NULL;

    threads->query = query;
    threads->threads = g_hash_table_new_full (g_str_hash, g_str_equal,
					      free, NULL);

    threads->messages = notmuch_query_search_messages (query);

    threads->thread_id = NULL;

    talloc_set_destructor (threads, _notmuch_threads_destructor);

    return threads;
}

void
notmuch_query_destroy (notmuch_query_t *query)
{
    talloc_free (query);
}

notmuch_bool_t
notmuch_threads_has_more (notmuch_threads_t *threads)
{
    notmuch_message_t *message;

    if (threads->thread_id)
	return TRUE;

    while (notmuch_messages_has_more (threads->messages))
    {
	message = notmuch_messages_get (threads->messages);

	threads->thread_id = notmuch_message_get_thread_id (message);

	if (! g_hash_table_lookup_extended (threads->threads,
					    threads->thread_id,
					    NULL, NULL))
	{
	    g_hash_table_insert (threads->threads,
				 xstrdup (threads->thread_id), NULL);
	    notmuch_messages_advance (threads->messages);
	    return TRUE;
	}

	notmuch_messages_advance (threads->messages);
    }

    threads->thread_id = NULL;
    return FALSE;
}

notmuch_thread_t *
notmuch_threads_get (notmuch_threads_t *threads)
{
    if (! notmuch_threads_has_more (threads))
	return NULL;

    return _notmuch_thread_create (threads->query,
				   threads->query->notmuch,
				   threads->thread_id,
				   threads->query->query_string);
}

void
notmuch_threads_advance (notmuch_threads_t *threads)
{
    threads->thread_id = NULL;
}

void
notmuch_threads_destroy (notmuch_threads_t *threads)
{
    talloc_free (threads);
}

unsigned
notmuch_query_count_messages (notmuch_query_t *query)
{
    notmuch_database_t *notmuch = query->notmuch;
    const char *query_string = query->query_string;
    Xapian::doccount count = 0;

    try {
	Xapian::Enquire enquire (*notmuch->xapian_db);
	Xapian::Query mail_query (talloc_asprintf (query, "%s%s",
						   _find_prefix ("type"),
						   "mail"));
	Xapian::Query string_query, final_query;
	Xapian::MSet mset;
	unsigned int flags = (Xapian::QueryParser::FLAG_BOOLEAN |
			      Xapian::QueryParser::FLAG_PHRASE |
			      Xapian::QueryParser::FLAG_LOVEHATE |
			      Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE |
			      Xapian::QueryParser::FLAG_WILDCARD |
			      Xapian::QueryParser::FLAG_PURE_NOT);

	if (strcmp (query_string, "") == 0) {
	    final_query = mail_query;
	} else {
	    string_query = notmuch->query_parser->
		parse_query (query_string, flags);
	    final_query = Xapian::Query (Xapian::Query::OP_AND,
					 mail_query, string_query);
	}

	enquire.set_weighting_scheme(Xapian::BoolWeight());
	enquire.set_docid_order(Xapian::Enquire::ASCENDING);

#if DEBUG_QUERY
	fprintf (stderr, "Final query is:\n%s\n", final_query.get_description().c_str());
#endif

	enquire.set_query (final_query);

	mset = enquire.get_mset (0, notmuch->xapian_db->get_doccount ());

	count = mset.get_matches_estimated();

    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred: %s\n",
		 error.get_msg().c_str());
	fprintf (stderr, "Query string was: %s\n", query->query_string);
    }

    return count;
}
