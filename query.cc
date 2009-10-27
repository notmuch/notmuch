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

struct _notmuch_message_results {
    notmuch_database_t *notmuch;
    Xapian::MSetIterator iterator;
    Xapian::MSetIterator iterator_end;
};

struct _notmuch_thread_results {
    notmuch_database_t *notmuch;
    GPtrArray *threads;
    unsigned int index;
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

    query->sort = NOTMUCH_SORT_DATE_OLDEST_FIRST;

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
_notmuch_message_results_destructor (notmuch_message_results_t *results)
{
    results->iterator.~MSetIterator ();
    results->iterator_end.~MSetIterator ();

    return 0;
}

notmuch_message_results_t *
notmuch_query_search_messages (notmuch_query_t *query)
{
    notmuch_database_t *notmuch = query->notmuch;
    const char *query_string = query->query_string;
    notmuch_message_results_t *results;

    results = talloc (query, notmuch_message_results_t);
    if (unlikely (results == NULL))
	return NULL;

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
			      Xapian::QueryParser::FLAG_WILDCARD);

	if (strcmp (query_string, "") == 0) {
	    final_query = mail_query;
	} else {
	    string_query = notmuch->query_parser->
		parse_query (query_string, flags);
	    final_query = Xapian::Query (Xapian::Query::OP_AND,
					 mail_query, string_query);
	}

	switch (query->sort) {
	case NOTMUCH_SORT_DATE_OLDEST_FIRST:
	    enquire.set_sort_by_value (NOTMUCH_VALUE_TIMESTAMP, FALSE);
	    break;
	case NOTMUCH_SORT_DATE_NEWEST_FIRST:
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

	results->notmuch = notmuch;

	new (&results->iterator) Xapian::MSetIterator ();
	new (&results->iterator_end) Xapian::MSetIterator ();

	talloc_set_destructor (results, _notmuch_message_results_destructor);

	results->iterator = mset.begin ();
	results->iterator_end = mset.end ();

    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred: %s\n",
		 error.get_msg().c_str());
    }

    return results;
}

/* Glib objects force use to use a talloc destructor as well, (but not
 * nearly as ugly as the for message_results due to C++ objects). At
 * this point, I'd really like to have some talloc-friendly
 * equivalents for the few pieces of glib that I'm using. */
static int
_notmuch_thread_results_destructor (notmuch_thread_results_t *results)
{
    g_ptr_array_free (results->threads, TRUE);

    return 0;
}

notmuch_thread_results_t *
notmuch_query_search_threads (notmuch_query_t *query)
{
    notmuch_thread_results_t *thread_results;
    notmuch_thread_t *thread;
    const char *thread_id;
    notmuch_message_results_t *message_results;
    notmuch_message_t *message;
    notmuch_tags_t *tags;
    const char *tag;
    GHashTable *seen;

    thread_results = talloc (query, notmuch_thread_results_t);
    if (thread_results == NULL)
	return NULL;

    thread_results->notmuch = query->notmuch;
    thread_results->threads = g_ptr_array_new ();
    thread_results->index = 0;

    talloc_set_destructor (thread_results, _notmuch_thread_results_destructor);

    seen = g_hash_table_new_full (g_str_hash, g_str_equal,
				  free, NULL);

    for (message_results = notmuch_query_search_messages (query);
	 notmuch_message_results_has_more (message_results);
	 notmuch_message_results_advance (message_results))
    {
	message = notmuch_message_results_get (message_results);

	thread_id = notmuch_message_get_thread_id (message);

	if (! g_hash_table_lookup_extended (seen,
					    thread_id, NULL,
					    (void **) &thread))
	{
	    const char *subject;

	    thread = _notmuch_thread_create (query, query->notmuch,
					     thread_id);

	    subject = _notmuch_message_get_subject (message);

	    _notmuch_thread_set_subject (thread, subject);

	    g_hash_table_insert (seen, xstrdup (thread_id), thread);

	    g_ptr_array_add (thread_results->threads, thread);
	}

	for (tags = notmuch_message_get_tags (message);
	     notmuch_tags_has_more (tags);
	     notmuch_tags_advance (tags))
	{
	    tag = notmuch_tags_get (tags);
	    _notmuch_thread_add_tag (thread, tag);
	}

	notmuch_message_destroy (message);
    }

    g_hash_table_unref (seen);

    return thread_results;
}

void
notmuch_query_destroy (notmuch_query_t *query)
{
    talloc_free (query);
}

notmuch_bool_t
notmuch_message_results_has_more (notmuch_message_results_t *results)
{
    return (results->iterator != results->iterator_end);
}

notmuch_message_t *
notmuch_message_results_get (notmuch_message_results_t *results)
{
    notmuch_message_t *message;
    Xapian::docid doc_id;
    notmuch_private_status_t status;

    if (! notmuch_message_results_has_more (results))
	return NULL;

    doc_id = *results->iterator;

    message = _notmuch_message_create (results,
				       results->notmuch, doc_id,
				       &status);

    if (message == NULL &&
	status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND)
    {
	INTERNAL_ERROR ("a results iterator contains a non-existent document ID.\n");
    }

    return message;
}

void
notmuch_message_results_advance (notmuch_message_results_t *results)
{
    results->iterator++;
}

void
notmuch_message_results_destroy (notmuch_message_results_t *results)
{
    talloc_free (results);
}

notmuch_bool_t
notmuch_thread_results_has_more (notmuch_thread_results_t *results)
{
    return (results->index < results->threads->len);
}

notmuch_thread_t *
notmuch_thread_results_get (notmuch_thread_results_t *results)
{
    if (! notmuch_thread_results_has_more (results))
	return NULL;

    return (notmuch_thread_t *) g_ptr_array_index (results->threads,
						   results->index);
}

void
notmuch_thread_results_advance (notmuch_thread_results_t *results)
{
    results->index++;
}

void
notmuch_thread_results_destroy (notmuch_thread_results_t *results)
{
    talloc_free (results);
}
