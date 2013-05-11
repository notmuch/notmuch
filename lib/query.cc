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

struct _notmuch_query {
    notmuch_database_t *notmuch;
    const char *query_string;
    notmuch_sort_t sort;
    notmuch_string_list_t *exclude_terms;
    notmuch_exclude_t omit_excluded;
};

typedef struct _notmuch_mset_messages {
    notmuch_messages_t base;
    notmuch_database_t *notmuch;
    Xapian::MSetIterator iterator;
    Xapian::MSetIterator iterator_end;
} notmuch_mset_messages_t;

struct _notmuch_doc_id_set {
    unsigned char *bitmap;
    unsigned int bound;
};

#define DOCIDSET_WORD(bit) ((bit) / CHAR_BIT)
#define DOCIDSET_BIT(bit) ((bit) % CHAR_BIT)

struct visible _notmuch_threads {
    notmuch_query_t *query;

    /* The ordered list of doc ids matched by the query. */
    GArray *doc_ids;
    /* Our iterator's current position in doc_ids. */
    unsigned int doc_id_pos;
    /* The set of matched docid's that have not been assigned to a
     * thread. Initially, this contains every docid in doc_ids. */
    notmuch_doc_id_set_t match_set;
};

/* We need this in the message functions so forward declare. */
static notmuch_bool_t
_notmuch_doc_id_set_init (void *ctx,
			  notmuch_doc_id_set_t *doc_ids,
			  GArray *arr);

static notmuch_bool_t
_debug_query (void)
{
    char *env = getenv ("NOTMUCH_DEBUG_QUERY");
    return (env && strcmp (env, "") != 0);
}

notmuch_query_t *
notmuch_query_create (notmuch_database_t *notmuch,
		      const char *query_string)
{
    notmuch_query_t *query;

    if (_debug_query ())
	fprintf (stderr, "Query string is:\n%s\n", query_string);

    query = talloc (NULL, notmuch_query_t);
    if (unlikely (query == NULL))
	return NULL;

    query->notmuch = notmuch;

    query->query_string = talloc_strdup (query, query_string);

    query->sort = NOTMUCH_SORT_NEWEST_FIRST;

    query->exclude_terms = _notmuch_string_list_create (query);

    query->omit_excluded = NOTMUCH_EXCLUDE_TRUE;

    return query;
}

const char *
notmuch_query_get_query_string (notmuch_query_t *query)
{
    return query->query_string;
}

void
notmuch_query_set_omit_excluded (notmuch_query_t *query,
				 notmuch_exclude_t omit_excluded)
{
    query->omit_excluded = omit_excluded;
}

void
notmuch_query_set_sort (notmuch_query_t *query, notmuch_sort_t sort)
{
    query->sort = sort;
}

notmuch_sort_t
notmuch_query_get_sort (notmuch_query_t *query)
{
    return query->sort;
}

void
notmuch_query_add_tag_exclude (notmuch_query_t *query, const char *tag)
{
    char *term = talloc_asprintf (query, "%s%s", _find_prefix ("tag"), tag);
    _notmuch_string_list_append (query->exclude_terms, term);
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

/* Return a query that matches messages with the excluded tags
 * registered with query.  Any tags that explicitly appear in xquery
 * will not be excluded, and will be removed from the list of exclude
 * tags.  The caller of this function has to combine the returned
 * query appropriately.*/
static Xapian::Query
_notmuch_exclude_tags (notmuch_query_t *query, Xapian::Query xquery)
{
    Xapian::Query exclude_query = Xapian::Query::MatchNothing;

    for (notmuch_string_node_t *term = query->exclude_terms->head; term;
	 term = term->next) {
	Xapian::TermIterator it = xquery.get_terms_begin ();
	Xapian::TermIterator end = xquery.get_terms_end ();
	for (; it != end; it++) {
	    if ((*it).compare (term->string) == 0)
		break;
	}
	if (it == end)
	    exclude_query = Xapian::Query (Xapian::Query::OP_OR,
				    exclude_query, Xapian::Query (term->string));
	else
	    term->string = talloc_strdup (query, "");
    }
    return exclude_query;
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
	Xapian::Query string_query, final_query, exclude_query;
	Xapian::MSet mset;
	Xapian::MSetIterator iterator;
	unsigned int flags = (Xapian::QueryParser::FLAG_BOOLEAN |
			      Xapian::QueryParser::FLAG_PHRASE |
			      Xapian::QueryParser::FLAG_LOVEHATE |
			      Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE |
			      Xapian::QueryParser::FLAG_WILDCARD |
			      Xapian::QueryParser::FLAG_PURE_NOT);

	if (strcmp (query_string, "") == 0 ||
	    strcmp (query_string, "*") == 0)
	{
	    final_query = mail_query;
	} else {
	    string_query = notmuch->query_parser->
		parse_query (query_string, flags);
	    final_query = Xapian::Query (Xapian::Query::OP_AND,
					 mail_query, string_query);
	}
	messages->base.excluded_doc_ids = NULL;

	if (query->exclude_terms) {
	    exclude_query = _notmuch_exclude_tags (query, final_query);

	    if (query->omit_excluded != NOTMUCH_EXCLUDE_FALSE)
		final_query = Xapian::Query (Xapian::Query::OP_AND_NOT,
					     final_query, exclude_query);
	    else {
		exclude_query = Xapian::Query (Xapian::Query::OP_AND,
					   exclude_query, final_query);

		enquire.set_weighting_scheme (Xapian::BoolWeight());
		enquire.set_query (exclude_query);

		mset = enquire.get_mset (0, notmuch->xapian_db->get_doccount ());

		GArray *excluded_doc_ids = g_array_new (FALSE, FALSE, sizeof (unsigned int));

		for (iterator = mset.begin (); iterator != mset.end (); iterator++) {
		    unsigned int doc_id = *iterator;
		    g_array_append_val (excluded_doc_ids, doc_id);
		}
		messages->base.excluded_doc_ids = talloc (messages, _notmuch_doc_id_set);
		_notmuch_doc_id_set_init (query, messages->base.excluded_doc_ids,
					  excluded_doc_ids);
		g_array_unref (excluded_doc_ids);
	    }
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
        case NOTMUCH_SORT_UNSORTED:
	    break;
	}

	if (_debug_query ()) {
	    fprintf (stderr, "Exclude query is:\n%s\n",
		     exclude_query.get_description ().c_str ());
	    fprintf (stderr, "Final query is:\n%s\n",
		     final_query.get_description ().c_str ());
	}

	enquire.set_query (final_query);

	mset = enquire.get_mset (0, notmuch->xapian_db->get_doccount ());

	messages->iterator = mset.begin ();
	messages->iterator_end = mset.end ();

	return &messages->base;

    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred performing query: %s\n",
		 error.get_msg().c_str());
	fprintf (stderr, "Query string was: %s\n", query->query_string);
	notmuch->exception_reported = TRUE;
	talloc_free (messages);
	return NULL;
    }
}

notmuch_bool_t
_notmuch_mset_messages_valid (notmuch_messages_t *messages)
{
    notmuch_mset_messages_t *mset_messages;

    mset_messages = (notmuch_mset_messages_t *) messages;

    return (mset_messages->iterator != mset_messages->iterator_end);
}

static Xapian::docid
_notmuch_mset_messages_get_doc_id (notmuch_messages_t *messages)
{
    notmuch_mset_messages_t *mset_messages;

    mset_messages = (notmuch_mset_messages_t *) messages;

    if (! _notmuch_mset_messages_valid (&mset_messages->base))
	return 0;

    return *mset_messages->iterator;
}

notmuch_message_t *
_notmuch_mset_messages_get (notmuch_messages_t *messages)
{
    notmuch_message_t *message;
    Xapian::docid doc_id;
    notmuch_private_status_t status;
    notmuch_mset_messages_t *mset_messages;

    mset_messages = (notmuch_mset_messages_t *) messages;

    if (! _notmuch_mset_messages_valid (&mset_messages->base))
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

    if (messages->excluded_doc_ids &&
	_notmuch_doc_id_set_contains (messages->excluded_doc_ids, doc_id))
	notmuch_message_set_flag (message, NOTMUCH_MESSAGE_FLAG_EXCLUDED, TRUE);

    return message;
}

void
_notmuch_mset_messages_move_to_next (notmuch_messages_t *messages)
{
    notmuch_mset_messages_t *mset_messages;

    mset_messages = (notmuch_mset_messages_t *) messages;

    mset_messages->iterator++;
}

static notmuch_bool_t
_notmuch_doc_id_set_init (void *ctx,
			  notmuch_doc_id_set_t *doc_ids,
			  GArray *arr)
{
    unsigned int max = 0;
    unsigned char *bitmap;

    for (unsigned int i = 0; i < arr->len; i++)
	max = MAX(max, g_array_index (arr, unsigned int, i));
    bitmap = talloc_zero_array (ctx, unsigned char, DOCIDSET_WORD(max) + 1);

    if (bitmap == NULL)
	return FALSE;

    doc_ids->bitmap = bitmap;
    doc_ids->bound = max + 1;

    for (unsigned int i = 0; i < arr->len; i++) {
	unsigned int doc_id = g_array_index (arr, unsigned int, i);
	bitmap[DOCIDSET_WORD(doc_id)] |= 1 << DOCIDSET_BIT(doc_id);
    }

    return TRUE;
}

notmuch_bool_t
_notmuch_doc_id_set_contains (notmuch_doc_id_set_t *doc_ids,
			      unsigned int doc_id)
{
    if (doc_id >= doc_ids->bound)
	return FALSE;
    return doc_ids->bitmap[DOCIDSET_WORD(doc_id)] & (1 << DOCIDSET_BIT(doc_id));
}

void
_notmuch_doc_id_set_remove (notmuch_doc_id_set_t *doc_ids,
                            unsigned int doc_id)
{
    if (doc_id < doc_ids->bound)
	doc_ids->bitmap[DOCIDSET_WORD(doc_id)] &= ~(1 << DOCIDSET_BIT(doc_id));
}

/* Glib objects force use to use a talloc destructor as well, (but not
 * nearly as ugly as the for messages due to C++ objects). At
 * this point, I'd really like to have some talloc-friendly
 * equivalents for the few pieces of glib that I'm using. */
static int
_notmuch_threads_destructor (notmuch_threads_t *threads)
{
    if (threads->doc_ids)
	g_array_unref (threads->doc_ids);

    return 0;
}

notmuch_threads_t *
notmuch_query_search_threads (notmuch_query_t *query)
{
    notmuch_threads_t *threads;
    notmuch_messages_t *messages;

    threads = talloc (query, notmuch_threads_t);
    if (threads == NULL)
	return NULL;
    threads->doc_ids = NULL;
    talloc_set_destructor (threads, _notmuch_threads_destructor);

    threads->query = query;

    messages = notmuch_query_search_messages (query);
    if (messages == NULL) {
	    talloc_free (threads);
	    return NULL;
    }

    threads->doc_ids = g_array_new (FALSE, FALSE, sizeof (unsigned int));
    while (notmuch_messages_valid (messages)) {
	unsigned int doc_id = _notmuch_mset_messages_get_doc_id (messages);
	g_array_append_val (threads->doc_ids, doc_id);
	notmuch_messages_move_to_next (messages);
    }
    threads->doc_id_pos = 0;

    talloc_free (messages);

    if (! _notmuch_doc_id_set_init (threads, &threads->match_set,
				    threads->doc_ids)) {
	talloc_free (threads);
	return NULL;
    }

    return threads;
}

void
notmuch_query_destroy (notmuch_query_t *query)
{
    talloc_free (query);
}

notmuch_bool_t
notmuch_threads_valid (notmuch_threads_t *threads)
{
    unsigned int doc_id;

    while (threads->doc_id_pos < threads->doc_ids->len) {
	doc_id = g_array_index (threads->doc_ids, unsigned int,
				threads->doc_id_pos);
	if (_notmuch_doc_id_set_contains (&threads->match_set, doc_id))
	    break;

	threads->doc_id_pos++;
    }

    return threads->doc_id_pos < threads->doc_ids->len;
}

notmuch_thread_t *
notmuch_threads_get (notmuch_threads_t *threads)
{
    unsigned int doc_id;

    if (! notmuch_threads_valid (threads))
	return NULL;

    doc_id = g_array_index (threads->doc_ids, unsigned int,
			    threads->doc_id_pos);
    return _notmuch_thread_create (threads->query,
				   threads->query->notmuch,
				   doc_id,
				   &threads->match_set,
				   threads->query->exclude_terms,
				   threads->query->omit_excluded,
				   threads->query->sort);
}

void
notmuch_threads_move_to_next (notmuch_threads_t *threads)
{
    threads->doc_id_pos++;
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
	Xapian::Query string_query, final_query, exclude_query;
	Xapian::MSet mset;
	unsigned int flags = (Xapian::QueryParser::FLAG_BOOLEAN |
			      Xapian::QueryParser::FLAG_PHRASE |
			      Xapian::QueryParser::FLAG_LOVEHATE |
			      Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE |
			      Xapian::QueryParser::FLAG_WILDCARD |
			      Xapian::QueryParser::FLAG_PURE_NOT);

	if (strcmp (query_string, "") == 0 ||
	    strcmp (query_string, "*") == 0)
	{
	    final_query = mail_query;
	} else {
	    string_query = notmuch->query_parser->
		parse_query (query_string, flags);
	    final_query = Xapian::Query (Xapian::Query::OP_AND,
					 mail_query, string_query);
	}

	exclude_query = _notmuch_exclude_tags (query, final_query);

	final_query = Xapian::Query (Xapian::Query::OP_AND_NOT,
					 final_query, exclude_query);

	enquire.set_weighting_scheme(Xapian::BoolWeight());
	enquire.set_docid_order(Xapian::Enquire::ASCENDING);

	if (_debug_query ()) {
	    fprintf (stderr, "Exclude query is:\n%s\n",
		     exclude_query.get_description ().c_str ());
	    fprintf (stderr, "Final query is:\n%s\n",
		     final_query.get_description ().c_str ());
	}

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

unsigned
notmuch_query_count_threads (notmuch_query_t *query)
{
    notmuch_messages_t *messages;
    GHashTable *hash;
    unsigned int count;
    notmuch_sort_t sort;

    sort = query->sort;
    query->sort = NOTMUCH_SORT_UNSORTED;
    messages = notmuch_query_search_messages (query);
    query->sort = sort;
    if (messages == NULL)
	return 0;

    hash = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, NULL);
    if (hash == NULL) {
	talloc_free (messages);
	return 0;
    }

    while (notmuch_messages_valid (messages)) {
	notmuch_message_t *message = notmuch_messages_get (messages);
	const char *thread_id = notmuch_message_get_thread_id (message);
	char *thread_id_copy = talloc_strdup (messages, thread_id);
	if (unlikely (thread_id_copy == NULL)) {
	    notmuch_message_destroy (message);
	    count = 0;
	    goto DONE;
	}
	g_hash_table_insert (hash, thread_id_copy, NULL);
	notmuch_message_destroy (message);
	notmuch_messages_move_to_next (messages);
    }

    count = g_hash_table_size (hash);

  DONE:
    g_hash_table_unref (hash);
    talloc_free (messages);

    return count;
}
