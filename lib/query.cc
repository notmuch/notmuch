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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
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
    notmuch_bool_t parsed;
    Xapian::Query xapian_query;
    std::set<std::string> terms;
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

struct _notmuch_threads {
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

/* Explicit destructor call for placement new */
static int
_notmuch_query_destructor (notmuch_query_t *query) {
    query->xapian_query.~Query();
    query->terms.~set<std::string>();
    return 0;
}

notmuch_query_t *
notmuch_query_create (notmuch_database_t *notmuch,
		      const char *query_string)
{
    notmuch_query_t *query;

    if (_debug_query ())
	fprintf (stderr, "Query string is:\n%s\n", query_string);

    query = talloc (notmuch, notmuch_query_t);
    if (unlikely (query == NULL))
	return NULL;

    new (&query->xapian_query) Xapian::Query ();
    new (&query->terms) std::set<std::string> ();
    query->parsed = FALSE;

    talloc_set_destructor (query, _notmuch_query_destructor);

    query->notmuch = notmuch;

    query->query_string = talloc_strdup (query, query_string);

    query->sort = NOTMUCH_SORT_NEWEST_FIRST;

    query->exclude_terms = _notmuch_string_list_create (query);

    query->omit_excluded = NOTMUCH_EXCLUDE_TRUE;

    return query;
}

static notmuch_status_t
_notmuch_query_ensure_parsed (notmuch_query_t *query)
{
    if (query->parsed)
	return NOTMUCH_STATUS_SUCCESS;

    try {
	query->xapian_query =
	    query->notmuch->query_parser->
		parse_query (query->query_string, NOTMUCH_QUERY_PARSER_FLAGS);

       /* Xapian doesn't support skip_to on terms from a query since
	*  they are unordered, so cache a copy of all terms in
	*  something searchable.
	*/

	for (Xapian::TermIterator t = query->xapian_query.get_terms_begin ();
	     t != query->xapian_query.get_terms_end (); ++t)
	    query->terms.insert (*t);

	query->parsed = TRUE;

    } catch (const Xapian::Error &error) {
	if (!query->notmuch->exception_reported) {
	    _notmuch_database_log (query->notmuch,
				   "A Xapian exception occurred parsing query: %s\n",
				   error.get_msg ().c_str ());
	    _notmuch_database_log_append (query->notmuch,
					  "Query string was: %s\n",
					  query->query_string);
	    query->notmuch->exception_reported = TRUE;
	}

	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
    return NOTMUCH_STATUS_SUCCESS;
}

const char *
notmuch_query_get_query_string (const notmuch_query_t *query)
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
notmuch_query_get_sort (const notmuch_query_t *query)
{
    return query->sort;
}

notmuch_status_t
notmuch_query_add_tag_exclude (notmuch_query_t *query, const char *tag)
{
    notmuch_status_t status;
    char *term;

    status = _notmuch_query_ensure_parsed (query);
    if (status)
	return status;

    term = talloc_asprintf (query, "%s%s", _find_prefix ("tag"), tag);
    if (query->terms.count(term) != 0)
	return NOTMUCH_STATUS_IGNORED;

    _notmuch_string_list_append (query->exclude_terms, term);
    return NOTMUCH_STATUS_SUCCESS;
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
 * registered with query. The caller of this function has to combine the returned
 * query appropriately.*/
static Xapian::Query
_notmuch_exclude_tags (notmuch_query_t *query)
{
    Xapian::Query exclude_query = Xapian::Query::MatchNothing;

    for (notmuch_string_node_t *term = query->exclude_terms->head; term;
	 term = term->next) {
	exclude_query = Xapian::Query (Xapian::Query::OP_OR,
				       exclude_query, Xapian::Query (term->string));
    }
    return exclude_query;
}


notmuch_status_t
notmuch_query_search_messages_st (notmuch_query_t *query,
				  notmuch_messages_t **out)
{
    return notmuch_query_search_messages (query, out);
}

notmuch_status_t
notmuch_query_search_messages (notmuch_query_t *query,
				  notmuch_messages_t **out)
{
    return _notmuch_query_search_documents (query, "mail", out);
}

notmuch_status_t
_notmuch_query_search_documents (notmuch_query_t *query,
				 const char *type,
				 notmuch_messages_t **out)
{
    notmuch_database_t *notmuch = query->notmuch;
    const char *query_string = query->query_string;
    notmuch_mset_messages_t *messages;
    notmuch_status_t status;

    status = _notmuch_query_ensure_parsed (query);
    if (status)
	return status;

    messages = talloc (query, notmuch_mset_messages_t);
    if (unlikely (messages == NULL))
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

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
						   type));
	Xapian::Query final_query, exclude_query;
	Xapian::MSet mset;
	Xapian::MSetIterator iterator;

	if (strcmp (query_string, "") == 0 ||
	    strcmp (query_string, "*") == 0)
	{
	    final_query = mail_query;
	} else {
	    final_query = Xapian::Query (Xapian::Query::OP_AND,
					 mail_query, query->xapian_query);
	}
	messages->base.excluded_doc_ids = NULL;

	if ((query->omit_excluded != NOTMUCH_EXCLUDE_FALSE) && (query->exclude_terms)) {
	    exclude_query = _notmuch_exclude_tags (query);

	    if (query->omit_excluded == NOTMUCH_EXCLUDE_TRUE ||
		query->omit_excluded == NOTMUCH_EXCLUDE_ALL)
	    {
		final_query = Xapian::Query (Xapian::Query::OP_AND_NOT,
					     final_query, exclude_query);
	    } else { /* NOTMUCH_EXCLUDE_FLAG */
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

	*out = &messages->base;
	return NOTMUCH_STATUS_SUCCESS;

    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch,
			       "A Xapian exception occurred performing query: %s\n",
			       error.get_msg().c_str());
	_notmuch_database_log_append (notmuch,
			       "Query string was: %s\n",
			       query->query_string);

	notmuch->exception_reported = TRUE;
	talloc_free (messages);
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
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

notmuch_status_t
notmuch_query_search_threads_st (notmuch_query_t *query, notmuch_threads_t **out)
{
    return notmuch_query_search_threads(query, out);
}

notmuch_status_t
notmuch_query_search_threads (notmuch_query_t *query,
			      notmuch_threads_t **out)
{
    notmuch_threads_t *threads;
    notmuch_messages_t *messages;
    notmuch_status_t status;

    threads = talloc (query, notmuch_threads_t);
    if (threads == NULL)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;
    threads->doc_ids = NULL;
    talloc_set_destructor (threads, _notmuch_threads_destructor);

    threads->query = query;

    status = notmuch_query_search_messages (query, &messages);
    if (status) {
	talloc_free (threads);
	return status;
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
	return NOTMUCH_STATUS_OUT_OF_MEMORY;
    }

    *out = threads;
    return NOTMUCH_STATUS_SUCCESS;
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

    if (! threads)
	return FALSE;

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

notmuch_status_t
notmuch_query_count_messages_st (notmuch_query_t *query, unsigned *count_out)
{
    return notmuch_query_count_messages (query, count_out);
}

notmuch_status_t
notmuch_query_count_messages (notmuch_query_t *query, unsigned *count_out)
{
    return _notmuch_query_count_documents (query, "mail", count_out);
}

notmuch_status_t
_notmuch_query_count_documents (notmuch_query_t *query, const char *type, unsigned *count_out)
{
    notmuch_database_t *notmuch = query->notmuch;
    const char *query_string = query->query_string;
    Xapian::doccount count = 0;
    notmuch_status_t status;

    status = _notmuch_query_ensure_parsed (query);
    if (status)
	return status;

    try {
	Xapian::Enquire enquire (*notmuch->xapian_db);
	Xapian::Query mail_query (talloc_asprintf (query, "%s%s",
						   _find_prefix ("type"),
						   type));
	Xapian::Query final_query, exclude_query;
	Xapian::MSet mset;

	if (strcmp (query_string, "") == 0 ||
	    strcmp (query_string, "*") == 0)
	{
	    final_query = mail_query;
	} else {
	    final_query = Xapian::Query (Xapian::Query::OP_AND,
					 mail_query, query->xapian_query);
	}

	exclude_query = _notmuch_exclude_tags (query);

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

	/*
	 * Set the checkatleast parameter to the number of documents
	 * in the database to make get_matches_estimated() exact.
	 * Set the max parameter to 0 to avoid fetching documents we will discard.
	 */
	mset = enquire.get_mset (0, 0,
				 notmuch->xapian_db->get_doccount ());

	count = mset.get_matches_estimated();

    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch,
			       "A Xapian exception occurred performing query: %s\n",
			       error.get_msg().c_str());
	_notmuch_database_log_append (notmuch,
				      "Query string was: %s\n",
				      query->query_string);
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    *count_out = count;
    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_query_count_threads_st (notmuch_query_t *query, unsigned *count)
{
    return notmuch_query_count_threads (query, count);
}

notmuch_status_t
notmuch_query_count_threads (notmuch_query_t *query, unsigned *count)
{
    notmuch_messages_t *messages;
    GHashTable *hash;
    notmuch_sort_t sort;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    sort = query->sort;
    query->sort = NOTMUCH_SORT_UNSORTED;
    ret = notmuch_query_search_messages (query, &messages);
    if (ret)
	return ret;
    query->sort = sort;
    if (messages == NULL)
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;

    hash = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, NULL);
    if (hash == NULL) {
	talloc_free (messages);
	return NOTMUCH_STATUS_OUT_OF_MEMORY;
    }

    while (notmuch_messages_valid (messages)) {
	notmuch_message_t *message = notmuch_messages_get (messages);
	const char *thread_id = notmuch_message_get_thread_id (message);
	char *thread_id_copy = talloc_strdup (messages, thread_id);
	if (unlikely (thread_id_copy == NULL)) {
	    notmuch_message_destroy (message);
	    ret = NOTMUCH_STATUS_OUT_OF_MEMORY;
	    goto DONE;
	}
	g_hash_table_insert (hash, thread_id_copy, NULL);
	notmuch_message_destroy (message);
	notmuch_messages_move_to_next (messages);
    }

    *count = g_hash_table_size (hash);

  DONE:
    g_hash_table_unref (hash);
    talloc_free (messages);

    return ret;
}

notmuch_database_t *
notmuch_query_get_database (const notmuch_query_t *query)
{
    return query->notmuch;
}
