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

#include <xapian.h>

const char *NOTMUCH_QUERY_ALL = "";

struct _notmuch_query {
    notmuch_database_t *notmuch;
    const char *query_string;
    notmuch_sort_t sort;
};

struct _notmuch_results {
    notmuch_database_t *notmuch;
    Xapian::PostingIterator iterator;
    Xapian::PostingIterator iterator_end;
};

notmuch_query_t *
notmuch_query_create (notmuch_database_t *notmuch,
		      const char *query_string)
{
    notmuch_query_t *query;

    query = talloc (NULL, notmuch_query_t);
    if (unlikely (query == NULL))
	return NULL;

    query->notmuch = notmuch;

    /* Special-case NOTMUCH_QUERY_ALL so we see it and not a copy. */
    if (query_string == NOTMUCH_QUERY_ALL)
	query->query_string = query_string;
    else
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
_notmuch_results_destructor (notmuch_results_t *results)
{
    results->iterator.~PostingIterator ();
    results->iterator_end.~PostingIterator ();

    return 0;
}

notmuch_results_t *
notmuch_query_search (notmuch_query_t *query)
{
    notmuch_results_t *results;

    results = talloc (query, notmuch_results_t);
    if (unlikely (results == NULL))
	return NULL;

    try {
	if (query->query_string != NOTMUCH_QUERY_ALL) {
	    fprintf (stderr, "Error: Arbitrary search strings are not supported yet. Come back soon!\n");
	    exit (1);
	}

	results->notmuch = query->notmuch;
	new (&results->iterator) Xapian::PostingIterator ();
	new (&results->iterator_end) Xapian::PostingIterator ();

	talloc_set_destructor (results, _notmuch_results_destructor);

	results->iterator = query->notmuch->xapian_db->postlist_begin ("");
	results->iterator_end = query->notmuch->xapian_db->postlist_end ("");

    } catch (const Xapian::Error &error) {
	fprintf (stderr, "A Xapian exception occurred: %s\n",
		 error.get_msg().c_str());
    }

    return results;
}

void
notmuch_query_destroy (notmuch_query_t *query)
{
    talloc_free (query);
}

notmuch_bool_t
notmuch_results_has_more (notmuch_results_t *results)
{
    return (results->iterator != results->iterator_end);
}

notmuch_message_t *
notmuch_results_get (notmuch_results_t *results)
{
    Xapian::docid doc_id;

    doc_id = *results->iterator;

    return _notmuch_message_create (results,
				    results->notmuch, doc_id);
}

void
notmuch_results_advance (notmuch_results_t *results)
{
    results->iterator++;
}

void
notmuch_results_destroy (notmuch_results_t *results)
{
    talloc_free (results);
}
