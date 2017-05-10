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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private.h"
#include "database-private.h"

#include <gmime/gmime.h>
#include <glib.h> /* GHashTable */

#define EMPTY_STRING(s) ((s)[0] == '\0')

struct _notmuch_thread {
    notmuch_database_t *notmuch;
    char *thread_id;
    char *subject;
    GHashTable *authors_hash;
    GPtrArray *authors_array;
    GHashTable *matched_authors_hash;
    GPtrArray *matched_authors_array;
    char *authors;
    GHashTable *tags;

    /* All messages, oldest first. */
    notmuch_message_list_t *message_list;
    /* Top-level messages, oldest first. */
    notmuch_message_list_t *toplevel_list;

    GHashTable *message_hash;
    int total_messages;
    int matched_messages;
    time_t oldest;
    time_t newest;
};

static int
_notmuch_thread_destructor (notmuch_thread_t *thread)
{
    g_hash_table_unref (thread->authors_hash);
    g_hash_table_unref (thread->matched_authors_hash);
    g_hash_table_unref (thread->tags);
    g_hash_table_unref (thread->message_hash);

    if (thread->authors_array) {
	g_ptr_array_free (thread->authors_array, TRUE);
	thread->authors_array = NULL;
    }

    if (thread->matched_authors_array) {
	g_ptr_array_free (thread->matched_authors_array, TRUE);
	thread->matched_authors_array = NULL;
    }

    return 0;
}

/* Add each author of the thread to the thread's authors_hash and to
 * the thread's authors_array. */
static void
_thread_add_author (notmuch_thread_t *thread,
		    const char *author)
{
    char *author_copy;

    if (author == NULL)
	return;

    if (g_hash_table_lookup_extended (thread->authors_hash,
				      author, NULL, NULL))
	return;

    author_copy = talloc_strdup (thread, author);

    g_hash_table_insert (thread->authors_hash, author_copy, NULL);

    g_ptr_array_add (thread->authors_array, author_copy);
}

/* Add each matched author of the thread to the thread's
 * matched_authors_hash and to the thread's matched_authors_array. */
static void
_thread_add_matched_author (notmuch_thread_t *thread,
			    const char *author)
{
    char *author_copy;

    if (author == NULL)
	return;

    if (g_hash_table_lookup_extended (thread->matched_authors_hash,
				      author, NULL, NULL))
	return;

    author_copy = talloc_strdup (thread, author);

    g_hash_table_insert (thread->matched_authors_hash, author_copy, NULL);

    g_ptr_array_add (thread->matched_authors_array, author_copy);
}

/* Construct an authors string from matched_authors_array and
 * authors_array. The string contains matched authors first, then
 * non-matched authors (with the two groups separated by '|'). Within
 * each group, authors are listed in date order. */
static void
_resolve_thread_authors_string (notmuch_thread_t *thread)
{
    unsigned int i;
    char *author;
    int first_non_matched_author = 1;

    /* First, list all matched authors in date order. */
    for (i = 0; i < thread->matched_authors_array->len; i++) {
	author = (char *) g_ptr_array_index (thread->matched_authors_array, i);
	if (thread->authors)
	    thread->authors = talloc_asprintf (thread, "%s, %s",
					       thread->authors,
					       author);
	else
	    thread->authors = author;
    }

    /* Next, append any non-matched authors that haven't already appeared. */
    for (i = 0; i < thread->authors_array->len; i++) {
	author = (char *) g_ptr_array_index (thread->authors_array, i);
	if (g_hash_table_lookup_extended (thread->matched_authors_hash,
					  author, NULL, NULL))
	    continue;
	if (first_non_matched_author) {
	    thread->authors = talloc_asprintf (thread, "%s| %s",
					       thread->authors,
					       author);
	} else {
	    thread->authors = talloc_asprintf (thread, "%s, %s",
					       thread->authors,
					       author);
	}

	first_non_matched_author = 0;
    }

    g_ptr_array_free (thread->authors_array, TRUE);
    thread->authors_array = NULL;
    g_ptr_array_free (thread->matched_authors_array, TRUE);
    thread->matched_authors_array = NULL;
}

/* clean up the ugly "Lastname, Firstname" format that some mail systems
 * (most notably, Exchange) are creating to be "Firstname Lastname"
 * To make sure that we don't change other potential situations where a
 * comma is in the name, we check that we match one of these patterns
 * "Last, First" <first.last@company.com>
 * "Last, First MI" <first.mi.last@company.com>
 */
static char *
_thread_cleanup_author (notmuch_thread_t *thread,
			const char *author, const char *from)
{
    char *clean_author,*test_author;
    const char *comma;
    char *blank;
    int fname,lname;

    if (author == NULL)
	return NULL;
    clean_author = talloc_strdup(thread, author);
    if (clean_author == NULL)
	return NULL;
    /* check if there's a comma in the name and that there's a
     * component of the name behind it (so the name doesn't end with
     * the comma - in which case the string that strchr finds is just
     * one character long ",\0").
     * Otherwise just return the copy of the original author name that
     * we just made*/
    comma = strchr(author,',');
    if (comma && strlen(comma) > 1) {
	/* let's assemble what we think is the correct name */
	lname = comma - author;

	/* Skip all the spaces after the comma */
	fname = strlen(author) - lname - 1;
	comma += 1;
	while (*comma == ' ') {
	    fname -= 1;
	    comma += 1;
	}
	strncpy(clean_author, comma, fname);

	*(clean_author+fname) = ' ';
	strncpy(clean_author + fname + 1, author, lname);
	*(clean_author+fname+1+lname) = '\0';
	/* make a temporary copy and see if it matches the email */
	test_author = talloc_strdup(thread,clean_author);

	blank=strchr(test_author,' ');
	while (blank != NULL) {
	    *blank = '.';
	    blank=strchr(test_author,' ');
	}
	if (strcasestr(from, test_author) == NULL)
	    /* we didn't identify this as part of the email address
	    * so let's punt and return the original author */
	    strcpy (clean_author, author);
    }
    return clean_author;
}

/* Add 'message' as a message that belongs to 'thread'.
 *
 * The 'thread' will talloc_steal the 'message' and hold onto a
 * reference to it.
 */
static void
_thread_add_message (notmuch_thread_t *thread,
		     notmuch_message_t *message,
		     notmuch_string_list_t *exclude_terms,
		     notmuch_exclude_t omit_exclude)
{
    notmuch_tags_t *tags;
    const char *tag;
    InternetAddressList *list = NULL;
    InternetAddress *address;
    const char *from, *author;
    char *clean_author;
    notmuch_bool_t message_excluded = FALSE;

    if (omit_exclude != NOTMUCH_EXCLUDE_FALSE) {
	for (tags = notmuch_message_get_tags (message);
	     notmuch_tags_valid (tags);
	     notmuch_tags_move_to_next (tags))
	{
	    tag = notmuch_tags_get (tags);
	    /* Is message excluded? */
	    for (notmuch_string_node_t *term = exclude_terms->head;
		 term != NULL;
		 term = term->next)
	    {
		/* Check for an empty string, and then ignore initial 'K'. */
		if (*(term->string) && strcmp(tag, (term->string + 1)) == 0) {
		    message_excluded = TRUE;
		    break;
		}
	    }
	}
    }

    if (message_excluded && omit_exclude == NOTMUCH_EXCLUDE_ALL)
	return;

    _notmuch_message_list_add_message (thread->message_list,
				       talloc_steal (thread, message));
    thread->total_messages++;

    g_hash_table_insert (thread->message_hash,
			 xstrdup (notmuch_message_get_message_id (message)),
			 message);

    from = notmuch_message_get_header (message, "from");
    if (from)
	list = internet_address_list_parse_string (from);

    if (list) {
	address = internet_address_list_get_address (list, 0);
	if (address) {
	    author = internet_address_get_name (address);
	    /* We treat quoted empty names as if they were empty. */
	    if (author == NULL || author[0] == '\0') {
		InternetAddressMailbox *mailbox;
		mailbox = INTERNET_ADDRESS_MAILBOX (address);
		author = internet_address_mailbox_get_addr (mailbox);
	    }
	    clean_author = _thread_cleanup_author (thread, author, from);
	    _thread_add_author (thread, clean_author);
	    _notmuch_message_set_author (message, clean_author);
	}
	g_object_unref (G_OBJECT (list));
    }

    if (! thread->subject) {
	const char *subject;
	subject = notmuch_message_get_header (message, "subject");
	thread->subject = talloc_strdup (thread, subject ? subject : "");
    }

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags))
    {
	tag = notmuch_tags_get (tags);
	g_hash_table_insert (thread->tags, xstrdup (tag), NULL);
    }

    /* Mark excluded messages. */
    if (message_excluded)
	notmuch_message_set_flag (message, NOTMUCH_MESSAGE_FLAG_EXCLUDED, TRUE);
}

static void
_thread_set_subject_from_message (notmuch_thread_t *thread,
				  notmuch_message_t *message)
{
    const char *subject;
    const char *cleaned_subject;

    subject = notmuch_message_get_header (message, "subject");
    if (! subject)
	return;

    if ((strncasecmp (subject, "Re: ", 4) == 0) ||
	(strncasecmp (subject, "Aw: ", 4) == 0) ||
	(strncasecmp (subject, "Vs: ", 4) == 0) ||
	(strncasecmp (subject, "Sv: ", 4) == 0)) {

	cleaned_subject = talloc_strndup (thread,
					  subject + 4,
					  strlen(subject) - 4);
    } else {
	cleaned_subject = talloc_strdup (thread, subject);
    }

    if (! EMPTY_STRING(cleaned_subject)) {
	if (thread->subject)
	    talloc_free (thread->subject);

	thread->subject = talloc_strdup (thread, cleaned_subject);
    }
}

/* Add a message to this thread which is known to match the original
 * search specification. The 'sort' parameter controls whether the
 * oldest or newest matching subject is applied to the thread as a
 * whole. */
static void
_thread_add_matched_message (notmuch_thread_t *thread,
			     notmuch_message_t *message,
			     notmuch_sort_t sort)
{
    time_t date;
    notmuch_message_t *hashed_message;

    date = notmuch_message_get_date (message);

    if (date < thread->oldest || ! thread->matched_messages) {
	thread->oldest = date;
	if (sort == NOTMUCH_SORT_OLDEST_FIRST)
	    _thread_set_subject_from_message (thread, message);
    }

    if (date > thread->newest || ! thread->matched_messages) {
	thread->newest = date;
	const char *cur_subject = notmuch_thread_get_subject(thread);
	if (sort != NOTMUCH_SORT_OLDEST_FIRST || EMPTY_STRING(cur_subject))
	    _thread_set_subject_from_message (thread, message);
    }

    if (!notmuch_message_get_flag (message, NOTMUCH_MESSAGE_FLAG_EXCLUDED))
	thread->matched_messages++;

    if (g_hash_table_lookup_extended (thread->message_hash,
			    notmuch_message_get_message_id (message), NULL,
			    (void **) &hashed_message)) {
	notmuch_message_set_flag (hashed_message,
				  NOTMUCH_MESSAGE_FLAG_MATCH, 1);
    }

    _thread_add_matched_author (thread, _notmuch_message_get_author (hashed_message));
}

static void
_resolve_thread_relationships (notmuch_thread_t *thread)
{
    notmuch_message_node_t *node;
    notmuch_message_t *message, *parent;
    const char *in_reply_to;

    for (node = thread->message_list->head; node; node = node->next) {
	message = node->message;
	in_reply_to = _notmuch_message_get_in_reply_to (message);
	if (in_reply_to && strlen (in_reply_to) &&
	    g_hash_table_lookup_extended (thread->message_hash,
					  in_reply_to, NULL,
					  (void **) &parent))
	    _notmuch_message_add_reply (parent, message);
	else
	    _notmuch_message_list_add_message (thread->toplevel_list, message);
    }

    /* XXX: After scanning through the entire list looking for parents
     * via "In-Reply-To", we should do a second pass that looks at the
     * list of messages IDs in the "References" header instead. (And
     * for this the parent would be the "deepest" message of all the
     * messages found in the "References" list.)
     *
     * Doing this will allow messages and sub-threads to be positioned
     * correctly in the thread even when an intermediate message is
     * missing from the thread.
     */
}

/* Create a new notmuch_thread_t object by finding the thread
 * containing the message with the given doc ID, treating any messages
 * contained in match_set as "matched".  Remove all messages in the
 * thread from match_set.
 *
 * Creating the thread will perform a database search to get all
 * messages belonging to the thread and will get the first subject
 * line, the total count of messages, and all authors in the thread.
 * Each message in the thread is checked against match_set to allow
 * for a separate count of matched messages, and to allow a viewer to
 * display these messages differently.
 *
 * Here, 'ctx' is talloc context for the resulting thread object.
 *
 * This function returns NULL in the case of any error.
 */
notmuch_thread_t *
_notmuch_thread_create (void *ctx,
			notmuch_database_t *notmuch,
			unsigned int seed_doc_id,
			notmuch_doc_id_set_t *match_set,
			notmuch_string_list_t *exclude_terms,
			notmuch_exclude_t omit_excluded,
			notmuch_sort_t sort)
{
    void *local = talloc_new (ctx);
    notmuch_thread_t *thread = NULL;
    notmuch_message_t *seed_message;
    const char *thread_id;
    char *thread_id_query_string;
    notmuch_query_t *thread_id_query;

    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_status_t status;

    seed_message = _notmuch_message_create (local, notmuch, seed_doc_id, NULL);
    if (! seed_message)
	INTERNAL_ERROR ("Thread seed message %u does not exist", seed_doc_id);

    thread_id = notmuch_message_get_thread_id (seed_message);
    thread_id_query_string = talloc_asprintf (local, "thread:%s", thread_id);
    if (unlikely (thread_id_query_string == NULL))
	goto DONE;

    thread_id_query = talloc_steal (
	local, notmuch_query_create (notmuch, thread_id_query_string));
    if (unlikely (thread_id_query == NULL))
	goto DONE;

    thread = talloc (local, notmuch_thread_t);
    if (unlikely (thread == NULL))
	goto DONE;

    talloc_set_destructor (thread, _notmuch_thread_destructor);

    thread->notmuch = notmuch;
    thread->thread_id = talloc_strdup (thread, thread_id);
    thread->subject = NULL;
    thread->authors_hash = g_hash_table_new_full (g_str_hash, g_str_equal,
						  NULL, NULL);
    thread->authors_array = g_ptr_array_new ();
    thread->matched_authors_hash = g_hash_table_new_full (g_str_hash,
							  g_str_equal,
							  NULL, NULL);
    thread->matched_authors_array = g_ptr_array_new ();
    thread->authors = NULL;
    thread->tags = g_hash_table_new_full (g_str_hash, g_str_equal,
					  free, NULL);

    thread->message_list = _notmuch_message_list_create (thread);
    thread->toplevel_list = _notmuch_message_list_create (thread);
    if (unlikely (thread->message_list == NULL ||
		  thread->toplevel_list == NULL)) {
	thread = NULL;
	goto DONE;
    }

    thread->message_hash = g_hash_table_new_full (g_str_hash, g_str_equal,
						  free, NULL);

    thread->total_messages = 0;
    thread->matched_messages = 0;
    thread->oldest = 0;
    thread->newest = 0;

    /* We use oldest-first order unconditionally here to obtain the
     * proper author ordering for the thread. The 'sort' parameter
     * passed to this function is used only to indicate whether the
     * oldest or newest subject is desired. */
    notmuch_query_set_sort (thread_id_query, NOTMUCH_SORT_OLDEST_FIRST);

    status = notmuch_query_search_messages (thread_id_query, &messages);
    if (status)
	goto DONE;

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages))
    {
	unsigned int doc_id;

	message = notmuch_messages_get (messages);
	doc_id = _notmuch_message_get_doc_id (message);
	if (doc_id == seed_doc_id)
	    message = seed_message;

	_thread_add_message (thread, message, exclude_terms, omit_excluded);

	if ( _notmuch_doc_id_set_contains (match_set, doc_id)) {
	    _notmuch_doc_id_set_remove (match_set, doc_id);
	    _thread_add_matched_message (thread, message, sort);
	}

	_notmuch_message_close (message);
    }

    _resolve_thread_authors_string (thread);

    _resolve_thread_relationships (thread);

    /* Commit to returning thread. */
    (void) talloc_steal (ctx, thread);

  DONE:
    talloc_free (local);
    return thread;
}

notmuch_messages_t *
notmuch_thread_get_toplevel_messages (notmuch_thread_t *thread)
{
    return _notmuch_messages_create (thread->toplevel_list);
}

notmuch_messages_t *
notmuch_thread_get_messages (notmuch_thread_t *thread)
{
    return _notmuch_messages_create (thread->message_list);
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
    notmuch_string_list_t *tags;
    GList *keys, *l;

    tags = _notmuch_string_list_create (thread);
    if (unlikely (tags == NULL))
	return NULL;

    keys = g_hash_table_get_keys (thread->tags);

    for (l = keys; l; l = l->next)
	_notmuch_string_list_append (tags, (char *) l->data);

    g_list_free (keys);

    _notmuch_string_list_sort (tags);

    return _notmuch_tags_create (thread, tags);
}

void
notmuch_thread_destroy (notmuch_thread_t *thread)
{
    talloc_free (thread);
}
