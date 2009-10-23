/* message.cc - Results of message-based searches from a notmuch database
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

struct _notmuch_message {
    notmuch_database_t *notmuch;
    Xapian::docid doc_id;
    char *message_id;
    Xapian::Document doc;
};

struct _notmuch_tags {
    Xapian::TermIterator iterator;
    Xapian::TermIterator iterator_end;
};

struct _notmuch_thread_ids {
    char *current;
    char *next;
};

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

/* These prefix values are specifically chosen to be compatible
 * with sup, (http://sup.rubyforge.org), written by
 * William Morgan <wmorgan-sup@masanjin.net>, and released
 * under the GNU GPL v2.
 */

typedef struct {
    const char *name;
    const char *prefix;
} prefix_t;

prefix_t NORMAL_PREFIX[] = {
    { "subject", "S" },
    { "body", "B" },
    { "from_name", "FN" },
    { "to_name", "TN" },
    { "name", "N" },
    { "attachment", "A" }
};

prefix_t BOOLEAN_PREFIX[] = {
    { "type", "K" },
    { "from_email", "FE" },
    { "to_email", "TE" },
    { "email", "E" },
    { "date", "D" },
    { "label", "L" },
    { "tag", "L" },
    { "source_id", "I" },
    { "attachment_extension", "O" },
    { "msgid", "Q" },
    { "thread", "H" },
    { "ref", "R" }
};

const char *
_find_prefix (const char *name)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE (NORMAL_PREFIX); i++)
	if (strcmp (name, NORMAL_PREFIX[i].name) == 0)
	    return NORMAL_PREFIX[i].prefix;

    for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX); i++)
	if (strcmp (name, BOOLEAN_PREFIX[i].name) == 0)
	    return BOOLEAN_PREFIX[i].prefix;

    return "";
}

/* We end up having to call the destructor explicitly because we had
 * to use "placement new" in order to initialize C++ objects within a
 * block that we allocated with talloc. So C++ is making talloc
 * slightly less simple to use, (we wouldn't need
 * talloc_set_destructor at all otherwise).
 */
static int
_notmuch_message_destructor (notmuch_message_t *message)
{
    message->doc.~Document ();

    return 0;
}

notmuch_message_t *
_notmuch_message_create (const void *talloc_owner,
			 notmuch_database_t *notmuch,
			 unsigned int doc_id)
{
    notmuch_message_t *message;

    message = talloc (talloc_owner, notmuch_message_t);
    if (unlikely (message == NULL))
	return NULL;

    message->notmuch = notmuch;
    message->doc_id = doc_id;
    message->message_id = NULL; /* lazily created */
    new (&message->doc) Xapian::Document;

    talloc_set_destructor (message, _notmuch_message_destructor);

    message->doc = notmuch->xapian_db->get_document (doc_id);

    return message;
}

const char *
notmuch_message_get_message_id (notmuch_message_t *message)
{
    Xapian::TermIterator i;

    if (message->message_id)
	return message->message_id;

    i = message->doc.termlist_begin ();
    i.skip_to (_find_prefix ("msgid"));

    /* XXX: This should really be an internal error, but we'll need to
     * fix the add_message side of things first. */
    if (i == message->doc.termlist_end ())
	return NULL;

    message->message_id = talloc_strdup (message, (*i).c_str () + 1);
    return message->message_id;
}

/* We end up having to call the destructors explicitly because we had
 * to use "placement new" in order to initialize C++ objects within a
 * block that we allocated with talloc. So C++ is making talloc
 * slightly less simple to use, (we wouldn't need
 * talloc_set_destructor at all otherwise).
 */
static int
_notmuch_tags_destructor (notmuch_tags_t *tags)
{
    tags->iterator.~TermIterator ();
    tags->iterator_end.~TermIterator ();

    return 0;
}

notmuch_tags_t *
notmuch_message_get_tags (notmuch_message_t *message)
{
    notmuch_tags_t *tags;

    tags = talloc (message, notmuch_tags_t);
    if (unlikely (tags == NULL))
	return NULL;

    new (&tags->iterator) Xapian::TermIterator;
    new (&tags->iterator_end) Xapian::TermIterator;

    talloc_set_destructor (tags, _notmuch_tags_destructor);

    tags->iterator = message->doc.termlist_begin ();
    tags->iterator.skip_to (_find_prefix ("tag"));
    tags->iterator_end = message->doc.termlist_end ();

    return tags;
}

notmuch_thread_ids_t *
notmuch_message_get_thread_ids (notmuch_message_t *message)
{
    notmuch_thread_ids_t *thread_ids;
    std::string id_str;

    thread_ids = talloc (message, notmuch_thread_ids_t);
    if (unlikely (thread_ids == NULL))
	return NULL;

    id_str = message->doc.get_value (NOTMUCH_VALUE_THREAD);
    thread_ids->next = talloc_strdup (message, id_str.c_str ());

    /* Initialize thread_ids->current and terminate first ID. */
    notmuch_thread_ids_advance (thread_ids);

    return thread_ids;
}

/* Synchronize changes made to message->doc into the database. */
static void
_notmuch_message_sync (notmuch_message_t *message)
{
    Xapian::WritableDatabase *db = message->notmuch->xapian_db;

    db->replace_document (message->doc_id, message->doc);
}

/* Add a name:value term to 'message', (the actual term will be
 * encoded by prefixing the value with a short prefix). See
 * NORMAL_PREFIX and BOOLEAN_PREFIX arrays for the mapping of term
 * names to prefix values.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_set_sync. */
notmuch_private_status_t
_notmuch_message_add_term (notmuch_message_t *message,
			   const char *prefix_name,
			   const char *value)
{

    char *term;

    if (value == NULL)
	return NOTMUCH_PRIVATE_STATUS_NULL_POINTER;

    term = talloc_asprintf (message, "%s%s",
			    _find_prefix (prefix_name), value);

    if (strlen (term) > NOTMUCH_TERM_MAX)
	return NOTMUCH_PRIVATE_STATUS_TERM_TOO_LONG;

    message->doc.add_term (term);

    talloc_free (term);

    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

/* Remove a name:value term from 'message', (the actual term will be
 * encoded by prefixing the value with a short prefix). See
 * NORMAL_PREFIX and BOOLEAN_PREFIX arrays for the mapping of term
 * names to prefix values.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_set_sync. */
notmuch_private_status_t
_notmuch_message_remove_term (notmuch_message_t *message,
			      const char *prefix_name,
			      const char *value)
{
    char *term;

    if (value == NULL)
	return NOTMUCH_PRIVATE_STATUS_NULL_POINTER;

    term = talloc_asprintf (message, "%s%s",
			    _find_prefix (prefix_name), value);

    if (strlen (term) > NOTMUCH_TERM_MAX)
	return NOTMUCH_PRIVATE_STATUS_TERM_TOO_LONG;

    message->doc.remove_term (term);

    talloc_free (term);

    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_add_tag (notmuch_message_t *message, const char *tag)
{
    notmuch_private_status_t status;

    if (tag == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (strlen (tag) > NOTMUCH_TAG_MAX)
	return NOTMUCH_STATUS_TAG_TOO_LONG;

    status = _notmuch_message_add_term (message, "tag", tag);
    if (status) {
	fprintf (stderr, "Internal error: _notmuch_message_add_term return unexpected value: %d\n",
		 status);
	exit (1);
    }

    _notmuch_message_sync (message);

    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_status_t
notmuch_message_remove_tag (notmuch_message_t *message, const char *tag)
{
    notmuch_private_status_t status;

    if (tag == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (strlen (tag) > NOTMUCH_TAG_MAX)
	return NOTMUCH_STATUS_TAG_TOO_LONG;

    status = _notmuch_message_remove_term (message, "tag", tag);
    if (status) {
	fprintf (stderr, "Internal error: _notmuch_message_remove_term return unexpected value: %d\n",
		 status);
	exit (1);
    }

    _notmuch_message_sync (message);

    return NOTMUCH_STATUS_SUCCESS;
}

void
notmuch_message_destroy (notmuch_message_t *message)
{
    talloc_free (message);
}

notmuch_bool_t
notmuch_tags_has_more (notmuch_tags_t *tags)
{
    std::string s;

    if (tags->iterator == tags->iterator_end)
	return FALSE;

    s = *tags->iterator;
    if (s.size () && s[0] == 'L')
	return TRUE;
    else
	return FALSE;
}

const char *
notmuch_tags_get (notmuch_tags_t *tags)
{
    return talloc_strdup (tags, (*tags->iterator).c_str () + 1);
}

void
notmuch_tags_advance (notmuch_tags_t *tags)
{
    tags->iterator++;
}

void
notmuch_tags_destroy (notmuch_tags_t *tags)
{
    talloc_free (tags);
}

notmuch_bool_t
notmuch_thread_ids_has_more (notmuch_thread_ids_t *thread_ids)
{
    if (thread_ids->current == NULL || *thread_ids->current == '\0')
	return FALSE;
    else
	return TRUE;
}

const char *
notmuch_thread_ids_get (notmuch_thread_ids_t *thread_ids)
{
    return thread_ids->current;
}

void
notmuch_thread_ids_advance (notmuch_thread_ids_t *thread_ids)
{
    thread_ids->current = strsep (&thread_ids->next, ",");
}

void
notmuch_thread_ids_destroy (notmuch_thread_ids_t *thread_ids)
{
    talloc_free (thread_ids);
}
