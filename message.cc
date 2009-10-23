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
    char *filename;
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

/* "128 bits of thread-id ought to be enough for anybody" */
#define NOTMUCH_THREAD_ID_BITS	 128
#define NOTMUCH_THREAD_ID_DIGITS (NOTMUCH_THREAD_ID_BITS / 4)
typedef struct _thread_id {
    char str[NOTMUCH_THREAD_ID_DIGITS + 1];
} thread_id_t;

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

/* Create a new notmuch_message_t object for an existing document in
 * the database.
 *
 * Here, 'talloc owner' is an optional talloc context to which the new
 * message will belong. This allows for the caller to not bother
 * calling notmuch_message_destroy on the message, and no that all
 * memory will be reclaimed with 'talloc_owner' is free. The caller
 * still can call notmuch_message_destroy when finished with the
 * message if desired.
 *
 * The 'talloc_owner' argument can also be NULL, in which case the
 * caller *is* responsible for calling notmuch_message_destroy.
 *
 * If no document exists in the database with document ID of 'doc_id'
 * then this function returns NULL.
 */
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
    message->filename = NULL; /* lazily created */
    new (&message->doc) Xapian::Document;

    talloc_set_destructor (message, _notmuch_message_destructor);

    try {
	message->doc = notmuch->xapian_db->get_document (doc_id);
    } catch (const Xapian::DocNotFoundError &error) {
	talloc_free (message);
	return NULL;
    }

    return message;
}

/* Create a new notmuch_message_t object for a specific message ID,
 * (which may or may not already exist in the databas).
 *
 * Here, 'talloc owner' is an optional talloc context to which the new
 * message will belong. This allows for the caller to not bother
 * calling notmuch_message_destroy on the message, and no that all
 * memory will be reclaimed with 'talloc_owner' is free. The caller
 * still can call notmuch_message_destroy when finished with the
 * message if desired.
 *
 * The 'talloc_owner' argument can also be NULL, in which case the
 * caller *is* responsible for calling notmuch_message_destroy.
 *
 * If there is already a document with message ID 'message_id' in the
 * database, then the returned message can be used to query/modify the
 * document. Otherwise, a new document will be inserted into the
 * database before this function returns;
 */
notmuch_message_t *
_notmuch_message_create_for_message_id (const void *talloc_owner,
					notmuch_database_t *notmuch,
					const char *message_id)
{
    notmuch_message_t *message;
    Xapian::Document doc;
    unsigned int doc_id;
    char *term;

    message = notmuch_database_find_message (notmuch, message_id);
    if (message)
	return talloc_steal (talloc_owner, message);

    term = talloc_asprintf (NULL, "%s%s",
			    _find_prefix ("msgid"), message_id);
    doc.add_term (term);
    talloc_free (term);

    doc.add_value (NOTMUCH_VALUE_MESSAGE_ID, message_id);

    doc_id = notmuch->xapian_db->add_document (doc);

    return _notmuch_message_create (talloc_owner, notmuch, doc_id);
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

/* Set the filename for 'message' to 'filename'.
 *
 * XXX: We should still figure out what we want to do for multiple
 * files with identical message IDs. We will probably want to store a
 * list of filenames here, (so that this will be "add_filename"
 * instead of "set_filename"). Which would make this very similar to
 * add_thread_ids.
 *
 * This change will not be reflected in the database until the next
 * call to _notmuch_message_set_sync. */
void
_notmuch_message_set_filename (notmuch_message_t *message,
			       const char *filename)
{
    if (message->filename)
	talloc_free (message->filename);
    message->doc.set_data (filename);
}

const char *
notmuch_message_get_filename (notmuch_message_t *message)
{
    std::string filename_str;

    if (message->filename)
	return message->filename;

    filename_str = message->doc.get_data ();
    message->filename = talloc_strdup (message, filename_str.c_str ());

    return message->filename;
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

void
_notmuch_message_set_date (notmuch_message_t *message,
			   const char *date)
{
    time_t time_value;

    time_value = notmuch_parse_date (date, NULL);

    message->doc.add_value (NOTMUCH_VALUE_DATE,
			    Xapian::sortable_serialise (time_value));
}

void
_notmuch_message_add_thread_id (notmuch_message_t *message,
				const char *thread_id)
{
    std::string id_str;

    _notmuch_message_add_term (message, "thread", thread_id);

    id_str = message->doc.get_value (NOTMUCH_VALUE_THREAD);

    if (id_str.empty ()) {
	message->doc.add_value (NOTMUCH_VALUE_THREAD, thread_id);
    } else {
	size_t pos;

	/* Think about using a hash here if there's any performance
	 * problem. */
	pos = id_str.find (thread_id);
	if (pos == std::string::npos) {
	    id_str.append (",");
	    id_str.append (thread_id);
	    message->doc.add_value (NOTMUCH_VALUE_THREAD, id_str);
	}
    }
}

static void
thread_id_generate (thread_id_t *thread_id)
{
    static int seeded = 0;
    FILE *dev_random;
    uint32_t value;
    char *s;
    int i;

    if (! seeded) {
	dev_random = fopen ("/dev/random", "r");
	if (dev_random == NULL) {
	    srand (time (NULL));
	} else {
	    fread ((void *) &value, sizeof (value), 1, dev_random);
	    srand (value);
	    fclose (dev_random);
	}
	seeded = 1;
    }

    s = thread_id->str;
    for (i = 0; i < NOTMUCH_THREAD_ID_DIGITS; i += 8) {
	value = rand ();
	sprintf (s, "%08x", value);
	s += 8;
    }
}

void
_notmuch_message_ensure_thread_id (notmuch_message_t *message)
{
    /* If not part of any existing thread, generate a new thread_id. */
    thread_id_t thread_id;

    thread_id_generate (&thread_id);
    _notmuch_message_add_term (message, "thread", thread_id.str);
    message->doc.add_value (NOTMUCH_VALUE_THREAD, thread_id.str);
}

/* Synchronize changes made to message->doc out into the database. */
void
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
