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

struct _notmuch_thread {
    notmuch_database_t *notmuch;
    char *thread_id;
};

/* Create a new notmuch_thread_t object for an existing document in
 * the database.
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
_notmuch_thread_create (const void *talloc_owner,
			notmuch_database_t *notmuch,
			const char *thread_id)
{
    notmuch_thread_t *thread;

    thread = talloc (talloc_owner, notmuch_thread_t);
    if (unlikely (thread == NULL))
	return NULL;

    thread->notmuch = notmuch;
    thread->thread_id = talloc_strdup (thread, thread_id);

    return thread;
}

const char *
notmuch_thread_get_thread_id (notmuch_thread_t *thread)
{
    return thread->thread_id;
}

void
notmuch_thread_destroy (notmuch_thread_t *thread)
{
    talloc_free (thread);
}
