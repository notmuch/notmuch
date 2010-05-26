/* The Ruby interface to the notmuch mail library
 *
 * Copyright © 2010 Ali Polatel
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
 * Author: Ali Polatel <alip@exherbo.org>
 */

#include "defs.h"

/*
 * call-seq: QUERY.destroy => nil
 *
 * Destroys the query, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_query_destroy(VALUE self)
{
    notmuch_query_t *query;

    Data_Get_Notmuch_Query(self, query);

    notmuch_query_destroy(query);
    DATA_PTR(self) = NULL;

    return Qnil;
}

/*
 * call-seq: QUERY.sort=(fixnum) => nil
 *
 * Set sort type of the +QUERY+
 */
VALUE
notmuch_rb_query_set_sort(VALUE self, VALUE sortv)
{
    notmuch_query_t *query;

    Data_Get_Notmuch_Query(self, query);

    if (!FIXNUM_P(sortv))
        rb_raise(rb_eTypeError, "Not a Fixnum");

    notmuch_query_set_sort(query, FIX2UINT(sortv));

    return Qnil;
}

/*
 * call-seq: QUERY.search_threads => THREADS
 *
 * Search for threads
 */
VALUE
notmuch_rb_query_search_threads(VALUE self)
{
    notmuch_query_t *query;
    notmuch_threads_t *threads;

    Data_Get_Notmuch_Query(self, query);

    threads = notmuch_query_search_threads(query);
    if (!threads)
        rb_raise(notmuch_rb_eMemoryError, "Out of memory");

    return Data_Wrap_Struct(notmuch_rb_cThreads, NULL, NULL, threads);
}

/*
 * call-seq: QUERY.search_messages => MESSAGES
 *
 * Search for messages
 */
VALUE
notmuch_rb_query_search_messages(VALUE self)
{
    notmuch_query_t *query;
    notmuch_messages_t *messages;

    Data_Get_Notmuch_Query(self, query);

    messages = notmuch_query_search_messages(query);
    if (!messages)
        rb_raise(notmuch_rb_eMemoryError, "Out of memory");

    return Data_Wrap_Struct(notmuch_rb_cMessages, NULL, NULL, messages);
}
