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
 * call-seq: QUERY.sort=(fixnum) => nil
 *
 * Set sort type of the +QUERY+
 */
VALUE
notmuch_rb_query_set_sort(VALUE self, VALUE sortv)
{
    notmuch_rb_query_t *query;

    Data_Get_Struct(self, notmuch_rb_query_t, query);

    if (!FIXNUM_P(sortv))
        rb_raise(rb_eTypeError, "Not a fixnum");

    notmuch_query_set_sort(query->nm_query, FIX2UINT(sortv));
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
    notmuch_rb_query_t *query;
    notmuch_rb_threads_t *threads;
    VALUE threadsv;

    Data_Get_Struct(self, notmuch_rb_query_t, query);

    threadsv = Data_Make_Struct(notmuch_rb_cThreads, notmuch_rb_threads_t,
            notmuch_rb_threads_mark, notmuch_rb_threads_free, threads);
    threads->nm_threads = notmuch_query_search_threads(query->nm_query);
    threads->query = self;
    if (!threads->nm_threads)
        rb_raise(notmuch_rb_eMemoryError, "out of memory");

    return threadsv;
}

/*
 * call-seq: QUERY.search_messages => MESSAGES
 *
 * Search for messages
 */
VALUE
notmuch_rb_query_search_messages(VALUE self)
{
    notmuch_rb_query_t *query;
    notmuch_rb_messages_t *messages;
    VALUE messagesv;

    Data_Get_Struct(self, notmuch_rb_query_t, query);

    messagesv = Data_Make_Struct(notmuch_rb_cMessages, notmuch_rb_messages_t,
            notmuch_rb_messages_mark, notmuch_rb_messages_free, messages);
    messages->nm_messages = notmuch_query_search_messages(query->nm_query);
    messages->parent = self;
    if (!messages->nm_messages)
        rb_raise(notmuch_rb_eMemoryError, "out of memory");

    return messagesv;
}
