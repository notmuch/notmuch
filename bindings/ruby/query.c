/* The Ruby interface to the notmuch mail library
 *
 * Copyright © 2010, 2011, 2012 Ali Polatel
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
 * Author: Ali Polatel <alip@exherbo.org>
 */

#include "defs.h"

/*
 * call-seq: QUERY.destroy! => nil
 *
 * Destroys the query, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_query_destroy (VALUE self)
{
    notmuch_query_t *query;

    Data_Get_Notmuch_Query (self, query);

    notmuch_query_destroy (query);
    DATA_PTR (self) = NULL;

    return Qnil;
}

/*
 * call-seq: QUERY.sort => fixnum
 *
 * Get sort type of the +QUERY+
 */
VALUE
notmuch_rb_query_get_sort (VALUE self)
{
    notmuch_query_t *query;

    Data_Get_Notmuch_Query (self, query);

    return FIX2INT (notmuch_query_get_sort (query));
}

/*
 * call-seq: QUERY.sort=(fixnum) => nil
 *
 * Set sort type of the +QUERY+
 */
VALUE
notmuch_rb_query_set_sort (VALUE self, VALUE sortv)
{
    notmuch_query_t *query;

    Data_Get_Notmuch_Query (self, query);

    if (!FIXNUM_P (sortv))
	rb_raise (rb_eTypeError, "Not a Fixnum");

    notmuch_query_set_sort (query, FIX2UINT (sortv));

    return Qnil;
}

/*
 * call-seq: QUERY.to_s => string
 *
 * Get query string of the +QUERY+
 */
VALUE
notmuch_rb_query_get_string (VALUE self)
{
    notmuch_query_t *query;

    Data_Get_Notmuch_Query (self, query);

    return rb_str_new2 (notmuch_query_get_query_string (query));
}

/*
 * call-seq: QUERY.add_tag_exclude(tag) => nil
 *
 * Add a tag that will be excluded from the query results by default.
 */
VALUE
notmuch_rb_query_add_tag_exclude (VALUE self, VALUE tagv)
{
    notmuch_query_t *query;
    const char *tag;

    Data_Get_Notmuch_Query (self, query);
    tag = RSTRING_PTR(tagv);

    notmuch_query_add_tag_exclude(query, tag);
    return Qnil;
}

/*
 * call-seq: QUERY.omit_excluded=(boolean) => nil
 *
 * Specify whether to omit excluded results or simply flag them.
 * By default, this is set to +true+.
 */
VALUE
notmuch_rb_query_set_omit_excluded (VALUE self, VALUE omitv)
{
    notmuch_query_t *query;

    Data_Get_Notmuch_Query (self, query);

    notmuch_query_set_omit_excluded (query, RTEST (omitv));

    return Qnil;
}

/*
 * call-seq: QUERY.search_threads => THREADS
 *
 * Search for threads
 */
VALUE
notmuch_rb_query_search_threads (VALUE self)
{
    notmuch_query_t *query;
    notmuch_threads_t *threads;
    notmuch_status_t status;

    Data_Get_Notmuch_Query (self, query);

    status = notmuch_query_search_threads (query, &threads);
    if (status)
	notmuch_rb_status_raise (status);

    return Data_Wrap_Struct (notmuch_rb_cThreads, NULL, NULL, threads);
}

/*
 * call-seq: QUERY.search_messages => MESSAGES
 *
 * Search for messages
 */
VALUE
notmuch_rb_query_search_messages (VALUE self)
{
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_status_t status;

    Data_Get_Notmuch_Query (self, query);

    status = notmuch_query_search_messages (query, &messages);
    if (status)
	notmuch_rb_status_raise (status);

    return Data_Wrap_Struct (notmuch_rb_cMessages, NULL, NULL, messages);
}

/*
 * call-seq: QUERY.count_messages => Fixnum
 *
 * Return an estimate of the number of messages matching a search
 */
VALUE
notmuch_rb_query_count_messages (VALUE self)
{
    notmuch_query_t *query;
    notmuch_status_t status;
    unsigned int count;

    Data_Get_Notmuch_Query (self, query);

    status = notmuch_query_count_messages (query, &count);
    if (status)
	notmuch_rb_status_raise (status);

    return UINT2NUM(count);
}

/*
 * call-seq: QUERY.count_threads => Fixnum
 *
 * Return an estimate of the number of threads matching a search
 */
VALUE
notmuch_rb_query_count_threads (VALUE self)
{
    notmuch_query_t *query;
    notmuch_status_t status;
    unsigned int count;

    Data_Get_Notmuch_Query (self, query);

    status = notmuch_query_count_threads (query, &count);
    if (status)
	notmuch_rb_status_raise (status);

    return UINT2NUM(count);
}
