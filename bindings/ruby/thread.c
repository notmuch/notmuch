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
 * call-seq: THREAD.thread_id => String
 *
 * Returns the thread id
 */
VALUE
notmuch_rb_thread_get_thread_id(VALUE self)
{
    const char *tid;
    notmuch_rb_thread_t *thread;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    tid = notmuch_thread_get_thread_id(thread->nm_thread);
    return tid ? rb_str_new2(tid) : Qnil;
}

/*
 * call-seq: THREAD.total_messages => fixnum
 *
 * Returns the number of total messages
 */
VALUE
notmuch_rb_thread_get_total_messages(VALUE self)
{
    notmuch_rb_thread_t *thread;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    return INT2FIX(notmuch_thread_get_total_messages(thread->nm_thread));
}

/*
 * call-seq: THREAD.toplevel_messages => MESSAGES
 *
 * Get a Notmuch::Messages iterator for the top level messages in thread.
 */
VALUE
notmuch_rb_thread_get_toplevel_messages(VALUE self)
{
    notmuch_rb_messages_t *messages;
    notmuch_rb_thread_t *thread;
    VALUE messagesv;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    messagesv = Data_Make_Struct(notmuch_rb_cMessages, notmuch_rb_messages_t,
            notmuch_rb_messages_mark, notmuch_rb_messages_free, messages);
    messages->nm_messages = notmuch_thread_get_toplevel_messages(thread->nm_thread);
    messages->parent = self;
    if (!messages->nm_messages)
        rb_raise(notmuch_rb_eMemoryError, "out of memory");

    return messagesv;
}

/*
 * call-seq: THREAD.matched_messages => fixnum
 *
 * Get the number of messages in thread that matched the search
 */
VALUE
notmuch_rb_thread_get_matched_messages(VALUE self)
{
    notmuch_rb_thread_t *thread;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    return INT2FIX(notmuch_thread_get_matched_messages(thread->nm_thread));
}

/*
 * call-seq: THREAD.authors => String
 *
 * Get a comma-separated list of the names of the authors.
 */
VALUE
notmuch_rb_thread_get_authors(VALUE self)
{
    const char *authors;
    notmuch_rb_thread_t *thread;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    authors = notmuch_thread_get_authors(thread->nm_thread);
    return authors ? rb_str_new2(authors) : Qnil;
}

/*
 * call-seq: THREAD.subject => String
 *
 * Returns the subject of the thread
 */
VALUE
notmuch_rb_thread_get_subject(VALUE self)
{
    const char *subject;
    notmuch_rb_thread_t *thread;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    subject = notmuch_thread_get_subject(thread->nm_thread);
    return subject ? rb_str_new2(subject) : Qnil;
}

/*
 * call-seq: THREAD.oldest_date => Fixnum
 *
 * Get the date of the oldest message in thread.
 */
VALUE
notmuch_rb_thread_get_oldest_date(VALUE self)
{
    notmuch_rb_thread_t *thread;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    return UINT2NUM(notmuch_thread_get_oldest_date(thread->nm_thread));
}

/*
 * call-seq: THREAD.newest_date => fixnum
 *
 * Get the date of the newest message in thread.
 */
VALUE
notmuch_rb_thread_get_newest_date(VALUE self)
{
    notmuch_rb_thread_t *thread;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    return UINT2NUM(notmuch_thread_get_newest_date(thread->nm_thread));
}

/*
 * call-seq: THREAD.tags => TAGS
 *
 * Get a Notmuch::Tags iterator for the tags of the thread
 */
VALUE
notmuch_rb_thread_get_tags(VALUE self)
{
    notmuch_rb_thread_t *thread;
    notmuch_rb_tags_t *tags;
    VALUE tagsv;

    Data_Get_Struct(self, notmuch_rb_thread_t, thread);

    tagsv = Data_Make_Struct(notmuch_rb_cTags, notmuch_rb_tags_t,
            notmuch_rb_tags_mark, notmuch_rb_tags_free, tags);
    tags->nm_tags = notmuch_thread_get_tags(thread->nm_thread);
    tags->parent = self;
    if (!tags->nm_tags)
        rb_raise(notmuch_rb_eMemoryError, "out of memory");

    return tagsv;
}
