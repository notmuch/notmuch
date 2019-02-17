/* The Ruby interface to the notmuch mail library
 *
 * Copyright © 2010, 2011 Ali Polatel
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
 * call-seq: THREAD.destroy! => nil
 *
 * Destroys the thread, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_thread_destroy (VALUE self)
{
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    notmuch_thread_destroy (thread);
    DATA_PTR (self) = NULL;

    return Qnil;
}

/*
 * call-seq: THREAD.thread_id => String
 *
 * Returns the thread id
 */
VALUE
notmuch_rb_thread_get_thread_id (VALUE self)
{
    const char *tid;
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    tid = notmuch_thread_get_thread_id (thread);

    return rb_str_new2 (tid);
}

/*
 * call-seq: THREAD.total_messages => fixnum
 *
 * Returns the number of total messages
 */
VALUE
notmuch_rb_thread_get_total_messages (VALUE self)
{
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    return INT2FIX (notmuch_thread_get_total_messages (thread));
}

/*
 * call-seq: THREAD.toplevel_messages => MESSAGES
 *
 * Get a Notmuch::Messages iterator for the top level messages in thread.
 */
VALUE
notmuch_rb_thread_get_toplevel_messages (VALUE self)
{
    notmuch_messages_t *messages;
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    messages = notmuch_thread_get_toplevel_messages (thread);
    if (!messages)
	rb_raise (notmuch_rb_eMemoryError, "Out of memory");

    return Data_Wrap_Struct (notmuch_rb_cMessages, NULL, NULL, messages);
}

/*
 * call-seq: THREAD.messages => MESSAGES
 *
 * Get a Notmuch::Messages iterator for the all messages in thread.
 */
VALUE
notmuch_rb_thread_get_messages (VALUE self)
{
    notmuch_messages_t *messages;
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    messages = notmuch_thread_get_messages (thread);
    if (!messages)
	rb_raise (notmuch_rb_eMemoryError, "Out of memory");

    return Data_Wrap_Struct (notmuch_rb_cMessages, NULL, NULL, messages);
}

/*
 * call-seq: THREAD.matched_messages => fixnum
 *
 * Get the number of messages in thread that matched the search
 */
VALUE
notmuch_rb_thread_get_matched_messages (VALUE self)
{
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    return INT2FIX (notmuch_thread_get_matched_messages (thread));
}

/*
 * call-seq: THREAD.authors => String
 *
 * Get a comma-separated list of the names of the authors.
 */
VALUE
notmuch_rb_thread_get_authors (VALUE self)
{
    const char *authors;
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    authors = notmuch_thread_get_authors (thread);

    return rb_str_new2 (authors);
}

/*
 * call-seq: THREAD.subject => String
 *
 * Returns the subject of the thread
 */
VALUE
notmuch_rb_thread_get_subject (VALUE self)
{
    const char *subject;
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    subject = notmuch_thread_get_subject (thread);

    return rb_str_new2 (subject);
}

/*
 * call-seq: THREAD.oldest_date => Fixnum
 *
 * Get the date of the oldest message in thread.
 */
VALUE
notmuch_rb_thread_get_oldest_date (VALUE self)
{
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    return UINT2NUM (notmuch_thread_get_oldest_date (thread));
}

/*
 * call-seq: THREAD.newest_date => fixnum
 *
 * Get the date of the newest message in thread.
 */
VALUE
notmuch_rb_thread_get_newest_date (VALUE self)
{
    notmuch_thread_t *thread;

    Data_Get_Notmuch_Thread (self, thread);

    return UINT2NUM (notmuch_thread_get_newest_date (thread));
}

/*
 * call-seq: THREAD.tags => TAGS
 *
 * Get a Notmuch::Tags iterator for the tags of the thread
 */
VALUE
notmuch_rb_thread_get_tags (VALUE self)
{
    notmuch_thread_t *thread;
    notmuch_tags_t *tags;

    Data_Get_Notmuch_Thread (self, thread);

    tags = notmuch_thread_get_tags (thread);
    if (!tags)
	rb_raise (notmuch_rb_eMemoryError, "Out of memory");

    return Data_Wrap_Struct (notmuch_rb_cTags, NULL, NULL, tags);
}
