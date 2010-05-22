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

/* call-seq: MESSAGES.each {|item| block } => MESSAGES
 *
 * Calls +block+ once for each message in +self+, passing that element as a
 * parameter.
 */
VALUE
notmuch_rb_messages_each(VALUE self)
{
    notmuch_rb_message_t *message;
    notmuch_rb_messages_t *messages;
    VALUE messagev;

    Data_Get_Struct(self, notmuch_rb_messages_t, messages);
    if (!messages->nm_messages)
        return self;

    for (; notmuch_messages_valid(messages->nm_messages);
            notmuch_messages_move_to_next(messages->nm_messages))
    {
        messagev = Data_Make_Struct(notmuch_rb_cMessage, notmuch_rb_message_t,
                notmuch_rb_message_mark, notmuch_rb_message_free, message);
        message->nm_message = notmuch_messages_get(messages->nm_messages);
        message->parent = self;
        rb_yield(messagev);
    }

    return self;
}

/*
 * call-seq: MESSAGES.tags => TAGS
 *
 * Collect tags from the messages
 */
VALUE
notmuch_rb_messages_collect_tags(VALUE self)
{
    notmuch_rb_tags_t *tags;
    notmuch_rb_messages_t *messages;
    VALUE tagsv;

    Data_Get_Struct(self, notmuch_rb_messages_t, messages);

    tagsv = Data_Make_Struct(notmuch_rb_cTags, notmuch_rb_tags_t,
            notmuch_rb_tags_mark, notmuch_rb_tags_free, tags);
    tags->nm_tags = notmuch_messages_collect_tags(messages->nm_messages);
    tags->parent = self;
    if (!tags->nm_tags)
        rb_raise(notmuch_rb_eMemoryError, "out of memory");

    return tagsv;
}
