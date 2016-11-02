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
 * call-seq: MESSAGES.destroy! => nil
 *
 * Destroys the messages, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_messages_destroy (VALUE self)
{
    notmuch_messages_t *messages;

    Data_Get_Notmuch_Messages (self, messages);

    notmuch_messages_destroy (messages);
    DATA_PTR (self) = NULL;

    return Qnil;
}

/* call-seq: MESSAGES.each {|item| block } => MESSAGES
 *
 * Calls +block+ once for each message in +self+, passing that element as a
 * parameter.
 */
VALUE
notmuch_rb_messages_each (VALUE self)
{
    notmuch_message_t *message;
    notmuch_messages_t *messages;

    Data_Get_Notmuch_Messages (self, messages);

    for (; notmuch_messages_valid (messages); notmuch_messages_move_to_next (messages)) {
	message = notmuch_messages_get (messages);
	rb_yield (Data_Wrap_Struct (notmuch_rb_cMessage, NULL, NULL, message));
    }

    return self;
}

/*
 * call-seq: MESSAGES.tags => TAGS
 *
 * Collect tags from the messages
 */
VALUE
notmuch_rb_messages_collect_tags (VALUE self)
{
    notmuch_tags_t *tags;
    notmuch_messages_t *messages;

    Data_Get_Notmuch_Messages (self, messages);

    tags = notmuch_messages_collect_tags (messages);
    if (!tags)
	rb_raise (notmuch_rb_eMemoryError, "Out of memory");

    return Data_Wrap_Struct (notmuch_rb_cTags, NULL, NULL, tags);
}
