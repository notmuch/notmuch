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
 * call-seq: TAGS.destroy! => nil
 *
 * Destroys the tags, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_tags_destroy (VALUE self)
{
    notmuch_tags_t *tags;

    Data_Get_Notmuch_Tags (self, tags);

    notmuch_tags_destroy (tags);
    DATA_PTR (self) = NULL;

    return Qnil;
}

/*
 * call-seq: TAGS.each {|item| block } => TAGS
 *
 * Calls +block+ once for each element in +self+, passing that element as a
 * parameter.
 */
VALUE
notmuch_rb_tags_each (VALUE self)
{
    const char *tag;
    notmuch_tags_t *tags;

    Data_Get_Notmuch_Tags (self, tags);

    for (; notmuch_tags_valid (tags); notmuch_tags_move_to_next (tags)) {
	tag = notmuch_tags_get (tags);
	rb_yield (rb_str_new2 (tag));
    }

    return self;
}
