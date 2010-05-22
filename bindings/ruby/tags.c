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
 * call-seq: TAGS.each {|item| block } => TAGS
 *
 * Calls +block+ once for each element in +self+, passing that element as a
 * parameter.
 */
VALUE
notmuch_rb_tags_each(VALUE self)
{
    const char *tag;
    notmuch_rb_tags_t *tags;

    Data_Get_Struct(self, notmuch_rb_tags_t, tags);
    if (!tags->nm_tags)
        return self;

    for (; notmuch_tags_valid(tags->nm_tags);
            notmuch_tags_move_to_next(tags->nm_tags)) {
        tag = notmuch_tags_get(tags->nm_tags);
        rb_yield(tag ? rb_str_new2(tag) : Qnil);
    }

    return self;
}
