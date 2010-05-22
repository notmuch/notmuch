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
 * call-seq: FILENAMES.each {|item| block } => FILENAMES
 *
 * Calls +block+ once for each element in +self+, passing that element as a
 * parameter.
 */
VALUE
notmuch_rb_filenames_each(VALUE self)
{
    notmuch_rb_filenames_t *flist;

    Data_Get_Struct(self, notmuch_rb_filenames_t, flist);
    if (!flist->nm_flist)
        return self;

    for (; notmuch_filenames_valid(flist->nm_flist);
            notmuch_filenames_move_to_next(flist->nm_flist))
        rb_yield(rb_str_new2(notmuch_filenames_get(flist->nm_flist)));

    return self;
}
