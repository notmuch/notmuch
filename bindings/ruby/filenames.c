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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Ali Polatel <alip@exherbo.org>
 */

#include "defs.h"

/*
 * call-seq: FILENAMES.destroy! => nil
 *
 * Destroys the filenames, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_filenames_destroy (VALUE self)
{
    notmuch_filenames_t *fnames;

    Data_Get_Notmuch_FileNames (self, fnames);

    notmuch_filenames_destroy (fnames);
    DATA_PTR (self) = NULL;

    return Qnil;
}

/*
 * call-seq: FILENAMES.each {|item| block } => FILENAMES
 *
 * Calls +block+ once for each element in +self+, passing that element as a
 * parameter.
 */
VALUE
notmuch_rb_filenames_each (VALUE self)
{
    notmuch_filenames_t *fnames;

    Data_Get_Notmuch_FileNames (self, fnames);

    for (; notmuch_filenames_valid (fnames); notmuch_filenames_move_to_next (fnames))
	rb_yield (rb_str_new2 (notmuch_filenames_get (fnames)));

    return self;
}
