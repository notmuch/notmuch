#include "defs.h"

VALUE
notmuch_rb_filenames_get (notmuch_filenames_t *fnames)
{
    VALUE rb_array = rb_ary_new ();

    for (; notmuch_filenames_valid (fnames); notmuch_filenames_move_to_next (fnames))
	rb_ary_push (rb_array, rb_str_new2 (notmuch_filenames_get (fnames)));
    return rb_array;
}
