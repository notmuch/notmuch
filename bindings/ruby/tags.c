#include "defs.h"

VALUE
notmuch_rb_tags_get (notmuch_tags_t *tags)
{
    VALUE rb_array = rb_ary_new ();

    for (; notmuch_tags_valid (tags); notmuch_tags_move_to_next (tags)) {
	const char *tag = notmuch_tags_get (tags);
	rb_ary_push (rb_array, rb_str_new2 (tag));
    }
    return rb_array;
}
