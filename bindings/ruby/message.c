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
 * call-seq: MESSAGE.destroy! => nil
 *
 * Destroys the message, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_message_destroy (VALUE self)
{
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    notmuch_message_destroy (message);
    DATA_PTR (self) = NULL;

    return Qnil;
}

/*
 * call-seq: MESSAGE.message_id => String
 *
 * Get the message ID of 'message'.
 */
VALUE
notmuch_rb_message_get_message_id (VALUE self)
{
    const char *msgid;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    msgid = notmuch_message_get_message_id (message);

    return rb_str_new2 (msgid);
}

/*
 * call-seq: MESSAGE.thread_id => String
 *
 * Get the thread ID of 'message'.
 */
VALUE
notmuch_rb_message_get_thread_id (VALUE self)
{
    const char *tid;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    tid = notmuch_message_get_thread_id (message);

    return rb_str_new2 (tid);
}

/*
 * call-seq: MESSAGE.replies => MESSAGES
 *
 * Get a Notmuch::Messages enumerable for all of the replies to 'message'.
 */
VALUE
notmuch_rb_message_get_replies (VALUE self)
{
    notmuch_messages_t *messages;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    messages = notmuch_message_get_replies (message);

    return Data_Wrap_Struct (notmuch_rb_cMessages, NULL, NULL, messages);
}

/*
 * call-seq: MESSAGE.filename => String
 *
 * Get a filename for the email corresponding to 'message'
 */
VALUE
notmuch_rb_message_get_filename (VALUE self)
{
    const char *fname;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    fname = notmuch_message_get_filename (message);

    return rb_str_new2 (fname);
}

/*
 * call-seq: MESSAGE.filenames => FILENAMES
 *
 * Get all filenames for the email corresponding to MESSAGE.
 */
VALUE
notmuch_rb_message_get_filenames (VALUE self)
{
    notmuch_filenames_t *fnames;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    fnames = notmuch_message_get_filenames (message);

    return Data_Wrap_Struct (notmuch_rb_cFileNames, NULL, NULL, fnames);
}

/*
 * call-seq: MESSAGE.get_flag (flag) => true or false
 *
 * Get a value of a flag for the email corresponding to 'message'
 */
VALUE
notmuch_rb_message_get_flag (VALUE self, VALUE flagv)
{
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    if (!FIXNUM_P (flagv))
	rb_raise (rb_eTypeError, "Flag not a Fixnum");

    return notmuch_message_get_flag (message, FIX2INT (flagv)) ? Qtrue : Qfalse;
}

/*
 * call-seq: MESSAGE.set_flag (flag, value) => nil
 *
 * Set a value of a flag for the email corresponding to 'message'
 */
VALUE
notmuch_rb_message_set_flag (VALUE self, VALUE flagv, VALUE valuev)
{
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    if (!FIXNUM_P (flagv))
	rb_raise (rb_eTypeError, "Flag not a Fixnum");

    notmuch_message_set_flag (message, FIX2INT (flagv), RTEST (valuev));

    return Qnil;
}

/*
 * call-seq: MESSAGE.date => Fixnum
 *
 * Get the date of 'message'
 */
VALUE
notmuch_rb_message_get_date (VALUE self)
{
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    return UINT2NUM (notmuch_message_get_date (message));
}

/*
 * call-seq: MESSAGE.header (name) => String
 *
 * Get the value of the specified header from 'message'
 */
VALUE
notmuch_rb_message_get_header (VALUE self, VALUE headerv)
{
    const char *header, *value;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    SafeStringValue (headerv);
    header = RSTRING_PTR (headerv);

    value = notmuch_message_get_header (message, header);
    if (!value)
	rb_raise (notmuch_rb_eMemoryError, "Out of memory");

    return rb_str_new2 (value);
}

/*
 * call-seq: MESSAGE.tags => TAGS
 *
 * Get a Notmuch::Tags enumerable for all of the tags of 'message'.
 */
VALUE
notmuch_rb_message_get_tags (VALUE self)
{
    notmuch_message_t *message;
    notmuch_tags_t *tags;

    Data_Get_Notmuch_Message (self, message);

    tags = notmuch_message_get_tags (message);
    if (!tags)
	rb_raise (notmuch_rb_eMemoryError, "Out of memory");

    return Data_Wrap_Struct (notmuch_rb_cTags, NULL, NULL, tags);
}

/*
 * call-seq: MESSAGE.add_tag (tag) => true
 *
 * Add a tag to the 'message'
 */
VALUE
notmuch_rb_message_add_tag (VALUE self, VALUE tagv)
{
    const char *tag;
    notmuch_status_t ret;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    SafeStringValue (tagv);
    tag = RSTRING_PTR (tagv);

    ret = notmuch_message_add_tag (message, tag);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: MESSAGE.remove_tag (tag) => true
 *
 * Remove a tag from the 'message'
 */
VALUE
notmuch_rb_message_remove_tag (VALUE self, VALUE tagv)
{
    const char *tag;
    notmuch_status_t ret;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    SafeStringValue (tagv);
    tag = RSTRING_PTR (tagv);

    ret = notmuch_message_remove_tag (message, tag);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: MESSAGE.remove_all_tags => true
 *
 * Remove all tags of the 'message'
 */
VALUE
notmuch_rb_message_remove_all_tags (VALUE self)
{
    notmuch_status_t ret;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    ret = notmuch_message_remove_all_tags (message);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: MESSAGE.maildir_flags_to_tags => true
 *
 * Add/remove tags according to maildir flags in the message filename (s)
 */
VALUE
notmuch_rb_message_maildir_flags_to_tags (VALUE self)
{
    notmuch_status_t ret;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    ret = notmuch_message_maildir_flags_to_tags (message);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: MESSAGE.tags_to_maildir_flags => true
 *
 * Rename message filename (s) to encode tags as maildir flags
 */
VALUE
notmuch_rb_message_tags_to_maildir_flags (VALUE self)
{
    notmuch_status_t ret;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    ret = notmuch_message_tags_to_maildir_flags (message);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: MESSAGE.freeze => true
 *
 * Freeze the 'message'
 */
VALUE
notmuch_rb_message_freeze (VALUE self)
{
    notmuch_status_t ret;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    ret = notmuch_message_freeze (message);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: MESSAGE.thaw => true
 *
 * Thaw a 'message'
 */
VALUE
notmuch_rb_message_thaw (VALUE self)
{
    notmuch_status_t ret;
    notmuch_message_t *message;

    Data_Get_Notmuch_Message (self, message);

    ret = notmuch_message_thaw (message);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}
