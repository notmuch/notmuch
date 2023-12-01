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

void
notmuch_rb_status_raise (notmuch_status_t status)
{
    switch (status) {
    case NOTMUCH_STATUS_SUCCESS:
    case NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
	break;
    case NOTMUCH_STATUS_OUT_OF_MEMORY:
	rb_raise (notmuch_rb_eMemoryError, "out of memory");
    case NOTMUCH_STATUS_READ_ONLY_DATABASE:
	rb_raise (notmuch_rb_eReadOnlyError, "read-only database");
    case NOTMUCH_STATUS_XAPIAN_EXCEPTION:
	rb_raise (notmuch_rb_eXapianError, "xapian exception");
    case NOTMUCH_STATUS_FILE_ERROR:
	rb_raise (notmuch_rb_eFileError, "failed to read/write file");
    case NOTMUCH_STATUS_FILE_NOT_EMAIL:
	rb_raise (notmuch_rb_eFileNotEmailError, "file not email");
    case NOTMUCH_STATUS_NULL_POINTER:
	rb_raise (notmuch_rb_eNullPointerError, "null pointer");
    case NOTMUCH_STATUS_TAG_TOO_LONG:
	rb_raise (notmuch_rb_eTagTooLongError, "tag too long");
    case NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW:
	rb_raise (notmuch_rb_eUnbalancedFreezeThawError, "unbalanced freeze/thaw");
    case NOTMUCH_STATUS_UNBALANCED_ATOMIC:
	rb_raise (notmuch_rb_eUnbalancedAtomicError, "unbalanced atomic");
    default:
	rb_raise (notmuch_rb_eBaseError, "unknown notmuch error");
    }
}
