/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2012 Jameson Rollins
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
 * Authors: Jameson Rollins <jrollins@finestructure.net>
 */

#include "crypto.h"
#include <strings.h>
#define unused(x) x __attribute__ ((unused))

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

void _notmuch_crypto_cleanup (unused(_notmuch_crypto_t *crypto))
{
}

GMimeObject *
_notmuch_crypto_decrypt (bool *attempted,
			 notmuch_decryption_policy_t decrypt,
			 notmuch_message_t *message,
			 GMimeMultipartEncrypted *part,
			 GMimeDecryptResult **decrypt_result,
			 GError **err)
{
    GMimeObject *ret = NULL;
    if (decrypt == NOTMUCH_DECRYPT_FALSE)
	return NULL;

    /* try decryption with session key if one is stashed */
    if (message) {
	notmuch_message_properties_t *list = NULL;

	for (list = notmuch_message_get_properties (message, "session-key", TRUE);
	     notmuch_message_properties_valid (list); notmuch_message_properties_move_to_next (list)) {
	    if (err && *err) {
		g_error_free (*err);
		*err = NULL;
	    }
	    if (attempted)
		*attempted = true;
	    ret = g_mime_multipart_encrypted_decrypt (part,
						      GMIME_DECRYPT_NONE,
						      notmuch_message_properties_value (list),
						      decrypt_result, err);
	    if (ret)
		break;
	}
	if (list)
	    notmuch_message_properties_destroy (list);
	if (ret)
	    return ret;
    }

    if (err && *err) {
	g_error_free (*err);
	*err = NULL;
    }

    if (decrypt == NOTMUCH_DECRYPT_AUTO)
	return ret;

    if (attempted)
	*attempted = true;
    GMimeDecryptFlags flags = GMIME_DECRYPT_NONE;
    if (decrypt == NOTMUCH_DECRYPT_TRUE && decrypt_result)
	flags |= GMIME_DECRYPT_EXPORT_SESSION_KEY;
    ret = g_mime_multipart_encrypted_decrypt(part, flags, NULL,
					     decrypt_result, err);
    return ret;
}
