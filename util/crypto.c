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
			 g_mime_3_unused(GMimeCryptoContext* crypto_ctx),
			 GMimeMultipartEncrypted *part,
			 GMimeDecryptResult **decrypt_result,
			 GError **err)
{
    GMimeObject *ret = NULL;
    if (decrypt == NOTMUCH_DECRYPT_FALSE)
	return NULL;

    /* the versions of notmuch that can support session key decryption */
#if HAVE_GMIME_SESSION_KEYS
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
#endif

    if (err && *err) {
	g_error_free (*err);
	*err = NULL;
    }

    if (decrypt == NOTMUCH_DECRYPT_AUTO)
	return ret;

    if (attempted)
	*attempted = true;
#if (GMIME_MAJOR_VERSION < 3)
#if HAVE_GMIME_SESSION_KEYS
    gboolean oldgetsk = g_mime_crypto_context_get_retrieve_session_key (crypto_ctx);
    gboolean newgetsk = (decrypt == NOTMUCH_DECRYPT_TRUE && decrypt_result);
    if (newgetsk != oldgetsk)
	/* This could return an error, but we can't do anything about it, so ignore it */
	g_mime_crypto_context_set_retrieve_session_key (crypto_ctx, newgetsk, NULL);
#endif
    ret = g_mime_multipart_encrypted_decrypt(part, crypto_ctx,
					     decrypt_result, err);
#if HAVE_GMIME_SESSION_KEYS
    if (newgetsk != oldgetsk)
	g_mime_crypto_context_set_retrieve_session_key (crypto_ctx, oldgetsk, NULL);
#endif
#else
    GMimeDecryptFlags flags = GMIME_DECRYPT_NONE;
    if (decrypt == NOTMUCH_DECRYPT_TRUE && decrypt_result)
	flags |= GMIME_DECRYPT_EXPORT_SESSION_KEY;
    ret = g_mime_multipart_encrypted_decrypt(part, flags, NULL,
					     decrypt_result, err);
#endif
    return ret;
}
