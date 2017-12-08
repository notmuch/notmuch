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

#if (GMIME_MAJOR_VERSION < 3)
/* Create or pass on a GPG context (GMime 2.6) */
static notmuch_status_t
get_gpg_context (_notmuch_crypto_t *crypto, GMimeCryptoContext **ctx)
{
    if (ctx == NULL || crypto == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (crypto->gpgctx) {
	*ctx = crypto->gpgctx;
	return NOTMUCH_STATUS_SUCCESS;
    }

    /* TODO: GMimePasswordRequestFunc */
    crypto->gpgctx = g_mime_gpg_context_new (NULL, crypto->gpgpath ? crypto->gpgpath : "gpg");
    if (! crypto->gpgctx) {
	return NOTMUCH_STATUS_FAILED_CRYPTO_CONTEXT_CREATION;
    }

    g_mime_gpg_context_set_use_agent ((GMimeGpgContext *) crypto->gpgctx, true);
    g_mime_gpg_context_set_always_trust ((GMimeGpgContext *) crypto->gpgctx, false);

    *ctx = crypto->gpgctx;
    return NOTMUCH_STATUS_SUCCESS;
}

/* Create or pass on a PKCS7 context (GMime 2.6) */
static notmuch_status_t
get_pkcs7_context (_notmuch_crypto_t *crypto, GMimeCryptoContext **ctx)
{
    if (ctx == NULL || crypto == NULL)
	return NOTMUCH_STATUS_NULL_POINTER;

    if (crypto->pkcs7ctx) {
	*ctx = crypto->pkcs7ctx;
	return NOTMUCH_STATUS_SUCCESS;
    }

    /* TODO: GMimePasswordRequestFunc */
    crypto->pkcs7ctx = g_mime_pkcs7_context_new (NULL);
    if (! crypto->pkcs7ctx) {
	return NOTMUCH_STATUS_FAILED_CRYPTO_CONTEXT_CREATION;
    }

    g_mime_pkcs7_context_set_always_trust ((GMimePkcs7Context *) crypto->pkcs7ctx,
					   false);

    *ctx = crypto->pkcs7ctx;
    return NOTMUCH_STATUS_SUCCESS;
}
static const struct {
    const char *protocol;
    notmuch_status_t (*get_context) (_notmuch_crypto_t *crypto, GMimeCryptoContext **ctx);
} protocols[] = {
    {
	.protocol = "application/pgp-signature",
	.get_context = get_gpg_context,
    },
    {
	.protocol = "application/pgp-encrypted",
	.get_context = get_gpg_context,
    },
    {
	.protocol = "application/pkcs7-signature",
	.get_context = get_pkcs7_context,
    },
    {
	.protocol = "application/x-pkcs7-signature",
	.get_context = get_pkcs7_context,
    },
};

/* for the specified protocol return the context pointer (initializing
 * if needed) */
notmuch_status_t
_notmuch_crypto_get_gmime_ctx_for_protocol (_notmuch_crypto_t *crypto,
					    const char *protocol,
					    GMimeCryptoContext **ctx)
{
    if (! protocol)
	return NOTMUCH_STATUS_MALFORMED_CRYPTO_PROTOCOL;

    /* As per RFC 1847 section 2.1: "the [protocol] value token is
     * comprised of the type and sub-type tokens of the Content-Type".
     * As per RFC 1521 section 2: "Content-Type values, subtypes, and
     * parameter names as defined in this document are
     * case-insensitive."  Thus, we use strcasecmp for the protocol.
     */
    for (size_t i = 0; i < ARRAY_SIZE (protocols); i++) {
	if (strcasecmp (protocol, protocols[i].protocol) == 0)
	    return protocols[i].get_context (crypto, ctx);
    }

    return NOTMUCH_STATUS_UNKNOWN_CRYPTO_PROTOCOL;
}

void
_notmuch_crypto_cleanup (_notmuch_crypto_t *crypto)
{
    if (crypto->gpgctx) {
	g_object_unref (crypto->gpgctx);
	crypto->gpgctx = NULL;
    }

    if (crypto->pkcs7ctx) {
	g_object_unref (crypto->pkcs7ctx);
	crypto->pkcs7ctx = NULL;
    }
}
#else
void _notmuch_crypto_cleanup (unused(_notmuch_crypto_t *crypto))
{
}
#endif

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
#if (GMIME_MAJOR_VERSION < 3)
	    ret = g_mime_multipart_encrypted_decrypt_session (part,
							      crypto_ctx,
							      notmuch_message_properties_value (list),
							      decrypt_result, err);
#else
	    ret = g_mime_multipart_encrypted_decrypt (part,
						      GMIME_DECRYPT_NONE,
						      notmuch_message_properties_value (list),
						      decrypt_result, err);
#endif
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
