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
