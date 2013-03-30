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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Authors: Jameson Rollins <jrollins@finestructure.net>
 */

#include "notmuch-client.h"

#ifdef GMIME_ATLEAST_26

/* Create a GPG context (GMime 2.6) */
static notmuch_crypto_context_t *
create_gpg_context (void)
{
    notmuch_crypto_context_t *gpgctx;

    /* TODO: GMimePasswordRequestFunc */
    gpgctx = g_mime_gpg_context_new (NULL, "gpg");
    if (! gpgctx)
	return NULL;

    g_mime_gpg_context_set_use_agent ((GMimeGpgContext *) gpgctx, TRUE);
    g_mime_gpg_context_set_always_trust ((GMimeGpgContext *) gpgctx, FALSE);

    return gpgctx;
}

#else /* GMIME_ATLEAST_26 */

/* Create a GPG context (GMime 2.4) */
static notmuch_crypto_context_t *
create_gpg_context (void)
{
    GMimeSession *session;
    notmuch_crypto_context_t *gpgctx;

    session = g_object_new (g_mime_session_get_type (), NULL);
    gpgctx = g_mime_gpg_context_new (session, "gpg");
    g_object_unref (session);

    if (! gpgctx)
	return NULL;

    g_mime_gpg_context_set_always_trust ((GMimeGpgContext *) gpgctx, FALSE);

    return gpgctx;
}

#endif /* GMIME_ATLEAST_26 */

/* for the specified protocol return the context pointer (initializing
 * if needed) */
notmuch_crypto_context_t *
notmuch_crypto_get_context (notmuch_crypto_t *crypto, const char *protocol)
{
    notmuch_crypto_context_t *cryptoctx = NULL;

    /* As per RFC 1847 section 2.1: "the [protocol] value token is
     * comprised of the type and sub-type tokens of the Content-Type".
     * As per RFC 1521 section 2: "Content-Type values, subtypes, and
     * parameter names as defined in this document are
     * case-insensitive."  Thus, we use strcasecmp for the protocol.
     */
    if (strcasecmp (protocol, "application/pgp-signature") == 0 ||
	strcasecmp (protocol, "application/pgp-encrypted") == 0) {
	if (! crypto->gpgctx) {
	    crypto->gpgctx = create_gpg_context ();
	    if (! crypto->gpgctx)
		fprintf (stderr, "Failed to construct gpg context.\n");
	}
	cryptoctx = crypto->gpgctx;
    } else {
	fprintf (stderr, "Unknown or unsupported cryptographic protocol.\n");
    }

    return cryptoctx;
}

int
notmuch_crypto_cleanup (notmuch_crypto_t *crypto)
{
    if (crypto->gpgctx) {
	g_object_unref (crypto->gpgctx);
	crypto->gpgctx = NULL;
    }

    return 0;
}
