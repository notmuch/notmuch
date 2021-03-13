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
#include "error_util.h"
#define unused(x) x __attribute__ ((unused))

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

void
_notmuch_crypto_cleanup (unused(_notmuch_crypto_t *crypto))
{
}

GMimeObject *
_notmuch_crypto_decrypt (bool *attempted,
			 notmuch_decryption_policy_t decrypt,
			 notmuch_message_t *message,
			 GMimeObject *part,
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
	     notmuch_message_properties_valid (list); notmuch_message_properties_move_to_next (
		 list)) {
	    if (err && *err) {
		g_error_free (*err);
		*err = NULL;
	    }
	    if (attempted)
		*attempted = true;
	    if (GMIME_IS_MULTIPART_ENCRYPTED (part)) {
		ret = g_mime_multipart_encrypted_decrypt (GMIME_MULTIPART_ENCRYPTED (part),
							  GMIME_DECRYPT_NONE,
							  notmuch_message_properties_value (list),
							  decrypt_result, err);
	    } else if (GMIME_IS_APPLICATION_PKCS7_MIME (part)) {
		GMimeApplicationPkcs7Mime *pkcs7 = GMIME_APPLICATION_PKCS7_MIME (part);
		GMimeSecureMimeType type = g_mime_application_pkcs7_mime_get_smime_type (pkcs7);
		if (type == GMIME_SECURE_MIME_TYPE_ENVELOPED_DATA) {
		    ret = g_mime_application_pkcs7_mime_decrypt (pkcs7,
								 GMIME_DECRYPT_NONE,
								 notmuch_message_properties_value (
								     list),
								 decrypt_result, err);
		}
	    }
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
    if (GMIME_IS_MULTIPART_ENCRYPTED (part)) {
	ret = g_mime_multipart_encrypted_decrypt (GMIME_MULTIPART_ENCRYPTED (part), flags, NULL,
						  decrypt_result, err);
    } else if (GMIME_IS_APPLICATION_PKCS7_MIME (part)) {
	GMimeApplicationPkcs7Mime *pkcs7 = GMIME_APPLICATION_PKCS7_MIME (part);
	GMimeSecureMimeType p7type = g_mime_application_pkcs7_mime_get_smime_type (pkcs7);
	if (p7type == GMIME_SECURE_MIME_TYPE_ENVELOPED_DATA) {
	    ret = g_mime_application_pkcs7_mime_decrypt (pkcs7, flags, NULL,
							 decrypt_result, err);
	}
    }
    return ret;
}

static int
_notmuch_message_crypto_destructor (_notmuch_message_crypto_t *msg_crypto)
{
    if (! msg_crypto)
	return 0;
    if (msg_crypto->sig_list)
	g_object_unref (msg_crypto->sig_list);
    if (msg_crypto->payload_subject)
	talloc_free (msg_crypto->payload_subject);
    return 0;
}

_notmuch_message_crypto_t *
_notmuch_message_crypto_new (void *ctx)
{
    _notmuch_message_crypto_t *ret = talloc_zero (ctx, _notmuch_message_crypto_t);

    talloc_set_destructor (ret, _notmuch_message_crypto_destructor);
    return ret;
}

notmuch_status_t
_notmuch_message_crypto_potential_sig_list (_notmuch_message_crypto_t *msg_crypto,
					    GMimeSignatureList *sigs)
{
    if (! msg_crypto)
	return NOTMUCH_STATUS_NULL_POINTER;

    /* Signatures that arrive after a payload part during DFS are not
     * part of the cryptographic envelope: */
    if (msg_crypto->payload_encountered)
	return NOTMUCH_STATUS_SUCCESS;

    if (msg_crypto->sig_list)
	g_object_unref (msg_crypto->sig_list);

    /* This signature list needs to persist as long as the _n_m_crypto
     * object survives. Increasing its reference counter prevents
     * garbage-collection until after _n_m_crypto_destroy is
     * called. */
    msg_crypto->sig_list = sigs;
    if (sigs)
	g_object_ref (sigs);

    if (msg_crypto->decryption_status == NOTMUCH_MESSAGE_DECRYPTED_FULL)
	msg_crypto->signature_encrypted = true;

    return NOTMUCH_STATUS_SUCCESS;
}


bool
_notmuch_message_crypto_potential_payload (_notmuch_message_crypto_t *msg_crypto, GMimeObject *part,
					   GMimeObject *parent, int childnum)
{
    const char *protected_headers = NULL;
    const char *forwarded = NULL;
    const char *subject = NULL;

    if ((! msg_crypto) || (! part))
	INTERNAL_ERROR ("_notmuch_message_crypto_potential_payload() got NULL for %s\n",
			msg_crypto? "part" : "msg_crypto");

    /* only fire on the first payload part encountered */
    if (msg_crypto->payload_encountered)
	return false;

    /* the first child of multipart/encrypted that matches the
     * encryption protocol should be "control information" metadata,
     * not payload.  So we skip it. (see
     * https://tools.ietf.org/html/rfc1847#page-8) */
    if (parent && GMIME_IS_MULTIPART_ENCRYPTED (parent) && childnum ==
	GMIME_MULTIPART_ENCRYPTED_VERSION) {
	const char *enc_type = g_mime_object_get_content_type_parameter (parent, "protocol");
	GMimeContentType *ct = g_mime_object_get_content_type (part);
	if (ct && enc_type) {
	    const char *part_type = g_mime_content_type_get_mime_type (ct);
	    if (part_type && strcmp (part_type, enc_type) == 0)
		return false;
	}
    }

    msg_crypto->payload_encountered = true;

    /* don't bother recording anything if there is no cryptographic
     * envelope: */
    if ((msg_crypto->decryption_status != NOTMUCH_MESSAGE_DECRYPTED_FULL) &&
	(msg_crypto->sig_list == NULL))
	return false;

    /* Verify that this payload has headers that are intended to be
     * exported to the larger message: */

    /* Consider a payload that uses Alexei Melinkov's forwarded="no" for
     * message/global or message/rfc822:
     * https://tools.ietf.org/html/draft-melnikov-smime-header-signing-05#section-4 */
    forwarded = g_mime_object_get_content_type_parameter (part, "forwarded");
    if (GMIME_IS_MESSAGE_PART (part) && forwarded && strcmp (forwarded, "no") == 0) {
	GMimeMessage *message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));
	subject = g_mime_message_get_subject (message);
	/* FIXME: handle more than just Subject: at some point */
    } else {
	/* Consider "memoryhole"-style protected headers as practiced by Enigmail and K-9 */
	protected_headers = g_mime_object_get_content_type_parameter (part, "protected-headers");
	if (protected_headers && strcasecmp ("v1", protected_headers) == 0)
	    subject = g_mime_object_get_header (part, "Subject");
	/* FIXME: handle more than just Subject: at some point */
    }

    if (subject) {
	if (msg_crypto->payload_subject)
	    talloc_free (msg_crypto->payload_subject);
	msg_crypto->payload_subject = talloc_strdup (msg_crypto, subject);
    }

    return true;
}


notmuch_status_t
_notmuch_message_crypto_successful_decryption (_notmuch_message_crypto_t *msg_crypto)
{
    if (! msg_crypto)
	return NOTMUCH_STATUS_NULL_POINTER;

    /* see the rationale for different values of
     * _notmuch_message_decryption_status_t in util/crypto.h */
    if (! msg_crypto->payload_encountered)
	msg_crypto->decryption_status = NOTMUCH_MESSAGE_DECRYPTED_FULL;
    else if (msg_crypto->decryption_status == NOTMUCH_MESSAGE_DECRYPTED_NONE)
	msg_crypto->decryption_status = NOTMUCH_MESSAGE_DECRYPTED_PARTIAL;

    return NOTMUCH_STATUS_SUCCESS;
}
