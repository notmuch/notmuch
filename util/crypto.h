#ifndef _CRYPTO_H
#define _CRYPTO_H

#include <stdbool.h>
#include "gmime-extra.h"
#include "notmuch.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _notmuch_crypto {
    bool verify;
    notmuch_decryption_policy_t decrypt;
} _notmuch_crypto_t;

GMimeObject *
_notmuch_crypto_decrypt (bool *attempted,
			 notmuch_decryption_policy_t decrypt,
			 notmuch_message_t *message,
			 GMimeObject *part,
			 GMimeDecryptResult **decrypt_result,
			 GError **err);

void
_notmuch_crypto_cleanup (_notmuch_crypto_t *crypto);

/* The user probably wants to know if the entire message was in the
 * clear.  When replying, the MUA probably wants to know whether there
 * was any part decrypted in the message.  And when displaying to the
 * user, we probably only want to display "encrypted message" if the
 * entire message was covered by encryption. */
typedef enum {
    NOTMUCH_MESSAGE_DECRYPTED_NONE = 0,
    NOTMUCH_MESSAGE_DECRYPTED_PARTIAL,
    NOTMUCH_MESSAGE_DECRYPTED_FULL,
} _notmuch_message_decryption_status_t;

/* description of the cryptographic state of a given message overall;
 * for use by simple user agents.
 */
typedef struct _notmuch_message_crypto {
    /* encryption status: partial, full, none */
    _notmuch_message_decryption_status_t decryption_status;
    /* FIXME: can we show what key(s) a fully-encrypted message was
     * encrypted to? This data is not necessarily cryptographically
     * reliable; even when we decrypt, we might not know which public
     * key was used (e.g. if we're using a session key). */

    /* signature status of the whole message (either the whole message
     * is signed, or it is not) -- this means that partially-signed
     * messages will get no signature status. */
    GMimeSignatureList *sig_list;
    /* if part of the message was signed, and the MUA is clever, it
     * can determine on its own exactly which part and try to make
     * more sense of it. */

    /* mark this flag once we encounter a payload (i.e. something that
     * is not part of the cryptographic envelope) */
    bool payload_encountered;

    /* the value of any "Subject:" header in the cryptographic payload
     * (the top level part within the crypto envelope), converted to
     * UTF-8 */
    char *payload_subject;

    /* if both signed and encrypted, was the signature encrypted? */
    bool signature_encrypted;
} _notmuch_message_crypto_t;


/* _notmuch_message_crypto_t objects should be released with
 * talloc_free (), or they will be released along with their parent
 * context.
 */
_notmuch_message_crypto_t *
_notmuch_message_crypto_new (void *ctx);

/* call potential_sig_list during a depth-first-search on a message to
 * consider a particular signature as relevant for the message.
 */
notmuch_status_t
_notmuch_message_crypto_potential_sig_list (_notmuch_message_crypto_t *msg_crypto,
					    GMimeSignatureList *sigs);

/* call successful_decryption during a depth-first-search on a message
 * to indicate that a part was successfully decrypted.
 */
notmuch_status_t
_notmuch_message_crypto_successful_decryption (_notmuch_message_crypto_t *msg_crypto);

/* call potential_payload during a depth-first-search on a message
 * when encountering a message part that is not part of the envelope.
 *
 * Returns true if part is the root of the cryptographic payload of
 * this message.
 */
bool
_notmuch_message_crypto_potential_payload (_notmuch_message_crypto_t *msg_crypto, GMimeObject *part,
					   GMimeObject *parent, int childnum);


#ifdef __cplusplus
}
#endif
#endif
