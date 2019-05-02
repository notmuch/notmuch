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
			 GMimeMultipartEncrypted *part,
			 GMimeDecryptResult **decrypt_result,
			 GError **err);

void
_notmuch_crypto_cleanup (_notmuch_crypto_t *crypto);

#ifdef __cplusplus
}
#endif
#endif
