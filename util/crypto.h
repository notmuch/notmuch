#ifndef _CRYPTO_H
#define _CRYPTO_H

#include <stdbool.h>
#include "gmime-extra.h"
#include "notmuch.h"

typedef struct _notmuch_crypto {
    bool verify;
    notmuch_decryption_policy_t decrypt;
#if (GMIME_MAJOR_VERSION < 3)
    GMimeCryptoContext* gpgctx;
    GMimeCryptoContext* pkcs7ctx;
    const char *gpgpath;
#endif
} _notmuch_crypto_t;

GMimeObject *
_notmuch_crypto_decrypt (notmuch_message_t *message,
			 GMimeCryptoContext* crypto_ctx,
			 GMimeMultipartEncrypted *part,
			 GMimeDecryptResult **decrypt_result,
			 GError **err);

#if (GMIME_MAJOR_VERSION < 3)
notmuch_status_t
_notmuch_crypto_get_gmime_ctx_for_protocol (_notmuch_crypto_t *crypto,
					    const char *protocol,
					    GMimeCryptoContext **ctx);
#endif

void
_notmuch_crypto_cleanup (_notmuch_crypto_t *crypto);

#endif
