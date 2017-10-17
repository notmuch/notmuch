#ifndef _CRYPTO_H
#define _CRYPTO_H

#include <stdbool.h>
#if (GMIME_MAJOR_VERSION < 3)
#include "gmime-extra.h"
#include "notmuch.h"
#endif

typedef struct _notmuch_crypto {
    bool verify;
    bool decrypt;
#if (GMIME_MAJOR_VERSION < 3)
    GMimeCryptoContext* gpgctx;
    GMimeCryptoContext* pkcs7ctx;
    const char *gpgpath;
#endif
} _notmuch_crypto_t;


#if (GMIME_MAJOR_VERSION < 3)
notmuch_status_t
_notmuch_crypto_get_gmime_ctx_for_protocol (_notmuch_crypto_t *crypto,
					    const char *protocol,
					    GMimeCryptoContext **ctx);
#endif

void
_notmuch_crypto_cleanup (_notmuch_crypto_t *crypto);

#endif
