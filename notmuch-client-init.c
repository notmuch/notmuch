#include "notmuch-client.h"
#include "gmime-filter-reply.h"

/* Caller is responsible for only calling this once */

void
notmuch_client_init (void)
{
#if ! GLIB_CHECK_VERSION (2, 35, 1)
    g_type_init ();
#endif

    g_mime_init ();

    g_mime_filter_reply_module_init ();

    talloc_enable_null_tracking ();
}
