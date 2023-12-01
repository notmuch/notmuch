#include "notmuch-private.h"

#include <mutex>

static void do_init ()
{
    /* Initialize the GLib type system and threads */
#if ! GLIB_CHECK_VERSION (2, 35, 1)
    g_type_init ();
#endif

    g_mime_init ();
    _notmuch_filter_init ();
}

void
_notmuch_init ()
{
    static std::once_flag initialized;
    std::call_once (initialized, do_init);
}
