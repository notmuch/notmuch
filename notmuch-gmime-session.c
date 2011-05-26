#include "notmuch-client.h"

/* CRUFTY BOILERPLATE for GMimeSession (dkg thinks this will go away once GMime 2.6 comes out) */
typedef struct _NotmuchGmimeSession NotmuchGmimeSession;
typedef struct _NotmuchGmimeSessionClass NotmuchGmimeSessionClass;

struct _NotmuchGmimeSession {
    GMimeSession parent_object;
};

struct _NotmuchGmimeSessionClass {
    GMimeSessionClass parent_class;
};

static void notmuch_gmime_session_class_init (NotmuchGmimeSessionClass *klass);

static GMimeSessionClass *parent_class = NULL;

GType
notmuch_gmime_session_get_type (void)
{
    static GType type = 0;

    if (!type) {
	static const GTypeInfo info = {
	    sizeof (NotmuchGmimeSessionClass),
	    NULL, /* base_class_init */
	    NULL, /* base_class_finalize */
	    (GClassInitFunc) notmuch_gmime_session_class_init,
	    NULL, /* class_finalize */
	    NULL, /* class_data */
	    sizeof (NotmuchGmimeSession),
	    0,    /* n_preallocs */
	    NULL, /* object_init */
	    NULL, /* value_table */
	};
	type = g_type_register_static (GMIME_TYPE_SESSION, "NotmuchGmimeSession", &info, 0);
    }
    return type;
}

static void
notmuch_gmime_session_class_init (NotmuchGmimeSessionClass *klass)
{
    GMimeSessionClass *session_class = GMIME_SESSION_CLASS (klass);
    parent_class = g_type_class_ref (GMIME_TYPE_SESSION);
    session_class->request_passwd = NULL;
}
/* END CRUFTY BOILERPLATE */
