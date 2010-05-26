DEFS+= -DGITHEAD=\"$(GITHEAD)\"
AM_CFLAGS= @NOTMUCH_DELIVER_CFLAGS@ $(glib_CFLAGS)

noinst_HEADERS=\
	       maildircreate.h \
	       maildirmisc.h

bin_PROGRAMS=\
	     notmuch-deliver

notmuch_deliver_SOURCES=\
			maildircreate.c \
			maildiropen.c \
			maildirmkdir.c \
			main.c
notmuch_deliver_LDADD=\
		      $(top_builddir)/numlib/libnumlib.la \
		      $(glib_LIBS)
