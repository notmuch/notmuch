DEFS+= -DGITHEAD=\"$(GITHEAD)\"
AM_CPPFLAGS= \
  -I$(top_builddir)/maildrop/numlib \
  -I$(top_srcdir)/maildrop \
  -I$(top_srcdir)/maildrop/maildir
AM_CFLAGS= @NOTMUCH_DELIVER_CFLAGS@ $(glib_CFLAGS)

noinst_HEADERS=\
	       $(top_srcdir)/maildrop/maildir/maildircreate.h \
	       $(top_srcdir)/maildrop/maildir/maildirmisc.h

bin_PROGRAMS=\
	     notmuch-deliver

notmuch_deliver_SOURCES=\
			$(top_srcdir)/maildrop/maildir/maildircreate.c \
			$(top_srcdir)/maildrop/maildir/maildiropen.c \
			$(top_srcdir)/maildrop/maildir/maildirmkdir.c \
			main.c
notmuch_deliver_LDADD=\
		      $(top_builddir)/maildrop/numlib/libnumlib.la \
		      $(glib_LIBS)
