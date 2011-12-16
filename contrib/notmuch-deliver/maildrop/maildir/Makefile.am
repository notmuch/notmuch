#$Id: Makefile.am,v 1.59 2009/06/27 16:32:38 mrsam Exp $
#
# Copyright 1998 - 2005 Double Precision, Inc.  See COPYING for
# distribution information.


noinst_LTLIBRARIES=libmaildir.la


DOCS=	deliverquota.html.in deliverquota.8.in \
	maildiracl.1.in \
	maildir.html maildir.5 \
	maildiracl.html.in \
	maildirmake.html.in maildirmake.1.in maildirquota.html maildirquota.7 \
	maildirkw.html maildirkw.1

if HAVE_SGML
BUILT_SOURCES=maildirsharedrc.h maildirfilterconfig.h quotawarnmsg.h \
	mailbot.h autoresponsequota.h $(noinst_DATA) $(DOCS)
else
BUILT_SOURCES=maildirsharedrc.h maildirfilterconfig.h quotawarnmsg.h \
	mailbot.h autoresponsequota.h $(noinst_DATA)
endif

noinst_DATA=deliverquota.html maildirmake.html deliverquota.8 maildirmake.1 \
	maildiracl.html maildiracl.1 maildir.libdeps

libmaildir_la_SOURCES=autoresponse.c autoresponse.h \
	maildiraclt.c maildiraclt.h \
	maildircache.c maildircache.h \
	maildircreate.c maildircreate.h \
	maildirfilename.c maildirgetnew.c \
	maildirfilter.c maildirfilter2.c \
	maildirfilter.h maildirfiltertypelist.h\
	maildirflags.c maildirmkdir.c \
	maildirgetquota.c maildirgetquota.h \
	maildirinfo.c maildirinfo.h \
	maildirkeywords.c maildirkeywords2.c maildirkeywords3.c \
	maildirkeywords4.cpp \
	maildirkeywords.h maildirlist.c maildirlock.c \
	maildirmake2.c \
	maildirnewshared.c maildirnewshared.h maildirnewshared2.c \
	maildiropen.c maildirparsequota.c \
	maildirpath.c maildirpurgetmp.c maildirmisc.h \
	maildirrename.c \
	maildirsearch.c maildirsearchC.cpp maildirsearch.h \
	maildirshared.c maildirshared2.c maildirdelfolder.c\
	maildirquota.c maildirquota.h maildirrequota.c maildirrequota.h \
	maildirwatch.c maildirwatch.h loginexec.c loginexec.h

noinst_PROGRAMS=deliverquota maildirmake testmaildirfilter maildirkwtest \
	maildirkw maildiracl maildiraclttest

deliverquota_SOURCES=deliverquota.c
deliverquota_DEPENDENCIES=libmaildir.la ../rfc822/librfc822.la \
				../numlib/libnumlib.la
deliverquota_LDADD=libmaildir.la ../rfc822/librfc822.la ../numlib/libnumlib.la
deliverquota_LDFLAGS=-static

maildirmake_SOURCES=maildirmake.c
maildirmake_DEPENDENCIES=libmaildir.la ../numlib/libnumlib.la \
			../rfc822/librfc822.la
maildirmake_LDADD=libmaildir.la ../numlib/libnumlib.la \
			../rfc822/librfc822.la
maildirmake_LDFLAGS=-static

testmaildirfilter_SOURCES=maildirfiltertypelist.h testmaildirfilter.c
testmaildirfilter_DEPENDENCIES=libmaildir.la ../numlib/libnumlib.la
testmaildirfilter_LDADD=libmaildir.la ../numlib/libnumlib.la @LIBPCRE@
testmaildirfilter_LDFLAGS=-static

maildirkwtest_SOURCES=maildirkwtest.c
maildirkwtest_LDADD=libmaildir.la
maildirkwtest_DEPENDENCIES=libmaildir.la
maildirkwtest_LDFLAGS=-static

maildirkw_SOURCES=maildirkw.c
maildirkw_LDADD=libmaildir.la ../liblock/liblock.la ../numlib/libnumlib.la \
		`cat maildir.libdeps`
maildirkw_DEPENDENCIES=libmaildir.la ../liblock/liblock.la \
	../numlib/libnumlib.la maildir.libdeps
maildirkw_LDFLAGS=-static

maildiracl=maildiracl.c
maildiracl_LDADD=libmaildir.la ../liblock/liblock.la ../numlib/libnumlib.la \
			`cat maildir.libdeps`
maildiracl_DEPENDENCIES=libmaildir.la ../liblock/liblock.la \
		../numlib/libnumlib.la maildir.libdeps
maildiracl_LDFLAGS=-static

maildiraclttest_SOURCES=testmaildiraclt.c
maildiraclttest_LDADD=libmaildir.la ../liblock/liblock.la \
		../numlib/libnumlib.la `cat maildir.libdeps`
maildiraclttest_DEPENDENCIES=libmaildir.la ../liblock/liblock.la \
		../numlib/libnumlib.la maildir.libdeps
maildiraclttest_LDFLAGS=-static

EXTRA_DIST=README.maildirquota.html README.maildirquota.txt \
	README.imapkeywords.html \
	README.maildirfilter.html \
	README.sharedfolders.html README.sharedfolders.txt \
	maildirkwtest.txt \
	testsuite testsuite.txt testsuite2 testsuite2.txt quotawarnmsg $(DOCS)

HTML2TXT=links -dump -no-numbering

README.maildirquota.txt: README.maildirquota.html
	$(HTML2TXT) README.maildirquota.html >README.maildirquota.txt

README.sharedfolders.txt: README.sharedfolders.html
	$(HTML2TXT) README.sharedfolders.html >README.sharedfolders.txt

mailbot.h: config.status
	echo '#define MAILBOT "@MAILBOT@"' >mailbot.h

maildirsharedrc.h: config.status
	echo '#define MAILDIRSHAREDRC "$(sysconfdir)/maildirshared"' >maildirsharedrc.h

maildirfilterconfig.h: config.status
	echo '#define MAILDIRFILTERCONFIG "$(sysconfdir)/maildirfilterconfig"' >maildirfilterconfig.h

autoresponsequota.h: config.status
	echo '#define AUTORESPONSEQUOTA "$(sysconfdir)/autoresponsesquota"' >autoresponsequota.h

quotawarnmsg.h: config.status
	echo '#define QUOTAWARNMSG "$(sysconfdir)/quotawarnmsg"' >quotawarnmsg.h
maildir.libdeps: config.status
	echo @LIBFAM@ >maildir.libdeps

clean-local:
	rm -rf maildir.libdeps testmd

check-am:
	@SHELL@ $(srcdir)/testsuite 2>&1 | cmp - $(srcdir)/testsuite.txt
	test "@LIBPCRE@" != "" || exit 0 ; @SHELL@ $(srcdir)/testsuite2 2>&1 | cmp - $(srcdir)/testsuite2.txt
	LC_ALL=C; export LC_ALL; ./maildirkwtest | cmp -s - $(srcdir)/maildirkwtest.txt
	LC_ALL=C; export LC_ALL; ./maildiraclttest

if HAVE_SGML

deliverquota.html.in: deliverquota.sgml ../docbook/sgml2html
	../docbook/sgml2html deliverquota.sgml deliverquota.html.in

deliverquota.8.in: deliverquota.sgml ../docbook/sgml2html
	../docbook/sgml2man deliverquota.sgml deliverquota.8.in
	mv deliverquota.8 deliverquota.8.in

maildirmake.html.in: maildirmake.sgml ../docbook/sgml2html
	../docbook/sgml2html maildirmake.sgml maildirmake.html.in

maildirmake.1.in: maildirmake.sgml ../docbook/sgml2html
	../docbook/sgml2man maildirmake.sgml maildirmake.1.in
	mv maildirmake.1 maildirmake.1.in

maildiracl.html.in: maildiracl.sgml ../docbook/sgml2html
	../docbook/sgml2html maildiracl.sgml maildiracl.html.in

maildiracl.1.in: maildiracl.sgml ../docbook/sgml2html
	../docbook/sgml2man maildiracl.sgml maildiracl.1.in
	mv maildiracl.1 maildiracl.1.in

maildirquota.html: maildirquota.sgml ../docbook/sgml2html
	../docbook/sgml2html maildirquota.sgml maildirquota.html

maildirquota.7: maildirquota.sgml ../docbook/sgml2man
	../docbook/sgml2man maildirquota.sgml maildirquota.7

maildir.html: maildir.sgml ../docbook/sgml2html
	../docbook/sgml2html maildir.sgml maildir.html

maildir.5: maildir.sgml ../docbook/sgml2man
	../docbook/sgml2man maildir.sgml maildir.5

maildirkw.html: maildirkw.sgml ../docbook/sgml2html
	../docbook/sgml2html maildirkw.sgml maildirkw.html

maildirkw.1: maildirkw.sgml ../docbook/sgml2man
	../docbook/sgml2man maildirkw.sgml maildirkw.1

endif

deliverquota.html: deliverquota.html.in
	./config.status --file=$@

deliverquota.8: deliverquota.8.in
	./config.status --file=$@

maildirmake.html: maildirmake.html.in
	./config.status --file=$@

maildirmake.1: maildirmake.1.in
	./config.status --file=$@

maildiracl.html: maildiracl.html.in
	./config.status --file=$@

maildiracl.1: maildiracl.1.in
	./config.status --file=$@

# autoresponse.c: autoresponsequota.h

DISTCLEANFILES=$(BUILT_SOURCES)
