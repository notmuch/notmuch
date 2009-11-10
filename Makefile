PROGS=notmuch

WARN_FLAGS=-Wall -Wextra -Wmissing-declarations -Wwrite-strings -Wswitch-enum

NOTMUCH_DEPENDS_FLAGS=-I./lib `pkg-config --cflags glib-2.0 gmime-2.4 talloc`
NOTMUCH_CXX_DEPENDS_FLAGS=$(NOTMUCH_DEPENDS_FLAGS) `xapian-config --cxxflags`

NOTMUCH_CFLAGS=$(WARN_FLAGS) -O0 -g $(NOTMUCH_DEPENDS_FLAGS)
NOTMUCH_CXXFLAGS=$(WARN_FLAGS) -O0 -g $(NOTMUCH_CXX_DEPENDS_FLAGS)

NOTMUCH_LDFLAGS=`pkg-config --libs glib-2.0 gmime-2.4 talloc` \
		`xapian-config --libs`

LIBRARY=			\
	lib/database.o		\
	lib/index.o		\
	lib/libsha1.o		\
	lib/message.o		\
	lib/message-file.o	\
	lib/query.o		\
	lib/sha1.o		\
	lib/tags.o		\
	lib/thread.o		\
	lib/xutil.o

MAIN=			\
	notmuch.o

all: $(PROGS)

%.o: %.cc
	$(CXX) -c $(CFLAGS) $(CXXFLAGS) $(NOTMUCH_CXXFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(NOTMUCH_CFLAGS) $< -o $@

notmuch: $(MAIN) $(LIBRARY)
	$(CC) $(NOTMUCH_LDFLAGS) $^ -o $@

Makefile.dep: *.c lib/*.c lib/*.cc
	$(CXX) -M $(CPPFLAGS) $(NOTMUCH_DEPENDS_FLAGS) \
	$(NOTMUCH_CXX_DEPENDS_FLAGS) $^ > $@
-include Makefile.dep

notmuch.1.gz:
	gzip --stdout notmuch.1 > notmuch.1.gz

install: all notmuch.1.gz
	install -C -D notmuch $(DESTDIR)/usr/bin/notmuch
	install -C -D notmuch.1.gz $(DESTDIR)/usr/share/man/man1
	install -C -D notmuch-completion.bash \
		$(DESTDIR)/etc/bash_completion.d/notmuch

clean:
	rm -f $(PROGS) lib/*.o *.o Makefile.dep
