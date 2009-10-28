PROGS=notmuch

WARN_FLAGS=-Wall -Wextra -Wmissing-declarations -Wwrite-strings -Wswitch-enum

CDEPENDS_FLAGS=`pkg-config --cflags glib-2.0 gmime-2.4 talloc`
CXXDEPENDS_FLAGS=$(CDEPENDS_FLAGS) `xapian-config --cxxflags`

MYCFLAGS=$(WARN_FLAGS) -O0 -g $(CDEPENDS_FLAGS)
MYCXXFLAGS=$(WARN_FLAGS) -O0 -g $(CXXDEPENDS_FLAGS)

MYLDFLAGS=`pkg-config --libs glib-2.0 gmime-2.4 talloc` `xapian-config --libs`

LIBRARY=		\
	database.o	\
	date.o		\
	index.o		\
	libsha1.o	\
	message.o	\
	message-file.o	\
	query.o		\
	sha1.o		\
	tags.o		\
	thread.o	\
	xutil.o

MAIN=			\
	notmuch.o

all: $(PROGS)

%.o: %.cc
	$(CXX) -c $(CFLAGS) $(CXXFLAGS) $(MYCXXFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(MYCFLAGS) $< -o $@

notmuch: $(MAIN) $(LIBRARY)
	$(CC) $(MYLDFLAGS) $^ -o $@

Makefile.dep: *.c *.cc
	$(CXX) -M $(CPPFLAGS) $(CDEPENDS_FLAGS) $(CXXDEPENDS_FLAGS) $^ > $@
-include Makefile.dep

clean:
	rm -f $(PROGS) *.o Makefile.dep
