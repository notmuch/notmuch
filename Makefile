PROGS=notmuch

WARN_FLAGS=-Wall -Wextra -Wmissing-declarations -Wwrite-strings -Wswitch-enum

CDEPENDS_FLAGS=`pkg-config --cflags glib-2.0 talloc`
CXXDEPENDS_FLAGS=`pkg-config --cflags glib-2.0 talloc` `xapian-config --cxxflags`

MYCFLAGS=$(WARN_FLAGS) -O0 -g $(CDEPENDS_FLAGS)
MYCXXFLAGS=$(WARN_FLAGS) -O0 -g $(CXXDEPENDS_FLAGS)

MYLDFLAGS=`pkg-config --libs glib-2.0 talloc` `xapian-config --libs`

MODULES=		\
	notmuch.o	\
	database.o	\
	date.o		\
	message.o	\
	message-file.o	\
	query.o		\
	sha1.o		\
	libsha1.o	\
	xutil.o

all: $(PROGS)

%.o: %.cc
	$(CXX) -c $(CFLAGS) $(CXXFLAGS) $(MYCXXFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(MYCFLAGS) $< -o $@

notmuch: $(MODULES)
	$(CC) $(MYLDFLAGS) $^ -o $@

Makefile.dep: *.c *.cc
	$(CC) -M $(CPPFLAGS) $(CDEPENDS_FLAGS) $^ > $@
-include Makefile.dep

clean:
	rm -f $(PROGS) *.o Makefile.dep
