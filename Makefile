PROGS=notmuch

MYCFLAGS=-Wall -O0 -g `pkg-config --cflags glib-2.0 talloc`
MYCXXFLAGS=$(MYCFLAGS) `xapian-config --cxxflags`

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
	$(CXX) -c $(CXXFLAGS) $(MYCXXFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(MYCFLAGS) $< -o $@

notmuch: $(MODULES)
	$(CC) $(MYLDFLAGS) $^ -o $@

Makefile.dep: *.c *.cc
	$(CC) -M $(CPPFLAGS) $(MYCFLAGS) $^ > $@
-include Makefile.dep

clean:
	rm -f $(PROGS) *.o Makefile.dep
