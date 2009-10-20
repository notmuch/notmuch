PROGS=notmuch

MYCFLAGS=-Wall -O0 -g `pkg-config --cflags glib-2.0`
MYCXXFLAGS=$(MYCFLAGS) `xapian-config --cxxflags`

MYLDFLAGS=`pkg-config --libs glib-2.0` `xapian-config --libs`

all: $(PROGS)

%.o: %.cc
	$(CXX) -c $(CXXFLAGS) $(MYCXXFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(MYCFLAGS) $< -o $@

notmuch: notmuch.o database.o date.o message.o xutil.o
	$(CC) $(MYLDFLAGS) $^ -o $@

Makefile.dep: *.c *.cc
	$(CC) -M $(CPPFLAGS) $(MYCFLAGS) $^ > $@
-include Makefile.dep

clean:
	rm -f $(PROGS) *.o Makefile.dep
