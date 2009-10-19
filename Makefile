PROGS=notmuch notmuch-index-message xapian-dump

MYCFLAGS=-Wall -O0 -g `pkg-config --cflags gmime-2.4`
MYCXXFLAGS=$(MYCFLAGS) `xapian-config --cxxflags`

MYLDFLAGS=`pkg-config --libs gmime-2.4` `xapian-config --libs`

all: $(PROGS)

%.o: %.cc
	$(CXX) -c $(CXXFLAGS) $(MYCXXFLAGS) $^ -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(MYCFLAGS) $^ -o $@

notmuch: notmuch.o database.o xutil.o
	$(CC) $(MYLDFLAGS) $^ -o $@

notmuch-index-message: notmuch-index-message.cc
	$(CXX) $(CXXFLAGS) $(MYCXXFLAGS) notmuch-index-message.cc `pkg-config --cflags --libs gmime-2.4` `xapian-config --cxxflags --libs` -o notmuch-index-message

xapian-dump: xapian-dump.cc
	$(CXX) $(CXXFLAGS) $(MYCXXFLAGS) xapian-dump.cc `xapian-config --libs --cxxflags` -o xapian-dump

clean:
	rm -f $(PROGS) *.o
