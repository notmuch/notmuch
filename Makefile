PROGS=notmuch notmuch-index-message xapian-dump

MYCFLAGS=-Wall -O0 -g
MYCXXFLAGS=-Wall -O0 -g

all: $(PROGS)

notmuch: notmuch.c
	$(CC) $(CFLAGS) $(MYCFLAGS) notmuch.c `pkg-config --cflags --libs glib-2.0` -o notmuch

notmuch-index-message: notmuch-index-message.cc
	$(CXX) $(CXXFLAGS) $(MYCXXFLAGS) notmuch-index-message.cc `pkg-config --cflags --libs gmime-2.4` `xapian-config --cxxflags --libs` -o notmuch-index-message

xapian-dump: xapian-dump.cc
	$(CXX) $(CXXFLAGS) $(MYCXXFLAGS) xapian-dump.cc `xapian-config --libs --cxxflags` -o xapian-dump

clean:
	rm -f $(PROGS)
