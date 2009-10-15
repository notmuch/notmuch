PROGS=notmuch-index-message xapian-dump

MYCXXFLAGS=-Wall -O0 -g

all: $(PROGS)

notmuch-index-message: notmuch-index-message.cc
	$(CC) $(CXXFLAGS) $(MYCXXFLAGS) notmuch-index-message.cc `pkg-config --cflags --libs gmime-2.4` `xapian-config --cxxflags --libs` -o notmuch-index-message

xapian-dump: xapian-dump.cc
	$(CXX) $(CXXFLAGS) $(MYCXXFLAGS) xapian-dump.cc `xapian-config --libs --cxxflags` -o xapian-dump

clean:
	rm -f $(PROGS)
