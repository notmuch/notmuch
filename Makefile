PROGS=g_mime_test xapian-dump

MYCFLAGS=-Wall
MYCXXFLAGS=-Wall

all: $(PROGS)

g_mime_test: g_mime_test.c
	$(CC) $(CFLAGS) $(MYCFLAGS) g_mime_test.c `pkg-config --cflags --libs gmime-2.4` -o g_mime_test

xapian-dump: xapian-dump.cc
	$(CXX) $(CXXFLAGS) $(MYCXXFLAGS) xapian-dump.cc `xapian-config --libs --cxxflags` -o xapian-dump

clean:
	rm -f $(PROGS)
