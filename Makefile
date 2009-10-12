PROGS=g_mime_test

all: $(PROGS)

g_mime_test: g_mime_test.c
	$(CC) g_mime_test.c `pkg-config --cflags --libs gmime-2.4` -o g_mime_test

clean:
	rm -f $(PROGS)
