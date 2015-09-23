#include <stdio.h>
#include <stdlib.h>
#include <xapian.h>
#include <notmuch.h>

int main (int argc, char** argv)
{
    notmuch_database_t *notmuch;
    char *message = NULL;

    if (argc != 3)
	return 1;

    if (notmuch_database_open_verbose (argv[1], NOTMUCH_DATABASE_MODE_READ_ONLY,
				       &notmuch, &message)) {
	if (message) {
	    fputs (message, stderr);
	    free (message);
	}
    }

    try {
	(void) new Xapian::WritableDatabase (argv[2], Xapian::DB_OPEN);
    } catch (const Xapian::Error &error) {
	printf("caught %s\n", error.get_msg().c_str());
	return 0;
    }

    return 1;
}
