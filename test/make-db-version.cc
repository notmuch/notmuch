/* Create an empty notmuch database with a specific version and
 * features. */

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <xapian.h>

int main(int argc, char **argv)
{
    if (argc != 4) {
	fprintf (stderr, "Usage: %s mailpath version features\n", argv[0]);
	exit (2);
    }

    std::string nmpath (argv[1]);
    nmpath += "/.notmuch";
    if (mkdir (nmpath.c_str (), 0777) < 0) {
	perror (("failed to create " + nmpath).c_str ());
	exit (1);
    }

    try {
	Xapian::WritableDatabase db (
	    nmpath + "/xapian", Xapian::DB_CREATE_OR_OPEN);
	db.set_metadata ("version", argv[2]);
	db.set_metadata ("features", argv[3]);
	db.commit ();
    } catch (const Xapian::Error &e) {
	fprintf (stderr, "%s\n", e.get_description ().c_str ());
	exit (1);
    }
}
