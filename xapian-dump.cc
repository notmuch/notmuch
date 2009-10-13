/* xapian-dump: Dump document IDs and associated terms from a Xapian database
 *
 * Copyright © 2009 Carl Worth
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include <cstdlib>
#include <iostream>

#include <xapian.h>

using namespace std;

static void
print_document (Xapian::Database db, Xapian::docid id)
{
    Xapian::TermIterator i;

    printf ("Document %u:\n", id);

    for (i = db.termlist_begin (id); i != db.termlist_end (id); i++)
	cout << "\t" << *i << endl;
}

int
main (int argc, char *argv[])
{
    const char *database_path;

    if (argc < 2) {
	fprintf (stderr, "Usage: %s <path-to-xapian-database>\n",
		 argv[0]);
	exit (1);
    }

    database_path = argv[1];

    try {

	Xapian::Database db;
        Xapian::PostingIterator i;
	Xapian::docid doc_id;

	db = Xapian::Database (database_path);
	for (i = db.postlist_begin (""); i != db.postlist_end (""); i++) {
	    doc_id = *i;

	    print_document (db, doc_id);
	}

    } catch (const Xapian::Error &error) {
	cerr << "A Xapian exception occurred: " << error.get_msg () << endl;
	exit (1);
    }

    return 0;
}
