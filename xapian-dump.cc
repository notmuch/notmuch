/* xapian-dump: Create a textual dump of a Xapian database.
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

/* Currently the dumped data includes:
 *
 * 	All document IDs
 *
 * And for each document ID:
 *
 *	All terms
 *	All values
 *
 * Things not yet dumped include:
 *
 * Data associated with a document.
 */

#include <cstdlib>
#include <iostream>

#include <xapian.h>

using namespace std;

static void
print_document_terms (Xapian::Document doc)
{
    Xapian::TermIterator i;

    printf ("Terms:\n");

    for (i = doc.termlist_begin (); i != doc.termlist_end (); i++)
	cout << "\t" << *i << endl;
}

static void
print_document_values (Xapian::Document doc)
{
    Xapian::ValueIterator i;

    printf ("Values:\n");

    for (i = doc.values_begin (); i != doc.values_end (); i++)
	cout << "\t" << i.get_valueno() << ": " << *i << endl;
}

static void
print_document (Xapian::Database db, Xapian::docid id)
{
    Xapian::Document doc;

    printf ("Document %u:\n", id);

    doc = db.get_document (id);

    print_document_terms (doc);

    print_document_values (doc);
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
