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
 *	Document data
 *	All document terms
 *	All document values
 */

#include <cstdlib>
#include <iostream>
#include <algorithm>

#include <xapian.h>

using namespace std;

vector<int> UNSERIALIZE;

static void
print_document_terms (Xapian::Document doc)
{
    Xapian::TermIterator i;

    printf ("    Terms:\n");

    for (i = doc.termlist_begin (); i != doc.termlist_end (); i++)
	cout << "\t" << *i << endl;
}

static int
vector_int_contains (vector<int> v, int i)
{
    vector<int>::iterator result;

    result = find (v.begin(), v.end(), i);

    return result != v.end();
}

static void
print_document_values (Xapian::Document doc)
{
    Xapian::ValueIterator i;
    int value_no, value_int;
    double value_float;

    printf ("    Values:\n");

    for (i = doc.values_begin (); i != doc.values_end (); i++) {
	value_no = i.get_valueno();

	cout << "\t" << i.get_valueno() << ": ";

	if (vector_int_contains (UNSERIALIZE, value_no)) {
	    value_float = Xapian::sortable_unserialise (*i);
	    value_int = value_float;
	    if (value_int == value_float)
		cout << value_int;
	    else
		cout << value_float;
	} else {
	    cout << *i;
	}

	cout << endl;
    }
}

static void
print_document (Xapian::Database db, Xapian::docid id)
{
    Xapian::Document doc;

    printf ("Document %u:\n", id);

    doc = db.get_document (id);

    printf ("    Data:\n");
    cout << "\t" << doc.get_data () << endl;

    print_document_terms (doc);

    print_document_values (doc);
}

int
main (int argc, char *argv[])
{
    const char *database_path;
    int i;

    if (argc < 2) {
	fprintf (stderr, "Usage: %s <path-to-xapian-database> [value_nos...]\n",
		 argv[0]);
	fprintf (stderr, "Dumps data from the given database.\n");
	fprintf (stderr, "The values corresponding to any value numbers given on the command line\n");
	fprintf (stderr, "will be unserialized to an before being printed.\n");
	exit (1);
    }

    database_path = argv[1];

    UNSERIALIZE = vector<int> ();

    for (i = 2; i < argc; i++)
	UNSERIALIZE.push_back (atoi (argv[i]));

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
