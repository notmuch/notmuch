#include <iostream>
#include <cstdlib>
#include <xapian.h>

int main(int argc, char **argv) {

    if (argc < 2) {
	std::cerr << "usage: ghost-report xapian-dir" << std::endl;
	exit(1);
    }

    Xapian::Database db(argv[1]);
    std::cout << db.get_termfreq("Tghost") << std::endl;
}
