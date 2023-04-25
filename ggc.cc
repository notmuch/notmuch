#include <xapian.h>
#include <iostream>
int main(int argc, char **argv){
  if (argc != 2) {
    fprintf (stderr, "usage: ggc xapian-database\n");
    exit (1);
  }

  Xapian::Database db(argv[1]);
  Xapian::Enquire enquire(db);

  enquire.set_query(Xapian::Query("Tghost"));

  auto mset = enquire.get_mset (0,db.get_doccount ());

  for (auto iter=mset.begin (); iter != mset.end(); iter++){
    std::string mid, tid;
    auto doc = iter.get_document ();
    auto term_iter = doc.termlist_begin ();

    term_iter.skip_to ("Q");
    mid=(*term_iter).substr(1);
    std::cout << "docid = " <<  *iter;
    std::cout << " mid " << mid << std::endl;
  }
}
