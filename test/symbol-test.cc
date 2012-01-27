#include <stdio.h>
#include <xapian.h>
#include <notmuch.h>


int main() {
  (void) notmuch_database_open("fakedb", NOTMUCH_DATABASE_MODE_READ_ONLY);

  try {
    (void) new Xapian::WritableDatabase("./nonexistant", Xapian::DB_OPEN);
  } catch (const Xapian::Error &error) {
    printf("caught %s\n", error.get_msg().c_str());
    return 0;
  }

  return 1;
}
