#!/usr/bin/env bash
test_description="database revision tracking"

. ./test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "notmuch_database_get_revision"
test_C ${MAIL_DIR} <<'EOF'
#include <stdio.h>
#include <string.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   unsigned long revision;
   const char *uuid;

   unsigned long rev;

   stat = notmuch_database_open (argv[1], NOTMUCH_DATABASE_MODE_READ_ONLY, &db);
   if (stat)
       fputs ("open failed\n", stderr);
   revision = notmuch_database_get_revision (db, &uuid);
   printf("%s\t%lu\n", uuid, revision);
}
EOF
notmuch_uuid_sanitize < OUTPUT > CLEAN
cat <<'EOF' >EXPECTED
== stdout ==
UUID	53
== stderr ==
EOF
test_expect_equal_file EXPECTED CLEAN

test_done
