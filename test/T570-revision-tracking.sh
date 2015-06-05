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

grep '^[0-9a-f]' OUTPUT > INITIAL_OUTPUT

test_begin_subtest "output of count matches test code"
notmuch count --lastmod '*' | cut -f2-3 > OUTPUT
test_expect_equal_file INITIAL_OUTPUT OUTPUT

test_begin_subtest "modification count increases"
before=$(notmuch count --lastmod '*' | cut -f3)
notmuch tag +a-random-tag-8743632 '*'
after=$(notmuch count --lastmod '*' | cut -f3)
result=$(($before < $after))
test_expect_equal 1 ${result}
test_done
