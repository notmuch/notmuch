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

notmuch count --lastmod '*' | cut -f2 > UUID

test_expect_success 'search succeeds with correct uuid' \
		    "notmuch search --uuid=$(cat UUID) '*'"

test_expect_success 'uuid works as global option ' \
		    "notmuch --uuid=$(cat UUID) search '*'"

test_expect_code 1 'uuid works as global option II' \
		    "notmuch --uuid=this-is-no-uuid search '*'"

test_expect_code 1 'search fails with incorrect uuid' \
		 "notmuch search --uuid=this-is-no-uuid '*'"

test_expect_success 'show succeeds with correct uuid' \
		    "notmuch show --uuid=$(cat UUID) '*'"

test_expect_code 1 'show fails with incorrect uuid' \
		 "notmuch show --uuid=this-is-no-uuid '*'"

test_expect_success 'tag succeeds with correct uuid' \
		    "notmuch tag --uuid=$(cat UUID) +test '*'"

test_expect_code 1 'tag fails with incorrect uuid' \
		 "notmuch tag --uuid=this-is-no-uuid '*' +test2"

test_done
