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

test_begin_subtest "search succeeds with correct uuid"
test_expect_success "notmuch search --uuid=$(cat UUID) '*'"

test_begin_subtest "uuid works as global option"
test_expect_success "notmuch --uuid=$(cat UUID) search '*'"

test_begin_subtest "uuid works as global option II"
test_expect_code 1 "notmuch --uuid=this-is-no-uuid search '*'"

test_begin_subtest "search fails with incorrect uuid"
test_expect_code 1 "notmuch search --uuid=this-is-no-uuid '*'"

test_begin_subtest "show succeeds with correct uuid"
test_expect_success "notmuch show --uuid=$(cat UUID) '*'"

test_begin_subtest "show fails with incorrect uuid"
test_expect_code 1 "notmuch show --uuid=this-is-no-uuid '*'"

test_begin_subtest "tag succeeds with correct uuid"
test_expect_success "notmuch tag --uuid=$(cat UUID) +test '*'"

test_begin_subtest "tag fails with incorrect uuid"
test_expect_code 1 "notmuch tag --uuid=this-is-no-uuid '*' +test2"

test_begin_subtest 'lastmod:0.. matches everything'
total=$(notmuch count '*')
modtotal=$(notmuch count lastmod:0..)
test_expect_equal "$total" "$modtotal"

test_begin_subtest 'lastmod:1000000.. matches nothing'
modtotal=$(notmuch count lastmod:1000000..)
test_expect_equal 0 "$modtotal"

test_begin_subtest 'exclude one message using lastmod'
lastmod=$(notmuch count --lastmod '*' | cut -f3)
total=$(notmuch count '*')
notmuch tag +4EFC743A.3060609@april.org id:4EFC743A.3060609@april.org
subtotal=$(notmuch count lastmod:..$lastmod)
result=$(($subtotal == $total-1))
test_expect_equal 1 "$result"

test_done
