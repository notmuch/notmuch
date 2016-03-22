#!/usr/bin/env bash
test_description="library config API"

. ./test-lib.sh || exit 1

add_email_corpus

cat <<EOF > c_head
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <notmuch.h>

void run(int line, notmuch_status_t ret)
{
   if (ret) {
	fprintf (stderr, "line %d: %s\n", line, ret);
	exit (1);
   }
}

#define RUN(v)  run(__LINE__, v);

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   char *val;
   notmuch_status_t stat;

   RUN(notmuch_database_open (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db));

EOF

cat <<EOF > c_tail
   RUN(notmuch_database_destroy(db));
}
EOF

test_begin_subtest "notmuch_database_{set,get}_config"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   RUN(notmuch_database_set_config (db, "testkey1", "testvalue1"));
   RUN(notmuch_database_set_config (db, "testkey2", "testvalue2"));
   RUN(notmuch_database_get_config (db, "testkey1", &val));
   printf("testkey1 = %s\n", val);
   RUN(notmuch_database_get_config (db, "testkey2", &val));
   printf("testkey2 = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
testkey1 = testvalue1
testkey2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "notmuch_database_get_config_list: empty list"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   notmuch_config_list_t *list;
   RUN(notmuch_database_get_config_list (db, "nonexistent", &list));
   printf("valid = %d\n", notmuch_config_list_valid (list));
   notmuch_config_list_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
valid = 0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "notmuch_database_get_config_list: all pairs"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   notmuch_config_list_t *list;
   RUN(notmuch_database_set_config (db, "zzzafter", "afterval"));
   RUN(notmuch_database_set_config (db, "aaabefore", "beforeval"));
   RUN(notmuch_database_get_config_list (db, "", &list));
   for (; notmuch_config_list_valid (list); notmuch_config_list_move_to_next (list)) {
      printf("%s %s\n", notmuch_config_list_key (list), notmuch_config_list_value(list));
   }
   notmuch_config_list_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
aaabefore beforeval
testkey1 testvalue1
testkey2 testvalue2
zzzafter afterval
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_database_get_config_list: one prefix"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   notmuch_config_list_t *list;
   RUN(notmuch_database_get_config_list (db, "testkey", &list));
   for (; notmuch_config_list_valid (list); notmuch_config_list_move_to_next (list)) {
      printf("%s %s\n", notmuch_config_list_key (list), notmuch_config_list_value(list));
   }
   notmuch_config_list_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
testkey1 testvalue1
testkey2 testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "dump config"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
    RUN(notmuch_database_set_config (db, "key with spaces", "value, with, spaces!"));
}
EOF
notmuch dump --include=config >OUTPUT
cat <<'EOF' >EXPECTED
#notmuch-dump batch-tag:2 config
#@ aaabefore beforeval
#@ key%20with%20spaces value,%20with,%20spaces%21
#@ testkey1 testvalue1
#@ testkey2 testvalue2
#@ zzzafter afterval
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "restore config"
notmuch dump --include=config >EXPECTED
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
    RUN(notmuch_database_set_config (db, "testkey1", "mutatedvalue"));
}
EOF
notmuch restore --include=config <EXPECTED
notmuch dump --include=config >OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_done
