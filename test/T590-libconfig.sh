#!/usr/bin/env bash
test_description="library config API"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

cat <<EOF > c_head
#include <string.h>
#include <stdlib.h>
#include <notmuch-test.h>

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   char *val;
   notmuch_status_t stat;

   EXPECT0(notmuch_database_open_with_config (argv[1],
                                              NOTMUCH_DATABASE_MODE_READ_WRITE,
                                              argv[2],
                                              NULL,
                                              &db,
                                              NULL));

EOF

cat <<EOF > c_tail
   EXPECT0(notmuch_database_destroy(db));
}
EOF

test_begin_subtest "notmuch_database_{set,get}_config"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
   EXPECT0(notmuch_database_set_config (db, "test.key1", "testvalue1"));
   EXPECT0(notmuch_database_set_config (db, "test.key2", "testvalue2"));
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "notmuch_database_get_config_list: empty list"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
   notmuch_config_list_t *list;
   EXPECT0(notmuch_database_get_config_list (db, "nonexistent", &list));
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

test_begin_subtest "notmuch_database_get_config_list: closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   notmuch_config_list_t *list;
   EXPECT0(notmuch_database_close (db));
   stat = notmuch_database_get_config_list (db, "nonexistent", &list);
   printf("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_database_get_config_list: all pairs"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
   notmuch_config_list_t *list;
   EXPECT0(notmuch_database_set_config (db, "zzzafter", "afterval"));
   EXPECT0(notmuch_database_set_config (db, "aaabefore", "beforeval"));
   EXPECT0(notmuch_database_get_config_list (db, "", &list));
   for (; notmuch_config_list_valid (list); notmuch_config_list_move_to_next (list)) {
      printf("%s %s\n", notmuch_config_list_key (list), notmuch_config_list_value(list));
   }
   notmuch_config_list_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
aaabefore beforeval
test.key1 testvalue1
test.key2 testvalue2
zzzafter afterval
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_database_get_config_list: all pairs (closed db)"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   notmuch_config_list_t *list;
   EXPECT0(notmuch_database_get_config_list (db, "", &list));
   EXPECT0(notmuch_database_close (db));
   for (; notmuch_config_list_valid (list); notmuch_config_list_move_to_next (list)) {
      printf("%s %d\n", notmuch_config_list_key (list), NULL == notmuch_config_list_value(list));
   }
   notmuch_config_list_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
aaabefore 1
test.key1 1
test.key2 1
zzzafter 1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_database_get_config_list: one prefix"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
   notmuch_config_list_t *list;
   EXPECT0(notmuch_database_get_config_list (db, "test.key", &list));
   for (; notmuch_config_list_valid (list); notmuch_config_list_move_to_next (list)) {
      printf("%s %s\n", notmuch_config_list_key (list), notmuch_config_list_value(list));
   }
   notmuch_config_list_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 testvalue1
test.key2 testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "dump config"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
    EXPECT0(notmuch_database_set_config (db, "key with spaces", "value, with, spaces!"));
}
EOF
notmuch dump --include=config >OUTPUT
cat <<'EOF' >EXPECTED
#notmuch-dump batch-tag:3 config
#@ aaabefore beforeval
#@ key%20with%20spaces value,%20with,%20spaces%21
#@ test.key1 testvalue1
#@ test.key2 testvalue2
#@ zzzafter afterval
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "restore config"
notmuch dump --include=config >EXPECTED
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
    EXPECT0(notmuch_database_set_config (db, "test.key1", "mutatedvalue"));
}
EOF
notmuch restore --include=config <EXPECTED
notmuch dump --include=config >OUTPUT
test_expect_equal_file EXPECTED OUTPUT

backup_database
test_begin_subtest "override config from file"
notmuch config set test.key1 overridden
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = overridden
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

test_done
