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
   char *msg = NULL;

   for (int i = 1; i < argc; i++)
      if (strcmp (argv[i], "%NULL%") == 0) argv[i] = NULL;

   stat = notmuch_database_open_with_config (argv[1],
                                              NOTMUCH_DATABASE_MODE_READ_WRITE,
                                              argv[2],
                                              argv[3],
                                              &db,
                                              &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database\n%s\n%s\n", notmuch_status_to_string (stat), msg ? msg : "");
     exit (1);
   }
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
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

test_begin_subtest "NOTMUCH_CONFIG_HOOK_DIR: traditional"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
   const char *val = notmuch_config_get (db, NOTMUCH_CONFIG_HOOK_DIR);
   printf("database.hook_dir = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
database.hook_dir = MAIL_DIR/.notmuch/hooks
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "NOTMUCH_CONFIG_HOOK_DIR: xdg"
dir="${HOME}/.config/notmuch/default/hooks"
mkdir -p $dir
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
   const char *val = notmuch_config_get (db, NOTMUCH_CONFIG_HOOK_DIR);
   printf("database.hook_dir = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
database.hook_dir = CWD/home/.config/notmuch/default/hooks
== stderr ==
EOF
rmdir $dir
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_config_get_values"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
    notmuch_config_values_t *values;
    EXPECT0(notmuch_config_set (db, NOTMUCH_CONFIG_NEW_TAGS, "a;b;c"));
    for (values = notmuch_config_get_values (db, NOTMUCH_CONFIG_NEW_TAGS);
	 notmuch_config_values_valid (values);
	 notmuch_config_values_move_to_next (values))
    {
	  puts (notmuch_config_values_get (values));
    }
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
a
b
c
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

test_begin_subtest "notmuch_config_get_values (restart)"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
    notmuch_config_values_t *values;
    EXPECT0(notmuch_config_set (db, NOTMUCH_CONFIG_NEW_TAGS, "a;b;c"));
    for (values = notmuch_config_get_values (db, NOTMUCH_CONFIG_NEW_TAGS);
	 notmuch_config_values_valid (values);
	 notmuch_config_values_move_to_next (values))
    {
	  puts (notmuch_config_values_get (values));
    }
    for (notmuch_config_values_start (values);
	 notmuch_config_values_valid (values);
	 notmuch_config_values_move_to_next (values))
    {
	  puts (notmuch_config_values_get (values));
    }
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
a
b
c
a
b
c
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest "notmuch_config_get_values, trailing ;"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
    notmuch_config_values_t *values;
    EXPECT0(notmuch_config_set (db, NOTMUCH_CONFIG_NEW_TAGS, "a;b;c"));
    for (values = notmuch_config_get_values (db, NOTMUCH_CONFIG_NEW_TAGS);
	 notmuch_config_values_valid (values);
	 notmuch_config_values_move_to_next (values))
    {
	  puts (notmuch_config_values_get (values));
    }
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
a
b
c
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest "get config by key"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
   printf("before = %s\n", notmuch_config_get (db, NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS));
   EXPECT0(notmuch_database_set_config (db, "maildir.synchronize_flags", "false"));
   printf("after = %s\n", notmuch_config_get (db, NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS));
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
before = true
after = false
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest "set config by key"
notmuch config set test.key1 overridden
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG}
{
   printf("before = %s\n", notmuch_config_get (db, NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS));
   EXPECT0(notmuch_config_set (db, NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS, "false"));
   printf("after = %s\n", notmuch_config_get (db, NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS));
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
before = true
after = false
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

test_begin_subtest "load default values"
export MAILDIR=${MAIL_DIR}
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} '' %NULL%
{
    notmuch_config_key_t key;
    for (key = NOTMUCH_CONFIG_FIRST;
	 key < NOTMUCH_CONFIG_LAST;
	 key = (notmuch_config_key_t)(key + 1)) {
	const char *val = notmuch_config_get (db, key);
        printf("%s\n", val ? val : "NULL" );
    }
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
MAIL_DIR
MAIL_DIR
MAIL_DIR/.notmuch/hooks

inbox;unread
NULL
true
NULL
NULL
NULL
== stderr ==
EOF
unset MAILDIR
test_expect_equal_file EXPECTED OUTPUT

backup_database
test_begin_subtest "override config from \${NOTMUCH_CONFIG}"
notmuch config set test.key1 overridden
# second argument omitted to make argv[2] == NULL
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
notmuch config set test.key1
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = overridden
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest "override config from \${HOME}/.notmuch-config"
ovconfig=${HOME}/.notmuch-config
cp ${NOTMUCH_CONFIG} ${ovconfig}
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
notmuch --config=${ovconfig} config set test.key1 overridden-home
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} %NULL% %NULL%
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
rm -f ${ovconfig}
NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = overridden-home
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest "override config from \${XDG_CONFIG_HOME}/notmuch"
ovconfig=${HOME}/.config/notmuch/default/config
mkdir -p $(dirname ${ovconfig})
cp ${NOTMUCH_CONFIG} ${ovconfig}
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
notmuch --config=${ovconfig} config set test.key1 overridden-xdg
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} %NULL% %NULL%
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
rm -f ${ovconfig}
NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = overridden-xdg
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest "override config from \${XDG_CONFIG_HOME}/notmuch with profile"
ovconfig=${HOME}/.config/notmuch/work/config
mkdir -p $(dirname ${ovconfig})
cp ${NOTMUCH_CONFIG} ${ovconfig}
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
notmuch --config=${ovconfig} config set test.key1 overridden-xdg-profile
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} %NULL% work
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
rm -f ${ovconfig}
NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = overridden-xdg-profile
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest "override config from \${HOME}/.notmuch-config.work (via args)"
ovconfig=${HOME}/.notmuch-config.work
cp ${NOTMUCH_CONFIG} ${ovconfig}
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
notmuch --config=${ovconfig} config set test.key1 overridden-profile
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} %NULL% work
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
#rm -f ${ovconfig}
NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = overridden-profile
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

test_begin_subtest "no config, fail to open database"
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
cat c_head - c_tail <<'EOF' | test_C %NULL% '' %NULL%
{
   printf("NOT RUN");
}
EOF
NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
error opening database
Something went wrong trying to read or write a file
Error: Cannot open database at CWD/home/.local/share/notmuch/default: No such file or directory.

EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "open database from NOTMUCH_DATABASE"
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
export NOTMUCH_DATABASE=${MAIL_DIR}
cat c_head - c_tail <<'EOF' | test_C %NULL% '' %NULL%
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
unset NOTMUCH_DATABASE
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "NOTMUCH_DATABASE overrides config"
old_path=$(notmuch config get database.path)
notmuch config set database.path /nonexistent
export NOTMUCH_DATABASE=${MAIL_DIR}
cat c_head - c_tail <<'EOF' | test_C %NULL% '' %NULL%
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
unset NOTMUCH_DATABASE
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
== stderr ==
EOF
notmuch config set database.path "${old_path}"
test_expect_equal_file EXPECTED OUTPUT


test_done
