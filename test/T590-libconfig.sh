#!/usr/bin/env bash
test_description="library config API"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

_libconfig_sanitize() {
    ${NOTMUCH_PYTHON} /dev/fd/3 3<<'EOF'
import os, sys, pwd, socket

pw = pwd.getpwuid(os.getuid())
user = pw.pw_name
name = pw.pw_gecos.partition(",")[0]

for l in sys.stdin:
    if l[:4] == "08: ":
        l = l.replace(user, "USERNAME", 1)
    elif l[:4] == "10: ":
        l = l.replace("'" + name, "'USER_FULL_NAME", 1)
    sys.stdout.write(l)
EOF
}

cat <<EOF > c_head
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

test_begin_subtest "notmuch_config_get_values (ignore leading/trailing whitespace)"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
    notmuch_config_values_t *values;
    EXPECT0(notmuch_config_set (db, NOTMUCH_CONFIG_NEW_TAGS, " a ; b c ; d "));
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
b c
d
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

test_begin_subtest "notmuch_config_get_values_string"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
    notmuch_config_values_t *values;
    EXPECT0(notmuch_database_set_config (db, "test.list", "x;y;z"));
    for (values = notmuch_config_get_values_string (db, "test.list");
	 notmuch_config_values_valid (values);
	 notmuch_config_values_move_to_next (values))
    {
	  puts (notmuch_config_values_get (values));
    }
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
x
y
z
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
	printf("%02d: '%s'\n", key, val ? val : "NULL" );
    }
}
EOF

_libconfig_sanitize < OUTPUT > OUTPUT.clean

cat <<'EOF' >EXPECTED
== stdout ==
00: 'MAIL_DIR'
01: 'MAIL_DIR'
02: 'MAIL_DIR/.notmuch/hooks'
03: 'MAIL_DIR/.notmuch/backups'
04: ''
05: 'unread;inbox'
06: ''
07: 'true'
08: 'USERNAME@localhost'
09: 'NULL'
10: 'USER_FULL_NAME'
11: '8000'
12: 'NULL'
== stderr ==
EOF
unset MAILDIR
test_expect_equal_file EXPECTED OUTPUT.clean

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
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
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
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
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
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
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
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
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
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
error opening database
No database found
Error: could not locate database.

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
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
unset NOTMUCH_DATABASE
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "NOTMUCH_DATABASE overrides config"
cp notmuch-config notmuch-config.bak
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
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
unset NOTMUCH_DATABASE
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
== stderr ==
EOF
cp notmuch-config.bak notmuch-config
test_expect_equal_file EXPECTED OUTPUT

cat <<EOF > c_head2
#include <notmuch-test.h>

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   char *val;
   notmuch_status_t stat;
   char *msg = NULL;

   for (int i = 1; i < argc; i++)
      if (strcmp (argv[i], "%NULL%") == 0) argv[i] = NULL;

   stat = notmuch_database_load_config (argv[1],
                                        argv[2],
                                        argv[3],
                                        &db,
                                        &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS  && stat != NOTMUCH_STATUS_NO_CONFIG) {
     fprintf (stderr, "error opening database\n%d: %s\n%s\n", stat,
	      notmuch_status_to_string (stat), msg ? msg : "");
     exit (1);
   }
EOF


test_begin_subtest "notmuch_database_get_config (ndlc)"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR} %NULL% %NULL%
{
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


test_begin_subtest "notmuch_database_get_config_list: all pairs (ndlc)"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
   notmuch_config_list_t *list;
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
key with spaces value, with, spaces!
test.key1 testvalue1
test.key2 testvalue2
zzzafter afterval
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_database_get_config_list: one prefix (ndlc)"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
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

test_begin_subtest "list by keys (ndlc)"
notmuch config set search.exclude_tags "foo;bar;fub"
notmuch config set new.ignore "sekrit_junk"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR} %NULL% %NULL%
{
    notmuch_config_key_t key;
    for (key = NOTMUCH_CONFIG_FIRST;
	 key < NOTMUCH_CONFIG_LAST;
	 key = (notmuch_config_key_t)(key + 1)) {
	const char *val = notmuch_config_get (db, key);
	printf("%02d: '%s'\n", key, val ? val : "NULL" );
    }
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
00: 'MAIL_DIR'
01: 'MAIL_DIR'
02: 'MAIL_DIR/.notmuch/hooks'
03: 'MAIL_DIR/.notmuch/backups'
04: 'foo;bar;fub'
05: 'unread;inbox'
06: 'sekrit_junk'
07: 'true'
08: 'test_suite@notmuchmail.org'
09: 'test_suite_other@notmuchmail.org;test_suite@otherdomain.org'
10: 'Notmuch Test Suite'
11: '8000'
12: 'NULL'
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "load default values (ndlc, nonexistent config)"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR} /nonexistent %NULL%
{
    notmuch_config_key_t key;
    for (key = NOTMUCH_CONFIG_FIRST;
	 key < NOTMUCH_CONFIG_LAST;
	 key = (notmuch_config_key_t)(key + 1)) {
	const char *val = notmuch_config_get (db, key);
	printf("%02d: '%s'\n", key, val ? val : "NULL" );
    }
}
EOF

_libconfig_sanitize < OUTPUT > OUTPUT.clean

cat <<'EOF' >EXPECTED
== stdout ==
00: 'MAIL_DIR'
01: 'MAIL_DIR'
02: 'MAIL_DIR/.notmuch/hooks'
03: 'MAIL_DIR/.notmuch/backups'
04: ''
05: 'unread;inbox'
06: ''
07: 'true'
08: 'USERNAME@localhost'
09: 'NULL'
10: 'USER_FULL_NAME'
11: '8000'
12: 'NULL'
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT.clean

backup_database
test_begin_subtest "override config from \${HOME}/.notmuch-config (ndlc)"
ovconfig=${HOME}/.notmuch-config
cp ${NOTMUCH_CONFIG} ${ovconfig}
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
notmuch --config=${ovconfig} config set test.key1 overridden-home
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR} %NULL% %NULL%
{
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
rm -f ${ovconfig}
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = overridden-home
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

test_begin_subtest "notmuch_config_get_pairs: prefix (ndlc)"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
   notmuch_config_pairs_t *list;
   for (list =  notmuch_config_get_pairs (db, "user.");
        notmuch_config_pairs_valid (list);
        notmuch_config_pairs_move_to_next (list)) {
     printf("%s %s\n", notmuch_config_pairs_key (list), notmuch_config_pairs_value(list));
   }
   notmuch_config_pairs_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
user.name Notmuch Test Suite
user.other_email test_suite_other@notmuchmail.org;test_suite@otherdomain.org
user.primary_email test_suite@notmuchmail.org
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_config_get_pairs: all pairs (ndlc)"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} %NULL%
{
   notmuch_config_pairs_t *list;
   for (list =  notmuch_config_get_pairs (db, "");
        notmuch_config_pairs_valid (list);
        notmuch_config_pairs_move_to_next (list)) {
     printf("%s %s\n", notmuch_config_pairs_key (list), notmuch_config_pairs_value(list));
   }
   notmuch_config_pairs_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
aaabefore beforeval
database.autocommit 8000
database.backup_dir MAIL_DIR/.notmuch/backups
database.hook_dir MAIL_DIR/.notmuch/hooks
database.mail_root MAIL_DIR
database.path MAIL_DIR
key with spaces value, with, spaces!
maildir.synchronize_flags true
new.ignore sekrit_junk
new.tags unread;inbox
search.exclude_tags foo;bar;fub
show.extra_headers (null)
test.key1 testvalue1
test.key2 testvalue2
user.name Notmuch Test Suite
user.other_email test_suite_other@notmuchmail.org;test_suite@otherdomain.org
user.primary_email test_suite@notmuchmail.org
zzzafter afterval
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

cat <<EOF > c_head3
#include <notmuch-test.h>
int main (int argc, char **argv) {
  notmuch_status_t stat;
  notmuch_database_t *db = NULL;
EOF

cat <<EOF > c_tail3
  printf("db == NULL: %d\n", db == NULL);
}
EOF

test_begin_subtest "open: database set to null on missing config"
cat c_head3 - c_tail3 <<'EOF' | test_C ${MAIL_DIR}
  notmuch_status_t st = notmuch_database_open_with_config(argv[1],
							  NOTMUCH_DATABASE_MODE_READ_ONLY,
							  "/nonexistent", NULL, &db, NULL);
EOF
cat <<EOF> EXPECTED
== stdout ==
db == NULL: 1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "open: database set to null on missing config (env)"
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
export NOTMUCH_CONFIG="/nonexistent"
cat c_head3 - c_tail3 <<'EOF' | test_C ${MAIL_DIR}
  notmuch_status_t st = notmuch_database_open_with_config(argv[1],
							  NOTMUCH_DATABASE_MODE_READ_ONLY,
							  NULL, NULL, &db, NULL);
EOF
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<EOF> EXPECTED
== stdout ==
db == NULL: 1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "create: database set to null on missing config"
cat c_head3 - c_tail3 <<'EOF' | test_C ${MAIL_DIR} "/nonexistent"
  notmuch_status_t st = notmuch_database_create_with_config(argv[1],argv[2], NULL, &db, NULL);
EOF
cat <<EOF> EXPECTED
== stdout ==
db == NULL: 1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "create: database set to null on missing config (env)"
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
export NOTMUCH_CONFIG="/nonexistent"
cat c_head3 - c_tail3 <<'EOF' | test_C ${MAIL_DIR}
  notmuch_status_t st = notmuch_database_create_with_config(argv[1],
							  NULL, NULL, &db, NULL);
EOF
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<EOF> EXPECTED
== stdout ==
db == NULL: 1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "load_config: database set non-null on missing config"
cat c_head3 - c_tail3 <<'EOF' | test_C ${MAIL_DIR} "/nonexistent"
  notmuch_status_t st = notmuch_database_load_config(argv[1],argv[2], NULL, &db, NULL);
EOF
cat <<EOF> EXPECTED
== stdout ==
db == NULL: 0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "load_config: database non-null on missing config (env)"
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
export NOTMUCH_CONFIG="/nonexistent"
cat c_head3 - c_tail3 <<'EOF' | test_C ${MAIL_DIR}
  notmuch_status_t st = notmuch_database_load_config(argv[1], NULL, NULL, &db, NULL);
EOF
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
cat <<EOF> EXPECTED
== stdout ==
db == NULL: 0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "load_config: database set to NULL on fatal error"
cat c_head3 - c_tail3 <<'EOF' | test_C
  notmuch_status_t st = notmuch_database_load_config("relative", NULL, NULL, &db, NULL);
EOF
cat <<EOF> EXPECTED
== stdout ==
db == NULL: 1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "open: database parameter overrides implicit config"
cp $NOTMUCH_CONFIG ${NOTMUCH_CONFIG}.bak
notmuch config set database.path ${MAIL_DIR}/nonexistent
cat c_head3 - c_tail3 <<'EOF' | test_C ${MAIL_DIR}
  const char *path = NULL;
  notmuch_status_t st = notmuch_database_open_with_config(argv[1],
							  NOTMUCH_DATABASE_MODE_READ_ONLY,
							  NULL, NULL, &db, NULL);
  printf ("status: %d\n", st);
  path = notmuch_database_get_path (db);
  printf ("path: %s\n", path ? path : "(null)");
EOF
cp ${NOTMUCH_CONFIG}.bak ${NOTMUCH_CONFIG}
cat <<EOF> EXPECTED
== stdout ==
status: 0
path: MAIL_DIR
db == NULL: 0
== stderr ==
EOF
notmuch_dir_sanitize < OUTPUT > OUTPUT.clean
test_expect_equal_file EXPECTED OUTPUT.clean

cat <<EOF > c_body
  notmuch_status_t st = notmuch_database_open_with_config(NULL,
							  NOTMUCH_DATABASE_MODE_READ_ONLY,
							  "", NULL, &db, NULL);
  printf ("status == SUCCESS: %d\n", st == NOTMUCH_STATUS_SUCCESS);
  if (db) {
    const char *mail_root = NULL;
    mail_root = notmuch_config_get (db, NOTMUCH_CONFIG_MAIL_ROOT);
    printf ("mail_root: %s\n", mail_root ? mail_root : "(null)");
  }
EOF

cat <<EOF> EXPECTED.common
== stdout ==
status == SUCCESS: 0
db == NULL: 1
== stderr ==
EOF

test_begin_subtest "open/error: config=empty with no mail root in db "
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
cat c_head3 c_body c_tail3 | test_C
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
notmuch_dir_sanitize < OUTPUT > OUTPUT.clean
test_expect_equal_file EXPECTED.common OUTPUT.clean

test_begin_subtest "open/error: config=empty with no mail root in db (xdg)"
old_NOTMUCH_CONFIG=${NOTMUCH_CONFIG}
unset NOTMUCH_CONFIG
backup_database
mkdir -p home/.local/share/notmuch
mv mail/.notmuch home/.local/share/notmuch/default
cat c_head3 c_body c_tail3 | test_C
restore_database
export NOTMUCH_CONFIG=${old_NOTMUCH_CONFIG}
notmuch_dir_sanitize < OUTPUT > OUTPUT.clean
test_expect_equal_file EXPECTED.common OUTPUT.clean

test_done
