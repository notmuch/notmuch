#!/usr/bin/env bash
test_description="message property API"

. ./test-lib.sh || exit 1

add_email_corpus

cat <<EOF > c_head
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <talloc.h>
#include <notmuch-test.h>

void print_properties (notmuch_message_t *message, const char *prefix, notmuch_bool_t exact) {
    notmuch_message_properties_t *list;
    for (list = notmuch_message_get_properties (message, prefix, exact);
         notmuch_message_properties_valid (list); notmuch_message_properties_move_to_next (list)) {
       printf("%s\n", notmuch_message_properties_value(list));
    }
    notmuch_message_properties_destroy (list);
}

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_message_t *message = NULL;
   const char *val;
   notmuch_status_t stat;

   EXPECT0(notmuch_database_open (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db));
   EXPECT0(notmuch_database_find_message(db, "4EFC743A.3060609@april.org", &message));
   if (message == NULL) {
	fprintf (stderr, "unable to find message");
	exit (1);
   }
EOF

cat <<EOF > c_tail
   EXPECT0(notmuch_database_destroy(db));
}
EOF

test_begin_subtest "notmuch_message_{add,get,remove}_property"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   EXPECT0(notmuch_message_add_property (message, "testkey1", "testvalue1"));
   EXPECT0(notmuch_message_get_property (message, "testkey1", &val));
   printf("testkey1[1] = %s\n", val);
   EXPECT0(notmuch_message_add_property (message, "testkey2", "this value has spaces and = sign"));
   EXPECT0(notmuch_message_get_property (message, "testkey1", &val));
   printf("testkey1[2] = %s\n", val);
   EXPECT0(notmuch_message_get_property (message, "testkey1", &val));

   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val);

   /* Add second value for key */
   EXPECT0(notmuch_message_add_property (message, "testkey2", "zztestvalue3"));
   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val);

   /* remove first value for key */
   EXPECT0(notmuch_message_remove_property (message, "testkey2", "this value has spaces and = sign"));
   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val);

   /* remove non-existant value for key */
   EXPECT0(notmuch_message_remove_property (message, "testkey2", "this value has spaces and = sign"));
   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val);

   /* remove only value for key */
   EXPECT0(notmuch_message_remove_property (message, "testkey2", "zztestvalue3"));
   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val == NULL ? "NULL" : val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
testkey1[1] = testvalue1
testkey1[2] = testvalue1
testkey2 = this value has spaces and = sign
testkey2 = this value has spaces and = sign
testkey2 = zztestvalue3
testkey2 = zztestvalue3
testkey2 = NULL
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_message_remove_all_properties"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
EXPECT0(notmuch_message_remove_all_properties (message, NULL));
print_properties (message, "", FALSE);
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_message_get_properties: empty list"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   notmuch_message_properties_t *list;
   list = notmuch_message_get_properties (message, "nonexistent", TRUE);
   printf("valid = %d\n", notmuch_message_properties_valid (list));
   notmuch_message_properties_destroy (list);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
valid = 0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_message_properties: one value"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
print_properties (message, "testkey1", TRUE);
EOF
cat <<'EOF' >EXPECTED
== stdout ==
testvalue1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_message_properties: multiple values"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
EXPECT0(notmuch_message_add_property (message, "testkey1", "bob"));
EXPECT0(notmuch_message_add_property (message, "testkey1", "testvalue2"));
EXPECT0(notmuch_message_add_property (message, "testkey1", "alice"));
print_properties (message, "testkey1", TRUE);
EOF
cat <<'EOF' >EXPECTED
== stdout ==
alice
bob
testvalue1
testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_message_properties: prefix"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
EXPECT0(notmuch_message_add_property (message, "testkey3", "bob3"));
EXPECT0(notmuch_message_add_property (message, "testkey3", "testvalue3"));
EXPECT0(notmuch_message_add_property (message, "testkey3", "alice3"));
print_properties (message, "testkey", FALSE);
EOF
cat <<'EOF' >EXPECTED
== stdout ==
alice
bob
testvalue1
testvalue2
alice3
bob3
testvalue3
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_message_properties: modify during iteration"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
    const char *keys[1000] = {NULL};
    const char *vals[1000] = {NULL};
    notmuch_message_properties_t *properties;
    int i;

    for (properties = notmuch_message_get_properties (message, "", FALSE), i=0;
	 notmuch_message_properties_valid (properties);
	 notmuch_message_properties_move_to_next (properties), i++)
    {
	const char *key, *value;

	keys[i]=talloc_strdup(message,
		    notmuch_message_properties_key (properties));
        vals[i]=talloc_strdup(message,
		    notmuch_message_properties_value (properties));

	EXPECT0(notmuch_message_remove_property (message, keys[i], vals[i]));
    }

    print_properties (message, "", FALSE);

    for (i = 0; keys[i] && vals[i]; i++) {
        EXPECT0(notmuch_message_add_property (message, keys[i], vals[i]));
    }
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "dump message properties"
cat <<EOF > PROPERTIES
#= 4EFC743A.3060609@april.org fancy%20key%20with%20%c3%a1cc%c3%a8nts=import%20value%20with%20= testkey1=alice testkey1=bob testkey1=testvalue1 testkey1=testvalue2 testkey3=alice3 testkey3=bob3 testkey3=testvalue3
EOF
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
EXPECT0(notmuch_message_add_property (message, "fancy key with áccènts", "import value with ="));
EOF
notmuch dump | grep '^#=' > OUTPUT
test_expect_equal_file PROPERTIES OUTPUT

test_begin_subtest "dump _only_ message properties"
cat <<EOF > EXPECTED
#notmuch-dump batch-tag:3 properties
#= 4EFC743A.3060609@april.org fancy%20key%20with%20%c3%a1cc%c3%a8nts=import%20value%20with%20= testkey1=alice testkey1=bob testkey1=testvalue1 testkey1=testvalue2 testkey3=alice3 testkey3=bob3 testkey3=testvalue3
EOF
notmuch dump --include=properties > OUTPUT
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "restore missing message property (single line)"
notmuch dump | grep '^#=' > BEFORE1
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
EXPECT0(notmuch_message_remove_property (message, "testkey1", "bob"));
EOF
notmuch restore < BEFORE1
notmuch dump | grep '^#=' > OUTPUT
test_expect_equal_file PROPERTIES OUTPUT


test_begin_subtest "restore missing message property (full dump)"
notmuch dump > BEFORE2
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
EXPECT0(notmuch_message_remove_property (message, "testkey1", "bob"));
EOF
notmuch restore < BEFORE2
notmuch dump | grep '^#=' > OUTPUT
test_expect_equal_file PROPERTIES OUTPUT

test_begin_subtest "restore clear extra message property"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
EXPECT0(notmuch_message_add_property (message, "testkey1", "charles"));
EOF
notmuch restore < BEFORE2
notmuch dump | grep '^#=' > OUTPUT
test_expect_equal_file PROPERTIES OUTPUT

test_begin_subtest "test 'property:' queries: empty"
notmuch search property:testkey1=charles > OUTPUT
test_expect_equal_file /dev/null OUTPUT

test_begin_subtest "test 'property:' queries: single message"
notmuch search --output=messages property:testkey1=alice > OUTPUT
cat <<EOF >EXPECTED
id:4EFC743A.3060609@april.org
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
