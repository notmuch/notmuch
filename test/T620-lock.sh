#!/usr/bin/env bash
test_description="locking"
. ./test-lib.sh || exit 1

if [ "${NOTMUCH_HAVE_XAPIAN_DB_RETRY_LOCK}" = "0" ]; then
    test_subtest_missing_external_prereq_["lock retry support"]=t
fi

add_email_corpus

test_begin_subtest "blocking open"
test_C ${MAIL_DIR} <<'EOF'
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <notmuch-test.h>

void
taggit (notmuch_database_t *db, const char *tag)
{
    notmuch_message_t *message;

    EXPECT0 (notmuch_database_find_message (db, "4EFC743A.3060609@april.org", &message));
    if (message == NULL) {
	fprintf (stderr, "unable to find message");
	exit (1);
    }

    EXPECT0 (notmuch_message_add_tag (message, tag));
    notmuch_message_destroy (message);
}

int
main (int argc, char **argv)
{
    pid_t child;
    const char *path = argv[1];

    child = fork ();
    if (child == -1) {
	fprintf (stderr, "fork failed\n");
	exit (1);
    }

    if (child == 0) {
	notmuch_database_t *db2;

	sleep (1);
	EXPECT0 (notmuch_database_open (path, NOTMUCH_DATABASE_MODE_READ_WRITE, &db2));
	taggit (db2, "child");
	EXPECT0 (notmuch_database_close (db2));
    } else {
	notmuch_database_t *db;

	EXPECT0 (notmuch_database_open (path, NOTMUCH_DATABASE_MODE_READ_WRITE, &db));
	taggit (db, "parent");
	sleep (2);
	EXPECT0 (notmuch_database_close (db));
	wait (NULL);
    }
}

EOF
notmuch search --output=tags id:4EFC743A.3060609@april.org >> OUTPUT
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
child
inbox
parent
unread
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
