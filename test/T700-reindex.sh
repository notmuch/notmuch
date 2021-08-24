#!/usr/bin/env bash
test_description='reindexing messages'
. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus


if [ $NOTMUCH_HAVE_SFSEXP -eq 1 ]; then

    count=$(notmuch count --lastmod '*' | cut -f 3)
    for query in '()' '(not)' '(and)' '(or ())' '(or (not))' '(or (and))' \
		'(or (and) (or) (not (and)))'; do
	test_begin_subtest "reindex all messages: $query"
	notmuch reindex --query=sexp "$query"
	output=$(notmuch count --lastmod '*' | cut -f 3)
	count=$((count + 1))
	test_expect_equal "$output" "$count"
    done

fi

notmuch tag +usertag1 '*'

notmuch search '*' 2>1 | notmuch_search_sanitize > initial-threads
notmuch search --output=messages '*' 2>/dev/null > initial-message-ids
notmuch dump > initial-dump

test_begin_subtest 'reindex preserves threads'
notmuch reindex '*'
notmuch search '*' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file initial-threads OUTPUT

test_begin_subtest 'reindex after removing duplicate file preserves threads'
# remove one copy
sed 's,3/3(4),3/3,' < initial-threads > EXPECTED
mv $MAIL_DIR/bar/18:2, duplicate-msg-1.eml
notmuch reindex '*'
notmuch search '*' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'reindex preserves message-ids'
notmuch reindex '*'
notmuch search --output=messages '*' > OUTPUT
test_expect_equal_file initial-message-ids OUTPUT

test_begin_subtest 'reindex preserves tags'
notmuch reindex '*'
notmuch dump > OUTPUT
test_expect_equal_file initial-dump OUTPUT

test_begin_subtest 'reindex preserves tags with special prefixes'
notmuch tag +attachment2 +encrypted2 +signed2 '*'
notmuch dump > EXPECTED
notmuch reindex '*'
notmuch dump > OUTPUT
notmuch tag -attachment2 -encrypted2 -signed2 '*'
test_expect_equal_file EXPECTED OUTPUT

backup_database
test_begin_subtest 'reindex moves a message between threads'
notmuch search --output=threads id:87iqd9rn3l.fsf@vertex.dottedmag > EXPECTED
# re-parent
sed -i 's/1258471718-6781-1-git-send-email-dottedmag@dottedmag.net/87iqd9rn3l.fsf@vertex.dottedmag/' $MAIL_DIR/02:2,*
notmuch reindex id:1258471718-6781-2-git-send-email-dottedmag@dottedmag.net
notmuch search --output=threads id:1258471718-6781-2-git-send-email-dottedmag@dottedmag.net > OUTPUT
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest 'reindex detects removal of all files'
notmuch search --output=messages not id:20091117232137.GA7669@griffis1.net> EXPECTED
# remove both copies
mv $MAIL_DIR/cur/51:2,* duplicate-message-2.eml
notmuch reindex id:20091117232137.GA7669@griffis1.net
notmuch search --output=messages '*' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest 'reindex detects removal of all files'
notmuch search --output=messages not id:20091117232137.GA7669@griffis1.net> EXPECTED
# remove both copies
mv $MAIL_DIR/cur/51:2,* duplicate-message-2.eml
notmuch reindex id:20091117232137.GA7669@griffis1.net
notmuch search --output=messages '*' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT
restore_database

test_begin_subtest "reindex preserves properties"
cat <<EOF > prop-dump
#= 1258471718-6781-1-git-send-email-dottedmag@dottedmag.net userprop=userval
#= 1258471718-6781-2-git-send-email-dottedmag@dottedmag.net userprop=userval
#= 1258491078-29658-1-git-send-email-dottedmag@dottedmag.net userprop=userval1
#= 20091117190054.GU3165@dottiness.seas.harvard.edu userprop=userval
#= 20091117203301.GV3165@dottiness.seas.harvard.edu userprop=userval3
#= 87fx8can9z.fsf@vertex.dottedmag userprop=userval2
#= 87iqd9rn3l.fsf@vertex.dottedmag userprop=userval
#= 87lji4lx9v.fsf@yoom.home.cworth.org userprop=userval3
#= 87lji5cbwo.fsf@yoom.home.cworth.org userprop=userval
#= cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com userprop=userval
EOF
notmuch restore < prop-dump
notmuch reindex '*'
notmuch dump | grep '^#=' | sort > OUTPUT
test_expect_equal_file prop-dump OUTPUT

add_email_corpus lkml

test_begin_subtest "reindex of lkml corpus preserves threads"
notmuch search '*' | notmuch_search_sanitize > EXPECTED
notmuch reindex '*'
notmuch search '*' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "reindex after removing corpus"
tar cf backup.tar mail/cur
find mail/cur -type f -delete
test_expect_success "notmuch reindex '*'"
tar xf backup.tar

test_done
