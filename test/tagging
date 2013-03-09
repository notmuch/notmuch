#!/usr/bin/env bash
test_description='"notmuch tag"'
. ./test-lib.sh

add_message '[subject]=One'
add_message '[subject]=Two'

test_begin_subtest "Adding tags"
notmuch tag +tag1 +tag2 +tag3 \*
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag1 tag2 tag3 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 tag2 tag3 unread)"

test_begin_subtest "Removing tags"
notmuch tag -tag1 -tag2 \*
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag3 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag3 unread)"

test_expect_code 1 "No tag operations" 'notmuch tag One'
test_expect_code 1 "No query" 'notmuch tag +tag2'

test_begin_subtest "Redundant tagging"
notmuch tag +tag1 -tag3 One
notmuch tag +tag1 -tag3 \*
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag1 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 unread)"

test_begin_subtest "Remove all"
notmuch tag --remove-all One
notmuch tag --remove-all +tag5 +tag6 +unread Two
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One ()
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (tag5 tag6 unread)"

test_begin_subtest "Remove all with a no-op"
notmuch tag +inbox +tag1 +unread One
notmuch tag --remove-all +foo +inbox +tag1 -foo +unread Two
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag1 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 unread)"

test_begin_subtest "Special characters in tags"
notmuch tag +':" ' \*
notmuch tag -':" ' Two
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (:\"  inbox tag1 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 unread)"

test_begin_subtest "Tagging order"
notmuch tag +tag4 -tag4 One
notmuch tag -tag4 +tag4 Two
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (:\"  inbox tag1 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 tag4 unread)"

test_begin_subtest "--batch"
notmuch tag --batch <<EOF
# %20 is a space in tag
-:"%20 -tag1 +tag5 +tag6 -- One
+tag1 -tag1 -tag4 +tag4 -- Two
-tag6 One
+tag5 Two
EOF
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag5 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag4 tag5 unread)"

# generate a common input file for the next several tests.
cat > batch.in  <<EOF
# %40 is an @ in tag
+%40 -tag5 +tag6 -- One
+tag1 -tag1 -tag4 +tag4 -- Two
-tag5 +tag6 Two
EOF

cat > batch.expected <<EOF
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (@ inbox tag6 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag4 tag6 unread)
EOF

test_begin_subtest "--input"
notmuch dump --format=batch-tag > backup.tags
notmuch tag --input=batch.in
notmuch search \* | notmuch_search_sanitize > OUTPUT
notmuch restore --format=batch-tag < backup.tags
test_expect_equal_file batch.expected OUTPUT

test_begin_subtest "--batch --input"
notmuch dump --format=batch-tag > backup.tags
notmuch tag --batch --input=batch.in
notmuch search \* | notmuch_search_sanitize > OUTPUT
notmuch restore --format=batch-tag < backup.tags
test_expect_equal_file batch.expected OUTPUT

test_begin_subtest "--batch, blank lines and comments"
notmuch dump | sort > EXPECTED
notmuch tag --batch <<EOF
# this line is a comment; the next has only white space
 	 

# the previous line is empty
EOF
notmuch dump | sort > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest '--batch: checking error messages'
notmuch dump --format=batch-tag > BACKUP
notmuch tag --batch <<EOF 2>OUTPUT
# the next line has a space
 
# this line has no tag operations, but this is permitted in batch format.
a
+0
+a +b
# trailing whitespace
+a +b 
+c +d --
# this is a harmless comment, do not yell about it.

# the previous line was blank; also no yelling please
+%zz -- id:whatever
# the next non-comment line should report an an empty tag error for
# batch tagging, but not for restore
+ +e -- id:foo
+- -- id:foo
EOF

cat <<EOF > EXPECTED
Warning: no query string [+0]
Warning: no query string [+a +b]
Warning: missing query string [+a +b ]
Warning: no query string after -- [+c +d --]
Warning: hex decoding of tag %zz failed [+%zz -- id:whatever]
Warning: empty tag forbidden [+ +e -- id:foo]
Warning: tag starting with '-' forbidden [+- -- id:foo]
EOF

notmuch restore --format=batch-tag < BACKUP
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest '--batch: tags with quotes'
notmuch dump --format=batch-tag > BACKUP

notmuch tag --batch <<EOF
+%22%27%22%27%22%22%27%27 -- One
-%22%27%22%27%22%22%27%27 -- One
+%22%27%22%22%22%27 -- One
+%22%27%22%27%22%22%27%27 -- Two
EOF

cat <<EOF > EXPECTED
+%22%27%22%22%22%27 +inbox +tag5 +unread -- id:msg-001@notmuch-test-suite
+%22%27%22%27%22%22%27%27 +inbox +tag4 +tag5 +unread -- id:msg-002@notmuch-test-suite
EOF

notmuch dump --format=batch-tag | sort > OUTPUT
notmuch restore --format=batch-tag < BACKUP
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest '--batch: tags with punctuation and space'
notmuch dump --format=batch-tag > BACKUP

notmuch tag --batch <<EOF
+%21@%23%24%25%5e%26%2a%29-_=+%5b%7b%5c%20%7c%3b%3a%27%20%22,.%3c%60%7e -- One
-%21@%23%24%25%5e%26%2a%29-_=+%5b%7b%5c%20%7c%3b%3a%27%20%22,.%3c%60%7e -- One
+%21@%23%24%25%5e%26%2a%29-_=+%5b%7b%5c%20%7c%3b%3a%27%20%22,.%3c%20%60%7e -- Two
-%21@%23%24%25%5e%26%2a%29-_=+%5b%7b%5c%20%7c%3b%3a%27%20%22,.%3c%20%60%7e -- Two
+%21@%23%20%24%25%5e%26%2a%29-_=+%5b%7b%5c%20%7c%3b%3a%27%20%22,.%3c%60%7e -- One
+%21@%23%20%24%25%5e%26%2a%29-_=+%5b%7b%5c%20%7c%3b%3a%27%20%22,.%3c%60%7e -- Two
EOF

cat <<EOF > EXPECTED
+%21@%23%20%24%25%5e%26%2a%29-_=+%5b%7b%5c%20%7c%3b%3a%27%20%22,.%3c%60%7e +inbox +tag4 +tag5 +unread -- id:msg-002@notmuch-test-suite
+%21@%23%20%24%25%5e%26%2a%29-_=+%5b%7b%5c%20%7c%3b%3a%27%20%22,.%3c%60%7e +inbox +tag5 +unread -- id:msg-001@notmuch-test-suite
EOF

notmuch dump --format=batch-tag | sort > OUTPUT
notmuch restore --format=batch-tag < BACKUP
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest '--batch: unicode tags'
notmuch dump --format=batch-tag > BACKUP

notmuch tag --batch <<EOF
+%2a@%7d%cf%b5%f4%85%80%adO3%da%a7 -- One
+=%e0%ac%95%c8%b3+%ef%aa%95%c8%a64w%c7%9d%c9%a2%cf%b3%d6%82%24B%c4%a9%c5%a1UX%ee%99%b0%27E7%ca%a4%d0%8b%5d -- One
+A%e1%a0%bc%de%8b%d5%b2V%d9%9b%f3%b5%a2%a3M%d8%a1u@%f0%a0%ac%948%7e%f0%ab%86%af%27 -- One
+R -- One
+%da%88=f%cc%b9I%ce%af%7b%c9%97%e3%b9%8bH%cb%92X%d2%8c6 -- One
+%dc%9crh%d2%86B%e5%97%a2%22t%ed%99%82d -- One
+L%df%85%ef%a1%a5m@%d3%96%c2%ab%d4%9f%ca%b8%f3%b3%a2%bf%c7%b1_u%d7%b4%c7%b1 -- One
+P%c4%98%2f -- One
+%7e%d1%8b%25%ec%a0%ae%d1%a0M%3b%e3%b6%b7%e9%a4%87%3c%db%9a%cc%a8%e1%96%9d -- One
+%c4%bf7%c7%ab9H%c4%99k%ea%91%bd%c3%8ck%e2%b3%8dk%c5%952V%e4%99%b2%d9%b3%e4%8b%bda%5b%24%c7%9b -- One
+%2a@%7d%cf%b5%f4%85%80%adO3%da%a7  +=%e0%ac%95%c8%b3+%ef%aa%95%c8%a64w%c7%9d%c9%a2%cf%b3%d6%82%24B%c4%a9%c5%a1UX%ee%99%b0%27E7%ca%a4%d0%8b%5d  +A%e1%a0%bc%de%8b%d5%b2V%d9%9b%f3%b5%a2%a3M%d8%a1u@%f0%a0%ac%948%7e%f0%ab%86%af%27  +R  +%da%88=f%cc%b9I%ce%af%7b%c9%97%e3%b9%8bH%cb%92X%d2%8c6  +%dc%9crh%d2%86B%e5%97%a2%22t%ed%99%82d  +L%df%85%ef%a1%a5m@%d3%96%c2%ab%d4%9f%ca%b8%f3%b3%a2%bf%c7%b1_u%d7%b4%c7%b1  +P%c4%98%2f  +%7e%d1%8b%25%ec%a0%ae%d1%a0M%3b%e3%b6%b7%e9%a4%87%3c%db%9a%cc%a8%e1%96%9d  +%c4%bf7%c7%ab9H%c4%99k%ea%91%bd%c3%8ck%e2%b3%8dk%c5%952V%e4%99%b2%d9%b3%e4%8b%bda%5b%24%c7%9b -- Two
EOF

cat <<EOF > EXPECTED
+%2a@%7d%cf%b5%f4%85%80%adO3%da%a7 +=%e0%ac%95%c8%b3+%ef%aa%95%c8%a64w%c7%9d%c9%a2%cf%b3%d6%82%24B%c4%a9%c5%a1UX%ee%99%b0%27E7%ca%a4%d0%8b%5d +A%e1%a0%bc%de%8b%d5%b2V%d9%9b%f3%b5%a2%a3M%d8%a1u@%f0%a0%ac%948%7e%f0%ab%86%af%27 +L%df%85%ef%a1%a5m@%d3%96%c2%ab%d4%9f%ca%b8%f3%b3%a2%bf%c7%b1_u%d7%b4%c7%b1 +P%c4%98%2f +R +inbox +tag4 +tag5 +unread +%7e%d1%8b%25%ec%a0%ae%d1%a0M%3b%e3%b6%b7%e9%a4%87%3c%db%9a%cc%a8%e1%96%9d +%c4%bf7%c7%ab9H%c4%99k%ea%91%bd%c3%8ck%e2%b3%8dk%c5%952V%e4%99%b2%d9%b3%e4%8b%bda%5b%24%c7%9b +%da%88=f%cc%b9I%ce%af%7b%c9%97%e3%b9%8bH%cb%92X%d2%8c6 +%dc%9crh%d2%86B%e5%97%a2%22t%ed%99%82d -- id:msg-002@notmuch-test-suite
+%2a@%7d%cf%b5%f4%85%80%adO3%da%a7 +=%e0%ac%95%c8%b3+%ef%aa%95%c8%a64w%c7%9d%c9%a2%cf%b3%d6%82%24B%c4%a9%c5%a1UX%ee%99%b0%27E7%ca%a4%d0%8b%5d +A%e1%a0%bc%de%8b%d5%b2V%d9%9b%f3%b5%a2%a3M%d8%a1u@%f0%a0%ac%948%7e%f0%ab%86%af%27 +L%df%85%ef%a1%a5m@%d3%96%c2%ab%d4%9f%ca%b8%f3%b3%a2%bf%c7%b1_u%d7%b4%c7%b1 +P%c4%98%2f +R +inbox +tag5 +unread +%7e%d1%8b%25%ec%a0%ae%d1%a0M%3b%e3%b6%b7%e9%a4%87%3c%db%9a%cc%a8%e1%96%9d +%c4%bf7%c7%ab9H%c4%99k%ea%91%bd%c3%8ck%e2%b3%8dk%c5%952V%e4%99%b2%d9%b3%e4%8b%bda%5b%24%c7%9b +%da%88=f%cc%b9I%ce%af%7b%c9%97%e3%b9%8bH%cb%92X%d2%8c6 +%dc%9crh%d2%86B%e5%97%a2%22t%ed%99%82d -- id:msg-001@notmuch-test-suite
EOF

notmuch dump --format=batch-tag | sort > OUTPUT
notmuch restore --format=batch-tag < BACKUP
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--batch: only space and % needs to be encoded."
notmuch dump --format=batch-tag > BACKUP

notmuch tag --batch <<EOF
+winner *
+foo::bar%25 -- (One and Two) or (One and tag:winner)
+found::it -- tag:foo::bar%
# ignore this line and the next

+space%20in%20tags -- Two
# add tag '(tags)', among other stunts.
+crazy{ +(tags) +&are +#possible\ -- tag:"space in tags"
+match*crazy -- tag:crazy{
+some_tag -- id:"this is ""nauty)"""
EOF

cat <<EOF > EXPECTED
+%23possible%5c +%26are +%28tags%29 +crazy%7b +inbox +match%2acrazy +space%20in%20tags +tag4 +tag5 +unread +winner -- id:msg-002@notmuch-test-suite
+foo%3a%3abar%25 +found%3a%3ait +inbox +tag5 +unread +winner -- id:msg-001@notmuch-test-suite
EOF

notmuch dump --format=batch-tag | sort > OUTPUT
notmuch restore --format=batch-tag < BACKUP
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest '--batch: unicode message-ids'

${TEST_DIRECTORY}/random-corpus --config-path=${NOTMUCH_CONFIG} \
     --num-messages=100

notmuch dump --format=batch-tag | sed 's/^.* -- /+common_tag -- /' | \
    sort > EXPECTED

notmuch dump --format=batch-tag | sed 's/^.* -- /  -- /' | \
    notmuch restore --format=batch-tag

notmuch tag --batch < EXPECTED

notmuch dump --format=batch-tag| \
    sort > OUTPUT

test_expect_equal_file EXPECTED OUTPUT

test_expect_code 1 "Empty tag names" 'notmuch tag + One'

test_expect_code 1 "Tag name beginning with -" 'notmuch tag +- One'

test_done
