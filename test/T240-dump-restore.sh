#!/usr/bin/env bash
test_description="\"notmuch dump\" and \"notmuch restore\""
. ./test-lib.sh || exit 1

NOTMUCH_NEW > /dev/null
test_begin_subtest "dump header"
cat <<EOF > EXPECTED
#notmuch-dump batch-tag:3 config,properties,tags
EOF
notmuch dump > OUTPUT
test_expect_equal_file EXPECTED OUTPUT
add_email_corpus

test_begin_subtest "Dumping all tags"
test_expect_success 'generate_message && notmuch new && notmuch dump > dump.expected'

# The use of from:cworth is rather arbitrary: it matches some of the
# email corpus' messages, but not all of them.

test_begin_subtest "Dumping all tags II"
test_expect_success \
  'notmuch tag +ABC +DEF -- from:cworth &&
  notmuch dump > dump-ABC_DEF.expected &&
  ! cmp dump.expected dump-ABC_DEF.expected'

test_begin_subtest "Clearing all tags"
test_expect_success \
  'sed -e "s/(\([^(]*\))$/()/" < dump.expected > clear.expected &&
  notmuch restore --input=clear.expected &&
  notmuch dump > clear.actual &&
  test_cmp clear.expected clear.actual'

test_begin_subtest "Clearing all tags"
test_expect_success \
  'notmuch tag +ABC +DEF -- from:cworth &&
  notmuch restore --accumulate < dump.expected &&
  notmuch dump > dump.actual &&
  test_cmp dump-ABC_DEF.expected dump.actual'

test_begin_subtest "Restoring original tags"
test_expect_success \
  'notmuch restore --input=dump.expected &&
  notmuch dump > dump.actual &&
  test_cmp dump.expected dump.actual'

test_begin_subtest "Restore with nothing to do"
test_expect_success \
  'notmuch restore < dump.expected &&
  notmuch dump > dump.actual &&
  test_cmp dump.expected dump.actual'

test_begin_subtest "Accumulate with existing tags"
test_expect_success \
  'notmuch restore --accumulate --input=dump.expected &&
  notmuch dump > dump.actual &&
  test_cmp dump.expected dump.actual'

test_begin_subtest "Accumulate with no tags"
test_expect_success \
  'notmuch restore --accumulate < clear.expected &&
  notmuch dump > dump.actual &&
  test_cmp dump.expected dump.actual'

test_begin_subtest "Accumulate with new tags"
test_expect_success \
  'notmuch restore --input=dump.expected &&
  notmuch restore --accumulate --input=dump-ABC_DEF.expected &&
  notmuch dump >  OUTPUT.$test_count &&
  notmuch restore --input=dump.expected &&
  test_cmp dump-ABC_DEF.expected OUTPUT.$test_count'

# notmuch restore currently only considers the first argument.
test_begin_subtest "Invalid restore invocation"
test_expect_success \
  'test_must_fail notmuch restore --input=dump.expected another_one'

test_begin_subtest "dump --output=outfile"
notmuch dump --output=dump-outfile.actual
test_expect_equal_file dump.expected dump-outfile.actual

test_begin_subtest "dump --output=outfile --"
notmuch dump --output=dump-1-arg-dash.actual --
test_expect_equal_file dump.expected dump-1-arg-dash.actual

# gzipped output

test_begin_subtest "dump --gzip"
notmuch dump --gzip > dump-gzip.gz
gunzip dump-gzip.gz
test_expect_equal_file dump.expected dump-gzip

test_begin_subtest "dump --gzip --output=outfile"
notmuch dump --gzip --output=dump-gzip-outfile.gz
gunzip dump-gzip-outfile.gz
test_expect_equal_file dump.expected dump-gzip-outfile

test_begin_subtest "restoring gzipped stdin"
notmuch dump --gzip --output=backup.gz
notmuch tag +new_tag '*'
notmuch restore < backup.gz
notmuch dump --output=dump.actual
test_expect_equal_file dump.expected dump.actual

test_begin_subtest "restoring gzipped file"
notmuch dump --gzip --output=backup.gz
notmuch tag +new_tag '*'
notmuch restore --input=backup.gz
notmuch dump --output=dump.actual
test_expect_equal_file dump.expected dump.actual

# Note, we assume all messages from cworth have a message-id
# containing cworth.org

{ head -1 dump.expected ; grep 'cworth[.]org' dump.expected; } > dump-cworth.expected

test_begin_subtest "dump -- from:cworth"
notmuch dump -- from:cworth > dump-dash-cworth.actual
test_expect_equal_file dump-cworth.expected dump-dash-cworth.actual

test_begin_subtest "dump --output=outfile from:cworth"
notmuch dump --output=dump-outfile-cworth.actual from:cworth
test_expect_equal_file dump-cworth.expected dump-outfile-cworth.actual

test_begin_subtest "dump --output=outfile -- from:cworth"
notmuch dump --output=dump-outfile-dash-inbox.actual -- from:cworth
test_expect_equal_file dump-cworth.expected dump-outfile-dash-inbox.actual

test_begin_subtest "Check for a safe set of message-ids"
notmuch search --output=messages from:cworth | sed s/^id:// > EXPECTED
notmuch search --output=messages from:cworth | sed s/^id:// |\
	$TEST_DIRECTORY/hex-xcode --direction=encode > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "format=batch-tag, dump sanity check."
NOTMUCH_DUMP_TAGS --format=sup from:cworth | cut -f1 -d' ' | \
    sort > EXPECTED.$test_count
NOTMUCH_DUMP_TAGS --format=batch-tag from:cworth | sed 's/^.*-- id://' | \
    sort > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest "format=batch-tag, missing newline"
printf "+a_tag_without_newline -- id:20091117232137.GA7669@griffis1.net" > IN
notmuch restore --accumulate < IN
NOTMUCH_DUMP_TAGS id:20091117232137.GA7669@griffis1.net > OUT
cat <<EOF > EXPECTED
+a_tag_without_newline +inbox +unread -- id:20091117232137.GA7669@griffis1.net
EOF
test_expect_equal_file EXPECTED OUT

test_begin_subtest "format=batch-tag, # round-trip"
notmuch dump --format=sup | sort > EXPECTED.$test_count
notmuch dump --format=batch-tag > DUMPFILE
notmuch restore --format=batch-tag < DUMPFILE
notmuch dump --format=sup | sort > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest "format=batch-tag, # blank lines and comments"
notmuch dump --format=batch-tag| sort > EXPECTED.$test_count
notmuch restore <<EOF
# this line is a comment; the next has only white space
 	 

# the previous line is empty
EOF
notmuch dump --format=batch-tag | sort > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest "format=batch-tag, # reverse-round-trip empty tag"
cat <<EOF >EXPECTED.$test_count
+ -- id:20091117232137.GA7669@griffis1.net
EOF
notmuch restore --format=batch-tag < EXPECTED.$test_count
NOTMUCH_DUMP_TAGS --format=batch-tag id:20091117232137.GA7669@griffis1.net > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

tag1='comic_swear=$&^%$^%\\//-+$^%$'
enc1=$($TEST_DIRECTORY/hex-xcode --direction=encode "$tag1")

tag2=$(printf 'this\n tag\t has\n spaces')
enc2=$($TEST_DIRECTORY/hex-xcode --direction=encode "$tag2")

enc3='%c3%91%c3%a5%c3%b0%c3%a3%c3%a5%c3%a9-%c3%8f%c3%8a'
tag3=$($TEST_DIRECTORY/hex-xcode --direction=decode $enc3)

notmuch dump --format=batch-tag > BACKUP

notmuch tag +"$tag1" +"$tag2" +"$tag3" -inbox -unread "*"

# initial segment of file used for several tests below.
cat <<EOF > comments-and-blanks
# this is a comment

# next line has leading whitespace
  	

EOF

test_begin_subtest 'restoring empty file is not an error'
notmuch restore < /dev/null 2>OUTPUT.$test_count
cp /dev/null EXPECTED
test_expect_equal_file EXPECTED OUTPUT.$test_count

test_begin_subtest 'file of comments and blank lines is not an error'
notmuch restore --input=comments-and-blanks
ret_val=$?
test_expect_equal "$ret_val" "0"

cp comments-and-blanks leading-comments-blanks-batch-tag
echo "+some_tag -- id:yun1vjwegii.fsf@aiko.keithp.com" \
    >> leading-comments-blanks-batch-tag

test_begin_subtest 'detect format=batch-tag with leading comments and blanks'
notmuch restore --input=leading-comments-blanks-batch-tag
notmuch search --output=tags id:yun1vjwegii.fsf@aiko.keithp.com > OUTPUT.$test_count
echo "some_tag" > EXPECTED
test_expect_equal_file EXPECTED OUTPUT.$test_count

cp comments-and-blanks leading-comments-blanks-sup
echo "yun1vjwegii.fsf@aiko.keithp.com (another_tag)" \
    >> leading-comments-blanks-sup

test_begin_subtest 'detect format=sup with leading comments and blanks'
notmuch restore --input=leading-comments-blanks-sup
notmuch search --output=tags id:yun1vjwegii.fsf@aiko.keithp.com > OUTPUT.$test_count
echo "another_tag" > EXPECTED
test_expect_equal_file EXPECTED OUTPUT.$test_count

test_begin_subtest 'format=batch-tag, round trip with strange tags'
notmuch dump --format=batch-tag > EXPECTED.$test_count
notmuch dump --format=batch-tag > DUMPFILE
notmuch restore --format=batch-tag < DUMPFILE
notmuch dump --format=batch-tag > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest 'format=batch-tag, checking encoded output'
NOTMUCH_DUMP_TAGS --format=batch-tag -- from:cworth |\
	 awk "{ print \"+$enc1 +$enc2 +$enc3 -- \" \$5 }" > EXPECTED.$test_count
NOTMUCH_DUMP_TAGS --format=batch-tag -- from:cworth  > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest 'restoring sane tags'
notmuch restore --format=batch-tag < BACKUP
notmuch dump --format=batch-tag > OUTPUT.$test_count
test_expect_equal_file BACKUP OUTPUT.$test_count

test_begin_subtest 'format=batch-tag, restore=auto'
notmuch dump --format=batch-tag > EXPECTED.$test_count
notmuch tag -inbox -unread "*"
notmuch restore --format=auto < EXPECTED.$test_count
notmuch dump --format=batch-tag > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest 'format=sup, restore=auto'
notmuch dump --format=sup > EXPECTED.$test_count
notmuch tag -inbox -unread "*"
notmuch restore --format=auto < EXPECTED.$test_count
notmuch dump --format=sup > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest 'format=batch-tag, restore=default'
notmuch dump --format=batch-tag > EXPECTED.$test_count
notmuch tag -inbox -unread "*"
notmuch restore < EXPECTED.$test_count
notmuch dump --format=batch-tag > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest 'format=sup, restore=default'
notmuch dump --format=sup > EXPECTED.$test_count
notmuch tag -inbox -unread "*"
notmuch restore < EXPECTED.$test_count
notmuch dump --format=sup > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest 'restore: checking error messages'
notmuch restore <<EOF 2>OUTPUT
# the next line has a space
 
a
+0
+a +b
# trailing whitespace
+a +b 
+c +d --
# this is a harmless comment, do not yell about it.

# the previous line was blank; also no yelling please
+%zz -- id:whatever
+e +f id:"
+e +f tag:abc
# the next non-comment line should report an an empty tag error for
# batch tagging, but not for restore
+ +e -- id:20091117232137.GA7669@griffis1.net
# valid id, but warning about missing message
+e id:missing_message_id
# exercise parser
+e -- id:some)stuff
+e -- id:some stuff
+e -- id:some"stuff
+e -- id:"a_message_id_with""_a_quote"
+e -- id:"a message id with spaces"
+e --  id:an_id_with_leading_and_trailing_ws \

EOF

cat <<EOF > EXPECTED
Warning: cannot parse query: a (skipping)
Warning: no query string [+0]
Warning: no query string [+a +b]
Warning: missing query string [+a +b ]
Warning: no query string after -- [+c +d --]
Warning: hex decoding of tag %zz failed [+%zz -- id:whatever]
Warning: cannot parse query: id:" (skipping)
Warning: not an id query: tag:abc (skipping)
Warning: cannot apply tags to missing message: missing_message_id
Warning: cannot parse query: id:some)stuff (skipping)
Warning: cannot parse query: id:some stuff (skipping)
Warning: cannot apply tags to missing message: some"stuff
Warning: cannot apply tags to missing message: a_message_id_with"_a_quote
Warning: cannot apply tags to missing message: a message id with spaces
Warning: cannot apply tags to missing message: an_id_with_leading_and_trailing_ws
EOF

test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'roundtripping random message-ids and tags'

    ${TEST_DIRECTORY}/random-corpus --config-path=${NOTMUCH_CONFIG} \
			--num-messages=100

     notmuch dump --format=batch-tag| \
	 sort > EXPECTED.$test_count

     notmuch tag +this_tag_is_very_unlikely_to_be_random '*'

     notmuch restore --format=batch-tag < EXPECTED.$test_count

     notmuch dump --format=batch-tag| \
	 sort > OUTPUT.$test_count

test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_done

# Note the database is "poisoned" for sup format at this point.
