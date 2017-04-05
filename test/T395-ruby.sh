#!/usr/bin/env bash
test_description="ruby bindings"
. ./test-lib.sh || exit 1

if [ "${NOTMUCH_HAVE_RUBY_DEV}" = "0" ]; then
    test_subtest_missing_external_prereq_["ruby development files"]=t
fi

add_email_corpus

test_begin_subtest "compare thread ids"
test_ruby <<"EOF"
require 'notmuch'
$maildir = ENV['MAIL_DIR']
if not $maildir then
  abort('environment variable MAIL_DIR must be set')
end
@db = Notmuch::Database.new($maildir)
@q = @db.query('tag:inbox')
@q.sort = Notmuch::SORT_OLDEST_FIRST
for t in @q.search_threads do
  print t.thread_id, "\n"
end
EOF
notmuch search --sort=oldest-first --output=threads tag:inbox | sed s/^thread:// > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "compare message ids"
test_ruby <<"EOF"
require 'notmuch'
$maildir = ENV['MAIL_DIR']
if not $maildir then
  abort('environment variable MAIL_DIR must be set')
end
@db = Notmuch::Database.new($maildir)
@q = @db.query('tag:inbox')
@q.sort = Notmuch::SORT_OLDEST_FIRST
for m in @q.search_messages do
  print m.message_id, "\n"
end
EOF
notmuch search --sort=oldest-first --output=messages tag:inbox | sed s/^id:// > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get non-existent file"
test_ruby <<"EOF"
require 'notmuch'
$maildir = ENV['MAIL_DIR']
if not $maildir then
  abort('environment variable MAIL_DIR must be set')
end
@db = Notmuch::Database.new($maildir)
result = @db.find_message_by_filename('i-dont-exist')
print (result == nil)
EOF
test_expect_equal "$(cat OUTPUT)" "true"

test_begin_subtest "count messages"
test_ruby <<"EOF"
require 'notmuch'
$maildir = ENV['MAIL_DIR']
if not $maildir then
  abort('environment variable MAIL_DIR must be set')
end
@db = Notmuch::Database.new($maildir)
@q = @db.query('tag:inbox')
print @q.count_messages(),"\n"
EOF
notmuch count --output=messages tag:inbox > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "count threads"
test_ruby <<"EOF"
require 'notmuch'
$maildir = ENV['MAIL_DIR']
if not $maildir then
  abort('environment variable MAIL_DIR must be set')
end
@db = Notmuch::Database.new($maildir)
@q = @db.query('tag:inbox')
print @q.count_threads(),"\n"
EOF
notmuch count --output=threads tag:inbox > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get all tags"
test_ruby <<"EOF"
require 'notmuch'
$maildir = ENV['MAIL_DIR']
if not $maildir then
  abort('environment variable MAIL_DIR must be set')
end
@db = Notmuch::Database.new($maildir)
@t = @db.all_tags()
for tag in @t do
   print tag,"\n"
end
EOF
notmuch search --output=tags '*' > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_done
