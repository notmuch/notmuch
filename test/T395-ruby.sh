#!/usr/bin/env bash
test_description="ruby bindings"
. $(dirname "$0")/test-lib.sh || exit 1

if [ "${NOTMUCH_HAVE_RUBY_DEV}" = "0" ]; then
    test_subtest_missing_external_prereq_["ruby development files"]=t
fi

add_email_corpus

test_ruby() {
    (
	cat <<-EOF
	require 'notmuch'
	@db = Notmuch::Database.new('$MAIL_DIR')
	EOF
	cat
    ) | $NOTMUCH_RUBY -I "$NOTMUCH_BUILDDIR/bindings/ruby"> OUTPUT
    test_expect_equal_file EXPECTED OUTPUT
}

test_begin_subtest "compare thread ids"
notmuch search --sort=oldest-first --output=threads tag:inbox | sed s/^thread:// > EXPECTED
test_ruby <<"EOF"
@q = @db.query('tag:inbox')
@q.sort = Notmuch::SORT_OLDEST_FIRST
for t in @q.search_threads do
  puts t.thread_id
end
EOF

test_begin_subtest "compare message ids"
notmuch search --sort=oldest-first --output=messages tag:inbox | sed s/^id:// > EXPECTED
test_ruby <<"EOF"
@q = @db.query('tag:inbox')
@q.sort = Notmuch::SORT_OLDEST_FIRST
for m in @q.search_messages do
  puts m.message_id
end
EOF

test_begin_subtest "get non-existent file"
echo true > EXPECTED
test_ruby <<"EOF"
result = @db.find_message_by_filename('i-dont-exist')
puts (result == nil)
EOF

test_begin_subtest "count messages"
notmuch count --output=messages tag:inbox > EXPECTED
test_ruby <<"EOF"
@q = @db.query('tag:inbox')
puts @q.count_messages()
EOF

test_begin_subtest "count threads"
notmuch count --output=threads tag:inbox > EXPECTED
test_ruby <<"EOF"
@q = @db.query('tag:inbox')
puts @q.count_threads()
EOF

test_begin_subtest "get all tags"
notmuch search --output=tags '*' > EXPECTED
test_ruby <<"EOF"
@t = @db.all_tags()
for tag in @t do
   puts tag
end
EOF

test_done
