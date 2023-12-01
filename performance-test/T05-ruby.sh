#!/usr/bin/env bash

test_description='ruby bindings'

. $(dirname "$0")/perf-test-lib.sh || exit 1

if [ "${NOTMUCH_HAVE_RUBY_DEV}" = "0" ]; then
    echo "missing prerequisites: ruby development files"
    exit 0
fi

time_start

time_run 'print all messages' "$NOTMUCH_RUBY -I '$NOTMUCH_BUILDDIR/bindings/ruby' <<'EOF'
require 'notmuch'
db = Notmuch::Database.new('$MAIL_DIR')
100.times.each do
    db.query('').search_messages.each do |msg|
	puts msg.message_id
    end
end
EOF"

time_done
