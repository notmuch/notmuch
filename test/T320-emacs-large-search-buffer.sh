#!/usr/bin/env bash
test_description="Emacs with large search results buffer"
. ./test-lib.sh || exit 1

x=xxxxxxxxxx # 10
x=$x$x$x$x$x$x$x$x$x$x # 100
x=$x$x$x$x$x$x$x$x$x # 900

# We generate a long subject here (over 900 bytes) so that the emacs
# search results get large quickly. With 30 such messages we should
# cross several 4kB page boundaries and see the bug.
n=30
for i in $(seq 1 $n); do
  # Roughly 100B2 KiB per message.  That is, we need two messages in order to
  # exceed the typical size of the pipe buffer (4 KiB on commodity systems).
  generate_message '[subject]="$x $i of $n"'
done

notmuch new > /dev/null

test_begin_subtest "Ensure that emacs doesn't drop results"
notmuch search '*' > EXPECTED
sed -i -e 's/^thread:[0-9a-f]*  //' -e 's/;//' -e 's/xx*/[BLOB]/' EXPECTED
echo 'End of search results.' >> EXPECTED

test_emacs '(notmuch-search "*")
	    (notmuch-test-wait)
	    (test-output)'
sed -i -e s',  *, ,g' -e 's/xxx*/[BLOB]/g' OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_done
