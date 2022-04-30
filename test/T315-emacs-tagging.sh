#!/usr/bin/env bash

test_description="emacs interface"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

EXPECTED=$NOTMUCH_SRCDIR/test/emacs.expected-output

test_require_emacs
add_email_corpus

test_begin_subtest "Add tag from search view"
os_x_darwin_thread=$(notmuch search --output=threads id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com)
test_emacs "(notmuch-search \"$os_x_darwin_thread\")
	    (notmuch-test-wait)
	    (execute-kbd-macro \"+tag-from-search-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox tag-from-search-view unread)"

test_begin_subtest "Remove tag from search view"
test_emacs "(notmuch-search \"$os_x_darwin_thread\")
	    (notmuch-test-wait)
	    (execute-kbd-macro \"-tag-from-search-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)"

test_begin_subtest "Add tag (large query)"
# We use a long query to force us into batch mode and use a funny tag
# that requires escaping for batch tagging.
test_emacs "(notmuch-tag (concat \"$os_x_darwin_thread\" \" or \" (mapconcat #'identity (make-list notmuch-tag-argument-limit \"x\") \"-\")) (list \"+tag-from-%-large-query\"))"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox tag-from-%-large-query unread)"
notmuch tag -tag-from-%-large-query $os_x_darwin_thread

test_begin_subtest "notmuch-show: add single tag to single message"
test_emacs "(notmuch-show \"$os_x_darwin_thread\")
	    (execute-kbd-macro \"+tag-from-show-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox tag-from-show-view unread)"

test_begin_subtest "notmuch-show: remove single tag from single message"
test_emacs "(notmuch-show \"$os_x_darwin_thread\")
	    (execute-kbd-macro \"-tag-from-show-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)"

test_begin_subtest "notmuch-show: add multiple tags to single message"
test_emacs "(notmuch-show \"$os_x_darwin_thread\")
	    (execute-kbd-macro \"+tag1-from-show-view +tag2-from-show-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox tag1-from-show-view tag2-from-show-view unread)"

test_begin_subtest "notmuch-show: remove multiple tags from single message"
test_emacs "(notmuch-show \"$os_x_darwin_thread\")
	    (execute-kbd-macro \"-tag1-from-show-view -tag2-from-show-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)"

test_begin_subtest "notmuch-show: before-tag-hook is run, variables are defined"
output=$(test_emacs '(let ((notmuch-test-tag-hook-output nil)
	          (notmuch-before-tag-hook (function notmuch-test-tag-hook)))
	       (notmuch-show "id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com")
	       (execute-kbd-macro "+activate-hook\n")
	       (execute-kbd-macro "-activate-hook\n")
	       notmuch-test-tag-hook-output)')
test_expect_equal "$output" \
'(("id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com" "-activate-hook")
 ("id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com" "+activate-hook"))'

test_begin_subtest "notmuch-show: after-tag-hook is run, variables are defined"
output=$(test_emacs '(let ((notmuch-test-tag-hook-output nil)
	          (notmuch-after-tag-hook (function notmuch-test-tag-hook)))
	       (notmuch-show "id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com")
	       (execute-kbd-macro "+activate-hook\n")
	       (execute-kbd-macro "-activate-hook\n")
	       notmuch-test-tag-hook-output)')
test_expect_equal "$output" \
'(("id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com" "-activate-hook")
 ("id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com" "+activate-hook"))'


test_begin_subtest "Search thread tag operations are race-free"
add_message '[subject]="Search race test"'
gen_msg_id_1=$gen_msg_id
generate_message '[in-reply-to]="<'$gen_msg_id_1'>"' \
	    '[references]="<'$gen_msg_id_1'>"' \
	    '[subject]="Search race test two"'
test_emacs '(notmuch-search "subject:\"search race test\"")
	    (notmuch-test-wait)
	    (notmuch-poll)
	    (execute-kbd-macro "+search-thread-race-tag")'
output=$(notmuch search --output=messages 'tag:search-thread-race-tag')
test_expect_equal "$output" "id:$gen_msg_id_1"

test_begin_subtest "Search global tag operations are race-free"
generate_message '[in-reply-to]="<'$gen_msg_id_1'>"' \
	    '[references]="<'$gen_msg_id_1'>"' \
	    '[subject]="Re: Search race test"'
test_emacs '(notmuch-search "subject:\"search race test\" -subject:two")
	    (notmuch-test-wait)
	    (notmuch-poll)
	    (execute-kbd-macro "*+search-global-race-tag")'
output=$(notmuch search --output=messages 'tag:search-global-race-tag')
test_expect_equal "$output" "id:$gen_msg_id_1"

test_begin_subtest "undo with empty history is an error"
test_emacs "(let ((notmuch-tag-history nil))
  (test-log-error
   (notmuch-tag-undo)))
  "
cat <<EOF > EXPECTED
(error no further notmuch undo information)
EOF
test_expect_equal_file EXPECTED MESSAGES

for mode in search show tree unthreaded; do
    test_begin_subtest "undo tagging in $mode mode"
    test_emacs "(let ((notmuch-tag-history nil))
      (notmuch-$mode \"$os_x_darwin_thread\")
      (notmuch-test-wait)
      (execute-kbd-macro \"+tag-to-be-undone-$mode\")
      (notmuch-tag-undo)
      (notmuch-test-wait))"
    count=$(notmuch count "tag:tag-to-be-undone-$mode")
    test_expect_equal "$count" "0"

    test_begin_subtest "undo tagging in $mode mode (multiple operations)"
    test_emacs "(let ((notmuch-tag-history nil))
      (notmuch-$mode \"$os_x_darwin_thread\")
      (notmuch-test-wait)
      (execute-kbd-macro \"+one-$mode\")
      (execute-kbd-macro \"+two-$mode\")
      (notmuch-tag-undo)
      (notmuch-test-wait)
      (execute-kbd-macro \"+three-$mode\"))"
    output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
    notmuch tag "-one-$mode" "-three-$mode" $os_x_darwin_thread
    test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox one-$mode three-$mode unread)"

    test_begin_subtest "undo tagging in $mode mode (multiple undo)"
    test_emacs "(let ((notmuch-tag-history nil))
      (notmuch-$mode \"$os_x_darwin_thread\")
      (notmuch-test-wait)
      (execute-kbd-macro \"+one-$mode\")
      (execute-kbd-macro \"+two-$mode\")
      (notmuch-tag-undo)
      (notmuch-test-wait)
      (notmuch-tag-undo)
      (notmuch-test-wait)
      (execute-kbd-macro \"+three-$mode\"))"
    output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
    notmuch tag "-one-$mode" "-three-$mode" $os_x_darwin_thread
    test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox three-$mode unread)"

    test_begin_subtest "undo tagging in $mode mode (via binding)"
    test_emacs "(let ((notmuch-tag-history nil))
      (notmuch-$mode \"$os_x_darwin_thread\")
      (notmuch-test-wait)
      (execute-kbd-macro \"+tag-to-be-undone-$mode\")
      (execute-kbd-macro (kbd \"C-x u\"))
      (notmuch-test-wait))"
    count=$(notmuch count "tag:tag-to-be-undone-$mode")
    test_expect_equal "$count" "0"
done

test_done
