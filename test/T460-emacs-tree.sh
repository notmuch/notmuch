#!/usr/bin/env bash

test_description="emacs tree view interface"
. ./test-lib.sh || exit 1

EXPECTED=$TEST_DIRECTORY/tree.expected-output

add_email_corpus

test_begin_subtest "Basic notmuch-tree view in emacs"
test_emacs '(notmuch-tree "tag:inbox")
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox OUTPUT

test_begin_subtest "Refreshed notmuch-tree view in emacs"
test_emacs '(notmuch-tree "tag:inbox")
	    (notmuch-test-wait)
	    (notmuch-tree-refresh-view)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox OUTPUT

# In the following tag tests we make sure the display is updated
# correctly and, in a separate test, that the database is updated
# correctly.

test_begin_subtest "Tag message in notmuch tree view (display)"
test_emacs '(notmuch-tree "tag:inbox")
	    (notmuch-test-wait)
	    (forward-line)
	    (notmuch-tree-tag (list "+test_tag"))
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox-tagged OUTPUT

test_begin_subtest "Tag message in notmuch tree view (database)"
output=$(notmuch search --output=messages 'tag:test_tag')
test_expect_equal "$output" "id:877h1wv7mg.fsf@inf-8657.int-evry.fr"

test_begin_subtest "Untag message in notmuch tree view"
test_emacs '(notmuch-tree "tag:inbox")
	    (notmuch-test-wait)
	    (forward-line)
	    (notmuch-tree-tag (list "-test_tag"))
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox OUTPUT

test_begin_subtest "Untag message in notmuch tree view (database)"
output=$(notmuch search --output=messages 'tag:test_tag')
test_expect_equal "$output" ""

test_begin_subtest "Tag thread in notmuch tree view"
test_emacs '(notmuch-tree "tag:inbox")
	    (notmuch-test-wait)
	    ;; move to a sizable thread
	    (forward-line 26)
	    (notmuch-tree-tag-thread (list "+test_thread_tag"))
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox-thread-tagged OUTPUT

test_begin_subtest "Tag message in notmuch tree view (database)"
output=$(notmuch search --output=messages 'tag:test_thread_tag')
test_expect_equal "$output" \
"id:87ocn0qh6d.fsf@yoom.home.cworth.org
id:20091118005040.GA25380@dottiness.seas.harvard.edu
id:yunaayketfm.fsf@aiko.keithp.com
id:87fx8can9z.fsf@vertex.dottedmag
id:20091117203301.GV3165@dottiness.seas.harvard.edu
id:87iqd9rn3l.fsf@vertex.dottedmag
id:20091117190054.GU3165@dottiness.seas.harvard.edu"

test_begin_subtest "Untag thread in notmuch tree view"
test_emacs '(notmuch-tree "tag:inbox")
	    (notmuch-test-wait)
	    ;; move to the same sizable thread as above
	    (forward-line 26)
	    (notmuch-tree-tag-thread (list "-test_thread_tag"))
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox OUTPUT

test_begin_subtest "Untag message in notmuch tree view (database)"
output=$(notmuch search --output=messages 'tag:test_thread_tag')
test_expect_equal "$output" ""

test_begin_subtest "Navigation of notmuch-hello to search results"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-tree-from-search-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox OUTPUT

test_begin_subtest "Tree view of a single thread (from search)"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-tree-from-search-thread)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-single-thread OUTPUT

test_begin_subtest "Tree view of a single thread (from show)"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-search-show-thread)
	    (notmuch-tree-from-show-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-single-thread OUTPUT

test_begin_subtest "Message window of tree view"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-search-next-thread)
	    (notmuch-tree-from-search-thread)
	    (notmuch-test-wait)
	    (select-window notmuch-tree-message-window)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-show-window OUTPUT

test_begin_subtest "Stash id"
output=$(test_emacs '(notmuch-tree "id:1258498485-sup-142@elly")
		     (notmuch-test-wait)
		     (notmuch-show-stash-message-id)')
test_expect_equal "$output" "\"Stashed: id:1258498485-sup-142@elly\""

test_begin_subtest "Move to next matching message"
output=$(test_emacs '(notmuch-tree "from:cworth")
		     (notmuch-test-wait)
		     (notmuch-tree-next-matching-message)
		     (notmuch-show-stash-message-id)')
test_expect_equal "$output" "\"Stashed: id:878we4qdqf.fsf@yoom.home.cworth.org\""

test_begin_subtest "Move to next thread"
output=$(test_emacs '(notmuch-tree "tag:inbox")
		     (notmuch-test-wait)
		     (forward-line 26)
		     (notmuch-tree-next-thread)
		     (notmuch-show-stash-message-id)')
test_expect_equal "$output" "\"Stashed: id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net\""

test_begin_subtest "Move to previous thread"
output=$(test_emacs '(notmuch-tree "tag:inbox")
		     (notmuch-test-wait)
		     (forward-line 26)
		     (notmuch-tree-prev-thread)
		     (notmuch-show-stash-message-id)')
test_expect_equal "$output" "\"Stashed: id:20091117190054.GU3165@dottiness.seas.harvard.edu\""

test_begin_subtest "Move to previous previous thread"
output=$(test_emacs '(notmuch-tree "tag:inbox")
		     (notmuch-test-wait)
		     (forward-line 26)
		     (notmuch-tree-prev-thread)
		     (notmuch-tree-prev-thread)
		     (notmuch-show-stash-message-id)')
test_expect_equal "$output" "\"Stashed: id:1258493565-13508-1-git-send-email-keithp@keithp.com\""

test_done
