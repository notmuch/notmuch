#!/usr/bin/env bash

test_description="exclude options persist between Emacs search and tree modes"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

EXPECTED=$NOTMUCH_SRCDIR/test/emacs-exclude.expected-output

test_require_emacs
add_email_corpus
notmuch config set search.exclude_tags deleted
notmuch tag +deleted -- 'from:"Stewart Smith"' or 'from:"Chris Wilson"'

# Basic test cases just asserting exclude option is working and consistent.

test_begin_subtest "Search doesn't contain excluded mail by default"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-search-tag-inbox-without-excluded OUTPUT

test_begin_subtest "Search from search box doesn't contain excluded mail by default"
test_subtest_known_broken
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "Search:")
	    (forward-char 1)
	    (insert "tag:inbox")
	    (widget-field-activate (point))
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-search-tag-inbox-without-excluded OUTPUT

test_begin_subtest "Toggling exclude in search will show excluded mail"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-search-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-search-tag-inbox-with-excluded OUTPUT

test_begin_subtest "Tree search doesn't contain excluded mail by default"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-tree-from-search-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox-without-excluded OUTPUT

test_begin_subtest "Toggling exclude in tree search will show excluded mail"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-tree-from-search-current-query)
	    (notmuch-test-wait)
	    (notmuch-tree-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox-with-excluded OUTPUT

test_begin_subtest "Unthreaded search doesn't contain excluded mail by default"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-unthreaded-from-search-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-unthreaded-tag-inbox-without-excluded OUTPUT

test_begin_subtest "Toggling exclude in unthreaded will show excluded mail"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-unthreaded-from-search-current-query)
	    (notmuch-test-wait)
	    (notmuch-tree-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-unthreaded-tag-inbox-with-excluded OUTPUT

# Cycling from search to tree to unthreaded and vice versa will persist the current
# value of notmuch-search-hide-excluded.

test_begin_subtest "Value of hide-excluded from search persists into tree search"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-search-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (notmuch-tree-from-search-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox-with-excluded OUTPUT

test_begin_subtest "Value of hide-excluded from search persists into unthreaded"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-search-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (notmuch-unthreaded-from-search-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-unthreaded-tag-inbox-with-excluded OUTPUT

test_begin_subtest "Value of hide-excluded from tree persists into search"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-tree-from-search-current-query)
	    (notmuch-test-wait)
	    (notmuch-tree-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (notmuch-search-from-tree-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-search-tag-inbox-with-excluded OUTPUT

test_begin_subtest "Value of hide-excluded from tree persists into unthreaded"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-tree-from-search-current-query)
	    (notmuch-test-wait)
	    (notmuch-tree-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (notmuch-unthreaded-from-tree-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-unthreaded-tag-inbox-with-excluded OUTPUT

test_begin_subtest "Value of hide-excluded from unthreaded persists into tree"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-unthreaded-from-search-current-query)
	    (notmuch-test-wait)
	    (notmuch-tree-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (notmuch-tree-from-unthreaded-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-tree-tag-inbox-with-excluded OUTPUT

test_begin_subtest "Value of hide-excluded from unthreaded persists into search"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (notmuch-unthreaded-from-search-current-query)
	    (notmuch-test-wait)
	    (notmuch-tree-toggle-hide-excluded)
	    (notmuch-test-wait)
	    (notmuch-search-from-tree-current-query)
	    (notmuch-test-wait)
	    (test-output)
	    (delete-other-windows)'
test_expect_equal_file $EXPECTED/notmuch-search-tag-inbox-with-excluded OUTPUT

test_done
