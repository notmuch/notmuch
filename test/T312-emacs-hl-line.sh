#!/usr/bin/env bash

test_description="emacs hl-line-mode"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

EXPECTED=$NOTMUCH_SRCDIR/test/emacs.expected-output

test_require_emacs
add_email_corpus

test_begin_subtest "line 1, search"
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-search "tag:inbox")
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			 (let ((start (overlay-start hl-line-overlay))
			       (end (overlay-end hl-line-overlay)))
			   (list start (> end start)))
			 (list 1 t)))'

test_begin_subtest "line 1, tree"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-tree "tag:inbox")
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			 (let ((start (overlay-start hl-line-overlay))
			       (end (overlay-end hl-line-overlay)))
			   (list start (> end start)))
			 (list 1 t)))'

test_begin_subtest "line 1, unthreaded"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-tree "tag:inbox")
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			 (let ((start (overlay-start hl-line-overlay))
			       (end (overlay-end hl-line-overlay)))
			   (list start (> end start)))
			 (list 1 t)))'

test_begin_subtest "line 1, search, refresh"
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-search "tag:inbox")
			(notmuch-test-wait)
			(notmuch-refresh-this-buffer)
			(notmuch-test-wait)
			(notmuch-test-expect-equal (overlay-start hl-line-overlay) 1))'

test_begin_subtest "line 1, tree, refresh"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-tree "tag:inbox")
			(notmuch-test-wait)
			(notmuch-refresh-this-buffer)
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			 (let ((start (overlay-start hl-line-overlay))
			       (end (overlay-end hl-line-overlay)))
			   (list start (> end start)))
			 (list 1 t)))'

test_begin_subtest "line 1, unthreaded, refresh"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-tree "tag:inbox")
			(notmuch-test-wait)
			(notmuch-refresh-this-buffer)
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			 (let ((start (overlay-start hl-line-overlay))
			       (end (overlay-end hl-line-overlay)))
			   (list start (> end start)))
			 (list 1 t)))'


test_begin_subtest "line 12, notmuch-search"
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-search "tag:inbox")
			(notmuch-test-wait)
			(forward-line 11)
			(notmuch-post-command)
			(notmuch-test-expect-equal
			   (line-number-at-pos (overlay-start hl-line-overlay)) 12))'

test_begin_subtest "line 12, tree"
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-tree "tag:inbox")
			(notmuch-test-wait)
			(forward-line 11)
			(notmuch-post-command)
			(notmuch-test-expect-equal
			   (line-number-at-pos (overlay-start hl-line-overlay)) 12))'

test_begin_subtest "line 12, unthreaded"
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-unthreaded "tag:inbox")
			(notmuch-test-wait)
			(forward-line 11)
			(notmuch-post-command)
			(notmuch-test-expect-equal
			   (line-number-at-pos (overlay-start hl-line-overlay)) 12))'

test_begin_subtest "line 12, search, refresh"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-search "tag:inbox")
			(notmuch-test-wait)
			(forward-line 11)
			(notmuch-post-command)
			(notmuch-refresh-this-buffer)
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			   (line-number-at-pos (overlay-start hl-line-overlay)) 12))'

test_begin_subtest "line 12, tree, refresh"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-tree "tag:inbox")
			(notmuch-test-wait)
			(forward-line 11)
			(notmuch-post-command)
			(notmuch-refresh-this-buffer)
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			   (line-number-at-pos (overlay-start hl-line-overlay)) 12))'

test_begin_subtest "line 12, unthreaded, refresh"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line t))
			(notmuch-tree "tag:inbox")
			(notmuch-test-wait)
			(forward-line 11)
			(notmuch-post-command)
			(notmuch-refresh-this-buffer)
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			   (line-number-at-pos (overlay-start hl-line-overlay)) 12))'

test_begin_subtest "hl-line disabled, search"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line nil))
			(notmuch-search "tag:inbox")
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			   (or hl-line-mode hl-line-overlay)
			   nil))'

test_begin_subtest "hl-line disabled, tree"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line nil))
			(notmuch-tree "tag:inbox")
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			   (or hl-line-mode hl-line-overlay)
			   nil))'

test_begin_subtest "hl-line disabled, unthreaded"
test_subtest_known_broken
test_emacs_expect_t '(let ((notmuch-hl-line nil))
			(notmuch-unthreaded "tag:inbox")
			(notmuch-test-wait)
			(notmuch-test-expect-equal
			   (or hl-line-mode hl-line-overlay)
			   nil))'

test_done

