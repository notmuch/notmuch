#!/usr/bin/env bash

test_description="emacs notmuch-hello view"
. ./test-lib.sh || exit 1

EXPECTED=$TEST_DIRECTORY/emacs.expected-output

add_email_corpus

test_begin_subtest "User-defined section with inbox tag"
test_emacs "(let ((notmuch-hello-sections
                   (list (lambda () (notmuch-hello-insert-searches
                                     \"Test\" '((\"inbox\" . \"tag:inbox\")))))))
           (notmuch-hello)
           (test-output))"
test_expect_equal_file $EXPECTED/notmuch-hello-new-section OUTPUT

test_begin_subtest "User-defined section with empty, hidden entry"
test_emacs "(let ((notmuch-hello-sections
                   (list (lambda () (notmuch-hello-insert-searches
                                     \"Test-with-empty\"
                                     '((\"inbox\" . \"tag:inbox\")
                                       (\"doesnotexist\" . \"tag:doesnotexist\"))
                                     :hide-empty-searches t)))))
             (notmuch-hello)
             (test-output))"
test_expect_equal_file $EXPECTED/notmuch-hello-section-with-empty OUTPUT

test_begin_subtest "User-defined section, unread tag filtered out"
test_emacs "(let ((notmuch-hello-sections
                   (list (lambda () (notmuch-hello-insert-tags-section
                                     \"Test-with-filtered\"
                                     :hide-tags '(\"unread\"))))))
             (notmuch-hello)
             (test-output))"
test_expect_equal_file $EXPECTED/notmuch-hello-section-hidden-tag OUTPUT

test_begin_subtest "User-defined section, different query for counts"
test_emacs "(let ((notmuch-hello-sections
                   (list (lambda () (notmuch-hello-insert-tags-section
                                     \"Test-with-counts\"
                                     :filter-count \"tag:signed\")))))
             (notmuch-hello)
             (test-output))"
test_expect_equal_file $EXPECTED/notmuch-hello-section-counts OUTPUT

test_begin_subtest "Empty custom tags section"
test_emacs "(let* ((widget (widget-create 'notmuch-hello-tags-section))
                   (notmuch-hello-sections (list (widget-value widget))))
             (notmuch-hello)
             (test-output))"
test_expect_equal_file $EXPECTED/notmuch-hello-empty-custom-tags-section OUTPUT

test_begin_subtest "Empty custom queries section"
test_emacs "(let* ((widget (widget-create 'notmuch-hello-query-section))
                   (notmuch-hello-sections (list (widget-value widget))))
             (notmuch-hello)
             (test-output))"
test_expect_equal_file $EXPECTED/notmuch-hello-empty-custom-queries-section OUTPUT

test_begin_subtest "Column alignment for tag/queries with long names"
tag=a-very-long-tag # length carefully calculated for 80 characters window width
notmuch tag +$tag '*'
test_emacs '(notmuch-hello)
            (test-output)'
notmuch tag -$tag '*'
test_expect_equal_file $EXPECTED/notmuch-hello-long-names OUTPUT

test_done
