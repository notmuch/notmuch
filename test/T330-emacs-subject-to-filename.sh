#!/usr/bin/env bash

test_description="emacs: mail subject to filename"
. ./test-lib.sh || exit 1

# emacs server can't be started in a child process with $(test_emacs ...)
test_emacs '(ignore)' > /dev/null

# test notmuch-wash-subject-to-patch-sequence-number (subject)
test_begin_subtest "no patch sequence number"
output=$(test_emacs '(format "%S" (notmuch-wash-subject-to-patch-sequence-number
      "[PATCH] A normal patch subject without numbers"))'
)
test_expect_equal "$output" '"nil"'

test_begin_subtest "patch sequence number #1"
output=$(test_emacs '(notmuch-wash-subject-to-patch-sequence-number
      "[PATCH 2/3] A most regular patch subject")'
)
test_expect_equal "$output" 2

test_begin_subtest "patch sequence number #2"
output=$(test_emacs '(notmuch-wash-subject-to-patch-sequence-number
      "  [dummy list prefix]  [RFC PATCH v2 13/42]  Special prefixes")'
)
test_expect_equal "$output" 13

test_begin_subtest "patch sequence number #3"
output=$(test_emacs '(notmuch-wash-subject-to-patch-sequence-number
      "[PATCH 2/3] [PATCH 032/037] use the last prefix")'
)
test_expect_equal "$output" 32

test_begin_subtest "patch sequence number #4"
output=$(test_emacs '(notmuch-wash-subject-to-patch-sequence-number
      "[dummy list prefix] [PATCH 2/3] PATCH 3/3] do not use a broken prefix")'
)
test_expect_equal "$output" 2

test_begin_subtest "patch sequence number #5"
output=$(test_emacs '(notmuch-wash-subject-to-patch-sequence-number
      "[RFC][PATCH 3/5][PATCH 4/5][PATCH 5/5] A made up test")'
)
test_expect_equal "$output" 5

test_begin_subtest "patch sequence number #6"
output=$(test_emacs '(notmuch-wash-subject-to-patch-sequence-number
      "[PATCH 2/3] this -> [PATCH 3/3] is not a prefix anymore [nor this 4/4]")'
)
test_expect_equal "$output" 2

test_begin_subtest "patch sequence number #7"
output=$(test_emacs '(notmuch-wash-subject-to-patch-sequence-number
      "[liberally accept crapola right before123/456and after] the numbers")'
)
test_expect_equal "$output" 123

# test notmuch-wash-subject-to-filename (subject &optional maxlen)
test_begin_subtest "filename #1"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      "just a subject line")'
)
test_expect_equal "$output" '"just-a-subject-line"'

test_begin_subtest "filename #2"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      " [any]  [prefixes are ] [removed!] from the subject")'
)
test_expect_equal "$output" '"from-the-subject"'

test_begin_subtest "filename #3"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      "  leading and trailing space  ")'
)
test_expect_equal "$output" '"leading-and-trailing-space"'

test_begin_subtest "filename #4"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      "!#  leading ()// &%, and in between_and_trailing garbage ()(&%%")'
)
test_expect_equal "$output" '"-leading-and-in-between_and_trailing-garbage"'

test_begin_subtest "filename #5"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-_01234567890")'
)
test_expect_equal "$output" '"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-_01234567890"'

test_begin_subtest "filename #6"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      "sequences of ... are squashed and trailing are removed ...")'
)
test_expect_equal "$output" '"sequences-of-.-are-squashed-and-trailing-are-removed"'

test_begin_subtest "filename #7"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      "max length test" 1)'
)
test_expect_equal "$output" '"m"'

test_begin_subtest "filename #8"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      "max length test /&(/%&/%%&¤%¤" 20)'
)
test_expect_equal "$output" '"max-length-test"'

test_begin_subtest "filename #9"
output=$(test_emacs '(notmuch-wash-subject-to-filename
      "[a prefix] [is only separated] by [spaces], so \"by\" is not okay!")'
)
test_expect_equal "$output" '"by-spaces-so-by-is-not-okay"'

# test notmuch-wash-subject-to-patch-filename (subject)
test_begin_subtest "patch filename #1"
output=$(test_emacs '(notmuch-wash-subject-to-patch-filename
      "[RFC][PATCH 099/100] rewrite notmuch")'
)
test_expect_equal "$output" '"0099-rewrite-notmuch.patch"'

test_begin_subtest "patch filename #2"
output=$(test_emacs '(notmuch-wash-subject-to-patch-filename
      "[RFC PATCH v1] has no patch number, default to 1")'
)
test_expect_equal "$output" '"0001-has-no-patch-number-default-to-1.patch"'

test_begin_subtest "patch filename #3"
output=$(test_emacs '(notmuch-wash-subject-to-patch-filename
      "[PATCH 4/5] the maximum length of a patch filename is 52 + patch sequence number + .patch extension")'
)
test_expect_equal "$output" '"0004-the-maximum-length-of-a-patch-filename-is-52-patch-s.patch"'

test_begin_subtest "patch filename #4"
output=$(test_emacs '(notmuch-wash-subject-to-patch-filename
      "[PATCH 4/5] the maximum length of a patch filename is 52 + patchh ! sequence number + .patch extension, *before* trimming trailing - and .")'
)
test_expect_equal "$output" '"0004-the-maximum-length-of-a-patch-filename-is-52-patchh.patch"'

test_done
