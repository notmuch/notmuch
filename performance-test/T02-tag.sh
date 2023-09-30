#!/usr/bin/env bash

test_description='tagging'

. $(dirname "$0")/perf-test-lib.sh || exit 1

time_start

time_run 'tag * +new_tag' "notmuch tag +new_tag '*'"
time_run 'tag * +existing_tag' "notmuch tag +new_tag '*'"
time_run 'tag * -existing_tag' "notmuch tag -new_tag '*'"
time_run 'tag * -missing_tag' "notmuch tag -new_tag '*'"

time_run 'tag * +maildir_flag F' "notmuch tag +flagged '*'"
time_run 'tag * -maildir_flag F' "notmuch tag -flagged '*'"
time_run 'tag * +maildir_flag P' "notmuch tag +passed '*'"
time_run 'tag * -maildir_flag P' "notmuch tag -passed '*'"
time_run 'tag * +maildir_flag D' "notmuch tag +draft '*'"
time_run 'tag * -maildir_flag D' "notmuch tag -draft '*'"
time_run 'tag * +maildir_flag S' "notmuch tag -unread '*'"
time_run 'tag * -maildir_flag S' "notmuch tag +unread '*'"

time_done
