#!/bin/bash

test_description='tagging'

. ./perf-test-lib.sh

time_start

time_run 'tag * +new_tag' "notmuch tag +new_tag '*'"
time_run 'tag * +existing_tag' "notmuch tag +new_tag '*'"
time_run 'tag * -existing_tag' "notmuch tag -new_tag '*'"
time_run 'tag * -missing_tag' "notmuch tag -new_tag '*'"

time_done
