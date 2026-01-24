#!/usr/bin/env bash

test_description='git-remote-notmuch'

. $(dirname "$0")/perf-test-lib.sh || exit 1

add_tags() {
    local dir=$1
    local denom=$2
    local olddir=$(pwd)

    cd $dir
    find . -name tags -type f |
	while read -r path; do
	      if [ $(($RANDOM % $denom)) -eq 0 ]; then
		  echo $RANDOM >> $path
	      fi
	done

    cd $olddir
}

time_start

time_run 'clone --bare' "git clone --quiet --bare -b master notmuch::default default.git"
time_run 'clone' "git clone --quiet -b master notmuch:// repo"

time_run "push (no changes)" "git -C repo push --quiet origin master"

add_tags repo 10
git -C repo add -u
git -C repo commit --quiet -m'add tags to 10% of messages'
time_run "push (10% changed)" "git -C repo push --quiet origin master"

add_tags repo 4
git -C repo add -u
git -C repo commit --quiet -m'add tags to 25% of messages'
time_run "push (25% changed)" "git -C repo push --quiet origin master"

add_tags repo 2
git -C repo add -u
git -C repo commit --quiet -m'add tags to 50% of messages'
time_run "push (50% changed)" "git -C repo push --quiet origin master"

hash=$(git -C repo hash-object --stdin -w < /dev/null)
# replace all files with empty files
git -C repo ls-tree -r HEAD | sed "s/blob [^\t]*/blob $hash/" \
       | git -C repo update-index --index-info
git -C repo commit --quiet -m'zero tags' 2>>log.txt 1>&2

time_run "push (rem. all tags)" "git -C repo push --quiet origin master"

time_done
