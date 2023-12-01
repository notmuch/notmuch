#!/usr/bin/env bash

test_description='emacs operations'

. $(dirname "$0")/perf-test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

test_require_emacs

time_start

print_emacs_header

MSGS=$(notmuch search --output=messages "*" | shuf -n 50 | awk '{printf " \"%s\"",$1}')

time_emacs "tag messages" \
"(dolist (msg (list $MSGS))
   (notmuch-tag msg (list \"+test\"))
   (notmuch-tag msg (list \"-test\"))))"

time_emacs "show warmup" \
	   '(notmuch-show "thread:{id:tip-4f8219875a0dad2cfad9e93a3fafcd9626db98d2@git.kernel.org}")'

time_emacs "show thread #1" \
	   '(notmuch-show "thread:{id:tip-4f8219875a0dad2cfad9e93a3fafcd9626db98d2@git.kernel.org}")'

time_emacs "depth bound #1" \
	   '(let ((notmuch-show-depth-limit 0))
		(notmuch-show "thread:{id:tip-4f8219875a0dad2cfad9e93a3fafcd9626db98d2@git.kernel.org}"))'

time_emacs "height bound #1" \
	   '(let ((notmuch-show-height-limit -1))
		(notmuch-show "thread:{id:tip-4f8219875a0dad2cfad9e93a3fafcd9626db98d2@git.kernel.org}"))'

time_emacs "size bound #1" \
	   '(let ((notmuch-show-max-text-part-size 1))
		(notmuch-show "thread:{id:tip-4f8219875a0dad2cfad9e93a3fafcd9626db98d2@git.kernel.org}"))'

time_emacs "show thread #2" \
	   '(notmuch-show "thread:{id:20101208005731.943729010@clark.site}")'

time_emacs "depth bound #2" \
	   '(let ((notmuch-show-depth-limit 0))
		(notmuch-show "thread:{id:20101208005731.943729010@clark.site}"))'

time_emacs "height bound #2" \
	   '(let ((notmuch-show-height-limit -1))
		(notmuch-show "thread:{id:20101208005731.943729010@clark.site}"))'

time_emacs "size bound #2" \
	   '(let ((notmuch-show-max-text-part-size 1))
		(notmuch-show "thread:{id:20101208005731.943729010@clark.site}"))'

time_emacs "show thread #3" \
	   '(notmuch-show "thread:{id:20120109014938.GE20796@mit.edu}")'

time_emacs "depth bound #3" \
	   '(let ((notmuch-show-depth-limit 0))
		(notmuch-show "thread:{id:20120109014938.GE20796@mit.edu}"))'

time_emacs "height bound #3" \
	   '(let ((notmuch-show-height-limit -1))
		(notmuch-show "thread:{id:20120109014938.GE20796@mit.edu}"))'

time_emacs "size bound #3" \
	   '(let ((notmuch-show-max-text-part-size 1))
		(notmuch-show "thread:{id:20120109014938.GE20796@mit.edu}"))'

time_emacs "show thread #4" \
	   '(notmuch-show "thread:{id:1280704593.25620.48.camel@mulgrave.site}")'

time_emacs "depth bound #4" \
	   '(let ((notmuch-show-depth-limit 0))
		(notmuch-show "thread:{id:1280704593.25620.48.camel@mulgrave.site}"))'

time_emacs "height bound #4" \
	   '(let ((notmuch-show-height-limit -1))
		(notmuch-show "thread:{id:1280704593.25620.48.camel@mulgrave.site}"))'

time_emacs "size bound #4" \
	   '(let ((notmuch-show-max-text-part-size 1))
		(notmuch-show "thread:{id:1280704593.25620.48.camel@mulgrave.site}"))'

time_done
