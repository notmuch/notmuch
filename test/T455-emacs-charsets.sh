#!/usr/bin/env bash

test_description="emacs notmuch-show charset handling"
. ./test-lib.sh || exit 1


UTF8_YEN=$'\xef\xbf\xa5'
BIG5_YEN=$'\xa2\x44'

# Add four messages with unusual encoding requirements:
#
# 1) text/plain in quoted-printable big5
generate_message \
    [id]=test-plain@example.com \
    '[content-type]="text/plain; charset=big5"' \
    '[content-transfer-encoding]=quoted-printable' \
    '[body]="Yen: =A2=44"'

# 2) text/plain in 8bit big5
generate_message \
    [id]=test-plain-8bit@example.com \
    '[content-type]="text/plain; charset=big5"' \
    '[content-transfer-encoding]=8bit' \
    '[body]="Yen: '$BIG5_YEN'"'

# 3) text/html in quoted-printable big5
generate_message \
    [id]=test-html@example.com \
    '[content-type]="text/html; charset=big5"' \
    '[content-transfer-encoding]=quoted-printable' \
    '[body]="<html><body>Yen: =A2=44</body></html>"'

# 4) application/octet-stream in quoted-printable of big5 text
generate_message \
    [id]=test-binary@example.com \
    '[content-type]="application/octet-stream"' \
    '[content-transfer-encoding]=quoted-printable' \
    '[body]="Yen: =A2=44"'

notmuch new > /dev/null

# Test rendering

test_begin_subtest "Text parts are decoded when rendering"
test_emacs '(notmuch-show "id:test-plain@example.com")
	    (test-visible-output "OUTPUT.raw")'
awk 'show {print} /^$/ {show=1}' < OUTPUT.raw > OUTPUT
cat <<EOF >EXPECTED
Yen: $UTF8_YEN
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "8bit text parts are decoded when rendering"
test_emacs '(notmuch-show "id:test-plain-8bit@example.com")
	    (test-visible-output "OUTPUT.raw")'
awk 'show {print} /^$/ {show=1}' < OUTPUT.raw > OUTPUT
cat <<EOF >EXPECTED
Yen: $UTF8_YEN
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "HTML parts are decoded when rendering"
test_emacs '(notmuch-show "id:test-html@example.com")
	    (test-visible-output "OUTPUT.raw")'
awk 'show {print} /^$/ {show=1}' < OUTPUT.raw > OUTPUT
cat <<EOF >EXPECTED
[ text/html ]
Yen: $UTF8_YEN
EOF
test_expect_equal_file EXPECTED OUTPUT

# Test saving

test_begin_subtest "Text parts are not decoded when saving"
rm -f part
test_emacs '(notmuch-show "id:test-plain@example.com")
	    (search-forward "Yen")
	    (let ((standard-input "\"part\""))
	       (notmuch-show-save-part))'
cat <<EOF >EXPECTED
Yen: $BIG5_YEN
EOF
test_expect_equal_file part EXPECTED

test_begin_subtest "8bit text parts are not decoded when saving"
rm -f part
test_emacs '(notmuch-show "id:test-plain-8bit@example.com")
	    (search-forward "Yen")
	    (let ((standard-input "\"part\""))
	       (notmuch-show-save-part))'
cat <<EOF >EXPECTED
Yen: $BIG5_YEN
EOF
test_expect_equal_file part EXPECTED

test_begin_subtest "HTML parts are not decoded when saving"
rm -f part
test_emacs '(notmuch-show "id:test-html@example.com")
	    (search-forward "Yen")
	    (let ((standard-input "\"part\""))
	       (notmuch-show-save-part))'
cat <<EOF >EXPECTED
<html><body>Yen: $BIG5_YEN</body></html>
EOF
test_expect_equal_file part EXPECTED

test_begin_subtest "Binary parts are not decoded when saving"
rm -f part
test_emacs '(notmuch-show "id:test-binary@example.com")
	    (search-forward "application/")
	    (let ((standard-input "\"part\""))
	       (notmuch-show-save-part))'
cat <<EOF >EXPECTED
Yen: $BIG5_YEN
EOF
test_expect_equal_file part EXPECTED

# Test message viewing

test_begin_subtest "Text message are not decoded when viewing"
test_emacs '(notmuch-show "id:test-plain@example.com")
	    (notmuch-show-view-raw-message)
	    (test-visible-output "OUTPUT.raw")'
awk 'show {print} /^$/ {show=1}' < OUTPUT.raw > OUTPUT
cat <<EOF >EXPECTED
Yen: =A2=44
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "8bit text message are not decoded when viewing"
test_emacs '(notmuch-show "id:test-plain-8bit@example.com")
	    (notmuch-show-view-raw-message)
	    (test-visible-output "OUTPUT.raw")'
awk 'show {print} /^$/ {show=1}' < OUTPUT.raw > OUTPUT
cat <<EOF >EXPECTED
Yen: $BIG5_YEN
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
