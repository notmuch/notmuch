#!/usr/bin/env bash

test_description="emacs reply"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

EXPECTED=$NOTMUCH_SRCDIR/test/emacs-reply.expected-output

test_require_emacs

add_email_corpus attachment

test_begin_subtest "tar not inlined by default"
test_emacs '(notmuch-mua-new-reply "id:874llc2bkp.fsf@curie.anarc.at")
	(test-visible-output "OUTPUT.raw")'
cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Antoine Beaupré <anarcat@orangeseeds.org>
Subject: Re: bug: "no top level messages" crash on Zen email loops
In-Reply-To: <874llc2bkp.fsf@curie.anarc.at>
Fcc: MAIL_DIR/sent
--text follows this line--
Antoine Beaupré <anarcat@orangeseeds.org> writes:

> And obviously I forget the frigging attachment.
>
>
> PS: don't we have a "you forgot to actually attach the damn file" plugin
> when we detect the word "attachment" and there's no attach? :p
EOF
notmuch_dir_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

add_email_corpus duplicate

ID2=87r2geywh9.fsf@tethera.net
for dup in {1..2}; do
    test_begin_subtest "body, duplicate=${dup}"
    test_emacs "(notmuch-show \"id:${ID2}\")
	   (notmuch-test-wait)
	   (notmuch-show-choose-duplicate $dup)
	   (notmuch-test-wait)
	   (notmuch-show-reply)
	   (test-visible-output \"OUTPUT.raw\")"
    output=$(grep '^> # body' OUTPUT.raw)
    test_expect_equal "$output" "> # body ${dup}"
done

ID3=87r2ecrr6x.fsf@zephyr.silentflame.com
test_begin_subtest "duplicate=3, subject"
test_emacs "(notmuch-show \"id:${ID3}\")
	   (notmuch-test-wait)
	   (notmuch-show-choose-duplicate 3)
	   (notmuch-test-wait)
	   (notmuch-show-reply)
	   (test-visible-output \"OUTPUT\")"
output=$(sed -n 's/^Subject: //p' OUTPUT)
file=$(notmuch search --output=files id:${ID3} | head -n 3 | tail -n 1)
subject=$(sed -n 's/^Subject: //p' $file)
test_expect_equal "$output" "Re: $subject"

test_begin_subtest "duplicate=4"
test_emacs "(notmuch-show \"id:${ID3}\")
	   (notmuch-show-choose-duplicate 4)
	   (notmuch-test-wait)
	   (notmuch-show-reply)
	   (test-visible-output \"OUTPUT.raw\")"
notmuch_dir_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file_nonempty $EXPECTED/notmuch-reply-duplicate-4 OUTPUT

add_email_corpus default

ID4=1258471718-6781-2-git-send-email-dottedmag@dottedmag.net
test_begin_subtest "if message-cite-reply-position is 'above, position citation below signature"
test_emacs "(let ((message-cite-reply-position 'above)
             (message-signature \"EMAIL SIGNATURE\"))
         (notmuch-mua-new-reply \"id:${ID4}\")
         (test-visible-output \"OUTPUT.raw\"))"
cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Mikhail Gusarov <dottedmag@dottedmag.net>
Subject: Re: [notmuch] [PATCH 2/2] Include <stdint.h> to get uint32_t in C++ file with gcc 4.4
In-Reply-To: <1258471718-6781-2-git-send-email-dottedmag@dottedmag.net>
Fcc: MAIL_DIR/sent
--text follows this line--

-- 
EMAIL SIGNATURE

"Mikhail Gusarov" <dottedmag@dottedmag.net> writes:

> Signed-off-by: Mikhail Gusarov <dottedmag at dottedmag.net>
> ---
>  lib/message.cc |    2 ++
>  1 files changed, 2 insertions(+), 0 deletions(-)
>
> diff --git a/lib/message.cc b/lib/message.cc
> index 72c350f..a4b090b 100644
> --- a/lib/message.cc
> +++ b/lib/message.cc
> @@ -21,6 +21,8 @@
>  #include "notmuch-private.h"
>  #include "database-private.h"
>  
> +#include <stdint.h>
> +
>  #include <gmime/gmime.h>
>  
>  #include <xapian.h>
> -- 
> 1.6.3.3
EOF
notmuch_dir_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "if message-cite-reply-position is 'traditional, position citation above signature"
test_emacs "(let ((message-cite-reply-position 'traditional)
             (message-signature \"EMAIL SIGNATURE\"))
         (notmuch-mua-new-reply \"id:${ID4}\")
         (test-visible-output \"OUTPUT.raw\"))"
cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Mikhail Gusarov <dottedmag@dottedmag.net>
Subject: Re: [notmuch] [PATCH 2/2] Include <stdint.h> to get uint32_t in C++ file with gcc 4.4
In-Reply-To: <1258471718-6781-2-git-send-email-dottedmag@dottedmag.net>
Fcc: MAIL_DIR/sent
--text follows this line--
"Mikhail Gusarov" <dottedmag@dottedmag.net> writes:

> Signed-off-by: Mikhail Gusarov <dottedmag at dottedmag.net>
> ---
>  lib/message.cc |    2 ++
>  1 files changed, 2 insertions(+), 0 deletions(-)
>
> diff --git a/lib/message.cc b/lib/message.cc
> index 72c350f..a4b090b 100644
> --- a/lib/message.cc
> +++ b/lib/message.cc
> @@ -21,6 +21,8 @@
>  #include "notmuch-private.h"
>  #include "database-private.h"
>  
> +#include <stdint.h>
> +
>  #include <gmime/gmime.h>
>  
>  #include <xapian.h>
> -- 
> 1.6.3.3
>
>

-- 
EMAIL SIGNATURE
EOF
notmuch_dir_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "if message-cite-reply-position is 'below, position citation above signature"
test_emacs "(let ((message-cite-reply-position 'below)
             (message-signature \"EMAIL SIGNATURE\"))
         (notmuch-mua-new-reply \"id:${ID4}\")
         (test-visible-output \"OUTPUT.raw\"))"
cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Mikhail Gusarov <dottedmag@dottedmag.net>
Subject: Re: [notmuch] [PATCH 2/2] Include <stdint.h> to get uint32_t in C++ file with gcc 4.4
In-Reply-To: <1258471718-6781-2-git-send-email-dottedmag@dottedmag.net>
Fcc: MAIL_DIR/sent
--text follows this line--
"Mikhail Gusarov" <dottedmag@dottedmag.net> writes:

> Signed-off-by: Mikhail Gusarov <dottedmag at dottedmag.net>
> ---
>  lib/message.cc |    2 ++
>  1 files changed, 2 insertions(+), 0 deletions(-)
>
> diff --git a/lib/message.cc b/lib/message.cc
> index 72c350f..a4b090b 100644
> --- a/lib/message.cc
> +++ b/lib/message.cc
> @@ -21,6 +21,8 @@
>  #include "notmuch-private.h"
>  #include "database-private.h"
>  
> +#include <stdint.h>
> +
>  #include <gmime/gmime.h>
>  
>  #include <xapian.h>
> -- 
> 1.6.3.3
>
>

-- 
EMAIL SIGNATURE
EOF
notmuch_dir_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_done
