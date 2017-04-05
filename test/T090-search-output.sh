#!/usr/bin/env bash
test_description='various settings for "notmuch search --output="'
. ./test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "--output=threads"
notmuch search --output=threads '*' | sed -e s/thread:.*/thread:THREADID/ >OUTPUT
cat <<EOF >EXPECTED
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
thread:THREADID
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=threads --format=json"
notmuch search --format=json --output=threads '*' | sed -e s/\".*\"/\"THREADID\"/ >OUTPUT
cat <<EOF >EXPECTED
["THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID",
"THREADID"]
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--output=messages"
notmuch search --output=messages '*' >OUTPUT
cat <<EOF >EXPECTED
id:4EFC743A.3060609@april.org
id:877h1wv7mg.fsf@inf-8657.int-evry.fr
id:1258544095-16616-1-git-send-email-chris@chris-wilson.co.uk
id:877htoqdbo.fsf@yoom.home.cworth.org
id:878we4qdqf.fsf@yoom.home.cworth.org
id:87aaykqe24.fsf@yoom.home.cworth.org
id:87bpj0qeng.fsf@yoom.home.cworth.org
id:87fx8cqf8v.fsf@yoom.home.cworth.org
id:87hbssqfix.fsf@yoom.home.cworth.org
id:87iqd8qgiz.fsf@yoom.home.cworth.org
id:87k4xoqgnl.fsf@yoom.home.cworth.org
id:87ocn0qh6d.fsf@yoom.home.cworth.org
id:87pr7gqidx.fsf@yoom.home.cworth.org
id:867hto2p0t.fsf@fortitudo.i-did-not-set--mail-host-address--so-tickle-me
id:1258532999-9316-1-git-send-email-keithp@keithp.com
id:86aayk2rbj.fsf@fortitudo.i-did-not-set--mail-host-address--so-tickle-me
id:86d43g2w3y.fsf@fortitudo.i-did-not-set--mail-host-address--so-tickle-me
id:ddd65cda0911172214t60d22b63hcfeb5a19ab54a39b@mail.gmail.com
id:86einw2xof.fsf@fortitudo.i-did-not-set--mail-host-address--so-tickle-me
id:736613.51770.qm@web113505.mail.gq1.yahoo.com
id:1258520223-15328-1-git-send-email-jan@ryngle.com
id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com
id:1258510940-7018-1-git-send-email-stewart@flamingspork.com
id:yunzl6kd1w0.fsf@aiko.keithp.com
id:yun1vjwegii.fsf@aiko.keithp.com
id:yun3a4cegoa.fsf@aiko.keithp.com
id:1258509400-32511-1-git-send-email-stewart@flamingspork.com
id:1258506353-20352-1-git-send-email-stewart@flamingspork.com
id:20091118010116.GC25380@dottiness.seas.harvard.edu
id:20091118005829.GB25380@dottiness.seas.harvard.edu
id:20091118005040.GA25380@dottiness.seas.harvard.edu
id:cf0c4d610911171623q3e27a0adx802e47039b57604b@mail.gmail.com
id:1258500222-32066-1-git-send-email-ingmar@exherbo.org
id:20091117232137.GA7669@griffis1.net
id:20091118002059.067214ed@hikari
id:1258498485-sup-142@elly
id:f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com
id:f35dbb950911171435ieecd458o853c873e35f4be95@mail.gmail.com
id:1258496327-12086-1-git-send-email-jan@ryngle.com
id:1258493565-13508-1-git-send-email-keithp@keithp.com
id:yunaayketfm.fsf@aiko.keithp.com
id:yunbpj0etua.fsf@aiko.keithp.com
id:1258491078-29658-1-git-send-email-dottedmag@dottedmag.net
id:87fx8can9z.fsf@vertex.dottedmag
id:20091117203301.GV3165@dottiness.seas.harvard.edu
id:87lji4lx9v.fsf@yoom.home.cworth.org
id:cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com
id:87iqd9rn3l.fsf@vertex.dottedmag
id:20091117190054.GU3165@dottiness.seas.harvard.edu
id:87lji5cbwo.fsf@yoom.home.cworth.org
id:1258471718-6781-2-git-send-email-dottedmag@dottedmag.net
id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=messages --duplicate=1"
notmuch search --output=messages --duplicate=1 '*' >OUTPUT
# reuse same EXPECTED as above
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=messages --duplicate=2"
notmuch search --output=messages --duplicate=2 '*' >OUTPUT
cat <<EOF >EXPECTED
id:20091117232137.GA7669@griffis1.net
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=messages --duplicate=3"
notmuch search --output=messages --duplicate=3 '*' >OUTPUT
cat <<EOF >EXPECTED
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=messages --format=json"
notmuch search --format=json --output=messages '*' >OUTPUT
cat <<EOF >EXPECTED
["4EFC743A.3060609@april.org",
"877h1wv7mg.fsf@inf-8657.int-evry.fr",
"1258544095-16616-1-git-send-email-chris@chris-wilson.co.uk",
"877htoqdbo.fsf@yoom.home.cworth.org",
"878we4qdqf.fsf@yoom.home.cworth.org",
"87aaykqe24.fsf@yoom.home.cworth.org",
"87bpj0qeng.fsf@yoom.home.cworth.org",
"87fx8cqf8v.fsf@yoom.home.cworth.org",
"87hbssqfix.fsf@yoom.home.cworth.org",
"87iqd8qgiz.fsf@yoom.home.cworth.org",
"87k4xoqgnl.fsf@yoom.home.cworth.org",
"87ocn0qh6d.fsf@yoom.home.cworth.org",
"87pr7gqidx.fsf@yoom.home.cworth.org",
"867hto2p0t.fsf@fortitudo.i-did-not-set--mail-host-address--so-tickle-me",
"1258532999-9316-1-git-send-email-keithp@keithp.com",
"86aayk2rbj.fsf@fortitudo.i-did-not-set--mail-host-address--so-tickle-me",
"86d43g2w3y.fsf@fortitudo.i-did-not-set--mail-host-address--so-tickle-me",
"ddd65cda0911172214t60d22b63hcfeb5a19ab54a39b@mail.gmail.com",
"86einw2xof.fsf@fortitudo.i-did-not-set--mail-host-address--so-tickle-me",
"736613.51770.qm@web113505.mail.gq1.yahoo.com",
"1258520223-15328-1-git-send-email-jan@ryngle.com",
"ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com",
"1258510940-7018-1-git-send-email-stewart@flamingspork.com",
"yunzl6kd1w0.fsf@aiko.keithp.com",
"yun1vjwegii.fsf@aiko.keithp.com",
"yun3a4cegoa.fsf@aiko.keithp.com",
"1258509400-32511-1-git-send-email-stewart@flamingspork.com",
"1258506353-20352-1-git-send-email-stewart@flamingspork.com",
"20091118010116.GC25380@dottiness.seas.harvard.edu",
"20091118005829.GB25380@dottiness.seas.harvard.edu",
"20091118005040.GA25380@dottiness.seas.harvard.edu",
"cf0c4d610911171623q3e27a0adx802e47039b57604b@mail.gmail.com",
"1258500222-32066-1-git-send-email-ingmar@exherbo.org",
"20091117232137.GA7669@griffis1.net",
"20091118002059.067214ed@hikari",
"1258498485-sup-142@elly",
"f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com",
"f35dbb950911171435ieecd458o853c873e35f4be95@mail.gmail.com",
"1258496327-12086-1-git-send-email-jan@ryngle.com",
"1258493565-13508-1-git-send-email-keithp@keithp.com",
"yunaayketfm.fsf@aiko.keithp.com",
"yunbpj0etua.fsf@aiko.keithp.com",
"1258491078-29658-1-git-send-email-dottedmag@dottedmag.net",
"87fx8can9z.fsf@vertex.dottedmag",
"20091117203301.GV3165@dottiness.seas.harvard.edu",
"87lji4lx9v.fsf@yoom.home.cworth.org",
"cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com",
"87iqd9rn3l.fsf@vertex.dottedmag",
"20091117190054.GU3165@dottiness.seas.harvard.edu",
"87lji5cbwo.fsf@yoom.home.cworth.org",
"1258471718-6781-2-git-send-email-dottedmag@dottedmag.net",
"1258471718-6781-1-git-send-email-dottedmag@dottedmag.net"]
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=messages --format=json --duplicate=1"
notmuch search --output=messages --format=json --duplicate=1 '*' >OUTPUT
# reuse same EXPECTED as above
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=messages --format=json --duplicate=2"
notmuch search --output=messages --format=json --duplicate=2 '*' >OUTPUT
cat <<EOF >EXPECTED
["20091117232137.GA7669@griffis1.net"]
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=messages --format=json --duplicate=3"
notmuch search --output=messages --format=json --duplicate=3 '*' >OUTPUT
cat <<EOF >EXPECTED
[]
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=files"
notmuch search --output=files '*' | notmuch_search_files_sanitize | sort >OUTPUT
cat <<EOF >EXPECTED
MAIL_DIR/01:2,
MAIL_DIR/02:2,
MAIL_DIR/bar/17:2,
MAIL_DIR/bar/18:2,
MAIL_DIR/bar/baz/05:2,
MAIL_DIR/bar/baz/23:2,
MAIL_DIR/bar/baz/24:2,
MAIL_DIR/bar/baz/cur/25:2,
MAIL_DIR/bar/baz/cur/26:2,
MAIL_DIR/bar/baz/new/27:2,
MAIL_DIR/bar/baz/new/28:2,
MAIL_DIR/bar/cur/19:2,
MAIL_DIR/bar/cur/20:2,
MAIL_DIR/bar/new/21:2,
MAIL_DIR/bar/new/22:2,
MAIL_DIR/cur/29:2,
MAIL_DIR/cur/30:2,
MAIL_DIR/cur/31:2,
MAIL_DIR/cur/32:2,
MAIL_DIR/cur/33:2,
MAIL_DIR/cur/34:2,
MAIL_DIR/cur/35:2,
MAIL_DIR/cur/36:2,
MAIL_DIR/cur/37:2,
MAIL_DIR/cur/38:2,
MAIL_DIR/cur/39:2,
MAIL_DIR/cur/40:2,
MAIL_DIR/cur/41:2,
MAIL_DIR/cur/42:2,
MAIL_DIR/cur/43:2,
MAIL_DIR/cur/44:2,
MAIL_DIR/cur/45:2,
MAIL_DIR/cur/46:2,
MAIL_DIR/cur/47:2,
MAIL_DIR/cur/48:2,
MAIL_DIR/cur/49:2,
MAIL_DIR/cur/50:2,
MAIL_DIR/cur/51:2,
MAIL_DIR/cur/52:2,
MAIL_DIR/cur/53:2,
MAIL_DIR/foo/06:2,
MAIL_DIR/foo/baz/11:2,
MAIL_DIR/foo/baz/12:2,
MAIL_DIR/foo/baz/cur/13:2,
MAIL_DIR/foo/baz/cur/14:2,
MAIL_DIR/foo/baz/new/15:2,
MAIL_DIR/foo/baz/new/16:2,
MAIL_DIR/foo/cur/07:2,
MAIL_DIR/foo/cur/08:2,
MAIL_DIR/foo/new/03:2,
MAIL_DIR/foo/new/09:2,
MAIL_DIR/foo/new/10:2,
MAIL_DIR/new/04:2,
EOF
test_expect_equal_file EXPECTED OUTPUT

dup1=$(notmuch search --output=files id:20091117232137.GA7669@griffis1.net | head -n 1 | sed -e "s,$MAIL_DIR,MAIL_DIR,")
dup2=$(notmuch search --output=files id:20091117232137.GA7669@griffis1.net | tail -n 1 | sed -e "s,$MAIL_DIR,MAIL_DIR,")

test_begin_subtest "--output=files --duplicate=1"
notmuch search --output=files --duplicate=1 '*' | notmuch_search_files_sanitize | sort >OUTPUT
cat <<EOF | sort >EXPECTED
$dup1
MAIL_DIR/cur/52:2,
MAIL_DIR/cur/53:2,
MAIL_DIR/cur/50:2,
MAIL_DIR/cur/49:2,
MAIL_DIR/cur/48:2,
MAIL_DIR/cur/47:2,
MAIL_DIR/cur/46:2,
MAIL_DIR/cur/45:2,
MAIL_DIR/cur/44:2,
MAIL_DIR/cur/43:2,
MAIL_DIR/cur/42:2,
MAIL_DIR/cur/41:2,
MAIL_DIR/cur/40:2,
MAIL_DIR/cur/39:2,
MAIL_DIR/cur/38:2,
MAIL_DIR/cur/37:2,
MAIL_DIR/cur/36:2,
MAIL_DIR/cur/35:2,
MAIL_DIR/cur/34:2,
MAIL_DIR/cur/33:2,
MAIL_DIR/cur/32:2,
MAIL_DIR/cur/31:2,
MAIL_DIR/cur/30:2,
MAIL_DIR/cur/29:2,
MAIL_DIR/bar/baz/new/28:2,
MAIL_DIR/bar/baz/new/27:2,
MAIL_DIR/bar/baz/cur/26:2,
MAIL_DIR/bar/baz/cur/25:2,
MAIL_DIR/bar/baz/24:2,
MAIL_DIR/bar/baz/23:2,
MAIL_DIR/bar/new/22:2,
MAIL_DIR/bar/new/21:2,
MAIL_DIR/bar/cur/19:2,
MAIL_DIR/bar/cur/20:2,
MAIL_DIR/bar/17:2,
MAIL_DIR/foo/baz/new/16:2,
MAIL_DIR/foo/baz/new/15:2,
MAIL_DIR/foo/baz/cur/14:2,
MAIL_DIR/foo/baz/cur/13:2,
MAIL_DIR/foo/baz/12:2,
MAIL_DIR/foo/baz/11:2,
MAIL_DIR/foo/new/10:2,
MAIL_DIR/foo/new/09:2,
MAIL_DIR/foo/cur/08:2,
MAIL_DIR/foo/06:2,
MAIL_DIR/bar/baz/05:2,
MAIL_DIR/new/04:2,
MAIL_DIR/foo/new/03:2,
MAIL_DIR/foo/cur/07:2,
MAIL_DIR/02:2,
MAIL_DIR/01:2,
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=files --format=json"
notmuch search --format=json --output=files '*' | notmuch_search_files_sanitize \
    | test_sort_json >OUTPUT
cat <<EOF | test_sort_json >EXPECTED
["MAIL_DIR/cur/52:2,",
"MAIL_DIR/cur/53:2,",
"MAIL_DIR/cur/50:2,",
"MAIL_DIR/cur/49:2,",
"MAIL_DIR/cur/48:2,",
"MAIL_DIR/cur/47:2,",
"MAIL_DIR/cur/46:2,",
"MAIL_DIR/cur/45:2,",
"MAIL_DIR/cur/44:2,",
"MAIL_DIR/cur/43:2,",
"MAIL_DIR/cur/42:2,",
"MAIL_DIR/cur/41:2,",
"MAIL_DIR/cur/40:2,",
"MAIL_DIR/cur/39:2,",
"MAIL_DIR/cur/38:2,",
"MAIL_DIR/cur/37:2,",
"MAIL_DIR/cur/36:2,",
"MAIL_DIR/cur/35:2,",
"MAIL_DIR/cur/34:2,",
"MAIL_DIR/cur/33:2,",
"MAIL_DIR/cur/32:2,",
"MAIL_DIR/cur/31:2,",
"MAIL_DIR/cur/30:2,",
"MAIL_DIR/cur/29:2,",
"MAIL_DIR/bar/baz/new/28:2,",
"MAIL_DIR/bar/baz/new/27:2,",
"MAIL_DIR/bar/baz/cur/26:2,",
"MAIL_DIR/bar/baz/cur/25:2,",
"MAIL_DIR/bar/baz/24:2,",
"MAIL_DIR/bar/baz/23:2,",
"MAIL_DIR/bar/new/22:2,",
"MAIL_DIR/bar/new/21:2,",
"MAIL_DIR/bar/cur/19:2,",
"MAIL_DIR/bar/18:2,",
"MAIL_DIR/cur/51:2,",
"MAIL_DIR/bar/cur/20:2,",
"MAIL_DIR/bar/17:2,",
"MAIL_DIR/foo/baz/new/16:2,",
"MAIL_DIR/foo/baz/new/15:2,",
"MAIL_DIR/foo/baz/cur/14:2,",
"MAIL_DIR/foo/baz/cur/13:2,",
"MAIL_DIR/foo/baz/12:2,",
"MAIL_DIR/foo/baz/11:2,",
"MAIL_DIR/foo/new/10:2,",
"MAIL_DIR/foo/new/09:2,",
"MAIL_DIR/foo/cur/08:2,",
"MAIL_DIR/foo/06:2,",
"MAIL_DIR/bar/baz/05:2,",
"MAIL_DIR/new/04:2,",
"MAIL_DIR/foo/new/03:2,",
"MAIL_DIR/foo/cur/07:2,",
"MAIL_DIR/02:2,",
"MAIL_DIR/01:2,"]
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=files --format=json --duplicate=2"
notmuch search --format=json --output=files --duplicate=2 '*' | notmuch_search_files_sanitize >OUTPUT
cat <<EOF >EXPECTED
["$dup2"]
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=tags"
notmuch search --output=tags '*' >OUTPUT
cat <<EOF >EXPECTED
attachment
inbox
signed
unread
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--output=tags --format=json"
notmuch search --format=json --output=tags '*' >OUTPUT
cat <<EOF >EXPECTED
["attachment",
"inbox",
"signed",
"unread"]
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "sanitize output for quoted-printable line-breaks in author and subject"
add_message "[subject]='two =?ISO-8859-1?Q?line=0A_subject?=
	headers'"
notmuch search id:"$gen_msg_id" | notmuch_search_sanitize >OUTPUT
cat <<EOF >EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; two line  subject headers (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search for non-existent message prints nothing"
notmuch search "no-message-matches-this" > OUTPUT
echo -n >EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search --format=json for non-existent message prints proper empty json"
notmuch search --format=json "no-message-matches-this" > OUTPUT
echo "[]" >EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_done
