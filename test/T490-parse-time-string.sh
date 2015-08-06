#!/usr/bin/env bash
test_description="date/time parser module"
. ./test-lib.sh || exit 1

# Sanity/smoke tests for the date/time parser independent of notmuch

_date ()
{
    date -d "$*" +%s
}

_parse_time ()
{
    ${TEST_DIRECTORY}/parse-time --format=%s "$*"
}

test_begin_subtest "date(1) default format without TZ code"
test_expect_equal "$(_parse_time Fri Aug 3 23:06:06 2012)" "$(_date Fri Aug 3 23:06:06 2012)"

test_begin_subtest "date(1) --rfc-2822 format"
test_expect_equal "$(_parse_time Fri, 03 Aug 2012 23:07:46 +0100)" "$(_date Fri, 03 Aug 2012 23:07:46 +0100)"

test_begin_subtest "date(1) --rfc=3339=seconds format"
test_expect_equal "$(_parse_time 2012-08-03 23:09:37+03:00)" "$(_date 2012-08-03 23:09:37+03:00)"

test_begin_subtest "Date parser tests"
REFERENCE=$(_date Tue Jan 11 12:13:14 +0000 2011)
cat <<EOF > INPUT
now          ==> Tue Jan 11 12:13:14 +0000 2011
2010-1-1     ==> ERROR: DATEFORMAT
Jan 2        ==> Sun Jan 02 12:13:14 +0000 2011
Mon          ==> Mon Jan 10 12:13:14 +0000 2011
last Friday  ==> ERROR: FORMAT
2 hours ago  ==> Tue Jan 11 10:13:14 +0000 2011
last month   ==> Sat Dec 11 12:13:14 +0000 2010
month ago    ==> Sat Dec 11 12:13:14 +0000 2010
two mo       ==> Thu Nov 11 12:13:14 +0000 2010
3M           ==> Mon Oct 11 12:13:14 +0000 2010
4-mont       ==> Sat Sep 11 12:13:14 +0000 2010
5m           ==> Tue Jan 11 12:08:14 +0000 2011
dozen mi     ==> Tue Jan 11 12:01:14 +0000 2011
8am          ==> Tue Jan 11 08:00:00 +0000 2011
monday       ==> Mon Jan 10 12:13:14 +0000 2011
yesterday    ==> Mon Jan 10 12:13:14 +0000 2011
tomorrow     ==> ERROR: KEYWORD
             ==> Tue Jan 11 12:13:14 +0000 2011 # empty string is reference time

Aug 3 23:06:06 2012             ==> Fri Aug 03 23:06:06 +0000 2012 # date(1) default format without TZ code
Fri, 03 Aug 2012 23:07:46 +0100 ==> Fri Aug 03 22:07:46 +0000 2012 # rfc-2822
2012-08-03 23:09:37+03:00       ==> Fri Aug 03 20:09:37 +0000 2012 # rfc-3339 seconds

10:30:40     ==> Tue Jan 11 10:30:40 +0000 2011
10:30:40     ==^> Tue Jan 11 10:30:40 +0000 2011
10:30:40     ==^^> Tue Jan 11 10:30:40 +0000 2011
10:30:40     ==_> Tue Jan 11 10:30:40 +0000 2011

10s           ==> Tue Jan 11 12:13:04 +0000 2011
19701223s     ==> Fri May 28 11:39:31 +0000 2010
19701223      ==> Wed Dec 23 12:13:14 +0000 1970

19701223 +0100 ==> Wed Dec 23 12:13:14 +0000 1970 # Timezone is ignored without an error

today ==^^> Wed Jan 12 00:00:00 +0000 2011
today ==^> Tue Jan 11 23:59:59 +0000 2011
today ==_> Tue Jan 11 00:00:00 +0000 2011

this week ==^^> Sun Jan 16 00:00:00 +0000 2011
this week ==^> Sat Jan 15 23:59:59 +0000 2011
this week ==_> Sun Jan 09 00:00:00 +0000 2011

two months ago ==> Thu Nov 11 12:13:14 +0000 2010
two months ==> Thu Nov 11 12:13:14 +0000 2010

@1348569850 ==> Tue Sep 25 10:44:10 +0000 2012
@10 ==> Thu Jan 01 00:00:10 +0000 1970
EOF

${TEST_DIRECTORY}/parse-time --ref=${REFERENCE} < INPUT > OUTPUT
test_expect_equal_file INPUT OUTPUT

test_begin_subtest "Second rounding tests"
REFERENCE=$(_date Tue Jan 11 12:13:14 +0000 2011)
cat <<EOF > INPUT
9:15         ==> Tue Jan 11 09:15:14 +0000 2011
12:34        ==> Tue Jan 11 12:34:14 +0000 2011
10:30        ==> Tue Jan 11 10:30:14 +0000 2011
10:30        ==^> Tue Jan 11 10:30:59 +0000 2011
10:30        ==^^> Tue Jan 11 10:31:00 +0000 2011
10:30        ==_> Tue Jan 11 10:30:00 +0000 2011
EOF
${TEST_DIRECTORY}/parse-time --ref=${REFERENCE} < INPUT > OUTPUT
test_expect_equal_file INPUT OUTPUT

test_done
