#!/usr/bin/env bash
test_description="message id parsing"

. $(dirname "$0")/test-lib.sh || exit 1

test_begin_subtest "good message ids"
${TEST_DIRECTORY}/message-id-parse <<EOF >OUTPUT
<018b1a8f2d1df62e804ce88b65401304832dfbbf.1346614915.git.jani@nikula.org>
<1530507300.raoomurnbf.astroid@strange.none>
<1258787708-21121-2-git-send-email-keithp@keithp.com>
EOF
cat <<EOF >EXPECTED
GOOD: 018b1a8f2d1df62e804ce88b65401304832dfbbf.1346614915.git.jani@nikula.org
GOOD: 1530507300.raoomurnbf.astroid@strange.none
GOOD: 1258787708-21121-2-git-send-email-keithp@keithp.com
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "leading and trailing space is OK"
${TEST_DIRECTORY}/message-id-parse <<EOF >OUTPUT
   <018b1a8f2d1df62e804ce88b65401304832dfbbf.1346614915.git.jani@nikula.org>
<1530507300.raoomurnbf.astroid@strange.none>    
    <1258787708-21121-2-git-send-email-keithp@keithp.com>
EOF
cat <<EOF >EXPECTED
GOOD: 018b1a8f2d1df62e804ce88b65401304832dfbbf.1346614915.git.jani@nikula.org
GOOD: 1530507300.raoomurnbf.astroid@strange.none
GOOD: 1258787708-21121-2-git-send-email-keithp@keithp.com
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "<> delimeters are required"
${TEST_DIRECTORY}/message-id-parse <<EOF >OUTPUT
018b1a8f2d1df62e804ce88b65401304832dfbbf.1346614915.git.jani@nikula.org>
<1530507300.raoomurnbf.astroid@strange.none
1258787708-21121-2-git-send-email-keithp@keithp.com
EOF
cat <<EOF >EXPECTED
BAD: 018b1a8f2d1df62e804ce88b65401304832dfbbf.1346614915.git.jani@nikula.org>
BAD: <1530507300.raoomurnbf.astroid@strange.none
BAD: 1258787708-21121-2-git-send-email-keithp@keithp.com
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "embedded whitespace is forbidden"
${TEST_DIRECTORY}/message-id-parse <<EOF >OUTPUT
<018b1a8f2d1df62e804ce88b65401304832dfbbf.1346614915 .git.jani@nikula.org>
<1530507300.raoomurnbf.astroid	@strange.none>
<1258787708-21121-2-git-send-email-keithp@keithp.com>
EOF
cat <<EOF >EXPECTED
BAD: <018b1a8f2d1df62e804ce88b65401304832dfbbf.1346614915 .git.jani@nikula.org>
BAD: <1530507300.raoomurnbf.astroid	@strange.none>
BAD: <1258787708-21121-2-git-send-email-keithp@keithp.com>
EOF
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "folded real life bad In-Reply-To values"
${TEST_DIRECTORY}/message-id-parse <<EOF >OUTPUT
<22597.31869.380767.339702@chiark.greenend.org.uk> (Ian Jackson's message of "Mon, 5 Dec 2016 14:41:01 +0000")
<20170625141242.loaalhis2eodo66n@gaara.hadrons.org>  <149719990964.27883.13021127452105787770.reportbug@seneca.home.org>
Your message of Tue, 09 Dec 2014 13:21:11 +0100. <1900758.CgLNVPbY9N@liber>
EOF
cat <<EOF >EXPECTED
BAD: <22597.31869.380767.339702@chiark.greenend.org.uk> (Ian Jackson's message of "Mon, 5 Dec 2016 14:41:01 +0000")
BAD: <20170625141242.loaalhis2eodo66n@gaara.hadrons.org>  <149719990964.27883.13021127452105787770.reportbug@seneca.home.org>
BAD: Your message of Tue, 09 Dec 2014 13:21:11 +0100. <1900758.CgLNVPbY9N@liber>
EOF
test_expect_equal_file EXPECTED OUTPUT


test_done
