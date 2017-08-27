#!/usr/bin/env bash

test_description="emacs interface"
. ./test-lib.sh || exit 1

EXPECTED=$TEST_DIRECTORY/emacs.expected-output

add_email_corpus

# syntax errors in test-lib.el cause mysterious failures
test_begin_subtest "Syntax of emacs test library"
test_expect_success "${TEST_EMACS} -Q --batch --load $TEST_DIRECTORY/test-lib.el"

test_begin_subtest "Basic notmuch-hello view in emacs"
test_emacs '(notmuch-hello)
	    (test-output)'
test_expect_equal_file $EXPECTED/notmuch-hello OUTPUT

test_begin_subtest "Saved search with 0 results"
test_emacs '(let ((notmuch-show-empty-saved-searches t)
		  (notmuch-saved-searches
		   '\''(("inbox" . "tag:inbox")
			("unread" . "tag:unread")
			("empty" . "tag:doesnotexist"))))
	      (notmuch-hello)
	      (test-output))'
test_expect_equal_file $EXPECTED/notmuch-hello-with-empty OUTPUT

test_begin_subtest "No saved searches displayed (all with 0 results)"
test_emacs '(let ((notmuch-saved-searches
		   '\''(("empty" . "tag:doesnotexist"))))
	      (notmuch-hello)
	      (test-output))'
test_expect_equal_file $EXPECTED/notmuch-hello-no-saved-searches OUTPUT

test_begin_subtest "Basic notmuch-search view in emacs"
test_emacs '(notmuch-search "tag:inbox")
	    (notmuch-test-wait)
	    (test-output)'
test_expect_equal_file $EXPECTED/notmuch-search-tag-inbox OUTPUT

test_begin_subtest "Incremental parsing of search results"
test_emacs "(ad-enable-advice 'notmuch-search-process-filter 'around 'pessimal)
	    (ad-activate 'notmuch-search-process-filter)
	    (notmuch-search \"tag:inbox\")
	    (notmuch-test-wait)
	    (ad-disable-advice 'notmuch-search-process-filter 'around 'pessimal)
	    (ad-activate 'notmuch-search-process-filter)
	    (test-output)"
test_expect_equal_file $EXPECTED/notmuch-search-tag-inbox OUTPUT

test_begin_subtest "Navigation of notmuch-hello to search results"
test_emacs '(notmuch-hello)
	    (goto-char (point-min))
	    (re-search-forward "inbox")
	    (widget-button-press (1- (point)))
	    (notmuch-test-wait)
	    (test-output)'
test_expect_equal_file $EXPECTED/notmuch-hello-view-inbox OUTPUT

test_begin_subtest "Basic notmuch-show view in emacs"
maildir_storage_thread=$(notmuch search --output=threads id:20091117190054.GU3165@dottiness.seas.harvard.edu)
test_emacs "(notmuch-show \"$maildir_storage_thread\")
	    (test-output)"
test_expect_equal_file $EXPECTED/notmuch-show-thread-maildir-storage OUTPUT

test_begin_subtest "Basic notmuch-show view in emacs default indentation"
maildir_storage_thread=$(notmuch search --output=threads id:20091117190054.GU3165@dottiness.seas.harvard.edu)
test_emacs "(let ((notmuch-show-indent-messages-width 1))
	      (notmuch-show \"$maildir_storage_thread\")
	      (test-output))"
test_expect_equal_file $EXPECTED/notmuch-show-thread-maildir-storage OUTPUT

test_begin_subtest "Basic notmuch-show view in emacs without indentation"
maildir_storage_thread=$(notmuch search --output=threads id:20091117190054.GU3165@dottiness.seas.harvard.edu)
test_emacs "(let ((notmuch-show-indent-messages-width 0))
	      (notmuch-show \"$maildir_storage_thread\")
	      (test-output))"
test_expect_equal_file $EXPECTED/notmuch-show-thread-maildir-storage-without-indentation OUTPUT

test_begin_subtest "Basic notmuch-show view in emacs with fourfold indentation"
maildir_storage_thread=$(notmuch search --output=threads id:20091117190054.GU3165@dottiness.seas.harvard.edu)
test_emacs "(let ((notmuch-show-indent-messages-width 4))
	      (notmuch-show \"$maildir_storage_thread\")
	      (test-output))"
test_expect_equal_file $EXPECTED/notmuch-show-thread-maildir-storage-with-fourfold-indentation OUTPUT

test_begin_subtest "notmuch-show for message with invalid From"
test_subtest_broken_gmime_3
add_message "[subject]=\"message-with-invalid-from\"" \
	    "[from]=\"\\\"Invalid \\\" From\\\" <test_suite@notmuchmail.org>\""
thread=$(notmuch search --output=threads subject:message-with-invalid-from)
test_emacs "(notmuch-show \"$thread\")
	    (test-output \"OUTPUT.raw\")"
cat <<EOF >EXPECTED
"Invalid " (2001-01-05) (inbox)
Subject: message-with-invalid-from
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: GENERATED_DATE

This is just a test message (#1)
EOF
notmuch_date_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Navigation of notmuch-search to thread view"
test_emacs '(notmuch-search "tag:inbox")
	    (notmuch-test-wait)
	    (goto-char (point-min))
	    (re-search-forward "Working with Maildir")
	    (notmuch-search-show-thread)
	    (notmuch-test-wait)
	    (test-output)'
test_expect_equal_file $EXPECTED/notmuch-show-thread-maildir-storage OUTPUT

test_begin_subtest "Add tag from search view"
os_x_darwin_thread=$(notmuch search --output=threads id:ddd65cda0911171950o4eea4389v86de9525e46052d3@mail.gmail.com)
test_emacs "(notmuch-search \"$os_x_darwin_thread\")
	    (notmuch-test-wait)
	    (execute-kbd-macro \"+tag-from-search-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox tag-from-search-view unread)"

test_begin_subtest "Remove tag from search view"
test_emacs "(notmuch-search \"$os_x_darwin_thread\")
	    (notmuch-test-wait)
	    (execute-kbd-macro \"-tag-from-search-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)"

test_begin_subtest "Add tag (large query)"
# We use a long query to force us into batch mode and use a funny tag
# that requires escaping for batch tagging.
test_emacs "(notmuch-tag (concat \"$os_x_darwin_thread\" \" or \" (mapconcat #'identity (make-list notmuch-tag-argument-limit \"x\") \"-\")) (list \"+tag-from-%-large-query\"))"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox tag-from-%-large-query unread)"
notmuch tag -tag-from-%-large-query $os_x_darwin_thread

test_begin_subtest "notmuch-show: add single tag to single message"
test_emacs "(notmuch-show \"$os_x_darwin_thread\")
	    (execute-kbd-macro \"+tag-from-show-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox tag-from-show-view unread)"

test_begin_subtest "notmuch-show: remove single tag from single message"
test_emacs "(notmuch-show \"$os_x_darwin_thread\")
	    (execute-kbd-macro \"-tag-from-show-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)"

test_begin_subtest "notmuch-show: add multiple tags to single message"
test_emacs "(notmuch-show \"$os_x_darwin_thread\")
	    (execute-kbd-macro \"+tag1-from-show-view +tag2-from-show-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox tag1-from-show-view tag2-from-show-view unread)"

test_begin_subtest "notmuch-show: remove multiple tags from single message"
test_emacs "(notmuch-show \"$os_x_darwin_thread\")
	    (execute-kbd-macro \"-tag1-from-show-view -tag2-from-show-view\")"
output=$(notmuch search $os_x_darwin_thread | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)"

test_begin_subtest "Message with .. in Message-Id:"
add_message [id]=123..456@example '[subject]="Message with .. in Message-Id"'
test_emacs '(notmuch-search "id:\"123..456@example\"")
	    (notmuch-test-wait)
	    (execute-kbd-macro "+search-add")
	    (execute-kbd-macro "+search-remove")
	    (execute-kbd-macro "-search-remove")
	    (notmuch-show "id:\"123..456@example\"")
	    (notmuch-test-wait)
	    (execute-kbd-macro "+show-add")
	    (execute-kbd-macro "+show-remove")
	    (execute-kbd-macro "-show-remove")'
output=$(notmuch search 'id:"123..456@example"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Message with .. in Message-Id (inbox search-add show-add)"

test_begin_subtest "Message with quote in Message-Id:"
add_message '[id]="\"quote\"@example"' '[subject]="Message with quote in Message-Id"'
test_emacs '(notmuch-search "subject:\"Message with quote\"")
	    (notmuch-test-wait)
	    (execute-kbd-macro "+search-add")
            (notmuch-search-show-thread)
	    (notmuch-test-wait)
	    (execute-kbd-macro "+show-add")'
output=$(notmuch search 'id:"""quote""@example"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Message with quote in Message-Id (inbox search-add show-add)"

test_begin_subtest "Sending a message via (fake) SMTP"
emacs_deliver_message \
    'Testing message sent via SMTP' \
    'This is a test that messages are sent via SMTP' \
    '(message-goto-to)
     (kill-whole-line)
     (insert "To: user@example.com\n")'
sed \
    -e s',^Message-ID: <.*>$,Message-ID: <XXX>,' \
    -e s',^\(Content-Type: text/plain\); charset=us-ascii$,\1,' < sent_message >OUTPUT
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: user@example.com
Subject: Testing message sent via SMTP
Date: 01 Jan 2000 12:00:00 -0000
Message-ID: <XXX>
MIME-Version: 1.0
Content-Type: text/plain

This is a test that messages are sent via SMTP
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Folding a long header when sending via (fake) SMTP"
long_subject="This is a long subject `echo {1..1000}`"
emacs_deliver_message \
    "${long_subject}" \
    'This is a test that long headers are folded when messages are sent via SMTP' \
    '(message-goto-to)
     (kill-whole-line)
     (insert "To: user@example.com\n")'
sed \
    -e s',^Message-ID: <.*>$,Message-ID: <XXX>,' \
    -e s',^\(Content-Type: text/plain\); charset=us-ascii$,\1,' < sent_message >OUTPUT
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: user@example.com
Subject: This is a long subject 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43
 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93
 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131
 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149
 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167
 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185
 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203
 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221
 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257
 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275
 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293
 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311
 312 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 329
 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347
 348 349 350 351 352 353 354 355 356 357 358 359 360 361 362 363 364 365
 366 367 368 369 370 371 372 373 374 375 376 377 378 379 380 381 382 383
 384 385 386 387 388 389 390 391 392 393 394 395 396 397 398 399 400 401
 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 418 419
 420 421 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437
 438 439 440 441 442 443 444 445 446 447 448 449 450 451 452 453 454 455
 456 457 458 459 460 461 462 463 464 465 466 467 468 469 470 471 472 473
 474 475 476 477 478 479 480 481 482 483 484 485 486 487 488 489 490 491
 492 493 494 495 496 497 498 499 500 501 502 503 504 505 506 507 508 509
 510 511 512 513 514 515 516 517 518 519 520 521 522 523 524 525 526 527
 528 529 530 531 532 533 534 535 536 537 538 539 540 541 542 543 544 545
 546 547 548 549 550 551 552 553 554 555 556 557 558 559 560 561 562 563
 564 565 566 567 568 569 570 571 572 573 574 575 576 577 578 579 580 581
 582 583 584 585 586 587 588 589 590 591 592 593 594 595 596 597 598 599
 600 601 602 603 604 605 606 607 608 609 610 611 612 613 614 615 616 617
 618 619 620 621 622 623 624 625 626 627 628 629 630 631 632 633 634 635
 636 637 638 639 640 641 642 643 644 645 646 647 648 649 650 651 652 653
 654 655 656 657 658 659 660 661 662 663 664 665 666 667 668 669 670 671
 672 673 674 675 676 677 678 679 680 681 682 683 684 685 686 687 688 689
 690 691 692 693 694 695 696 697 698 699 700 701 702 703 704 705 706 707
 708 709 710 711 712 713 714 715 716 717 718 719 720 721 722 723 724 725
 726 727 728 729 730 731 732 733 734 735 736 737 738 739 740 741 742 743
 744 745 746 747 748 749 750 751 752 753 754 755 756 757 758 759 760 761
 762 763 764 765 766 767 768 769 770 771 772 773 774 775 776 777 778 779
 780 781 782 783 784 785 786 787 788 789 790 791 792 793 794 795 796 797
 798 799 800 801 802 803 804 805 806 807 808 809 810 811 812 813 814 815
 816 817 818 819 820 821 822 823 824 825 826 827 828 829 830 831 832 833
 834 835 836 837 838 839 840 841 842 843 844 845 846 847 848 849 850 851
 852 853 854 855 856 857 858 859 860 861 862 863 864 865 866 867 868 869
 870 871 872 873 874 875 876 877 878 879 880 881 882 883 884 885 886 887
 888 889 890 891 892 893 894 895 896 897 898 899 900 901 902 903 904 905
 906 907 908 909 910 911 912 913 914 915 916 917 918 919 920 921 922 923
 924 925 926 927 928 929 930 931 932 933 934 935 936 937 938 939 940 941
 942 943 944 945 946 947 948 949 950 951 952 953 954 955 956 957 958 959
 960 961 962 963 964 965 966 967 968 969 970 971 972 973 974 975 976 977
 978 979 980 981 982 983 984 985 986 987 988 989 990 991 992 993 994 995
 996 997 998 999 1000
Date: 01 Jan 2000 12:00:00 -0000
Message-ID: <XXX>
MIME-Version: 1.0
Content-Type: text/plain

This is a test that long headers are folded when messages are sent via SMTP
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Verify that sent messages are saved/searchable (via FCC)"
notmuch new > /dev/null
output=$(notmuch search 'subject:"testing message sent via SMTP"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; Testing message sent via SMTP (inbox)"

test_begin_subtest "notmuch-fcc-dirs set to nil"
test_emacs "(let ((notmuch-fcc-dirs nil))
	      (notmuch-mua-mail)
	      (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: 
Subject: 
--text follows this line--
EOF
test_expect_equal_file EXPECTED OUTPUT

# Make another FCC maildir specific for the next test
mkdir -p mail/sent-string/cur
mkdir -p mail/sent-string/new
mkdir -p mail/sent-string/tmp

test_begin_subtest "notmuch-fcc-dirs set to a string"
test_emacs "(let ((notmuch-fcc-dirs \"sent-string\"))
	      (notmuch-mua-mail)
	      (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: 
Subject: 
Fcc: ${MAIL_DIR}/sent-string
--text follows this line--
EOF
test_expect_equal_file EXPECTED OUTPUT

# Make more FCC maildirs specific for the next test
mkdir -p mail/sent-list-match/cur
mkdir -p mail/sent-list-match/new
mkdir -p mail/sent-list-match/tmp
mkdir -p mail/failure/cur
mkdir -p mail/failure/new
mkdir -p mail/failure/tmp

test_begin_subtest "notmuch-fcc-dirs set to a list (with match)"
test_emacs "(let ((notmuch-fcc-dirs
		   '((\"notmuchmail.org\" . \"sent-list-match\")
		     (\".*\" . \"failure\"))))
	      (notmuch-mua-mail)
	      (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: 
Subject: 
Fcc: ${MAIL_DIR}/sent-list-match
--text follows this line--
EOF
test_expect_equal_file EXPECTED OUTPUT

# Make another FCC maildir specific for the next test
mkdir -p mail/sent-list-catch-all/cur
mkdir -p mail/sent-list-catch-all/new
mkdir -p mail/sent-list-catch-all/tmp

test_begin_subtest "notmuch-fcc-dirs set to a list (catch-all)"
test_emacs "(let ((notmuch-fcc-dirs
		   '((\"example.com\" . \"failure\")
		     (\".*\" . \"sent-list-catch-all\"))))
	      (notmuch-mua-mail)
	      (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: 
Subject: 
Fcc: ${MAIL_DIR}/sent-list-catch-all
--text follows this line--
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch-fcc-dirs set to a list (no match)"
test_emacs "(let ((notmuch-fcc-dirs
		   '((\"example.com\" . \"failure\")
		     (\"nomatchhere.net\" . \"failure\"))))
	      (notmuch-mua-mail)
	      (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: 
Subject: 
--text follows this line--
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Reply within emacs"
test_emacs '(let ((message-hidden-headers ''()))
	    (notmuch-search "subject:\"testing message sent via SMTP\"")
	    (notmuch-test-wait)
	    (notmuch-search-reply-to-thread)
	    (test-output))'
sed -i -e 's/^In-Reply-To: <.*>$/In-Reply-To: <XXX>/' OUTPUT
sed -i -e 's/^References: <.*>$/References: <XXX>/' OUTPUT
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: user@example.com
Subject: Re: Testing message sent via SMTP
In-Reply-To: <XXX>
Fcc: ${MAIL_DIR}/sent
References: <XXX>
--text follows this line--
Notmuch Test Suite <test_suite@notmuchmail.org> writes:

> This is a test that messages are sent via SMTP
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Reply from alternate address within emacs"
add_message '[from]="Sender <sender@example.com>"' \
	     [to]=test_suite_other@notmuchmail.org

test_emacs "(let ((message-hidden-headers '()))
	    (notmuch-search \"id:\\\"${gen_msg_id}\\\"\")
	    (notmuch-test-wait)
	    (notmuch-search-reply-to-thread)
	    (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite_other@notmuchmail.org>
To: Sender <sender@example.com>
Subject: Re: ${test_subtest_name}
In-Reply-To: <${gen_msg_id}>
Fcc: ${MAIL_DIR}/sent
References: <${gen_msg_id}>
--text follows this line--
Sender <sender@example.com> writes:

> This is just a test message (#${gen_msg_cnt})
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Reply from address in named group list within emacs"
add_message '[from]="Sender <sender@example.com>"' \
            '[to]=group:test_suite@notmuchmail.org,someone@example.com\;' \
             [cc]=test_suite_other@notmuchmail.org

test_emacs "(let ((message-hidden-headers '()))
	    (notmuch-search \"id:\\\"${gen_msg_id}\\\"\")
	    (notmuch-test-wait)
	    (notmuch-search-reply-to-thread)
	    (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Sender <sender@example.com>, someone@example.com
Subject: Re: ${test_subtest_name}
In-Reply-To: <${gen_msg_id}>
Fcc: ${MAIL_DIR}/sent
References: <${gen_msg_id}>
--text follows this line--
Sender <sender@example.com> writes:

> This is just a test message (#${gen_msg_cnt})
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Reply within emacs to a multipart/mixed message"
test_emacs '(let ((message-hidden-headers ''()))
	    (notmuch-show "id:20091118002059.067214ed@hikari")
		(notmuch-show-reply)
		(test-output))'
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Adrian Perez de Castro <aperez@igalia.com>, notmuch@notmuchmail.org
Subject: Re: [notmuch] Introducing myself
In-Reply-To: <20091118002059.067214ed@hikari>
Fcc: ${MAIL_DIR}/sent
References: <20091118002059.067214ed@hikari>
--text follows this line--
Adrian Perez de Castro <aperez@igalia.com> writes:

> Hello to all,
>
> I have just heard about Not Much today in some random Linux-related news
> site (LWN?), my name is Adrian Perez and I work as systems administrator
> (although I can do some code as well :P). I have always thought that the
> ideas behind Sup were great, but after some time using it, I got tired of
> the oddities that it has. I also do not like doing things like having to
> install Ruby just for reading and sorting mails. Some time ago I thought
> about doing something like Not Much and in fact I played a bit with the
> Python+Xapian and the Python+Whoosh combinations, because I find relaxing
> to code things in Python when I am not working and also it is installed
> by default on most distribution. I got to have some mailboxes indexed and
> basic searching working a couple of months ago. Lately I have been very
> busy and had no time for coding, and them... boom! Not Much appears -- and
> it is almost exactly what I was trying to do, but faster. I have been
> playing a bit with Not Much today, and I think it has potential.
>
> Also, I would like to share one idea I had in mind, that you might find
> interesting: One thing I have found very annoying is having to re-tag my
> mail when the indexes get b0rked (it happened a couple of times to me while
> using Sup), so I was planning to mails as read/unread and adding the tags
> not just to the index, but to the mail text itself, e.g. by adding a
> "X-Tags" header field or by reusing the "Keywords" one. This way, the index
> could be totally recreated by re-reading the mail directories, and this
> would also allow to a tools like OfflineIMAP [1] to get the mails into a
> local maildir, tagging and indexing the mails with the e-mail reader and
> then syncing back the messages with the "X-Tags" header to the IMAP server.
> This would allow to use the mail reader from a different computer and still
> have everything tagged finely.
>
> Best regards,
>
>
> ---
> [1] http://software.complete.org/software/projects/show/offlineimap
>
> -- 
> Adrian Perez de Castro <aperez@igalia.com>
> Igalia - Free Software Engineering
> _______________________________________________
> notmuch mailing list
> notmuch@notmuchmail.org
> http://notmuchmail.org/mailman/listinfo/notmuch
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Reply within emacs to a multipart/alternative message"
test_emacs '(let ((message-hidden-headers ''()))
	    (notmuch-show "id:cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com")
		(notmuch-show-reply)
		(test-output))'
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Alex Botero-Lowry <alex.boterolowry@gmail.com>, notmuch@notmuchmail.org
Subject: Re: [notmuch] preliminary FreeBSD support
In-Reply-To: <cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com>
Fcc: ${MAIL_DIR}/sent
References: <cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com>
--text follows this line--
Alex Botero-Lowry <alex.boterolowry@gmail.com> writes:

> I saw the announcement this morning, and was very excited, as I had been
> hoping sup would be turned into a library,
> since I like the concept more than the UI (I'd rather an emacs interface).
>
> I did a preliminary compile which worked out fine, but
> sysconf(_SC_SC_GETPW_R_SIZE_MAX) returns -1 on
> FreeBSD, so notmuch_config_open segfaulted.
>
> Attached is a patch that supplies a default buffer size of 64 in cases where
> -1 is returned.
>
> http://www.opengroup.org/austin/docs/austin_328.txt - seems to indicate this
> is acceptable behavior,
> and http://mail-index.netbsd.org/pkgsrc-bugs/2006/06/07/msg016808.htmlspecifically
> uses 64 as the
> buffer size.
> From e3bc4bbd7b9d0d086816ab5f8f2d6ffea1dd3ea4 Mon Sep 17 00:00:00 2001
> From: Alexander Botero-Lowry <alex.boterolowry@gmail.com>
> Date: Tue, 17 Nov 2009 11:30:39 -0800
> Subject: [PATCH] Deal with situation where sysconf(_SC_GETPW_R_SIZE_MAX) returns -1
>
> ---
>  notmuch-config.c |    2 ++
>  1 files changed, 2 insertions(+), 0 deletions(-)
>
> diff --git a/notmuch-config.c b/notmuch-config.c
> index 248149c..e7220d8 100644
> --- a/notmuch-config.c
> +++ b/notmuch-config.c
> @@ -77,6 +77,7 @@ static char *
>  get_name_from_passwd_file (void *ctx)
>  {
>      long pw_buf_size = sysconf(_SC_GETPW_R_SIZE_MAX);
> +    if (pw_buf_size == -1) pw_buf_size = 64;
>      char *pw_buf = talloc_zero_size (ctx, pw_buf_size);
>      struct passwd passwd, *ignored;
>      char *name;
> @@ -101,6 +102,7 @@ static char *
>  get_username_from_passwd_file (void *ctx)
>  {
>      long pw_buf_size = sysconf(_SC_GETPW_R_SIZE_MAX);
> +    if (pw_buf_size == -1) pw_buf_size = 64;
>      char *pw_buf = talloc_zero_size (ctx, pw_buf_size);
>      struct passwd passwd, *ignored;
>      char *name;
> -- 
> 1.6.5.2
>
> _______________________________________________
> notmuch mailing list
> notmuch@notmuchmail.org
> http://notmuchmail.org/mailman/listinfo/notmuch
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Reply within emacs to an html-only message"
add_message '[content-type]="text/html"' \
	    '[body]="Hi,<br />This is an <b>HTML</b> test message.<br /><br />OK?"'
test_emacs "(let ((message-hidden-headers '()))
	    (notmuch-show \"id:${gen_msg_id}\")
	    (notmuch-show-reply)
	    (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: 
Subject: Re: Reply within emacs to an html-only message
In-Reply-To: <${gen_msg_id}>
Fcc: ${MAIL_DIR}/sent
References: <${gen_msg_id}>
--text follows this line--
Notmuch Test Suite <test_suite@notmuchmail.org> writes:

> Hi,This is an HTML test message.OK?
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Reply within emacs to message from self"
test_subtest_known_broken
add_message '[from]="test_suite@notmuchmail.org"' \
	    '[to]="test_suite@notmuchmail.org"'
test_emacs "(let ((message-hidden-headers '()))
	    (notmuch-show \"id:${gen_msg_id}\")
	    (notmuch-show-reply)
	    (test-output))"
sed -i -e 's/^In-Reply-To: <.*>$/In-Reply-To: <XXX>/' OUTPUT
sed -i -e 's/^References: <.*>$/References: <XXX>/' OUTPUT
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: test_suite@notmuchmail.org
Subject: Re: Reply within emacs to message from self
In-Reply-To: <XXX>
Fcc: ${MAIL_DIR}/sent
References: <XXX>
--text follows this line--
test_suite@notmuchmail.org writes:

> This is just a test message (#7)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Quote MML tags in reply"
message_id='test-emacs-mml-quoting@message.id'
add_message [id]="$message_id" \
	    "[subject]='$test_subtest_name'" \
	    '[body]="<#part disposition=inline>"'
test_emacs "(let ((message-hidden-headers '()))
	      (notmuch-show \"id:$message_id\")
	      (notmuch-show-reply)
	      (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: 
Subject: Re: Quote MML tags in reply
In-Reply-To: <test-emacs-mml-quoting@message.id>
Fcc: ${MAIL_DIR}/sent
References: <test-emacs-mml-quoting@message.id>
--text follows this line--
Notmuch Test Suite <test_suite@notmuchmail.org> writes:

> <#!part disposition=inline>
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Save attachment from within emacs using notmuch-show-save-attachments"
# save as archive to test that Emacs does not re-compress .gz
test_emacs '(let ((standard-input "\"attachment1.gz\""))
	      (notmuch-show "id:cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com")
	      (notmuch-show-save-attachments))'
test_expect_equal_file attachment1.gz "$EXPECTED/attachment"

test_begin_subtest "Save attachment from within emacs using notmuch-show-save-part"
# save as archive to test that Emacs does not re-compress .gz
test_emacs '(let ((standard-input "\"attachment2.gz\""))
	      (notmuch-show "id:cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com")
	      (search-forward "0001-Deal-with")
	      (notmuch-show-save-part))'
test_expect_equal_file attachment2.gz "$EXPECTED/attachment"

test_begin_subtest "Save 8bit attachment from within emacs using notmuch-show-save-attachments"

add_message '[subject]="Attachment with 8bit chars"' \
	'[header]="MIME-Version: 1.0"' \
	'[content-type]="multipart/mixed; boundary=\"abcd\""' \
	'[body]="--abcd
Content-Type: text/plain

Attachment follows:

--abcd
Content-Type: application/octet-stream; name=\"sample\"
Content-Transfer-Encoding: 8bit
Content-Disposition: attachment; filename=\"sample\"

“¡ Hey ! It compiles ¡ Ship it !”

--abcd--
"'
test_emacs '(notmuch-show "id:'"${gen_msg_id}"'")
	    (delete-file "OUTPUT")
	    (let ((standard-input "\"OUTPUT\""))
	      (notmuch-show-save-attachments))'

test_expect_equal "$(cat OUTPUT)" '“¡ Hey ! It compiles ¡ Ship it !”'

test_begin_subtest "View raw message within emacs"
test_emacs '(notmuch-show "id:cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com")
	    (notmuch-show-view-raw-message)
	    (test-output)'
test_expect_equal_file $EXPECTED/raw-message-cf0c4d-52ad0a OUTPUT

test_begin_subtest "Hiding/showing signature in notmuch-show view"
maildir_storage_thread=$(notmuch search --output=threads id:20091117190054.GU3165@dottiness.seas.harvard.edu)
test_emacs "(notmuch-show \"$maildir_storage_thread\")
	    (search-forward \"Click/Enter to show.\")
	    (button-activate (button-at (point)))
	    (search-backward \"Click/Enter to hide.\")
	    (button-activate (button-at (point)))
	    (test-output)"
test_expect_equal_file $EXPECTED/notmuch-show-thread-maildir-storage OUTPUT

test_begin_subtest "Detection and hiding of top-post quoting of message"
add_message '[subject]="The problem with top-posting"' \
	    [id]=top-post-target \
	    '[body]="A: Because it messes up the order in which people normally read text.
Q: Why is top-posting such a bad thing?
A: Top-posting.
Q: What is the most annoying thing in e-mail?"'
add_message '[from]="Top Poster <top@poster.com>"' \
	    [in-reply-to]=top-post-target \
	    [references]=top-post-target \
	    '[subject]="Re: The problem with top-posting"' \
	    '[body]="Thanks for the advice! I will be sure to put it to good use.

-Top Poster

----- Original Message -----
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmai.org>
Sent: Fri, 05 Jan 2001 15:43:57 +0000
Subject: The problem with top-posting

Q: Why is top-posting such a bad thing?
A: Top-posting.
Q: What is the most annoying thing in e-mail?"'
test_emacs "(notmuch-show \"top-posting\")
	    (test-visible-output \"OUTPUT.raw\")"
echo "Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-05) (inbox)
Subject: The problem with top-posting
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: GENERATED_DATE

A: Because it messes up the order in which people normally read text.
Q: Why is top-posting such a bad thing?
A: Top-posting.
Q: What is the most annoying thing in e-mail?
Top Poster <top@poster.com> (2001-01-05) (inbox unread)
Subject: Re: The problem with top-posting
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: GENERATED_DATE

Thanks for the advice! I will be sure to put it to good use.

-Top Poster

[ 9-line hidden original message. Click/Enter to show. ]" > EXPECTED
notmuch_date_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Hiding message in notmuch-show view"
test_emacs '(notmuch-show "id:f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com")
	    (notmuch-show-toggle-message)
	    (test-visible-output)'
test_expect_equal_file $EXPECTED/notmuch-show-thread-with-hidden-messages OUTPUT

test_begin_subtest "Hiding message with visible citation in notmuch-show view"
test_emacs '(notmuch-show "id:f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com")
	    (search-forward "Click/Enter to show.")
	    (button-activate (button-at (point)))
	    (notmuch-show-toggle-message)
	    (test-visible-output)'
test_expect_equal_file $EXPECTED/notmuch-show-thread-with-hidden-messages OUTPUT

test_begin_subtest "notmuch-show: show message headers"
test_emacs \
	'(let ((notmuch-message-headers '\''("Subject" "To" "Cc" "Date"))
	       (notmuch-message-headers-visible t))
	   (notmuch-show "id:f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com")
	   (test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-message-with-headers-visible OUTPUT

test_begin_subtest "notmuch-show: hide message headers"
test_emacs \
	'(let ((notmuch-message-headers '\''("Subject" "To" "Cc" "Date"))
	       (notmuch-message-headers-visible nil))
	   (notmuch-show "id:f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com")
	   (test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-message-with-headers-hidden OUTPUT

test_begin_subtest "notmuch-show: hide message headers (w/ notmuch-show-toggle-visibility-headers)"
test_emacs \
	'(let ((notmuch-message-headers '\''("Subject" "To" "Cc" "Date"))
	       (notmuch-message-headers-visible t))
	   (notmuch-show "id:f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com")
	   (notmuch-show-toggle-visibility-headers)
	   (test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-message-with-headers-hidden OUTPUT

test_begin_subtest "notmuch-show: collapse all messages in thread"
test_emacs '(notmuch-show "id:f35dbb950911171435ieecd458o853c873e35f4be95@mail.gmail.com")
	(let ((current-prefix-arg t))
	  (notmuch-show-open-or-close-all)
	  (test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-thread-with-all-messages-collapsed OUTPUT

test_begin_subtest "notmuch-show: uncollapse all messages in thread"
test_emacs '(notmuch-show "id:f35dbb950911171435ieecd458o853c873e35f4be95@mail.gmail.com")
	(notmuch-show-open-or-close-all)
	(test-visible-output)'
test_expect_equal_file $EXPECTED/notmuch-show-thread-with-all-messages-uncollapsed OUTPUT

test_begin_subtest "Stashing in notmuch-show"
add_message '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' \
    '[from]="Some One <someone@somewhere.org>"' \
    '[to]="Some One Else <notsomeone@somewhere.org>"' \
    '[cc]="Notmuch <notmuch@notmuchmail.org>"' \
    '[subject]="Stash my stashables"' \
    '[id]="bought"' \
    '[body]="Unable to stash body. Where did you get it in the first place?!?"'
notmuch tag +stashtest id:${gen_msg_id}
test_emacs '(notmuch-show "id:\"bought\"")
	(notmuch-show-stash-date)
	(notmuch-show-stash-from)
	(notmuch-show-stash-to)
	(notmuch-show-stash-cc)
	(notmuch-show-stash-subject)
	(notmuch-show-stash-message-id)
	(notmuch-show-stash-message-id-stripped)
	(notmuch-show-stash-tags)
	(notmuch-show-stash-filename)
	(notmuch-show-stash-mlarchive-link "Gmane")
	(notmuch-show-stash-mlarchive-link "MARC")
	(notmuch-show-stash-mlarchive-link "Mail Archive, The")
	(switch-to-buffer
	  (generate-new-buffer "*test-stashing*"))
	(dotimes (i 12)
	  (yank)
	  (insert "\n")
	  (rotate-yank-pointer 1))
	(reverse-region (point-min) (point-max))
	    (test-output)'
cat <<EOF >EXPECTED
Sat, 01 Jan 2000 12:00:00 +0000
Some One <someone@somewhere.org>
Some One Else <notsomeone@somewhere.org>
Notmuch <notmuch@notmuchmail.org>
Stash my stashables
id:bought
bought
inbox,stashtest
${gen_msg_filename}
http://mid.gmane.org/bought
https://marc.info/?i=bought
https://mid.mail-archive.com/bought
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Stashing in notmuch-search"
test_emacs '(notmuch-search "id:\"bought\"")
	(notmuch-test-wait)
	(notmuch-search-stash-thread-id)
	(switch-to-buffer
	  (generate-new-buffer "*test-stashing*"))
	(yank)
	    (test-output)'
sed -i -e 's/^thread:.*$/thread:XXX/' OUTPUT
test_expect_equal "$(cat OUTPUT)" "thread:XXX"

test_begin_subtest 'notmuch-show-advance-and-archive with invisible signature'
message1='id:20091118010116.GC25380@dottiness.seas.harvard.edu'
message2='id:1258491078-29658-1-git-send-email-dottedmag@dottedmag.net'
test_emacs "(notmuch-show \"$message2\")
	    (test-output \"EXPECTED\")"
test_emacs "(notmuch-search \"$message1 or $message2\")
	    (notmuch-test-wait)
	    (notmuch-search-show-thread)
	    (goto-char (point-max))
	    (redisplay)
	    (notmuch-show-advance-and-archive)
	    (test-output)"
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Refresh show buffer"
test_emacs '(notmuch-show "id:f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com")
	    (test-visible-output "EXPECTED")
	    (notmuch-show-refresh-view)
	    (test-visible-output)'
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Refresh modified show buffer"
test_emacs '(notmuch-show "id:f35dbb950911171438k5df6eb56k77b6c0944e2e79ae@mail.gmail.com")
	    (notmuch-show-toggle-message)
	    (notmuch-show-next-message)
	    (notmuch-show-toggle-message)
	    (test-visible-output "EXPECTED")
	    (notmuch-show-refresh-view)
	    (test-visible-output)'
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Do not call notmuch for non-inlinable application/mpeg parts"
id='message-with-application/mpeg-attachment@notmuchmail.org'
emacs_fcc_message \
    'Message with application/mpeg attachment' \
    '' \
    "(message-goto-eoh)
     (insert \"Message-ID: <$id>\n\")
     (message-goto-body)
     (mml-insert-part \"application/mpeg\")
     (insert \"a fake mp3 file\")"
notmuch_counter_reset
test_emacs "(let ((notmuch-command \"$notmuch_counter_command\"))
	      (notmuch-show \"id:$id\"))"
test_expect_equal $(notmuch_counter_value) 1

test_begin_subtest "Do not call notmuch for non-inlinable audio/mpeg parts"
id='message-with-audio/mpeg-attachment@notmuchmail.org'
emacs_fcc_message \
    'Message with audio/mpeg attachment' \
    '' \
    "(message-goto-eoh)
     (insert \"Message-ID: <$id>\n\")
     (message-goto-body)
     (mml-insert-part \"audio/mpeg\")
     (insert \"a fake mp3 file\")"
notmuch_counter_reset
test_emacs "(let ((notmuch-command \"$notmuch_counter_command\"))
	      (notmuch-show \"id:$id\"))"
test_expect_equal $(notmuch_counter_value) 1

test_begin_subtest "notmuch-hello-mode hook is called"
counter=$(test_emacs \
    '(let ((notmuch-hello-mode-hook-counter 0))
       (kill-buffer "*notmuch-hello*")
       (notmuch-hello)
       notmuch-hello-mode-hook-counter)'
)
test_expect_equal "$counter" 1

test_begin_subtest "notmuch-hello-mode hook is not called on updates"
counter=$(test_emacs \
    '(let ((notmuch-hello-mode-hook-counter 0))
       (kill-buffer "*notmuch-hello*")
       (notmuch-hello)
       (notmuch-hello-update)
       notmuch-hello-mode-hook-counter)'
)
test_expect_equal "$counter" 1

test_begin_subtest "notmuch-hello-refresh hook is called"
counter=$(test_emacs \
    '(let ((notmuch-hello-refresh-hook-counter 0))
       (kill-buffer "*notmuch-hello*")
       (notmuch-hello)
       notmuch-hello-refresh-hook-counter)'
)
test_expect_equal "$counter" 1

test_begin_subtest "notmuch-hello-refresh hook is called on updates"
counter=$(test_emacs \
    '(let ((notmuch-hello-refresh-hook-counter 0))
       (kill-buffer "*notmuch-hello*")
       (notmuch-hello)
       (notmuch-hello-update)
       notmuch-hello-refresh-hook-counter)'
)
test_expect_equal "$counter" 2


add_message '[subject]="HTML mail with images"' \
    '[content-type]="multipart/related; boundary=abcd"' \
    '[body]="--abcd
Content-Type: text/html

<img src="cid:330@goomoji.gmail"> smiley

--abcd
Content-Type: image/gif
Content-Transfer-Encoding: base64
Content-ID: <330@goomoji.gmail>

R0lGODlhDAAMAKIFAF5LAP/zxAAAANyuAP/gaP///wAAAAAAACH5BAEAAAUALAAAAAAMAAwAAAMl
WLPcGjDKFYi9lxKBOaGcF35DhWHamZUW0K4mAbiwWtuf0uxFAgA7
--abcd--"'
test_emacs "(let ((mm-text-html-renderer
		   (if (assq 'shr mm-text-html-renderer-alist)
		       'shr 'html2text)))
	      (notmuch-show \"id:${gen_msg_id}\"))
	    (test-output)" > /dev/null
# Different Emacs versions and renderers give very different results,
# so just check that something reasonable showed up.  We first cat the
# output so the test framework will print it if the test fails.
test_begin_subtest "Rendering HTML mail with images"
test_expect_success 'cat OUTPUT && grep -q smiley OUTPUT'

test_begin_subtest "Search handles subprocess error exit codes"
cat > notmuch_fail <<EOF
#!/bin/sh
echo '()'
exit 1
EOF
chmod a+x notmuch_fail
test_emacs "(let ((notmuch-command \"$PWD/notmuch_fail\"))
	       (with-current-buffer \"*Messages*\"
                 (let ((inhibit-read-only t)) (erase-buffer)))
	       (with-current-buffer (get-buffer-create \"*Notmuch errors*\")
                 (erase-buffer))
	       (notmuch-search \"tag:inbox\")
	       (notmuch-test-wait)
	       (with-current-buffer \"*Messages*\"
		  (test-output \"MESSAGES\"))
	       (with-current-buffer \"*Notmuch errors*\"
		  (test-output \"ERROR\"))
	       (test-output))"

test_expect_equal "$(notmuch_emacs_error_sanitize notmuch_fail OUTPUT MESSAGES ERROR)" "\
=== OUTPUT ===
End of search results.
=== MESSAGES ===
YYY/notmuch_fail exited with status 1 (see *Notmuch errors* for more details)
=== ERROR ===
[XXX]
YYY/notmuch_fail exited with status 1
command: YYY/notmuch_fail search --format\=sexp --format-version\=4 --sort\=newest-first tag\:inbox
exit status: 1"

test_begin_subtest "Search handles subprocess warnings"
cat > notmuch_fail <<EOF
#!/bin/sh
echo '()'
echo This is a warning >&2
echo This is another warning >&2
exit 0
EOF
chmod a+x notmuch_fail
test_emacs "(let ((notmuch-command \"$PWD/notmuch_fail\"))
	       (with-current-buffer \"*Messages*\"
                 (let ((inhibit-read-only t)) (erase-buffer)))
	       (with-current-buffer (get-buffer-create \"*Notmuch errors*\")
                 (erase-buffer))
	       (notmuch-search \"tag:inbox\")
	       (notmuch-test-wait)
	       (with-current-buffer \"*Messages*\"
		  (test-output \"MESSAGES\"))
	       (with-current-buffer \"*Notmuch errors*\"
		  (test-output \"ERROR\"))
	       (test-output))"
sed -i -e 's/^\[.*\]$/[XXX]/' ERROR
test_expect_equal "$(cat OUTPUT; echo ---; cat MESSAGES; echo ---; cat ERROR)" "\
End of search results.
---
This is a warning (see *Notmuch errors* for more details)
---
[XXX]
This is a warning
This is another warning"

test_begin_subtest "Search thread tag operations are race-free"
add_message '[subject]="Search race test"'
gen_msg_id_1=$gen_msg_id
generate_message '[in-reply-to]="<'$gen_msg_id_1'>"' \
	    '[references]="<'$gen_msg_id_1'>"' \
	    '[subject]="Search race test two"'
test_emacs '(notmuch-search "subject:\"search race test\"")
	    (notmuch-test-wait)
	    (notmuch-poll)
	    (execute-kbd-macro "+search-thread-race-tag")'
output=$(notmuch search --output=messages 'tag:search-thread-race-tag')
test_expect_equal "$output" "id:$gen_msg_id_1"

test_begin_subtest "Search global tag operations are race-free"
generate_message '[in-reply-to]="<'$gen_msg_id_1'>"' \
	    '[references]="<'$gen_msg_id_1'>"' \
	    '[subject]="Re: Search race test"'
test_emacs '(notmuch-search "subject:\"search race test\" -subject:two")
	    (notmuch-test-wait)
	    (notmuch-poll)
	    (execute-kbd-macro "*+search-global-race-tag")'
output=$(notmuch search --output=messages 'tag:search-global-race-tag')
test_expect_equal "$output" "id:$gen_msg_id_1"

test_begin_subtest "Term escaping"
output=$(test_emacs "(mapcar 'notmuch-escape-boolean-term (list
	\"\"
	\"abc\`~\!@#\$%^&*-=_+123\"
	\"(abc\"
	\")abc\"
	\"\\\"abc\"
	\"\x01xyz\"
	\"\\x201cxyz\\x201d\"))")
test_expect_equal "$output" '("\"\"" "abc`~!@#$%^&*-=_+123" "\"(abc\"" "\")abc\"" "\"\"\"abc\"" "\"'$'\x01''xyz\"" "\"“xyz”\"")'

test_done
