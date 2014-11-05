#!/usr/bin/env bash
test_description='"notmuch address" in several variants'
. ./test-lib.sh

add_email_corpus

test_begin_subtest "--output=sender"
notmuch address --output=sender '*' >OUTPUT
cat <<EOF >EXPECTED
François Boulogne <boulogne.f@gmail.com>
Olivier Berger <olivier.berger@it-sudparis.eu>
Chris Wilson <chris@chris-wilson.co.uk>
Carl Worth <cworth@cworth.org>
Alexander Botero-Lowry <alex.boterolowry@gmail.com>
Keith Packard <keithp@keithp.com>
Jjgod Jiang <gzjjgod@gmail.com>
Rolland Santimano <rollandsantimano@yahoo.com>
Jan Janak <jan@ryngle.com>
Stewart Smith <stewart@flamingspork.com>
Lars Kellogg-Stedman <lars@seas.harvard.edu>
Alex Botero-Lowry <alex.boterolowry@gmail.com>
Ingmar Vanhassel <ingmar@exherbo.org>
Aron Griffis <agriffis@n01se.net>
Adrian Perez de Castro <aperez@igalia.com>
Israel Herraiz <isra@herraiz.org>
Mikhail Gusarov <dottedmag@dottedmag.net>
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "--output=sender --format=json"
notmuch address --output=sender --format=json '*' >OUTPUT
cat <<EOF >EXPECTED
[{"name": "François Boulogne", "address": "boulogne.f@gmail.com", "name-addr": "François Boulogne <boulogne.f@gmail.com>"},
{"name": "Olivier Berger", "address": "olivier.berger@it-sudparis.eu", "name-addr": "Olivier Berger <olivier.berger@it-sudparis.eu>"},
{"name": "Chris Wilson", "address": "chris@chris-wilson.co.uk", "name-addr": "Chris Wilson <chris@chris-wilson.co.uk>"},
{"name": "Carl Worth", "address": "cworth@cworth.org", "name-addr": "Carl Worth <cworth@cworth.org>"},
{"name": "Alexander Botero-Lowry", "address": "alex.boterolowry@gmail.com", "name-addr": "Alexander Botero-Lowry <alex.boterolowry@gmail.com>"},
{"name": "Keith Packard", "address": "keithp@keithp.com", "name-addr": "Keith Packard <keithp@keithp.com>"},
{"name": "Jjgod Jiang", "address": "gzjjgod@gmail.com", "name-addr": "Jjgod Jiang <gzjjgod@gmail.com>"},
{"name": "Rolland Santimano", "address": "rollandsantimano@yahoo.com", "name-addr": "Rolland Santimano <rollandsantimano@yahoo.com>"},
{"name": "Jan Janak", "address": "jan@ryngle.com", "name-addr": "Jan Janak <jan@ryngle.com>"},
{"name": "Stewart Smith", "address": "stewart@flamingspork.com", "name-addr": "Stewart Smith <stewart@flamingspork.com>"},
{"name": "Lars Kellogg-Stedman", "address": "lars@seas.harvard.edu", "name-addr": "Lars Kellogg-Stedman <lars@seas.harvard.edu>"},
{"name": "Alex Botero-Lowry", "address": "alex.boterolowry@gmail.com", "name-addr": "Alex Botero-Lowry <alex.boterolowry@gmail.com>"},
{"name": "Ingmar Vanhassel", "address": "ingmar@exherbo.org", "name-addr": "Ingmar Vanhassel <ingmar@exherbo.org>"},
{"name": "Aron Griffis", "address": "agriffis@n01se.net", "name-addr": "Aron Griffis <agriffis@n01se.net>"},
{"name": "Adrian Perez de Castro", "address": "aperez@igalia.com", "name-addr": "Adrian Perez de Castro <aperez@igalia.com>"},
{"name": "Israel Herraiz", "address": "isra@herraiz.org", "name-addr": "Israel Herraiz <isra@herraiz.org>"},
{"name": "Mikhail Gusarov", "address": "dottedmag@dottedmag.net", "name-addr": "Mikhail Gusarov <dottedmag@dottedmag.net>"}]
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "--output=recipients"
notmuch address --output=recipients '*' >OUTPUT
cat <<EOF >EXPECTED
Allan McRae <allan@archlinux.org>
"Discussion about the Arch User Repository (AUR)" <aur-general@archlinux.org>
olivier.berger@it-sudparis.eu
notmuch@notmuchmail.org
notmuch <notmuch@notmuchmail.org>
Keith Packard <keithp@keithp.com>
Mikhail Gusarov <dottedmag@dottedmag.net>
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "--output=sender --output=recipients"
notmuch address --output=sender --output=recipients '*' >OUTPUT
cat <<EOF >EXPECTED
François Boulogne <boulogne.f@gmail.com>
Allan McRae <allan@archlinux.org>
"Discussion about the Arch User Repository (AUR)" <aur-general@archlinux.org>
Olivier Berger <olivier.berger@it-sudparis.eu>
olivier.berger@it-sudparis.eu
Chris Wilson <chris@chris-wilson.co.uk>
notmuch@notmuchmail.org
Carl Worth <cworth@cworth.org>
Alexander Botero-Lowry <alex.boterolowry@gmail.com>
Keith Packard <keithp@keithp.com>
Jjgod Jiang <gzjjgod@gmail.com>
Rolland Santimano <rollandsantimano@yahoo.com>
Jan Janak <jan@ryngle.com>
Stewart Smith <stewart@flamingspork.com>
Lars Kellogg-Stedman <lars@seas.harvard.edu>
notmuch <notmuch@notmuchmail.org>
Alex Botero-Lowry <alex.boterolowry@gmail.com>
Ingmar Vanhassel <ingmar@exherbo.org>
Aron Griffis <agriffis@n01se.net>
Adrian Perez de Castro <aperez@igalia.com>
Israel Herraiz <isra@herraiz.org>
Mikhail Gusarov <dottedmag@dottedmag.net>
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "without --output"
notmuch address '*' >OUTPUT
# Use EXPECTED from previous subtest
test_expect_equal_file OUTPUT EXPECTED

test_done
