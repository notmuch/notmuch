#!/bin/sh

if test -z "$RDOC"; then
    RDOC=rdoc
    if which rdoc19 >/dev/null 2>&1; then
        RDOC=rdoc19
    fi
fi

set -e
set -x

$RDOC --main 'Notmuch' --title 'Notmuch Ruby API' --op ruby *.c

if test "$1" = "--upload"; then
    rsync -avze ssh --delete --partial --progress ruby bach.exherbo.org:public_html/notmuch/
fi
