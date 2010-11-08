#!/bin/sh
# vim: set sw=4 et sts=4 tw=80 :

# RDoc fails to document C extension split into many files.
# This is a hack to generate documentation properly.

rm -fr ruby
cat *.c > rdoc-sucks.c
rdoc --main 'Notmuch' --title 'Notmuch Ruby API' --op ruby rdoc-sucks.c
rm -f rdoc-sucks.c
