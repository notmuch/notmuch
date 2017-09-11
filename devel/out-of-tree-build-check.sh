#!/bin/sh
# test out-of-tree builds in a temp directory
# passes all args to make

set -eu

srcdir="$(cd "$(dirname "$0")"/.. && pwd)"
builddir=$(mktemp -d)

cd $builddir

$srcdir/configure
make "$@"

rm -rf $builddir
