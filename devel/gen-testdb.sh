#!/usr/bin/env bash
#
# NAME
#	gen-testdb.sh - generate test databases
#
# SYNOPSIS
#	gen-testdb.sh -v NOTMUCH-VERSION [-c CORPUS-PATH] [-s TAR-SUFFIX]
#
# DESCRIPTION
#	Generate a tarball containing the specified test corpus and
#	the corresponding notmuch database, indexed using a specific
#	version of notmuch, resulting in a specific version of the
#	database.
#
#	The specific version of notmuch will be built on the fly.
#	Therefore the script must be run within a git repository to be
#	able to build the old versions of notmuch.
#
#	This script reuses the test infrastructure, and the script
#	must be run from within the test directory.
#
#	The output tarballs, named database-<TAR-SUFFIX>.tar.gz, are
#	placed in the test/test-databases directory.
#
# OPTIONS
#	-v NOTMUCH-VERSION
#		Notmuch version in terms of a git tag or commit to use
#		for generating the database. Required.
#
#	-c CORPUS-PATH
#		Path to a corpus to use for generating the
#		database. Due to CWD changes within the test
#		infrastructure, use absolute paths. Defaults to the
#		test corpus.
#
#	-s TAR-SUFFIX
#		Suffix for the tarball basename. Empty by default.
#
# EXAMPLE
#
#	Generate a database indexed with notmuch 0.17. Use the default
#	test corpus. Name the tarball database-v1.tar.gz to reflect
#	the fact that notmuch 0.17 used database version 1.
#
#	$ cd test
#	$ ../devel/gen-testdb.sh -v 0.17 -s v1
#
# CAVEATS
#	Test infrastructure options won't work.
#
#	Any existing databases with the same name will be overwritten.
#
#	It may not be possible to build old versions of notmuch with
#	the set of dependencies that satisfy building the current
#	version of notmuch.
#
# AUTHOR
#	Jani Nikula <jani@nikula.org>
#
# LICENSE
#	Same as notmuch test infrastructure (GPLv2+).
#

test_description="database generation abusing test infrastructure"

# immediate exit on subtest failure; see test_failure_ in test-lib.sh
immediate=t

VERSION=
CORPUS=
SUFFIX=

while getopts v:c:s: opt; do
    case "$opt" in
	v) VERSION="$OPTARG";;
	c) CORPUS="$OPTARG";;
	s) SUFFIX="-$OPTARG";;
    esac
done
shift `expr $OPTIND - 1`

. ./test-lib.sh || exit 1

SHORT_CORPUS=$(basename ${CORPUS:-database})
DBNAME=${SHORT_CORPUS}${SUFFIX}
TARBALLNAME=${DBNAME}.tar.xz

CORPUS=${CORPUS:-${TEST_DIRECTORY}/corpus}

test_expect_code 0 "notmuch version specified on the command line" \
    "test -n ${VERSION}"

test_expect_code 0 "the specified version ${VERSION} refers to a commit" \
    "git show ${VERSION} >/dev/null 2>&1"

BUILD_DIR="notmuch-${VERSION}"
test_expect_code 0 "generate snapshot of notmuch version ${VERSION}" \
    "git -C $TEST_DIRECTORY/.. archive --prefix=${BUILD_DIR}/ --format=tar ${VERSION} | tar x"

# force version string
git describe --match '[0-9.]*' ${VERSION} > ${BUILD_DIR}/version

test_expect_code 0 "configure and build notmuch version ${VERSION}" \
    "make -C ${BUILD_DIR}"

# use the newly built notmuch
export PATH=./${BUILD_DIR}:$PATH

test_begin_subtest "verify the newly built notmuch version"
test_expect_equal "`notmuch --version`" "notmuch `cat ${BUILD_DIR}/version`"

# replace the existing mails, if any, with the specified corpus
rm -rf ${MAIL_DIR}
cp -a ${CORPUS} ${MAIL_DIR}

test_expect_code 0 "index the corpus" \
    "notmuch new"

# wrap the resulting mail store and database in a tarball

cp -a ${MAIL_DIR} ${TMP_DIRECTORY}/${DBNAME}
tar Jcf ${TMP_DIRECTORY}/${TARBALLNAME} -C ${TMP_DIRECTORY} ${DBNAME}
mkdir -p  ${TEST_DIRECTORY}/test-databases
cp -a ${TMP_DIRECTORY}/${TARBALLNAME} ${TEST_DIRECTORY}/test-databases
test_expect_code 0 "create the output tarball ${TARBALLNAME}" \
    "test -f ${TEST_DIRECTORY}/test-databases/${TARBALLNAME}"

# generate a checksum file
test_expect_code 0 "compute checksum" \
    "(cd ${TEST_DIRECTORY}/test-databases/ && sha256sum ${TARBALLNAME} > ${TARBALLNAME}.sha256)"
test_done
