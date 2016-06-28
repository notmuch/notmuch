#!/usr/bin/env bash

set -eu
#set -x # or enter bash -x ... on command line

if [ x"${BASH_VERSION-}" = x ]
then	echo
	echo "Please execute this script using 'bash' interpreter"
	echo
	exit 1
fi

set -o posix
set -o pipefail # bash feature

readonly DEFAULT_IFS="$IFS" # Note: In this particular case quotes are needed.

# Avoid locale-specific differences in output of executed commands
LANG=C LC_ALL=C; export LANG LC_ALL

readonly PV_FILE='bindings/python/notmuch/version.py'

# Using array here turned out to be unnecessarily complicated
emsgs=''
emsg_count=0
append_emsg ()
{
	emsg_count=$((emsg_count + 1))
	emsgs="${emsgs:+$emsgs\n}  $1"
}

for f in ./version debian/changelog NEWS "$PV_FILE"
do
	if   [ ! -f "$f" ]; then append_emsg "File '$f' is missing"
	elif [ ! -r "$f" ]; then append_emsg "File '$f' is unreadable"
	elif [ ! -s "$f" ]; then append_emsg "File '$f' is empty"
	fi
done

if [ -n "$emsgs" ]
then
	echo 'Release files problems; fix these and try again:'
	echo -e "$emsgs"
	exit 1
fi

if read VERSION
then
	if read rest
	then	echo "'version' file contains more than one line"
		exit 1
	fi
else
	echo "Reading './version' file failed (suprisingly!)"
	exit 1
fi < ./version

readonly VERSION

# In the rest of this file, tests collect list of errors to be fixed

echo -n "Checking that git working directory is clean... "
git_status=`git status --porcelain`
if [ "$git_status" = '' ]
then
	echo Yes.
else
	echo No.
	append_emsg "Git working directory is not clean (git status --porcelain)."
fi
unset git_status

verfail ()
{
	echo No.
	append_emsg "$@"
	append_emsg "  Please follow the instructions in RELEASING to choose a version"
}

echo -n "Checking that '$VERSION' is good with digits and periods... "
case $VERSION in
	*[!0-9.]*)
		verfail "'$VERSION' contains other characters than digits and periods" ;;
	.*)	verfail "'$VERSION' begins with a period" ;;
	*.)	verfail "'$VERSION' ends with a period" ;;
	*..*)	verfail "'$VERSION' contains two consecutive periods" ;;
	*.*)	echo Yes. ;;
	*)	verfail "'$VERSION' is a single number" ;;
esac

echo -n "Checking that this is Debian package for notmuch... "
read deb_notmuch deb_version rest < debian/changelog
if [ "$deb_notmuch" = 'notmuch' ]
then
	echo Yes.
else
	echo No.
	append_emsg "Package name '$deb_notmuch' is not 'notmuch' in debian/changelog"
fi

echo -n "Checking that Debian package version is $VERSION-1... "

if [ "$deb_version" = "($VERSION-1)" ]
then
	echo Yes.
else
	echo No.
	append_emsg "Version '$deb_version' is not '($VERSION-1)' in debian/changelog"
fi

echo -n "Checking that python bindings version is $VERSION... "
py_version=`python -c "with open('$PV_FILE') as vf: exec(vf.read()); print(__VERSION__)"`
if [ "$py_version" = "$VERSION" ]
then
	echo Yes.
else
	echo No.
	append_emsg "Version '$py_version' is not '$VERSION' in $PV_FILE"
fi

echo -n "Checking that NEWS header is tidy... "
if [ "`exec sed 's/./=/g; 1q' NEWS`" = "`exec sed '1d; 2q' NEWS`" ]
then
	echo Yes.
else
	echo No.
	if [ "`exec sed '1d; s/=//g; 2q' NEWS`" != '' ]
	then
		append_emsg "Line 2 in NEWS file is not all '=':s"
	else
		append_emsg "Line 2 in NEWS file does not have the same length as line 1"
	fi
fi

echo -n "Checking that this is Notmuch NEWS... "
read news_notmuch news_version news_date < NEWS
if [ "$news_notmuch" = "Notmuch" ]
then
	echo Yes.
else
	echo No.
	append_emsg "First word '$news_notmuch' is not 'Notmuch' in NEWS file"
fi

echo -n "Checking that NEWS version is $VERSION... "
if [ "$news_version" = "$VERSION" ]
then
	echo Yes.
else
	echo No.
	append_emsg "Version '$news_version' in NEWS file is not '$VERSION'"
fi

#eval `date '+year=%Y mon=%m day=%d'`
today0utc=`date --date=0Z +%s` # gnu date feature

echo -n "Checking that NEWS date is right... "
case $news_date in
 '('[2-9][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]')')
	newsdate0utc=`nd=${news_date#\\(}; date --date="${nd%)} 0Z" +%s`
	ddiff=$((newsdate0utc - today0utc))
	if [ $ddiff -lt -86400 ] # since beginning of yesterday...
	then
		echo No.
		append_emsg "Date $news_date in NEWS file is too much in the past"
	elif [ $ddiff -gt 172800 ] # up to end of tomorrow...
	then
		echo No.
		append_emsg "Date $news_date in NEWS file is too much in the future"
	else
		echo Yes.
	fi ;;
 *)
	echo No.
	append_emsg "Date '$news_date' in NEWS file is not in format (yyyy-mm-dd)"
esac

year=`exec date +%Y`
echo -n "Checking that copyright in documentation contains 2009-$year... "
# Read the value of variable `copyright' defined in 'doc/conf.py'.
# As __file__ is not defined when python command is given from command line,
# it is defined before contents of 'doc/conf.py' (which dereferences __file__)
# is executed.
copyrightline=`exec python -c "with open('doc/conf.py') as cf: __file__ = ''; exec(cf.read()); print(copyright)"`
case $copyrightline in
	*2009-$year*)
		echo Yes. ;;
	*)
		echo No.
		append_emsg "The copyright in doc/conf.py line '$copyrightline' does not contain '2009-$year'"
esac

if [ -n "$emsgs" ]
then
	echo
	echo 'Release check failed; check these issues:'
	echo -e "$emsgs"
	exit 1
fi

echo 'All checks this script executed completed successfully.'
echo 'Make sure that everything else mentioned in RELEASING'
echo 'file is in order, too.'


# Local variables:
# mode: shell-script
# sh-basic-offset: 8
# tab-width: 8
# End:
# vi: set sw=8 ts=8
