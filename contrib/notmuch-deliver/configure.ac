dnl vim: set sw=4 sts=4 ts=4 noet ft=config foldmethod=marker foldmarker={{{,}}} :

dnl {{{ Program, version
AC_PREREQ(2.59)
AC_INIT([src/main.c])
AC_CANONICAL_SYSTEM

VERSION_MAJOR=0
VERSION_MINOR=1
VERSION_FULL="$VERSION_MAJOR.$VERSION_MINOR"
VERSION="$VERSION_FULL"

AC_SUBST([VERSION_MAJOR])
AC_SUBST([VERSION_MINOR])
AC_SUBST([VERSION_FULL])

AM_INIT_AUTOMAKE(notmuch-deliver, [$VERSION_FULL])
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

dnl {{{ Git revision
AC_MSG_CHECKING([for git head])
if test -d "${GIT_DIR:-${ac_top_srcdir:-./}/.git}" ; then
	GITHEAD=`git describe 2>/dev/null`
	if test -z ${GITHEAD} ; then
		GITHEAD=`git rev-parse HEAD`
	fi
	if test -n "`git diff-index -m --name-only HEAD`" ; then
		GITHEAD=${GITHEAD}-dirty
	fi
	if test -n "${GITHEAD}" ; then
		GITHEAD="-${GITHEAD}"
	fi
fi
AC_MSG_RESULT([$GITHEAD])
AC_SUBST([GITHEAD])
dnl }}}
dnl }}}

dnl {{{ Toolchain checks
AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CC
AC_PROG_CC_C99
if test x"$ac_cv_prog_cc_c99" = x"no"; then
	AC_MSG_ERROR([notmuch-deliver requires a C compiler that supports ISO C99!])
fi
AC_PROG_LIBTOOL
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MAKE_SET
dnl }}}

dnl {{{ Check for headers
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([sys/stat.h unistd.h sysexits.h utime.h])
dnl }}}

dnl {{{ Check for typedefs, structures and compiler characteristics
AC_C_CONST
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_UID_T
AC_TYPE_PID_T
AC_STRUCT_TM
dnl }}}

dnl {{{ Check for library functions
AC_CHECK_FUNCS([setgroups initgroups symlink readlink strcasecmp utime utimes])
dnl }}}

dnl {{{ gethostname()
AC_CACHE_CHECK([for missing gethostname prototype],
			   maildir_cv_SYS_GETHOSTNAME,
			   AC_TRY_COMPILE([
#ifdef HAVE_UNISTD_H
#include	<unistd.h>
#endif

int gethostname(int,int);
],,[maildir_cv_SYS_GETHOSTNAME=yes], [maildir_cv_SYS_GETHOSTNAME=no]))

if test x"$maildir_cv_SYS_GETHOSTNAME" = x"no" ; then
	AC_DEFINE_UNQUOTED(HAS_GETHOSTNAME, 1, [Whether gethostname() is prototyped])
fi
dnl }}}

dnl {{{ Check for maildir target separator
if test "$target_os" = "cygwin" ; then
	AC_DEFINE_UNQUOTED(MDIRSEP, "!", [Maildir target separator])
else
	AC_DEFINE_UNQUOTED(MDIRSEP, ":", [Maildir target separator])
fi
dnl }}}

dnl {{{ Make pkg-config work
PKG_PROG_PKG_CONFIG([0.9.0])
dnl }}}

dnl {{{ Check for libraries
GLIB_REQUIRED=2.16

PKG_CHECK_MODULES([glib], [glib-2.0 >= $GLIB_REQUIRED],,
				  [AC_MSG_ERROR([notmuch-deliver requires glib-$GLIB_REQUIRED or newer])])
AC_CHECK_LIB(notmuch, notmuch_database_create,,
			 [AC_MSG_ERROR([notmuch-deliver requires notmuch mail indexing library])])
dnl }}}

dnl {{{ Extra CFLAGS
NOTMUCH_DELIVER_CFLAGS=
WANTED_CFLAGS="-Wall -W -Wextra -Wvla -Wformat=2 -Wformat-security -Wformat-nonliteral -Winit-self -Wfloat-equal -Wno-deprecated-declarations -Wmissing-declarations -Wmissing-noreturn -Wmissing-prototypes -Wredundant-decls  -Wshadow -Wpointer-arith -Wstrict-prototypes -Wcast-qual -Wwrite-strings -pedantic"
for flag in $WANTED_CFLAGS ; do
	AX_CHECK_COMPILER_FLAGS([$flag], [NOTMUCH_DELIVER_CFLAGS="$NOTMUCH_DELIVER_CFLAGS $flag"],)
done
dnl }}}

dnl {{{ Profiling
AC_MSG_CHECKING([whether gprof symbols are wanted])
AC_ARG_ENABLE([gprof],
			  [AS_HELP_STRING([--enable-gprof],
							  [Add gprof symbols (-pg) (for debugging)])],
			  PROFILE="$enableval",
			  PROFILE="no")
AC_MSG_RESULT([$PROFILE])
if test x"$PROFILE" = x"yes" ; then
	AX_CHECK_COMPILER_FLAGS([-pg],, AC_MSG_ERROR([Your compiler doesn't support -pg flag]))
	NOTMUCH_DELIVER_CFLAGS="$NOTMUCH_DELIVER_CFLAGS -pg"
	LDFLAGS="$LDFLAGS -pg"
fi
AC_MSG_CHECKING([whether coverage testing should be enabled])
AC_ARG_ENABLE([gcov],
			  [AS_HELP_STRING([--enable-gcov],
							  [add gcov support (for debugging)])],
			  COVERAGE="$enableval",
			  COVERAGE="no")
AC_MSG_RESULT([$COVERAGE])
if test x"$COVERAGE" = x"yes" ; then
	AX_CHECK_COMPILER_FLAGS([-fprofile-arcs],,
							AC_MSG_ERROR([Your compiler doesn't support -fprofile-arcs flag]))
	AX_CHECK_COMPILER_FLAGS([-ftest-coverage],,
							AC_MSG_ERROR([Your compiler doesn't support -ftest-coverage flag]))
	NOTMUCH_DELIVER_CFLAGS="$NOTMUCH_DELIVER_CFLAGS -fprofile-arcs -ftest-coverage"
fi
AC_SUBST([NOTMUCH_DELIVER_CFLAGS])
dnl }}}

dnl {{{ Output
AM_CONFIG_HEADER(config.h)
AC_OUTPUT(
		  Makefile
		  numlib/Makefile
		  src/Makefile
		  )
dnl }}}
