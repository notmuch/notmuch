/*
** Copyright 1998 - 2002 Double Precision, Inc.  See COPYING for
** distribution information.
*/

#if	HAVE_CONFIG_H
#include	"config.h"
#endif
#include	<sys/types.h>
#if	HAVE_UNISTD_H
#include	<unistd.h>
#endif
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<grp.h>
#include	<pwd.h>
#include	<errno.h>

#include	"numlib.h"


void libmail_changegroup(gid_t gid)
{
	if ( setgid(gid))
	{
		perror("setgid");
		exit(1);
	}

#if HAVE_SETGROUPS
	if ( getuid() == 0 && setgroups(1, &gid) )
	{
		perror("setgroups");
		exit(1);
	}
#endif
}

void libmail_changeuidgid(uid_t uid, gid_t gid)
{
	libmail_changegroup(gid);
	if ( setuid(uid))
	{
		perror("setuid");
		exit(1);
	}
}

void libmail_changeusername(const char *uname, const gid_t *forcegrp)
{
struct passwd *pw;
uid_t	changeuid;
gid_t	changegid;

/* uname might be a pointer returned from a previous called to getpw(),
** and libc has a problem getting it back.
*/
char	*p=malloc(strlen(uname)+1);

	if (!p)
	{
		perror("malloc");
		exit(1);
	}
	strcpy(p, uname);

	errno=ENOENT;
	if ((pw=getpwnam(p)) == 0)
	{
		free(p);
		perror("getpwnam");
		exit(1);
	}
	free(p);

	changeuid=pw->pw_uid;

	if ( !forcegrp )	forcegrp= &pw->pw_gid;

	changegid= *forcegrp;

	if ( setgid( changegid ))
	{
		perror("setgid");
		exit(1);
	}

#if HAVE_INITGROUPS
	if ( getuid() == 0 && initgroups(pw->pw_name, changegid) )
	{
		perror("initgroups");
		exit(1);
	}
#else
#if HAVE_SETGROUPS
	if ( getuid() == 0 && setgroups(1, &changegid) )
	{
		perror("setgroups");
		exit(1);
	}
#endif
#endif

	if (setuid(changeuid))
	{
		perror("setuid");
		exit(1);
	}
}
