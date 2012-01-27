/*
** Copyright 2000 Double Precision, Inc.
** See COPYING for distribution information.
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include	<sys/types.h>
#include	<sys/stat.h>
#include	<string.h>
#include	<stdlib.h>
#include	<time.h>
#if	HAVE_UNISTD_H
#include	<unistd.h>
#endif
#include	<stdio.h>
#include	<ctype.h>
#include	<errno.h>
#include	<fcntl.h>

#include	"maildirmisc.h"


char *maildir_getlink(const char *filename)
{
#if     HAVE_READLINK
size_t	bufsiz;
char	*buf;

	bufsiz=0;
	buf=0;

	for (;;)
	{
	int	n;

		if (buf)	free(buf);
		bufsiz += 256;
		if ((buf=malloc(bufsiz)) == 0)
		{
			perror("malloc");
			return (0);
		}
		if ((n=readlink(filename, buf, bufsiz)) < 0)
		{
			free(buf);
			return (0);
		}
		if (n < bufsiz)
		{
			buf[n]=0;
			break;
		}
	}
	return (buf);
#else
	return (0);
#endif
}

int maildir_semisafeopen(const char *path, int mode, int perm)
{

#if	HAVE_READLINK

char	*l=maildir_getlink(path);

	if (l)
	{
	int	f;

		if (*l != '/')
		{
		char	*q=malloc(strlen(path)+strlen(l)+2);
		char	*s;

			if (!q)
			{
				free(l);
				return (-1);
			}

			strcpy(q, path);
			if ((s=strchr(q, '/')) != 0)
				s[1]=0;
			else	*q=0;
			strcat(q, l);
			free(l);
			l=q;
		}

		f=maildir_safeopen(l, mode, perm);

		free(l);
		return (f);
	}
#endif

	return (maildir_safeopen(path, mode, perm));
}
		
int maildir_safeopen(const char *path, int mode, int perm)
{
	struct	stat	stat1;

	return maildir_safeopen_stat(path, mode, perm, &stat1);
}

int maildir_safeopen_stat(const char *path, int mode, int perm,
			  struct stat *stat1)
{
	struct	stat	stat2;

	int	fd=open(path, mode
#ifdef	O_NONBLOCK
			| O_NONBLOCK
#else
			| O_NDELAY
#endif
			, perm);

	if (fd < 0)	return (fd);
	if (fcntl(fd, F_SETFL, (mode & O_APPEND)) || fstat(fd, stat1)
	    || lstat(path, &stat2))
	{
		close(fd);
		return (-1);
	}

	if (stat1->st_dev != stat2.st_dev || stat1->st_ino != stat2.st_ino)
	{
		close(fd);
		errno=ENOENT;
		return (-1);
	}

	return (fd);
}
