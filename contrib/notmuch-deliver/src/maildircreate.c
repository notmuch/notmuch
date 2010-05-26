/*
** Copyright 1998 - 2003 Double Precision, Inc.
** See COPYING for distribution information.
*/

#include	"maildircreate.h"
#include	"maildirmisc.h"
#include	<sys/types.h>
#if	HAVE_SYS_STAT_H
#include	<sys/stat.h>
#endif

#if TIME_WITH_SYS_TIME
#include	<sys/time.h>
#include	<time.h>
#else
#if HAVE_SYS_TIME_H
#include	<sys/time.h>
#else
#include	<time.h>
#endif
#endif
#if	HAVE_SYS_STAT_H
#include	<sys/stat.h>
#endif

#if	HAVE_UNISTD_H
#include	<unistd.h>
#endif
#include	<string.h>
#include	<errno.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<fcntl.h>
#include	"numlib/numlib.h"


static const char rcsid[]="$Id: maildircreate.c,v 1.6 2003/01/26 04:07:03 mrsam Exp $";

FILE *maildir_tmpcreate_fp(struct maildir_tmpcreate_info *info)
{
	int fd=maildir_tmpcreate_fd(info);
	FILE *fp;

	if (fd < 0)
		return NULL;

	fp=fdopen(fd, "w+");

	if (fp == NULL)
	{
		close(fd);
		return NULL;
	}

	return fp;
}

static int maildir_tmpcreate_fd_do(struct maildir_tmpcreate_info *info);

#define KEEPTRYING	(60 * 60)
#define SLEEPFOR	3

int maildir_tmpcreate_fd(struct maildir_tmpcreate_info *info)
{
	int i;

	if (!info->doordie)
		return (maildir_tmpcreate_fd_do(info));

	for (i=0; i<KEEPTRYING / SLEEPFOR; i++)
	{
		int fd=maildir_tmpcreate_fd_do(info);

		if (fd >= 0 || errno != EAGAIN)
			return fd;

		sleep(SLEEPFOR);
	}

	return -1;
}

static int maildir_tmpcreate_fd_do(struct maildir_tmpcreate_info *info)
{
	const char *maildir=info->maildir;
	const char *uniq=info->uniq;
	const char *hostname=info->hostname;

	char hostname_buf[256];
	char time_buf[NUMBUFSIZE];
	char usec_buf[NUMBUFSIZE];
	char pid_buf[NUMBUFSIZE];
	char len_buf[NUMBUFSIZE+3];
	char dev_buf[NUMBUFSIZE];
	char ino_buf[NUMBUFSIZE];
	struct timeval tv;

	struct stat stat_buf;
	int fd;

	if (!maildir)
		maildir=".";
	if (!uniq)
		uniq="";

	if (!hostname || !*hostname)
	{
		hostname_buf[sizeof(hostname_buf)-1]=0;
		if (gethostname(hostname_buf, sizeof(hostname_buf)-1) < 0)
			strcpy(hostname_buf, "localhost");
		hostname=hostname_buf;
	}

	gettimeofday(&tv, NULL);

	libmail_str_time_t(tv.tv_sec, time_buf);
	libmail_str_time_t(tv.tv_usec, usec_buf);
	libmail_str_pid_t(getpid(), pid_buf);
	len_buf[0]=0;
	if (info->msgsize > 0)
	{
		strcpy(len_buf, ",S=");
		libmail_str_size_t(info->msgsize, len_buf+3);
	}

	if (info->tmpname)
		free(info->tmpname);

	info->tmpname=malloc(strlen(maildir)+strlen(uniq)+
			     strlen(hostname)+strlen(time_buf)+
			     strlen(usec_buf)+
			     strlen(pid_buf)+strlen(len_buf)+100);

	if (!info->tmpname)
	{
		maildir_tmpcreate_free(info);
		return -1;
	}

	strcpy(info->tmpname, maildir);
	strcat(info->tmpname, "/tmp/");
	strcat(info->tmpname, time_buf);
	strcat(info->tmpname, ".M");
	strcat(info->tmpname, usec_buf);
	strcat(info->tmpname, "P");
	strcat(info->tmpname, pid_buf);

	if (*uniq)
		strcat(strcat(info->tmpname, "_"), uniq);
	strcat(info->tmpname, ".");
	strcat(info->tmpname, hostname);
	strcat(info->tmpname, len_buf);

	if (stat( info->tmpname, &stat_buf) == 0)
	{
		maildir_tmpcreate_free(info);
		errno=EAGAIN;
		return -1;
	}

	if (errno != ENOENT)
	{
		maildir_tmpcreate_free(info);
		if (errno == EAGAIN)
			errno=EIO;
		return -1;
	}

	if ((fd=maildir_safeopen_stat(info->tmpname, O_CREAT|O_RDWR|O_TRUNC,
				      info->openmode, &stat_buf)) < 0)
	{
		maildir_tmpcreate_free(info);
		return -1;
	}

	libmail_strh_dev_t(stat_buf.st_dev, dev_buf);
	libmail_strh_ino_t(stat_buf.st_ino, ino_buf);

	if (info->newname)
		free(info->newname);

	info->newname=malloc(strlen(info->tmpname)+strlen(ino_buf)+
			     strlen(dev_buf)+3);

	if (!info->newname)
	{
		maildir_tmpcreate_free(info);
		unlink(info->tmpname);
		close(fd);
		if (errno == EAGAIN)
			errno=EIO;
		return -1;
	}

	strcpy(info->newname, maildir);
	strcat(info->newname, "/new/");
	strcat(info->newname, time_buf);
	strcat(info->newname, ".M");
	strcat(info->newname, usec_buf);
	strcat(info->newname, "P");
	strcat(info->newname, pid_buf);
	strcat(info->newname, "V");
	strcat(info->newname, dev_buf);
	strcat(info->newname, "I");
	strcat(info->newname, ino_buf);
	if (*uniq)
		strcat(strcat(info->newname, "_"), uniq);
	strcat(info->newname, ".");
	strcat(info->newname, hostname);
	strcat(info->newname, len_buf);

	return fd;
}

void maildir_tmpcreate_free(struct maildir_tmpcreate_info *info)
{
	if (info->tmpname)
		free(info->tmpname);
	info->tmpname=NULL;

	if (info->newname)
		free(info->newname);
	info->newname=NULL;
}

int maildir_movetmpnew(const char *tmpname, const char *newname)
{
	if (link(tmpname, newname) == 0)
	{
		unlink(tmpname);
		return 0;
	}

	if (errno != EXDEV)
		return -1;

	/* AFS? */

	return rename(tmpname, newname);
}
