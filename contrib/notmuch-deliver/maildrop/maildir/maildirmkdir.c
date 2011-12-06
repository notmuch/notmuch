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
#if	HAVE_UNISTD_H
#include	<unistd.h>
#endif
#include	<errno.h>

#include	"maildirmisc.h"


int maildir_mkdir(const char *dir)
{
char	*buf, *p;
size_t	l;

	if (dir == 0 || dir[0] == 0)
	{
		errno = EINVAL;
		return (-1);
	}
	l = strlen(dir);
	if ((buf = malloc(l + sizeof("/tmp"))) == 0)
	{
		errno = ENOMEM;
		return (-1);
	}
	strcpy(buf, dir);
	strcpy(buf+l, "/cur");

	/* We do mkdir -p here */

	p = buf+1;
	while ((p = strchr(p, '/')) != 0)
	{
		*p = '\0';
		if (mkdir(buf, 0700) < 0 && errno != EEXIST)
		{
			free(buf);
			return (-1);
		}
		*p++ = '/';
	}

	if (mkdir(buf, 0700) < 0 && errno != EEXIST) {
		free(buf);
		return (-1);
	}
	strcpy(buf+l, "/new");
	if (mkdir(buf, 0700) < 0 && errno != EEXIST) {
		free(buf);
		return (-1);
	}
	/*
	 *  make /tmp last because this is the one we open first -
	 *  the existence of this directory implies the whole
	 *  Maildir structure is complete
	 */
	strcpy(buf+l, "/tmp");
	if (mkdir(buf, 0700) < 0 && errno != EEXIST) {
		free(buf);
		return (-1);
	}
	free(buf);
	return (0);
}

