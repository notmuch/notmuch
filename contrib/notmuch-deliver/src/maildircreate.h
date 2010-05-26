#ifndef	maildircreate_h
#define	maildircreate_h

/*
** Copyright 1998 - 2003 Double Precision, Inc.
** See COPYING for distribution information.
*/

#if	HAVE_CONFIG_H
#include	"config.h"
#endif

#include	<stdio.h>

#ifdef  __cplusplus
extern "C" {
#endif

static const char maildircreate_h_rcsid[]="$Id: maildircreate.h,v 1.10 2006/10/29 00:03:53 mrsam Exp $";

	/* Create messages in maildirs */

struct maildir_tmpcreate_info {
	const char *maildir;
	unsigned long msgsize;  /* If known, 0 otherwise (must use requota later)*/
	const char *uniq;	/* You need when creating multiple msgs */
	const char *hostname;	/* If known, NULL otherwise */
	int openmode;		/* Default open mode */
	int doordie;		/* Loop until we get it right. */
	char *tmpname;	/* On exit, filename in tmp */
	char *newname; /* On exit, filename in new */
};

#define maildir_tmpcreate_init(i) \
	do \
	{ \
		memset( (i), 0, sizeof(*(i))); \
		(i)->openmode=0644; \
	} while(0)

int maildir_tmpcreate_fd(struct maildir_tmpcreate_info *);
FILE *maildir_tmpcreate_fp(struct maildir_tmpcreate_info *);
void maildir_tmpcreate_free(struct maildir_tmpcreate_info *);

	/* Move created message from tmp to new */
int maildir_movetmpnew(const char *tmpname, const char *newname);

#ifdef  __cplusplus
}
#endif

#endif
