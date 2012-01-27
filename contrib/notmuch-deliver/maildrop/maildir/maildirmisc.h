#ifndef	maildirmisc_h
#define	maildirmisc_h

/*
** Copyright 2000-2003 Double Precision, Inc.
** See COPYING for distribution information.
*/

#if	HAVE_CONFIG_H
#include	"config.h"
#endif

#if HAVE_SYS_STAT_H
#include	<sys/stat.h>
#endif

#ifdef  __cplusplus
extern "C" {
#endif


/*
**
** Miscellaneous maildir-related code
**
*/

/* Some special folders */

#define	INBOX	"INBOX"
#define	DRAFTS	"Drafts"
#define	SENT	"Sent"
#define	TRASH	"Trash"
#define	SHARED	"shared"

#define	SHAREDSUBDIR	"shared-folders"

#define NEWSHAREDSP "#shared"
#define	NEWSHARED "#shared."

#define PUBLIC "public" /* SMAP */

int maildir_make(const char *maildir, int perm, int subdirperm,
		int folder);

int maildir_del(const char *maildir);

int maildir_del_content(const char *maildir);

char *maildir_name2dir(const char *maildir,	/* DIR location */
		       const char *foldername); /* INBOX.name */

char *maildir_location(const char *homedir,
		       const char *maildir);
/*
** Homedir is the account's home directory, "maildir" is where the account's
** default Maildir is configured to be (usually "./Maildir").  Combine the
** two to produce an absolute pathname.
*/


char *maildir_folderdir(const char *,		/* maildir */
	const char *);				/* folder name */
	/* Returns the directory corresponding to foldername (foldername is
	** checked to make sure that it's a valid name, else we set errno
	** to EINVAL, and return (0).
	*/

char *maildir_filename(const char *,		/* maildir */
	const char *,				/* folder */
	const char *);				/* filename */
	/*
	** Builds the filename to this message, suitable for opening.
	** If the file doesn't appear to be there, search the maildir to
	** see if someone changed the flags, and return the current filename.
	*/

int maildir_safeopen(const char *,		/* filename */
	int,				/* mode */
	int);				/* perm */

/*
**	Same arguments as open().  When we're accessing a shared maildir,
**	prevent someone from playing cute and dumping a bunch of symlinks
**	in there.  This function will open the indicate file only if the
**	last component is not a symlink.
**	This is implemented by opening the file with O_NONBLOCK (to prevent
**	a DOS attack of someone pointing the symlink to a pipe, causing
**	the open to hang), clearing O_NONBLOCK, then stat-int the file
**	descriptor, lstating the filename, and making sure that dev/ino
**	match.
*/

int maildir_semisafeopen(const char *,	/* filename */
	int,				/* mode */
	int);				/* perm */

/*
** Same thing, except that we allow ONE level of soft link indirection,
** because we're reading from our own maildir, which points to the
** message in the sharable maildir.
*/

int maildir_safeopen_stat(const char *path, int mode, int perm,
			  struct stat *stat1);
	/* Sane as maildir_safeopen(), except that we also initialize a
	** struct stat, saving an extra syscall to the caller.
	*/

int maildir_mkdir(const char *);	/* directory */
/*
** Create maildir including all subdirectories in the path (like mkdir -p)
*/

void maildir_purgetmp(const char *);		/* maildir */
	/* purges old stuff out of tmp */

void maildir_purge(const char *,		/* directory */
	unsigned);				/* time_t to purge */

void maildir_getnew(const char *,		/* maildir */
	const char *,				/* folder */
	void (*)(const char *, void *),		/* Callback function for
						** every moved msg.
						*/
	void *arg);				/* Passthrough callback arg */

	/* move messages from new to cur */

int maildir_deletefolder(const char *,		/* maildir */
	const char *);				/* folder */
	/* deletes a folder */

void maildir_list(const char *maildir,
		  void (*func)(const char *, void *),
		  void *voidp);

void maildir_list_sharable(const char *,	/* maildir */
	void (*)(const char *, void *),		/* callback function */
	void *);				/* 2nd arg to callback func */
	/* list sharable folders */

int maildir_shared_subscribe(const char *,	/* maildir */
		const char *);			/* folder */
	/* subscribe to a shared folder */

void maildir_list_shared(const char *,		/* maildir */
	void (*)(const char *, void *),		/* callback function */
	void *);			/* 2nd arg to the callback func */
	/* list subscribed folders */

int maildir_shared_unsubscribe(const char *,	/* maildir */
		const char *);			/* folder */
	/* unsubscribe from a shared folder */

char *maildir_shareddir(const char *,		/* maildir */
	const char *);				/* folder */
	/*
	** Validate and return a path to a shared folder.  folderdir must be
	** a name of a valid shared folder.
	*/

void maildir_shared_sync(const char *);		/* maildir */
	/* "sync" the shared folder */

int maildir_sharedisro(const char *);		/* maildir */
	/* maildir is a shared read-only folder */

int maildir_unlinksharedmsg(const char *);	/* filename */
	/* Remove a message from a shared folder */

/* Internal function that reads a symlink */

char *maildir_getlink(const char *);

	/* Determine whether the maildir filename has a certain flag */

int maildir_hasflag(const char *filename, char);

#define	MAILDIR_DELETED(f)	maildir_hasflag((f), 'T')

	/*
	** Hierarchical maildir rename.
	*/

#define MAILDIR_RENAME_FOLDER 1
#define MAILDIR_RENAME_SUBFOLDERS 2

int maildir_rename(const char *maildir, /* Path to the maildir */
		   const char *oldname, /* .foldername */
		   const char *newname, /* .foldername */
		   int flags, /* See above */
		   void (*callback_func)(const char *old_path,
					 const char *new_path)
		   );

#ifdef  __cplusplus
}
#endif

#endif
