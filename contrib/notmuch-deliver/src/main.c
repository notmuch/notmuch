/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon deliverquota of maildrop which is:
 *   Copyright 1998 - 2009 Double Precision, Inc.
 *
 * This file is part of the notmuch-deliver. notmuch-deliver is free software;
 * you can redistribute it and/or modify it under the terms of the GNU General
 * Public License version 2, as published by the Free Software Foundation.
 *
 * notmuch-deliver is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <errno.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SPLICE
#include <fcntl.h>
#endif

#ifdef HAVE_SYSEXITS_H
#include <sysexits.h>
#endif

#include <glib.h>
#include <notmuch.h>

#include "maildircreate.h"
#include "maildirmisc.h"

#ifndef EX_USAGE
#define EX_USAGE 64
#endif

#ifndef EX_SOFTWARE
#define EX_SOFTWARE 70
#endif

#ifndef EX_OSERR
#define EX_OSERR 71
#endif

#ifndef EX_IOERR
#define EX_IOERR 74
#endif

#ifndef EX_TEMPFAIL
#define EX_TEMPFAIL 75
#endif

#ifndef EX_NOPERM
#define EX_NOPERM 77
#endif

#ifndef EX_CONFIG
#define EX_CONFIG 78
#endif

static gboolean opt_create, opt_fatal, opt_folder, opt_version;
static gboolean opt_verbose = FALSE;
static gchar **opt_tags = NULL;
static gchar **opt_rtags = NULL;

static GOptionEntry options[] = {
	{"version", 'V', 0, G_OPTION_ARG_NONE, &opt_version,
		"Display version", NULL},
	{"verbose", 'v', 0, G_OPTION_ARG_NONE, &opt_verbose,
		"Be verbose (useful for debugging)", NULL},
	{"create", 'c', 0, G_OPTION_ARG_NONE, &opt_create,
		"Create the maildir if it doesn't exist", NULL},
	{"folder", 'f', 0, G_OPTION_ARG_NONE, &opt_folder,
		"Add a dot before FOLDER, e.g: Local => $MAILDIR/.Local", NULL},
	{"tag", 't', 0, G_OPTION_ARG_STRING_ARRAY, &opt_tags,
		"Add a tag to the message, may be specified multiple times", "TAG"},
	{"remove-tag", 'r', 0, G_OPTION_ARG_STRING_ARRAY, &opt_rtags,
		"Remove a tag from the message, may be specified multiple times", "TAG"},
	{"fatal-add", 0, 0, G_OPTION_ARG_NONE, &opt_fatal,
		"If adding the mail to the database fails, unlink it and return non-zero", NULL},
	{NULL, 0, 0, 0, NULL, NULL, NULL},
};

static void
about(void)
{
	printf(PACKAGE"-"VERSION GITHEAD "\n");
}

static void
log_handler(G_GNUC_UNUSED const gchar *domain, GLogLevelFlags level,
	const gchar *message, G_GNUC_UNUSED gpointer user_data)
{
	g_return_if_fail(message != NULL && message[0] != '\0');

	if (!opt_verbose && (level & G_LOG_LEVEL_DEBUG))
		return;

	g_printerr(PACKAGE": %s\n", message);
}

static gboolean
load_keyfile(const gchar *path, gchar **db_path, gchar ***tags)
{
	GKeyFile *fd;
	GError *error;

	fd = g_key_file_new();
	error = NULL;
	if (!g_key_file_load_from_file(fd, path, G_KEY_FILE_NONE, &error)) {
		g_printerr("Failed to parse `%s': %s", path, error->message);
		g_error_free(error);
		g_key_file_free(fd);
		return FALSE;
	}

	*db_path = g_key_file_get_string(fd, "database", "path", &error);
	if (*db_path == NULL) {
		g_critical("Failed to parse database.path from `%s': %s", path, error->message);
		g_error_free(error);
		g_key_file_free(fd);
		return FALSE;
	}

	*tags = g_key_file_get_string_list(fd, "new", "tags", NULL, NULL);

	g_key_file_free(fd);
	return TRUE;
}

#ifdef HAVE_SPLICE
static int
save_splice(int fdin, int fdout)
{
	int ret, written, pfd[2];

	if (pipe(pfd) < 0) {
		g_critical("Failed to create pipe: %s", g_strerror(errno));
		return EX_IOERR;
	}

	for (;;) {
		ret = splice(fdin, NULL, pfd[1], NULL, 4096, 0);
		if (!ret)
			break;
		if (ret < 0) {
			g_critical("Splicing data from standard input failed: %s",
				g_strerror(errno));
			close(pfd[0]);
			close(pfd[1]);
			return EX_IOERR;
		}

		do {
			written = splice(pfd[0], NULL, fdout, NULL, ret, 0);
			if (!written) {
				g_critical("Splicing data to temporary file failed: %s",
					g_strerror(errno));
				close(pfd[0]);
				close(pfd[1]);
				return EX_IOERR;
			}
			if (written < 0) {
				g_critical("Splicing data to temporary file failed: %s",
					g_strerror(errno));
				close(pfd[0]);
				close(pfd[1]);
				return EX_IOERR;
			}
			ret -= written;
		} while (ret);
	}

	close(pfd[0]);
	close(pfd[1]);
	return 0;
}
#endif /* HAVE_SPLICE */

static int
save_readwrite(int fdin, int fdout)
{
	int ret, written;
	char buf[4096], p;

	for (;;) {
		ret = read(fdin, buf, 4096);
		if (!ret)
			break;
		if (ret < 0) {
			if (errno == EINTR)
				continue;
			g_critical("Reading from standard input failed: %s",
				g_strerror(errno));
			return EX_IOERR;
		}
		p = buf;
		do {
			written = write(fdout, p, ret);
			if (!written)
				return EX_IOERR;
			if (written < 0) {
				if (errno == EINTR)
					continue;
				g_critical("Writing to temporary file failed: %s",
					g_strerror(errno));
				return EX_IOERR;
			}
			p += written;
			ret -= written;
		} while (ret);
	}

	return 0;
}

static int
save_maildir(int fdin, const char *dir, int auto_create, char **path)
{
	int fdout, ret;
	struct maildir_tmpcreate_info info;

	maildir_tmpcreate_init(&info);
	info.openmode = 0666;
	info.maildir = dir;
	info.doordie = 1;

	while ((fdout = maildir_tmpcreate_fd(&info)) < 0)
	{
		if (errno == ENOENT && auto_create && maildir_mkdir(dir) == 0)
		{
			auto_create = 0;
			continue;
		}

		g_critical("Failed to create temporary file `%s': %s",
			info.tmpname, g_strerror(errno));
		return EX_TEMPFAIL;
	}

	g_debug("Reading from standard input and writing to `%s'", info.tmpname);
#ifdef HAVE_SPLICE
	ret = g_getenv("NOTMUCH_DELIVER_NO_SPLICE")
		? save_readwrite(fdin, fdout)
		: save_splice(fdin, fdout);
#else
	ret = save_readwrite(fdin, fdout);
#endif /* HAVE_SPLICE */
	if (ret)
		goto fail;

	close(fdout);
	g_debug("Moving `%s' to `%s'", info.tmpname, info.newname);
	if (maildir_movetmpnew(info.tmpname, info.newname)) {
		g_critical("Moving `%s' to `%s' failed: %s",
			info.tmpname, info.newname, g_strerror(errno));
		unlink(info.tmpname);
		return EX_IOERR;
	}

	if (path)
		*path = g_strdup(info.newname);

	maildir_tmpcreate_free(&info);

	return 0;

fail:
	g_debug("Unlinking `%s'", info.tmpname);
	unlink(info.tmpname);
	return EX_IOERR;
}

static int
add_tags(notmuch_message_t *message, char **tags)
{
	unsigned i;
	notmuch_status_t ret;

	if (!tags)
		return 0;

	for (i = 0; tags[i]; i++) {
		ret = notmuch_message_add_tag(message, tags[i]);
		if (ret != NOTMUCH_STATUS_SUCCESS)
			g_warning("Failed to add tag `%s': %s",
				tags[i], notmuch_status_to_string(ret));
	}

	return i;
}

static int
rm_tags(notmuch_message_t *message, char **tags)
{
	unsigned i;
	notmuch_status_t ret;

	if (!tags)
		return 0;

	for (i = 0; tags[i]; i++) {
		ret = notmuch_message_remove_tag(message, tags[i]);
		if (ret != NOTMUCH_STATUS_SUCCESS)
			g_warning("Failed to remove tag `%s': %s",
				tags[i], notmuch_status_to_string(ret));
	}

	return i;
}

static int
save_database(notmuch_database_t *db, const char *path, char **default_tags)
{
	notmuch_status_t ret;
	notmuch_message_t *message;

	g_debug("Adding `%s' to notmuch database", path);
	ret = notmuch_database_add_message(db, path, &message);
	switch (ret) {
	case NOTMUCH_STATUS_SUCCESS:
		break;
	case NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
		g_debug("Message is a duplicate, not adding tags");
		return 0;
	default:
		g_warning("Failed to add `%s' to notmuch database: %s",
			path, notmuch_status_to_string(ret));
		return EX_SOFTWARE;
	}

	g_debug("Message isn't a duplicate, adding tags");
	add_tags(message, default_tags);
	add_tags(message, opt_tags);
	rm_tags(message, opt_rtags);

	return 0;
}

int
main(int argc, char **argv)
{
	int ret;
	gchar *conf_path, *db_path, *folder, *maildir, *mail;
	gchar **conf_tags;
	GOptionContext *ctx;
	GError *error = NULL;
	notmuch_database_t *db;

	ctx = g_option_context_new("[FOLDER]");
	g_option_context_add_main_entries(ctx, options, PACKAGE);
	g_option_context_set_summary(ctx, PACKAGE"-"VERSION GITHEAD" - notmuch delivery tool");
	g_option_context_set_description(ctx,
		"\nConfiguration:\n"
		"  "PACKAGE" uses notmuch's configuration file to determine database path and\n"
		"  initial tags to add to new messages. You may set NOTMUCH_CONFIG environment\n"
		"  variable to specify an alternative configuration file.\n"
		"\nEnvironment:\n"
		"  NOTMUCH_CONFIG: Path to notmuch configuration file\n"
		"  NOTMUCH_DELIVER_NO_SPLICE: Don't use splice() even if it's available\n"
		"\nExit codes:\n"
		"  0   => Successful run\n"
		"  64  => Usage error\n"
		"  70  => Failed to open the database\n"
		"         (or to add to the database if --fatal-add is specified)\n"
		"  71  => Input output errors\n"
		"         (failed to read from standard input)\n"
		"         (failed to write to temporary file)\n"
		"  76  => Failed to open/create maildir\n"
		"  78  => Configuration error (wrt .notmuch-config)\n");

	g_log_set_default_handler(log_handler, NULL);

	if (!g_option_context_parse(ctx, &argc, &argv, &error)) {
		g_critical("Option parsing failed: %s", error->message);
		g_option_context_free(ctx);
		g_error_free(error);
		return EX_USAGE;
	}
	g_option_context_free(ctx);

	if (opt_version) {
		about();
		return 0;
	}

	if (g_getenv("NOTMUCH_CONFIG"))
		conf_path = g_strdup(g_getenv("NOTMUCH_CONFIG"));
	else if (g_getenv("HOME"))
		conf_path = g_build_filename(g_getenv("HOME"), ".notmuch-config", NULL);
	else {
		g_critical("Neither NOTMUCH_CONFIG nor HOME set");
		return EX_USAGE;
	}

	db_path = NULL;
	conf_tags = NULL;
	g_debug("Parsing configuration from `%s'", conf_path);
	if (!load_keyfile(conf_path, &db_path, &conf_tags)) {
		g_free(conf_path);
		return EX_CONFIG;
	}
	g_free(conf_path);

	if (argc > 1) {
		folder = g_strdup_printf("%s%s", opt_folder ? "." : "", argv[1]);
		maildir = g_build_filename(db_path, folder, NULL);
		g_free(folder);
	}
	else
		maildir = g_strdup(db_path);

	g_debug("Opening notmuch database `%s'", db_path);
	db = notmuch_database_open(db_path, NOTMUCH_DATABASE_MODE_READ_WRITE);
	g_free(db_path);
	if (db == NULL)
		return EX_SOFTWARE;
	if (notmuch_database_needs_upgrade(db)) {
		g_message("Upgrading database");
		notmuch_database_upgrade(db, NULL, NULL);
	}

	g_debug("Opening maildir `%s'", maildir);
	if ((ret = save_maildir(STDIN_FILENO, maildir, opt_create, &mail)) != 0) {
		g_free(maildir);
		return ret;
	}
	g_free(maildir);

	if ((ret = save_database(db, mail, conf_tags)) != 0 && opt_fatal) {
		g_warning("Unlinking `%s'", mail);
		unlink(mail);
		return ret;
	}
	g_strfreev(conf_tags);
	g_strfreev(opt_tags);
	g_strfreev(opt_rtags);
	g_free(mail);

	notmuch_database_close(db);

	return 0;
}
