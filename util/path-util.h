/*
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef NOTMUCH_UTIL_PATH_UTIL_H_
#define NOTMUCH_UTIL_PATH_UTIL_H_

#include "notmuch.h"

#ifdef __cplusplus
extern "C" {
#endif

char *
notmuch_canonicalize_file_name (const char *path);

notmuch_status_t
mkdir_recursive (const void *ctx, const char *path, int mode, char **status_string);

notmuch_status_t
sync_dir (const char *path, char **status_string);

#ifdef __cplusplus
}
#endif

#endif /* NOTMUCH_UTIL_PATH_UTIL_H_ */
