/* notmuch - Not much of an email program, (just index and search)
 *
 * This file is part of notmuch.
 *
 * Copyright © 2011 Jani Nikula
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Jani Nikula <jani@nikula.org>
 */

#include "notmuch-client.h"
#include <sys/wait.h>

int
notmuch_run_hook (const char *db_path, const char *hook)
{
    char *hook_path;
    int status = 0;
    pid_t pid;

    hook_path = talloc_asprintf (NULL, "%s/%s/%s/%s", db_path, ".notmuch",
				 "hooks", hook);
    if (hook_path == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    /* Check access before fork() for speed and simplicity of error handling. */
    if (access (hook_path, X_OK) == -1) {
	/* Ignore ENOENT. It's okay not to have a hook, hook dir, or even
	 * notmuch dir. Dangling symbolic links also result in ENOENT, but
	 * we'll ignore that too for simplicity. */
	if (errno != ENOENT) {
	    fprintf (stderr, "Error: %s hook access failed: %s\n", hook,
		     strerror (errno));
	    status = 1;
	}
	goto DONE;
    }

    /* Flush any buffered output before forking. */
    fflush (stdout);

    pid = fork();
    if (pid == -1) {
	fprintf (stderr, "Error: %s hook fork failed: %s\n", hook,
		 strerror (errno));
	status = 1;
	goto DONE;
    } else if (pid == 0) {
	execl (hook_path, hook_path, NULL);
	/* Same as above for ENOENT, but unlikely now. Indicate all other errors
	 * to parent through non-zero exit status. */
	if (errno != ENOENT) {
	    fprintf (stderr, "Error: %s hook execution failed: %s\n", hook,
		     strerror (errno));
	    status = 1;
	}
	exit (status);
    }

    if (waitpid (pid, &status, 0) == -1) {
	fprintf (stderr, "Error: %s hook wait failed: %s\n", hook,
		 strerror (errno));
	status = 1;
	goto DONE;
    }

    if (!WIFEXITED (status) || WEXITSTATUS (status)) {
	if (WIFEXITED (status)) {
	    fprintf (stderr, "Error: %s hook failed with status %d\n",
		     hook, WEXITSTATUS (status));
	} else if (WIFSIGNALED (status)) {
	    fprintf (stderr, "Error: %s hook terminated with signal %d\n",
		     hook, WTERMSIG (status));
	}
	status = 1;
    }

  DONE:
    talloc_free (hook_path);

    return status;
}
