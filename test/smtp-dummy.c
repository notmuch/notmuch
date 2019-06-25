/* smtp-dummy - Dummy SMTP server that delivers mail to the given file
 *
 * Copyright © 2010 Carl Worth
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
 * Authors: Carl Worth <cworth@cworth.org>
 */

/* This (non-compliant) SMTP server listens on localhost, port 25025
 * and delivers a mail received to the given filename, (specified as a
 * command-line argument). It exists after the first client connection
 * completes.
 *
 * It implements very little of the SMTP protocol, even less than
 * specified as the minimum implementation in the SMTP RFC, (not
 * implementing RSET, NOOP, nor VRFY). And it doesn't do any
 * error-checking on the input.
 *
 * That is to say, if you use this program, you will very likely find
 * cases where it doesn't do everything your SMTP client expects. You
 * have been warned.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>

#define STRNCMP_LITERAL(var, literal) \
    strncmp ((var), (literal), sizeof (literal) - 1)

static void
receive_data_to_file (FILE *peer, FILE *output)
{
    char *line = NULL;
    size_t line_size;
    ssize_t line_len;

    while ((line_len = getline (&line, &line_size, peer)) != -1) {
	if (STRNCMP_LITERAL (line, ".\r\n") == 0)
	    break;
	if (line_len < 2)
	    continue;
	if (line[line_len - 1] == '\n' && line[line_len - 2] == '\r') {
	    line[line_len - 2] = '\n';
	    line[line_len - 1] = '\0';
	}
	fprintf (output, "%s",
		 line[0] == '.' ? line + 1 : line);
    }

    free (line);
}

static int
process_command (FILE *peer, FILE *output, const char *command)
{
    if (STRNCMP_LITERAL (command, "EHLO ") == 0) {
	fprintf (peer, "502 not implemented\r\n");
	fflush (peer);
    } else if (STRNCMP_LITERAL (command, "HELO ") == 0) {
	fprintf (peer, "250 localhost\r\n");
	fflush (peer);
    } else if (STRNCMP_LITERAL (command, "MAIL FROM:") == 0 ||
	       STRNCMP_LITERAL (command, "RCPT TO:") == 0) {
	fprintf (peer, "250 OK\r\n");
	fflush (peer);
    } else if (STRNCMP_LITERAL (command, "DATA") == 0) {
	fprintf (peer, "354 End data with <CR><LF>.<CR><LF>\r\n");
	fflush (peer);
	receive_data_to_file (peer, output);
	fprintf (peer, "250 OK\r\n");
	fflush (peer);
    } else if (STRNCMP_LITERAL (command, "QUIT") == 0) {
	fprintf (peer, "221 BYE\r\n");
	fflush (peer);
	return 1;
    } else {
	fprintf (stderr, "Unknown command: %s\n", command);
    }
    return 0;
}

static void
do_smtp_to_file (FILE *peer, FILE *output)
{
    char *line = NULL;
    size_t line_size;
    ssize_t line_len;

    fprintf (peer, "220 localhost smtp-dummy\r\n");
    fflush (peer);

    while ((line_len = getline (&line, &line_size, peer)) != -1) {
	if (process_command (peer, output, line))
	    break;
    }

    free (line);
}

int
main (int argc, char *argv[])
{
    const char *progname;
    char *output_filename;
    FILE *peer_file = NULL, *output = NULL;
    int sock = -1, peer, err;
    struct sockaddr_in addr, peer_addr;
    struct hostent *hostinfo;
    socklen_t peer_addr_len;
    int reuse;
    int background;
    int ret = 0;
    socklen_t addrlen;

    progname = argv[0];

    background = 0;
    for (; argc >= 2; argc--, argv++) {
	if (argv[1][0] != '-')
	    break;
	if (strcmp (argv[1], "--") == 0) {
	    argc--;
	    argv++;
	    break;
	}
	if (strcmp (argv[1], "--background") == 0) {
	    background = 1;
	    continue;
	}
	fprintf (stderr, "%s: unregognized option '%s'\n",
		 progname, argv[1]);
	return 1;
    }

    if (argc != 2) {
	fprintf (stderr,
		 "Usage: %s [--background] <output-file>\n", progname);
	return 1;
    }

    output_filename = argv[1];
    output = fopen (output_filename, "w");
    if (output == NULL) {
	fprintf (stderr, "Failed to open %s for writing: %s\n",
		 output_filename, strerror (errno));
	ret = 1;
	goto DONE;
    }

    sock = socket (AF_INET, SOCK_STREAM, 0);
    if (sock == -1) {
	fprintf (stderr, "Error: socket() failed: %s\n",
		 strerror (errno));
	ret = 1;
	goto DONE;
    }

    reuse = 1;
    err = setsockopt (sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof (reuse));
    if (err) {
	fprintf (stderr, "Error: setsockopt() failed: %s\n",
		 strerror (errno));
	ret = 1;
	goto DONE;
    }

    hostinfo = gethostbyname ("localhost");
    if (hostinfo == NULL) {
	fprintf (stderr, "Unknown host: localhost\n");
	ret = 1;
	goto DONE;
    }

    memset (&addr, 0, sizeof (addr));
    addr.sin_family = AF_INET;
    addr.sin_port = 0;
    addr.sin_addr = *(struct in_addr *) hostinfo->h_addr;
    err = bind (sock, (struct sockaddr *) &addr, sizeof (addr));
    if (err) {
	fprintf (stderr, "Error: bind() failed: %s\n",
		 strerror (errno));
	close (sock);
	ret = 1;
	goto DONE;
    }

    addrlen = sizeof (addr);
    err = getsockname (sock, (struct sockaddr *) &addr, &addrlen);
    if (err) {
	fprintf (stderr, "Error: getsockname() failed: %s\n",
		 strerror (errno));
	close (sock);
	ret = 1;
	goto DONE;
    }

    printf ("smtp_dummy_port='%d'\n", ntohs (addr.sin_port));

    err = listen (sock, 1);
    if (err) {
	fprintf (stderr, "Error: listen() failed: %s\n",
		 strerror (errno));
	close (sock);
	ret = 1;
	goto DONE;
    }

    if (background) {
	int pid = fork ();
	if (pid > 0) {
	    printf ("smtp_dummy_pid='%d'\n", pid);
	    fflush (stdout);
	    close (sock);
	    ret = 0;
	    goto DONE;
	}
	if (pid < 0) {
	    fprintf (stderr, "Error: fork() failed: %s\n",
		     strerror (errno));
	    close (sock);
	    ret = 1;
	    goto DONE;
	}
	/* Reached if pid == 0 (the child process). */
	/* Close stdout so that the one interested in pid value will
	 * also get EOF. */
	close (STDOUT_FILENO);
	/* dup2() will re-reserve fd of stdout (1) (opportunistically),
	 * in case fd of stderr (2) is open. If that was not open we
	 * don't care fd of stdout (1) either. */
	dup2 (STDERR_FILENO, STDOUT_FILENO);

	/* This process is now out of reach of shell's job control.
	 * To resolve the rare but possible condition where this
	 * "daemon" is started but never connected this process will
	 * (only) have 30 seconds to exist. */
	alarm (30);
    }

    peer_addr_len = sizeof (peer_addr);
    peer = accept (sock, (struct sockaddr *) &peer_addr, &peer_addr_len);
    if (peer == -1) {
	fprintf (stderr, "Error: accept() failed: %s\n",
		 strerror (errno));
	ret = 1;
	goto DONE;
    }

    peer_file = fdopen (peer, "w+");
    if (peer_file == NULL) {
	fprintf (stderr, "Error: fdopen() failed: %s\n",
		 strerror (errno));
	ret = 1;
	goto DONE;
    }

    do_smtp_to_file (peer_file, output);

  DONE:
    if (output)
	fclose (output);
    if (peer_file)
	fclose (peer_file);
    if (sock >= 0)
	close (sock);

    return ret;
}
