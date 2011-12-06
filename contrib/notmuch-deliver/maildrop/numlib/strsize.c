/*
** Copyright 2001 Double Precision, Inc.
** See COPYING for distribution information.
*/

#if	HAVE_CONFIG_H
#include	"config.h"
#endif
#include	"numlib.h"
#include	<string.h>


static void cat_n(char *buf, unsigned long n)
{
char    bb[NUMBUFSIZE+1];
char    *p=bb+sizeof(bb)-1;

        *p=0;
        do
        {
                *--p = "0123456789"[n % 10];
                n=n/10;
        } while (n);
        strcat(buf, p);
}

char *libmail_str_sizekb(unsigned long n, char *sizebuf)
{
        /* If size is less than 1K bytes, display it as 0.xK */

        if (n < 1024)
        {
                strcpy(sizebuf, "0.");
                cat_n(sizebuf, (int)(10 * n / 1024 ));
                strcat(sizebuf, "K");
        }
        /* If size is less than 1 meg, display is as xK */

        else if (n < 1024 * 1024)
        {
                *sizebuf=0;
                cat_n(sizebuf, (unsigned long)(n+512)/1024);
                strcat(sizebuf, "K");
        }

        /* Otherwise, display in megabytes */

        else
        {
        unsigned long nm=(double)n / (1024.0 * 1024.0) * 10;

                *sizebuf=0;
                cat_n( sizebuf, nm / 10);
                strcat(sizebuf, ".");
                cat_n( sizebuf, nm % 10);
                strcat(sizebuf, "M");
        }

	return (sizebuf);
}

