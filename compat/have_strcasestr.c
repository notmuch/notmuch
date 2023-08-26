#define _GNU_SOURCE
#include <strings.h> /* strcasecmp() in POSIX */
#include <string.h> /* strcasecmp() in *BSD */

int
main ()
{
    char *found;
    const char *haystack, *needle;

    found = strcasestr (haystack, needle);
}
