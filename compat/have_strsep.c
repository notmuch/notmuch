#define _GNU_SOURCE
#include <string.h>

int main()
{
    char *found;
    char **stringp;
    const char *delim;

    found = strsep(stringp, delim);
}
