#define _GNU_SOURCE
#include <strings.h>

int main()
{
    char *found;
    const char *haystack, *needle;

    found = strcasestr(haystack, needle);
}
