#define _GNU_SOURCE
#include <stdio.h>
#include <sys/types.h>

int main()
{
    ssize_t count = 0;
    size_t n = 0;
    char **lineptr = NULL;
    FILE *stream = NULL;

    count = getline(lineptr, &n, stream);
}
