#include <time.h>
#include <stdio.h>

int main()
{
    struct tm tm;

    (void) asctime_r (&tm, NULL);

    return (0);
}
