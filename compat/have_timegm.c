#include <time.h>
#include "compat.h"

int main()
{
    return (int) timegm((struct tm *)0);
}
