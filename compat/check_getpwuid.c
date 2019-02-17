#include <stdio.h>
#include <pwd.h>

int main()
{
    struct passwd passwd, *ignored;

    (void) getpwuid_r (0, &passwd, NULL, 0, &ignored);

    return (0);
}
