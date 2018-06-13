#include <dirent.h>

int main()
{
    struct dirent ent;

    (void) ent.d_type;

    return 0;
}
