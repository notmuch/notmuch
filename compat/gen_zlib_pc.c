#include <stdio.h>
#include <zlib.h>

static const char *template =
	"prefix=/usr\n"
	"exec_prefix=${prefix}\n"
	"libdir=${exec_prefix}/lib\n"
	"\n"
	"Name: zlib\n"
	"Description: zlib compression library\n"
	"Version: %s\n"
	"Libs: -lz\n";

int main(void)
{
	printf(template, ZLIB_VERSION);
	return 0;
}
