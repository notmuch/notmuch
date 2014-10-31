#include <stdio.h>
#include "command-line-arguments.h"


int main(int argc, char **argv){

    int opt_index=1;

    int kw_val=0;
    int fl_val=0;
    int int_val=0;
    char *pos_arg1=NULL;
    char *pos_arg2=NULL;
    char *string_val=NULL;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_KEYWORD, &kw_val, "keyword", 'k',
	  (notmuch_keyword_t []){ { "one", 1 },
				  { "two", 2 },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_KEYWORD_FLAGS, &fl_val, "flag", 'f',
	  (notmuch_keyword_t []){ { "one",   1 << 0},
				  { "two",   1 << 1 },
				  { "three", 1 << 2 },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_INT, &int_val, "int", 'i', 0},
	{ NOTMUCH_OPT_STRING, &string_val, "string", 's', 0},
	{ NOTMUCH_OPT_POSITION, &pos_arg1, 0,0, 0},
	{ NOTMUCH_OPT_POSITION, &pos_arg2, 0,0, 0},
	{ 0, 0, 0, 0, 0 } };

    opt_index = parse_arguments(argc, argv, options, 1);

    if (opt_index < 0)
	return 1;

    if (kw_val)
	printf("keyword %d\n", kw_val);

    if (fl_val)
	printf("flags %d\n", fl_val);

    if (int_val)
	printf("int %d\n", int_val);

    if (string_val)
	printf("string %s\n", string_val);

    if (pos_arg1)
	printf("positional arg 1 %s\n", pos_arg1);

    if (pos_arg2)
	printf("positional arg 2 %s\n", pos_arg2);


    for ( ; opt_index < argc ; opt_index ++) {
	printf("non parsed arg %d = %s\n", opt_index, argv[opt_index]);
    }

    return 0;
}
