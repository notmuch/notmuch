#include <stdio.h>
#include "command-line-arguments.h"


int main(int argc, char **argv){

    int opt_index=1;

    int kw_val=0;
    int fl_val=0;
    int int_val=0;
    const char *pos_arg1=NULL;
    const char *pos_arg2=NULL;
    const char *string_val=NULL;
    notmuch_bool_t bool_val = FALSE;
    notmuch_bool_t fl_set = FALSE, int_set = FALSE, bool_set = FALSE,
	kw_set = FALSE, string_set = FALSE, pos1_set = FALSE, pos2_set = FALSE;

    notmuch_opt_desc_t parent_options[] = {
	{ .opt_flags = &fl_val, .name = "flag", .present = &fl_set, .keywords =
	  (notmuch_keyword_t []){ { "one",   1 << 0},
				  { "two",   1 << 1 },
				  { "three", 1 << 2 },
				  { 0, 0 } } },
	{ .opt_int = &int_val, .name = "int", .present = &int_set },
	{ }
    };

    notmuch_opt_desc_t options[] = {
	{ .opt_bool = &bool_val, .name = "boolean", .present = &bool_set },
	{ .opt_keyword = &kw_val, .name = "keyword", .present = &kw_set, .keywords =
	  (notmuch_keyword_t []){ { "zero", 0 },
				  { "one", 1 },
				  { "two", 2 },
				  { 0, 0 } } },
	{ .opt_inherit = parent_options },
	{ .opt_string = &string_val, .name = "string", .present = &string_set },
	{ .opt_position = &pos_arg1, .present = &pos1_set },
	{ .opt_position = &pos_arg2, .present = &pos2_set },
	{ }
    };

    opt_index = parse_arguments(argc, argv, options, 1);

    if (opt_index < 0)
	return 1;

    if (bool_set)
	printf("boolean %d\n", bool_val);

    if (kw_set)
	printf("keyword %d\n", kw_val);

    if (fl_set)
	printf("flags %d\n", fl_val);

    if (int_set)
	printf("int %d\n", int_val);

    if (string_set)
	printf("string %s\n", string_val);

    if (pos1_set)
	printf("positional arg 1 %s\n", pos_arg1);

    if (pos2_set)
	printf("positional arg 2 %s\n", pos_arg2);


    for ( ; opt_index < argc ; opt_index ++) {
	printf("non parsed arg %d = %s\n", opt_index, argv[opt_index]);
    }

    return 0;
}
