#ifndef DUMP_RESTORE_PRIVATE_H
#define DUMP_RESTORE_PRIVATE_H

#include "hex-escape.h"
#include "command-line-arguments.h"

typedef enum dump_formats {
    DUMP_FORMAT_AUTO,
    DUMP_FORMAT_BATCH_TAG,
    DUMP_FORMAT_SUP
} dump_format_t;

#endif
