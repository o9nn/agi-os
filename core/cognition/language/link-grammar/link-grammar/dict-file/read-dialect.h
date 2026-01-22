#ifndef _READ_DIALECT_H_
#define _READ_DIALECT_H_
#include "api-types.h"
#include "dict-common/dict-common.h"
bool dialect_file_read(Dictionary, const char *);
bool dialect_read_from_one_line_str(Dictionary, Dialect *, const char *);
#endif