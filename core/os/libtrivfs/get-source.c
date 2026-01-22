#include "priv.h"
error_t __attribute__ ((weak))
trivfs_get_source (char *source, size_t source_len)
{
return EOPNOTSUPP;
}