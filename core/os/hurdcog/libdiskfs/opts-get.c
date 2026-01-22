#include "priv.h"
error_t
diskfs_append_args (char **argz, size_t *argz_len)
{
return diskfs_append_std_options (argz, argz_len);
}