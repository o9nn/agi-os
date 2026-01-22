#include "netfs.h"
error_t
netfs_append_args (char **argz, size_t *argz_len)
{
return netfs_append_std_options (argz, argz_len);
}