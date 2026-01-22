#include "netfs.h"
error_t
netfs_set_options (const char *argz, size_t argz_len)
{
if (netfs_runtime_argp)
return fshelp_set_options (netfs_runtime_argp, 0, argz, argz_len, 0);
else
return EOPNOTSUPP;
}