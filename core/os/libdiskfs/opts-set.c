#include <hurd/fshelp.h>
#include "priv.h"
error_t
diskfs_set_options (const char *argz, size_t argz_len)
{
if (diskfs_runtime_argp)
return fshelp_set_options (diskfs_runtime_argp, 0, argz, argz_len, 0);
else
return EOPNOTSUPP;
}