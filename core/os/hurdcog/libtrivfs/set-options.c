#include <hurd/fshelp.h>
#include "priv.h"
error_t
trivfs_set_options (struct trivfs_control *fsys, const char *argz, size_t argz_len)
{
if (trivfs_runtime_argp)
return fshelp_set_options (trivfs_runtime_argp, 0, argz, argz_len, fsys);
else
return EOPNOTSUPP;
}