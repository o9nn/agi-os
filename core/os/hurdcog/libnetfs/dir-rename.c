#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_dir_rename (struct protid *fromdiruser, const_string_t fromname,
struct protid *todiruser, const_string_t toname, int excl)
{
error_t err;
if (!fromdiruser)
return EOPNOTSUPP;
if (!todiruser)
return EXDEV;
err = netfs_attempt_rename (fromdiruser->user, fromdiruser->po->np,
fromname, todiruser->po->np, toname, excl);
if (!err)
mach_port_deallocate (mach_task_self (), todiruser->pi.port_right);
return err;
}