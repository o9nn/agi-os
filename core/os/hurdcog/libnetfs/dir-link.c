#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_dir_link (struct protid *diruser, struct protid *fileuser,
const_string_t name, int excl)
{
error_t err;
if (!diruser)
return EOPNOTSUPP;
if (!fileuser)
return EXDEV;
err = netfs_attempt_link (diruser->user, diruser->po->np,
fileuser->po->np, name, excl);
if (!err)
mach_port_deallocate (mach_task_self (), fileuser->pi.port_right);
return err;
}