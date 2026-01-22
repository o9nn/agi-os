#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_check_access (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int *allowed)
{
if (! cred)
return EOPNOTSUPP;
if (! trivfs_check_access_hook)
file_check_access (cred->realnode, allowed);
else
(*trivfs_check_access_hook) (cred->po->cntl, cred->user,
cred->realnode, allowed);
return 0;
}