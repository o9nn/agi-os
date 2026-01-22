#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_getcontrol (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t *cntl, mach_msg_type_name_t *cntltype)
{
if (!cred)
return EOPNOTSUPP;
if (!cred->isroot)
return EPERM;
*cntl = ports_get_right (cred->po->cntl);
*cntltype = MACH_MSG_TYPE_MAKE_SEND;
return 0;
}