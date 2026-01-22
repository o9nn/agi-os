#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_syncfs (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int wait,
int dochildren)
{
return cred ? file_sync (cred->realnode, wait, 0) : EOPNOTSUPP;
}