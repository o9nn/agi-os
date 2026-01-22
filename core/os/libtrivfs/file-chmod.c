#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_chmod (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
mode_t mode)
{
return cred ? file_chmod (cred->realnode, mode) : EOPNOTSUPP;
}