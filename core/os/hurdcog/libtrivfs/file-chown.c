#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_chown (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
uid_t uid, gid_t gid)
{
return cred ? file_chown (cred->realnode, uid, gid) : EOPNOTSUPP;
}