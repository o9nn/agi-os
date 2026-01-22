#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_chflags (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int flags)
{
return cred ? file_chflags (cred->realnode, flags) : EOPNOTSUPP;
}