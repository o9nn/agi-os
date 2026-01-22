#include "priv.h"
kern_return_t
trivfs_S_dir_notice_changes (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t notify)
{
return cred ? ENOTDIR : EOPNOTSUPP;
}