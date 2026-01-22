#include "priv.h"
kern_return_t
trivfs_S_dir_mkdir (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
const_string_t name, mode_t mode)
{
return cred ? ENOTDIR : EOPNOTSUPP;
}