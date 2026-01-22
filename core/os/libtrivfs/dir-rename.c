#include "priv.h"
kern_return_t
trivfs_S_dir_rename (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
const_string_t name,
struct trivfs_protid *cred2, const_string_t name2, int excl)
{
if (!cred)
return EOPNOTSUPP;
if (!cred2)
return EXDEV;
return ENOTDIR;
}