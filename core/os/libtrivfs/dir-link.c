#include "priv.h"
kern_return_t
trivfs_S_dir_link (struct trivfs_protid *dir,
mach_port_t reply, mach_msg_type_name_t reply_type,
struct trivfs_protid *file, const_string_t name, int excl)
{
if (!file)
return EOPNOTSUPP;
if (!dir)
return EXDEV;
return ENOTDIR;
}