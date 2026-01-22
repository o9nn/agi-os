#include "priv.h"
kern_return_t
trivfs_S_dir_mkfile (struct trivfs_protid *file,
mach_port_t reply, mach_msg_type_name_t reply_type,
int flags,
mode_t mode,
mach_port_t *newnod,
mach_msg_type_name_t *newnodetype)
{
return file ? ENOTDIR : EOPNOTSUPP;
}