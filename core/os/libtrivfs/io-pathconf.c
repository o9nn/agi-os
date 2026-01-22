#include "priv.h"
#include "trivfs_io_S.h"
kern_return_t
trivfs_S_io_pathconf (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int name, int *val)
{
if (cred)
return io_pathconf (cred->realnode, name, val);
else
return EOPNOTSUPP;
}