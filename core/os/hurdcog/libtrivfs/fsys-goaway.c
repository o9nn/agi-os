#include "priv.h"
#include "trivfs_fsys_S.h"
kern_return_t
trivfs_S_fsys_goaway (struct trivfs_control *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
int flags)
{
if (!cred)
return EOPNOTSUPP;
return trivfs_goaway (cred, flags);
}