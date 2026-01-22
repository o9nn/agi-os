#include "priv.h"
#include "trivfs_io_S.h"
kern_return_t
trivfs_S_io_revoke (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type)
{
return cred ? io_revoke (cred->realnode) : EOPNOTSUPP;
}