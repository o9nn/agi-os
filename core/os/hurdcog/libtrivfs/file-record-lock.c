#include "priv.h"
#include <stdio.h>
kern_return_t
trivfs_S_file_record_lock (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int cmd, struct flock64 *lock,
mach_port_t rendezvous)
{
return EOPNOTSUPP;
}