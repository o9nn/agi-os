#include "priv.h"
kern_return_t
trivfs_S_dir_readdir (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
data_t *data,
size_t *datalen,
boolean_t *data_dealloc,
int entry,
int nentries,
vm_size_t bufsiz,
int *amount)
{
return cred ? ENOTDIR : EOPNOTSUPP;
}