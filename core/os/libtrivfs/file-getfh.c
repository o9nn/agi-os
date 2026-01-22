#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_getfh (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
data_t *data, mach_msg_type_number_t *datalen)
{
return EOPNOTSUPP;
}