#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_get_translator (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
data_t *trans,
mach_msg_type_name_t *translen)
{
return EOPNOTSUPP;
}