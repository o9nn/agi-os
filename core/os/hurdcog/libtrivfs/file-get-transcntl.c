#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_get_translator_cntl (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t *cntl,
mach_msg_type_name_t *cntl_type)
{
return EOPNOTSUPP;
}