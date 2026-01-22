#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_set_translator (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int actflags,
int passflags,
int oldtransflags,
const_data_t trans,
mach_msg_type_number_t translen,
mach_port_t existing)
{
return EOPNOTSUPP;
}