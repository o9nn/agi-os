#include "priv.h"
#include "fsys_S.h"
kern_return_t
diskfs_S_fsys_forward (mach_port_t server,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t requestor, const_data_t argz,
mach_msg_type_number_t argz_len)
{
return EOPNOTSUPP;
}