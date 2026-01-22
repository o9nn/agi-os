#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_get_storage_info (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
mach_port_t **ports,
mach_msg_type_name_t *ports_type,
mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
off_t **offsets,
mach_msg_type_number_t *num_offsets,
data_t *data, mach_msg_type_number_t *data_len)
{
return EOPNOTSUPP;
}