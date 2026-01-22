#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_get_storage_info (struct protid *user,
mach_port_t **ports,
mach_msg_type_name_t *ports_type,
mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
off_t **offsets,
mach_msg_type_number_t *num_offsets,
data_t *data, mach_msg_type_number_t *data_len)
{
error_t err;
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_file_get_storage_info (user->user, user->po->np, ports,
ports_type, num_ports, ints,
num_ints, offsets, num_offsets,
data, data_len);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}