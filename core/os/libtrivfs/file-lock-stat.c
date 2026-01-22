#include "priv.h"
#include "trivfs_fs_S.h"
#include <fcntl.h>
#include <sys/file.h>
kern_return_t
trivfs_S_file_lock_stat (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int *mystatus, int *otherstatus)
{
return EOPNOTSUPP;
}