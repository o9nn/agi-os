#include "priv.h"
#include "trivfs_fs_S.h"
#include "trivfs_io_S.h"
kern_return_t
trivfs_S_file_reparent (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t parent,
mach_port_t *new, mach_msg_type_name_t *new_type)
{
error_t ret = trivfs_S_io_duplicate (cred, reply, reply_type, new, new_type);
if (!ret)
mach_port_deallocate (mach_task_self (), parent);
return ret;
}