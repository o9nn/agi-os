#include "priv.h"
void
trivfs_clean_cntl (void *arg)
{
struct trivfs_control *cntl = arg;
mach_port_destroy (mach_task_self (), cntl->filesys_id);
mach_port_destroy (mach_task_self (), cntl->file_id);
mach_port_deallocate (mach_task_self (), cntl->underlying);
trivfs_remove_control_port_class (cntl->pi.class);
trivfs_remove_port_bucket (cntl->pi.bucket);
trivfs_remove_protid_port_class (cntl->protid_class);
trivfs_remove_port_bucket (cntl->protid_bucket);
}