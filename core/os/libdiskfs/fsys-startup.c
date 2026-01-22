#include "priv.h"
#include "fsys_S.h"
kern_return_t
diskfs_S_fsys_startup (mach_port_t port,
mach_port_t reply,
mach_msg_type_name_t replytype,
int flags,
mach_port_t ctl,
mach_port_t *real,
mach_msg_type_name_t *realpoly)
{
return diskfs_execboot_fsys_startup (port, flags, ctl, real, realpoly);
}