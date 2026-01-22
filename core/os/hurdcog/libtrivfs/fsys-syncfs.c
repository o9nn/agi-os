#include "priv.h"
#include "trivfs_fsys_S.h"
kern_return_t
trivfs_S_fsys_syncfs (struct trivfs_control *cntl,
mach_port_t reply,
mach_msg_type_name_t replytype,
int wait,
int dochildren)
{
return cntl ? file_sync (cntl->underlying, wait, 0) : EOPNOTSUPP;
}