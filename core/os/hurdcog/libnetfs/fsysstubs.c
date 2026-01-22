#include "netfs.h"
#include "fsys_S.h"
kern_return_t __attribute__((weak))
netfs_S_fsys_getfile (struct netfs_control *cntl,
mach_port_t reply,
mach_msg_type_name_t reply_type,
const uid_t *uids, mach_msg_type_number_t nuids,
const gid_t *gids, mach_msg_type_number_t ngids,
const_data_t handle, mach_msg_type_number_t handlelen,
mach_port_t *file, mach_msg_type_name_t *filetype)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
netfs_S_fsys_getpriv (struct netfs_control *cntl,
mach_port_t reply,
mach_msg_type_name_t reply_type,
mach_port_t *host, mach_msg_type_name_t *hosttp,
mach_port_t *dev, mach_msg_type_name_t *devtp,
mach_port_t *fs, mach_msg_type_name_t *fstp)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
netfs_S_fsys_init (struct netfs_control *cntl,
mach_port_t reply,
mach_msg_type_name_t reply_type,
mach_port_t proc, auth_t auth)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
netfs_S_fsys_forward (mach_port_t cntl,
mach_port_t reply,
mach_msg_type_name_t reply_type,
mach_port_t request,
const_data_t argv, mach_msg_type_number_t argvlen)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
netfs_S_fsys_startup (mach_port_t bootstrap,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int flags,
mach_port_t contrl,
mach_port_t *realnod,
mach_msg_type_name_t *realnodetype)
{
return EOPNOTSUPP;
}