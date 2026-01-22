#include "priv.h"
#include "trivfs_fsys_S.h"
kern_return_t __attribute__((weak))
trivfs_S_fsys_startup (mach_port_t bootport,
mach_port_t reply,
mach_msg_type_name_t replytype,
int flags,
mach_port_t cntl,
mach_port_t *realnode,
mach_msg_type_name_t *realnodetype)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_fsys_getpriv (struct trivfs_control *cntl,
mach_port_t reply,
mach_msg_type_name_t replytype,
mach_port_t *host, mach_msg_type_name_t *host_privPoly,
mach_port_t *dev, mach_msg_type_name_t *devPoly,
mach_port_t *fstask, mach_msg_type_name_t *fstPoly)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_fsys_init (struct trivfs_control *control,
mach_port_t reply,
mach_msg_type_name_t replytype,
mach_port_t proc,
auth_t auth)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_fsys_getfile (struct trivfs_control *cntl,
mach_port_t reply,
mach_msg_type_name_t replytype,
const uid_t *genuids,
mach_msg_type_number_t ngenuids,
const uid_t *gengids,
mach_msg_type_number_t ngengids,
const_data_t handle,
mach_msg_type_number_t handlesize,
mach_port_t *file,
mach_msg_type_name_t *filetype)
{
return EOPNOTSUPP;
}