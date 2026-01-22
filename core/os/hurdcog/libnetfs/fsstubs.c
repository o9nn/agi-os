#include "netfs.h"
#include "fs_S.h"
#include "ifsock_S.h"
kern_return_t __attribute__((weak))
netfs_S_file_notice_changes (struct protid *user,
mach_port_t port)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
netfs_S_file_getfh (struct protid *user,
data_t *data, mach_msg_type_number_t *ndata)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
netfs_S_ifsock_getsockaddr (struct protid *user,
mach_port_t *address)
{
return EOPNOTSUPP;
}