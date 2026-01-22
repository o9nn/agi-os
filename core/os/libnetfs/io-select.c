#include "netfs.h"
#include "io_S.h"
#include <hurd/ports.h>
kern_return_t
netfs_S_io_select (struct protid *user,
mach_port_t reply,
mach_msg_type_name_t replytype,
int *type)
{
if (!user)
return EOPNOTSUPP;
*type &= ~SELECT_URG;
return 0;
}
kern_return_t
netfs_S_io_select_timeout (struct protid *user,
mach_port_t reply,
mach_msg_type_name_t replytype,
struct timespec ts,
int *type)
{
return netfs_S_io_select (user, reply, replytype, type);
}