#include "netfs.h"
#include "fsys_S.h"
#include "fsys_reply_U.h"
#include <stdlib.h>
#include <errno.h>
#include <hurd/ports.h>
kern_return_t
netfs_S_fsys_goaway (struct netfs_control *pt,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int flags)
{
error_t err;
if (!pt)
return EOPNOTSUPP;
err = netfs_shutdown (flags);
if (! err)
{
fsys_goaway_reply (reply, reply_type, 0);
exit (0);
}
return err;
}