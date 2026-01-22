#include "priv.h"
#include "fsys_S.h"
#include "fsys_reply_U.h"
kern_return_t
diskfs_S_fsys_goaway (struct diskfs_control *pt,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int flags)
{
error_t ret;
if (!pt)
return EOPNOTSUPP;
ret = diskfs_shutdown (flags);
if (ret == 0)
{
fsys_goaway_reply (reply, reply_type, 0);
exit (0);
}
return ret;
}