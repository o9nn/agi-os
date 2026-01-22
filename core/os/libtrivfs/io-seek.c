#include "priv.h"
#include "trivfs_io_S.h"
#include <assert-backtrace.h>
kern_return_t
trivfs_S_io_seek (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
off_t off,
int whence,
off_t *newp)
{
assert_backtrace (!trivfs_support_read && !trivfs_support_write);
return EOPNOTSUPP;
}