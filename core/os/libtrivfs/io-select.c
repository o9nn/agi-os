#include "priv.h"
#include "trivfs_io_S.h"
#include <assert-backtrace.h>
kern_return_t __attribute__((weak))
trivfs_S_io_select (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
int *seltype)
{
if (!cred)
return EOPNOTSUPP;
if (*seltype & (SELECT_READ|SELECT_URG))
assert_backtrace (!trivfs_support_read);
if (*seltype & (SELECT_WRITE|SELECT_URG))
assert_backtrace (!trivfs_support_write);
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_select_timeout (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
struct timespec ts,
int *seltype)
{
return trivfs_S_io_select (cred, reply, replytype, seltype);
}