#include "priv.h"
#include "trivfs_io_S.h"
#include <assert-backtrace.h>
kern_return_t
trivfs_S_io_readable (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
vm_size_t *amount)
{
assert_backtrace (!trivfs_support_read);
return EOPNOTSUPP;
}