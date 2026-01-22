#include "priv.h"
#include "trivfs_fs_S.h"
#include <assert-backtrace.h>
kern_return_t
trivfs_S_file_set_size (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
off_t size)
{
assert_backtrace (!trivfs_support_write);
return EOPNOTSUPP;
}