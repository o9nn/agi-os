#include "priv.h"
#include "trivfs_io_S.h"
#include <assert-backtrace.h>
#include <fcntl.h>
kern_return_t
trivfs_S_io_write (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
const_data_t data,
mach_msg_type_number_t datalen,
off_t off,
vm_size_t *amt)
{
if (!(trivfs_allow_open & O_WRITE))
return EBADF;
assert_backtrace (!trivfs_support_write);
return EOPNOTSUPP;
}