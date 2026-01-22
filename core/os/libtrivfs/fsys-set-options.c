#include <hurd/fshelp.h>
#include "priv.h"
#include "trivfs_fsys_S.h"
kern_return_t
trivfs_S_fsys_set_options (struct trivfs_control *cntl,
mach_port_t reply, mach_msg_type_name_t reply_type,
const_data_t data, mach_msg_type_number_t len,
int do_children)
{
if (cntl)
return trivfs_set_options (cntl, data, len);
else
return EOPNOTSUPP;
}