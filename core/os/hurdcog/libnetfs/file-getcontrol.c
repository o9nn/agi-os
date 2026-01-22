#include "netfs.h"
#include "fs_S.h"
#include <hurd/fshelp.h>
kern_return_t
netfs_S_file_getcontrol (struct protid *user,
mach_port_t *control,
mach_msg_type_name_t *controltype)
{
error_t err;
struct port_info *pi;
if (!user)
return EOPNOTSUPP;
err = fshelp_iscontroller (&netfs_root_node->nn_stat, user->user);
if (err)
return err;
err = ports_create_port (netfs_control_class, netfs_port_bucket,
sizeof (struct port_info), &pi);
if (err)
return err;
*control = ports_get_right (pi);
*controltype = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (pi);
return 0;
}