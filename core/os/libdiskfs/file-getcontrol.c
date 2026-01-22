#include "priv.h"
#include "fs_S.h"
#include <hurd/fshelp.h>
kern_return_t
diskfs_S_file_getcontrol (struct protid *cred,
mach_port_t *control,
mach_msg_type_name_t *controltype)
{
error_t err;
struct port_info *newpi;
if (!cred)
return EOPNOTSUPP;
err = fshelp_iscontroller (&diskfs_root_node->dn_stat, cred->user);
if (err)
return err;
err = ports_create_port (diskfs_control_class, diskfs_port_bucket,
sizeof (struct port_info), &newpi);
if (err)
return err;
pthread_spin_lock (&_diskfs_control_lock);
_diskfs_ncontrol_ports++;
pthread_spin_unlock (&_diskfs_control_lock);
*control = ports_get_right (newpi);
*controltype = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newpi);
return 0;
}