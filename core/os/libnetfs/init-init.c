#include "netfs.h"
#include <error.h>
size_t const _netfs_sizeof_struct_node = sizeof (struct node);
struct node *netfs_root_node = 0;
struct port_bucket *netfs_port_bucket = 0;
struct port_class *netfs_protid_class = 0;
struct port_class *netfs_control_class = 0;
auth_t netfs_auth_server_port = 0;
mach_port_t netfs_fsys_identity;
volatile struct mapped_time_value *netfs_mtime;
void
netfs_init (void)
{
error_t err;
err = maptime_map (0, 0, &netfs_mtime);
if (err)
err = maptime_map (1, 0, &netfs_mtime);
if (err)
error (2, err, "mapping time");
netfs_protid_class = ports_create_class (netfs_release_protid, 0);
netfs_control_class = ports_create_class (0, 0);
netfs_port_bucket = ports_create_bucket ();
netfs_auth_server_port = getauth ();
mach_port_allocate (mach_task_self (), MACH_PORT_RIGHT_RECEIVE,
&netfs_fsys_identity);
}