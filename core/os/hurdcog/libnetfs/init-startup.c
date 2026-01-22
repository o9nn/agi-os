#include <stdio.h>
#include <error.h>
#include <hurd/fsys.h>
#include "netfs.h"
mach_port_t
netfs_startup (mach_port_t bootstrap, int flags)
{
error_t err;
mach_port_t realnode, right;
struct port_info *newpi;
if (bootstrap == MACH_PORT_NULL)
error (10, 0, "Must be started as a translator");
err = ports_create_port (netfs_control_class, netfs_port_bucket,
sizeof (struct port_info), &newpi);
if (!err)
{
right = ports_get_send_right (newpi);
err = fsys_startup (bootstrap, flags, right, MACH_MSG_TYPE_COPY_SEND,
&realnode);
mach_port_deallocate (mach_task_self (), right);
ports_port_deref (newpi);
}
if (err)
error (11, err, "Translator startup failure: fsys_startup");
mach_port_deallocate (mach_task_self (), bootstrap);
mach_port_t proc = getproc ();
if (proc == MACH_PORT_NULL)
error (12, err, "Translator startup failure: getproc");
err = proc_mark_important (proc);
if (err && err != EPERM && err != EMIG_BAD_ID)
error (13, err, "Translator startup failure: proc_mark_important");
mach_port_deallocate (mach_task_self (), proc);
return realnode;
}