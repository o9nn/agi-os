#include <hurd.h>
#include <hurd/fsys.h>
#include <assert-backtrace.h>
#include <signal.h>
#include <error.h>
#include "priv.h"
error_t
trivfs_startup(mach_port_t bootstrap, int flags,
struct port_class *control_class,
struct port_bucket *control_bucket,
struct port_class *protid_class,
struct port_bucket *protid_bucket,
struct trivfs_control **control)
{
mach_port_t underlying, right;
struct trivfs_control *fsys;
error_t err =
trivfs_create_control (MACH_PORT_NULL,
control_class, control_bucket,
protid_class, protid_bucket,
&fsys);
if (err)
return err;
right = ports_get_send_right (fsys);
err = fsys_startup (bootstrap, flags, right, MACH_MSG_TYPE_COPY_SEND,
&underlying);
mach_port_deallocate (mach_task_self (), right);
if (! err)
fsys->underlying = underlying;
ports_port_deref (fsys);
if (!err && control)
*control = fsys;
if (! err)
{
mach_port_t proc = getproc ();
if (proc == MACH_PORT_NULL)
return 0;
err = proc_mark_important (proc);
if (err == EPERM || err == EMIG_BAD_ID)
err = 0;
mach_port_deallocate (mach_task_self (), proc);
}
return err;
}
error_t
trivfs_startup_debug(const char *file_name,
struct port_class *control_class,
struct port_bucket *control_bucket,
struct port_class *protid_class,
struct port_bucket *protid_bucket,
struct trivfs_control **control)
{
mach_port_t underlying, right, goaway;
struct trivfs_control *fsys;
error_t err =
trivfs_create_control (MACH_PORT_NULL,
control_class, control_bucket,
protid_class, protid_bucket,
&fsys);
if (err)
return err;
right = ports_get_send_right (fsys);
goaway = ports_get_send_right (fsys);
underlying = file_name_lookup(file_name, 0, 0);
if (underlying == MACH_PORT_NULL)
err = errno;
else
err = file_set_translator(underlying, 0, FS_TRANS_SET, 0, "", 0,
right, MACH_MSG_TYPE_COPY_SEND);
mach_port_deallocate (mach_task_self (), right);
if (! err)
fsys->underlying = underlying;
ports_port_deref (fsys);
if (!err && control)
*control = fsys;
void handler_sigterm(int signum)
{
error_t ee;
ee = fsys_goaway(goaway, 0);
if (ee == ESUCCESS)
{
mach_port_deallocate (mach_task_self (), goaway);
}
else if (ee != EBUSY)
{
error(99, err, "fsys_goaway");
}
}
struct sigaction sa;
memset(&sa, 0, sizeof(sa));
sa.sa_handler = handler_sigterm;
if (sigaction(SIGTERM, &sa, NULL) < 0)
err = errno;
return err;
}