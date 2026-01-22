#include "netfs.h"
#include "fsys_S.h"
#include <hurd/fsys.h>
struct args
{
int wait;
};
static error_t
helper (void *cookie, const char *name, mach_port_t control)
{
struct args *args = cookie;
(void) name;
fsys_syncfs (control, args->wait, 1);
return 0;
}
kern_return_t
netfs_S_fsys_syncfs (struct netfs_control *cntl,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int wait,
int children)
{
struct iouser *cred;
error_t err;
struct args args = { wait };
if (! cntl)
return EOPNOTSUPP;
if (children)
fshelp_map_active_translators (helper, &args);
err = iohelp_create_simple_iouser (&cred, 0, 0);
if (err)
return err;
err = netfs_attempt_syncfs (cred, wait);
iohelp_free_iouser (cred);
return err;
}