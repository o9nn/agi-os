#include "priv.h"
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
diskfs_S_fsys_syncfs (struct diskfs_control *pi,
mach_port_t reply,
mach_msg_type_name_t replytype,
int wait,
int children)
{
struct args args = { wait };
if (!pi)
return EOPNOTSUPP;
pthread_rwlock_rdlock (&diskfs_fsys_lock);
if (children)
fshelp_map_active_translators (helper, &args);
if (diskfs_synchronous)
wait = 1;
if (! diskfs_readonly)
{
diskfs_sync_everything (wait);
diskfs_set_hypermetadata (wait, 0);
}
pthread_rwlock_unlock (&diskfs_fsys_lock);
return 0;
}