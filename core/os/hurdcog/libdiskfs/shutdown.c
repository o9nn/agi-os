#include "priv.h"
#include <hurd/fsys.h>
struct args
{
int flags;
};
static error_t
helper (void *cookie, const char *name, mach_port_t control)
{
struct args *args = cookie;
error_t err;
(void) name;
err = fsys_goaway (control, args->flags);
if (err == MIG_SERVER_DIED || err == MACH_SEND_INVALID_DEST)
err = 0;
return err;
}
error_t
diskfs_shutdown (int flags)
{
int nports = -1;
error_t err;
struct args args = { flags };
if ((flags & FSYS_GOAWAY_UNLINK)
&& S_ISDIR (diskfs_root_node->dn_stat.st_mode))
return EBUSY;
if (flags & FSYS_GOAWAY_RECURSE)
{
err = fshelp_map_active_translators (helper, &args);
if (err)
return err;
}
pthread_rwlock_wrlock (&diskfs_fsys_lock);
err = ports_inhibit_class_rpcs (diskfs_protid_class);
if (err)
{
pthread_rwlock_unlock (&diskfs_fsys_lock);
return err;
}
diskfs_sync_everything (1);
diskfs_set_hypermetadata (1, 1);
_diskfs_diskdirty = 0;
nports = ports_count_class (diskfs_protid_class);
if (((flags & FSYS_GOAWAY_FORCE) == 0)
&& (nports || diskfs_pager_users ()))
{
ports_enable_class (diskfs_protid_class);
ports_resume_class_rpcs (diskfs_protid_class);
pthread_rwlock_unlock (&diskfs_fsys_lock);
return EBUSY;
}
if (!diskfs_readonly && (flags & FSYS_GOAWAY_NOSYNC) == 0)
{
diskfs_shutdown_pager ();
diskfs_set_hypermetadata (1, 1);
}
return 0;
}