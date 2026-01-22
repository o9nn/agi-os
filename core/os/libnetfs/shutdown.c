#include "priv.h"
#include <errno.h>
#include <sys/stat.h>
#include <hurd/fsys.h>
#include <hurd/fshelp.h>
#include <pthread.h>
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
netfs_shutdown (int flags)
{
struct args args = { flags };
int nports;
int err;
if ((flags & FSYS_GOAWAY_UNLINK)
&& S_ISDIR (netfs_root_node->nn_stat.st_mode))
return EBUSY;
if (flags & FSYS_GOAWAY_RECURSE)
{
err = fshelp_map_active_translators (helper, &args);
if (err)
return err;
}
#ifdef NOTYET
pthread_rwlock_wrlock (&netfs_fsys_lock);
#endif
err = ports_inhibit_class_rpcs (netfs_protid_class);
if (err)
{
#ifdef  NOTYET
pthread_rwlock_unlock (&netfs_fsys_lock);
#endif
return err;
}
nports = ports_count_class (netfs_protid_class);
if (((flags & FSYS_GOAWAY_FORCE) == 0) && nports)
{
ports_enable_class (netfs_protid_class);
ports_resume_class_rpcs (netfs_protid_class);
#ifdef NOTYET
pthread_rwlock_unlock (&netfs_fsys_lock);
#endif
return EBUSY;
}
if (!(flags & FSYS_GOAWAY_NOSYNC))
{
err = netfs_attempt_syncfs (0, flags);
if (err)
return err;
}
return 0;
}