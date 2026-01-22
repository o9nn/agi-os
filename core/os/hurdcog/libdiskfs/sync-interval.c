#include <errno.h>
#include <pthread.h>
#include <unistd.h>
#include <hurd/fsys.h>
#include "priv.h"
int diskfs_sync_interval = 0;
static pthread_t periodic_sync_thread;
static struct port_info *pi;
static void * periodic_sync (void *);
error_t
diskfs_set_sync_interval (int interval)
{
error_t err = 0;
if (! pi)
{
err = ports_create_port (diskfs_control_class, diskfs_port_bucket,
sizeof (struct port_info), &pi);
if (err)
return err;
}
err = ports_inhibit_port_rpcs (pi);
if (err)
return err;
if (interval == 0)
periodic_sync_thread = 0;
else
{
err = pthread_create (&periodic_sync_thread, NULL, periodic_sync,
(void *)(intptr_t) interval);
if (!err)
pthread_detach (periodic_sync_thread);
else
{
errno = err;
perror ("pthread_create");
}
}
if (!err)
diskfs_sync_interval = interval;
ports_resume_port_rpcs (pi);
return err;
}
static void *
periodic_sync (void *arg)
{
int interval = (int)(uintptr_t) arg;
pthread_setname_np (pthread_self (), "sync");
for (;;)
{
error_t err;
struct rpc_info link;
err = ports_begin_rpc (pi, 0, &link);
if (periodic_sync_thread != pthread_self ())
{
ports_end_rpc (pi, &link);
return NULL;
}
if (! err)
{
if (! diskfs_readonly)
{
pthread_rwlock_rdlock (&diskfs_fsys_lock);
if (_diskfs_diskdirty)
{
diskfs_sync_everything (0);
diskfs_set_hypermetadata (0, 0);
}
pthread_rwlock_unlock (&diskfs_fsys_lock);
}
ports_end_rpc (pi, &link);
}
sleep (interval);
}
return NULL;
}