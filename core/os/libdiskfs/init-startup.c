#include "priv.h"
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <error.h>
#include <hurd/fsys.h>
#include <hurd/paths.h>
#include <hurd/startup.h>
#include "startup_S.h"
char *_diskfs_chroot_directory;
static void
diskfs_call_fsys_startup (mach_port_t bootstrap, int flags,
mach_port_t *realnode)
{
error_t err;
struct port_info *newpi;
mach_port_t right;
err = ports_create_port (diskfs_control_class, diskfs_port_bucket,
sizeof (struct port_info), &newpi);
if (! err)
{
right = ports_get_send_right (newpi);
err = fsys_startup (bootstrap, flags, right,
MACH_MSG_TYPE_COPY_SEND, realnode);
mach_port_deallocate (mach_task_self (), right);
ports_port_deref (newpi);
}
if (err)
error (1, err, "Translator startup failure: fsys_startup");
_diskfs_ncontrol_ports++;
}
mach_port_t
diskfs_startup_diskfs (mach_port_t fs_bootstrap, int flags)
{
error_t err;
mach_port_t realnode, bootstrap;
if (_diskfs_chroot_directory != NULL)
{
struct node *np, *old;
struct protid *rootpi;
struct peropen *rootpo;
while (*_diskfs_chroot_directory == '/')
++_diskfs_chroot_directory;
pthread_mutex_lock (&diskfs_root_node->lock);
err = diskfs_make_peropen (diskfs_root_node, O_READ|O_EXEC,
0, &rootpo);
assert_perror_backtrace (err);
err = diskfs_create_protid (rootpo, 0, &rootpi);
assert_perror_backtrace (err);
err = diskfs_lookup (diskfs_root_node, _diskfs_chroot_directory,
LOOKUP, &np, NULL, rootpi);
pthread_mutex_unlock (&diskfs_root_node->lock);
ports_port_deref (rootpi);
if (err == EAGAIN)
error (1, 0, "`--virtual-root=%s' specifies the real root directory",
_diskfs_chroot_directory);
else if (err)
error (1, err, "`%s' not found", _diskfs_chroot_directory);
if (!S_ISDIR (np->dn_stat.st_mode))
{
pthread_mutex_unlock (&np->lock);
error (1, ENOTDIR, "%s", _diskfs_chroot_directory);
}
old = diskfs_root_node;
diskfs_root_node = np;
pthread_mutex_unlock (&np->lock);
diskfs_nput (old);
}
if (fs_bootstrap != MACH_PORT_NULL)
{
diskfs_call_fsys_startup (fs_bootstrap, flags, &realnode);
mach_port_deallocate (mach_task_self (), fs_bootstrap);
_diskfs_init_completed ();
}
else
{
realnode = MACH_PORT_NULL;
diskfs_start_bootstrap ();
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap != MACH_PORT_NULL)
{
diskfs_call_fsys_startup (bootstrap, flags, &realnode);
mach_port_deallocate (mach_task_self (), bootstrap);
}
}
if (diskfs_default_sync_interval)
diskfs_set_sync_interval (diskfs_default_sync_interval);
return realnode;
}
error_t
diskfs_S_startup_dosync (mach_port_t handle)
{
error_t err = 0;
struct port_info *pi
= ports_lookup_port (diskfs_port_bucket, handle,
diskfs_shutdown_notification_class);
if (!pi)
return EOPNOTSUPP;
if (! diskfs_readonly)
{
diskfs_sync_everything (0);
diskfs_set_hypermetadata (0, 0);
pthread_rwlock_wrlock (&diskfs_fsys_lock);
err = ports_inhibit_class_rpcs (diskfs_protid_class);
if (! err)
{
diskfs_sync_everything (1);
diskfs_set_hypermetadata (1, 1);
_diskfs_diskdirty = 0;
diskfs_readonly = 1;
diskfs_readonly_changed (1);
ports_resume_class_rpcs (diskfs_protid_class);
}
pthread_rwlock_unlock (&diskfs_fsys_lock);
}
ports_port_deref (pi);
return err;
}
void
_diskfs_init_completed (void)
{
startup_t init;
process_t proc;
error_t err;
struct port_info *pi;
mach_port_t notify;
char *name;
proc = getproc ();
assert_backtrace (proc);
err = ports_create_port (diskfs_shutdown_notification_class,
diskfs_port_bucket, sizeof (struct port_info),
&pi);
if (err)
goto errout;
err = proc_mark_important (proc);
mach_port_deallocate (mach_task_self (), proc);
if (err && err != EPERM && err != EMIG_BAD_ID)
goto errout;
init = file_name_lookup (_SERVERS_STARTUP, 0, 0);
if (init == MACH_PORT_NULL)
{
err = errno;
if (err == EPERM)
return;
goto errout;
}
notify = ports_get_send_right (pi);
ports_port_deref (pi);
asprintf (&name,
"%s %s", program_invocation_short_name, diskfs_disk_name ?: "-");
err = startup_request_notification (init, notify,
MACH_MSG_TYPE_COPY_SEND, name);
mach_port_deallocate (mach_task_self (), notify);
mach_port_deallocate (mach_task_self (), init);
free (name);
if (err && err != EPERM)
goto errout;
return;
errout:
error (0, err, "Warning: cannot request shutdown notification");
}