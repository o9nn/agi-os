#include "priv.h"
#include <string.h>
#include <assert-backtrace.h>
error_t
diskfs_start_protid (struct peropen *po, struct protid **cred)
{
error_t err =
ports_create_port_noinstall (diskfs_protid_class, diskfs_port_bucket,
sizeof (struct protid), cred);
if (! err)
{
(*cred)->po = po;
(*cred)->shared_object = MACH_PORT_NULL;
(*cred)->mapped = 0;
}
return err;
}
void
diskfs_finish_protid (struct protid *cred, struct iouser *user)
{
error_t err;
if (!user)
err = iohelp_create_simple_iouser (&cred->user, 0, 0);
else
err = iohelp_dup_iouser (&cred->user, user);
assert_perror_backtrace (err);
err = mach_port_move_member (mach_task_self (), cred->pi.port_right,
diskfs_port_bucket->portset);
assert_perror_backtrace (err);
}
error_t
diskfs_create_protid (struct peropen *po, struct iouser *user,
struct protid **cred)
{
error_t err = diskfs_start_protid (po, cred);
if (! err)
diskfs_finish_protid (*cred, user);
return err;
}