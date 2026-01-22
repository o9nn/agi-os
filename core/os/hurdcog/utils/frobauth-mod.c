#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <hurd.h>
#include <hurd/msg.h>
#include <error.h>
#include "frobauth.h"
error_t
frobauth_modify (struct frobauth *frobauth,
const auth_t *auths, size_t num_auths,
error_t (*modify) (struct ugids *ugids,
const struct ugids *change,
pid_t pid, void *hook),
void (*print_info) (const struct ugids *new,
const struct ugids *old,
const struct ugids *change,
pid_t pid, void *hook),
void *hook)
{
int i;
int ok = 1;
size_t num_all_auths = num_auths + 1;
auth_t all_auths[num_all_auths];
pid_t cur_pid = getpid ();
process_t proc_server = getproc ();
bcopy (auths, all_auths, num_auths * sizeof *auths);
for (i = 0; i < frobauth->num_pids; i++)
if (frobauth->pids[i] != cur_pid)
{
mach_port_t msgport;
pid_t pid = frobauth->pids[i];
error_t err = proc_getmsgport (proc_server, pid, &msgport);
if (err)
error (0, err, "%d: Cannot get message port", pid);
else
{
task_t task;
err = proc_pid2task (proc_server, pid, &task);
if (err)
error (0, err, "%d", pid);
else
{
auth_t old_auth;
err = msg_get_init_port (msgport, task, INIT_PORT_AUTH,
&old_auth);
if (err)
error (0, err, "%d: Cannot get auth port", pid);
else
{
struct ugids old = UGIDS_INIT;
err = ugids_merge_auth (&old, old_auth);
if (err)
error (0, err, "%d: Cannot get auth port ids", pid);
else
{
struct ugids new = UGIDS_INIT;
ugids_imply_all (&old);
err = ugids_set (&new, &old);
err = (*modify) (&new, &frobauth->ugids, pid, hook);
if (err)
error (99, err, "%d: Cannot modify ids", pid);
else if (! ugids_equal (&new, &old))
{
if (! frobauth->dry_run)
{
auth_t new_auth;
all_auths[num_all_auths - 1] = old_auth;
err = ugids_make_auth (&new,
all_auths,
num_all_auths,
&new_auth);
if (err)
error (0, err,
"%d: Authentication failure", pid);
else
{
err =
msg_set_init_port (msgport, task,
INIT_PORT_AUTH,
new_auth,
MACH_MSG_TYPE_COPY_SEND);
mach_port_deallocate (mach_task_self (),
new_auth);
if (err)
error (0, err, "%d", pid);
}
}
if (frobauth->verbose && !err)
(*print_info) (&new, &old, &frobauth->ugids,
pid, hook);
}
else if (frobauth->verbose)
printf ("%d: Nothing changed\n", pid);
ugids_fini (&new);
}
ugids_fini (&old);
mach_port_deallocate (mach_task_self (), old_auth);
}
mach_port_deallocate (mach_task_self (), task);
}
mach_port_deallocate (mach_task_self (), msgport);
}
if (err)
ok = 0;
}
return ok;
}