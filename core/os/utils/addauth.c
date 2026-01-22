#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <argp.h>
#include <idvec.h>
#include <ugids.h>
#include <error.h>
#include <hurd.h>
#include <hurd/msg.h>
#include <version.h>
#include "frobauth.h"
const char *argp_program_version = STANDARD_HURD_VERSION (addauth);
static struct argp_child child_argps[] = {{ &frobauth_ea_argp }, { 0 }};
static char doc[] =
"Add new user/group ids to the authentication of selected processes";
extern error_t
get_nonsugid_ids (struct idvec *uids, struct idvec *gids);
int
main (int argc, char *argv[])
{
int i;
error_t err;
auth_t auth;
char *ids_rep = 0;
process_t proc_server = getproc();
struct frobauth frobauth = FROBAUTH_INIT;
struct idvec have_uids = IDVEC_INIT, have_gids = IDVEC_INIT;
struct argp argp = { 0, 0, 0, doc, child_argps };
frobauth.require_ids = 1;
argp_parse (&argp, argc, argv, 0, 0, &frobauth);
err = get_nonsugid_ids (&have_uids, &have_gids);
if (err)
error (52, err, "Cannot get invoking authentication");
err = ugids_verify_make_auth (&frobauth.ugids, &have_uids, &have_gids, 0, 0,
0, 0, &auth);
if (err == EACCES)
error (15, 0, "Invalid password");
else if (err)
error (16, err, "Authentication failure");
if (frobauth.verbose)
ids_rep = ugids_rep (&frobauth.ugids, 1, 1, 0, 0, 0);
for (i = 0; i < frobauth.num_pids; i++)
{
mach_port_t msgport;
pid_t pid = frobauth.pids[i];
error_t err = proc_getmsgport (proc_server, pid, &msgport);
if (err)
error (0, err, "%d: Cannot get message port", pid);
else
{
if (! frobauth.dry_run)
err = msg_add_auth (msgport, auth);
if (err)
error (0, err, "%d: Cannot add authentication", pid);
else if (frobauth.verbose)
printf ("%d: Added %s\n", pid, ids_rep);
mach_port_deallocate (mach_task_self (), msgport);
}
}
return 0;
}