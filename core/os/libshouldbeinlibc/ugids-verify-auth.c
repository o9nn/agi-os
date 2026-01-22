#include <stdlib.h>
#include <hurd.h>
#include <hurd/paths.h>
#include <hurd/password.h>
#include "idvec.h"
#include "ugids.h"
struct svma_state
{
file_t server;
auth_t *auths;
size_t num_auths;
};
static error_t
svma_state_add_auths (struct svma_state *ss,
const auth_t *auths, size_t num_auths)
{
auth_t *new = realloc (ss->auths,
(ss->num_auths + num_auths) * sizeof (auth_t));
if (new)
{
ss->auths = new;
while (num_auths--)
ss->auths[ss->num_auths++] = *auths++;
return 0;
}
else
return ENOMEM;
}
static error_t
server_verify_make_auth (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook)
{
auth_t auth;
struct svma_state *svma_state = hook;
error_t (*check) (io_t server, uid_t id, const char *passwd, auth_t *auth) =
is_group ? password_check_group : password_check_user;
error_t err = (*check) (svma_state->server, id, password, &auth);
if (! err)
{
err = svma_state_add_auths (svma_state, &auth, 1);
if (err)
mach_port_deallocate (mach_task_self (), auth);
}
return err;
}
error_t
ugids_verify_make_auth (const struct ugids *ugids,
const struct idvec *have_uids,
const struct idvec *have_gids,
char *(*getpass_fn) (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *getpass_hook,
const auth_t *from, size_t num_from,
auth_t *auth)
{
error_t err;
struct svma_state svma_state;
error_t (*verify_fn) (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook)
= server_verify_make_auth;
void *verify_hook = &svma_state;
svma_state.server = file_name_lookup (_SERVERS_PASSWORD, 0, 0);
if (svma_state.server == MACH_PORT_NULL)
{
verify_fn = 0;
verify_hook = 0;
}
else
{
svma_state.auths = NULL;
svma_state.num_auths = 0;
}
err = ugids_verify (ugids, have_uids, have_gids,
getpass_fn, getpass_hook, verify_fn, verify_hook);
if (! err)
{
if (verify_fn)
{
if (num_from > 0)
err = svma_state_add_auths (&svma_state, from, num_from);
if (! err)
{
auth_t cur_auth = getauth ();
err =
auth_makeauth (cur_auth,
svma_state.auths, MACH_MSG_TYPE_COPY_SEND,
svma_state.num_auths,
ugids->eff_uids.ids, ugids->eff_uids.num,
ugids->avail_uids.ids, ugids->avail_uids.num,
ugids->eff_gids.ids, ugids->eff_gids.num,
ugids->avail_gids.ids, ugids->avail_gids.num,
auth);
mach_port_deallocate (mach_task_self (), cur_auth);
svma_state.num_auths -= num_from;
}
}
else
err = ugids_make_auth (ugids, from, num_from, auth);
}
if (verify_fn)
{
unsigned int i;
for (i = 0; i < svma_state.num_auths; i++)
mach_port_deallocate (mach_task_self (), svma_state.auths[i]);
mach_port_deallocate (mach_task_self (), svma_state.server);
if (svma_state.num_auths > 0)
free (svma_state.auths);
}
return err;
}