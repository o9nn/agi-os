#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <mach.h>
#include <pthread.h>
#include <hurd.h>
#include <hurd/startup.h>
#include <hurd/paths.h>
#include <hurd/ports.h>
#include <hurd/ihash.h>
#include <idvec.h>
#include <assert-backtrace.h>
#include <argp.h>
#include <error.h>
#include <version.h>
#include "auth_S.h"
#include "auth_reply_U.h"
#include "auth.h"
const char *argp_program_version = STANDARD_HURD_VERSION(auth);
struct authhandle
{
struct port_info pi;
struct idvec euids, egids, auids, agids;
};
struct port_bucket *auth_bucket;
struct port_class *authhandle_portclass;
static error_t
create_authhandle (struct authhandle **new)
{
error_t err = ports_create_port (authhandle_portclass, auth_bucket,
sizeof **new, new);
if (! err)
memset (&(*new)->euids, 0, (void *)&(*new)[1] - (void *)&(*new)->euids);
return err;
}
static void
destroy_authhandle (void *p)
{
struct authhandle *h = p;
idvec_free_contents (&h->euids);
idvec_free_contents (&h->egids);
idvec_free_contents (&h->auids);
idvec_free_contents (&h->agids);
}
static inline void
idvec_copyout (struct idvec *idvec, uid_t **ids,
mach_msg_type_number_t *nids)
{
if (idvec->num > *nids)
*ids = idvec->ids;
else if (idvec->num)
memcpy (*ids, idvec->ids, idvec->num * sizeof *ids);
*nids = idvec->num;
}
#define C(auth, ids) idvec_copyout (&auth->ids, ids, n##ids)
#define OUTIDS(auth) (C (auth, euids), C (auth, egids), \
C (auth, auids), C (auth, agids))
kern_return_t
S_auth_getids (struct authhandle *auth,
uid_t **euids,
mach_msg_type_number_t *neuids,
uid_t **auids,
mach_msg_type_number_t *nauids,
uid_t **egids,
mach_msg_type_number_t *negids,
uid_t **agids,
mach_msg_type_number_t *nagids)
{
if (! auth)
return EOPNOTSUPP;
OUTIDS (auth);
return 0;
}
kern_return_t
S_auth_makeauth (struct authhandle *auth,
const mach_port_t *authpts, mach_msg_type_number_t nauths,
const uid_t *euids, mach_msg_type_number_t neuids,
const uid_t *auids, mach_msg_type_number_t nauids,
const uid_t *egids, mach_msg_type_number_t negids,
const uid_t *agids, mach_msg_type_number_t nagids,
mach_port_t *newhandle)
{
struct authhandle *newauth, *auths[1 + nauths];
int hasroot = 0;
error_t err;
size_t i, j;
if (!auth)
return EOPNOTSUPP;
auths[0] = auth;
for (i = 0; i < nauths; i++)
auths[i + 1] = auth_port_to_handle (authpts[i]);
++nauths;
#define isuid(uid, auth) \
(idvec_contains (&(auth)->euids, uid) \
|| idvec_contains (&(auth)->auids, uid))
#define groupmember(gid, auth) \
(idvec_contains (&(auth)->egids, gid) \
|| idvec_contains (&(auth)->agids, gid))
#define isroot(auth) isuid (0, auth)
for (i = 0; i < nauths; i++)
if (auths[i] && isroot (auths[i]))
{
hasroot = 1;
break;
}
if (!hasroot)
{
int has_it;
for (i = 0; i < neuids; i++)
{
has_it = 0;
for (j = 0; j < nauths; j++)
if (auths[j] && isuid (euids[i], auths[j]))
{
has_it = 1;
break;
}
if (!has_it)
goto eperm;
}
for (i = 0; i < nauids; i++)
{
has_it = 0;
for (j = 0; j < nauths; j++)
if (auths[j] && isuid (auids[i], auths[j]))
{
has_it = 1;
break;
}
if (!has_it)
goto eperm;
}
for (i = 0; i < negids; i++)
{
has_it = 0;
for (j = 0; j < nauths; j++)
if (auths[j] && groupmember (egids[i], auths[j]))
{
has_it = 1;
break;
}
if (!has_it)
goto eperm;
}
for (i = 0; i < nagids; i++)
{
has_it = 0;
for (j = 0; j < nauths; j++)
if (auths[j] && groupmember (agids[i], auths[j]))
{
has_it = 1;
break;
}
if (!has_it)
goto eperm;
}
}
err = create_authhandle (&newauth);
#define MERGE S (euids); S (egids); S (auids); S (agids);
#define S(uids) if (!err) err = idvec_merge_ids (&newauth->uids, uids, n##uids)
MERGE;
#undef S
if (! err)
{
for (j = 1; j < nauths; ++j)
mach_port_deallocate (mach_task_self (), authpts[j - 1]);
*newhandle = ports_get_right (newauth);
ports_port_deref (newauth);
}
for (j = 1; j < nauths; j++)
if (auths[j])
ports_port_deref (auths[j]);
return err;
eperm:
for (j = 1; j < nauths; j++)
if (auths[j])
ports_port_deref (auths[j]);
return EPERM;
}
struct pending_user
{
hurd_ihash_locp_t locp;
pthread_cond_t wakeup;
struct authhandle *user;
mach_port_t passthrough;
};
struct pending_server
{
hurd_ihash_locp_t locp;
pthread_cond_t wakeup;
};
struct hurd_ihash pending_users
= HURD_IHASH_INITIALIZER (offsetof (struct pending_user, locp));
struct hurd_ihash pending_servers
= HURD_IHASH_INITIALIZER (offsetof (struct pending_server, locp));
pthread_mutex_t pending_lock = PTHREAD_MUTEX_INITIALIZER;
kern_return_t
S_auth_user_authenticate (struct authhandle *userauth,
mach_port_t reply,
mach_msg_type_name_t reply_type,
mach_port_t rendezvous,
mach_port_t *newport,
mach_msg_type_name_t *newporttype)
{
struct pending_server *s;
struct pending_user u;
error_t err;
if (! userauth)
return EOPNOTSUPP;
if (! MACH_PORT_VALID (rendezvous))
return EINVAL;
u.user = userauth;
pthread_cond_init (&u.wakeup, NULL);
pthread_mutex_lock (&pending_lock);
err = hurd_ihash_add (&pending_users, rendezvous, &u);
if (err) {
pthread_mutex_unlock (&pending_lock);
return err;
}
ports_port_ref (userauth);
s = hurd_ihash_find (&pending_servers, rendezvous);
if (s) {
hurd_ihash_locp_remove (&pending_servers, s->locp);
pthread_cond_signal (&s->wakeup);
}
ports_interrupt_self_on_port_death (userauth, rendezvous);
if (pthread_hurd_cond_wait_np (&u.wakeup, &pending_lock) &&
hurd_ihash_find (&pending_users, rendezvous))
{
hurd_ihash_locp_remove (&pending_users, u.locp);
mach_port_type_t type;
mach_port_type (mach_task_self (), rendezvous, &type);
err = type & MACH_PORT_TYPE_DEAD_NAME ? EINVAL : EINTR;
}
pthread_mutex_unlock (&pending_lock);
if (! err)
{
*newport = u.passthrough;
*newporttype = MACH_MSG_TYPE_MOVE_SEND;
mach_port_deallocate (mach_task_self (), rendezvous);
}
return err;
}
kern_return_t
S_auth_server_authenticate (struct authhandle *serverauth,
mach_port_t reply,
mach_msg_type_name_t reply_type,
mach_port_t rendezvous,
mach_port_t newport,
mach_msg_type_name_t newport_type,
uid_t **euids,
mach_msg_type_number_t *neuids,
uid_t **auids,
mach_msg_type_number_t *nauids,
uid_t **egids,
mach_msg_type_number_t *negids,
uid_t **agids,
mach_msg_type_number_t *nagids)
{
struct pending_user *u;
struct authhandle *user;
error_t err = 0;
if (! serverauth)
return EOPNOTSUPP;
if (! MACH_PORT_VALID (rendezvous))
return EINVAL;
pthread_mutex_lock (&pending_lock);
u = hurd_ihash_find (&pending_users, rendezvous);
if (! u)
{
struct pending_server s;
pthread_cond_init (&s.wakeup, NULL);
err = hurd_ihash_add (&pending_servers, rendezvous, &s);
if (! err)
{
ports_interrupt_self_on_port_death (serverauth, rendezvous);
if (pthread_hurd_cond_wait_np (&s.wakeup, &pending_lock) &&
hurd_ihash_find (&pending_servers, rendezvous))
{
hurd_ihash_locp_remove (&pending_servers, s.locp);
mach_port_type_t type;
mach_port_type (mach_task_self (), rendezvous, &type);
err = type & MACH_PORT_TYPE_DEAD_NAME ? EINVAL : EINTR;
}
else
{
u = hurd_ihash_find (&pending_users, rendezvous);
if (! u)
err = EINTR;
}
}
}
if (u)
{
error_t err2;
hurd_ihash_locp_remove (&pending_users, u->locp);
user = u->user;
pthread_mutex_unlock (&pending_lock);
err2 = auth_server_authenticate_reply (reply, reply_type, 0,
user->euids.ids, user->euids.num,
user->auids.ids, user->auids.num,
user->egids.ids, user->egids.num,
user->agids.ids, user->agids.num);
if (err2)
mach_port_deallocate (mach_task_self (), reply);
pthread_mutex_lock (&pending_lock);
u->passthrough = newport;
pthread_cond_signal (&u->wakeup);
}
pthread_mutex_unlock (&pending_lock);
if (err)
return err;
ports_port_deref (user);
mach_port_deallocate (mach_task_self (), rendezvous);
return MIG_NO_REPLY;
}
#include "../libports/notify_S.h"
#include "../libports/interrupt_S.h"
static int
auth_demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = auth_server_routine (inp)) ||
(routine = ports_interrupt_server_routine (inp)) ||
(routine = ports_notify_server_routine (inp)))
{
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
int
main (int argc, char **argv)
{
error_t err;
mach_port_t boot;
mach_port_t startup;
process_t proc;
mach_port_t hostpriv, masterdev;
struct authhandle *firstauth;
struct argp argp = { 0, 0, 0, "Hurd standard authentication server." };
argp_parse (&argp, argc, argv, 0, 0, 0);
auth_bucket = ports_create_bucket ();
authhandle_portclass = ports_create_class (&destroy_authhandle, 0);
err = create_authhandle (&firstauth);
assert_perror_backtrace (err);
idvec_add (&firstauth->euids, 0);
idvec_add (&firstauth->auids, 0);
idvec_add (&firstauth->auids, 0);
idvec_merge (&firstauth->egids, &firstauth->euids);
idvec_merge (&firstauth->agids, &firstauth->auids);
err = task_get_bootstrap_port (mach_task_self (), &boot);
assert_perror_backtrace (err);
if (boot == MACH_PORT_NULL)
error (2, 0, "auth server can only be run by init during boot");
err = startup_authinit (boot, ports_get_right (firstauth),
MACH_MSG_TYPE_MAKE_SEND, &proc);
if (err)
error (2, err, "cannot contact init for bootstrap");
proc_getprivports (proc, &hostpriv, &masterdev);
proc_register_version (proc, hostpriv, "auth", "", HURD_VERSION);
mach_port_deallocate (mach_task_self (), masterdev);
_hurd_port_set (&_hurd_ports[INIT_PORT_PROC], proc);
_hurd_proc_init (argv, NULL, 0);
startup = file_name_lookup (_SERVERS_STARTUP, 0, 0);
if (! MACH_PORT_VALID (startup))
{
error (0, errno, "%s", _SERVERS_STARTUP);
startup = boot;
}
startup_essential_task (startup, mach_task_self (), MACH_PORT_NULL, "auth",
hostpriv);
if (startup != boot)
mach_port_deallocate (mach_task_self (), startup);
mach_port_deallocate (mach_task_self (), boot);
mach_port_deallocate (mach_task_self (), hostpriv);
while (1)
ports_manage_port_operations_multithread (auth_bucket,
auth_demuxer,
30 * 1000, 0, 0);
}