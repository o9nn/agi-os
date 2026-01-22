#include <mach.h>
#include <mach/task_notify.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>
#include <hurd/hurd_types.h>
#include <stdlib.h>
#include <string.h>
#include <mach/notify.h>
#include <sys/wait.h>
#include <mach/mig_errors.h>
#include <sys/resource.h>
#include <hurd/auth.h>
#include <assert-backtrace.h>
#include <pids.h>
#include "proc.h"
#include "process_S.h"
#include "mutated_ourmsg_U.h"
#include "proc_exc_S.h"
#include "proc_exc_U.h"
#include "task_notify_S.h"
#include <hurd/signal.h>
extern mach_msg_return_t
mach_msg_receive (mach_msg_header_t *msg);
static inline struct ids *
make_ids (const uid_t *eff_uids, size_t n_eff_uids,
const uid_t *avail_uids, size_t n_avail_uids)
{
struct ids *i;
i = malloc (sizeof (struct ids) + sizeof (uid_t) * (n_eff_uids + n_avail_uids));
if (! i)
return NULL;
i->i_nuids = n_eff_uids + n_avail_uids;
i->i_refcnt = 1;
memcpy (&i->i_uids[0], eff_uids, sizeof (uid_t) * n_eff_uids);
memcpy (&i->i_uids[n_eff_uids], avail_uids, sizeof (uid_t) * n_avail_uids);
return i;
}
static inline void
ids_ref (struct ids *i)
{
i->i_refcnt ++;
}
static inline void
ids_rele (struct ids *i)
{
i->i_refcnt --;
if (i->i_refcnt == 0)
free (i);
}
int
check_uid (struct proc *p, uid_t uid)
{
int i;
for (i = 0; i < p->p_id->i_nuids; i++)
if (p->p_id->i_uids[i] == uid || p->p_id->i_uids[i] == 0)
return 1;
return 0;
}
kern_return_t
S_proc_reauthenticate (struct proc *p, mach_port_t rendport)
{
error_t err;
mach_port_t new_proc_port, prev;
mach_msg_header_t msg;
struct ids *new_ids;
uid_t gubuf[50], aubuf[50], ggbuf[50], agbuf[50];
uid_t *gen_uids, *aux_uids, *gen_gids, *aux_gids;
mach_msg_type_number_t ngen_uids, naux_uids, ngen_gids, naux_gids;
if (!p)
return EOPNOTSUPP;
gen_uids = gubuf;
aux_uids = aubuf;
gen_gids = ggbuf;
aux_gids = agbuf;
ngen_uids = sizeof (gubuf) / sizeof (uid_t);
naux_uids = sizeof (aubuf) / sizeof (uid_t);
ngen_gids = sizeof (ggbuf) / sizeof (uid_t);
naux_gids = sizeof (agbuf) / sizeof (uid_t);
new_proc_port = mach_reply_port ();
err = mach_port_request_notification (mach_task_self (), new_proc_port,
MACH_NOTIFY_NO_SENDERS, 1,
new_proc_port,
MACH_MSG_TYPE_MAKE_SEND_ONCE,
&prev);
assert_perror_backtrace (err);
assert_backtrace (prev == MACH_PORT_NULL);
pthread_mutex_unlock (&global_lock);
do
err = auth_server_authenticate (authserver,
rendport, MACH_MSG_TYPE_COPY_SEND,
new_proc_port, MACH_MSG_TYPE_MAKE_SEND,
&gen_uids, &ngen_uids,
&aux_uids, &naux_uids,
&gen_gids, &ngen_gids,
&aux_gids, &naux_gids);
while (err == EINTR);
if (!err)
{
msg.msgh_size = sizeof (mach_msg_header_t);
msg.msgh_local_port = new_proc_port;
err = mach_msg_receive (&msg);
}
pthread_mutex_lock (&global_lock);
if (err)
goto out;
if (msg.msgh_id != 24064)
err = EINVAL;
mach_msg_destroy (&msg);
if (err)
goto out;
if (p->p_dead)
{
err = EAGAIN;
goto out;
}
new_ids = make_ids (gen_uids, ngen_uids, aux_uids, naux_uids);
if (!new_ids)
{
err = errno;
goto out;
}
mach_port_deallocate (mach_task_self (), rendport);
ids_rele (p->p_id);
p->p_id = new_ids;
ports_reallocate_from_external (p, new_proc_port);
out:
if (gen_uids != gubuf)
munmap (gen_uids, ngen_uids * sizeof (uid_t));
if (aux_uids != aubuf)
munmap (aux_uids, naux_uids * sizeof (uid_t));
if (gen_gids != ggbuf)
munmap (gen_gids, ngen_gids * sizeof (uid_t));
if (aux_gids != agbuf)
munmap (aux_gids, naux_gids * sizeof (uid_t));
if (err)
mach_port_mod_refs (mach_task_self (), new_proc_port,
MACH_PORT_RIGHT_RECEIVE, -1);
return err;
}
kern_return_t
S_proc_child (struct proc *parentp,
task_t childt)
{
struct proc *childp;
if (!parentp)
return EOPNOTSUPP;
childp = task_find (childt);
if (!childp)
return ESRCH;
if (childp->p_parentset)
return EBUSY;
mach_port_deallocate (mach_task_self (), childt);
if (!--childp->p_login->l_refcnt)
free (childp->p_login);
childp->p_login = parentp->p_login;
childp->p_login->l_refcnt++;
ids_rele (childp->p_id);
ids_ref (parentp->p_id);
childp->p_id = parentp->p_id;
assert_backtrace (childp->p_parent == init_proc);
if (childp->p_sib)
childp->p_sib->p_prevsib = childp->p_prevsib;
*childp->p_prevsib = childp->p_sib;
childp->p_parent = parentp;
childp->p_sib = parentp->p_ochild;
childp->p_prevsib = &parentp->p_ochild;
if (parentp->p_ochild)
parentp->p_ochild->p_prevsib = &childp->p_sib;
parentp->p_ochild = childp;
if (childp->p_pgrp != parentp->p_pgrp)
{
leave_pgrp (childp);
childp->p_pgrp = parentp->p_pgrp;
join_pgrp (childp);
}
else if (childp->p_msgport != MACH_PORT_NULL)
nowait_msg_proc_newids (childp->p_msgport, childp->p_task,
childp->p_parent->p_pid, childp->p_pgrp->pg_pgid,
!childp->p_pgrp->pg_orphcnt);
childp->p_parentset = 1;
if (! childp->start_code && ! childp->end_code)
{
childp->start_code = parentp->start_code;
childp->end_code = parentp->end_code;
}
if (! childp->exe && parentp->exe)
childp->exe = strdup (parentp->exe);
if (MACH_PORT_VALID (parentp->p_task_namespace))
{
mach_port_mod_refs (mach_task_self (), parentp->p_task_namespace,
MACH_PORT_RIGHT_SEND, +1);
childp->p_task_namespace = parentp->p_task_namespace;
}
return 0;
}
kern_return_t
S_proc_reassign (struct proc *p,
task_t newt)
{
struct proc *stubp;
if (!p)
return EOPNOTSUPP;
stubp = task_find (newt);
if (!stubp)
return ESRCH;
if (stubp == p)
return EINVAL;
mach_port_deallocate (mach_task_self (), newt);
remove_proc_from_hash (p);
task_terminate (p->p_task);
mach_port_deallocate (mach_task_self (), p->p_task);
p->p_task = stubp->p_task;
ports_transfer_right (p, stubp);
if (p->p_msgport != MACH_PORT_NULL)
{
mach_port_deallocate (mach_task_self (), p->p_msgport);
p->p_msgport = MACH_PORT_NULL;
p->p_deadmsg = 1;
}
p->p_argv = stubp->p_argv;
p->p_envp = stubp->p_envp;
stubp->p_task = MACH_PORT_NULL;
process_has_exited (stubp);
stubp->p_waited = 1;
complete_exit (stubp);
add_proc_to_hash (p);
return 0;
}
kern_return_t
S_proc_reauthenticate_reassign (struct proc *p,
mach_port_t rendezvous,
task_t new_task)
{
error_t err;
mach_port_t new_proc_port, prev;
mach_msg_header_t msg;
struct proc *stubp;
struct ids *new_ids;
uid_t gubuf[50], aubuf[50], ggbuf[50], agbuf[50];
uid_t *gen_uids, *aux_uids, *gen_gids, *aux_gids;
mach_msg_type_number_t ngen_uids, naux_uids, ngen_gids, naux_gids;
if (!p)
return EOPNOTSUPP;
if (!MACH_PORT_VALID (rendezvous))
return EINVAL;
stubp = task_find (new_task);
if (!stubp)
return ESRCH;
if (stubp == p)
return EINVAL;
ports_port_ref (stubp);
gen_uids = gubuf;
aux_uids = aubuf;
gen_gids = ggbuf;
aux_gids = agbuf;
ngen_uids = sizeof (gubuf) / sizeof (uid_t);
naux_uids = sizeof (aubuf) / sizeof (uid_t);
ngen_gids = sizeof (ggbuf) / sizeof (uid_t);
naux_gids = sizeof (agbuf) / sizeof (uid_t);
new_proc_port = mach_reply_port ();
err = mach_port_request_notification (mach_task_self (), new_proc_port,
MACH_NOTIFY_NO_SENDERS, 1,
new_proc_port,
MACH_MSG_TYPE_MAKE_SEND_ONCE,
&prev);
assert_perror_backtrace (err);
assert_backtrace (prev == MACH_PORT_NULL);
pthread_mutex_unlock (&global_lock);
do
err = auth_server_authenticate (authserver,
rendezvous,
MACH_MSG_TYPE_COPY_SEND,
new_proc_port,
MACH_MSG_TYPE_MAKE_SEND,
&gen_uids, &ngen_uids,
&aux_uids, &naux_uids,
&gen_gids, &ngen_gids,
&aux_gids, &naux_gids);
while (err == EINTR);
if (!err)
{
msg.msgh_size = sizeof (mach_msg_header_t);
msg.msgh_local_port = new_proc_port;
err = mach_msg_receive (&msg);
}
pthread_mutex_lock (&global_lock);
if (err)
goto out;
if (msg.msgh_id != 24064)
err = EINVAL;
mach_msg_destroy (&msg);
if (err)
goto out;
if (p->p_dead || stubp->p_dead || stubp->p_task != new_task)
{
err = EAGAIN;
goto out;
}
new_ids = make_ids (gen_uids, ngen_uids, aux_uids, naux_uids);
if (!new_ids)
{
err = errno;
goto out;
}
ids_rele (p->p_id);
p->p_id = new_ids;
mach_port_deallocate (mach_task_self (), new_task);
mach_port_deallocate (mach_task_self (), rendezvous);
remove_proc_from_hash (p);
task_terminate (p->p_task);
mach_port_deallocate (mach_task_self (), p->p_task);
p->p_task = stubp->p_task;
stubp->p_task = MACH_PORT_NULL;
ports_destroy_right (stubp);
ports_reallocate_from_external (p, new_proc_port);
if (MACH_PORT_VALID (p->p_msgport))
{
mach_port_deallocate (mach_task_self (), p->p_msgport);
p->p_msgport = MACH_PORT_NULL;
p->p_deadmsg = 1;
}
p->p_argv = stubp->p_argv;
p->p_envp = stubp->p_envp;
process_has_exited (stubp);
stubp->p_waited = 1;
complete_exit (stubp);
add_proc_to_hash (p);
err = 0;
out:
if (gen_uids != gubuf)
munmap (gen_uids, ngen_uids * sizeof (uid_t));
if (aux_uids != aubuf)
munmap (aux_uids, naux_uids * sizeof (uid_t));
if (gen_gids != ggbuf)
munmap (gen_gids, ngen_gids * sizeof (uid_t));
if (aux_gids != agbuf)
munmap (aux_gids, naux_gids * sizeof (uid_t));
if (err)
mach_port_mod_refs (mach_task_self (), new_proc_port,
MACH_PORT_RIGHT_RECEIVE, -1);
ports_port_deref (stubp);
return err;
}
kern_return_t
S_proc_setowner (struct proc *p,
uid_t owner,
int clear)
{
return EOPNOTSUPP;
}
kern_return_t
S_proc_reauthenticate_complete (struct proc *p)
{
return EOPNOTSUPP;
}
kern_return_t
S_proc_getpids (struct proc *p,
pid_t *pid,
pid_t *ppid,
int *orphaned)
{
if (!p)
return EOPNOTSUPP;
*pid = p->p_pid;
*ppid = p->p_parent->p_pid;
*orphaned = !p->p_pgrp->pg_orphcnt;
return 0;
}
kern_return_t
S_proc_set_arg_locations (struct proc *p,
vm_address_t argv,
vm_address_t envp)
{
if (!p)
return EOPNOTSUPP;
p->p_argv = argv;
p->p_envp = envp;
return 0;
}
kern_return_t
S_proc_get_arg_locations (struct proc *p,
vm_address_t *argv,
vm_address_t *envp)
{
*argv = p->p_argv;
*envp = p->p_envp;
return 0;
}
kern_return_t
S_proc_set_entry (struct proc *p, vm_address_t entry)
{
if (!p)
return EOPNOTSUPP;
p->p_entry = entry;
return 0;
}
kern_return_t
S_proc_get_entry (struct proc *p, vm_address_t *entry)
{
*entry = p->p_entry;
return 0;
}
kern_return_t
S_proc_dostop (struct proc *p,
thread_t contthread)
{
thread_t threadbuf[2], *threads = threadbuf;
mach_msg_type_number_t nthreads = sizeof (threadbuf) / sizeof (thread_t);
int i;
error_t err;
if (!p)
return EOPNOTSUPP;
err = task_suspend (p->p_task);
if (err)
return err;
err = task_threads (p->p_task, &threads, &nthreads);
if (err)
{
task_resume (p->p_task);
return err;
}
for (i = 0; i < nthreads; i++)
{
if (threads[i] != contthread)
thread_suspend (threads[i]);
mach_port_deallocate (mach_task_self (), threads[i]);
}
if (threads != threadbuf)
munmap (threads, nthreads * sizeof (thread_t));
err = task_resume (p->p_task);
if (err)
return err;
mach_port_deallocate (mach_task_self (), contthread);
return 0;
}
void
exc_clean (void *arg)
{
struct exc *e = arg;
mach_port_deallocate (mach_task_self (), e->forwardport);
}
kern_return_t
S_proc_handle_exceptions (struct proc *p,
mach_port_t msgport,
mach_port_t forwardport,
int flavor,
thread_state_t new_state,
mach_msg_type_number_t statecnt)
{
struct exc *e;
error_t err;
err = ports_import_port (exc_class, proc_bucket, msgport,
(sizeof (struct exc)
+ (statecnt * sizeof (natural_t))), &e);
if (err)
return err;
e->forwardport = forwardport;
e->flavor = flavor;
e->statecnt = statecnt;
memcpy (e->thread_state, new_state, statecnt * sizeof (natural_t));
ports_port_deref (e);
return 0;
}
kern_return_t
S_proc_exception_raise (struct exc *e,
mach_port_t reply,
mach_msg_type_name_t reply_type,
mach_port_t thread,
mach_port_t task,
integer_t exception,
integer_t code,
long_integer_t subcode)
{
error_t err;
struct proc *p;
if (!e || e->pi.bucket != proc_bucket || e->pi.class != exc_class)
return EOPNOTSUPP;
p = task_find (task);
if (! p)
{
return EINVAL;
}
err = proc_exception_raise (e->forwardport,
reply, reply_type, MACH_SEND_NOTIFY,
thread, task, exception, code, subcode);
switch (err)
{
struct hurd_signal_detail hsd;
int signo;
case 0:
err = thread_set_state (thread, e->flavor, e->thread_state, e->statecnt);
if (err)
return err;
mach_port_deallocate (mach_task_self (), thread);
mach_port_deallocate (mach_task_self (), task);
return MIG_NO_REPLY;
default:
case MACH_SEND_NOTIFY_IN_PROGRESS:
hsd.exc = exception;
hsd.exc_code = code;
hsd.exc_subcode = subcode;
_hurd_exception2signal (&hsd, &signo);
p->p_exiting = 1;
p->p_status = W_EXITCODE (0, signo);
p->p_sigcode = hsd.code;
task_terminate (task);
mach_port_destroy (mach_task_self (), thread);
return MIG_NO_REPLY;
}
}
static void
count_up (struct proc *p, void *counter)
{
++*(int *)counter;
}
static void
store_pid (struct proc *p, void *loc)
{
*(*(pid_t **)loc)++ = p->p_pid;
}
kern_return_t
S_proc_getallpids (struct proc *p,
pid_t **pids,
mach_msg_type_number_t *pidslen)
{
int nprocs;
pid_t *loc;
add_tasks (0);
nprocs = 0;
prociterate (count_up, &nprocs);
if (nprocs > *pidslen)
{
*pids = mmap (0, nprocs * sizeof (pid_t), PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
if (*pids == MAP_FAILED)
return ENOMEM;
}
loc = *pids;
prociterate (store_pid, &loc);
*pidslen = nprocs;
return 0;
}
struct proc *
allocate_proc (task_t task)
{
error_t err;
struct proc *p;
err = ports_create_port (proc_class, proc_bucket, sizeof (struct proc), &p);
if (err)
return NULL;
memset (&p->p_pi + 1, 0, sizeof *p - sizeof p->p_pi);
p->p_task = task;
p->p_task_namespace = MACH_PORT_NULL;
p->p_msgport = MACH_PORT_NULL;
pthread_cond_init (&p->p_wakeup, NULL);
return p;
}
struct proc *
create_init_proc (void)
{
static const uid_t zero;
struct proc *p;
const char *rootsname = "root";
p = allocate_proc (MACH_PORT_NULL);
assert_backtrace (p);
p->p_pid = HURD_PID_INIT;
p->p_parent = p;
p->p_sib = 0;
p->p_prevsib = &p->p_ochild;
p->p_ochild = p;
p->p_parentset = 1;
p->p_deadmsg = 1;
p->p_important = 1;
p->p_id = make_ids (&zero, 1, &zero, 0);
assert_backtrace (p->p_id);
p->p_loginleader = 1;
p->p_login = malloc (sizeof (struct login) + strlen (rootsname) + 1);
assert_backtrace (p->p_login);
p->p_login->l_refcnt = 1;
strcpy (p->p_login->l_name, rootsname);
boot_setsid (p);
return p;
}
void
proc_death_notify (struct proc *p)
{
error_t err;
err = ports_request_dead_name_notification (p, p->p_task, NULL);
assert_perror_backtrace (err);
}
void
complete_proc (struct proc *p, pid_t pid)
{
static struct login *nulllogin;
static struct ids nullids = { i_refcnt : 1, i_nuids : 0};
const char nullsname [] = "<none>";
if (!nulllogin)
{
nulllogin = malloc (sizeof (struct login) + sizeof (nullsname) + 1);
nulllogin->l_refcnt = 1;
strcpy (nulllogin->l_name, nullsname);
}
p->p_pid = pid;
if (pid == HURD_PID_STARTUP)
{
static const uid_t zero;
p->p_id = make_ids (&zero, 1, &zero, 0);
assert_backtrace (p->p_id);
}
else
{
ids_ref (&nullids);
p->p_id = &nullids;
}
p->p_login = nulllogin;
p->p_login->l_refcnt++;
p->p_parent = init_proc;
p->p_sib = init_proc->p_ochild;
p->p_prevsib = &init_proc->p_ochild;
if (p->p_sib)
p->p_sib->p_prevsib = &p->p_sib;
init_proc->p_ochild = p;
p->p_loginleader = 0;
p->p_ochild = 0;
p->p_parentset = 0;
p->p_pgrp = init_proc->p_pgrp;
if (pid != HURD_PID_STARTUP)
add_proc_to_hash (p);
join_pgrp (p);
if (pid != HURD_PID_STARTUP)
proc_death_notify (p);
}
static struct proc *
new_proc (task_t task)
{
struct proc *p;
p = allocate_proc (task);
if (p)
complete_proc (p, genpid ());
return p;
}
int
namespace_is_subprocess (struct proc *p)
{
return (p
&& MACH_PORT_VALID (p->p_task_namespace)
&& p->p_parent
&& MACH_PORT_VALID (p->p_parent->p_task_namespace));
}
error_t
namespace_translate_pids (mach_port_t namespace, pid_t *pids, size_t pids_len)
{
size_t i;
task_t *tasks;
tasks = calloc (pids_len, sizeof *tasks);
if (tasks == NULL)
{
pthread_mutex_lock (&global_lock);
return ENOMEM;
}
for (i = 0; i < pids_len; i++)
proc_pid2task (namespace, pids[i], &tasks[i]);
pthread_mutex_lock (&global_lock);
for (i = 0; i < pids_len; i++)
if (MACH_PORT_VALID (tasks[i]))
{
struct proc *p = task_find_nocreate (tasks[i]);
mach_port_deallocate (mach_task_self (), tasks[i]);
pids[i] = p ? p->p_pid : (pid_t) -1;
}
else
pids[i] = (pid_t) -1;
free (tasks);
return 0;
}
struct proc *
namespace_find_root (struct proc *p)
{
for (;
MACH_PORT_VALID (p->p_parent->p_task_namespace);
p = p->p_parent)
{
}
return p;
}
static void
namespace_terminate (struct proc *p, void *cookie)
{
mach_port_t *namespacep = cookie;
if (p->p_task_namespace == *namespacep)
task_terminate (p->p_task);
}
void
process_has_exited (struct proc *p)
{
if (p->p_dead)
return;
p->p_waited = 0;
if (p->p_task != MACH_PORT_NULL)
alert_parent (p);
if (p->p_msgport)
mach_port_deallocate (mach_task_self (), p->p_msgport);
p->p_msgport = MACH_PORT_NULL;
prociterate ((void (*) (struct proc *, void *))check_message_dying, p);
ports_destroy_right (p);
if (!--p->p_login->l_refcnt)
free (p->p_login);
free (p->exe);
p->exe = NULL;
ids_rele (p->p_id);
if (p->p_ochild)
{
struct proc *reparent_to = init_proc;
struct proc *tp;
int isdead = 0;
if (MACH_PORT_VALID (p->p_task_namespace))
{
tp = namespace_find_root (p);
if (p == tp)
{
prociterate (namespace_terminate, &p->p_task_namespace);
mach_port_deallocate (mach_task_self (), p->p_task_namespace);
p->p_task_namespace = MACH_PORT_NULL;
}
else
reparent_to = tp;
}
for (tp = p->p_ochild; tp->p_sib; tp = tp->p_sib)
{
if (tp->p_msgport != MACH_PORT_NULL)
nowait_msg_proc_newids (tp->p_msgport, tp->p_task,
1, tp->p_pgrp->pg_pgid,
!tp->p_pgrp->pg_orphcnt);
tp->p_parent = reparent_to;
if (tp->p_dead)
isdead = 1;
}
if (tp->p_msgport != MACH_PORT_NULL)
nowait_msg_proc_newids (tp->p_msgport, tp->p_task,
1, tp->p_pgrp->pg_pgid,
!tp->p_pgrp->pg_orphcnt);
tp->p_parent = reparent_to;
tp->p_sib = reparent_to->p_ochild;
if (tp->p_sib)
tp->p_sib->p_prevsib = &tp->p_sib;
reparent_to->p_ochild = p->p_ochild;
p->p_ochild->p_prevsib = &reparent_to->p_ochild;
if (isdead)
alert_parent (reparent_to);
}
if (p->p_waiting || p->p_msgportwait)
pthread_cond_broadcast (&p->p_wakeup);
p->p_dead = 1;
ports_interrupt_rpcs (p);
if (MACH_PORT_VALID (p->p_task_namespace))
{
mach_port_deallocate (mach_task_self (), p->p_task_namespace);
p->p_waited = 1;
complete_exit (p);
}
}
void
complete_exit (struct proc *p)
{
assert_backtrace (p->p_dead);
assert_backtrace (p->p_waited);
remove_proc_from_hash (p);
if (p->p_task != MACH_PORT_NULL)
{
mach_port_deallocate (mach_task_self (), p->p_task);
p->p_task = MACH_PORT_NULL;
}
if (p->p_sib)
p->p_sib->p_prevsib = p->p_prevsib;
*p->p_prevsib = p->p_sib;
leave_pgrp (p);
ports_port_deref (p);
}
struct proc *
add_tasks (task_t task)
{
mach_port_t *psets;
mach_msg_type_number_t npsets;
int i;
struct proc *foundp = 0;
host_processor_sets (mach_host_self (), &psets, &npsets);
for (i = 0; i < npsets; i++)
{
mach_port_t psetpriv;
mach_port_t *tasks;
mach_msg_type_number_t ntasks;
int j;
if (!foundp)
{
host_processor_set_priv (_hurd_host_priv, psets[i], &psetpriv);
processor_set_tasks (psetpriv, &tasks, &ntasks);
for (j = 0; j < ntasks; j++)
{
int set = 0;
if (! MACH_PORT_VALID (tasks[j]))
continue;
if (!foundp)
{
struct proc *p = task_find_nocreate (tasks[j]);
if (!p)
{
p = new_proc (tasks[j]);
if (p)
set = 1;
}
if (!foundp && tasks[j] == task)
foundp = p;
}
if (!set)
mach_port_deallocate (mach_task_self (), tasks[j]);
}
munmap (tasks, ntasks * sizeof (task_t));
mach_port_deallocate (mach_task_self (), psetpriv);
}
mach_port_deallocate (mach_task_self (), psets[i]);
}
munmap (psets, npsets * sizeof (mach_port_t));
return foundp;
}
int
genpid (void)
{
#define WRAP_AROUND 30000
#define START_OVER 100
static int nextpid = 1;
static int wrap = WRAP_AROUND;
while (nextpid < wrap && !pidfree (nextpid))
++nextpid;
if (nextpid >= wrap)
{
nextpid = START_OVER;
while (!pidfree (nextpid))
nextpid++;
while (nextpid > wrap)
wrap *= 2;
}
return nextpid++;
}
kern_return_t
S_proc_set_init_task(struct proc *callerp,
task_t task)
{
struct proc *shadow;
if (! callerp)
return EOPNOTSUPP;
if (callerp != startup_proc)
return EPERM;
shadow = task_find_nocreate (task);
if (shadow)
{
shadow->p_dead = 1;
shadow->p_waited = 1;
mach_port_deallocate (mach_task_self (), shadow->p_task);
shadow->p_task = MACH_PORT_NULL;
complete_exit (shadow);
}
init_proc->p_task = task;
add_proc_to_hash (init_proc);
proc_death_notify (init_proc);
return 0;
}
kern_return_t
S_proc_mark_important (struct proc *p)
{
if (!p)
return EOPNOTSUPP;
if (! check_uid (p, 0) && p->p_parent != startup_proc)
return EPERM;
p->p_important = 1;
return 0;
}
kern_return_t
S_proc_is_important (struct proc *callerp,
boolean_t *essential)
{
if (!callerp)
return EOPNOTSUPP;
*essential = callerp->p_important;
return 0;
}
kern_return_t
S_proc_set_code (struct proc *callerp,
vm_address_t start_code,
vm_address_t end_code)
{
if (!callerp)
return EOPNOTSUPP;
callerp->start_code = start_code;
callerp->end_code = end_code;
return 0;
}
kern_return_t
S_proc_get_code (struct proc *callerp,
vm_address_t *start_code,
vm_address_t *end_code)
{
if (!callerp)
return EOPNOTSUPP;
*start_code = callerp->start_code;
*end_code = callerp->end_code;
return 0;
}
kern_return_t
S_mach_notify_new_task (struct port_info *notify,
mach_port_t task,
mach_port_t parent)
{
struct proc *parentp, *childp;
if (! notify
|| (kernel_proc == NULL && notify->class != generic_port_class)
|| (kernel_proc != NULL && notify != (struct port_info *) kernel_proc))
return EOPNOTSUPP;
if (task == MACH_PORT_DEAD)
return ESRCH;
parentp = task_find_nocreate (parent);
if (! parentp)
return ESRCH;
childp = task_find_nocreate (task);
if (! childp)
{
mach_port_mod_refs (mach_task_self (), task, MACH_PORT_RIGHT_SEND, +1);
childp = new_proc (task);
}
if (MACH_PORT_VALID (parentp->p_task_namespace))
{
error_t err;
mach_port_mod_refs (mach_task_self (), task, MACH_PORT_RIGHT_SEND, +1);
err = S_proc_child (parentp, task);
assert_perror_backtrace (err);
return mach_notify_new_task (childp->p_task_namespace, task, parent);
}
mach_port_deallocate (mach_task_self (), task);
mach_port_deallocate (mach_task_self (), parent);
return 0;
}
kern_return_t
S_proc_make_task_namespace (struct proc *callerp,
mach_port_t notify)
{
if (! callerp)
return EOPNOTSUPP;
if (! MACH_PORT_VALID (notify))
return EINVAL;
if (MACH_PORT_VALID (callerp->p_task_namespace))
return EBUSY;
callerp->p_task_namespace = notify;
return 0;
}