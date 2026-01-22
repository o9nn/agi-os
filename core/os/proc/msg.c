#include <mach.h>
#include <hurd.h>
#include "proc.h"
#include <hurd/startup.h>
#include <assert-backtrace.h>
#include <stdlib.h>
#include <stdio.h>
void
check_message_return (struct proc *p, void *availpaddr)
{
if (p->p_msgportwait)
{
pthread_cond_broadcast (&p->p_wakeup);
p->p_msgportwait = 0;
}
}
static void *
tickle_startup (void *startupport)
{
pthread_setname_np (pthread_self (), "startup");
startup_essential_task ((mach_port_t) (uintptr_t) startupport,
mach_task_self (), MACH_PORT_NULL,
"proc", _hurd_host_priv);
return NULL;
}
error_t
S_proc_setmsgport (struct proc *p,
mach_port_t reply, mach_msg_type_name_t replytype,
mach_port_t msgport,
mach_port_t *oldmsgport,
mach_msg_type_name_t *oldmsgport_type)
{
if (!p)
return EOPNOTSUPP;
*oldmsgport = p->p_msgport;
*oldmsgport_type = MACH_MSG_TYPE_MOVE_SEND;
p->p_msgport = msgport;
p->p_deadmsg = 0;
if (p->p_checkmsghangs)
prociterate (check_message_return, p);
p->p_checkmsghangs = 0;
if (p == startup_proc && startup_fallback)
{
pthread_t thread;
error_t err;
err = pthread_create (&thread, NULL, tickle_startup,
(void*) (uintptr_t) msgport);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
}
return 0;
}
void
check_message_dying (struct proc *p, struct proc *dyingp)
{
if (p->p_msgportwait)
{
pthread_cond_broadcast (&p->p_wakeup);
p->p_msgportwait = 0;
}
}
int
check_msgport_death (struct proc *p)
{
if (p->p_msgport != MACH_PORT_NULL)
{
mach_port_type_t type;
error_t err;
err = mach_port_type (mach_task_self (), p->p_msgport, &type);
if (err || (type & MACH_PORT_TYPE_DEAD_NAME))
{
mach_port_deallocate (mach_task_self (), p->p_msgport);
p->p_msgport = MACH_PORT_NULL;
p->p_deadmsg = 1;
return 1;
}
}
return 0;
}
error_t
S_proc_getmsgport (struct proc *callerp,
mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
pid_t pid,
mach_port_t *msgport,
mach_msg_type_name_t *msgport_type)
{
int cancel;
struct proc *p;
if (!callerp)
return EOPNOTSUPP;
p = pid_find_allow_zombie (pid);
if (namespace_is_subprocess (p))
{
error_t err;
pid_t pid_sub;
pthread_mutex_unlock (&global_lock);
err = proc_task2pid (p->p_task_namespace, p->p_task, &pid_sub);
if (! err)
err = proc_getmsgport (p->p_task_namespace, pid_sub, msgport);
pthread_mutex_lock (&global_lock);
if (! err)
{
*msgport_type = MACH_MSG_TYPE_MOVE_SEND;
return 0;
}
}
restart:
while (p && p->p_deadmsg && !p->p_dead)
{
callerp->p_msgportwait = 1;
p->p_checkmsghangs = 1;
cancel = pthread_hurd_cond_wait_np (&callerp->p_wakeup, &global_lock);
if (callerp->p_dead)
return EOPNOTSUPP;
if (cancel)
return EINTR;
p = pid_find_allow_zombie (pid);
}
if (!p)
return ESRCH;
if (check_msgport_death (p))
goto restart;
*msgport_type = MACH_MSG_TYPE_COPY_SEND;
*msgport = p->p_msgport;
return 0;
}