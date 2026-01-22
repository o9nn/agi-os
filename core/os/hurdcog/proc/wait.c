#include <mach.h>
#include <sys/types.h>
#include <hurd/hurd_types.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <mach/task_info.h>
#include "proc.h"
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdlib.h>
#include <assert-backtrace.h>
#define assert	assert_backtrace
#include "process_S.h"
#include <mach/mig_errors.h>
#ifndef WCONTINUED
#define WCONTINUED 4
#endif
#ifndef WNOWAIT
#define WNOWAIT 8
#endif
#ifndef WEXITED
#define WEXITED 16
#endif
static inline void
rusage_add (struct rusage *acc, const struct rusage *b)
{
timeradd (&acc->ru_utime, &b->ru_utime, &acc->ru_utime);
timeradd (&acc->ru_stime, &b->ru_stime, &acc->ru_stime);
acc->ru_maxrss += b->ru_maxrss;
acc->ru_ixrss += b->ru_ixrss;
acc->ru_idrss += b->ru_idrss;
acc->ru_isrss += b->ru_isrss;
acc->ru_minflt += b->ru_minflt;
acc->ru_majflt += b->ru_majflt;
acc->ru_nswap += b->ru_nswap;
acc->ru_inblock += b->ru_inblock;
acc->ru_oublock += b->ru_oublock;
acc->ru_msgsnd += b->ru_msgsnd;
acc->ru_msgrcv += b->ru_msgrcv;
acc->ru_nsignals += b->ru_nsignals;
acc->ru_nvcsw += b->ru_nvcsw;
acc->ru_nivcsw += b->ru_nivcsw;
}
void
sample_rusage (struct proc *p)
{
struct task_basic_info bi;
struct task_events_info ei;
struct task_thread_times_info tti;
mach_msg_type_number_t count;
error_t err;
count = TASK_BASIC_INFO_COUNT;
err = task_info (p->p_task, TASK_BASIC_INFO,
(task_info_t) &bi, &count);
if (err)
memset (&bi, 0, sizeof bi);
count = TASK_EVENTS_INFO_COUNT;
err = task_info (p->p_task, TASK_EVENTS_INFO,
(task_info_t) &ei, &count);
if (err)
memset (&ei, 0, sizeof ei);
count = TASK_THREAD_TIMES_INFO_COUNT;
err = task_info (p->p_task, TASK_THREAD_TIMES_INFO,
(task_info_t) &tti, &count);
if (err)
memset (&tti, 0, sizeof tti);
time_value_add (&bi.user_time, &tti.user_time);
time_value_add (&bi.system_time, &tti.system_time);
memset (&p->p_rusage, 0, sizeof (struct rusage));
p->p_rusage.ru_utime.tv_sec = bi.user_time.seconds;
p->p_rusage.ru_utime.tv_usec = bi.user_time.microseconds;
p->p_rusage.ru_stime.tv_sec = bi.system_time.seconds;
p->p_rusage.ru_stime.tv_usec = bi.system_time.microseconds;
p->p_rusage.ru_majflt = ei.pageins;
p->p_rusage.ru_minflt = ei.faults - ei.pageins;
p->p_rusage.ru_msgsnd = ei.messages_sent;
p->p_rusage.ru_msgrcv = ei.messages_received;
}
static inline int
waiter_cares (pid_t wait_pid, pid_t mypgrp,
pid_t pid, pid_t pgrp)
{
return (wait_pid == pid ||
wait_pid == -pgrp ||
wait_pid == WAIT_ANY ||
(wait_pid == WAIT_MYPGRP && pgrp == mypgrp));
}
void
alert_parent (struct proc *p)
{
rusage_add (&p->p_parent->p_child_rusage, &p->p_rusage);
rusage_add (&p->p_parent->p_child_rusage, &p->p_child_rusage);
send_signal (p->p_parent->p_msgport, SIGCHLD, CLD_EXITED, p->p_parent->p_task);
if (!p->p_exiting)
{
p->p_status = W_EXITCODE (0, SIGKILL);
p->p_sigcode = -1;
}
if (p->p_parent->p_waiting)
{
pthread_cond_broadcast (&p->p_parent->p_wakeup);
p->p_parent->p_waiting = 0;
}
}
kern_return_t
S_proc_waitid (struct proc *p,
mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
pid_t pid,
int options,
int *status,
int *sigcode,
struct rusage *ru,
pid_t *pid_status)
{
int cancel;
int reap (struct proc *child)
{
if (child->p_waited
|| ((!child->p_dead || !(options & WEXITED))
&& (!child->p_stopped
|| !(child->p_traced || (options & WUNTRACED)))
&& (!child->p_continued || !(options & WCONTINUED))))
return 0;
*status = child->p_status;
*sigcode = child->p_sigcode;
*ru = child->p_rusage;
*pid_status = child->p_pid;
if (!(options & WNOWAIT))
{
child->p_waited = 1;
if (child->p_dead)
complete_exit (child);
}
return 1;
}
if (!p)
return EOPNOTSUPP;
start_over:
if (!p->p_ochild)
return ECHILD;
if (pid > 0)
{
struct proc *child = pid_find_allow_zombie (pid);
if (!child || child->p_parent != p)
return ECHILD;
if (reap (child))
return 0;
}
else
{
struct proc *child;
int had_a_match = pid == 0;
for (child = p->p_ochild; child; child = child->p_sib)
if (waiter_cares (pid, p->p_pgrp->pg_pgid,
child->p_pid, child->p_pgrp->pg_pgid))
{
if (reap (child))
return 0;
had_a_match = 1;
}
if (!had_a_match)
return ECHILD;
}
if (options & WNOHANG)
return EWOULDBLOCK;
p->p_waiting = 1;
cancel = pthread_hurd_cond_wait_np (&p->p_wakeup, &global_lock);
if (p->p_dead)
return EOPNOTSUPP;
if (cancel)
return EINTR;
goto start_over;
}
kern_return_t
S_proc_wait (struct proc *p,
mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
pid_t pid,
int options,
int *status,
int *sigcode,
struct rusage *ru,
pid_t *pid_status)
{
return S_proc_waitid(p, reply_port, reply_port_type, pid,
options | WEXITED,
status, sigcode, ru, pid_status);
}
kern_return_t
S_proc_mark_stop (struct proc *p,
int signo,
int sigcode)
{
if (!p)
return EOPNOTSUPP;
p->p_stopped = 1;
p->p_continued = 0;
p->p_status = W_STOPCODE (signo);
p->p_sigcode = sigcode;
p->p_waited = 0;
if (p->p_parent->p_waiting)
{
pthread_cond_broadcast (&p->p_parent->p_wakeup);
p->p_parent->p_waiting = 0;
}
if (!p->p_parent->p_nostopcld)
send_signal (p->p_parent->p_msgport, SIGCHLD, CLD_STOPPED, p->p_parent->p_task);
return 0;
}
kern_return_t
S_proc_mark_exit (struct proc *p,
int status,
int sigcode)
{
if (!p)
return EOPNOTSUPP;
if (WIFSTOPPED (status))
return EINVAL;
sample_rusage (p);
if (p->p_exiting)
return EBUSY;
p->p_exiting = 1;
p->p_status = status;
p->p_sigcode = sigcode;
return 0;
}
kern_return_t
S_proc_mark_cont (struct proc *p)
{
if (!p)
return EOPNOTSUPP;
p->p_stopped = 0;
p->p_continued = 1;
p->p_status = __W_CONTINUED;
p->p_waited = 0;
if (p->p_parent->p_waiting)
{
pthread_cond_broadcast (&p->p_parent->p_wakeup);
p->p_parent->p_waiting = 0;
}
if (!p->p_parent->p_nostopcld)
send_signal (p->p_parent->p_msgport, SIGCHLD, CLD_CONTINUED, p->p_parent->p_task);
return 0;
}
kern_return_t
S_proc_mark_traced (struct proc *p)
{
if (!p)
return EOPNOTSUPP;
p->p_traced = 1;
return 0;
}
kern_return_t
S_proc_mod_stopchild (struct proc *p,
int value)
{
if (!p)
return EOPNOTSUPP;
p->p_nostopcld = ! value;
return 0;
}