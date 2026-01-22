#include <mach.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <hurd/hurd_types.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/resource.h>
#include <assert-backtrace.h>
#include <hurd/msg.h>
#include "proc.h"
#include "process_S.h"
int
check_owner (struct proc *proc1, struct proc *proc2)
{
if (proc1 == proc2)
return 1;
if (!proc1->p_id || !proc1->p_id->i_nuids)
return 0;
if (check_uid (proc1, 0))
return 1;
if (!proc2->p_id || !proc2->p_id->i_nuids)
return 0;
for (size_t i = 0; i < proc2->p_id->i_nuids; i++)
if (!check_uid (proc1, proc2->p_id->i_uids[i]))
return 0;
return 1;
}
kern_return_t
S_proc_pid2task (struct proc *callerp,
pid_t pid,
task_t *t)
{
struct proc *p;
if (!callerp)
return EOPNOTSUPP;
p = pid_find_allow_zombie (pid);
if (!p)
return ESRCH;
if (p->p_dead)
{
*t = MACH_PORT_NULL;
return 0;
}
if (! check_owner (callerp, p))
return EPERM;
assert_backtrace (MACH_PORT_VALID (p->p_task));
*t = p->p_task;
return 0;
}
kern_return_t
S_proc_task2pid (struct proc *callerp,
task_t t,
pid_t *pid)
{
struct proc *p = task_find (t);
if (!p)
return ESRCH;
*pid = p->p_pid;
mach_port_deallocate (mach_task_self (), t);
return 0;
}
kern_return_t
S_proc_task2proc (struct proc *callerp,
task_t t,
mach_port_t *outproc,
mach_msg_type_name_t *outproc_type)
{
struct proc *p = task_find (t);
if (!p)
return ESRCH;
if (namespace_is_subprocess (p))
{
error_t err;
pthread_mutex_unlock (&global_lock);
err = proc_task2proc (p->p_task_namespace, t, outproc);
pthread_mutex_lock (&global_lock);
if (! err)
{
*outproc_type = MACH_MSG_TYPE_MOVE_SEND;
mach_port_deallocate (mach_task_self (), t);
return 0;
}
}
*outproc = ports_get_right (p);
*outproc_type = MACH_MSG_TYPE_MAKE_SEND;
mach_port_deallocate (mach_task_self (), t);
return 0;
}
kern_return_t
S_proc_proc2task (struct proc *p,
task_t *t)
{
if (!p)
return EOPNOTSUPP;
*t = p->p_task;
return 0;
}
kern_return_t
S_proc_pid2proc (struct proc *callerp,
pid_t pid,
mach_port_t *outproc,
mach_msg_type_name_t *outproc_type)
{
struct proc *p;
if (!callerp)
return EOPNOTSUPP;
p = pid_find_allow_zombie (pid);
if (!p)
return ESRCH;
if (p->p_dead)
{
*outproc = MACH_PORT_NULL;
return 0;
}
if (! check_owner (callerp, p))
return EPERM;
if (namespace_is_subprocess (p))
{
error_t err;
pthread_mutex_unlock (&global_lock);
err = proc_task2proc (p->p_task_namespace, p->p_task, outproc);
pthread_mutex_lock (&global_lock);
if (! err)
{
*outproc_type = MACH_MSG_TYPE_MOVE_SEND;
return 0;
}
}
*outproc = ports_get_right (p);
*outproc_type = MACH_MSG_TYPE_MAKE_SEND;
return 0;
}
static error_t
get_string (task_t t,
vm_address_t addr,
char **str, size_t *len)
{
vm_address_t readaddr;
vm_address_t data;
mach_msg_type_number_t readlen;
error_t err;
char *c;
readaddr = trunc_page (addr);
err = vm_read (t, readaddr, vm_page_size * 2, &data, &readlen);
if (err == KERN_INVALID_ADDRESS)
err = vm_read (t, readaddr, vm_page_size, &data, &readlen);
if (err == MACH_SEND_INVALID_DEST)
err = ESRCH;
if (err)
return err;
c = memchr ((char *) (data + (addr - readaddr)), '\0',
readlen - (addr - readaddr));
if (c == NULL)
err = KERN_INVALID_ADDRESS;
else
{
c++;
*len = c - (char *) (data + (addr - readaddr));
*str = malloc (*len);
if (*str == NULL)
err = ENOMEM;
else
memcpy (*str, (char *) data + (addr - readaddr), *len);
}
munmap ((caddr_t) data, readlen);
return err;
}
static error_t
get_vector (task_t task,
vm_address_t addr,
vm_address_t **vec)
{
vm_address_t readaddr;
vm_size_t readsize;
vm_address_t scanned;
error_t err;
*vec = NULL;
readaddr = trunc_page (addr);
readsize = 0;
scanned = addr;
do
{
vm_address_t data;
mach_msg_type_number_t readlen = 0;
vm_address_t *t;
readsize += vm_page_size;
err = vm_read (task, readaddr, readsize, &data, &readlen);
if (err == MACH_SEND_INVALID_DEST)
err = ESRCH;
if (err)
return err;
for (t = (vm_address_t *) (data + (scanned - readaddr));
t < (vm_address_t *) (data + readlen);
++t)
if (*t == 0)
{
++t;
*vec = malloc ((char *)t - (char *)(data + (addr - readaddr)));
if (*vec == NULL)
err = ENOMEM;
else
memcpy (*vec, (char *)(data + (addr - readaddr)),
(char *)t - (char *)(data + (addr - readaddr)));
break;
}
scanned = readaddr + readlen;
munmap ((caddr_t) data, readlen);
} while (!err && *vec == NULL);
return err;
}
static error_t
get_string_array (task_t t,
vm_address_t loc,
vm_address_t *buf,
mach_msg_type_number_t *buflen)
{
char *bp;
vm_address_t *vector, *vp;
error_t err;
vm_address_t origbuf = *buf;
err = get_vector (t, loc, &vector);
if (err)
return err;
bp = (char *) *buf;
for (vp = vector; *vp; ++vp)
{
char *string;
size_t len;
err = get_string (t, *vp, &string, &len);
if (err)
{
free (vector);
if (*buf != origbuf)
munmap ((caddr_t) *buf, *buflen);
return err;
}
if (len > (char *) *buf + *buflen - bp)
{
char *newbuf;
vm_size_t prev_len = bp - (char *) *buf;
vm_size_t newsize = *buflen * 2;
if (newsize < prev_len + len)
newsize = round_page (prev_len + len);
newbuf = mmap (0, newsize, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (newbuf == MAP_FAILED)
{
err = errno;
free (string);
free (vector);
if (*buf != origbuf)
munmap ((caddr_t) *buf, *buflen);
return err;
}
memcpy (newbuf, (char *) *buf, prev_len);
bp = newbuf + prev_len;
if (*buf != origbuf)
munmap ((caddr_t) *buf, *buflen);
*buf = (vm_address_t) newbuf;
*buflen = newsize;
}
memcpy (bp, string, len);
bp += len;
free (string);
}
free (vector);
*buflen = bp - (char *) *buf;
return 0;
}
kern_return_t
S_proc_getprocargs (struct proc *callerp,
pid_t pid,
data_t *buf,
mach_msg_type_number_t *buflen)
{
struct proc *p = pid_find (pid);
if (!p)
return ESRCH;
if (namespace_is_subprocess (p))
{
error_t err;
pid_t pid_sub;
pthread_mutex_unlock (&global_lock);
err = proc_task2pid (p->p_task_namespace, p->p_task, &pid_sub);
if (! err)
err = proc_getprocargs (p->p_task_namespace, pid_sub, buf, buflen);
pthread_mutex_lock (&global_lock);
if (! err)
return 0;
}
return get_string_array (p->p_task, p->p_argv, (vm_address_t *) buf, buflen);
}
kern_return_t
S_proc_getprocenv (struct proc *callerp,
pid_t pid,
data_t *buf,
mach_msg_type_number_t *buflen)
{
struct proc *p = pid_find (pid);
if (!p)
return ESRCH;
if (namespace_is_subprocess (p))
{
error_t err;
pid_t pid_sub;
pthread_mutex_unlock (&global_lock);
err = proc_task2pid (p->p_task_namespace, p->p_task, &pid_sub);
if (! err)
err = proc_getprocenv (p->p_task_namespace, pid_sub, buf, buflen);
pthread_mutex_lock (&global_lock);
if (! err)
return 0;
}
return get_string_array (p->p_task, p->p_envp, (vm_address_t *)buf, buflen);
}
#define PI_FETCH_THREAD_DETAILS  \
(PI_FETCH_THREAD_SCHED | PI_FETCH_THREAD_BASIC | PI_FETCH_THREAD_WAITS)
kern_return_t
S_proc_getprocinfo (struct proc *callerp,
pid_t pid,
int *flags,
int **piarray,
mach_msg_type_number_t *piarraylen,
data_t *waits, mach_msg_type_number_t *waits_len)
{
struct proc *p = pid_find (pid);
struct procinfo *pi;
mach_msg_type_number_t nthreads;
thread_t *thds;
error_t err = 0;
size_t structsize;
int i;
int pi_alloced = 0, waits_alloced = 0;
mach_msg_type_number_t waits_used = 0;
mach_msg_type_number_t tkcount, thcount;
struct proc *tp;
task_t task;
mach_port_t msgport;
int owned;
if (!p)
return ESRCH;
if (namespace_is_subprocess (p))
{
error_t err;
pid_t pid_sub;
pthread_mutex_unlock (&global_lock);
err = proc_task2pid (p->p_task_namespace, p->p_task, &pid_sub);
if (! err)
err = proc_getprocinfo (p->p_task_namespace, pid_sub, flags,
piarray, piarraylen, waits, waits_len);
if (! err && *piarray && *piarraylen * sizeof (int) >= sizeof *pi)
{
task_t t_ppid = MACH_PORT_NULL;
task_t t_pgrp = MACH_PORT_NULL;
task_t t_session = MACH_PORT_NULL;
task_t t_logincollection = MACH_PORT_NULL;
pi = (struct procinfo *) *piarray;
if (pi->ppid != pid_sub)
proc_pid2task (p->p_task_namespace, pi->ppid, &t_ppid);
proc_pid2task (p->p_task_namespace, pi->pgrp, &t_pgrp);
proc_pid2task (p->p_task_namespace, pi->session, &t_session);
proc_pid2task (p->p_task_namespace, pi->logincollection,
&t_logincollection);
pthread_mutex_lock (&global_lock);
if (MACH_PORT_VALID (t_ppid))
{
struct proc *q = task_find (t_ppid);
pi->ppid = q ? q->p_pid : (pid_t) -1;
mach_port_deallocate (mach_task_self (), t_ppid);
}
else
{
pi->ppid = namespace_find_root (p)->p_pid;
}
if (MACH_PORT_VALID (t_pgrp))
{
struct proc *q = task_find (t_pgrp);
pi->pgrp = q ? q->p_pid : (pid_t) -1;
mach_port_deallocate (mach_task_self (), t_pgrp);
}
if (MACH_PORT_VALID (t_session))
{
struct proc *q = task_find (t_session);
pi->session = q ? q->p_pid : (pid_t) -1;
mach_port_deallocate (mach_task_self (), t_session);
}
if (MACH_PORT_VALID (t_logincollection))
{
struct proc *q = task_find (t_logincollection);
pi->logincollection = q ? q->p_pid : (pid_t) -1;
mach_port_deallocate (mach_task_self (), t_logincollection);
}
return 0;
}
pthread_mutex_lock (&global_lock);
err = 0;
}
task = p->p_task;
check_msgport_death (p);
msgport = p->p_msgport;
if (*flags & PI_FETCH_THREAD_DETAILS)
*flags |= PI_FETCH_THREADS;
if (*flags & PI_FETCH_THREADS)
{
err = task_threads (p->p_task, &thds, &nthreads);
if (err == MACH_SEND_INVALID_DEST)
err = ESRCH;
if (err)
return err;
}
else
nthreads = 0;
structsize = sizeof (struct procinfo);
if (*flags & PI_FETCH_THREAD_DETAILS)
structsize += nthreads * sizeof (pi->threadinfos[0]);
if (structsize / sizeof (int) > *piarraylen)
{
*piarray = mmap (0, structsize, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*piarray == MAP_FAILED)
{
err = errno;
if (*flags & PI_FETCH_THREADS)
{
for (i = 0; i < nthreads; i++)
mach_port_deallocate (mach_task_self (), thds[i]);
munmap (thds, nthreads * sizeof (thread_t));
}
return err;
}
pi_alloced = 1;
}
*piarraylen = structsize / sizeof (int);
pi = (struct procinfo *) *piarray;
owned = p->p_id && p->p_id->i_nuids;
pi->state =
((p->p_stopped ? PI_STOPPED : 0)
| (p->p_exec ? PI_EXECED : 0)
| (p->p_waiting ? PI_WAITING : 0)
| (!p->p_pgrp->pg_orphcnt ? PI_ORPHAN : 0)
| (p->p_msgport == MACH_PORT_NULL ? PI_NOMSG : 0)
| (p->p_pgrp->pg_session->s_sid == p->p_pid ? PI_SESSLD : 0)
| (owned ? 0 : PI_NOTOWNED)
| (!p->p_parentset ? PI_NOPARENT : 0)
| (p->p_traced ? PI_TRACED : 0)
| (p->p_msgportwait ? PI_GETMSG : 0)
| (p->p_loginleader ? PI_LOGINLD : 0));
pi->owner = owned ? p->p_id->i_uids[0] : 0;
pi->ppid = p->p_parent->p_pid;
pi->pgrp = p->p_pgrp->pg_pgid;
pi->session = p->p_pgrp->pg_session->s_sid;
for (tp = p; !tp->p_loginleader; tp = tp->p_parent)
assert_backtrace (tp);
pi->logincollection = tp->p_pid;
if (p->p_dead || p->p_stopped)
{
pi->exitstatus = p->p_status;
pi->sigcode = p->p_sigcode;
}
else
pi->exitstatus = pi->sigcode = 0;
pi->nthreads = nthreads;
pthread_mutex_unlock (&global_lock);
if (*flags & PI_FETCH_TASKINFO)
{
tkcount = TASK_BASIC_INFO_COUNT;
err = task_info (task, TASK_BASIC_INFO,
(task_info_t) &pi->taskinfo, &tkcount);
if (err == MACH_SEND_INVALID_DEST)
err = ESRCH;
#ifdef TASK_SCHED_TIMESHARE_INFO
if (!err)
{
tkcount = TASK_SCHED_TIMESHARE_INFO_COUNT;
err = task_info (task, TASK_SCHED_TIMESHARE_INFO,
(int *)&pi->timeshare_base_info, &tkcount);
if (err == KERN_INVALID_POLICY)
{
pi->timeshare_base_info.base_priority = -1;
err = 0;
}
}
#endif
}
if (*flags & PI_FETCH_TASKEVENTS)
{
tkcount = TASK_EVENTS_INFO_COUNT;
err = task_info (task, TASK_EVENTS_INFO,
(task_info_t) &pi->taskevents, &tkcount);
if (err == MACH_SEND_INVALID_DEST)
err = ESRCH;
if (err)
{
*flags &= ~PI_FETCH_TASKEVENTS;
err = 0;
}
}
for (i = 0; i < nthreads; i++)
{
if (*flags & PI_FETCH_THREAD_DETAILS)
pi->threadinfos[i].died = 0;
if (*flags & PI_FETCH_THREAD_BASIC)
{
thcount = THREAD_BASIC_INFO_COUNT;
err = thread_info (thds[i], THREAD_BASIC_INFO,
(thread_info_t) &pi->threadinfos[i].pis_bi,
&thcount);
if (err == MACH_SEND_INVALID_DEST)
{
pi->threadinfos[i].died = 1;
err = 0;
continue;
}
else if (err)
{
*flags &= ~PI_FETCH_THREAD_BASIC;
err = 0;
}
}
if (*flags & PI_FETCH_THREAD_SCHED)
{
thcount = THREAD_SCHED_INFO_COUNT;
err = thread_info (thds[i], THREAD_SCHED_INFO,
(thread_info_t) &pi->threadinfos[i].pis_si,
&thcount);
#ifdef HAVE_STRUCT_THREAD_SCHED_INFO_LAST_PROCESSOR
if (err == 0)
if (thcount < 8)
pi->threadinfos[i].pis_si.last_processor = 0;
#endif
if (err == MACH_SEND_INVALID_DEST)
{
pi->threadinfos[i].died = 1;
err = 0;
continue;
}
if (err)
{
*flags &= ~PI_FETCH_THREAD_SCHED;
err = 0;
}
}
if (*flags & PI_FETCH_THREAD_WAITS)
{
if (msgport == MACH_PORT_NULL)
*flags &= ~PI_FETCH_THREAD_WAITS;
else
{
string_t desc;
size_t desc_len;
if (msg_report_wait (msgport, thds[i],
desc, &pi->threadinfos[i].rpc_block))
desc[0] = '\0';
desc_len = strnlen (desc, sizeof desc);
if (waits_used + desc_len + 1 > *waits_len)
{
char *new_waits = 0;
mach_msg_type_number_t new_len =
round_page (waits_used + desc_len + 1);
new_waits = mmap (0, new_len, PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
err = (new_waits == MAP_FAILED) ? errno : 0;
if (err)
*flags &= ~PI_FETCH_THREAD_WAITS;
else
{
if (waits_used > 0)
memcpy (new_waits, *waits, waits_used);
if (*waits_len > 0 && waits_alloced)
munmap (*waits, *waits_len);
*waits = new_waits;
*waits_len = new_len;
waits_alloced = 1;
}
}
if (waits_used + desc_len + 1 <= *waits_len)
{
memcpy (*waits + waits_used, desc, desc_len);
waits_used += desc_len;
(*waits)[waits_used++] = '\0';
}
}
}
mach_port_deallocate (mach_task_self (), thds[i]);
}
if (*flags & PI_FETCH_THREADS)
munmap (thds, nthreads * sizeof (thread_t));
if (err && pi_alloced)
munmap (*piarray, structsize);
if (err && waits_alloced)
munmap (*waits, *waits_len);
else
*waits_len = waits_used;
pthread_mutex_lock (&global_lock);
return err;
}
kern_return_t
S_proc_make_login_coll (struct proc *p)
{
if (!p)
return EOPNOTSUPP;
p->p_loginleader = 1;
return 0;
}
kern_return_t
S_proc_getloginid (struct proc *callerp,
pid_t pid,
pid_t *leader)
{
struct proc *proc = pid_find (pid);
struct proc *p = proc;
if (!proc)
return ESRCH;
if (namespace_is_subprocess (p))
{
error_t err;
pid_t pid_sub;
pthread_mutex_unlock (&global_lock);
err = proc_task2pid (p->p_task_namespace, p->p_task, &pid_sub);
if (! err)
err = proc_getloginid (p->p_task_namespace, pid_sub, leader);
if (! err)
err = namespace_translate_pids (p->p_task_namespace, leader, 1);
else
pthread_mutex_lock (&global_lock);
if (! err)
return 0;
}
for (p = proc; !p->p_loginleader; p = p->p_parent)
assert_backtrace (p);
*leader = p->p_pid;
return 0;
}
kern_return_t
S_proc_getloginpids (struct proc *callerp,
pid_t id,
pid_t **pids,
mach_msg_type_number_t *npids)
{
error_t err = 0;
struct proc *l = pid_find (id);
struct proc *p;
struct proc **tail, **new, **parray;
int parraysize;
int i;
if (!l)
return ESRCH;
if (namespace_is_subprocess (l))
{
error_t err;
pid_t pid_sub;
pthread_mutex_unlock (&global_lock);
err = proc_task2pid (l->p_task_namespace, l->p_task, &pid_sub);
if (! err)
err = proc_getloginpids (l->p_task_namespace, pid_sub, pids, npids);
if (! err)
err = namespace_translate_pids (l->p_task_namespace, *pids, *npids);
else
pthread_mutex_lock (&global_lock);
if (! err)
return 0;
}
if (!l || !l->p_loginleader)
return ESRCH;
parraysize = 50;
parray = malloc (sizeof (struct proc *) * parraysize);
if (! parray)
return ENOMEM;
parray[0] = l;
for (tail = parray, new = &parray[1]; tail != new; tail++)
{
for (p = (*tail)->p_ochild; p; p = p->p_sib)
if (!p->p_loginleader)
{
if (new - parray > parraysize)
{
struct proc **newparray;
ptrdiff_t tail_offset = tail - parray;
ptrdiff_t new_offset = new - parray;
newparray = realloc (parray, ((parraysize *= 2)
* sizeof (struct proc *)));
if (! newparray)
{
free (parray);
return ENOMEM;
}
tail = newparray + tail_offset;
new = newparray + new_offset;
parray = newparray;
}
*new++ = p;
}
}
if (*npids < new - parray)
{
*pids = mmap (0, (new - parray) * sizeof (pid_t), PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
if (*pids == MAP_FAILED)
err = errno;
}
if (! err)
{
*npids = new - parray;
for (i = 0; i < *npids; i++)
(*pids)[i] = parray[i]->p_pid;
}
free (parray);
return err;
}
kern_return_t
S_proc_setlogin (struct proc *p,
const_string_t login)
{
struct login *l;
if (!p)
return EOPNOTSUPP;
if (!check_uid (p, 0))
return EPERM;
l = malloc (sizeof (struct login) + strlen (login) + 1);
if (! l)
return ENOMEM;
l->l_refcnt = 1;
strcpy (l->l_name, login);
if (!--p->p_login->l_refcnt)
free (p->p_login);
p->p_login = l;
return 0;
}
kern_return_t
S_proc_getlogin (struct proc *p,
string_t login)
{
if (!p)
return EOPNOTSUPP;
strcpy (login, p->p_login->l_name);
return 0;
}
kern_return_t
S_proc_get_tty (struct proc *p, pid_t pid,
mach_port_t *tty, mach_msg_type_name_t *tty_type)
{
return EOPNOTSUPP;
}
kern_return_t
S_proc_getnports (struct proc *callerp,
pid_t pid,
mach_msg_type_number_t *nports)
{
struct proc *p = pid_find (pid);
mach_port_array_t names;
mach_msg_type_number_t ncount;
mach_port_type_array_t types;
mach_msg_type_number_t tcount;
error_t err = 0;
if (!p)
return ESRCH;
err = mach_port_names (p->p_task, &names, &ncount, &types, &tcount);
if (err == KERN_INVALID_TASK)
err = ESRCH;
if (!err) {
*nports = ncount;
munmap (names, ncount * sizeof (mach_port_t));
munmap (types, tcount * sizeof (mach_port_type_t));
}
return err;
}
kern_return_t
S_proc_set_exe (struct proc *p,
const_string_t path)
{
char *copy;
if (!p)
return EOPNOTSUPP;
copy = strdup(path);
if (! copy)
return ENOMEM;
free(p->exe);
p->exe = copy;
return 0;
}
kern_return_t
S_proc_get_exe (struct proc *callerp,
pid_t pid,
string_t path)
{
struct proc *p = pid_find (pid);
if (!p)
return ESRCH;
if (p->exe)
snprintf (path, 1024 , "%s", p->exe);
else
path[0] = 0;
return 0;
}
kern_return_t
S_proc_getchildren_rusage (struct proc *p, struct rusage *ru)
{
if (!p)
return EOPNOTSUPP;
*ru = p->p_child_rusage;
return 0;
}