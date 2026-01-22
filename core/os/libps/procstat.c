#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert-backtrace.h>
#include <string.h>
#include "ps.h"
#include "common.h"
#include "ps_msg.h"
char *proc_stat_state_tags = "TZRHDSIN<u+slfmpoxwg";
typedef typeof (((struct procinfo *)0)->threadinfos[0]) threadinfo_data_t;
typedef threadinfo_data_t *threadinfo_t;
static int
thread_state (thread_basic_info_t bi)
{
int state = 0;
switch (bi->run_state)
{
case TH_STATE_RUNNING:
state |= PSTAT_STATE_T_RUN;
break;
case TH_STATE_UNINTERRUPTIBLE:
state |= PSTAT_STATE_T_WAIT;
break;
case TH_STATE_HALTED:
state |= PSTAT_STATE_T_HALT;
break;
case TH_STATE_STOPPED:
state |= PSTAT_STATE_T_HALT | PSTAT_STATE_T_UNCLEAN;
break;
case TH_STATE_WAITING:
state |= bi->sleep_time < 20 ? PSTAT_STATE_T_SLEEP : PSTAT_STATE_T_IDLE;
break;
}
if (bi->base_priority < 25)
state |= PSTAT_STATE_T_NASTY;
else if (bi->base_priority > 25)
state |= PSTAT_STATE_T_NICE;
return state;
}
#define PSTAT_PROCINFO_THREAD \
(PSTAT_THREAD_BASIC | PSTAT_THREAD_SCHED | PSTAT_THREAD_WAIT)
#define PSTAT_PROCINFO_TASK_THREAD_DEP \
(PSTAT_PROCINFO_THREAD | PSTAT_NUM_THREADS | PSTAT_THREAD_WAITS)
#define PSTAT_PROCINFO_TASK \
(PSTAT_PROCINFO_TASK_THREAD_DEP | PSTAT_PROC_INFO \
| PSTAT_TASK_BASIC | PSTAT_TASK_EVENTS)
#define PSTAT_PROCINFO PSTAT_PROCINFO_TASK
#define PSTAT_PROCINFO_MERGE (PSTAT_TASK_BASIC | PSTAT_TASK_EVENTS)
#define PSTAT_PROCINFO_REFETCH (PSTAT_PROCINFO - PSTAT_PROCINFO_MERGE)
static error_t
fetch_procinfo (process_t server, pid_t pid,
ps_flags_t need, ps_flags_t *have,
struct procinfo **pi,
mach_msg_type_number_t *pi_size,
char **waits,
mach_msg_type_number_t *waits_len)
{
static const struct { ps_flags_t ps_flag; int pi_flags; } map[] =
{
{ PSTAT_TASK_BASIC, PI_FETCH_TASKINFO },
{ PSTAT_TASK_EVENTS, PI_FETCH_TASKEVENTS },
{ PSTAT_NUM_THREADS, PI_FETCH_THREADS },
{ PSTAT_THREAD_BASIC, PI_FETCH_THREAD_BASIC | PI_FETCH_THREADS },
{ PSTAT_THREAD_SCHED, PI_FETCH_THREAD_SCHED | PI_FETCH_THREADS },
{ PSTAT_THREAD_WAITS, PI_FETCH_THREAD_WAITS | PI_FETCH_THREADS },
{ 0, }
};
int pi_flags = 0;
int i;
for (i = 0; map[i].ps_flag; i++)
if ((need & map[i].ps_flag) && !(*have & map[i].ps_flag))
pi_flags |= map[i].pi_flags;
if (pi_flags || ((need & PSTAT_PROC_INFO) && !(*have & PSTAT_PROC_INFO)))
{
error_t err;
*pi_size /= sizeof (int);
err = proc_getprocinfo (server, pid, &pi_flags,
(procinfo_t *)pi, pi_size, waits, waits_len);
*pi_size *= sizeof (int);
if (! err)
{
*have |= PSTAT_PROC_INFO;
for (i = 0; map[i].ps_flag; i++)
if ((pi_flags & map[i].pi_flags) == map[i].pi_flags)
*have |= map[i].ps_flag;
}
return err;
}
else
return 0;
}
#define PROCINFO_MALLOC_SIZE \
(sizeof (struct procinfo) + 4 * sizeof (threadinfo_data_t))
#define WAITS_MALLOC_SIZE 128
static ps_flags_t
merge_procinfo (struct proc_stat *ps, ps_flags_t need, ps_flags_t have)
{
error_t err;
struct procinfo *new_pi, old_pi_hdr;
mach_msg_type_number_t new_pi_size;
char *new_waits = 0;
mach_msg_type_number_t new_waits_len = 0;
ps_flags_t really_need = need | (have & PSTAT_PROCINFO_REFETCH);
ps_flags_t really_have = have & ~PSTAT_PROCINFO_REFETCH;
if (have & PSTAT_PROCINFO)
old_pi_hdr = *ps->proc_info;
else
{
ps->proc_info = malloc (PROCINFO_MALLOC_SIZE);
ps->proc_info_size = PROCINFO_MALLOC_SIZE;
ps->proc_info_vm_alloced = 0;
if (! ps->proc_info)
return ENOMEM;
}
new_pi = ps->proc_info;
new_pi_size = ps->proc_info_size;
if (really_need & PSTAT_THREAD_WAITS)
{
if (! (have & PSTAT_THREAD_WAITS))
{
ps->thread_waits = malloc (WAITS_MALLOC_SIZE);
ps->thread_waits_len = WAITS_MALLOC_SIZE;
ps->thread_waits_vm_alloced = 0;
}
new_waits = ps->thread_waits;
new_waits_len = ps->thread_waits_len;
}
err = fetch_procinfo (ps->context->server, ps->pid, really_need, &really_have,
&new_pi, &new_pi_size,
&new_waits, &new_waits_len);
if (err)
{
if (! (have & PSTAT_PROCINFO))
free (new_pi);
if ((really_need & PSTAT_THREAD_WAITS) && !(have & PSTAT_THREAD_WAITS))
free (new_waits);
return have;
}
if (have & PSTAT_TASK_BASIC)
memcpy (&new_pi->taskinfo, &old_pi_hdr.taskinfo,
sizeof (struct task_basic_info));
if (have & PSTAT_TASK_EVENTS)
memcpy (&new_pi->taskevents, &old_pi_hdr.taskevents,
sizeof (struct task_events_info));
if (new_pi != ps->proc_info)
{
if (ps->proc_info_vm_alloced)
munmap (ps->proc_info, ps->proc_info_size);
else
free (ps->proc_info);
ps->proc_info = new_pi;
ps->proc_info_size = new_pi_size;
ps->proc_info_vm_alloced = 1;
}
if (really_need & PSTAT_THREAD_WAITS)
{
if (! (really_have & PSTAT_THREAD_WAITS))
new_waits = 0;
if (new_waits != ps->thread_waits)
{
if (ps->thread_waits_vm_alloced)
munmap (ps->thread_waits, ps->thread_waits_len);
else
free (ps->thread_waits);
ps->thread_waits = new_waits;
ps->thread_waits_len = new_waits_len;
ps->thread_waits_vm_alloced = 1;
}
}
return really_have;
}
static ps_flags_t
add_preconditions (ps_flags_t flags, struct ps_context *context)
{
if ((flags & PSTAT_USER_MASK)
&& context->user_hooks && context->user_hooks->dependencies)
flags |= (*context->user_hooks->dependencies) (flags & PSTAT_USER_MASK);
if (flags & PSTAT_TTY)
flags |= PSTAT_CTTYID;
if (flags & PSTAT_STATE)
flags |= PSTAT_PROC_INFO | PSTAT_THREAD_BASIC;
if (flags & PSTAT_OWNER)
flags |= PSTAT_OWNER_UID;
if (flags & PSTAT_OWNER_UID)
flags |= PSTAT_PROC_INFO;
if (flags & PSTAT_SUSPEND_COUNT)
flags |= PSTAT_TASK_BASIC | PSTAT_THREAD_BASIC;
if (flags & PSTAT_TIMES)
flags |= PSTAT_TASK_BASIC | PSTAT_THREAD_BASIC;
if (flags & (PSTAT_CTTYID | PSTAT_CWDIR | PSTAT_AUTH | PSTAT_UMASK)
&& !(flags & PSTAT_NO_MSGPORT))
{
flags |= PSTAT_MSGPORT;
flags |= PSTAT_TASK;
}
return flags;
}
#define PSTAT_TEST_MSGPORT \
(PSTAT_NUM_THREADS | PSTAT_SUSPEND_COUNT | PSTAT_THREAD_BASIC)
#define PSTAT_USES_MSGPORT \
(PSTAT_MSGPORT | PSTAT_THREAD_WAIT | PSTAT_THREAD_WAITS)
static int
should_suppress_msgport (struct proc_stat *ps)
{
ps_flags_t have = ps->flags;
if ((have & PSTAT_SUSPEND_COUNT) && ps->suspend_count != 0)
return TRUE;
if ((have & PSTAT_THREAD_BASIC) && ps->thread_basic_info->suspend_count != 0)
return TRUE;
if ((have & PSTAT_NUM_THREADS) && ps->num_threads == 0)
return TRUE;
return FALSE;
}
#define SUPPRESS_MSGPORT_FLAGS(flags) \
(((flags) & ~PSTAT_USES_MSGPORT) | PSTAT_NO_MSGPORT)
static struct thread_basic_info *
summarize_thread_basic_info (struct procinfo *pi, ps_flags_t have)
{
int i;
unsigned num_threads = 0, num_run_threads = 0;
thread_basic_info_t tbi = malloc (sizeof (struct thread_basic_info));
int run_base_priority = 0, run_cur_priority = 0;
int total_base_priority = 0, total_cur_priority = 0;
if (!tbi)
return 0;
memset (tbi, 0, sizeof *tbi);
for (i = 0; i < pi->nthreads; i++)
if (! pi->threadinfos[i].died
&& ! (pi->threadinfos[i].pis_bi.flags & TH_FLAGS_IDLE))
{
thread_basic_info_t bi = &pi->threadinfos[i].pis_bi;
int thread_run_state = bi->run_state;
if (tbi->run_state == 0)
tbi->run_state = thread_run_state;
else if (tbi->run_state == TH_STATE_RUNNING
|| thread_run_state == TH_STATE_RUNNING)
tbi->run_state = TH_STATE_RUNNING;
else if (tbi->run_state != bi->run_state)
tbi->run_state = -1;
tbi->cpu_usage += bi->cpu_usage;
tbi->sleep_time += bi->sleep_time;
if (i == 0 || tbi->suspend_count > bi->suspend_count)
tbi->suspend_count = bi->suspend_count;
tbi->user_time.seconds += bi->user_time.seconds;
tbi->user_time.microseconds += bi->user_time.microseconds;
tbi->system_time.seconds += bi->system_time.seconds;
tbi->system_time.microseconds += bi->system_time.microseconds;
if (tbi->run_state == TH_STATE_RUNNING)
{
run_base_priority += bi->base_priority;
run_cur_priority += bi->base_priority;
num_run_threads++;
}
else
{
total_base_priority += bi->base_priority;
total_cur_priority += bi->base_priority;
}
num_threads++;
}
if (num_threads > 0)
{
tbi->sleep_time /= num_threads;
if (num_run_threads > 0)
{
tbi->base_priority = run_base_priority / num_run_threads;
tbi->cur_priority = run_cur_priority / num_run_threads;
}
else
{
tbi->base_priority = total_base_priority / num_threads;
tbi->cur_priority = total_cur_priority / num_threads;
}
}
if (have & PSTAT_TASK_BASIC)
{
tbi->user_time.seconds += pi->taskinfo.user_time.seconds;
tbi->user_time.microseconds += pi->taskinfo.user_time.microseconds;
tbi->system_time.seconds += pi->taskinfo.system_time.seconds;
tbi->system_time.microseconds += pi->taskinfo.system_time.microseconds;
}
tbi->user_time.seconds += tbi->user_time.microseconds / 1000000;
tbi->user_time.microseconds %= 1000000;
tbi->system_time.seconds += tbi->system_time.microseconds / 1000000;
tbi->system_time.microseconds %= 1000000;
return tbi;
}
static struct thread_sched_info *
summarize_thread_sched_info (struct procinfo *pi)
{
int i;
unsigned num_threads = 0;
thread_sched_info_t tsi = malloc (sizeof (struct thread_sched_info));
if (!tsi)
return 0;
memset (tsi, 0, sizeof *tsi);
for (i = 0; i < pi->nthreads; i++)
if (! pi->threadinfos[i].died
&& ! (pi->threadinfos[i].pis_bi.flags & TH_FLAGS_IDLE))
{
thread_sched_info_t si = &pi->threadinfos[i].pis_si;
tsi->base_priority += si->base_priority;
tsi->cur_priority += si->cur_priority;
tsi->max_priority += si->max_priority;
tsi->depress_priority += si->depress_priority;
num_threads++;
}
if (num_threads > 0)
{
tsi->base_priority /= num_threads;
tsi->cur_priority /= num_threads;
tsi->max_priority /= num_threads;
tsi->depress_priority /= num_threads;
}
return tsi;
}
static int
summarize_thread_states (struct procinfo *pi)
{
int i;
int state = 0;
for (i = 0; i < pi->nthreads; i++)
if (! pi->threadinfos[i].died
&& ! (pi->threadinfos[i].pis_bi.flags & TH_FLAGS_IDLE))
state |= thread_state (&pi->threadinfos[i].pis_bi);
return state;
}
static void
summarize_thread_waits (struct procinfo *pi, char *waits, size_t waits_len,
char **wait, mach_msg_id_t *rpc)
{
int i;
char *next_wait = waits;
*wait = 0;
*rpc = 0;
for (i = 0; i < pi->nthreads; i++)
if (! pi->threadinfos[i].died)
{
if (next_wait > waits + waits_len)
break;
else
{
int left = waits + waits_len - next_wait;
if (pi->threadinfos[i].pis_bi.flags & TH_FLAGS_IDLE)
;
else if (strncmp (next_wait, "msgport", left) == 0
|| strncmp (next_wait, "itimer", left) == 0)
;
else if (*wait)
{
*wait = "*";
*rpc = 0;
break;
}
else
{
*wait = next_wait;
*rpc = pi->threadinfos[i].rpc_block;
}
next_wait += strnlen (next_wait, left) + 1;
}
}
}
static unsigned
count_threads (struct procinfo *pi, ps_flags_t have)
{
if (have & (PSTAT_PROCINFO_TASK_THREAD_DEP & ~PSTAT_NUM_THREADS))
{
int i;
unsigned num_threads = 0;
for (i = 0; i < pi->nthreads; i++)
if (! pi->threadinfos[i].died)
num_threads++;
return num_threads;
}
else
return pi->nthreads;
}
threadinfo_t
get_thread_info (struct procinfo *pi, unsigned index)
{
int i;
for (i = 0; i < pi->nthreads; i++)
if (! pi->threadinfos[i].died && index-- == 0)
return &pi->threadinfos[i];
return 0;
}
char *
get_thread_wait (char *waits, size_t waits_len, unsigned n)
{
char *wait = waits;
while (n-- && wait)
if (wait >= waits + waits_len)
wait = 0;
else
wait += strnlen (wait, waits + waits_len - wait) + 1;
return wait;
}
static void *
clone (void *src, size_t size)
{
void *dst = malloc (size);
if (dst)
memcpy (dst, src, size);
return dst;
}
static ps_flags_t
set_procinfo_flags (struct proc_stat *ps, ps_flags_t need, ps_flags_t have)
{
if (have & PSTAT_PID)
{
struct procinfo *pi;
ps_flags_t had = have;
if (! (have & PSTAT_PROCINFO))
{
ps->proc_info = 0;
ps->proc_info_size = 0;
ps->thread_waits = 0;
ps->thread_waits_len = 0;
}
if ((need & PSTAT_THREAD_WAIT) && !(need & PSTAT_THREAD_WAITS))
{
if (! (have & PSTAT_NUM_THREADS))
{
have = merge_procinfo (ps, PSTAT_NUM_THREADS, have);
if (have & PSTAT_NUM_THREADS)
ps->num_threads = count_threads (ps->proc_info, have);
}
if ((have & PSTAT_NUM_THREADS) && ps->num_threads <= 3)
need |= PSTAT_THREAD_WAITS;
}
have = merge_procinfo (ps, need, have);
pi = ps->proc_info;
if (have & PSTAT_TASK_BASIC)
ps->task_basic_info = &pi->taskinfo;
if (have & PSTAT_TASK_EVENTS)
ps->task_events_info = &pi->taskevents;
if (have & PSTAT_NUM_THREADS)
ps->num_threads = count_threads (pi, have);
if (had & PSTAT_THREAD_BASIC)
free (ps->thread_basic_info);
if (have & PSTAT_THREAD_BASIC)
ps->thread_basic_info = summarize_thread_basic_info (pi, have);
if (had & PSTAT_THREAD_SCHED)
free (ps->thread_sched_info);
if (have & PSTAT_THREAD_SCHED)
ps->thread_sched_info = summarize_thread_sched_info (pi);
if (have & PSTAT_THREAD_WAITS)
{
summarize_thread_waits (pi,
ps->thread_waits, ps->thread_waits_len,
&ps->thread_wait, &ps->thread_rpc);
have |= PSTAT_THREAD_WAIT;
}
else if (!(have & PSTAT_NO_MSGPORT)
&& (have & PSTAT_NUM_THREADS) && ps->num_threads > 3)
{
ps->thread_wait = "*";
ps->thread_rpc = 0;
have |= PSTAT_THREAD_WAIT;
}
}
else
{
struct proc_stat *origin = ps->thread_origin;
ps_flags_t oflags =
(need & PSTAT_PROCINFO_THREAD)
| ((need & PSTAT_THREAD_WAIT) ? PSTAT_THREAD_WAITS : 0);
proc_stat_set_flags (origin, oflags);
oflags = origin->flags;
if (oflags & PSTAT_PROCINFO_THREAD)
{
threadinfo_t ti =
get_thread_info (origin->proc_info, ps->thread_index);
need &= ~have;
if ((need & PSTAT_THREAD_BASIC) && (oflags & PSTAT_THREAD_BASIC)
&& (ps->thread_basic_info =
clone (&ti->pis_bi, sizeof (struct thread_basic_info))))
have |= PSTAT_THREAD_BASIC;
if ((need & PSTAT_THREAD_SCHED) && (oflags & PSTAT_THREAD_SCHED)
&& (ps->thread_sched_info =
clone (&ti->pis_si, sizeof (struct thread_sched_info))))
have |= PSTAT_THREAD_SCHED;
if ((need & PSTAT_THREAD_WAIT) && (oflags & PSTAT_THREAD_WAITS))
{
ps->thread_wait =
get_thread_wait (origin->thread_waits,
origin->thread_waits_len,
ps->thread_index);
if (ps->thread_wait)
{
ps->thread_rpc = ti->rpc_block;
have |= PSTAT_THREAD_WAIT;
}
}
}
ps->inapp |= need & ~have & PSTAT_PROCINFO & ~PSTAT_PROCINFO_THREAD;
}
return have;
}
error_t
proc_stat_set_flags (struct proc_stat *ps, ps_flags_t flags)
{
ps_flags_t have = ps->flags;
ps_flags_t need;
ps_flags_t no_msgport_flags;
ps_flags_t test_msgport_flags;
process_t server = ps_context_server (ps->context);
void suppress_msgport (void)
{
need &= ~(flags & ~no_msgport_flags);
have = SUPPRESS_MSGPORT_FLAGS (have);
}
flags &= ~ps->failed;
if (flags & PSTAT_NO_MSGPORT)
have = SUPPRESS_MSGPORT_FLAGS (have);
if (have & PSTAT_NO_MSGPORT)
flags = SUPPRESS_MSGPORT_FLAGS (flags);
no_msgport_flags =
add_preconditions (SUPPRESS_MSGPORT_FLAGS (flags), ps->context);
flags = add_preconditions (flags, ps->context);
if (flags & PSTAT_USES_MSGPORT)
{
test_msgport_flags = add_preconditions (PSTAT_TEST_MSGPORT, ps->context);
flags |= test_msgport_flags;
}
else
test_msgport_flags = 0;
need = flags & ~have & ~ps->failed;
#define NEED(flag, precond) \
({ \
ps_flags_t __flag = (flag), _precond = (precond); \
int val; \
if (! (__flag & need)) \
val = 0; \
else if ((_precond & have) == _precond) \
val = 1; \
else \
{ \
val = 0; \
if (_precond & ps->inapp) \
ps->inapp |= __flag; \
} \
val; \
})
#define MGET(flag, precond, call) \
({ \
error_t err; \
ps_flags_t _flag = (flag); \
if (NEED (_flag, precond)) \
{ \
err = (call); \
if (!err) \
have |= _flag; \
} \
else \
err = 0; \
err; \
})
#define MP_MGET(flag, precond, call) \
({ error_t err = MGET (flag, (precond) | PSTAT_MSGPORT, call); \
if (err == EMACH_RCV_TIMED_OUT) suppress_msgport (); \
err; \
})
if (need & ~have & test_msgport_flags & PSTAT_PROCINFO)
have = set_procinfo_flags (ps, need & ~have & test_msgport_flags, have);
if (NEED (PSTAT_SUSPEND_COUNT,
((have & PSTAT_PID) ? PSTAT_TASK_BASIC : PSTAT_THREAD_BASIC)))
{
if (have & PSTAT_PID)
ps->suspend_count = ps->task_basic_info->suspend_count;
else
ps->suspend_count = ps->thread_basic_info->suspend_count;
have |= PSTAT_SUSPEND_COUNT;
}
ps->flags = have;
if (should_suppress_msgport (ps))
suppress_msgport ();
if (need & ~have & PSTAT_PROCINFO)
have = set_procinfo_flags (ps, need, have);
MGET(PSTAT_MSGPORT, PSTAT_PID, proc_getmsgport (server, ps->pid, &ps->msgport));
MGET(PSTAT_PROCESS, PSTAT_PID, proc_pid2proc (server, ps->pid, &ps->process));
MGET(PSTAT_TASK, PSTAT_PID, proc_pid2task (server, ps->pid, &ps->task));
if ((need & PSTAT_STATE) && (have & (PSTAT_PROC_INFO | PSTAT_THREAD_BASIC)))
{
ps->state = 0;
if (have & PSTAT_THREAD_BASIC)
{
if (have & PSTAT_THREAD)
ps->state |= thread_state (ps->thread_basic_info);
else
ps->state |= summarize_thread_states (ps->proc_info);
}
if (have & PSTAT_PROC_INFO)
{
int pi_flags = ps->proc_info->state;
if (pi_flags & PI_STOPPED)
ps->state |= PSTAT_STATE_P_STOP;
if (pi_flags & PI_ZOMBIE)
ps->state |= PSTAT_STATE_P_ZOMBIE;
if (pi_flags & PI_SESSLD)
ps->state |= PSTAT_STATE_P_SESSLDR;
if (pi_flags & PI_LOGINLD)
ps->state |= PSTAT_STATE_P_LOGINLDR;
if (!(pi_flags & PI_EXECED))
ps->state |= PSTAT_STATE_P_FORKED;
if (pi_flags & PI_NOMSG)
ps->state |= PSTAT_STATE_P_NOMSG;
if (pi_flags & PI_NOPARENT)
ps->state |= PSTAT_STATE_P_NOPARENT;
if (pi_flags & PI_ORPHAN)
ps->state |= PSTAT_STATE_P_ORPHAN;
if (pi_flags & PI_TRACED)
ps->state |= PSTAT_STATE_P_TRACE;
if (pi_flags & PI_WAITING)
ps->state |= PSTAT_STATE_P_WAIT;
if (pi_flags & PI_GETMSG)
ps->state |= PSTAT_STATE_P_GETMSG;
}
have |= PSTAT_STATE;
}
if (NEED (PSTAT_ARGS, PSTAT_PID))
{
mach_msg_type_number_t args_len = 100;
char *buf = malloc (args_len);
ps->args = buf;
if (ps->args)
{
if (proc_getprocargs (server, ps->pid, &ps->args, &args_len))
free (buf);
else
{
have |= PSTAT_ARGS;
ps->args_len = (size_t) args_len;
ps->args_vm_alloced = (ps->args != buf);
if (ps->args_vm_alloced)
free (buf);
}
}
}
if (NEED (PSTAT_ENV, PSTAT_PID))
{
mach_msg_type_number_t env_len = 100;
char *buf = malloc (env_len);
ps->env = buf;
if (ps->env)
{
if (proc_getprocenv (server, ps->pid, &ps->env, &env_len))
free (buf);
else
{
have |= PSTAT_ENV;
ps->env_len = (size_t) env_len;
ps->env_vm_alloced = (ps->env != buf);
if (ps->env_vm_alloced)
free (buf);
}
}
}
if (NEED (PSTAT_EXE, PSTAT_PID))
{
ps->exe = malloc (sizeof(string_t));
if (ps->exe)
{
if (proc_get_exe (server, ps->pid, ps->exe))
free (ps->exe);
else
{
ps->exe_len = strlen(ps->exe);
have |= PSTAT_EXE;
ps->exe_vm_alloced = 0;
}
}
}
MP_MGET (PSTAT_CTTYID, PSTAT_TASK,
ps_msg_get_init_port (ps->msgport, ps->task,
INIT_PORT_CTTYID, &ps->cttyid));
MP_MGET (PSTAT_CWDIR, PSTAT_TASK,
ps_msg_get_init_port (ps->msgport, ps->task,
INIT_PORT_CWDIR, &ps->cwdir));
MP_MGET (PSTAT_AUTH, PSTAT_TASK,
ps_msg_get_init_port (ps->msgport, ps->task, INIT_PORT_AUTH,
&ps->auth));
MP_MGET (PSTAT_UMASK, PSTAT_TASK,
ps_msg_get_init_int (ps->msgport, ps->task, INIT_UMASK,
(int *) &ps->umask));
if (NEED (PSTAT_OWNER_UID, PSTAT_PROC_INFO))
{
if (ps->proc_info->state & PI_NOTOWNED)
ps->owner_uid = -1;
else
ps->owner_uid = ps->proc_info->owner;
have |= PSTAT_OWNER_UID;
}
if (NEED (PSTAT_OWNER, PSTAT_OWNER_UID))
{
if (ps->owner_uid < 0)
{
ps->owner = 0;
have |= PSTAT_OWNER;
}
else if (! ps_context_find_user (ps->context, ps->owner_uid, &ps->owner))
have |= PSTAT_OWNER;
}
if (NEED (PSTAT_TTY, PSTAT_CTTYID))
if (ps_context_find_tty_by_cttyid (ps->context, ps->cttyid, &ps->tty) == 0)
have |= PSTAT_TTY;
MGET (PSTAT_NUM_PORTS, PSTAT_PID,
proc_getnports (server, ps->pid, &ps->num_ports));
if ((need & PSTAT_TIMES) && (have & (PSTAT_TASK_BASIC | PSTAT_THREAD_BASIC)))
have |= PSTAT_TIMES;
ps->failed |= (need & ~PSTAT_USER_MASK) & ~have;
ps->flags = have;
need &= ~have;
if (need && ps->context->user_hooks && ps->context->user_hooks->fetch)
{
have |= (*ps->context->user_hooks->fetch) (ps, need, have);
ps->failed = (ps->failed | need) & ~have;
ps->flags = have;
}
return 0;
}
void
_proc_stat_free (struct proc_stat *ps)
{
if (ps->context->user_hooks && ps->context->user_hooks->cleanup)
(*ps->context->user_hooks->cleanup) (ps);
#define MFREEPORT(flag, port) \
((ps->flags & (flag)) \
? mach_port_deallocate(mach_task_self (), (ps->port)) : 0)
#define MFREEMEM(flag, mem, size, vm_alloced, sbuf, eltype) \
(((ps->flags & (flag)) && ps->mem != sbuf) \
? (vm_alloced ? (VMFREE(ps->mem, size * sizeof (eltype))) : free (ps->mem)) : 0)
MFREEPORT (PSTAT_PROCESS, process);
MFREEPORT (PSTAT_TASK, task);
MFREEPORT (PSTAT_MSGPORT, msgport);
MFREEPORT (PSTAT_CTTYID, cttyid);
MFREEPORT (PSTAT_CWDIR, cwdir);
MFREEPORT (PSTAT_AUTH, auth);
MFREEMEM (PSTAT_PROC_INFO, proc_info, ps->proc_info_size,
ps->proc_info_vm_alloced, 0, char);
MFREEMEM (PSTAT_THREAD_BASIC, thread_basic_info, 0, 0, 0, 0);
MFREEMEM (PSTAT_THREAD_SCHED, thread_sched_info, 0, 0, 0, 0);
MFREEMEM (PSTAT_ARGS, args, ps->args_len, ps->args_vm_alloced, 0, char);
MFREEMEM (PSTAT_ENV, env, ps->env_len, ps->env_vm_alloced, 0, char);
MFREEMEM (PSTAT_TASK_EVENTS, task_events_info, ps->task_events_info_size,
0, &ps->task_events_info_buf, char);
MFREEMEM (PSTAT_THREAD_WAITS, thread_waits, ps->thread_waits_len,
ps->thread_waits_vm_alloced, 0, char);
MFREEMEM (PSTAT_EXE, exe, sizeof(string_t), ps->exe_vm_alloced, 0, char);
FREE (ps);
}
error_t
_proc_stat_create (pid_t pid, struct ps_context *context, struct proc_stat **ps)
{
*ps = NEW (struct proc_stat);
if (*ps == NULL)
return ENOMEM;
(*ps)->pid = pid;
(*ps)->flags = PSTAT_PID;
(*ps)->failed = 0;
(*ps)->inapp = PSTAT_THREAD;
(*ps)->context = context;
(*ps)->hook = 0;
return 0;
}
error_t
proc_stat_thread_create (struct proc_stat *ps, unsigned index, struct proc_stat **thread_ps)
{
error_t err = proc_stat_set_flags (ps, PSTAT_NUM_THREADS);
if (err)
return err;
else if (index >= ps->num_threads)
return EINVAL;
else
{
struct proc_stat *tps = NEW (struct proc_stat);
if (tps == NULL)
return ENOMEM;
tps->pid = -1;
tps->flags = PSTAT_THREAD;
tps->failed = 0;
tps->inapp = PSTAT_PID;
tps->thread_origin = ps;
tps->thread_index = index;
tps->context = ps->context;
*thread_ps = tps;
return 0;
}
}