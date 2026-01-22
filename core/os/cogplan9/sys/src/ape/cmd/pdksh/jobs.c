#include "sh.h"
#include "ksh_stat.h"
#include "ksh_wait.h"
#include "ksh_times.h"
#include "tty.h"
#ifndef CHILD_MAX
# if defined(HAVE_SYSCONF) && defined(_SC_CHILD_MAX)
# define CHILD_MAX sysconf(_SC_CHILD_MAX)
# else
# ifdef _POSIX_CHILD_MAX
# define CHILD_MAX ((_POSIX_CHILD_MAX) * 2)
# else
# define CHILD_MAX 20
# endif
# endif
#endif
#ifdef JOBS
# if defined(HAVE_TCSETPGRP) || defined(TIOCSPGRP)
# define TTY_PGRP
# endif
# ifdef BSD_PGRP
# define setpgid setpgrp
# define getpgID() getpgrp(0)
# else
# define getpgID() getpgrp()
# endif
# if defined(TTY_PGRP) && !defined(HAVE_TCSETPGRP)
int tcsetpgrp ARGS((int fd, pid_t grp));
int tcgetpgrp ARGS((int fd));
int
tcsetpgrp(fd, grp)
int fd;
pid_t grp;
{
return ioctl(fd, TIOCSPGRP, &grp);
}
int
tcgetpgrp(fd)
int fd;
{
int r, grp;
if ((r = ioctl(fd, TIOCGPGRP, &grp)) < 0)
return r;
return grp;
}
# endif
#else
# undef TTY_PGRP
# undef NEED_PGRP_SYNC
#endif
#define PRUNNING 0
#define PEXITED 1
#define PSIGNALLED 2
#define PSTOPPED 3
typedef struct proc Proc;
struct proc {
Proc *next;
int state;
WAIT_T status;
pid_t pid;
char command[48];
};
#define JP_NONE 0
#define JP_SHORT 1
#define JP_MEDIUM 2
#define JP_LONG 3
#define JP_PGRP 4
#define PJ_ON_FRONT 0
#define PJ_PAST_STOPPED 1
#define JF_STARTED 0x001
#define JF_WAITING 0x002
#define JF_W_ASYNCNOTIFY 0x004
#define JF_XXCOM 0x008
#define JF_FG 0x010
#define JF_SAVEDTTY 0x020
#define JF_CHANGED 0x040
#define JF_KNOWN 0x080
#define JF_ZOMBIE 0x100
#define JF_REMOVE 0x200
#define JF_USETTYMODE 0x400
#define JF_SAVEDTTYPGRP 0x800
typedef struct job Job;
struct job {
Job *next;
int job;
int flags;
int state;
int status;
pid_t pgrp;
pid_t ppid;
INT32 age;
clock_t systime;
clock_t usrtime;
Proc *proc_list;
Proc *last_proc;
#ifdef KSH
Coproc_id coproc_id;
#endif
#ifdef TTY_PGRP
TTY_state ttystate;
pid_t saved_ttypgrp;
#endif
};
#define JW_NONE 0x00
#define JW_INTERRUPT 0x01
#define JW_ASYNCNOTIFY 0x02
#define JW_STOPPEDWAIT 0x04
#define JL_OK 0
#define JL_NOSUCH 1
#define JL_AMBIG 2
#define JL_INVALID 3
static const char *const lookup_msgs[] = {
null,
"no such job",
"ambiguous",
"argument must be %job or process id",
(char *) 0
};
clock_t j_systime, j_usrtime;
static Job *job_list;
static Job *last_job;
static Job *async_job;
static pid_t async_pid;
static int nzombie;
static INT32 njobs;
static int child_max;
#ifdef JOB_SIGS
static int held_sigchld;
#endif
#ifdef JOBS
static struct shf *shl_j;
#endif
#ifdef NEED_PGRP_SYNC
static int j_sync_pipe[2];
static int j_sync_open;
#endif
#ifdef TTY_PGRP
static int ttypgrp_ok;
static pid_t restore_ttypgrp = -1;
static pid_t our_pgrp;
static int const tt_sigs[] = { SIGTSTP, SIGTTIN, SIGTTOU };
#endif
static void j_set_async ARGS((Job *j));
static void j_startjob ARGS((Job *j));
static int j_waitj ARGS((Job *j, int flags, const char *where));
static RETSIGTYPE j_sigchld ARGS((int sig));
static void j_print ARGS((Job *j, int how, struct shf *shf));
static Job *j_lookup ARGS((const char *cp, int *ecodep));
static Job *new_job ARGS((void));
static Proc *new_proc ARGS((void));
static void check_job ARGS((Job *j));
static void put_job ARGS((Job *j, int where));
static void remove_job ARGS((Job *j, const char *where));
static void kill_job ARGS((Job *j));
static void fill_command ARGS((char *c, int len, struct op *t));
void
j_init(mflagset)
int mflagset;
{
child_max = CHILD_MAX;
#ifdef JOB_SIGS
sigemptyset(&sm_default);
sigprocmask(SIG_SETMASK, &sm_default, (sigset_t *) 0);
sigemptyset(&sm_sigchld);
sigaddset(&sm_sigchld, SIGCHLD);
setsig(&sigtraps[SIGCHLD], j_sigchld,
SS_RESTORE_ORIG|SS_FORCE|SS_SHTRAP);
#else
setsig(&sigtraps[SIGCHLD], SIG_DFL, SS_RESTORE_ORIG|SS_FORCE);
#endif
#ifdef JOBS
if (!mflagset && Flag(FTALKING))
Flag(FMONITOR) = 1;
shl_j = shf_fdopen(2, SHF_WR, (struct shf *) 0);
# ifdef TTY_PGRP
if (Flag(FMONITOR) || Flag(FTALKING)) {
int i;
for (i = NELEM(tt_sigs); --i >= 0; ) {
sigtraps[tt_sigs[i]].flags |= TF_SHELL_USES;
setsig(&sigtraps[tt_sigs[i]], SIG_IGN,
SS_RESTORE_IGN|SS_FORCE);
}
}
# endif
if (Flag(FMONITOR))
j_change();
else
#endif
if (Flag(FTALKING))
tty_init(TRUE);
}
void
j_exit()
{
Job *j;
int killed = 0;
for (j = job_list; j != (Job *) 0; j = j->next) {
if (j->ppid == procpid
&& (j->state == PSTOPPED
|| (j->state == PRUNNING
&& ((j->flags & JF_FG)
|| (Flag(FLOGIN) && !Flag(FNOHUP)
&& procpid == kshpid)))))
{
killed = 1;
killpg(j->pgrp, SIGHUP);
#ifdef JOBS
if (j->state == PSTOPPED)
killpg(j->pgrp, SIGCONT);
#endif
}
}
if (killed)
sleep(1);
j_notify();
#ifdef JOBS
# ifdef TTY_PGRP
if (kshpid == procpid && restore_ttypgrp >= 0) {
tcsetpgrp(tty_fd, restore_ttypgrp);
setpgid(0, restore_ttypgrp);
}
# endif
if (Flag(FMONITOR)) {
Flag(FMONITOR) = 0;
j_change();
}
#endif
}
#ifdef JOBS
void
j_change()
{
int i;
if (Flag(FMONITOR)) {
tty_init(FALSE);
# ifdef TTY_PGRP
ttypgrp_ok = tty_fd >= 0 && tty_devtty;
if (ttypgrp_ok && (our_pgrp = getpgID()) < 0) {
warningf(FALSE, "j_init: getpgrp() failed: %s",
strerror(errno));
ttypgrp_ok = 0;
}
if (ttypgrp_ok) {
setsig(&sigtraps[SIGTTIN], SIG_DFL,
SS_RESTORE_ORIG|SS_FORCE);
while (1) {
pid_t ttypgrp;
if ((ttypgrp = tcgetpgrp(tty_fd)) < 0) {
warningf(FALSE,
"j_init: tcgetpgrp() failed: %s",
strerror(errno));
ttypgrp_ok = 0;
break;
}
if (ttypgrp == our_pgrp)
break;
kill(0, SIGTTIN);
}
}
for (i = NELEM(tt_sigs); --i >= 0; )
setsig(&sigtraps[tt_sigs[i]], SIG_IGN,
SS_RESTORE_DFL|SS_FORCE);
if (ttypgrp_ok && our_pgrp != kshpid) {
if (setpgid(0, kshpid) < 0) {
warningf(FALSE,
"j_init: setpgid() failed: %s",
strerror(errno));
ttypgrp_ok = 0;
} else {
if (tcsetpgrp(tty_fd, kshpid) < 0) {
warningf(FALSE,
"j_init: tcsetpgrp() failed: %s",
strerror(errno));
ttypgrp_ok = 0;
} else
restore_ttypgrp = our_pgrp;
our_pgrp = kshpid;
}
}
# if defined(NTTYDISC) && defined(TIOCSETD) && !defined(HAVE_TERMIOS_H) && !defined(HAVE_TERMIO_H)
if (ttypgrp_ok) {
int ldisc = NTTYDISC;
if (ioctl(tty_fd, TIOCSETD, &ldisc) < 0)
warningf(FALSE,
"j_init: can't set new line discipline: %s",
strerror(errno));
}
# endif
if (!ttypgrp_ok)
warningf(FALSE, "warning: won't have full job control");
# endif
if (tty_fd >= 0)
get_tty(tty_fd, &tty_state);
} else {
# ifdef TTY_PGRP
ttypgrp_ok = 0;
if (Flag(FTALKING))
for (i = NELEM(tt_sigs); --i >= 0; )
setsig(&sigtraps[tt_sigs[i]], SIG_IGN,
SS_RESTORE_IGN|SS_FORCE);
else
for (i = NELEM(tt_sigs); --i >= 0; ) {
if (sigtraps[tt_sigs[i]].flags & (TF_ORIG_IGN
|TF_ORIG_DFL))
setsig(&sigtraps[tt_sigs[i]],
(sigtraps[tt_sigs[i]].flags & TF_ORIG_IGN) ? SIG_IGN : SIG_DFL,
SS_RESTORE_ORIG|SS_FORCE);
}
# endif
if (!Flag(FTALKING))
tty_close();
}
}
#endif
int
exchild(t, flags, close_fd)
struct op *t;
int flags;
int close_fd;
{
static Proc *last_proc;
int i;
#ifdef JOB_SIGS
sigset_t omask;
#endif
Proc *p;
Job *j;
int rv = 0;
int forksleep;
int ischild;
if (flags & XEXEC)
return execute(t, flags & (XEXEC | XERROK));
#ifdef JOB_SIGS
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
#endif
p = new_proc();
p->next = (Proc *) 0;
p->state = PRUNNING;
WSTATUS(p->status) = 0;
p->pid = 0;
if (flags&XPIPEI) {
if (!last_job)
internal_errorf(1, "exchild: XPIPEI and no last_job - pid %d", (int) procpid);
j = last_job;
last_proc->next = p;
last_proc = p;
} else {
#ifdef NEED_PGRP_SYNC
if (j_sync_open) {
j_sync_open = 0;
closepipe(j_sync_pipe);
}
if (flags & XPIPEO) {
openpipe(j_sync_pipe);
j_sync_open = 1;
}
#endif
j = new_job();
j->flags = (flags & XXCOM) ? JF_XXCOM
: ((flags & XBGND) ? 0 : (JF_FG|JF_USETTYMODE));
j->usrtime = j->systime = 0;
j->state = PRUNNING;
j->pgrp = 0;
j->ppid = procpid;
j->age = ++njobs;
j->proc_list = p;
#ifdef KSH
j->coproc_id = 0;
#endif
last_job = j;
last_proc = p;
put_job(j, PJ_PAST_STOPPED);
}
fill_command(p->command, sizeof(p->command), t);
forksleep = 1;
while ((i = fork()) < 0 && errno == EAGAIN && forksleep < 32) {
if (intrsig)
break;
sleep(forksleep);
forksleep <<= 1;
}
if (i < 0) {
kill_job(j);
remove_job(j, "fork failed");
#ifdef NEED_PGRP_SYNC
if (j_sync_open) {
closepipe(j_sync_pipe);
j_sync_open = 0;
}
#endif
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
errorf("cannot fork - try again");
}
ischild = i == 0;
if (ischild)
p->pid = procpid = getpid();
else
p->pid = i;
#ifdef JOBS
if (Flag(FMONITOR) && !(flags&XXCOM)) {
int dotty = 0;
# ifdef NEED_PGRP_SYNC
int first_child_sync = 0;
# endif
# ifdef NEED_PGRP_SYNC
if (j_sync_open) {
if (j->pgrp == 0) {
close(j_sync_pipe[ischild]);
j_sync_pipe[ischild] = -1;
first_child_sync = ischild;
} else if (ischild) {
j_sync_open = 0;
closepipe(j_sync_pipe);
}
}
# endif
if (j->pgrp == 0) {
j->pgrp = p->pid;
dotty = 1;
}
setpgid(p->pid, j->pgrp);
# ifdef TTY_PGRP
if (ttypgrp_ok && dotty && !(flags & XBGND))
tcsetpgrp(tty_fd, j->pgrp);
# endif
# ifdef NEED_PGRP_SYNC
if (first_child_sync) {
char c;
while (read(j_sync_pipe[0], &c, 1) == -1
&& errno == EINTR)
;
close(j_sync_pipe[0]);
j_sync_open = 0;
}
# endif
}
#endif
if (close_fd >= 0 && (((flags & XPCLOSE) && !ischild)
|| ((flags & XCCLOSE) && ischild)))
close(close_fd);
if (ischild) {
#ifdef KSH
if (flags & XCOPROC)
coproc_cleanup(FALSE);
#endif
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
cleanup_parents_env();
#ifdef TTY_PGRP
if (Flag(FMONITOR) && !(flags & XXCOM)) {
for (i = NELEM(tt_sigs); --i >= 0; )
setsig(&sigtraps[tt_sigs[i]], SIG_DFL,
SS_RESTORE_DFL|SS_FORCE);
}
#endif
#ifdef HAVE_NICE
if (Flag(FBGNICE) && (flags & XBGND))
nice(4);
#endif
if ((flags & XBGND) && !Flag(FMONITOR)) {
setsig(&sigtraps[SIGINT], SIG_IGN,
SS_RESTORE_IGN|SS_FORCE);
setsig(&sigtraps[SIGQUIT], SIG_IGN,
SS_RESTORE_IGN|SS_FORCE);
if (!(flags & (XPIPEI | XCOPROC))) {
int fd = open("/dev/null", 0);
(void) ksh_dup2(fd, 0, TRUE);
close(fd);
}
}
remove_job(j, "child");
nzombie = 0;
#ifdef JOBS
ttypgrp_ok = 0;
Flag(FMONITOR) = 0;
#endif
Flag(FTALKING) = 0;
#ifdef OS2
if (tty_fd >= 0)
flags |= XINTACT;
#endif
tty_close();
cleartraps();
execute(t, (flags & XERROK) | XEXEC);
internal_errorf(0, "exchild: execute() returned");
unwind(LLEAVE);
}
change_random();
if (!(flags & XPIPEO)) {
#ifdef TTY_PGRP
#endif
j_startjob(j);
#ifdef KSH
if (flags & XCOPROC) {
j->coproc_id = coproc.id;
coproc.njobs++;
coproc.job = (void *) j;
}
#endif
if (flags & XBGND) {
j_set_async(j);
if (Flag(FTALKING)) {
shf_fprintf(shl_out, "[%d]", j->job);
for (p = j->proc_list; p; p = p->next)
shf_fprintf(shl_out, " %d", p->pid);
shf_putchar('\n', shl_out);
shf_flush(shl_out);
}
} else
rv = j_waitj(j, JW_NONE, "jw:last proc");
}
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
return rv;
}
void
startlast()
{
#ifdef JOB_SIGS
sigset_t omask;
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
#endif
if (last_job) {
last_job->flags |= JF_WAITING;
j_startjob(last_job);
}
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
}
int
waitlast()
{
int rv;
Job *j;
#ifdef JOB_SIGS
sigset_t omask;
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
#endif
j = last_job;
if (!j || !(j->flags & JF_STARTED)) {
if (!j)
warningf(TRUE, "waitlast: no last job");
else
internal_errorf(0, "waitlast: not started");
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
return 125;
}
rv = j_waitj(j, JW_NONE, "jw:waitlast");
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
return rv;
}
int
waitfor(cp, sigp)
const char *cp;
int *sigp;
{
int rv;
Job *j;
int ecode;
int flags = JW_INTERRUPT|JW_ASYNCNOTIFY;
#ifdef JOB_SIGS
sigset_t omask;
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
#endif
*sigp = 0;
if (cp == (char *) 0) {
for (j = job_list; j; j = j->next)
if (j->ppid == procpid && j->state == PRUNNING)
break;
if (!j) {
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
return -1;
}
} else if ((j = j_lookup(cp, &ecode))) {
flags &= ~JW_ASYNCNOTIFY;
if (j->ppid != procpid) {
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
return -1;
}
} else {
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
if (ecode != JL_NOSUCH)
bi_errorf("%s: %s", cp, lookup_msgs[ecode]);
return -1;
}
rv = j_waitj(j, flags, "jw:waitfor");
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
if (rv < 0)
*sigp = 128 + -rv;
return rv;
}
int
j_kill(cp, sig)
const char *cp;
int sig;
{
Job *j;
Proc *p;
int rv = 0;
int ecode;
#ifdef JOB_SIGS
sigset_t omask;
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
#endif
if ((j = j_lookup(cp, &ecode)) == (Job *) 0) {
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
bi_errorf("%s: %s", cp, lookup_msgs[ecode]);
return 1;
}
if (j->pgrp == 0) {
for (p=j->proc_list; p != (Proc *) 0; p = p->next)
if (kill(p->pid, sig) < 0) {
bi_errorf("%s: %s", cp, strerror(errno));
rv = 1;
}
} else {
#ifdef JOBS
if (j->state == PSTOPPED && (sig == SIGTERM || sig == SIGHUP))
(void) killpg(j->pgrp, SIGCONT);
#endif
if (killpg(j->pgrp, sig) < 0) {
bi_errorf("%s: %s", cp, strerror(errno));
rv = 1;
}
}
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
return rv;
}
#ifdef JOBS
int
j_resume(cp, bg)
const char *cp;
int bg;
{
Job *j;
Proc *p;
int ecode;
int running;
int rv = 0;
sigset_t omask;
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
if ((j = j_lookup(cp, &ecode)) == (Job *) 0) {
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
bi_errorf("%s: %s", cp, lookup_msgs[ecode]);
return 1;
}
if (j->pgrp == 0) {
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
bi_errorf("job not job-controlled");
return 1;
}
if (bg)
shprintf("[%d] ", j->job);
running = 0;
for (p = j->proc_list; p != (Proc *) 0; p = p->next) {
if (p->state == PSTOPPED) {
p->state = PRUNNING;
WSTATUS(p->status) = 0;
running = 1;
}
shprintf("%s%s", p->command, p->next ? "| " : null);
}
shprintf(newline);
shf_flush(shl_stdout);
if (running)
j->state = PRUNNING;
put_job(j, PJ_PAST_STOPPED);
if (bg)
j_set_async(j);
else {
# ifdef TTY_PGRP
if (j->state == PRUNNING) {
if (ttypgrp_ok && (j->flags & JF_SAVEDTTY)) {
set_tty(tty_fd, &j->ttystate, TF_NONE);
}
if (ttypgrp_ok && tcsetpgrp(tty_fd, (j->flags & JF_SAVEDTTYPGRP) ? j->saved_ttypgrp : j->pgrp) < 0) {
if (j->flags & JF_SAVEDTTY) {
set_tty(tty_fd, &tty_state, TF_NONE);
}
sigprocmask(SIG_SETMASK, &omask,
(sigset_t *) 0);
bi_errorf("1st tcsetpgrp(%d, %d) failed: %s",
tty_fd, (int) ((j->flags & JF_SAVEDTTYPGRP) ? j->saved_ttypgrp : j->pgrp), strerror(errno));
return 1;
}
}
# endif
j->flags |= JF_FG;
j->flags &= ~JF_KNOWN;
if (j == async_job)
async_job = (Job *) 0;
}
if (j->state == PRUNNING && killpg(j->pgrp, SIGCONT) < 0) {
int err = errno;
if (!bg) {
j->flags &= ~JF_FG;
# ifdef TTY_PGRP
if (ttypgrp_ok && (j->flags & JF_SAVEDTTY)) {
set_tty(tty_fd, &tty_state, TF_NONE);
}
if (ttypgrp_ok && tcsetpgrp(tty_fd, our_pgrp) < 0) {
warningf(TRUE,
"fg: 2nd tcsetpgrp(%d, %d) failed: %s",
tty_fd, (int) our_pgrp,
strerror(errno));
}
# endif
}
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
bi_errorf("cannot continue job %s: %s",
cp, strerror(err));
return 1;
}
if (!bg) {
# ifdef TTY_PGRP
if (ttypgrp_ok) {
j->flags &= ~(JF_SAVEDTTY | JF_SAVEDTTYPGRP);
}
# endif
rv = j_waitj(j, JW_NONE, "jw:resume");
}
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
return rv;
}
#endif
int
j_stopped_running()
{
Job *j;
int which = 0;
for (j = job_list; j != (Job *) 0; j = j->next) {
#ifdef JOBS
if (j->ppid == procpid && j->state == PSTOPPED)
which |= 1;
#endif
if (Flag(FLOGIN) && !Flag(FNOHUP) && procpid == kshpid
&& j->ppid == procpid && j->state == PRUNNING)
which |= 2;
}
if (which) {
shellf("You have %s%s%s jobs\n",
which & 1 ? "stopped" : "",
which == 3 ? " and " : "",
which & 2 ? "running" : "");
return 1;
}
return 0;
}
int
j_jobs(cp, slp, nflag)
const char *cp;
int slp;
int nflag;
{
Job *j, *tmp;
int how;
int zflag = 0;
#ifdef JOB_SIGS
sigset_t omask;
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
#endif
if (nflag < 0) {
nflag = 0;
zflag = 1;
}
if (cp) {
int ecode;
if ((j = j_lookup(cp, &ecode)) == (Job *) 0) {
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
bi_errorf("%s: %s", cp, lookup_msgs[ecode]);
return 1;
}
} else
j = job_list;
how = slp == 0 ? JP_MEDIUM : (slp == 1 ? JP_LONG : JP_PGRP);
for (; j; j = j->next) {
if ((!(j->flags & JF_ZOMBIE) || zflag)
&& (!nflag || (j->flags & JF_CHANGED)))
{
j_print(j, how, shl_stdout);
if (j->state == PEXITED || j->state == PSIGNALLED)
j->flags |= JF_REMOVE;
}
if (cp)
break;
}
for (j = job_list; j; j = tmp) {
tmp = j->next;
if (j->flags & JF_REMOVE)
remove_job(j, "jobs");
}
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
return 0;
}
void
j_notify()
{
Job *j, *tmp;
#ifdef JOB_SIGS
sigset_t omask;
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
#endif
for (j = job_list; j; j = j->next) {
#ifdef JOBS
if (Flag(FMONITOR) && (j->flags & JF_CHANGED))
j_print(j, JP_MEDIUM, shl_out);
#endif
if (j->state == PEXITED || j->state == PSIGNALLED)
j->flags |= JF_REMOVE;
}
for (j = job_list; j; j = tmp) {
tmp = j->next;
if (j->flags & JF_REMOVE)
remove_job(j, "notify");
}
shf_flush(shl_out);
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
}
pid_t
j_async()
{
#ifdef JOB_SIGS
sigset_t omask;
sigprocmask(SIG_BLOCK, &sm_sigchld, &omask);
#endif
if (async_job)
async_job->flags |= JF_KNOWN;
#ifdef JOB_SIGS
sigprocmask(SIG_SETMASK, &omask, (sigset_t *) 0);
#endif
return async_pid;
}
static void
j_set_async(j)
Job *j;
{
Job *jl, *oldest;
if (async_job && (async_job->flags & (JF_KNOWN|JF_ZOMBIE)) == JF_ZOMBIE)
remove_job(async_job, "async");
if (!(j->flags & JF_STARTED)) {
internal_errorf(0, "j_async: job not started");
return;
}
async_job = j;
async_pid = j->last_proc->pid;
while (nzombie > child_max) {
oldest = (Job *) 0;
for (jl = job_list; jl; jl = jl->next)
if (jl != async_job && (jl->flags & JF_ZOMBIE)
&& (!oldest || jl->age < oldest->age))
oldest = jl;
if (!oldest) {
if (!(async_job->flags & JF_ZOMBIE) || nzombie != 1) {
internal_errorf(0, "j_async: bad nzombie (%d)", nzombie);
nzombie = 0;
}
break;
}
remove_job(oldest, "zombie");
}
}
static void
j_startjob(j)
Job *j;
{
Proc *p;
j->flags |= JF_STARTED;
for (p = j->proc_list; p->next; p = p->next)
;
j->last_proc = p;
#ifdef NEED_PGRP_SYNC
if (j_sync_open) {
j_sync_open = 0;
closepipe(j_sync_pipe);
}
#endif
#ifdef JOB_SIGS
if (held_sigchld) {
held_sigchld = 0;
kill(procpid, SIGCHLD);
}
#endif
}
static int
j_waitj(j, flags, where)
Job *j;
int flags;
const char *where;
{
int rv;
j->flags |= JF_WAITING;
if (flags & JW_ASYNCNOTIFY)
j->flags |= JF_W_ASYNCNOTIFY;
if (!Flag(FMONITOR))
flags |= JW_STOPPEDWAIT;
while ((volatile int) j->state == PRUNNING
|| ((flags & JW_STOPPEDWAIT)
&& (volatile int) j->state == PSTOPPED))
{
#ifdef JOB_SIGS
sigsuspend(&sm_default);
#else
j_sigchld(SIGCHLD);
#endif
if (fatal_trap) {
int oldf = j->flags & (JF_WAITING|JF_W_ASYNCNOTIFY);
j->flags &= ~(JF_WAITING|JF_W_ASYNCNOTIFY);
runtraps(TF_FATAL);
j->flags |= oldf;
}
if ((flags & JW_INTERRUPT) && (rv = trap_pending())) {
j->flags &= ~(JF_WAITING|JF_W_ASYNCNOTIFY);
return -rv;
}
}
j->flags &= ~(JF_WAITING|JF_W_ASYNCNOTIFY);
if (j->flags & JF_FG) {
WAIT_T status;
j->flags &= ~JF_FG;
#ifdef TTY_PGRP
if (Flag(FMONITOR) && ttypgrp_ok && j->pgrp) {
if (j->state == PSTOPPED
&& (j->saved_ttypgrp = tcgetpgrp(tty_fd)) >= 0)
j->flags |= JF_SAVEDTTYPGRP;
if (tcsetpgrp(tty_fd, our_pgrp) < 0) {
warningf(TRUE,
"j_waitj: tcsetpgrp(%d, %d) failed: %s",
tty_fd, (int) our_pgrp,
strerror(errno));
}
if (j->state == PSTOPPED) {
j->flags |= JF_SAVEDTTY;
get_tty(tty_fd, &j->ttystate);
}
}
#endif
if (tty_fd >= 0) {
if (j->state == PEXITED && j->status == 0
&& (j->flags & JF_USETTYMODE))
{
get_tty(tty_fd, &tty_state);
} else {
set_tty(tty_fd, &tty_state,
(j->state == PEXITED) ? 0 : TF_MIPSKLUDGE);
if (j->state == PSTOPPED)
j->flags &= ~JF_USETTYMODE;
}
}
#ifdef JOBS
status = j->last_proc->status;
if (Flag(FMONITOR) && j->state == PSIGNALLED
&& WIFSIGNALED(status)
&& (sigtraps[WTERMSIG(status)].flags & TF_TTY_INTR))
trapsig(WTERMSIG(status));
#endif
}
j_usrtime = j->usrtime;
j_systime = j->systime;
rv = j->status;
if (!(flags & JW_ASYNCNOTIFY)
&& (!Flag(FMONITOR) || j->state != PSTOPPED))
{
j_print(j, JP_SHORT, shl_out);
shf_flush(shl_out);
}
if (j->state != PSTOPPED
&& (!Flag(FMONITOR) || !(flags & JW_ASYNCNOTIFY)))
remove_job(j, where);
return rv;
}
static RETSIGTYPE
j_sigchld(sig)
int sig;
{
int errno_ = errno;
Job *j;
Proc UNINITIALIZED(*p);
int pid;
WAIT_T status;
struct tms t0, t1;
#ifdef JOB_SIGS
for (j = job_list; j; j = j->next)
if (j->ppid == procpid && !(j->flags & JF_STARTED)) {
held_sigchld = 1;
return RETSIGVAL;
}
#endif
ksh_times(&t0);
do {
#ifdef JOB_SIGS
pid = ksh_waitpid(-1, &status, (WNOHANG|WUNTRACED));
#else
pid = wait(&status);
#endif
if (pid <= 0)
break;
ksh_times(&t1);
for (j = job_list; j != (Job *) 0; j = j->next)
for (p = j->proc_list; p != (Proc *) 0; p = p->next)
if (p->pid == pid)
goto found;
found:
if (j == (Job *) 0) {
t0 = t1;
continue;
}
j->usrtime += t1.tms_cutime - t0.tms_cutime;
j->systime += t1.tms_cstime - t0.tms_cstime;
t0 = t1;
p->status = status;
#ifdef JOBS
if (WIFSTOPPED(status))
p->state = PSTOPPED;
else
#endif
if (WIFSIGNALED(status))
p->state = PSIGNALLED;
else
p->state = PEXITED;
check_job(j);
}
#ifdef JOB_SIGS
while (1);
#else
while (0);
#endif
errno = errno_;
return RETSIGVAL;
}
static void
check_job(j)
Job *j;
{
int jstate;
Proc *p;
if (!(j->flags & JF_STARTED)) {
internal_errorf(0, "check_job: job started (flags 0x%x)",
j->flags);
return;
}
jstate = PRUNNING;
for (p=j->proc_list; p != (Proc *) 0; p = p->next) {
if (p->state == PRUNNING)
return;
if (p->state > jstate)
jstate = p->state;
}
j->state = jstate;
switch (j->last_proc->state) {
case PEXITED:
j->status = WEXITSTATUS(j->last_proc->status);
break;
case PSIGNALLED:
j->status = 128 + WTERMSIG(j->last_proc->status);
break;
default:
j->status = 0;
break;
}
#ifdef KSH
if (j->state == PEXITED || j->state == PSIGNALLED) {
if (coproc.job == j) {
coproc.job = (void *) 0;
coproc_write_close(coproc.write);
}
if (j->coproc_id && j->coproc_id == coproc.id
&& --coproc.njobs == 0)
coproc_readw_close(coproc.read);
}
#endif
j->flags |= JF_CHANGED;
#ifdef JOBS
if (Flag(FMONITOR) && !(j->flags & JF_XXCOM)) {
if (j->state == PSTOPPED)
put_job(j, PJ_ON_FRONT);
if (Flag(FNOTIFY)
&& (j->flags & (JF_WAITING|JF_W_ASYNCNOTIFY)) != JF_WAITING)
{
{
struct env *ep;
int fd = 2;
for (ep = e; ep; ep = ep->oenv)
if (ep->savefd && ep->savefd[2])
fd = ep->savefd[2];
shf_reopen(fd, SHF_WR, shl_j);
}
j_print(j, JP_MEDIUM, shl_j);
shf_flush(shl_j);
if (!(j->flags & JF_WAITING) && j->state != PSTOPPED)
remove_job(j, "notify");
}
}
#endif
if (!Flag(FMONITOR) && !(j->flags & (JF_WAITING|JF_FG))
&& j->state != PSTOPPED)
{
if (j == async_job || (j->flags & JF_KNOWN)) {
j->flags |= JF_ZOMBIE;
j->job = -1;
nzombie++;
} else
remove_job(j, "checkjob");
}
}
static void
j_print(j, how, shf)
Job *j;
int how;
struct shf *shf;
{
Proc *p;
int state;
WAIT_T status;
int coredumped;
char jobchar = ' ';
char buf[64];
const char *filler;
int output = 0;
if (how == JP_PGRP) {
shf_fprintf(shf, "%d\n", j->pgrp ? j->pgrp
: (j->last_proc ? j->last_proc->pid : 0));
return;
}
j->flags &= ~JF_CHANGED;
filler = j->job > 10 ? "\n       " : "\n      ";
if (j == job_list)
jobchar = '+';
else if (j == job_list->next)
jobchar = '-';
for (p = j->proc_list; p != (Proc *) 0;) {
coredumped = 0;
switch (p->state) {
case PRUNNING:
strcpy(buf, "Running");
break;
case PSTOPPED:
strcpy(buf, sigtraps[WSTOPSIG(p->status)].mess);
break;
case PEXITED:
if (how == JP_SHORT)
buf[0] = '\0';
else if (WEXITSTATUS(p->status) == 0)
strcpy(buf, "Done");
else
shf_snprintf(buf, sizeof(buf), "Done (%d)",
WEXITSTATUS(p->status));
break;
case PSIGNALLED:
if (WIFCORED(p->status))
coredumped = 1;
if (how == JP_SHORT && !coredumped
&& (WTERMSIG(p->status) == SIGINT
|| WTERMSIG(p->status) == SIGPIPE)) {
buf[0] = '\0';
} else
strcpy(buf, sigtraps[WTERMSIG(p->status)].mess);
break;
}
if (how != JP_SHORT)
if (p == j->proc_list)
shf_fprintf(shf, "[%d] %c ", j->job, jobchar);
else
shf_fprintf(shf, "%s", filler);
if (how == JP_LONG)
shf_fprintf(shf, "%5d ", p->pid);
if (how == JP_SHORT) {
if (buf[0]) {
output = 1;
shf_fprintf(shf, "%s%s ",
buf, coredumped ? " (core dumped)" : null);
}
} else {
output = 1;
shf_fprintf(shf, "%-20s %s%s%s", buf, p->command,
p->next ? "|" : null,
coredumped ? " (core dumped)" : null);
}
state = p->state;
status = p->status;
p = p->next;
while (p && p->state == state
&& WSTATUS(p->status) == WSTATUS(status))
{
if (how == JP_LONG)
shf_fprintf(shf, "%s%5d %-20s %s%s", filler, p->pid,
space, p->command, p->next ? "|" : null);
else if (how == JP_MEDIUM)
shf_fprintf(shf, " %s%s", p->command,
p->next ? "|" : null);
p = p->next;
}
}
if (output)
shf_fprintf(shf, newline);
}
static Job *
j_lookup(cp, ecodep)
const char *cp;
int *ecodep;
{
Job *j, *last_match;
Proc *p;
int len, job = 0;
if (digit(*cp)) {
job = atoi(cp);
for (j = job_list; j != (Job *) 0; j = j->next)
if (j->last_proc && j->last_proc->pid == job)
return j;
for (j = job_list; j != (Job *) 0; j = j->next)
if (j->pgrp && j->pgrp == job)
return j;
if (ecodep)
*ecodep = JL_NOSUCH;
return (Job *) 0;
}
if (*cp != '%') {
if (ecodep)
*ecodep = JL_INVALID;
return (Job *) 0;
}
switch (*++cp) {
case '\0':
case '+':
case '%':
if (job_list != (Job *) 0)
return job_list;
break;
case '-':
if (job_list != (Job *) 0 && job_list->next)
return job_list->next;
break;
case '0': case '1': case '2': case '3': case '4':
case '5': case '6': case '7': case '8': case '9':
job = atoi(cp);
for (j = job_list; j != (Job *) 0; j = j->next)
if (j->job == job)
return j;
break;
case '?':
last_match = (Job *) 0;
for (j = job_list; j != (Job *) 0; j = j->next)
for (p = j->proc_list; p != (Proc *) 0; p = p->next)
if (strstr(p->command, cp+1) != (char *) 0) {
if (last_match) {
if (ecodep)
*ecodep = JL_AMBIG;
return (Job *) 0;
}
last_match = j;
}
if (last_match)
return last_match;
break;
default:
len = strlen(cp);
last_match = (Job *) 0;
for (j = job_list; j != (Job *) 0; j = j->next)
if (strncmp(cp, j->proc_list->command, len) == 0) {
if (last_match) {
if (ecodep)
*ecodep = JL_AMBIG;
return (Job *) 0;
}
last_match = j;
}
if (last_match)
return last_match;
break;
}
if (ecodep)
*ecodep = JL_NOSUCH;
return (Job *) 0;
}
static Job *free_jobs;
static Proc *free_procs;
static Job *
new_job()
{
int i;
Job *newj, *j;
if (free_jobs != (Job *) 0) {
newj = free_jobs;
free_jobs = free_jobs->next;
} else
newj = (Job *) alloc(sizeof(Job), APERM);
for (i = 1; ; i++) {
for (j = job_list; j && j->job != i; j = j->next)
;
if (j == (Job *) 0)
break;
}
newj->job = i;
return newj;
}
static Proc *
new_proc()
{
Proc *p;
if (free_procs != (Proc *) 0) {
p = free_procs;
free_procs = free_procs->next;
} else
p = (Proc *) alloc(sizeof(Proc), APERM);
return p;
}
static void
remove_job(j, where)
Job *j;
const char *where;
{
Proc *p, *tmp;
Job **prev, *curr;
prev = &job_list;
curr = *prev;
for (; curr != (Job *) 0 && curr != j; prev = &curr->next, curr = *prev)
;
if (curr != j) {
internal_errorf(0, "remove_job: job not found (%s)", where);
return;
}
*prev = curr->next;
for (p = j->proc_list; p != (Proc *) 0; ) {
tmp = p;
p = p->next;
tmp->next = free_procs;
free_procs = tmp;
}
if ((j->flags & JF_ZOMBIE) && j->ppid == procpid)
--nzombie;
j->next = free_jobs;
free_jobs = j;
if (j == last_job)
last_job = (Job *) 0;
if (j == async_job)
async_job = (Job *) 0;
}
static void
put_job(j, where)
Job *j;
int where;
{
Job **prev, *curr;
prev = &job_list;
curr = job_list;
for (; curr && curr != j; prev = &curr->next, curr = *prev)
;
if (curr == j)
*prev = curr->next;
switch (where) {
case PJ_ON_FRONT:
j->next = job_list;
job_list = j;
break;
case PJ_PAST_STOPPED:
prev = &job_list;
curr = job_list;
for (; curr && curr->state == PSTOPPED; prev = &curr->next,
curr = *prev)
;
j->next = curr;
*prev = j;
break;
}
}
static void
kill_job(j)
Job *j;
{
Proc *p;
for (p = j->proc_list; p != (Proc *) 0; p = p->next)
if (p->pid != 0)
(void) kill(p->pid, SIGKILL);
}
static void
fill_command(c, len, t)
char *c;
int len;
struct op *t;
{
int alen;
char **ap;
if (t->type == TEXEC || t->type == TCOM) {
ap = t->args;
--len;
while (len > 0 && *ap != (char *) 0) {
alen = strlen(*ap);
if (alen > len)
alen = len;
memcpy(c, *ap, alen);
c += alen;
len -= alen;
if (len > 0) {
*c++ = ' '; len--;
}
ap++;
}
*c = '\0';
} else
snptreef(c, len, "%T", t);
}