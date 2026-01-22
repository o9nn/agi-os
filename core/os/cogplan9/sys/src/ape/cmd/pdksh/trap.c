#define FROM_TRAP_C
#include "sh.h"
Trap sigtraps[SIGNALS+1] = {
{ SIGEXIT_, "EXIT", "Signal 0" },
{ 1 , "HUP", "Hangup" },
{ 2 , "INT", "Interrupt" },
{ 3 , "QUIT", "Quit" },
{ 4 , "ILL", "Illegal instruction" },
{ 5 , "ABRT", "Abort" },
{ 6 , "FPE", "Floating point exception" },
{ 7 , "KILL", "Killed" },
{ 8 , "SEGV", "Memory fault" },
{ 9 , "PIPE", "Broken pipe" },
{ 10 , "ALRM", "Alarm clock" },
{ 11 , "TERM", "Terminated" },
{ 12 , "USR1", "User defined signal 1" },
{ 13 , "USR2", "User defined signal 2" },
{ 14 , "BUS", "Bus error" },
{ 15 , "CHLD", "Child exited" },
{ 16 , "CONT", "Continued" },
{ 17 , "STOP", "Stopped (signal)" },
{ 18 , "TSTP", "Stopped" },
{ 19 , "TTIN", "Stopped (tty input)" },
{ 20 , "TTOU", "Stopped (tty output)" },
{ SIGERR_, "ERR", "Error handler" },
};
static struct sigaction Sigact_ign, Sigact_trap;
void
inittraps()
{
#ifdef HAVE_SYS_SIGLIST
# ifndef SYS_SIGLIST_DECLARED
extern char *sys_siglist[];
# endif
int i;
for (i = 0; i < NSIG; i++)
if (!sigtraps[i].name && sys_siglist[i] && sys_siglist[i][0])
sigtraps[i].mess = sys_siglist[i];
#endif
sigemptyset(&Sigact_ign.sa_mask);
Sigact_ign.sa_flags = KSH_SA_FLAGS;
Sigact_ign.sa_handler = SIG_IGN;
Sigact_trap = Sigact_ign;
Sigact_trap.sa_handler = trapsig;
sigtraps[SIGINT].flags |= TF_DFL_INTR | TF_TTY_INTR;
sigtraps[SIGQUIT].flags |= TF_DFL_INTR | TF_TTY_INTR;
sigtraps[SIGTERM].flags |= TF_DFL_INTR;
sigtraps[SIGHUP].flags |= TF_FATAL;
sigtraps[SIGCHLD].flags |= TF_SHELL_USES;
setsig(&sigtraps[SIGINT], trapsig, SS_RESTORE_ORIG);
setsig(&sigtraps[SIGQUIT], trapsig, SS_RESTORE_ORIG);
setsig(&sigtraps[SIGTERM], trapsig, SS_RESTORE_ORIG);
setsig(&sigtraps[SIGHUP], trapsig, SS_RESTORE_ORIG);
}
#ifdef KSH
static RETSIGTYPE alarm_catcher ARGS((int sig));
void
alarm_init()
{
sigtraps[SIGALRM].flags |= TF_SHELL_USES;
setsig(&sigtraps[SIGALRM], alarm_catcher,
SS_RESTORE_ORIG|SS_FORCE|SS_SHTRAP);
}
static RETSIGTYPE
alarm_catcher(sig)
int sig;
{
if (ksh_tmout_state == TMOUT_READING) {
int left = alarm(0);
if (left == 0) {
ksh_tmout_state = TMOUT_LEAVING;
intrsig = 1;
} else
alarm(left);
}
return RETSIGVAL;
}
#endif
Trap *
gettrap(name, igncase)
const char *name;
int igncase;
{
int i;
register Trap *p;
if (digit(*name)) {
int n;
if (getn(name, &n) && 0 <= n && n < SIGNALS)
return &sigtraps[n];
return NULL;
}
if (strncasecmp(name, "sig", 3) == 0)
name += 3;
for (p = sigtraps, i = SIGNALS+1; --i >= 0; p++)
if (p->name && (igncase ? strcasecmp(p->name, name) == 0
: strcmp(p->name, name) == 0))
return p;
return NULL;
}
RETSIGTYPE
trapsig(i)
int i;
{
Trap *p = &sigtraps[i];
trap = p->set = 1;
if (p->flags & TF_DFL_INTR)
intrsig = 1;
if ((p->flags & TF_FATAL) && !p->trap) {
fatal_trap = 1;
intrsig = 1;
}
if (p->shtrap)
(*p->shtrap)(i);
#ifdef V7_SIGNALS
if (sigtraps[i].cursig == trapsig)
sigaction(i, &Sigact_trap, (struct sigaction *) 0);
#endif
return RETSIGVAL;
}
void
intrcheck()
{
if (intrsig)
runtraps(TF_DFL_INTR|TF_FATAL);
}
int
fatal_trap_check()
{
int i;
Trap *p;
for (p = sigtraps, i = SIGNALS+1; --i >= 0; p++)
if (p->set && (p->flags & (TF_DFL_INTR|TF_FATAL)))
return 128 + p->signal;
return 0;
}
int
trap_pending()
{
int i;
Trap *p;
for (p = sigtraps, i = SIGNALS+1; --i >= 0; p++)
if (p->set && ((p->trap && p->trap[0])
|| ((p->flags & (TF_DFL_INTR|TF_FATAL))
&& !p->trap)))
return p->signal;
return 0;
}
void
runtraps(flag)
int flag;
{
int i;
register Trap *p;
#ifdef KSH
if (ksh_tmout_state == TMOUT_LEAVING) {
ksh_tmout_state = TMOUT_EXECUTING;
warningf(FALSE, "timed out waiting for input");
unwind(LEXIT);
} else
ksh_tmout_state = TMOUT_EXECUTING;
#endif
if (!flag)
trap = 0;
if (flag & TF_DFL_INTR)
intrsig = 0;
if (flag & TF_FATAL)
fatal_trap = 0;
for (p = sigtraps, i = SIGNALS+1; --i >= 0; p++)
if (p->set && (!flag
|| ((p->flags & flag) && p->trap == (char *) 0)))
runtrap(p);
}
void
runtrap(p)
Trap *p;
{
int i = p->signal;
char *trapstr = p->trap;
int oexstat;
int UNINITIALIZED(old_changed);
p->set = 0;
if (trapstr == (char *) 0) {
if (p->flags & TF_FATAL) {
exstat = 128 + i;
unwind(LLEAVE);
}
if (p->flags & TF_DFL_INTR) {
exstat = 128 + i;
unwind(LINTR);
}
return;
}
if (trapstr[0] == '\0')
return;
if (i == SIGEXIT_ || i == SIGERR_) {
old_changed = p->flags & TF_CHANGED;
p->flags &= ~TF_CHANGED;
p->trap = (char *) 0;
}
oexstat = exstat;
command(trapstr);
exstat = oexstat;
if (i == SIGEXIT_ || i == SIGERR_) {
if (p->flags & TF_CHANGED)
afree(trapstr, APERM);
else
p->trap = trapstr;
p->flags |= old_changed;
}
}
void
cleartraps()
{
int i;
Trap *p;
trap = 0;
intrsig = 0;
fatal_trap = 0;
for (i = SIGNALS+1, p = sigtraps; --i >= 0; p++) {
p->set = 0;
if ((p->flags & TF_USER_SET) && (p->trap && p->trap[0]))
settrap(p, (char *) 0);
}
}
void
restoresigs()
{
int i;
Trap *p;
for (i = SIGNALS+1, p = sigtraps; --i >= 0; p++)
if (p->flags & (TF_EXEC_IGN|TF_EXEC_DFL))
setsig(p, (p->flags & TF_EXEC_IGN) ? SIG_IGN : SIG_DFL,
SS_RESTORE_CURR|SS_FORCE);
}
void
settrap(p, s)
Trap *p;
char *s;
{
handler_t f;
if (p->trap)
afree(p->trap, APERM);
p->trap = str_save(s, APERM);
p->flags |= TF_CHANGED;
f = !s ? SIG_DFL : s[0] ? trapsig : SIG_IGN;
p->flags |= TF_USER_SET;
if ((p->flags & (TF_DFL_INTR|TF_FATAL)) && f == SIG_DFL)
f = trapsig;
else if (p->flags & TF_SHELL_USES) {
if (!(p->flags & TF_ORIG_IGN) || Flag(FTALKING)) {
p->flags &= ~(TF_EXEC_IGN|TF_EXEC_DFL);
if (f == SIG_IGN)
p->flags |= TF_EXEC_IGN;
else
p->flags |= TF_EXEC_DFL;
}
return;
}
setsig(p, f, SS_RESTORE_CURR|SS_USER);
}
int
block_pipe()
{
int restore_dfl = 0;
Trap *p = &sigtraps[SIGPIPE];
if (!(p->flags & (TF_ORIG_IGN|TF_ORIG_DFL))) {
setsig(p, SIG_IGN, SS_RESTORE_CURR);
if (p->flags & TF_ORIG_DFL)
restore_dfl = 1;
} else if (p->cursig == SIG_DFL) {
setsig(p, SIG_IGN, SS_RESTORE_CURR);
restore_dfl = 1;
}
return restore_dfl;
}
void
restore_pipe(restore_dfl)
int restore_dfl;
{
if (restore_dfl)
setsig(&sigtraps[SIGPIPE], SIG_DFL, SS_RESTORE_CURR);
}
int
setsig(p, f, flags)
Trap *p;
handler_t f;
int flags;
{
struct sigaction sigact;
if (p->signal == SIGEXIT_ || p->signal == SIGERR_)
return 1;
if (!(p->flags & (TF_ORIG_IGN|TF_ORIG_DFL))) {
sigaction(p->signal, &Sigact_ign, &sigact);
p->flags |= sigact.sa_handler == SIG_IGN ?
TF_ORIG_IGN : TF_ORIG_DFL;
p->cursig = SIG_IGN;
}
if ((p->flags & TF_ORIG_IGN) && !(flags & SS_FORCE)
&& (!(flags & SS_USER) || !Flag(FTALKING)))
return 0;
setexecsig(p, flags & SS_RESTORE_MASK);
if (!(flags & SS_USER))
p->shtrap = (handler_t) 0;
if (flags & SS_SHTRAP) {
p->shtrap = f;
f = trapsig;
}
if (p->cursig != f) {
p->cursig = f;
sigemptyset(&sigact.sa_mask);
sigact.sa_flags = KSH_SA_FLAGS;
sigact.sa_handler = f;
sigaction(p->signal, &sigact, (struct sigaction *) 0);
}
return 1;
}
void
setexecsig(p, restore)
Trap *p;
int restore;
{
if (!(p->flags & (TF_ORIG_IGN|TF_ORIG_DFL)))
internal_errorf(1, "setexecsig: unset signal %d(%s)",
p->signal, p->name);
p->flags &= ~(TF_EXEC_IGN|TF_EXEC_DFL);
switch (restore & SS_RESTORE_MASK) {
case SS_RESTORE_CURR:
break;
case SS_RESTORE_ORIG:
p->flags |= p->flags & TF_ORIG_IGN ? TF_EXEC_IGN : TF_EXEC_DFL;
break;
case SS_RESTORE_DFL:
p->flags |= TF_EXEC_DFL;
break;
case SS_RESTORE_IGN:
p->flags |= TF_EXEC_IGN;
break;
}
}