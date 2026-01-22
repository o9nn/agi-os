#include <u.h>
#include <libc.h>
#include <thread.h>
#include "threadimpl.h"
#define PIPEMNT	"/mnt/temp"
void
procexec(Channel *pidc, char *prog, char *args[])
{
int n;
Proc *p;
Thread *t;
_threaddebug(DBGEXEC, "procexec %s", prog);
p = _threadgetproc();
t = p->thread;
if(p->threads.head != t || p->threads.head->nextt != nil){
werrstr("not only thread in proc");
Bad:
if(pidc)
sendul(pidc, ~0);
return;
}
if(bind("#|", PIPEMNT, MREPL) < 0)
goto Bad;
if((p->exec.fd[0] = open(PIPEMNT "/data", OREAD)) < 0){
unmount(nil, PIPEMNT);
goto Bad;
}
if((p->exec.fd[1] = open(PIPEMNT "/data1", OWRITE|OCEXEC)) < 0){
close(p->exec.fd[0]);
unmount(nil, PIPEMNT);
goto Bad;
}
unmount(nil, PIPEMNT);
assert(p->needexec==0);
p->exec.prog = prog;
p->exec.args = args;
p->needexec = 1;
_sched();
close(p->exec.fd[1]);
if((n = read(p->exec.fd[0], p->exitstr, ERRMAX-1)) > 0){
p->exitstr[n] = '\0';
errstr(p->exitstr, ERRMAX);
close(p->exec.fd[0]);
goto Bad;
}
close(p->exec.fd[0]);
if(pidc)
sendul(pidc, t->ret);
_schedexecwait();
}
void
procexecl(Channel *pidc, char *f, ...)
{
procexec(pidc, f, &f+1);
}