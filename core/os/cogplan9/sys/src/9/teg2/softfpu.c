#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
int
fpudevprocio(Proc* proc, void* a, long n, uintptr offset, int write)
{
USED(proc, a, n, offset, write);
return 0;
}
void
fpunotify(Ureg*)
{
}
void
fpunoted(void)
{
}
void
fpusysrfork(Ureg*)
{
}
void
fpusysrforkchild(Proc*, Ureg *, Proc*)
{
}
void
fpuprocsave(Proc*)
{
}
void
fpuprocrestore(Proc*)
{
}
void
fpusysprocsetup(Proc*)
{
}
void
fpuinit(void)
{
}
int
fpuemu(Ureg* ureg)
{
int nfp;
if(waserror()){
splhi();
postnote(up, 1, up->errstr, NDebug);
return 1;
}
spllo();
nfp = fpiarm(ureg);
splhi();
poperror();
return nfp;
}
void
fpon(void)
{
}
void
fpoff(void)
{
}