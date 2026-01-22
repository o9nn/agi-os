#include <u.h>
#include <libc.h>
#include <ureg.h>
int	__noterestore(void);
void
notejmp(void *vr, jmp_buf j, int ret)
{
struct Ureg *r = vr;
r->r4 = ret;
if(ret == 0)
r->r4 = 1;
r->r5 = j[JMPBUFPC] - JMPBUFDPC;
r->pc = (ulong)__noterestore;
r->sp = j[JMPBUFSP];
noted(NCONT);
}