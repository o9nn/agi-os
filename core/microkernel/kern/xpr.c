#include <string.h>
#include <kern/debug.h>
#include <kern/xpr.h>
#include <kern/lock.h>
#include "cpu_number.h"
#include <machine/spl.h>
#include <vm/vm_kern.h>
def_simple_lock_data(static,	xprlock)
boolean_t xprenable = TRUE;
int nxprbufs = 0;
int xprflags = 0;
struct xprbuf *xprbase;
struct xprbuf *xprptr;
struct xprbuf *xprlast;
void xpr(
char 	*msg,
int 	arg1,
int 	arg2,
int 	arg3,
int 	arg4,
int 	arg5)
{
spl_t s;
struct xprbuf *x;
if (!xprenable || (xprptr == 0))
return;
s = splhigh();
simple_lock(&xprlock);
x = xprptr++;
if (xprptr >= xprlast) {
xprptr = xprbase;
}
*(struct xprbuf **)xprlast = xprptr;
simple_unlock(&xprlock);
splx(s);
x->msg = msg;
x->arg1 = arg1;
x->arg2 = arg2;
x->arg3 = arg3;
x->arg4 = arg4;
x->arg5 = arg5;
x->timestamp = XPR_TIMESTAMP;
x->cpuinfo = cpu_number();
}
void xprbootstrap(void)
{
vm_offset_t addr;
vm_size_t size;
kern_return_t kr;
simple_lock_init(&xprlock);
if (nxprbufs == 0)
return;
size = nxprbufs * sizeof(struct xprbuf) + sizeof xprptr;
kr = kmem_alloc_wired(kernel_map, &addr, size);
if (kr != KERN_SUCCESS)
panic("xprbootstrap");
if (xprenable) {
memset((void *) addr, 0, size);
}
xprbase = (struct xprbuf *) addr;
xprlast = &xprbase[nxprbufs];
xprptr = xprbase;
}
int		xprinitial = 0;
void xprinit(void)
{
xprflags |= xprinitial;
}
#if	MACH_KDB
#include <machine/setjmp.h>
#include <ddb/db_output.h>
extern jmp_buf_t *db_recover;
void xpr_dump(
struct xprbuf 	*base,
int 		nbufs)
{
jmp_buf_t db_jmpbuf;
jmp_buf_t *prev;
struct xprbuf *last, *ptr;
struct xprbuf *x;
int i;
spl_t s;
if (base == 0) {
base = xprbase;
nbufs = nxprbufs;
}
if (nbufs == 0)
return;
if (base == xprbase) {
s = splhigh();
simple_lock(&xprlock);
}
last = base + nbufs;
ptr = * (struct xprbuf **) last;
prev = db_recover;
if (_setjmp(db_recover = &db_jmpbuf) == 0)
for (x = ptr, i = 0; i < nbufs; i++) {
if (--x < base)
x = last - 1;
if (x->msg == 0)
break;
db_printf("<%d:%x:%x> ", x - base, x->cpuinfo, x->timestamp);
db_printf(x->msg, x->arg1,x->arg2,x->arg3,x->arg4,x->arg5);
}
db_recover = prev;
if (base == xprbase) {
simple_unlock(&xprlock);
(void) splx(s);
}
}
#endif