#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#define csr8r(r)	(((ulong *)PHYSCONS)[r])
#define csr8o(r, v)	(((ulong *)PHYSCONS)[r] = (v))
enum {
Thr		= 0,
Lsr		= 5,
};
enum {
Thre		= 0x20,
};
void	putc(int);
void
main(ulong aentry, ulong acode, ulong asize)
{
void (*kernel)(void);
static ulong entry, code, size;
putc('B'); putc('o'); putc('o'); putc('t');
entry = aentry;
code = acode;
size = asize;
setsp(entry-0x20-4);
memmove((void *)entry, (void *)code, size);
cleancache();
coherence();
putc(' ');
kernel = (void*)entry;
(*kernel)();
putc('?');
putc('!');
for(;;)
;
}
void
putc(int c)
{
int i;
for(i = 0; !(csr8r(Lsr) & Thre) && i < 1000000; i++)
;
csr8o(Thr, (uchar)c);
for(i = 0; !(csr8r(Lsr) & Thre) && i < 1000000; i++)
;
}
long
syscall(Ureg*)
{
return -1;
}
void
trap(Ureg *)
{
}