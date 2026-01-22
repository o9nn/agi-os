#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
Mbi	mbhdr;
int	nmmap;
Mbi	*multibootheader = &mbhdr;
MMap	mmap[32+1];
void
mkmultiboot(void)
{
MMap *lmmap;
multibootheader = (Mbi *)KADDR(BIOSTABLES);
memset(multibootheader, 0, sizeof *multibootheader);
lmmap = (MMap *)(multibootheader + 1);
memmove(lmmap, mmap, sizeof mmap);
multibootheader->cmdline = PADDR(BOOTLINE);
multibootheader->flags |= Fcmdline;
if(nmmap != 0){
multibootheader->mmapaddr = PADDR(lmmap);
multibootheader->mmaplength = nmmap*sizeof(MMap);
multibootheader->flags |= Fmmap;
}
multibootheader = (Mbi *)PADDR(multibootheader);
if(v_flag)
print("PADDR(&multibootheader) %#p\n", multibootheader);
}