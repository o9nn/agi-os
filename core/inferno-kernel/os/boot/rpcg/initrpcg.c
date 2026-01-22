#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include	"archrpcg.h"
#define	MB	(1024*1024)
enum {
UPMSIZE = 64,
DRAMSIZE = 16*MB,
FLASHSIZE = 4*MB,
WriteRAM = 0<<30,
ReadRAM = 1<<30,
ExecRAM = 2<<30,
SelUPMA = 0<<23,
SelUPMB = 1<<23,
};
static	ulong	upma50[UPMSIZE] = {
0xCFFFCC24,	0x0FFFCC04,	0x0CAFCC04,	0x03AFCC08,
0x3FBFCC27,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xCFFFCC24,	0x0FFFCC04,	0x0CAFCC84,	0x03AFCC88,
0x3FBFCC27,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xCFFFCC24,	0x0FFFCC04,	0x0CFFCC04,	0x03FFCC00,
0x3FFFCC27,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xCFFFCC24,	0x0FFFCC04,	0x0CFFCC84,	0x03FFCC84,
0x0CFFCC00,	0x33FFCC27,	0xFFFFCC25,	0xFFFFCC25,
0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xC0FFCC24,	0x03FFCC24,	0x0FFFCC24,	0x0FFFCC24,
0x3FFFCC27,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,	0xFFFFCC25,
};
void
sysinit0(int inrom)
{
ulong *upm;
IMM *io;
int i;
io = (IMM*)INTMEM;
io->tbscrk = KEEP_ALIVE_KEY;
io->tbscr = 0xC3;
io->rtcsck = KEEP_ALIVE_KEY;
io->rtcsc = 0xC1;
io->rtcsck = ~KEEP_ALIVE_KEY;
io->piscrk = KEEP_ALIVE_KEY;
io->piscr = 0x82;
return;
io->memc[BCSRCS].option = 0xFFFF8910;
io->memc[BCSRCS].base = BCSRMEM | 1;
if(1||!inrom)
return;
upm = upma50;
for(i=0; i<nelem(upma50); i++){
io->mdr = upm[i];
io->mcr = WriteRAM | SelUPMA | i;
}
io->mptpr = 0x0800;
io->mamr = (0x58<<24) | 0xA01430;
io->memc[DRAM1].option = ~(DRAMSIZE-1)|0x0E00;
io->memc[DRAM1].base = 0 | 0x81;
}