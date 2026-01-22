#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "archpaq.h"
#define	MB	(1024*1024)
enum {
DRAMSIZE = 8*MB,
FLASHSIZE = 8*MB,
UPMSIZE = 64,
SPEED = 50,
WriteRAM = 0<<30,
ReadRAM = 1<<30,
ExecRAM = 2<<30,
SelUPMA = 0<<23,
SelUPMB = 1<<23,
Once = 1<<8,
};
static ulong upmb50[UPMSIZE] = {
0x8FFFEC24,	0xFFFEC04,	0xCFFEC04,	0xFFEC04,
0xFFEC00,	0x37FFEC47,	0xFFFFFFFF,	0xFFFFFFFF,
0x8FFFEC24,	0xFFFEC04,	0x8FFEC04,	0xFFEC0C,
0x3FFEC00,	0xFFEC44,	0xFFCC08,	0xCFFCC44,
0xFFEC0C,	0x3FFEC00,	0xFFEC44,	0xFFCC00,
0x3FFFC847,	0x3FFFEC47,	0xFFFFFFFF,	0xFFFFFFFF,
0x8FAFCC24,	0xFAFCC04,	0xCAFCC00,	0x11BFCC47,
0xC0FFCC84,	0xFFFFFFFF,	0xFFFFFFFF,	0xFFFFFFFF,
0x8FAFCC24,	0xFAFCC04,	0xCAFCC00,	0x3AFCC4C,
0xCAFCC00,	0x3AFCC4C,	0xCAFCC00,	0x3AFCC4C,
0xCAFCC00,	0x33BFCC4F,	0xFFFFFFFF,	0xFFFFFFFF,
0xFFFFFFFF,	0xFFFFFFFF,	0xFFFFFFFF,	0xFFFFFFFF,
0xC0FFCC84,	0xFFCC04,	0x7FFCC04,	0x3FFFCC06,
0xFFFFCC85,	0xFFFFCC05,	0xFFFFCC05,	0xFFFFFFFF,
0xFFFFFFFF,	0xFFFFFFFF,	0xFFFFFFFF,	0xFFFFFFFF,
0x33FFCC07,	0xFFFFFFFF,	0xFFFFFFFF,	0xFFFFFFFF,
};
void
sysinit0(int inrom)
{
ulong *upm;
IMM *io;
int i;
io = (IMM*)INTMEM;
io->siumcr = 0x01012440;
io->sypcr = 0xFFFFFF88;
io->tbscrk = KEEP_ALIVE_KEY;
io->tbscr = 0xC3;
io->rtcsck = KEEP_ALIVE_KEY;
io->rtcsc = 0xC1;
io->rtcsck = ~KEEP_ALIVE_KEY;
io->piscrk = KEEP_ALIVE_KEY;
io->piscr = 0x82;
io->memc[BOOTCS].base = FLASHMEM | 1;
io->memc[BOOTCS].option = ~(FLASHSIZE-1)|(1<<8)|(2<<4);
if(!inrom)
return;
io->mptpr = 0x400;
io->mbmr = (0xC0<<24) | 0xA21114;
upm = upmb50;
for(i=0; i<UPMSIZE; i++){
io->mdr = upm[i];
io->mcr = WriteRAM | SelUPMB | i;
}
io->memc[DRAM1].option = ~(DRAMSIZE-1)|0x0800;
io->memc[DRAM1].base = 0 | 0xC1;
}