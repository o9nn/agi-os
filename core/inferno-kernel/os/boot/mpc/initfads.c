#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "archfads.h"
#define MB (1024*1024)
enum {
UPMSIZE = 64,
SPEED = 50,
SDRAMSIZE = 4*MB,
WriteRAM = 0<<30,
ReadRAM = 1<<30,
ExecRAM = 2<<30,
SelUPMA = 0<<23,
SelUPMB = 1<<23,
Once = 1<<8,
};
static ulong upma50[UPMSIZE] = {
0x8FFFEC24, 0xFFFEC04, 0xCFFEC04, 0xFFEC04,
0xFFEC00, 0x37FFEC47, 0xFFFFFFFF, 0xFFFFFFFF,
0x8FFFEC24, 0xFFFEC04, 0x8FFEC04, 0xFFEC0C,
0x3FFEC00, 0xFFEC44, 0xFFCC08, 0xCFFCC44,
0xFFEC0C, 0x3FFEC00, 0xFFEC44, 0xFFCC00,
0x3FFFC847, 0x3FFFEC47, 0xFFFFFFFF, 0xFFFFFFFF,
0x8FAFCC24, 0xFAFCC04, 0xCAFCC00, 0x11BFCC47,
0xC0FFCC84, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
0x8FAFCC24, 0xFAFCC04, 0xCAFCC00, 0x3AFCC4C,
0xCAFCC00, 0x3AFCC4C, 0xCAFCC00, 0x3AFCC4C,
0xCAFCC00, 0x33BFCC4F, 0xFFFFFFFF, 0xFFFFFFFF,
0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
0xC0FFCC84, 0xFFCC04, 0x7FFCC04, 0x3FFFCC06,
0xFFFFCC85, 0xFFFFCC05, 0xFFFFCC05, 0xFFFFFFFF,
0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
0x33FFCC07, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
};
static ulong upma20[UPMSIZE] = {
0x8FFFCC04, 0x08FFCC00, 0x33FFCC47, ~0, ~0, ~0, ~0, ~0,
[0x08] 0x8FFFCC04, 0x08FFCC08, 0x08FFCC08, 0x08FFCC08, 0x08FFCC00, 0x3FFFCC47, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
[0x18] 0x8FEFCC00, 0x39BFCC47, ~0, ~0, ~0, ~0, ~0, ~0,
[0x20] 0x8FEFCC00, 0x09AFCC48, 0x09AFCC48, 0x08AFCC48, 0x39BFCC47, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
[0x30] 0x80FFCC84, 0x17FFCC04, 0xFFFFCC86, 0xFFFFCC05, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
[0x3C] 0x33FFCC07, ~0, ~0, ~0,
};
void
sysinit0(int inrom)
{
ulong *upm, *bcsr;
IMM *io;
int i, mb;
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
io->memc[BCSRCS].option = 0xFFFF8110;
io->memc[BCSRCS].base = BCSRMEM | 1;
io->memc[BOOTCS].base = FLASHMEM | 1;
io->memc[BOOTCS].option = 0xFF800D54;
if(!inrom)
return;
bcsr = (ulong*)BCSRMEM;
switch((bcsr[2]>>23)&3){
default: return;
case 0: mb = 4; break;
case 1: mb = 32; break;
case 2: mb = 16; break;
case 3: mb = 8; break;
}
upm = upma50;
for(i=0; i<UPMSIZE; i++){
io->mdr = upm[i];
io->mcr = WriteRAM | SelUPMA | i;
}
io->mptpr = 0x0400;
if(SPEED >= 32)
io->mamr = (0x9C<<24) | 0xA21114;
else if(SPEED >= 20)
io->mamr = (0x60<<24) | 0xA21114;
else
io->mamr = (0x40<<24) | 0xA21114;
io->memc[DRAM1].option = ~((mb<<20)-1)|0x0800;
io->memc[DRAM1].base = 0 | 0x81;
}
static ulong upmb50[UPMSIZE] = {
[0x00] 0x1F07FC04, 0xEEAEFC04, 0x11ADFC04, 0xEFBBBC00, 0x1FF77C47,
[0x05] 0x1FF77C34, 0xEFEABC34, 0x1FB57C35,
[0x08] 0x1F07FC04, 0xEEAEFC04, 0x10ADFC04, 0xF0AFFC00, 0xF0AFFC00, 0xF1AFFC00, 0xEFBBBC00, 0x1FF77C47, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
[0x18] 0x1F27FC04, 0xEEAEBC00, 0x01B93C04, 0x1FF77C47, ~0, ~0, ~0, ~0,
[0x20] 0x1F07FC04, 0xEEAEBC00, 0x10AD7C00, 0xF0AFFC00, 0xF0AFFC00, 0xE1BBBC04, 0x1FF77C47, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
[0x30] 0x1FF5FC84, 0xFFFFFC04, 0xFFFFFC04, 0xFFFFFC04, 0xFFFFFC84, 0xFFFFFC07, ~0, ~0, ~0, ~0, ~0, ~0,
[0x3C] 0x7FFFFC07, ~0, ~0, ~0,
};
static ulong upmb32[UPMSIZE] = {
[0x00] 0x126CC04, 0xFB98C00, 0x1FF74C45, ~0, ~0,
[0x05] 0x1FE77C34, 0xEFAABC34, 0x1FA57C35,
[0x08] 0x0026FC04, 0x10ADFC00, 0xF0AFFC00, 0xF1AFFC00, 0xEFBBBC00, 0x1FF77C45, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
[0x18] 0x0E26BC04, 0x01B93C00, 0x1FF77C45, ~0, ~0, ~0, ~0, ~0,
[0x20] 0x0E26BC00, 0x10AD7C00, 0xF0AFFC00, 0xF0AFFC00, 0xE1BBBC04, 0x1FF77C45, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
[0x30] 0x1FF5FC84, 0xFFFFFC04, 0xFFFFFC84, 0xFFFFFC05, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
[0x3C] 0x7FFFFC07, ~0, ~0, ~0,
};
void
sdraminit(ulong base)
{
ulong *upm;
IMM *io;
int i;
io = (IMM*)INTMEM;
if(SPEED > 32)
upm = upmb50;
else
upm = upmb32;
for(i=0; i<UPMSIZE; i++){
io->mdr = upm[i];
io->mcr = WriteRAM | SelUPMB | i;
}
io->memc[SDRAM].option = ~(SDRAMSIZE-1)|0x0A00;
io->memc[SDRAM].base = base | 0xC1;
if(SPEED > 32){
io->mbmr = 0xD0802114;
io->mar = 0x88;
}else{
io->mbmr = 0x80802114;
io->mar = 0x48;
}
io->mcr = ExecRAM | SelUPMB | (SDRAM<<13) | Once | 5;
io->mbmr = (io->mbmr & ~0xF) | 8;
io->mcr = ExecRAM | SelUPMB | (SDRAM<<13) | Once | 0x30;
io->mbmr = (io->mbmr & ~0xF) | 4;
}