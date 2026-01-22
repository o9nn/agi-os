#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#define Image IMAGE
#include <draw.h>
#include <memdraw.h>
#include <cursor.h>
#include "screen.h"
enum {
PCIS3 = 0x5333,
SAVAGE3D = 0x8A20,
SAVAGE3DMV = 0x8A21,
SAVAGE4 = 0x8A22,
PROSAVAGEP = 0x8A25,
PROSAVAGEK = 0x8A26,
PROSAVAGE8 = 0x8D04,
SAVAGEMXMV = 0x8C10,
SAVAGEMX = 0x8C11,
SAVAGEIXMV = 0x8C12,
SAVAGEIX = 0x8C13,
SUPERSAVAGEIXC16 = 0x8C2E,
SAVAGE2000 = 0x9102,
VIRGE = 0x5631,
VIRGEGX2 = 0x8A10,
VIRGEDXGX = 0x8A01,
VIRGEVX = 0x883D,
VIRGEMX = 0x8C01,
VIRGEMXP = 0x8C03,
AURORA64VPLUS = 0x8812,
};
enum {
SubsystemStatus = 0x8504,
VsyncInt = 1<<0,
GeBusyInt = 1<<1,
BfifoFullInt = 1<<2,
BfifoEmptyInt = 1<<3,
CfifoFullInt = 1<<4,
CfifoEmptyInt = 1<<5,
BciInt = 1<<6,
LpbInt = 1<<7,
CbHiInt = 1<<16,
CbLoInt = 1<<17,
SubsystemCtl = 0x8504,
VsyncClr = 1<<0,
GeBusyClr = 1<<1,
BfifoFullClr = 1<<2,
BfifoEmptyClr = 1<<3,
CfifoFullClr = 1<<4,
CfifoEmptyClr = 1<<5,
BciClr = 1<<6,
LpbClr = 1<<7,
CbHiClr = 1<<16,
CbLoClr = 1<<17,
VsyncEna = 1<<8,
Busy2DEna = 1<<9,
BfifoFullEna = 1<<10,
BfifoEmptyEna = 1<<11,
CfifoFullEna = 1<<12,
CfifoEmptyEna = 1<<13,
SubsysBciEna = 1<<14,
CbHiEna = 1<<24,
CbLoEna = 1<<25,
GeSoftReset = 1<<15,
FifoStatus = 0x8508,
CwbEmpty = 1<<0,
CrbEmpty = 1<<1,
CobEmpty = 1<<2,
CfifoEmpty = 1<<3,
CwbFull = 1<<8,
CrbFull = 1<<9,
CobFull = 1<<10,
CfifoFull = 1<<11,
AdvFunCtl = 0x850C,
GeEna = 1<<0,
BigPixel = 1<<2,
LaEna = 1<<3,
Mclk_2 = 0<<8,
Mclk_4 = 1<<8,
Mclk = 2<<8,
Ic33mhz = 1<<16,
WakeupReg = 0x8510,
WakeupBit = 1<<0,
SourceY = 0x8100,
SourceX = 0x8102,
RectY = 0x8100,
RectX = 0x8102,
DestY = 0x8108,
DestX = 0x810A,
Height = 0x8148,
Width = 0x814A,
StartY = 0x8100,
StartX = 0x8102,
AxialStep = 0x8108,
DiagonalStep = 0x810A,
LineError = 0x8110,
MinorLength = 0x8148,
MajorLength = 0x814A,
DrawCmd = 0x8118,
CmdMagic = 0<<1,
AcrossPlane = 1<<1,
LastPixelOff = 1<<2,
Radial = 1<<3,
DoDraw = 1<<4,
DrawRight = 1<<5,
MajorY = 1<<6,
DrawDown = 1<<7,
Degree0 = 0<<5,
Degree45 = 1<<5,
Degree315 = 7<<5,
UseCPUData = 1<<8,
Bus8 = 0<<9,
Bus16 = 1<<9,
Bus32 = 2<<9,
Bus32AP = 3<<9,
CmdNop = 0<<13,
CmdLine = 1<<13,
CmdFill = 2<<13,
CmdBitblt = 6<<13,
CmdPatblt = 7<<13,
SrcGBD = 0<<16,
SrcPBD = 1<<16,
SrcSBD = 2<<16,
DstGBD = 0<<18,
DstPBD = 1<<18,
DstSBD = 2<<18,
BgColor = 0x8120,
FgColor = 0x8124,
BitplaneWmask = 0x8128,
BitplaneRmask = 0x812C,
CmpColor = 0x8130,
BgMix = 0x8134,
FgMix = 0x8136,
MixNew = 7,
SrcBg = 0<<5,
SrcFg = 1<<5,
SrcCPU = 2<<5,
SrcDisp = 3<<5,
TopScissors = 0x8138,
LeftScissors = 0x813A,
BottomScissors = 0x813C,
RightScissors = 0x813E,
PixCtl = 0x8140,
PixMagic = 0xA<<12,
PixMixFg = 0<<6,
PixMixCPU = 2<<6,
PixMixDisp = 3<<6,
MfMisc2Ctl = 0x8142,
MfMisc2Magic = 0xD<<12,
DstShift = 0,
SrcShift = 4,
WaitFifoEmpty = 2<<8,
MfMiscCtl = 0x8144,
MfMiscMagic = 0xE<<12,
UseHighBits = 1<<4,
ClipInvert = 1<<5,
SkipSame = 0<<6,
SkipDifferent = 1<<7,
CmpEna = 1<<8,
W32Ena = 1<<9,
ClipDis = 1<<11,
GBD1 = 0x8168,
GBD2 = 0x816C,
BDS64 = 1<<0,
GBDBciEna = 1<<3,
BlockWriteDis = 1<<28,
StrideShift = 0,
DepthShift = 16,
PBD1 = 0x8170,
PBD2 = 0x8174,
SBD1 = 0x8178,
SBD2 = 0x817C,
};
enum {
XStatus0 = 0x48C00,
CBEMaskA = 0x1FFFF,
CBEShiftA = 0,
BciIdleA = 1<<17,
Ge3IdleA = 1<<18,
Ge2IdleA = 1<<19,
McpIdleA = 1<<20,
MeIdleA = 1<<22,
PfPendA = 1<<23,
CBEMaskB = 0x1FFFFF,
CBEShiftB = 0,
BciIdleB = 1<<25,
Ge3IdleB = 1<<26,
Ge2IdleB = 1<<27,
McpIdleB = 1<<28,
MeIdleB = 1<<30,
PfPendB = 1<<31,
AltStatus0 = 0x48C60,
CBEMask = 0x1FFFF,
CBEShift = 0,
BciIdle = 1<<21,
Ge3Idle = 1<<22,
Ge2Idle = 1<<23,
McpIdle = 1<<24,
MeIdle = 1<<25,
PfPend = 1<<26,
XStatus1 = 0x48C04,
XStatus2 = 0x48C08,
ScanMask = 0x3FF,
ScanShift = 0,
VRTMask = 0x7F100,
VRTShift = 11,
CbThresh = 0x48C10,
CobOff = 0x48C14,
CobPtr = 0x48C18,
CobEna = 1<<2,
CobBciEna = 1<<3,
CbeMask = 0xFFFF8000,
CbeShift = 15,
AltStatus1 = 0x48C64,
};
struct {
ulong idletimeout;
ulong tostatw[16];
} savagestats;
enum {
Maxloop = 1<<20
};
static void
savagewaitidle(VGAscr *scr)
{
long x;
ulong *statw, mask, goal;
switch(scr->id){
case SAVAGE4:
case PROSAVAGEP:
case PROSAVAGEK:
case PROSAVAGE8:
statw = (ulong*)((uchar*)scr->mmio+AltStatus0);
mask = CBEMask | Ge2Idle;
goal = Ge2Idle;
break;
case SUPERSAVAGEIXC16:
case SAVAGEIXMV:
case SAVAGEMXMV:
statw = (ulong*)((uchar*)scr->mmio+XStatus0);
mask = CBEMaskA | Ge2IdleA;
goal = Ge2IdleA;
break;
default:
return;
}
for(x=0; x<Maxloop; x++)
if((*statw & mask) == goal)
return;
savagestats.tostatw[savagestats.idletimeout++&15] = *statw;
savagestats.tostatw[savagestats.idletimeout++&15] = (ulong)statw;
}
static int
savagefill(VGAscr *scr, Rectangle r, ulong sval)
{
uchar *mmio;
mmio = (uchar*)scr->mmio;
*(ulong*)(mmio+FgColor) = sval;
*(ulong*)(mmio+BgColor) = sval;
*(ulong*)(mmio+BgMix) = SrcFg|MixNew;
*(ulong*)(mmio+FgMix) = SrcFg|MixNew;
*(ushort*)(mmio+RectY) = r.min.y;
*(ushort*)(mmio+RectX) = r.min.x;
*(ushort*)(mmio+Width) = Dx(r)-1;
*(ushort*)(mmio+Height) = Dy(r)-1;
*(ulong*)(mmio+DrawCmd) = CmdMagic | DoDraw | CmdFill | DrawRight | DrawDown;
savagewaitidle(scr);
return 1;
}
static int
savagescroll(VGAscr *scr, Rectangle r, Rectangle sr)
{
uchar *mmio;
ulong cmd;
Point dp, sp;
cmd = CmdMagic | DoDraw | CmdBitblt | SrcPBD | DstGBD;
if(r.min.x <= sr.min.x){
cmd |= DrawRight;
dp.x = r.min.x;
sp.x = sr.min.x;
}else{
dp.x = r.max.x-1;
sp.x = sr.max.x-1;
}
if(r.min.y <= sr.min.y){
cmd |= DrawDown;
dp.y = r.min.y;
sp.y = sr.min.y;
}else{
dp.y = r.max.y-1;
sp.y = sr.max.y-1;
}
mmio = (uchar*)scr->mmio;
*(ushort*)(mmio+SourceX) = sp.x;
*(ushort*)(mmio+SourceY) = sp.y;
*(ushort*)(mmio+DestX) = dp.x;
*(ushort*)(mmio+DestY) = dp.y;
*(ushort*)(mmio+Width) = Dx(r)-1;
*(ushort*)(mmio+Height) = Dy(r)-1;
*(ulong*)(mmio+BgMix) = SrcDisp|MixNew;
*(ulong*)(mmio+FgMix) = SrcDisp|MixNew;
*(ulong*)(mmio+DrawCmd) = cmd;
savagewaitidle(scr);
return 1;
}
static void
savageblank(VGAscr*, int blank)
{
uchar seqD;
vgaxo(Seqx, 8, vgaxi(Seqx,8)|0x06);
seqD = vgaxi(Seqx, 0xD);
seqD &= 0x03;
if(blank)
seqD |= 0x50;
vgaxo(Seqx, 0xD, seqD);
if(blank)
vgaxo(Seqx, 0x31, vgaxi(Seqx, 0x31) & ~0x10);
else
vgaxo(Seqx, 0x31, vgaxi(Seqx, 0x31) | 0x10);
}
void
savageinit(VGAscr *scr)
{
uchar *mmio;
ulong bd;
switch(scr->id){
case SAVAGE4:
case PROSAVAGEP:
case PROSAVAGEK:
case PROSAVAGE8:
case SAVAGEIXMV:
case SUPERSAVAGEIXC16:
case SAVAGEMXMV:
break;
default:
print("unknown savage %.4lux\n", scr->id);
return;
}
mmio = (uchar*)scr->mmio;
if(mmio == nil) {
print("savageinit: no mmio\n");
return;
}
*(ushort*)(mmio+SubsystemCtl) = GeSoftReset;
delay(2);
*(ushort*)(mmio+SubsystemCtl) = 0;
savagewaitidle(scr);
*(ushort*)(mmio+CobPtr) &= ~CobBciEna;
*(ushort*)(mmio+GBD2) &= ~GBDBciEna;
savagewaitidle(scr);
*(ushort*)(mmio+MfMiscCtl) = MfMiscMagic|W32Ena|ClipDis;
savagewaitidle(scr);
*(ulong*)(mmio+BitplaneRmask) = ~0;
*(ulong*)(mmio+BitplaneWmask) = ~0;
savagewaitidle(scr);
*(ulong*)(mmio+AdvFunCtl) |= GeEna|LaEna;
savagewaitidle(scr);
bd = (scr->gscreen->depth<<DepthShift) |
(Dx(scr->gscreen->r)<<StrideShift) | BlockWriteDis
| BDS64;
*(ulong*)(mmio+GBD1) = 0;
*(ulong*)(mmio+GBD2) = bd;
*(ulong*)(mmio+PBD1) = 0;
*(ulong*)(mmio+PBD2) = bd;
*(ulong*)(mmio+SBD1) = 0;
*(ulong*)(mmio+SBD2) = bd;
*(ulong*)(mmio+GBD1) = 0;
*(ulong*)(mmio+GBD2) = bd;
*(ushort*)(mmio+GBD2+2) = bd>>16;
savagewaitidle(scr);
scr->fill = savagefill;
scr->scroll = savagescroll;
scr->blank = savageblank;
hwblank = 0;
}