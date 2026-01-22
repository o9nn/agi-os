typedef struct Ecapio Ecapio;
typedef struct Edbgio Edbgio;
#pragma incomplete Ecapio;
#pragma incomplete Edbgio;
enum
{
Cnports = 0xF,
Cdbgportshift = 20,
Cdbgportmask = 0xF,
C64 = 1<<0,
Cpfl = 1<<1,
Casp = 1<<2,
Ceecpshift = 8,
Ceecpmask = (1<<8) - 1,
Clegacy = 1,
CLbiossem = 2,
CLossem = 3,
CLcontrol = 4,
Lterm = 1,
Litd = 0<<1,
Lqh = 1<<1,
Lsitd = 2<<1,
Lfstn = 3<<1,
Cstop = 0x00000,
Crun = 0x00001,
Chcreset = 0x00002,
Cflsmask = 0x0000C,
Cfls1024 = 0x00000,
Cfls512 = 0x00004,
Cfls256 = 0x00008,
Cpse = 0x00010,
Case = 0x00020,
Ciasync = 0x00040,
Citcshift = 16,
Citcmask = 0xff << Citcshift,
Sasyncss = 0x08000,
Speriodss = 0x04000,
Srecl = 0x02000,
Shalted = 0x01000,
Sasync = 0x00020,
Sherr = 0x00010,
Sfrroll = 0x00008,
Sportchg = 0x00004,
Serrintr = 0x00002,
Sintr = 0x00001,
Sintrs = 0x0003F,
Iusb = 0x01,
Ierr = 0x02,
Iportchg = 0x04,
Ifrroll = 0x08,
Ihcerr = 0x10,
Iasync = 0x20,
Iall = 0x3F,
Callmine = 1,
Pspresent = 0x00000001,
Psstatuschg = 0x00000002,
Psenable = 0x00000004,
Pschange = 0x00000008,
Psresume = 0x00000040,
Pssuspend = 0x00000080,
Psreset = 0x00000100,
Pspower = 0x00001000,
Psowner = 0x00002000,
Pslinemask = 0x00000C00,
Pslow = 0x00000400,
Cowner = 0x40000000,
Cenable = 0x10000000,
Cdone = 0x00010000,
Cbusy = 0x00000400,
Cerrmask= 0x00000380,
Chwerr = 0x00000100,
Cterr = 0x00000080,
Cfailed = 0x00000040,
Cgo = 0x00000020,
Cwrite = 0x00000010,
Clen = 0x0000000F,
Prpidshift = 16,
Prpidmask = 0xFF,
Pspidshift = 8,
Pspidmask = 0xFF,
Ptokshift = 0,
Ptokmask = 0xFF,
Ptoggle = 0x00008800,
Ptogglemask = 0x0000FF00,
Adevshift = 8,
Adevmask = 0x7F,
Aepshift = 0,
Aepmask = 0xF,
};
struct Ecapio
{
ulong cap;
ulong parms;
ulong capparms;
ulong portroute;
};
struct Edbgio
{
ulong csw;
ulong pid;
uchar data[8];
ulong addr;
};