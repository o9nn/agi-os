enum {
BusCBUS		= 0,
BusCBUSII,
BusEISA,
BusFUTURE,
BusINTERN,
BusISA,
BusMBI,
BusMBII,
BusMCA,
BusMPI,
BusMPSA,
BusNUBUS,
BusPCI,
BusPCMCIA,
BusTC,
BusVL,
BusVME,
BusXPRESS,
BUSUNKNOWN = -1
};
#define MKBUS(t,b,d,f)	(((t)<<24)|(((b)&0xFF)<<16)|(((d)&0x1F)<<11)|(((f)&0x07)<<8))
#define BUSFNO(tbdf)	(((tbdf)>>8)&0x07)
#define BUSDNO(tbdf)	(((tbdf)>>11)&0x1F)
#define BUSBNO(tbdf)	(((tbdf)>>16)&0xFF)
#define BUSTYPE(tbdf)	((tbdf)>>24)
#define BUSBDF(tbdf)	((tbdf)&0x00FFFF00)
enum {
PciVID		= 0x00,
PciDID		= 0x02,
PciPCR		= 0x04,
PciPSR		= 0x06,
PciRID		= 0x08,
PciCCRp		= 0x09,
PciCCRu		= 0x0A,
PciCCRb		= 0x0B,
PciCLS		= 0x0C,
PciLTR		= 0x0D,
PciHDT		= 0x0E,
PciBST		= 0x0F,
};
enum {
Pcibcpci1	= 0,
Pcibcstore	= 1,
Pcibcnet	= 2,
Pcibcdisp	= 3,
Pcibcmmedia	= 4,
Pcibcmem	= 5,
Pcibcbridge	= 6,
Pcibccomm	= 7,
Pcibcbasesys	= 8,
Pcibcinput	= 9,
Pcibcdock	= 0xa,
Pcibcproc	= 0xb,
Pcibcserial	= 0xc,
Pcibcwireless	= 0xd,
Pcibcintell	= 0xe,
Pcibcsatcom	= 0xf,
Pcibccrypto	= 0x10,
Pcibcdacq	= 0x11,
};
enum {
Pciscscsi	= 0,
Pciscide	= 1,
Pciscether	= 0,
Pciscvga	= 0,
Pciscxga	= 1,
Pcisc3d		= 2,
Pcischostpci	= 0,
Pciscpcicpci	= 1,
Pciscserial	= 0,
Pciscmultiser	= 1,
Pciscusb	= 3,
};
enum {
PciCIS		= 0x28,
PciSVID		= 0x2C,
PciSID		= 0x2E,
PciEBAR0	= 0x30,
PciMGNT		= 0x3E,
PciMLT		= 0x3F,
};
enum {
PciPBN		= 0x18,
PciSBN		= 0x19,
PciUBN		= 0x1A,
PciSLTR		= 0x1B,
PciIBR		= 0x1C,
PciILR		= 0x1D,
PciSPSR		= 0x1E,
PciMBR		= 0x20,
PciMLR		= 0x22,
PciPMBR		= 0x24,
PciPMLR		= 0x26,
PciPUBR		= 0x28,
PciPULR		= 0x2C,
PciIUBR		= 0x30,
PciIULR		= 0x32,
PciEBAR1	= 0x28,
PciBCR		= 0x3E,
};
enum {
PciCBExCA	= 0x10,
PciCBSPSR	= 0x16,
PciCBPBN	= 0x18,
PciCBSBN	= 0x19,
PciCBUBN	= 0x1A,
PciCBSLTR	= 0x1B,
PciCBMBR0	= 0x1C,
PciCBMLR0	= 0x20,
PciCBMBR1	= 0x24,
PciCBMLR1	= 0x28,
PciCBIBR0	= 0x2C,
PciCBILR0	= 0x30,
PciCBIBR1	= 0x34,
PciCBILR1	= 0x38,
PciCBSVID	= 0x40,
PciCBSID	= 0x42,
PciCBLMBAR	= 0x44,
};
typedef struct Pcisiz Pcisiz;
struct Pcisiz
{
Pcidev*	dev;
int	siz;
int	bar;
};
typedef struct Pcidev Pcidev;
struct Pcidev
{
int	tbdf;
ushort	vid;
ushort	did;
ushort	pcr;
uchar	rid;
uchar	ccrp;
uchar	ccru;
uchar	ccrb;
uchar	cls;
uchar	ltr;
struct {
ulong	bar;
int	size;
} mem[6];
struct {
ulong	bar;
int	size;
} rom;
uchar	intl;
Pcidev*	list;
Pcidev*	link;
Pcidev*	bridge;
struct {
ulong	bar;
int	size;
} ioa, mema;
int	pmrb;
};
#define PCIWINDOW	0
#define PCIWADDR(va)	(PADDR(va)+PCIWINDOW)
enum {
AddrEfuse	= PHYSIO+0x1008c,
Addrpci		= PHYSIO+0x40000,
Addrpcibase	= PHYSIO+0x41800,
AddrMpp		= PHYSIO+0x10000,
AddrSdio	= PHYSIO+0x90000,
};
enum {
Socrevz0,
Socreva0 = 2,
Socreva1,
};
enum {
PciBAR0		= Addrpcibase + 4,
PciBAR1		= Addrpcibase + 8,
PciCP		= Addrpci + 0x64,
PciINTL		= Addrpci + 0x3c,
PciINTP		= PciINTL + 1,
};
enum {
Irqlo, Irqhi, Irqbridge,
};
enum {
IRQ0hisum,
IRQ0bridge,
IRQ0h2cdoorbell,
IRQ0c2hdoorbell,
_IRQ0reserved0,
IRQ0xor0chan0,
IRQ0xor0chan1,
IRQ0xor1chan0,
IRQ0xor1chan1,
IRQ0pex0int,
_IRQ0reserved1,
IRQ0gbe0sum,
IRQ0gbe0rx,
IRQ0gbe0tx,
IRQ0gbe0misc,
IRQ0gbe1sum,
IRQ0gbe1rx,
IRQ0gbe1tx,
IRQ0gbe1misc,
IRQ0usb0,
_IRQ0reserved2,
IRQ0sata,
IRQ0crypto,
IRQ0spi,
IRQ0audio,
_IRQ0reserved3,
IRQ0ts0,
_IRQ0reserved4,
IRQ0sdio,
IRQ0twsi,
IRQ0avb,
IRQ0tdm,
_IRQ1reserved0 = 0,
IRQ1uart0,
IRQ1uart1,
IRQ1gpiolo0,
IRQ1gpiolo1,
IRQ1gpiolo2,
IRQ1gpiolo3,
IRQ1gpiohi0,
IRQ1gpiohi1,
IRQ1gpiohi2,
IRQ1gpiohi3,
IRQ1xor0err,
IRQ1xor1err,
IRQ1pex0err,
_IRQ1reserved1,
IRQ1gbe0err,
IRQ1gbe1err,
IRQ1usberr,
IRQ1cryptoerr,
IRQ1audioerr,
_IRQ1reserved2,
_IRQ1reserved3,
IRQ1rtc,
IRQcpuself = 0,
IRQcputimer0,
IRQcputimer1,
IRQcputimerwd,
IRQaccesserr,
};
typedef struct IntrReg IntrReg;
struct IntrReg
{
struct {
ulong	irq;
ulong	irqmask;
ulong	fiqmask;
ulong	epmask;
} lo, hi;
};
typedef struct CpucsReg CpucsReg;
struct CpucsReg
{
ulong	cpucfg;
ulong	cpucsr;
ulong	rstout;
ulong	softreset;
ulong	irq;
ulong	irqmask;
ulong	mempm;
ulong	clockgate;
ulong	biu;
ulong	pad0;
ulong	l2cfg;
ulong	pad1[2];
ulong	l2tm0;
ulong	l2tm1;
ulong	pad2[2];
ulong	l2pm;
ulong	ram0;
ulong	ram1;
ulong	ram2;
ulong	ram3;
};
enum {
Cfgvecinithi	= 1<<1,
Cfgbigendreset	= 3<<1,
Cfgiprefetch	= 1<<16,
Cfgdprefetch	= 1<<17,
Reset		= 1<<1,
RstoutPex	= 1<<0,
RstoutWatchdog	= 1<<1,
RstoutSoft	= 1<<2,
ResetSystem	= 1<<0,
L2ecc		= 1<<2,
L2exists	= 1<<3,
L2writethru	= 1<<4,
};
enum {
Targdram	= 0,
Targflash	= 1,
Targcesasram	= 3,
Attrcs0		= 0xe,
Attrcs1		= 0xd,
Attrbootrom	= 0x1d,
Attrspi		= 0x1e,
Attrnand	= 0x2f,
Winenable	= 1<<0,
};
typedef struct Pciex Pciex;
struct Pciex {
ushort	venid;
ushort	devid;
ulong	csr;
ulong	revid;
ulong	bistcache;
ulong	bar0;
ulong	bar0hi;
ulong	bar1;
ulong	bar1hi;
ulong	bar2;
ulong	bar2hi;
ulong	_pad0;
ushort	ssvenid;
ushort	ssdevid;
ulong	rombar;
ulong	caplist;
ulong	_pad1;
ulong	intrpinline;
ulong	pmcap;
ulong	pmcsr;
ulong	_pad2[2];
ulong	msictl;
ulong	msiaddr;
ulong	msiaddrhi;
ulong	msidata;
ulong	cap;
ulong	devcap;
ulong	devcsr;
ulong	linkcap;
ulong	linkcsr;
uchar	_pad[0x40100-0x40074];
ulong	errrep;
ulong	uncorrerr;
ulong	uncorrerrmask;
ulong	uncorrerrsev;
ulong	correrr;
ulong	correrrmask;
ulong	errcap;
ulong	hdrlog[4];
};