enum {
Mhz = 1000*1000,
};
#define DUARTFREQ 3672000
enum
{
ILmin = 2,
ILpci = 2,
ILehci = 3,
ILenet1 = 4,
ILenet0 = 5,
ILduart0 = 6,
ILclock = 7,
ILmax = 7,
ILshift = 8,
};
#define Rstblockbase (ulong *)KSEG1ADDR(0x18060000)
#define Rstwdogctl (ulong *)KSEG1ADDR(0x18060008)
#define Wdoglast (1 << 31)
#define Wdogmask 3
#define Wdognoaction 0
#define Wdoggpintr 1
#define Wdognmi 2
#define Wdogreset 3
#define Rstwdogtimer (ulong *)KSEG1ADDR(0x1806000c)
#define Apbintrsts (ulong *)KSEG1ADDR(0x18060010)
#define Apbintrmask (ulong *)KSEG1ADDR(0x18060014)
#define Apbintrtimer 0
#define Apbintrerror 1
#define Apbintrgpio 2
#define Apbintruart 3
#define Apbintrwatchdog 4
#define Apbintrperf 5
#define Apbintrohci 6
#define Apbintrdma 7
#define Pciintrsts (ulong *)KSEG1ADDR(0x18060018)
#define Pciintrmask (ulong *)KSEG1ADDR(0x1806001C)
#define PCI_INTR_CORE (1 << 4)
#define Reset (ulong *)KSEG1ADDR(0x18060024)
#define Rstfullchip (1 << 24)
#define Rstcpucold (1 << 20)
#define Rstge1mac (1 << 13)
#define Rstge1phy (1 << 12)
#define Rstge0mac (1 << 9)
#define Rstge0phy (1 << 8)
#define Rstusbohcidll (1 << 6)
#define Rstusbhost (1 << 5)
#define Rstusbphy (1 << 4)
#define Rstpcibus (1 << 1)
#define Rstpcicore (1 << 0)
typedef struct Pcisiz Pcisiz;
typedef struct Pcidev Pcidev;
typedef struct Vctl Vctl;
struct Vctl {
Vctl* next;
char name[KNAMELEN];
int isintr;
int irq;
int tbdf;
int (*isr)(int);
int (*eoi)(int);
void (*f)(Ureg*, void*);
void* a;
};
enum {
BusCBUS = 0,
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
};
#define MKBUS(t,b,d,f) (((t)<<24)|(((b)&0xFF)<<16)|(((d)&0x1F)<<11)|(((f)&0x07)<<8))
#define BUSFNO(tbdf) (((tbdf)>>8)&0x07)
#define BUSDNO(tbdf) (((tbdf)>>11)&0x1F)
#define BUSBNO(tbdf) (((tbdf)>>16)&0xFF)
#define BUSTYPE(tbdf) ((tbdf)>>24)
#define BUSBDF(tbdf) ((tbdf)&0x00FFFF00)
#define BUSUNKNOWN (-1)
enum {
MaxEISA = 16,
CfgEISA = 0xC80,
};
enum {
PciVID = 0x00,
PciDID = 0x02,
PciPCR = 0x04,
PciPSR = 0x06,
PciRID = 0x08,
PciCCRp = 0x09,
PciCCRu = 0x0A,
PciCCRb = 0x0B,
PciCLS = 0x0C,
PciLTR = 0x0D,
PciHDT = 0x0E,
PciBST = 0x0F,
PciBAR0 = 0x10,
PciBAR1 = 0x14,
PciINTL = 0x3C,
PciINTP = 0x3D,
};
enum {
Pcibcpci1 = 0,
Pcibcstore = 1,
Pcibcnet = 2,
Pcibcdisp = 3,
Pcibcmmedia = 4,
Pcibcmem = 5,
Pcibcbridge = 6,
Pcibccomm = 7,
Pcibcbasesys = 8,
Pcibcinput = 9,
Pcibcdock = 0xa,
Pcibcproc = 0xb,
Pcibcserial = 0xc,
Pcibcwireless = 0xd,
Pcibcintell = 0xe,
Pcibcsatcom = 0xf,
Pcibccrypto = 0x10,
Pcibcdacq = 0x11,
};
enum {
Pciscscsi = 0,
Pciscide = 1,
Pciscsata = 6,
Pciscether = 0,
Pciscvga = 0,
Pciscxga = 1,
Pcisc3d = 2,
Pcischostpci = 0,
Pciscpcicpci = 1,
Pciscserial = 0,
Pciscmultiser = 1,
Pciscusb = 3,
};
enum {
PciCIS = 0x28,
PciSVID = 0x2C,
PciSID = 0x2E,
PciEBAR0 = 0x30,
PciMGNT = 0x3E,
PciMLT = 0x3F,
};
enum {
PciPBN = 0x18,
PciSBN = 0x19,
PciUBN = 0x1A,
PciSLTR = 0x1B,
PciIBR = 0x1C,
PciILR = 0x1D,
PciSPSR = 0x1E,
PciMBR = 0x20,
PciMLR = 0x22,
PciPMBR = 0x24,
PciPMLR = 0x26,
PciPUBR = 0x28,
PciPULR = 0x2C,
PciIUBR = 0x30,
PciIULR = 0x32,
PciEBAR1 = 0x28,
PciBCR = 0x3E,
};
enum {
PciCBExCA = 0x10,
PciCBSPSR = 0x16,
PciCBPBN = 0x18,
PciCBSBN = 0x19,
PciCBUBN = 0x1A,
PciCBSLTR = 0x1B,
PciCBMBR0 = 0x1C,
PciCBMLR0 = 0x20,
PciCBMBR1 = 0x24,
PciCBMLR1 = 0x28,
PciCBIBR0 = 0x2C,
PciCBILR0 = 0x30,
PciCBIBR1 = 0x34,
PciCBILR1 = 0x38,
PciCBSVID = 0x40,
PciCBSID = 0x42,
PciCBLMBAR = 0x44,
};
struct Pcisiz
{
Pcidev* dev;
int siz;
int bar;
};
struct Pcidev
{
int tbdf;
ushort vid;
ushort did;
ushort pcr;
uchar rid;
uchar ccrp;
uchar ccru;
uchar ccrb;
uchar cls;
uchar ltr;
struct {
ulong bar;
int size;
} mem[6];
struct {
ulong bar;
int size;
} rom;
uchar intl;
Pcidev* list;
Pcidev* link;
Pcidev* bridge;
struct {
ulong bar;
int size;
} ioa, mema;
int pmrb;
};
enum {
Vatiamd = 0x1002,
Vintel = 0x8086,
Vjmicron= 0x197b,
Vmarvell= 0x1b4b,
Vmyricom= 0x14c1,
};
#define PCIWINDOW 0
#define PCIWADDR(va) (PADDR(va)+PCIWINDOW)
#define ISAWINDOW 0
#define ISAWADDR(va) (PADDR(va)+ISAWINDOW)
enum
{
SMBquick,
SMBsend,
SMBbytewrite,
SMBwordwrite,
SMBrecv,
SMBbyteread,
SMBwordread,
};
typedef struct SMBus SMBus;
struct SMBus {
QLock;
Rendez r;
void *arg;
ulong base;
int busy;
void (*transact)(SMBus*, int, int, int, uchar*);
};
#pragma varargck type "T" int
#pragma varargck type "T" uint