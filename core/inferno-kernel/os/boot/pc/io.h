enum
{
Bptvec= 3,
Mathemuvec= 7,
Mathovervec= 9,
Matherr1vec= 16,
Faultvec= 14,
Syscallvec= 64,
VectorPIC = 24,
VectorCLOCK = VectorPIC+0,
VectorKBD = VectorPIC+1,
VectorUART1 = VectorPIC+3,
VectorUART0 = VectorPIC+4,
VectorPCMCIA = VectorPIC+5,
VectorFLOPPY = VectorPIC+6,
VectorLPT = VectorPIC+7,
VectorIRQ7 = VectorPIC+7,
VectorAUX = VectorPIC+12,
VectorIRQ13 = VectorPIC+13,
VectorATA0 = VectorPIC+14,
VectorATA1 = VectorPIC+15,
MaxVectorPIC = VectorPIC+15,
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
PciBAR2 = 0x18,
PciBAR3 = 0x1C,
PciBAR4 = 0x20,
PciBAR5 = 0x24,
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
PciCBBCTL = 0x3E,
PciCBSVID = 0x40,
PciCBSID = 0x42,
PciCBLMBAR = 0x44,
};
typedef struct Pcisiz Pcisiz;
struct Pcisiz
{
Pcidev* dev;
int siz;
int bar;
};
typedef struct Pcidev Pcidev;
typedef struct Pcidev {
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
#define PCIWINDOW 0
#define PCIWADDR(va) (PADDR(va)+PCIWINDOW)
#define ISAWINDOW 0
#define ISAWADDR(va) (PADDR(va)+ISAWINDOW)
typedef struct PCMslot PCMslot;
typedef struct PCMconftab PCMconftab;
struct PCMmap {
ulong ca;
ulong cea;
ulong isa;
int len;
int attr;
int ref;
};
struct PCMconftab
{
int index;
ushort irqs;
uchar irqtype;
uchar bit16;
struct {
ulong start;
ulong len;
} io[16];
int nio;
uchar vpp1;
uchar vpp2;
uchar memwait;
ulong maxwait;
ulong readywait;
ulong otherwait;
};
struct PCMslot
{
Lock;
int ref;
void *cp;
long memlen;
uchar base;
uchar slotno;
uchar special;
uchar already;
uchar occupied;
uchar battery;
uchar wrprot;
uchar powered;
uchar configed;
uchar enabled;
uchar busy;
ulong msec;
char verstr[512];
int ncfg;
struct {
ushort cpresent;
ulong caddr;
} cfg[8];
int nctab;
PCMconftab ctab[8];
PCMconftab *def;
Lock mlock;
int time;
PCMmap mmap[4];
};