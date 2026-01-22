#pragma varargck type "T" int
#pragma varargck type "T" uint
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
enum {
Barioaddr = 1<<0,
Barwidthshift = 1,
Barwidthmask = MASK(2),
Barwidth32 = 0,
Barwidth64 = 2,
Barprefetch = 1<<3,
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
int pmrb;
};
enum {
Vatiamd = 0x1002,
Vintel = 0x8086,
Vjmicron= 0x197b,
Vmarvell= 0x1b4b,
Vmyricom= 0x14c1,
Vnvidia = 0x10de,
Vrealtek= 0x10ec,
};
#define PCIWINDOW 0
#define PCIWADDR(va) (PADDR(va)+PCIWINDOW)