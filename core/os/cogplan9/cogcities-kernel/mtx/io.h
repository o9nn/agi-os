enum {
IrqCLOCK = 0,
IrqKBD = 1,
IrqUART1 = 3,
IrqUART0 = 4,
IrqPCMCIA = 5,
IrqFLOPPY = 6,
IrqLPT = 7,
IrqIRQ7 = 7,
IrqAUX = 12,
IrqIRQ13 = 13,
IrqATA0 = 14,
IrqATA1 = 15,
MaxIrqPIC = 15,
VectorPIC = 32,
MaxVectorPIC = VectorPIC+MaxIrqPIC,
};
typedef struct Vctl {
Vctl* next;
char name[KNAMELEN];
int isintr;
int irq;
int tbdf;
int (*isr)(int);
int (*eoi)(int);
void (*f)(Ureg*, void*);
void* a;
} Vctl;
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
#define BUSDF(tbdf) ((tbdf)&0x000FF00)
#define BUSBDF(tbdf) ((tbdf)&0x0FFFF00)
#define BUSUNKNOWN (-1)
enum {
MaxEISA = 16,
EISAconfig = 0xC80,
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
uchar rid;
uchar ccrp;
uchar ccru;
uchar ccrb;
struct {
ulong bar;
int size;
} mem[6];
uchar intl;
Pcidev* list;
Pcidev* link;
Pcidev* bridge;
struct {
ulong bar;
int size;
} ioa, mema;
ulong pcr;
};
#define PCIWINDOW 0x80000000
#define PCIWADDR(va) (PADDR(va)+PCIWINDOW)