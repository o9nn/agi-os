typedef struct BD BD;
typedef struct Ring Ring;
typedef struct MALdev MALdev;
typedef struct I2Cdev I2Cdev;
enum
{
VectorUIC= 0,
VectorUART0=VectorUIC,
VectorUART1,
VectorIIC,
VectorPCIECW,
VectorRsvd1,
VectorDMA0,
VectorDMA1,
VectorDMA2,
VectorDMA3,
VectorEtherwake,
VectorMALSERR,
VectorMALTXEOB,
VectorMALRXEOB,
VectorMALTXDE,
VectorMALRXDE,
VectorEMAC0,
VectorPCISERR,
VectorEMAC1,
VectorPCIPM,
VectorGPT0,
VectorGPT1,
VectorGPT2,
VectorGPT3,
VectorGPT4,
VectorIRQ= VectorUIC+25,
MaxVector= VectorIRQ+7,
IRQmask= 0xFF,
IRQactivelow= 1<<8,
IRQedge= 1<<9,
IRQcritical= 1<<10,
};
#define MKBUS(t,b,d,f) (((t)<<24)|(((b)&0xFF)<<16)|(((d)&0x1F)<<11)|(((f)&0x07)<<8))
#define BUSFNO(tbdf) (((tbdf)>>8)&0x07)
#define BUSDNO(tbdf) (((tbdf)>>11)&0x1F)
#define BUSBNO(tbdf) (((tbdf)>>16)&0xFF)
#define BUSTYPE(tbdf) ((tbdf)>>24)
#define BUSBDF(tbdf) ((tbdf)&0x00FFFF00)
#define BUSUNKNOWN (-1)
enum {
BusOPB,
BusPLB,
BusPCI,
MaxBus
};
struct BD {
ushort status;
ushort length;
ulong addr;
};
#define MAXIORING 256
#define BDBUFLIM (4096-16)
BD* bdalloc(ulong);
void bdfree(BD*, int);
void dumpbd(char*, BD*, int);
enum {
BDEmpty= 1<<15,
BDWrap= 1<<14,
BDContin= 1<<13,
BDLast= 1<<12,
BDFirst= 1<<11,
BDInt= 1<<10,
BDReady= 1<<15,
};
struct Ring {
BD* rdr;
Block** rxb;
int rdrx;
int nrdre;
BD* tdr;
Block** txb;
int tdrh;
int tdri;
int ntdre;
int ntq;
};
#define NEXT(x, l) (((x)+1)%(l))
#define PREV(x, l) (((x) == 0) ? (l)-1: (x)-1)
#define HOWMANY(x, y) (((x)+((y)-1))/(y))
#define ROUNDUP(x, y) (HOWMANY((x), (y))*(y))
typedef struct Mal Mal;
struct Mal {
int n;
int len;
int tx;
ulong mask;
void* arg;
void (*interrupt)(Ureg*, void*);
};
Mal* malchannel(int, int, void (*)(Ureg*, void*), void*);
void maltxreset(Mal*);
void maltxinit(Mal*, Ring*);
void maltxenable(Mal*);
void malrxreset(Mal*);
void malrxinit(Mal*, Ring*, ulong);
void malrxenable(Mal*);
void ioringreserve(int, ulong, int, ulong);
int ioringinit(Ring*, int, int);
typedef struct Gpioregs Gpioregs;
struct Gpioregs {
ulong or;
ulong tcr;
ulong osrh;
ulong osrl;
ulong tsrh;
ulong tsrl;
ulong odr;
ulong ir;
ulong rr1;
ulong pad[3];
ulong isr1h;
ulong isr1l;
};
enum {
Gpio_Alt1= 1<<0,
Gpio_OD= 1<<1,
Gpio_Tri= 1<<2,
Gpio_in= 1<<4,
Gpio_out= 1<<5,
};
void gpioreserve(ulong);
void gpioconfig(ulong, ulong);
ulong gpioget(ulong);
void gpioset(ulong, ulong);
void gpiorelease(ulong);
struct I2Cdev {
int addr;
int salen;
int tenbit;
};
long i2crecv(I2Cdev*, void*, long, ulong);
long i2csend(I2Cdev*, void*, long, ulong);
void i2csetup(int);
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
struct Pcidev
{
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
ulong pcr;
};
#define PCIWINDOW 0x80000000
#define PCIWADDR(va) (PADDR(va)+PCIWINDOW)