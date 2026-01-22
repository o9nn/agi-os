typedef struct BD BD;
typedef struct Ring Ring;
enum
{
IRQmask= 0xFF,
IRQactivelow= 0<<8,
IRQactivehigh= 1<<8,
IRQrising= 2<<8,
IRQfalling= 4<<8,
IRQmode= IRQactivelow | IRQactivehigh | IRQrising | IRQfalling,
IRQsoft= 1<<11,
IRQ= 0,
};
enum {
IRQwmlc= 31,
IRQwmts= 30,
IRQwmrs= 29,
IRQwmtbu= 28,
IRQwmrbu= 27,
IRQwmtps= 26,
IRQwmrps= 25,
IRQaber= 24,
IRQlmts= 17,
IRQlmrs= 16,
IRQlmtbu= 15,
IRQlmrbu= 14,
IRQlmtps= 13,
IRQlmrps= 12,
IRQums= 11,
IRQule= 10,
IRQurs= 9,
IRQuts= 8,
IRQtm1= 7,
IRQtm0= 6,
IRQext3= 5,
IRQext2= 4,
IRQext1= 3,
IRQext0= 2,
IRQccts= 1,
IRQccrs= 0,
};
#define MKBUS(t,b,d,f) (((t)<<24)|(((b)&0xFF)<<16)|(((d)&0x1F)<<11)|(((f)&0x07)<<8))
#define BUSFNO(tbdf) (((tbdf)>>8)&0x07)
#define BUSDNO(tbdf) (((tbdf)>>11)&0x1F)
#define BUSBNO(tbdf) (((tbdf)>>16)&0xFF)
#define BUSTYPE(tbdf) ((tbdf)>>24)
#define BUSBDF(tbdf) ((tbdf)&0x00FFFF00)
#define BUSUNKNOWN (-1)
enum {
BusIRQ = IRQ,
BusPCI,
MaxBus
};
#define INTRREG ((IntrReg*)PHYSINTR)
typedef struct IntrReg IntrReg;
struct IntrReg {
ulong mc;
ulong en;
ulong st;
ulong pw;
ulong pad0;
ulong pl;
ulong pt;
ulong pu;
ulong pe;
ulong pc;
ulong pbe;
ulong ms;
ulong hpf;
ulong hpi;
};
#define TIMERREG ((TimerReg*)PHYSTIMER)
typedef struct TimerReg TimerReg;
struct TimerReg {
ulong enable;
ulong count1;
ulong count0;
ulong pulse1;
ulong pulse0;
};
#define GPIOREG ((GpioReg*)PHYSGPIO)
typedef struct GpioReg GpioReg;
struct GpioReg {
ulong iopm;
ulong iopc;
ulong iopd;
};
enum {
GPIO_WLAN_act_o= 7,
GPIO_WLAN_100_o= 8,
GPIO_BT_act_o= 9,
GPIO_BT_100_o= 10,
GPIO_status_orange_o= 11,
GPIO_status_green_o= 12,
GPIO_button_i= 15,
GPIO_misc_mask_o= (1<<13)|(1<<14)|(1<<15)|(1<<4)|(1<<5)|(1<<6),
};
void gpioreserve(int);
void gpioconfig(int, ulong);
ulong gpioget(int);
void gpioset(int, int);
void gpiorelease(int);
enum {
Gpio_in= 0<<4,
Gpio_out= 1<<4,
};
struct BD {
ulong ctrl;
ulong size;
ulong addr;
ulong next;
};
enum {
BdBusy= 1<<31,
RxFS= 1<<30,
RxLS= 1<<29,
RxIPE= 1<<28,
RxTCPE= 1<<27,
RxUDPE= 1<<26,
RxES= 1<<25,
RxMF= 1<<24,
RxRE= 1<<19,
RxTL= 1<<18,
RxRF= 1<<17,
RxCE= 1<<16,
RxFT= 1<<15,
RxFL= 0x7FF,
BdWrap= 1<<25,
TxIC= 1<<31,
TxFS= 1<<30,
TxLS= 1<<29,
TxIPG= 1<<28,
TxTCPG= 1<<27,
TxUDPG= 1<<26,
};
BD* bdalloc(ulong);
void bdfree(BD*, int);
void dumpbd(char*, BD*, int);
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
int ioringinit(Ring*, int, int);
enum {
DmaOut= 0,
DmaIn= 1,
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