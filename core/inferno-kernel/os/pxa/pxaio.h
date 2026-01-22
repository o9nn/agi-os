typedef struct I2Cdev I2Cdev;
typedef struct PCMconftab PCMconftab;
typedef struct PCMmap PCMmap;
typedef struct PCMslot PCMslot;
#define INTRREG ((IntrReg*)PHYSINTR)
typedef struct IntrReg IntrReg;
struct IntrReg {
ulong icip;
ulong icmr;
ulong iclr;
ulong icfp;
ulong icpr;
ulong iccr;
};
enum
{
GPIOrising,
GPIOfalling,
GPIOboth,
IRQ,
};
enum {
IRQrtc= 31,
IRQhz= 30,
IRQtimer3= 29,
IRQtimer2= 28,
IRQtimer1= 27,
IRQtimer0= 26,
IRQdma= 25,
IRQssp= 24,
IRQmmc= 23,
IRQffuart= 22,
IRQbtuart= 21,
IRQstuart= 20,
IRQicp= 19,
IRQi2c= 18,
IRQlcd= 17,
IRQnssp= 16,
IRQac97= 14,
IRQi2s= 13,
IRQpmu= 12,
IRQusb= 11,
IRQgpio= 10,
IRQgpio1= 9,
IRQgpio0= 8,
IRQhwuart= 7,
};
#define GPIOREG ((GpioReg*)PHYSGPIO)
typedef struct GpioReg GpioReg;
struct GpioReg {
ulong gplr[3];
ulong gpdr[3];
ulong gpsr[3];
ulong gpcr[3];
ulong grer[3];
ulong gfer[3];
ulong gedr[3];
ulong gafr[6];
};
enum {
GPIO_GP_RST_1_i= 1,
GPIO_FFRXD_1_i= 34,
GPIO_FFTXD_2_o= 39,
MaxGPIObit= 84,
MaxGPIOIRQ= 1,
};
#define GPB(n) (1<<((n)&31))
#define GPR(n) ((n)>>5)
#define GPAF(n,v) ((v)<<(((n)&15)*2))
void gpioreserve(int);
void gpioconfig(int, ulong);
ulong gpioget(int);
void gpioset(int, int);
void gpiorelease(int);
enum {
Gpio_gpio= 0<<0,
Gpio_Alt1= 1<<0,
Gpio_Alt2= 2<<0,
Gpio_Alt3= 3<<0,
Gpio_in= 1<<2,
Gpio_out= 1<<3,
};
struct I2Cdev {
int addr;
int salen;
int tenbit;
};
long i2crecv(I2Cdev*, void*, long, ulong);
long i2csend(I2Cdev*, void*, long, ulong);
void i2csetup(int);
#define COREREG ((Coreregs*)PHYSCORE)
typedef struct Coreregs Coreregs;
struct Coreregs {
ulong cccr;
ulong cken;
ulong oscc;
};
#define RTCREG ((RTCreg*)PHYSRTC)
typedef struct RTCreg RTCreg;
struct RTCreg {
ulong rcnr;
ulong rtar;
ulong rtsr;
ulong rttr;
};
#define OSTMRREG ((OstmrReg*)PHYSOSTMR)
typedef struct OstmrReg OstmrReg;
struct OstmrReg {
ulong osmr[4];
ulong oscr;
ulong ossr;
ulong ower;
ulong oier;
};
#define PMGRREG ((PmgrReg*)PHYSPOWER)
typedef struct PmgrReg PmgrReg;
struct PmgrReg {
ulong pmcr;
ulong pssr;
ulong pspr;
ulong pwer;
ulong prer;
ulong pfer;
ulong pedr;
ulong pcfr;
ulong pgsr[3];
ulong rsvd;
ulong rcsr;
};
enum {
PWER_rtc = 1<<31,
PWER_we0 = 1<<0,
PSSR_sss = 1<<0,
PSSR_bfs = 1<<1,
PSSR_vfs = 1<<2,
PSSR_ph = 1<<4,
PSSR_rdh = 1<<5,
PMFW_fwake= 1<<1,
RSCR_gpr= 1<<3,
RSCR_smr= 1<<2,
RSCR_wdr= 1<<1,
RSCR_hwr= 1<<0,
};
#define MEMCFGREG ((MemcfgReg*)PHYSMEMCFG)
typedef struct MemcfgReg MemcfgReg;
struct MemcfgReg {
ulong mdcnfg;
ulong mdrefr;
ulong msc0;
ulong msc1;
ulong msc2;
ulong mecr;
ulong sxcnfg;
ulong sxmrs;
ulong mcmem0;
ulong mcmem1;
ulong mcatt0;
ulong mcatt1;
ulong mcio0;
ulong mcio1;
ulong mdmrs;
ulong boot_def;
ulong mdmrslp;
ulong sa1111cr;
};
#define LCDREG ((LcdReg*)PHYSLCD)
typedef struct LcdReg LcdReg;
struct LcdReg {
ulong lccr0;
ulong lccr1;
ulong lccr2;
ulong lccr3;
struct {
ulong fdadr;
ulong fsadr;
ulong fidr;
ulong ldcmd;
} frame[2];
ulong fbr[2];
ulong lcsr;
ulong liidr;
ulong trgbr;
ulong tcr;
};
#define USBREG ((UsbReg*)PHYSUSB)
typedef struct UsbReg UsbReg;
struct UsbReg {
ulong udccr;
ulong udccs[16];
ulong ufnrh;
ulong ufnrl;
ulong udbcr2;
ulong udbcr4;
ulong udbcr7;
ulong udbcr9;
ulong udbcr12;
ulong udbcr14;
ulong uddr[16];
ulong uicr0;
ulong uicr1;
ulong usir0;
ulong usir1;
};
enum {
DmaOut= 0,
DmaIn= 1,
DmaDREQ0= 0,
DmaDREQ1,
DmaI2S_i,
DmaI2S_o,
DmaBTUART_i,
DmaBTUART_o,
DmaFFUART_i,
DmaFFUART_o,
DmaAC97mic,
DmaAC97modem_i,
DmaAC97modem_o,
DmaAC97audio_i,
DmaAC97audio_o,
DmaSSP_i,
DmaSSP_o,
DmaNSSP_i,
DmaNSSP_o,
DmaICP_i,
DmaICP_o,
DmaSTUART_i,
DmaSTUART_o,
DmaMMC_i,
DmaMMC_o,
DmaRsvd0,
DmaRsvd1,
DmaUSB1,
DmaUSB2,
DmaUSB3,
DmaUSB4,
DmaHWUART_i,
DmaUSB6,
DmaUSB7,
DmaUSB8,
DmaUSB9,
DmaHWUART_o,
DmaUSB11,
DmaUSB12,
DmaUSB13,
DmaUSB14,
DmaRsvd2,
};
enum {
PCMready,
PCMeject,
PCMstschng,
};
#define PCMCIAcard(n) (PHYSPCMCIA0+((n)*PCMCIASIZE))
#define PCMCIAIO(n) (PCMCIAcard(n)+0x0)
#define PCMCIAAttr(n) (PCMCIAcard(n)+0x8000000)
#define PCMCIAMem(n) (PCMCIAcard(n)+0xC000000)
struct PCMmap {
ulong ca;
ulong cea;
ulong isa;
int len;
int attr;
};
struct PCMconftab
{
int index;
ushort irqs;
uchar irqtype;
uchar bit16;
uchar nlines;
struct {
ulong start;
ulong len;
} io[16];
int nio;
uchar vcc;
uchar vpp1;
uchar vpp2;
uchar memwait;
ulong maxwait;
ulong readywait;
ulong otherwait;
};
struct PCMslot
{
RWlock;
Ref ref;
long memlen;
uchar slotno;
void *regs;
void *mem;
void *attr;
uchar occupied;
uchar configed;
uchar busy;
uchar powered;
uchar battery;
uchar wrprot;
uchar enabled;
uchar special;
uchar dsize;
int cisread;
char verstr[512];
uchar cpresent;
ulong caddr;
int nctab;
PCMconftab ctab[8];
PCMconftab *def;
PCMmap memmap;
PCMmap attrmap;
};