typedef struct PCMconftab PCMconftab;
typedef struct PCMmap PCMmap;
typedef struct PCMslot PCMslot;
#define PCMCIAcard(n) (PHYSPCMCIA0+((n)*PCMCIASIZE))
#define PCMCIAIO(n) (PCMCIAcard(n)+0x0)
#define PCMCIAAttr(n) (PCMCIAcard(n)+0x8000000)
#define PCMCIAMem(n) (PCMCIAcard(n)+0xC000000)
#define INTRREG ((IntrReg*)PHYSINTR)
typedef struct IntrReg IntrReg;
struct IntrReg {
ulong icip;
ulong icmr;
ulong iclr;
ulong iccr;
ulong icfp;
ulong rsvd[3];
ulong icpr;
};
#define GPIObit(n) (n)
#define LCDbit (12)
#define UDCbit (13)
#define SDLCbit (14)
#define UARTbit(n) (15+((n)-1))
#define HSSPbit (16)
#define MCPbit (18)
#define SSPbit (19)
#define DMAbit(chan) (20+(chan))
#define OSTimerbit(n) (26+(n))
#define RTCticbit (30)
#define RTCalarmbit (31)
#define MaxIRQbit 31
#define MaxGPIObit 27
#define GPIOREG ((GpioReg*)PHYSGPIO)
typedef struct GpioReg GpioReg;
struct GpioReg {
ulong gplr;
ulong gpdr;
ulong gpsr;
ulong gpcr;
ulong grer;
ulong gfer;
ulong gedr;
ulong gafr;
};
enum {
GPIO_32KHZ_OUT_o = 1<<27,
GPIO_RCLK_OUT_o = 1<<26,
GPIO_RTC_clock_o = 1<<25,
GPIO_TREQB_i = 1<<23,
GPIO_TREQA_i = 1<<22,
GPIO_TICK_ACK_o = 1<<21,
GPIO_MCP_CLK_i = 1<<21,
GPIO_UART_SCLK3_i = 1<<20,
GPIO_SSP_CLK_i = 1<<19,
GPIO_UART_SCLK1_i = 1<<18,
GPIO_GPCLK_OUT_o = 1<<16,
GPIO_UART_RXD_i = 1<<15,
GPIO_UART_TXD_o = 1<<14,
GPIO_SSP_SFRM_o = 1<<13,
GPIO_SSP_SCLK_o = 1<<12,
GPIO_SSP_RXD_i = 1<<11,
GPIO_SSP_TXD_o = 1<<10,
GPIO_LDD8_15_o = 0xFF<<2,
GPIO_LDD15_o = 1<<9,
GPIO_LDD14_o = 1<<8,
GPIO_LDD13_o = 1<<7,
GPIO_LDD12_o = 1<<6,
GPIO_LDD11_o = 1<<5,
GPIO_LDD10_o = 1<<4,
GPIO_LDD9_o = 1<<3,
GPIO_LDD8_o = 1<<2,
};
#define RTCREG ((RtcReg*)PHYSRTC)
typedef struct RtcReg RtcReg;
struct RtcReg {
ulong rtar;
ulong rcnr;
ulong rttr;
ulong rsvd;
ulong rtsr;
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
ulong pcfr;
ulong ppcr;
ulong pgsr;
ulong posr;
};
enum
{
PCFR_opde = 1<<0,
PCFR_fp = 1<<1,
PCFR_fs = 1<<2,
PCFR_fo = 1<<3,
PWER_rtc = 1<<31,
PSSR_sss = 1<<0,
PSSR_bfs = 1<<1,
PSSR_vfs = 1<<2,
PSSR_dh = 1<<3,
PSSR_ph = 1<<4,
};
#define RESETREG ((ResetReg*)PHYSRESET)
typedef struct ResetReg ResetReg;
struct ResetReg {
ulong rsrr;
ulong rcsr;
ulong tucr;
};
#define MEMCFGREG ((MemcfgReg*)PHYSMEMCFG)
typedef struct MemcfgReg MemcfgReg;
struct MemcfgReg {
ulong mdcnfg;
ulong mdcas0[3];
ulong msc0;
ulong msc1;
ulong mecr;
ulong mdrefr;
ulong mdcas2[3];
ulong msc2;
ulong smcnfg;
};
#define DMAREG(n) ((DmaReg*)(PHYSDMA+0x20*(n)))
typedef struct DmaReg DmaReg;
struct DmaReg {
ulong ddar;
ulong dcsr_s;
ulong dcsr_c;
ulong dcsr;
struct {
ulong start;
ulong count;
} buf[2];
};
#define LCDREG ((LcdReg*)PHYSLCD)
typedef struct LcdReg LcdReg;
struct LcdReg {
ulong lccr0;
ulong lcsr;
ulong rsvd[2];
ulong dbar1;
ulong dcar1;
ulong dbar2;
ulong dcar2;
ulong lccr1;
ulong lccr2;
ulong lccr3;
};
#define USBREG ((UsbReg*)PHYSUSB)
typedef struct UsbReg UsbReg;
struct UsbReg {
ulong udccr;
ulong udcar;
ulong udcomp;
ulong udcimp;
ulong udccs0;
ulong udccs1;
ulong udccs2;
ulong udcd0;
ulong udcwc;
ulong rsvd1;
ulong udcdr;
ulong rsvd2;
ulong dcsr;
};
#define GPCLKREG ((GpclkReg*)PHYSGPCLK)
typedef struct GpclkReg GpclkReg;
struct GpclkReg {
ulong gpclkr0;
ulong rsvd[2];
ulong gpclkr1;
ulong gpclkr2;
};
#define UARTREG(n) ((UartReg*)(PHYSSERIAL(2*(n)-1)))
typedef struct UartReg UartReg;
struct UartReg {
ulong utcr0;
ulong utcr1;
ulong utcr2;
ulong utcr3;
ulong utcr4;
ulong utdr;
ulong rsvd;
ulong utsr0;
ulong utsr1;
};
#define HSSPREG ((HsspReg*)(0x80040060))
typedef struct HsspReg HsspReg;
struct HsspReg {
ulong hscr0;
ulong hscr1;
ulong rsvd1;
ulong hsdr;
ulong rsvd2;
ulong hssr0;
ulong hssr1;
};
#define MCPREG ((McpReg*)(PHYSMCP))
typedef struct McpReg McpReg;
struct McpReg {
ulong mccr;
ulong rsvd1;
ulong mcdr0;
ulong mcdr1;
ulong mcdr2;
ulong rsvd2;
ulong mcsr;
};
enum {
MCCR_M_LBM= 0x800000,
MCCR_M_ARM= 0x400000,
MCCR_M_ATM= 0x200000,
MCCR_M_TRM= 0x100000,
MCCR_M_TTM= 0x080000,
MCCR_M_ADM= 0x040000,
MCCR_M_ECS= 0x020000,
MCCR_M_MCE= 0x010000,
MCCR_V_TSD= 8,
MCCR_V_ASD= 0,
MCDR2_M_nRW= 0x010000,
MCDR2_V_RN= 17,
MCSR_M_TCE= 0x8000,
MCSR_M_ACE= 0X4000,
MCSR_M_CRC= 0x2000,
MCSR_M_CWC= 0x1000,
MCSR_M_TNE= 0x0800,
MCSR_M_TNF= 0x0400,
MCSR_M_ANE= 0x0200,
MCSR_M_ANF= 0x0100,
MCSR_M_TRO= 0x0080,
MCSR_M_TTU= 0x0040,
MCSR_M_ARO= 0x0020,
MCSR_M_ATU= 0x0010,
MCSR_M_TRS= 0x0008,
MCSR_M_TTS= 0x0004,
MCSR_M_ARS= 0x0002,
MCSR_M_ATS= 0x0001,
};
#define SSPREG ((SspReg*)PHYSSSP)
typedef struct SspReg SspReg;
struct SspReg {
ulong sscr0;
ulong sscr1;
ulong rsvd1;
ulong ssdr;
ulong rsvd2;
ulong sssr;
};
enum {
SSCR0_V_SCR= 0x08,
SSCR0_V_SSE= 0x07,
SSCR0_V_ECS= 0x06,
SSCR0_V_FRF= 0x04,
SSPCR0_M_DSS= 0x0000000F,
SSPCR0_M_FRF= 0x00000030,
SSPCR0_M_SSE= 0x00000080,
SSPCR0_M_SCR= 0x0000FF00,
SSPCR0_V_DSS= 0,
SSPCR0_V_FRF= 4,
SSPCR0_V_SSE= 7,
SSPCR0_V_SCR= 8,
SSPCR1_M_RIM= 0x00000001,
SSPCR1_M_TIN= 0x00000002,
SSPCR1_M_LBM= 0x00000004,
SSPCR1_V_RIM= 0,
SSPCR1_V_TIN= 1,
SSPCR1_V_LBM= 2,
SSPSR_M_TNF= 0x00000002,
SSPSR_M_RNE= 0x00000004,
SSPSR_M_BSY= 0x00000008,
SSPSR_M_TFS= 0x00000010,
SSPSR_M_RFS= 0x00000020,
SSPSR_M_ROR= 0x00000040,
SSPSR_V_TNF= 1,
SSPSR_V_RNE= 2,
SSPSR_V_BSY= 3,
SSPSR_V_TFS= 4,
SSPSR_V_RFS= 5,
SSPSR_V_ROR= 6,
};
#define PPCREG ((PpcReg*)PHYSPPC)
typedef struct PpcReg PpcReg;
struct PpcReg {
ulong ppdr;
ulong ppsr;
ulong ppar;
ulong psdr;
ulong ppfr;
uchar rsvd[0x1c];
ulong mccr1;
};
enum {
PPC_LDD0_7= 0xFF<<0,
PPC_L_PCLK= 1<<8,
PPC_L_LCLK= 1<<9,
PPC_L_FCLK= 1<<10,
PPC_L_BIAS= 1<<11,
PPC_TXD1= 1<<12,
PPC_RXD1= 1<<13,
PPC_TXD2= 1<<14,
PPC_RXD2= 1<<15,
PPC_TXD3= 1<<16,
PPC_RXD3= 1<<17,
PPC_TXD4= 1<<18,
PPC_RXD4= 1<<19,
PPC_SCLK= 1<<20,
PPC_SFRM= 1<<21,
PPAR_UPR= 1<<12,
PPAR_SPR= 1<<18,
};
enum {
BusCPU= 1,
BusGPIOfalling= 2,
BusGPIOrising = 3,
BusGPIOboth = 4,
BusMAX= 4,
BUSUNKNOWN= -1,
};
enum {
DmaOUT= 0,
DmaIN= 1,
DmaLittle= 0,
DmaBig= 1,
DmaUDC= 0,
DmaSDLC= 2,
DmaUART0= 4,
DmaHSSP= 6,
DmaUART1= 7,
DmaUART2= 8,
DmaMCPaudio= 10,
DmaMCPtelecom= 12,
DmaSSP= 14,
};
enum {
PCMready,
PCMeject,
PCMstschng,
};
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
int ncfg;
struct {
ushort cpresent;
ulong caddr;
} cfg[8];
int nctab;
PCMconftab ctab[8];
PCMconftab *def;
PCMmap memmap;
PCMmap attrmap;
};