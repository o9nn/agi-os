enum
{
ClockFreq=	3686400,
};
enum
{
IRQgpio0=	0,
IRQgpio1=	1,
IRQgpio2=	2,
IRQgpio3=	3,
IRQgpio4=	4,
IRQgpio5=	5,
IRQgpio6=	6,
IRQgpio7=	7,
IRQgpio8=	8,
IRQgpio9=	9,
IRQgpio10=	10,
IRQgpiohi=	11,
IRQlcd=		12,
IRQudc=		13,
IRQuart1b=	15,
IRQuart2=	16,
IRQuart3=	17,
IRQmcp=		18,
IRQssp=		19,
IRQdma0=	20,
IRQdma1=	21,
IRQdma2=	22,
IRQdma3=	23,
IRQdma4=	24,
IRQdma5=	25,
IRQtimer0=	26,
IRQtimer1=	27,
IRQtimer2=	28,
IRQtimer3=	29,
IRQsecond=	30,
IRQrtc=		31,
};
enum
{
GPIO_PWR_ON_i=		1<<0,
GPIO_UP_IRQ_i=		1<<1,
GPIO_LDD8_o=		1<<2,
GPIO_LDD9_o=		1<<3,
GPIO_LDD10_o=		1<<4,
GPIO_LDD11_o=		1<<5,
GPIO_LDD12_o=		1<<6,
GPIO_LDD13_o=		1<<7,
GPIO_LDD14_o=		1<<8,
GPIO_LDD15_o=		1<<9,
GPIO_CARD_IND1_i=	1<<10,
GPIO_CARD_IRQ1_i=	1<<11,
GPIO_CLK_SET0_o=	1<<12,
GPIO_CLK_SET1_o=	1<<13,
GPIO_L3_SDA_io=		1<<14,
GPIO_L3_MODE_o=		1<<15,
GPIO_L3_SCLK_o=		1<<16,
GPIO_CARD_IND0_i=	1<<17,
GPIO_KEY_ACT_i=		1<<18,
GPIO_SYS_CLK_i=		1<<19,
GPIO_BAT_FAULT_i=	1<<20,
GPIO_CARD_IRQ0_i=	1<<21,
GPIO_LOCK_i=		1<<22,
GPIO_COM_DCD_i=		1<<23,
GPIO_OPT_IRQ_i=		1<<24,
GPIO_COM_CTS_i=		1<<25,
GPIO_COM_RTS_o=		1<<26,
GPIO_OPT_IND_i=		1<<27,
GPIO_SSP_TXD_o=		1<<10,
GPIO_SSP_RXD_i=		1<<11,
GPIO_SSP_SCLK_o=	1<<12,
GPIO_SSP_SFRM_o=	1<<13,
GPIO_UART_TXD_o=	1<<14,
GPIO_UART_RXD_i=	1<<15,
GPIO_SDLC_SCLK_io=	1<<16,
GPIO_SDLC_AAF_o=	1<<17,
GPIO_UART_SCLK1_i=	1<<18,
GPIO_SSP_CLK_i=		1<<19,
GPIO_UART_SCLK3_i=	1<<20,
GPIO_MCP_CLK_i=		1<<21,
GPIO_TIC_ACK_o=		1<<21,
GPIO_MBGNT_o=		1<<21,
GPIO_TREQA_i=		1<<22,
GPIO_MBREQ_i=		1<<22,
GPIO_TREQB_i=		1<<23,
GPIO_1Hz_o=			1<<25,
GPIO_RCLK_o=		1<<26,
GPIO_32_768kHz_o=	1<<27,
};
enum
{
GPIOrising,
GPIOfalling,
GPIOboth,
IRQ,
};
typedef struct Uartregs Uartregs;
struct Uartregs
{
ulong	ctl[4];
ulong	dummya;
ulong	data;
ulong	dummyb;
ulong	status[2];
};
Uartregs *uart3regs;
typedef struct GPIOregs GPIOregs;
struct GPIOregs
{
ulong	level;
ulong	direction;
ulong	set;
ulong	clear;
ulong	rising;
ulong	falling;
ulong	edgestatus;
ulong	altfunc;
};
extern GPIOregs *gpioregs;
enum
{
EGPIO_prog_flash=	1<<0,
EGPIO_pcmcia_reset=	1<<1,
EGPIO_exppack_reset=	1<<2,
EGPIO_codec_reset=	1<<3,
EGPIO_exp_nvram_power=	1<<4,
EGPIO_exp_full_power=	1<<5,
EGPIO_lcd_3v=		1<<6,
EGPIO_rs232_power=	1<<7,
EGPIO_lcd_ic_power=	1<<8,
EGPIO_ir_power=		1<<9,
EGPIO_audio_power=	1<<10,
EGPIO_audio_ic_power=	1<<11,
EGPIO_audio_mute=	1<<12,
EGPIO_fir=		1<<13,
EGPIO_lcd_5v=		1<<14,
EGPIO_lcd_9v=		1<<15,
};
extern ulong *egpioreg;
typedef struct PPCregs PPCregs;
struct PPCregs {
ulong	direction;
ulong	state;
ulong	assignment;
ulong	sleepdir;
ulong	flags;
};
extern PPCregs *ppcregs;
typedef struct SSPregs SSPregs;
struct SSPregs {
ulong	control0;
ulong	control1;
ulong	dummy0;
ulong	data;
ulong	dummy1;
ulong	status;
};
extern SSPregs *sspregs;
typedef struct MCPregs MCPregs;
struct MCPregs {
ulong	control0;
ulong	reserved0;
ulong	data0;
ulong	data1;
ulong	data2;
ulong	reserved1;
ulong	status;
ulong	reserved[11];
ulong	control1;
};
extern MCPregs *mcpregs;
enum
{
MECR_io0=	0,
MECR_attr0=	5,
MECR_mem0=	10,
MECR_fast0=	11,
MECR_io1=	MECR_io0+16,
MECR_attr1=	MECR_attr0+16,
MECR_mem1=	MECR_mem0+16,
MECR_fast1=	MECR_fast0+16,
};
typedef struct MemConfRegs MemConfRegs;
struct MemConfRegs
{
ulong	mdcnfg;
ulong	mdcas00;
ulong	mdcas01;
ulong	mdcas02;
ulong	msc0;
ulong	msc1;
ulong	mecr;
ulong	mdrefr;
ulong	mdcas20;
ulong	mdcas21;
ulong	mdcas22;
ulong	msc2;
ulong	smcnfg;
};
extern MemConfRegs *memconfregs;
typedef struct PowerRegs PowerRegs;
struct PowerRegs
{
ulong	pmcr;
ulong	pssr;
ulong	pspr;
ulong	pwer;
ulong	pcfr;
ulong	ppcr;
ulong	pgsr;
ulong	posr;
};
extern PowerRegs *powerregs;