#define	SBUS(n)		(0x30000000+(n)*0x10000000)
#define	FRAMEBUF(n)	SBUS(n)
#define	FRAMEBUFID(n)	(SBUS(n)+0x000000)
#define	DISPLAYRAM(n)	(SBUS(n)+0x800000)
#define	CLOCK		0x71D00000
#define	CLOCKFREQ	1000000
#define SUPERIO_PHYS_PAGE		0x71300000
#define SUPERIO_INDEX_OFFSET		0x398
#define SUPERIO_DATA_OFFSET		0x399
#define SUPERIO_MOUSE_KBD_DATA_PORT	0x60
#define SUPERIO_MOUSE_KBD_CTL_PORT	0x64
#define AUDIO_PHYS_PAGE		0x66666666
#define AUDIO_INDEX_OFFSET	0x830
enum
{
Mousevec = 13,
Kbdvec = 13
};
#define	NVR_CKSUM_PHYS	0x71200000
#define	NVR_PHYS	0x71201000
#define DMA		0x78400000
#define SCSI		0x78800000
#define	ETHER		0x78C00000
#define	FLOPPY		0x71400000
#define	SYSINTR		0x71E10000
#define	TIMECONFIG	0x71D10010
#define	AUXIO1		0x71900000
#define	AUXIO2		0x71910000
typedef struct Sysint Sysint;
struct Sysint
{
ulong	pending;
ulong	mask;
ulong	maskclr;
ulong	maskset;
ulong	target;
};
enum {
MaskAllIntr = 1<<31,
MEIntr = 1<<30,
MSIIntr = 1<<29,
EMCIntr = 1<<28,
VideoIntr = 1<<20,
Timer10 = 1<<19,
EtherIntr = 1<<16,
SCCIntr = 1<<15,
KbdIntr = 1<<13,
};
#define	SBUSINTR(x)	(1<<((x)+6))
typedef struct SCCdev	SCCdev;
struct SCCdev
{
uchar	ptrb;
uchar	dummy1;
uchar	datab;
uchar	dummy2;
uchar	ptra;
uchar	dummy3;
uchar	dataa;
uchar	dummy4;
};
#define NVREAD	(4096-32)
#define NVWRITE	(0x800)
#define	IDOFF	(4096-8-32)
typedef struct RTCdev	RTCdev;
struct RTCdev
{
uchar	control;
uchar	sec;
uchar	min;
uchar	hour;
uchar	wday;
uchar	mday;
uchar	mon;
uchar	year;
};
#define RTCOFF		0xFF8
#define RTCREAD		(0x40)
#define RTCWRITE	(0x80)
typedef struct DMAdev DMAdev;
struct DMAdev {
ulong	csr;
ulong	addr;
ulong	count;
ulong	diag;
ulong	ecsr;
ulong	ediag;
ulong	cache;
uchar	base;
};
enum {
Int_pend	= 0x00000001,
Err_pend	= 0x00000002,
Pack_cnt	= 0x0000000C,
Int_en		= 0x00000010,
Dma_Flush	= 0x00000020,
Drain		= 0x00000040,
Dma_Reset	= 0x00000080,
Write		= 0x00000100,
En_dma		= 0x00000200,
Req_pend	= 0x00000400,
Byte_addr	= 0x00001800,
En_cnt		= 0x00002000,
Tc		= 0x00004000,
Ilacc		= 0x00008000,
Dev_id		= 0xF0000000,
};
typedef struct SCSIdev	SCSIdev;
struct SCSIdev {
uchar	countlo;
uchar	pad1[3];
uchar	countmi;
uchar	pad2[3];
uchar	fifo;
uchar	pad3[3];
uchar	cmd;
uchar	pad4[3];
union {
struct {
uchar	status;
uchar	pad05[3];
uchar	intr;
uchar	pad06[3];
uchar	step;
uchar	pad07[3];
uchar	fflags;
uchar	pad08[3];
uchar	config;
uchar	pad09[3];
uchar	Reserved1;
uchar	pad0A[3];
uchar	Reserved2;
uchar	pad0B[3];
uchar	conf2;
uchar	pad0C[3];
uchar	conf3;
uchar	pad0D[3];
uchar	partid;
uchar	pad0E[3];
uchar	fbottom;
uchar	pad0F[3];
};
struct {
uchar	destid;
uchar	pad15[3];
uchar	timeout;
uchar	pad16[3];
uchar	syncperiod;
uchar	pad17[3];
uchar	syncoffset;
uchar	pad18[3];
uchar	RW0;
uchar	pad19[3];
uchar	clkconf;
uchar	pad1A[3];
uchar	test;
uchar	pad1B[3];
uchar	RW1;
uchar	pad1C[3];
uchar	RW2;
uchar	pad1D[3];
uchar	counthi;
uchar	pad1E[3];
uchar	RW3;
uchar	pad1F[3];
};
};
};
enum {
E_Int_pend	= 0x00000001,
E_Err_pend	= 0x00000002,
E_draining	= 0x0000000C,
E_Int_en	= 0x00000010,
E_Invalidate	= 0x00000020,
E_Slave_err	= 0x00000040,
E_Reset		= 0x00000080,
E_Drain		= 0x00000400,
E_Dsbl_wr_drn	= 0x00000800,
E_Dsbl_rd_drn	= 0x00001000,
E_Ilacc		= 0x00008000,
E_Dsbl_buf_wr	= 0x00010000,
E_Dsbl_wr_inval	= 0x00020000,
E_Burst_size	= 0x000C0000,
E_Loop_test	= 0x00200000,
E_TP_select	= 0x00400000,
E_Dev_id	= 0xF0000000,
};