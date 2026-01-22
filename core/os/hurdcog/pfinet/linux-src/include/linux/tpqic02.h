#ifndef _LINUX_TPQIC02_H
#define _LINUX_TPQIC02_H
#include <linux/config.h>
#if CONFIG_QIC02_TAPE || CONFIG_QIC02_TAPE_MODULE
#include <linux/mtio.h>
#define WANGTEK		1
#define EVEREX		(WANGTEK+1)
#define EVEREX_811V	EVEREX
#define EVEREX_831V	EVEREX
#define ARCHIVE		3
#define ARCHIVE_SC400	ARCHIVE
#define ARCHIVE_SC402	ARCHIVE
#define ARCHIVE_SC499	ARCHIVE
#define MOUNTAIN	5
#define EMERALD		6
#define QIC02_TAPE_PORT_RANGE 	8
#ifndef CONFIG_QIC02_DYNCONF
#define QIC02_TAPE_DRIVE	MT_ISQIC02_ALL_FEATURES
#define QIC02_TAPE_IFC		WANGTEK
#define QIC02_TAPE_PORT 	0x300
#define QIC02_TAPE_IRQ		5
#define QIC02_TAPE_DMA		1
#undef QIC02_TAPE_DMA3_FIX
#if QIC02_TAPE_DRIVE == MT_ISWT5150
#define TP_HAVE_DENS	1
#define TP_HAVE_BSF	0
#define TP_HAVE_FSR	0
#define TP_HAVE_BSR	0
#define TP_HAVE_EOD	0
#define TP_HAVE_SEEK	0
#define TP_HAVE_TELL	0
#define TP_HAVE_RAS1	1
#define TP_HAVE_RAS2	1
#elif QIC02_TAPE_DRIVE == MT_ISARCHIVESC499
#define TP_HAVE_DENS	1
#define TP_HAVE_BSF	0
#define TP_HAVE_FSR	1
#define TP_HAVE_BSR	1
#define TP_HAVE_EOD	1
#define TP_HAVE_SEEK	0
#define TP_HAVE_TELL	0
#define TP_HAVE_RAS1	1
#define TP_HAVE_RAS2	1
#elif (QIC02_TAPE_DRIVE == MT_ISARCHIVE_2060L) || (QIC02_TAPE_DRIVE == MT_ISARCHIVE_2150L)
#define TP_HAVE_DENS	1
#define TP_HAVE_BSF	0
#define TP_HAVE_FSR	1
#define TP_HAVE_BSR	1
#define TP_HAVE_EOD	1
#define TP_HAVE_TELL	1
#define TP_HAVE_SEEK	1
#define TP_HAVE_RAS1	1
#define TP_HAVE_RAS2	1
#elif QIC02_TAPE_DRIVE == MT_ISARCHIVE_5945L2
#define TP_HAVE_DENS	1
#define TP_HAVE_BSF	0
#define TP_HAVE_FSR	1
#define TP_HAVE_BSR	1
#define TP_HAVE_EOD	1
#define TP_HAVE_TELL	1
#define TP_HAVE_SEEK	1
#define TP_HAVE_RAS1	1
#define TP_HAVE_RAS2	1
#elif QIC02_TAPE_DRIVE == MT_ISTEAC_MT2ST
#define TP_HAVE_DENS	0
#define TP_HAVE_BSF	0
#define TP_HAVE_FSR	1
#define TP_HAVE_BSR	1
#define TP_HAVE_EOD	1
#define TP_HAVE_SEEK	1
#define TP_HAVE_TELL	1
#define TP_HAVE_RAS1	1
#define TP_HAVE_RAS2	1
#elif QIC02_TAPE_DRIVE == MT_ISQIC02_ALL_FEATURES
#define TP_HAVE_DENS	1
#define TP_HAVE_BSF	1
#define TP_HAVE_FSR	1
#define TP_HAVE_BSR	1
#define TP_HAVE_EOD	1
#define TP_HAVE_SEEK	1
#define TP_HAVE_TELL	1
#define TP_HAVE_RAS1	1
#define TP_HAVE_RAS2	1
#else
#error No QIC-02 tape drive type defined!
#endif
#endif
#define WT_QIC02_STAT_PORT	(QIC02_TAPE_PORT)
#define WT_QIC02_CTL_PORT	(QIC02_TAPE_PORT)
#define WT_QIC02_CMD_PORT	(QIC02_TAPE_PORT+1)
#define WT_QIC02_DATA_PORT	(QIC02_TAPE_PORT+1)
#define WT_QIC02_STAT_POLARITY	0
#define WT_QIC02_STAT_READY	0x01
#define WT_QIC02_STAT_EXCEPTION	0x02
#define WT_QIC02_STAT_MASK	(WT_QIC02_STAT_READY|WT_QIC02_STAT_EXCEPTION)
#define WT_QIC02_STAT_RESETMASK	0x07
#define WT_QIC02_STAT_RESETVAL	(WT_QIC02_STAT_RESETMASK & ~WT_QIC02_STAT_EXCEPTION)
#define WT_QIC02_CTL_RESET	0x02
#define WT_QIC02_CTL_REQUEST	0x04
#define WT_CTL_ONLINE		0x01
#define WT_CTL_CMDOFF		0xC0
#define WT_CTL_DMA3		0x10
#define WT_CTL_DMA1		0x08
#define EMR_QIC02_STAT_PORT	(QIC02_TAPE_PORT)
#define EMR_QIC02_CTL_PORT	(QIC02_TAPE_PORT)
#define EMR_QIC02_CMD_PORT	(QIC02_TAPE_PORT+1)
#define EMR_QIC02_DATA_PORT	(QIC02_TAPE_PORT+1)
#define EMR_QIC02_STAT_POLARITY		1
#define EMR_QIC02_STAT_READY		0x01
#define EMR_QIC02_STAT_EXCEPTION	0x02
#define EMR_QIC02_STAT_MASK	(EMR_QIC02_STAT_READY|EMR_QIC02_STAT_EXCEPTION)
#define EMR_QIC02_STAT_RESETMASK	0x07
#define EMR_QIC02_STAT_RESETVAL	(EMR_QIC02_STAT_RESETMASK & ~EMR_QIC02_STAT_EXCEPTION)
#define EMR_QIC02_CTL_RESET	0x02
#define EMR_QIC02_CTL_REQUEST	0x04
#define EMR_CTL_ONLINE		0x01
#define EMR_CTL_CMDOFF		0xC0
#define EMR_CTL_DMA3		0x10
#define EMR_CTL_DMA1		0x08
#define AR_QIC02_STAT_PORT	(QIC02_TAPE_PORT+1)
#define AR_QIC02_CTL_PORT	(QIC02_TAPE_PORT+1)
#define AR_QIC02_CMD_PORT	(QIC02_TAPE_PORT)
#define AR_QIC02_DATA_PORT	(QIC02_TAPE_PORT)
#define AR_START_DMA_PORT	(QIC02_TAPE_PORT+2)
#define AR_RESET_DMA_PORT	(QIC02_TAPE_PORT+3)
#define AR_QIC02_STAT_POLARITY	0
#define AR_STAT_IRQF		0x80
#define AR_QIC02_STAT_READY	0x40
#define AR_QIC02_STAT_EXCEPTION	0x20
#define AR_QIC02_STAT_MASK	(AR_QIC02_STAT_READY|AR_QIC02_STAT_EXCEPTION)
#define AR_STAT_DMADONE		0x10
#define AR_STAT_DIRC		0x08
#define AR_QIC02_STAT_RESETMASK	0x70
#define AR_QIC02_STAT_RESETVAL	((AR_QIC02_STAT_RESETMASK & ~AR_STAT_IRQF & ~AR_QIC02_STAT_EXCEPTION) | AR_STAT_DMADONE)
#define AR_QIC02_CTL_RESET	0x80
#define AR_QIC02_CTL_REQUEST	0x40
#define AR_CTL_IEN		0x20
#define AR_CTL_DNIEN		0x10
#define MTN_QIC02_STAT_PORT	(QIC02_TAPE_PORT+1)
#define MTN_QIC02_CTL_PORT	(QIC02_TAPE_PORT+1)
#define MTN_QIC02_CMD_PORT	(QIC02_TAPE_PORT)
#define MTN_QIC02_DATA_PORT	(QIC02_TAPE_PORT)
#define MTN_W_SELECT_DMA_PORT	(QIC02_TAPE_PORT+2)
#define MTN_R_DESELECT_DMA_PORT	(QIC02_TAPE_PORT+2)
#define MTN_W_DMA_WRITE_PORT	(QIC02_TAPE_PORT+3)
#define MTN_QIC02_STAT_POLARITY	 0
#define MTN_QIC02_STAT_READY	 0x02
#define MTN_QIC02_STAT_EXCEPTION 0x04
#define MTN_QIC02_STAT_MASK	 (MTN_QIC02_STAT_READY|MTN_QIC02_STAT_EXCEPTION)
#define MTN_STAT_DMADONE	 0x01
#define MTN_QIC02_STAT_RESETMASK 0x07
#define MTN_QIC02_STAT_RESETVAL	 ((MTN_QIC02_STAT_RESETMASK & ~MTN_QIC02_STAT_EXCEPTION) | MTN_STAT_DMADONE)
#define MTN_QIC02_CTL_RESET_NOT	 0x80
#define MTN_QIC02_CTL_RESET	 0x80
#define MTN_QIC02_CTL_ONLINE	 0x40
#define MTN_QIC02_CTL_REQUEST	 0x20
#define MTN_QIC02_CTL_IRQ_DRIVER 0x10
#define MTN_QIC02_CTL_DMA_DRIVER 0x08
#define MTN_CTL_EXC_IEN		 0x04
#define MTN_CTL_RDY_IEN		 0x02
#define MTN_CTL_DNIEN		 0x01
#define MTN_CTL_ONLINE		(MTN_QIC02_CTL_RESET_NOT | MTN_QIC02_CTL_IRQ_DRIVER | MTN_QIC02_CTL_DMA_DRIVER)
#ifndef CONFIG_QIC02_DYNCONF
# define QIC02_TAPE_DEBUG	(qic02_tape_debug)
# if QIC02_TAPE_IFC == WANGTEK
#  define QIC02_STAT_POLARITY	WT_QIC02_STAT_POLARITY
#  define QIC02_STAT_PORT	WT_QIC02_STAT_PORT
#  define QIC02_CTL_PORT	WT_QIC02_CTL_PORT
#  define QIC02_CMD_PORT	WT_QIC02_CMD_PORT
#  define QIC02_DATA_PORT	WT_QIC02_DATA_PORT
#  define QIC02_STAT_READY	WT_QIC02_STAT_READY
#  define QIC02_STAT_EXCEPTION	WT_QIC02_STAT_EXCEPTION
#  define QIC02_STAT_MASK	WT_QIC02_STAT_MASK
#  define QIC02_STAT_RESETMASK	WT_QIC02_STAT_RESETMASK
#  define QIC02_STAT_RESETVAL	WT_QIC02_STAT_RESETVAL
#  define QIC02_CTL_RESET	WT_QIC02_CTL_RESET
#  define QIC02_CTL_REQUEST	WT_QIC02_CTL_REQUEST
#  if QIC02_TAPE_DMA == 3
#   ifdef QIC02_TAPE_DMA3_FIX
#    define WT_CTL_DMA		WT_CTL_DMA1
#   else
#    define WT_CTL_DMA		WT_CTL_DMA3
#   endif
#  elif QIC02_TAPE_DMA == 1
#    define WT_CTL_DMA		WT_CTL_DMA1
#  else
#   error Unsupported or incorrect DMA configuration.
#  endif
# elif QIC02_TAPE_IFC == EMERALD
#  define QIC02_STAT_POLARITY	EMR_QIC02_STAT_POLARITY
#  define QIC02_STAT_PORT	EMR_QIC02_STAT_PORT
#  define QIC02_CTL_PORT	EMR_QIC02_CTL_PORT
#  define QIC02_CMD_PORT	EMR_QIC02_CMD_PORT
#  define QIC02_DATA_PORT	EMR_QIC02_DATA_PORT
#  define QIC02_STAT_READY	EMR_QIC02_STAT_READY
#  define QIC02_STAT_EXCEPTION	EMR_QIC02_STAT_EXCEPTION
#  define QIC02_STAT_MASK	EMR_QIC02_STAT_MASK
#  define QIC02_STAT_RESETMASK	EMR_QIC02_STAT_RESETMASK
#  define QIC02_STAT_RESETVAL	EMR_QIC02_STAT_RESETVAL
#  define QIC02_CTL_RESET	EMR_QIC02_CTL_RESET
#  define QIC02_CTL_REQUEST	EMR_QIC02_CTL_REQUEST
#  if QIC02_TAPE_DMA == 3
#   ifdef QIC02_TAPE_DMA3_FIX
#    define EMR_CTL_DMA		EMR_CTL_DMA1
#   else
#    define EMR_CTL_DMA		EMR_CTL_DMA3
#   endif
#  elif QIC02_TAPE_DMA == 1
#    define EMR_CTL_DMA		EMR_CTL_DMA1
#  else
#   error Unsupported or incorrect DMA configuration.
#  endif
# elif QIC02_TAPE_IFC == ARCHIVE
#  define QIC02_STAT_POLARITY	AR_QIC02_STAT_POLARITY
#  define QIC02_STAT_PORT	AR_QIC02_STAT_PORT
#  define QIC02_CTL_PORT	AR_QIC02_CTL_PORT
#  define QIC02_CMD_PORT	AR_QIC02_CMD_PORT
#  define QIC02_DATA_PORT	AR_QIC02_DATA_PORT
#  define QIC02_STAT_READY	AR_QIC02_STAT_READY
#  define QIC02_STAT_EXCEPTION	AR_QIC02_STAT_EXCEPTION
#  define QIC02_STAT_MASK	AR_QIC02_STAT_MASK
#  define QIC02_STAT_RESETMASK	AR_QIC02_STAT_RESETMASK
#  define QIC02_STAT_RESETVAL	AR_QIC02_STAT_RESETVAL
#  define QIC02_CTL_RESET	AR_QIC02_CTL_RESET
#  define QIC02_CTL_REQUEST	AR_QIC02_CTL_REQUEST
#  if QIC02_TAPE_DMA > 3
#   error DMA channels other than 1 and 3 are not supported.
#  endif
# elif QIC02_TAPE_IFC == MOUNTAIN
#  define QIC02_STAT_POLARITY	MTN_QIC02_STAT_POLARITY
#  define QIC02_STAT_PORT	MTN_QIC02_STAT_PORT
#  define QIC02_CTL_PORT	MTN_QIC02_CTL_PORT
#  define QIC02_CMD_PORT	MTN_QIC02_CMD_PORT
#  define QIC02_DATA_PORT	MTN_QIC02_DATA_PORT
#  define QIC02_STAT_READY	MTN_QIC02_STAT_READY
#  define QIC02_STAT_EXCEPTION	MTN_QIC02_STAT_EXCEPTION
#  define QIC02_STAT_MASK	MTN_QIC02_STAT_MASK
#  define QIC02_STAT_RESETMASK	MTN_QIC02_STAT_RESETMASK
#  define QIC02_STAT_RESETVAL	MTN_QIC02_STAT_RESETVAL
#  define QIC02_CTL_RESET	MTN_QIC02_CTL_RESET
#  define QIC02_CTL_REQUEST	MTN_QIC02_CTL_REQUEST
#  if QIC02_TAPE_DMA > 3
#   error DMA channels other than 1 and 3 are not supported.
#  endif
# else
#  error No valid interface card specified!
# endif
# ifndef WT_CTL_DMA
#  define WT_CTL_DMA		WT_CTL_DMA1
# endif
#else
# define QIC02_TAPE_DRIVE	(qic02_tape_dynconf.mt_type)
# define QIC02_TAPE_IFC		(qic02_tape_ccb.ifc_type)
# define QIC02_TAPE_IRQ		(qic02_tape_dynconf.irqnr)
# define QIC02_TAPE_DMA		(qic02_tape_dynconf.dmanr)
# define QIC02_TAPE_PORT	(qic02_tape_dynconf.port)
# define WT_CTL_DMA		(qic02_tape_ccb.dma_enable_value)
# define QIC02_TAPE_DEBUG	(qic02_tape_dynconf.debug)
# define QIC02_STAT_PORT	(qic02_tape_ccb.port_stat)
# define QIC02_CTL_PORT 	(qic02_tape_ccb.port_ctl)
# define QIC02_CMD_PORT 	(qic02_tape_ccb.port_cmd)
# define QIC02_DATA_PORT 	(qic02_tape_ccb.port_data)
# define QIC02_STAT_POLARITY	(qic02_tape_ccb.stat_polarity)
# define QIC02_STAT_READY	(qic02_tape_ccb.stat_ready)
# define QIC02_STAT_EXCEPTION	(qic02_tape_ccb.stat_exception)
# define QIC02_STAT_MASK	(qic02_tape_ccb.stat_mask)
# define QIC02_STAT_RESETMASK	(qic02_tape_ccb.stat_resetmask)
# define QIC02_STAT_RESETVAL	(qic02_tape_ccb.stat_resetval)
# define QIC02_CTL_RESET	(qic02_tape_ccb.ctl_reset)
# define QIC02_CTL_REQUEST	(qic02_tape_ccb.ctl_request)
# define TP_HAVE_DENS		(qic02_tape_dynconf.have_dens)
# define TP_HAVE_BSF		(qic02_tape_dynconf.have_bsf)
# define TP_HAVE_FSR		(qic02_tape_dynconf.have_fsr)
# define TP_HAVE_BSR		(qic02_tape_dynconf.have_bsr)
# define TP_HAVE_EOD		(qic02_tape_dynconf.have_eod)
# define TP_HAVE_SEEK		(qic02_tape_dynconf.have_seek)
# define TP_HAVE_TELL		(qic02_tape_dynconf.have_tell)
# define TP_HAVE_RAS1		(qic02_tape_dynconf.have_ras1)
# define TP_HAVE_RAS2		(qic02_tape_dynconf.have_ras2)
#endif
#define AR_QCMDV_TELL_BLK	0xAE
#define AR_QCMDV_SEEK_BLK	0xAD
#define AR_SEEK_BUF_SIZE	3
#define QCMD_SEL_1	0x01
#define QCMD_SEL_2	0x02
#define QCMD_SEL_3	0x04
#define QCMD_SEL_4	0x08
#define	QCMD_REWIND	0x21
#define QCMD_ERASE	0x22
#define QCMD_RETEN	0x24
#define	QCMD_WRT_DATA	0x40
#define	QCMD_WRT_FM	0x60
#define	QCMD_RD_DATA	0x80
#define	QCMD_RD_FM	0xA0
#define	QCMD_RD_STAT	0xC0
#define QCMD_DENS_11	0x26
#define QCMD_DENS_24	0x27
#define QCMD_DENS_120	0x28
#define QCMD_DENS_150	0x29
#define QCMD_DENS_300	0x2A
#define QCMD_DENS_600	0x2B
#define	QCMD_WRTNU_DATA	0x40
#define QCMD_SPACE_FWD	0x81
#define QCMD_SPACE_BCK	0x89
#define QCMD_RD_FM_BCK	0xA8
#define QCMD_SEEK_EOD	0xA3
#define	QCMD_RD_STAT_X1	0xC1
#define	QCMD_RD_STAT_X2	0xC4
#define	QCMD_RD_STAT_X3	0xE0
#define QCMD_SELF_TST1	0xC2
#define QCMD_SELF_TST2	0xCA
#define QFA_ENABLE	0x2D
#define QFA_DATA	0x20
#define QFA_DIR		0x23
#define QFA_RD_POS	0xCF
#define QFA_SEEK_EOD	0xA1
#define QFA_SEEK_BLK	0xAF
#define TPQD_SENSE_TEXT	0x0001
#define TPQD_SENSE_CNTS 0x0002
#define TPQD_REWIND	0x0004
#define TPQD_TERM_CYCLE	0x0008
#define TPQD_IOCTLS	0x0010
#define TPQD_DMAX	0x0020
#define TPQD_BLKSZ	0x0040
#define TPQD_MISC	0x0080
#define TPQD_DEBUG	0x0100
#define TPQD_DIAGS	0x1000
#define TPQD_ALWAYS	0x8000
#define TPQD_DEFAULT_FLAGS	0x00fc
#define TPQDBG(f)	((QIC02_TAPE_DEBUG) & (TPQD_##f))
#define	TP_REWCLOSE(d)	((MINOR(d)&0x01) == 1)
#define	TP_DENS(dev)	((MINOR(dev) >> 1) & 0x07)
#define TP_UNIT(dev)	((MINOR(dev) >> 4) & 0x07)
#define TP_DIAGS(dev)	(QIC02_TAPE_DEBUG & TPQD_DIAGS)
struct tpstatus {
unsigned short	exs;
unsigned short	dec;
unsigned short	urc;
};
#define TPSTATSIZE	sizeof(struct tpstatus)
#define	TP_POR		0x100
#define	TP_EOR		0x200
#define	TP_PAR		0x400
#define	TP_BOM		0x800
#define	TP_MBD		0x1000
#define	TP_NDT		0x2000
#define	TP_ILL		0x4000
#define	TP_ST1		0x8000
#define	TP_FIL		0x01
#define	TP_BNL		0x02
#define	TP_UDA		0x04
#define	TP_EOM		0x08
#define	TP_WRP		0x10
#define	TP_USL		0x20
#define	TP_CNI		0x40
#define	TP_ST0		0x80
#define REPORT_ERR0	(TP_CNI|TP_USL|TP_WRP|TP_EOM|TP_UDA|TP_BNL|TP_FIL)
#define REPORT_ERR1	(TP_ILL|TP_NDT|TP_MBD|TP_PAR)
#define EXC_UNKNOWN	0
#define EXC_NDRV	1
#define EXC_NCART	2
#define EXC_WP		3
#define EXC_EOM		4
#define EXC_RWA		5
#define EXC_XBAD	6
#define EXC_XFILLER	7
#define EXC_NDT		8
#define EXC_NDTEOM	9
#define EXC_NDTBOM	10
#define EXC_FM		11
#define EXC_ILL		12
#define EXC_POR		13
#define EXC_MARGINAL	14
#define EXC_EOR		15
#define EXC_BOM		16
#define TAPE_NOTIFY_TIMEOUT	1000000
#define TE_OK	0
#define TE_EX	1
#define TE_ERR	2
#define TE_NS	3
#define TE_TIM	4
#define TE_DEAD	5
#define TE_END	6
#define TIM_S	(4*HZ)
#define TIM_M	(30*HZ)
#define TIM_R	(8*60*HZ)
#define TIM_F	(2*3600*HZ)
#define TIMERON(t)	timer_table[QIC02_TAPE_TIMER].expires = jiffies + (t); \
timer_active |= (1<<QIC02_TAPE_TIMER)
#define TIMEROFF	timer_active &= ~(1<<QIC02_TAPE_TIMER)
#define TIMERCONT	timer_active |= (1<<QIC02_TAPE_TIMER)
typedef char flag;
#define NO	0
#define YES	1
#ifdef TDEBUG
# define TPQDEB(s)	s
# define TPQPUTS(s)	tpqputs(s)
#else
# define TPQDEB(s)
# define TPQPUTS(s)
#endif
#define NR_BLK_BUF	20
#define TAPE_BLKSIZE	512
#define TPQBUF_SIZE	(TAPE_BLKSIZE*NR_BLK_BUF)
#define BLOCKS_BEYOND_EW	2
#define BOGUS_IRQ		32009
struct qic02_ccb {
long	ifc_type;
unsigned short	port_stat;
unsigned short	port_ctl;
unsigned short	port_cmd;
unsigned short	port_data;
unsigned short	stat_polarity;
unsigned short	stat_ready;
unsigned short	stat_exception;
unsigned short	stat_mask;
unsigned short	stat_resetmask;
unsigned short	stat_resetval;
unsigned short	ctl_reset;
unsigned short	ctl_request;
unsigned short	dma_enable_value;
};
#if MODULE
static int qic02_tape_init(void);
#else
extern int qic02_tape_init(void);
#endif
#endif
#endif