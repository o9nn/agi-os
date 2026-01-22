#define SCSI_NCR_DEBUG_FLAGS	(0)
#define NCR_GETCC_WITHMSG
#define LinuxVersionCode(v, p, s) (((v)<<16)+((p)<<8)+(s))
#ifdef MODULE
#include <linux/module.h>
#endif
#include <asm/dma.h>
#include <asm/io.h>
#include <asm/system.h>
#include <linux/delay.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/bios32.h>
#include <linux/pci.h>
#include <linux/string.h>
#include <linux/malloc.h>
#include <linux/mm.h>
#include <linux/ioport.h>
#include <linux/time.h>
#include <linux/timer.h>
#include <linux/stat.h>
#include <linux/version.h>
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
#include <linux/blk.h>
#else
#include "../block/blk.h"
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,35)
#include <linux/init.h>
#else
#ifndef	__initdata
#define	__initdata
#endif
#ifndef	__initfunc
#define	__initfunc(__arginit) __arginit
#endif
#endif
#include "scsi.h"
#include "hosts.h"
#include "constants.h"
#include "sd.h"
#include <linux/types.h>
typedef u32 u_int32;
#include "ncr53c8xx.h"
#ifndef SCSI_NCR_MYADDR
#define SCSI_NCR_MYADDR      (7)
#endif
#ifndef SCSI_NCR_MAX_TAGS
#define SCSI_NCR_MAX_TAGS    (4)
#endif
#ifdef SCSI_NCR_MAX_TARGET
#define MAX_TARGET  (SCSI_NCR_MAX_TARGET)
#else
#define MAX_TARGET  (16)
#endif
#ifdef SCSI_NCR_MAX_LUN
#define MAX_LUN    SCSI_NCR_MAX_LUN
#else
#define MAX_LUN    (1)
#endif
#ifndef SCSI_NCR_MIN_ASYNC
#define SCSI_NCR_MIN_ASYNC (40)
#endif
#ifdef SCSI_NCR_CAN_QUEUE
#define MAX_START   (SCSI_NCR_CAN_QUEUE + 4)
#else
#define MAX_START   (MAX_TARGET + 7 * SCSI_NCR_MAX_TAGS)
#endif
#define MAX_SCATTER (SCSI_NCR_MAX_SCATTER)
#if defined(SCSI_NCR_IOMAPPED)
#define NCR_IOMAPPED
#endif
#define NCR_SNOOP_TIMEOUT (1000000)
#define printf		printk
#define u_char		unsigned char
#define u_short		unsigned short
#define u_int		unsigned int
#define u_long		unsigned long
#ifndef MACH
typedef	u_long		vm_offset_t;
typedef	int		vm_size_t;
#endif
#define bcopy(s, d, n)	memcpy((d), (s), (n))
#define bzero(d, n)	memset((d), 0, (n))
#ifndef offsetof
#define offsetof(t, m)	((size_t) (&((t *)0)->m))
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
#define vtophys(p)	virt_to_bus(p)
#ifndef NCR_IOMAPPED
__initfunc(
static vm_offset_t remap_pci_mem(u_long base, u_long size)
)
{
u_long page_base	= ((u_long) base) & PAGE_MASK;
u_long page_offs	= ((u_long) base) - page_base;
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,0)
u_long page_remapped	= (u_long) ioremap(page_base, page_offs+size);
#else
u_long page_remapped	= (u_long) vremap(page_base, page_offs+size);
#endif
return (vm_offset_t) (page_remapped ? (page_remapped + page_offs) : 0UL);
}
__initfunc(
static void unmap_pci_mem(vm_offset_t vaddr, u_long size)
)
{
if (vaddr)
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,0)
iounmap((void *) (vaddr & PAGE_MASK));
#else
vfree((void *) (vaddr & PAGE_MASK));
#endif
}
#endif
#else
#define vtophys(p)	((u_long) (p))
#endif
static void DELAY(long us)
{
for (;us>1000;us-=1000) udelay(1000);
if (us) udelay(us);
}
#define ALIGN_SIZE(shift)	(1UL << shift)
#define ALIGN_MASK(shift)	(~(ALIGN_SIZE(shift)-1))
#define NCB_ALIGN_SHIFT		5
#define CCB_ALIGN_SHIFT		5
#define LCB_ALIGN_SHIFT		5
#define SCR_ALIGN_SHIFT		5
#define NCB_ALIGN_SIZE		ALIGN_SIZE(NCB_ALIGN_SHIFT)
#define NCB_ALIGN_MASK		ALIGN_MASK(NCB_ALIGN_SHIFT)
#define CCB_ALIGN_SIZE		ALIGN_SIZE(CCB_ALIGN_SHIFT)
#define CCB_ALIGN_MASK		ALIGN_MASK(CCB_ALIGN_SHIFT)
#define SCR_ALIGN_SIZE		ALIGN_SIZE(SCR_ALIGN_SHIFT)
#define SCR_ALIGN_MASK		ALIGN_MASK(SCR_ALIGN_SHIFT)
static void *m_alloc(int size, int a_shift)
{
u_long addr;
void *ptr;
u_long a_size, a_mask;
if (a_shift < 3)
a_shift = 3;
a_size	= ALIGN_SIZE(a_shift);
a_mask	= ALIGN_MASK(a_shift);
ptr = (void *) kmalloc(size + a_size, GFP_ATOMIC);
if (ptr) {
addr	= (((u_long) ptr) + a_size) & a_mask;
*((void **) (addr - sizeof(void *))) = ptr;
ptr	= (void *) addr;
}
return ptr;
}
#ifdef MODULE
static void m_free(void *ptr, int size)
{
u_long addr;
if (ptr) {
addr	= (u_long) ptr;
ptr	= *((void **) (addr - sizeof(void *)));
kfree(ptr);
}
}
#endif
#define XferNone	0
#define XferIn		1
#define XferOut		2
#define XferBoth	3
static int guess_xfer_direction(int opcode);
static struct Scsi_Host		*first_host	= NULL;
static Scsi_Host_Template	*the_template	= NULL;
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
struct proc_dir_entry proc_scsi_ncr53c8xx = {
PROC_SCSI_NCR53C8XX, 9, "ncr53c8xx",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
# ifdef SCSI_NCR_PROC_INFO_SUPPORT
int ncr53c8xx_proc_info(char *buffer, char **start, off_t offset,
int length, int hostno, int func);
# endif
#endif
static struct {
unsigned char and_map[MAX_TARGET];
} target_capabilities[SCSI_NCR_MAX_HOST] = { NCR53C8XX_TARGET_CAPABILITIES };
struct ncr_driver_setup {
unsigned master_parity	: 1;
unsigned scsi_parity	: 1;
unsigned disconnection	: 1;
unsigned special_features : 2;
unsigned ultra_scsi	: 2;
unsigned force_sync_nego: 1;
unsigned reverse_probe: 1;
unsigned pci_fix_up: 4;
u_char	use_nvram;
u_char	verbose;
u_char	default_tags;
u_short	default_sync;
u_short	debug;
u_char	burst_max;
u_char	led_pin;
u_char	max_wide;
u_char	settle_delay;
u_char	diff_support;
u_char	irqm;
u_char	bus_check;
};
static struct ncr_driver_setup
driver_setup			= SCSI_NCR_DRIVER_SETUP;
#ifdef	SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT
static struct ncr_driver_setup
driver_safe_setup __initdata	= SCSI_NCR_DRIVER_SAFE_SETUP;
#ifdef	MODULE
char *ncr53c8xx = 0;
#endif
#endif
#define ScsiResult(host_code, scsi_code) (((host_code) << 16) + ((scsi_code) & 0x7f))
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,0,0)
static void ncr53c8xx_select_queue_depths(struct Scsi_Host *host, struct scsi_device *devlist);
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,70)
static void ncr53c8xx_intr(int irq, void *dev_id, struct pt_regs * regs);
#else
static void ncr53c8xx_intr(int irq, struct pt_regs * regs);
#endif
static void ncr53c8xx_timeout(unsigned long np);
#define initverbose (driver_setup.verbose)
#define bootverbose (np->verbose)
#ifdef SCSI_NCR_NVRAM_SUPPORT
#define SYMBIOS_NVRAM_SIZE 368
#define SYMBIOS_NVRAM_ADDRESS 0x100
struct Symbios_nvram {
u_short start_marker;
u_short byte_count;
u_short checksum;
u_short	word0;
u_short	word2;
u_short	word4;
u_short	flags;
#define SYMBIOS_SCAM_ENABLE	(1)
#define SYMBIOS_PARITY_ENABLE	(1<<1)
#define SYMBIOS_VERBOSE_MSGS	(1<<2)
u_short	flags1;
#define SYMBIOS_SCAN_HI_LO	(1)
u_short	word10;
u_short	flags3;
#define SYMBIOS_REMOVABLE_FLAGS	(3)
u_char	host_id;
u_char	byte15;
u_short	word16;
u_short	word18;
struct Symbios_host{
u_char	word0;
u_short	device_id;
u_short	vendor_id;
u_char	byte6;
u_char	device_fn;
u_short	word8;
u_short	flags;
#define	SYMBIOS_INIT_SCAN_AT_BOOT	(1)
u_short	io_port;
} host[4];
struct Symbios_target {
u_short	flags;
#define SYMBIOS_DISCONNECT_ENABLE	(1)
#define SYMBIOS_SCAN_AT_BOOT_TIME	(1<<1)
#define SYMBIOS_SCAN_LUNS		(1<<2)
#define SYMBIOS_QUEUE_TAGS_ENABLED	(1<<3)
u_char	bus_width;
u_char	sync_offset;
u_char	sync_period;
u_char	byte6;
u_short	timeout;
} target[16];
u_char	spare_devices[19*8];
u_char	trailer[6];
};
typedef struct Symbios_nvram	Symbios_nvram;
typedef struct Symbios_host	Symbios_host;
typedef struct Symbios_target	Symbios_target;
#define TEKRAM_NVRAM_SIZE 64
#define TEKRAM_NVRAM_ADDRESS 0
struct Tekram_nvram {
struct Tekram_target {
u_char	flags;
#define	TEKRAM_PARITY_CHECK		(1)
#define TEKRAM_SYNC_NEGO		(1<<1)
#define TEKRAM_DISCONNECT_ENABLE	(1<<2)
#define	TEKRAM_START_CMD		(1<<3)
#define TEKRAM_TAGGED_COMMANDS		(1<<4)
#define TEKRAM_WIDE_NEGO		(1<<5)
u_char	sync_index;
u_short	word2;
} target[16];
u_char	host_id;
u_char	flags;
#define TEKRAM_MORE_THAN_2_DRIVES	(1)
#define TEKRAM_DRIVES_SUP_1GB		(1<<1)
#define	TEKRAM_RESET_ON_POWER_ON	(1<<2)
#define TEKRAM_ACTIVE_NEGATION		(1<<3)
#define TEKRAM_IMMEDIATE_SEEK		(1<<4)
#define	TEKRAM_SCAN_LUNS		(1<<5)
#define	TEKRAM_REMOVABLE_FLAGS		(3<<6)
u_char	boot_delay_index;
u_char	max_tags_index;
u_short	flags1;
#define TEKRAM_F2_F6_ENABLED		(1)
u_short	spare[29];
};
typedef struct Tekram_nvram	Tekram_nvram;
typedef struct Tekram_target	Tekram_target;
static u_char Tekram_sync[12] __initdata = {25,31,37,43,50,62,75,125,12,15,18,21};
#endif
typedef struct {
int	bus;
u_char	device_fn;
u_int	base;
u_int	base_2;
u_int	io_port;
int	irq;
u_int	port;
volatile struct	ncr_reg	*reg;
} ncr_slot;
typedef struct {
int type;
#define	SCSI_NCR_SYMBIOS_NVRAM	(1)
#define	SCSI_NCR_TEKRAM_NVRAM	(2)
#ifdef	SCSI_NCR_NVRAM_SUPPORT
union {
Symbios_nvram Symbios;
Tekram_nvram Tekram;
} data;
#endif
} ncr_nvram;
typedef struct {
ncr_slot  slot;
ncr_chip  chip;
ncr_nvram *nvram;
int attach_done;
} ncr_device;
#define DEBUG_ALLOC    (0x0001)
#define DEBUG_PHASE    (0x0002)
#define DEBUG_POLL     (0x0004)
#define DEBUG_QUEUE    (0x0008)
#define DEBUG_RESULT   (0x0010)
#define DEBUG_SCATTER  (0x0020)
#define DEBUG_SCRIPT   (0x0040)
#define DEBUG_TINY     (0x0080)
#define DEBUG_TIMING   (0x0100)
#define DEBUG_NEGO     (0x0200)
#define DEBUG_TAGS     (0x0400)
#define DEBUG_FREEZE   (0x0800)
#define DEBUG_RESTART  (0x1000)
#ifdef SCSI_NCR_DEBUG_INFO_SUPPORT
#define DEBUG_FLAGS ncr_debug
#else
#define DEBUG_FLAGS	SCSI_NCR_DEBUG_FLAGS
#endif
#define	assert(expression) { \
if (!(expression)) { \
(void)printf(\
"assertion \"%s\" failed: file \"%s\", line %d\n", \
#expression, \
__FILE__, __LINE__); \
} \
}
#if	defined(SCSI_NCR_BIG_ENDIAN)
#define ncr_offb(o)	(((o)&~3)+((~((o)&3))&3))
#define ncr_offw(o)	(((o)&~3)+((~((o)&3))&2))
#else
#define ncr_offb(o)	(o)
#define ncr_offw(o)	(o)
#endif
#if	defined(__BIG_ENDIAN) && !defined(SCSI_NCR_BIG_ENDIAN)
#define cpu_to_scr(dw)	cpu_to_le32(dw)
#define scr_to_cpu(dw)	le32_to_cpu(dw)
#elif	defined(__LITTLE_ENDIAN) && defined(SCSI_NCR_BIG_ENDIAN)
#define cpu_to_scr(dw)	cpu_to_be32(dw)
#define scr_to_cpu(dw)	be32_to_cpu(dw)
#else
#define cpu_to_scr(dw)	(dw)
#define scr_to_cpu(dw)	(dw)
#endif
#if defined(NCR_IOMAPPED)
#define	INB_OFF(o)		inb (np->port + ncr_offb(o))
#define	OUTB_OFF(o, val)	outb ((val), np->port + ncr_offb(o))
#if	defined(__BIG_ENDIAN) && !defined(SCSI_NCR_BIG_ENDIAN)
#define	INW_OFF(o)		inw_l2b (np->port + ncr_offw(o))
#define	INL_OFF(o)		inl_l2b (np->port + (o))
#define	OUTW_OFF(o, val)	outw_b2l ((val), np->port + ncr_offw(o))
#define	OUTL_OFF(o, val)	outl_b2l ((val), np->port + (o))
#elif	defined(__LITTLE_ENDIAN) && defined(SCSI_NCR_BIG_ENDIAN)
#define	INW_OFF(o)		inw_b2l (np->port + ncr_offw(o))
#define	INL_OFF(o)		inl_b2l (np->port + (o))
#define	OUTW_OFF(o, val)	outw_l2b ((val), np->port + ncr_offw(o))
#define	OUTL_OFF(o, val)	outl_l2b ((val), np->port + (o))
#else
#define	INW_OFF(o)		inw_raw (np->port + ncr_offw(o))
#define	INL_OFF(o)		inl_raw (np->port + (o))
#define	OUTW_OFF(o, val)	outw_raw ((val), np->port + ncr_offw(o))
#define	OUTL_OFF(o, val)	outl_raw ((val), np->port + (o))
#endif
#else
#define INB_OFF(o)		readb((char *)np->reg + ncr_offb(o))
#define OUTB_OFF(o, val)	writeb((val), (char *)np->reg + ncr_offb(o))
#if	defined(__BIG_ENDIAN) && !defined(SCSI_NCR_BIG_ENDIAN)
#define INW_OFF(o)		readw_l2b((char *)np->reg + ncr_offw(o))
#define INL_OFF(o)		readl_l2b((char *)np->reg + (o))
#define OUTW_OFF(o, val)	writew_b2l((val), (char *)np->reg + ncr_offw(o))
#define OUTL_OFF(o, val)	writel_b2l((val), (char *)np->reg + (o))
#elif	defined(__LITTLE_ENDIAN) && defined(SCSI_NCR_BIG_ENDIAN)
#define INW_OFF(o)		readw_b2l((char *)np->reg + ncr_offw(o))
#define INL_OFF(o)		readl_b2l((char *)np->reg + (o))
#define OUTW_OFF(o, val)	writew_l2b((val), (char *)np->reg + ncr_offw(o))
#define OUTL_OFF(o, val)	writel_l2b((val), (char *)np->reg + (o))
#else
#define INW_OFF(o)		readw_raw((char *)np->reg + ncr_offw(o))
#define INL_OFF(o)		readl_raw((char *)np->reg + (o))
#define OUTW_OFF(o, val)	writew_raw((val), (char *)np->reg + ncr_offw(o))
#define OUTL_OFF(o, val)	writel_raw((val), (char *)np->reg + (o))
#endif
#endif
#define INB(r)		INB_OFF (offsetof(struct ncr_reg,r))
#define INW(r)		INW_OFF (offsetof(struct ncr_reg,r))
#define INL(r)		INL_OFF (offsetof(struct ncr_reg,r))
#define OUTB(r, val)	OUTB_OFF (offsetof(struct ncr_reg,r), (val))
#define OUTW(r, val)	OUTW_OFF (offsetof(struct ncr_reg,r), (val))
#define OUTL(r, val)	OUTL_OFF (offsetof(struct ncr_reg,r), (val))
#define OUTONB(r, m)	OUTB(r, INB(r) | (m))
#define OUTOFFB(r, m)	OUTB(r, INB(r) & ~(m))
#define OUTONW(r, m)	OUTW(r, INW(r) | (m))
#define OUTOFFW(r, m)	OUTW(r, INW(r) & ~(m))
#define OUTONL(r, m)	OUTL(r, INL(r) | (m))
#define OUTOFFL(r, m)	OUTL(r, INL(r) & ~(m))
#define HS_IDLE		(0)
#define HS_BUSY		(1)
#define HS_NEGOTIATE	(2)
#define HS_DISCONNECT	(3)
#define HS_COMPLETE	(4)
#define HS_SEL_TIMEOUT	(5)
#define HS_RESET	(6)
#define HS_ABORTED	(7)
#define HS_TIMEOUT	(8)
#define HS_FAIL		(9)
#define HS_UNEXPECTED	(10)
#define HS_DONEMASK	(0xfc)
#define	SIR_SENSE_RESTART	(1)
#define	SIR_SENSE_FAILED	(2)
#define	SIR_STALL_RESTART	(3)
#define	SIR_STALL_QUEUE		(4)
#define	SIR_NEGO_SYNC		(5)
#define	SIR_NEGO_WIDE		(6)
#define	SIR_NEGO_FAILED		(7)
#define	SIR_NEGO_PROTO		(8)
#define	SIR_REJECT_RECEIVED	(9)
#define	SIR_REJECT_SENT		(10)
#define	SIR_IGN_RESIDUE		(11)
#define	SIR_MISSING_SAVE	(12)
#define	SIR_DATA_IO_IS_OUT	(13)
#define	SIR_DATA_IO_IS_IN	(14)
#define	SIR_MAX			(14)
#define	XE_OK		(0)
#define	XE_EXTRA_DATA	(1)
#define	XE_BAD_PHASE	(2)
#define NS_SYNC		(1)
#define NS_WIDE		(2)
#define	QUIRK_AUTOSAVE	(0x01)
#define	QUIRK_NOMSG	(0x02)
#define QUIRK_NOSYNC	(0x10)
#define QUIRK_NOWIDE16	(0x20)
#define	QUIRK_UPDATE	(0x80)
#define	INQ7_QUEUE	(0x02)
#define	INQ7_SYNC	(0x10)
#define	INQ7_WIDE16	(0x20)
#define CCB_MAGIC	(0xf2691ad2)
struct tcb;
struct lcb;
struct ccb;
struct ncb;
struct script;
typedef struct ncb * ncb_p;
typedef struct tcb * tcb_p;
typedef struct lcb * lcb_p;
typedef struct ccb * ccb_p;
struct link {
ncrcmd	l_cmd;
ncrcmd	l_paddr;
};
struct	usrcmd {
u_long	target;
u_long	lun;
u_long	data;
u_long	cmd;
};
#define UC_SETSYNC      10
#define UC_SETTAGS	11
#define UC_SETDEBUG	12
#define UC_SETORDER	13
#define UC_SETWIDE	14
#define UC_SETFLAG	15
#define UC_CLEARPROF	16
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
#define UC_DEBUG_ERROR_RECOVERY 17
#endif
#define	UF_TRACE	(0x01)
#define	UF_NODISC	(0x02)
#define	UF_NOSCAN	(0x04)
struct tstamp {
u_long start;
u_long end;
u_long select;
u_long command;
u_long status;
u_long disconnect;
u_long reselect;
};
struct profile {
u_long	num_trans;
u_long	num_kbytes;
u_long	rest_bytes;
u_long	num_disc;
u_long	num_break;
u_long	num_int;
u_long	num_fly;
u_long	ms_setup;
u_long	ms_data;
u_long	ms_disc;
u_long	ms_post;
};
struct tcb {
struct link   jump_tcb;
ncrcmd	getscr[6];
struct link   call_lun;
struct link   jump_lcb;
ccb_p   hold_cp;
ccb_p   nego_cp;
u_long	transfers;
u_long	bytes;
u_char	usrsync;
u_char	usrwide;
u_char	usrtags;
u_char	usrflag;
u_char	numtags;
u_char	maxtags;
u_short	num_good;
u_char	minsync;
u_char	sval;
u_short	period;
u_char	maxoffs;
u_char	quirks;
u_char	widedone;
u_char	wval;
#define MAX_INQUIRE 36
u_char	inqdata[MAX_INQUIRE];
lcb_p   lp[MAX_LUN];
};
struct lcb {
struct link	jump_lcb;
struct link	call_tag;
struct link	jump_ccb;
ccb_p	next_ccb;
u_char		reqccbs;
u_char		actccbs;
u_char		reqlink;
u_char		actlink;
u_char		usetags;
u_char		lasttag;
u_char		active;
u_char		opennings;
u_char	force_ordered_tag;
#define NCR_TIMEOUT_INCREASE	(5*HZ)
};
struct head {
struct link	launch;
u_int32		savep;
u_int32		lastp;
u_int32		goalp;
ccb_p	cp;
struct tstamp	stamp;
u_char		scr_st[4];
u_char		status[4];
};
#define  QU_REG	scr0
#define  HS_REG	scr1
#define  HS_PRT	nc_scr1
#define  SS_REG	scr2
#define  PS_REG	scr3
#define  actualquirks  phys.header.status[0]
#define  host_status   phys.header.status[1]
#define  scsi_status   phys.header.status[2]
#define  parity_status phys.header.status[3]
#define  xerr_st       header.scr_st[0]
#define  sync_st       header.scr_st[1]
#define  nego_st       header.scr_st[2]
#define  wide_st       header.scr_st[3]
#define  xerr_status   phys.xerr_st
#define  sync_status   phys.sync_st
#define  nego_status   phys.nego_st
#define  wide_status   phys.wide_st
struct dsb {
struct head	header;
struct scr_tblsel  select;
struct scr_tblmove smsg  ;
struct scr_tblmove smsg2 ;
struct scr_tblmove cmd   ;
struct scr_tblmove scmd  ;
struct scr_tblmove sense ;
struct scr_tblmove data [MAX_SCATTER];
};
struct ccb {
struct link		filler[2];
struct link		jump_ccb;
struct link		call_tmp;
struct dsb		phys;
ncrcmd			patch[8];
Scsi_Cmnd	*cmd;
int data_len;
u_char			scsi_smsg [8];
u_char			scsi_smsg2[8];
u_long		magic;
u_long		p_ccb;
u_long		tlimit;
ccb_p		link_ccb;
ccb_p		next_ccb;
u_char		sensecmd[6];
u_char			tag;
u_char			segments;
};
#define CCB_PHYS(cp,lbl)	(cp->p_ccb + offsetof(struct ccb, lbl))
struct ncb {
struct head     header;
int    unit;
char   chip_name[8];
char   inst_name[16];
struct timer_list timer;
int	ncr_cache;
Scsi_Cmnd *waiting_list;
u_long	settle_time;
u_char	release_stage;
u_char	verbose;
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
u_char	debug_error_recovery;
u_char	stalling;
u_char	assert_atn;
#endif
u_short	device_id;
u_char	revision_id;
u_char	sv_scntl0;
u_char	sv_scntl3;
u_char	sv_dmode;
u_char	sv_dcntl;
u_char	sv_ctest3;
u_char	sv_ctest4;
u_char	sv_ctest5;
u_char	sv_gpcntl;
u_char	sv_stest2;
u_char	sv_stest4;
u_char	rv_scntl0;
u_char	rv_scntl3;
u_char	rv_dmode;
u_char	rv_dcntl;
u_char	rv_ctest3;
u_char	rv_ctest4;
u_char	rv_ctest5;
u_char	rv_stest2;
u_char	scsi_mode;
struct link     jump_tcb;
vm_offset_t     vaddr;
vm_offset_t     paddr;
vm_offset_t     vaddr2;
vm_offset_t     paddr2;
volatile
struct ncr_reg* reg;
struct script	*script0;
struct scripth	*scripth0;
struct script	*script;
struct scripth	*scripth;
u_long		p_script;
u_long		p_scripth;
u_char	  myaddr;
u_char		maxburst;
u_char		minsync;
u_char		maxsync;
u_char		maxoffs;
u_char		multiplier;
u_char		clock_divn;
u_long		clock_khz;
u_int		features;
struct usrcmd	user;
u_char		order;
struct tcb	target[MAX_TARGET];
u_int32		squeue [MAX_START];
u_short		squeueput;
u_short		actccbs;
#if 0
u_long		heartbeat;
u_short		ticks;
u_short		latetime;
#endif
u_long		lasttime;
struct ncr_reg	regdump;
u_long		regtime;
struct profile	profile;
u_long		disc_phys;
u_long		disc_ref;
struct ccb      *ccb;
u_char		msgout[8];
u_char		msgin [8];
u_int32		lastmsg;
u_char		scratch;
u_char		maxwide;
u_char		disc;
u_int		port;
u_short		irq;
};
#define NCB_SCRIPT_PHYS(np,lbl)	 (np->p_script  + offsetof (struct script, lbl))
#define NCB_SCRIPTH_PHYS(np,lbl) (np->p_scripth + offsetof (struct scripth, lbl))
struct script {
ncrcmd	start		[  4];
ncrcmd	start0		[  2];
ncrcmd	start1		[  3];
ncrcmd  startpos	[  1];
ncrcmd  trysel		[  8];
ncrcmd	skip		[  8];
ncrcmd	skip2		[  3];
ncrcmd  idle		[  2];
ncrcmd	select		[ 22];
ncrcmd	prepare		[  4];
ncrcmd	loadpos		[ 14];
ncrcmd	prepare2	[ 24];
ncrcmd	setmsg		[  5];
ncrcmd  clrack		[  2];
ncrcmd  dispatch	[ 38];
ncrcmd	no_data		[ 17];
ncrcmd  checkatn	[ 10];
ncrcmd  command		[ 15];
ncrcmd  status		[ 27];
ncrcmd  msg_in		[ 26];
ncrcmd  msg_bad		[  6];
ncrcmd  complete	[ 13];
ncrcmd	cleanup		[ 12];
ncrcmd	cleanup0	[ 11];
ncrcmd	signal		[ 10];
ncrcmd  save_dp		[  5];
ncrcmd  restore_dp	[  5];
ncrcmd  disconnect	[ 12];
ncrcmd  disconnect0	[  5];
ncrcmd  disconnect1	[ 23];
ncrcmd	msg_out		[  9];
ncrcmd	msg_out_done	[  7];
ncrcmd  badgetcc	[  6];
ncrcmd	reselect	[  8];
ncrcmd	reselect1	[  8];
ncrcmd	reselect2	[  8];
ncrcmd	resel_tmp	[  5];
ncrcmd  resel_lun	[ 18];
ncrcmd	resel_tag	[ 24];
ncrcmd  data_io		[  6];
ncrcmd  data_in		[MAX_SCATTER * 4 + 4];
};
struct scripth {
ncrcmd  tryloop		[MAX_START*5+2];
ncrcmd  msg_parity	[  6];
ncrcmd	msg_reject	[  8];
ncrcmd	msg_ign_residue	[ 32];
ncrcmd  msg_extended	[ 18];
ncrcmd  msg_ext_2	[ 18];
ncrcmd	msg_wdtr	[ 27];
ncrcmd  msg_ext_3	[ 18];
ncrcmd	msg_sdtr	[ 27];
ncrcmd	msg_out_abort	[ 10];
ncrcmd  getcc		[  4];
ncrcmd  getcc1		[  5];
#ifdef NCR_GETCC_WITHMSG
ncrcmd	getcc2		[ 33];
#else
ncrcmd	getcc2		[ 14];
#endif
ncrcmd	getcc3		[ 10];
ncrcmd  data_out	[MAX_SCATTER * 4 + 4];
ncrcmd	aborttag	[  4];
ncrcmd	abort		[ 22];
ncrcmd	snooptest	[  9];
ncrcmd	snoopend	[  2];
};
static	void	ncr_alloc_ccb	(ncb_p np, u_long t, u_long l);
static	void	ncr_complete	(ncb_p np, ccb_p cp);
static	void	ncr_exception	(ncb_p np);
static	void	ncr_free_ccb	(ncb_p np, ccb_p cp, u_long t, u_long l);
static	void	ncr_getclock	(ncb_p np, int mult);
static	void	ncr_selectclock	(ncb_p np, u_char scntl3);
static	ccb_p	ncr_get_ccb	(ncb_p np, u_long t,u_long l);
static	void	ncr_init	(ncb_p np, int reset, char * msg, u_long code);
static	int	ncr_int_sbmc	(ncb_p np);
static	int	ncr_int_par	(ncb_p np);
static	void	ncr_int_ma	(ncb_p np);
static	void	ncr_int_sir	(ncb_p np);
static  void    ncr_int_sto     (ncb_p np);
static	u_long	ncr_lookup	(char* id);
static	void	ncr_negotiate	(struct ncb* np, struct tcb* tp);
static	void	ncr_opennings	(ncb_p np, lcb_p lp, Scsi_Cmnd * xp);
#ifdef SCSI_NCR_PROFILE_SUPPORT
static	void	ncb_profile	(ncb_p np, ccb_p cp);
#endif
static	void	ncr_script_copy_and_bind
(ncb_p np, ncrcmd *src, ncrcmd *dst, int len);
static  void    ncr_script_fill (struct script * scr, struct scripth * scripth);
static	int	ncr_scatter	(ccb_p cp, Scsi_Cmnd *cmd);
static	void	ncr_setmaxtags	(ncb_p np, tcb_p tp, u_long numtags);
static	void	ncr_getsync	(ncb_p np, u_char sfac, u_char *fakp, u_char *scntl3p);
static	void	ncr_setsync	(ncb_p np, ccb_p cp, u_char scntl3, u_char sxfer);
static	void	ncr_settags     (tcb_p tp, lcb_p lp);
static	void	ncr_setwide	(ncb_p np, ccb_p cp, u_char wide, u_char ack);
static	int	ncr_show_msg	(u_char * msg);
static	int	ncr_snooptest	(ncb_p np);
static	void	ncr_timeout	(ncb_p np);
static  void    ncr_wakeup      (ncb_p np, u_long code);
static	void	ncr_start_reset	(ncb_p np, int settle_delay);
static	int	ncr_reset_scsi_bus (ncb_p np, int enab_int, int settle_delay);
#ifdef SCSI_NCR_USER_COMMAND_SUPPORT
static	void	ncr_usercmd	(ncb_p np);
#endif
static int ncr_attach (Scsi_Host_Template *tpnt, int unit, ncr_device *device);
static void insert_into_waiting_list(ncb_p np, Scsi_Cmnd *cmd);
static Scsi_Cmnd *retrieve_from_waiting_list(int to_remove, ncb_p np, Scsi_Cmnd *cmd);
static void process_waiting_list(ncb_p np, int sts);
#define remove_from_waiting_list(np, cmd) \
retrieve_from_waiting_list(1, (np), (cmd))
#define requeue_waiting_list(np) process_waiting_list((np), DID_OK)
#define reset_waiting_list(np) process_waiting_list((np), DID_RESET)
#ifdef SCSI_NCR_NVRAM_SUPPORT
static	int	ncr_get_Symbios_nvram	(ncr_slot *np, Symbios_nvram *nvram);
static	int	ncr_get_Tekram_nvram	(ncr_slot *np, Tekram_nvram *nvram);
#endif
#ifdef SCSI_NCR_DEBUG_INFO_SUPPORT
static int ncr_debug = SCSI_NCR_DEBUG_FLAGS;
#endif
static inline char *ncr_name (ncb_p np)
{
return np->inst_name;
}
#define	RELOC_SOFTC	0x40000000
#define	RELOC_LABEL	0x50000000
#define	RELOC_REGISTER	0x60000000
#define	RELOC_KVAR	0x70000000
#define	RELOC_LABELH	0x80000000
#define	RELOC_MASK	0xf0000000
#define	NADDR(label)	(RELOC_SOFTC | offsetof(struct ncb, label))
#define PADDR(label)    (RELOC_LABEL | offsetof(struct script, label))
#define PADDRH(label)   (RELOC_LABELH | offsetof(struct scripth, label))
#define	RADDR(label)	(RELOC_REGISTER | REG(label))
#define	FADDR(label,ofs)(RELOC_REGISTER | ((REG(label))+(ofs)))
#define	KVAR(which)	(RELOC_KVAR | (which))
#define	SCRIPT_KVAR_JIFFIES	(0)
#define	SCRIPT_KVAR_FIRST		SCRIPT_KVAR_JIFFIES
#define	SCRIPT_KVAR_LAST		SCRIPT_KVAR_JIFFIES
static void *script_kvars[] __initdata =
{ (void *)&jiffies };
static	struct script script0 __initdata = {
{
#if 0
SCR_COPY (sizeof (((struct ncb *)0)->heartbeat)),
KVAR(SCRIPT_KVAR_JIFFIES),
NADDR (heartbeat),
#endif
SCR_LOAD_REG (dsa, 0xff),
0,
SCR_FROM_REG (ctest2),
0,
},{
SCR_INT ^ IFFALSE (0),
SIR_SENSE_RESTART,
},{
SCR_INT ^ IFFALSE (0),
SIR_STALL_RESTART,
SCR_JUMP,
},{
PADDRH(tryloop),
},{
SCR_COPY (4),
RADDR (temp),
RADDR (scratcha),
SCR_COPY (4),
RADDR (dsa),
RADDR (temp),
SCR_RETURN,
0
},{
SCR_COPY (4),
RADDR (scratcha),
PADDR (startpos),
SCR_COPY_F (4),
RADDR (dsa),
PADDR (skip2),
SCR_COPY (8),
PADDR (idle),
},{
0,
SCR_JUMP,
PADDR(start),
},{
SCR_JUMP,
PADDR(reselect),
},{
SCR_CLR (SCR_TRG),
0,
SCR_LOAD_REG (HS_REG, 0xff),
0,
SCR_SEL_TBL_ATN ^ offsetof (struct dsb, select),
PADDR (reselect),
SCR_JUMPR ^ IFTRUE (WHEN (SCR_MSG_IN)),
0,
SCR_FROM_REG (sdid),
0,
SCR_TO_REG (ctest0),
0,
SCR_MOVE_TBL ^ SCR_MSG_OUT,
offsetof (struct dsb, smsg),
#ifdef undef
SCR_JUMPR ^ IFTRUE (WHEN (SCR_MSG_OUT)),
-16,
#endif
SCR_CLR (SCR_ATN),
0,
SCR_COPY (1),
RADDR (sfbr),
NADDR (lastmsg),
SCR_COPY (4),
RADDR (scratcha),
PADDR (startpos),
},{
SCR_COPY_F (4),
RADDR (dsa),
PADDR (loadpos),
SCR_COPY (sizeof (struct head)),
},{
0,
NADDR (header),
SCR_COPY (8),
PADDR (idle),
NADDR (header.launch),
SCR_COPY (sizeof (u_long)),
KVAR(SCRIPT_KVAR_JIFFIES),
NADDR (header.stamp.select),
SCR_COPY (4),
NADDR (header.savep),
RADDR (temp),
SCR_COPY (4),
NADDR (header.status),
RADDR (scr0),
},{
SCR_COPY (1),
NADDR (sync_st),
RADDR (sxfer),
SCR_COPY (1),
NADDR (wide_st),
RADDR (scntl3),
SCR_LOAD_REG (scratcha, M_NOOP),
0,
SCR_COPY (1),
RADDR (scratcha),
NADDR (msgout),
SCR_COPY (1),
RADDR (scratcha),
NADDR (msgin),
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_FROM_REG (sbdl),
0,
SCR_JUMP ^ IFTRUE (DATA (M_EXTENDED)),
PADDR (msg_in),
SCR_JUMP ^ IFTRUE (DATA (M_REJECT)),
PADDRH (msg_reject),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_COPY (1),
RADDR (scratcha),
NADDR (msgout),
SCR_SET (SCR_ATN),
0,
},{
SCR_CLR (SCR_ACK),
0,
},{
SCR_FROM_REG (HS_REG),
0,
SCR_INT ^ IFTRUE (DATA (HS_NEGOTIATE)),
SIR_NEGO_FAILED,
SCR_REG_REG (socl, SCR_AND, CACK|CATN),
0,
SCR_RETURN ^ IFTRUE (WHEN (SCR_DATA_OUT)),
0,
SCR_JUMPR ^ IFFALSE (IF (SCR_DATA_IN)),
20,
SCR_COPY (4),
RADDR (scratcha),
RADDR (scratcha),
SCR_RETURN,
0,
SCR_JUMP ^ IFTRUE (IF (SCR_MSG_OUT)),
PADDR (msg_out),
SCR_JUMP ^ IFTRUE (IF (SCR_MSG_IN)),
PADDR (msg_in),
SCR_JUMP ^ IFTRUE (IF (SCR_COMMAND)),
PADDR (command),
SCR_JUMP ^ IFTRUE (IF (SCR_STATUS)),
PADDR (status),
SCR_LOAD_REG (scratcha, XE_BAD_PHASE),
0,
SCR_COPY (1),
RADDR (scratcha),
NADDR (xerr_st),
SCR_JUMPR ^ IFFALSE (IF (SCR_ILG_OUT)),
8,
SCR_MOVE_ABS (1) ^ SCR_ILG_OUT,
NADDR (scratch),
SCR_JUMPR ^ IFFALSE (IF (SCR_ILG_IN)),
8,
SCR_MOVE_ABS (1) ^ SCR_ILG_IN,
NADDR (scratch),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_LOAD_REG (scratcha, XE_EXTRA_DATA),
0,
SCR_COPY (1),
RADDR (scratcha),
NADDR (xerr_st),
SCR_JUMPR ^ IFFALSE (WHEN (SCR_DATA_OUT)),
8,
SCR_MOVE_ABS (1) ^ SCR_DATA_OUT,
NADDR (scratch),
SCR_JUMPR ^ IFFALSE (IF (SCR_DATA_IN)),
8,
SCR_MOVE_ABS (1) ^ SCR_DATA_IN,
NADDR (scratch),
SCR_CALL,
PADDR (dispatch),
SCR_JUMP,
PADDR (no_data),
},{
SCR_FROM_REG (socl),
0,
SCR_JUMP ^ IFFALSE (MASK (CATN, CATN)),
PADDR (dispatch),
SCR_REG_REG (PS_REG, SCR_ADD, 1),
0,
SCR_LOAD_REG (scratcha, M_ID_ERROR),
0,
SCR_JUMP,
PADDR (setmsg),
},{
SCR_FROM_REG (SS_REG),
0,
SCR_JUMPR ^ IFTRUE (DATA (S_CHECK_COND)),
28,
SCR_COPY (sizeof (u_long)),
KVAR(SCRIPT_KVAR_JIFFIES),
NADDR (header.stamp.command),
SCR_MOVE_TBL ^ SCR_COMMAND,
offsetof (struct dsb, cmd),
SCR_JUMP,
PADDR (dispatch),
SCR_MOVE_TBL ^ SCR_COMMAND,
offsetof (struct dsb, scmd),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_COPY (sizeof (u_long)),
KVAR(SCRIPT_KVAR_JIFFIES),
NADDR (header.stamp.status),
SCR_FROM_REG (SS_REG),
0,
SCR_JUMPR ^ IFFALSE (DATA (S_CHECK_COND)),
40,
SCR_MOVE_ABS (1) ^ SCR_STATUS,
NADDR (scratch),
SCR_TO_REG (SS_REG),
0,
SCR_REG_REG (SS_REG, SCR_OR, S_SENSE),
0,
SCR_LOAD_REG (HS_REG, HS_COMPLETE),
0,
SCR_JUMP,
PADDR (checkatn),
SCR_MOVE_ABS (1) ^ SCR_STATUS,
NADDR (scratch),
SCR_TO_REG (SS_REG),
0,
SCR_JUMP ^ IFTRUE (DATA (S_CHECK_COND)),
PADDR (checkatn),
SCR_LOAD_REG (HS_REG, HS_COMPLETE),
0,
SCR_JUMP,
PADDR (checkatn),
},{
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[0]),
SCR_TO_REG (scratcha),
0,
SCR_FROM_REG (socl),
0,
SCR_JUMP ^ IFTRUE (MASK (CATN, CATN)),
PADDRH (msg_parity),
SCR_FROM_REG (scratcha),
0,
SCR_JUMP ^ IFTRUE (DATA (M_COMPLETE)),
PADDR (complete),
SCR_JUMP ^ IFTRUE (DATA (M_SAVE_DP)),
PADDR (save_dp),
SCR_JUMP ^ IFTRUE (DATA (M_RESTORE_DP)),
PADDR (restore_dp),
SCR_JUMP ^ IFTRUE (DATA (M_DISCONNECT)),
PADDR (disconnect),
SCR_JUMP ^ IFTRUE (DATA (M_EXTENDED)),
PADDRH (msg_extended),
SCR_JUMP ^ IFTRUE (DATA (M_NOOP)),
PADDR (clrack),
SCR_JUMP ^ IFTRUE (DATA (M_REJECT)),
PADDRH (msg_reject),
SCR_JUMP ^ IFTRUE (DATA (M_IGN_RESIDUE)),
PADDRH (msg_ign_residue),
},{
SCR_INT,
SIR_REJECT_SENT,
SCR_LOAD_REG (scratcha, M_REJECT),
0,
SCR_JUMP,
PADDR (setmsg),
},{
SCR_FROM_REG (SS_REG),
0,
SCR_JUMPR ^ IFTRUE (MASK (S_SENSE, S_SENSE)),
12,
SCR_COPY (4),
RADDR (temp),
NADDR (header.lastp),
SCR_REG_REG (scntl2, SCR_AND, 0x7f),
0,
SCR_CLR (SCR_ACK|SCR_ATN),
0,
SCR_WAIT_DISC,
0,
},{
SCR_FROM_REG (dsa),
0,
SCR_JUMP ^ IFTRUE (DATA (0xff)),
PADDR (signal),
SCR_COPY (4),
RADDR (scr0),
NADDR (header.status),
SCR_COPY_F (4),
RADDR (dsa),
PADDR (cleanup0),
SCR_COPY (sizeof (struct head)),
NADDR (header),
},{
0,
SCR_FROM_REG (HS_REG),
0,
SCR_JUMPR ^ IFFALSE (MASK (0, HS_DONEMASK)),
16,
SCR_FROM_REG (SS_REG),
0,
SCR_JUMP ^ IFTRUE (DATA (S_CHECK_COND)),
PADDRH(getcc2),
SCR_LOAD_REG (dsa, 0xff),
0,
},{
SCR_FROM_REG (SS_REG),
0,
SCR_INT ^ IFTRUE (DATA (S_QUEUE_FULL)),
SIR_STALL_QUEUE,
SCR_FROM_REG (HS_REG),
0,
SCR_INT_FLY ^ IFFALSE (MASK (0, HS_DONEMASK)),
0,
SCR_JUMP,
PADDR(start),
},{
SCR_COPY (4),
RADDR (temp),
NADDR (header.savep),
SCR_JUMP,
PADDR (clrack),
},{
SCR_COPY (4),
NADDR (header.savep),
RADDR (temp),
SCR_JUMP,
PADDR (clrack),
},{
SCR_FROM_REG (QU_REG),
0,
SCR_JUMPR ^ IFFALSE (MASK (QUIRK_AUTOSAVE, QUIRK_AUTOSAVE)),
12,
SCR_COPY (4),
RADDR (temp),
NADDR (header.savep),
SCR_FROM_REG (temp),
0,
SCR_COPY_F (1),
NADDR (header.savep),
PADDR (disconnect0),
},{
SCR_JUMPR ^ IFTRUE (DATA (1)),
20,
SCR_COPY_F (1),
NADDR (header.goalp),
PADDR (disconnect1),
},{
SCR_INT ^ IFFALSE (DATA (1)),
SIR_MISSING_SAVE,
SCR_REG_REG (scntl2, SCR_AND, 0x7f),
0,
SCR_CLR (SCR_ACK|SCR_ATN),
0,
SCR_WAIT_DISC,
0,
SCR_COPY (sizeof (u_long)),
KVAR(SCRIPT_KVAR_JIFFIES),
NADDR (header.stamp.disconnect),
SCR_COPY (4),
NADDR (disc_phys),
RADDR (temp),
SCR_REG_REG (temp, SCR_ADD, 0x01),
0,
SCR_COPY (4),
RADDR (temp),
NADDR (disc_phys),
SCR_LOAD_REG (HS_REG, HS_DISCONNECT),
0,
SCR_JUMP,
PADDR (cleanup),
},{
SCR_MOVE_ABS (1) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_COPY (1),
RADDR (sfbr),
NADDR (lastmsg),
SCR_JUMP ^ IFTRUE (DATA (M_ABORT)),
PADDRH (msg_out_abort),
SCR_JUMP ^ IFTRUE (WHEN (SCR_MSG_OUT)),
PADDR (msg_out),
},{
SCR_LOAD_REG (scratcha, M_NOOP),
0,
SCR_COPY (4),
RADDR (scratcha),
NADDR (msgout),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_FROM_REG (ctest2),
0,
SCR_JUMP ^ IFTRUE (MASK (CSIGP,CSIGP)),
PADDRH (getcc2),
SCR_INT,
SIR_SENSE_FAILED,
},{
SCR_NO_OP,
0,
SCR_LOAD_REG (dsa, 0xff),
0,
SCR_CLR (SCR_TRG),
0,
SCR_WAIT_RESEL,
PADDR(reselect2),
},{
SCR_NO_OP,
0,
SCR_REG_SFBR (ssid, SCR_AND, 0x8F),
0,
SCR_TO_REG (ctest0),
0,
SCR_JUMP,
NADDR (jump_tcb),
},{
SCR_NO_OP,
0,
SCR_FROM_REG (ctest2),
0,
SCR_JUMP ^ IFTRUE (MASK (CSIGP,CSIGP)),
PADDR (start),
SCR_JUMP,
PADDR (reselect),
},{
SCR_COPY (4),
RADDR (temp),
RADDR (dsa),
SCR_JUMP,
PADDR (prepare),
},{
SCR_JUMPR ^ IFFALSE (WHEN (SCR_MSG_IN)),
48,
SCR_FROM_REG (sbdl),
0,
SCR_JUMPR ^ IFFALSE (MASK (M_IDENTIFY, 0x98)),
32,
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin),
SCR_CLR (SCR_ACK),
0,
SCR_REG_REG (sfbr, SCR_AND, 0x07),
0,
SCR_RETURN,
0,
SCR_LOAD_SFBR (0),
0,
SCR_RETURN,
0,
},{
SCR_JUMPR ^ IFFALSE (WHEN (SCR_MSG_IN)),
64,
SCR_FROM_REG (sbdl),
0,
SCR_JUMPR ^ IFFALSE (DATA (M_SIMPLE_TAG)),
48,
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin),
SCR_CLR (SCR_ACK),
0,
SCR_JUMPR ^ IFFALSE (WHEN (SCR_MSG_IN)),
24,
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin),
SCR_CLR (SCR_ACK|SCR_CARRY),
0,
SCR_RETURN,
0,
SCR_LOAD_SFBR (0),
0,
SCR_SET (SCR_CARRY),
0,
SCR_RETURN,
0,
},{
SCR_INT ^ IFTRUE (WHEN (SCR_DATA_OUT)),
SIR_DATA_IO_IS_OUT,
SCR_INT ^ IFTRUE (WHEN (SCR_DATA_IN)),
SIR_DATA_IO_IS_IN,
SCR_JUMP,
PADDR (no_data),
},{
0
}
};
static	struct scripth scripth0 __initdata = {
{
0
},{
SCR_REG_REG (PS_REG, SCR_ADD, 0x01),
0,
SCR_LOAD_REG (scratcha, M_PARITY),
0,
SCR_JUMP,
PADDR (setmsg),
},{
SCR_FROM_REG (HS_REG),
0,
SCR_INT ^ IFTRUE (DATA (HS_NEGOTIATE)),
SIR_NEGO_FAILED,
SCR_INT ^ IFFALSE (DATA (HS_NEGOTIATE)),
SIR_REJECT_RECEIVED,
SCR_JUMP,
PADDR (clrack),
},{
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[1]),
SCR_TO_REG (scratcha),
0,
SCR_FROM_REG (socl),
0,
SCR_JUMP ^ IFTRUE (MASK (CATN, CATN)),
PADDRH (msg_parity),
SCR_FROM_REG (scratcha),
0,
SCR_JUMP ^ IFTRUE (DATA (0)),
PADDR (clrack),
SCR_JUMPR ^ IFFALSE (DATA (1)),
40,
SCR_FROM_REG (scntl2),
0,
SCR_JUMPR ^ IFFALSE (MASK (WSR, WSR)),
16,
SCR_REG_REG (scntl2, SCR_OR, WSR),
0,
SCR_JUMP,
PADDR (clrack),
SCR_FROM_REG (scratcha),
0,
SCR_INT,
SIR_IGN_RESIDUE,
SCR_JUMP,
PADDR (clrack),
},{
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[1]),
SCR_TO_REG (scratcha),
0,
SCR_FROM_REG (socl),
0,
SCR_JUMP ^ IFTRUE (MASK (CATN, CATN)),
PADDRH (msg_parity),
SCR_FROM_REG (scratcha),
0,
SCR_JUMP ^ IFTRUE (DATA (3)),
PADDRH (msg_ext_3),
SCR_JUMP ^ IFFALSE (DATA (2)),
PADDR (msg_bad),
},{
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[2]),
SCR_TO_REG (scratcha),
0,
SCR_FROM_REG (socl),
0,
SCR_JUMP ^ IFTRUE (MASK (CATN, CATN)),
PADDRH (msg_parity),
SCR_FROM_REG (scratcha),
0,
SCR_JUMP ^ IFTRUE (DATA (M_X_WIDE_REQ)),
PADDRH (msg_wdtr),
SCR_JUMP,
PADDR (msg_bad)
},{
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[3]),
SCR_FROM_REG (socl),
0,
SCR_JUMP ^ IFTRUE (MASK (CATN, CATN)),
PADDRH (msg_parity),
SCR_INT,
SIR_NEGO_WIDE,
SCR_SET (SCR_ATN),
0,
SCR_CLR (SCR_ACK),
0,
SCR_INT ^ IFFALSE (WHEN (SCR_MSG_OUT)),
SIR_NEGO_PROTO,
SCR_MOVE_ABS (4) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_CLR (SCR_ATN),
0,
SCR_COPY (1),
RADDR (sfbr),
NADDR (lastmsg),
SCR_JUMP,
PADDR (msg_out_done),
},{
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[2]),
SCR_TO_REG (scratcha),
0,
SCR_FROM_REG (socl),
0,
SCR_JUMP ^ IFTRUE (MASK (CATN, CATN)),
PADDRH (msg_parity),
SCR_FROM_REG (scratcha),
0,
SCR_JUMP ^ IFTRUE (DATA (M_X_SYNC_REQ)),
PADDRH (msg_sdtr),
SCR_JUMP,
PADDR (msg_bad)
},{
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_MOVE_ABS (2) ^ SCR_MSG_IN,
NADDR (msgin[3]),
SCR_FROM_REG (socl),
0,
SCR_JUMP ^ IFTRUE (MASK (CATN, CATN)),
PADDRH (msg_parity),
SCR_INT,
SIR_NEGO_SYNC,
SCR_SET (SCR_ATN),
0,
SCR_CLR (SCR_ACK),
0,
SCR_INT ^ IFFALSE (WHEN (SCR_MSG_OUT)),
SIR_NEGO_PROTO,
SCR_MOVE_ABS (5) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_CLR (SCR_ATN),
0,
SCR_COPY (1),
RADDR (sfbr),
NADDR (lastmsg),
SCR_JUMP,
PADDR (msg_out_done),
},{
SCR_REG_REG (scntl2, SCR_AND, 0x7f),
0,
SCR_CLR (SCR_ACK|SCR_ATN),
0,
SCR_WAIT_DISC,
0,
SCR_LOAD_REG (HS_REG, HS_ABORTED),
0,
SCR_JUMP,
PADDR (cleanup),
},{
SCR_COPY_F (4),
RADDR (dsa),
PADDRH (getcc1),
SCR_COPY (sizeof (struct head)),
},{
0,
NADDR (header),
SCR_COPY (4),
NADDR (header.status),
RADDR (scr0),
},{
SCR_CALLR,
24,
SCR_MOVE_TBL ^ SCR_DATA_IN,
offsetof (struct dsb, sense),
SCR_CALL,
PADDR (checkatn),
SCR_JUMP,
PADDR (no_data),
SCR_COPY (4),
RADDR (temp),
NADDR (header.savep),
SCR_COPY (4),
PADDR (startpos),
RADDR (scratcha),
#ifdef NCR_GETCC_WITHMSG
SCR_FROM_REG (QU_REG),
0,
SCR_JUMP ^ IFTRUE (MASK (QUIRK_NOMSG, QUIRK_NOMSG)),
PADDRH(getcc3),
SCR_SEL_TBL_ATN ^ offsetof (struct dsb, select),
PADDR(badgetcc),
SCR_FROM_REG (sdid),
0,
SCR_TO_REG (ctest0),
0,
SCR_MOVE_TBL ^ SCR_MSG_OUT,
offsetof (struct dsb, smsg2),
SCR_CLR (SCR_ATN),
0,
SCR_COPY (1),
RADDR (sfbr),
NADDR (lastmsg),
SCR_JUMP,
PADDR (prepare2),
#endif
},{
SCR_SEL_TBL ^ offsetof (struct dsb, select),
PADDR(badgetcc),
SCR_FROM_REG (sdid),
0,
SCR_TO_REG (ctest0),
0,
SCR_JUMPR ^ IFTRUE (WHEN (SCR_MSG_IN)),
0,
SCR_JUMP,
PADDR (prepare2),
},{
0
},{
SCR_LOAD_REG (scratcha, M_ABORT_TAG),
0,
SCR_JUMPR ^ IFFALSE (CARRYSET),
8,
},{
SCR_LOAD_REG (scratcha, M_ABORT),
0,
SCR_COPY (1),
RADDR (scratcha),
NADDR (msgout),
SCR_SET (SCR_ATN),
0,
SCR_CLR (SCR_ACK),
0,
SCR_REG_REG (scntl2, SCR_AND, 0x7f),
0,
SCR_MOVE_ABS (1) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_COPY (1),
RADDR (sfbr),
NADDR (lastmsg),
SCR_CLR (SCR_ACK|SCR_ATN),
0,
SCR_WAIT_DISC,
0,
SCR_JUMP,
PADDR (start),
},{
SCR_COPY (4),
NADDR(ncr_cache),
RADDR (scratcha),
SCR_COPY (4),
RADDR (temp),
NADDR(ncr_cache),
SCR_COPY (4),
NADDR(ncr_cache),
RADDR (temp),
},{
SCR_INT,
99,
}
};
__initfunc(
void ncr_script_fill (struct script * scr, struct scripth * scrh)
)
{
int	i;
ncrcmd	*p;
p = scrh->tryloop;
for (i=0; i<MAX_START; i++) {
*p++ =SCR_COPY (4);
*p++ =NADDR (squeue[i]);
*p++ =RADDR (dsa);
*p++ =SCR_CALL;
*p++ =PADDR (trysel);
};
*p++ =SCR_JUMP;
*p++ =PADDRH(tryloop);
assert ((u_long)p == (u_long)&scrh->tryloop + sizeof (scrh->tryloop));
p = scr->data_in;
for (i=0; i<MAX_SCATTER; i++) {
*p++ =SCR_CALL ^ IFFALSE (WHEN (SCR_DATA_IN));
*p++ =PADDR (checkatn);
*p++ =SCR_MOVE_TBL ^ SCR_DATA_IN;
*p++ =offsetof (struct dsb, data[i]);
};
*p++ =SCR_CALL;
*p++ =PADDR (checkatn);
*p++ =SCR_JUMP;
*p++ =PADDR (no_data);
assert ((u_long)p == (u_long)&scr->data_in + sizeof (scr->data_in));
p = scrh->data_out;
for (i=0; i<MAX_SCATTER; i++) {
*p++ =SCR_CALL ^ IFFALSE (WHEN (SCR_DATA_OUT));
*p++ =PADDR (dispatch);
*p++ =SCR_MOVE_TBL ^ SCR_DATA_OUT;
*p++ =offsetof (struct dsb, data[i]);
};
*p++ =SCR_CALL;
*p++ =PADDR (dispatch);
*p++ =SCR_JUMP;
*p++ =PADDR (no_data);
assert ((u_long)p == (u_long)&scrh->data_out + sizeof (scrh->data_out));
}
__initfunc(
static void ncr_script_copy_and_bind (ncb_p np, ncrcmd *src, ncrcmd *dst, int len)
)
{
ncrcmd  opcode, new, old, tmp1, tmp2;
ncrcmd	*start, *end;
int relocs;
int opchanged = 0;
start = src;
end = src + len/4;
while (src < end) {
opcode = *src++;
*dst++ = cpu_to_scr(opcode);
if (opcode == 0) {
printf ("%s: ERROR0 IN SCRIPT at %d.\n",
ncr_name(np), (int) (src-start-1));
DELAY (1000000);
};
if (DEBUG_FLAGS & DEBUG_SCRIPT)
printf ("%p:  <%x>\n",
(src-1), (unsigned)opcode);
switch (opcode >> 28) {
case 0xc:
relocs = 2;
tmp1 = src[0];
if ((tmp1 & RELOC_MASK) == RELOC_KVAR)
tmp1 = 0;
tmp2 = src[1];
if ((tmp2 & RELOC_MASK) == RELOC_KVAR)
tmp2 = 0;
if ((tmp1 ^ tmp2) & 3) {
printf ("%s: ERROR1 IN SCRIPT at %d.\n",
ncr_name(np), (int) (src-start-1));
DELAY (1000000);
}
if ((opcode & SCR_NO_FLUSH) && !(np->features & FE_PFEN)) {
dst[-1] = cpu_to_scr(opcode & ~SCR_NO_FLUSH);
++opchanged;
}
break;
case 0x0:
relocs = 1;
break;
case 0x8:
if (opcode & 0x00800000)
relocs = 0;
else
relocs = 1;
break;
case 0x4:
case 0x5:
case 0x6:
case 0x7:
relocs = 1;
break;
default:
relocs = 0;
break;
};
if (relocs) {
while (relocs--) {
old = *src++;
switch (old & RELOC_MASK) {
case RELOC_REGISTER:
new = (old & ~RELOC_MASK) + np->paddr;
break;
case RELOC_LABEL:
new = (old & ~RELOC_MASK) + np->p_script;
break;
case RELOC_LABELH:
new = (old & ~RELOC_MASK) + np->p_scripth;
break;
case RELOC_SOFTC:
new = (old & ~RELOC_MASK) + vtophys(np);
break;
case RELOC_KVAR:
if (((old & ~RELOC_MASK) <
SCRIPT_KVAR_FIRST) ||
((old & ~RELOC_MASK) >
SCRIPT_KVAR_LAST))
panic("ncr KVAR out of range");
new = vtophys(script_kvars[old &
~RELOC_MASK]);
break;
case 0:
if (old == 0) {
new = old;
break;
}
default:
panic("ncr_script_copy_and_bind: weird relocation %x\n", old);
break;
}
*dst++ = cpu_to_scr(new);
}
} else
*dst++ = cpu_to_scr(*src++);
};
if (bootverbose > 1 && opchanged)
printf("%s: NO FLUSH bit removed from %d script instructions\n",
ncr_name(np), opchanged);
}
struct host_data {
struct ncb *ncb;
char ncb_align[NCB_ALIGN_SIZE-1];
struct ncb _ncb_data;
char ccb_align[CCB_ALIGN_SIZE-1];
struct ccb _ccb_data;
char scr_align[SCR_ALIGN_SIZE-1];
struct script script_data;
struct scripth scripth_data;
};
#define PRINT_LUN(np, target, lun) \
printf(KERN_INFO "%s-<%d,%d>: ", ncr_name(np), (int) (target), (int) (lun))
static void PRINT_ADDR(Scsi_Cmnd *cmd)
{
struct host_data *host_data = (struct host_data *) cmd->host->hostdata;
ncb_p np                    = host_data->ncb;
if (np) PRINT_LUN(np, cmd->target, cmd->lun);
}
#define _5M 5000000
static u_long div_10M[] =
{2*_5M, 3*_5M, 4*_5M, 6*_5M, 8*_5M, 12*_5M, 16*_5M};
#define burst_length(bc) (!(bc))? 0 : 1 << (bc)
#define burst_code(dmode, ctest4, ctest5) \
(ctest4) & 0x80? 0 : (((dmode) & 0xc0) >> 6) + ((ctest5) & 0x04) + 1
static inline void ncr_init_burst(ncb_p np, u_char bc)
{
np->rv_ctest4	&= ~0x80;
np->rv_dmode	&= ~(0x3 << 6);
np->rv_ctest5	&= ~0x4;
if (!bc) {
np->rv_ctest4	|= 0x80;
}
else {
--bc;
np->rv_dmode	|= ((bc & 0x3) << 6);
np->rv_ctest5	|= (bc & 0x4);
}
}
#ifdef SCSI_NCR_NVRAM_SUPPORT
__initfunc(
static void
ncr_Symbios_setup_target(ncb_p np, int target, Symbios_nvram *nvram)
)
{
tcb_p tp = &np->target[target];
Symbios_target *tn = &nvram->target[target];
tp->usrsync = tn->sync_period ? (tn->sync_period + 3) / 4 : 255;
tp->usrwide = tn->bus_width == 0x10 ? 1 : 0;
tp->usrtags =
(tn->flags & SYMBIOS_QUEUE_TAGS_ENABLED)? SCSI_NCR_MAX_TAGS : 0;
if (!(tn->flags & SYMBIOS_DISCONNECT_ENABLE))
tp->usrflag |= UF_NODISC;
if (!(tn->flags & SYMBIOS_SCAN_AT_BOOT_TIME))
tp->usrflag |= UF_NOSCAN;
}
__initfunc(
static void
ncr_Tekram_setup_target(ncb_p np, int target, Tekram_nvram *nvram)
)
{
tcb_p tp = &np->target[target];
struct Tekram_target *tn = &nvram->target[target];
int i;
if (tn->flags & TEKRAM_SYNC_NEGO) {
i = tn->sync_index & 0xf;
tp->usrsync = i < 12 ? Tekram_sync[i] : 255;
}
tp->usrwide = (tn->flags & TEKRAM_WIDE_NEGO) ? 1 : 0;
if (tn->flags & TEKRAM_TAGGED_COMMANDS) {
tp->usrtags = 2 << nvram->max_tags_index;
if (tp->usrtags > SCSI_NCR_MAX_TAGS)
tp->usrtags = SCSI_NCR_MAX_TAGS;
}
if (!(tn->flags & TEKRAM_DISCONNECT_ENABLE))
tp->usrflag = UF_NODISC;
if (!(tn->flags & TEKRAM_PARITY_CHECK))
np->rv_scntl0  &= ~0x0a;
}
#endif
__initfunc(
static int ncr_prepare_setting(ncb_p np, ncr_nvram *nvram)
)
{
u_char	burst_max;
u_long	period;
int i;
np->sv_scntl0	= INB(nc_scntl0) & 0x0a;
np->sv_scntl3	= INB(nc_scntl3) & 0x07;
np->sv_dmode	= INB(nc_dmode)  & 0xce;
np->sv_dcntl	= INB(nc_dcntl)  & 0xa8;
np->sv_ctest3	= INB(nc_ctest3) & 0x01;
np->sv_ctest4	= INB(nc_ctest4) & 0x80;
np->sv_ctest5	= INB(nc_ctest5) & 0x24;
np->sv_gpcntl	= INB(nc_gpcntl);
np->sv_stest2	= INB(nc_stest2) & 0x20;
np->sv_stest4	= INB(nc_stest4);
np->maxwide	= (np->features & FE_WIDE)? 1 : 0;
if	(np->features & FE_QUAD)
np->multiplier	= 4;
else if	(np->features & FE_DBLR)
np->multiplier	= 2;
else
np->multiplier	= 1;
np->clock_khz	= (np->features & FE_CLK80)? 80000 : 40000;
np->clock_khz	*= np->multiplier;
if (np->clock_khz != 40000)
ncr_getclock(np, np->multiplier);
i = np->clock_divn - 1;
while (i >= 0) {
--i;
if (10ul * SCSI_NCR_MIN_ASYNC * np->clock_khz > div_10M[i]) {
++i;
break;
}
}
np->rv_scntl3 = i+1;
period = (4 * div_10M[0] + np->clock_khz - 1) / np->clock_khz;
if	(period <= 250)		np->minsync = 10;
else if	(period <= 303)		np->minsync = 11;
else if	(period <= 500)		np->minsync = 12;
else				np->minsync = (period + 40 - 1) / 40;
if	(np->minsync < 25 && !(np->features & (FE_ULTRA|FE_ULTRA2)))
np->minsync = 25;
else if	(np->minsync < 12 && !(np->features & FE_ULTRA2))
np->minsync = 12;
period = (11 * div_10M[np->clock_divn - 1]) / (4 * np->clock_khz);
np->maxsync = period > 2540 ? 254 : period / 10;
#if defined SCSI_NCR_TRUST_BIOS_SETTING
np->rv_scntl0	= np->sv_scntl0;
np->rv_dmode	= np->sv_dmode;
np->rv_dcntl	= np->sv_dcntl;
np->rv_ctest3	= np->sv_ctest3;
np->rv_ctest4	= np->sv_ctest4;
np->rv_ctest5	= np->sv_ctest5;
burst_max	= burst_code(np->sv_dmode, np->sv_ctest4, np->sv_ctest5);
#else
burst_max	= driver_setup.burst_max;
if (burst_max == 255)
burst_max = burst_code(np->sv_dmode, np->sv_ctest4, np->sv_ctest5);
if (burst_max > 7)
burst_max = 7;
if (burst_max > np->maxburst)
burst_max = np->maxburst;
if (np->features & FE_ERL)
np->rv_dmode	|= ERL;
if (np->features & FE_BOF)
np->rv_dmode	|= BOF;
if (np->features & FE_ERMP)
np->rv_dmode	|= ERMP;
if (np->features & FE_PFEN)
np->rv_dcntl	|= PFEN;
if (np->features & FE_CLSE)
np->rv_dcntl	|= CLSE;
if (np->features & FE_WRIE)
np->rv_ctest3	|= WRIE;
if (np->features & FE_DFS)
np->rv_ctest5	|= DFS;
if (driver_setup.master_parity)
np->rv_ctest4	|= MPEE;
if (driver_setup.scsi_parity)
np->rv_scntl0	|= 0x0a;
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (nvram) {
switch(nvram->type) {
case SCSI_NCR_TEKRAM_NVRAM:
np->myaddr = nvram->data.Tekram.host_id & 0x0f;
break;
case SCSI_NCR_SYMBIOS_NVRAM:
if (!(nvram->data.Symbios.flags & SYMBIOS_PARITY_ENABLE))
np->rv_scntl0  &= ~0x0a;
np->myaddr = nvram->data.Symbios.host_id & 0x0f;
if (nvram->data.Symbios.flags & SYMBIOS_VERBOSE_MSGS)
np->verbose += 1;
break;
}
}
#endif
if (!np->myaddr) np->myaddr = INB(nc_scid) & 0x07;
if (!np->myaddr) np->myaddr = SCSI_NCR_MYADDR;
#endif
ncr_init_burst(np, burst_max);
if (!nvram || nvram->type != SCSI_NCR_TEKRAM_NVRAM) {
switch(driver_setup.diff_support) {
case 3:
if (INB(nc_gpreg) & 0x08)
break;
case 2:
np->rv_stest2	|= 0x20;
break;
case 1:
np->rv_stest2	|= (np->sv_stest2 & 0x20);
break;
default:
break;
}
}
if ((driver_setup.led_pin ||
(nvram && nvram->type == SCSI_NCR_SYMBIOS_NVRAM)) &&
!(np->sv_gpcntl & 0x01))
np->features |= FE_LED0;
switch(driver_setup.irqm) {
case 2:
np->rv_dcntl	|= IRQM;
break;
case 1:
np->rv_dcntl	|= (np->sv_dcntl & IRQM);
break;
default:
break;
}
for (i = 0 ; i < MAX_TARGET ; i++) {
tcb_p tp = &np->target[i];
tp->usrsync = 255;
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (nvram) {
switch(nvram->type) {
case SCSI_NCR_TEKRAM_NVRAM:
ncr_Tekram_setup_target(np, i, &nvram->data.Tekram);
break;
case SCSI_NCR_SYMBIOS_NVRAM:
ncr_Symbios_setup_target(np, i, &nvram->data.Symbios);
break;
}
if (driver_setup.use_nvram & 0x2)
tp->usrsync = driver_setup.default_sync;
if (driver_setup.use_nvram & 0x4)
tp->usrwide = driver_setup.max_wide;
if (driver_setup.use_nvram & 0x8)
tp->usrflag &= ~UF_NOSCAN;
}
else {
#else
if (1) {
#endif
tp->usrsync = driver_setup.default_sync;
tp->usrwide = driver_setup.max_wide;
tp->usrtags = driver_setup.default_tags;
if (!driver_setup.disconnection)
np->target[i].usrflag = UF_NODISC;
}
}
i = nvram ? nvram->type : 0;
printf(KERN_INFO "%s: %sID %d, Fast-%d%s%s\n", ncr_name(np),
i  == SCSI_NCR_SYMBIOS_NVRAM ? "Symbios format NVRAM, " :
(i == SCSI_NCR_TEKRAM_NVRAM  ? "Tekram format NVRAM, " : ""),
np->myaddr,
np->minsync < 12 ? 40 : (np->minsync < 25 ? 20 : 10),
(np->rv_scntl0 & 0xa)	? ", Parity Checking"	: ", NO Parity",
(np->rv_stest2 & 0x20)	? ", Differential"	: "");
if (bootverbose > 1) {
printf ("%s: initial SCNTL3/DMODE/DCNTL/CTEST3/4/5 = "
"(hex) %02x/%02x/%02x/%02x/%02x/%02x\n",
ncr_name(np), np->sv_scntl3, np->sv_dmode, np->sv_dcntl,
np->sv_ctest3, np->sv_ctest4, np->sv_ctest5);
printf ("%s: final   SCNTL3/DMODE/DCNTL/CTEST3/4/5 = "
"(hex) %02x/%02x/%02x/%02x/%02x/%02x\n",
ncr_name(np), np->rv_scntl3, np->rv_dmode, np->rv_dcntl,
np->rv_ctest3, np->rv_ctest4, np->rv_ctest5);
}
if (bootverbose && np->paddr2)
printf (KERN_INFO "%s: on-board RAM at 0x%lx\n",
ncr_name(np), np->paddr2);
return 0;
}
#ifdef SCSI_NCR_DEBUG_NVRAM
__initfunc(
void ncr_display_Symbios_nvram(ncb_p np, Symbios_nvram *nvram)
)
{
int i;
printf("%s: HOST ID=%d%s%s%s%s\n",
ncr_name(np), nvram->host_id & 0x0f,
(nvram->flags  & SYMBIOS_SCAM_ENABLE)	? " SCAM"	:"",
(nvram->flags  & SYMBIOS_PARITY_ENABLE)	? " PARITY"	:"",
(nvram->flags  & SYMBIOS_VERBOSE_MSGS)	? " VERSBOSE"	:"",
(nvram->flags1 & SYMBIOS_SCAN_HI_LO)	? " HI_LO"	:"");
for (i = 0 ; i < 15 ; i++) {
struct Symbios_target *tn = &nvram->target[i];
printf("%s-%d:%s%s%s%s WIDTH=%d SYNC=%d TMO=%d\n",
ncr_name(np), i,
(tn->flags & SYMBIOS_DISCONNECT_ENABLE)	? " DISC"	: "",
(tn->flags & SYMBIOS_SCAN_AT_BOOT_TIME)	? " SCAN_BOOT"	: "",
(tn->flags & SYMBIOS_SCAN_LUNS)		? " SCAN_LUNS"	: "",
(tn->flags & SYMBIOS_QUEUE_TAGS_ENABLED)? " TCQ"	: "",
tn->bus_width,
tn->sync_period / 4,
tn->timeout);
}
}
static u_char Tekram_boot_delay[7] __initdata = {3, 5, 10, 20, 30, 60, 120};
__initfunc(
void ncr_display_Tekram_nvram(ncb_p np, Tekram_nvram *nvram)
)
{
int i, tags, boot_delay;
char *rem;
tags = 2 << nvram->max_tags_index;
boot_delay = 0;
if (nvram->boot_delay_index < 6)
boot_delay = Tekram_boot_delay[nvram->boot_delay_index];
switch((nvram->flags & TEKRAM_REMOVABLE_FLAGS) >> 6) {
default:
case 0:	rem = "";			break;
case 1: rem = " REMOVABLE=boot device";	break;
case 2: rem = " REMOVABLE=all";		break;
}
printf("%s: HOST ID=%d%s%s%s%s%s%s%s%s%s BOOT DELAY=%d tags=%d\n",
ncr_name(np), nvram->host_id & 0x0f,
(nvram->flags1 & SYMBIOS_SCAM_ENABLE)	? " SCAM"	:"",
(nvram->flags & TEKRAM_MORE_THAN_2_DRIVES) ? " >2DRIVES"	:"",
(nvram->flags & TEKRAM_DRIVES_SUP_1GB)	? " >1GB"	:"",
(nvram->flags & TEKRAM_RESET_ON_POWER_ON) ? " RESET"	:"",
(nvram->flags & TEKRAM_ACTIVE_NEGATION)	? " ACT_NEG"	:"",
(nvram->flags & TEKRAM_IMMEDIATE_SEEK)	? " IMM_SEEK"	:"",
(nvram->flags & TEKRAM_SCAN_LUNS)	? " SCAN_LUNS"	:"",
(nvram->flags1 & TEKRAM_F2_F6_ENABLED)	? " F2_F6"	:"",
rem, boot_delay, tags);
for (i = 0; i <= 15; i++) {
int sync, j;
struct Tekram_target *tn = &nvram->target[i];
j = tn->sync_index & 0xf;
sync = j < 12 ? Tekram_sync[j] : 255;
printf("%s-%d:%s%s%s%s%s%s PERIOD=%d\n",
ncr_name(np), i,
(tn->flags & TEKRAM_PARITY_CHECK)	? " PARITY"	: "",
(tn->flags & TEKRAM_SYNC_NEGO)		? " SYNC"	: "",
(tn->flags & TEKRAM_DISCONNECT_ENABLE)	? " DISC"	: "",
(tn->flags & TEKRAM_START_CMD)		? " START"	: "",
(tn->flags & TEKRAM_TAGGED_COMMANDS)	? " TCQ"	: "",
(tn->flags & TEKRAM_WIDE_NEGO)		? " WIDE"	: "",
sync);
}
}
#endif
__initfunc(
static int ncr_attach (Scsi_Host_Template *tpnt, int unit, ncr_device *device)
)
{
struct host_data *host_data;
ncb_p np;
struct Scsi_Host *instance = 0;
u_long flags = 0;
ncr_nvram *nvram = device->nvram;
printf(KERN_INFO "ncr53c%s-%d: rev=0x%02x, base=0x%x, io_port=0x%x, irq=%d\n",
device->chip.name, unit, device->chip.revision_id, device->slot.base,
device->slot.io_port, device->slot.irq);
if (!(instance = scsi_register(tpnt, sizeof(*host_data))))
goto attach_error;
host_data = (struct host_data *) instance->hostdata;
np        = (ncb_p) (((u_long) &host_data->_ncb_data) & NCB_ALIGN_MASK);
host_data->ncb = np;
bzero (np, sizeof (*np));
np->ccb   = (ccb_p) (((u_long) &host_data->_ccb_data) & CCB_ALIGN_MASK);
bzero (np->ccb, sizeof (*np->ccb));
strncpy(np->chip_name, device->chip.name, sizeof(np->chip_name) - 1);
np->unit	= unit;
np->verbose	= driver_setup.verbose;
sprintf(np->inst_name, "ncr53c%s-%d", np->chip_name, np->unit);
np->device_id	= device->chip.device_id;
np->revision_id	= device->chip.revision_id;
np->features	= device->chip.features;
np->clock_divn	= device->chip.nr_divisor;
np->maxoffs	= device->chip.offset_max;
np->maxburst	= device->chip.burst_max;
np->script0  =
(struct script *) (((u_long) &host_data->script_data) & SCR_ALIGN_MASK);
np->scripth0 = &host_data->scripth_data;
init_timer(&np->timer);
np->timer.data     = (unsigned long) np;
np->timer.function = ncr53c8xx_timeout;
np->paddr	= device->slot.base;
np->paddr2	= (np->features & FE_RAM)? device->slot.base_2 : 0;
#ifndef NCR_IOMAPPED
np->vaddr = remap_pci_mem((u_long) np->paddr, (u_long) 128);
if (!np->vaddr) {
printf("%s: can't map memory mapped IO region\n", ncr_name(np));
goto attach_error;
}
else
if (bootverbose > 1)
printf("%s: using memory mapped IO at virtual address 0x%lx\n", ncr_name(np), (u_long) np->vaddr);
np->reg = (struct ncr_reg*) np->vaddr;
#endif
request_region(device->slot.io_port, 128, "ncr53c8xx");
np->port = device->slot.io_port;
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (nvram) {
switch(nvram->type) {
case SCSI_NCR_SYMBIOS_NVRAM:
#ifdef SCSI_NCR_DEBUG_NVRAM
ncr_display_Symbios_nvram(np, &nvram->data.Symbios);
#endif
break;
case SCSI_NCR_TEKRAM_NVRAM:
#ifdef SCSI_NCR_DEBUG_NVRAM
ncr_display_Tekram_nvram(np, &nvram->data.Tekram);
#endif
break;
default:
nvram = 0;
#ifdef SCSI_NCR_DEBUG_NVRAM
printf("%s: NVRAM: None or invalid data.\n", ncr_name(np));
#endif
}
}
#endif
(void)ncr_prepare_setting(np, nvram);
#ifndef NCR_IOMAPPED
if (np->paddr2 && sizeof(struct script) <= 4096) {
np->vaddr2 = remap_pci_mem((u_long) np->paddr2, (u_long) 4096);
if (!np->vaddr2) {
printf("%s: can't map memory mapped IO region\n", ncr_name(np));
goto attach_error;
}
else
if (bootverbose > 1)
printf("%s: on-board ram mapped at virtual address 0x%lx\n", ncr_name(np), (u_long) np->vaddr2);
}
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
instance->max_channel	= 0;
instance->max_id	= np->maxwide ? 16 : 8;
instance->max_lun	= SCSI_NCR_MAX_LUN;
#endif
#ifndef NCR_IOMAPPED
instance->base		= (char *) np->reg;
#endif
instance->irq		= device->slot.irq;
instance->io_port	= device->slot.io_port;
instance->n_io_port	= 128;
instance->dma_channel	= 0;
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,0,0)
instance->select_queue_depths = ncr53c8xx_select_queue_depths;
#endif
ncr_script_fill (&script0, &scripth0);
np->scripth	= np->scripth0;
np->p_scripth	= vtophys(np->scripth);
np->script	= (np->vaddr2) ? (struct script *) np->vaddr2 : np->script0;
np->p_script	= (np->vaddr2) ? np->paddr2 : vtophys(np->script0);
ncr_script_copy_and_bind (np, (ncrcmd *) &script0, (ncrcmd *) np->script0, sizeof(struct script));
ncr_script_copy_and_bind (np, (ncrcmd *) &scripth0, (ncrcmd *) np->scripth0, sizeof(struct scripth));
np->ccb->p_ccb		= vtophys (np->ccb);
if (np->features & FE_LED0) {
np->script0->reselect[0]  =
cpu_to_scr(SCR_REG_REG(gpreg, SCR_OR,  0x01));
np->script0->reselect1[0] =
cpu_to_scr(SCR_REG_REG(gpreg, SCR_AND, 0xfe));
np->script0->reselect2[0] =
cpu_to_scr(SCR_REG_REG(gpreg, SCR_AND, 0xfe));
}
np->jump_tcb.l_cmd	= cpu_to_scr(SCR_JUMP);
np->jump_tcb.l_paddr	= cpu_to_scr(NCB_SCRIPTH_PHYS (np, abort));
OUTB (nc_istat,  SRST);
DELAY (1000);
OUTB (nc_istat,  0   );
if (ncr_snooptest (np)) {
printf ("CACHE INCORRECTLY CONFIGURED.\n");
goto attach_error;
};
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,70)
#ifdef SCSI_NCR_SHARE_IRQ
if (bootverbose > 1)
printf("%s: requesting shared irq %d (dev_id=0x%lx)\n",
ncr_name(np), device->slot.irq, (u_long) np);
if (request_irq(device->slot.irq, ncr53c8xx_intr,
SA_INTERRUPT|SA_SHIRQ, "ncr53c8xx", np)) {
#else
if (request_irq(device->slot.irq, ncr53c8xx_intr,
SA_INTERRUPT, "ncr53c8xx", np)) {
#endif
#else
if (request_irq(device->slot.irq, ncr53c8xx_intr,
SA_INTERRUPT, "ncr53c8xx")) {
#endif
printf("%s: request irq %d failure\n", ncr_name(np), device->slot.irq);
goto attach_error;
}
np->irq = device->slot.irq;
save_flags(flags); cli();
if (ncr_reset_scsi_bus(np, 0, driver_setup.settle_delay) != 0) {
printf("%s: FATAL ERROR: CHECK SCSI BUS - CABLES, TERMINATION, DEVICE POWER etc.!\n", ncr_name(np));
restore_flags(flags);
goto attach_error;
}
ncr_exception (np);
restore_flags(flags);
np->disc = 1;
if (driver_setup.settle_delay > 2) {
printf("%s: waiting %d seconds for scsi devices to settle...\n",
ncr_name(np), driver_setup.settle_delay);
DELAY(1000000UL * driver_setup.settle_delay);
}
np->lasttime=0;
ncr_timeout (np);
#ifdef SCSI_NCR_ALWAYS_SIMPLE_TAG
np->order = M_SIMPLE_TAG;
#endif
if (!the_template) {
the_template = instance->hostt;
first_host = instance;
}
return 0;
attach_error:
if (!instance) return -1;
printf("%s: detaching...\n", ncr_name(np));
#ifndef NCR_IOMAPPED
if (np->vaddr) {
#ifdef DEBUG_NCR53C8XX
printf("%s: releasing memory mapped IO region %lx[%d]\n", ncr_name(np), (u_long) np->vaddr, 128);
#endif
unmap_pci_mem((vm_offset_t) np->vaddr, (u_long) 128);
}
if (np->vaddr2) {
#ifdef DEBUG_NCR53C8XX
printf("%s: releasing memory mapped IO region %lx[%d]\n", ncr_name(np), (u_long) np->vaddr2, 4096);
#endif
unmap_pci_mem((vm_offset_t) np->vaddr2, (u_long) 4096);
}
#endif
if (np->port) {
#ifdef DEBUG_NCR53C8XX
printf("%s: releasing IO region %x[%d]\n", ncr_name(np), np->port, 128);
#endif
release_region(np->port, 128);
}
if (np->irq) {
#ifdef DEBUG_NCR53C8XX
printf("%s: freeing irq %d\n", ncr_name(np), np->irq);
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,70)
free_irq(np->irq, np);
#else
free_irq(np->irq);
#endif
}
scsi_unregister(instance);
return -1;
}
int ncr_queue_command (Scsi_Cmnd *cmd, void (* done)(Scsi_Cmnd *))
{
struct Scsi_Host   *host      = cmd->host;
struct host_data   *host_data = (struct host_data *) host->hostdata;
ncb_p np                      = host_data->ncb;
tcb_p tp                      = &np->target[cmd->target];
ccb_p cp;
lcb_p lp;
int	segments;
u_char	qidx, nego, idmsg, *msgptr;
u_int  msglen, msglen2;
u_long	flags;
int	xfer_direction;
cmd->scsi_done     = done;
cmd->host_scribble = NULL;
cmd->SCp.ptr       = NULL;
cmd->SCp.buffer    = NULL;
if ((cmd->target == np->myaddr	  ) ||
(cmd->target >= MAX_TARGET) ||
(cmd->lun    >= MAX_LUN   )) {
return(DID_BAD_TARGET);
}
if (cmd->cmnd[0] == 0 && (tp->usrflag & UF_NOSCAN)) {
tp->usrflag &= ~UF_NOSCAN;
return DID_BAD_TARGET;
}
if (DEBUG_FLAGS & DEBUG_TINY) {
PRINT_ADDR(cmd);
printf ("CMD=%x ", cmd->cmnd[0]);
}
save_flags(flags); cli();
if (np->settle_time && cmd->timeout_per_command >= HZ &&
np->settle_time > jiffies + cmd->timeout_per_command - HZ) {
np->settle_time = jiffies + cmd->timeout_per_command - HZ;
}
if (np->settle_time || !(cp=ncr_get_ccb (np, cmd->target, cmd->lun))) {
insert_into_waiting_list(np, cmd);
restore_flags(flags);
return(DID_OK);
}
cp->cmd = cmd;
if (!tp->usrtags && cmd->device && cmd->device->tagged_queue) {
tp->usrtags = SCSI_NCR_MAX_TAGS;
ncr_setmaxtags (np, tp, SCSI_NCR_MAX_TAGS);
}
#ifdef SCSI_NCR_PROFILE_SUPPORT
bzero (&cp->phys.header.stamp, sizeof (struct tstamp));
cp->phys.header.stamp.start = jiffies;
#endif
if (tp->quirks & QUIRK_UPDATE) {
tp->quirks = ncr_lookup ((char*) &tp->inqdata[0]);
#ifndef NCR_GETCC_WITHMSG
if (tp->quirks) {
PRINT_ADDR(cmd);
printf ("quirks=%x.\n", tp->quirks);
}
#endif
}
nego = 0;
if (cmd->lun == 0 && !tp->nego_cp &&
(tp->inqdata[2] & 0x7) >= 2 && tp->inqdata[7]) {
if (!tp->widedone) {
if (tp->inqdata[7] & INQ7_WIDE16) {
nego = NS_WIDE;
} else
tp->widedone=1;
};
if (!nego && !tp->period) {
if ( 1
#if defined (CDROM_ASYNC)
&& ((tp->inqdata[0] & 0x1f) != 5)
#endif
&& (tp->inqdata[7] & INQ7_SYNC)) {
nego = NS_SYNC;
} else {
tp->period  =0xffff;
tp->sval = 0xe0;
PRINT_ADDR(cmd);
printf ("asynchronous.\n");
};
};
if (nego)
tp->nego_cp = cp;
};
if ((lp = tp->lp[cmd->lun]) && (lp->usetags)) {
while (!cp->tag) {
ccb_p cp2 = lp->next_ccb;
lp->lasttag = lp->lasttag % 255 + 1;
while (cp2 && cp2->tag != lp->lasttag)
cp2 = cp2->next_ccb;
if (cp2) continue;
cp->tag=lp->lasttag;
if (DEBUG_FLAGS & DEBUG_TAGS) {
PRINT_ADDR(cmd);
printf ("using tag #%d.\n", cp->tag);
}
}
} else {
cp->tag=0;
}
idmsg = M_IDENTIFY | cmd->lun;
if (cp != np->ccb && ((np->disc && !(tp->usrflag & UF_NODISC)) || cp->tag))
idmsg |= 0x40;
msgptr = cp->scsi_smsg;
msglen = 0;
msgptr[msglen++] = idmsg;
if (cp->tag) {
char tag;
tag = np->order;
if (tag == 0) {
switch (cmd->cmnd[0]) {
case 0x08:
case 0x28:
case 0xa8:
tag = M_SIMPLE_TAG;
break;
default:
tag = M_ORDERED_TAG;
}
}
if ((lp = tp->lp[cmd->lun]) && (lp->force_ordered_tag)) {
tag = M_ORDERED_TAG;
lp->force_ordered_tag = 0;
if (DEBUG_FLAGS & DEBUG_TAGS) {
PRINT_ADDR(cmd);
printf ("Ordered Queue Tag forced\n");
}
}
msgptr[msglen++] = tag;
msgptr[msglen++] = cp -> tag;
}
switch (nego) {
case NS_SYNC:
msgptr[msglen++] = M_EXTENDED;
msgptr[msglen++] = 3;
msgptr[msglen++] = M_X_SYNC_REQ;
msgptr[msglen++] = tp->maxoffs ? tp->minsync : 0;
msgptr[msglen++] = tp->maxoffs;
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("sync msgout: ");
ncr_show_msg (&cp->scsi_smsg [msglen-5]);
printf (".\n");
};
break;
case NS_WIDE:
msgptr[msglen++] = M_EXTENDED;
msgptr[msglen++] = 2;
msgptr[msglen++] = M_X_WIDE_REQ;
msgptr[msglen++] = tp->usrwide;
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("wide msgout: ");
ncr_show_msg (&cp->scsi_smsg [msglen-4]);
printf (".\n");
};
break;
};
cp -> scsi_smsg2 [0] = idmsg;
msglen2 = 1;
segments = ncr_scatter (cp, cp->cmd);
if (segments < 0) {
ncr_free_ccb(np, cp, cmd->target, cmd->lun);
restore_flags(flags);
return(DID_ERROR);
}
switch((int) cmd->cmnd[0]) {
case 0x08:
case 0x28:
case 0xA8:
xfer_direction = XferIn;
break;
case 0x0A:
case 0x2A:
case 0xAA:
xfer_direction = XferOut;
break;
default:
xfer_direction = guess_xfer_direction((int) cmd->cmnd[0]);
break;
}
cp->segments = segments;
if (!cp->data_len)
xfer_direction = XferNone;
switch (xfer_direction) {
u_long endp;
default:
case XferBoth:
cp->phys.header.savep =
cpu_to_scr(NCB_SCRIPT_PHYS (np, data_io));
cp->phys.header.goalp = cp->phys.header.savep;
break;
case XferIn:
endp = NCB_SCRIPT_PHYS (np, data_in) + MAX_SCATTER*16;
cp->phys.header.goalp = cpu_to_scr(endp + 8);
cp->phys.header.savep = cpu_to_scr(endp - segments*16);
break;
case XferOut:
endp = NCB_SCRIPTH_PHYS (np, data_out) + MAX_SCATTER*16;
cp->phys.header.goalp = cpu_to_scr(endp + 8);
cp->phys.header.savep = cpu_to_scr(endp - segments*16);
break;
case XferNone:
cp->phys.header.savep =
cpu_to_scr(NCB_SCRIPT_PHYS (np, no_data));
cp->phys.header.goalp = cp->phys.header.savep;
break;
}
cp->phys.header.lastp = cp->phys.header.savep;
cp->phys.header.cp		= cp;
cp->phys.header.launch.l_paddr	=
cpu_to_scr(NCB_SCRIPT_PHYS (np, select));
cp->phys.header.launch.l_cmd	= cpu_to_scr(SCR_JUMP);
cp->phys.select.sel_id		= cmd->target;
cp->phys.select.sel_scntl3	= tp->wval;
cp->phys.select.sel_sxfer	= tp->sval;
cp->phys.smsg.addr		= cpu_to_scr(CCB_PHYS (cp, scsi_smsg));
cp->phys.smsg.size		= cpu_to_scr(msglen);
cp->phys.smsg2.addr		= cpu_to_scr(CCB_PHYS (cp, scsi_smsg2));
cp->phys.smsg2.size		= cpu_to_scr(msglen2);
cp->phys.cmd.addr		= cpu_to_scr(vtophys (&cmd->cmnd[0]));
cp->phys.cmd.size		= cpu_to_scr(cmd->cmd_len);
cp->phys.scmd.addr		= cpu_to_scr(CCB_PHYS (cp, sensecmd));
cp->phys.scmd.size		= cpu_to_scr(6);
cp->sensecmd[0]			= 0x03;
cp->sensecmd[1]			= cmd->lun << 5;
cp->sensecmd[4]			= sizeof(cmd->sense_buffer);
cp->phys.sense.addr		=
cpu_to_scr(vtophys (&cmd->sense_buffer[0]));
cp->phys.sense.size		= cpu_to_scr(sizeof(cmd->sense_buffer));
cp->actualquirks		= tp->quirks;
cp->host_status			= nego ? HS_NEGOTIATE : HS_BUSY;
cp->scsi_status			= S_ILLEGAL;
cp->parity_status		= 0;
cp->xerr_status			= XE_OK;
cp->sync_status			= tp->sval;
cp->nego_status			= nego;
cp->wide_status			= tp->wval;
cp->jump_ccb.l_cmd	=
cpu_to_scr((SCR_JUMP ^ IFFALSE (DATA (cp->tag))));
if (cmd->timeout_per_command > 0)
cp->tlimit	= jiffies + cmd->timeout_per_command + NCR_TIMEOUT_INCREASE;
else
cp->tlimit	= jiffies + 3600 * HZ;
cp->magic		= CCB_MAGIC;
qidx = np->squeueput + 1;
if (qidx >= MAX_START) qidx=0;
np->squeue [qidx	 ] = cpu_to_scr(NCB_SCRIPT_PHYS (np, idle));
np->squeue [np->squeueput] = cpu_to_scr(CCB_PHYS (cp, phys));
np->squeueput = qidx;
if(DEBUG_FLAGS & DEBUG_QUEUE)
printf ("%s: queuepos=%d tryoffset=%d.\n", ncr_name (np),
np->squeueput,
(unsigned)(scr_to_cpu(np->script->startpos[0]) -
(NCB_SCRIPTH_PHYS (np, tryloop))));
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
if (!np->stalling)
#endif
OUTB (nc_istat, SIGP);
restore_flags(flags);
return(DID_OK);
}
static void ncr_start_reset(ncb_p np, int settle_delay)
{
u_long flags;
save_flags(flags); cli();
if (!np->settle_time) {
(void) ncr_reset_scsi_bus(np, 1, settle_delay);
}
restore_flags(flags);
}
static int ncr_reset_scsi_bus(ncb_p np, int enab_int, int settle_delay)
{
u_int32 term;
int retv = 0;
np->settle_time	= jiffies + settle_delay * HZ;
if (bootverbose > 1)
printf("%s: resetting, "
"command processing suspended for %d seconds\n",
ncr_name(np), settle_delay);
OUTB (nc_istat, SRST);
DELAY (1000);
OUTB (nc_istat, 0);
if (enab_int)
OUTW (nc_sien, RST);
OUTB (nc_stest3, TE);
OUTB (nc_dcntl, (np->rv_dcntl & IRQM));
OUTB (nc_scntl1, CRST);
DELAY (100);
if (!driver_setup.bus_check)
goto out;
term =	INB(nc_sstat0);
term =	((term & 2) << 7) + ((term & 1) << 16);
term |= ((INB(nc_sstat2) & 0x01) << 25) |
(INW(nc_sbdl) << 9) |
INB(nc_sbcl);
if (!(np->features & FE_WIDE))
term &= 0x3ffff;
if (term != (2<<7)) {
printf("%s: suspicious SCSI data while resetting the BUS.\n",
ncr_name(np));
printf("%s: %sdp0,d7-0,rst,req,ack,bsy,sel,atn,msg,c/d,i/o = "
"0x%lx, expecting 0x%lx\n",
ncr_name(np),
(np->features & FE_WIDE) ? "dp1,d15-8," : "",
(u_long)term, (u_long)(2<<7));
if (driver_setup.bus_check == 1)
retv = 1;
}
out:
OUTB (nc_scntl1, 0);
return retv;
}
int ncr_reset_bus (Scsi_Cmnd *cmd, int sync_reset)
{
struct Scsi_Host   *host      = cmd->host;
struct host_data   *host_data = (struct host_data *) host->hostdata;
ncb_p np                      = host_data->ncb;
ccb_p cp;
u_long flags;
int found;
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
if (np->stalling)
np->stalling = 0;
#endif
save_flags(flags); cli();
if (np->settle_time) {
restore_flags(flags);
return SCSI_RESET_PUNT;
}
ncr_start_reset(np, driver_setup.settle_delay);
for (found=0, cp=np->ccb; cp; cp=cp->link_ccb) {
if (cp->host_status == HS_IDLE) continue;
if (cp->cmd == cmd) {
found = 1;
break;
}
}
if (!found && retrieve_from_waiting_list(0, np, cmd))
found = 1;
reset_waiting_list(np);
ncr_wakeup(np, HS_RESET);
if (!found && sync_reset && !retrieve_from_waiting_list(0, np, cmd)) {
cmd->result = ScsiResult(DID_RESET, 0);
cmd->scsi_done(cmd);
}
restore_flags(flags);
return SCSI_RESET_SUCCESS;
}
static int ncr_abort_command (Scsi_Cmnd *cmd)
{
struct Scsi_Host   *host      = cmd->host;
struct host_data   *host_data = (struct host_data *) host->hostdata;
ncb_p np                      = host_data->ncb;
ccb_p cp;
u_long flags;
int found;
int retv;
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
if (np->stalling == 2)
np->stalling = 0;
#endif
save_flags(flags); cli();
if (remove_from_waiting_list(np, cmd)) {
cmd->result = ScsiResult(DID_ABORT, 0);
cmd->scsi_done(cmd);
restore_flags(flags);
return SCSI_ABORT_SUCCESS;
}
for (found=0, cp=np->ccb; cp; cp=cp->link_ccb) {
if (cp->host_status == HS_IDLE) continue;
if (cp->cmd == cmd) {
found = 1;
break;
}
}
if (!found) {
restore_flags(flags);
return SCSI_ABORT_NOT_RUNNING;
}
if (np->settle_time) {
restore_flags(flags);
return SCSI_ABORT_SNOOZE;
}
cp->jump_ccb.l_cmd = cpu_to_scr(SCR_JUMP);
if (cp->phys.header.launch.l_paddr ==
cpu_to_scr(NCB_SCRIPT_PHYS (np, select))) {
printf ("%s: abort ccb=%p (skip)\n", ncr_name (np), cp);
cp->phys.header.launch.l_paddr =
cpu_to_scr(NCB_SCRIPT_PHYS (np, skip));
}
cp->tlimit = 0;
retv = SCSI_ABORT_PENDING;
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
if (!np->stalling)
#endif
OUTB (nc_istat, SIGP);
restore_flags(flags);
return retv;
}
#ifdef MODULE
static int ncr_detach(ncb_p np)
{
ccb_p cp;
tcb_p tp;
lcb_p lp;
int target, lun;
int i;
printf("%s: releasing host resources\n", ncr_name(np));
#ifdef DEBUG_NCR53C8XX
printf("%s: stopping the timer\n", ncr_name(np));
#endif
np->release_stage = 1;
for (i = 50 ; i && np->release_stage != 2 ; i--) DELAY(100000);
if (np->release_stage != 2)
printf("%s: the timer seems to be already stopped\n", ncr_name(np));
else np->release_stage = 2;
#ifdef DEBUG_NCR53C8XX
printf("%s: disabling chip interrupts\n", ncr_name(np));
#endif
OUTW (nc_sien , 0);
OUTB (nc_dien , 0);
#ifdef DEBUG_NCR53C8XX
printf("%s: freeing irq %d\n", ncr_name(np), np->irq);
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,70)
free_irq(np->irq, np);
#else
free_irq(np->irq);
#endif
printf("%s: resetting chip\n", ncr_name(np));
OUTB (nc_istat,  SRST);
DELAY (1000);
OUTB (nc_istat,  0   );
OUTB(nc_dmode,	np->sv_dmode);
OUTB(nc_dcntl,	np->sv_dcntl);
OUTB(nc_ctest3,	np->sv_ctest3);
OUTB(nc_ctest4,	np->sv_ctest4);
OUTB(nc_ctest5,	np->sv_ctest5);
OUTB(nc_gpcntl,	np->sv_gpcntl);
OUTB(nc_stest2,	np->sv_stest2);
ncr_selectclock(np, np->sv_scntl3);
#ifndef NCR_IOMAPPED
#ifdef DEBUG_NCR53C8XX
printf("%s: releasing memory mapped IO region %lx[%d]\n", ncr_name(np), (u_long) np->vaddr, 128);
#endif
unmap_pci_mem((vm_offset_t) np->vaddr, (u_long) 128);
#ifdef DEBUG_NCR53C8XX
printf("%s: releasing memory mapped IO region %lx[%d]\n", ncr_name(np), (u_long) np->vaddr2, 4096);
#endif
unmap_pci_mem((vm_offset_t) np->vaddr2, (u_long) 4096);
#endif
#ifdef DEBUG_NCR53C8XX
printf("%s: releasing IO region %x[%d]\n", ncr_name(np), np->port, 128);
#endif
release_region(np->port, 128);
while ((cp=np->ccb->link_ccb) != NULL) {
np->ccb->link_ccb = cp->link_ccb;
if (cp->host_status) {
printf("%s: shall free an active ccb (host_status=%d)\n",
ncr_name(np), cp->host_status);
}
#ifdef DEBUG_NCR53C8XX
printf("%s: freeing ccb (%lx)\n", ncr_name(np), (u_long) cp);
#endif
m_free(cp, sizeof(*cp));
}
for (target = 0; target < MAX_TARGET ; target++) {
tp=&np->target[target];
for (lun = 0 ; lun < MAX_LUN ; lun++) {
lp = tp->lp[lun];
if (lp) {
#ifdef DEBUG_NCR53C8XX
printf("%s: freeing lp (%lx)\n", ncr_name(np), (u_long) lp);
#endif
m_free(lp, sizeof(*lp));
}
}
}
printf("%s: host resources successfully released\n", ncr_name(np));
return 1;
}
#endif
void ncr_complete (ncb_p np, ccb_p cp)
{
Scsi_Cmnd *cmd;
tcb_p tp;
lcb_p lp;
if (!cp || (cp->magic!=CCB_MAGIC) || !cp->cmd) return;
cp->magic = 1;
cp->tlimit= 0;
cmd = cp->cmd;
cp->jump_ccb.l_cmd = cpu_to_scr(SCR_JUMP);
cp->phys.header.launch.l_paddr = cpu_to_scr(NCB_SCRIPT_PHYS (np, idle));
#ifdef SCSI_NCR_PROFILE_SUPPORT
ncb_profile (np, cp);
#endif
if (DEBUG_FLAGS & DEBUG_TINY)
printf ("CCB=%lx STAT=%x/%x\n", (unsigned long)cp & 0xfff,
cp->host_status,cp->scsi_status);
cmd = cp->cmd;
cp->cmd = NULL;
tp = &np->target[cmd->target];
lp = tp->lp[cmd->lun];
if (cp == tp->nego_cp)
tp->nego_cp = 0;
if (cp->parity_status) {
PRINT_ADDR(cmd);
printf ("%d parity error(s), fallback.\n", cp->parity_status);
tp->usrsync=255;
tp->period =  0;
}
if (cp->xerr_status != XE_OK) {
PRINT_ADDR(cmd);
switch (cp->xerr_status) {
case XE_EXTRA_DATA:
printf ("extraneous data discarded.\n");
break;
case XE_BAD_PHASE:
printf ("illegal scsi phase (4/5).\n");
break;
default:
printf ("extended error %d.\n", cp->xerr_status);
break;
}
if (cp->host_status==HS_COMPLETE)
cp->host_status = HS_FAIL;
}
if (   (cp->host_status == HS_COMPLETE)
&& (cp->scsi_status == S_GOOD ||
cp->scsi_status == S_COND_MET)) {
cmd->result = ScsiResult(DID_OK, cp->scsi_status);
ncr_alloc_ccb (np, cmd->target, cmd->lun);
if (cmd->lun == 0 && cmd->cmnd[0] == 0x12) {
if (np->unit < SCSI_NCR_MAX_HOST) {
if (driver_setup.force_sync_nego)
((char *) cmd->request_buffer)[7] |= INQ7_SYNC;
else
((char *) cmd->request_buffer)[7] &=
(target_capabilities[np->unit].and_map[cmd->target]);
}
bcopy (	cmd->request_buffer,
&tp->inqdata,
sizeof (tp->inqdata));
ncr_setmaxtags (np, tp, driver_setup.default_tags);
ncr_negotiate (np, tp);
tp->quirks |= QUIRK_UPDATE;
}
if (lp) {
ncr_settags (tp, lp);
if (lp->reqlink != lp->actlink)
ncr_opennings (np, lp, cmd);
};
tp->bytes     += cp->data_len;
tp->transfers ++;
if (tp->numtags < tp->maxtags) {
++tp->num_good;
if (tp->num_good >= 100) {
tp->num_good = 0;
++tp->numtags;
if (tp->numtags == 1) {
PRINT_ADDR(cmd);
printf("tagged command queueing resumed\n");
}
}
}
} else if ((cp->host_status == HS_COMPLETE)
&& (cp->scsi_status == (S_SENSE|S_GOOD) ||
cp->scsi_status == (S_SENSE|S_CHECK_COND))) {
cmd->result = ScsiResult(DID_OK, S_CHECK_COND);
if (DEBUG_FLAGS & (DEBUG_RESULT|DEBUG_TINY)) {
u_char * p = (u_char*) & cmd->sense_buffer;
int i;
printf ("\n%s: sense data:", ncr_name (np));
for (i=0; i<14; i++) printf (" %x", *p++);
printf (".\n");
}
} else if ((cp->host_status == HS_COMPLETE)
&& (cp->scsi_status == S_BUSY ||
cp->scsi_status == S_CONFLICT)) {
cmd->result = ScsiResult(DID_OK, cp->scsi_status);
} else if ((cp->host_status == HS_COMPLETE)
&& (cp->scsi_status == S_QUEUE_FULL)) {
cmd->result = ScsiResult(DID_OK, cp->scsi_status);
if (tp->numtags) {
PRINT_ADDR(cmd);
printf("QUEUE FULL! suspending tagged command queueing\n");
tp->numtags	= 0;
tp->num_good	= 0;
if (lp) {
ncr_settags (tp, lp);
if (lp->reqlink != lp->actlink)
ncr_opennings (np, lp, cmd);
};
}
} else if ((cp->host_status == HS_SEL_TIMEOUT)
|| (cp->host_status == HS_TIMEOUT)) {
cmd->result = ScsiResult(DID_TIME_OUT, cp->scsi_status);
} else if (cp->host_status == HS_RESET) {
cmd->result = ScsiResult(DID_RESET, cp->scsi_status);
} else if (cp->host_status == HS_ABORTED) {
cmd->result = ScsiResult(DID_ABORT, cp->scsi_status);
} else {
PRINT_ADDR(cmd);
printf ("COMMAND FAILED (%x %x) @%p.\n",
cp->host_status, cp->scsi_status, cp);
cmd->result = ScsiResult(DID_ERROR, cp->scsi_status);
}
if (tp->usrflag & UF_TRACE) {
u_char * p;
int i;
PRINT_ADDR(cmd);
printf (" CMD:");
p = (u_char*) &cmd->cmnd[0];
for (i=0; i<cmd->cmd_len; i++) printf (" %x", *p++);
if (cp->host_status==HS_COMPLETE) {
switch (cp->scsi_status) {
case S_GOOD:
printf ("  GOOD");
break;
case S_CHECK_COND:
printf ("  SENSE:");
p = (u_char*) &cmd->sense_buffer;
for (i=0; i<14; i++)
printf (" %x", *p++);
break;
default:
printf ("  STAT: %x\n", cp->scsi_status);
break;
}
} else printf ("  HOSTERROR: %x", cp->host_status);
printf ("\n");
}
ncr_free_ccb (np, cp, cmd->target, cmd->lun);
if (np->waiting_list) requeue_waiting_list(np);
cmd->scsi_done (cmd);
}
void ncr_wakeup (ncb_p np, u_long code)
{
ccb_p cp = np->ccb;
while (cp) {
switch (cp->host_status) {
case HS_IDLE:
break;
case HS_DISCONNECT:
if(DEBUG_FLAGS & DEBUG_TINY) printf ("D");
case HS_BUSY:
case HS_NEGOTIATE:
if (!code) break;
cp->host_status = code;
default:
ncr_complete (np, cp);
break;
};
cp = cp -> link_ccb;
};
}
void ncr_init (ncb_p np, int reset, char * msg, u_long code)
{
int	i;
if (reset) {
OUTB (nc_istat,  SRST);
DELAY (10000);
}
else {
OUTB (nc_stest3, TE|CSF);
OUTONB (nc_ctest3, CLF);
}
if (msg) printf (KERN_INFO "%s: restart (%s).\n", ncr_name (np), msg);
for (i=0;i<MAX_START;i++)
np -> squeue [i] = cpu_to_scr(NCB_SCRIPT_PHYS (np, idle));
np->squeueput = 0;
np->script0->startpos[0] = cpu_to_scr(NCB_SCRIPTH_PHYS (np, tryloop));
np->script0->start0  [0] = cpu_to_scr(SCR_INT ^ IFFALSE (0));
ncr_wakeup (np, code);
OUTB (nc_istat,  0x00   );
OUTB (nc_scntl0, np->rv_scntl0 | 0xc0);
OUTB (nc_scntl1, 0x00);
ncr_selectclock(np, np->rv_scntl3);
OUTB (nc_scid  , RRE|np->myaddr);
OUTW (nc_respid, 1ul<<np->myaddr);
OUTB (nc_istat , SIGP	);
OUTB (nc_dmode , np->rv_dmode);
OUTB (nc_ctest5, np->rv_ctest5);
OUTB (nc_dcntl , NOCOM|np->rv_dcntl);
OUTB (nc_ctest3, np->rv_ctest3);
OUTB (nc_ctest4, np->rv_ctest4);
OUTB (nc_stest2, EXT|np->rv_stest2);
OUTB (nc_stest3, TE);
OUTB (nc_stime0, 0x0d	);
np->disc = 0;
if (np->features & FE_LED0) {
OUTOFFB (nc_gpcntl, 0x01);
}
if (np->vaddr2) {
if (bootverbose)
printf ("%s: copying script fragments into the on-board RAM ...\n", ncr_name(np));
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,0,0)
memcpy_toio(np->script, np->script0, sizeof(struct script));
#else
memcpy(np->script, np->script0, sizeof(struct script));
#endif
}
OUTW (nc_sien , STO|HTH|MA|SGE|UDC|RST);
OUTB (nc_dien , MDPE|BF|ABRT|SSI|SIR|IID);
if (np->features & FE_ULTRA2) {
OUTONW (nc_sien, SBMC);
np->scsi_mode = INB (nc_stest4) & SMODE;
}
for (i=0;i<MAX_TARGET;i++) {
tcb_p tp = &np->target[i];
tp->sval    = 0;
tp->wval    = np->rv_scntl3;
if (tp->usrsync != 255) {
if (tp->usrsync <= np->maxsync) {
if (tp->usrsync < np->minsync) {
tp->usrsync = np->minsync;
}
}
else
tp->usrsync = 255;
};
if (tp->usrwide > np->maxwide)
tp->usrwide = np->maxwide;
ncr_negotiate (np, tp);
}
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, start));
}
static void ncr_negotiate (struct ncb* np, struct tcb* tp)
{
u_long minsync = tp->usrsync;
if (np->scsi_mode && np->scsi_mode == SMODE_SE) {
if (minsync < 12) minsync = 12;
}
if ((minsync < 50) && (tp->inqdata[2] & 0x0f) < 2)
minsync=50;
if (minsync < np->minsync)
minsync = np->minsync;
if (minsync > np->maxsync)
minsync = 255;
tp->minsync = minsync;
tp->maxoffs = (minsync<255 ? np->maxoffs : 0);
tp->period=0;
tp->widedone=0;
}
static void ncr_getsync(ncb_p np, u_char sfac, u_char *fakp, u_char *scntl3p)
{
u_long	clk = np->clock_khz;
int	div = np->clock_divn;
u_long	fak;
u_long	per;
u_long	kpc;
if	(sfac <= 10)	per = 250;
else if	(sfac == 11)	per = 303;
else if	(sfac == 12)	per = 500;
else			per = 40 * sfac;
kpc = per * clk;
while (--div >= 0)
if (kpc >= (div_10M[div] << 2)) break;
fak = (kpc - 1) / div_10M[div] + 1;
#if 0
per = (fak * div_10M[div]) / clk;
if (div >= 1 && fak < 8) {
u_long fak2, per2;
fak2 = (kpc - 1) / div_10M[div-1] + 1;
per2 = (fak2 * div_10M[div-1]) / clk;
if (per2 < per && fak2 <= 8) {
fak = fak2;
per = per2;
--div;
}
}
#endif
if (fak < 4) fak = 4;
*fakp		= fak - 4;
*scntl3p	= ((div+1) << 4) + (sfac < 25 ? 0x80 : 0);
}
static void ncr_set_sync_wide_status (ncb_p np, u_char target)
{
ccb_p cp;
tcb_p tp = &np->target[target];
OUTB (nc_sxfer, tp->sval);
np->sync_st = tp->sval;
OUTB (nc_scntl3, tp->wval);
np->wide_st = tp->wval;
for (cp = np->ccb; cp; cp = cp->link_ccb) {
if (!cp->cmd) continue;
if (cp->cmd->target != target) continue;
cp->sync_status = tp->sval;
cp->wide_status = tp->wval;
};
}
static void ncr_setsync (ncb_p np, ccb_p cp, u_char scntl3, u_char sxfer)
{
Scsi_Cmnd *cmd;
tcb_p tp;
u_char target = INB (nc_ctest0) & 0x0f;
u_char idiv;
assert (cp);
if (!cp) return;
cmd = cp->cmd;
assert (cmd);
if (!cmd) return;
assert (target == (cmd->target & 0xf));
tp = &np->target[target];
if (!scntl3 || !(sxfer & 0x1f))
scntl3 = np->rv_scntl3;
scntl3 = (scntl3 & 0xf0) | (tp->wval & EWS) | (np->rv_scntl3 & 0x07);
idiv = ((scntl3 >> 4) & 0x7);
if ((sxfer & 0x1f) && idiv)
tp->period = (((sxfer>>5)+4)*div_10M[idiv-1])/np->clock_khz;
else
tp->period = 0xffff;
if (tp->sval == sxfer && tp->wval == scntl3) return;
tp->sval = sxfer;
tp->wval = scntl3;
PRINT_ADDR(cmd);
if (sxfer & 0x01f) {
unsigned f10 = 100000 << (tp->widedone ? tp->widedone -1 : 0);
unsigned mb10 = (f10 + tp->period/2) / tp->period;
char *scsi;
if (tp->period <= 2000) OUTOFFB (nc_stest2, EXT);
if	(tp->period < 500)	scsi = "FAST-40";
else if	(tp->period < 1000)	scsi = "FAST-20";
else if	(tp->period < 2000)	scsi = "FAST-10";
else				scsi = "FAST-5";
printf ("%s %sSCSI %d.%d MB/s (%d ns, offset %d)\n", scsi,
tp->widedone > 1 ? "WIDE " : "",
mb10 / 10, mb10 % 10, tp->period / 10, sxfer & 0x1f);
} else
printf ("%sasynchronous.\n", tp->widedone > 1 ? "wide " : "");
ncr_set_sync_wide_status(np, target);
}
static void ncr_setwide (ncb_p np, ccb_p cp, u_char wide, u_char ack)
{
Scsi_Cmnd *cmd;
u_short target = INB (nc_ctest0) & 0x0f;
tcb_p tp;
u_char	scntl3;
u_char	sxfer;
assert (cp);
if (!cp) return;
cmd = cp->cmd;
assert (cmd);
if (!cmd) return;
assert (target == (cmd->target & 0xf));
tp = &np->target[target];
tp->widedone  =  wide+1;
scntl3 = (tp->wval & (~EWS)) | (wide ? EWS : 0);
sxfer = ack ? 0 : tp->sval;
if (tp->sval == sxfer && tp->wval == scntl3) return;
tp->sval = sxfer;
tp->wval = scntl3;
if (bootverbose >= 2) {
PRINT_ADDR(cmd);
if (scntl3 & EWS)
printf ("WIDE SCSI (16 bit) enabled.\n");
else
printf ("WIDE SCSI disabled.\n");
}
ncr_set_sync_wide_status(np, target);
}
static void ncr_setmaxtags (ncb_p np, tcb_p tp, u_long numtags)
{
int l;
if (numtags > tp->usrtags)
numtags = tp->usrtags;
tp->numtags = numtags;
tp->maxtags = numtags;
for (l=0; l<MAX_LUN; l++) {
lcb_p lp;
u_char wastags;
if (!tp) break;
lp=tp->lp[l];
if (!lp) continue;
wastags = lp->usetags;
ncr_settags (tp, lp);
if (numtags > 1 && lp->reqccbs > 1) {
PRINT_LUN(np, tp - np->target, l);
printf("using tagged command queueing, up to %ld cmds/lun\n", numtags);
}
else if (numtags <= 1 && wastags) {
PRINT_LUN(np, tp - np->target, l);
printf("disabling tagged command queueing\n");
}
};
}
static void ncr_settags (tcb_p tp, lcb_p lp)
{
u_char reqtags, tmp;
if ((!tp) || (!lp)) return;
if ((  tp->inqdata[2] & 0x7) >= 2 &&
(  tp->inqdata[7] & INQ7_QUEUE) && ((tp->inqdata[0] & 0x1f)==0x00)
&& tp->numtags > 1) {
reqtags = tp->numtags;
if (lp->actlink <= 1)
lp->usetags=reqtags;
} else {
reqtags = 1;
if (lp->actlink <= 1)
lp->usetags=0;
};
tmp = lp->actccbs;
if (tmp > reqtags) tmp = reqtags;
lp->reqlink = tmp;
tmp = lp->actlink;
if (tmp < reqtags) tmp = reqtags;
lp->reqccbs = tmp;
}
#ifdef SCSI_NCR_USER_COMMAND_SUPPORT
static void ncr_usercmd (ncb_p np)
{
u_char t;
tcb_p tp;
switch (np->user.cmd) {
case 0: return;
case UC_SETSYNC:
for (t=0; t<MAX_TARGET; t++) {
if (!((np->user.target>>t)&1)) continue;
tp = &np->target[t];
tp->usrsync = np->user.data;
ncr_negotiate (np, tp);
};
break;
case UC_SETTAGS:
if (np->user.data > SCSI_NCR_MAX_TAGS)
np->user.data = SCSI_NCR_MAX_TAGS;
for (t=0; t<MAX_TARGET; t++) {
if (!((np->user.target>>t)&1)) continue;
np->target[t].usrtags = np->user.data;
ncr_setmaxtags (np, &np->target[t], np->user.data);
};
break;
case UC_SETDEBUG:
#ifdef SCSI_NCR_DEBUG_INFO_SUPPORT
ncr_debug = np->user.data;
#endif
break;
case UC_SETORDER:
np->order = np->user.data;
break;
case UC_SETWIDE:
for (t=0; t<MAX_TARGET; t++) {
u_long size;
if (!((np->user.target>>t)&1)) continue;
tp = &np->target[t];
size = np->user.data;
if (size > np->maxwide) size=np->maxwide;
tp->usrwide = size;
ncr_negotiate (np, tp);
};
break;
case UC_SETFLAG:
for (t=0; t<MAX_TARGET; t++) {
if (!((np->user.target>>t)&1)) continue;
tp = &np->target[t];
tp->usrflag = np->user.data;
};
break;
case UC_CLEARPROF:
bzero(&np->profile, sizeof(np->profile));
break;
#ifdef	UC_DEBUG_ERROR_RECOVERY
case UC_DEBUG_ERROR_RECOVERY:
np->debug_error_recovery = np->user.data;
break;
#endif
}
np->user.cmd=0;
}
#endif
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
static void ncr_trigger_errors (ncb_p np)
{
do {
static u_long last = 0l;
if (!np->debug_error_recovery)
break;
if (!last)
last = jiffies;
else if (jiffies < last + 30*HZ)
break;
last = jiffies;
if (np->debug_error_recovery == 1) {
int i;
printf("%s: testing error recovery from SCSI gross error...\n", ncr_name(np));
for (i = 0 ; i < MAX_TARGET ; i++) {
if (np->target[i].sval & 0x1f) {
np->target[i].sval &= ~0x1f;
np->target[i].sval += 2;
}
}
}
else if (np->debug_error_recovery == 2) {
printf("%s: testing error recovery from mid-level driver abort()...\n", ncr_name(np));
np->stalling = 2;
}
else if (np->debug_error_recovery == 3) {
printf("%s: testing error recovery from mid-level driver reset()...\n", ncr_name(np));
np->stalling = 3;
}
else if (np->debug_error_recovery == 4) {
printf("%s: testing data in parity error...\n", ncr_name(np));
np->assert_atn = 1;
}
} while (0);
}
#endif
static void ncr_timeout (ncb_p np)
{
u_long	thistime = jiffies;
u_long	count = 0;
ccb_p cp;
u_long flags;
if (np->release_stage) {
if (np->release_stage == 1) np->release_stage = 2;
return;
}
np->timer.expires =
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
jiffies +
#endif
SCSI_NCR_TIMER_INTERVAL;
add_timer(&np->timer);
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
ncr_trigger_errors (np);
#endif
if (np->settle_time) {
if (np->settle_time <= thistime) {
if (bootverbose > 1)
printf("%s: command processing resumed\n", ncr_name(np));
save_flags(flags); cli();
np->settle_time	= 0;
np->disc	= 1;
requeue_waiting_list(np);
restore_flags(flags);
}
return;
}
if (np->lasttime + (HZ>>2) <= thistime) {
save_flags(flags); cli();
np->lasttime = thistime;
if (np->profile.num_kbytes > (~0UL >> 2))
bzero(&np->profile, sizeof(np->profile));
#if 0
if (thistime < np->heartbeat + HZ + HZ)
np->latetime = 0;
else
np->latetime++;
#endif
for (cp=np->ccb; cp; cp=cp->link_ccb) {
if (!cp->host_status) continue;
count++;
if (cp->cmd && cp->tlimit && cp->tlimit <=
thistime + NCR_TIMEOUT_INCREASE + SCSI_NCR_TIMEOUT_ALERT) {
lcb_p lp;
lp = np->target[cp->cmd->target].lp[cp->cmd->lun];
if (lp && !lp->force_ordered_tag) {
lp->force_ordered_tag = 1;
}
}
if (cp->tlimit) continue;
switch (cp->host_status) {
case HS_BUSY:
case HS_NEGOTIATE:
if (cp->phys.header.launch.l_paddr ==
cpu_to_scr(NCB_SCRIPT_PHYS (np, skip)))
continue;
case HS_DISCONNECT:
cp->host_status=HS_ABORTED;
};
cp->tag = 0;
ncr_complete (np, cp);
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
if (!np->stalling)
#endif
OUTB (nc_istat, SIGP);
}
restore_flags(flags);
}
#ifdef SCSI_NCR_BROKEN_INTR
if (INB(nc_istat) & (INTF|SIP|DIP)) {
save_flags(flags); cli();
if (DEBUG_FLAGS & DEBUG_TINY) printf ("{");
ncr_exception (np);
if (DEBUG_FLAGS & DEBUG_TINY) printf ("}");
restore_flags(flags);
}
#endif
}
static void ncr_log_hard_error(ncb_p np, u_short sist, u_char dstat)
{
u_int32	dsp;
int	script_ofs;
int	script_size;
char	*script_name;
u_char	*script_base;
int	i;
dsp	= INL (nc_dsp);
if (dsp > np->p_script && dsp <= np->p_script + sizeof(struct script)) {
script_ofs	= dsp - np->p_script;
script_size	= sizeof(struct script);
script_base	= (u_char *) np->script;
script_name	= "script";
}
else if (np->p_scripth < dsp &&
dsp <= np->p_scripth + sizeof(struct scripth)) {
script_ofs	= dsp - np->p_scripth;
script_size	= sizeof(struct scripth);
script_base	= (u_char *) np->scripth;
script_name	= "scripth";
} else {
script_ofs	= dsp;
script_size	= 0;
script_base	= 0;
script_name	= "mem";
}
printf ("%s:%d: ERROR (%x:%x) (%x-%x-%x) (%x/%x) @ (%s %x:%08x).\n",
ncr_name (np), (unsigned)INB (nc_ctest0)&0x0f, dstat, sist,
(unsigned)INB (nc_socl), (unsigned)INB (nc_sbcl), (unsigned)INB (nc_sbdl),
(unsigned)INB (nc_sxfer),(unsigned)INB (nc_scntl3), script_name, script_ofs,
(unsigned)INL (nc_dbc));
if (((script_ofs & 3) == 0) &&
(unsigned)script_ofs < script_size) {
printf ("%s: script cmd = %08x\n", ncr_name(np),
(int) *(ncrcmd *)(script_base + script_ofs));
}
printf ("%s: regdump:", ncr_name(np));
for (i=0; i<16;i++)
printf (" %02x", (unsigned)INB_OFF(i));
printf (".\n");
}
void ncr_exception (ncb_p np)
{
u_char	istat, dstat;
u_short	sist;
int	i;
istat = INB (nc_istat);
if (istat & INTF) {
do {
OUTB (nc_istat, (istat & SIGP) | INTF);
istat = INB (nc_istat);
} while (istat & INTF);
if (DEBUG_FLAGS & DEBUG_TINY) printf ("F ");
np->profile.num_fly++;
ncr_wakeup (np, 0);
};
if (!(istat & (SIP|DIP)))
return;
np->profile.num_int++;
if (istat & CABRT)
OUTB (nc_istat, CABRT);
sist  = (istat & SIP) ? INW (nc_sist)  : 0;
dstat = (istat & DIP) ? INB (nc_dstat) : 0;
if (DEBUG_FLAGS & DEBUG_TINY)
printf ("<%d|%x:%x|%x:%x>",
(int)INB(nc_scr0),
dstat,sist,
(unsigned)INL(nc_dsp),
(unsigned)INL(nc_dbc));
if (!(sist  & (STO|GEN|HTH|SGE|UDC|RST)) &&
!(dstat & (MDPE|BF|ABRT|IID))) {
if ((sist & SBMC) && ncr_int_sbmc (np))
return;
if ((sist & PAR)  && ncr_int_par  (np))
return;
if (sist & MA) {
ncr_int_ma (np);
return;
}
if (dstat & SIR) {
ncr_int_sir (np);
return;
}
if (!(sist & (SBMC|PAR)) && !(dstat & SSI)) {
printf(	"%s: unknown interrupt(s) ignored, "
"ISTAT=%x DSTAT=%x SIST=%x\n",
ncr_name(np), istat, dstat, sist);
return;
}
OUTONB (nc_dcntl, (STD|NOCOM));
return;
};
if (sist & RST) {
ncr_init (np, 1, bootverbose ? "scsi reset" : NULL, HS_RESET);
return;
};
if ((sist & STO) &&
!(dstat & (MDPE|BF|ABRT))) {
OUTONB (nc_ctest3, CLF);
ncr_int_sto (np);
return;
};
if (jiffies - np->regtime > 10*HZ) {
np->regtime = jiffies;
for (i = 0; i<sizeof(np->regdump); i++)
((char*)&np->regdump)[i] = INB_OFF(i);
np->regdump.nc_dstat = dstat;
np->regdump.nc_sist  = sist;
};
ncr_log_hard_error(np, sist, dstat);
printf ("%s: have to clear fifos.\n", ncr_name (np));
OUTB (nc_stest3, TE|CSF);
OUTONB (nc_ctest3, CLF);
if ((sist & (SGE)) ||
(dstat & (MDPE|BF|ABORT|IID))) {
ncr_start_reset(np, driver_setup.settle_delay);
return;
};
if (sist & HTH) {
printf ("%s: handshake timeout\n", ncr_name(np));
ncr_start_reset(np, driver_setup.settle_delay);
return;
};
if (sist & UDC) {
printf ("%s: unexpected disconnect\n", ncr_name(np));
if (INB (nc_scr1) != 0xff) {
OUTB (nc_scr1, HS_UNEXPECTED);
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, cleanup));
};
ncr_start_reset(np, driver_setup.settle_delay);
return;
};
printf ("%s: unknown interrupt\n", ncr_name(np));
}
void ncr_int_sto (ncb_p np)
{
u_long dsa, scratcha, diff;
ccb_p cp;
if (DEBUG_FLAGS & DEBUG_TINY) printf ("T");
dsa = INL (nc_dsa);
cp = np->ccb;
while (cp && (CCB_PHYS (cp, phys) != dsa))
cp = cp->link_ccb;
if (cp) {
cp-> host_status = HS_SEL_TIMEOUT;
ncr_complete (np, cp);
};
scratcha = INL (nc_scratcha);
diff = scratcha - NCB_SCRIPTH_PHYS (np, tryloop);
if ((diff <= MAX_START * 20) && !(diff % 20)) {
np->script->startpos[0] = cpu_to_scr(scratcha);
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, start));
return;
};
ncr_init (np, 1, "selection timeout", HS_FAIL);
np->disc = 1;
}
static int ncr_int_sbmc (ncb_p np)
{
u_char scsi_mode = INB (nc_stest4) & SMODE;
printf("%s: SCSI bus mode change from %x to %x.\n",
ncr_name(np), np->scsi_mode, scsi_mode);
np->scsi_mode = scsi_mode;
np->settle_time	= jiffies + HZ;
ncr_init (np, 0, bootverbose ? "scsi mode change" : NULL, HS_RESET);
return 1;
}
static int ncr_int_par (ncb_p np)
{
printf("%s: SCSI parity error detected\n", ncr_name(np));
return 0;
}
static void ncr_int_ma (ncb_p np)
{
u_int32	dbc;
u_int32	rest;
u_int32	dsp;
u_int32	dsa;
u_int32	nxtdsp;
u_int32	*vdsp;
u_int32	oadr, olen;
u_int32	*tblp;
ncrcmd *newcmd;
u_char	cmd, sbcl;
ccb_p	cp;
dsp	= INL (nc_dsp);
dbc	= INL (nc_dbc);
sbcl	= INB (nc_sbcl);
cmd	= dbc >> 24;
rest	= dbc & 0xffffff;
if ((cmd & 1) == 0) {
u_char	ctest5, ss0, ss2;
u_short	delta;
ctest5 = (np->rv_ctest5 & DFS) ? INB (nc_ctest5) : 0;
if (ctest5 & DFS)
delta=(((ctest5 << 8) | (INB (nc_dfifo) & 0xff)) - rest) & 0x3ff;
else
delta=(INB (nc_dfifo) - rest) & 0x7f;
rest += delta;
ss0  = INB (nc_sstat0);
if (ss0 & OLF) rest++;
if (ss0 & ORF) rest++;
if (INB(nc_scntl3) & EWS) {
ss2 = INB (nc_sstat2);
if (ss2 & OLF1) rest++;
if (ss2 & ORF1) rest++;
};
OUTONB (nc_ctest3, CLF );
OUTB (nc_stest3, TE|CSF);
if (DEBUG_FLAGS & (DEBUG_TINY|DEBUG_PHASE))
printf ("P%x%x RL=%d D=%d SS0=%x ", cmd&7, sbcl&7,
(unsigned) rest, (unsigned) delta, ss0);
} else	{
if (DEBUG_FLAGS & (DEBUG_TINY|DEBUG_PHASE))
printf ("P%x%x RL=%d ", cmd&7, sbcl&7, rest);
if ((cmd & 7) != 1) {
OUTONB (nc_ctest3, CLF );
OUTB (nc_stest3, TE|CSF);
}
}
dsa = INL (nc_dsa);
cp  = np->ccb;
while (cp && (CCB_PHYS (cp, phys) != dsa))
cp = cp->link_ccb;
if (!cp) {
printf ("%s: SCSI phase error fixup: CCB already dequeued (0x%08lx)\n",
ncr_name (np), (u_long) np->header.cp);
return;
}
if (cp != np->header.cp) {
printf ("%s: SCSI phase error fixup: CCB address mismatch (0x%08lx != 0x%08lx)\n",
ncr_name (np), (u_long) cp, (u_long) np->header.cp);
}
if (dsp == vtophys (&cp->patch[2])) {
vdsp = &cp->patch[0];
nxtdsp = vdsp[3];
} else if (dsp == vtophys (&cp->patch[6])) {
vdsp = &cp->patch[4];
nxtdsp = vdsp[3];
} else if (dsp > np->p_script && dsp <= np->p_script + sizeof(struct script)) {
vdsp = (u_int32 *) ((char*)np->script - np->p_script + dsp -8);
nxtdsp = dsp;
} else {
vdsp = (u_int32 *) ((char*)np->scripth - np->p_scripth + dsp -8);
nxtdsp = dsp;
};
if (DEBUG_FLAGS & DEBUG_PHASE) {
printf ("\nCP=%p CP2=%p DSP=%x NXT=%x VDSP=%p CMD=%x ",
cp, np->header.cp,
(unsigned)dsp,
(unsigned)nxtdsp, vdsp, cmd);
};
oadr = scr_to_cpu(vdsp[1]);
if (cmd & 0x10) {
tblp = (u_int32 *) ((char*) &cp->phys + oadr);
olen = scr_to_cpu(tblp[0]);
oadr = scr_to_cpu(tblp[1]);
} else {
tblp = (u_int32 *) 0;
olen = scr_to_cpu(vdsp[0]) & 0xffffff;
};
if (DEBUG_FLAGS & DEBUG_PHASE) {
printf ("OCMD=%x\nTBLP=%p OLEN=%x OADR=%x\n",
(unsigned) (scr_to_cpu(vdsp[0]) >> 24),
tblp,
(unsigned) olen,
(unsigned) oadr);
};
if (cmd != (scr_to_cpu(vdsp[0]) >> 24)) {
PRINT_ADDR(cp->cmd);
printf ("internal error: cmd=%02x != %02x=(vdsp[0] >> 24)\n",
(unsigned)cmd, (unsigned)scr_to_cpu(vdsp[0]) >> 24);
return;
}
#ifdef	SCSI_NCR_DEBUG_ERROR_RECOVERY_SUPPORT
if ((cmd & 7) == 1 && np->assert_atn) {
np->assert_atn = 0;
OUTONB(nc_socl, CATN);
}
#endif
if (cmd & 0x06) {
PRINT_ADDR(cp->cmd);
printf ("phase change %x-%x %d@%08x resid=%d.\n",
cmd&7, sbcl&7, (unsigned)olen,
(unsigned)oadr, (unsigned)rest);
OUTONB (nc_dcntl, (STD|NOCOM));
return;
};
newcmd = cp->patch;
if (cp->phys.header.savep == cpu_to_scr(vtophys (newcmd))) newcmd+=4;
newcmd[0] = cpu_to_scr(((cmd & 0x0f) << 24) | rest);
newcmd[1] = cpu_to_scr(oadr + olen - rest);
newcmd[2] = cpu_to_scr(SCR_JUMP);
newcmd[3] = cpu_to_scr(nxtdsp);
if (DEBUG_FLAGS & DEBUG_PHASE) {
PRINT_ADDR(cp->cmd);
printf ("newcmd[%d] %x %x %x %x.\n",
(int) (newcmd - cp->patch),
(unsigned)scr_to_cpu(newcmd[0]),
(unsigned)scr_to_cpu(newcmd[1]),
(unsigned)scr_to_cpu(newcmd[2]),
(unsigned)scr_to_cpu(newcmd[3]));
}
np->profile.num_break++;
OUTL (nc_temp, vtophys (newcmd));
if ((cmd & 7) == 0)
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, dispatch));
else
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, checkatn));
}
static int ncr_show_msg (u_char * msg)
{
u_char i;
printf ("%x",*msg);
if (*msg==M_EXTENDED) {
for (i=1;i<8;i++) {
if (i-1>msg[1]) break;
printf ("-%x",msg[i]);
};
return (i+1);
} else if ((*msg & 0xf0) == 0x20) {
printf ("-%x",msg[1]);
return (2);
};
return (1);
}
void ncr_int_sir (ncb_p np)
{
u_char scntl3;
u_char chg, ofs, per, fak, wide;
u_char num = INB (nc_dsps);
ccb_p	cp=0;
u_long	dsa;
u_char	target = INB (nc_ctest0) & 0x0f;
tcb_p	tp     = &np->target[target];
int     i;
if (DEBUG_FLAGS & DEBUG_TINY) printf ("I#%d", num);
switch (num) {
case SIR_SENSE_RESTART:
case SIR_STALL_RESTART:
break;
case SIR_STALL_QUEUE:
goto out;
default:
dsa = INL (nc_dsa);
cp = np->ccb;
while (cp && (CCB_PHYS (cp, phys) != dsa))
cp = cp->link_ccb;
assert (cp);
if (!cp)
goto out;
assert (cp == np->header.cp);
if (cp != np->header.cp)
goto out;
}
switch (num) {
u_long endp;
case SIR_DATA_IO_IS_OUT:
case SIR_DATA_IO_IS_IN:
if (num == SIR_DATA_IO_IS_OUT) {
endp = NCB_SCRIPTH_PHYS (np, data_out) + MAX_SCATTER*16;
cp->phys.header.goalp = cpu_to_scr(endp + 8);
cp->phys.header.savep =
cpu_to_scr(endp - cp->segments*16);
} else {
endp = NCB_SCRIPT_PHYS (np, data_in)  + MAX_SCATTER*16;
cp->phys.header.goalp = cpu_to_scr(endp + 8);
cp->phys.header.savep =
cpu_to_scr(endp - cp->segments*16);
}
cp->phys.header.lastp	= cp->phys.header.savep;
np->header.savep	= cp->phys.header.savep;
np->header.goalp	= cp->phys.header.goalp;
np->header.lastp	= cp->phys.header.lastp;
OUTL (nc_temp,	scr_to_cpu(np->header.savep));
OUTL (nc_dsp,	scr_to_cpu(np->header.savep));
return;
case SIR_SENSE_RESTART:
if (DEBUG_FLAGS & DEBUG_RESTART)
printf ("%s: int#%d",ncr_name (np),num);
cp = (ccb_p) 0;
for (i=0; i<MAX_TARGET; i++) {
if (DEBUG_FLAGS & DEBUG_RESTART) printf (" t%d", i);
tp = &np->target[i];
if (DEBUG_FLAGS & DEBUG_RESTART) printf ("+");
cp = tp->hold_cp;
if (!cp) continue;
if (DEBUG_FLAGS & DEBUG_RESTART) printf ("+");
if ((cp->host_status==HS_BUSY) &&
(cp->scsi_status==S_CHECK_COND))
break;
if (DEBUG_FLAGS & DEBUG_RESTART) printf ("- (remove)");
tp->hold_cp = cp = (ccb_p) 0;
};
if (cp) {
if (DEBUG_FLAGS & DEBUG_RESTART)
printf ("+ restart job ..\n");
OUTL (nc_dsa, CCB_PHYS (cp, phys));
OUTL (nc_dsp, NCB_SCRIPTH_PHYS (np, getcc));
return;
};
if (DEBUG_FLAGS & DEBUG_RESTART) printf (" -- remove trap\n");
np->script->start0[0] =  cpu_to_scr(SCR_INT ^ IFFALSE (0));
break;
case SIR_SENSE_FAILED:
if (DEBUG_FLAGS & DEBUG_RESTART) {
PRINT_ADDR(cp->cmd);
printf ("in getcc reselect by t%d.\n",
(int)INB(nc_ssid) & 0x0f);
}
cp->host_status = HS_BUSY;
cp->scsi_status = S_CHECK_COND;
np->target[cp->cmd->target].hold_cp = cp;
np->script->start0[0] =  cpu_to_scr(SCR_INT);
break;
case SIR_NEGO_FAILED:
OUTB (HS_PRT, HS_BUSY);
case SIR_NEGO_PROTO:
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("negotiation failed sir=%x status=%x.\n",
num, cp->nego_status);
};
switch (cp->nego_status) {
case NS_SYNC:
ncr_setsync (np, cp, 0, 0xe0);
break;
case NS_WIDE:
ncr_setwide (np, cp, 0, 0);
break;
};
np->msgin [0] = M_NOOP;
np->msgout[0] = M_NOOP;
cp->nego_status = 0;
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, dispatch));
break;
case SIR_NEGO_SYNC:
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("sync msgin: ");
(void) ncr_show_msg (np->msgin);
printf (".\n");
};
chg = 0;
per = np->msgin[3];
ofs = np->msgin[4];
if (ofs==0) per=255;
if (ofs)
tp->inqdata[7] |= INQ7_SYNC;
if (per < np->minsync)
{chg = 1; per = np->minsync;}
if (per < tp->minsync)
{chg = 1; per = tp->minsync;}
if (ofs > tp->maxoffs)
{chg = 1; ofs = tp->maxoffs;}
fak	= 7;
scntl3	= 0;
if (ofs != 0) {
ncr_getsync(np, per, &fak, &scntl3);
if (fak > 7) {
chg = 1;
ofs = 0;
}
}
if (ofs == 0) {
fak	= 7;
per	= 0;
scntl3	= 0;
tp->minsync = 0;
}
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("sync: per=%d scntl3=0x%x ofs=%d fak=%d chg=%d.\n",
per, scntl3, ofs, fak, chg);
}
if (INB (HS_PRT) == HS_NEGOTIATE) {
OUTB (HS_PRT, HS_BUSY);
switch (cp->nego_status) {
case NS_SYNC:
if (chg) {
ncr_setsync (np, cp, 0, 0xe0);
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, msg_bad));
} else {
ncr_setsync (np, cp, scntl3, (fak<<5)|ofs);
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, clrack));
};
return;
case NS_WIDE:
ncr_setwide (np, cp, 0, 0);
break;
};
};
if (np->unit < SCSI_NCR_MAX_HOST) {
tp->inqdata[7] &=
(target_capabilities[np->unit].and_map[target]);
if (!(tp->inqdata[7] & INQ7_SYNC)) {
ofs = 0;
fak = 7;
}
}
ncr_setsync (np, cp, scntl3, (fak<<5)|ofs);
np->msgout[0] = M_EXTENDED;
np->msgout[1] = 3;
np->msgout[2] = M_X_SYNC_REQ;
np->msgout[3] = per;
np->msgout[4] = ofs;
cp->nego_status = NS_SYNC;
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("sync msgout: ");
(void) ncr_show_msg (np->msgout);
printf (".\n");
}
if (!ofs) {
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, msg_bad));
return;
}
np->msgin [0] = M_NOOP;
break;
case SIR_NEGO_WIDE:
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("wide msgin: ");
(void) ncr_show_msg (np->msgin);
printf (".\n");
};
chg  = 0;
wide = np->msgin[3];
if (wide)
tp->inqdata[7] |= INQ7_WIDE16;
if (wide > tp->usrwide)
{chg = 1; wide = tp->usrwide;}
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("wide: wide=%d chg=%d.\n", wide, chg);
}
if (INB (HS_PRT) == HS_NEGOTIATE) {
OUTB (HS_PRT, HS_BUSY);
switch (cp->nego_status) {
case NS_WIDE:
if (chg) {
ncr_setwide (np, cp, 0, 1);
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, msg_bad));
} else {
ncr_setwide (np, cp, wide, 1);
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, clrack));
};
return;
case NS_SYNC:
ncr_setsync (np, cp, 0, 0xe0);
break;
};
};
ncr_setwide (np, cp, wide, 1);
np->msgout[0] = M_EXTENDED;
np->msgout[1] = 2;
np->msgout[2] = M_X_WIDE_REQ;
np->msgout[3] = wide;
np->msgin [0] = M_NOOP;
cp->nego_status = NS_WIDE;
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printf ("wide msgout: ");
(void) ncr_show_msg (np->msgin);
printf (".\n");
}
break;
case SIR_REJECT_RECEIVED:
PRINT_ADDR(cp->cmd);
printf ("M_REJECT received (%x:%x).\n",
(unsigned)scr_to_cpu(np->lastmsg), np->msgout[0]);
break;
case SIR_REJECT_SENT:
PRINT_ADDR(cp->cmd);
printf ("M_REJECT sent for ");
(void) ncr_show_msg (np->msgin);
printf (".\n");
break;
case SIR_IGN_RESIDUE:
PRINT_ADDR(cp->cmd);
printf ("M_IGN_RESIDUE received, but not yet implemented.\n");
break;
case SIR_MISSING_SAVE:
PRINT_ADDR(cp->cmd);
printf ("M_DISCONNECT received, but datapointer not saved: "
"data=%x save=%x goal=%x.\n",
(unsigned) INL (nc_temp),
(unsigned) scr_to_cpu(np->header.savep),
(unsigned) scr_to_cpu(np->header.goalp));
break;
#if 0
case SIR_STALL_QUEUE:
PRINT_ADDR(cp->cmd);
printf ("queue full.\n");
np->script->start1[0] =  cpu_to_scr(SCR_INT);
ncr_setmaxtags (np, &np->target[target], 0);
case SIR_STALL_RESTART:
cp = np->ccb;
while (cp && cp->host_status != HS_DISCONNECT)
cp = cp->link_ccb;
if (cp) {
OUTL (nc_dsp, NCB_SCRIPT_PHYS (np, reselect));
return;
};
printf ("%s: queue empty.\n", ncr_name (np));
np->script->start1[0] =  cpu_to_scr(SCR_INT ^ IFFALSE (0));
break;
#endif
};
out:
OUTONB (nc_dcntl, (STD|NOCOM));
}
static	ccb_p ncr_get_ccb
(ncb_p np, u_long target, u_long lun)
{
lcb_p lp;
ccb_p cp = (ccb_p) 0;
lp = np->target[target].lp[lun];
if (lp && lp->opennings && (!lp->active || lp->active < lp->reqlink)) {
cp = lp->next_ccb;
while (cp && cp->magic) cp = cp->next_ccb;
if (cp) {
++lp->active;
--lp->opennings;
}
}
if ((!cp) && lp && lp->actccbs > 0)
return ((ccb_p) 0);
if (!cp) cp = np->ccb;
#if 0
while (cp->magic) {
if (flags & SCSI_NOSLEEP) break;
if (tsleep ((caddr_t)cp, PRIBIO|PCATCH, "ncr", 0))
break;
};
#endif
if (cp->magic)
return ((ccb_p) 0);
cp->magic = 1;
return (cp);
}
void ncr_free_ccb (ncb_p np, ccb_p cp, u_long target, u_long lun)
{
lcb_p lp;
assert (cp != NULL);
lp = np->target[target].lp[lun];
if (lp) {
--lp->active;
++lp->opennings;
}
cp -> host_status = HS_IDLE;
cp -> magic = 0;
#if 0
if (cp == np->ccb)
wakeup ((caddr_t) cp);
#endif
}
static	void ncr_alloc_ccb (ncb_p np, u_long target, u_long lun)
{
tcb_p tp;
lcb_p lp;
ccb_p cp;
assert (np != NULL);
if (target>=MAX_TARGET) return;
if (lun   >=MAX_LUN   ) return;
tp=&np->target[target];
if (!tp->jump_tcb.l_cmd) {
tp->jump_tcb.l_cmd   =
cpu_to_scr((SCR_JUMP^IFFALSE (DATA (0x80 + target))));
tp->jump_tcb.l_paddr = np->jump_tcb.l_paddr;
tp->getscr[0] =	(np->features & FE_PFEN) ?
cpu_to_scr(SCR_COPY(1)):cpu_to_scr(SCR_COPY_F(1));
tp->getscr[1] = cpu_to_scr(vtophys (&tp->sval));
tp->getscr[2] =
cpu_to_scr(np->paddr + offsetof (struct ncr_reg, nc_sxfer));
tp->getscr[3] =	(np->features & FE_PFEN) ?
cpu_to_scr(SCR_COPY(1)):cpu_to_scr(SCR_COPY_F(1));
tp->getscr[4] = cpu_to_scr(vtophys (&tp->wval));
tp->getscr[5] =
cpu_to_scr(np->paddr + offsetof (struct ncr_reg, nc_scntl3));
assert (( (offsetof(struct ncr_reg, nc_sxfer) ^
offsetof(struct tcb    , sval    )) &3) == 0);
assert (( (offsetof(struct ncr_reg, nc_scntl3) ^
offsetof(struct tcb    , wval    )) &3) == 0);
tp->call_lun.l_cmd   = cpu_to_scr(SCR_CALL);
tp->call_lun.l_paddr =
cpu_to_scr(NCB_SCRIPT_PHYS (np, resel_lun));
tp->jump_lcb.l_cmd   = cpu_to_scr(SCR_JUMP);
tp->jump_lcb.l_paddr = cpu_to_scr(NCB_SCRIPTH_PHYS (np, abort));
np->jump_tcb.l_paddr = cpu_to_scr(vtophys (&tp->jump_tcb));
}
lp = tp->lp[lun];
if (!lp) {
lp = (lcb_p) m_alloc (sizeof (struct lcb), LCB_ALIGN_SHIFT);
if (!lp) return;
if (DEBUG_FLAGS & DEBUG_ALLOC) {
PRINT_LUN(np, target, lun);
printf ("new lcb @%p.\n", lp);
}
bzero (lp, sizeof (*lp));
lp->jump_lcb.l_cmd   =
cpu_to_scr(SCR_JUMP ^ IFFALSE (DATA (lun)));
lp->jump_lcb.l_paddr = tp->jump_lcb.l_paddr;
lp->call_tag.l_cmd   = cpu_to_scr(SCR_CALL);
lp->call_tag.l_paddr =
cpu_to_scr(NCB_SCRIPT_PHYS (np, resel_tag));
lp->jump_ccb.l_cmd   = cpu_to_scr(SCR_JUMP);
lp->jump_ccb.l_paddr =
cpu_to_scr(NCB_SCRIPTH_PHYS (np, aborttag));
lp->actlink = 1;
lp->active  = 1;
tp->jump_lcb.l_paddr = cpu_to_scr(vtophys (&lp->jump_lcb));
tp->lp[lun] = lp;
ncr_setmaxtags (np, tp, driver_setup.default_tags);
}
if (np->actccbs >= MAX_START-2) return;
if (lp->actccbs && (lp->actccbs >= lp->reqccbs))
return;
cp = (ccb_p) m_alloc (sizeof (struct ccb), CCB_ALIGN_SHIFT);
if (!cp)
return;
if (DEBUG_FLAGS & DEBUG_ALLOC) {
PRINT_LUN(np, target, lun);
printf ("new ccb @%p.\n", cp);
}
lp->actccbs++;
np->actccbs++;
bzero (cp, sizeof (*cp));
cp->p_ccb	     = vtophys (cp);
cp->jump_ccb.l_cmd   = cpu_to_scr(SCR_JUMP);
cp->jump_ccb.l_paddr = lp->jump_ccb.l_paddr;
lp->jump_ccb.l_paddr = cpu_to_scr(CCB_PHYS (cp, jump_ccb));
cp->call_tmp.l_cmd   = cpu_to_scr(SCR_CALL);
cp->call_tmp.l_paddr = cpu_to_scr(NCB_SCRIPT_PHYS (np, resel_tmp));
cp->link_ccb      = np->ccb->link_ccb;
np->ccb->link_ccb = cp;
cp->next_ccb	= lp->next_ccb;
lp->next_ccb	= cp;
}
static void ncr_opennings (ncb_p np, lcb_p lp, Scsi_Cmnd * cmd)
{
if (lp->actlink > lp->reqlink) {
u_char diff = lp->actlink - lp->reqlink;
if (!diff) return;
if (diff > lp->opennings)
diff = lp->opennings;
lp->opennings	-= diff;
lp->actlink	-= diff;
if (DEBUG_FLAGS & DEBUG_TAGS)
printf ("%s: actlink: diff=%d, new=%d, req=%d\n",
ncr_name(np), diff, lp->actlink, lp->reqlink);
return;
};
if (lp->reqlink > lp->actlink) {
u_char diff = lp->reqlink - lp->actlink;
lp->opennings	+= diff;
lp->actlink	+= diff;
#if 0
wakeup ((caddr_t) xp->sc_link);
#endif
if (DEBUG_FLAGS & DEBUG_TAGS)
printf ("%s: actlink: diff=%d, new=%d, req=%d\n",
ncr_name(np), diff, lp->actlink, lp->reqlink);
};
}
static	int	ncr_scatter(ccb_p cp, Scsi_Cmnd *cmd)
{
struct scr_tblmove *data;
int segment	= 0;
int use_sg	= (int) cmd->use_sg;
#if 0
bzero (cp->phys.data, sizeof (cp->phys.data));
#endif
data		= cp->phys.data;
cp->data_len	= 0;
if (!use_sg) {
if (cmd->request_bufflen) {
data = &data[MAX_SCATTER - 1];
data[0].addr = cpu_to_scr(vtophys(cmd->request_buffer));
data[0].size = cpu_to_scr(cmd->request_bufflen);
cp->data_len = cmd->request_bufflen;
segment = 1;
}
}
else if (use_sg <= MAX_SCATTER) {
struct scatterlist *scatter = (struct scatterlist *)cmd->buffer;
data = &data[MAX_SCATTER - use_sg];
while (segment < use_sg) {
data[segment].addr =
cpu_to_scr(vtophys(scatter[segment].address));
data[segment].size =
cpu_to_scr(scatter[segment].length);
cp->data_len	   += scatter[segment].length;
++segment;
}
}
else {
return -1;
}
return segment;
}
#ifndef NCR_IOMAPPED
__initfunc(
static int ncr_regtest (struct ncb* np)
)
{
register volatile u_long data;
data = 0xffffffff;
OUTL_OFF(offsetof(struct ncr_reg, nc_dstat), data);
data = INL_OFF(offsetof(struct ncr_reg, nc_dstat));
#if 1
if (data == 0xffffffff) {
#else
if ((data & 0xe2f0fffd) != 0x02000080) {
#endif
printf ("CACHE TEST FAILED: reg dstat-sstat2 readback %x.\n",
(unsigned) data);
return (0x10);
};
return (0);
}
#endif
__initfunc(
static int ncr_snooptest (struct ncb* np)
)
{
u_long	ncr_rd, ncr_wr, ncr_bk, host_rd, host_wr, pc, err=0;
int	i;
#ifndef NCR_IOMAPPED
if (np->reg) {
err |= ncr_regtest (np);
if (err) return (err);
}
#endif
pc  = NCB_SCRIPTH_PHYS (np, snooptest);
host_wr = 1;
ncr_wr  = 2;
np->ncr_cache = cpu_to_scr(host_wr);
OUTL (nc_temp, ncr_wr);
OUTL (nc_dsp, pc);
for (i=0; i<NCR_SNOOP_TIMEOUT; i++)
if (INB(nc_istat) & (INTF|SIP|DIP))
break;
pc = INL (nc_dsp);
host_rd = scr_to_cpu(np->ncr_cache);
ncr_rd  = INL (nc_scratcha);
ncr_bk  = INL (nc_temp);
OUTB (nc_istat,  SRST);
DELAY (1000);
OUTB (nc_istat,  0   );
if (i>=NCR_SNOOP_TIMEOUT) {
printf ("CACHE TEST FAILED: timeout.\n");
return (0x20);
};
if (pc != NCB_SCRIPTH_PHYS (np, snoopend)+8) {
printf ("CACHE TEST FAILED: script execution failed.\n");
printf ("start=%08lx, pc=%08lx, end=%08lx\n",
(u_long) NCB_SCRIPTH_PHYS (np, snooptest), pc,
(u_long) NCB_SCRIPTH_PHYS (np, snoopend) +8);
return (0x40);
};
if (host_wr != ncr_rd) {
printf ("CACHE TEST FAILED: host wrote %d, ncr read %d.\n",
(int) host_wr, (int) ncr_rd);
err |= 1;
};
if (host_rd != ncr_wr) {
printf ("CACHE TEST FAILED: ncr wrote %d, host read %d.\n",
(int) ncr_wr, (int) host_rd);
err |= 2;
};
if (ncr_bk != ncr_wr) {
printf ("CACHE TEST FAILED: ncr wrote %d, read back %d.\n",
(int) ncr_wr, (int) ncr_bk);
err |= 4;
};
return (err);
}
#ifdef SCSI_NCR_PROFILE_SUPPORT
#define ncr_delta(from, to) \
( ((to) && (from))? (to) - (from) : -1 )
#define PROFILE  cp->phys.header.stamp
static	void ncb_profile (ncb_p np, ccb_p cp)
{
int co, st, en, di, se, post,work,disc;
u_long diff;
PROFILE.end = jiffies;
st = ncr_delta (PROFILE.start,PROFILE.status);
if (st<0) return;
co = ncr_delta (PROFILE.start,PROFILE.command);
if (co<0) return;
en = ncr_delta (PROFILE.start,PROFILE.end),
di = ncr_delta (PROFILE.start,PROFILE.disconnect),
se = ncr_delta (PROFILE.start,PROFILE.select);
post = en - st;
if (di>=0) disc = se-di; else  disc = 0;
work = (st - co) - disc;
diff = (np->disc_phys - np->disc_ref) & 0xff;
np->disc_ref += diff;
np->profile.num_trans	+= 1;
if (cp->cmd) {
np->profile.num_kbytes	+= (cp->cmd->request_bufflen >> 10);
np->profile.rest_bytes	+= (cp->cmd->request_bufflen & (0x400-1));
if (np->profile.rest_bytes >= 0x400) {
++np->profile.num_kbytes;
np->profile.rest_bytes	-= 0x400;
}
}
np->profile.num_disc	+= diff;
np->profile.ms_setup	+= co;
np->profile.ms_data	+= work;
np->profile.ms_disc	+= disc;
np->profile.ms_post	+= post;
}
#undef PROFILE
#endif
struct table_entry {
char *	manufacturer;
char *	model;
char *	version;
u_long	info;
};
static struct table_entry device_tab[] =
{
#ifdef NCR_GETCC_WITHMSG
{"", "", "", QUIRK_NOMSG},
{"SONY", "SDT-5000", "3.17", QUIRK_NOMSG},
{"WangDAT", "Model 2600", "01.7", QUIRK_NOMSG},
{"WangDAT", "Model 3200", "02.2", QUIRK_NOMSG},
{"WangDAT", "Model 1300", "02.4", QUIRK_NOMSG},
#endif
{"", "", "", 0}
};
static u_long ncr_lookup(char * id)
{
struct table_entry * p = device_tab;
char *d, *r, c;
for (;;p++) {
d = id+8;
r = p->manufacturer;
while ((c=*r++)) if (c!=*d++) break;
if (c) continue;
d = id+16;
r = p->model;
while ((c=*r++)) if (c!=*d++) break;
if (c) continue;
d = id+32;
r = p->version;
while ((c=*r++)) if (c!=*d++) break;
if (c) continue;
return (p->info);
}
}
static void ncr_selectclock(ncb_p np, u_char scntl3)
{
if (np->multiplier < 2) {
OUTB(nc_scntl3,	scntl3);
return;
}
if (bootverbose >= 2)
printf ("%s: enabling clock multiplier\n", ncr_name(np));
OUTB(nc_stest1, DBLEN);
if (np->multiplier > 2) {
int i = 20;
while (!(INB(nc_stest4) & LCKFRQ) && --i > 0)
DELAY(20);
if (!i)
printf("%s: the chip cannot lock the frequency\n", ncr_name(np));
} else
DELAY(20);
OUTB(nc_stest3, HSC);
OUTB(nc_scntl3,	scntl3);
OUTB(nc_stest1, (DBLEN|DBLSEL));
OUTB(nc_stest3, 0x00|TE);
}
__initfunc(
static unsigned ncrgetfreq (ncb_p np, int gen)
)
{
unsigned ms = 0;
OUTB (nc_stest1, 0);
OUTW (nc_sien , 0);
(void) INW (nc_sist);
OUTB (nc_dien , 0);
(void) INW (nc_sist);
OUTB (nc_scntl3, 4);
OUTB (nc_stime1, 0);
OUTB (nc_stime1, gen);
while (!(INW(nc_sist) & GEN) && ms++ < 100000)
DELAY(1000);
OUTB (nc_stime1, 0);
OUTB (nc_scntl3, 0);
if (bootverbose >= 2)
printf ("%s: Delay (GEN=%d): %u msec\n", ncr_name(np), gen, ms);
return ms ? ((1 << gen) * 4340) / ms : 0;
}
__initfunc(
static void ncr_getclock (ncb_p np, int mult)
)
{
unsigned char scntl3 = INB(nc_scntl3);
unsigned char stest1 = INB(nc_stest1);
unsigned f1;
np->multiplier = 1;
f1 = 40000;
if (mult > 1 && (stest1 & (DBLEN+DBLSEL)) == DBLEN+DBLSEL) {
if (bootverbose >= 2)
printf ("%s: clock multiplier found\n", ncr_name(np));
np->multiplier = mult;
}
if (np->multiplier != mult || (scntl3 & 7) < 3 || !(scntl3 & 1)) {
unsigned f2;
OUTB(nc_istat, SRST); DELAY(5); OUTB(nc_istat, 0);
(void) ncrgetfreq (np, 11);
f1 = ncrgetfreq (np, 11);
f2 = ncrgetfreq (np, 11);
if (bootverbose)
printf ("%s: NCR clock is %uKHz, %uKHz\n", ncr_name(np), f1, f2);
if (f1 > f2) f1 = f2;
if	(f1 <	45000)		f1 =  40000;
else if (f1 <	55000)		f1 =  50000;
else				f1 =  80000;
if (f1 < 80000 && mult > 1) {
if (bootverbose >= 2)
printf ("%s: clock multiplier assumed\n", ncr_name(np));
np->multiplier	= mult;
}
} else {
if	((scntl3 & 7) == 3)	f1 =  40000;
else if	((scntl3 & 7) == 5)	f1 =  80000;
else 				f1 = 160000;
f1 /= np->multiplier;
}
f1		*= np->multiplier;
np->clock_khz	= f1;
}
#ifndef uchar
#define uchar unsigned char
#endif
#ifndef ushort
#define ushort unsigned short
#endif
#ifndef ulong
#define ulong unsigned long
#endif
__initfunc(
void ncr53c8xx_setup(char *str, int *ints)
)
{
#ifdef SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT
char *cur = str;
char *pc, *pv;
int val;
int base;
int c;
while (cur != NULL && (pc = strchr(cur, ':')) != NULL) {
val = 0;
pv = pc;
c = *++pv;
if	(c == 'n')
val = 0;
else if	(c == 'y')
val = 1;
else {
base = 0;
#if 0
if	(c == '0') {
c = *pv++;
base = 8;
}
if	(c == 'x') {
++pv;
base = 16;
}
else if (c >= '0' && c <= '9')
base = 10;
else
break;
#endif
val = (int) simple_strtoul(pv, NULL, base);
}
if	(!strncmp(cur, "mpar:", 5))
driver_setup.master_parity	= val;
else if	(!strncmp(cur, "spar:", 5))
driver_setup.scsi_parity	= val;
else if	(!strncmp(cur, "disc:", 5))
driver_setup.disconnection	= val;
else if	(!strncmp(cur, "specf:", 6))
driver_setup.special_features = val;
else if	(!strncmp(cur, "ultra:", 6))
driver_setup.ultra_scsi	= val;
else if	(!strncmp(cur, "fsn:", 4))
driver_setup.force_sync_nego	= val;
else if	(!strncmp(cur, "revprob:", 8))
driver_setup.reverse_probe	= val;
else if	(!strncmp(cur, "tags:", 5)) {
if (val > SCSI_NCR_MAX_TAGS)
val = SCSI_NCR_MAX_TAGS;
driver_setup.default_tags	= val;
}
else if	(!strncmp(cur, "sync:", 5))
driver_setup.default_sync	= val;
else if	(!strncmp(cur, "verb:", 5))
driver_setup.verbose	= val;
else if	(!strncmp(cur, "debug:", 6))
driver_setup.debug	= val;
else if	(!strncmp(cur, "burst:", 6))
driver_setup.burst_max	= val;
else if	(!strncmp(cur, "led:", 4))
driver_setup.led_pin	= val;
else if	(!strncmp(cur, "wide:", 5))
driver_setup.max_wide	= val? 1:0;
else if	(!strncmp(cur, "settle:", 7))
driver_setup.settle_delay= val;
else if	(!strncmp(cur, "diff:", 5))
driver_setup.diff_support= val;
else if	(!strncmp(cur, "irqm:", 5))
driver_setup.irqm	= val;
else if	(!strncmp(cur, "pcifix:", 7))
driver_setup.pci_fix_up	= val;
else if	(!strncmp(cur, "buschk:", 7))
driver_setup.bus_check	= val;
#ifdef SCSI_NCR_NVRAM_SUPPORT
else if	(!strncmp(cur, "nvram:", 6))
driver_setup.use_nvram	= val;
#endif
else if	(!strncmp(cur, "safe:", 5) && val)
memcpy(&driver_setup, &driver_safe_setup, sizeof(driver_setup));
else
printf("ncr53c8xx_setup: unexpected boot option '%.*s' ignored\n", (int)(pc-cur+1), cur);
#ifdef MODULE
if ((cur = strchr(cur, ' ')) != NULL)
#else
if ((cur = strchr(cur, ',')) != NULL)
#endif
++cur;
}
#endif
}
static int ncr53c8xx_pci_init(Scsi_Host_Template *tpnt,
uchar bus, uchar device_fn, ncr_device *device);
__initfunc(
static void ncr_print_driver_setup(void)
)
{
#define YesNo(y)	y ? 'y' : 'n'
printk("ncr53c8xx: setup=disc:%c,specf:%d,ultra:%c,tags:%d,sync:%d,burst:%d,wide:%c,diff:%d\n",
YesNo(driver_setup.disconnection),
driver_setup.special_features,
YesNo(driver_setup.ultra_scsi),
driver_setup.default_tags,
driver_setup.default_sync,
driver_setup.burst_max,
YesNo(driver_setup.max_wide),
driver_setup.diff_support);
printk("ncr53c8xx: setup=mpar:%c,spar:%c,fsn=%c,verb:%d,debug:0x%x,led:%c,settle:%d,irqm:%d\n",
YesNo(driver_setup.master_parity),
YesNo(driver_setup.scsi_parity),
YesNo(driver_setup.force_sync_nego),
driver_setup.verbose,
driver_setup.debug,
YesNo(driver_setup.led_pin),
driver_setup.settle_delay,
driver_setup.irqm);
#undef YesNo
}
static ncr_chip	ncr_chip_table[] __initdata	= SCSI_NCR_CHIP_TABLE;
static ushort	ncr_chip_ids[]   __initdata	= SCSI_NCR_CHIP_IDS;
#ifdef SCSI_NCR_NVRAM_SUPPORT
__initfunc(
static int
ncr_attach_using_nvram(Scsi_Host_Template *tpnt, int nvram_index, int count, ncr_device device[])
)
{
int i, j;
int attach_count = 0;
ncr_nvram  *nvram;
ncr_device *devp;
if (!nvram_index)
return 0;
for (i = 0, nvram_index = -1; i < count; i++) {
devp  = &device[i];
nvram = devp->nvram;
if (!nvram)
continue;
if (nvram->type == SCSI_NCR_SYMBIOS_NVRAM) {
if (nvram_index == -1)
nvram_index = i;
#ifdef SCSI_NCR_DEBUG_NVRAM
printf("ncr53c8xx: NVRAM: Symbios format Boot Block, 53c%s, PCI bus %d, device %d, function %d\n",
devp->chip.name, devp->slot.bus,
(int) (devp->slot.device_fn & 0xf8) >> 3,
(int) devp->slot.device_fn & 7);
for (j = 0 ; j < 4 ; j++) {
Symbios_host *h = &nvram->data.Symbios.host[j];
printf("ncr53c8xx: BOOT[%d] device_id=%04x vendor_id=%04x device_fn=%02x io_port=%04x %s\n",
j,		h->device_id,	h->vendor_id,
h->device_fn,	h->io_port,
(h->flags & SYMBIOS_INIT_SCAN_AT_BOOT) ? "SCAN AT BOOT" : "");
}
}
else if (nvram->type == SCSI_NCR_TEKRAM_NVRAM) {
printf("ncr53c8xx: NVRAM: Tekram format data, 53c%s, PCI bus %d, device %d, function %d\n",
devp->chip.name, devp->slot.bus,
(int) (devp->slot.device_fn & 0xf8) >> 3,
(int) devp->slot.device_fn & 7);
#endif
}
}
if (nvram_index >= 0 && nvram_index < count)
nvram = device[nvram_index].nvram;
else
nvram = 0;
if (!nvram)
goto out;
for (i = 0; i < 4; i++) {
Symbios_host *h = &nvram->data.Symbios.host[i];
for (j = 0 ; j < count ; j++) {
devp = &device[j];
if (h->device_fn == devp->slot.device_fn &&
#if 0
h->bus	 == devp->slot.bus	 &&
#endif
h->device_id == devp->chip.device_id)
break;
}
if (j < count && !devp->attach_done) {
if (!ncr_attach (tpnt, attach_count, devp))
attach_count++;
devp->attach_done = 1;
}
}
out:
return attach_count;
}
#endif
__initfunc(
int ncr53c8xx_detect(Scsi_Host_Template *tpnt)
)
{
int i, j;
int chips;
int count = 0;
uchar bus, device_fn;
short index;
int attach_count = 0;
ncr_device device[8];
#ifdef SCSI_NCR_NVRAM_SUPPORT
ncr_nvram  nvram[4];
int k, nvrams;
#endif
int hosts;
#ifdef SCSI_NCR_NVRAM_SUPPORT
int nvram_index = 0;
#endif
if (initverbose >= 2)
ncr_print_driver_setup();
#ifdef SCSI_NCR_DEBUG_INFO_SUPPORT
ncr_debug = driver_setup.debug;
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
tpnt->proc_dir = &proc_scsi_ncr53c8xx;
# ifdef SCSI_NCR_PROC_INFO_SUPPORT
tpnt->proc_info = ncr53c8xx_proc_info;
# endif
#endif
#if	defined(SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT) && defined(MODULE)
if (ncr53c8xx)
ncr53c8xx_setup(ncr53c8xx, (int *) 0);
#endif
if (!pcibios_present())
return 0;
chips	= sizeof(ncr_chip_ids)	/ sizeof(ncr_chip_ids[0]);
hosts	= sizeof(device)	/ sizeof(device[0]);
#ifdef SCSI_NCR_NVRAM_SUPPORT
k = 0;
if (driver_setup.use_nvram & 0x1)
nvrams	= sizeof(nvram)	/ sizeof(nvram[0]);
else
nvrams	= 0;
#endif
for (j = 0; j < chips ; ++j) {
i = driver_setup.reverse_probe ? chips-1 - j : j;
for (index = 0; ; index++) {
char *msg = "";
if ((pcibios_find_device(PCI_VENDOR_ID_NCR, ncr_chip_ids[i],
index, &bus, &device_fn)) ||
(count == hosts))
break;
#ifdef SCSI_NCR_NVRAM_SUPPORT
device[count].nvram = k < nvrams ? &nvram[k] : 0;
#else
device[count].nvram = 0;
#endif
if (ncr53c8xx_pci_init(tpnt, bus, device_fn, &device[count])) {
device[count].nvram = 0;
continue;
}
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (device[count].nvram) {
++k;
nvram_index |= device[count].nvram->type;
switch (device[count].nvram->type) {
case SCSI_NCR_TEKRAM_NVRAM:
msg = "with Tekram NVRAM";
break;
case SCSI_NCR_SYMBIOS_NVRAM:
msg = "with Symbios NVRAM";
break;
default:
msg = "";
device[count].nvram = 0;
--k;
}
}
#endif
printf(KERN_INFO "ncr53c8xx: 53c%s detected %s\n",
device[count].chip.name, msg);
++count;
}
}
#ifdef SCSI_NCR_NVRAM_SUPPORT
attach_count = ncr_attach_using_nvram(tpnt, nvram_index, count, device);
#endif
for (i= 0; i < count; i++) {
if (!device[i].attach_done &&
!ncr_attach (tpnt, attach_count, &device[i])) {
attach_count++;
}
}
return attach_count;
}
__initfunc(
static int ncr53c8xx_pci_init(Scsi_Host_Template *tpnt,
uchar bus, uchar device_fn, ncr_device *device)
)
{
ushort vendor_id, device_id, command;
uchar cache_line_size, latency_timer;
uchar irq, revision;
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
uint base, base_2, io_port;
#else
ulong base, base_2;
#endif
int i;
#ifdef SCSI_NCR_NVRAM_SUPPORT
ncr_nvram *nvram = device->nvram;
#endif
ncr_chip *chip;
printk(KERN_INFO "ncr53c8xx: at PCI bus %d, device %d, function %d\n",
bus, (int) (device_fn & 0xf8) >> 3, (int) device_fn & 7);
(void) pcibios_read_config_word(bus, device_fn,
PCI_VENDOR_ID, &vendor_id);
(void) pcibios_read_config_word(bus, device_fn,
PCI_DEVICE_ID, &device_id);
(void) pcibios_read_config_word(bus, device_fn,
PCI_COMMAND, &command);
(void) pcibios_read_config_dword(bus, device_fn,
PCI_BASE_ADDRESS_0, &io_port);
(void) pcibios_read_config_dword(bus, device_fn,
PCI_BASE_ADDRESS_1, &base);
(void) pcibios_read_config_dword(bus, device_fn,
PCI_BASE_ADDRESS_2, &base_2);
(void) pcibios_read_config_byte(bus, device_fn,
PCI_CLASS_REVISION,&revision);
(void) pcibios_read_config_byte(bus, device_fn,
PCI_INTERRUPT_LINE, &irq);
(void) pcibios_read_config_byte(bus, device_fn,
PCI_CACHE_LINE_SIZE, &cache_line_size);
(void) pcibios_read_config_byte(bus, device_fn,
PCI_LATENCY_TIMER, &latency_timer);
chip = 0;
for (i = 0; i < sizeof(ncr_chip_table)/sizeof(ncr_chip_table[0]); i++) {
if (device_id != ncr_chip_table[i].device_id)
continue;
if (revision > ncr_chip_table[i].revision_id)
continue;
chip = &device->chip;
memcpy(chip, &ncr_chip_table[i], sizeof(*chip));
chip->revision_id = revision;
break;
}
if (!chip) {
printk("ncr53c8xx: not initializing, device not supported\n");
return -1;
}
#ifdef __powerpc__
if ((command &
(PCI_COMMAND_MASTER|PCI_COMMAND_IO|PCI_COMMAND_MEMORY)) !=
(PCI_COMMAND_MASTER|PCI_COMMAND_IO|PCI_COMMAND_MEMORY)) {
printk("ncr53c8xx : setting PCI master/io/command bit\n");
command |= PCI_COMMAND_MASTER|PCI_COMMAND_IO|PCI_COMMAND_MEMORY;
pcibios_write_config_word(bus, device_fn, PCI_COMMAND, command);
}
if (io_port >= 0x10000000) {
io_port = (io_port & 0x00FFFFFF) | 0x01000000;
pcibios_write_config_dword(bus, device_fn, PCI_BASE_ADDRESS_0, io_port);
}
if (base >= 0x10000000) {
base = (base & 0x00FFFFFF) | 0x01000000;
pcibios_write_config_dword(bus, device_fn, PCI_BASE_ADDRESS_1, base);
}
#endif
if (command & PCI_COMMAND_IO) {
if ((io_port & 3) != 1) {
printk("ncr53c8xx: disabling I/O mapping since base address 0 (0x%x)\n"
"           bits 0..1 indicate a non-IO mapping\n", (int) io_port);
io_port = 0;
}
else
io_port &= PCI_BASE_ADDRESS_IO_MASK;
}
else
io_port = 0;
if (command & PCI_COMMAND_MEMORY) {
if ((base & PCI_BASE_ADDRESS_SPACE) != PCI_BASE_ADDRESS_SPACE_MEMORY) {
printk("ncr53c8xx: disabling memory mapping since base address 1\n"
"            contains a non-memory mapping\n");
base = 0;
}
else
base &= PCI_BASE_ADDRESS_MEM_MASK;
}
else
base = 0;
if (!io_port && !base) {
printk("ncr53c8xx: not initializing, both I/O and memory mappings disabled\n");
return -1;
}
base_2 &= PCI_BASE_ADDRESS_MEM_MASK;
if (io_port && check_region (io_port, 128)) {
printk("ncr53c8xx: IO region 0x%x to 0x%x is in use\n",
(int) io_port, (int) (io_port + 127));
return -1;
}
if (!(command & PCI_COMMAND_MASTER)) {
printk("ncr53c8xx: not initializing, BUS MASTERING was disabled\n");
return -1;
}
if (!(driver_setup.special_features & 1))
chip->features &= ~FE_SPECIAL_SET;
else {
if (driver_setup.special_features & 2)
chip->features &= ~FE_WRIE;
}
if (driver_setup.ultra_scsi < 2 && (chip->features & FE_ULTRA2)) {
chip->features |=  FE_ULTRA;
chip->features &= ~FE_ULTRA2;
}
if (driver_setup.ultra_scsi < 1)
chip->features &= ~FE_ULTRA;
if (!driver_setup.max_wide)
chip->features &= ~FE_WIDE;
#ifdef	SCSI_NCR_PCI_FIX_UP_SUPPORT
#if defined(__i386) && !defined(MODULE)
if ((driver_setup.pci_fix_up & 1) &&
(chip->features & FE_CLSE) && cache_line_size == 0) {
#if LINUX_VERSION_CODE < LinuxVersionCode(2,1,75)
extern char x86;
switch(x86) {
#else
switch(boot_cpu_data.x86) {
#endif
case 4:	cache_line_size = 4; break;
case 5:	cache_line_size = 8; break;
}
if (cache_line_size)
(void) pcibios_write_config_byte(bus, device_fn,
PCI_CACHE_LINE_SIZE, cache_line_size);
if (initverbose)
printk("ncr53c8xx: setting PCI_CACHE_LINE_SIZE to %d (fix-up).\n", cache_line_size);
}
if ((driver_setup.pci_fix_up & 2) && cache_line_size &&
(chip->features & FE_WRIE) && !(command & PCI_COMMAND_INVALIDATE)) {
command |= PCI_COMMAND_INVALIDATE;
(void) pcibios_write_config_word(bus, device_fn,
PCI_COMMAND, command);
if (initverbose)
printk("ncr53c8xx: setting PCI_COMMAND_INVALIDATE bit (fix-up).\n");
}
#endif
if (!(chip->features & FE_CLSE)) {
int burst_max = chip->burst_max;
if (cache_line_size == 0) {
chip->features	&= ~FE_ERL;
if (burst_max > 3)
burst_max = 3;
}
else {
while (cache_line_size < (1 << burst_max))
--burst_max;
}
chip->burst_max = burst_max;
}
if (latency_timer == 0 && chip->burst_max)
printk("ncr53c8xx: PCI_LATENCY_TIMER=0, bursting should'nt be allowed.\n");
if ((driver_setup.pci_fix_up & 4) && chip->burst_max) {
uchar lt = (1 << chip->burst_max) + 6 + 10;
if (latency_timer < lt) {
latency_timer = lt;
if (initverbose)
printk("ncr53c8xx: setting PCI_LATENCY_TIMER to %d bus clocks (fix-up).\n", latency_timer);
(void) pcibios_write_config_byte(bus, device_fn,
PCI_LATENCY_TIMER, latency_timer);
}
}
if ((chip->features & FE_CLSE) && cache_line_size == 0) {
chip->features &= ~FE_CACHE_SET;
printk("ncr53c8xx: PCI_CACHE_LINE_SIZE not set, features based on CACHE LINE SIZE not used.\n");
}
if ((chip->features & FE_WRIE) && !(command & PCI_COMMAND_INVALIDATE)) {
chip->features &= ~FE_WRIE;
printk("ncr53c8xx: PCI_COMMAND_INVALIDATE not set, WRITE AND INVALIDATE not used\n");
}
#endif
device->slot.bus	= bus;
device->slot.device_fn	= device_fn;
device->slot.base	= base;
device->slot.base_2	= base_2;
device->slot.io_port	= io_port;
device->slot.irq	= irq;
device->attach_done	= 0;
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (!nvram)
goto out;
#ifdef NCR_IOMAPPED
request_region(io_port, 128, "ncr53c8xx");
device->slot.port = io_port;
#else
device->slot.reg = (struct ncr_reg *) remap_pci_mem((ulong) base, 128);
if (!device->slot.reg)
goto out;
#endif
if	(!ncr_get_Symbios_nvram(&device->slot, &nvram->data.Symbios))
nvram->type = SCSI_NCR_SYMBIOS_NVRAM;
else if	(!ncr_get_Tekram_nvram(&device->slot, &nvram->data.Tekram))
nvram->type = SCSI_NCR_TEKRAM_NVRAM;
else
nvram->type = 0;
out:
#ifdef NCR_IOMAPPED
release_region(device->slot.port, 128);
#else
unmap_pci_mem((vm_offset_t) device->slot.reg, (u_long) 128);
#endif
#endif
return 0;
}
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,0,0)
static void ncr53c8xx_select_queue_depths(struct Scsi_Host *host, struct scsi_device *devlist)
{
struct scsi_device *device;
for (device = devlist; device; device = device->next) {
if (device->host == host) {
#if SCSI_NCR_MAX_TAGS > 1
if (device->tagged_supported) {
device->queue_depth = SCSI_NCR_MAX_TAGS;
}
else {
device->queue_depth = 2;
}
#else
device->queue_depth = 1;
#endif
#ifdef DEBUG_NCR53C8XX
printk("ncr53c8xx_select_queue_depth: id=%d, lun=%d, queue_depth=%d\n",
device->id, device->lun, device->queue_depth);
#endif
}
}
}
#endif
int ncr53c8xx_queue_command (Scsi_Cmnd *cmd, void (* done)(Scsi_Cmnd *))
{
int sts;
#ifdef DEBUG_NCR53C8XX
printk("ncr53c8xx_queue_command\n");
#endif
if ((sts = ncr_queue_command(cmd, done)) != DID_OK) {
cmd->result = ScsiResult(sts, 0);
done(cmd);
#ifdef DEBUG_NCR53C8XX
printk("ncr53c8xx : command not queued - result=%d\n", sts);
#endif
return sts;
}
#ifdef DEBUG_NCR53C8XX
printk("ncr53c8xx : command successfully queued\n");
#endif
return sts;
}
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,70)
static void ncr53c8xx_intr(int irq, void *dev_id, struct pt_regs * regs)
{
#ifdef DEBUG_NCR53C8XX
printk("ncr53c8xx : interrupt received\n");
#endif
if (DEBUG_FLAGS & DEBUG_TINY) printf ("[");
ncr_exception((ncb_p) dev_id);
if (DEBUG_FLAGS & DEBUG_TINY) printf ("]\n");
}
#else
static void ncr53c8xx_intr(int irq, struct pt_regs * regs)
{
struct Scsi_Host *host;
struct host_data *host_data;
for (host = first_host; host; host = host->next) {
if (host->hostt == the_template && host->irq == irq) {
host_data = (struct host_data *) host->hostdata;
if (DEBUG_FLAGS & DEBUG_TINY) printf ("[");
ncr_exception(host_data->ncb);
if (DEBUG_FLAGS & DEBUG_TINY) printf ("]\n");
}
}
}
#endif
static void ncr53c8xx_timeout(unsigned long np)
{
ncr_timeout((ncb_p) np);
}
#if defined SCSI_RESET_SYNCHRONOUS && defined SCSI_RESET_ASYNCHRONOUS
int ncr53c8xx_reset(Scsi_Cmnd *cmd, unsigned int reset_flags)
{
int sts;
unsigned long flags;
printk("ncr53c8xx_reset: pid=%lu reset_flags=%x serial_number=%ld serial_number_at_timeout=%ld\n",
cmd->pid, reset_flags, cmd->serial_number, cmd->serial_number_at_timeout);
save_flags(flags); cli();
#if defined SCSI_RESET_NOT_RUNNING
if (cmd->serial_number != cmd->serial_number_at_timeout) {
sts = SCSI_RESET_NOT_RUNNING;
goto out;
}
#endif
sts = ncr_reset_bus(cmd,
(reset_flags & (SCSI_RESET_SYNCHRONOUS | SCSI_RESET_ASYNCHRONOUS)) == SCSI_RESET_SYNCHRONOUS);
#if defined SCSI_RESET_HOST_RESET
if (sts == SCSI_RESET_SUCCESS)
sts |= SCSI_RESET_HOST_RESET;
#endif
out:
restore_flags(flags);
return sts;
}
#else
int ncr53c8xx_reset(Scsi_Cmnd *cmd)
{
printk("ncr53c8xx_reset: command pid %lu\n", cmd->pid);
return ncr_reset_bus(cmd, 1);
}
#endif
#if defined SCSI_RESET_SYNCHRONOUS && defined SCSI_RESET_ASYNCHRONOUS
int ncr53c8xx_abort(Scsi_Cmnd *cmd)
{
int sts;
unsigned long flags;
printk("ncr53c8xx_abort: pid=%lu serial_number=%ld serial_number_at_timeout=%ld\n",
cmd->pid, cmd->serial_number, cmd->serial_number_at_timeout);
save_flags(flags); cli();
if (cmd->serial_number != cmd->serial_number_at_timeout) {
sts = SCSI_ABORT_NOT_RUNNING;
goto out;
}
sts = ncr_abort_command(cmd);
out:
restore_flags(flags);
return sts;
}
#else
int ncr53c8xx_abort(Scsi_Cmnd *cmd)
{
printk("ncr53c8xx_abort: command pid %lu\n", cmd->pid);
return ncr_abort_command(cmd);
}
#endif
#ifdef MODULE
int ncr53c8xx_release(struct Scsi_Host *host)
{
#ifdef DEBUG_NCR53C8XX
printk("ncr53c8xx : release\n");
#endif
ncr_detach(((struct host_data *) host->hostdata)->ncb);
return 1;
}
#endif
#define next_wcmd host_scribble
static void insert_into_waiting_list(ncb_p np, Scsi_Cmnd *cmd)
{
Scsi_Cmnd *wcmd;
#ifdef DEBUG_WAITING_LIST
printf("%s: cmd %lx inserted into waiting list\n", ncr_name(np), (u_long) cmd);
#endif
cmd->next_wcmd = 0;
if (!(wcmd = np->waiting_list)) np->waiting_list = cmd;
else {
while ((wcmd->next_wcmd) != 0)
wcmd = (Scsi_Cmnd *) wcmd->next_wcmd;
wcmd->next_wcmd = (char *) cmd;
}
}
static Scsi_Cmnd *retrieve_from_waiting_list(int to_remove, ncb_p np, Scsi_Cmnd *cmd)
{
Scsi_Cmnd *wcmd;
if (!(wcmd = np->waiting_list)) return 0;
while (wcmd->next_wcmd) {
if (cmd == (Scsi_Cmnd *) wcmd->next_wcmd) {
if (to_remove) {
wcmd->next_wcmd = cmd->next_wcmd;
cmd->next_wcmd = 0;
}
#ifdef DEBUG_WAITING_LIST
printf("%s: cmd %lx retrieved from waiting list\n", ncr_name(np), (u_long) cmd);
#endif
return cmd;
}
}
return 0;
}
static void process_waiting_list(ncb_p np, int sts)
{
Scsi_Cmnd *waiting_list, *wcmd;
waiting_list = np->waiting_list;
np->waiting_list = 0;
#ifdef DEBUG_WAITING_LIST
if (waiting_list) printf("%s: waiting_list=%lx processing sts=%d\n", ncr_name(np), (u_long) waiting_list, sts);
#endif
while ((wcmd = waiting_list) != 0) {
waiting_list = (Scsi_Cmnd *) wcmd->next_wcmd;
wcmd->next_wcmd = 0;
if (sts == DID_OK) {
#ifdef DEBUG_WAITING_LIST
printf("%s: cmd %lx trying to requeue\n", ncr_name(np), (u_long) wcmd);
#endif
sts = ncr_queue_command(wcmd, wcmd->scsi_done);
}
if (sts != DID_OK) {
#ifdef DEBUG_WAITING_LIST
printf("%s: cmd %lx done forced sts=%d\n", ncr_name(np), (u_long) wcmd, sts);
#endif
wcmd->result = ScsiResult(sts, 0);
wcmd->scsi_done(wcmd);
}
}
}
#undef next_wcmd
static int guess_xfer_direction(int opcode)
{
int d;
switch(opcode) {
case 0x12:
case 0x4D:
case 0x5A:
case 0x1A:
case 0x3C:
case 0x1C:
case 0x03:
d = XferIn;
break;
case 0x39:
case 0x3A:
case 0x18:
case 0x4C:
case 0x55:
case 0x3B:
case 0x1D:
case 0x40:
case 0x15:
d = XferOut;
break;
case 0x00:
d = XferNone;
break;
default:
d = XferBoth;
break;
}
return d;
}
#ifdef SCSI_NCR_PROC_INFO_SUPPORT
#ifdef SCSI_NCR_USER_COMMAND_SUPPORT
#define is_digit(c)	((c) >= '0' && (c) <= '9')
#define digit_to_bin(c)	((c) - '0')
#define is_space(c)	((c) == ' ' || (c) == '\t')
static int skip_spaces(char *ptr, int len)
{
int cnt, c;
for (cnt = len; cnt > 0 && (c = *ptr++) && is_space(c); cnt--);
return (len - cnt);
}
static int get_int_arg(char *ptr, int len, u_long *pv)
{
int	cnt, c;
u_long	v;
for (v = 0, cnt = len; cnt > 0 && (c = *ptr++) && is_digit(c); cnt--) {
v = (v * 10) + digit_to_bin(c);
}
if (pv)
*pv = v;
return (len - cnt);
}
static int is_keyword(char *ptr, int len, char *verb)
{
int verb_len = strlen(verb);
if (len >= strlen(verb) && !memcmp(verb, ptr, verb_len))
return verb_len;
else
return 0;
}
#define SKIP_SPACES(min_spaces)						\
if ((arg_len = skip_spaces(ptr, len)) < (min_spaces))		\
return -EINVAL;						\
ptr += arg_len; len -= arg_len;
#define GET_INT_ARG(v)							\
if (!(arg_len = get_int_arg(ptr, len, &(v))))			\
return -EINVAL;						\
ptr += arg_len; len -= arg_len;
static int ncr_user_command(ncb_p np, char *buffer, int length)
{
char *ptr	= buffer;
int len		= length;
struct usrcmd	 *uc = &np->user;
int		arg_len;
u_long 		target;
bzero(uc, sizeof(*uc));
if (len > 0 && ptr[len-1] == '\n')
--len;
if	((arg_len = is_keyword(ptr, len, "setsync")) != 0)
uc->cmd = UC_SETSYNC;
else if	((arg_len = is_keyword(ptr, len, "settags")) != 0)
uc->cmd = UC_SETTAGS;
else if	((arg_len = is_keyword(ptr, len, "setorder")) != 0)
uc->cmd = UC_SETORDER;
else if	((arg_len = is_keyword(ptr, len, "setwide")) != 0)
uc->cmd = UC_SETWIDE;
else if	((arg_len = is_keyword(ptr, len, "setdebug")) != 0)
uc->cmd = UC_SETDEBUG;
else if	((arg_len = is_keyword(ptr, len, "setflag")) != 0)
uc->cmd = UC_SETFLAG;
else if	((arg_len = is_keyword(ptr, len, "clearprof")) != 0)
uc->cmd = UC_CLEARPROF;
#ifdef	UC_DEBUG_ERROR_RECOVERY
else if	((arg_len = is_keyword(ptr, len, "debug_error_recovery")) != 0)
uc->cmd = UC_DEBUG_ERROR_RECOVERY;
#endif
else
arg_len = 0;
#ifdef DEBUG_PROC_INFO
printf("ncr_user_command: arg_len=%d, cmd=%ld\n", arg_len, uc->cmd);
#endif
if (!arg_len)
return -EINVAL;
ptr += arg_len; len -= arg_len;
switch(uc->cmd) {
case UC_SETSYNC:
case UC_SETTAGS:
case UC_SETWIDE:
case UC_SETFLAG:
SKIP_SPACES(1);
if ((arg_len = is_keyword(ptr, len, "all")) != 0) {
ptr += arg_len; len -= arg_len;
uc->target = ~0;
} else {
GET_INT_ARG(target);
uc->target = (1<<target);
#ifdef DEBUG_PROC_INFO
printf("ncr_user_command: target=%ld\n", target);
#endif
}
break;
}
switch(uc->cmd) {
case UC_SETSYNC:
case UC_SETTAGS:
case UC_SETWIDE:
SKIP_SPACES(1);
GET_INT_ARG(uc->data);
#ifdef DEBUG_PROC_INFO
printf("ncr_user_command: data=%ld\n", uc->data);
#endif
break;
case UC_SETORDER:
SKIP_SPACES(1);
if	((arg_len = is_keyword(ptr, len, "simple")))
uc->data = M_SIMPLE_TAG;
else if	((arg_len = is_keyword(ptr, len, "ordered")))
uc->data = M_ORDERED_TAG;
else if	((arg_len = is_keyword(ptr, len, "default")))
uc->data = 0;
else
return -EINVAL;
break;
case UC_SETDEBUG:
while (len > 0) {
SKIP_SPACES(1);
if	((arg_len = is_keyword(ptr, len, "alloc")))
uc->data |= DEBUG_ALLOC;
else if	((arg_len = is_keyword(ptr, len, "phase")))
uc->data |= DEBUG_PHASE;
else if	((arg_len = is_keyword(ptr, len, "poll")))
uc->data |= DEBUG_POLL;
else if	((arg_len = is_keyword(ptr, len, "queue")))
uc->data |= DEBUG_QUEUE;
else if	((arg_len = is_keyword(ptr, len, "result")))
uc->data |= DEBUG_RESULT;
else if	((arg_len = is_keyword(ptr, len, "scatter")))
uc->data |= DEBUG_SCATTER;
else if	((arg_len = is_keyword(ptr, len, "script")))
uc->data |= DEBUG_SCRIPT;
else if	((arg_len = is_keyword(ptr, len, "tiny")))
uc->data |= DEBUG_TINY;
else if	((arg_len = is_keyword(ptr, len, "timing")))
uc->data |= DEBUG_TIMING;
else if	((arg_len = is_keyword(ptr, len, "nego")))
uc->data |= DEBUG_NEGO;
else if	((arg_len = is_keyword(ptr, len, "tags")))
uc->data |= DEBUG_TAGS;
else if	((arg_len = is_keyword(ptr, len, "freeze")))
uc->data |= DEBUG_FREEZE;
else if	((arg_len = is_keyword(ptr, len, "restart")))
uc->data |= DEBUG_RESTART;
else
return -EINVAL;
ptr += arg_len; len -= arg_len;
}
#ifdef DEBUG_PROC_INFO
printf("ncr_user_command: data=%ld\n", uc->data);
#endif
break;
case UC_SETFLAG:
while (len > 0) {
SKIP_SPACES(1);
if	((arg_len = is_keyword(ptr, len, "trace")))
uc->data |= UF_TRACE;
else if	((arg_len = is_keyword(ptr, len, "no_disc")))
uc->data |= UF_NODISC;
else
return -EINVAL;
ptr += arg_len; len -= arg_len;
}
break;
#ifdef	UC_DEBUG_ERROR_RECOVERY
case UC_DEBUG_ERROR_RECOVERY:
SKIP_SPACES(1);
if	((arg_len = is_keyword(ptr, len, "sge")))
uc->data = 1;
else if	((arg_len = is_keyword(ptr, len, "abort")))
uc->data = 2;
else if	((arg_len = is_keyword(ptr, len, "reset")))
uc->data = 3;
else if	((arg_len = is_keyword(ptr, len, "parity")))
uc->data = 4;
else if	((arg_len = is_keyword(ptr, len, "none")))
uc->data = 0;
else
return -EINVAL;
ptr += arg_len; len -= arg_len;
break;
#endif
default:
break;
}
if (len)
return -EINVAL;
else {
long flags;
save_flags(flags); cli();
ncr_usercmd (np);
restore_flags(flags);
}
return length;
}
#endif
#ifdef SCSI_NCR_USER_INFO_SUPPORT
struct info_str
{
char *buffer;
int length;
int offset;
int pos;
};
static void copy_mem_info(struct info_str *info, char *data, int len)
{
if (info->pos + len > info->length)
len = info->length - info->pos;
if (info->pos + len < info->offset) {
info->pos += len;
return;
}
if (info->pos < info->offset) {
data += (info->offset - info->pos);
len  -= (info->offset - info->pos);
}
if (len > 0) {
memcpy(info->buffer + info->pos, data, len);
info->pos += len;
}
}
static int copy_info(struct info_str *info, char *fmt, ...)
{
va_list args;
char buf[81];
int len;
va_start(args, fmt);
len = vsprintf(buf, fmt, args);
va_end(args);
copy_mem_info(info, buf, len);
return len;
}
#define to_ms(t) ((t) * 1000 / HZ)
static int ncr_host_info(ncb_p np, char *ptr, off_t offset, int len)
{
struct info_str info;
info.buffer	= ptr;
info.length	= len;
info.offset	= offset;
info.pos	= 0;
copy_info(&info, "General information:\n");
copy_info(&info, "  Chip NCR53C%s, ",	np->chip_name);
copy_info(&info, "device id 0x%x, ",	np->device_id);
copy_info(&info, "revision id 0x%x\n",	np->revision_id);
copy_info(&info, "  IO port address 0x%lx, ", (u_long) np->port);
copy_info(&info, "IRQ number %d\n", (int) np->irq);
#ifndef NCR_IOMAPPED
if (np->reg)
copy_info(&info, "  Using memory mapped IO at virtual address 0x%lx\n",
(u_long) np->reg);
#endif
copy_info(&info, "  Synchronous period factor %d, ", (int) np->minsync);
copy_info(&info, "max commands per lun %d\n", SCSI_NCR_MAX_TAGS);
if (driver_setup.debug || driver_setup.verbose > 1) {
copy_info(&info, "  Debug flags 0x%x, ", driver_setup.debug);
copy_info(&info, "verbosity level %d\n", driver_setup.verbose);
}
#ifdef SCSI_NCR_PROFILE_SUPPORT
copy_info(&info, "Profiling information:\n");
copy_info(&info, "  %-12s = %lu\n", "num_trans",np->profile.num_trans);
copy_info(&info, "  %-12s = %lu\n", "num_kbytes",np->profile.num_kbytes);
copy_info(&info, "  %-12s = %lu\n", "num_disc",	np->profile.num_disc);
copy_info(&info, "  %-12s = %lu\n", "num_break",np->profile.num_break);
copy_info(&info, "  %-12s = %lu\n", "num_int",	np->profile.num_int);
copy_info(&info, "  %-12s = %lu\n", "num_fly",	np->profile.num_fly);
copy_info(&info, "  %-12s = %lu\n", "ms_setup",	to_ms(np->profile.ms_setup));
copy_info(&info, "  %-12s = %lu\n", "ms_data",	to_ms(np->profile.ms_data));
copy_info(&info, "  %-12s = %lu\n", "ms_disc",	to_ms(np->profile.ms_disc));
copy_info(&info, "  %-12s = %lu\n", "ms_post",	to_ms(np->profile.ms_post));
#endif
return info.pos > info.offset? info.pos - info.offset : 0;
}
#endif
int ncr53c8xx_proc_info(char *buffer, char **start, off_t offset,
int length, int hostno, int func)
{
struct Scsi_Host *host;
struct host_data *host_data;
ncb_p ncb = 0;
int retv;
#ifdef DEBUG_PROC_INFO
printf("ncr53c8xx_proc_info: hostno=%d, func=%d\n", hostno, func);
#endif
for (host = first_host; host; host = host->next) {
if (host->hostt == the_template && host->host_no == hostno) {
host_data = (struct host_data *) host->hostdata;
ncb = host_data->ncb;
break;
}
}
if (!ncb)
return -EINVAL;
if (func) {
#ifdef	SCSI_NCR_USER_COMMAND_SUPPORT
retv = ncr_user_command(ncb, buffer, length);
#else
retv = -EINVAL;
#endif
}
else {
if (start)
*start = buffer;
#ifdef SCSI_NCR_USER_INFO_SUPPORT
retv = ncr_host_info(ncb, buffer, offset, length);
#else
retv = -EINVAL;
#endif
}
return retv;
}
#endif
#ifdef SCSI_NCR_NVRAM_SUPPORT
#define SET_BIT 0
#define CLR_BIT 1
#define SET_CLK 2
#define CLR_CLK 3
static u_short nvram_read_data(ncr_slot *np, u_char *data, int len, u_char *gpreg, u_char *gpcntl);
static void nvram_start(ncr_slot *np, u_char *gpreg);
static void nvram_write_byte(ncr_slot *np, u_char *ack_data, u_char write_data, u_char *gpreg, u_char *gpcntl);
static void nvram_read_byte(ncr_slot *np, u_char *read_data, u_char ack_data, u_char *gpreg, u_char *gpcntl);
static void nvram_readAck(ncr_slot *np, u_char *read_bit, u_char *gpreg, u_char *gpcntl);
static void nvram_writeAck(ncr_slot *np, u_char write_bit, u_char *gpreg, u_char *gpcntl);
static void nvram_doBit(ncr_slot *np, u_char *read_bit, u_char write_bit, u_char *gpreg);
static void nvram_stop(ncr_slot *np, u_char *gpreg);
static void nvram_setBit(ncr_slot *np, u_char write_bit, u_char *gpreg, int bit_mode);
__initfunc(
static int ncr_get_Symbios_nvram (ncr_slot *np, Symbios_nvram *nvram)
)
{
static u_char Symbios_trailer[6] = {0xfe, 0xfe, 0, 0, 0, 0};
u_char	gpcntl, gpreg;
u_char	old_gpcntl, old_gpreg;
u_short	csum;
u_char	ack_data;
int	retv = 1;
old_gpreg	= INB (nc_gpreg);
old_gpcntl	= INB (nc_gpcntl);
gpcntl		= old_gpcntl & 0xfc;
OUTB (nc_gpreg,  old_gpreg);
OUTB (nc_gpcntl, gpcntl);
gpreg = old_gpreg;
nvram_setBit(np, 0, &gpreg, CLR_CLK);
nvram_setBit(np, 0, &gpreg, CLR_BIT);
nvram_stop(np, &gpreg);
nvram_start(np, &gpreg);
nvram_write_byte(np, &ack_data,
0xa0 | ((SYMBIOS_NVRAM_ADDRESS >> 7) & 0x0e), &gpreg, &gpcntl);
if (ack_data & 0x01)
goto out;
nvram_write_byte(np, &ack_data,
(SYMBIOS_NVRAM_ADDRESS & 0x7f) << 1, &gpreg, &gpcntl);
if (ack_data & 0x01)
goto out;
nvram_start(np, &gpreg);
nvram_write_byte(np, &ack_data,
0xa1 | ((SYMBIOS_NVRAM_ADDRESS >> 7) & 0x0e), &gpreg, &gpcntl);
if (ack_data & 0x01)
goto out;
gpcntl |= 0x01;
OUTB (nc_gpcntl, gpcntl);
csum = nvram_read_data(np,
(u_char *) nvram, sizeof(*nvram), &gpreg, &gpcntl);
gpcntl &= 0xfe;
OUTB (nc_gpcntl, gpcntl);
nvram_stop(np, &gpreg);
#ifdef SCSI_NCR_DEBUG_NVRAM
printf("ncr53c8xx: NvRAM marker=%x trailer=%x %x %x %x %x %x byte_count=%d/%d checksum=%x/%x\n",
nvram->start_marker,
nvram->trailer[0], nvram->trailer[1], nvram->trailer[2],
nvram->trailer[3], nvram->trailer[4], nvram->trailer[5],
nvram->byte_count, sizeof(*nvram) - 12,
nvram->checksum, csum);
#endif
if (nvram->start_marker == 0 &&
!memcmp(nvram->trailer, Symbios_trailer, 6) &&
nvram->byte_count == sizeof(*nvram) - 12 &&
csum == nvram->checksum)
retv = 0;
out:
OUTB (nc_gpcntl, old_gpcntl);
OUTB (nc_gpreg,  old_gpreg);
return retv;
}
__initfunc(
static u_short nvram_read_data(ncr_slot *np, u_char *data, int len, u_char *gpreg, u_char *gpcntl)
)
{
int	x;
u_short	csum;
for (x = 0; x < len; x++)
nvram_read_byte(np, &data[x], (x == (len - 1)), gpreg, gpcntl);
for (x = 6, csum = 0; x < len - 6; x++)
csum += data[x];
return csum;
}
__initfunc(
static void nvram_start(ncr_slot *np, u_char *gpreg)
)
{
nvram_setBit(np, 1, gpreg, SET_BIT);
nvram_setBit(np, 0, gpreg, SET_CLK);
nvram_setBit(np, 0, gpreg, CLR_BIT);
nvram_setBit(np, 0, gpreg, CLR_CLK);
}
__initfunc(
static void nvram_write_byte(ncr_slot *np, u_char *ack_data, u_char write_data, u_char *gpreg, u_char *gpcntl)
)
{
int x;
for (x = 0; x < 8; x++)
nvram_doBit(np, 0, (write_data >> (7 - x)) & 0x01, gpreg);
nvram_readAck(np, ack_data, gpreg, gpcntl);
}
__initfunc(
static void nvram_read_byte(ncr_slot *np, u_char *read_data, u_char ack_data, u_char *gpreg, u_char *gpcntl)
)
{
int x;
u_char read_bit;
*read_data = 0;
for (x = 0; x < 8; x++) {
nvram_doBit(np, &read_bit, 1, gpreg);
*read_data |= ((read_bit & 0x01) << (7 - x));
}
nvram_writeAck(np, ack_data, gpreg, gpcntl);
}
__initfunc(
static void nvram_writeAck(ncr_slot *np, u_char write_bit, u_char *gpreg, u_char *gpcntl)
)
{
OUTB (nc_gpcntl, *gpcntl & 0xfe);
nvram_doBit(np, 0, write_bit, gpreg);
OUTB (nc_gpcntl, *gpcntl);
}
__initfunc(
static void nvram_readAck(ncr_slot *np, u_char *read_bit, u_char *gpreg, u_char *gpcntl)
)
{
OUTB (nc_gpcntl, *gpcntl | 0x01);
nvram_doBit(np, read_bit, 1, gpreg);
OUTB (nc_gpcntl, *gpcntl);
}
__initfunc(
static void nvram_doBit(ncr_slot *np, u_char *read_bit, u_char write_bit, u_char *gpreg)
)
{
nvram_setBit(np, write_bit, gpreg, SET_BIT);
nvram_setBit(np, 0, gpreg, SET_CLK);
if (read_bit)
*read_bit = INB (nc_gpreg);
nvram_setBit(np, 0, gpreg, CLR_CLK);
nvram_setBit(np, 0, gpreg, CLR_BIT);
}
__initfunc(
static void nvram_stop(ncr_slot *np, u_char *gpreg)
)
{
nvram_setBit(np, 0, gpreg, SET_CLK);
nvram_setBit(np, 1, gpreg, SET_BIT);
}
__initfunc(
static void nvram_setBit(ncr_slot *np, u_char write_bit, u_char *gpreg, int bit_mode)
)
{
DELAY(5);
switch (bit_mode){
case SET_BIT:
*gpreg |= write_bit;
break;
case CLR_BIT:
*gpreg &= 0xfe;
break;
case SET_CLK:
*gpreg |= 0x02;
break;
case CLR_CLK:
*gpreg &= 0xfd;
break;
}
OUTB (nc_gpreg, *gpreg);
DELAY(5);
}
#undef SET_BIT 0
#undef CLR_BIT 1
#undef SET_CLK 2
#undef CLR_CLK 3
static u_short Tnvram_read_data(ncr_slot *np, u_short *data, int len, u_char *gpreg);
static void Tnvram_Send_Command(ncr_slot *np, u_short write_data, u_char *read_bit, u_char *gpreg);
static void Tnvram_Read_Word(ncr_slot *np, u_short *nvram_data, u_char *gpreg);
static void Tnvram_Read_Bit(ncr_slot *np, u_char *read_bit, u_char *gpreg);
static void Tnvram_Write_Bit(ncr_slot *np, u_char write_bit, u_char *gpreg);
static void Tnvram_Stop(ncr_slot *np, u_char *gpreg);
static void Tnvram_Clk(ncr_slot *np, u_char *gpreg);
__initfunc(
static int ncr_get_Tekram_nvram (ncr_slot *np, Tekram_nvram *nvram)
)
{
u_char gpcntl, gpreg;
u_char old_gpcntl, old_gpreg;
u_short csum;
old_gpreg	= INB (nc_gpreg);
old_gpcntl	= INB (nc_gpcntl);
gpreg = old_gpreg & 0xe9;
OUTB (nc_gpreg, gpreg);
gpcntl = (old_gpcntl & 0xe9) | 0x09;
OUTB (nc_gpcntl, gpcntl);
csum = Tnvram_read_data(np, (u_short *) nvram,
sizeof(*nvram) / sizeof(short), &gpreg);
OUTB (nc_gpcntl, old_gpcntl);
OUTB (nc_gpreg,  old_gpreg);
if (csum != 0x1234)
return 1;
return 0;
}
__initfunc(
static u_short Tnvram_read_data(ncr_slot *np, u_short *data, int len, u_char *gpreg)
)
{
u_char	read_bit;
u_short	csum;
int	x;
for (x = 0, csum = 0; x < len; x++)  {
Tnvram_Send_Command(np, 0x180 | x, &read_bit, gpreg);
if (read_bit & 0x01)
return 0;
Tnvram_Read_Word(np, &data[x], gpreg);
csum += data[x];
Tnvram_Stop(np, gpreg);
}
return csum;
}
__initfunc(
static void Tnvram_Send_Command(ncr_slot *np, u_short write_data, u_char *read_bit, u_char *gpreg)
)
{
int x;
for (x = 0; x < 9; x++)
Tnvram_Write_Bit(np, (u_char) (write_data >> (8 - x)), gpreg);
*read_bit = INB (nc_gpreg);
}
__initfunc(
static void Tnvram_Read_Word(ncr_slot *np, u_short *nvram_data, u_char *gpreg)
)
{
int x;
u_char read_bit;
*nvram_data = 0;
for (x = 0; x < 16; x++) {
Tnvram_Read_Bit(np, &read_bit, gpreg);
if (read_bit & 0x01)
*nvram_data |=  (0x01 << (15 - x));
else
*nvram_data &= ~(0x01 << (15 - x));
}
}
__initfunc(
static void Tnvram_Read_Bit(ncr_slot *np, u_char *read_bit, u_char *gpreg)
)
{
DELAY(2);
Tnvram_Clk(np, gpreg);
*read_bit = INB (nc_gpreg);
}
__initfunc(
static void Tnvram_Write_Bit(ncr_slot *np, u_char write_bit, u_char *gpreg)
)
{
if (write_bit & 0x01)
*gpreg |= 0x02;
else
*gpreg &= 0xfd;
*gpreg |= 0x10;
OUTB (nc_gpreg, *gpreg);
DELAY(2);
Tnvram_Clk(np, gpreg);
}
__initfunc(
static void Tnvram_Stop(ncr_slot *np, u_char *gpreg)
)
{
*gpreg &= 0xef;
OUTB (nc_gpreg, *gpreg);
DELAY(2);
Tnvram_Clk(np, gpreg);
}
__initfunc(
static void Tnvram_Clk(ncr_slot *np, u_char *gpreg)
)
{
OUTB (nc_gpreg, *gpreg | 0x04);
DELAY(2);
OUTB (nc_gpreg, *gpreg);
}
#endif
#ifdef MODULE
Scsi_Host_Template driver_template = NCR53C8XX;
#include "scsi_module.c"
#endif