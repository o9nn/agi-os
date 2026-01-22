#define AIC7XXX_FAKE_NEGOTIATION_CMDS
#define AIC7XXX_STRICT_PCI_SETUP
#if defined(MODULE) || defined(PCMCIA)
#include <linux/module.h>
#endif
#if defined(PCMCIA)
# undef MODULE
#endif
#include <stdarg.h>
#include <asm/io.h>
#include <asm/irq.h>
#include <asm/byteorder.h>
#include <linux/version.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/kernel.h>
#include <linux/ioport.h>
#include <linux/delay.h>
#include <linux/sched.h>
#include <linux/pci.h>
#include <linux/proc_fs.h>
#include <linux/blk.h>
#include <linux/tqueue.h>
#include <linux/tasks.h>
#include "sd.h"
#include "scsi.h"
#include "hosts.h"
#include "aic7xxx.h"
#include "aic7xxx/sequencer.h"
#include "aic7xxx/scsi_message.h"
#include "aic7xxx_reg.h"
#include <scsi/scsicam.h>
#include <linux/stat.h>
#include <linux/malloc.h>
#include <linux/config.h>
#define VIRT_TO_BUS(a) (unsigned int)virt_to_bus((void *)(a))
struct proc_dir_entry proc_scsi_aic7xxx = {
PROC_SCSI_AIC7XXX, 7, "aic7xxx",
S_IFDIR | S_IRUGO | S_IXUGO, 2,
0, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};
#define AIC7XXX_C_VERSION "5.1.13"
#define NUMBER(arr) (sizeof(arr) / sizeof(arr[0]))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define ALL_TARGETS -1
#define ALL_CHANNELS -1
#define ALL_LUNS -1
#define MAX_TARGETS 16
#define MAX_LUNS 8
#ifndef TRUE
# define TRUE 1
#endif
#ifndef FALSE
# define FALSE 0
#endif
#ifndef KERNEL_VERSION
# define KERNEL_VERSION(x,y,z) (((x)<<16)+((y)<<8)+(z))
#endif
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,1,92)
# if defined(__sparc_v9__) || defined(__powerpc__)
# error "PPC and Sparc platforms are only support under 2.1.92 and above"
# endif
# include <linux/bios32.h>
#endif
#if defined(__powerpc__)
# define MMAPIO
# ifdef mb
# undef mb
# endif
# define mb() \
__asm__ __volatile__("eieio" ::: "memory")
#elif defined(__i386__)
# define MMAPIO
# ifdef mb
# undef mb
# endif
# define mb() \
__asm__ __volatile__("lock ; addl $0,0(%%esp)": : :"memory")
#elif defined(__alpha__)
# ifdef mb
# undef mb
# endif
# define mb() \
__asm__ __volatile__("mb": : :"memory")
#endif
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,0)
# include <asm/spinlock.h>
# include <linux/smp.h>
# define cpuid smp_processor_id()
# if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
# define DRIVER_LOCK_INIT \
spin_lock_init(&p->spin_lock);
# define DRIVER_LOCK \
if(!p->cpu_lock_count[cpuid]) { \
spin_lock_irqsave(&p->spin_lock, cpu_flags); \
p->cpu_lock_count[cpuid]++; \
} else { \
p->cpu_lock_count[cpuid]++; \
}
# define DRIVER_UNLOCK \
if(--p->cpu_lock_count[cpuid] == 0) \
spin_unlock_irqrestore(&p->spin_lock, cpu_flags);
# else
# define DRIVER_LOCK_INIT
# define DRIVER_LOCK
# define DRIVER_UNLOCK
# endif
#else
# define cpuid 0
# define DRIVER_LOCK_INIT
# define DRIVER_LOCK \
save_flags(cpu_flags); \
cli();
# define DRIVER_UNLOCK \
restore_flags(cpu_flags);
# define le32_to_cpu(x) (x)
# define cpu_to_le32(x) (x)
#endif
#ifdef CONFIG_AIC7XXX_CMDS_PER_DEVICE
#define AIC7XXX_CMDS_PER_DEVICE CONFIG_AIC7XXX_CMDS_PER_DEVICE
#else
#define AIC7XXX_CMDS_PER_DEVICE 8
#endif
#ifdef CONFIG_AIC7XXX_RESET_DELAY
#define AIC7XXX_RESET_DELAY CONFIG_AIC7XXX_RESET_DELAY
#else
#define AIC7XXX_RESET_DELAY 5
#endif
#ifdef CONFIG_AIC7XXX_PROC_STATS
#define AIC7XXX_PROC_STATS
#endif
typedef struct
{
unsigned char tag_commands[16];
} adapter_tag_info_t;
#ifdef CONFIG_AIC7XXX_TCQ_ON_BY_DEFAULT
#define DEFAULT_TAG_COMMANDS {0, 0, 0, 0, 0, 0, 0, 0,\
0, 0, 0, 0, 0, 0, 0, 0}
#else
#define DEFAULT_TAG_COMMANDS {255, 255, 255, 255, 255, 255, 255, 255,\
255, 255, 255, 255, 255, 255, 255, 255}
#endif
static adapter_tag_info_t aic7xxx_tag_info[] =
{
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS},
{DEFAULT_TAG_COMMANDS}
};
static const char *board_names[] = {
"AIC-7xxx Unknown",
"Adaptec AIC-7810 Hardware RAID Controller",
"Adaptec AIC-7770 SCSI host adapter",
"Adaptec AHA-274X SCSI host adapter",
"Adaptec AHA-284X SCSI host adapter",
"Adaptec AIC-7850 SCSI host adapter",
"Adaptec AIC-7855 SCSI host adapter",
"Adaptec AIC-7860 Ultra SCSI host adapter",
"Adaptec AHA-2940A Ultra SCSI host adapter",
"Adaptec AIC-7870 SCSI host adapter",
"Adaptec AHA-294X SCSI host adapter",
"Adaptec AHA-394X SCSI host adapter",
"Adaptec AHA-398X SCSI host adapter",
"Adaptec AHA-2944 SCSI host adapter",
"Adaptec AIC-7880 Ultra SCSI host adapter",
"Adaptec AHA-294X Ultra SCSI host adapter",
"Adaptec AHA-394X Ultra SCSI host adapter",
"Adaptec AHA-398X Ultra SCSI host adapter",
"Adaptec AHA-2944 Ultra SCSI host adapter",
"Adaptec AIC-7895 Ultra SCSI host adapter",
"Adaptec AIC-7890/1 Ultra2 SCSI host adapter",
"Adaptec AHA-293X Ultra2 SCSI host adapter",
"Adaptec AHA-294X Ultra2 SCSI host adapter",
"Adaptec AIC-7896/7 Ultra2 SCSI host adapter",
"Adaptec AHA-394X Ultra2 SCSI host adapter",
"Adaptec AHA-395X Ultra2 SCSI host adapter",
"Adaptec PCMCIA SCSI controller",
"Adaptec AIC-7892 Ultra 160/m SCSI host adapter",
"Adaptec AIC-7899 Ultra 160/m SCSI host adapter",
};
#define DID_UNDERFLOW DID_ERROR
#define DID_RETRY_COMMAND DID_ERROR
#define HSCSIID 0x07
#define SCSI_RESET 0x040
#define MINSLOT 1
#define MAXSLOT 15
#define SLOTBASE(x) ((x) << 12)
#define BASE_TO_SLOT(x) ((x) >> 12)
#define AHC_HID0 0x80
#define AHC_HID1 0x81
#define AHC_HID2 0x82
#define AHC_HID3 0x83
#define MINREG 0xC00
#define MAXREG 0xCBF
#define INTDEF 0x5C
#define CLASS_PROGIF_REVID 0x08
#define DEVREVID 0x000000FFul
#define PROGINFC 0x0000FF00ul
#define SUBCLASS 0x00FF0000ul
#define BASECLASS 0xFF000000ul
#define CSIZE_LATTIME 0x0C
#define CACHESIZE 0x0000003Ful
#define LATTIME 0x0000FF00ul
#define DEVCONFIG 0x40
#define SCBSIZE32 0x00010000ul
#define MPORTMODE 0x00000400ul
#define RAMPSM 0x00000200ul
#define RAMPSM_ULTRA2 0x00000004
#define VOLSENSE 0x00000100ul
#define SCBRAMSEL 0x00000080ul
#define SCBRAMSEL_ULTRA2 0x00000008
#define MRDCEN 0x00000040ul
#define EXTSCBTIME 0x00000020ul
#define EXTSCBPEN 0x00000010ul
#define BERREN 0x00000008ul
#define DACEN 0x00000004ul
#define STPWLEVEL 0x00000002ul
#define DIFACTNEGEN 0x00000001ul
#define SCAMCTL 0x1a
#define CCSCBBADDR 0xf0
typedef enum {C46 = 6, C56_66 = 8} seeprom_chip_type;
struct seeprom_config {
#define CFXFER 0x0007
#define CFSYNCH 0x0008
#define CFDISC 0x0010
#define CFWIDEB 0x0020
#define CFSYNCHISULTRA 0x0040
#define CFNEWULTRAFORMAT 0x0080
#define CFSTART 0x0100
#define CFINCBIOS 0x0200
#define CFRNFOUND 0x0400
#define CFMULTILUN 0x0800
#define CFWBCACHEYES 0x4000
#define CFWBCACHENC 0xc000
unsigned short device_flags[16];
#define CFSUPREM 0x0001
#define CFSUPREMB 0x0002
#define CFBIOSEN 0x0004
#define CFSM2DRV 0x0010
#define CF284XEXTEND 0x0020
#define CFEXTEND 0x0080
unsigned short bios_control;
#define CFAUTOTERM 0x0001
#define CFULTRAEN 0x0002
#define CF284XSELTO 0x0003
#define CF284XFIFO 0x000C
#define CFSTERM 0x0004
#define CFWSTERM 0x0008
#define CFSPARITY 0x0010
#define CF284XSTERM 0x0020
#define CFRESETB 0x0040
#define CFBPRIMARY 0x0100
#define CFSEAUTOTERM 0x0400
#define CFLVDSTERM 0x0800
unsigned short adapter_control;
#define CFSCSIID 0x000F
#define CFBRTIME 0xFF00
unsigned short brtime_id;
#define CFMAXTARG 0x00FF
unsigned short max_targets;
unsigned short res_1[11];
unsigned short checksum;
};
#define SELBUS_MASK 0x0a
#define SELNARROW 0x00
#define SELBUSB 0x08
#define SINGLE_BUS 0x00
#define SCB_TARGET(scb) \
(((scb)->hscb->target_channel_lun & TID) >> 4)
#define SCB_LUN(scb) \
((scb)->hscb->target_channel_lun & LID)
#define SCB_IS_SCSIBUS_B(scb) \
(((scb)->hscb->target_channel_lun & SELBUSB) != 0)
#define aic7xxx_error(cmd) ((cmd)->SCp.Status)
#define aic7xxx_status(cmd) ((cmd)->SCp.sent_command)
#define aic7xxx_position(cmd) ((cmd)->SCp.have_data_in)
static struct aic7xxx_host *first_aic7xxx = NULL;
struct hw_scatterlist {
unsigned int address;
unsigned int length;
};
#define AIC7XXX_MAX_SG 128
#define AIC7XXX_MAXSCB 255
struct aic7xxx_hwscb {
unsigned char control;
unsigned char target_channel_lun;
unsigned char target_status;
unsigned char SG_segment_count;
unsigned int SG_list_pointer;
unsigned char residual_SG_segment_count;
unsigned char residual_data_count[3];
unsigned int data_pointer;
unsigned int data_count;
unsigned int SCSI_cmd_pointer;
unsigned char SCSI_cmd_length;
unsigned char tag;
#define SCB_PIO_TRANSFER_SIZE 26
unsigned char next;
unsigned char prev;
unsigned int pad;
};
typedef enum {
SCB_FREE = 0x0000,
SCB_WAITINGQ = 0x0002,
SCB_ACTIVE = 0x0004,
SCB_SENSE = 0x0008,
SCB_ABORT = 0x0010,
SCB_DEVICE_RESET = 0x0020,
SCB_RESET = 0x0040,
SCB_RECOVERY_SCB = 0x0080,
SCB_WAS_BUSY = 0x0100,
SCB_MSGOUT_SENT = 0x0200,
SCB_MSGOUT_SDTR = 0x0400,
SCB_MSGOUT_WDTR = 0x0800,
SCB_MSGOUT_BITS = SCB_MSGOUT_SENT |
SCB_MSGOUT_SDTR |
SCB_MSGOUT_WDTR,
SCB_QUEUED_ABORT = 0x1000,
SCB_QUEUED_FOR_DONE = 0x2000
} scb_flag_type;
typedef enum {
AHC_FNONE = 0x00000000,
AHC_PAGESCBS = 0x00000001,
AHC_CHANNEL_B_PRIMARY = 0x00000002,
AHC_USEDEFAULTS = 0x00000004,
AHC_INDIRECT_PAGING = 0x00000008,
AHC_CHNLB = 0x00000020,
AHC_CHNLC = 0x00000040,
AHC_EXTEND_TRANS_A = 0x00000100,
AHC_EXTEND_TRANS_B = 0x00000200,
AHC_TERM_ENB_A = 0x00000400,
AHC_TERM_ENB_SE_LOW = 0x00000400,
AHC_TERM_ENB_B = 0x00000800,
AHC_TERM_ENB_SE_HIGH = 0x00000800,
AHC_HANDLING_REQINITS = 0x00001000,
AHC_TARGETMODE = 0x00002000,
AHC_NEWEEPROM_FMT = 0x00004000,
AHC_RESET_DELAY = 0x00080000,
AHC_A_SCANNED = 0x00100000,
AHC_B_SCANNED = 0x00200000,
AHC_MULTI_CHANNEL = 0x00400000,
AHC_BIOS_ENABLED = 0x00800000,
AHC_SEEPROM_FOUND = 0x01000000,
AHC_TERM_ENB_LVD = 0x02000000,
AHC_ABORT_PENDING = 0x04000000,
AHC_RESET_PENDING = 0x08000000,
#define AHC_IN_ISR_BIT 28
AHC_IN_ISR = 0x10000000,
AHC_IN_ABORT = 0x20000000,
AHC_IN_RESET = 0x40000000,
AHC_EXTERNAL_SRAM = 0x80000000
} ahc_flag_type;
typedef enum {
AHC_NONE = 0x0000,
AHC_CHIPID_MASK = 0x00ff,
AHC_AIC7770 = 0x0001,
AHC_AIC7850 = 0x0002,
AHC_AIC7860 = 0x0003,
AHC_AIC7870 = 0x0004,
AHC_AIC7880 = 0x0005,
AHC_AIC7890 = 0x0006,
AHC_AIC7895 = 0x0007,
AHC_AIC7896 = 0x0008,
AHC_AIC7892 = 0x0009,
AHC_AIC7899 = 0x000a,
AHC_VL = 0x0100,
AHC_EISA = 0x0200,
AHC_PCI = 0x0400,
} ahc_chip;
typedef enum {
AHC_FENONE = 0x0000,
AHC_ULTRA = 0x0001,
AHC_ULTRA2 = 0x0002,
AHC_WIDE = 0x0004,
AHC_TWIN = 0x0008,
AHC_MORE_SRAM = 0x0010,
AHC_CMD_CHAN = 0x0020,
AHC_QUEUE_REGS = 0x0040,
AHC_SG_PRELOAD = 0x0080,
AHC_SPIOCAP = 0x0100,
AHC_ULTRA160 = 0x0200,
AHC_AIC7770_FE = AHC_FENONE,
AHC_AIC7850_FE = AHC_SPIOCAP,
AHC_AIC7860_FE = AHC_ULTRA|AHC_SPIOCAP,
AHC_AIC7870_FE = AHC_FENONE,
AHC_AIC7880_FE = AHC_ULTRA,
AHC_AIC7890_FE = AHC_MORE_SRAM|AHC_CMD_CHAN|AHC_ULTRA2|
AHC_QUEUE_REGS|AHC_SG_PRELOAD,
AHC_AIC7895_FE = AHC_MORE_SRAM|AHC_CMD_CHAN|AHC_ULTRA,
AHC_AIC7896_FE = AHC_AIC7890_FE,
AHC_AIC7892_FE = AHC_AIC7890_FE|AHC_ULTRA160,
AHC_AIC7899_FE = AHC_AIC7890_FE|AHC_ULTRA160,
} ahc_feature;
struct aic7xxx_scb {
struct aic7xxx_hwscb *hscb;
Scsi_Cmnd *cmd;
struct aic7xxx_scb *q_next;
volatile scb_flag_type flags;
struct hw_scatterlist *sg_list;
unsigned char tag_action;
unsigned char sg_count;
unsigned char sense_cmd[6];
unsigned int sg_length;
void *kmalloc_ptr;
};
typedef struct {
struct aic7xxx_scb *head;
struct aic7xxx_scb *tail;
} scb_queue_type;
static struct {
unsigned char errno;
const char *errmesg;
} hard_error[] = {
{ ILLHADDR, "Illegal Host Access" },
{ ILLSADDR, "Illegal Sequencer Address referenced" },
{ ILLOPCODE, "Illegal Opcode in sequencer program" },
{ SQPARERR, "Sequencer Ram Parity Error" },
{ DPARERR, "Data-Path Ram Parity Error" },
{ MPARERR, "Scratch Ram/SCB Array Ram Parity Error" },
{ PCIERRSTAT,"PCI Error detected" },
{ CIOPARERR, "CIOBUS Parity Error" }
};
static unsigned char
generic_sense[] = { REQUEST_SENSE, 0, 0, 0, 255, 0 };
typedef struct {
scb_queue_type free_scbs;
struct aic7xxx_scb *scb_array[AIC7XXX_MAXSCB];
struct aic7xxx_hwscb *hscbs;
unsigned char numscbs;
unsigned char maxhscbs;
unsigned char maxscbs;
void *hscb_kmalloc_ptr;
} scb_data_type;
struct target_cmd {
unsigned char mesg_bytes[4];
unsigned char command[28];
};
#define AHC_TRANS_CUR 0x0001
#define AHC_TRANS_ACTIVE 0x0002
#define AHC_TRANS_GOAL 0x0004
#define AHC_TRANS_USER 0x0008
#define AHC_TRANS_QUITE 0x0010
typedef struct {
unsigned char cur_width;
unsigned char goal_width;
unsigned char cur_period;
unsigned char goal_period;
unsigned char cur_offset;
unsigned char goal_offset;
unsigned char user_width;
unsigned char user_period;
unsigned char user_offset;
} transinfo_type;
struct aic7xxx_host {
volatile ahc_flag_type flags;
ahc_feature features;
unsigned long base;
volatile unsigned char *maddr;
unsigned long isr_count;
unsigned long spurious_int;
scb_data_type *scb_data;
volatile unsigned short needsdtr;
volatile unsigned short sdtr_pending;
volatile unsigned short needwdtr;
volatile unsigned short wdtr_pending;
struct aic7xxx_cmd_queue {
Scsi_Cmnd *head;
Scsi_Cmnd *tail;
} completeq;
volatile scb_queue_type waiting_scbs;
unsigned short discenable;
unsigned short tagenable;
unsigned short orderedtag;
unsigned char unpause;
unsigned char pause;
volatile unsigned char qoutfifonext;
volatile unsigned char activescbs;
volatile unsigned char max_activescbs;
volatile unsigned char qinfifonext;
#define DEVICE_PRESENT 0x01
#define BUS_DEVICE_RESET_PENDING 0x02
#define DEVICE_RESET_DELAY 0x04
#define DEVICE_PRINT_SDTR 0x08
#define DEVICE_PRINT_WDTR 0x10
#define DEVICE_WAS_BUSY 0x20
#define DEVICE_SCANNED 0x80
volatile unsigned char dev_flags[MAX_TARGETS];
volatile unsigned char dev_active_cmds[MAX_TARGETS];
volatile unsigned char dev_temp_queue_depth[MAX_TARGETS];
unsigned char dev_commands_sent[MAX_TARGETS];
unsigned int dev_timer_active;
struct timer_list dev_timer;
unsigned long dev_expires[MAX_TARGETS];
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,0)
spinlock_t spin_lock;
volatile unsigned char cpu_lock_count[NR_CPUS];
#endif
#ifdef AIC7XXX_FAKE_NEGOTIATION_CMDS
Scsi_Cmnd *dev_wdtr_cmnd[MAX_TARGETS];
Scsi_Cmnd *dev_sdtr_cmnd[MAX_TARGETS];
#endif
unsigned char dev_last_queue_full[MAX_TARGETS];
unsigned char dev_last_queue_full_count[MAX_TARGETS];
unsigned char dev_max_queue_depth[MAX_TARGETS];
volatile scb_queue_type delayed_scbs[MAX_TARGETS];
unsigned char msg_buf[9];
unsigned char msg_type;
#define MSG_TYPE_NONE 0x00
#define MSG_TYPE_INITIATOR_MSGOUT 0x01
#define MSG_TYPE_INITIATOR_MSGIN 0x02
unsigned char msg_len;
unsigned char msg_index;
transinfo_type transinfo[MAX_TARGETS];
volatile unsigned char untagged_scbs[256];
volatile unsigned char qoutfifo[256];
volatile unsigned char qinfifo[256];
unsigned int irq;
int instance;
int scsi_id;
int scsi_id_b;
unsigned int bios_address;
int board_name_index;
unsigned short needsdtr_copy;
unsigned short needwdtr_copy;
unsigned short ultraenb;
unsigned short bios_control;
unsigned short adapter_control;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
struct pci_dev *pdev;
#endif
unsigned char pci_bus;
unsigned char pci_device_fn;
struct seeprom_config sc;
unsigned short sc_type;
unsigned short sc_size;
struct aic7xxx_host *next;
struct Scsi_Host *host;
int host_no;
unsigned long mbase;
ahc_chip chip;
struct aic7xxx_xferstats {
long w_total;
long r_total;
#ifdef AIC7XXX_PROC_STATS
long w_bins[8];
long r_bins[8];
#endif
} stats[MAX_TARGETS];
#if 0
struct target_cmd *targetcmds;
unsigned int num_targetcmds;
#endif
};
#define AHC_SYNCRATE_ULTRA2 0
#define AHC_SYNCRATE_ULTRA 2
#define AHC_SYNCRATE_FAST 5
static struct aic7xxx_syncrate {
#define ULTRA_SXFR 0x100
int sxfr_ultra2;
int sxfr;
unsigned char period;
const char *rate[2];
} aic7xxx_syncrates[] = {
{ 0x13, 0x000, 10, {"40.0", "80.0"} },
{ 0x14, 0x000, 11, {"33.0", "66.6"} },
{ 0x15, 0x100, 12, {"20.0", "40.0"} },
{ 0x16, 0x110, 15, {"16.0", "32.0"} },
{ 0x17, 0x120, 18, {"13.4", "26.8"} },
{ 0x18, 0x000, 25, {"10.0", "20.0"} },
{ 0x19, 0x010, 31, {"8.0", "16.0"} },
{ 0x1a, 0x020, 37, {"6.67", "13.3"} },
{ 0x1b, 0x030, 43, {"5.7", "11.4"} },
{ 0x10, 0x040, 50, {"5.0", "10.0"} },
{ 0x00, 0x050, 56, {"4.4", "8.8" } },
{ 0x00, 0x060, 62, {"4.0", "8.0" } },
{ 0x00, 0x070, 68, {"3.6", "7.2" } },
{ 0x00, 0x000, 0, {NULL, NULL} },
};
#define CTL_OF_SCB(scb) (((scb->hscb)->target_channel_lun >> 3) & 0x1), \
(((scb->hscb)->target_channel_lun >> 4) & 0xf), \
((scb->hscb)->target_channel_lun & 0x07)
#define CTL_OF_CMD(cmd) ((cmd->channel) & 0x01), \
((cmd->target) & 0x0f), \
((cmd->lun) & 0x07)
#define TARGET_INDEX(cmd) ((cmd)->target | ((cmd)->channel << 3))
#define WARN_LEAD KERN_WARNING "(scsi%d:%d:%d:%d) "
#define INFO_LEAD KERN_INFO "(scsi%d:%d:%d:%d) "
static unsigned int aic7xxx_no_reset = 0;
static int aic7xxx_reverse_scan = 0;
static unsigned int aic7xxx_extended = 0;
static int aic7xxx_irq_trigger = -1;
static int aic7xxx_override_term = -1;
static int aic7xxx_stpwlev = -1;
static int aic7xxx_panic_on_abort = 0;
static int aic7xxx_pci_parity = 0;
static int aic7xxx_dump_card = 0;
static int aic7xxx_dump_sequencer = 0;
static int aic7xxx_no_probe = 0;
#ifdef MODULE
static char * aic7xxx = NULL;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,18)
MODULE_PARM(aic7xxx, "s");
#endif
static char dummy_buffer[60] = "Please don't trounce on me insmod!!\n";
#endif
#define VERBOSE_NORMAL 0x0000
#define VERBOSE_NEGOTIATION 0x0001
#define VERBOSE_SEQINT 0x0002
#define VERBOSE_SCSIINT 0x0004
#define VERBOSE_PROBE 0x0008
#define VERBOSE_PROBE2 0x0010
#define VERBOSE_NEGOTIATION2 0x0020
#define VERBOSE_MINOR_ERROR 0x0040
#define VERBOSE_TRACING 0x0080
#define VERBOSE_ABORT 0x0f00
#define VERBOSE_ABORT_MID 0x0100
#define VERBOSE_ABORT_FIND 0x0200
#define VERBOSE_ABORT_PROCESS 0x0400
#define VERBOSE_ABORT_RETURN 0x0800
#define VERBOSE_RESET 0xf000
#define VERBOSE_RESET_MID 0x1000
#define VERBOSE_RESET_FIND 0x2000
#define VERBOSE_RESET_PROCESS 0x4000
#define VERBOSE_RESET_RETURN 0x8000
static int aic7xxx_verbose = VERBOSE_NORMAL | VERBOSE_NEGOTIATION |
VERBOSE_PROBE;
static void aic7xxx_panic_abort(struct aic7xxx_host *p, Scsi_Cmnd *cmd);
static void aic7xxx_print_card(struct aic7xxx_host *p);
static void aic7xxx_print_scratch_ram(struct aic7xxx_host *p);
static void aic7xxx_print_sequencer(struct aic7xxx_host *p, int downloaded);
#ifdef AIC7XXX_VERBOSE_DEBUGGING
static void aic7xxx_check_scbs(struct aic7xxx_host *p, char *buffer);
#endif
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,0)
static inline void
mdelay(int milliseconds)
{
int i;
for(i=0; i<milliseconds; i++)
udelay(1000);
}
static inline int
time_after_eq(unsigned long a, unsigned long b)
{
return((long)((a) - (b)) >= 0L);
}
static inline int
timer_pending(struct timer_list *timer)
{
return( timer->prev != NULL );
}
#define PCI_DEVICE_ID_ADAPTEC_1480A 0x6075
#endif
static inline unsigned char
aic_inb(struct aic7xxx_host *p, long port)
{
#ifdef MMAPIO
unsigned char x;
if(p->maddr)
{
x = p->maddr[port];
}
else
{
x = inb(p->base + port);
}
mb();
return(x);
#else
return(inb(p->base + port));
#endif
}
static inline void
aic_outb(struct aic7xxx_host *p, unsigned char val, long port)
{
#ifdef MMAPIO
if(p->maddr)
{
p->maddr[port] = val;
}
else
{
outb(val, p->base + port);
}
mb();
#else
outb(val, p->base + port);
#endif
}
void
aic7xxx_setup(char *s, int *dummy)
{
int i, n;
char *p;
char *end;
static struct {
const char *name;
unsigned int *flag;
} options[] = {
{ "extended", &aic7xxx_extended },
{ "no_reset", &aic7xxx_no_reset },
{ "irq_trigger", &aic7xxx_irq_trigger },
{ "verbose", &aic7xxx_verbose },
{ "reverse_scan",&aic7xxx_reverse_scan },
{ "override_term", &aic7xxx_override_term },
{ "stpwlev", &aic7xxx_stpwlev },
{ "no_probe", &aic7xxx_no_probe },
{ "panic_on_abort", &aic7xxx_panic_on_abort },
{ "pci_parity", &aic7xxx_pci_parity },
{ "dump_card", &aic7xxx_dump_card },
{ "dump_sequencer", &aic7xxx_dump_sequencer },
{ "tag_info", NULL }
};
end = strchr(s, '\0');
for (p = strtok(s, ",."); p; p = strtok(NULL, ",."))
{
for (i = 0; i < NUMBER(options); i++)
{
n = strlen(options[i].name);
if (!strncmp(options[i].name, p, n))
{
if (!strncmp(p, "tag_info", n))
{
if (p[n] == ':')
{
char *base;
char *tok, *tok_end, *tok_end2;
char tok_list[] = { '.', ',', '{', '}', '\0' };
int i, instance = -1, device = -1;
unsigned char done = FALSE;
base = p;
tok = base + n + 1;
tok_end = strchr(tok, '\0');
if (tok_end < end)
*tok_end = ',';
while(!done)
{
switch(*tok)
{
case '{':
if (instance == -1)
instance = 0;
else if (device == -1)
device = 0;
tok++;
break;
case '}':
if (device != -1)
device = -1;
else if (instance != -1)
instance = -1;
tok++;
break;
case ',':
case '.':
if (instance == -1)
done = TRUE;
else if (device >= 0)
device++;
else if (instance >= 0)
instance++;
if ( (device >= MAX_TARGETS) ||
(instance >= NUMBER(aic7xxx_tag_info)) )
done = TRUE;
tok++;
if (!done)
{
base = tok;
}
break;
case '\0':
done = TRUE;
break;
default:
done = TRUE;
tok_end = strchr(tok, '\0');
for(i=0; tok_list[i]; i++)
{
tok_end2 = strchr(tok, tok_list[i]);
if ( (tok_end2) && (tok_end2 < tok_end) )
{
tok_end = tok_end2;
done = FALSE;
}
}
if ( (instance >= 0) && (device >= 0) &&
(instance < NUMBER(aic7xxx_tag_info)) &&
(device < MAX_TARGETS) )
aic7xxx_tag_info[instance].tag_commands[device] =
simple_strtoul(tok, NULL, 0) & 0xff;
tok = tok_end;
break;
}
}
while((p != base) && (p != NULL))
p = strtok(NULL, ",.");
}
}
else if (p[n] == ':')
{
*(options[i].flag) = simple_strtoul(p + n + 1, NULL, 0);
}
else if (!strncmp(p, "verbose", n))
{
*(options[i].flag) = 0xff09;
}
else
{
*(options[i].flag) = ~(*(options[i].flag));
}
}
}
}
}
static inline void
pause_sequencer(struct aic7xxx_host *p)
{
aic_outb(p, p->pause, HCNTRL);
while ((aic_inb(p, HCNTRL) & PAUSE) == 0)
{
;
}
}
static inline void
unpause_sequencer(struct aic7xxx_host *p, int unpause_always)
{
if (unpause_always ||
( !(aic_inb(p, INTSTAT) & (SCSIINT | SEQINT | BRKADRINT)) &&
!(p->flags & AHC_HANDLING_REQINITS) ) )
{
aic_outb(p, p->unpause, HCNTRL);
}
}
static inline void
restart_sequencer(struct aic7xxx_host *p)
{
aic_outb(p, 0, SEQADDR0);
aic_outb(p, 0, SEQADDR1);
aic_outb(p, FASTMODE, SEQCTL);
}
#include "aic7xxx_seq.c"
static int
aic7xxx_check_patch(struct aic7xxx_host *p,
struct sequencer_patch **start_patch, int start_instr, int *skip_addr)
{
struct sequencer_patch *cur_patch;
struct sequencer_patch *last_patch;
int num_patches;
num_patches = sizeof(sequencer_patches)/sizeof(struct sequencer_patch);
last_patch = &sequencer_patches[num_patches];
cur_patch = *start_patch;
while ((cur_patch < last_patch) && (start_instr == cur_patch->begin))
{
if (cur_patch->patch_func(p) == 0)
{
*skip_addr = start_instr + cur_patch->skip_instr;
cur_patch += cur_patch->skip_patch;
}
else
{
cur_patch++;
}
}
*start_patch = cur_patch;
if (start_instr < *skip_addr)
return (0);
return(1);
}
static void
aic7xxx_download_instr(struct aic7xxx_host *p, int instrptr,
unsigned char *dconsts)
{
union ins_formats instr;
struct ins_format1 *fmt1_ins;
struct ins_format3 *fmt3_ins;
unsigned char opcode;
instr = *(union ins_formats*) &seqprog[instrptr * 4];
instr.integer = le32_to_cpu(instr.integer);
fmt1_ins = &instr.format1;
fmt3_ins = NULL;
opcode = instr.format1.opcode;
switch (opcode)
{
case AIC_OP_JMP:
case AIC_OP_JC:
case AIC_OP_JNC:
case AIC_OP_CALL:
case AIC_OP_JNE:
case AIC_OP_JNZ:
case AIC_OP_JE:
case AIC_OP_JZ:
{
struct sequencer_patch *cur_patch;
int address_offset;
unsigned int address;
int skip_addr;
int i;
fmt3_ins = &instr.format3;
address_offset = 0;
address = fmt3_ins->address;
cur_patch = sequencer_patches;
skip_addr = 0;
for (i = 0; i < address;)
{
aic7xxx_check_patch(p, &cur_patch, i, &skip_addr);
if (skip_addr > i)
{
int end_addr;
end_addr = MIN(address, skip_addr);
address_offset += end_addr - i;
i = skip_addr;
}
else
{
i++;
}
}
address -= address_offset;
fmt3_ins->address = address;
}
case AIC_OP_OR:
case AIC_OP_AND:
case AIC_OP_XOR:
case AIC_OP_ADD:
case AIC_OP_ADC:
case AIC_OP_BMOV:
if (fmt1_ins->parity != 0)
{
fmt1_ins->immediate = dconsts[fmt1_ins->immediate];
}
fmt1_ins->parity = 0;
case AIC_OP_ROL:
if ((p->features & AHC_ULTRA2) != 0)
{
int i, count;
for ( i=0, count=0; i < 31; i++)
{
unsigned int mask;
mask = 0x01 << i;
if ((instr.integer & mask) != 0)
count++;
}
if (!(count & 0x01))
instr.format1.parity = 1;
}
else
{
if (fmt3_ins != NULL)
{
instr.integer = fmt3_ins->immediate |
(fmt3_ins->source << 8) |
(fmt3_ins->address << 16) |
(fmt3_ins->opcode << 25);
}
else
{
instr.integer = fmt1_ins->immediate |
(fmt1_ins->source << 8) |
(fmt1_ins->destination << 16) |
(fmt1_ins->ret << 24) |
(fmt1_ins->opcode << 25);
}
}
aic_outb(p, (instr.integer & 0xff), SEQRAM);
aic_outb(p, ((instr.integer >> 8) & 0xff), SEQRAM);
aic_outb(p, ((instr.integer >> 16) & 0xff), SEQRAM);
aic_outb(p, ((instr.integer >> 24) & 0xff), SEQRAM);
break;
default:
panic("aic7xxx: Unknown opcode encountered in sequencer program.");
break;
}
}
static void
aic7xxx_loadseq(struct aic7xxx_host *p)
{
struct sequencer_patch *cur_patch;
int i;
int downloaded;
int skip_addr;
unsigned char download_consts[4] = {0, 0, 0, 0};
if (aic7xxx_verbose & VERBOSE_PROBE)
{
printk(KERN_INFO "(scsi%d) Downloading sequencer code...", p->host_no);
}
#if 0
download_consts[TMODE_NUMCMDS] = p->num_targetcmds;
#endif
download_consts[TMODE_NUMCMDS] = 0;
cur_patch = &sequencer_patches[0];
downloaded = 0;
skip_addr = 0;
aic_outb(p, PERRORDIS|LOADRAM|FAILDIS|FASTMODE, SEQCTL);
aic_outb(p, 0, SEQADDR0);
aic_outb(p, 0, SEQADDR1);
for (i = 0; i < sizeof(seqprog) / 4; i++)
{
if (aic7xxx_check_patch(p, &cur_patch, i, &skip_addr) == 0)
{
continue;
}
aic7xxx_download_instr(p, i, &download_consts[0]);
downloaded++;
}
aic_outb(p, 0, SEQADDR0);
aic_outb(p, 0, SEQADDR1);
aic_outb(p, FASTMODE | FAILDIS, SEQCTL);
unpause_sequencer(p, TRUE);
mdelay(1);
pause_sequencer(p);
aic_outb(p, FASTMODE, SEQCTL);
if (aic7xxx_verbose & VERBOSE_PROBE)
{
printk(" %d instructions downloaded\n", downloaded);
}
if (aic7xxx_dump_sequencer)
aic7xxx_print_sequencer(p, downloaded);
}
static void
aic7xxx_print_sequencer(struct aic7xxx_host *p, int downloaded)
{
int i, k, temp;
aic_outb(p, PERRORDIS|LOADRAM|FAILDIS|FASTMODE, SEQCTL);
aic_outb(p, 0, SEQADDR0);
aic_outb(p, 0, SEQADDR1);
k = 0;
for (i=0; i < downloaded; i++)
{
if ( k == 0 )
printk("%03x: ", i);
temp = aic_inb(p, SEQRAM);
temp |= (aic_inb(p, SEQRAM) << 8);
temp |= (aic_inb(p, SEQRAM) << 16);
temp |= (aic_inb(p, SEQRAM) << 24);
printk("%08x", temp);
if ( ++k == 8 )
{
printk("\n");
k = 0;
}
else
printk(" ");
}
aic_outb(p, 0, SEQADDR0);
aic_outb(p, 0, SEQADDR1);
aic_outb(p, FASTMODE | FAILDIS, SEQCTL);
unpause_sequencer(p, TRUE);
mdelay(1);
pause_sequencer(p);
aic_outb(p, FASTMODE, SEQCTL);
printk("\n");
}
static void
aic7xxx_delay(int seconds)
{
mdelay(seconds * 1000);
}
const char *
aic7xxx_info(struct Scsi_Host *dooh)
{
static char buffer[256];
char *bp;
struct aic7xxx_host *p;
bp = &buffer[0];
p = (struct aic7xxx_host *)dooh->hostdata;
memset(bp, 0, sizeof(buffer));
strcpy(bp, "Adaptec AHA274x/284x/294x (EISA/VLB/PCI-Fast SCSI) ");
strcat(bp, AIC7XXX_C_VERSION);
strcat(bp, "/");
strcat(bp, AIC7XXX_H_VERSION);
strcat(bp, "\n");
strcat(bp, "       <");
strcat(bp, board_names[p->board_name_index]);
strcat(bp, ">");
return(bp);
}
static struct aic7xxx_syncrate *
aic7xxx_find_syncrate(struct aic7xxx_host *p, unsigned int *period,
unsigned int maxsync)
{
struct aic7xxx_syncrate *syncrate;
syncrate = &aic7xxx_syncrates[maxsync];
while ( (syncrate->rate[0] != NULL) &&
(!(p->features & AHC_ULTRA2) || syncrate->sxfr_ultra2) )
{
if ( *period <= syncrate->period )
{
if(syncrate == &aic7xxx_syncrates[maxsync])
{
*period = syncrate->period;
}
break;
}
syncrate++;
}
if ( (*period == 0) || (syncrate->rate[0] == NULL) ||
((p->features & AHC_ULTRA2) && (syncrate->sxfr_ultra2 == 0)) )
{
*period = 0;
syncrate = NULL;
}
return (syncrate);
}
static unsigned int
aic7xxx_find_period(struct aic7xxx_host *p, unsigned int scsirate,
unsigned int maxsync)
{
struct aic7xxx_syncrate *syncrate;
if ((p->features & AHC_ULTRA2) != 0)
{
scsirate &= SXFR_ULTRA2;
}
else
{
scsirate &= SXFR;
}
syncrate = &aic7xxx_syncrates[maxsync];
while (syncrate->rate[0] != NULL)
{
if ((p->features & AHC_ULTRA2) != 0)
{
if (syncrate->sxfr_ultra2 == 0)
break;
else if (scsirate == syncrate->sxfr_ultra2)
return (syncrate->period);
}
else if (scsirate == (syncrate->sxfr & ~ULTRA_SXFR))
{
return (syncrate->period);
}
syncrate++;
}
return (0);
}
static void
aic7xxx_validate_offset(struct aic7xxx_host *p,
struct aic7xxx_syncrate *syncrate, unsigned int *offset, int wide)
{
unsigned int maxoffset;
if (syncrate == NULL)
{
maxoffset = 0;
}
else if (p->features & AHC_ULTRA2)
{
maxoffset = MAX_OFFSET_ULTRA2;
}
else
{
if (wide)
maxoffset = MAX_OFFSET_16BIT;
else
maxoffset = MAX_OFFSET_8BIT;
}
*offset = MIN(*offset, maxoffset);
}
static void
aic7xxx_set_syncrate(struct aic7xxx_host *p, struct aic7xxx_syncrate *syncrate,
int target, int channel, unsigned int period, unsigned int offset,
unsigned int type)
{
unsigned char tindex;
unsigned short target_mask;
unsigned char lun;
unsigned int old_period, old_offset;
tindex = target | (channel << 3);
target_mask = 0x01 << tindex;
lun = aic_inb(p, SCB_TCL) & 0x07;
if (syncrate == NULL)
{
period = 0;
offset = 0;
}
old_period = p->transinfo[tindex].cur_period;
old_offset = p->transinfo[tindex].cur_offset;
if (type & AHC_TRANS_CUR)
{
unsigned int scsirate;
scsirate = aic_inb(p, TARG_SCSIRATE + tindex);
if (p->features & AHC_ULTRA2)
{
scsirate &= ~SXFR_ULTRA2;
if (syncrate != NULL)
{
scsirate |= syncrate->sxfr_ultra2;
}
if (type & AHC_TRANS_ACTIVE)
{
aic_outb(p, offset, SCSIOFFSET);
}
aic_outb(p, offset, TARG_OFFSET + tindex);
}
else
{
scsirate &= ~(SXFR|SOFS);
p->ultraenb &= ~target_mask;
if (syncrate != NULL)
{
if (syncrate->sxfr & ULTRA_SXFR)
{
p->ultraenb |= target_mask;
}
scsirate |= (syncrate->sxfr & SXFR);
scsirate |= (offset & SOFS);
}
if (type & AHC_TRANS_ACTIVE)
{
unsigned char sxfrctl0;
sxfrctl0 = aic_inb(p, SXFRCTL0);
sxfrctl0 &= ~FAST20;
if (p->ultraenb & target_mask)
sxfrctl0 |= FAST20;
aic_outb(p, sxfrctl0, SXFRCTL0);
}
aic_outb(p, p->ultraenb & 0xff, ULTRA_ENB);
aic_outb(p, (p->ultraenb >> 8) & 0xff, ULTRA_ENB + 1 );
}
if (type & AHC_TRANS_ACTIVE)
{
aic_outb(p, scsirate, SCSIRATE);
}
aic_outb(p, scsirate, TARG_SCSIRATE + tindex);
p->transinfo[tindex].cur_period = period;
p->transinfo[tindex].cur_offset = offset;
if ( !(type & AHC_TRANS_QUITE) &&
(aic7xxx_verbose & VERBOSE_NEGOTIATION) &&
(p->dev_flags[tindex] & DEVICE_PRINT_SDTR) )
{
if (offset)
{
int rate_mod = (scsirate & WIDEXFER) ? 1 : 0;
printk(INFO_LEAD "Synchronous at %s Mbyte/sec, "
"offset %d.\n", p->host_no, channel, target, lun,
syncrate->rate[rate_mod], offset);
}
else
{
printk(INFO_LEAD "Using asynchronous transfers.\n",
p->host_no, channel, target, lun);
}
p->dev_flags[tindex] &= ~DEVICE_PRINT_SDTR;
}
}
if (type & AHC_TRANS_GOAL)
{
p->transinfo[tindex].goal_period = period;
p->transinfo[tindex].goal_offset = offset;
}
if (type & AHC_TRANS_USER)
{
p->transinfo[tindex].user_period = period;
p->transinfo[tindex].user_offset = offset;
}
}
static void
aic7xxx_set_width(struct aic7xxx_host *p, int target, int channel, int lun,
unsigned int width, unsigned int type)
{
unsigned char tindex;
unsigned short target_mask;
unsigned int old_width, new_offset;
tindex = target | (channel << 3);
target_mask = 1 << tindex;
old_width = p->transinfo[tindex].cur_width;
if (p->features & AHC_ULTRA2)
new_offset = MAX_OFFSET_ULTRA2;
else if (width == MSG_EXT_WDTR_BUS_16_BIT)
new_offset = MAX_OFFSET_16BIT;
else
new_offset = MAX_OFFSET_8BIT;
if (type & AHC_TRANS_CUR)
{
unsigned char scsirate;
scsirate = aic_inb(p, TARG_SCSIRATE + tindex);
scsirate &= ~WIDEXFER;
if (width == MSG_EXT_WDTR_BUS_16_BIT)
scsirate |= WIDEXFER;
aic_outb(p, scsirate, TARG_SCSIRATE + tindex);
if (type & AHC_TRANS_ACTIVE)
aic_outb(p, scsirate, SCSIRATE);
p->transinfo[tindex].cur_width = width;
if ((aic7xxx_verbose & VERBOSE_NEGOTIATION2) &&
(p->dev_flags[tindex] & DEVICE_PRINT_WDTR))
{
printk(INFO_LEAD "Using %s transfers\n", p->host_no, channel, target,
lun, (scsirate & WIDEXFER) ? "Wide(16bit)" : "Narrow(8bit)" );
p->dev_flags[tindex] &= ~DEVICE_PRINT_WDTR;
}
}
if (type & AHC_TRANS_GOAL)
p->transinfo[tindex].goal_width = width;
if (type & AHC_TRANS_USER)
p->transinfo[tindex].user_width = width;
if (p->transinfo[tindex].goal_offset)
p->transinfo[tindex].goal_offset = new_offset;
}
static void
scbq_init(volatile scb_queue_type *queue)
{
queue->head = NULL;
queue->tail = NULL;
}
static inline void
scbq_insert_head(volatile scb_queue_type *queue, struct aic7xxx_scb *scb)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags;
#endif
DRIVER_LOCK
scb->q_next = queue->head;
queue->head = scb;
if (queue->tail == NULL)
queue->tail = queue->head;
DRIVER_UNLOCK
}
static inline struct aic7xxx_scb *
scbq_remove_head(volatile scb_queue_type *queue)
{
struct aic7xxx_scb * scbp;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags;
#endif
DRIVER_LOCK
scbp = queue->head;
if (queue->head != NULL)
queue->head = queue->head->q_next;
if (queue->head == NULL)
queue->tail = NULL;
DRIVER_UNLOCK
return(scbp);
}
static inline void
scbq_remove(volatile scb_queue_type *queue, struct aic7xxx_scb *scb)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags;
#endif
DRIVER_LOCK
if (queue->head == scb)
{
scbq_remove_head(queue);
}
else
{
struct aic7xxx_scb *curscb = queue->head;
while ((curscb != NULL) && (curscb->q_next != scb))
{
curscb = curscb->q_next;
}
if (curscb != NULL)
{
curscb->q_next = scb->q_next;
if (scb->q_next == NULL)
{
queue->tail = curscb;
}
}
}
DRIVER_UNLOCK
}
static inline void
scbq_insert_tail(volatile scb_queue_type *queue, struct aic7xxx_scb *scb)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags;
#endif
DRIVER_LOCK
scb->q_next = NULL;
if (queue->tail != NULL)
queue->tail->q_next = scb;
queue->tail = scb;
if (queue->head == NULL)
queue->head = queue->tail;
DRIVER_UNLOCK
}
static int
aic7xxx_match_scb(struct aic7xxx_host *p, struct aic7xxx_scb *scb,
int target, int channel, int lun, unsigned char tag)
{
int targ = (scb->hscb->target_channel_lun >> 4) & 0x0F;
int chan = (scb->hscb->target_channel_lun >> 3) & 0x01;
int slun = scb->hscb->target_channel_lun & 0x07;
int match;
match = ((chan == channel) || (channel == ALL_CHANNELS));
if (match != 0)
match = ((targ == target) || (target == ALL_TARGETS));
if (match != 0)
match = ((lun == slun) || (lun == ALL_LUNS));
if (match != 0)
match = ((tag == scb->hscb->tag) || (tag == SCB_LIST_NULL));
if (aic7xxx_verbose & (VERBOSE_ABORT_PROCESS | VERBOSE_RESET_PROCESS))
{
printk(KERN_INFO "(scsi%d:%d:%d:%d:tag%d) %s search criteria"
" (scsi%d:%d:%d:%d:tag%d)\n", p->host_no, CTL_OF_SCB(scb),
scb->hscb->tag, (match) ? "matches" : "doesn't match",
p->host_no, channel, target, lun, tag);
}
return (match);
}
static void
aic7xxx_add_curscb_to_free_list(struct aic7xxx_host *p)
{
aic_outb(p, SCB_LIST_NULL, SCB_TAG);
aic_outb(p, 0, SCB_CONTROL);
aic_outb(p, aic_inb(p, FREE_SCBH), SCB_NEXT);
aic_outb(p, aic_inb(p, SCBPTR), FREE_SCBH);
}
static unsigned char
aic7xxx_rem_scb_from_disc_list(struct aic7xxx_host *p, unsigned char scbptr)
{
unsigned char next;
unsigned char prev;
aic_outb(p, scbptr, SCBPTR);
next = aic_inb(p, SCB_NEXT);
prev = aic_inb(p, SCB_PREV);
aic7xxx_add_curscb_to_free_list(p);
if (prev != SCB_LIST_NULL)
{
aic_outb(p, prev, SCBPTR);
aic_outb(p, next, SCB_NEXT);
}
else
{
aic_outb(p, next, DISCONNECTED_SCBH);
}
if (next != SCB_LIST_NULL)
{
aic_outb(p, next, SCBPTR);
aic_outb(p, prev, SCB_PREV);
}
return next;
}
static inline void
aic7xxx_busy_target(struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
p->untagged_scbs[scb->hscb->target_channel_lun] = scb->hscb->tag;
}
static inline unsigned char
aic7xxx_index_busy_target(struct aic7xxx_host *p, unsigned char tcl,
int unbusy)
{
unsigned char busy_scbid;
busy_scbid = p->untagged_scbs[tcl];
if (unbusy)
{
p->untagged_scbs[tcl] = SCB_LIST_NULL;
}
return (busy_scbid);
}
static unsigned char
aic7xxx_find_scb(struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
unsigned char saved_scbptr;
unsigned char curindex;
saved_scbptr = aic_inb(p, SCBPTR);
curindex = 0;
for (curindex = 0; curindex < p->scb_data->maxhscbs; curindex++)
{
aic_outb(p, curindex, SCBPTR);
if (aic_inb(p, SCB_TAG) == scb->hscb->tag)
{
break;
}
}
aic_outb(p, saved_scbptr, SCBPTR);
if (curindex >= p->scb_data->maxhscbs)
{
curindex = SCB_LIST_NULL;
}
return (curindex);
}
static int
aic7xxx_allocate_scb(struct aic7xxx_host *p)
{
struct aic7xxx_scb *scbp = NULL;
int scb_size = sizeof(struct aic7xxx_scb) +
sizeof (struct hw_scatterlist) * AIC7XXX_MAX_SG;
int i;
int step = PAGE_SIZE / 1024;
unsigned long scb_count = 0;
struct hw_scatterlist *hsgp;
struct aic7xxx_scb *scb_ap;
unsigned long temp;
if (p->scb_data->numscbs < p->scb_data->maxscbs)
{
for ( i=step;; i *= 2 )
{
if ( (scb_size * (i-1)) >= ( (PAGE_SIZE * (i/step)) - 64 ) )
{
i /= 2;
break;
}
}
scb_count = MIN( (i-1), p->scb_data->maxscbs - p->scb_data->numscbs);
scb_ap = (struct aic7xxx_scb *)kmalloc(scb_size * scb_count, GFP_ATOMIC);
if (scb_ap != NULL)
{
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
{
if (p->scb_data->numscbs == 0)
printk(INFO_LEAD "Allocating initial %ld SCB structures.\n",
p->host_no, -1, -1, -1, scb_count);
else
printk(INFO_LEAD "Allocating %ld additional SCB structures.\n",
p->host_no, -1, -1, -1, scb_count);
}
#endif
memset(scb_ap, 0, scb_count * scb_size);
temp = (unsigned long) &scb_ap[scb_count];
temp += 1023;
temp &= ~1023;
hsgp = (struct hw_scatterlist *)temp;
for (i=0; i < scb_count; i++)
{
scbp = &scb_ap[i];
scbp->hscb = &p->scb_data->hscbs[p->scb_data->numscbs];
scbp->sg_list = &hsgp[i * AIC7XXX_MAX_SG];
memset(scbp->hscb, 0, sizeof(struct aic7xxx_hwscb));
scbp->hscb->tag = p->scb_data->numscbs;
p->scb_data->scb_array[p->scb_data->numscbs++] = scbp;
scbq_insert_head(&p->scb_data->free_scbs, scbp);
}
scbp->kmalloc_ptr = scb_ap;
}
else
{
return(0);
}
}
return(scb_count);
}
static void
aic7xxx_queue_cmd_complete(struct aic7xxx_host *p, Scsi_Cmnd *cmd)
{
cmd->host_scribble = (char *)p->completeq.head;
p->completeq.head = cmd;
}
static void
aic7xxx_done_cmds_complete(struct aic7xxx_host *p)
{
Scsi_Cmnd *cmd;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags = 0;
#endif
DRIVER_LOCK
while (p->completeq.head != NULL)
{
cmd = p->completeq.head;
p->completeq.head = (Scsi_Cmnd *)cmd->host_scribble;
cmd->host_scribble = NULL;
cmd->scsi_done(cmd);
}
DRIVER_UNLOCK
}
static void
aic7xxx_free_scb(struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
scb->flags = SCB_FREE;
scb->cmd = NULL;
scb->sg_count = 0;
scb->sg_length = 0;
scb->tag_action = 0;
scb->hscb->control = 0;
scb->hscb->target_status = 0;
scb->hscb->target_channel_lun = SCB_LIST_NULL;
scbq_insert_head(&p->scb_data->free_scbs, scb);
}
static void
aic7xxx_done(struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
Scsi_Cmnd *cmd = scb->cmd;
int tindex = TARGET_INDEX(cmd);
struct aic7xxx_scb *scbp;
unsigned char queue_depth;
if (scb->flags & SCB_RECOVERY_SCB)
{
p->flags &= ~AHC_ABORT_PENDING;
}
if (scb->flags & SCB_RESET)
{
cmd->result = (DID_RESET << 16) | (cmd->result & 0xffff);
}
else if (scb->flags & SCB_ABORT)
{
cmd->result = (DID_RESET << 16) | (cmd->result & 0xffff);
}
else if (!(p->dev_flags[tindex] & DEVICE_SCANNED))
{
if ( (cmd->cmnd[0] == INQUIRY) && (cmd->result == DID_OK) )
{
char *buffer;
p->dev_flags[tindex] |= DEVICE_PRESENT;
if(cmd->use_sg)
{
struct scatterlist *sg;
sg = (struct scatterlist *)cmd->request_buffer;
buffer = (char *)sg[0].address;
}
else
{
buffer = (char *)cmd->request_buffer;
}
#define WIDE_INQUIRY_BITS 0x60
#define SYNC_INQUIRY_BITS 0x10
if ( (buffer[7] & WIDE_INQUIRY_BITS) &&
(p->features & AHC_WIDE) )
{
p->needwdtr |= (1<<tindex);
p->needwdtr_copy |= (1<<tindex);
if ( (p->flags & AHC_SEEPROM_FOUND) &&
(p->transinfo[tindex].user_width != MSG_EXT_WDTR_BUS_16_BIT) )
p->transinfo[tindex].goal_width = MSG_EXT_WDTR_BUS_8_BIT;
else
p->transinfo[tindex].goal_width = MSG_EXT_WDTR_BUS_16_BIT;
}
else
{
p->needwdtr &= ~(1<<tindex);
p->needwdtr_copy &= ~(1<<tindex);
pause_sequencer(p);
aic7xxx_set_width(p, cmd->target, cmd->channel, cmd->lun,
MSG_EXT_WDTR_BUS_8_BIT, (AHC_TRANS_ACTIVE |
AHC_TRANS_GOAL |
AHC_TRANS_CUR) );
unpause_sequencer(p, FALSE);
}
if (buffer[7] & SYNC_INQUIRY_BITS)
{
p->needsdtr |= (1<<tindex);
p->needsdtr_copy |= (1<<tindex);
if (p->flags & AHC_SEEPROM_FOUND)
{
p->transinfo[tindex].goal_period = p->transinfo[tindex].user_period;
p->transinfo[tindex].goal_offset = p->transinfo[tindex].user_offset;
}
else
{
if (p->features & AHC_ULTRA2)
{
p->transinfo[tindex].goal_period =
aic7xxx_syncrates[AHC_SYNCRATE_ULTRA2].period;
}
else if (p->features & AHC_ULTRA)
{
p->transinfo[tindex].goal_period =
aic7xxx_syncrates[AHC_SYNCRATE_ULTRA].period;
}
else
{
p->transinfo[tindex].goal_period =
aic7xxx_syncrates[AHC_SYNCRATE_FAST].period;
}
if (p->features & AHC_ULTRA2)
p->transinfo[tindex].goal_offset = MAX_OFFSET_ULTRA2;
else if (p->transinfo[tindex].goal_width == MSG_EXT_WDTR_BUS_16_BIT)
p->transinfo[tindex].goal_offset = MAX_OFFSET_16BIT;
else
p->transinfo[tindex].goal_offset = MAX_OFFSET_8BIT;
}
}
else
{
p->needsdtr &= ~(1<<tindex);
p->needsdtr_copy &= ~(1<<tindex);
p->transinfo[tindex].goal_period = 0;
p->transinfo[tindex].goal_offset = 0;
}
p->dev_flags[tindex] |= DEVICE_SCANNED;
p->dev_flags[tindex] |= DEVICE_PRINT_WDTR | DEVICE_PRINT_SDTR;
#undef WIDE_INQUIRY_BITS
#undef SYNC_INQUIRY_BITS
}
}
else if ((scb->flags & (SCB_MSGOUT_WDTR | SCB_MSGOUT_SDTR)) != 0)
{
unsigned short mask;
int message_error = FALSE;
mask = 0x01 << tindex;
if ((scb->flags & SCB_SENSE) &&
((scb->cmd->sense_buffer[12] == 0x43) ||
(scb->cmd->sense_buffer[12] == 0x49)))
{
message_error = TRUE;
}
if (scb->flags & SCB_MSGOUT_WDTR)
{
p->wdtr_pending &= ~mask;
if (message_error)
{
if ( (aic7xxx_verbose & VERBOSE_NEGOTIATION2) &&
(p->dev_flags[tindex] & DEVICE_PRINT_WDTR) )
{
printk(INFO_LEAD "Device failed to complete Wide Negotiation "
"processing and\n", p->host_no, CTL_OF_SCB(scb));
printk(INFO_LEAD "returned a sense error code for invalid message, "
"disabling future\n", p->host_no, CTL_OF_SCB(scb));
printk(INFO_LEAD "Wide negotiation to this device.\n", p->host_no,
CTL_OF_SCB(scb));
p->dev_flags[tindex] &= ~DEVICE_PRINT_WDTR;
}
p->needwdtr &= ~mask;
p->needwdtr_copy &= ~mask;
}
}
if (scb->flags & SCB_MSGOUT_SDTR)
{
p->sdtr_pending &= ~mask;
if (message_error)
{
if ( (aic7xxx_verbose & VERBOSE_NEGOTIATION2) &&
(p->dev_flags[tindex] & DEVICE_PRINT_SDTR) )
{
printk(INFO_LEAD "Device failed to complete Sync Negotiation "
"processing and\n", p->host_no, CTL_OF_SCB(scb));
printk(INFO_LEAD "returned a sense error code for invalid message, "
"disabling future\n", p->host_no, CTL_OF_SCB(scb));
printk(INFO_LEAD "Sync negotiation to this device.\n", p->host_no,
CTL_OF_SCB(scb));
p->dev_flags[tindex] &= ~DEVICE_PRINT_SDTR;
}
p->needsdtr &= ~mask;
p->needsdtr_copy &= ~mask;
}
}
}
queue_depth = p->dev_temp_queue_depth[tindex];
if (queue_depth >= p->dev_active_cmds[tindex])
{
scbp = scbq_remove_head(&p->delayed_scbs[tindex]);
if (scbp)
{
if (queue_depth == 1)
{
scbq_insert_head(&p->waiting_scbs, scbp);
}
else
{
scbq_insert_tail(&p->waiting_scbs, scbp);
}
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Moving SCB from delayed to waiting queue.\n",
p->host_no, CTL_OF_SCB(scbp));
#endif
if (queue_depth > p->dev_active_cmds[tindex])
{
scbp = scbq_remove_head(&p->delayed_scbs[tindex]);
if (scbp)
scbq_insert_tail(&p->waiting_scbs, scbp);
}
}
}
if ( !(scb->tag_action) && (p->tagenable & (1<<tindex)) )
{
p->dev_temp_queue_depth[tindex] = p->dev_max_queue_depth[tindex];
}
p->dev_active_cmds[tindex]--;
p->activescbs--;
if (aic7xxx_index_busy_target(p, scb->hscb->target_channel_lun, FALSE) ==
scb->hscb->tag)
{
aic7xxx_index_busy_target(p, scb->hscb->target_channel_lun, TRUE);
}
{
int actual;
actual = scb->sg_length;
if ((actual >= 512) && (((cmd->result >> 16) & 0xf) == DID_OK))
{
struct aic7xxx_xferstats *sp;
#ifdef AIC7XXX_PROC_STATS
long *ptr;
int x;
#endif
sp = &p->stats[TARGET_INDEX(cmd)];
if ( (cmd->request.cmd == WRITE) || (cmd->data_cmnd[0] == WRITE_6) ||
(cmd->data_cmnd[0] == WRITE_FILEMARKS) )
{
sp->w_total++;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if ( (sp->w_total > 16) && (aic7xxx_verbose > 0xffff) )
aic7xxx_verbose &= 0xffff;
#endif
#ifdef AIC7XXX_PROC_STATS
ptr = sp->w_bins;
#endif
}
else
{
sp->r_total++;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if ( (sp->r_total > 16) && (aic7xxx_verbose > 0xffff) )
aic7xxx_verbose &= 0xffff;
#endif
#ifdef AIC7XXX_PROC_STATS
ptr = sp->r_bins;
#endif
}
#ifdef AIC7XXX_PROC_STATS
x = -10;
while(actual)
{
actual >>= 1;
x++;
}
if (x < 0)
{
ptr[0]++;
}
else if (x > 7)
{
ptr[7]++;
}
else
{
ptr[x]++;
}
#endif
}
}
aic7xxx_free_scb(p, scb);
aic7xxx_queue_cmd_complete(p, cmd);
}
static void
aic7xxx_run_done_queue(struct aic7xxx_host *p, int complete)
{
struct aic7xxx_scb *scb;
int i, found = 0;
for (i = 0; i < p->scb_data->numscbs; i++)
{
scb = p->scb_data->scb_array[i];
if (scb->flags & SCB_QUEUED_FOR_DONE)
{
if (aic7xxx_verbose & (VERBOSE_ABORT_PROCESS | VERBOSE_RESET_PROCESS))
printk(INFO_LEAD "Aborting scb %d\n",
p->host_no, CTL_OF_SCB(scb), scb->hscb->tag);
found++;
aic7xxx_done(p, scb);
}
}
if (aic7xxx_verbose & (VERBOSE_ABORT_RETURN | VERBOSE_RESET_RETURN))
{
printk(INFO_LEAD "%d commands found and queued for "
"completion.\n", p->host_no, -1, -1, -1, found);
}
if (complete)
{
aic7xxx_done_cmds_complete(p);
}
}
static unsigned char
aic7xxx_abort_waiting_scb(struct aic7xxx_host *p, struct aic7xxx_scb *scb,
unsigned char scbpos, unsigned char prev)
{
unsigned char curscb, next;
curscb = aic_inb(p, SCBPTR);
aic_outb(p, scbpos, SCBPTR);
next = aic_inb(p, SCB_NEXT);
aic7xxx_add_curscb_to_free_list(p);
if (prev == SCB_LIST_NULL)
{
aic_outb(p, next, WAITING_SCBH);
}
else
{
aic_outb(p, prev, SCBPTR);
aic_outb(p, next, SCB_NEXT);
}
aic_outb(p, curscb, SCBPTR);
return (next);
}
static int
aic7xxx_search_qinfifo(struct aic7xxx_host *p, int target, int channel,
int lun, unsigned char tag, int flags, int requeue,
volatile scb_queue_type *queue)
{
int found;
unsigned char qinpos, qintail;
struct aic7xxx_scb *scbp;
found = 0;
qinpos = aic_inb(p, QINPOS);
qintail = p->qinfifonext;
p->qinfifonext = qinpos;
while (qinpos != qintail)
{
scbp = p->scb_data->scb_array[p->qinfifo[qinpos++]];
if (aic7xxx_match_scb(p, scbp, target, channel, lun, tag))
{
if (requeue && (queue != NULL))
{
if (scbp->flags & SCB_WAITINGQ)
{
scbq_remove(queue, scbp);
scbq_remove(&p->waiting_scbs, scbp);
scbq_remove(&p->delayed_scbs[TARGET_INDEX(scbp->cmd)], scbp);
p->dev_active_cmds[TARGET_INDEX(scbp->cmd)]++;
p->activescbs++;
}
scbq_insert_tail(queue, scbp);
p->dev_active_cmds[TARGET_INDEX(scbp->cmd)]--;
p->activescbs--;
scbp->flags |= SCB_WAITINGQ;
if ( !(scbp->tag_action & TAG_ENB) )
{
aic7xxx_index_busy_target(p, scbp->hscb->target_channel_lun,
TRUE);
}
}
else if (requeue)
{
p->qinfifo[p->qinfifonext++] = scbp->hscb->tag;
}
else
{
scbp->flags = flags | (scbp->flags & SCB_RECOVERY_SCB);
if (aic7xxx_index_busy_target(p, scbp->hscb->target_channel_lun,
FALSE) == scbp->hscb->tag)
{
aic7xxx_index_busy_target(p, scbp->hscb->target_channel_lun,
TRUE);
}
}
found++;
}
else
{
p->qinfifo[p->qinfifonext++] = scbp->hscb->tag;
}
}
qinpos = p->qinfifonext;
while(qinpos != qintail)
{
p->qinfifo[qinpos++] = SCB_LIST_NULL;
}
if (p->features & AHC_QUEUE_REGS)
aic_outb(p, p->qinfifonext, HNSCB_QOFF);
else
aic_outb(p, p->qinfifonext, KERNEL_QINPOS);
return (found);
}
static int
aic7xxx_scb_on_qoutfifo(struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
int i=0;
while(p->qoutfifo[(p->qoutfifonext + i) & 0xff ] != SCB_LIST_NULL)
{
if(p->qoutfifo[(p->qoutfifonext + i) & 0xff ] == scb->hscb->tag)
return TRUE;
else
i++;
}
return FALSE;
}
static void
aic7xxx_reset_device(struct aic7xxx_host *p, int target, int channel,
int lun, unsigned char tag)
{
struct aic7xxx_scb *scbp;
unsigned char active_scb, tcl;
int i = 0, j, init_lists = FALSE;
active_scb = aic_inb(p, SCBPTR);
if (aic7xxx_verbose & (VERBOSE_RESET_PROCESS | VERBOSE_ABORT_PROCESS))
printk(INFO_LEAD "Reset device, active_scb %d\n",
p->host_no, channel, target, lun, active_scb);
{
int min_target, max_target;
struct aic7xxx_scb *scbp, *prev_scbp;
if (target == ALL_TARGETS)
{
switch (channel)
{
case 0:
min_target = 0;
max_target = (p->features & AHC_WIDE) ? 15 : 7;
break;
case 1:
min_target = 8;
max_target = 15;
break;
case ALL_CHANNELS:
default:
min_target = 0;
max_target = (p->features & (AHC_TWIN|AHC_WIDE)) ? 15 : 7;
break;
}
}
else
{
min_target = target | (channel << 3);
max_target = min_target;
}
for (i = min_target; i <= max_target; i++)
{
if ( i == p->scsi_id )
{
continue;
}
if (aic7xxx_verbose & (VERBOSE_ABORT_PROCESS | VERBOSE_RESET_PROCESS))
printk(INFO_LEAD "Cleaning up status information "
"and delayed_scbs.\n", p->host_no, channel, i, lun);
p->dev_flags[i] &= ~BUS_DEVICE_RESET_PENDING;
if ( tag == SCB_LIST_NULL )
{
p->dev_flags[i] |= DEVICE_PRINT_WDTR | DEVICE_PRINT_SDTR |
DEVICE_RESET_DELAY;
p->dev_expires[i] = jiffies + (4 * HZ);
p->dev_timer_active |= (0x01 << i);
p->dev_last_queue_full_count[i] = 0;
p->dev_last_queue_full[i] = 0;
p->dev_temp_queue_depth[i] =
p->dev_max_queue_depth[i];
}
for(j=0; j<MAX_LUNS; j++)
{
if (channel == 1)
tcl = ((i << 4) & 0x70) | (channel << 3) | j;
else
tcl = (i << 4) | (channel << 3) | j;
if ( (aic7xxx_index_busy_target(p, tcl, FALSE) == tag) ||
(tag == SCB_LIST_NULL) )
aic7xxx_index_busy_target(p, tcl, TRUE);
}
j = 0;
prev_scbp = NULL;
scbp = p->delayed_scbs[i].head;
while ( (scbp != NULL) && (j++ <= (p->scb_data->numscbs + 1)) )
{
prev_scbp = scbp;
scbp = scbp->q_next;
if ( prev_scbp == scbp )
{
if (aic7xxx_verbose & (VERBOSE_ABORT | VERBOSE_RESET))
printk(WARN_LEAD "Yikes!! scb->q_next == scb "
"in the delayed_scbs queue!\n", p->host_no, channel, i, lun);
scbp = NULL;
prev_scbp->q_next = NULL;
p->delayed_scbs[i].tail = prev_scbp;
}
if (aic7xxx_match_scb(p, prev_scbp, target, channel, lun, tag))
{
scbq_remove(&p->delayed_scbs[i], prev_scbp);
if (prev_scbp->flags & SCB_WAITINGQ)
{
p->dev_active_cmds[i]++;
p->activescbs++;
}
prev_scbp->flags &= ~(SCB_ACTIVE | SCB_WAITINGQ);
prev_scbp->flags |= SCB_RESET | SCB_QUEUED_FOR_DONE;
}
}
if ( j > (p->scb_data->maxscbs + 1) )
{
if (aic7xxx_verbose & (VERBOSE_ABORT | VERBOSE_RESET))
printk(WARN_LEAD "Yikes!! There's a loop in the "
"delayed_scbs queue!\n", p->host_no, channel, i, lun);
scbq_init(&p->delayed_scbs[i]);
}
if ( !(p->dev_timer_active & (0x01 << MAX_TARGETS)) ||
time_after_eq(p->dev_timer.expires, p->dev_expires[i]) )
{
del_timer(&p->dev_timer);
p->dev_timer.expires = p->dev_expires[i];
add_timer(&p->dev_timer);
p->dev_timer_active |= (0x01 << MAX_TARGETS);
}
}
}
if (aic7xxx_verbose & (VERBOSE_ABORT_PROCESS | VERBOSE_RESET_PROCESS))
printk(INFO_LEAD "Cleaning QINFIFO.\n", p->host_no, channel, target, lun );
aic7xxx_search_qinfifo(p, target, channel, lun, tag,
SCB_RESET | SCB_QUEUED_FOR_DONE, FALSE, NULL);
if (aic7xxx_verbose & (VERBOSE_ABORT_PROCESS | VERBOSE_RESET_PROCESS))
printk(INFO_LEAD "Cleaning waiting_scbs.\n", p->host_no, channel,
target, lun );
{
struct aic7xxx_scb *scbp, *prev_scbp;
j = 0;
prev_scbp = NULL;
scbp = p->waiting_scbs.head;
while ( (scbp != NULL) && (j++ <= (p->scb_data->numscbs + 1)) )
{
prev_scbp = scbp;
scbp = scbp->q_next;
if ( prev_scbp == scbp )
{
if (aic7xxx_verbose & (VERBOSE_ABORT | VERBOSE_RESET))
printk(WARN_LEAD "Yikes!! scb->q_next == scb "
"in the waiting_scbs queue!\n", p->host_no, CTL_OF_SCB(scbp));
scbp = NULL;
prev_scbp->q_next = NULL;
p->waiting_scbs.tail = prev_scbp;
}
if (aic7xxx_match_scb(p, prev_scbp, target, channel, lun, tag))
{
scbq_remove(&p->waiting_scbs, prev_scbp);
if (prev_scbp->flags & SCB_WAITINGQ)
{
p->dev_active_cmds[TARGET_INDEX(prev_scbp->cmd)]++;
p->activescbs++;
}
prev_scbp->flags &= ~(SCB_ACTIVE | SCB_WAITINGQ);
prev_scbp->flags |= SCB_RESET | SCB_QUEUED_FOR_DONE;
}
}
if ( j > (p->scb_data->maxscbs + 1) )
{
if (aic7xxx_verbose & (VERBOSE_ABORT | VERBOSE_RESET))
printk(WARN_LEAD "Yikes!! There's a loop in the "
"waiting_scbs queue!\n", p->host_no, channel, target, lun);
scbq_init(&p->waiting_scbs);
}
}
if (aic7xxx_verbose & (VERBOSE_ABORT_PROCESS | VERBOSE_RESET_PROCESS))
printk(INFO_LEAD "Cleaning waiting for selection "
"list.\n", p->host_no, channel, target, lun);
{
unsigned char next, prev, scb_index;
next = aic_inb(p, WAITING_SCBH);
prev = SCB_LIST_NULL;
j = 0;
while ( (next != SCB_LIST_NULL) && (j++ <= (p->scb_data->maxscbs + 1)) )
{
aic_outb(p, next, SCBPTR);
scb_index = aic_inb(p, SCB_TAG);
if (scb_index >= p->scb_data->numscbs)
{
printk(WARN_LEAD "Waiting List inconsistency; SCB index=%d, "
"numscbs=%d\n", p->host_no, channel, target, lun, scb_index,
p->scb_data->numscbs);
next = aic_inb(p, SCB_NEXT);
aic7xxx_add_curscb_to_free_list(p);
}
else
{
scbp = p->scb_data->scb_array[scb_index];
if (aic7xxx_match_scb(p, scbp, target, channel, lun, tag))
{
next = aic7xxx_abort_waiting_scb(p, scbp, next, prev);
if (scbp->flags & SCB_WAITINGQ)
{
p->dev_active_cmds[TARGET_INDEX(scbp->cmd)]++;
p->activescbs++;
}
scbp->flags &= ~(SCB_ACTIVE | SCB_WAITINGQ);
scbp->flags |= SCB_RESET | SCB_QUEUED_FOR_DONE;
if (prev == SCB_LIST_NULL)
{
aic_outb(p, aic_inb(p, SCSISEQ) & ~ENSELO, SCSISEQ);
aic_outb(p, CLRSELTIMEO, CLRSINT1);
}
}
else
{
prev = next;
next = aic_inb(p, SCB_NEXT);
}
}
}
if ( j > (p->scb_data->maxscbs + 1) )
{
printk(WARN_LEAD "Yikes!!  There is a loop in the waiting for "
"selection list!\n", p->host_no, channel, target, lun);
init_lists = TRUE;
}
}
if (aic7xxx_verbose & (VERBOSE_ABORT_PROCESS | VERBOSE_RESET_PROCESS))
printk(INFO_LEAD "Cleaning disconnected scbs "
"list.\n", p->host_no, channel, target, lun);
if (p->flags & AHC_PAGESCBS)
{
unsigned char next, prev, scb_index;
next = aic_inb(p, DISCONNECTED_SCBH);
prev = SCB_LIST_NULL;
j = 0;
while ( (next != SCB_LIST_NULL) && (j++ <= (p->scb_data->maxscbs + 1)) )
{
aic_outb(p, next, SCBPTR);
scb_index = aic_inb(p, SCB_TAG);
if (scb_index > p->scb_data->numscbs)
{
printk(WARN_LEAD "Disconnected List inconsistency; SCB index=%d, "
"numscbs=%d\n", p->host_no, channel, target, lun, scb_index,
p->scb_data->numscbs);
next = aic7xxx_rem_scb_from_disc_list(p, next);
}
else
{
scbp = p->scb_data->scb_array[scb_index];
if (aic7xxx_match_scb(p, scbp, target, channel, lun, tag))
{
next = aic7xxx_rem_scb_from_disc_list(p, next);
if (scbp->flags & SCB_WAITINGQ)
{
p->dev_active_cmds[TARGET_INDEX(scbp->cmd)]++;
p->activescbs++;
}
scbp->flags &= ~(SCB_ACTIVE | SCB_WAITINGQ);
scbp->flags |= SCB_RESET | SCB_QUEUED_FOR_DONE;
scbp->hscb->control = 0;
}
else
{
prev = next;
next = aic_inb(p, SCB_NEXT);
}
}
}
if ( j > (p->scb_data->maxscbs + 1) )
{
printk(WARN_LEAD "Yikes!!  There is a loop in the disconnected list!\n",
p->host_no, channel, target, lun);
init_lists = TRUE;
}
}
if (p->flags & AHC_PAGESCBS)
{
unsigned char next;
j = 0;
next = aic_inb(p, FREE_SCBH);
if ( (next >= p->scb_data->maxhscbs) && (next != SCB_LIST_NULL) )
{
printk(WARN_LEAD "Bogus FREE_SCBH!.\n", p->host_no, channel,
target, lun);
init_lists = TRUE;
next = SCB_LIST_NULL;
}
while ( (next != SCB_LIST_NULL) && (j++ <= (p->scb_data->maxscbs + 1)) )
{
aic_outb(p, next, SCBPTR);
if (aic_inb(p, SCB_TAG) < p->scb_data->numscbs)
{
printk(WARN_LEAD "Free list inconsistency!.\n", p->host_no, channel,
target, lun);
init_lists = TRUE;
next = SCB_LIST_NULL;
}
else
{
aic_outb(p, SCB_LIST_NULL, SCB_TAG);
aic_outb(p, 0, SCB_CONTROL);
next = aic_inb(p, SCB_NEXT);
}
}
if ( j > (p->scb_data->maxscbs + 1) )
{
printk(WARN_LEAD "Yikes!!  There is a loop in the free list!\n",
p->host_no, channel, target, lun);
init_lists = TRUE;
}
}
if (init_lists)
{
aic_outb(p, SCB_LIST_NULL, FREE_SCBH);
aic_outb(p, SCB_LIST_NULL, WAITING_SCBH);
aic_outb(p, SCB_LIST_NULL, DISCONNECTED_SCBH);
}
for (i = p->scb_data->maxhscbs - 1; i >= 0; i--)
{
unsigned char scbid;
aic_outb(p, i, SCBPTR);
if (init_lists)
{
aic_outb(p, SCB_LIST_NULL, SCB_TAG);
aic_outb(p, SCB_LIST_NULL, SCB_NEXT);
aic_outb(p, SCB_LIST_NULL, SCB_PREV);
aic_outb(p, 0, SCB_CONTROL);
aic7xxx_add_curscb_to_free_list(p);
}
else
{
scbid = aic_inb(p, SCB_TAG);
if (scbid < p->scb_data->numscbs)
{
scbp = p->scb_data->scb_array[scbid];
if (aic7xxx_match_scb(p, scbp, target, channel, lun, tag))
{
aic_outb(p, 0, SCB_CONTROL);
aic_outb(p, SCB_LIST_NULL, SCB_TAG);
aic7xxx_add_curscb_to_free_list(p);
}
}
}
}
for (i = 0; i < p->scb_data->numscbs; i++)
{
scbp = p->scb_data->scb_array[i];
if ((scbp->flags & SCB_ACTIVE) &&
aic7xxx_match_scb(p, scbp, target, channel, lun, tag) &&
!aic7xxx_scb_on_qoutfifo(p, scbp))
{
if (scbp->flags & SCB_WAITINGQ)
{
scbq_remove(&p->waiting_scbs, scbp);
scbq_remove(&p->delayed_scbs[TARGET_INDEX(scbp->cmd)], scbp);
p->dev_active_cmds[TARGET_INDEX(scbp->cmd)]++;
p->activescbs++;
}
scbp->flags |= SCB_RESET | SCB_QUEUED_FOR_DONE;
scbp->flags &= ~(SCB_ACTIVE | SCB_WAITINGQ);
}
}
aic_outb(p, active_scb, SCBPTR);
}
static void
aic7xxx_clear_intstat(struct aic7xxx_host *p)
{
aic_outb(p, CLRSELDO | CLRSELDI | CLRSELINGO, CLRSINT0);
aic_outb(p, CLRSELTIMEO | CLRATNO | CLRSCSIRSTI | CLRBUSFREE | CLRSCSIPERR |
CLRPHASECHG | CLRREQINIT, CLRSINT1);
aic_outb(p, CLRSCSIINT | CLRSEQINT | CLRBRKADRINT | CLRPARERR, CLRINT);
}
static void
aic7xxx_reset_current_bus(struct aic7xxx_host *p)
{
aic_outb(p, aic_inb(p, SIMODE1) & ~ENSCSIRST, SIMODE1);
aic_outb(p, aic_inb(p, SCSISEQ) | SCSIRSTO, SCSISEQ);
while ( (aic_inb(p, SCSISEQ) & SCSIRSTO) == 0)
mdelay(5);
mdelay(10);
aic_outb(p, 0, SCSISEQ);
mdelay(5);
aic7xxx_clear_intstat(p);
aic_outb(p, aic_inb(p, SIMODE1) | ENSCSIRST, SIMODE1);
}
static void
aic7xxx_reset_channel(struct aic7xxx_host *p, int channel, int initiate_reset)
{
unsigned long offset_min, offset_max;
unsigned char sblkctl;
int cur_channel;
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Reset channel called, %s initiate reset.\n",
p->host_no, channel, -1, -1, (initiate_reset==TRUE) ? "will" : "won't" );
if (channel == 1)
{
p->needsdtr |= (p->needsdtr_copy & 0xFF00);
p->sdtr_pending &= 0x00FF;
offset_min = 8;
offset_max = 16;
}
else
{
if (p->features & AHC_WIDE)
{
p->needsdtr = p->needsdtr_copy;
p->needwdtr = p->needwdtr_copy;
p->sdtr_pending = 0x0;
p->wdtr_pending = 0x0;
offset_min = 0;
offset_max = 16;
}
else
{
p->needsdtr |= (p->needsdtr_copy & 0x00FF);
p->sdtr_pending &= 0xFF00;
offset_min = 0;
offset_max = 8;
}
}
while (offset_min < offset_max)
{
aic_outb(p, 0, TARG_SCSIRATE + offset_min);
if (p->features & AHC_ULTRA2)
{
aic_outb(p, 0, TARG_OFFSET + offset_min);
}
offset_min++;
}
sblkctl = aic_inb(p, SBLKCTL);
if ( (p->chip & AHC_CHIPID_MASK) == AHC_AIC7770 )
cur_channel = (sblkctl & SELBUSB) >> 3;
else
cur_channel = 0;
if ( (cur_channel != channel) && (p->features & AHC_TWIN) )
{
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Stealthily resetting idle channel.\n", p->host_no,
channel, -1, -1);
aic_outb(p, sblkctl ^ SELBUSB, SBLKCTL);
aic_outb(p, aic_inb(p, SIMODE1) & ~ENBUSFREE, SIMODE1);
if (initiate_reset)
{
aic7xxx_reset_current_bus(p);
}
aic_outb(p, aic_inb(p, SCSISEQ) & (ENSELI|ENRSELI|ENAUTOATNP), SCSISEQ);
aic7xxx_clear_intstat(p);
aic_outb(p, sblkctl, SBLKCTL);
}
else
{
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Resetting currently active channel.\n", p->host_no,
channel, -1, -1);
aic_outb(p, aic_inb(p, SIMODE1) & ~(ENBUSFREE|ENREQINIT),
SIMODE1);
p->flags &= ~AHC_HANDLING_REQINITS;
p->msg_type = MSG_TYPE_NONE;
p->msg_len = 0;
if (initiate_reset)
{
aic7xxx_reset_current_bus(p);
}
aic_outb(p, aic_inb(p, SCSISEQ) & (ENSELI|ENRSELI|ENAUTOATNP), SCSISEQ);
aic7xxx_clear_intstat(p);
}
if (aic7xxx_verbose & VERBOSE_RESET_RETURN)
printk(INFO_LEAD "Channel reset\n", p->host_no, channel, -1, -1);
aic7xxx_reset_device(p, ALL_TARGETS, channel, ALL_LUNS, SCB_LIST_NULL);
if ( !(p->features & AHC_TWIN) )
{
restart_sequencer(p);
}
return;
}
static void
aic7xxx_run_waiting_queues(struct aic7xxx_host *p)
{
struct aic7xxx_scb *scb;
int tindex;
int sent;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags = 0;
#endif
if (p->waiting_scbs.head == NULL)
return;
sent = 0;
DRIVER_LOCK
while ((scb = scbq_remove_head(&p->waiting_scbs)) != NULL)
{
tindex = TARGET_INDEX(scb->cmd);
if ( !scb->tag_action && (p->tagenable & (1<<tindex)) )
{
p->dev_temp_queue_depth[tindex] = 1;
}
if ( (p->dev_active_cmds[tindex] >=
p->dev_temp_queue_depth[tindex]) ||
(p->dev_flags[tindex] & (DEVICE_RESET_DELAY|DEVICE_WAS_BUSY)) ||
(p->flags & AHC_RESET_DELAY) )
{
scbq_insert_tail(&p->delayed_scbs[tindex], scb);
}
else
{
scb->flags &= ~SCB_WAITINGQ;
p->dev_active_cmds[tindex]++;
p->activescbs++;
if ( !(scb->tag_action) )
{
aic7xxx_busy_target(p, scb);
}
p->qinfifo[p->qinfifonext++] = scb->hscb->tag;
sent++;
}
}
if (sent)
{
if (p->features & AHC_QUEUE_REGS)
aic_outb(p, p->qinfifonext, HNSCB_QOFF);
else
{
pause_sequencer(p);
aic_outb(p, p->qinfifonext, KERNEL_QINPOS);
unpause_sequencer(p, FALSE);
}
if (p->activescbs > p->max_activescbs)
p->max_activescbs = p->activescbs;
}
DRIVER_UNLOCK
}
#ifdef CONFIG_PCI
#define DPE 0x80
#define SSE 0x40
#define RMA 0x20
#define RTA 0x10
#define STA 0x08
#define DPR 0x01
static void
aic7xxx_pci_intr(struct aic7xxx_host *p)
{
unsigned char status1;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
pci_read_config_byte(p->pdev, PCI_STATUS + 1, &status1);
#else
pcibios_read_config_byte(p->pci_bus, p->pci_device_fn,
PCI_STATUS + 1, &status1);
#endif
if ( (status1 & DPE) && (aic7xxx_verbose & VERBOSE_MINOR_ERROR) )
printk(WARN_LEAD "Data Parity Error during PCI address or PCI write"
"phase.\n", p->host_no, -1, -1, -1);
if ( (status1 & SSE) && (aic7xxx_verbose & VERBOSE_MINOR_ERROR) )
printk(WARN_LEAD "Signal System Error Detected\n", p->host_no,
-1, -1, -1);
if ( (status1 & RMA) && (aic7xxx_verbose & VERBOSE_MINOR_ERROR) )
printk(WARN_LEAD "Received a PCI Master Abort\n", p->host_no,
-1, -1, -1);
if ( (status1 & RTA) && (aic7xxx_verbose & VERBOSE_MINOR_ERROR) )
printk(WARN_LEAD "Received a PCI Target Abort\n", p->host_no,
-1, -1, -1);
if ( (status1 & STA) && (aic7xxx_verbose & VERBOSE_MINOR_ERROR) )
printk(WARN_LEAD "Signaled a PCI Target Abort\n", p->host_no,
-1, -1, -1);
if ( (status1 & DPR) && (aic7xxx_verbose & VERBOSE_MINOR_ERROR) )
printk(WARN_LEAD "Data Parity Error has been reported via PCI pin "
"PERR#\n", p->host_no, -1, -1, -1);
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
pci_write_config_byte(p->pdev, PCI_STATUS + 1, status1);
#else
pcibios_write_config_byte(p->pci_bus, p->pci_device_fn,
PCI_STATUS + 1, status1);
#endif
if (status1 & (DPR|RMA|RTA))
aic_outb(p, CLRPARERR, CLRINT);
if ( (aic7xxx_panic_on_abort) && (p->spurious_int > 500) )
aic7xxx_panic_abort(p, NULL);
}
#endif
static void
aic7xxx_timer(struct aic7xxx_host *p)
{
int i, j;
unsigned long cpu_flags = 0;
struct aic7xxx_scb *scb;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
DRIVER_LOCK
#else
spin_lock_irqsave(&io_request_lock, cpu_flags);
#endif
p->dev_timer_active &= ~(0x01 << MAX_TARGETS);
if ( (p->dev_timer_active & (0x01 << p->scsi_id)) &&
time_after_eq(jiffies, p->dev_expires[p->scsi_id]) )
{
p->flags &= ~AHC_RESET_DELAY;
p->dev_timer_active &= ~(0x01 << p->scsi_id);
}
for(i=0; i<MAX_TARGETS; i++)
{
if ( (i != p->scsi_id) &&
(p->dev_timer_active & (0x01 << i)) &&
time_after_eq(jiffies, p->dev_expires[i]) )
{
p->dev_timer_active &= ~(0x01 << i);
p->dev_flags[i] &= ~(DEVICE_RESET_DELAY|DEVICE_WAS_BUSY);
p->dev_temp_queue_depth[i] = p->dev_max_queue_depth[i];
j = 0;
while ( ((scb = scbq_remove_head(&p->delayed_scbs[i])) != NULL) &&
(j++ < p->scb_data->numscbs) )
{
scbq_insert_tail(&p->waiting_scbs, scb);
}
if (j == p->scb_data->numscbs)
{
printk(INFO_LEAD "timer: Yikes, loop in delayed_scbs list.\n",
p->host_no, 0, i, -1);
scbq_init(&p->delayed_scbs[i]);
scbq_init(&p->waiting_scbs);
}
}
else if ( p->dev_timer_active & (0x01 << i) )
{
if ( p->dev_timer_active & (0x01 << MAX_TARGETS) )
{
if ( time_after_eq(p->dev_timer.expires, p->dev_expires[i]) )
{
p->dev_timer.expires = p->dev_expires[i];
}
}
else
{
p->dev_timer.expires = p->dev_expires[i];
p->dev_timer_active |= (0x01 << MAX_TARGETS);
}
}
}
if ( p->dev_timer_active & (0x01 << MAX_TARGETS) )
{
add_timer(&p->dev_timer);
}
aic7xxx_run_waiting_queues(p);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
DRIVER_UNLOCK
#else
spin_unlock_irqrestore(&io_request_lock, cpu_flags);
#endif
}
static void
aic7xxx_construct_sdtr(struct aic7xxx_host *p, unsigned char period,
unsigned char offset)
{
p->msg_buf[p->msg_index++] = MSG_EXTENDED;
p->msg_buf[p->msg_index++] = MSG_EXT_SDTR_LEN;
p->msg_buf[p->msg_index++] = MSG_EXT_SDTR;
p->msg_buf[p->msg_index++] = period;
p->msg_buf[p->msg_index++] = offset;
p->msg_len += 5;
}
static void
aic7xxx_construct_wdtr(struct aic7xxx_host *p, unsigned char bus_width)
{
p->msg_buf[p->msg_index++] = MSG_EXTENDED;
p->msg_buf[p->msg_index++] = MSG_EXT_WDTR_LEN;
p->msg_buf[p->msg_index++] = MSG_EXT_WDTR;
p->msg_buf[p->msg_index++] = bus_width;
p->msg_len += 4;
}
static void
aic7xxx_calculate_residual (struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
struct aic7xxx_hwscb *hscb;
Scsi_Cmnd *cmd;
int actual, i;
cmd = scb->cmd;
hscb = scb->hscb;
if (((scb->hscb->control & DISCONNECTED) == 0) &&
(scb->flags & SCB_SENSE) == 0)
{
actual = scb->sg_length;
for (i=1; i < hscb->residual_SG_segment_count; i++)
{
actual -= scb->sg_list[scb->sg_count - i].length;
}
actual -= (hscb->residual_data_count[2] << 16) |
(hscb->residual_data_count[1] << 8) |
hscb->residual_data_count[0];
if (actual < cmd->underflow)
{
if (aic7xxx_verbose & VERBOSE_MINOR_ERROR)
printk(INFO_LEAD "Underflow - Wanted %u, %s %u, residual SG "
"count %d.\n", p->host_no, CTL_OF_SCB(scb), cmd->underflow,
(cmd->request.cmd == WRITE) ? "wrote" : "read", actual,
hscb->residual_SG_segment_count);
aic7xxx_error(cmd) = DID_RETRY_COMMAND;
aic7xxx_status(cmd) = hscb->target_status;
}
}
hscb->residual_data_count[2] = 0;
hscb->residual_data_count[1] = 0;
hscb->residual_data_count[0] = 0;
hscb->residual_SG_segment_count = 0;
}
static void
aic7xxx_handle_device_reset(struct aic7xxx_host *p, int target, int channel)
{
unsigned short targ_mask;
unsigned char tindex = target;
tindex |= ((channel & 0x01) << 3);
targ_mask = (0x01 << tindex);
p->needsdtr |= (p->needsdtr_copy & targ_mask);
p->needwdtr |= (p->needwdtr_copy & targ_mask);
p->sdtr_pending &= ~targ_mask;
p->wdtr_pending &= ~targ_mask;
aic_outb(p, 0, TARG_SCSIRATE + tindex);
if (p->features & AHC_ULTRA2)
aic_outb(p, 0, TARG_OFFSET + tindex);
aic7xxx_reset_device(p, target, channel, ALL_LUNS, SCB_LIST_NULL);
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Bus Device Reset delivered.\n", p->host_no, channel,
target, -1);
aic7xxx_run_done_queue(p, FALSE);
}
static void
aic7xxx_handle_seqint(struct aic7xxx_host *p, unsigned char intstat)
{
struct aic7xxx_scb *scb;
unsigned short target_mask;
unsigned char target, lun, tindex;
unsigned char queue_flag = FALSE;
char channel;
target = ((aic_inb(p, SAVED_TCL) >> 4) & 0x0f);
if ( (p->chip & AHC_CHIPID_MASK) == AHC_AIC7770 )
channel = (aic_inb(p, SBLKCTL) & SELBUSB) >> 3;
else
channel = 0;
tindex = target + (channel << 3);
lun = aic_inb(p, SAVED_TCL) & 0x07;
target_mask = (0x01 << tindex);
aic_outb(p, CLRSEQINT, CLRINT);
switch (intstat & SEQINT_MASK)
{
case NO_MATCH:
{
aic_outb(p, aic_inb(p, SCSISEQ) & (ENSELI|ENRSELI|ENAUTOATNP),
SCSISEQ);
printk(WARN_LEAD "No active SCB for reconnecting target - Issuing "
"BUS DEVICE RESET.\n", p->host_no, channel, target, lun);
printk(WARN_LEAD "      SAVED_TCL=0x%x, ARG_1=0x%x, SEQADDR=0x%x\n",
p->host_no, channel, target, lun,
aic_inb(p, SAVED_TCL), aic_inb(p, ARG_1),
(aic_inb(p, SEQADDR1) << 8) | aic_inb(p, SEQADDR0));
}
break;
case SEND_REJECT:
{
if (aic7xxx_verbose & VERBOSE_MINOR_ERROR)
printk(INFO_LEAD "Rejecting unknown message (0x%x) received from "
"target, SEQ_FLAGS=0x%x\n", p->host_no, channel, target, lun,
aic_inb(p, ACCUM), aic_inb(p, SEQ_FLAGS));
}
break;
case NO_IDENT:
{
if (aic7xxx_verbose & (VERBOSE_SEQINT | VERBOSE_RESET_MID))
printk(INFO_LEAD "Target did not send an IDENTIFY message; "
"LASTPHASE 0x%x, SAVED_TCL 0x%x\n", p->host_no, channel, target,
lun, aic_inb(p, LASTPHASE), aic_inb(p, SAVED_TCL));
aic7xxx_reset_channel(p, channel, TRUE);
aic7xxx_run_done_queue(p, FALSE);
}
break;
case BAD_PHASE:
if (aic_inb(p, LASTPHASE) == P_BUSFREE)
{
if (aic7xxx_verbose & VERBOSE_SEQINT)
printk(INFO_LEAD "Missed busfree.\n", p->host_no, channel,
target, lun);
restart_sequencer(p);
}
else
{
if (aic7xxx_verbose & VERBOSE_SEQINT)
printk(INFO_LEAD "Unknown scsi bus phase, continuing\n", p->host_no,
channel, target, lun);
}
break;
case EXTENDED_MSG:
{
p->msg_type = MSG_TYPE_INITIATOR_MSGIN;
p->msg_len = 0;
p->msg_index = 0;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Enabling REQINITs for MSG_IN\n", p->host_no,
channel, target, lun);
#endif
p->flags |= AHC_HANDLING_REQINITS;
aic_outb(p, aic_inb(p, SIMODE1) | ENREQINIT, SIMODE1);
return;
}
case REJECT_MSG:
{
unsigned char scb_index;
unsigned char last_msg;
scb_index = aic_inb(p, SCB_TAG);
scb = p->scb_data->scb_array[scb_index];
last_msg = aic_inb(p, LAST_MSG);
if ( (last_msg == MSG_IDENTIFYFLAG) &&
(scb->tag_action) &&
!(scb->flags & SCB_MSGOUT_BITS) )
{
if (scb->tag_action == MSG_ORDERED_Q_TAG)
{
p->orderedtag &= ~target_mask;
scb->tag_action = MSG_SIMPLE_Q_TAG;
scb->hscb->control &= ~SCB_TAG_TYPE;
scb->hscb->control |= MSG_SIMPLE_Q_TAG;
aic_outb(p, scb->hscb->control, SCB_CONTROL);
aic_outb(p, MSG_IDENTIFYFLAG, MSG_OUT);
aic_outb(p, aic_inb(p, SCSISIGI) | ATNO, SCSISIGO);
}
else if (scb->tag_action == MSG_SIMPLE_Q_TAG)
{
unsigned char i, reset = 0;
struct aic7xxx_scb *scbp;
int old_verbose;
p->tagenable &= ~target_mask;
p->orderedtag &= ~target_mask;
p->dev_max_queue_depth[tindex] =
p->dev_temp_queue_depth[tindex] = 1;
scb->tag_action = 0;
scb->hscb->control &= ~(TAG_ENB | SCB_TAG_TYPE);
aic_outb(p, scb->hscb->control, SCB_CONTROL);
old_verbose = aic7xxx_verbose;
aic7xxx_verbose &= ~(VERBOSE_RESET|VERBOSE_ABORT);
for (i=0; i!=p->scb_data->numscbs; i++)
{
scbp = p->scb_data->scb_array[i];
if ((scbp->flags & SCB_ACTIVE) && (scbp != scb))
{
if (aic7xxx_match_scb(p, scbp, target, channel, lun, i))
{
aic7xxx_reset_device(p, target, channel, lun, i);
reset++;
}
aic7xxx_run_done_queue(p, FALSE);
}
}
aic7xxx_verbose = old_verbose;
aic7xxx_busy_target(p, scb);
printk(INFO_LEAD "Device is refusing tagged commands, using "
"untagged I/O.\n", p->host_no, channel, target, lun);
aic_outb(p, MSG_IDENTIFYFLAG, MSG_OUT);
aic_outb(p, aic_inb(p, SCSISIGI) | ATNO, SCSISIGO);
}
}
else if (scb->flags & SCB_MSGOUT_WDTR)
{
p->needwdtr &= ~target_mask;
p->needwdtr_copy &= ~target_mask;
p->wdtr_pending &= ~target_mask;
scb->flags &= ~SCB_MSGOUT_BITS;
aic7xxx_set_width(p, target, channel, lun, MSG_EXT_WDTR_BUS_8_BIT,
(AHC_TRANS_ACTIVE|AHC_TRANS_GOAL|AHC_TRANS_CUR));
aic7xxx_set_syncrate(p, NULL, target, channel, 0, 0,
AHC_TRANS_ACTIVE|AHC_TRANS_CUR|AHC_TRANS_QUITE);
if ( (p->needsdtr_copy & target_mask) &&
!(p->sdtr_pending & target_mask) )
{
p->sdtr_pending |= target_mask;
scb->flags |= SCB_MSGOUT_SDTR;
aic_outb(p, HOST_MSG, MSG_OUT);
aic_outb(p, aic_inb(p, SCSISIGO) | ATNO, SCSISIGO);
}
}
else if (scb->flags & SCB_MSGOUT_SDTR)
{
p->needsdtr &= ~target_mask;
p->needsdtr_copy &= ~target_mask;
p->sdtr_pending &= ~target_mask;
scb->flags &= ~SCB_MSGOUT_SDTR;
aic7xxx_set_syncrate(p, NULL, target, channel, 0, 0,
(AHC_TRANS_CUR|AHC_TRANS_ACTIVE|AHC_TRANS_GOAL));
}
else if (aic7xxx_verbose & VERBOSE_SEQINT)
{
printk(INFO_LEAD "Received MESSAGE_REJECT for unknown cause.  "
"Ignoring.\n", p->host_no, channel, target, lun);
}
}
break;
case BAD_STATUS:
{
unsigned char scb_index;
struct aic7xxx_hwscb *hscb;
Scsi_Cmnd *cmd;
aic_outb(p, 0, RETURN_1);
scb_index = aic_inb(p, SCB_TAG);
if (scb_index > p->scb_data->numscbs)
{
printk(WARN_LEAD "Invalid SCB during SEQINT 0x%02x, SCB_TAG %d.\n",
p->host_no, channel, target, lun, intstat, scb_index);
break;
}
scb = p->scb_data->scb_array[scb_index];
hscb = scb->hscb;
if (!(scb->flags & SCB_ACTIVE) || (scb->cmd == NULL))
{
printk(WARN_LEAD "Invalid SCB during SEQINT 0x%x, scb %d, flags 0x%x,"
" cmd 0x%lx.\n", p->host_no, channel, target, lun, intstat,
scb_index, scb->flags, (unsigned long) scb->cmd);
}
else
{
cmd = scb->cmd;
hscb->target_status = aic_inb(p, SCB_TARGET_STATUS);
aic7xxx_status(cmd) = hscb->target_status;
cmd->result = hscb->target_status;
switch (status_byte(hscb->target_status))
{
case GOOD:
if (aic7xxx_verbose & VERBOSE_SEQINT)
printk(INFO_LEAD "Interrupted for status of GOOD???\n",
p->host_no, CTL_OF_SCB(scb));
break;
case COMMAND_TERMINATED:
case CHECK_CONDITION:
if ( !(scb->flags & SCB_SENSE) )
{
if ( hscb->residual_SG_segment_count != 0 )
aic7xxx_calculate_residual(p, scb);
memcpy(&scb->sense_cmd[0], &generic_sense[0],
sizeof(generic_sense));
scb->sense_cmd[1] = (cmd->lun << 5);
scb->sense_cmd[4] = sizeof(cmd->sense_buffer);
scb->sg_list[0].address =
cpu_to_le32(VIRT_TO_BUS(&cmd->sense_buffer[0]));
scb->sg_list[0].length =
cpu_to_le32(sizeof(cmd->sense_buffer));
hscb->control = 0;
hscb->target_status = 0;
hscb->SG_list_pointer =
cpu_to_le32(VIRT_TO_BUS(&scb->sg_list[0]));
hscb->data_pointer = scb->sg_list[0].address;
hscb->data_count = scb->sg_list[0].length;
hscb->SCSI_cmd_pointer =
cpu_to_le32(VIRT_TO_BUS(&scb->sense_cmd[0]));
hscb->SCSI_cmd_length = COMMAND_SIZE(scb->sense_cmd[0]);
hscb->residual_SG_segment_count = 0;
hscb->residual_data_count[0] = 0;
hscb->residual_data_count[1] = 0;
hscb->residual_data_count[2] = 0;
scb->sg_count = hscb->SG_segment_count = 1;
scb->sg_length = sizeof(cmd->sense_buffer);
scb->tag_action = 0;
#ifdef AIC7XXX_FAKE_NEGOTIATION_CMDS
if ( (scb->cmd->cmnd[0] != TEST_UNIT_READY) &&
(p->dev_flags[tindex] & DEVICE_SCANNED) &&
!(p->wdtr_pending & target_mask) &&
!(p->sdtr_pending & target_mask) )
{
p->needwdtr |= (p->needwdtr_copy & target_mask);
p->needsdtr |= (p->needsdtr_copy & target_mask);
}
else if ( (scb->cmd == p->dev_wdtr_cmnd[tindex]) ||
(scb->cmd == p->dev_sdtr_cmnd[tindex]) )
{
scb->flags &= ~SCB_MSGOUT_BITS;
if ( (scb->cmd == p->dev_wdtr_cmnd[tindex]) &&
!(p->sdtr_pending & target_mask) &&
(p->needsdtr & target_mask) )
{
p->sdtr_pending |= target_mask;
hscb->control |= MK_MESSAGE;
scb->flags |= SCB_MSGOUT_SDTR;
}
if (cmd->next->cmnd[0] != TEST_UNIT_READY)
{
scb->sg_list[0].address =
cpu_to_le32(VIRT_TO_BUS(&cmd->next->sense_buffer[0]));
hscb->data_pointer = scb->sg_list[0].address;
}
}
#else
if ( (scb->cmd->cmnd[0] != TEST_UNIT_READY) &&
!(scb->flags & SCB_MSGOUT_BITS) &&
(scb->cmd->lun == 0) &&
(p->dev_flags[TARGET_INDEX(scb->cmd)] & DEVICE_SCANNED) )
{
if ( (p->needwdtr_copy & target_mask) &&
!(p->wdtr_pending & target_mask) &&
!(p->sdtr_pending & target_mask) )
{
p->needwdtr |= target_mask;
p->wdtr_pending |= target_mask;
hscb->control |= MK_MESSAGE;
scb->flags |= SCB_MSGOUT_WDTR;
}
if ( p->needsdtr_copy & target_mask )
{
p->needsdtr |= target_mask;
if ( !(p->wdtr_pending & target_mask) &&
!(p->sdtr_pending & target_mask) )
{
p->sdtr_pending |= target_mask;
hscb->control |= MK_MESSAGE;
scb->flags |= SCB_MSGOUT_SDTR;
}
}
}
else
scb->flags &= ~SCB_MSGOUT_BITS;
#endif
scb->flags |= SCB_SENSE;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
{
if (scb->flags & SCB_MSGOUT_BITS)
printk(INFO_LEAD "Requesting SENSE with %s\n", p->host_no,
CTL_OF_SCB(scb), (scb->flags & SCB_MSGOUT_SDTR) ?
"SDTR" : "WDTR");
else
printk(INFO_LEAD "Requesting SENSE, no MSG\n", p->host_no,
CTL_OF_SCB(scb));
}
#endif
aic7xxx_busy_target(p, scb);
aic_outb(p, SEND_SENSE, RETURN_1);
aic7xxx_error(cmd) = DID_OK;
break;
}
aic7xxx_error(cmd) = DID_OK;
scb->flags &= ~SCB_SENSE;
break;
case QUEUE_FULL:
queue_flag = TRUE;
case BUSY:
{
struct aic7xxx_scb *next_scbp, *prev_scbp;
unsigned char active_hscb, next_hscb, prev_hscb, scb_index;
aic7xxx_search_qinfifo(p, target, channel, lun,
SCB_LIST_NULL, 0, TRUE,
&p->delayed_scbs[tindex]);
next_scbp = p->waiting_scbs.head;
while ( next_scbp != NULL )
{
prev_scbp = next_scbp;
next_scbp = next_scbp->q_next;
if ( aic7xxx_match_scb(p, prev_scbp, target, channel, lun,
SCB_LIST_NULL) )
{
scbq_remove(&p->waiting_scbs, prev_scbp);
scbq_insert_tail(&p->delayed_scbs[tindex],
prev_scbp);
}
}
next_scbp = NULL;
active_hscb = aic_inb(p, SCBPTR);
prev_hscb = next_hscb = scb_index = SCB_LIST_NULL;
next_hscb = aic_inb(p, WAITING_SCBH);
while (next_hscb != SCB_LIST_NULL)
{
aic_outb(p, next_hscb, SCBPTR);
scb_index = aic_inb(p, SCB_TAG);
if (scb_index < p->scb_data->numscbs)
{
next_scbp = p->scb_data->scb_array[scb_index];
if (aic7xxx_match_scb(p, next_scbp, target, channel, lun,
SCB_LIST_NULL) )
{
if (next_scbp->flags & SCB_WAITINGQ)
{
p->dev_active_cmds[tindex]++;
p->activescbs--;
scbq_remove(&p->delayed_scbs[tindex], next_scbp);
scbq_remove(&p->waiting_scbs, next_scbp);
}
scbq_insert_head(&p->delayed_scbs[tindex],
next_scbp);
next_scbp->flags |= SCB_WAITINGQ;
p->dev_active_cmds[tindex]--;
p->activescbs--;
next_hscb = aic_inb(p, SCB_NEXT);
aic_outb(p, 0, SCB_CONTROL);
aic_outb(p, SCB_LIST_NULL, SCB_TAG);
aic7xxx_add_curscb_to_free_list(p);
if (prev_hscb == SCB_LIST_NULL)
{
aic_outb(p, aic_inb(p, SCSISEQ) & ~ENSELO, SCSISEQ);
aic_outb(p, CLRSELTIMEO, CLRSINT1);
aic_outb(p, next_hscb, WAITING_SCBH);
}
else
{
aic_outb(p, prev_hscb, SCBPTR);
aic_outb(p, next_hscb, SCB_NEXT);
}
}
else
{
prev_hscb = next_hscb;
next_hscb = aic_inb(p, SCB_NEXT);
}
}
}
aic_outb(p, active_hscb, SCBPTR);
if (scb->flags & SCB_WAITINGQ)
{
scbq_remove(&p->delayed_scbs[tindex], scb);
scbq_remove(&p->waiting_scbs, scb);
p->dev_active_cmds[tindex]++;
p->activescbs++;
}
scbq_insert_head(&p->delayed_scbs[tindex], scb);
p->dev_active_cmds[tindex]--;
p->activescbs--;
scb->flags |= SCB_WAITINGQ | SCB_WAS_BUSY;
if ( !(p->dev_timer_active & (0x01 << tindex)) )
{
p->dev_timer_active |= (0x01 << tindex);
if ( p->dev_active_cmds[tindex] )
{
p->dev_expires[tindex] = jiffies + HZ;
}
else
{
p->dev_expires[tindex] = jiffies + (HZ / 10);
}
if ( !(p->dev_timer_active & (0x01 << MAX_TARGETS)) )
{
p->dev_timer.expires = p->dev_expires[tindex];
p->dev_timer_active |= (0x01 << MAX_TARGETS);
add_timer(&p->dev_timer);
}
else if ( time_after_eq(p->dev_timer.expires,
p->dev_expires[tindex]) )
{
del_timer(&p->dev_timer);
p->dev_timer.expires = p->dev_expires[tindex];
add_timer(&p->dev_timer);
}
}
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose & VERBOSE_MINOR_ERROR)
{
if (queue_flag)
printk(INFO_LEAD "Queue full received; queue depth %d, "
"active %d\n", p->host_no, CTL_OF_SCB(scb),
p->dev_max_queue_depth[tindex],
p->dev_active_cmds[tindex]);
else
printk(INFO_LEAD "Target busy\n", p->host_no, CTL_OF_SCB(scb));
}
#endif
if (queue_flag)
{
p->dev_temp_queue_depth[tindex] =
p->dev_active_cmds[tindex];
if ( p->dev_last_queue_full[tindex] !=
p->dev_active_cmds[tindex] )
{
p->dev_last_queue_full[tindex] =
p->dev_active_cmds[tindex];
p->dev_last_queue_full_count[tindex] = 0;
}
else
{
p->dev_last_queue_full_count[tindex]++;
}
if ( (p->dev_last_queue_full_count[tindex] > 14) &&
(p->dev_active_cmds[tindex] > 4) )
{
if (aic7xxx_verbose & VERBOSE_NEGOTIATION2)
printk(INFO_LEAD "Queue depth reduced to %d\n", p->host_no,
CTL_OF_SCB(scb), p->dev_active_cmds[tindex]);
p->dev_max_queue_depth[tindex] =
p->dev_active_cmds[tindex];
p->dev_last_queue_full[tindex] = 0;
p->dev_last_queue_full_count[tindex] = 0;
}
else
{
p->dev_flags[tindex] |= DEVICE_WAS_BUSY;
}
}
break;
}
default:
if (aic7xxx_verbose & VERBOSE_SEQINT)
printk(INFO_LEAD "Unexpected target status 0x%x.\n", p->host_no,
CTL_OF_SCB(scb), scb->hscb->target_status);
if (!aic7xxx_error(cmd))
{
aic7xxx_error(cmd) = DID_RETRY_COMMAND;
}
break;
}
}
}
break;
case AWAITING_MSG:
{
unsigned char scb_index, msg_out;
scb_index = aic_inb(p, SCB_TAG);
msg_out = aic_inb(p, MSG_OUT);
scb = p->scb_data->scb_array[scb_index];
p->msg_index = p->msg_len = 0;
if ( !(scb->flags & SCB_DEVICE_RESET) &&
(aic_inb(p, MSG_OUT) == MSG_IDENTIFYFLAG) &&
(scb->hscb->control & TAG_ENB) )
{
p->msg_buf[p->msg_index++] = scb->tag_action;
p->msg_buf[p->msg_index++] = scb->hscb->tag;
p->msg_len += 2;
}
if (scb->flags & SCB_DEVICE_RESET)
{
p->msg_buf[p->msg_index++] = MSG_BUS_DEV_RESET;
p->msg_len++;
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Bus device reset mailed.\n",
p->host_no, CTL_OF_SCB(scb));
}
else if (scb->flags & SCB_ABORT)
{
if (scb->tag_action)
{
p->msg_buf[p->msg_index++] = MSG_ABORT_TAG;
}
else
{
p->msg_buf[p->msg_index++] = MSG_ABORT;
}
p->msg_len++;
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "Abort message mailed.\n", p->host_no,
CTL_OF_SCB(scb));
}
else if (scb->flags & SCB_MSGOUT_WDTR)
{
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Sending WDTR message.\n", p->host_no,
CTL_OF_SCB(scb));
#endif
aic7xxx_construct_wdtr(p,
p->transinfo[TARGET_INDEX(scb->cmd)].goal_width);
}
else if (scb->flags & SCB_MSGOUT_SDTR)
{
unsigned int max_sync, period;
if (p->transinfo[tindex].goal_offset)
{
if (p->features & AHC_ULTRA2)
p->transinfo[tindex].goal_offset = MAX_OFFSET_ULTRA2;
else if (p->transinfo[tindex].cur_width == MSG_EXT_WDTR_BUS_16_BIT)
p->transinfo[tindex].goal_offset = MAX_OFFSET_16BIT;
else
p->transinfo[tindex].goal_offset = MAX_OFFSET_8BIT;
}
if (p->features & AHC_ULTRA2)
{
if ( (aic_inb(p, SBLKCTL) & ENAB40) &&
!(aic_inb(p, SSTAT2) & EXP_ACTIVE) )
{
max_sync = AHC_SYNCRATE_ULTRA2;
}
else
{
max_sync = AHC_SYNCRATE_ULTRA;
}
}
else if (p->features & AHC_ULTRA)
{
max_sync = AHC_SYNCRATE_ULTRA;
}
else
{
max_sync = AHC_SYNCRATE_FAST;
}
period = p->transinfo[tindex].goal_period;
aic7xxx_find_syncrate(p, &period, max_sync);
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Sending SDTR %d/%d message.\n", p->host_no,
CTL_OF_SCB(scb),
p->transinfo[tindex].goal_period,
p->transinfo[tindex].goal_offset);
#endif
aic7xxx_construct_sdtr(p, period,
p->transinfo[tindex].goal_offset);
}
else
{
sti();
panic("aic7xxx: AWAITING_MSG for an SCB that does "
"not have a waiting message.\n");
}
scb->flags |= SCB_MSGOUT_SENT;
p->msg_index = 0;
p->msg_type = MSG_TYPE_INITIATOR_MSGOUT;
p->flags |= AHC_HANDLING_REQINITS;
aic_outb(p, aic_inb(p, SIMODE1) | ENREQINIT, SIMODE1);
return;
}
break;
case DATA_OVERRUN:
{
unsigned char scb_index = aic_inb(p, SCB_TAG);
unsigned char lastphase = aic_inb(p, LASTPHASE);
unsigned int i;
scb = (p->scb_data->scb_array[scb_index]);
if ( !(scb->flags & SCB_SENSE) )
{
printk(WARN_LEAD "Data overrun detected in %s phase, tag %d;\n",
p->host_no, CTL_OF_SCB(scb),
(lastphase == P_DATAIN) ? "Data-In" : "Data-Out", scb->hscb->tag);
printk(KERN_WARNING "  %s seen Data Phase. Length=%d, NumSGs=%d.\n",
(aic_inb(p, SEQ_FLAGS) & DPHASE) ? "Have" : "Haven't",
scb->sg_length, scb->sg_count);
for (i = 0; i < scb->sg_count; i++)
{
printk(KERN_WARNING "     sg[%d] - Addr 0x%x : Length %d\n",
i,
le32_to_cpu(scb->sg_list[i].address),
le32_to_cpu(scb->sg_list[i].length) );
}
aic7xxx_error(scb->cmd) = DID_ERROR;
}
else
printk(INFO_LEAD "Data Overrun during SEND_SENSE operation.\n",
p->host_no, CTL_OF_SCB(scb));
}
break;
#if AIC7XXX_NOT_YET
case TRACEPOINT:
{
printk(INFO_LEAD "Tracepoint #1 reached.\n", p->host_no, channel,
target, lun);
}
break;
case TRACEPOINT2:
{
printk(INFO_LEAD "Tracepoint #2 reached.\n", p->host_no, channel,
target, lun);
}
break;
case MSG_BUFFER_BUSY:
printk("aic7xxx: Message buffer busy.\n");
break;
case MSGIN_PHASEMIS:
printk("aic7xxx: Message-in phasemis.\n");
break;
#endif
default:
printk(WARN_LEAD "Unknown SEQINT, INTSTAT 0x%x, SCSISIGI 0x%x.\n",
p->host_no, channel, target, lun, intstat,
aic_inb(p, SCSISIGI));
break;
}
unpause_sequencer(p, TRUE);
}
static int
aic7xxx_parse_msg(struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
int reject, reply, done;
unsigned char target_scsirate, tindex;
unsigned short target_mask;
unsigned char target, channel, lun;
target = scb->cmd->target;
channel = scb->cmd->channel;
lun = scb->cmd->lun;
reply = reject = done = FALSE;
tindex = TARGET_INDEX(scb->cmd);
target_scsirate = aic_inb(p, TARG_SCSIRATE + tindex);
target_mask = (0x01 << tindex);
if (p->msg_buf[0] != MSG_EXTENDED)
{
reject = TRUE;
}
if ( !reject && (p->msg_len > 2) )
{
switch(p->msg_buf[2])
{
case MSG_EXT_SDTR:
{
unsigned int period, offset;
unsigned char maxsync, saved_offset;
struct aic7xxx_syncrate *syncrate;
if (p->msg_buf[1] != MSG_EXT_SDTR_LEN)
{
reject = TRUE;
break;
}
if (p->msg_len < (MSG_EXT_SDTR_LEN + 2))
{
break;
}
period = p->msg_buf[3];
saved_offset = offset = p->msg_buf[4];
if (p->features & AHC_ULTRA2)
{
if ( (aic_inb(p, SBLKCTL) & ENAB40) &&
!(aic_inb(p, SSTAT2) & EXP_ACTIVE) )
{
maxsync = AHC_SYNCRATE_ULTRA2;
}
else
{
maxsync = AHC_SYNCRATE_ULTRA;
}
}
else if (p->features & AHC_ULTRA)
{
maxsync = AHC_SYNCRATE_ULTRA;
}
else
{
maxsync = AHC_SYNCRATE_FAST;
}
if ( (scb->flags & (SCB_MSGOUT_SENT|SCB_MSGOUT_SDTR)) !=
(SCB_MSGOUT_SENT|SCB_MSGOUT_SDTR) )
{
if (!(p->dev_flags[tindex] & DEVICE_SCANNED))
{
p->transinfo[tindex].goal_period =
p->transinfo[tindex].user_period;
p->transinfo[tindex].goal_offset =
p->transinfo[tindex].user_offset;
p->needsdtr_copy |= target_mask;
}
if ( !p->transinfo[tindex].goal_offset )
period = 255;
if ( p->transinfo[tindex].goal_period > period )
period = p->transinfo[tindex].goal_period;
}
syncrate = aic7xxx_find_syncrate(p, &period, maxsync);
aic7xxx_validate_offset(p, syncrate, &offset,
target_scsirate & WIDEXFER);
aic7xxx_set_syncrate(p, syncrate, target, channel, period,
offset, AHC_TRANS_ACTIVE|AHC_TRANS_CUR);
if ( (offset == 0) || (offset != saved_offset) ||
((scb->flags & (SCB_MSGOUT_SENT|SCB_MSGOUT_SDTR)) !=
(SCB_MSGOUT_SENT|SCB_MSGOUT_SDTR) ) )
{
aic7xxx_set_syncrate(p, syncrate, target, channel, period,
offset, AHC_TRANS_GOAL|AHC_TRANS_QUITE);
if ( offset == 0 )
{
p->needsdtr_copy &= ~target_mask;
}
}
p->needsdtr &= ~target_mask;
p->sdtr_pending &= ~target_mask;
if ( ((scb->flags & (SCB_MSGOUT_SENT|SCB_MSGOUT_SDTR)) ==
(SCB_MSGOUT_SENT|SCB_MSGOUT_SDTR)) &&
(offset == saved_offset) )
{
scb->flags &= ~SCB_MSGOUT_BITS;
}
else
{
scb->flags &= ~SCB_MSGOUT_BITS;
scb->flags |= SCB_MSGOUT_SDTR;
aic_outb(p, HOST_MSG, MSG_OUT);
aic_outb(p, aic_inb(p, SCSISIGO) | ATNO, SCSISIGO);
}
done = TRUE;
break;
}
case MSG_EXT_WDTR:
{
unsigned char bus_width;
if (p->msg_buf[1] != MSG_EXT_WDTR_LEN)
{
reject = TRUE;
break;
}
if (p->msg_len < (MSG_EXT_WDTR_LEN + 2))
{
break;
}
bus_width = p->msg_buf[3];
if ( (scb->flags & (SCB_MSGOUT_SENT|SCB_MSGOUT_WDTR)) ==
(SCB_MSGOUT_SENT|SCB_MSGOUT_WDTR) )
{
switch(bus_width)
{
default:
{
reject = TRUE;
if ( (aic7xxx_verbose & VERBOSE_NEGOTIATION2) &&
((p->dev_flags[tindex] & DEVICE_PRINT_WDTR) ||
(aic7xxx_verbose > 0xffff)) )
{
printk(INFO_LEAD "Requesting %d bit transfers, rejecting.\n",
p->host_no, CTL_OF_SCB(scb), 8 * (0x01 << bus_width));
p->dev_flags[tindex] &= ~DEVICE_PRINT_WDTR;
}
}
case MSG_EXT_WDTR_BUS_8_BIT:
{
bus_width = MSG_EXT_WDTR_BUS_8_BIT;
p->needwdtr_copy &= ~target_mask;
break;
}
case MSG_EXT_WDTR_BUS_16_BIT:
{
break;
}
}
scb->flags &= ~SCB_MSGOUT_BITS;
p->wdtr_pending &= ~target_mask;
p->needwdtr &= ~target_mask;
}
else
{
scb->flags &= ~SCB_MSGOUT_BITS;
scb->flags |= SCB_MSGOUT_WDTR;
reply = TRUE;
if ( !(p->dev_flags[tindex] & DEVICE_SCANNED) )
{
p->transinfo[tindex].goal_period =
p->transinfo[tindex].user_period;
p->transinfo[tindex].goal_offset =
p->transinfo[tindex].user_offset;
p->transinfo[tindex].goal_width =
p->transinfo[tindex].user_width;
p->needwdtr_copy |= target_mask;
p->needsdtr_copy |= target_mask;
}
switch(bus_width)
{
default:
{
if ( (p->features & AHC_WIDE) &&
(p->transinfo[tindex].goal_width ==
MSG_EXT_WDTR_BUS_16_BIT) )
{
bus_width = MSG_EXT_WDTR_BUS_16_BIT;
break;
}
}
case MSG_EXT_WDTR_BUS_8_BIT:
{
p->needwdtr_copy &= ~target_mask;
bus_width = MSG_EXT_WDTR_BUS_8_BIT;
aic7xxx_set_width(p, target, channel, lun, bus_width,
AHC_TRANS_GOAL|AHC_TRANS_QUITE);
break;
}
}
p->needwdtr &= ~target_mask;
p->wdtr_pending &= ~target_mask;
aic_outb(p, HOST_MSG, MSG_OUT);
aic_outb(p, aic_inb(p, SCSISIGO) | ATNO, SCSISIGO);
}
aic7xxx_set_width(p, target, channel, lun, bus_width,
AHC_TRANS_ACTIVE|AHC_TRANS_CUR);
aic7xxx_set_syncrate(p, NULL, target, channel, 0, 0,
AHC_TRANS_ACTIVE|AHC_TRANS_CUR|AHC_TRANS_QUITE);
if ( (p->needsdtr_copy & target_mask) &&
!(p->sdtr_pending & target_mask))
{
p->needsdtr |= target_mask;
if ( !reject && !reply )
{
scb->flags &= ~SCB_MSGOUT_WDTR;
if (p->transinfo[tindex].goal_period)
{
p->sdtr_pending |= target_mask;
scb->flags |= SCB_MSGOUT_SDTR;
aic_outb(p, HOST_MSG, MSG_OUT);
aic_outb(p, aic_inb(p, SCSISIGO) | ATNO, SCSISIGO);
}
}
}
done = TRUE;
break;
}
default:
{
reject = TRUE;
break;
}
}
}
if (reject)
{
aic_outb(p, MSG_MESSAGE_REJECT, MSG_OUT);
aic_outb(p, aic_inb(p, SCSISIGO) | ATNO, SCSISIGO);
done = TRUE;
}
return(done);
}
static void
aic7xxx_handle_reqinit(struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
unsigned char lastbyte;
unsigned char phasemis;
int done = FALSE;
switch(p->msg_type)
{
case MSG_TYPE_INITIATOR_MSGOUT:
{
if (p->msg_len == 0)
panic("aic7xxx: REQINIT with no active message!\n");
lastbyte = (p->msg_index == (p->msg_len - 1));
phasemis = ( aic_inb(p, SCSISIGI) & PHASE_MASK) != P_MESGOUT;
if (lastbyte || phasemis)
{
p->msg_len = 0;
p->msg_type = MSG_TYPE_NONE;
aic_outb(p, aic_inb(p, SIMODE1) & ~ENREQINIT, SIMODE1);
aic_outb(p, CLRSCSIINT, CLRINT);
p->flags &= ~AHC_HANDLING_REQINITS;
if (phasemis == 0)
{
aic_outb(p, p->msg_buf[p->msg_index], SINDEX);
aic_outb(p, 0, RETURN_1);
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Completed sending of REQINIT message.\n",
p->host_no, CTL_OF_SCB(scb));
#endif
}
else
{
aic_outb(p, MSGOUT_PHASEMIS, RETURN_1);
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "PHASEMIS while sending REQINIT message.\n",
p->host_no, CTL_OF_SCB(scb));
#endif
}
unpause_sequencer(p, TRUE);
}
else
{
aic_outb(p, CLRREQINIT, CLRSINT1);
aic_outb(p, CLRSCSIINT, CLRINT);
aic_outb(p, p->msg_buf[p->msg_index++], SCSIDATL);
}
break;
}
case MSG_TYPE_INITIATOR_MSGIN:
{
phasemis = ( aic_inb(p, SCSISIGI) & PHASE_MASK ) != P_MESGIN;
if (phasemis == 0)
{
p->msg_len++;
p->msg_buf[p->msg_index] = aic_inb(p, SCSIBUSL);
done = aic7xxx_parse_msg(p, scb);
aic_outb(p, CLRREQINIT, CLRSINT1);
aic_outb(p, CLRSCSIINT, CLRINT);
aic_inb(p, SCSIDATL);
p->msg_index++;
}
if (phasemis || done)
{
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
{
if (phasemis)
printk(INFO_LEAD "PHASEMIS while receiving REQINIT message.\n",
p->host_no, CTL_OF_SCB(scb));
else
printk(INFO_LEAD "Completed receipt of REQINIT message.\n",
p->host_no, CTL_OF_SCB(scb));
}
#endif
p->msg_len = 0;
p->msg_type = MSG_TYPE_NONE;
aic_outb(p, aic_inb(p, SIMODE1) & ~ENREQINIT, SIMODE1);
aic_outb(p, CLRSCSIINT, CLRINT);
p->flags &= ~AHC_HANDLING_REQINITS;
unpause_sequencer(p, TRUE);
}
break;
}
default:
{
panic("aic7xxx: Unknown REQINIT message type.\n");
break;
}
}
}
static void
aic7xxx_handle_scsiint(struct aic7xxx_host *p, unsigned char intstat)
{
unsigned char scb_index;
unsigned char status;
struct aic7xxx_scb *scb;
scb_index = aic_inb(p, SCB_TAG);
status = aic_inb(p, SSTAT1);
if (scb_index < p->scb_data->numscbs)
{
scb = p->scb_data->scb_array[scb_index];
if ((scb->flags & SCB_ACTIVE) == 0)
{
scb = NULL;
}
}
else
{
scb = NULL;
}
if ((status & SCSIRSTI) != 0)
{
int channel;
if ( (p->chip & AHC_CHIPID_MASK) == AHC_AIC7770 )
channel = (aic_inb(p, SBLKCTL) & SELBUSB) >> 3;
else
channel = 0;
if (aic7xxx_verbose & VERBOSE_RESET)
printk(WARN_LEAD "Someone else reset the channel!!\n",
p->host_no, channel, -1, -1);
aic7xxx_reset_channel(p, channel, FALSE);
aic7xxx_run_done_queue(p, FALSE);
scb = NULL;
}
else if ( ((status & BUSFREE) != 0) && ((status & SELTO) == 0) )
{
unsigned char lastphase = aic_inb(p, LASTPHASE);
unsigned char saved_tcl = aic_inb(p, SAVED_TCL);
unsigned char target = (saved_tcl >> 4) & 0x0F;
int channel;
int printerror = TRUE;
if ( (p->chip & AHC_CHIPID_MASK) == AHC_AIC7770 )
channel = (aic_inb(p, SBLKCTL) & SELBUSB) >> 3;
else
channel = 0;
aic_outb(p, aic_inb(p, SCSISEQ) & (ENSELI|ENRSELI|ENAUTOATNP),
SCSISEQ);
if (lastphase == P_MESGOUT)
{
unsigned char message;
message = aic_inb(p, SINDEX);
if ((message == MSG_ABORT) || (message == MSG_ABORT_TAG))
{
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "SCB %d abort delivered.\n", p->host_no,
CTL_OF_SCB(scb), scb->hscb->tag);
aic7xxx_reset_device(p, target, channel, ALL_LUNS,
(message == MSG_ABORT) ? SCB_LIST_NULL : scb->hscb->tag );
aic7xxx_run_done_queue(p, FALSE);
scb = NULL;
printerror = 0;
}
else if (message == MSG_BUS_DEV_RESET)
{
aic7xxx_handle_device_reset(p, target, channel);
scb = NULL;
printerror = 0;
}
}
if (printerror != 0)
{
if (scb != NULL)
{
unsigned char tag;
if ((scb->hscb->control & TAG_ENB) != 0)
{
tag = scb->hscb->tag;
}
else
{
tag = SCB_LIST_NULL;
}
aic7xxx_reset_device(p, target, channel, ALL_LUNS, tag);
aic7xxx_run_done_queue(p, FALSE);
}
printk(INFO_LEAD "Unexpected busfree, LASTPHASE = 0x%x, "
"SEQADDR = 0x%x\n", p->host_no, channel, target, -1, lastphase,
(aic_inb(p, SEQADDR1) << 8) | aic_inb(p, SEQADDR0));
scb = NULL;
}
aic_outb(p, MSG_NOOP, MSG_OUT);
aic_outb(p, aic_inb(p, SIMODE1) & ~(ENBUSFREE|ENREQINIT),
SIMODE1);
p->flags &= ~AHC_HANDLING_REQINITS;
aic_outb(p, CLRBUSFREE, CLRSINT1);
aic_outb(p, CLRSCSIINT, CLRINT);
restart_sequencer(p);
unpause_sequencer(p, TRUE);
}
else if ((status & SELTO) != 0)
{
unsigned char scbptr;
unsigned char nextscb;
Scsi_Cmnd *cmd;
scbptr = aic_inb(p, WAITING_SCBH);
if (scbptr > p->scb_data->maxhscbs)
{
printk(INFO_LEAD "Invalid WAITING_SCBH value %d, improvising.\n",
p->host_no, -1, -1, -1, scbptr);
if (p->scb_data->maxhscbs > 4)
scbptr &= (p->scb_data->maxhscbs - 1);
else
scbptr &= 0x03;
}
aic_outb(p, scbptr, SCBPTR);
scb_index = aic_inb(p, SCB_TAG);
scb = NULL;
if (scb_index < p->scb_data->numscbs)
{
scb = p->scb_data->scb_array[scb_index];
if ((scb->flags & SCB_ACTIVE) == 0)
{
scb = NULL;
}
}
if (scb == NULL)
{
printk(WARN_LEAD "Referenced SCB %d not valid during SELTO.\n",
p->host_no, -1, -1, -1, scb_index);
printk(KERN_WARNING "        SCSISEQ = 0x%x SEQADDR = 0x%x SSTAT0 = 0x%x "
"SSTAT1 = 0x%x\n", aic_inb(p, SCSISEQ),
aic_inb(p, SEQADDR0) | (aic_inb(p, SEQADDR1) << 8),
aic_inb(p, SSTAT0), aic_inb(p, SSTAT1));
if (aic7xxx_panic_on_abort)
aic7xxx_panic_abort(p, NULL);
}
else
{
cmd = scb->cmd;
cmd->result = (DID_TIME_OUT << 16);
aic_outb(p, 0, SCB_CONTROL);
aic_outb(p, MSG_NOOP, MSG_OUT);
nextscb = aic_inb(p, SCB_NEXT);
aic_outb(p, nextscb, WAITING_SCBH);
aic7xxx_add_curscb_to_free_list(p);
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Selection Timeout.\n", p->host_no, CTL_OF_SCB(scb));
#endif
if (scb->flags & SCB_QUEUED_ABORT)
{
cmd->result = 0;
scb = NULL;
}
}
aic_outb(p, 0, SCSISEQ);
aic_outb(p, aic_inb(p, SIMODE1) & ~(ENREQINIT|ENBUSFREE), SIMODE1);
p->flags &= ~AHC_HANDLING_REQINITS;
aic_outb(p, CLRSELTIMEO | CLRBUSFREE, CLRSINT1);
aic_outb(p, CLRSCSIINT, CLRINT);
restart_sequencer(p);
unpause_sequencer(p, TRUE);
}
else if (scb == NULL)
{
printk(WARN_LEAD "aic7xxx_isr - referenced scb not valid "
"during scsiint 0x%x scb(%d)\n"
"      SIMODE0 0x%x, SIMODE1 0x%x, SSTAT0 0x%x, SEQADDR 0x%x\n",
p->host_no, -1, -1, -1, status, scb_index, aic_inb(p, SIMODE0),
aic_inb(p, SIMODE1), aic_inb(p, SSTAT0),
(aic_inb(p, SEQADDR1) << 8) | aic_inb(p, SEQADDR0));
aic_outb(p, status, CLRSINT1);
aic_outb(p, CLRSCSIINT, CLRINT);
unpause_sequencer(p, TRUE);
scb = NULL;
}
else if (status & SCSIPERR)
{
char *phase;
Scsi_Cmnd *cmd;
unsigned char mesg_out = MSG_NOOP;
unsigned char lastphase = aic_inb(p, LASTPHASE);
cmd = scb->cmd;
switch (lastphase)
{
case P_DATAOUT:
phase = "Data-Out";
break;
case P_DATAIN:
phase = "Data-In";
mesg_out = MSG_INITIATOR_DET_ERR;
break;
case P_COMMAND:
phase = "Command";
break;
case P_MESGOUT:
phase = "Message-Out";
break;
case P_STATUS:
phase = "Status";
mesg_out = MSG_INITIATOR_DET_ERR;
break;
case P_MESGIN:
phase = "Message-In";
mesg_out = MSG_PARITY_ERROR;
break;
default:
phase = "unknown";
break;
}
printk(WARN_LEAD "Parity error during %s phase.\n",
p->host_no, CTL_OF_SCB(scb), phase);
if (mesg_out != MSG_NOOP)
{
aic_outb(p, mesg_out, MSG_OUT);
scb = NULL;
}
aic_outb(p, CLRSCSIPERR, CLRSINT1);
aic_outb(p, CLRSCSIINT, CLRINT);
unpause_sequencer(p, TRUE);
}
else if ( (status & REQINIT) &&
(p->flags & AHC_HANDLING_REQINITS) )
{
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Handling REQINIT, SSTAT1=0x%x.\n", p->host_no,
CTL_OF_SCB(scb), aic_inb(p, SSTAT1));
#endif
aic7xxx_handle_reqinit(p, scb);
return;
}
else
{
if (aic7xxx_verbose & VERBOSE_SCSIINT)
printk(INFO_LEAD "Unknown SCSIINT status, SSTAT1(0x%x).\n",
p->host_no, -1, -1, -1, status);
aic_outb(p, status, CLRSINT1);
aic_outb(p, CLRSCSIINT, CLRINT);
unpause_sequencer(p, TRUE);
scb = NULL;
}
if (scb != NULL)
{
aic7xxx_done(p, scb);
}
}
#ifdef AIC7XXX_VERBOSE_DEBUGGING
static void
aic7xxx_check_scbs(struct aic7xxx_host *p, char *buffer)
{
unsigned char saved_scbptr, free_scbh, dis_scbh, wait_scbh, temp;
int i, bogus, lost;
static unsigned char scb_status[AIC7XXX_MAXSCB];
#define SCB_NO_LIST 0
#define SCB_FREE_LIST 1
#define SCB_WAITING_LIST 2
#define SCB_DISCONNECTED_LIST 4
#define SCB_CURRENTLY_ACTIVE 8
bogus = FALSE;
memset(&scb_status[0], 0, sizeof(scb_status));
pause_sequencer(p);
saved_scbptr = aic_inb(p, SCBPTR);
if (saved_scbptr >= p->scb_data->maxhscbs)
{
printk("Bogus SCBPTR %d\n", saved_scbptr);
bogus = TRUE;
}
scb_status[saved_scbptr] = SCB_CURRENTLY_ACTIVE;
free_scbh = aic_inb(p, FREE_SCBH);
if ( (free_scbh != SCB_LIST_NULL) &&
(free_scbh >= p->scb_data->maxhscbs) )
{
printk("Bogus FREE_SCBH %d\n", free_scbh);
bogus = TRUE;
}
else
{
temp = free_scbh;
while( (temp != SCB_LIST_NULL) && (temp < p->scb_data->maxhscbs) )
{
if(scb_status[temp] & 0x07)
{
printk("HSCB %d on multiple lists, status 0x%02x", temp,
scb_status[temp] | SCB_FREE_LIST);
bogus = TRUE;
}
scb_status[temp] |= SCB_FREE_LIST;
aic_outb(p, temp, SCBPTR);
temp = aic_inb(p, SCB_NEXT);
}
}
dis_scbh = aic_inb(p, DISCONNECTED_SCBH);
if ( (dis_scbh != SCB_LIST_NULL) &&
(dis_scbh >= p->scb_data->maxhscbs) )
{
printk("Bogus DISCONNECTED_SCBH %d\n", dis_scbh);
bogus = TRUE;
}
else
{
temp = dis_scbh;
while( (temp != SCB_LIST_NULL) && (temp < p->scb_data->maxhscbs) )
{
if(scb_status[temp] & 0x07)
{
printk("HSCB %d on multiple lists, status 0x%02x", temp,
scb_status[temp] | SCB_DISCONNECTED_LIST);
bogus = TRUE;
}
scb_status[temp] |= SCB_DISCONNECTED_LIST;
aic_outb(p, temp, SCBPTR);
temp = aic_inb(p, SCB_NEXT);
}
}
wait_scbh = aic_inb(p, WAITING_SCBH);
if ( (wait_scbh != SCB_LIST_NULL) &&
(wait_scbh >= p->scb_data->maxhscbs) )
{
printk("Bogus WAITING_SCBH %d\n", wait_scbh);
bogus = TRUE;
}
else
{
temp = wait_scbh;
while( (temp != SCB_LIST_NULL) && (temp < p->scb_data->maxhscbs) )
{
if(scb_status[temp] & 0x07)
{
printk("HSCB %d on multiple lists, status 0x%02x", temp,
scb_status[temp] | SCB_WAITING_LIST);
bogus = TRUE;
}
scb_status[temp] |= SCB_WAITING_LIST;
aic_outb(p, temp, SCBPTR);
temp = aic_inb(p, SCB_NEXT);
}
}
lost=0;
for(i=0; i < p->scb_data->maxhscbs; i++)
{
aic_outb(p, i, SCBPTR);
temp = aic_inb(p, SCB_NEXT);
if ( ((temp != SCB_LIST_NULL) &&
(temp >= p->scb_data->maxhscbs)) )
{
printk("HSCB %d bad, SCB_NEXT invalid(%d).\n", i, temp);
bogus = TRUE;
}
if ( temp == i )
{
printk("HSCB %d bad, SCB_NEXT points to self.\n", i);
bogus = TRUE;
}
temp = aic_inb(p, SCB_PREV);
if ((temp != SCB_LIST_NULL) &&
(temp >= p->scb_data->maxhscbs))
{
printk("HSCB %d bad, SCB_PREV invalid(%d).\n", i, temp);
bogus = TRUE;
}
if (scb_status[i] == 0)
lost++;
if (lost > 1)
{
printk("Too many lost scbs.\n");
bogus=TRUE;
}
}
aic_outb(p, saved_scbptr, SCBPTR);
unpause_sequencer(p, FALSE);
if (bogus)
{
printk("Bogus parameters found in card SCB array structures.\n");
printk("%s\n", buffer);
aic7xxx_panic_abort(p, NULL);
}
return;
}
#endif
static void
aic7xxx_isr(int irq, void *dev_id, struct pt_regs *regs)
{
struct aic7xxx_host *p;
unsigned char intstat;
p = (struct aic7xxx_host *)dev_id;
if (!((intstat = aic_inb(p, INTSTAT)) & INT_PEND))
{
#ifdef CONFIG_PCI
if ( (p->chip & AHC_PCI) && (p->spurious_int > 500) &&
!(p->flags & AHC_HANDLING_REQINITS) )
{
if ( aic_inb(p, ERROR) & PCIERRSTAT )
{
aic7xxx_pci_intr(p);
}
p->spurious_int = 0;
}
else if ( !(p->flags & AHC_HANDLING_REQINITS) )
{
p->spurious_int++;
}
#endif
return;
}
p->spurious_int = 0;
p->isr_count++;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if ( (p->isr_count < 16) && (aic7xxx_verbose > 0xffff) &&
(aic7xxx_panic_on_abort) && (p->flags & AHC_PAGESCBS) )
aic7xxx_check_scbs(p, "Bogus settings at start of interrupt.");
#endif
if (intstat & CMDCMPLT)
{
struct aic7xxx_scb *scb = NULL;
Scsi_Cmnd *cmd;
unsigned char scb_index;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if(aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Command Complete Int.\n", p->host_no, -1, -1, -1);
#endif
aic_outb(p, CLRCMDINT, CLRINT);
while (p->qoutfifo[p->qoutfifonext] != SCB_LIST_NULL)
{
scb_index = p->qoutfifo[p->qoutfifonext];
p->qoutfifo[p->qoutfifonext++] = SCB_LIST_NULL;
if ( scb_index >= p->scb_data->numscbs )
scb = NULL;
else
scb = p->scb_data->scb_array[scb_index];
if (scb == NULL)
{
printk(WARN_LEAD "CMDCMPLT with invalid SCB index %d\n", p->host_no,
-1, -1, -1, scb_index);
continue;
}
else if (!(scb->flags & SCB_ACTIVE) || (scb->cmd == NULL))
{
printk(WARN_LEAD "CMDCMPLT without command for SCB %d, SCB flags "
"0x%x, cmd 0x%lx\n", p->host_no, -1, -1, -1, scb_index, scb->flags,
(unsigned long) scb->cmd);
continue;
}
else if (scb->flags & SCB_QUEUED_ABORT)
{
pause_sequencer(p);
if ( ((aic_inb(p, LASTPHASE) & PHASE_MASK) != P_BUSFREE) &&
(aic_inb(p, SCB_TAG) == scb->hscb->tag) )
{
unpause_sequencer(p, FALSE);
continue;
}
aic7xxx_reset_device(p, scb->cmd->target, scb->cmd->channel,
scb->cmd->lun, scb->hscb->tag);
scb->flags &= ~(SCB_QUEUED_FOR_DONE | SCB_RESET | SCB_ABORT |
SCB_QUEUED_ABORT);
unpause_sequencer(p, FALSE);
}
else if (scb->flags & SCB_ABORT)
{
scb->flags &= ~(SCB_ABORT|SCB_RESET);
}
switch (status_byte(scb->hscb->target_status))
{
case QUEUE_FULL:
case BUSY:
scb->hscb->target_status = 0;
scb->cmd->result = 0;
aic7xxx_error(scb->cmd) = DID_OK;
break;
default:
cmd = scb->cmd;
if (scb->hscb->residual_SG_segment_count != 0)
{
aic7xxx_calculate_residual(p, scb);
}
cmd->result |= (aic7xxx_error(cmd) << 16);
aic7xxx_done(p, scb);
break;
}
}
}
if (intstat & BRKADRINT)
{
int i;
unsigned char errno = aic_inb(p, ERROR);
printk(KERN_ERR "(scsi%d) BRKADRINT error(0x%x):\n", p->host_no, errno);
for (i = 0; i < NUMBER(hard_error); i++)
{
if (errno & hard_error[i].errno)
{
printk(KERN_ERR "  %s\n", hard_error[i].errmesg);
}
}
printk(KERN_ERR "(scsi%d)   SEQADDR=0x%x\n", p->host_no,
(((aic_inb(p, SEQADDR1) << 8) & 0x100) | aic_inb(p, SEQADDR0)));
if (aic7xxx_panic_on_abort)
aic7xxx_panic_abort(p, NULL);
#ifdef CONFIG_PCI
if (errno & PCIERRSTAT)
aic7xxx_pci_intr(p);
#endif
if (errno & (SQPARERR | ILLOPCODE | ILLSADDR))
{
sti();
panic("aic7xxx: unrecoverable BRKADRINT.\n");
}
if (errno & ILLHADDR)
{
printk(KERN_ERR "(scsi%d) BUG! Driver accessed chip without first "
"pausing controller!\n", p->host_no);
}
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (errno & DPARERR)
{
if (aic_inb(p, DMAPARAMS) & DIRECTION)
printk("(scsi%d) while DMAing SCB from host to card.\n", p->host_no);
else
printk("(scsi%d) while DMAing SCB from card to host.\n", p->host_no);
}
#endif
aic_outb(p, CLRPARERR | CLRBRKADRINT, CLRINT);
unpause_sequencer(p, FALSE);
}
if (intstat & SEQINT)
{
aic7xxx_handle_seqint(p, intstat);
}
if (intstat & SCSIINT)
{
aic7xxx_handle_scsiint(p, intstat);
}
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if ( (p->isr_count < 16) && (aic7xxx_verbose > 0xffff) &&
(aic7xxx_panic_on_abort) && (p->flags & AHC_PAGESCBS) )
aic7xxx_check_scbs(p, "Bogus settings at end of interrupt.");
#endif
}
static void
do_aic7xxx_isr(int irq, void *dev_id, struct pt_regs *regs)
{
unsigned long cpu_flags;
struct aic7xxx_host *p;
p = (struct aic7xxx_host *)dev_id;
if(!p)
return;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,95)
spin_lock_irqsave(&io_request_lock, cpu_flags);
if(test_and_set_bit(AHC_IN_ISR_BIT, &p->flags))
{
return;
}
do
{
aic7xxx_isr(irq, dev_id, regs);
} while ( (aic_inb(p, INTSTAT) & INT_PEND) );
aic7xxx_done_cmds_complete(p);
aic7xxx_run_waiting_queues(p);
clear_bit(AHC_IN_ISR_BIT, &p->flags);
spin_unlock_irqrestore(&io_request_lock, cpu_flags);
#else
if(set_bit(AHC_IN_ISR_BIT, (int *)&p->flags))
{
return;
}
DRIVER_LOCK
do
{
aic7xxx_isr(irq, dev_id, regs);
} while ( (aic_inb(p, INTSTAT) & INT_PEND) );
DRIVER_UNLOCK
aic7xxx_done_cmds_complete(p);
aic7xxx_run_waiting_queues(p);
clear_bit(AHC_IN_ISR_BIT, (int *)&p->flags);
#endif
}
static void
aic7xxx_device_queue_depth(struct aic7xxx_host *p, Scsi_Device *device)
{
int default_depth = 3;
unsigned char tindex;
unsigned short target_mask;
tindex = device->id | (device->channel << 3);
target_mask = (1 << tindex);
device->queue_depth = default_depth;
p->dev_temp_queue_depth[tindex] = 1;
p->dev_max_queue_depth[tindex] = 1;
p->tagenable &= ~target_mask;
if (device->tagged_supported)
{
int tag_enabled = TRUE;
default_depth = AIC7XXX_CMDS_PER_DEVICE;
if (!(p->discenable & target_mask))
{
if (aic7xxx_verbose & VERBOSE_NEGOTIATION2)
printk(INFO_LEAD "Disconnection disabled, unable to "
"enable tagged queueing.\n",
p->host_no, device->channel, device->id, device->lun);
}
else
{
if (p->instance >= NUMBER(aic7xxx_tag_info))
{
static int print_warning = TRUE;
if(print_warning)
{
printk(KERN_INFO "aic7xxx: WARNING, insufficient tag_info instances for"
" installed controllers.\n");
printk(KERN_INFO "aic7xxx: Please update the aic7xxx_tag_info array in"
" the aic7xxx.c source file.\n");
print_warning = FALSE;
}
device->queue_depth = default_depth;
}
else
{
if (aic7xxx_tag_info[p->instance].tag_commands[tindex] == 255)
{
tag_enabled = FALSE;
device->queue_depth = 3;
}
else if (aic7xxx_tag_info[p->instance].tag_commands[tindex] == 0)
{
device->queue_depth = default_depth;
}
else
{
device->queue_depth =
aic7xxx_tag_info[p->instance].tag_commands[tindex];
}
}
if ((device->tagged_queue == 0) && tag_enabled)
{
if (aic7xxx_verbose & VERBOSE_NEGOTIATION2)
{
printk(INFO_LEAD "Enabled tagged queuing, queue depth %d.\n",
p->host_no, device->channel, device->id,
device->lun, device->queue_depth);
}
p->dev_max_queue_depth[tindex] = device->queue_depth;
p->dev_temp_queue_depth[tindex] = device->queue_depth;
p->tagenable |= target_mask;
p->orderedtag |= target_mask;
device->tagged_queue = 1;
device->current_tag = SCB_LIST_NULL;
}
}
}
}
static void
aic7xxx_select_queue_depth(struct Scsi_Host *host,
Scsi_Device *scsi_devs)
{
Scsi_Device *device;
struct aic7xxx_host *p = (struct aic7xxx_host *) host->hostdata;
int scbnum;
scbnum = 0;
for (device = scsi_devs; device != NULL; device = device->next)
{
if (device->host == host)
{
aic7xxx_device_queue_depth(p, device);
scbnum += device->queue_depth;
}
}
while (scbnum > p->scb_data->numscbs)
{
if ( aic7xxx_allocate_scb(p) == 0 )
return;
}
}
#if defined(__i386__) || defined(__alpha__)
static int
aic7xxx_probe(int slot, int base, ahc_flag_type *flags)
{
int i;
unsigned char buf[4];
static struct {
int n;
unsigned char signature[sizeof(buf)];
ahc_chip type;
int bios_disabled;
} AIC7xxx[] = {
{ 4, { 0x04, 0x90, 0x77, 0x70 },
AHC_AIC7770|AHC_EISA, FALSE },
{ 4, { 0x04, 0x90, 0x77, 0x71 },
AHC_AIC7770|AHC_EISA, FALSE },
{ 4, { 0x04, 0x90, 0x77, 0x56 },
AHC_AIC7770|AHC_VL, FALSE },
{ 4, { 0x04, 0x90, 0x77, 0x57 },
AHC_AIC7770|AHC_VL, TRUE }
};
for (i = 0; i < sizeof(buf); i++)
{
outb(0x80 + i, base);
buf[i] = inb(base + i);
}
for (i = 0; i < NUMBER(AIC7xxx); i++)
{
if (!memcmp(buf, AIC7xxx[i].signature, AIC7xxx[i].n))
{
if (inb(base + 4) & 1)
{
if (AIC7xxx[i].bios_disabled)
{
*flags |= AHC_USEDEFAULTS;
}
else
{
*flags |= AHC_BIOS_ENABLED;
}
return (i);
}
printk("aic7xxx: <Adaptec 7770 SCSI Host Adapter> "
"disabled at slot %d, ignored.\n", slot);
}
}
return (-1);
}
#endif
static int
read_284x_seeprom(struct aic7xxx_host *p, struct seeprom_config *sc)
{
int i = 0, k = 0;
unsigned char temp;
unsigned short checksum = 0;
unsigned short *seeprom = (unsigned short *) sc;
struct seeprom_cmd {
unsigned char len;
unsigned char bits[3];
};
struct seeprom_cmd seeprom_read = {3, {1, 1, 0}};
#define CLOCK_PULSE(p) \
while ((aic_inb(p, STATUS_2840) & EEPROM_TF) == 0) \
{ \
; \
} \
(void) aic_inb(p, SEECTL_2840);
for (k = 0; k < (sizeof(*sc) / 2); k++)
{
aic_outb(p, CK_2840 | CS_2840, SEECTL_2840);
CLOCK_PULSE(p);
for (i = 0; i < seeprom_read.len; i++)
{
temp = CS_2840 | seeprom_read.bits[i];
aic_outb(p, temp, SEECTL_2840);
CLOCK_PULSE(p);
temp = temp ^ CK_2840;
aic_outb(p, temp, SEECTL_2840);
CLOCK_PULSE(p);
}
for (i = 5; i >= 0; i--)
{
temp = k;
temp = (temp >> i) & 1;
temp = CS_2840 | temp;
aic_outb(p, temp, SEECTL_2840);
CLOCK_PULSE(p);
temp = temp ^ CK_2840;
aic_outb(p, temp, SEECTL_2840);
CLOCK_PULSE(p);
}
for (i = 0; i <= 16; i++)
{
temp = CS_2840;
aic_outb(p, temp, SEECTL_2840);
CLOCK_PULSE(p);
temp = temp ^ CK_2840;
seeprom[k] = (seeprom[k] << 1) | (aic_inb(p, STATUS_2840) & DI_2840);
aic_outb(p, temp, SEECTL_2840);
CLOCK_PULSE(p);
}
if (k < (sizeof(*sc) / 2) - 1)
{
checksum = checksum + seeprom[k];
}
aic_outb(p, 0, SEECTL_2840);
CLOCK_PULSE(p);
aic_outb(p, CK_2840, SEECTL_2840);
CLOCK_PULSE(p);
aic_outb(p, 0, SEECTL_2840);
CLOCK_PULSE(p);
}
#if 0
printk("Computed checksum 0x%x, checksum read 0x%x\n", checksum, sc->checksum);
printk("Serial EEPROM:");
for (k = 0; k < (sizeof(*sc) / 2); k++)
{
if (((k % 8) == 0) && (k != 0))
{
printk("\n              ");
}
printk(" 0x%x", seeprom[k]);
}
printk("\n");
#endif
if (checksum != sc->checksum)
{
printk("aic7xxx: SEEPROM checksum error, ignoring SEEPROM settings.\n");
return (0);
}
return (1);
#undef CLOCK_PULSE
}
static int
acquire_seeprom(struct aic7xxx_host *p)
{
int wait;
aic_outb(p, SEEMS, SEECTL);
wait = 1000;
while ((wait > 0) && ((aic_inb(p, SEECTL) & SEERDY) == 0))
{
wait--;
mdelay(1);
}
if ((aic_inb(p, SEECTL) & SEERDY) == 0)
{
aic_outb(p, 0, SEECTL);
return (0);
}
return (1);
}
static void
release_seeprom(struct aic7xxx_host *p)
{
aic_outb(p, 0, SEECTL);
}
static int
read_seeprom(struct aic7xxx_host *p, int offset,
unsigned short *scarray, unsigned int len, seeprom_chip_type chip)
{
int i = 0, k;
unsigned char temp;
unsigned short checksum = 0;
struct seeprom_cmd {
unsigned char len;
unsigned char bits[3];
};
struct seeprom_cmd seeprom_read = {3, {1, 1, 0}};
#define CLOCK_PULSE(p) \
while ((aic_inb(p, SEECTL) & SEERDY) == 0) \
{ \
; \
}
if (acquire_seeprom(p) == 0)
{
return (0);
}
for (k = 0; k < len; k++)
{
aic_outb(p, SEEMS | SEECK | SEECS, SEECTL);
CLOCK_PULSE(p);
for (i = 0; i < seeprom_read.len; i++)
{
temp = SEEMS | SEECS | (seeprom_read.bits[i] << 1);
aic_outb(p, temp, SEECTL);
CLOCK_PULSE(p);
temp = temp ^ SEECK;
aic_outb(p, temp, SEECTL);
CLOCK_PULSE(p);
}
for (i = ((int) chip - 1); i >= 0; i--)
{
temp = k + offset;
temp = (temp >> i) & 1;
temp = SEEMS | SEECS | (temp << 1);
aic_outb(p, temp, SEECTL);
CLOCK_PULSE(p);
temp = temp ^ SEECK;
aic_outb(p, temp, SEECTL);
CLOCK_PULSE(p);
}
for (i = 0; i <= 16; i++)
{
temp = SEEMS | SEECS;
aic_outb(p, temp, SEECTL);
CLOCK_PULSE(p);
temp = temp ^ SEECK;
scarray[k] = (scarray[k] << 1) | (aic_inb(p, SEECTL) & SEEDI);
aic_outb(p, temp, SEECTL);
CLOCK_PULSE(p);
}
if (k < (len - 1))
{
checksum = checksum + scarray[k];
}
aic_outb(p, SEEMS, SEECTL);
CLOCK_PULSE(p);
aic_outb(p, SEEMS | SEECK, SEECTL);
CLOCK_PULSE(p);
aic_outb(p, SEEMS, SEECTL);
CLOCK_PULSE(p);
}
release_seeprom(p);
#if 0
printk("Computed checksum 0x%x, checksum read 0x%x\n",
checksum, scarray[len - 1]);
printk("Serial EEPROM:");
for (k = 0; k < len; k++)
{
if (((k % 8) == 0) && (k != 0))
{
printk("\n              ");
}
printk(" 0x%x", scarray[k]);
}
printk("\n");
#endif
if ( (checksum != scarray[len - 1]) || (checksum == 0) )
{
return (0);
}
return (1);
#undef CLOCK_PULSE
}
static void
write_brdctl(struct aic7xxx_host *p, unsigned char value)
{
unsigned char brdctl;
if ((p->chip & AHC_CHIPID_MASK) == AHC_AIC7895)
{
brdctl = BRDSTB;
if (p->flags & AHC_CHNLB)
brdctl |= BRDCS;
}
else if (p->features & AHC_ULTRA2)
brdctl = 0;
else
brdctl = BRDSTB | BRDCS;
aic_outb(p, brdctl, BRDCTL);
udelay(1);
brdctl |= value;
aic_outb(p, brdctl, BRDCTL);
udelay(1);
if (p->features & AHC_ULTRA2)
brdctl |= BRDSTB_ULTRA2;
else
brdctl &= ~BRDSTB;
aic_outb(p, brdctl, BRDCTL);
udelay(1);
if (p->features & AHC_ULTRA2)
brdctl = 0;
else
brdctl &= ~BRDCS;
aic_outb(p, brdctl, BRDCTL);
udelay(1);
}
static unsigned char
read_brdctl(struct aic7xxx_host *p)
{
unsigned char brdctl, value;
if ((p->chip & AHC_CHIPID_MASK) == AHC_AIC7895)
{
brdctl = BRDRW;
if (p->flags & AHC_CHNLB)
brdctl |= BRDCS;
}
else if (p->features & AHC_ULTRA2)
brdctl = BRDRW_ULTRA2;
else
brdctl = BRDRW | BRDCS;
aic_outb(p, brdctl, BRDCTL);
udelay(1);
value = aic_inb(p, BRDCTL);
aic_outb(p, 0, BRDCTL);
udelay(1);
return (value);
}
static void
aic785x_cable_detect(struct aic7xxx_host *p, int *int_50,
int *ext_present, int *eeprom)
{
unsigned char brdctl;
aic_outb(p, BRDRW | BRDCS, BRDCTL);
udelay(1);
aic_outb(p, 0, BRDCTL);
udelay(1);
brdctl = aic_inb(p, BRDCTL);
udelay(1);
*int_50 = !(brdctl & BRDDAT5);
*ext_present = !(brdctl & BRDDAT6);
*eeprom = (aic_inb(p, SPIOCAP) & EEPROM);
}
static void
aic787x_cable_detect(struct aic7xxx_host *p, int *int_50, int *int_68,
int *ext_present, int *eeprom)
{
unsigned char brdctl;
write_brdctl(p, 0);
brdctl = read_brdctl(p);
*int_50 = !(brdctl & BRDDAT6);
*int_68 = !(brdctl & BRDDAT7);
write_brdctl(p, BRDDAT5);
brdctl = read_brdctl(p);
*ext_present = !(brdctl & BRDDAT6);
*eeprom = !(brdctl & BRDDAT7);
}
static void
aic7xxx_ultra2_term_detect(struct aic7xxx_host *p, int *enableSE_low,
int *enableSE_high, int *enableLVD_low,
int *enableLVD_high, int *eprom_present)
{
unsigned char brdctl;
brdctl = read_brdctl(p);
*eprom_present = (brdctl & BRDDAT7);
*enableSE_high = (brdctl & BRDDAT6);
*enableSE_low = (brdctl & BRDDAT5);
*enableLVD_high = (brdctl & BRDDAT4);
*enableLVD_low = (brdctl & BRDDAT3);
}
static void
configure_termination(struct aic7xxx_host *p)
{
int internal50_present = 0;
int internal68_present = 0;
int external_present = 0;
int eprom_present = 0;
int enableSE_low = 0;
int enableSE_high = 0;
int enableLVD_low = 0;
int enableLVD_high = 0;
unsigned char brddat = 0;
unsigned char max_target = 0;
unsigned char sxfrctl1 = aic_inb(p, SXFRCTL1);
if (acquire_seeprom(p))
{
if (p->features & (AHC_WIDE|AHC_TWIN))
max_target = 16;
else
max_target = 8;
aic_outb(p, SEEMS | SEECS, SEECTL);
sxfrctl1 &= ~STPWEN;
if ( (p->adapter_control & CFAUTOTERM) ||
(p->features & AHC_ULTRA2) )
{
if ( (p->adapter_control & CFAUTOTERM) && !(p->features & AHC_ULTRA2) )
{
printk(KERN_INFO "(scsi%d) Warning - detected auto-termination\n",
p->host_no);
printk(KERN_INFO "(scsi%d) Please verify driver detected settings are "
"correct.\n", p->host_no);
printk(KERN_INFO "(scsi%d) If not, then please properly set the device "
"termination\n", p->host_no);
printk(KERN_INFO "(scsi%d) in the Adaptec SCSI BIOS by hitting CTRL-A "
"when prompted\n", p->host_no);
printk(KERN_INFO "(scsi%d) during machine bootup.\n", p->host_no);
}
if (p->features & AHC_ULTRA2)
{
if (aic7xxx_override_term == -1)
aic7xxx_ultra2_term_detect(p, &enableSE_low, &enableSE_high,
&enableLVD_low, &enableLVD_high,
&eprom_present);
if (!(p->adapter_control & CFSEAUTOTERM))
{
enableSE_low = (p->adapter_control & CFSTERM);
enableSE_high = (p->adapter_control & CFWSTERM);
}
if (!(p->adapter_control & CFAUTOTERM))
{
enableLVD_low = enableLVD_high = (p->adapter_control & CFLVDSTERM);
}
internal50_present = 0;
internal68_present = 1;
external_present = 1;
}
else if ( (p->chip & AHC_CHIPID_MASK) >= AHC_AIC7870 )
{
aic787x_cable_detect(p, &internal50_present, &internal68_present,
&external_present, &eprom_present);
}
else
{
aic785x_cable_detect(p, &internal50_present, &external_present,
&eprom_present);
}
if (max_target <= 8)
internal68_present = 0;
if ( !(p->features & AHC_ULTRA2) )
{
if (max_target > 8)
{
printk(KERN_INFO "(scsi%d) Cables present (Int-50 %s, Int-68 %s, "
"Ext-68 %s)\n", p->host_no,
internal50_present ? "YES" : "NO",
internal68_present ? "YES" : "NO",
external_present ? "YES" : "NO");
}
else
{
printk(KERN_INFO "(scsi%d) Cables present (Int-50 %s, Ext-50 %s)\n",
p->host_no,
internal50_present ? "YES" : "NO",
external_present ? "YES" : "NO");
}
}
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(KERN_INFO "(scsi%d) EEPROM %s present.\n", p->host_no,
eprom_present ? "is" : "is not");
if ( !(p->features & AHC_ULTRA2) &&
(internal50_present && internal68_present && external_present) )
{
printk(KERN_INFO "(scsi%d) Illegal cable configuration!!  Only two\n",
p->host_no);
printk(KERN_INFO "(scsi%d) connectors on the SCSI controller may be "
"in use at a time!\n", p->host_no);
internal50_present = external_present = 0;
enableSE_high = enableSE_low = 1;
}
if ((max_target > 8) &&
((external_present == 0) || (internal68_present == 0) ||
(enableSE_high != 0)))
{
brddat |= BRDDAT6;
p->flags |= AHC_TERM_ENB_SE_HIGH;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(KERN_INFO "(scsi%d) SE High byte termination Enabled\n",
p->host_no);
}
if ( (((internal50_present ? 1 : 0) +
(internal68_present ? 1 : 0) +
(external_present ? 1 : 0)) <= 1) ||
(enableSE_low != 0) )
{
if (p->features & AHC_ULTRA2)
brddat |= BRDDAT5;
else
sxfrctl1 |= STPWEN;
p->flags |= AHC_TERM_ENB_SE_LOW;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(KERN_INFO "(scsi%d) SE Low byte termination Enabled\n",
p->host_no);
}
if (enableLVD_low != 0)
{
sxfrctl1 |= STPWEN;
p->flags |= AHC_TERM_ENB_LVD;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(KERN_INFO "(scsi%d) LVD Low byte termination Enabled\n",
p->host_no);
}
if (enableLVD_high != 0)
{
brddat |= BRDDAT4;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(KERN_INFO "(scsi%d) LVD High byte termination Enabled\n",
p->host_no);
}
}
else
{
if (p->adapter_control & CFSTERM)
{
if (p->features & AHC_ULTRA2)
brddat |= BRDDAT5;
else
sxfrctl1 |= STPWEN;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(KERN_INFO "(scsi%d) SE Low byte termination Enabled\n",
p->host_no);
}
if (p->adapter_control & CFWSTERM)
{
brddat |= BRDDAT6;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(KERN_INFO "(scsi%d) SE High byte termination Enabled\n",
p->host_no);
}
}
write_brdctl(p, brddat);
release_seeprom(p);
aic_outb(p, sxfrctl1, SXFRCTL1);
}
}
static void
detect_maxscb(struct aic7xxx_host *p)
{
int i;
if (p->scb_data->maxhscbs == 0)
{
aic_outb(p, 0, FREE_SCBH);
for (i = 0; i < AIC7XXX_MAXSCB; i++)
{
aic_outb(p, i, SCBPTR);
aic_outb(p, i, SCB_CONTROL);
if (aic_inb(p, SCB_CONTROL) != i)
break;
aic_outb(p, 0, SCBPTR);
if (aic_inb(p, SCB_CONTROL) != 0)
break;
aic_outb(p, i, SCBPTR);
aic_outb(p, 0, SCB_CONTROL);
aic_outb(p, i + 1, SCB_NEXT);
aic_outb(p, i - 1, SCB_PREV);
aic_outb(p, SCB_LIST_NULL, SCB_TAG);
aic_outb(p, SCB_LIST_NULL, SCB_BUSYTARGETS);
aic_outb(p, SCB_LIST_NULL, SCB_BUSYTARGETS+1);
aic_outb(p, SCB_LIST_NULL, SCB_BUSYTARGETS+2);
aic_outb(p, SCB_LIST_NULL, SCB_BUSYTARGETS+3);
}
aic_outb(p, i - 1, SCBPTR);
aic_outb(p, SCB_LIST_NULL, SCB_NEXT);
aic_outb(p, 0, SCBPTR);
aic_outb(p, 0, SCB_CONTROL);
p->scb_data->maxhscbs = i;
if ( i == AIC7XXX_MAXSCB )
p->flags &= ~AHC_PAGESCBS;
}
}
static int
aic7xxx_register(Scsi_Host_Template *template, struct aic7xxx_host *p,
int reset_delay)
{
int i, result;
int max_targets;
int found = 1;
unsigned char term, scsi_conf;
struct Scsi_Host *host;
request_region(p->base, MAXREG - MINREG, "aic7xxx");
host = p->host;
p->scb_data->maxscbs = AIC7XXX_MAXSCB;
host->can_queue = AIC7XXX_MAXSCB;
host->cmd_per_lun = 3;
host->sg_tablesize = AIC7XXX_MAX_SG;
host->select_queue_depths = aic7xxx_select_queue_depth;
host->this_id = p->scsi_id;
host->io_port = p->base;
host->n_io_port = 0xFF;
host->base = (unsigned char *) p->mbase;
host->irq = p->irq;
if (p->features & AHC_WIDE)
{
host->max_id = 16;
}
if (p->features & AHC_TWIN)
{
host->max_channel = 1;
}
p->host = host;
p->host_no = host->host_no;
host->unique_id = p->instance;
p->isr_count = 0;
p->next = NULL;
p->completeq.head = NULL;
p->completeq.tail = NULL;
scbq_init(&p->scb_data->free_scbs);
scbq_init(&p->waiting_scbs);
init_timer(&p->dev_timer);
p->dev_timer.data = (unsigned long)p;
p->dev_timer.function = (void *)aic7xxx_timer;
p->dev_timer_active = 0;
for (i = 0; i < NUMBER(p->untagged_scbs); i++)
{
p->untagged_scbs[i] = SCB_LIST_NULL;
p->qinfifo[i] = SCB_LIST_NULL;
p->qoutfifo[i] = SCB_LIST_NULL;
}
p->qinfifonext = 0;
p->qoutfifonext = 0;
for (i = 0; i < MAX_TARGETS; i++)
{
p->dev_commands_sent[i] = 0;
p->dev_flags[i] = 0;
p->dev_active_cmds[i] = 0;
p->dev_last_queue_full[i] = 0;
p->dev_last_queue_full_count[i] = 0;
p->dev_max_queue_depth[i] = 1;
p->dev_temp_queue_depth[i] = 1;
p->dev_expires[i] = 0;
scbq_init(&p->delayed_scbs[i]);
}
printk(KERN_INFO "(scsi%d) <%s> found at ", p->host_no,
board_names[p->board_name_index]);
switch(p->chip)
{
case (AHC_AIC7770|AHC_EISA):
printk("EISA slot %d\n", p->pci_device_fn);
break;
case (AHC_AIC7770|AHC_VL):
printk("VLB slot %d\n", p->pci_device_fn);
break;
default:
printk("PCI %d/%d\n", PCI_SLOT(p->pci_device_fn),
PCI_FUNC(p->pci_device_fn));
break;
}
if (p->features & AHC_TWIN)
{
printk(KERN_INFO "(scsi%d) Twin Channel, A SCSI ID %d, B SCSI ID %d, ",
p->host_no, p->scsi_id, p->scsi_id_b);
}
else
{
char *channel;
channel = "";
if ((p->flags & AHC_MULTI_CHANNEL) != 0)
{
channel = " A";
if ( (p->flags & (AHC_CHNLB|AHC_CHNLC)) != 0 )
{
channel = (p->flags & AHC_CHNLB) ? " B" : " C";
}
}
if (p->features & AHC_WIDE)
{
printk(KERN_INFO "(scsi%d) Wide ", p->host_no);
}
else
{
printk(KERN_INFO "(scsi%d) Narrow ", p->host_no);
}
printk("Channel%s, SCSI ID=%d, ", channel, p->scsi_id);
}
aic_outb(p, 0, SEQ_FLAGS);
detect_maxscb(p);
printk("%d/%d SCBs\n", p->scb_data->maxhscbs, p->scb_data->maxscbs);
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk(KERN_INFO "(scsi%d) BIOS %sabled, IO Port 0x%lx, IRQ %d\n",
p->host_no, (p->flags & AHC_BIOS_ENABLED) ? "en" : "dis",
p->base, p->irq);
printk(KERN_INFO "(scsi%d) IO Memory at 0x%lx, MMAP Memory at 0x%lx\n",
p->host_no, p->mbase, (unsigned long)p->maddr);
}
#ifdef CONFIG_PCI
if (aic7xxx_stpwlev != -1)
{
if ( (p->chip & ~AHC_CHIPID_MASK) == AHC_PCI)
{
unsigned char devconfig;
#if LINUX_KERNEL_VERSION > KERNEL_VERSION(2,1,92)
pci_read_config_byte(p->pdev, DEVCONFIG, &devconfig);
#else
pcibios_read_config_byte(p->pci_bus, p->pci_device_fn,
DEVCONFIG, &devconfig);
#endif
if ( (aic7xxx_stpwlev >> p->instance) & 0x01 )
{
devconfig |= 0x02;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk("(scsi%d) Force setting STPWLEV bit\n", p->host_no);
}
else
{
devconfig &= ~0x02;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk("(scsi%d) Force clearing STPWLEV bit\n", p->host_no);
}
#if LINUX_KERNEL_VERSION > KERNEL_VERSION(2,1,92)
pci_write_config_byte(p->pdev, DEVCONFIG, devconfig);
#else
pcibios_write_config_byte(p->pci_bus, p->pci_device_fn,
DEVCONFIG, devconfig);
#endif
}
}
#endif
if (aic7xxx_override_term != -1)
{
if ( (p->chip & ~AHC_CHIPID_MASK) == AHC_PCI)
{
unsigned char term_override;
term_override = ( (aic7xxx_override_term >> (p->instance * 4)) & 0x0f);
p->adapter_control &=
~(CFSTERM|CFWSTERM|CFLVDSTERM|CFAUTOTERM|CFSEAUTOTERM);
if ( (p->features & AHC_ULTRA2) && (term_override & 0x0c) )
{
p->adapter_control |= CFLVDSTERM;
}
if (term_override & 0x02)
{
p->adapter_control |= CFWSTERM;
}
if (term_override & 0x01)
{
p->adapter_control |= CFSTERM;
}
}
}
if ( (p->flags & AHC_SEEPROM_FOUND) || (aic7xxx_override_term != -1) )
{
if (p->features & AHC_SPIOCAP)
{
if ( aic_inb(p, SPIOCAP) & SSPIOCPS )
configure_termination(p);
}
else if ((p->chip & AHC_CHIPID_MASK) >= AHC_AIC7870)
{
configure_termination(p);
}
}
aic7xxx_clear_intstat(p);
if (p->features & AHC_TWIN)
{
aic_outb(p, aic_inb(p, SBLKCTL) | SELBUSB, SBLKCTL);
term = ((p->flags & AHC_TERM_ENB_B) != 0) ? STPWEN : 0;
aic_outb(p, p->scsi_id_b, SCSIID);
scsi_conf = aic_inb(p, SCSICONF + 1);
aic_outb(p, DFON | SPIOEN, SXFRCTL0);
aic_outb(p, (scsi_conf & ENSPCHK) | STIMESEL | term |
ENSTIMER | ACTNEGEN, SXFRCTL1);
aic_outb(p, 0, SIMODE0);
aic_outb(p, ENSELTIMO | ENSCSIRST | ENSCSIPERR, SIMODE1);
aic_outb(p, 0, SCSIRATE);
aic_outb(p, aic_inb(p, SBLKCTL) & ~SELBUSB, SBLKCTL);
}
term = ((p->flags & AHC_TERM_ENB_SE_LOW) != 0) ? STPWEN : 0;
if (p->features & AHC_ULTRA2)
aic_outb(p, p->scsi_id, SCSIID_ULTRA2);
else
aic_outb(p, p->scsi_id, SCSIID);
scsi_conf = aic_inb(p, SCSICONF);
aic_outb(p, DFON | SPIOEN, SXFRCTL0);
aic_outb(p, (scsi_conf & ENSPCHK) | STIMESEL | term |
ENSTIMER | ACTNEGEN, SXFRCTL1);
aic_outb(p, 0, SIMODE0);
aic_outb(p, ENSELTIMO | ENSCSIRST | ENSCSIPERR, SIMODE1);
aic_outb(p, 0, SCSIRATE);
if ( p->features & AHC_ULTRA2)
aic_outb(p, 0, SCSIOFFSET);
if ((p->features & (AHC_TWIN|AHC_WIDE)) == 0)
{
max_targets = 8;
}
else
{
max_targets = 16;
}
if (!(aic7xxx_no_reset))
{
for (i = 0; i < max_targets; i++)
{
aic_outb(p, 0, TARG_SCSIRATE + i);
if (p->features & AHC_ULTRA2)
{
aic_outb(p, 0, TARG_OFFSET + i);
}
p->transinfo[i].cur_offset = 0;
p->transinfo[i].cur_period = 0;
p->transinfo[i].cur_width = MSG_EXT_WDTR_BUS_8_BIT;
}
aic_outb(p, 0, ULTRA_ENB);
aic_outb(p, 0, ULTRA_ENB + 1);
p->ultraenb = 0;
}
{
size_t array_size;
unsigned int hscb_physaddr;
unsigned long temp;
array_size = p->scb_data->maxscbs * sizeof(struct aic7xxx_hwscb);
if (p->scb_data->hscbs == NULL)
{
p->scb_data->hscbs = kmalloc(array_size + 0x1f, GFP_ATOMIC);
}
if (p->scb_data->hscbs == NULL)
{
printk("(scsi%d) Unable to allocate hardware SCB array; "
"failing detection.\n", p->host_no);
p->irq = 0;
return(0);
}
p->scb_data->hscb_kmalloc_ptr = p->scb_data->hscbs;
temp = (unsigned long)p->scb_data->hscbs;
temp += 0x1f;
temp &= ~0x1f;
p->scb_data->hscbs = (struct aic7xxx_hwscb *)temp;
memset(p->scb_data->hscbs, 0, array_size);
hscb_physaddr = VIRT_TO_BUS(p->scb_data->hscbs);
aic_outb(p, hscb_physaddr & 0xFF, HSCB_ADDR);
aic_outb(p, (hscb_physaddr >> 8) & 0xFF, HSCB_ADDR + 1);
aic_outb(p, (hscb_physaddr >> 16) & 0xFF, HSCB_ADDR + 2);
aic_outb(p, (hscb_physaddr >> 24) & 0xFF, HSCB_ADDR + 3);
hscb_physaddr = VIRT_TO_BUS(&p->untagged_scbs[0]);
aic_outb(p, hscb_physaddr & 0xFF, SCBID_ADDR);
aic_outb(p, (hscb_physaddr >> 8) & 0xFF, SCBID_ADDR + 1);
aic_outb(p, (hscb_physaddr >> 16) & 0xFF, SCBID_ADDR + 2);
aic_outb(p, (hscb_physaddr >> 24) & 0xFF, SCBID_ADDR + 3);
}
aic_outb(p, 0, QINPOS);
aic_outb(p, 0, KERNEL_QINPOS);
aic_outb(p, 0, QOUTPOS);
if(p->features & AHC_QUEUE_REGS)
{
aic_outb(p, SCB_QSIZE_256, QOFF_CTLSTA);
aic_outb(p, 0, SDSCB_QOFF);
aic_outb(p, 0, SNSCB_QOFF);
aic_outb(p, 0, HNSCB_QOFF);
}
aic_outb(p, SCB_LIST_NULL, WAITING_SCBH);
aic_outb(p, SCB_LIST_NULL, DISCONNECTED_SCBH);
aic_outb(p, MSG_NOOP, MSG_OUT);
aic_outb(p, MSG_NOOP, LAST_MSG);
aic_outb(p, 0, TMODE_CMDADDR);
aic_outb(p, 0, TMODE_CMDADDR + 1);
aic_outb(p, 0, TMODE_CMDADDR + 2);
aic_outb(p, 0, TMODE_CMDADDR + 3);
aic_outb(p, 0, TMODE_CMDADDR_NEXT);
p->next = first_aic7xxx;
first_aic7xxx = p;
aic7xxx_clear_intstat(p);
aic7xxx_allocate_scb(p);
aic7xxx_loadseq(p);
aic_outb(p, aic_inb(p, SBLKCTL) & ~AUTOFLUSHDIS, SBLKCTL);
if ( (p->chip & AHC_CHIPID_MASK) == AHC_AIC7770 )
{
aic_outb(p, ENABLE, BCTL);
}
if ( !(aic7xxx_no_reset) )
{
if (p->features & AHC_TWIN)
{
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(KERN_INFO "(scsi%d) Resetting channel B\n", p->host_no);
aic_outb(p, aic_inb(p, SBLKCTL) | SELBUSB, SBLKCTL);
aic7xxx_reset_current_bus(p);
aic_outb(p, aic_inb(p, SBLKCTL) & ~SELBUSB, SBLKCTL);
}
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
char *channel = "";
if (p->flags & AHC_MULTI_CHANNEL)
{
channel = " A";
if (p->flags & (AHC_CHNLB|AHC_CHNLC))
channel = (p->flags & AHC_CHNLB) ? " B" : " C";
}
printk(KERN_INFO "(scsi%d) Resetting channel%s\n", p->host_no, channel);
}
if (p->features & AHC_ULTRA2)
mdelay(250);
aic7xxx_reset_current_bus(p);
if (!reset_delay)
aic7xxx_delay(AIC7XXX_RESET_DELAY);
}
else
{
if (!reset_delay)
{
printk(KERN_INFO "(scsi%d) Not resetting SCSI bus.  Note: Don't use "
"the no_reset\n", p->host_no);
printk(KERN_INFO "(scsi%d) option unless you have a verifiable need "
"for it.\n", p->host_no);
printk(KERN_INFO "(scsi%d) The no_reset option is known to break some "
"systems,\n", p->host_no);
printk(KERN_INFO "(scsi%d) and is not supported by the driver author\n",
p->host_no);
aic7xxx_delay(AIC7XXX_RESET_DELAY);
}
}
if (!(p->chip & AHC_PCI))
{
result = (request_irq(p->irq, do_aic7xxx_isr, 0, "aic7xxx", p));
}
else
{
result = (request_irq(p->irq, do_aic7xxx_isr, SA_SHIRQ,
"aic7xxx", p));
if (result < 0)
{
result = (request_irq(p->irq, do_aic7xxx_isr, SA_INTERRUPT | SA_SHIRQ,
"aic7xxx", p));
}
}
if (result < 0)
{
printk(KERN_WARNING "(scsi%d) Couldn't register IRQ %d, ignoring "
"controller.\n", p->host_no, p->irq);
p->irq = 0;
return (0);
}
unpause_sequencer(p, TRUE);
return (found);
}
int
aic7xxx_chip_reset(struct aic7xxx_host *p)
{
unsigned char sblkctl;
int wait;
aic_outb(p, PAUSE | CHIPRST, HCNTRL);
wait = 1000;
while (--wait && !(aic_inb(p, HCNTRL) & CHIPRSTACK))
{
mdelay(1);
}
pause_sequencer(p);
sblkctl = aic_inb(p, SBLKCTL) & (SELBUSB|SELWIDE);
if (p->chip & AHC_PCI)
sblkctl &= ~SELBUSB;
switch( sblkctl )
{
case 0:
break;
case 2:
p->features |= AHC_WIDE;
break;
case 8:
p->features |= AHC_TWIN;
p->flags |= AHC_MULTI_CHANNEL;
break;
default:
printk(KERN_WARNING "aic7xxx: Unsupported adapter type %d, ignoring.\n",
aic_inb(p, SBLKCTL) & 0x0a);
return(-1);
}
return(0);
}
static struct aic7xxx_host *
aic7xxx_alloc(Scsi_Host_Template *sht, struct aic7xxx_host *temp)
{
struct aic7xxx_host *p = NULL;
struct Scsi_Host *host;
int i;
host = scsi_register(sht, sizeof(struct aic7xxx_host));
if (host != NULL)
{
p = (struct aic7xxx_host *) host->hostdata;
memset(p, 0, sizeof(struct aic7xxx_host));
*p = *temp;
p->host = host;
p->scb_data = kmalloc(sizeof(scb_data_type), GFP_ATOMIC);
if (p->scb_data != NULL)
{
memset(p->scb_data, 0, sizeof(scb_data_type));
scbq_init (&p->scb_data->free_scbs);
}
else
{
release_region(p->base, MAXREG - MINREG);
scsi_unregister(host);
return(NULL);
}
p->host_no = host->host_no;
p->tagenable = 0;
p->orderedtag = 0;
for (i=0; i<MAX_TARGETS; i++)
{
p->transinfo[i].goal_period = 0;
p->transinfo[i].goal_offset = 0;
p->transinfo[i].goal_width = MSG_EXT_WDTR_BUS_8_BIT;
}
DRIVER_LOCK_INIT
}
return (p);
}
static void
aic7xxx_free(struct aic7xxx_host *p)
{
int i;
if (p->scb_data != NULL)
{
if (p->scb_data->hscbs != NULL)
{
kfree(p->scb_data->hscb_kmalloc_ptr);
p->scb_data->hscbs = p->scb_data->hscb_kmalloc_ptr = NULL;
}
for (i = 0; i < p->scb_data->numscbs; i++)
{
if (p->scb_data->scb_array[i]->kmalloc_ptr != NULL)
kfree(p->scb_data->scb_array[i]->kmalloc_ptr);
p->scb_data->scb_array[i] = NULL;
}
kfree(p->scb_data);
}
for (i = 0; i < MAX_TARGETS; i++)
{
if(p->dev_wdtr_cmnd[i])
kfree(p->dev_wdtr_cmnd[i]);
if(p->dev_sdtr_cmnd[i])
kfree(p->dev_sdtr_cmnd[i]);
}
}
static void
aic7xxx_load_seeprom(struct aic7xxx_host *p, unsigned char *sxfrctl1)
{
int have_seeprom = 0;
int i, max_targets, mask;
unsigned char scsirate, scsi_conf;
unsigned short scarray[128];
struct seeprom_config *sc = (struct seeprom_config *) scarray;
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk(KERN_INFO "aic7xxx: Loading serial EEPROM...");
}
switch (p->chip)
{
case (AHC_AIC7770|AHC_EISA):
if (aic_inb(p, SCSICONF) & TERM_ENB)
p->flags |= AHC_TERM_ENB_A;
if ( (p->features & AHC_TWIN) && (aic_inb(p, SCSICONF + 1) & TERM_ENB) )
p->flags |= AHC_TERM_ENB_B;
aic_outb(p, 0, DISC_DSB);
aic_outb(p, 0, DISC_DSB + 1);
break;
case (AHC_AIC7770|AHC_VL):
have_seeprom = read_284x_seeprom(p, (struct seeprom_config *) scarray);
break;
default:
have_seeprom = read_seeprom(p, (p->flags & (AHC_CHNLB|AHC_CHNLC)),
scarray, p->sc_size, p->sc_type);
if (!have_seeprom)
{
if(p->sc_type == C46)
have_seeprom = read_seeprom(p, (p->flags & (AHC_CHNLB|AHC_CHNLC)),
scarray, p->sc_size, C56_66);
else
have_seeprom = read_seeprom(p, (p->flags & (AHC_CHNLB|AHC_CHNLC)),
scarray, p->sc_size, C46);
}
if (!have_seeprom)
{
p->sc_size = 128;
have_seeprom = read_seeprom(p, (p->flags & (AHC_CHNLB|AHC_CHNLC)),
scarray, p->sc_size, p->sc_type);
if (!have_seeprom)
{
if(p->sc_type == C46)
have_seeprom = read_seeprom(p, (p->flags & (AHC_CHNLB|AHC_CHNLC)),
scarray, p->sc_size, C56_66);
else
have_seeprom = read_seeprom(p, (p->flags & (AHC_CHNLB|AHC_CHNLC)),
scarray, p->sc_size, C46);
}
}
break;
}
if (!have_seeprom)
{
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk("\naic7xxx: No SEEPROM available.\n");
}
p->flags |= AHC_NEWEEPROM_FMT;
if (aic_inb(p, SCSISEQ) == 0)
{
p->flags |= AHC_USEDEFAULTS;
p->flags &= ~AHC_BIOS_ENABLED;
p->scsi_id = p->scsi_id_b = 7;
*sxfrctl1 |= STPWEN;
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk("aic7xxx: Using default values.\n");
}
}
else if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk("aic7xxx: Using leftover BIOS values.\n");
}
if ( ((p->chip & ~AHC_CHIPID_MASK) == AHC_PCI) && (*sxfrctl1 & STPWEN) )
{
p->flags |= AHC_TERM_ENB_SE_LOW | AHC_TERM_ENB_SE_HIGH;
sc->adapter_control &= ~CFAUTOTERM;
sc->adapter_control |= CFSTERM | CFWSTERM | CFLVDSTERM;
}
if (aic7xxx_extended)
p->flags |= (AHC_EXTEND_TRANS_A | AHC_EXTEND_TRANS_B);
else
p->flags &= ~(AHC_EXTEND_TRANS_A | AHC_EXTEND_TRANS_B);
}
else
{
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk("done\n");
}
p->flags |= AHC_SEEPROM_FOUND;
*sxfrctl1 = 0;
p->scsi_id = (sc->brtime_id & CFSCSIID);
if ((p->chip & AHC_CHIPID_MASK) == AHC_AIC7770)
{
if (sc->bios_control & CF284XEXTEND)
p->flags |= AHC_EXTEND_TRANS_A;
if (sc->adapter_control & CF284XSTERM)
{
*sxfrctl1 |= STPWEN;
p->flags |= AHC_TERM_ENB_SE_LOW | AHC_TERM_ENB_SE_HIGH;
}
}
else
{
if (sc->bios_control & CFEXTEND)
p->flags |= AHC_EXTEND_TRANS_A;
if (sc->bios_control & CFBIOSEN)
p->flags |= AHC_BIOS_ENABLED;
else
p->flags &= ~AHC_BIOS_ENABLED;
if (sc->adapter_control & CFSTERM)
{
*sxfrctl1 |= STPWEN;
p->flags |= AHC_TERM_ENB_SE_LOW | AHC_TERM_ENB_SE_HIGH;
}
}
p->sc = *sc;
}
p->discenable = 0;
max_targets = MIN(sc->max_targets & CFMAXTARG,
((p->features & (AHC_TWIN | AHC_WIDE)) ? 16 : 8));
if (have_seeprom)
{
for (i = 0; i < max_targets; i++)
{
if( ((p->features & AHC_ULTRA) &&
!(sc->adapter_control & CFULTRAEN) &&
(sc->device_flags[i] & CFSYNCHISULTRA)) ||
(sc->device_flags[i] & CFNEWULTRAFORMAT) )
{
p->flags |= AHC_NEWEEPROM_FMT;
break;
}
}
}
for (i = 0; i < max_targets; i++)
{
mask = (0x01 << i);
if (!have_seeprom)
{
if (aic_inb(p, SCSISEQ) != 0)
{
p->discenable =
~(aic_inb(p, DISC_DSB) | (aic_inb(p, DISC_DSB + 1) << 8) );
p->ultraenb =
(aic_inb(p, ULTRA_ENB) | (aic_inb(p, ULTRA_ENB + 1) << 8) );
sc->device_flags[i] = (p->discenable & mask) ? CFDISC : 0;
if (aic_inb(p, TARG_SCSIRATE + i) & WIDEXFER)
sc->device_flags[i] |= CFWIDEB;
if (p->features & AHC_ULTRA2)
{
if (aic_inb(p, TARG_OFFSET + i))
{
sc->device_flags[i] |= CFSYNCH;
sc->device_flags[i] |= (aic_inb(p, TARG_SCSIRATE + i) & 0x07);
if ( (aic_inb(p, TARG_SCSIRATE + i) & 0x18) == 0x18 )
sc->device_flags[i] |= CFSYNCHISULTRA;
}
}
else
{
if (aic_inb(p, TARG_SCSIRATE + i) & ~WIDEXFER)
{
sc->device_flags[i] |= CFSYNCH;
if (p->features & AHC_ULTRA)
sc->device_flags[i] |= ((p->ultraenb & mask) ?
CFSYNCHISULTRA : 0);
}
}
}
else
{
sc->device_flags[i] = CFDISC;
if (p->features & AHC_WIDE)
sc->device_flags[i] |= CFWIDEB;
if (p->features & AHC_ULTRA2)
sc->device_flags[i] |= 3;
else if (p->features & AHC_ULTRA)
sc->device_flags[i] |= CFSYNCHISULTRA;
sc->device_flags[i] |= CFSYNCH;
aic_outb(p, 0, TARG_SCSIRATE + i);
if (p->features & AHC_ULTRA2)
aic_outb(p, 0, TARG_OFFSET + i);
}
}
if (sc->device_flags[i] & CFDISC)
{
p->discenable |= mask;
}
if (p->flags & AHC_NEWEEPROM_FMT)
{
if ( (sc->device_flags[i] & CFNEWULTRAFORMAT) &&
!(p->features & AHC_ULTRA2) )
{
if ((sc->device_flags[i] & (CFXFER)) == 0x03)
{
sc->device_flags[i] &= ~CFXFER;
sc->device_flags[i] |= CFSYNCHISULTRA;
}
}
if (sc->device_flags[i] & CFSYNCHISULTRA)
{
p->ultraenb |= mask;
}
}
else if (sc->adapter_control & CFULTRAEN)
{
p->ultraenb |= mask;
}
if ( (sc->device_flags[i] & CFSYNCH) == 0)
{
sc->device_flags[i] &= ~CFXFER;
p->ultraenb &= ~mask;
p->transinfo[i].user_offset = 0;
p->transinfo[i].user_period = 0;
p->transinfo[i].cur_offset = 0;
p->transinfo[i].cur_period = 0;
p->needsdtr_copy &= ~mask;
}
else
{
if (p->features & AHC_ULTRA2)
{
p->transinfo[i].user_offset = MAX_OFFSET_ULTRA2;
p->transinfo[i].cur_offset = aic_inb(p, TARG_OFFSET + i);
scsirate = (sc->device_flags[i] & CFXFER) |
((p->ultraenb & mask) ? 0x18 : 0x10);
p->transinfo[i].user_period = aic7xxx_find_period(p, scsirate,
AHC_SYNCRATE_ULTRA2);
p->transinfo[i].cur_period = aic7xxx_find_period(p,
aic_inb(p, TARG_SCSIRATE + i),
AHC_SYNCRATE_ULTRA2);
}
else
{
scsirate = (sc->device_flags[i] & CFXFER) << 4;
if (sc->device_flags[i] & CFWIDEB)
p->transinfo[i].user_offset = MAX_OFFSET_16BIT;
else
p->transinfo[i].user_offset = MAX_OFFSET_8BIT;
if (p->features & AHC_ULTRA)
{
short ultraenb;
ultraenb = aic_inb(p, ULTRA_ENB) |
(aic_inb(p, ULTRA_ENB + 1) << 8);
p->transinfo[i].user_period = aic7xxx_find_period(p,
scsirate,
(p->ultraenb & mask) ?
AHC_SYNCRATE_ULTRA :
AHC_SYNCRATE_FAST);
p->transinfo[i].cur_period = aic7xxx_find_period(p,
aic_inb(p, TARG_SCSIRATE + i),
(ultraenb & mask) ?
AHC_SYNCRATE_ULTRA :
AHC_SYNCRATE_FAST);
}
else
p->transinfo[i].user_period = aic7xxx_find_period(p,
scsirate, AHC_SYNCRATE_FAST);
}
p->needsdtr_copy |= mask;
}
if ( (sc->device_flags[i] & CFWIDEB) && (p->features & AHC_WIDE) )
{
p->transinfo[i].user_width = MSG_EXT_WDTR_BUS_16_BIT;
p->needwdtr_copy |= mask;
}
else
{
p->transinfo[i].user_width = MSG_EXT_WDTR_BUS_8_BIT;
p->needwdtr_copy &= ~mask;
}
p->transinfo[i].cur_width =
(aic_inb(p, TARG_SCSIRATE + i) & WIDEXFER) ?
MSG_EXT_WDTR_BUS_16_BIT : MSG_EXT_WDTR_BUS_8_BIT;
}
aic_outb(p, ~(p->discenable & 0xFF), DISC_DSB);
aic_outb(p, ~((p->discenable >> 8) & 0xFF), DISC_DSB + 1);
p->needwdtr = p->needwdtr_copy;
p->needsdtr = p->needsdtr_copy;
p->wdtr_pending = p->sdtr_pending = 0;
if (p->features & AHC_ULTRA)
p->ultraenb = aic_inb(p, ULTRA_ENB) | (aic_inb(p, ULTRA_ENB + 1) << 8);
scsi_conf = (p->scsi_id & HSCSIID);
if(have_seeprom)
{
p->adapter_control = sc->adapter_control;
p->bios_control = sc->bios_control;
switch (p->chip & AHC_CHIPID_MASK)
{
case AHC_AIC7895:
case AHC_AIC7896:
if (p->adapter_control & CFBPRIMARY)
p->flags |= AHC_CHANNEL_B_PRIMARY;
default:
break;
}
if (sc->adapter_control & CFSPARITY)
scsi_conf |= ENSPCHK;
}
else
{
scsi_conf |= ENSPCHK | RESET_SCSI;
}
if ( (p->chip & ~AHC_CHIPID_MASK) == AHC_PCI )
{
aic_outb(p, scsi_conf, SCSICONF);
aic_outb(p, p->scsi_id, SCSICONF + 1);
}
}
int
aic7xxx_detect(Scsi_Host_Template *template)
{
struct aic7xxx_host *temp_p = NULL;
struct aic7xxx_host *current_p = NULL;
struct aic7xxx_host *list_p = NULL;
int found = 0;
#if defined(__i386__) || defined(__alpha__)
ahc_flag_type flags = 0;
int type;
#endif
unsigned char sxfrctl1;
#if defined(__i386__) || defined(__alpha__)
unsigned char hcntrl, hostconf;
unsigned int slot, base;
#endif
#ifdef MODULE
if(aic7xxx)
aic7xxx_setup(aic7xxx, NULL);
if(dummy_buffer[0] != 'P')
printk(KERN_WARNING "aic7xxx: Please read the file /usr/src/linux/drivers"
"/scsi/README.aic7xxx\n"
"aic7xxx: to see the proper way to specify options to the aic7xxx "
"module\n"
"aic7xxx: Specifically, don't use any commas when passing arguments to\n"
"aic7xxx: insmod or else it might trash certain memory areas.\n");
#endif
template->proc_dir = &proc_scsi_aic7xxx;
template->sg_tablesize = AIC7XXX_MAX_SG;
#if defined(__i386__) || defined(__alpha__)
slot = MINSLOT;
while ( (slot <= MAXSLOT) && !(aic7xxx_no_probe) )
{
base = SLOTBASE(slot) + MINREG;
if (check_region(base, MAXREG - MINREG))
{
slot++;
continue;
}
flags = 0;
type = aic7xxx_probe(slot, base + AHC_HID0, &flags);
if (type == -1)
{
slot++;
continue;
}
temp_p = kmalloc(sizeof(struct aic7xxx_host), GFP_ATOMIC);
if (temp_p == NULL)
{
printk(KERN_WARNING "aic7xxx: Unable to allocate device space.\n");
slot++;
continue;
}
if (aic7xxx_irq_trigger == 1)
hcntrl = IRQMS;
else if (aic7xxx_irq_trigger == 0)
hcntrl = 0;
else
hcntrl = inb(base + HCNTRL) & IRQMS;
memset(temp_p, 0, sizeof(struct aic7xxx_host));
temp_p->unpause = hcntrl | INTEN;
temp_p->pause = hcntrl | PAUSE | INTEN;
temp_p->base = base;
temp_p->mbase = 0;
temp_p->maddr = 0;
temp_p->pci_bus = 0;
temp_p->pci_device_fn = slot;
aic_outb(temp_p, hcntrl | PAUSE, HCNTRL);
while( (aic_inb(temp_p, HCNTRL) & PAUSE) == 0 ) ;
if (aic7xxx_chip_reset(temp_p) == -1)
temp_p->irq = 0;
else
temp_p->irq = aic_inb(temp_p, INTDEF) & 0x0F;
temp_p->flags |= AHC_PAGESCBS;
switch (temp_p->irq)
{
case 9:
case 10:
case 11:
case 12:
case 14:
case 15:
break;
default:
printk(KERN_WARNING "aic7xxx: Host adapter uses unsupported IRQ "
"level %d, ignoring.\n", temp_p->irq);
kfree(temp_p);
slot++;
continue;
}
if (list_p == NULL)
{
list_p = current_p = temp_p;
}
else
{
current_p = list_p;
while (current_p->next != NULL)
current_p = current_p->next;
current_p->next = temp_p;
}
switch (type)
{
case 0:
temp_p->board_name_index = 2;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk("aic7xxx: <%s> at EISA %d\n",
board_names[2], slot);
case 1:
{
temp_p->chip = AHC_AIC7770 | AHC_EISA;
temp_p->features |= AHC_AIC7770_FE;
temp_p->bios_control = aic_inb(temp_p, HA_274_BIOSCTRL);
if (temp_p->board_name_index == 0)
{
temp_p->board_name_index = 3;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk("aic7xxx: <%s> at EISA %d\n",
board_names[3], slot);
}
if (temp_p->bios_control & CHANNEL_B_PRIMARY)
{
temp_p->flags |= AHC_CHANNEL_B_PRIMARY;
}
if ((temp_p->bios_control & BIOSMODE) == BIOSDISABLED)
{
temp_p->flags &= ~AHC_BIOS_ENABLED;
}
else
{
temp_p->flags &= ~AHC_USEDEFAULTS;
temp_p->flags |= AHC_BIOS_ENABLED;
if ( (temp_p->bios_control & 0x20) == 0 )
{
temp_p->bios_address = 0xcc000;
temp_p->bios_address += (0x4000 * (temp_p->bios_control & 0x07));
}
else
{
temp_p->bios_address = 0xd0000;
temp_p->bios_address += (0x8000 * (temp_p->bios_control & 0x06));
}
}
temp_p->adapter_control = aic_inb(temp_p, SCSICONF) << 8;
temp_p->adapter_control |= aic_inb(temp_p, SCSICONF + 1);
if (temp_p->features & AHC_WIDE)
{
temp_p->scsi_id = temp_p->adapter_control & HWSCSIID;
temp_p->scsi_id_b = temp_p->scsi_id;
}
else
{
temp_p->scsi_id = (temp_p->adapter_control >> 8) & HSCSIID;
temp_p->scsi_id_b = temp_p->adapter_control & HSCSIID;
}
aic7xxx_load_seeprom(temp_p, &sxfrctl1);
break;
}
case 2:
case 3:
temp_p->chip = AHC_AIC7770 | AHC_VL;
temp_p->features |= AHC_AIC7770_FE;
if (type == 2)
temp_p->flags |= AHC_BIOS_ENABLED;
else
temp_p->flags &= ~AHC_BIOS_ENABLED;
if (aic_inb(temp_p, SCSICONF) & TERM_ENB)
sxfrctl1 = STPWEN;
aic7xxx_load_seeprom(temp_p, &sxfrctl1);
temp_p->board_name_index = 4;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk("aic7xxx: <%s> at VLB %d\n",
board_names[2], slot);
switch( aic_inb(temp_p, STATUS_2840) & BIOS_SEL )
{
case 0x00:
temp_p->bios_address = 0xe0000;
break;
case 0x20:
temp_p->bios_address = 0xc8000;
break;
case 0x40:
temp_p->bios_address = 0xd0000;
break;
case 0x60:
temp_p->bios_address = 0xd8000;
break;
default:
break;
}
break;
default:
break;
}
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk(KERN_INFO "aic7xxx: BIOS %sabled, IO Port 0x%lx, IRQ %d (%s)\n",
(temp_p->flags & AHC_USEDEFAULTS) ? "dis" : "en", temp_p->base,
temp_p->irq,
(temp_p->pause & IRQMS) ? "level sensitive" : "edge triggered");
printk(KERN_INFO "aic7xxx: Extended translation %sabled.\n",
(temp_p->flags & AHC_EXTEND_TRANS_A) ? "en" : "dis");
}
hostconf = aic_inb(temp_p, HOSTCONF);
aic_outb(temp_p, hostconf & DFTHRSH, BUSSPD);
aic_outb(temp_p, (hostconf << 2) & BOFF, BUSTIME);
slot++;
found++;
}
#endif
#ifdef CONFIG_PCI
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
if (pci_present())
#else
if (pcibios_present())
#endif
{
struct
{
unsigned short vendor_id;
unsigned short device_id;
ahc_chip chip;
ahc_flag_type flags;
ahc_feature features;
int board_name_index;
unsigned short seeprom_size;
unsigned short seeprom_type;
} const aic_pdevs[] = {
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7810, AHC_NONE,
AHC_FNONE, AHC_FENONE, 1,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7850, AHC_AIC7850,
AHC_PAGESCBS, AHC_AIC7850_FE, 5,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7855, AHC_AIC7850,
AHC_PAGESCBS, AHC_AIC7850_FE, 6,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7821, AHC_AIC7860,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7860_FE, 7,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_3860, AHC_AIC7860,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7860_FE, 7,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7860, AHC_AIC7860,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7860_FE, 7,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7861, AHC_AIC7860,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7860_FE, 8,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7870, AHC_AIC7870,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7870_FE, 9,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7871, AHC_AIC7870,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7870_FE, 10,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7872, AHC_AIC7870,
AHC_PAGESCBS | AHC_BIOS_ENABLED | AHC_MULTI_CHANNEL,
AHC_AIC7870_FE, 11,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7873, AHC_AIC7870,
AHC_PAGESCBS | AHC_BIOS_ENABLED | AHC_MULTI_CHANNEL,
AHC_AIC7870_FE, 12,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7874, AHC_AIC7870,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7870_FE, 13,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7880, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7880_FE, 14,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7881, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7880_FE, 15,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7882, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED | AHC_MULTI_CHANNEL,
AHC_AIC7880_FE, 16,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7883, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED | AHC_MULTI_CHANNEL,
AHC_AIC7880_FE, 17,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7884, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7880_FE, 18,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7885, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7880_FE, 18,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7886, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7880_FE, 18,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7887, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7880_FE, 18,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7888, AHC_AIC7880,
AHC_PAGESCBS | AHC_BIOS_ENABLED, AHC_AIC7880_FE, 18,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_7895, AHC_AIC7895,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED | AHC_MULTI_CHANNEL,
AHC_AIC7895_FE, 19,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7890, AHC_AIC7890,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7890_FE, 20,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7890B, AHC_AIC7890,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7890_FE, 20,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_2930U2, AHC_AIC7890,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7890_FE, 21,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_2940U2, AHC_AIC7890,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7890_FE, 22,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7896, AHC_AIC7896,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED | AHC_MULTI_CHANNEL,
AHC_AIC7896_FE, 23,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_3940U2, AHC_AIC7896,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED | AHC_MULTI_CHANNEL,
AHC_AIC7896_FE, 24,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_3950U2D, AHC_AIC7896,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED | AHC_MULTI_CHANNEL,
AHC_AIC7896_FE, 25,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC, PCI_DEVICE_ID_ADAPTEC_1480A, AHC_AIC7860,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7860_FE, 26,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7892A, AHC_AIC7892,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7892_FE, 27,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7892B, AHC_AIC7892,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7892_FE, 27,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7892D, AHC_AIC7892,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7892_FE, 27,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7892P, AHC_AIC7892,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7892_FE, 27,
32, C46 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7899A, AHC_AIC7899,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7899_FE, 28,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7899B, AHC_AIC7899,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7899_FE, 28,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7899D, AHC_AIC7899,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7899_FE, 28,
32, C56_66 },
{PCI_VENDOR_ID_ADAPTEC2, PCI_DEVICE_ID_ADAPTEC2_7899P, AHC_AIC7899,
AHC_PAGESCBS | AHC_NEWEEPROM_FMT | AHC_BIOS_ENABLED,
AHC_AIC7899_FE, 28,
32, C56_66 },
};
unsigned short command;
unsigned int devconfig, i, oldverbose;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
struct pci_dev *pdev = NULL;
#else
int index;
unsigned int piobase, mmapbase;
unsigned char pci_bus, pci_devfn, pci_irq;
#endif
for (i = 0; i < NUMBER(aic_pdevs); i++)
{
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
pdev = NULL;
while ((pdev = pci_find_device(aic_pdevs[i].vendor_id,
aic_pdevs[i].device_id,
pdev)))
#else
index = 0;
while (!(pcibios_find_device(aic_pdevs[i].vendor_id,
aic_pdevs[i].device_id,
index++, &pci_bus, &pci_devfn)) )
#endif
{
if ( i == 0 )
{
if (aic7xxx_verbose & (VERBOSE_PROBE|VERBOSE_PROBE2))
{
printk(KERN_INFO "aic7xxx: The 7810 RAID controller is not "
"supported by\n");
printk(KERN_INFO "         this driver, we are ignoring it.\n");
}
}
else if ( (temp_p = kmalloc(sizeof(struct aic7xxx_host),
GFP_ATOMIC)) != NULL )
{
memset(temp_p, 0, sizeof(struct aic7xxx_host));
temp_p->chip = aic_pdevs[i].chip | AHC_PCI;
temp_p->flags = aic_pdevs[i].flags;
temp_p->features = aic_pdevs[i].features;
temp_p->board_name_index = aic_pdevs[i].board_name_index;
temp_p->sc_size = aic_pdevs[i].seeprom_size;
temp_p->sc_type = aic_pdevs[i].seeprom_type;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
temp_p->irq = pdev->irq;
temp_p->pdev = pdev;
temp_p->pci_bus = pdev->bus->number;
temp_p->pci_device_fn = pdev->devfn;
temp_p->base = pdev->base_address[0];
temp_p->mbase = pdev->base_address[1];
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk("aic7xxx: <%s> at PCI %d/%d\n",
board_names[aic_pdevs[i].board_name_index],
PCI_SLOT(temp_p->pdev->devfn),
PCI_FUNC(temp_p->pdev->devfn));
pci_read_config_word(pdev, PCI_COMMAND, &command);
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk("aic7xxx: Initial PCI_COMMAND value was 0x%x\n",
(int)command);
}
#ifdef AIC7XXX_STRICT_PCI_SETUP
command |= PCI_COMMAND_SERR | PCI_COMMAND_PARITY |
PCI_COMMAND_INVALIDATE | PCI_COMMAND_MASTER |
PCI_COMMAND_MEMORY | PCI_COMMAND_IO;
#else
command |= PCI_COMMAND_MASTER | PCI_COMMAND_MEMORY | PCI_COMMAND_IO;
#endif
if (aic7xxx_pci_parity == 0)
command &= ~(PCI_COMMAND_SERR | PCI_COMMAND_PARITY);
pci_write_config_word(pdev, PCI_COMMAND, command);
#ifdef AIC7XXX_STRICT_PCI_SETUP
pci_read_config_dword(pdev, DEVCONFIG, &devconfig);
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk("aic7xxx: Initial DEVCONFIG value was 0x%x\n", devconfig);
}
devconfig |= 0x80000000;
if ((aic7xxx_pci_parity == 0) || (aic7xxx_pci_parity == -1))
{
devconfig &= ~(0x00000008);
}
else
{
devconfig |= 0x00000008;
}
pci_write_config_dword(pdev, DEVCONFIG, devconfig);
#endif
#else
temp_p->pci_bus = pci_bus;
temp_p->pci_device_fn = pci_devfn;
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk("aic7xxx: <%s> at PCI %d/%d\n",
board_names[aic_pdevs[i].board_name_index],
PCI_SLOT(temp_p->pci_device_fn),
PCI_FUNC(temp_p->pci_device_fn));
pcibios_read_config_byte(pci_bus, pci_devfn, PCI_INTERRUPT_LINE,
&pci_irq);
temp_p->irq = pci_irq;
pcibios_read_config_dword(pci_bus, pci_devfn, PCI_BASE_ADDRESS_0,
&piobase);
temp_p->base = piobase;
pcibios_read_config_dword(pci_bus, pci_devfn, PCI_BASE_ADDRESS_1,
&mmapbase);
temp_p->mbase = mmapbase;
pcibios_read_config_word(pci_bus, pci_devfn, PCI_COMMAND, &command);
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk("aic7xxx: Initial PCI_COMMAND value was 0x%x\n",
(int)command);
}
#ifdef AIC7XXX_STRICT_PCI_SETUP
command |= PCI_COMMAND_SERR | PCI_COMMAND_PARITY |
PCI_COMMAND_INVALIDATE | PCI_COMMAND_MASTER |
PCI_COMMAND_MEMORY | PCI_COMMAND_IO;
#else
command |= PCI_COMMAND_MASTER | PCI_COMMAND_MEMORY | PCI_COMMAND_IO;
#endif
if (aic7xxx_pci_parity == 0)
command &= ~(PCI_COMMAND_SERR | PCI_COMMAND_PARITY);
pcibios_write_config_word(pci_bus, pci_devfn, PCI_COMMAND, command);
#ifdef AIC7XXX_STRICT_PCI_SETUP
pcibios_read_config_dword(pci_bus, pci_devfn, DEVCONFIG, &devconfig);
if (aic7xxx_verbose & VERBOSE_PROBE2)
{
printk("aic7xxx: Initial DEVCONFIG value was 0x%x\n", devconfig);
}
devconfig |= 0x80000000;
if ((aic7xxx_pci_parity == 0) || (aic7xxx_pci_parity == -1))
{
devconfig &= ~(0x00000008);
}
else
{
devconfig |= 0x00000008;
}
pcibios_write_config_dword(pci_bus, pci_devfn, DEVCONFIG, devconfig);
#endif
#endif
temp_p->base &= PCI_BASE_ADDRESS_IO_MASK;
temp_p->mbase &= PCI_BASE_ADDRESS_MEM_MASK;
temp_p->unpause = INTEN;
temp_p->pause = temp_p->unpause | PAUSE;
if ( ((temp_p->base == 0) &&
(temp_p->mbase == 0)) ||
(temp_p->irq == 0) )
{
printk("aic7xxx: <%s> at PCI %d/%d\n",
board_names[aic_pdevs[i].board_name_index],
PCI_SLOT(temp_p->pci_device_fn),
PCI_FUNC(temp_p->pci_device_fn));
printk("aic7xxx: Controller disabled by BIOS, ignoring.\n");
kfree(temp_p);
temp_p = NULL;
continue;
}
#ifdef MMAPIO
{
unsigned long page_offset, base;
base = temp_p->mbase & PAGE_MASK;
page_offset = temp_p->mbase - base;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,0)
temp_p->maddr = ioremap_nocache(base, page_offset + 256);
#else
temp_p->maddr = vremap(base, page_offset + 256);
#endif
if(temp_p->maddr)
{
temp_p->maddr += page_offset;
if(aic_inb(temp_p, HCNTRL) == 0xff)
{
printk(KERN_INFO "aic7xxx: <%s> at PCI %d/%d\n",
board_names[aic_pdevs[i].board_name_index],
PCI_SLOT(temp_p->pci_device_fn),
PCI_FUNC(temp_p->pci_device_fn));
printk(KERN_INFO "aic7xxx: MMAPed I/O failed, reverting to "
"Programmed I/O.\n");
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,0)
iounmap((void *) (((unsigned long) temp_p->maddr) & PAGE_MASK));
#else
vfree((void *) (((unsigned long) temp_p->maddr) & PAGE_MASK));
#endif
temp_p->maddr = 0;
}
}
}
#endif
pause_sequencer(temp_p);
oldverbose = aic7xxx_verbose;
aic7xxx_verbose = 0;
aic7xxx_pci_intr(temp_p);
aic7xxx_verbose = oldverbose;
temp_p->bios_address = 0;
if (temp_p->features & AHC_ULTRA2)
temp_p->scsi_id = aic_inb(temp_p, SCSIID_ULTRA2) & OID;
else
temp_p->scsi_id = aic_inb(temp_p, SCSIID) & OID;
sxfrctl1 = aic_inb(temp_p, SXFRCTL1) & STPWEN;
if (aic7xxx_chip_reset(temp_p) == -1)
{
kfree(temp_p);
temp_p = NULL;
continue;
}
switch (temp_p->chip & AHC_CHIPID_MASK)
{
case AHC_AIC7870:
case AHC_AIC7880:
if(temp_p->flags & AHC_MULTI_CHANNEL)
{
switch(PCI_SLOT(temp_p->pci_device_fn))
{
case 5:
temp_p->flags |= AHC_CHNLB;
break;
case 8:
temp_p->flags |= AHC_CHNLB;
break;
case 12:
temp_p->flags |= AHC_CHNLC;
break;
default:
break;
}
}
break;
case AHC_AIC7895:
case AHC_AIC7896:
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
if (PCI_FUNC(temp_p->pdev->devfn) != 0)
{
temp_p->flags |= AHC_CHNLB;
}
if ((temp_p->chip & AHC_CHIPID_MASK) == AHC_AIC7895)
{
pci_read_config_dword(pdev, DEVCONFIG, &devconfig);
devconfig |= SCBSIZE32;
pci_write_config_dword(pdev, DEVCONFIG, devconfig);
}
#else
if (PCI_FUNC(temp_p->pci_device_fn) != 0)
{
temp_p->flags |= AHC_CHNLB;
}
if ((temp_p->chip & AHC_CHIPID_MASK) == AHC_AIC7895)
{
pcibios_read_config_dword(pci_bus, pci_devfn, DEVCONFIG,
&devconfig);
devconfig |= SCBSIZE32;
pcibios_write_config_dword(pci_bus, pci_devfn, DEVCONFIG,
devconfig);
}
#endif
break;
default:
break;
}
switch (temp_p->chip & AHC_CHIPID_MASK)
{
case AHC_AIC7890:
case AHC_AIC7896:
aic_outb(temp_p, 0, SCAMCTL);
aic_outb(temp_p, (aic_inb(temp_p, DSCOMMAND0) |
CACHETHEN | MPARCKEN | USCBSIZE32 |
CIOPARCKEN) & ~DPARCKEN, DSCOMMAND0);
aic7xxx_load_seeprom(temp_p, &sxfrctl1);
break;
case AHC_AIC7850:
case AHC_AIC7860:
aic_outb(temp_p, (aic_inb(temp_p, DSCOMMAND0) |
CACHETHEN | MPARCKEN) & ~DPARCKEN,
DSCOMMAND0);
default:
aic7xxx_load_seeprom(temp_p, &sxfrctl1);
break;
case AHC_AIC7880:
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
pci_read_config_dword(pdev, DEVCONFIG, &devconfig);
#else
pcibios_read_config_dword(pci_bus, pci_devfn, DEVCONFIG,
&devconfig);
#endif
if ((devconfig & 0xff) >= 1)
{
aic_outb(temp_p, (aic_inb(temp_p, DSCOMMAND0) |
CACHETHEN | MPARCKEN) & ~DPARCKEN,
DSCOMMAND0);
}
aic7xxx_load_seeprom(temp_p, &sxfrctl1);
break;
}
switch(temp_p->chip & AHC_CHIPID_MASK)
{
case AHC_AIC7895:
case AHC_AIC7896:
current_p = list_p;
while(current_p != NULL)
{
if ( (current_p->pci_bus == temp_p->pci_bus) &&
(PCI_SLOT(current_p->pci_device_fn) ==
PCI_SLOT(temp_p->pci_device_fn)) )
{
if ( PCI_FUNC(current_p->pci_device_fn) == 0 )
{
temp_p->flags |=
(current_p->flags & AHC_CHANNEL_B_PRIMARY);
temp_p->flags &= ~(AHC_BIOS_ENABLED|AHC_USEDEFAULTS);
temp_p->flags |=
(current_p->flags & (AHC_BIOS_ENABLED|AHC_USEDEFAULTS));
}
else
{
current_p->flags |=
(temp_p->flags & AHC_CHANNEL_B_PRIMARY);
current_p->flags &= ~(AHC_BIOS_ENABLED|AHC_USEDEFAULTS);
current_p->flags |=
(temp_p->flags & (AHC_BIOS_ENABLED|AHC_USEDEFAULTS));
}
}
current_p = current_p->next;
}
break;
default:
break;
}
switch(temp_p->chip & AHC_CHIPID_MASK)
{
default:
break;
case AHC_AIC7895:
case AHC_AIC7896:
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
pci_read_config_dword(pdev, DEVCONFIG, &devconfig);
#else
pcibios_read_config_dword(pci_bus, pci_devfn, DEVCONFIG,
&devconfig);
#endif
if (temp_p->features & AHC_ULTRA2)
{
if (aic_inb(temp_p, DSCOMMAND0) & RAMPSM_ULTRA2)
{
aic_outb(temp_p,
aic_inb(temp_p, DSCOMMAND0) & ~SCBRAMSEL_ULTRA2,
DSCOMMAND0);
temp_p->flags |= AHC_EXTERNAL_SRAM;
devconfig |= EXTSCBPEN;
}
}
else if (devconfig & RAMPSM)
{
devconfig &= ~SCBRAMSEL;
devconfig |= EXTSCBPEN;
temp_p->flags |= AHC_EXTERNAL_SRAM;
}
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
pci_write_config_dword(pdev, DEVCONFIG, devconfig);
#else
pcibios_write_config_dword(pci_bus, pci_devfn, DEVCONFIG,
devconfig);
#endif
if ( (temp_p->flags & AHC_EXTERNAL_SRAM) &&
(temp_p->flags & AHC_CHNLB) )
aic_outb(temp_p, 1, CCSCBBADDR);
break;
}
aic_outb(temp_p,
(aic_inb(temp_p, SBLKCTL) & ~(DIAGLEDEN | DIAGLEDON)),
SBLKCTL);
if (temp_p->features & AHC_ULTRA2)
{
aic_outb(temp_p, RD_DFTHRSH_75 | WR_DFTHRSH_75, DFF_THRSH);
}
else
{
aic_outb(temp_p, DFTHRSH_100, DSPCISTATUS);
}
if ( list_p == NULL )
{
list_p = current_p = temp_p;
}
else
{
current_p = list_p;
while(current_p->next != NULL)
current_p = current_p->next;
current_p->next = temp_p;
}
temp_p->next = NULL;
found++;
}
else
{
printk("aic7xxx: Found <%s>\n",
board_names[aic_pdevs[i].board_name_index]);
printk(KERN_INFO "aic7xxx: Unable to allocate device memory, "
"skipping.\n");
}
}
}
}
#endif CONFIG_PCI
{
struct aic7xxx_host *sort_list[4] = { NULL, NULL, NULL, NULL };
struct aic7xxx_host *vlb, *pci;
struct aic7xxx_host *prev_p;
struct aic7xxx_host *p;
unsigned char left;
prev_p = vlb = pci = NULL;
temp_p = list_p;
while (temp_p != NULL)
{
switch(temp_p->chip & ~AHC_CHIPID_MASK)
{
case AHC_EISA:
case AHC_VL:
{
p = temp_p;
if (p->flags & AHC_BIOS_ENABLED)
vlb = sort_list[0];
else
vlb = sort_list[2];
if (vlb == NULL)
{
vlb = temp_p;
temp_p = temp_p->next;
vlb->next = NULL;
}
else
{
current_p = vlb;
prev_p = NULL;
while ( (current_p != NULL) &&
(current_p->bios_address < temp_p->bios_address))
{
prev_p = current_p;
current_p = current_p->next;
}
if (prev_p != NULL)
{
prev_p->next = temp_p;
temp_p = temp_p->next;
prev_p->next->next = current_p;
}
else
{
vlb = temp_p;
temp_p = temp_p->next;
vlb->next = current_p;
}
}
if (p->flags & AHC_BIOS_ENABLED)
sort_list[0] = vlb;
else
sort_list[2] = vlb;
break;
}
default:
{
p = temp_p;
if (p->flags & AHC_BIOS_ENABLED)
pci = sort_list[1];
else
pci = sort_list[3];
if (pci == NULL)
{
pci = temp_p;
temp_p = temp_p->next;
pci->next = NULL;
}
else
{
current_p = pci;
prev_p = NULL;
if (!aic7xxx_reverse_scan)
{
while ( (current_p != NULL) &&
( (PCI_SLOT(current_p->pci_device_fn) |
(current_p->pci_bus << 8)) <
(PCI_SLOT(temp_p->pci_device_fn) |
(temp_p->pci_bus << 8)) ) )
{
prev_p = current_p;
current_p = current_p->next;
}
}
else
{
while ( (current_p != NULL) &&
( (PCI_SLOT(current_p->pci_device_fn) |
(current_p->pci_bus << 8)) >
(PCI_SLOT(temp_p->pci_device_fn) |
(temp_p->pci_bus << 8)) ) )
{
prev_p = current_p;
current_p = current_p->next;
}
}
if ( (current_p) && (temp_p->flags & AHC_MULTI_CHANNEL) &&
(temp_p->pci_bus == current_p->pci_bus) &&
(PCI_SLOT(temp_p->pci_device_fn) ==
PCI_SLOT(current_p->pci_device_fn)) )
{
if (temp_p->flags & AHC_CHNLB)
{
if ( !(temp_p->flags & AHC_CHANNEL_B_PRIMARY) )
{
prev_p = current_p;
current_p = current_p->next;
}
}
else
{
if (temp_p->flags & AHC_CHANNEL_B_PRIMARY)
{
prev_p = current_p;
current_p = current_p->next;
}
}
}
if (prev_p != NULL)
{
prev_p->next = temp_p;
temp_p = temp_p->next;
prev_p->next->next = current_p;
}
else
{
pci = temp_p;
temp_p = temp_p->next;
pci->next = current_p;
}
}
if (p->flags & AHC_BIOS_ENABLED)
sort_list[1] = pci;
else
sort_list[3] = pci;
break;
}
}
}
{
int i;
left = found;
for (i=0; i<NUMBER(sort_list); i++)
{
temp_p = sort_list[i];
while(temp_p != NULL)
{
template->name = board_names[temp_p->board_name_index];
p = aic7xxx_alloc(template, temp_p);
if (p != NULL)
{
p->instance = found - left;
if (aic7xxx_register(template, p, (--left)) == 0)
{
found--;
aic7xxx_release(p->host);
scsi_unregister(p->host);
}
else if (aic7xxx_dump_card)
{
pause_sequencer(p);
aic7xxx_print_card(p);
aic7xxx_print_scratch_ram(p);
unpause_sequencer(p, TRUE);
}
}
current_p = temp_p;
temp_p = (struct aic7xxx_host *)temp_p->next;
kfree(current_p);
}
}
}
}
return (found);
}
#ifdef AIC7XXX_FAKE_NEGOTIATION_CMDS
static void
aic7xxx_negotiation_complete(Scsi_Cmnd *cmd)
{
return;
}
static void
aic7xxx_build_negotiation_cmnd(struct aic7xxx_host *p, Scsi_Cmnd *old_cmd,
int tindex)
{
if ( (p->needwdtr & (1<<tindex)) && !(p->wdtr_pending & (1<<tindex)) )
{
if(p->dev_wdtr_cmnd[tindex] == NULL)
{
Scsi_Cmnd *cmd;
if (!(p->dev_wdtr_cmnd[tindex] = kmalloc(sizeof(Scsi_Cmnd), GFP_ATOMIC)) )
{
return;
}
cmd = p->dev_wdtr_cmnd[tindex];
memset(cmd, 0, sizeof(Scsi_Cmnd));
memcpy(cmd, old_cmd, sizeof(Scsi_Cmnd));
memset(&cmd->cmnd[0], 0, sizeof(cmd->cmnd));
memset(&cmd->data_cmnd[0], 0, sizeof(cmd->data_cmnd));
cmd->lun = 0;
cmd->request_bufflen = 0;
cmd->request_buffer = NULL;
cmd->use_sg = cmd->old_use_sg = cmd->sglist_len = 0;
cmd->bufflen = 0;
cmd->buffer = NULL;
cmd->underflow = 0;
cmd->cmd_len = 6;
}
p->dev_wdtr_cmnd[tindex]->next = old_cmd;
aic7xxx_queue(p->dev_wdtr_cmnd[tindex],
aic7xxx_negotiation_complete);
}
else if ( (p->needsdtr & (1<<tindex)) && !(p->sdtr_pending & (1<<tindex)) &&
!(p->wdtr_pending & (1<<tindex)) )
{
if(p->dev_sdtr_cmnd[tindex] == NULL)
{
Scsi_Cmnd *cmd;
if (!(p->dev_sdtr_cmnd[tindex] = kmalloc(sizeof(Scsi_Cmnd), GFP_ATOMIC)) )
{
return;
}
cmd = p->dev_sdtr_cmnd[tindex];
memset(cmd, 0, sizeof(Scsi_Cmnd));
memcpy(cmd, old_cmd, sizeof(Scsi_Cmnd));
memset(&cmd->cmnd[0], 0, sizeof(cmd->cmnd));
memset(&cmd->data_cmnd[0], 0, sizeof(cmd->data_cmnd));
cmd->lun = 0;
cmd->request_bufflen = 0;
cmd->request_buffer = NULL;
cmd->use_sg = cmd->old_use_sg = cmd->sglist_len = 0;
cmd->bufflen = 0;
cmd->buffer = NULL;
cmd->underflow = 0;
cmd->cmd_len = 6;
}
p->dev_sdtr_cmnd[tindex]->next = old_cmd;
aic7xxx_queue(p->dev_sdtr_cmnd[tindex],
aic7xxx_negotiation_complete);
}
}
#endif
#ifdef AIC7XXX_VERBOSE_DEBUGGING
static void
aic7xxx_print_scb(struct aic7xxx_host *p, struct aic7xxx_scb *scb)
{
int i;
unsigned char *x;
x = (unsigned char *)&scb->hscb->control;
for(i=0; i<32; i++)
{
printk("%02x ", x[i]);
}
printk("\n");
}
#endif
static void
aic7xxx_buildscb(struct aic7xxx_host *p, Scsi_Cmnd *cmd,
struct aic7xxx_scb *scb)
{
unsigned short mask;
struct aic7xxx_hwscb *hscb;
mask = (0x01 << TARGET_INDEX(cmd));
hscb = scb->hscb;
hscb->control = 0;
scb->tag_action = 0;
if (p->discenable & mask)
{
hscb->control |= DISCENB;
if (p->tagenable & mask)
{
cmd->tag = hscb->tag;
p->dev_commands_sent[TARGET_INDEX(cmd)]++;
if (p->dev_commands_sent[TARGET_INDEX(cmd)] < 200)
{
hscb->control |= MSG_SIMPLE_Q_TAG;
scb->tag_action = MSG_SIMPLE_Q_TAG;
}
else
{
if (p->orderedtag & mask)
{
hscb->control |= MSG_ORDERED_Q_TAG;
scb->tag_action = MSG_ORDERED_Q_TAG;
}
else
{
hscb->control |= MSG_SIMPLE_Q_TAG;
scb->tag_action = MSG_SIMPLE_Q_TAG;
}
p->dev_commands_sent[TARGET_INDEX(cmd)] = 0;
}
}
}
if (p->dev_flags[TARGET_INDEX(cmd)] & DEVICE_SCANNED)
{
#ifdef AIC7XXX_FAKE_NEGOTIATION_CMDS
if ( (p->needwdtr & mask) && !(p->wdtr_pending & mask) )
{
if (cmd == p->dev_wdtr_cmnd[TARGET_INDEX(cmd)])
{
p->wdtr_pending |= mask;
scb->flags |= SCB_MSGOUT_WDTR;
hscb->control &= DISCENB;
hscb->control |= MK_MESSAGE;
scb->tag_action = 0;
}
else
{
aic7xxx_build_negotiation_cmnd(p, cmd, TARGET_INDEX(cmd));
}
}
else if ( (p->needsdtr & mask) && !(p->sdtr_pending & mask) &&
!(p->wdtr_pending & mask) )
{
if (cmd == p->dev_sdtr_cmnd[TARGET_INDEX(cmd)])
{
p->sdtr_pending |= mask;
scb->flags |= SCB_MSGOUT_SDTR;
hscb->control &= DISCENB;
hscb->control |= MK_MESSAGE;
scb->tag_action = 0;
}
else if (cmd != p->dev_wdtr_cmnd[TARGET_INDEX(cmd)])
{
aic7xxx_build_negotiation_cmnd(p, cmd, TARGET_INDEX(cmd));
}
}
#else
if ( (p->needwdtr & mask) && !(p->wdtr_pending & mask) &&
!(p->sdtr_pending & mask) && (cmd->lun == 0) )
{
p->wdtr_pending |= mask;
scb->flags |= SCB_MSGOUT_WDTR;
hscb->control &= DISCENB;
hscb->control |= MK_MESSAGE;
scb->tag_action = 0;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Building WDTR command.\n", p->host_no,
CTL_OF_CMD(cmd));
#endif
}
else if ( (p->needsdtr & mask) && !(p->wdtr_pending & mask) &&
!(p->sdtr_pending & mask) && (cmd->lun == 0) )
{
p->sdtr_pending |= mask;
scb->flags |= SCB_MSGOUT_SDTR;
hscb->control &= DISCENB;
hscb->control |= MK_MESSAGE;
scb->tag_action = 0;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (aic7xxx_verbose > 0xffff)
printk(INFO_LEAD "Building SDTR command.\n", p->host_no,
CTL_OF_CMD(cmd));
#endif
}
#endif
}
hscb->target_channel_lun = ((cmd->target << 4) & 0xF0) |
((cmd->channel & 0x01) << 3) | (cmd->lun & 0x07);
hscb->SCSI_cmd_length = cmd->cmd_len;
hscb->SCSI_cmd_pointer = cpu_to_le32(VIRT_TO_BUS(cmd->cmnd));
if (cmd->use_sg)
{
struct scatterlist *sg;
int i;
sg = (struct scatterlist *)cmd->request_buffer;
scb->sg_length = 0;
for (i = 0; i < cmd->use_sg; i++)
{
scb->sg_list[i].address = cpu_to_le32(VIRT_TO_BUS(sg[i].address));
scb->sg_list[i].length = cpu_to_le32(sg[i].length);
scb->sg_length += sg[i].length;
}
hscb->data_pointer = scb->sg_list[0].address;
hscb->data_count = scb->sg_list[0].length;
scb->sg_count = cmd->use_sg;
hscb->SG_segment_count = cmd->use_sg;
hscb->SG_list_pointer = cpu_to_le32(VIRT_TO_BUS(&scb->sg_list[1]));
}
else
{
if (cmd->request_bufflen)
{
scb->sg_count = 1;
scb->sg_list[0].address = cpu_to_le32(VIRT_TO_BUS(cmd->request_buffer));
scb->sg_list[0].length = cpu_to_le32(cmd->request_bufflen);
scb->sg_length = cmd->request_bufflen;
hscb->SG_segment_count = 1;
hscb->SG_list_pointer = cpu_to_le32(VIRT_TO_BUS(&scb->sg_list[0]));
hscb->data_count = scb->sg_list[0].length;
hscb->data_pointer = scb->sg_list[0].address;
}
else
{
scb->sg_count = 0;
scb->sg_length = 0;
hscb->SG_segment_count = 0;
hscb->SG_list_pointer = 0;
hscb->data_count = 0;
hscb->data_pointer = 0;
}
}
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if((cmd->cmnd[0] == TEST_UNIT_READY) && (aic7xxx_verbose & VERBOSE_PROBE2))
{
aic7xxx_print_scb(p, scb);
}
#endif
}
int
aic7xxx_queue(Scsi_Cmnd *cmd, void (*fn)(Scsi_Cmnd *))
{
struct aic7xxx_host *p;
struct aic7xxx_scb *scb;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
int tindex = TARGET_INDEX(cmd);
#endif
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags = 0;
#endif
p = (struct aic7xxx_host *) cmd->host->hostdata;
#ifdef AIC7XXX_VERBOSE_DEBUGGING
if (!(p->flags & AHC_A_SCANNED) && (cmd->channel == 0))
{
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(INFO_LEAD "Scanning channel for devices.\n",
p->host_no, 0, -1, -1);
p->flags |= AHC_A_SCANNED;
}
else
{
if (!(p->flags & AHC_B_SCANNED) && (cmd->channel == 1))
{
if (aic7xxx_verbose & VERBOSE_PROBE2)
printk(INFO_LEAD "Scanning channel for devices.\n",
p->host_no, 1, -1, -1);
p->flags |= AHC_B_SCANNED;
}
}
if (p->dev_active_cmds[tindex] > (cmd->device->queue_depth + 1))
{
printk(WARN_LEAD "Commands queued exceeds queue "
"depth, active=%d\n",
p->host_no, CTL_OF_CMD(cmd),
p->dev_active_cmds[tindex]);
if ( p->dev_active_cmds[tindex] > 220 )
p->dev_active_cmds[tindex] = 0;
}
#endif
scb = scbq_remove_head(&p->scb_data->free_scbs);
if (scb == NULL)
{
DRIVER_LOCK
aic7xxx_allocate_scb(p);
DRIVER_UNLOCK
scb = scbq_remove_head(&p->scb_data->free_scbs);
}
if (scb == NULL)
{
printk(WARN_LEAD "Couldn't get a free SCB.\n", p->host_no,
CTL_OF_CMD(cmd));
cmd->result = (DID_BUS_BUSY << 16);
DRIVER_LOCK
aic7xxx_queue_cmd_complete(p, cmd);
DRIVER_UNLOCK
return 0;
}
else
{
scb->cmd = cmd;
aic7xxx_position(cmd) = scb->hscb->tag;
aic7xxx_buildscb(p, cmd, scb);
cmd->scsi_done = fn;
cmd->result = DID_OK;
memset(cmd->sense_buffer, 0, sizeof(cmd->sense_buffer));
aic7xxx_error(cmd) = DID_OK;
aic7xxx_status(cmd) = 0;
cmd->host_scribble = NULL;
scb->flags |= SCB_ACTIVE | SCB_WAITINGQ;
DRIVER_LOCK
scbq_insert_tail(&p->waiting_scbs, scb);
if ( (p->flags & (AHC_IN_ISR | AHC_IN_ABORT | AHC_IN_RESET)) == 0)
{
aic7xxx_run_waiting_queues(p);
}
DRIVER_UNLOCK
}
return (0);
}
static int
aic7xxx_bus_device_reset(struct aic7xxx_host *p, Scsi_Cmnd *cmd)
{
struct aic7xxx_scb *scb;
struct aic7xxx_hwscb *hscb;
int result = -1;
int channel;
unsigned char saved_scbptr, lastphase;
unsigned char hscb_index;
int disconnected;
scb = (p->scb_data->scb_array[aic7xxx_position(cmd)]);
hscb = scb->hscb;
lastphase = aic_inb(p, LASTPHASE);
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
{
printk(INFO_LEAD "Bus Device reset, scb flags 0x%x, ",
p->host_no, CTL_OF_SCB(scb), scb->flags);
switch (lastphase)
{
case P_DATAOUT:
printk("Data-Out phase\n");
break;
case P_DATAIN:
printk("Data-In phase\n");
break;
case P_COMMAND:
printk("Command phase\n");
break;
case P_MESGOUT:
printk("Message-Out phase\n");
break;
case P_STATUS:
printk("Status phase\n");
break;
case P_MESGIN:
printk("Message-In phase\n");
break;
default:
printk("while idle, LASTPHASE = 0x%x\n", lastphase);
break;
}
printk(INFO_LEAD "SCSISIGI 0x%x, SEQADDR 0x%x, SSTAT0 0x%x, SSTAT1 "
"0x%x\n", p->host_no, CTL_OF_SCB(scb),
aic_inb(p, SCSISIGI),
aic_inb(p, SEQADDR0) | (aic_inb(p, SEQADDR1) << 8),
aic_inb(p, SSTAT0), aic_inb(p, SSTAT1));
}
channel = cmd->channel;
saved_scbptr = aic_inb(p, SCBPTR);
disconnected = FALSE;
if (lastphase != P_BUSFREE)
{
if (aic_inb(p, SCB_TAG) >= p->scb_data->numscbs)
{
printk(WARN_LEAD "Invalid SCB ID %d is active, "
"SCB flags = 0x%x.\n", p->host_no,
CTL_OF_CMD(cmd), scb->hscb->tag, scb->flags);
return(SCSI_RESET_ERROR);
}
if (scb->hscb->tag == aic_inb(p, SCB_TAG))
{
if ( (lastphase != P_MESGOUT) && (lastphase != P_MESGIN) )
{
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Device reset message in "
"message buffer\n", p->host_no, CTL_OF_SCB(scb));
scb->flags |= SCB_RESET | SCB_DEVICE_RESET;
aic7xxx_error(scb->cmd) = DID_RESET;
p->dev_flags[TARGET_INDEX(scb->cmd)] |=
BUS_DEVICE_RESET_PENDING;
aic_outb(p, HOST_MSG, MSG_OUT);
aic_outb(p, lastphase | ATNO, SCSISIGO);
return(SCSI_RESET_PENDING);
}
else
{
printk(WARN_LEAD "Device reset, Message buffer "
"in use\n", p->host_no, CTL_OF_SCB(scb));
scb->flags |= SCB_RESET | SCB_DEVICE_RESET;
aic7xxx_error(scb->cmd) = DID_RESET;
p->dev_flags[TARGET_INDEX(scb->cmd)] |=
BUS_DEVICE_RESET_PENDING;
return(SCSI_RESET_ERROR);
}
}
}
hscb_index = aic7xxx_find_scb(p, scb);
if (hscb_index == SCB_LIST_NULL)
{
disconnected = (aic7xxx_scb_on_qoutfifo(p, scb)) ? FALSE : TRUE;
}
else
{
aic_outb(p, hscb_index, SCBPTR);
if (aic_inb(p, SCB_CONTROL) & DISCONNECTED)
{
disconnected = TRUE;
}
}
if (disconnected)
{
scb->hscb->control |= MK_MESSAGE;
scb->flags |= SCB_RESET | SCB_DEVICE_RESET;
p->dev_flags[TARGET_INDEX(scb->cmd)] |=
BUS_DEVICE_RESET_PENDING;
if (hscb_index != SCB_LIST_NULL)
{
unsigned char scb_control;
aic_outb(p, hscb_index, SCBPTR);
scb_control = aic_inb(p, SCB_CONTROL);
aic_outb(p, scb_control | MK_MESSAGE, SCB_CONTROL);
}
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Queueing device reset "
"command.\n", p->host_no, CTL_OF_SCB(scb));
p->qinfifo[p->qinfifonext++] = scb->hscb->tag;
if (p->features & AHC_QUEUE_REGS)
aic_outb(p, p->qinfifonext, HNSCB_QOFF);
else
aic_outb(p, p->qinfifonext, KERNEL_QINPOS);
scb->flags |= SCB_QUEUED_ABORT;
result = SCSI_RESET_PENDING;
}
else if (result == -1)
{
result = SCSI_RESET_ERROR;
}
aic_outb(p, saved_scbptr, SCBPTR);
return (result);
}
void
aic7xxx_panic_abort(struct aic7xxx_host *p, Scsi_Cmnd *cmd)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,0)
int i, mask, found, need_tag;
struct aic7xxx_scb *scb;
unsigned char qinpos, hscbp;
found = FALSE;
#endif
printk("aic7xxx driver version %s/%s\n", AIC7XXX_C_VERSION,
UTS_RELEASE);
printk("Controller type:\n    %s\n", board_names[p->board_name_index]);
printk("p->flags=0x%x, p->chip=0x%x, p->features=0x%x, "
"sequencer %s paused\n",
p->flags, p->chip, p->features,
(aic_inb(p, HCNTRL) & PAUSE) ? "is" : "isn't" );
pause_sequencer(p);
disable_irq(p->irq);
aic7xxx_print_card(p);
aic7xxx_print_scratch_ram(p);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,0)
for(i=0; i<MAX_TARGETS; i++)
{
if(p->dev_flags[i] & DEVICE_PRESENT)
{
mask = (0x01 << i);
printk(INFO_LEAD "dev_flags=0x%x, WDTR:%c/%c/%c, SDTR:%c/%c/%c,"
" q_depth=%d:%d\n",
p->host_no, 0, i, 0, p->dev_flags[i],
(p->wdtr_pending & mask) ? 'Y' : 'N',
(p->needwdtr & mask) ? 'Y' : 'N',
(p->needwdtr_copy & mask) ? 'Y' : 'N',
(p->sdtr_pending & mask) ? 'Y' : 'N',
(p->needsdtr & mask) ? 'Y' : 'N',
(p->needsdtr_copy & mask) ? 'Y' : 'N',
p->dev_active_cmds[i],
p->dev_max_queue_depth[i] );
printk(INFO_LEAD "targ_scsirate=0x%x", p->host_no, 0, i, 0,
aic_inb(p, TARG_SCSIRATE + i));
if (p->features & AHC_ULTRA2)
printk(", targ_offset=%d", aic_inb(p, TARG_OFFSET + i));
printk("\n");
}
}
need_tag = -1;
if ( cmd )
{
scb = p->scb_data->scb_array[aic7xxx_position(cmd)];
if ( (scb->flags & SCB_ACTIVE) && (scb->cmd == cmd) )
{
printk("Timed out command is scb #%d:\n", scb->hscb->tag);
printk("Tag%d: flags=0x%x, control=0x%x, TCL=0x%x, %s\n", scb->hscb->tag,
scb->flags, scb->hscb->control, scb->hscb->target_channel_lun,
(scb->flags & SCB_WAITINGQ) ? "WAITINGQ" : "Sent" );
need_tag = scb->hscb->tag;
if (scb->flags & SCB_WAITINGQ) found=TRUE;
}
}
printk("QINFIFO: (TAG) ");
qinpos = aic_inb(p, QINPOS);
while ( qinpos != p->qinfifonext )
{
if (p->qinfifo[qinpos] == need_tag)
found=TRUE;
printk("%d ", p->qinfifo[qinpos++]);
}
printk("\n");
printk("Current SCB: (SCBPTR/TAG/CONTROL) %d/%d/0x%x\n", aic_inb(p, SCBPTR),
aic_inb(p, SCB_TAG), aic_inb(p, SCB_CONTROL) );
if (aic_inb(p, SCB_TAG) == need_tag) found=TRUE;
printk("WAITING_SCBS: (SCBPTR/TAG/CONTROL) %d->",
hscbp = aic_inb(p, WAITING_SCBH));
while (hscbp != SCB_LIST_NULL)
{
aic_outb(p, hscbp, SCBPTR);
printk("%d/%d/0x%x ", hscbp, aic_inb(p, SCB_TAG), aic_inb(p, SCB_CONTROL));
hscbp = aic_inb(p, SCB_NEXT);
if (aic_inb(p, SCB_TAG) == need_tag) found=TRUE;
}
printk("\n");
printk("DISCONNECTED_SCBS: (SCBPTR/TAG/CONTROL) %d->",
hscbp = aic_inb(p, DISCONNECTED_SCBH));
while (hscbp != SCB_LIST_NULL)
{
aic_outb(p, hscbp, SCBPTR);
printk("%d/%d/0x%x ", hscbp, aic_inb(p, SCB_TAG), aic_inb(p, SCB_CONTROL));
hscbp = aic_inb(p, SCB_NEXT);
if (aic_inb(p, SCB_TAG) == need_tag) found=TRUE;
}
printk("\n");
printk("FREE_SCBS: (SCBPTR/TAG/CONTROL) %d->",
hscbp = aic_inb(p, FREE_SCBH));
while (hscbp != SCB_LIST_NULL)
{
aic_outb(p, hscbp, SCBPTR);
printk("%d/%d/0x%x ", hscbp, aic_inb(p, SCB_TAG), aic_inb(p, SCB_CONTROL));
hscbp = aic_inb(p, SCB_NEXT);
}
printk("\n");
if (found == FALSE)
{
printk("SCBPTR CONTROL TAG PREV NEXT\n");
for(i=0; i<p->scb_data->maxhscbs; i++)
{
aic_outb(p, i, SCBPTR);
printk("   %3d      %02x  %02x   %02x   %02x\n", i,
aic_inb(p, SCB_CONTROL), aic_inb(p, SCB_TAG),
aic_inb(p, SCB_PREV), aic_inb(p, SCB_NEXT));
}
}
for (i=0; i < p->scb_data->numscbs; i++)
{
scb = p->scb_data->scb_array[i];
if ( (scb->flags & SCB_ACTIVE) && (scb->cmd != cmd) )
{
printk("Tag%d: flags=0x%x, control=0x%x, TCL=0x%x, %s\n", scb->hscb->tag,
scb->flags, scb->hscb->control, scb->hscb->target_channel_lun,
(scb->flags & SCB_WAITINGQ) ? "WAITINGQ" : "Sent" );
}
}
#endif
sti();
for(;;) barrier();
}
int
aic7xxx_abort(Scsi_Cmnd *cmd)
{
struct aic7xxx_scb *scb = NULL;
struct aic7xxx_host *p;
int result, found=0;
unsigned char tmp_char, saved_hscbptr, next_hscbptr, prev_hscbptr;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags = 0;
#endif
Scsi_Cmnd *cmd_next, *cmd_prev;
p = (struct aic7xxx_host *) cmd->host->hostdata;
scb = (p->scb_data->scb_array[aic7xxx_position(cmd)]);
if (aic7xxx_panic_on_abort)
aic7xxx_panic_abort(p, cmd);
DRIVER_LOCK
pause_sequencer(p);
while ( (aic_inb(p, INTSTAT) & INT_PEND) && !(p->flags & AHC_IN_ISR))
{
aic7xxx_isr(p->irq, p, (void *)NULL);
pause_sequencer(p);
aic7xxx_done_cmds_complete(p);
}
if ((scb == NULL) || (cmd->serial_number != cmd->serial_number_at_timeout))
{
if (aic7xxx_verbose & VERBOSE_ABORT_MID)
printk(INFO_LEAD "Abort called with bogus Scsi_Cmnd "
"pointer.\n", p->host_no, CTL_OF_CMD(cmd));
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_ABORT_NOT_RUNNING);
}
if (scb->cmd != cmd)
{
cmd_next = p->completeq.head;
cmd_prev = NULL;
while (cmd_next != NULL)
{
if (cmd_next == cmd)
{
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "Abort called for command "
"on completeq, completing.\n", p->host_no, CTL_OF_CMD(cmd));
if ( cmd_prev == NULL )
p->completeq.head = (Scsi_Cmnd *)cmd_next->host_scribble;
else
cmd_prev->host_scribble = cmd_next->host_scribble;
cmd_next->scsi_done(cmd_next);
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_ABORT_NOT_RUNNING);
}
cmd_prev = cmd_next;
cmd_next = (Scsi_Cmnd *)cmd_next->host_scribble;
}
if (aic7xxx_verbose & VERBOSE_ABORT_MID)
printk(INFO_LEAD "Abort called for already completed"
" command.\n", p->host_no, CTL_OF_CMD(cmd));
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_ABORT_NOT_RUNNING);
}
if ( scb->flags & (SCB_ABORT | SCB_RESET | SCB_QUEUED_ABORT) )
{
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "SCB aborted once already, "
"escalating.\n", p->host_no, CTL_OF_SCB(scb));
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_ABORT_SNOOZE);
}
if ( (p->flags & (AHC_RESET_PENDING | AHC_ABORT_PENDING)) ||
(p->dev_flags[TARGET_INDEX(scb->cmd)] &
BUS_DEVICE_RESET_PENDING) )
{
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "Reset/Abort pending for this "
"device, not wasting our time.\n", p->host_no, CTL_OF_SCB(scb));
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_ABORT_PENDING);
}
found = 0;
p->flags |= AHC_IN_ABORT;
if (aic7xxx_verbose & VERBOSE_ABORT)
printk(INFO_LEAD "Aborting scb %d, flags 0x%x\n",
p->host_no, CTL_OF_SCB(scb), scb->hscb->tag, scb->flags);
if ( scb->hscb->tag == aic_inb(p, SCB_TAG) )
{
result = aic_inb(p, LASTPHASE);
switch (result)
{
case P_DATAOUT:
case P_DATAIN:
case P_COMMAND:
case P_STATUS:
case P_MESGOUT:
case P_MESGIN:
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "SCB is currently active.  "
"Waiting on completion.\n", p->host_no, CTL_OF_SCB(scb));
unpause_sequencer(p, FALSE);
p->flags &= ~AHC_IN_ABORT;
scb->flags |= SCB_RECOVERY_SCB;
p->flags |= AHC_ABORT_PENDING;
DRIVER_UNLOCK
return(SCSI_ABORT_PENDING);
break;
default:
break;
}
}
if ((found == 0) && (scb->flags & SCB_WAITINGQ))
{
int tindex = TARGET_INDEX(cmd);
#ifdef AIC7XXX_FAKE_NEGOTIATION_CMDS
unsigned short mask;
mask = (1 << tindex);
if (p->wdtr_pending & mask)
{
if (p->dev_wdtr_cmnd[tindex]->next != cmd)
found = 1;
else
found = 0;
}
else if (p->sdtr_pending & mask)
{
if (p->dev_sdtr_cmnd[tindex]->next != cmd)
found = 1;
else
found = 0;
}
else
{
found = 1;
}
if (found == 0)
{
unpause_sequencer(p, TRUE);
scb->flags |= SCB_ABORT;
DRIVER_UNLOCK
return(SCSI_ABORT_PENDING);
}
#endif
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "SCB found on waiting list and "
"aborted.\n", p->host_no, CTL_OF_SCB(scb));
scbq_remove(&p->waiting_scbs, scb);
scbq_remove(&p->delayed_scbs[tindex], scb);
p->dev_active_cmds[tindex]++;
p->activescbs++;
scb->flags &= ~(SCB_WAITINGQ | SCB_ACTIVE);
scb->flags |= SCB_ABORT | SCB_QUEUED_FOR_DONE;
found = 1;
}
if ( found == 0 )
{
if ( ((found = aic7xxx_search_qinfifo(p, cmd->target,
cmd->channel,
cmd->lun, scb->hscb->tag, SCB_ABORT | SCB_QUEUED_FOR_DONE,
FALSE, NULL)) != 0) &&
(aic7xxx_verbose & VERBOSE_ABORT_PROCESS))
printk(INFO_LEAD "SCB found in QINFIFO and "
"aborted.\n", p->host_no, CTL_OF_SCB(scb));
}
if ( found == 0 )
{
unsigned char scb_next_ptr;
prev_hscbptr = SCB_LIST_NULL;
saved_hscbptr = aic_inb(p, SCBPTR);
next_hscbptr = aic_inb(p, WAITING_SCBH);
while ( next_hscbptr != SCB_LIST_NULL )
{
aic_outb(p, next_hscbptr, SCBPTR );
if ( scb->hscb->tag == aic_inb(p, SCB_TAG) )
{
found = 1;
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "SCB found on hardware waiting"
" list and aborted.\n", p->host_no, CTL_OF_SCB(scb));
if ( prev_hscbptr == SCB_LIST_NULL )
{
aic_outb(p, aic_inb(p, SCB_NEXT), WAITING_SCBH);
aic_outb(p, aic_inb(p, SCSISEQ) & ~ENSELO, SCSISEQ);
aic_outb(p, CLRSELTIMEO, CLRSINT1);
}
else
{
scb_next_ptr = aic_inb(p, SCB_NEXT);
aic_outb(p, prev_hscbptr, SCBPTR);
aic_outb(p, scb_next_ptr, SCB_NEXT);
aic_outb(p, next_hscbptr, SCBPTR);
}
aic_outb(p, SCB_LIST_NULL, SCB_TAG);
aic_outb(p, 0, SCB_CONTROL);
aic7xxx_add_curscb_to_free_list(p);
scb->flags = SCB_ABORT | SCB_QUEUED_FOR_DONE;
break;
}
prev_hscbptr = next_hscbptr;
next_hscbptr = aic_inb(p, SCB_NEXT);
}
aic_outb(p, saved_hscbptr, SCBPTR );
}
if ( found == 0 )
{
p->flags |= AHC_ABORT_PENDING;
scb->flags |= SCB_QUEUED_ABORT | SCB_ABORT | SCB_RECOVERY_SCB;
scb->hscb->control |= MK_MESSAGE;
result=aic7xxx_find_scb(p, scb);
if ( result != SCB_LIST_NULL )
{
saved_hscbptr = aic_inb(p, SCBPTR);
aic_outb(p, result, SCBPTR);
tmp_char = aic_inb(p, SCB_CONTROL);
aic_outb(p, tmp_char | MK_MESSAGE, SCB_CONTROL);
aic_outb(p, saved_hscbptr, SCBPTR);
}
if (aic7xxx_verbose & VERBOSE_ABORT_PROCESS)
printk(INFO_LEAD "SCB disconnected.  Queueing Abort"
" SCB.\n", p->host_no, CTL_OF_SCB(scb));
p->qinfifo[p->qinfifonext++] = scb->hscb->tag;
if (p->features & AHC_QUEUE_REGS)
aic_outb(p, p->qinfifonext, HNSCB_QOFF);
else
aic_outb(p, p->qinfifonext, KERNEL_QINPOS);
}
if (found)
{
aic7xxx_run_done_queue(p, TRUE);
aic7xxx_run_waiting_queues(p);
}
p->flags &= ~AHC_IN_ABORT;
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
if ( found != 0 )
return(SCSI_ABORT_SUCCESS);
else
return(SCSI_ABORT_PENDING);
}
int
aic7xxx_reset(Scsi_Cmnd *cmd, unsigned int flags)
{
struct aic7xxx_scb *scb = NULL;
struct aic7xxx_host *p;
int tindex;
int result = -1;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
unsigned long cpu_flags = 0;
#endif
#define DEVICE_RESET 0x01
#define BUS_RESET 0x02
#define HOST_RESET 0x04
#define FAIL 0x08
#define RESET_DELAY 0x10
int action;
Scsi_Cmnd *cmd_prev, *cmd_next;
if ( cmd == NULL )
{
printk(KERN_WARNING "(scsi?:?:?:?) Reset called with NULL Scsi_Cmnd "
"pointer, failing.\n");
return(SCSI_RESET_SNOOZE);
}
p = (struct aic7xxx_host *) cmd->host->hostdata;
scb = (p->scb_data->scb_array[aic7xxx_position(cmd)]);
tindex = TARGET_INDEX(cmd);
if (aic7xxx_panic_on_abort)
aic7xxx_panic_abort(p, cmd);
DRIVER_LOCK
pause_sequencer(p);
while ( (aic_inb(p, INTSTAT) & INT_PEND) && !(p->flags & AHC_IN_ISR))
{
aic7xxx_isr(p->irq, p, (void *)NULL );
pause_sequencer(p);
aic7xxx_done_cmds_complete(p);
}
if (scb == NULL)
{
if (aic7xxx_verbose & VERBOSE_RESET_MID)
printk(INFO_LEAD "Reset called with bogus Scsi_Cmnd"
"->SCB mapping, improvising.\n", p->host_no, CTL_OF_CMD(cmd));
if ( flags & SCSI_RESET_SUGGEST_HOST_RESET )
{
action = HOST_RESET;
}
else
{
action = BUS_RESET;
}
}
else if (scb->cmd != cmd)
{
if (aic7xxx_verbose & VERBOSE_RESET_MID)
printk(INFO_LEAD "Reset called with recycled SCB "
"for cmd.\n", p->host_no, CTL_OF_CMD(cmd));
cmd_prev = NULL;
cmd_next = p->completeq.head;
while ( cmd_next != NULL )
{
if (cmd_next == cmd)
{
if (aic7xxx_verbose & VERBOSE_RESET_RETURN)
printk(INFO_LEAD "Reset, found cmd on completeq"
", completing.\n", p->host_no, CTL_OF_CMD(cmd));
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_RESET_NOT_RUNNING);
}
cmd_prev = cmd_next;
cmd_next = (Scsi_Cmnd *)cmd_next->host_scribble;
}
if ( !(flags & SCSI_RESET_SYNCHRONOUS) )
{
if (aic7xxx_verbose & VERBOSE_RESET_RETURN)
printk(INFO_LEAD "Reset, cmd not found,"
" failing.\n", p->host_no, CTL_OF_CMD(cmd));
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_RESET_NOT_RUNNING);
}
else
{
if (aic7xxx_verbose & VERBOSE_RESET_MID)
printk(INFO_LEAD "Reset called, no scb, "
"flags 0x%x\n", p->host_no, CTL_OF_CMD(cmd), flags);
scb = NULL;
action = HOST_RESET;
}
}
else
{
if (aic7xxx_verbose & VERBOSE_RESET_MID)
printk(INFO_LEAD "Reset called, scb %d, flags "
"0x%x\n", p->host_no, CTL_OF_SCB(scb), scb->hscb->tag, scb->flags);
if ( aic7xxx_scb_on_qoutfifo(p, scb) )
{
if(aic7xxx_verbose & VERBOSE_RESET_RETURN)
printk(INFO_LEAD "SCB on qoutfifo, returning.\n", p->host_no,
CTL_OF_SCB(scb));
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_RESET_NOT_RUNNING);
}
if ( flags & SCSI_RESET_SUGGEST_HOST_RESET )
{
action = HOST_RESET;
}
else if ( flags & SCSI_RESET_SUGGEST_BUS_RESET )
{
action = BUS_RESET;
}
else
{
action = DEVICE_RESET;
}
}
if ( (action & DEVICE_RESET) &&
(p->dev_flags[tindex] & BUS_DEVICE_RESET_PENDING) )
{
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Bus device reset already sent to "
"device, escalating.\n", p->host_no, CTL_OF_CMD(cmd));
action = BUS_RESET;
}
if ( (action & DEVICE_RESET) &&
(scb->flags & SCB_QUEUED_ABORT) )
{
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
{
printk(INFO_LEAD "Have already attempted to reach "
"device with queued\n", p->host_no, CTL_OF_CMD(cmd));
printk(INFO_LEAD "message, will escalate to bus "
"reset.\n", p->host_no, CTL_OF_CMD(cmd));
}
action = BUS_RESET;
}
if ( (action & DEVICE_RESET) &&
(p->flags & (AHC_RESET_PENDING | AHC_ABORT_PENDING)) )
{
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Bus device reset stupid when "
"other action has failed.\n", p->host_no, CTL_OF_CMD(cmd));
action = BUS_RESET;
}
if ( (action & BUS_RESET) && !(p->features & AHC_TWIN) )
{
action = HOST_RESET;
}
if ( (p->dev_flags[tindex] & DEVICE_RESET_DELAY) &&
!(action & (HOST_RESET | BUS_RESET)))
{
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
{
printk(INFO_LEAD "Reset called too soon after last "
"reset without requesting\n", p->host_no, CTL_OF_CMD(cmd));
printk(INFO_LEAD "bus or host reset, escalating.\n", p->host_no,
CTL_OF_CMD(cmd));
}
action = BUS_RESET;
}
if ( (p->flags & AHC_RESET_DELAY) &&
(action & (HOST_RESET | BUS_RESET)) )
{
if (aic7xxx_verbose & VERBOSE_RESET_PROCESS)
printk(INFO_LEAD "Reset called too soon after "
"last bus reset, delaying.\n", p->host_no, CTL_OF_CMD(cmd));
action = RESET_DELAY;
}
switch (action)
{
case RESET_DELAY:
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_RESET_PENDING);
break;
case FAIL:
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(SCSI_RESET_ERROR);
break;
case DEVICE_RESET:
p->flags |= AHC_IN_RESET;
result = aic7xxx_bus_device_reset(p, cmd);
aic7xxx_run_done_queue(p, TRUE);
aic7xxx_run_waiting_queues(p);
unpause_sequencer(p, FALSE);
p->flags &= ~AHC_IN_RESET;
DRIVER_UNLOCK
return(result);
break;
case BUS_RESET:
case HOST_RESET:
default:
p->flags |= AHC_IN_RESET | AHC_RESET_DELAY;
p->dev_expires[p->scsi_id] = jiffies + (3 * HZ);
p->dev_timer_active |= (0x01 << p->scsi_id);
if ( !(p->dev_timer_active & (0x01 << MAX_TARGETS)) ||
time_after_eq(p->dev_timer.expires, p->dev_expires[p->scsi_id]) )
{
del_timer(&p->dev_timer);
p->dev_timer.expires = p->dev_expires[p->scsi_id];
add_timer(&p->dev_timer);
p->dev_timer_active |= (0x01 << MAX_TARGETS);
}
aic7xxx_reset_channel(p, cmd->channel, TRUE);
if ( (p->features & AHC_TWIN) && (action & HOST_RESET) )
{
aic7xxx_reset_channel(p, cmd->channel ^ 0x01, TRUE);
restart_sequencer(p);
}
if (action != HOST_RESET)
result = SCSI_RESET_SUCCESS | SCSI_RESET_BUS_RESET;
else
{
result = SCSI_RESET_SUCCESS | SCSI_RESET_HOST_RESET;
aic_outb(p, aic_inb(p, SIMODE1) & ~(ENREQINIT|ENBUSFREE),
SIMODE1);
aic7xxx_clear_intstat(p);
p->flags &= ~AHC_HANDLING_REQINITS;
p->msg_type = MSG_TYPE_NONE;
p->msg_index = 0;
p->msg_len = 0;
}
aic7xxx_run_done_queue(p, TRUE);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,132)
if ( flags & SCSI_RESET_SYNCHRONOUS )
{
cmd->result = DID_BUS_BUSY << 16;
cmd->done(cmd);
}
#endif
p->flags &= ~AHC_IN_RESET;
aic7xxx_run_waiting_queues(p);
unpause_sequencer(p, FALSE);
DRIVER_UNLOCK
return(result);
break;
}
}
int
aic7xxx_biosparam(Disk *disk, kdev_t dev, int geom[])
{
int heads, sectors, cylinders, ret;
struct aic7xxx_host *p;
struct buffer_head *bh;
p = (struct aic7xxx_host *) disk->device->host->hostdata;
bh = bread(MKDEV(MAJOR(dev), MINOR(dev)&~0xf), 0, 1024);
if ( bh )
{
ret = scsi_partsize(bh, disk->capacity, &geom[2], &geom[0], &geom[1]);
brelse(bh);
if ( ret != -1 )
return(ret);
}
heads = 64;
sectors = 32;
cylinders = disk->capacity / (heads * sectors);
if ((p->flags & AHC_EXTEND_TRANS_A) && (cylinders > 1024))
{
heads = 255;
sectors = 63;
cylinders = disk->capacity / (heads * sectors);
}
geom[0] = heads;
geom[1] = sectors;
geom[2] = cylinders;
return (0);
}
int
aic7xxx_release(struct Scsi_Host *host)
{
struct aic7xxx_host *p = (struct aic7xxx_host *) host->hostdata;
struct aic7xxx_host *next, *prev;
if(p->irq)
free_irq(p->irq, p);
release_region(p->base, MAXREG - MINREG);
#ifdef MMAPIO
if(p->maddr)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,0)
vfree((void *) (((unsigned long) p->maddr) & PAGE_MASK));
#else
iounmap((void *) (((unsigned long) p->maddr) & PAGE_MASK));
#endif
}
#endif
prev = NULL;
next = first_aic7xxx;
while(next != NULL)
{
if(next == p)
{
if(prev == NULL)
first_aic7xxx = next->next;
else
prev->next = next->next;
}
else
{
prev = next;
}
next = next->next;
}
aic7xxx_free(p);
return(0);
}
static void
aic7xxx_print_card(struct aic7xxx_host *p)
{
int i, j, k, chip;
static struct register_ranges {
int num_ranges;
int range_val[32];
} cards_ds[] = {
{ 0, {0,} },
{10, {0x00, 0x05, 0x08, 0x11, 0x18, 0x19, 0x1f, 0x1f, 0x60, 0x60,
0x62, 0x66, 0x80, 0x8e, 0x90, 0x95, 0x97, 0x97, 0x9b, 0x9f} },
{ 9, {0x00, 0x05, 0x08, 0x11, 0x18, 0x1f, 0x60, 0x60, 0x62, 0x66,
0x80, 0x8e, 0x90, 0x95, 0x97, 0x97, 0x9a, 0x9f} },
{ 9, {0x00, 0x05, 0x08, 0x11, 0x18, 0x1f, 0x60, 0x60, 0x62, 0x66,
0x80, 0x8e, 0x90, 0x95, 0x97, 0x97, 0x9a, 0x9f} },
{10, {0x00, 0x05, 0x08, 0x11, 0x18, 0x19, 0x1c, 0x1f, 0x60, 0x60,
0x62, 0x66, 0x80, 0x8e, 0x90, 0x95, 0x97, 0x97, 0x9a, 0x9f} },
{10, {0x00, 0x05, 0x08, 0x11, 0x18, 0x1a, 0x1c, 0x1f, 0x60, 0x60,
0x62, 0x66, 0x80, 0x8e, 0x90, 0x95, 0x97, 0x97, 0x9a, 0x9f} },
{16, {0x00, 0x05, 0x08, 0x11, 0x18, 0x1f, 0x60, 0x60, 0x62, 0x66,
0x84, 0x8e, 0x90, 0x95, 0x97, 0x97, 0x9a, 0x9a, 0x9f, 0x9f,
0xe0, 0xf1, 0xf4, 0xf4, 0xf6, 0xf6, 0xf8, 0xf8, 0xfa, 0xfc,
0xfe, 0xff} },
{12, {0x00, 0x05, 0x08, 0x11, 0x18, 0x19, 0x1b, 0x1f, 0x60, 0x60,
0x62, 0x66, 0x80, 0x8e, 0x90, 0x95, 0x97, 0x97, 0x9a, 0x9a,
0x9f, 0x9f, 0xe0, 0xf1} },
{16, {0x00, 0x05, 0x08, 0x11, 0x18, 0x1f, 0x60, 0x60, 0x62, 0x66,
0x84, 0x8e, 0x90, 0x95, 0x97, 0x97, 0x9a, 0x9a, 0x9f, 0x9f,
0xe0, 0xf1, 0xf4, 0xf4, 0xf6, 0xf6, 0xf8, 0xf8, 0xfa, 0xfc,
0xfe, 0xff} },
};
#ifdef CONFIG_PCI
static struct register_ranges cards_ns[] = {
{ 0, {0,} },
{ 0, {0,} },
{ 7, {0x04, 0x08, 0x0c, 0x0e, 0x10, 0x17, 0x28, 0x2b, 0x30, 0x33,
0x3c, 0x41, 0x43, 0x47} },
{ 7, {0x04, 0x08, 0x0c, 0x0e, 0x10, 0x17, 0x28, 0x2b, 0x30, 0x33,
0x3c, 0x41, 0x43, 0x47} },
{ 5, {0x04, 0x08, 0x0c, 0x0e, 0x10, 0x17, 0x30, 0x33, 0x3c, 0x41} },
{ 5, {0x04, 0x08, 0x0c, 0x0e, 0x10, 0x17, 0x30, 0x34, 0x3c, 0x47} },
{ 5, {0x04, 0x08, 0x0c, 0x1b, 0x30, 0x34, 0x3c, 0x43, 0xdc, 0xe3} },
{ 6, {0x04, 0x08, 0x0c, 0x0e, 0x10, 0x17, 0x30, 0x34, 0x3c, 0x47,
0xdc, 0xe3} },
{ 6, {0x04, 0x08, 0x0c, 0x1b, 0x30, 0x34, 0x3c, 0x43, 0xdc, 0xe3,
0xff, 0xff} }
};
#endif
chip = p->chip & AHC_CHIPID_MASK;
printk("%s at ",
board_names[p->board_name_index]);
switch(p->chip & ~AHC_CHIPID_MASK)
{
case AHC_VL:
printk("VLB Slot %d.\n", p->pci_device_fn);
break;
case AHC_EISA:
printk("EISA Slot %d.\n", p->pci_device_fn);
break;
case AHC_PCI:
default:
printk("PCI %d/%d.\n", PCI_SLOT(p->pci_device_fn),
PCI_FUNC(p->pci_device_fn));
break;
}
#ifdef CONFIG_PCI
{
unsigned char temp;
printk("PCI Dump:\n");
k=0;
for(i=0; i<cards_ns[chip].num_ranges; i++)
{
for(j = cards_ns[chip].range_val[ i * 2 ];
j <= cards_ns[chip].range_val[ i * 2 + 1 ] ;
j++)
{
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,92)
pci_read_config_byte(p->pdev, j, &temp);
#else
pcibios_read_config_byte(p->pci_bus, p->pci_device_fn, j, &temp);
#endif
printk("%02x:%02x ", j, temp);
if(++k == 13)
{
printk("\n");
k = 0;
}
}
}
}
if(k != 0)
printk("\n");
#endif
printk("Card Dump:\n");
k = 0;
for(i=0; i<cards_ds[chip].num_ranges; i++)
{
for(j = cards_ds[chip].range_val[ i * 2 ];
j <= cards_ds[chip].range_val[ i * 2 + 1 ] ;
j++)
{
printk("%02x:%02x ", j, aic_inb(p, j));
if(++k == 13)
{
printk("\n");
k=0;
}
}
}
if(k != 0)
printk("\n");
if (p->flags & AHC_SEEPROM_FOUND)
{
unsigned short *sc1;
sc1 = (unsigned short *)&p->sc;
printk("SEEPROM dump.\n");
for(i=1; i<=32; i++)
{
printk("0x%04x", sc1[i-1]);
if ( (i % 8) == 0 )
printk("\n");
else
printk("  ");
}
}
if(p->features & AHC_QUEUE_REGS)
{
aic_outb(p, 0, SDSCB_QOFF);
aic_outb(p, 0, SNSCB_QOFF);
aic_outb(p, 0, HNSCB_QOFF);
}
}
static void
aic7xxx_print_scratch_ram(struct aic7xxx_host *p)
{
int i, k;
k = 0;
printk("Scratch RAM:\n");
for(i = SRAM_BASE; i < SEQCTL; i++)
{
printk("%02x:%02x ", i, aic_inb(p, i));
if(++k == 13)
{
printk("\n");
k=0;
}
}
if (p->features & AHC_MORE_SRAM)
{
for(i = TARG_OFFSET; i < 0x80; i++)
{
printk("%02x:%02x ", i, aic_inb(p, i));
if(++k == 13)
{
printk("\n");
k=0;
}
}
}
printk("\n");
}
#include "aic7xxx_proc.c"
#ifdef MODULE
Scsi_Host_Template driver_template = AIC7XXX;
#include "scsi_module.c"
#endif