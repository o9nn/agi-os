#ifndef _WD7000_H
#include <linux/version.h>
#include <linux/types.h>
#include <linux/kdev_t.h>
#ifndef NULL
#define NULL 0L
#endif
#define WD7000_Q 16
#define WD7000_SG 16
#ifdef WD7000_DEFINES
#define OGMB_CNT 16
#define ICMB_CNT 32
#define MAX_SCBS (4 * WD7000_Q)
typedef volatile struct {
unchar status;
unchar scbptr[3];
} Mailbox;
typedef struct {
struct Scsi_Host *sh;
int iobase;
int irq;
int dma;
int int_counter;
int bus_on;
int bus_off;
struct {
Mailbox ogmb[OGMB_CNT];
Mailbox icmb[ICMB_CNT];
} mb;
int next_ogmb;
unchar control;
unchar rev1;
unchar rev2;
} Adapter;
#define IRQ_MIN 3
#define IRQ_MAX 15
#define IRQS (IRQ_MAX - IRQ_MIN + 1)
#define BUS_ON 64
#define BUS_OFF 15
typedef struct {
short irq;
short dma;
uint iobase;
short bus_on;
short bus_off;
} Config;
typedef struct {
const char *sig;
ulong ofs;
uint len;
} Signature;
#define ASC_STAT 0
#define ASC_COMMAND 0
#define ASC_INTR_STAT 1
#define ASC_INTR_ACK 1
#define ASC_CONTROL 2
#define INT_IM 0x80
#define CMD_RDY 0x40
#define CMD_REJ 0x20
#define ASC_INIT 0x10
#define ASC_STATMASK 0xf0
#define NO_OP 0
#define INITIALIZATION 1
#define DISABLE_UNS_INTR 2
#define ENABLE_UNS_INTR 3
#define INTR_ON_FREE_OGMB 4
#define SOFT_RESET 5
#define HARD_RESET_ACK 6
#define START_OGMB 0x80
#define SCAN_OGMBS 0xc0
typedef struct {
unchar op;
unchar ID;
unchar bus_on;
unchar bus_off;
unchar rsvd;
unchar mailboxes[3];
unchar ogmbs;
unchar icmbs;
} InitCmd;
#define MB_INTR 0xC0
#define IMB_INTR 0x40
#define MB_MASK 0x3f
#define INT_EN 0x08
#define DMA_EN 0x04
#define SCSI_RES 0x02
#define ASC_RES 0x01
typedef struct {
unchar len[3];
unchar ptr[3];
} Sgb;
typedef struct {
unchar op;
unchar idlun;
unchar cdb[12];
volatile unchar status;
volatile unchar vue;
unchar maxlen[3];
unchar dataptr[3];
unchar linkptr[3];
unchar direc;
unchar reserved2[6];
Scsi_Cmnd *SCpnt;
Sgb sgb[WD7000_SG];
Adapter *host;
unchar used;
} Scb;
#define ICB_OP_MASK 0x80
#define ICB_OP_OPEN_RBUF 0x80
#define ICB_OP_RECV_CMD 0x81
#define ICB_OP_RECV_DATA 0x82
#define ICB_OP_RECV_SDATA 0x83
#define ICB_OP_SEND_DATA 0x84
#define ICB_OP_SEND_STAT 0x86
#define ICB_OP_READ_INIT 0x88
#define ICB_OP_READ_ID 0x89
#define ICB_OP_SET_UMASK 0x8A
#define ICB_OP_GET_UMASK 0x8B
#define ICB_OP_GET_REVISION 0x8C
#define ICB_OP_DIAGNOSTICS 0x8D
#define ICB_OP_SET_EPARMS 0x8E
#define ICB_OP_GET_EPARMS 0x8F
typedef struct {
unchar op;
unchar IDlun;
unchar len[3];
unchar ptr[3];
unchar rsvd[7];
volatile unchar vue;
volatile unchar status;
volatile unchar phase;
} IcbRecvCmd;
typedef struct {
unchar op;
unchar IDlun;
unchar stat;
unchar rsvd[12];
volatile unchar vue;
volatile unchar status;
volatile unchar phase;
} IcbSendStat;
typedef struct {
unchar op;
volatile unchar primary;
volatile unchar secondary;
unchar rsvd[12];
volatile unchar vue;
volatile unchar status;
volatile unchar phase;
} IcbRevLvl;
typedef struct {
unchar op;
volatile unchar mask[14];
#if 0
unchar rsvd[12];
#endif
volatile unchar vue;
volatile unchar status;
volatile unchar phase;
} IcbUnsMask;
typedef struct {
unchar op;
unchar type;
unchar len[3];
unchar ptr[3];
unchar rsvd[7];
volatile unchar vue;
volatile unchar status;
volatile unchar phase;
} IcbDiag;
#define ICB_DIAG_POWERUP 0
#define ICB_DIAG_WALKING 1
#define ICB_DIAG_DMA 2
#define ICB_DIAG_FULL 3
typedef struct {
unchar op;
unchar rsvd1;
unchar len[3];
unchar ptr[3];
unchar idx[2];
unchar rsvd2[5];
volatile unchar vue;
volatile unchar status;
volatile unchar phase;
} IcbParms;
typedef struct {
unchar op;
unchar data[14];
volatile unchar vue;
volatile unchar status;
volatile unchar phase;
} IcbAny;
typedef union {
unchar op;
IcbRecvCmd recv_cmd;
IcbSendStat send_stat;
IcbRevLvl rev_lvl;
IcbDiag diag;
IcbParms eparms;
IcbAny icb;
unchar data[18];
} Icb;
#define WAITnexttimeout 200
typedef union {
int i;
unchar u[sizeof (int)];
} i_u;
#endif
#if (LINUX_VERSION_CODE >= 0x020100)
#define WD7000 { \
proc_dir: &proc_scsi_wd7000, \
proc_info: wd7000_proc_info, \
name: "Western Digital WD-7000", \
detect: wd7000_detect, \
command: wd7000_command, \
queuecommand: wd7000_queuecommand, \
abort: wd7000_abort, \
reset: wd7000_reset, \
bios_param: wd7000_biosparam, \
can_queue: WD7000_Q, \
this_id: 7, \
sg_tablesize: WD7000_SG, \
cmd_per_lun: 1, \
unchecked_isa_dma: 1, \
use_clustering: ENABLE_CLUSTERING, \
use_new_eh_code: 0 \
}
#else
#define WD7000 { \
proc_dir: &proc_scsi_wd7000, \
proc_info: wd7000_proc_info, \
name: "Western Digital WD-7000", \
detect: wd7000_detect, \
command: wd7000_command, \
queuecommand: wd7000_queuecommand, \
abort: wd7000_abort, \
reset: wd7000_reset, \
bios_param: wd7000_biosparam, \
can_queue: WD7000_Q, \
this_id: 7, \
sg_tablesize: WD7000_SG, \
cmd_per_lun: 1, \
unchecked_isa_dma: 1, \
use_clustering: ENABLE_CLUSTERING, \
}
#endif
extern struct proc_dir_entry proc_scsi_wd7000;
#ifdef WD7000_DEFINES
int wd7000_diagnostics (Adapter *, int);
int wd7000_init (Adapter *);
void wd7000_revision (Adapter *);
#endif
void wd7000_setup (char *, int *);
int make_code (uint, uint);
void wd7000_intr_handle (int, void *, struct pt_regs *);
void do_wd7000_intr_handle (int, void *, struct pt_regs *);
int wd7000_queuecommand (Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
int wd7000_command (Scsi_Cmnd *);
int wd7000_set_info (char *, int, struct Scsi_Host *);
int wd7000_proc_info (char *, char **, off_t, int, int, int);
int wd7000_detect (Scsi_Host_Template *);
int wd7000_abort (Scsi_Cmnd *);
int wd7000_reset (Scsi_Cmnd *, uint);
int wd7000_biosparam (Disk *, kdev_t, int *);
#endif