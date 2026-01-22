#undef HAVE_OLD_UX4F_FIRMWARE
#include <linux/version.h>
#define LinuxVersionCode(v, p, s) (((v)<<16)+((p)<<8)+(s))
#define MAX_INT_PARAM 10
#if defined(MODULE)
#include <linux/module.h>
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,26)
MODULE_PARM(io_port, "1-" __MODULE_STRING(MAX_INT_PARAM) "i");
MODULE_PARM(linked_comm, "i");
MODULE_PARM(have_old_firmware, "i");
MODULE_PARM(link_statistics, "i");
MODULE_PARM(max_queue_depth, "i");
MODULE_PARM(use_new_eh_code, "i");
MODULE_PARM(ext_tran, "i");
MODULE_AUTHOR("Dario Ballabio");
#endif
#endif
#include <linux/string.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/ioport.h>
#include <linux/delay.h>
#include <asm/io.h>
#include <asm/system.h>
#include <asm/byteorder.h>
#include <linux/proc_fs.h>
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "sd.h"
#include <asm/dma.h>
#include <asm/irq.h>
#include "u14-34f.h"
#include <linux/stat.h>
#include <linux/config.h>
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,36)
#include <linux/init.h>
#else
#define __initfunc(A) A
#define __initdata
#define __init
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,101)
#include <asm/spinlock.h>
#define IRQ_FLAGS
#define IRQ_LOCK
#define IRQ_LOCK_SAVE
#define IRQ_UNLOCK
#define IRQ_UNLOCK_RESTORE
#define SPIN_FLAGS unsigned long spin_flags;
#define SPIN_LOCK spin_lock_irq(&io_request_lock);
#define SPIN_LOCK_SAVE spin_lock_irqsave(&io_request_lock, spin_flags);
#define SPIN_UNLOCK spin_unlock_irq(&io_request_lock);
#define SPIN_UNLOCK_RESTORE \
spin_unlock_irqrestore(&io_request_lock, spin_flags);
static int use_new_eh_code = TRUE;
#else
#define IRQ_FLAGS unsigned long irq_flags;
#define IRQ_LOCK cli();
#define IRQ_LOCK_SAVE do {save_flags(irq_flags); cli();} while (0);
#define IRQ_UNLOCK sti();
#define IRQ_UNLOCK_RESTORE do {restore_flags(irq_flags);} while (0);
#define SPIN_FLAGS
#define SPIN_LOCK
#define SPIN_LOCK_SAVE
#define SPIN_UNLOCK
#define SPIN_UNLOCK_RESTORE
static int use_new_eh_code = FALSE;
#endif
struct proc_dir_entry proc_scsi_u14_34f = {
PROC_SCSI_U14_34F, 6, "u14_34f",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#define PRODUCT_ID1  0x56
#define PRODUCT_ID2  0x40
#define ISA  0
#define ESA 1
#define OP_HOST_ADAPTER   0x1
#define OP_SCSI           0x2
#define OP_RESET          0x4
#define DTD_SCSI          0x0
#define DTD_IN            0x1
#define DTD_OUT           0x2
#define DTD_NONE          0x3
#define HA_CMD_INQUIRY    0x1
#define HA_CMD_SELF_DIAG  0x2
#define HA_CMD_READ_BUFF  0x3
#define HA_CMD_WRITE_BUFF 0x4
#undef  DEBUG_LINKED_COMMANDS
#undef  DEBUG_DETECT
#undef  DEBUG_INTERRUPT
#undef  DEBUG_RESET
#undef  DEBUG_GENERATE_ERRORS
#undef  DEBUG_GENERATE_ABORTS
#undef  DEBUG_GEOMETRY
#define MAX_ISA 3
#define MAX_VESA 1
#define MAX_EISA 0
#define MAX_PCI 0
#define MAX_BOARDS (MAX_ISA + MAX_VESA + MAX_EISA + MAX_PCI)
#define MAX_CHANNEL 1
#define MAX_LUN 8
#define MAX_TARGET 8
#define MAX_MAILBOXES 16
#define MAX_SGLIST 32
#define MAX_SAFE_SGLIST 16
#define MAX_INTERNAL_RETRIES 64
#define MAX_CMD_PER_LUN 2
#define MAX_TAGGED_CMD_PER_LUN (MAX_MAILBOXES - MAX_CMD_PER_LUN)
#define SKIP ULONG_MAX
#define FALSE 0
#define TRUE 1
#define FREE 0
#define IN_USE   1
#define LOCKED   2
#define IN_RESET 3
#define IGNORE   4
#define READY    5
#define ABORTING 6
#define NO_DMA  0xff
#define MAXLOOP  10000
#define REG_LCL_MASK      0
#define REG_LCL_INTR      1
#define REG_SYS_MASK      2
#define REG_SYS_INTR      3
#define REG_PRODUCT_ID1   4
#define REG_PRODUCT_ID2   5
#define REG_CONFIG1       6
#define REG_CONFIG2       7
#define REG_OGM           8
#define REG_ICM           12
#define REGION_SIZE       13
#define BSY_ASSERTED      0x01
#define IRQ_ASSERTED      0x01
#define CMD_RESET         0xc0
#define CMD_OGM_INTR      0x01
#define CMD_CLR_INTR      0x01
#define CMD_ENA_INTR      0x81
#define ASOK              0x00
#define ASST              0x91
#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr)[0])
#define YESNO(a) ((a) ? 'y' : 'n')
#define TLDEV(type) ((type) == TYPE_DISK || (type) == TYPE_ROM)
#define PACKED          __attribute__((packed))
struct sg_list {
unsigned int address;
unsigned int num_bytes;
};
struct mscp {
unsigned char opcode: 3;
unsigned char xdir: 2;
unsigned char dcn: 1;
unsigned char ca: 1;
unsigned char sg: 1;
unsigned char target: 3;
unsigned char channel: 2;
unsigned char lun: 3;
unsigned int data_address PACKED;
unsigned int data_len PACKED;
unsigned int link_address PACKED;
unsigned char clink_id;
unsigned char use_sg;
unsigned char sense_len;
unsigned char scsi_cdbs_len;
unsigned char scsi_cdbs[12];
unsigned char adapter_status;
unsigned char target_status;
unsigned int sense_addr PACKED;
Scsi_Cmnd *SCpnt;
unsigned int index;
struct sg_list *sglist;
};
struct hostdata {
struct mscp cp[MAX_MAILBOXES];
unsigned int cp_stat[MAX_MAILBOXES];
unsigned int last_cp_used;
unsigned int iocount;
int board_number;
char board_name[16];
char board_id[256];
int in_reset;
int target_to[MAX_TARGET][MAX_CHANNEL];
int target_redo[MAX_TARGET][MAX_CHANNEL];
unsigned int retries;
unsigned long last_retried_pid;
unsigned char subversion;
unsigned char heads;
unsigned char sectors;
unsigned char slot;
};
static struct Scsi_Host *sh[MAX_BOARDS + 1];
static const char *driver_name = "Ux4F";
static char sha[MAX_BOARDS];
static unsigned int num_boards = MAX_BOARDS;
static unsigned long io_port[] __initdata = {
SKIP,    SKIP,   SKIP,   SKIP,   SKIP,   SKIP,   SKIP,   SKIP,
SKIP,    SKIP,
0x330, 0x340, 0x230, 0x240, 0x210, 0x130, 0x140,
0x0
};
#define HD(board) ((struct hostdata *) &sh[board]->hostdata)
#define BN(board) (HD(board)->board_name)
#define SWAP_BYTE(x) ((unsigned long)( \
(((unsigned long)(x) & 0x000000ffU) << 24) | \
(((unsigned long)(x) & 0x0000ff00U) <<  8) | \
(((unsigned long)(x) & 0x00ff0000U) >>  8) | \
(((unsigned long)(x) & 0xff000000U) >> 24)))
#if defined(__BIG_ENDIAN)
#define H2DEV(x) SWAP_BYTE(x)
#else
#define H2DEV(x) (x)
#endif
#define DEV2H(x) H2DEV(x)
#define V2DEV(addr) ((addr) ? H2DEV(virt_to_bus((void *)addr)) : 0)
#define DEV2V(addr) ((addr) ? DEV2H(bus_to_virt((unsigned long)addr)) : 0)
static void do_interrupt_handler(int, void *, struct pt_regs *);
static void flush_dev(Scsi_Device *, unsigned long, unsigned int, unsigned int);
static int do_trace = FALSE;
static int setup_done = FALSE;
static int link_statistics = 0;
static int ext_tran = FALSE;
#if defined(HAVE_OLD_UX4F_FIRMWARE)
static int have_old_firmware = TRUE;
#else
static int have_old_firmware = FALSE;
#endif
#if defined(CONFIG_SCSI_U14_34F_LINKED_COMMANDS)
static int linked_comm = TRUE;
#else
static int linked_comm = FALSE;
#endif
#if defined(CONFIG_SCSI_U14_34F_MAX_TAGS)
static int max_queue_depth = CONFIG_SCSI_U14_34F_MAX_TAGS;
#else
static int max_queue_depth = MAX_CMD_PER_LUN;
#endif
static void select_queue_depths(struct Scsi_Host *host, Scsi_Device *devlist) {
Scsi_Device *dev;
int j, ntag = 0, nuntag = 0, tqd, utqd;
IRQ_FLAGS
IRQ_LOCK_SAVE
j = ((struct hostdata *) host->hostdata)->board_number;
for(dev = devlist; dev; dev = dev->next) {
if (dev->host != host) continue;
if (TLDEV(dev->type) && (dev->tagged_supported || linked_comm))
ntag++;
else
nuntag++;
}
utqd = MAX_CMD_PER_LUN;
tqd = (host->can_queue - utqd * nuntag) / (ntag ? ntag : 1);
if (tqd > max_queue_depth) tqd = max_queue_depth;
if (tqd < MAX_CMD_PER_LUN) tqd = MAX_CMD_PER_LUN;
for(dev = devlist; dev; dev = dev->next) {
char *tag_suffix = "", *link_suffix = "";
if (dev->host != host) continue;
if (TLDEV(dev->type) && (dev->tagged_supported || linked_comm))
dev->queue_depth = tqd;
else
dev->queue_depth = utqd;
if (TLDEV(dev->type)) {
if (linked_comm && dev->queue_depth > 2)
link_suffix = ", sorted";
else
link_suffix = ", unsorted";
}
if (dev->tagged_supported && TLDEV(dev->type) && dev->tagged_queue)
tag_suffix = ", tagged";
else if (dev->tagged_supported && TLDEV(dev->type))
tag_suffix = ", untagged";
printk("%s: scsi%d, channel %d, id %d, lun %d, cmds/lun %d%s%s.\n",
BN(j), host->host_no, dev->channel, dev->id, dev->lun,
dev->queue_depth, link_suffix, tag_suffix);
}
IRQ_UNLOCK_RESTORE
return;
}
static inline int wait_on_busy(unsigned long iobase, unsigned int loop) {
while (inb(iobase + REG_LCL_INTR) & BSY_ASSERTED) {
udelay(1L);
if (--loop == 0) return TRUE;
}
return FALSE;
}
static int board_inquiry(unsigned int j) {
struct mscp *cpp;
unsigned int time, limit = 0;
cpp = &HD(j)->cp[0];
memset(cpp, 0, sizeof(struct mscp));
cpp->opcode = OP_HOST_ADAPTER;
cpp->xdir = DTD_IN;
cpp->data_address = V2DEV(HD(j)->board_id);
cpp->data_len = H2DEV(sizeof(HD(j)->board_id));
cpp->scsi_cdbs_len = 6;
cpp->scsi_cdbs[0] = HA_CMD_INQUIRY;
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
printk("%s: board_inquiry, adapter busy.\n", BN(j));
return TRUE;
}
HD(j)->cp_stat[0] = IGNORE;
outb(CMD_CLR_INTR, sh[j]->io_port + REG_SYS_INTR);
outl(V2DEV(cpp), sh[j]->io_port + REG_OGM);
outb(CMD_OGM_INTR, sh[j]->io_port + REG_LCL_INTR);
SPIN_UNLOCK
IRQ_UNLOCK
time = jiffies;
while ((jiffies - time) < HZ && limit++ < 20000) udelay(100L);
IRQ_LOCK
SPIN_LOCK
if (cpp->adapter_status || HD(j)->cp_stat[0] != FREE) {
HD(j)->cp_stat[0] = FREE;
printk("%s: board_inquiry, err 0x%x.\n", BN(j), cpp->adapter_status);
return TRUE;
}
return FALSE;
}
__initfunc (static inline int port_detect \
(unsigned long port_base, unsigned int j, Scsi_Host_Template *tpnt)) {
unsigned char irq, dma_channel, subversion, i;
unsigned char in_byte;
char *bus_type, dma_name[16];
void *bios_segment_table[8] = {
NULL,
(void *) 0xc4000, (void *) 0xc8000, (void *) 0xcc000, (void *) 0xd0000,
(void *) 0xd4000, (void *) 0xd8000, (void *) 0xdc000
};
unsigned char interrupt_table[4] = { 15, 14, 11, 10 };
unsigned char dma_channel_table[4] = { 5, 6, 7, 0 };
struct {
unsigned char heads;
unsigned char sectors;
} mapping_table[4] = {
{ 16, 63 }, { 64, 32 }, { 64, 63 }, { 64, 32 }
};
struct config_1 {
unsigned char bios_segment: 3;
unsigned char removable_disks_as_fixed: 1;
unsigned char interrupt: 2;
unsigned char dma_channel: 2;
} config_1;
struct config_2 {
unsigned char ha_scsi_id: 3;
unsigned char mapping_mode: 2;
unsigned char bios_drive_number: 1;
unsigned char tfr_port: 2;
} config_2;
char name[16];
sprintf(name, "%s%d", driver_name, j);
if(check_region(port_base, REGION_SIZE)) {
printk("%s: address 0x%03lx in use, skipping probe.\n", name, port_base);
return FALSE;
}
if (inb(port_base + REG_PRODUCT_ID1) != PRODUCT_ID1) return FALSE;
in_byte = inb(port_base + REG_PRODUCT_ID2);
if ((in_byte & 0xf0) != PRODUCT_ID2) return FALSE;
*(char *)&config_1 = inb(port_base + REG_CONFIG1);
*(char *)&config_2 = inb(port_base + REG_CONFIG2);
irq = interrupt_table[config_1.interrupt];
dma_channel = dma_channel_table[config_1.dma_channel];
subversion = (in_byte & 0x0f);
if (request_irq(irq, do_interrupt_handler,
SA_INTERRUPT | ((subversion == ESA) ? SA_SHIRQ : 0),
driver_name, (void *) &sha[j])) {
printk("%s: unable to allocate IRQ %u, detaching.\n", name, irq);
return FALSE;
}
if (subversion == ISA && request_dma(dma_channel, driver_name)) {
printk("%s: unable to allocate DMA channel %u, detaching.\n",
name, dma_channel);
free_irq(irq, &sha[j]);
return FALSE;
}
if (have_old_firmware) tpnt->use_clustering = DISABLE_CLUSTERING;
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,101)
tpnt->use_new_eh_code = use_new_eh_code;
#else
use_new_eh_code = FALSE;
#endif
sh[j] = scsi_register(tpnt, sizeof(struct hostdata));
if (sh[j] == NULL) {
printk("%s: unable to register host, detaching.\n", name);
free_irq(irq, &sha[j]);
if (subversion == ISA) free_dma(dma_channel);
return FALSE;
}
sh[j]->io_port = port_base;
sh[j]->unique_id = port_base;
sh[j]->n_io_port = REGION_SIZE;
sh[j]->base = bios_segment_table[config_1.bios_segment];
sh[j]->irq = irq;
sh[j]->sg_tablesize = MAX_SGLIST;
sh[j]->this_id = config_2.ha_scsi_id;
sh[j]->can_queue = MAX_MAILBOXES;
sh[j]->cmd_per_lun = MAX_CMD_PER_LUN;
sh[j]->select_queue_depths = select_queue_depths;
#if defined(DEBUG_DETECT)
{
unsigned char sys_mask, lcl_mask;
sys_mask = inb(sh[j]->io_port + REG_SYS_MASK);
lcl_mask = inb(sh[j]->io_port + REG_LCL_MASK);
printk("SYS_MASK 0x%x, LCL_MASK 0x%x.\n", sys_mask, lcl_mask);
}
#endif
if (sh[j]->this_id == 0) sh[j]->this_id = -1;
if (sh[j]->base == 0) outb(CMD_ENA_INTR, sh[j]->io_port + REG_SYS_MASK);
request_region(sh[j]->io_port, sh[j]->n_io_port, driver_name);
memset(HD(j), 0, sizeof(struct hostdata));
HD(j)->heads = mapping_table[config_2.mapping_mode].heads;
HD(j)->sectors = mapping_table[config_2.mapping_mode].sectors;
HD(j)->subversion = subversion;
HD(j)->board_number = j;
if (have_old_firmware) sh[j]->sg_tablesize = MAX_SAFE_SGLIST;
if (HD(j)->subversion == ESA) {
sh[j]->unchecked_isa_dma = FALSE;
sh[j]->dma_channel = NO_DMA;
sprintf(BN(j), "U34F%d", j);
bus_type = "VESA";
}
else {
sh[j]->wish_block = TRUE;
sh[j]->unchecked_isa_dma = TRUE;
disable_dma(dma_channel);
clear_dma_ff(dma_channel);
set_dma_mode(dma_channel, DMA_MODE_CASCADE);
enable_dma(dma_channel);
sh[j]->dma_channel = dma_channel;
sprintf(BN(j), "U14F%d", j);
bus_type = "ISA";
}
sh[j]->max_channel = MAX_CHANNEL - 1;
sh[j]->max_id = MAX_TARGET;
sh[j]->max_lun = MAX_LUN;
if (HD(j)->subversion == ISA && !board_inquiry(j)) {
HD(j)->board_id[40] = 0;
if (strcmp(&HD(j)->board_id[32], "06000600")) {
printk("%s: %s.\n", BN(j), &HD(j)->board_id[8]);
printk("%s: firmware %s is outdated, FW PROM should be 28004-006.\n",
BN(j), &HD(j)->board_id[32]);
sh[j]->hostt->use_clustering = DISABLE_CLUSTERING;
sh[j]->sg_tablesize = MAX_SAFE_SGLIST;
}
}
if (dma_channel == NO_DMA) sprintf(dma_name, "%s", "BMST");
else                       sprintf(dma_name, "DMA %u", dma_channel);
for (i = 0; i < sh[j]->can_queue; i++)
if (! ((&HD(j)->cp[i])->sglist = kmalloc(
sh[j]->sg_tablesize * sizeof(struct sg_list),
(sh[j]->unchecked_isa_dma ? GFP_DMA : 0) | GFP_ATOMIC))) {
printk("%s: kmalloc SGlist failed, mbox %d, detaching.\n", BN(j), i);
u14_34f_release(sh[j]);
return FALSE;
}
if (max_queue_depth > MAX_TAGGED_CMD_PER_LUN)
max_queue_depth = MAX_TAGGED_CMD_PER_LUN;
if (max_queue_depth < MAX_CMD_PER_LUN) max_queue_depth = MAX_CMD_PER_LUN;
if (j == 0) {
printk("UltraStor 14F/34F: Copyright (C) 1994-1998 Dario Ballabio.\n");
printk("%s config options -> of:%c, lc:%c, mq:%d, eh:%c, et:%c.\n",
driver_name, YESNO(have_old_firmware), YESNO(linked_comm),
max_queue_depth, YESNO(use_new_eh_code), YESNO(ext_tran));
}
printk("%s: %s 0x%03lx, BIOS 0x%05x, IRQ %u, %s, SG %d, MB %d.\n",
BN(j), bus_type, (unsigned long)sh[j]->io_port, (int)sh[j]->base,
sh[j]->irq, dma_name, sh[j]->sg_tablesize, sh[j]->can_queue);
if (sh[j]->max_id > 8 || sh[j]->max_lun > 8)
printk("%s: wide SCSI support enabled, max_id %u, max_lun %u.\n",
BN(j), sh[j]->max_id, sh[j]->max_lun);
for (i = 0; i <= sh[j]->max_channel; i++)
printk("%s: SCSI channel %u enabled, host target ID %d.\n",
BN(j), i, sh[j]->this_id);
return TRUE;
}
__initfunc (void u14_34f_setup(char *str, int *ints)) {
int i, argc = ints[0];
char *cur = str, *pc;
if (argc > 0) {
if (argc > MAX_INT_PARAM) argc = MAX_INT_PARAM;
for (i = 0; i < argc; i++) io_port[i] = ints[i + 1];
io_port[i] = 0;
setup_done = TRUE;
}
while (cur && (pc = strchr(cur, ':'))) {
int val = 0, c = *++pc;
if (c == 'n' || c == 'N') val = FALSE;
else if (c == 'y' || c == 'Y') val = TRUE;
else val = (int) simple_strtoul(pc, NULL, 0);
if (!strncmp(cur, "lc:", 3)) linked_comm = val;
else if (!strncmp(cur, "of:", 3)) have_old_firmware = val;
else if (!strncmp(cur, "mq:", 3))  max_queue_depth = val;
else if (!strncmp(cur, "ls:", 3))  link_statistics = val;
else if (!strncmp(cur, "eh:", 3))  use_new_eh_code = val;
else if (!strncmp(cur, "et:", 3))  ext_tran = val;
if ((cur = strchr(cur, ','))) ++cur;
}
return;
}
__initfunc (int u14_34f_detect(Scsi_Host_Template *tpnt)) {
unsigned int j = 0, k;
IRQ_FLAGS
IRQ_LOCK_SAVE
tpnt->proc_dir = &proc_scsi_u14_34f;
#if defined(MODULE)
if(io_port[0] != SKIP) {
setup_done = TRUE;
io_port[MAX_INT_PARAM] = 0;
}
#endif
for (k = 0; k < MAX_BOARDS + 1; k++) sh[k] = NULL;
for (k = 0; io_port[k]; k++) {
if (io_port[k] == SKIP) continue;
if (j < MAX_BOARDS && port_detect(io_port[k], j, tpnt)) j++;
}
num_boards = j;
IRQ_UNLOCK_RESTORE
return j;
}
static inline void build_sg_list(struct mscp *cpp, Scsi_Cmnd *SCpnt) {
unsigned int k, data_len = 0;
struct scatterlist *sgpnt;
sgpnt = (struct scatterlist *) SCpnt->request_buffer;
for (k = 0; k < SCpnt->use_sg; k++) {
cpp->sglist[k].address = V2DEV(sgpnt[k].address);
cpp->sglist[k].num_bytes = H2DEV(sgpnt[k].length);
data_len += sgpnt[k].length;
}
cpp->use_sg = SCpnt->use_sg;
cpp->data_address = V2DEV(cpp->sglist);
cpp->data_len = H2DEV(data_len);
}
static inline int do_qcomm(Scsi_Cmnd *SCpnt, void (*done)(Scsi_Cmnd *)) {
unsigned int i, j, k;
struct mscp *cpp;
static const unsigned char data_out_cmds[] = {
0x0a, 0x2a, 0x15, 0x55, 0x04, 0x07, 0x18, 0x1d, 0x24, 0x2e,
0x30, 0x31, 0x32, 0x38, 0x39, 0x3a, 0x3b, 0x3d, 0x3f, 0x40,
0x41, 0x4c, 0xaa, 0xae, 0xb0, 0xb1, 0xb2, 0xb6, 0xea, 0x1b
};
static const unsigned char data_none_cmds[] = {
0x01, 0x0b, 0x10, 0x11, 0x13, 0x16, 0x17, 0x19, 0x2b, 0x1e,
0x2c, 0xac, 0x2f, 0xaf, 0x33, 0xb3, 0x35, 0x36, 0x45, 0x47,
0x48, 0x49, 0xa9, 0x4b, 0xa5, 0xa6, 0xb5
};
j = ((struct hostdata *) SCpnt->host->hostdata)->board_number;
if (SCpnt->host_scribble)
panic("%s: qcomm, pid %ld, SCpnt %p already active.\n",
BN(j), SCpnt->pid, SCpnt);
i = HD(j)->last_cp_used + 1;
for (k = 0; k < sh[j]->can_queue; k++, i++) {
if (i >= sh[j]->can_queue) i = 0;
if (HD(j)->cp_stat[i] == FREE) {
HD(j)->last_cp_used = i;
break;
}
}
if (k == sh[j]->can_queue) {
printk("%s: qcomm, no free mailbox.\n", BN(j));
return 1;
}
cpp = &HD(j)->cp[i];
memset(cpp, 0, sizeof(struct mscp) - sizeof(struct sg_list *));
SCpnt->scsi_done = done;
cpp->index = i;
SCpnt->host_scribble = (unsigned char *) &cpp->index;
if (do_trace) printk("%s: qcomm, mbox %d, target %d.%d:%d, pid %ld.\n",
BN(j), i, SCpnt->channel, SCpnt->target,
SCpnt->lun, SCpnt->pid);
cpp->xdir = DTD_IN;
for (k = 0; k < ARRAY_SIZE(data_out_cmds); k++)
if (SCpnt->cmnd[0] == data_out_cmds[k]) {
cpp->xdir = DTD_OUT;
break;
}
if (cpp->xdir == DTD_IN)
for (k = 0; k < ARRAY_SIZE(data_none_cmds); k++)
if (SCpnt->cmnd[0] == data_none_cmds[k]) {
cpp->xdir = DTD_NONE;
break;
}
cpp->opcode = OP_SCSI;
cpp->channel = SCpnt->channel;
cpp->target = SCpnt->target;
cpp->lun = SCpnt->lun;
cpp->SCpnt = SCpnt;
cpp->sense_addr = V2DEV(SCpnt->sense_buffer);
cpp->sense_len = sizeof SCpnt->sense_buffer;
if (SCpnt->use_sg) {
cpp->sg = TRUE;
build_sg_list(cpp, SCpnt);
}
else {
cpp->data_address = V2DEV(SCpnt->request_buffer);
cpp->data_len = H2DEV(SCpnt->request_bufflen);
}
cpp->scsi_cdbs_len = SCpnt->cmd_len;
memcpy(cpp->scsi_cdbs, SCpnt->cmnd, cpp->scsi_cdbs_len);
if (linked_comm && SCpnt->device->queue_depth > 2
&& TLDEV(SCpnt->device->type)) {
HD(j)->cp_stat[i] = READY;
flush_dev(SCpnt->device, SCpnt->request.sector, j, FALSE);
return 0;
}
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
SCpnt->host_scribble = NULL;
printk("%s: qcomm, target %d.%d:%d, pid %ld, adapter busy.\n",
BN(j), SCpnt->channel, SCpnt->target, SCpnt->lun, SCpnt->pid);
return 1;
}
outl(V2DEV(cpp), sh[j]->io_port + REG_OGM);
outb(CMD_OGM_INTR, sh[j]->io_port + REG_LCL_INTR);
HD(j)->cp_stat[i] = IN_USE;
return 0;
}
int u14_34f_queuecommand(Scsi_Cmnd *SCpnt, void (*done)(Scsi_Cmnd *)) {
int rtn;
IRQ_FLAGS
IRQ_LOCK_SAVE
rtn = do_qcomm(SCpnt, done);
IRQ_UNLOCK_RESTORE
return rtn;
}
static inline int do_old_abort(Scsi_Cmnd *SCarg) {
unsigned int i, j;
j = ((struct hostdata *) SCarg->host->hostdata)->board_number;
if (SCarg->host_scribble == NULL ||
(SCarg->serial_number_at_timeout &&
(SCarg->serial_number != SCarg->serial_number_at_timeout))) {
printk("%s: abort, target %d.%d:%d, pid %ld inactive.\n",
BN(j), SCarg->channel, SCarg->target, SCarg->lun, SCarg->pid);
return SCSI_ABORT_NOT_RUNNING;
}
i = *(unsigned int *)SCarg->host_scribble;
printk("%s: abort, mbox %d, target %d.%d:%d, pid %ld.\n",
BN(j), i, SCarg->channel, SCarg->target, SCarg->lun, SCarg->pid);
if (i >= sh[j]->can_queue)
panic("%s: abort, invalid SCarg->host_scribble.\n", BN(j));
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
printk("%s: abort, timeout error.\n", BN(j));
return SCSI_ABORT_ERROR;
}
if (HD(j)->cp_stat[i] == FREE) {
printk("%s: abort, mbox %d is free.\n", BN(j), i);
return SCSI_ABORT_NOT_RUNNING;
}
if (HD(j)->cp_stat[i] == IN_USE) {
printk("%s: abort, mbox %d is in use.\n", BN(j), i);
if (SCarg != HD(j)->cp[i].SCpnt)
panic("%s: abort, mbox %d, SCarg %p, cp SCpnt %p.\n",
BN(j), i, SCarg, HD(j)->cp[i].SCpnt);
if (inb(sh[j]->io_port + REG_SYS_INTR) & IRQ_ASSERTED)
printk("%s: abort, mbox %d, interrupt pending.\n", BN(j), i);
return SCSI_ABORT_SNOOZE;
}
if (HD(j)->cp_stat[i] == IN_RESET) {
printk("%s: abort, mbox %d is in reset.\n", BN(j), i);
return SCSI_ABORT_ERROR;
}
if (HD(j)->cp_stat[i] == LOCKED) {
printk("%s: abort, mbox %d is locked.\n", BN(j), i);
return SCSI_ABORT_NOT_RUNNING;
}
if (HD(j)->cp_stat[i] == READY || HD(j)->cp_stat[i] == ABORTING) {
SCarg->result = DID_ABORT << 16;
SCarg->host_scribble = NULL;
HD(j)->cp_stat[i] = FREE;
printk("%s, abort, mbox %d ready, DID_ABORT, pid %ld done.\n",
BN(j), i, SCarg->pid);
SCarg->scsi_done(SCarg);
return SCSI_ABORT_SUCCESS;
}
panic("%s: abort, mbox %d, invalid cp_stat.\n", BN(j), i);
}
int u14_34f_old_abort(Scsi_Cmnd *SCarg) {
int rtn;
IRQ_FLAGS
IRQ_LOCK_SAVE
rtn = do_old_abort(SCarg);
IRQ_UNLOCK_RESTORE
return rtn;
}
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,101)
static inline int do_abort(Scsi_Cmnd *SCarg) {
unsigned int i, j;
j = ((struct hostdata *) SCarg->host->hostdata)->board_number;
if (SCarg->host_scribble == NULL) {
printk("%s: abort, target %d.%d:%d, pid %ld inactive.\n",
BN(j), SCarg->channel, SCarg->target, SCarg->lun, SCarg->pid);
return SUCCESS;
}
i = *(unsigned int *)SCarg->host_scribble;
printk("%s: abort, mbox %d, target %d.%d:%d, pid %ld.\n",
BN(j), i, SCarg->channel, SCarg->target, SCarg->lun, SCarg->pid);
if (i >= sh[j]->can_queue)
panic("%s: abort, invalid SCarg->host_scribble.\n", BN(j));
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
printk("%s: abort, timeout error.\n", BN(j));
return FAILED;
}
if (HD(j)->cp_stat[i] == FREE) {
printk("%s: abort, mbox %d is free.\n", BN(j), i);
return SUCCESS;
}
if (HD(j)->cp_stat[i] == IN_USE) {
printk("%s: abort, mbox %d is in use.\n", BN(j), i);
if (SCarg != HD(j)->cp[i].SCpnt)
panic("%s: abort, mbox %d, SCarg %p, cp SCpnt %p.\n",
BN(j), i, SCarg, HD(j)->cp[i].SCpnt);
if (inb(sh[j]->io_port + REG_SYS_INTR) & IRQ_ASSERTED)
printk("%s: abort, mbox %d, interrupt pending.\n", BN(j), i);
if (SCarg->eh_state == SCSI_STATE_TIMEOUT) {
SCarg->host_scribble = NULL;
HD(j)->cp_stat[i] = FREE;
printk("%s, abort, mbox %d, eh_state timeout, pid %ld.\n",
BN(j), i, SCarg->pid);
return SUCCESS;
}
return FAILED;
}
if (HD(j)->cp_stat[i] == IN_RESET) {
printk("%s: abort, mbox %d is in reset.\n", BN(j), i);
return FAILED;
}
if (HD(j)->cp_stat[i] == LOCKED) {
printk("%s: abort, mbox %d is locked.\n", BN(j), i);
return SUCCESS;
}
if (HD(j)->cp_stat[i] == READY || HD(j)->cp_stat[i] == ABORTING) {
SCarg->result = DID_ABORT << 16;
SCarg->host_scribble = NULL;
HD(j)->cp_stat[i] = FREE;
printk("%s, abort, mbox %d ready, DID_ABORT, pid %ld done.\n",
BN(j), i, SCarg->pid);
SCarg->scsi_done(SCarg);
return SUCCESS;
}
panic("%s: abort, mbox %d, invalid cp_stat.\n", BN(j), i);
}
int u14_34f_abort(Scsi_Cmnd *SCarg) {
return do_abort(SCarg);
}
#endif
static inline int do_old_reset(Scsi_Cmnd *SCarg) {
unsigned int i, j, time, k, c, limit = 0;
int arg_done = FALSE;
Scsi_Cmnd *SCpnt;
j = ((struct hostdata *) SCarg->host->hostdata)->board_number;
printk("%s: reset, enter, target %d.%d:%d, pid %ld.\n",
BN(j), SCarg->channel, SCarg->target, SCarg->lun, SCarg->pid);
if (SCarg->host_scribble == NULL)
printk("%s: reset, pid %ld inactive.\n", BN(j), SCarg->pid);
if (SCarg->serial_number_at_timeout &&
(SCarg->serial_number != SCarg->serial_number_at_timeout)) {
printk("%s: reset, pid %ld, reset not running.\n", BN(j), SCarg->pid);
return SCSI_RESET_NOT_RUNNING;
}
if (HD(j)->in_reset) {
printk("%s: reset, exit, already in reset.\n", BN(j));
return SCSI_RESET_ERROR;
}
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
printk("%s: reset, exit, timeout error.\n", BN(j));
return SCSI_RESET_ERROR;
}
HD(j)->retries = 0;
for (c = 0; c <= sh[j]->max_channel; c++)
for (k = 0; k < sh[j]->max_id; k++) {
HD(j)->target_redo[k][c] = TRUE;
HD(j)->target_to[k][c] = 0;
}
for (i = 0; i < sh[j]->can_queue; i++) {
if (HD(j)->cp_stat[i] == FREE) continue;
if (HD(j)->cp_stat[i] == LOCKED) {
HD(j)->cp_stat[i] = FREE;
printk("%s: reset, locked mbox %d forced free.\n", BN(j), i);
continue;
}
if (!(SCpnt = HD(j)->cp[i].SCpnt))
panic("%s: reset, mbox %d, SCpnt == NULL.\n", BN(j), i);
if (HD(j)->cp_stat[i] == READY || HD(j)->cp_stat[i] == ABORTING) {
HD(j)->cp_stat[i] = ABORTING;
printk("%s: reset, mbox %d aborting, pid %ld.\n",
BN(j), i, SCpnt->pid);
}
else {
HD(j)->cp_stat[i] = IN_RESET;
printk("%s: reset, mbox %d in reset, pid %ld.\n",
BN(j), i, SCpnt->pid);
}
if (SCpnt->host_scribble == NULL)
panic("%s: reset, mbox %d, garbled SCpnt.\n", BN(j), i);
if (*(unsigned int *)SCpnt->host_scribble != i)
panic("%s: reset, mbox %d, index mismatch.\n", BN(j), i);
if (SCpnt->scsi_done == NULL)
panic("%s: reset, mbox %d, SCpnt->scsi_done == NULL.\n", BN(j), i);
if (SCpnt == SCarg) arg_done = TRUE;
}
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
printk("%s: reset, cannot reset, timeout error.\n", BN(j));
return SCSI_RESET_ERROR;
}
outb(CMD_RESET, sh[j]->io_port + REG_LCL_INTR);
printk("%s: reset, board reset done, enabling interrupts.\n", BN(j));
#if defined(DEBUG_RESET)
do_trace = TRUE;
#endif
HD(j)->in_reset = TRUE;
SPIN_UNLOCK
IRQ_UNLOCK
time = jiffies;
while ((jiffies - time) < (10 * HZ) && limit++ < 200000) udelay(100L);
IRQ_LOCK
SPIN_LOCK
printk("%s: reset, interrupts disabled, loops %d.\n", BN(j), limit);
for (i = 0; i < sh[j]->can_queue; i++) {
if (HD(j)->cp_stat[i] == IN_RESET) {
SCpnt = HD(j)->cp[i].SCpnt;
SCpnt->result = DID_RESET << 16;
SCpnt->host_scribble = NULL;
HD(j)->cp_stat[i] = LOCKED;
printk("%s, reset, mbox %d locked, DID_RESET, pid %ld done.\n",
BN(j), i, SCpnt->pid);
}
else if (HD(j)->cp_stat[i] == ABORTING) {
SCpnt = HD(j)->cp[i].SCpnt;
SCpnt->result = DID_RESET << 16;
SCpnt->host_scribble = NULL;
HD(j)->cp_stat[i] = FREE;
printk("%s, reset, mbox %d aborting, DID_RESET, pid %ld done.\n",
BN(j), i, SCpnt->pid);
}
else
continue;
SCpnt->scsi_done(SCpnt);
IRQ_LOCK
}
HD(j)->in_reset = FALSE;
do_trace = FALSE;
if (arg_done) {
printk("%s: reset, exit, success.\n", BN(j));
return SCSI_RESET_SUCCESS;
}
else {
printk("%s: reset, exit, wakeup.\n", BN(j));
return SCSI_RESET_PUNT;
}
}
int u14_34f_old_reset(Scsi_Cmnd *SCarg, unsigned int reset_flags) {
int rtn;
IRQ_FLAGS
IRQ_LOCK_SAVE
rtn = do_old_reset(SCarg);
IRQ_UNLOCK_RESTORE
return rtn;
}
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,101)
static inline int do_reset(Scsi_Cmnd *SCarg) {
unsigned int i, j, time, k, c, limit = 0;
int arg_done = FALSE;
Scsi_Cmnd *SCpnt;
j = ((struct hostdata *) SCarg->host->hostdata)->board_number;
printk("%s: reset, enter, target %d.%d:%d, pid %ld.\n",
BN(j), SCarg->channel, SCarg->target, SCarg->lun, SCarg->pid);
if (SCarg->host_scribble == NULL)
printk("%s: reset, pid %ld inactive.\n", BN(j), SCarg->pid);
if (HD(j)->in_reset) {
printk("%s: reset, exit, already in reset.\n", BN(j));
return FAILED;
}
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
printk("%s: reset, exit, timeout error.\n", BN(j));
return FAILED;
}
HD(j)->retries = 0;
for (c = 0; c <= sh[j]->max_channel; c++)
for (k = 0; k < sh[j]->max_id; k++) {
HD(j)->target_redo[k][c] = TRUE;
HD(j)->target_to[k][c] = 0;
}
for (i = 0; i < sh[j]->can_queue; i++) {
if (HD(j)->cp_stat[i] == FREE) continue;
if (HD(j)->cp_stat[i] == LOCKED) {
HD(j)->cp_stat[i] = FREE;
printk("%s: reset, locked mbox %d forced free.\n", BN(j), i);
continue;
}
if (!(SCpnt = HD(j)->cp[i].SCpnt))
panic("%s: reset, mbox %d, SCpnt == NULL.\n", BN(j), i);
if (HD(j)->cp_stat[i] == READY || HD(j)->cp_stat[i] == ABORTING) {
HD(j)->cp_stat[i] = ABORTING;
printk("%s: reset, mbox %d aborting, pid %ld.\n",
BN(j), i, SCpnt->pid);
}
else {
HD(j)->cp_stat[i] = IN_RESET;
printk("%s: reset, mbox %d in reset, pid %ld.\n",
BN(j), i, SCpnt->pid);
}
if (SCpnt->host_scribble == NULL)
panic("%s: reset, mbox %d, garbled SCpnt.\n", BN(j), i);
if (*(unsigned int *)SCpnt->host_scribble != i)
panic("%s: reset, mbox %d, index mismatch.\n", BN(j), i);
if (SCpnt->scsi_done == NULL)
panic("%s: reset, mbox %d, SCpnt->scsi_done == NULL.\n", BN(j), i);
if (SCpnt == SCarg) arg_done = TRUE;
}
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
printk("%s: reset, cannot reset, timeout error.\n", BN(j));
return FAILED;
}
outb(CMD_RESET, sh[j]->io_port + REG_LCL_INTR);
printk("%s: reset, board reset done, enabling interrupts.\n", BN(j));
#if defined(DEBUG_RESET)
do_trace = TRUE;
#endif
HD(j)->in_reset = TRUE;
SPIN_UNLOCK
IRQ_UNLOCK
time = jiffies;
while ((jiffies - time) < (10 * HZ) && limit++ < 200000) udelay(100L);
IRQ_LOCK
SPIN_LOCK
printk("%s: reset, interrupts disabled, loops %d.\n", BN(j), limit);
for (i = 0; i < sh[j]->can_queue; i++) {
if (HD(j)->cp_stat[i] == IN_RESET) {
SCpnt = HD(j)->cp[i].SCpnt;
SCpnt->result = DID_RESET << 16;
SCpnt->host_scribble = NULL;
HD(j)->cp_stat[i] = LOCKED;
printk("%s, reset, mbox %d locked, DID_RESET, pid %ld done.\n",
BN(j), i, SCpnt->pid);
}
else if (HD(j)->cp_stat[i] == ABORTING) {
SCpnt = HD(j)->cp[i].SCpnt;
SCpnt->result = DID_RESET << 16;
SCpnt->host_scribble = NULL;
HD(j)->cp_stat[i] = FREE;
printk("%s, reset, mbox %d aborting, DID_RESET, pid %ld done.\n",
BN(j), i, SCpnt->pid);
}
else
continue;
SCpnt->scsi_done(SCpnt);
IRQ_LOCK
}
HD(j)->in_reset = FALSE;
do_trace = FALSE;
if (arg_done) printk("%s: reset, exit, pid %ld done.\n", BN(j), SCarg->pid);
else          printk("%s: reset, exit.\n", BN(j));
return SUCCESS;
}
int u14_34f_reset(Scsi_Cmnd *SCarg) {
return do_reset(SCarg);
}
#endif
int u14_34f_biosparam(Disk *disk, kdev_t dev, int *dkinfo) {
unsigned int j = 0;
int size = disk->capacity;
dkinfo[0] = HD(j)->heads;
dkinfo[1] = HD(j)->sectors;
dkinfo[2] = size / (HD(j)->heads * HD(j)->sectors);
if (ext_tran && (scsicam_bios_param(disk, dev, dkinfo) < 0)) {
dkinfo[0] = 255;
dkinfo[1] = 63;
dkinfo[2] = size / (dkinfo[0] * dkinfo[1]);
}
#if defined (DEBUG_GEOMETRY)
printk ("%s: biosparam, head=%d, sec=%d, cyl=%d.\n", driver_name,
dkinfo[0], dkinfo[1], dkinfo[2]);
#endif
return FALSE;
}
static void sort(unsigned long sk[], unsigned int da[], unsigned int n,
unsigned int rev) {
unsigned int i, j, k, y;
unsigned long x;
for (i = 0; i < n - 1; i++) {
k = i;
for (j = k + 1; j < n; j++)
if (rev) {
if (sk[j] > sk[k]) k = j;
}
else {
if (sk[j] < sk[k]) k = j;
}
if (k != i) {
x = sk[k]; sk[k] = sk[i]; sk[i] = x;
y = da[k]; da[k] = da[i]; da[i] = y;
}
}
return;
}
static inline int reorder(unsigned int j, unsigned long cursec,
unsigned int ihdlr, unsigned int il[], unsigned int n_ready) {
Scsi_Cmnd *SCpnt;
struct mscp *cpp;
unsigned int k, n;
unsigned int rev = FALSE, s = TRUE, r = TRUE;
unsigned int input_only = TRUE, overlap = FALSE;
unsigned long sl[n_ready], pl[n_ready], ll[n_ready];
unsigned long maxsec = 0, minsec = ULONG_MAX, seek = 0, iseek = 0;
unsigned long ioseek = 0;
static unsigned int flushcount = 0, batchcount = 0, sortcount = 0;
static unsigned int readycount = 0, ovlcount = 0, inputcount = 0;
static unsigned int readysorted = 0, revcount = 0;
static unsigned long seeksorted = 0, seeknosort = 0;
if (link_statistics && !(++flushcount % link_statistics))
printk("fc %d bc %d ic %d oc %d rc %d rs %d sc %d re %d"\
" av %ldK as %ldK.\n", flushcount, batchcount, inputcount,
ovlcount, readycount, readysorted, sortcount, revcount,
seeknosort / (readycount + 1),
seeksorted / (readycount + 1));
if (n_ready <= 1) return FALSE;
for (n = 0; n < n_ready; n++) {
k = il[n]; cpp = &HD(j)->cp[k]; SCpnt = cpp->SCpnt;
if (!(cpp->xdir == DTD_IN)) input_only = FALSE;
if (SCpnt->request.sector < minsec) minsec = SCpnt->request.sector;
if (SCpnt->request.sector > maxsec) maxsec = SCpnt->request.sector;
sl[n] = SCpnt->request.sector;
ioseek += SCpnt->request.nr_sectors;
if (!n) continue;
if (sl[n] < sl[n - 1]) s = FALSE;
if (sl[n] > sl[n - 1]) r = FALSE;
if (link_statistics) {
if (sl[n] > sl[n - 1])
seek += sl[n] - sl[n - 1];
else
seek += sl[n - 1] - sl[n];
}
}
if (link_statistics) {
if (cursec > sl[0]) seek += cursec - sl[0]; else seek += sl[0] - cursec;
}
if (cursec > ((maxsec + minsec) / 2)) rev = TRUE;
if (ioseek > ((maxsec - minsec) / 2)) rev = FALSE;
if (!((rev && r) || (!rev && s))) sort(sl, il, n_ready, rev);
if (!input_only) for (n = 0; n < n_ready; n++) {
k = il[n]; cpp = &HD(j)->cp[k]; SCpnt = cpp->SCpnt;
ll[n] = SCpnt->request.nr_sectors; pl[n] = SCpnt->pid;
if (!n) continue;
if ((sl[n] == sl[n - 1]) || (!rev && ((sl[n - 1] + ll[n - 1]) > sl[n]))
|| (rev && ((sl[n] + ll[n]) > sl[n - 1]))) overlap = TRUE;
}
if (overlap) sort(pl, il, n_ready, FALSE);
if (link_statistics) {
if (cursec > sl[0]) iseek = cursec - sl[0]; else iseek = sl[0] - cursec;
batchcount++; readycount += n_ready, seeknosort += seek / 1024;
if (input_only) inputcount++;
if (overlap) { ovlcount++; seeksorted += iseek / 1024; }
else seeksorted += (iseek + maxsec - minsec) / 1024;
if (rev && !r)     {  revcount++; readysorted += n_ready; }
if (!rev && !s)    { sortcount++; readysorted += n_ready; }
}
#if defined(DEBUG_LINKED_COMMANDS)
if (link_statistics && (overlap || !(flushcount % link_statistics)))
for (n = 0; n < n_ready; n++) {
k = il[n]; cpp = &HD(j)->cp[k]; SCpnt = cpp->SCpnt;
printk("%s %d.%d:%d pid %ld mb %d fc %d nr %d sec %ld ns %ld"\
" cur %ld s:%c r:%c rev:%c in:%c ov:%c xd %d.\n",
(ihdlr ? "ihdlr" : "qcomm"), SCpnt->channel, SCpnt->target,
SCpnt->lun, SCpnt->pid, k, flushcount, n_ready,
SCpnt->request.sector, SCpnt->request.nr_sectors, cursec,
YESNO(s), YESNO(r), YESNO(rev), YESNO(input_only),
YESNO(overlap), cpp->xdir);
}
#endif
return overlap;
}
static void flush_dev(Scsi_Device *dev, unsigned long cursec, unsigned int j,
unsigned int ihdlr) {
Scsi_Cmnd *SCpnt;
struct mscp *cpp;
unsigned int k, n, n_ready = 0, il[MAX_MAILBOXES];
for (k = 0; k < sh[j]->can_queue; k++) {
if (HD(j)->cp_stat[k] != READY && HD(j)->cp_stat[k] != IN_USE) continue;
cpp = &HD(j)->cp[k]; SCpnt = cpp->SCpnt;
if (SCpnt->device != dev) continue;
if (HD(j)->cp_stat[k] == IN_USE) return;
il[n_ready++] = k;
}
if (reorder(j, cursec, ihdlr, il, n_ready)) n_ready = 1;
for (n = 0; n < n_ready; n++) {
k = il[n]; cpp = &HD(j)->cp[k]; SCpnt = cpp->SCpnt;
if (wait_on_busy(sh[j]->io_port, MAXLOOP)) {
printk("%s: %s, target %d.%d:%d, pid %ld, mbox %d, adapter"\
" busy, will abort.\n", BN(j), (ihdlr ? "ihdlr" : "qcomm"),
SCpnt->channel, SCpnt->target, SCpnt->lun, SCpnt->pid, k);
HD(j)->cp_stat[k] = ABORTING;
continue;
}
outl(V2DEV(cpp), sh[j]->io_port + REG_OGM);
outb(CMD_OGM_INTR, sh[j]->io_port + REG_LCL_INTR);
HD(j)->cp_stat[k] = IN_USE;
}
}
static inline void ihdlr(int irq, unsigned int j) {
Scsi_Cmnd *SCpnt;
unsigned int i, k, c, status, tstatus, reg, ret;
struct mscp *spp, *cpp;
if (sh[j]->irq != irq)
panic("%s: ihdlr, irq %d, sh[j]->irq %d.\n", BN(j), irq, sh[j]->irq);
if (!((reg = inb(sh[j]->io_port + REG_SYS_INTR)) & IRQ_ASSERTED)) return;
HD(j)->iocount++;
if (do_trace) printk("%s: ihdlr, enter, irq %d, count %d.\n", BN(j), irq,
HD(j)->iocount);
if (wait_on_busy(sh[j]->io_port, 20 * MAXLOOP)) {
outb(CMD_CLR_INTR, sh[j]->io_port + REG_SYS_INTR);
printk("%s: ihdlr, busy timeout error,  irq %d, reg 0x%x, count %d.\n",
BN(j), irq, reg, HD(j)->iocount);
return;
}
ret = inl(sh[j]->io_port + REG_ICM);
spp = (struct mscp *)DEV2V(ret);
cpp = spp;
outb(CMD_CLR_INTR, sh[j]->io_port + REG_SYS_INTR);
#if defined(DEBUG_GENERATE_ABORTS)
if ((HD(j)->iocount > 500) && ((HD(j)->iocount % 500) < 3)) return;
#endif
i = cpp - HD(j)->cp;
if (cpp < HD(j)->cp || cpp >= HD(j)->cp + sh[j]->can_queue
|| i >= sh[j]->can_queue)
panic("%s: ihdlr, invalid mscp bus address %p, cp0 %p.\n", BN(j),
(void *)ret, HD(j)->cp);
if (HD(j)->cp_stat[i] == IGNORE) {
HD(j)->cp_stat[i] = FREE;
return;
}
else if (HD(j)->cp_stat[i] == LOCKED) {
HD(j)->cp_stat[i] = FREE;
printk("%s: ihdlr, mbox %d unlocked, count %d.\n", BN(j), i,
HD(j)->iocount);
return;
}
else if (HD(j)->cp_stat[i] == FREE) {
printk("%s: ihdlr, mbox %d is free, count %d.\n", BN(j), i,
HD(j)->iocount);
return;
}
else if (HD(j)->cp_stat[i] == IN_RESET)
printk("%s: ihdlr, mbox %d is in reset.\n", BN(j), i);
else if (HD(j)->cp_stat[i] != IN_USE)
panic("%s: ihdlr, mbox %d, invalid cp_stat: %d.\n",
BN(j), i, HD(j)->cp_stat[i]);
HD(j)->cp_stat[i] = FREE;
SCpnt = cpp->SCpnt;
if (SCpnt == NULL) panic("%s: ihdlr, mbox %d, SCpnt == NULL.\n", BN(j), i);
if (SCpnt->host_scribble == NULL)
panic("%s: ihdlr, mbox %d, pid %ld, SCpnt %p garbled.\n", BN(j), i,
SCpnt->pid, SCpnt);
if (*(unsigned int *)SCpnt->host_scribble != i)
panic("%s: ihdlr, mbox %d, pid %ld, index mismatch %d.\n",
BN(j), i, SCpnt->pid, *(unsigned int *)SCpnt->host_scribble);
if (linked_comm && SCpnt->device->queue_depth > 2
&& TLDEV(SCpnt->device->type))
flush_dev(SCpnt->device, SCpnt->request.sector, j, TRUE);
tstatus = status_byte(spp->target_status);
#if defined(DEBUG_GENERATE_ERRORS)
if ((HD(j)->iocount > 500) && ((HD(j)->iocount % 200) < 2))
spp->adapter_status = 0x01;
#endif
switch (spp->adapter_status) {
case ASOK:
if (tstatus == BUSY && SCpnt->device->type != TYPE_TAPE)
status = DID_ERROR << 16;
else if (tstatus != GOOD && SCpnt->device->type == TYPE_DISK
&& HD(j)->target_redo[SCpnt->target][SCpnt->channel])
status = DID_BUS_BUSY << 16;
else if (tstatus == CHECK_CONDITION
&& SCpnt->device->type == TYPE_DISK
&& (SCpnt->sense_buffer[2] & 0xf) == RECOVERED_ERROR)
status = DID_BUS_BUSY << 16;
else
status = DID_OK << 16;
if (tstatus == GOOD)
HD(j)->target_redo[SCpnt->target][SCpnt->channel] = FALSE;
if (spp->target_status && SCpnt->device->type == TYPE_DISK)
printk("%s: ihdlr, target %d.%d:%d, pid %ld, "\
"target_status 0x%x, sense key 0x%x.\n", BN(j),
SCpnt->channel, SCpnt->target, SCpnt->lun,
SCpnt->pid, spp->target_status,
SCpnt->sense_buffer[2]);
HD(j)->target_to[SCpnt->target][SCpnt->channel] = 0;
if (HD(j)->last_retried_pid == SCpnt->pid) HD(j)->retries = 0;
break;
case ASST:
if (HD(j)->target_to[SCpnt->target][SCpnt->channel] > 1)
status = DID_ERROR << 16;
else {
status = DID_TIME_OUT << 16;
HD(j)->target_to[SCpnt->target][SCpnt->channel]++;
}
break;
case 0x93:
case 0x94:
case 0x96:
case 0xa3:
for (c = 0; c <= sh[j]->max_channel; c++)
for (k = 0; k < sh[j]->max_id; k++)
HD(j)->target_redo[k][c] = TRUE;
case 0x92:
if (SCpnt->device->type != TYPE_TAPE
&& HD(j)->retries < MAX_INTERNAL_RETRIES) {
#if defined(DID_SOFT_ERROR)
status = DID_SOFT_ERROR << 16;
#else
status = DID_BUS_BUSY << 16;
#endif
HD(j)->retries++;
HD(j)->last_retried_pid = SCpnt->pid;
}
else
status = DID_ERROR << 16;
break;
case 0x01:
case 0x02:
case 0x03:
case 0x84:
case 0x9b:
case 0x9f:
case 0xff:
default:
status = DID_ERROR << 16;
break;
}
SCpnt->result = status | spp->target_status;
#if defined(DEBUG_INTERRUPT)
if (SCpnt->result || do_trace)
#else
if ((spp->adapter_status != ASOK && HD(j)->iocount >  1000) ||
(spp->adapter_status != ASOK &&
spp->adapter_status != ASST && HD(j)->iocount <= 1000) ||
do_trace || msg_byte(spp->target_status))
#endif
printk("%s: ihdlr, mbox %2d, err 0x%x:%x,"\
" target %d.%d:%d, pid %ld, reg 0x%x, count %d.\n",
BN(j), i, spp->adapter_status, spp->target_status,
SCpnt->channel, SCpnt->target, SCpnt->lun, SCpnt->pid,
reg, HD(j)->iocount);
SCpnt->host_scribble = NULL;
SCpnt->scsi_done(SCpnt);
if (do_trace) printk("%s: ihdlr, exit, irq %d, count %d.\n", BN(j), irq,
HD(j)->iocount);
return;
}
static void do_interrupt_handler(int irq, void *shap, struct pt_regs *regs) {
unsigned int j;
IRQ_FLAGS
SPIN_FLAGS
if ((j = (unsigned int)((char *)shap - sha)) >= num_boards) return;
SPIN_LOCK_SAVE
IRQ_LOCK_SAVE
ihdlr(irq, j);
IRQ_UNLOCK_RESTORE
SPIN_UNLOCK_RESTORE
}
int u14_34f_release(struct Scsi_Host *shpnt) {
unsigned int i, j;
IRQ_FLAGS
IRQ_LOCK_SAVE
for (j = 0; sh[j] != NULL && sh[j] != shpnt; j++);
if (sh[j] == NULL) panic("%s: release, invalid Scsi_Host pointer.\n",
driver_name);
for (i = 0; i < sh[j]->can_queue; i++)
if ((&HD(j)->cp[i])->sglist) kfree((&HD(j)->cp[i])->sglist);
free_irq(sh[j]->irq, &sha[j]);
if (sh[j]->dma_channel != NO_DMA) free_dma(sh[j]->dma_channel);
release_region(sh[j]->io_port, sh[j]->n_io_port);
scsi_unregister(sh[j]);
IRQ_UNLOCK_RESTORE
return FALSE;
}
#if defined(MODULE)
Scsi_Host_Template driver_template = ULTRASTOR_14_34F;
#include "scsi_module.c"
#endif