#include <linux/version.h>
#define LinuxVersionCode(v, p, s) (((v)<<16)+((p)<<8)+(s))
#define MAX_INT_PARAM 10
#if defined(MODULE)
#include <linux/module.h>
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,26)
MODULE_PARM(io_port, "1-" __MODULE_STRING(MAX_INT_PARAM) "i");
MODULE_PARM(linked_comm, "i");
MODULE_PARM(tagged_comm, "i");
MODULE_PARM(link_statistics, "i");
MODULE_PARM(max_queue_depth, "i");
MODULE_PARM(tag_mode, "i");
MODULE_PARM(use_new_eh_code, "i");
MODULE_PARM(ext_tran, "i");
MODULE_PARM(rev_scan, "i");
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
#include "eata.h"
#include <linux/stat.h>
#include <linux/config.h>
#include <linux/pci.h>
#if LINUX_VERSION_CODE < LinuxVersionCode(2,1,93)
#include <linux/bios32.h>
#endif
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
struct proc_dir_entry proc_scsi_eata2x = {
PROC_SCSI_EATA2X, 6, "eata2x",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#define ISA 0
#define ESA 1
#undef FORCE_CONFIG
#undef DEBUG_LINKED_COMMANDS
#undef DEBUG_DETECT
#undef DEBUG_PCI_DETECT
#undef DEBUG_INTERRUPT
#undef DEBUG_RESET
#undef DEBUG_GENERATE_ERRORS
#undef DEBUG_GENERATE_ABORTS
#undef DEBUG_GEOMETRY
#define MAX_ISA 4
#define MAX_VESA 0
#define MAX_EISA 15
#define MAX_PCI 16
#define MAX_BOARDS (MAX_ISA + MAX_VESA + MAX_EISA + MAX_PCI)
#define MAX_CHANNEL 4
#define MAX_LUN 32
#define MAX_TARGET 32
#define MAX_MAILBOXES 64
#define MAX_SGLIST 64
#define MAX_LARGE_SGLIST 122
#define MAX_INTERNAL_RETRIES 64
#define MAX_CMD_PER_LUN 2
#define MAX_TAGGED_CMD_PER_LUN (MAX_MAILBOXES - MAX_CMD_PER_LUN)
#define SKIP ULONG_MAX
#define FALSE 0
#define TRUE 1
#define FREE 0
#define IN_USE 1
#define LOCKED 2
#define IN_RESET 3
#define IGNORE 4
#define READY 5
#define ABORTING 6
#define NO_DMA 0xff
#define MAXLOOP 10000
#define TAG_MIXED 0
#define TAG_SIMPLE 1
#define TAG_HEAD 2
#define TAG_ORDERED 3
#define REG_CMD 7
#define REG_STATUS 7
#define REG_AUX_STATUS 8
#define REG_DATA 0
#define REG_DATA2 1
#define REG_SEE 6
#define REG_LOW 2
#define REG_LM 3
#define REG_MID 4
#define REG_MSB 5
#define REGION_SIZE 9
#define MAX_ISA_ADDR 0x03ff
#define MIN_EISA_ADDR 0x1c88
#define MAX_EISA_ADDR 0xfc88
#define BSY_ASSERTED 0x80
#define DRQ_ASSERTED 0x08
#define ABSY_ASSERTED 0x01
#define IRQ_ASSERTED 0x02
#define READ_CONFIG_PIO 0xf0
#define SET_CONFIG_PIO 0xf1
#define SEND_CP_PIO 0xf2
#define RECEIVE_SP_PIO 0xf3
#define TRUNCATE_XFR_PIO 0xf4
#define RESET_PIO 0xf9
#define READ_CONFIG_DMA 0xfd
#define SET_CONFIG_DMA 0xfe
#define SEND_CP_DMA 0xff
#define ASOK 0x00
#define ASST 0x01
#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr)[0])
#define YESNO(a) ((a) ? 'y' : 'n')
#define TLDEV(type) ((type) == TYPE_DISK || (type) == TYPE_ROM)
#define EATA_SIGNATURE 0x41544145
#define EATA_2_0A_SIZE 28
#define EATA_2_0B_SIZE 30
#define EATA_2_0C_SIZE 34
struct eata_info {
ulong data_len;
ulong sign;
unchar :4,
version:4;
unchar ocsena:1,
tarsup:1,
trnxfr:1,
morsup:1,
dmasup:1,
drqvld:1,
ata:1,
haaval:1;
ushort cp_pad_len;
unchar host_addr[4];
ulong cp_len;
ulong sp_len;
ushort queue_size;
ushort unused;
ushort scatt_size;
unchar irq:4,
irq_tr:1,
second:1,
drqx:2;
unchar sync;
unchar isaena:1,
forcaddr:1,
large_sg:1,
res1:1,
:4;
unchar max_id:5,
max_chan:3;
unchar max_lun;
unchar :4,
m1:1,
idquest:1,
pci:1,
eisa:1;
unchar raidnum;
unchar notused;
ushort ipad[247];
};
struct eata_config {
ushort len;
unchar edis:1,
ocena:1,
mdpena:1,
tarena:1,
:4;
unchar cpad[511];
};
struct mssp {
unchar adapter_status:7,
eoc:1;
unchar target_status;
unchar unused[2];
ulong inv_res_len;
struct mscp *cpp;
char mess[12];
};
struct sg_list {
unsigned int address;
unsigned int num_bytes;
};
struct mscp {
unchar sreset:1,
init:1,
reqsen:1,
sg:1,
:1,
interp:1,
dout:1,
din:1;
unchar sense_len;
unchar unused[3];
unchar fwnest:1,
:7;
unchar phsunit:1,
iat:1,
hbaci:1,
:5;
unchar target:5,
channel:3;
unchar lun:5,
luntar:1,
dispri:1,
one:1;
unchar mess[3];
unchar cdb[12];
ulong data_len;
struct mscp *cpp;
ulong data_address;
ulong sp_addr;
ulong sense_addr;
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
unsigned char protocol_rev;
struct mssp sp[2];
};
static struct Scsi_Host *sh[MAX_BOARDS + 1];
static const char *driver_name = "EATA";
static char sha[MAX_BOARDS];
static unsigned int num_boards = MAX_BOARDS;
static unsigned long io_port[] __initdata = {
SKIP, SKIP, SKIP, SKIP, SKIP, SKIP, SKIP, SKIP,
SKIP, SKIP,
0x1f0,
SKIP, SKIP, SKIP, SKIP, SKIP, SKIP, SKIP, SKIP,
SKIP, SKIP, SKIP, SKIP, SKIP, SKIP, SKIP, SKIP,
0x1c88, 0x2c88, 0x3c88, 0x4c88, 0x5c88, 0x6c88, 0x7c88, 0x8c88,
0x9c88, 0xac88, 0xbc88, 0xcc88, 0xdc88, 0xec88, 0xfc88,
0x170, 0x230, 0x330,
0x0
};
#define HD(board) ((struct hostdata *) &sh[board]->hostdata)
#define BN(board) (HD(board)->board_name)
#define H2DEV(x) htonl(x)
#define DEV2H(x) H2DEV(x)
#define V2DEV(addr) ((addr) ? H2DEV(virt_to_bus((void *)addr)) : 0)
#define DEV2V(addr) ((addr) ? DEV2H(bus_to_virt((unsigned long)addr)) : 0)
static void do_interrupt_handler(int, void *, struct pt_regs *);
static void flush_dev(Scsi_Device *, unsigned long, unsigned int, unsigned int);
static int do_trace = FALSE;
static int setup_done = FALSE;
static int link_statistics = 0;
static int tag_mode = TAG_MIXED;
static int ext_tran = FALSE;
static int rev_scan = TRUE;
#if defined(CONFIG_SCSI_EATA_TAGGED_QUEUE)
static int tagged_comm = TRUE;
#else
static int tagged_comm = FALSE;
#endif
#if defined(CONFIG_SCSI_EATA_LINKED_COMMANDS)
static int linked_comm = TRUE;
#else
static int linked_comm = FALSE;
#endif
#if defined(CONFIG_SCSI_EATA_MAX_TAGS)
static int max_queue_depth = CONFIG_SCSI_EATA_MAX_TAGS;
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
if (tagged_comm && dev->tagged_supported && TLDEV(dev->type)) {
dev->tagged_queue = 1;
dev->current_tag = 1;
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
while (inb(iobase + REG_AUX_STATUS) & ABSY_ASSERTED) {
udelay(1L);
if (--loop == 0) return TRUE;
}
return FALSE;
}
static inline int do_dma(unsigned long iobase, unsigned int addr, unchar cmd) {
if (wait_on_busy(iobase, (addr ? MAXLOOP * 100 : MAXLOOP))) return TRUE;
if ((addr = V2DEV(addr))) {
outb((char) (addr >> 24), iobase + REG_LOW);
outb((char) (addr >> 16), iobase + REG_LM);
outb((char) (addr >> 8), iobase + REG_MID);
outb((char) addr, iobase + REG_MSB);
}
outb(cmd, iobase + REG_CMD);
return FALSE;
}
static inline int read_pio(unsigned long iobase, ushort *start, ushort *end) {
unsigned int loop = MAXLOOP;
ushort *p;
for (p = start; p <= end; p++) {
while (!(inb(iobase + REG_STATUS) & DRQ_ASSERTED)) {
udelay(1L);
if (--loop == 0) return TRUE;
}
loop = MAXLOOP;
*p = inw(iobase);
}
return FALSE;
}
__initfunc (static inline int
get_pci_irq(unsigned long port_base, unsigned char *apic_irq)) {
#if defined(CONFIG_PCI)
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,93)
unsigned int addr;
struct pci_dev *dev = NULL;
if (!pci_present()) return FALSE;
while((dev = pci_find_class(PCI_CLASS_STORAGE_SCSI << 8, dev))) {
if (pci_read_config_dword(dev, PCI_BASE_ADDRESS_0, &addr)) continue;
#if defined(DEBUG_PCI_DETECT)
printk("%s: get_pci_irq, bus %d, devfn 0x%x, addr 0x%x, apic_irq %u.\n",
driver_name, dev->bus->number, dev->devfn, addr, dev->irq);
#endif
if ((addr & PCI_BASE_ADDRESS_SPACE) != PCI_BASE_ADDRESS_SPACE_IO)
continue;
if ((addr & PCI_BASE_ADDRESS_IO_MASK) + PCI_BASE_ADDRESS_0 == port_base) {
*apic_irq = dev->irq;
return TRUE;
}
}
#endif
#endif
return FALSE;
}
__initfunc (static inline int port_detect \
(unsigned long port_base, unsigned int j, Scsi_Host_Template *tpnt)) {
unsigned char irq, dma_channel, subversion, i;
unsigned char protocol_rev, apic_irq;
struct eata_info info;
char *bus_type, dma_name[16], tag_type;
unsigned char dma_channel_table[4] = { 5, 6, 7, 0 };
char name[16];
sprintf(name, "%s%d", driver_name, j);
printk("\rprobing eata on %lx", port_base);
if(check_region(port_base, REGION_SIZE)) {
printk("%s: address 0x%03lx in use, skipping probe.\n", name, port_base);
return FALSE;
}
if (do_dma(port_base, 0, READ_CONFIG_PIO)) return FALSE;
if (read_pio(port_base, (ushort *)&info, (ushort *)&info.ipad[0]))
return FALSE;
if (info.sign != EATA_SIGNATURE) return FALSE;
if (DEV2H(info.data_len) < EATA_2_0A_SIZE) {
printk("%s: config structure size (%ld bytes) too short, detaching.\n",
name, DEV2H(info.data_len));
return FALSE;
}
else if (DEV2H(info.data_len) == EATA_2_0A_SIZE)
protocol_rev = 'A';
else if (DEV2H(info.data_len) == EATA_2_0B_SIZE)
protocol_rev = 'B';
else
protocol_rev = 'C';
if (!setup_done && j > 0 && j <= MAX_PCI) {
bus_type = "PCI";
subversion = ESA;
}
else if (port_base > MAX_EISA_ADDR || (protocol_rev == 'C' && info.pci)) {
bus_type = "PCI";
subversion = ESA;
}
else if (port_base >= MIN_EISA_ADDR || (protocol_rev == 'C' && info.eisa)) {
bus_type = "EISA";
subversion = ESA;
}
else if (protocol_rev == 'C' && !info.eisa && !info.pci) {
bus_type = "ISA";
subversion = ISA;
}
else if (port_base > MAX_ISA_ADDR) {
bus_type = "PCI";
subversion = ESA;
}
else {
bus_type = "ISA";
subversion = ISA;
}
if (!info.haaval || info.ata) {
printk("%s: address 0x%03lx, unusable %s board (%d%d), detaching.\n",
name, port_base, bus_type, info.haaval, info.ata);
return FALSE;
}
if (info.drqvld) {
if (subversion == ESA)
printk("%s: warning, weird %s board using DMA.\n", name, bus_type);
subversion = ISA;
dma_channel = dma_channel_table[3 - info.drqx];
}
else {
if (subversion == ISA)
printk("%s: warning, weird %s board not using DMA.\n", name, bus_type);
subversion = ESA;
dma_channel = NO_DMA;
}
if (!info.dmasup)
printk("%s: warning, DMA protocol support not asserted.\n", name);
irq = info.irq;
if (subversion == ESA && !info.irq_tr)
printk("%s: warning, LEVEL triggering is suggested for IRQ %u.\n",
name, irq);
if (get_pci_irq(port_base, &apic_irq) && (irq != apic_irq)) {
printk("%s: IRQ %u mapped to IO-APIC IRQ %u.\n", name, irq, apic_irq);
irq = apic_irq;
}
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
#if defined(FORCE_CONFIG)
{
struct eata_config config;
memset((char *)&config, 0, sizeof(struct eata_config));
config.len = (ushort) htons((ushort)510);
config.ocena = TRUE;
if (do_dma(port_base, (unsigned int)&config, SET_CONFIG_DMA)) {
printk("%s: busy timeout sending configuration, detaching.\n", name);
return FALSE;
}
}
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
sh[j]->dma_channel = dma_channel;
sh[j]->irq = irq;
sh[j]->sg_tablesize = (ushort) ntohs(info.scatt_size);
sh[j]->this_id = (ushort) info.host_addr[3];
sh[j]->can_queue = (ushort) ntohs(info.queue_size);
sh[j]->cmd_per_lun = MAX_CMD_PER_LUN;
sh[j]->select_queue_depths = select_queue_depths;
request_region(sh[j]->io_port, sh[j]->n_io_port, driver_name);
memset(HD(j), 0, sizeof(struct hostdata));
HD(j)->subversion = subversion;
HD(j)->protocol_rev = protocol_rev;
HD(j)->board_number = j;
if (HD(j)->subversion == ESA)
sh[j]->unchecked_isa_dma = FALSE;
else {
sh[j]->wish_block = TRUE;
sh[j]->unchecked_isa_dma = TRUE;
disable_dma(dma_channel);
clear_dma_ff(dma_channel);
set_dma_mode(dma_channel, DMA_MODE_CASCADE);
enable_dma(dma_channel);
}
strcpy(BN(j), name);
if (sh[j]->sg_tablesize > MAX_SGLIST || sh[j]->sg_tablesize < 2) {
printk("%s: detect, wrong n. of SG lists %d, fixed.\n",
BN(j), sh[j]->sg_tablesize);
sh[j]->sg_tablesize = MAX_SGLIST;
}
if (sh[j]->can_queue > MAX_MAILBOXES || sh[j]->can_queue < 2) {
printk("%s: detect, wrong n. of mbox %d, fixed.\n",
BN(j), sh[j]->can_queue);
sh[j]->can_queue = MAX_MAILBOXES;
}
if (protocol_rev != 'A') {
if (info.max_chan > 0 && info.max_chan < MAX_CHANNEL)
sh[j]->max_channel = info.max_chan;
if (info.max_id > 7 && info.max_id < MAX_TARGET)
sh[j]->max_id = info.max_id + 1;
if (info.large_sg && sh[j]->sg_tablesize == MAX_SGLIST)
sh[j]->sg_tablesize = MAX_LARGE_SGLIST;
}
if (protocol_rev == 'C') {
if (info.max_lun > 7 && info.max_lun < MAX_LUN)
sh[j]->max_lun = info.max_lun + 1;
}
if (dma_channel == NO_DMA) sprintf(dma_name, "%s", "BMST");
else sprintf(dma_name, "DMA %u", dma_channel);
for (i = 0; i < sh[j]->can_queue; i++)
if (! ((&HD(j)->cp[i])->sglist = kmalloc(
sh[j]->sg_tablesize * sizeof(struct sg_list),
(sh[j]->unchecked_isa_dma ? GFP_DMA : 0) | GFP_ATOMIC))) {
printk("%s: kmalloc SGlist failed, mbox %d, detaching.\n", BN(j), i);
eata2x_release(sh[j]);
return FALSE;
}
if (max_queue_depth > MAX_TAGGED_CMD_PER_LUN)
max_queue_depth = MAX_TAGGED_CMD_PER_LUN;
if (max_queue_depth < MAX_CMD_PER_LUN) max_queue_depth = MAX_CMD_PER_LUN;
if (tagged_comm) {
if (tag_mode == TAG_SIMPLE) tag_type = '1';
else if (tag_mode == TAG_HEAD) tag_type = '2';
else if (tag_mode == TAG_ORDERED) tag_type = '3';
else tag_type = 'y';
}
else tag_type = 'n';
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,101)
sh[j]->hostt->use_new_eh_code = use_new_eh_code;
#else
use_new_eh_code = FALSE;
#endif
if (j == 0) {
printk("EATA/DMA 2.0x: Copyright (C) 1994-1998 Dario Ballabio.\n");
printk("%s config options -> tc:%c, lc:%c, mq:%d, eh:%c, rs:%c, et:%c.\n",
driver_name, tag_type, YESNO(linked_comm), max_queue_depth,
YESNO(use_new_eh_code), YESNO(rev_scan), YESNO(ext_tran));
}
printk("%s: 2.0%c, %s 0x%03lx, IRQ %u, %s, SG %d, MB %d.\n",
BN(j), HD(j)->protocol_rev, bus_type, (unsigned long)sh[j]->io_port,
sh[j]->irq, dma_name, sh[j]->sg_tablesize, sh[j]->can_queue);
if (sh[j]->max_id > 8 || sh[j]->max_lun > 8)
printk("%s: wide SCSI support enabled, max_id %u, max_lun %u.\n",
BN(j), sh[j]->max_id, sh[j]->max_lun);
for (i = 0; i <= sh[j]->max_channel; i++)
printk("%s: SCSI channel %u enabled, host target ID %d.\n",
BN(j), i, info.host_addr[3 - i]);
#if defined(DEBUG_DETECT)
printk("%s: Vers. 0x%x, ocs %u, tar %u, trnxfr %u, more %u, SYNC 0x%x, "\
"sec. %u, infol %ld, cpl %ld spl %ld.\n", name, info.version,
info.ocsena, info.tarsup, info.trnxfr, info.morsup, info.sync,
info.second, DEV2H(info.data_len), DEV2H(info.cp_len),
DEV2H(info.sp_len));
if (protocol_rev == 'B' || protocol_rev == 'C')
printk("%s: isaena %u, forcaddr %u, max_id %u, max_chan %u, "\
"large_sg %u, res1 %u.\n", name, info.isaena, info.forcaddr,
info.max_id, info.max_chan, info.large_sg, info.res1);
if (protocol_rev == 'C')
printk("%s: max_lun %u, m1 %u, idquest %u, pci %u, eisa %u, "\
"raidnum %u.\n", name, info.max_lun, info.m1, info.idquest,
info.pci, info.eisa, info.raidnum);
#endif
return TRUE;
}
__initfunc (void eata2x_setup(char *str, int *ints)) {
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
else if (!strncmp(cur, "tc:", 3)) tagged_comm = val;
else if (!strncmp(cur, "tm:", 3)) tag_mode = val;
else if (!strncmp(cur, "mq:", 3)) max_queue_depth = val;
else if (!strncmp(cur, "ls:", 3)) link_statistics = val;
else if (!strncmp(cur, "eh:", 3)) use_new_eh_code = val;
else if (!strncmp(cur, "et:", 3)) ext_tran = val;
else if (!strncmp(cur, "rs:", 3)) rev_scan = val;
if ((cur = strchr(cur, ','))) ++cur;
}
return;
}
__initfunc (static void add_pci_ports(void)) {
#if defined(CONFIG_PCI)
unsigned int addr, k;
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,93)
struct pci_dev *dev = NULL;
if (!pci_present()) return;
for (k = 0; k < MAX_PCI; k++) {
if (!(dev = pci_find_class(PCI_CLASS_STORAGE_SCSI << 8, dev))) break;
if (pci_read_config_dword(dev, PCI_BASE_ADDRESS_0, &addr)) continue;
#if defined(DEBUG_PCI_DETECT)
printk("%s: detect, seq. %d, bus %d, devfn 0x%x, addr 0x%x.\n",
driver_name, k, dev->bus->number, dev->devfn, addr);
#endif
if ((addr & PCI_BASE_ADDRESS_SPACE) != PCI_BASE_ADDRESS_SPACE_IO)
continue;
io_port[MAX_INT_PARAM + (rev_scan ? (MAX_PCI - k) : (1 + k))] =
(addr & PCI_BASE_ADDRESS_IO_MASK) + PCI_BASE_ADDRESS_0;
}
#else
unsigned short i = 0;
unsigned char bus, devfn;
if (!pcibios_present()) return;
for (k = 0; k < MAX_PCI; k++) {
if (pcibios_find_class(PCI_CLASS_STORAGE_SCSI << 8, i++, &bus, &devfn)
!= PCIBIOS_SUCCESSFUL) break;
if (pcibios_read_config_dword(bus, devfn, PCI_BASE_ADDRESS_0, &addr)
!= PCIBIOS_SUCCESSFUL) continue;
#if defined(DEBUG_PCI_DETECT)
printk("%s: detect, seq. %d, bus %d, devfn 0x%x, addr 0x%x.\n",
driver_name, k, bus, devfn, addr);
#endif
if ((addr & PCI_BASE_ADDRESS_SPACE) != PCI_BASE_ADDRESS_SPACE_IO)
continue;
io_port[MAX_INT_PARAM + (rev_scan ? (MAX_PCI - k) : (1 + k))] =
(addr & PCI_BASE_ADDRESS_IO_MASK) + PCI_BASE_ADDRESS_0;
}
#endif
#endif
return;
}
__initfunc (int eata2x_detect(Scsi_Host_Template *tpnt)) {
unsigned int j = 0, k;
IRQ_FLAGS
IRQ_LOCK_SAVE
tpnt->proc_dir = &proc_scsi_eata2x;
#if defined(MODULE)
if(io_port[0] != SKIP) {
setup_done = TRUE;
io_port[MAX_INT_PARAM] = 0;
}
#endif
for (k = 0; k < MAX_BOARDS + 1; k++) sh[k] = NULL;
if (!setup_done) add_pci_ports();
for (k = 0; io_port[k]; k++) {
if (io_port[k] == SKIP) continue;
if (j < MAX_BOARDS && port_detect(io_port[k], j, tpnt)) j++;
}
num_boards = j;
IRQ_UNLOCK_RESTORE
return j;
}
static inline void build_sg_list(struct mscp *cpp, Scsi_Cmnd *SCpnt) {
unsigned int k;
struct scatterlist *sgpnt;
sgpnt = (struct scatterlist *) SCpnt->request_buffer;
for (k = 0; k < SCpnt->use_sg; k++) {
cpp->sglist[k].address = V2DEV(sgpnt[k].address);
cpp->sglist[k].num_bytes = H2DEV(sgpnt[k].length);
}
cpp->data_address = V2DEV(cpp->sglist);
cpp->data_len = H2DEV((SCpnt->use_sg * sizeof(struct sg_list)));
}
static inline int do_qcomm(Scsi_Cmnd *SCpnt, void (*done)(Scsi_Cmnd *)) {
unsigned int i, j, k;
struct mscp *cpp;
struct mssp *spp;
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
spp = &HD(j)->sp[0];
cpp->sp_addr = V2DEV(spp);
cpp->cpp = cpp;
SCpnt->scsi_done = done;
cpp->index = i;
SCpnt->host_scribble = (unsigned char *) &cpp->index;
if (do_trace) printk("%s: qcomm, mbox %d, target %d.%d:%d, pid %ld.\n",
BN(j), i, SCpnt->channel, SCpnt->target,
SCpnt->lun, SCpnt->pid);
for (k = 0; k < ARRAY_SIZE(data_out_cmds); k++)
if (SCpnt->cmnd[0] == data_out_cmds[k]) {
cpp->dout = TRUE;
break;
}
if ((cpp->din = !cpp->dout))
for (k = 0; k < ARRAY_SIZE(data_none_cmds); k++)
if (SCpnt->cmnd[0] == data_none_cmds[k]) {
cpp->din = FALSE;
break;
}
cpp->reqsen = TRUE;
cpp->dispri = TRUE;
#if 0
if (SCpnt->device->type == TYPE_TAPE) cpp->hbaci = TRUE;
#endif
cpp->one = TRUE;
cpp->channel = SCpnt->channel;
cpp->target = SCpnt->target;
cpp->lun = SCpnt->lun;
cpp->SCpnt = SCpnt;
cpp->sense_addr = V2DEV(SCpnt->sense_buffer);
cpp->sense_len = sizeof SCpnt->sense_buffer;
if (SCpnt->device->tagged_queue) {
if (HD(j)->target_redo[SCpnt->target][SCpnt->channel] ||
HD(j)->target_to[SCpnt->target][SCpnt->channel])
cpp->mess[0] = ORDERED_QUEUE_TAG;
else if (tag_mode == TAG_SIMPLE) cpp->mess[0] = SIMPLE_QUEUE_TAG;
else if (tag_mode == TAG_HEAD) cpp->mess[0] = HEAD_OF_QUEUE_TAG;
else if (tag_mode == TAG_ORDERED) cpp->mess[0] = ORDERED_QUEUE_TAG;
else if (SCpnt->device->current_tag == 0)
cpp->mess[0] = ORDERED_QUEUE_TAG;
else if (SCpnt->device->current_tag == 1)
cpp->mess[0] = HEAD_OF_QUEUE_TAG;
else
cpp->mess[0] = SIMPLE_QUEUE_TAG;
cpp->mess[1] = SCpnt->device->current_tag++;
}
if (SCpnt->use_sg) {
cpp->sg = TRUE;
build_sg_list(cpp, SCpnt);
}
else {
cpp->data_address = V2DEV(SCpnt->request_buffer);
cpp->data_len = H2DEV(SCpnt->request_bufflen);
}
memcpy(cpp->cdb, SCpnt->cmnd, SCpnt->cmd_len);
if (linked_comm && SCpnt->device->queue_depth > 2
&& TLDEV(SCpnt->device->type)) {
HD(j)->cp_stat[i] = READY;
flush_dev(SCpnt->device, SCpnt->request.sector, j, FALSE);
return 0;
}
if (do_dma(sh[j]->io_port, (unsigned int) cpp, SEND_CP_DMA)) {
SCpnt->host_scribble = NULL;
printk("%s: qcomm, target %d.%d:%d, pid %ld, adapter busy.\n",
BN(j), SCpnt->channel, SCpnt->target, SCpnt->lun, SCpnt->pid);
return 1;
}
HD(j)->cp_stat[i] = IN_USE;
return 0;
}
int eata2x_queuecommand(Scsi_Cmnd *SCpnt, void (*done)(Scsi_Cmnd *)) {
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
if (inb(sh[j]->io_port + REG_AUX_STATUS) & IRQ_ASSERTED)
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
int eata2x_old_abort(Scsi_Cmnd *SCarg) {
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
if (inb(sh[j]->io_port + REG_AUX_STATUS) & IRQ_ASSERTED)
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
int eata2x_abort(Scsi_Cmnd *SCarg) {
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
if (do_dma(sh[j]->io_port, 0, RESET_PIO)) {
printk("%s: reset, cannot reset, timeout error.\n", BN(j));
return SCSI_RESET_ERROR;
}
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
int eata2x_old_reset(Scsi_Cmnd *SCarg, unsigned int reset_flags) {
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
if (do_dma(sh[j]->io_port, 0, RESET_PIO)) {
printk("%s: reset, cannot reset, timeout error.\n", BN(j));
return FAILED;
}
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
else printk("%s: reset, exit.\n", BN(j));
return SUCCESS;
}
int eata2x_reset(Scsi_Cmnd *SCarg) {
return do_reset(SCarg);
}
#endif
int eata2x_biosparam(Disk *disk, kdev_t dev, int *dkinfo) {
int size = disk->capacity;
if (ext_tran || (scsicam_bios_param(disk, dev, dkinfo) < 0)) {
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
if (!cpp->din) input_only = FALSE;
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
if (rev && !r) { revcount++; readysorted += n_ready; }
if (!rev && !s) { sortcount++; readysorted += n_ready; }
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
YESNO(overlap), cpp->din);
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
if (do_dma(sh[j]->io_port, (unsigned int) cpp, SEND_CP_DMA)) {
printk("%s: %s, target %d.%d:%d, pid %ld, mbox %d, adapter"\
" busy, will abort.\n", BN(j), (ihdlr ? "ihdlr" : "qcomm"),
SCpnt->channel, SCpnt->target, SCpnt->lun, SCpnt->pid, k);
HD(j)->cp_stat[k] = ABORTING;
continue;
}
HD(j)->cp_stat[k] = IN_USE;
}
}
static inline void ihdlr(int irq, unsigned int j) {
Scsi_Cmnd *SCpnt;
unsigned int i, k, c, status, tstatus, reg;
struct mssp *dspp, *spp;
struct mscp *cpp;
if (sh[j]->irq != irq)
panic("%s: ihdlr, irq %d, sh[j]->irq %d.\n", BN(j), irq, sh[j]->irq);
if (!(inb(sh[j]->io_port + REG_AUX_STATUS) & IRQ_ASSERTED)) return;
HD(j)->iocount++;
if (do_trace) printk("%s: ihdlr, enter, irq %d, count %d.\n", BN(j), irq,
HD(j)->iocount);
if (wait_on_busy(sh[j]->io_port, 20 * MAXLOOP)) {
reg = inb(sh[j]->io_port + REG_STATUS);
printk("%s: ihdlr, busy timeout error,  irq %d, reg 0x%x, count %d.\n",
BN(j), irq, reg, HD(j)->iocount);
return;
}
dspp = &HD(j)->sp[0];
spp = &HD(j)->sp[1];
memcpy(spp, dspp, sizeof(struct mssp));
memset(dspp, 0, sizeof(struct mssp));
reg = inb(sh[j]->io_port + REG_STATUS);
if (spp->eoc == FALSE)
printk("%s: ihdlr, spp->eoc == FALSE, irq %d, reg 0x%x, count %d.\n",
BN(j), irq, reg, HD(j)->iocount);
if (spp->cpp == NULL)
printk("%s: ihdlr, spp->cpp == NULL,  irq %d, reg 0x%x, count %d.\n",
BN(j), irq, reg, HD(j)->iocount);
if (spp->eoc == FALSE || spp->cpp == NULL) return;
cpp = spp->cpp;
#if defined(DEBUG_GENERATE_ABORTS)
if ((HD(j)->iocount > 500) && ((HD(j)->iocount % 500) < 3)) return;
#endif
i = cpp - HD(j)->cp;
if (cpp < HD(j)->cp || cpp >= HD(j)->cp + sh[j]->can_queue
|| i >= sh[j]->can_queue)
panic("%s: ihdlr, invalid mscp bus address %p, cp0 %p.\n", BN(j),
cpp, HD(j)->cp);
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
case 0x02:
if (HD(j)->target_to[SCpnt->target][SCpnt->channel] > 1)
status = DID_ERROR << 16;
else {
status = DID_TIME_OUT << 16;
HD(j)->target_to[SCpnt->target][SCpnt->channel]++;
}
break;
case 0x03:
case 0x04:
for (c = 0; c <= sh[j]->max_channel; c++)
for (k = 0; k < sh[j]->max_id; k++)
HD(j)->target_redo[k][c] = TRUE;
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
case 0x05:
case 0x06:
case 0x07:
case 0x08:
case 0x09:
case 0x0a:
case 0x0b:
case 0x0c:
default:
status = DID_ERROR << 16;
break;
}
SCpnt->result = status | spp->target_status;
#if defined(DEBUG_INTERRUPT)
if (SCpnt->result || do_trace)
#else
if ((spp->adapter_status != ASOK && HD(j)->iocount > 1000) ||
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
int eata2x_release(struct Scsi_Host *shpnt) {
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
Scsi_Host_Template driver_template = EATA;
#include "scsi_module.c"
#endif