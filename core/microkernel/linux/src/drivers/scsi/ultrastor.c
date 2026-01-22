#ifdef MODULE
#include <linux/module.h>
#endif
#include <linux/stddef.h>
#include <linux/string.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/ioport.h>
#include <linux/proc_fs.h>
#include <asm/io.h>
#include <asm/bitops.h>
#include <asm/system.h>
#include <asm/dma.h>
#define ULTRASTOR_PRIVATE
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "ultrastor.h"
#include "sd.h"
#include<linux/stat.h>
struct proc_dir_entry proc_scsi_ultrastor = {
PROC_SCSI_ULTRASTOR, 9, "ultrastor",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#define FALSE 0
#define TRUE 1
#ifndef ULTRASTOR_DEBUG
#define ULTRASTOR_DEBUG (UD_ABORT|UD_CSIR|UD_RESET)
#endif
#define VERSION "1.12"
#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr)[0])
#define PACKED		__attribute__((packed))
#define ALIGNED(x)	__attribute__((aligned(x)))
typedef struct {
unsigned int address;
unsigned int num_bytes;
} ultrastor_sg_list;
struct mscp {
unsigned char opcode: 3;
unsigned char xdir: 2;
unsigned char dcn: 1;
unsigned char ca: 1;
unsigned char sg: 1;
unsigned char target_id: 3;
unsigned char ch_no: 2;
unsigned char lun: 3;
unsigned int transfer_data PACKED;
unsigned int transfer_data_length PACKED;
unsigned int command_link PACKED;
unsigned char scsi_command_link_id;
unsigned char number_of_sg_list;
unsigned char length_of_sense_byte;
unsigned char length_of_scsi_cdbs;
unsigned char scsi_cdbs[12];
unsigned char adapter_status;
unsigned char target_status;
unsigned int sense_data PACKED;
void (*done)(Scsi_Cmnd *);
Scsi_Cmnd *SCint;
ultrastor_sg_list sglist[ULTRASTOR_24F_MAX_SG];
};
#define U14F_PRODUCT_ID(port) ((port) + 0x4)
#define CONFIG(port) ((port) + 0x6)
#define LCL_DOORBELL_MASK(port) ((port) + 0x0)
#define LCL_DOORBELL_INTR(port) ((port) + 0x1)
#define SYS_DOORBELL_MASK(port) ((port) + 0x2)
#define SYS_DOORBELL_INTR(port) ((port) + 0x3)
static struct ultrastor_config
{
unsigned short port_address;
unsigned short doorbell_address;
unsigned short ogm_address;
unsigned short icm_address;
const void *bios_segment;
unsigned char interrupt: 4;
unsigned char dma_channel: 3;
unsigned char bios_drive_number: 1;
unsigned char heads;
unsigned char sectors;
unsigned char ha_scsi_id: 3;
unsigned char subversion: 4;
unsigned char revision;
unsigned char slot;
#ifdef PRINT_U24F_VERSION
volatile int csir_done;
#endif
#if ULTRASTOR_MAX_CMDS == 1
unsigned char mscp_busy;
#else
unsigned short mscp_free;
#endif
volatile unsigned char aborted[ULTRASTOR_MAX_CMDS];
struct mscp mscp[ULTRASTOR_MAX_CMDS];
} config = {0};
int ultrastor_bus_reset = 0;
static const void *const bios_segment_table[8] = {
NULL,	     (void *)0xC4000, (void *)0xC8000, (void *)0xCC000,
(void *)0xD0000, (void *)0xD4000, (void *)0xD8000, (void *)0xDC000,
};
static const unsigned char interrupt_table_14f[4] = { 15, 14, 11, 10 };
static const unsigned char dma_channel_table_14f[4] = { 5, 6, 7, 0 };
static const struct {
unsigned char heads;
unsigned char sectors;
} mapping_table[4] = { { 16, 63 }, { 64, 32 }, { 64, 63 }, { 64, 32 } };
#ifndef PORT_OVERRIDE
static const unsigned short ultrastor_ports_14f[] = {
0x330, 0x340,  0x230, 0x240, 0x210, 0x130, 0x140,
};
#endif
static void ultrastor_interrupt(int, void *, struct pt_regs *);
static inline void build_sg_list(struct mscp *, Scsi_Cmnd *SCpnt);
static inline int find_and_clear_bit_16(unsigned short *field)
{
int rv;
unsigned long flags;
save_flags(flags);
cli();
if (*field == 0) panic("No free mscp");
asm("xorl %0,%0\n0:\tbsfw %1,%w0\n\tbtr %0,%1\n\tjnc 0b"
: "=&r" (rv), "+m" (*field));
restore_flags(flags);
return rv;
}
static inline unsigned char xchgb(unsigned char reg,
volatile unsigned char *mem)
{
__asm__ ("xchgb %0,%1" : "=q" (reg), "=m" (*mem) : "0" (reg));
return reg;
}
#if ULTRASTOR_DEBUG & (UD_COMMAND | UD_ABORT)
static void log_ultrastor_abort(register struct ultrastor_config *config,
int command)
{
static char fmt[80] = "abort %d (%x); MSCP free pool: %x;";
register int i;
unsigned long flags;
save_flags(flags);
cli();
for (i = 0; i < ULTRASTOR_MAX_CMDS; i++)
{
fmt[20 + i*2] = ' ';
if (! (config->mscp_free & (1 << i)))
fmt[21 + i*2] = '0' + config->mscp[i].target_id;
else
fmt[21 + i*2] = '-';
}
fmt[20 + ULTRASTOR_MAX_CMDS * 2] = '\n';
fmt[21 + ULTRASTOR_MAX_CMDS * 2] = 0;
printk(fmt, command, &config->mscp[command], config->mscp_free);
restore_flags(flags);
}
#endif
static int ultrastor_14f_detect(Scsi_Host_Template * tpnt)
{
size_t i;
unsigned char in_byte, version_byte = 0;
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
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US14F: detect: called\n");
#endif
if (config.bios_segment)
return FALSE;
#ifdef PORT_OVERRIDE
if(check_region(PORT_OVERRIDE, 0xc)) {
printk("Ultrastor I/O space already in use\n");
return FALSE;
};
config.port_address = PORT_OVERRIDE;
#else
for (i = 0; i < ARRAY_SIZE(ultrastor_ports_14f); i++) {
if(check_region(ultrastor_ports_14f[i], 0x0c)) continue;
config.port_address = ultrastor_ports_14f[i];
#endif
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US14F: detect: testing port address %03X\n", config.port_address);
#endif
in_byte = inb(U14F_PRODUCT_ID(config.port_address));
if (in_byte != US14F_PRODUCT_ID_0) {
#if (ULTRASTOR_DEBUG & UD_DETECT)
# ifdef PORT_OVERRIDE
printk("US14F: detect: wrong product ID 0 - %02X\n", in_byte);
# else
printk("US14F: detect: no adapter at port %03X\n", config.port_address);
# endif
#endif
#ifdef PORT_OVERRIDE
return FALSE;
#else
continue;
#endif
}
in_byte = inb(U14F_PRODUCT_ID(config.port_address) + 1);
if ((in_byte & 0xF0) != US14F_PRODUCT_ID_1) {
#if (ULTRASTOR_DEBUG & UD_DETECT)
# ifdef PORT_OVERRIDE
printk("US14F: detect: wrong product ID 1 - %02X\n", in_byte);
# else
printk("US14F: detect: no adapter at port %03X\n", config.port_address);
# endif
#endif
#ifdef PORT_OVERRIDE
return FALSE;
#else
continue;
#endif
}
version_byte = in_byte;
#ifndef PORT_OVERRIDE
break;
}
if (i == ARRAY_SIZE(ultrastor_ports_14f)) {
# if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US14F: detect: no port address found!\n");
# endif
return FALSE;
}
#endif
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US14F: detect: adapter found at port address %03X\n",
config.port_address);
#endif
outb(ultrastor_bus_reset ? 0xc2 : 0x82, LCL_DOORBELL_MASK(config.port_address));
request_region(config.port_address, 0x0c,"ultrastor");
*(char *)&config_1 = inb(CONFIG(config.port_address + 0));
*(char *)&config_2 = inb(CONFIG(config.port_address + 1));
config.bios_segment = bios_segment_table[config_1.bios_segment];
config.doorbell_address = config.port_address;
config.ogm_address = config.port_address + 0x8;
config.icm_address = config.port_address + 0xC;
config.interrupt = interrupt_table_14f[config_1.interrupt];
config.ha_scsi_id = config_2.ha_scsi_id;
config.heads = mapping_table[config_2.mapping_mode].heads;
config.sectors = mapping_table[config_2.mapping_mode].sectors;
config.bios_drive_number = config_2.bios_drive_number;
config.subversion = (version_byte & 0x0F);
if (config.subversion == U34F)
config.dma_channel = 0;
else
config.dma_channel = dma_channel_table_14f[config_1.dma_channel];
if (!config.bios_segment) {
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US14F: detect: not detected.\n");
#endif
return FALSE;
}
if (config.subversion != U34F)
if (!config.dma_channel || !(config_2.tfr_port & 0x2)) {
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US14F: detect: consistency check failed\n");
#endif
return FALSE;
}
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US14F: detect: detect succeeded\n"
"  Port address: %03X\n"
"  BIOS segment: %05X\n"
"  Interrupt: %u\n"
"  DMA channel: %u\n"
"  H/A SCSI ID: %u\n"
"  Subversion: %u\n",
config.port_address, config.bios_segment, config.interrupt,
config.dma_channel, config.ha_scsi_id, config.subversion);
#endif
tpnt->this_id = config.ha_scsi_id;
tpnt->unchecked_isa_dma = (config.subversion != U34F);
#if ULTRASTOR_MAX_CMDS > 1
config.mscp_free = ~0;
#endif
if (request_irq(config.interrupt, ultrastor_interrupt, 0, "Ultrastor", NULL)) {
printk("Unable to allocate IRQ%u for UltraStor controller.\n",
config.interrupt);
return FALSE;
}
if (config.dma_channel && request_dma(config.dma_channel,"Ultrastor")) {
printk("Unable to allocate DMA channel %u for UltraStor controller.\n",
config.dma_channel);
free_irq(config.interrupt, NULL);
return FALSE;
}
tpnt->sg_tablesize = ULTRASTOR_14F_MAX_SG;
printk("UltraStor driver version" VERSION ".  Using %d SG lists.\n",
ULTRASTOR_14F_MAX_SG);
return TRUE;
}
static int ultrastor_24f_detect(Scsi_Host_Template * tpnt)
{
register int i;
struct Scsi_Host * shpnt = NULL;
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US24F: detect");
#endif
for (i = 1; i < 15; i++)
{
unsigned char config_1, config_2;
unsigned short addr = (i << 12) | ULTRASTOR_24F_PORT;
if (inb(addr) != US24F_PRODUCT_ID_0 &&
inb(addr+1) != US24F_PRODUCT_ID_1 &&
inb(addr+2) != US24F_PRODUCT_ID_2)
continue;
config.revision = inb(addr+3);
config.slot = i;
if (! (inb(addr+4) & 1))
{
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("U24F: found disabled card in slot %u\n", i);
#endif
continue;
}
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("U24F: found card in slot %u\n", i);
#endif
config_1 = inb(addr + 5);
config.bios_segment = bios_segment_table[config_1 & 7];
switch(config_1 >> 4)
{
case 1:
config.interrupt = 15;
break;
case 2:
config.interrupt = 14;
break;
case 4:
config.interrupt = 11;
break;
case 8:
config.interrupt = 10;
break;
default:
printk("U24F: invalid IRQ\n");
return FALSE;
}
if (request_irq(config.interrupt, ultrastor_interrupt, 0, "Ultrastor", NULL))
{
printk("Unable to allocate IRQ%u for UltraStor controller.\n",
config.interrupt);
return FALSE;
}
config.port_address = addr;
config.doorbell_address = addr + 12;
config.ogm_address = addr + 0x17;
config.icm_address = addr + 0x1C;
config_2 = inb(addr + 7);
config.ha_scsi_id = config_2 & 7;
config.heads = mapping_table[(config_2 >> 3) & 3].heads;
config.sectors = mapping_table[(config_2 >> 3) & 3].sectors;
#if (ULTRASTOR_DEBUG & UD_DETECT)
printk("US24F: detect: detect succeeded\n"
"  Port address: %03X\n"
"  BIOS segment: %05X\n"
"  Interrupt: %u\n"
"  H/A SCSI ID: %u\n",
config.port_address, config.bios_segment,
config.interrupt, config.ha_scsi_id);
#endif
tpnt->this_id = config.ha_scsi_id;
tpnt->unchecked_isa_dma = 0;
tpnt->sg_tablesize = ULTRASTOR_24F_MAX_SG;
shpnt = scsi_register(tpnt, 0);
shpnt->irq = config.interrupt;
shpnt->dma_channel = config.dma_channel;
shpnt->io_port = config.port_address;
#if ULTRASTOR_MAX_CMDS > 1
config.mscp_free = ~0;
#endif
outb(0, addr + 0x16);
outb(0, addr + 0x1B);
outb(ultrastor_bus_reset ? 0xc2 : 0x82, LCL_DOORBELL_MASK(addr+12));
outb(0x02, SYS_DOORBELL_MASK(addr+12));
printk("UltraStor driver version " VERSION ".  Using %d SG lists.\n",
tpnt->sg_tablesize);
return TRUE;
}
return FALSE;
}
int ultrastor_detect(Scsi_Host_Template * tpnt)
{
tpnt->proc_dir = &proc_scsi_ultrastor;
return ultrastor_14f_detect(tpnt) || ultrastor_24f_detect(tpnt);
}
const char *ultrastor_info(struct Scsi_Host * shpnt)
{
static char buf[64];
if (config.slot)
sprintf(buf, "UltraStor 24F SCSI @ Slot %u IRQ%u",
config.slot, config.interrupt);
else if (config.subversion)
sprintf(buf, "UltraStor 34F SCSI @ Port %03X BIOS %05X IRQ%u",
config.port_address, (int)config.bios_segment,
config.interrupt);
else
sprintf(buf, "UltraStor 14F SCSI @ Port %03X BIOS %05X IRQ%u DMA%u",
config.port_address, (int)config.bios_segment,
config.interrupt, config.dma_channel);
return buf;
}
static inline void build_sg_list(register struct mscp *mscp, Scsi_Cmnd *SCpnt)
{
struct scatterlist *sl;
long transfer_length = 0;
int i, max;
sl = (struct scatterlist *) SCpnt->request_buffer;
max = SCpnt->use_sg;
for (i = 0; i < max; i++) {
mscp->sglist[i].address = (unsigned int)sl[i].address;
mscp->sglist[i].num_bytes = sl[i].length;
transfer_length += sl[i].length;
}
mscp->number_of_sg_list = max;
mscp->transfer_data = (unsigned int)mscp->sglist;
mscp->transfer_data_length = transfer_length;
}
int ultrastor_queuecommand(Scsi_Cmnd *SCpnt, void (*done)(Scsi_Cmnd *))
{
register struct mscp *my_mscp;
#if ULTRASTOR_MAX_CMDS > 1
int mscp_index;
#endif
unsigned int status;
unsigned long flags;
if ((config.mscp_free & ((1U << ULTRASTOR_MAX_CMDS) - 1)) == 0)
panic("ultrastor_queuecommand: no free MSCP\n");
mscp_index = find_and_clear_bit_16(&config.mscp_free);
if (xchgb(0xff, &config.aborted[mscp_index]) != 0)
{
status = DID_ABORT << 16;
goto aborted;
}
my_mscp = &config.mscp[mscp_index];
#if 1
*(unsigned char *)my_mscp = OP_SCSI | (DTD_SCSI << 3);
#else
my_mscp->opcode = OP_SCSI;
my_mscp->xdir = DTD_SCSI;
my_mscp->dcn = FALSE;
#endif
my_mscp->ca = SCpnt->device->type != TYPE_TAPE;
my_mscp->target_id = SCpnt->target;
my_mscp->ch_no = 0;
my_mscp->lun = SCpnt->lun;
if (SCpnt->use_sg) {
my_mscp->sg = TRUE;
build_sg_list(my_mscp, SCpnt);
} else {
my_mscp->sg = FALSE;
my_mscp->transfer_data = (unsigned int)SCpnt->request_buffer;
my_mscp->transfer_data_length = SCpnt->request_bufflen;
}
my_mscp->command_link = 0;
my_mscp->scsi_command_link_id = 0;
my_mscp->length_of_sense_byte = sizeof SCpnt->sense_buffer;
my_mscp->length_of_scsi_cdbs = SCpnt->cmd_len;
memcpy(my_mscp->scsi_cdbs, SCpnt->cmnd, my_mscp->length_of_scsi_cdbs);
my_mscp->adapter_status = 0;
my_mscp->target_status = 0;
my_mscp->sense_data = (unsigned int)&SCpnt->sense_buffer;
my_mscp->done = done;
my_mscp->SCint = SCpnt;
SCpnt->host_scribble = (unsigned char *)my_mscp;
retry:
if (config.slot)
while (inb(config.ogm_address - 1) != 0 &&
config.aborted[mscp_index] == 0xff) barrier();
while ((inb(LCL_DOORBELL_INTR(config.doorbell_address)) &
(config.slot ? 2 : 1))
&& config.aborted[mscp_index] == 0xff) barrier();
save_flags(flags);
cli();
if (inb(LCL_DOORBELL_INTR(config.doorbell_address)) &
(config.slot ? 2 : 1))
{
restore_flags(flags);
goto retry;
}
status = xchgb(0, &config.aborted[mscp_index]);
if (status != 0xff) {
restore_flags(flags);
#if ULTRASTOR_DEBUG & (UD_COMMAND | UD_ABORT)
printk("USx4F: queuecommand: aborted\n");
#if ULTRASTOR_MAX_CMDS > 1
log_ultrastor_abort(&config, mscp_index);
#endif
#endif
status <<= 16;
aborted:
set_bit(mscp_index, &config.mscp_free);
#if ULTRASTOR_MAX_CMDS > 1
SCpnt->result = status;
done(SCpnt);
return 0;
#else
return status;
#endif
}
outl((unsigned int)my_mscp, config.ogm_address);
if (config.slot) {
outb(1, config.ogm_address - 1);
outb(0x2, LCL_DOORBELL_INTR(config.doorbell_address));
} else {
outb(0x1, LCL_DOORBELL_INTR(config.doorbell_address));
}
restore_flags(flags);
#if (ULTRASTOR_DEBUG & UD_COMMAND)
printk("USx4F: queuecommand: returning\n");
#endif
return 0;
}
int ultrastor_abort(Scsi_Cmnd *SCpnt)
{
#if ULTRASTOR_DEBUG & UD_ABORT
char out[108];
unsigned char icm_status = 0, ogm_status = 0;
unsigned int icm_addr = 0, ogm_addr = 0;
#endif
unsigned int mscp_index;
unsigned char old_aborted;
void (*done)(Scsi_Cmnd *);
if(config.slot)
return SCSI_ABORT_SNOOZE;
if(!SCpnt->host_scribble)
return SCSI_ABORT_NOT_RUNNING;
mscp_index = ((struct mscp *)SCpnt->host_scribble) - config.mscp;
if (mscp_index >= ULTRASTOR_MAX_CMDS)
panic("Ux4F aborting invalid MSCP");
#if ULTRASTOR_DEBUG & UD_ABORT
if (config.slot)
{
int port0 = (config.slot << 12) | 0xc80;
int i;
unsigned long flags;
save_flags(flags);
cli();
strcpy(out, "OGM %d:%x ICM %d:%x ports:  ");
for (i = 0; i < 16; i++)
{
unsigned char p = inb(port0 + i);
out[28 + i * 3] = "0123456789abcdef"[p >> 4];
out[29 + i * 3] = "0123456789abcdef"[p & 15];
out[30 + i * 3] = ' ';
}
out[28 + i * 3] = '\n';
out[29 + i * 3] = 0;
ogm_status = inb(port0 + 22);
ogm_addr = inl(port0 + 23);
icm_status = inb(port0 + 27);
icm_addr = inl(port0 + 28);
restore_flags(flags);
}
if (config.slot ? inb(config.icm_address - 1) == 2 :
(inb(SYS_DOORBELL_INTR(config.doorbell_address)) & 1))
{
unsigned long flags;
save_flags(flags);
printk("Ux4F: abort while completed command pending\n");
restore_flags(flags);
cli();
ultrastor_interrupt(0, NULL, NULL);
restore_flags(flags);
return SCSI_ABORT_SUCCESS;
}
#endif
old_aborted = xchgb(DID_ABORT, &config.aborted[mscp_index]);
if (old_aborted == 0xff)
return SCSI_ABORT_SUCCESS;
if (config.slot && inb(config.ogm_address - 1) == 0)
{
unsigned long flags;
save_flags(flags);
cli();
outl((int)&config.mscp[mscp_index], config.ogm_address);
inb(0xc80);
outb(0x80, config.ogm_address - 1);
outb(0x2, LCL_DOORBELL_INTR(config.doorbell_address));
#if ULTRASTOR_DEBUG & UD_ABORT
log_ultrastor_abort(&config, mscp_index);
printk(out, ogm_status, ogm_addr, icm_status, icm_addr);
#endif
restore_flags(flags);
return SCSI_ABORT_PENDING;
}
#if ULTRASTOR_DEBUG & UD_ABORT
log_ultrastor_abort(&config, mscp_index);
#endif
#if ULTRASTOR_DEBUG & UD_ABORT
if (config.mscp[mscp_index].SCint != SCpnt)
printk("abort: command mismatch, %p != %p\n",
config.mscp[mscp_index].SCint, SCpnt);
#endif
if (config.mscp[mscp_index].SCint == 0)
return SCSI_ABORT_NOT_RUNNING;
if (config.mscp[mscp_index].SCint != SCpnt) panic("Bad abort");
config.mscp[mscp_index].SCint = 0;
done = config.mscp[mscp_index].done;
config.mscp[mscp_index].done = 0;
SCpnt->result = DID_ABORT << 16;
done(SCpnt);
return SCSI_ABORT_SUCCESS;
}
int ultrastor_reset(Scsi_Cmnd * SCpnt, unsigned int reset_flags)
{
unsigned long flags;
register int i;
#if (ULTRASTOR_DEBUG & UD_RESET)
printk("US14F: reset: called\n");
#endif
if(config.slot)
return SCSI_RESET_PUNT;
save_flags(flags);
cli();
outb(0xc0, LCL_DOORBELL_INTR(config.doorbell_address));
if (config.slot)
{
outb(0, config.ogm_address - 1);
outb(0, config.icm_address - 1);
}
#if ULTRASTOR_MAX_CMDS == 1
if (config.mscp_busy && config.mscp->done && config.mscp->SCint)
{
config.mscp->SCint->result = DID_RESET << 16;
config.mscp->done(config.mscp->SCint);
}
config.mscp->SCint = 0;
#else
for (i = 0; i < ULTRASTOR_MAX_CMDS; i++)
{
if (! (config.mscp_free & (1 << i)) &&
config.mscp[i].done && config.mscp[i].SCint)
{
config.mscp[i].SCint->result = DID_RESET << 16;
config.mscp[i].done(config.mscp[i].SCint);
config.mscp[i].done = 0;
}
config.mscp[i].SCint = 0;
}
#endif
memset((unsigned char *)config.aborted, 0, sizeof config.aborted);
#if ULTRASTOR_MAX_CMDS == 1
config.mscp_busy = 0;
#else
config.mscp_free = ~0;
#endif
restore_flags(flags);
return SCSI_RESET_SUCCESS;
}
int ultrastor_biosparam(Disk * disk, kdev_t dev, int * dkinfo)
{
int size = disk->capacity;
unsigned int s = config.heads * config.sectors;
dkinfo[0] = config.heads;
dkinfo[1] = config.sectors;
dkinfo[2] = size / s;
#if 0
if (dkinfo[2] > 1024)
dkinfo[2] = 1024;
#endif
return 0;
}
static void ultrastor_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
unsigned int status;
#if ULTRASTOR_MAX_CMDS > 1
unsigned int mscp_index;
#endif
register struct mscp *mscp;
void (*done)(Scsi_Cmnd *);
Scsi_Cmnd *SCtmp;
#if ULTRASTOR_MAX_CMDS == 1
mscp = &config.mscp[0];
#else
mscp = (struct mscp *)inl(config.icm_address);
mscp_index = mscp - config.mscp;
if (mscp_index >= ULTRASTOR_MAX_CMDS) {
printk("Ux4F interrupt: bad MSCP address %x\n", (unsigned int) mscp);
ultrastor_reset(NULL, 0);
return;
}
#endif
if (config.slot) {
unsigned char icm_status = inb(config.icm_address - 1);
#if ULTRASTOR_DEBUG & (UD_INTERRUPT|UD_ERROR|UD_ABORT)
if (icm_status != 1 && icm_status != 2)
printk("US24F: ICM status %x for MSCP %d (%x)\n", icm_status,
mscp_index, (unsigned int) mscp);
#endif
outb(2, SYS_DOORBELL_INTR(config.doorbell_address));
outb(0, config.icm_address - 1);
if (icm_status == 4) {
printk("UltraStor abort command failed\n");
return;
}
if (icm_status == 3) {
void (*done)(Scsi_Cmnd *) = mscp->done;
if (done) {
mscp->done = 0;
mscp->SCint->result = DID_ABORT << 16;
done(mscp->SCint);
}
return;
}
} else {
outb(1, SYS_DOORBELL_INTR(config.doorbell_address));
}
SCtmp = mscp->SCint;
mscp->SCint = NULL;
if (SCtmp == 0)
{
#if ULTRASTOR_DEBUG & (UD_ABORT|UD_INTERRUPT)
printk("MSCP %d (%x): no command\n", mscp_index, (unsigned int) mscp);
#endif
#if ULTRASTOR_MAX_CMDS == 1
config.mscp_busy = FALSE;
#else
set_bit(mscp_index, &config.mscp_free);
#endif
config.aborted[mscp_index] = 0;
return;
}
done = mscp->done;
mscp->done = 0;
switch (mscp->adapter_status)
{
case 0:
status = DID_OK << 16;
break;
case 0x01:
case 0x02:
case 0x03:
default:
status = DID_ERROR << 16;
break;
case 0x84:
status = DID_ABORT << 16;
break;
case 0x91:
status = DID_TIME_OUT << 16;
break;
}
SCtmp->result = status | mscp->target_status;
SCtmp->host_scribble = 0;
#if ULTRASTOR_MAX_CMDS == 1
config.mscp_busy = FALSE;
#else
set_bit(mscp_index, &config.mscp_free);
#endif
#if ULTRASTOR_DEBUG & (UD_ABORT|UD_INTERRUPT)
if (config.aborted[mscp_index])
printk("Ux4 interrupt: MSCP %d (%x) aborted = %d\n",
mscp_index, (unsigned int) mscp, config.aborted[mscp_index]);
#endif
config.aborted[mscp_index] = 0;
if (done)
done(SCtmp);
else
printk("US14F: interrupt: unexpected interrupt\n");
if (config.slot ? inb(config.icm_address - 1) :
(inb(SYS_DOORBELL_INTR(config.doorbell_address)) & 1))
#if (ULTRASTOR_DEBUG & UD_MULTI_CMD)
printk("Ux4F: multiple commands completed\n");
#else
;
#endif
#if (ULTRASTOR_DEBUG & UD_INTERRUPT)
printk("USx4F: interrupt: returning\n");
#endif
}
#ifdef MODULE
Scsi_Host_Template driver_template = ULTRASTOR_14F;
#include "scsi_module.c"
#endif