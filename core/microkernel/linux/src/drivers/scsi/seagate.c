#ifdef MACH
#define ARBITRATE
#define SLOW_HANDSHAKE
#define FAST32
#endif
#include <linux/module.h>
#include <asm/io.h>
#include <asm/system.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/config.h>
#include <linux/proc_fs.h>
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "seagate.h"
#include "constants.h"
#include<linux/stat.h>
struct proc_dir_entry proc_scsi_seagate = {
PROC_SCSI_SEAGATE, 7, "seagate",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#ifndef IRQ
#define IRQ 5
#endif
#if (defined(FAST32) && !defined(FAST))
#define FAST
#endif
#if defined(SLOW_RATE) && !defined(SLOW_HANDSHAKE)
#define SLOW_HANDSHAKE
#endif
#if defined(SLOW_HANDSHAKE) && !defined(SLOW_RATE)
#define SLOW_RATE 50
#endif
#if defined(LINKED)
#undef LINKED
#endif
static int internal_command(unsigned char target, unsigned char lun,
const void *cmnd,
void *buff, int bufflen, int reselect);
static int incommand;
static const void *base_address = NULL;
#ifdef notyet
static volatile int abort_confirm = 0;
#endif
static volatile void *st0x_cr_sr;
static volatile void *st0x_dr;
static volatile int st0x_aborted=0;
static unsigned char controller_type = 0;
static unsigned char irq = IRQ;
#define retcode(result) (((result) << 16) | (message << 8) | status)
#define STATUS (*(volatile unsigned char *) st0x_cr_sr)
#define CONTROL STATUS
#define DATA (*(volatile unsigned char *) st0x_dr)
#define WRITE_CONTROL(d) { writeb((d), st0x_cr_sr); }
#define WRITE_DATA(d) { writeb((d), st0x_dr); }
void st0x_setup (char *str, int *ints) {
controller_type = SEAGATE;
base_address = (void *) ints[1];
irq = ints[2];
}
void tmc8xx_setup (char *str, int *ints) {
controller_type = FD;
base_address = (void *) ints[1];
irq = ints[2];
}
#ifndef OVERRIDE
static const char * seagate_bases[] = {
(char *) 0xc8000, (char *) 0xca000, (char *) 0xcc000,
(char *) 0xce000, (char *) 0xdc000, (char *) 0xde000
};
typedef struct {
const char *signature ;
unsigned offset;
unsigned length;
unsigned char type;
} Signature;
static const Signature signatures[] = {
#ifdef CONFIG_SCSI_SEAGATE
{"ST01 v1.7  (C) Copyright 1987 Seagate", 15, 37, SEAGATE},
{"SCSI BIOS 2.00  (C) Copyright 1987 Seagate", 15, 40, SEAGATE},
{"SEAGATE SCSI BIOS ",16, 17, SEAGATE},
{"SEAGATE SCSI BIOS ",17, 17, SEAGATE},
{"FUTURE DOMAIN CORP. (C) 1986-1989 V5.0C2/14/89", 5, 46, FD},
{"FUTURE DOMAIN CORP. (C) 1986-1989 V6.0A7/28/89", 5, 46, FD},
{"FUTURE DOMAIN CORP. (C) 1986-1990 V6.0105/31/90",5, 47, FD},
{"FUTURE DOMAIN CORP. (C) 1986-1990 V6.0209/18/90",5, 47, FD},
{"FUTURE DOMAIN CORP. (C) 1986-1990 V7.009/18/90", 5, 46, FD},
{"FUTURE DOMAIN CORP. (C) 1992 V8.00.004/02/92", 5, 44, FD},
{"IBM F1 BIOS V1.1004/30/92", 5, 25, FD},
{"FUTURE DOMAIN TMC-950", 5, 21, FD},
#endif
}
;
#define NUM_SIGNATURES (sizeof(signatures) / sizeof(Signature))
#endif
static int hostno = -1;
static void seagate_reconnect_intr(int, void *, struct pt_regs *);
#ifdef FAST
static int fast = 1;
#endif
#ifdef SLOW_HANDSHAKE
static int borken_calibration = 0;
static void borken_init (void) {
register int count = 0, start = jiffies + 1, stop = start + 25;
while (jiffies < start);
for (;jiffies < stop; ++count);
borken_calibration = (count * 4) / (SLOW_RATE*1024);
if (borken_calibration < 1)
borken_calibration = 1;
#if (DEBUG & DEBUG_BORKEN)
printk("scsi%d : borken calibrated to %dK/sec, %d cycles per transfer\n",
hostno, BORKEN_RATE, borken_calibration);
#endif
}
static inline void borken_wait(void) {
register int count;
for (count = borken_calibration; count && (STATUS & STAT_REQ);
--count);
#if (DEBUG & DEBUG_BORKEN)
if (count)
printk("scsi%d : borken timeout\n", hostno);
#endif
}
#endif
int seagate_st0x_detect (Scsi_Host_Template * tpnt)
{
struct Scsi_Host *instance;
#ifndef OVERRIDE
int i,j;
#endif
tpnt->proc_dir = &proc_scsi_seagate;
#ifdef DEBUG
printk("Autodetecting ST0x / TMC-8xx\n");
#endif
if (hostno != -1)
{
printk ("ERROR : seagate_st0x_detect() called twice.\n");
return 0;
}
if (!controller_type) {
#ifdef OVERRIDE
base_address = (void *) OVERRIDE;
#ifdef CONTROLLER
controller_type = CONTROLLER;
#else
#error Please use -DCONTROLLER=SEAGATE or -DCONTROLLER=FD to override controller type
#endif
#ifdef DEBUG
printk("Base address overridden to %x, controller type is %s\n",
base_address,controller_type == SEAGATE ? "SEAGATE" : "FD");
#endif
#else
for (i = 0; i < (sizeof (seagate_bases) / sizeof (char * )); ++i)
for (j = 0; !base_address && j < NUM_SIGNATURES; ++j)
if (!memcmp ((const void *) (seagate_bases[i] +
signatures[j].offset), (const void *) signatures[j].signature,
signatures[j].length)) {
base_address = (const void *) seagate_bases[i];
controller_type = signatures[j].type;
}
#endif
}
tpnt->this_id = (controller_type == SEAGATE) ? 7 : 6;
tpnt->name = (controller_type == SEAGATE) ? ST0X_ID_STR : FD_ID_STR;
if (base_address)
{
st0x_cr_sr =(void *) (((const unsigned char *) base_address) + (controller_type == SEAGATE ? 0x1a00 : 0x1c00));
st0x_dr = (void *) (((const unsigned char *) base_address ) + (controller_type == SEAGATE ? 0x1c00 : 0x1e00));
#ifdef DEBUG
printk("%s detected. Base address = %x, cr = %x, dr = %x\n", tpnt->name, base_address, st0x_cr_sr, st0x_dr);
#endif
instance = scsi_register(tpnt, 0);
hostno = instance->host_no;
if (request_irq((int) irq, seagate_reconnect_intr, SA_INTERRUPT,
(controller_type == SEAGATE) ? "seagate" : "tmc-8xx", NULL)) {
printk("scsi%d : unable to allocate IRQ%d\n",
hostno, (int) irq);
return 0;
}
instance->irq = irq;
instance->io_port = (unsigned int) base_address;
#ifdef SLOW_HANDSHAKE
borken_init();
#endif
printk("%s options:"
#ifdef ARBITRATE
" ARBITRATE"
#endif
#ifdef SLOW_HANDSHAKE
" SLOW_HANDSHAKE"
#endif
#ifdef FAST
#ifdef FAST32
" FAST32"
#else
" FAST"
#endif
#endif
#ifdef LINKED
" LINKED"
#endif
"\n", tpnt->name);
return 1;
}
else
{
#ifdef DEBUG
printk("ST0x / TMC-8xx not detected.\n");
#endif
return 0;
}
}
const char *seagate_st0x_info(struct Scsi_Host * shpnt) {
static char buffer[64];
sprintf(buffer, "%s at irq %d, address 0x%05X",
(controller_type == SEAGATE) ? ST0X_ID_STR : FD_ID_STR,
irq, (unsigned int)base_address);
return buffer;
}
int seagate_st0x_proc_info(char *buffer, char **start, off_t offset,
int length, int hostno, int inout)
{
const char *info = seagate_st0x_info(NULL);
int len;
int pos;
int begin;
if (inout) return(-ENOSYS);
begin = 0;
strcpy(buffer,info);
strcat(buffer,"\n");
pos = len = strlen(buffer);
if (pos<offset) {
len = 0;
begin = pos;
}
*start = buffer + (offset - begin);
len -= (offset - begin);
if ( len > length ) len = length;
return(len);
}
static unsigned char current_target, current_lun;
static unsigned char *current_cmnd, *current_data;
static int current_nobuffs;
static struct scatterlist *current_buffer;
static int current_bufflen;
#ifdef LINKED
static int linked_connected = 0;
static unsigned char linked_target, linked_lun;
#endif
static void (*done_fn)(Scsi_Cmnd *) = NULL;
static Scsi_Cmnd * SCint = NULL;
#define NO_RECONNECT 0
#define RECONNECT_NOW 1
#define CAN_RECONNECT 2
#ifdef LINKED
#define LINKED_RIGHT 3
#define LINKED_WRONG 4
#endif
static int should_reconnect = 0;
static void seagate_reconnect_intr(int irq, void *dev_id, struct pt_regs *regs)
{
int temp;
Scsi_Cmnd * SCtmp;
sti();
#if (DEBUG & PHASE_RESELECT)
printk("scsi%d : seagate_reconnect_intr() called\n", hostno);
#endif
if (!should_reconnect)
printk("scsi%d: unexpected interrupt.\n", hostno);
else {
should_reconnect = 0;
#if (DEBUG & PHASE_RESELECT)
printk("scsi%d : internal_command("
"%d, %08x, %08x, %d, RECONNECT_NOW\n", hostno,
current_target, current_data, current_bufflen);
#endif
temp = internal_command (current_target, current_lun,
current_cmnd, current_data, current_bufflen,
RECONNECT_NOW);
if (msg_byte(temp) != DISCONNECT) {
if (done_fn) {
#if (DEBUG & PHASE_RESELECT)
printk("scsi%d : done_fn(%d,%08x)", hostno,
hostno, temp);
#endif
if(!SCint) panic("SCint == NULL in seagate");
SCtmp = SCint;
SCint = NULL;
SCtmp->result = temp;
done_fn (SCtmp);
} else
printk("done_fn() not defined.\n");
}
}
}
static int recursion_depth = 0;
int seagate_st0x_queue_command (Scsi_Cmnd * SCpnt, void (*done)(Scsi_Cmnd *))
{
int result, reconnect;
Scsi_Cmnd * SCtmp;
done_fn = done;
current_target = SCpnt->target;
current_lun = SCpnt->lun;
current_cmnd = SCpnt->cmnd;
current_data = (unsigned char *) SCpnt->request_buffer;
current_bufflen = SCpnt->request_bufflen;
SCint = SCpnt;
if(recursion_depth) {
return 0;
};
recursion_depth++;
do{
#ifdef LINKED
current_cmnd[SCpnt->cmd_len] |= 0x01;
if (linked_connected) {
#if (DEBUG & DEBUG_LINKED)
printk("scsi%d : using linked commands, current I_T_L nexus is ",
hostno);
#endif
if ((linked_target == current_target) &&
(linked_lun == current_lun)) {
#if (DEBUG & DEBUG_LINKED)
printk("correct\n");
#endif
reconnect = LINKED_RIGHT;
} else {
#if (DEBUG & DEBUG_LINKED)
printk("incorrect\n");
#endif
reconnect = LINKED_WRONG;
}
} else
#endif
reconnect = CAN_RECONNECT;
result = internal_command (SCint->target, SCint->lun, SCint->cmnd, SCint->request_buffer,
SCint->request_bufflen,
reconnect);
if (msg_byte(result) == DISCONNECT) break;
SCtmp = SCint;
SCint = NULL;
SCtmp->result = result;
done_fn (SCtmp);
} while(SCint);
recursion_depth--;
return 0;
}
int seagate_st0x_command (Scsi_Cmnd * SCpnt) {
return internal_command (SCpnt->target, SCpnt->lun, SCpnt->cmnd, SCpnt->request_buffer,
SCpnt->request_bufflen,
(int) NO_RECONNECT);
}
static int internal_command(unsigned char target, unsigned char lun, const void *cmnd,
void *buff, int bufflen, int reselect) {
int len = 0;
unsigned char *data = NULL;
struct scatterlist *buffer = NULL;
int nobuffs = 0;
int clock;
int temp;
#ifdef SLOW_HANDSHAKE
int borken;
#endif
#if (DEBUG & PHASE_DATAIN) || (DEBUG & PHASE_DATOUT)
int transfered = 0;
#endif
#if (((DEBUG & PHASE_ETC) == PHASE_ETC) || (DEBUG & PRINT_COMMAND) || \
(DEBUG & PHASE_EXIT))
int i;
#endif
#if ((DEBUG & PHASE_ETC) == PHASE_ETC)
int phase=0, newphase;
#endif
int done = 0;
unsigned char status = 0;
unsigned char message = 0;
register unsigned char status_read;
unsigned transfersize = 0, underflow = 0;
incommand = 0;
st0x_aborted = 0;
#ifdef SLOW_HANDSHAKE
borken = (int) SCint->device->borken;
#endif
#if (DEBUG & PRINT_COMMAND)
printk ("scsi%d : target = %d, command = ", hostno, target);
print_command((unsigned char *) cmnd);
printk("\n");
#endif
#if (DEBUG & PHASE_RESELECT)
switch (reselect) {
case RECONNECT_NOW :
printk("scsi%d : reconnecting\n", hostno);
break;
#ifdef LINKED
case LINKED_RIGHT :
printk("scsi%d : connected, can reconnect\n", hostno);
break;
case LINKED_WRONG :
printk("scsi%d : connected to wrong target, can reconnect\n",
hostno);
break;
#endif
case CAN_RECONNECT :
printk("scsi%d : allowed to reconnect\n", hostno);
break;
default :
printk("scsi%d : not allowed to reconnect\n", hostno);
}
#endif
if (target == (controller_type == SEAGATE ? 7 : 6))
return DID_BAD_TARGET;
switch (reselect) {
case RECONNECT_NOW:
#if (DEBUG & PHASE_RESELECT)
printk("scsi%d : phase RESELECT \n", hostno);
#endif
clock = jiffies + 10;
for (;;) {
temp = STATUS;
if ((temp & STAT_IO) && !(temp & STAT_BSY))
break;
if (jiffies > clock) {
#if (DEBUG & PHASE_RESELECT)
printk("scsi%d : RESELECT timed out while waiting for IO .\n",
hostno);
#endif
return (DID_BAD_INTR << 16);
}
}
if (!((temp = DATA) & (controller_type == SEAGATE ? 0x80 : 0x40)))
{
#if (DEBUG & PHASE_RESELECT)
printk("scsi%d : detected reconnect request to different target.\n"
"\tData bus = %d\n", hostno, temp);
#endif
return (DID_BAD_INTR << 16);
}
if (!(temp & (1 << current_target)))
{
printk("scsi%d : Unexpected reselect interrupt.  Data bus = %d\n",
hostno, temp);
return (DID_BAD_INTR << 16);
}
buffer=current_buffer;
cmnd=current_cmnd;
data=current_data;
len=current_bufflen;
nobuffs=current_nobuffs;
#if 1
CONTROL = (BASE_CMD | CMD_DRVR_ENABLE | CMD_BSY);
#else
CONTROL = (BASE_CMD | CMD_BSY);
#endif
for (clock = jiffies + 10; (jiffies < clock) && (STATUS & STAT_SEL););
if (jiffies >= clock)
{
CONTROL = (BASE_CMD | CMD_INTR);
#if (DEBUG & PHASE_RESELECT)
printk("scsi%d : RESELECT timed out while waiting for SEL.\n",
hostno);
#endif
return (DID_BAD_INTR << 16);
}
CONTROL = BASE_CMD;
break;
case CAN_RECONNECT:
#ifdef LINKED
connect_loop :
#endif
#if (DEBUG & PHASE_BUS_FREE)
printk ("scsi%d : phase = BUS FREE \n", hostno);
#endif
clock = jiffies + ST0X_BUS_FREE_DELAY;
#if !defined (ARBITRATE)
while (((STATUS | STATUS | STATUS) &
(STAT_BSY | STAT_SEL)) &&
(!st0x_aborted) && (jiffies < clock));
if (jiffies > clock)
return retcode(DID_BUS_BUSY);
else if (st0x_aborted)
return retcode(st0x_aborted);
#endif
#if (DEBUG & PHASE_SELECTION)
printk("scsi%d : phase = SELECTION\n", hostno);
#endif
clock = jiffies + ST0X_SELECTION_DELAY;
#if defined(ARBITRATE)
cli();
CONTROL = 0;
DATA = (controller_type == SEAGATE) ? 0x80 : 0x40;
CONTROL = CMD_START_ARB;
sti();
while (!((status_read = STATUS) & (STAT_ARB_CMPL | STAT_SEL)) &&
(jiffies < clock) && !st0x_aborted);
if (!(status_read & STAT_ARB_CMPL)) {
#if (DEBUG & PHASE_SELECTION)
if (status_read & STAT_SEL)
printk("scsi%d : arbitration lost\n", hostno);
else
printk("scsi%d : arbitration timeout.\n", hostno);
#endif
CONTROL = BASE_CMD;
return retcode(DID_NO_CONNECT);
};
#if (DEBUG & PHASE_SELECTION)
printk("scsi%d : arbitration complete\n", hostno);
#endif
#endif
cli();
DATA = (unsigned char) ((1 << target) | (controller_type == SEAGATE ? 0x80 : 0x40));
CONTROL = BASE_CMD | CMD_DRVR_ENABLE | CMD_SEL |
(reselect ? CMD_ATTN : 0);
sti();
while (!((status_read = STATUS) & STAT_BSY) &&
(jiffies < clock) && !st0x_aborted)
#if 0 && (DEBUG & PHASE_SELECTION)
{
temp = clock - jiffies;
if (!(jiffies % 5))
printk("seagate_st0x_timeout : %d            \r",temp);
}
printk("Done.                                             \n");
printk("scsi%d : status = %02x, seagate_st0x_timeout = %d, aborted = %02x \n",
hostno, status_read, temp, st0x_aborted);
#else
;
#endif
if ((jiffies >= clock) && !(status_read & STAT_BSY))
{
#if (DEBUG & PHASE_SELECTION)
printk ("scsi%d : NO CONNECT with target %d, status = %x \n",
hostno, target, STATUS);
#endif
return retcode(DID_NO_CONNECT);
}
if (st0x_aborted) {
CONTROL = BASE_CMD;
if (STATUS & STAT_BSY) {
printk("scsi%d : BST asserted after we've been aborted.\n",
hostno);
seagate_st0x_reset(NULL, 0);
return retcode(DID_RESET);
}
return retcode(st0x_aborted);
}
if ((nobuffs = SCint->use_sg)) {
#if (DEBUG & DEBUG_SG)
{
int i;
printk("scsi%d : scatter gather requested, using %d buffers.\n",
hostno, nobuffs);
for (i = 0; i < nobuffs; ++i)
printk("scsi%d : buffer %d address = %08x length = %d\n",
hostno, i, buffer[i].address, buffer[i].length);
}
#endif
buffer = (struct scatterlist *) SCint->buffer;
len = buffer->length;
data = (unsigned char *) buffer->address;
} else {
#if (DEBUG & DEBUG_SG)
printk("scsi%d : scatter gather not requested.\n", hostno);
#endif
buffer = NULL;
len = SCint->request_bufflen;
data = (unsigned char *) SCint->request_buffer;
}
#if (DEBUG & (PHASE_DATAIN | PHASE_DATAOUT))
printk("scsi%d : len = %d\n", hostno, len);
#endif
break;
#ifdef LINKED
case LINKED_RIGHT:
break;
case LINKED_WRONG:
break;
#endif
}
CONTROL = BASE_CMD | CMD_DRVR_ENABLE |
(((reselect == CAN_RECONNECT)
#ifdef LINKED
|| (reselect == LINKED_WRONG)
#endif
) ? CMD_ATTN : 0) ;
#if ((DEBUG & PHASE_ETC) == PHASE_ETC)
printk("scsi%d : phase = INFORMATION TRANSFER\n", hostno);
#endif
incommand = 1;
transfersize = SCint->transfersize;
underflow = SCint->underflow;
while (((status_read = STATUS) & STAT_BSY) && !st0x_aborted && !done)
{
#ifdef PARITY
if (status_read & STAT_PARITY)
{
printk("scsi%d : got parity error\n", hostno);
st0x_aborted = DID_PARITY;
}
#endif
if (status_read & STAT_REQ)
{
#if ((DEBUG & PHASE_ETC) == PHASE_ETC)
if ((newphase = (status_read & REQ_MASK)) != phase)
{
phase = newphase;
switch (phase)
{
case REQ_DATAOUT:
printk("scsi%d : phase = DATA OUT\n",
hostno);
break;
case REQ_DATAIN :
printk("scsi%d : phase = DATA IN\n",
hostno);
break;
case REQ_CMDOUT :
printk("scsi%d : phase = COMMAND OUT\n",
hostno);
break;
case REQ_STATIN :
printk("scsi%d : phase = STATUS IN\n",
hostno);
break;
case REQ_MSGOUT :
printk("scsi%d : phase = MESSAGE OUT\n",
hostno);
break;
case REQ_MSGIN :
printk("scsi%d : phase = MESSAGE IN\n",
hostno);
break;
default :
printk("scsi%d : phase = UNKNOWN\n",
hostno);
st0x_aborted = DID_ERROR;
}
}
#endif
switch (status_read & REQ_MASK)
{
case REQ_DATAOUT :
#ifdef FAST
if (!len) {
#if 0
printk("scsi%d: underflow to target %d lun %d \n",
hostno, target, lun);
st0x_aborted = DID_ERROR;
fast = 0;
#endif
break;
}
if (fast && transfersize && !(len % transfersize) && (len >= transfersize)
#ifdef FAST32
&& !(transfersize % 4)
#endif
) {
#if (DEBUG & DEBUG_FAST)
printk("scsi%d : FAST transfer, underflow = %d, transfersize = %d\n"
"         len = %d, data = %08x\n", hostno, SCint->underflow,
SCint->transfersize, len, data);
#endif
{
#ifdef FAST32
unsigned int *iop = phys_to_virt (st0x_dr);
const unsigned int *dp = (unsigned int *) data;
int xferlen = transfersize >> 2;
#else
unsigned char *iop = phys_to_virt (st0x_dr);
const unsigned char *dp = data;
int xferlen = transfersize;
#endif
for (; xferlen; --xferlen)
*iop = *dp++;
}
len -= transfersize;
data += transfersize;
#if (DEBUG & DEBUG_FAST)
printk("scsi%d : FAST transfer complete len = %d data = %08x\n",
hostno, len, data);
#endif
} else
#endif
{
while (len)
{
unsigned char stat;
stat = STATUS;
if (!(stat & STAT_BSY) || ((stat & REQ_MASK) != REQ_DATAOUT))
break;
if (stat & STAT_REQ)
{
WRITE_DATA (*data++);
--len;
}
}
}
if (!len && nobuffs) {
--nobuffs;
++buffer;
len = buffer->length;
data = (unsigned char *) buffer->address;
#if (DEBUG & DEBUG_SG)
printk("scsi%d : next scatter-gather buffer len = %d address = %08x\n",
hostno, len, data);
#endif
}
break;
case REQ_DATAIN :
#ifdef SLOW_HANDSHAKE
if (borken) {
#if (DEBUG & (PHASE_DATAIN))
transfered += len;
#endif
for (; len && (STATUS & (REQ_MASK | STAT_REQ)) == (REQ_DATAIN |
STAT_REQ); --len) {
*data++ = DATA;
borken_wait();
}
#if (DEBUG & (PHASE_DATAIN))
transfered -= len;
#endif
} else
#endif
#ifdef FAST
if (fast && transfersize && !(len % transfersize) && (len >= transfersize)
#ifdef FAST32
&& !(transfersize % 4)
#endif
) {
#if (DEBUG & DEBUG_FAST)
printk("scsi%d : FAST transfer, underflow = %d, transfersize = %d\n"
"         len = %d, data = %08x\n", hostno, SCint->underflow,
SCint->transfersize, len, data);
#endif
{
#ifdef FAST32
const unsigned int *iop = phys_to_virt (st0x_dr);
unsigned int *dp = (unsigned int *) data;
int xferlen = len >> 2;
#else
const unsigned char *iop = phys_to_virt (st0x_dr);
unsigned char *dp = data;
int xferlen = len;
#endif
for (; xferlen; --xferlen)
*dp++ = *iop;
}
len -= transfersize;
data += transfersize;
#if (DEBUG & PHASE_DATAIN)
printk("scsi%d: transfered += %d\n", hostno, transfersize);
transfered += transfersize;
#endif
#if (DEBUG & DEBUG_FAST)
printk("scsi%d : FAST transfer complete len = %d data = %08x\n",
hostno, len, data);
#endif
} else
#endif
{
#if (DEBUG & PHASE_DATAIN)
printk("scsi%d: transfered += %d\n", hostno, len);
transfered += len;
#endif
while (len)
{
unsigned char stat;
stat = STATUS;
if (!(stat & STAT_BSY) || ((stat & REQ_MASK) != REQ_DATAIN))
break;
if (stat & STAT_REQ)
{
*data++ = DATA;
--len;
}
}
#if (DEBUG & PHASE_DATAIN)
printk("scsi%d: transfered -= %d\n", hostno, len);
transfered -= len;
#endif
}
if (!len && nobuffs) {
--nobuffs;
++buffer;
len = buffer->length;
data = (unsigned char *) buffer->address;
#if (DEBUG & DEBUG_SG)
printk("scsi%d : next scatter-gather buffer len = %d address = %08x\n",
hostno, len, data);
#endif
}
break;
case REQ_CMDOUT :
while (((status_read = STATUS) & STAT_BSY) &&
((status_read & REQ_MASK) == REQ_CMDOUT))
if (status_read & STAT_REQ) {
DATA = *(const unsigned char *) cmnd;
cmnd = 1+(const unsigned char *) cmnd;
#ifdef SLOW_HANDSHAKE
if (borken)
borken_wait();
#endif
}
break;
case REQ_STATIN :
status = DATA;
break;
case REQ_MSGOUT :
CONTROL = BASE_CMD | CMD_DRVR_ENABLE;
switch (reselect) {
case CAN_RECONNECT:
DATA = IDENTIFY(1, lun);
#if (DEBUG & (PHASE_RESELECT | PHASE_MSGOUT))
printk("scsi%d : sent IDENTIFY message.\n", hostno);
#endif
break;
#ifdef LINKED
case LINKED_WRONG:
DATA = ABORT;
linked_connected = 0;
reselect = CAN_RECONNECT;
goto connect_loop;
#if (DEBUG & (PHASE_MSGOUT | DEBUG_LINKED))
printk("scsi%d : sent ABORT message to cancel incorrect I_T_L nexus.\n", hostno);
#endif
#endif
#if (DEBUG & DEBUG_LINKED)
printk("correct\n");
#endif
default:
DATA = NOP;
printk("scsi%d : target %d requested MSGOUT, sent NOP message.\n", hostno, target);
}
break;
case REQ_MSGIN :
switch (message = DATA) {
case DISCONNECT :
should_reconnect = 1;
current_data = data;
current_buffer = buffer;
current_bufflen = len;
current_nobuffs = nobuffs;
#ifdef LINKED
linked_connected = 0;
#endif
done=1;
#if (DEBUG & (PHASE_RESELECT | PHASE_MSGIN))
printk("scsi%d : disconnected.\n", hostno);
#endif
break;
#ifdef LINKED
case LINKED_CMD_COMPLETE:
case LINKED_FLG_CMD_COMPLETE:
#endif
case COMMAND_COMPLETE :
#if (DEBUG & PHASE_MSGIN)
printk("scsi%d : command complete.\n", hostno);
#endif
done = 1;
break;
case ABORT :
#if (DEBUG & PHASE_MSGIN)
printk("scsi%d : abort message.\n", hostno);
#endif
done=1;
break;
case SAVE_POINTERS :
current_buffer = buffer;
current_bufflen = len;
current_data = data;
current_nobuffs = nobuffs;
#if (DEBUG & PHASE_MSGIN)
printk("scsi%d : pointers saved.\n", hostno);
#endif
break;
case RESTORE_POINTERS:
buffer=current_buffer;
cmnd=current_cmnd;
data=current_data;
len=current_bufflen;
nobuffs=current_nobuffs;
#if (DEBUG & PHASE_MSGIN)
printk("scsi%d : pointers restored.\n", hostno);
#endif
break;
default:
if (message & 0x80) {
#if (DEBUG & PHASE_MSGIN)
printk("scsi%d : IDENTIFY message received from id %d, lun %d.\n",
hostno, target, message & 7);
#endif
} else {
#if (DEBUG & PHASE_MSGIN)
printk("scsi%d : unknown message %d from target %d.\n",
hostno, message, target);
#endif
}
}
break;
default :
printk("scsi%d : unknown phase.\n", hostno);
st0x_aborted = DID_ERROR;
}
#ifdef SLOW_HANDSHAKE
if (borken)
borken_wait();
#endif
}
}
#if (DEBUG & (PHASE_DATAIN | PHASE_DATAOUT | PHASE_EXIT))
printk("scsi%d : Transfered %d bytes\n", hostno, transfered);
#endif
#if (DEBUG & PHASE_EXIT)
#if 0
printk("Buffer : \n");
for (i = 0; i < 20; ++i)
printk ("%02x  ", ((unsigned char *) data)[i]);
printk("\n");
#endif
printk("scsi%d : status = ", hostno);
print_status(status);
printk("message = %02x\n", message);
#endif
#ifdef notyet
if (st0x_aborted) {
if (STATUS & STAT_BSY) {
seagate_st0x_reset(NULL);
st0x_aborted = DID_RESET;
}
abort_confirm = 1;
}
#endif
#ifdef LINKED
else {
switch (message) {
case LINKED_CMD_COMPLETE :
case LINKED_FLG_CMD_COMPLETE :
message = COMMAND_COMPLETE;
linked_target = current_target;
linked_lun = current_lun;
linked_connected = 1;
#if (DEBUG & DEBUG_LINKED)
printk("scsi%d : keeping I_T_L nexus established for linked command.\n",
hostno);
#endif
if ((status == INTERMEDIATE_GOOD) ||
(status == INTERMEDIATE_C_GOOD))
status = GOOD;
break;
default :
#if (DEBUG & DEBUG_LINKED)
printk("scsi%d : closing I_T_L nexus.\n", hostno);
#endif
linked_connected = 0;
}
}
#endif
if (should_reconnect) {
#if (DEBUG & PHASE_RESELECT)
printk("scsi%d : exiting seagate_st0x_queue_command() with reconnect enabled.\n",
hostno);
#endif
CONTROL = BASE_CMD | CMD_INTR ;
} else
CONTROL = BASE_CMD;
return retcode (st0x_aborted);
}
int seagate_st0x_abort (Scsi_Cmnd * SCpnt)
{
st0x_aborted = DID_ABORT;
return SCSI_ABORT_PENDING;
}
int seagate_st0x_reset (Scsi_Cmnd * SCpnt, unsigned int reset_flags)
{
unsigned clock;
#ifdef DEBUG
printk("In seagate_st0x_reset()\n");
#endif
CONTROL = BASE_CMD | CMD_RST;
clock=jiffies+2;
while (jiffies < clock);
CONTROL = BASE_CMD;
st0x_aborted = DID_RESET;
#ifdef DEBUG
printk("SCSI bus reset.\n");
#endif
return SCSI_RESET_WAKEUP;
}
#include <asm/segment.h>
#include "sd.h"
#include <scsi/scsi_ioctl.h>
int seagate_st0x_biosparam(Disk * disk, kdev_t dev, int* ip) {
unsigned char buf[256 + sizeof(int) * 2], cmd[6], *data, *page;
int *sizes, result, formatted_sectors, total_sectors;
int cylinders, heads, sectors;
int capacity;
if (disk->device->scsi_level < 2)
return -1;
sizes = (int *) buf;
data = (unsigned char *) (sizes + 2);
cmd[0] = MODE_SENSE;
cmd[1] = (disk->device->lun << 5) & 0xe5;
cmd[2] = 0x04;
cmd[3] = 0;
cmd[4] = 255;
cmd[5] = 0;
sizes[0] = 0;
sizes[1] = 256;
memcpy (data, cmd, 6);
if (!(result = kernel_scsi_ioctl (disk->device, SCSI_IOCTL_SEND_COMMAND, (void *) buf))) {
page = data + 4 + data[3];
heads = (int) page[5];
cylinders = (page[2] << 16) | (page[3] << 8) | page[4];
cmd[2] = 0x03;
memcpy (data, cmd, 6);
if (!(result = kernel_scsi_ioctl (disk->device, SCSI_IOCTL_SEND_COMMAND, (void *) buf))) {
page = data + 4 + data[3];
sectors = (page[10] << 8) | page[11];
formatted_sectors = (data[4 + 1] << 16) | (data[4 + 2] << 8) |
data[4 + 3] ;
total_sectors = (heads * cylinders * sectors);
printk("scsi%d : heads = %d cylinders = %d sectors = %d total = %d formatted = %d\n",
hostno, heads, cylinders, sectors, total_sectors, formatted_sectors);
if (!heads || !sectors || !cylinders)
result = -1;
else
cylinders -= ((total_sectors - formatted_sectors) / (heads * sectors));
if ((cylinders > 1024) || (sectors > 64)) {
capacity = heads * sectors * cylinders;
sectors = 17;
heads = 2;
capacity = capacity / sectors;
while (cylinders > 1024)
{
heads *= 2;
cylinders = capacity / heads;
}
}
ip[0] = heads;
ip[1] = sectors;
ip[2] = cylinders;
}
}
return result;
}
#ifdef MODULE
Scsi_Host_Template driver_template = SEAGATE_ST0X;
#include "scsi_module.c"
#endif