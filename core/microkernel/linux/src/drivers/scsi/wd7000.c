#ifdef MODULE
# include <linux/module.h>
#endif
#if (LINUX_VERSION_CODE >= 0x020100)
# include <asm/spinlock.h>
#endif
#include <stdarg.h>
#include <linux/kernel.h>
#include <linux/head.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/sched.h>
#include <linux/malloc.h>
#include <asm/system.h>
#include <asm/dma.h>
#include <asm/io.h>
#include <linux/ioport.h>
#include <linux/proc_fs.h>
#include <linux/blk.h>
#include <linux/version.h>
#include <linux/stat.h>
#include "scsi.h"
#include "hosts.h"
#include "sd.h"
#include <scsi/scsicam.h>
#undef WD7000_DEBUG
#define WD7000_DEFINES
#include "wd7000.h"
struct proc_dir_entry proc_scsi_wd7000 =
{
PROC_SCSI_7000FASST,
6,
"wd7000",
S_IFDIR | S_IRUGO | S_IXUGO,
2
};
static const long wd7000_biosaddr[] = {
0xc0000, 0xc2000, 0xc4000, 0xc6000, 0xc8000, 0xca000, 0xcc000, 0xce000,
0xd0000, 0xd2000, 0xd4000, 0xd6000, 0xd8000, 0xda000, 0xdc000, 0xde000
};
#define NUM_ADDRS (sizeof (wd7000_biosaddr) / sizeof (long))
static const ushort wd7000_iobase[] = {
0x0300, 0x0308, 0x0310, 0x0318, 0x0320, 0x0328, 0x0330, 0x0338,
0x0340, 0x0348, 0x0350, 0x0358, 0x0360, 0x0368, 0x0370, 0x0378,
0x0380, 0x0388, 0x0390, 0x0398, 0x03a0, 0x03a8, 0x03b0, 0x03b8,
0x03c0, 0x03c8, 0x03d0, 0x03d8, 0x03e0, 0x03e8, 0x03f0, 0x03f8
};
#define NUM_IOPORTS (sizeof (wd7000_iobase) / sizeof (ushort))
static const short wd7000_irq[] = { 3, 4, 5, 7, 9, 10, 11, 12, 14, 15 };
#define NUM_IRQS (sizeof (wd7000_irq) / sizeof (short))
static const short wd7000_dma[] = { 5, 6, 7 };
#define NUM_DMAS (sizeof (wd7000_dma) / sizeof (short))
static struct Scsi_Host *wd7000_host[IRQS];
static Config configs[] =
{
{ 15, 6, 0x350, BUS_ON, BUS_OFF },
{ 11, 5, 0x320, BUS_ON, BUS_OFF },
{ 7, 6, 0x350, BUS_ON, BUS_OFF },
{ -1, -1, 0x0, BUS_ON, BUS_OFF }
};
#define NUM_CONFIGS (sizeof(configs)/sizeof(Config))
static const Signature signatures[] =
{
{"SSTBIOS", 0x0000d, 7}
};
#define NUM_SIGNATURES (sizeof(signatures)/sizeof(Signature))
static Scb scbs[MAX_SCBS];
static void setup_error (char *mesg, int *ints)
{
if (ints[0] == 3)
printk ("wd7000_setup: \"wd7000=%d,%d,0x%x\" -> %s\n",
ints[1], ints[2], ints[3], mesg);
else if (ints[0] == 4)
printk ("wd7000_setup: \"wd7000=%d,%d,0x%x,%d\" -> %s\n",
ints[1], ints[2], ints[3], ints[4], mesg);
else
printk ("wd7000_setup: \"wd7000=%d,%d,0x%x,%d,%d\" -> %s\n",
ints[1], ints[2], ints[3], ints[4], ints[5], mesg);
}
void wd7000_setup (char *str, int *ints)
{
static short wd7000_card_num = 0;
short i, j;
if (wd7000_card_num >= NUM_CONFIGS) {
printk ("%s: Too many \"wd7000=\" configurations in "
"command line!\n", __FUNCTION__);
return;
}
if ((ints[0] < 3) || (ints[0] > 5))
printk ("%s: Error in command line!  "
"Usage: wd7000=<IRQ>,<DMA>,<IO>[,<BUS_ON>[,<BUS_OFF>]]\n",
__FUNCTION__);
else {
for (i = 0; i < NUM_IRQS; i++)
if (ints[1] == wd7000_irq[i])
break;
if (i == NUM_IRQS) {
setup_error ("invalid IRQ.", ints);
return;
}
else
configs[wd7000_card_num].irq = ints[1];
for (i = 0; i < NUM_DMAS; i++)
if (ints[2] == wd7000_dma[i])
break;
if (i == NUM_DMAS) {
setup_error ("invalid DMA channel.", ints);
return;
}
else
configs[wd7000_card_num].dma = ints[2];
for (i = 0; i < NUM_IOPORTS; i++)
if (ints[3] == wd7000_iobase[i])
break;
if (i == NUM_IOPORTS) {
setup_error ("invalid I/O base address.", ints);
return;
}
else
configs[wd7000_card_num].iobase = ints[3];
if (ints[0] > 3) {
if ((ints[4] < 500) || (ints[4] > 31875)) {
setup_error ("BUS_ON value is out of range (500 to 31875 nanoseconds)!",
ints);
configs[wd7000_card_num].bus_on = BUS_ON;
}
else
configs[wd7000_card_num].bus_on = ints[4] / 125;
}
else
configs[wd7000_card_num].bus_on = BUS_ON;
if (ints[0] > 4) {
if ((ints[5] < 500) || (ints[5] > 31875)) {
setup_error ("BUS_OFF value is out of range (500 to 31875 nanoseconds)!",
ints);
configs[wd7000_card_num].bus_off = BUS_OFF;
}
else
configs[wd7000_card_num].bus_off = ints[5] / 125;
}
else
configs[wd7000_card_num].bus_off = BUS_OFF;
if (wd7000_card_num) {
for (i = 0; i < (wd7000_card_num - 1); i++)
for (j = i + 1; j < wd7000_card_num; j++)
if (configs[i].irq == configs[j].irq) {
setup_error ("duplicated IRQ!", ints);
return;
}
else if (configs[i].dma == configs[j].dma) {
setup_error ("duplicated DMA channel!", ints);
return;
}
else if (configs[i].iobase == configs[j].iobase) {
setup_error ("duplicated I/O base address!", ints);
return;
}
}
#ifdef WD7000_DEBUG
printk ("%s: IRQ=%d, DMA=%d, I/O=0x%x, BUS_ON=%dns, BUS_OFF=%dns\n",
__FUNCTION__,
configs[wd7000_card_num].irq,
configs[wd7000_card_num].dma,
configs[wd7000_card_num].iobase,
configs[wd7000_card_num].bus_on * 125,
configs[wd7000_card_num].bus_off * 125);
#endif
wd7000_card_num++;
}
}
static inline void any2scsi (unchar *scsi, int any)
{
*scsi++ = ((i_u) any).u[2];
*scsi++ = ((i_u) any).u[1];
*scsi = ((i_u) any).u[0];
}
static inline int scsi2int (unchar *scsi)
{
i_u result;
result.i = 0;
result.u[2] = *scsi++;
result.u[1] = *scsi++;
result.u[0] = *scsi;
return (result.i);
}
static inline void wd7000_enable_intr (Adapter *host)
{
host->control |= INT_EN;
outb (host->control, host->iobase + ASC_CONTROL);
}
static inline void wd7000_enable_dma (Adapter *host)
{
host->control |= DMA_EN;
outb (host->control, host->iobase + ASC_CONTROL);
set_dma_mode (host->dma, DMA_MODE_CASCADE);
enable_dma (host->dma);
}
static inline short WAIT (uint port, uint mask, uint allof, uint noneof)
{
register uint WAITbits;
register ulong WAITtimeout = jiffies + WAITnexttimeout;
while (jiffies <= WAITtimeout) {
WAITbits = inb (port) & mask;
if (((WAITbits & allof) == allof) && ((WAITbits & noneof) == 0))
return (0);
}
return (1);
}
static inline void delay (uint how_long)
{
register ulong time = jiffies + how_long;
while (jiffies < time);
}
static inline int wd7000_command_out (Adapter *host, unchar *cmd, int len)
{
if (! WAIT (host->iobase + ASC_STAT, ASC_STATMASK, CMD_RDY, 0)) {
for ( ; len--; cmd++)
do {
outb (*cmd, host->iobase + ASC_COMMAND);
WAIT (host->iobase + ASC_STAT, ASC_STATMASK, CMD_RDY, 0);
} while (inb (host->iobase + ASC_STAT) & CMD_REJ);
return (1);
}
printk ("%s: WAIT failed (%d)\n", __FUNCTION__, len + 1);
return (0);
}
static inline void scbs_init (void)
{
short i;
for (i = 0; i < MAX_SCBS; i++)
memset ((void *) &(scbs[i]), 0, sizeof (Scb));
}
static inline Scb *scb_alloc (void)
{
Scb *scb = NULL;
ulong flags;
short i;
#ifdef WD7000_DEBUG
short free_scbs = 0;
#endif
save_flags (flags);
cli ();
for (i = 0; i < MAX_SCBS; i++)
if (! scbs[i].used) {
scbs[i].used = 1;
scb = &(scbs[i]);
break;
}
#ifdef WD7000_DEBUG
for (i = 0; i < MAX_SCBS; i++)
free_scbs += scbs[i].used ? 0 : 1;
printk ("wd7000_%s: allocating scb (0x%08x), %d scbs free\n",
__FUNCTION__, (int) scb, free_scbs);
#endif
restore_flags (flags);
return (scb);
}
static inline void scb_free (Scb *scb)
{
short i;
ulong flags;
save_flags (flags);
cli ();
for (i = 0; i < MAX_SCBS; i++)
if (&(scbs[i]) == scb) {
memset ((void *) &(scbs[i]), 0, sizeof (Scb));
break;
}
if (i == MAX_SCBS)
printk ("wd7000_%s: trying to free alien scb (0x%08x)...\n",
__FUNCTION__, (int) scb);
#ifdef WD7000_DEBUG
else
printk ("wd7000_%s: freeing scb (0x%08x)\n", __FUNCTION__, (int) scb);
#endif
restore_flags (flags);
}
static int mail_out (Adapter *host, Scb *scbptr)
{
register int i, ogmb;
ulong flags;
unchar start_ogmb;
Mailbox *ogmbs = host->mb.ogmb;
int *next_ogmb = &(host->next_ogmb);
#ifdef WD7000_DEBUG
printk ("wd7000_%s: 0x%08x", __FUNCTION__, (int) scbptr);
#endif
save_flags (flags);
cli ();
ogmb = *next_ogmb;
for (i = 0; i < OGMB_CNT; i++) {
if (ogmbs[ogmb].status == 0) {
#ifdef WD7000_DEBUG
printk (" using OGMB 0x%x", ogmb);
#endif
ogmbs[ogmb].status = 1;
any2scsi ((unchar *) ogmbs[ogmb].scbptr, (int) scbptr);
*next_ogmb = (ogmb + 1) % OGMB_CNT;
break;
}
else
ogmb = (ogmb + 1) % OGMB_CNT;
}
restore_flags (flags);
#ifdef WD7000_DEBUG
printk (", scb is 0x%08x", (int) scbptr);
#endif
if (i >= OGMB_CNT) {
#ifdef WD7000_DEBUG
printk (", no free OGMBs.\n");
#endif
return (0);
}
wd7000_enable_intr (host);
start_ogmb = START_OGMB | ogmb;
wd7000_command_out (host, &start_ogmb, 1);
#ifdef WD7000_DEBUG
printk (", awaiting interrupt.\n");
#endif
return (1);
}
int make_code (uint hosterr, uint scsierr)
{
#ifdef WD7000_DEBUG
int in_error = hosterr;
#endif
switch ((hosterr >> 8) & 0xff) {
case 0:
hosterr = DID_ERROR;
break;
case 1:
hosterr = DID_OK;
break;
case 2:
hosterr = DID_OK;
break;
case 4:
hosterr = DID_TIME_OUT;
break;
case 5:
hosterr = DID_RESET;
break;
case 6:
hosterr = DID_BAD_TARGET;
break;
case 80:
case 81:
hosterr = DID_BAD_INTR;
break;
case 82:
hosterr = DID_ABORT;
break;
case 83:
case 84:
hosterr = DID_RESET;
break;
default:
hosterr = DID_ERROR;
}
#ifdef WD7000_DEBUG
if (scsierr || hosterr)
printk ("\nSCSI command error: SCSI 0x%02x host 0x%04x return %d\n",
scsierr, in_error, hosterr);
#endif
return (scsierr | (hosterr << 16));
}
static void wd7000_scsi_done (Scsi_Cmnd *SCpnt)
{
#ifdef WD7000_DEBUG
printk ("%s: 0x%08x\n", __FUNCTION__, (int) SCpnt);
#endif
SCpnt->SCp.phase = 0;
}
static inline void wd7000_intr_ack (Adapter *host)
{
outb (0, host->iobase + ASC_INTR_ACK);
}
void wd7000_intr_handle (int irq, void *dev_id, struct pt_regs *regs)
{
register int flag, icmb, errstatus, icmb_status;
register int host_error, scsi_error;
register Scb *scb;
register IcbAny *icb;
register Scsi_Cmnd *SCpnt;
Adapter *host = (Adapter *) wd7000_host[irq - IRQ_MIN]->hostdata;
Mailbox *icmbs = host->mb.icmb;
host->int_counter++;
#ifdef WD7000_DEBUG
printk ("%s: irq = %d, host = 0x%08x\n", __FUNCTION__, irq, (int) host);
#endif
flag = inb (host->iobase + ASC_INTR_STAT);
#ifdef WD7000_DEBUG
printk ("%s: intr stat = 0x%02x\n", __FUNCTION__, flag);
#endif
if (! (inb (host->iobase + ASC_STAT) & INT_IM)) {
#ifdef WD7000_DEBUG
printk ("%s: phantom interrupt...\n", __FUNCTION__);
#endif
wd7000_intr_ack (host);
return;
}
if (flag & MB_INTR) {
if (! (flag & IMB_INTR)) {
#ifdef WD7000_DEBUG
printk ("%s: free outgoing mailbox\n", __FUNCTION__);
#endif
wd7000_intr_ack (host);
return;
}
else {
icmb = flag & MB_MASK;
icmb_status = icmbs[icmb].status;
if (icmb_status & 0x80) {
#ifdef WD7000_DEBUG
printk ("%s: unsolicited interrupt 0x%02x\n",
__FUNCTION__, icmb_status);
#endif
wd7000_intr_ack (host);
return;
}
scb = (Scb *) bus_to_virt (scsi2int ((unchar *) icmbs[icmb].scbptr));
icmbs[icmb].status = 0;
if (!(scb->op & ICB_OP_MASK)) {
SCpnt = scb->SCpnt;
if (--(SCpnt->SCp.phase) <= 0) {
host_error = scb->vue | (icmb_status << 8);
scsi_error = scb->status;
errstatus = make_code (host_error, scsi_error);
SCpnt->result = errstatus;
scb_free (scb);
SCpnt->scsi_done (SCpnt);
}
}
else {
icb = (IcbAny *) scb;
icb->status = icmb_status;
icb->phase = 0;
}
}
}
wd7000_intr_ack (host);
#ifdef WD7000_DEBUG
printk ("%s: return from interrupt handler\n", __FUNCTION__);
#endif
}
void do_wd7000_intr_handle (int irq, void *dev_id, struct pt_regs *regs)
{
#if (LINUX_VERSION_CODE >= 0x020100)
ulong flags;
spin_lock_irqsave (&io_request_lock, flags);
#endif
wd7000_intr_handle (irq, dev_id, regs);
#if (LINUX_VERSION_CODE >= 0x020100)
spin_unlock_irqrestore (&io_request_lock, flags);
#endif
}
int wd7000_queuecommand (Scsi_Cmnd *SCpnt, void (*done) (Scsi_Cmnd *))
{
register Scb *scb;
register Sgb *sgb;
register Adapter *host = (Adapter *) SCpnt->host->hostdata;
if ((scb = scb_alloc ()) == NULL) {
printk ("%s: Cannot allocate SCB!\n", __FUNCTION__);
return (0);
}
SCpnt->scsi_done = done;
SCpnt->SCp.phase = 1;
SCpnt->host_scribble = (unchar *) scb;
scb->idlun = ((SCpnt->target << 5) & 0xe0) | (SCpnt->lun & 7);
scb->direc = 0x40;
scb->SCpnt = SCpnt;
scb->host = host;
memcpy (scb->cdb, SCpnt->cmnd, SCpnt->cmd_len);
if (SCpnt->use_sg) {
struct scatterlist *sg = (struct scatterlist *) SCpnt->request_buffer;
uint i;
if (SCpnt->host->sg_tablesize == SG_NONE)
panic ("%s: scatter/gather not supported.\n", __FUNCTION__);
#ifdef WD7000_DEBUG
else
printk ("Using scatter/gather with %d elements.\n", SCpnt->use_sg);
#endif
sgb = scb->sgb;
scb->op = 1;
any2scsi (scb->dataptr, (int) sgb);
any2scsi (scb->maxlen, SCpnt->use_sg * sizeof (Sgb));
for (i = 0; i < SCpnt->use_sg; i++) {
any2scsi (sgb[i].ptr, (int) sg[i].address);
any2scsi (sgb[i].len, sg[i].length);
}
}
else {
scb->op = 0;
any2scsi (scb->dataptr, (int) SCpnt->request_buffer);
any2scsi (scb->maxlen, SCpnt->request_bufflen);
}
while (! mail_out (host, scb));
return (1);
}
int wd7000_command (Scsi_Cmnd *SCpnt)
{
if (! wd7000_queuecommand (SCpnt, wd7000_scsi_done))
return (-1);
while (SCpnt->SCp.phase > 0)
barrier ();
return (SCpnt->result);
}
int wd7000_diagnostics (Adapter *host, int code)
{
static IcbDiag icb = { ICB_OP_DIAGNOSTICS };
static unchar buf[256];
ulong timeout;
icb.type = code;
any2scsi (icb.len, sizeof (buf));
any2scsi (icb.ptr, (int) &buf);
icb.phase = 1;
mail_out (host, (Scb *) &icb);
for (timeout = jiffies + WAITnexttimeout; icb.phase && (jiffies < timeout); )
barrier ();
if (icb.phase) {
printk ("%s: timed out.\n", __FUNCTION__);
return (0);
}
if (make_code (icb.vue | (icb.status << 8), 0)) {
printk ("%s: failed (0x%02x,0x%02x)\n", __FUNCTION__, icb.vue, icb.status);
return (0);
}
return (1);
}
int wd7000_init (Adapter *host)
{
InitCmd init_cmd =
{
INITIALIZATION,
7,
host->bus_on,
host->bus_off,
0,
{ 0, 0, 0 },
OGMB_CNT,
ICMB_CNT
};
int diag;
outb (ASC_RES, host->iobase + ASC_CONTROL);
delay (1);
outb (0, host->iobase + ASC_CONTROL);
host->control = 0;
if (WAIT (host->iobase + ASC_STAT, ASC_STATMASK, CMD_RDY, 0)) {
printk ("%s: WAIT timed out.\n", __FUNCTION__);
return (0);
}
if ((diag = inb (host->iobase + ASC_INTR_STAT)) != 1) {
printk ("%s: ", __FUNCTION__);
switch (diag) {
case 2: printk ("RAM failure.\n");
break;
case 3: printk ("FIFO R/W failed\n");
break;
case 4: printk ("SBIC register R/W failed\n");
break;
case 5: printk ("Initialization D-FF failed.\n");
break;
case 6: printk ("Host IRQ D-FF failed.\n");
break;
case 7: printk ("ROM checksum error.\n");
break;
default: printk ("diagnostic code 0x%02Xh received.\n", diag);
}
return (0);
}
memset (&(host->mb), 0, sizeof (host->mb));
any2scsi ((unchar *) &(init_cmd.mailboxes), (int) &(host->mb));
if (! wd7000_command_out (host, (unchar *) &init_cmd, sizeof (init_cmd))) {
printk ("%s: adapter initialization failed.\n", __FUNCTION__);
return (0);
}
if (WAIT (host->iobase + ASC_STAT, ASC_STATMASK, ASC_INIT, 0)) {
printk ("%s: WAIT timed out.\n", __FUNCTION__);
return (0);
}
if (request_irq (host->irq, do_wd7000_intr_handle, SA_INTERRUPT, "wd7000", NULL)) {
printk ("%s: can't get IRQ %d.\n", __FUNCTION__, host->irq);
return (0);
}
if (request_dma (host->dma, "wd7000")) {
printk ("%s: can't get DMA channel %d.\n", __FUNCTION__, host->dma);
free_irq (host->irq, NULL);
return (0);
}
wd7000_enable_dma (host);
wd7000_enable_intr (host);
if (! wd7000_diagnostics (host, ICB_DIAG_FULL)) {
free_dma (host->dma);
free_irq (host->irq, NULL);
return (0);
}
return (1);
}
void wd7000_revision (Adapter *host)
{
static IcbRevLvl icb = { ICB_OP_GET_REVISION };
icb.phase = 1;
mail_out (host, (Scb *) &icb);
while (icb.phase)
barrier ();
host->rev1 = icb.primary;
host->rev2 = icb.secondary;
}
#undef SPRINTF
#define SPRINTF(args...) { if (pos < (buffer + length)) pos += sprintf (pos, ## args); }
int wd7000_set_info (char *buffer, int length, struct Scsi_Host *host)
{
ulong flags;
save_flags (flags);
cli ();
#ifdef WD7000_DEBUG
printk ("Buffer = <%.*s>, length = %d\n", length, buffer, length);
#endif
printk ("Sorry, this function is currently out of order...\n");
restore_flags (flags);
return (length);
}
int wd7000_proc_info (char *buffer, char **start, off_t offset, int length, int hostno, int inout)
{
struct Scsi_Host *host = NULL;
Scsi_Device *scd;
Adapter *adapter;
ulong flags;
char *pos = buffer;
short i;
#ifdef WD7000_DEBUG
Mailbox *ogmbs, *icmbs;
short count;
#endif
for (i = 0; i < IRQS; i++)
if (wd7000_host[i] && (wd7000_host[i]->host_no == hostno)) {
host = wd7000_host[i];
break;
}
if (! host)
return (-ESRCH);
if (inout)
return (wd7000_set_info (buffer, length, host));
adapter = (Adapter *) host->hostdata;
save_flags (flags);
cli ();
SPRINTF ("Host scsi%d: Western Digital WD-7000 (rev %d.%d)\n", hostno, adapter->rev1, adapter->rev2);
SPRINTF ("  IO base:      0x%x\n", adapter->iobase);
SPRINTF ("  IRQ:          %d\n", adapter->irq);
SPRINTF ("  DMA channel:  %d\n", adapter->dma);
SPRINTF ("  Interrupts:   %d\n", adapter->int_counter);
SPRINTF ("  BUS_ON time:  %d nanoseconds\n", adapter->bus_on * 125);
SPRINTF ("  BUS_OFF time: %d nanoseconds\n", adapter->bus_off * 125);
#ifdef WD7000_DEBUG
ogmbs = adapter->mb.ogmb;
icmbs = adapter->mb.icmb;
SPRINTF ("\nControl port value: 0x%x\n", adapter->control);
SPRINTF ("Incoming mailbox:\n");
SPRINTF ("  size: %d\n", ICMB_CNT);
SPRINTF ("  queued messages: ");
for (i = count = 0; i < ICMB_CNT; i++)
if (icmbs[i].status) {
count++;
SPRINTF ("0x%x ", i);
}
SPRINTF (count ? "\n" : "none\n");
SPRINTF ("Outgoing mailbox:\n");
SPRINTF ("  size: %d\n", OGMB_CNT);
SPRINTF ("  next message: 0x%x\n", adapter->next_ogmb);
SPRINTF ("  queued messages: ");
for (i = count = 0; i < OGMB_CNT; i++)
if (ogmbs[i].status) {
count++;
SPRINTF ("0x%x ", i);
}
SPRINTF (count ? "\n" : "none\n");
#endif
#if (LINUX_VERSION_CODE >= 0x020100)
scd = host->host_queue;
#else
scd = scsi_devices;
#endif
SPRINTF ("\nAttached devices: %s\n", scd ? "" : "none");
for ( ; scd; scd = scd->next)
if (scd->host->host_no == hostno) {
SPRINTF ("  [Channel: %02d, Id: %02d, Lun: %02d]  ",
scd->channel, scd->id, scd->lun);
SPRINTF ("%s ", (scd->type < MAX_SCSI_DEVICE_CODE) ?
scsi_device_types[(short) scd->type] : "Unknown device");
for (i = 0; (i < 8) && (scd->vendor[i] >= 0x20); i++)
SPRINTF ("%c", scd->vendor[i]);
SPRINTF (" ");
for (i = 0; (i < 16) && (scd->model[i] >= 0x20); i++)
SPRINTF ("%c", scd->model[i]);
SPRINTF ("\n");
}
SPRINTF ("\n");
restore_flags (flags);
*start = buffer + offset;
if ((pos - buffer) < offset)
return (0);
else if ((pos - buffer - offset) < length)
return (pos - buffer - offset);
else
return (length);
}
int wd7000_detect (Scsi_Host_Template *tpnt)
{
short present = 0, biosaddr_ptr, sig_ptr, i, pass;
short biosptr[NUM_CONFIGS];
uint iobase;
Adapter *host = NULL;
struct Scsi_Host *sh;
#ifdef WD7000_DEBUG
printk ("%s: started\n", __FUNCTION__);
#endif
scbs_init ();
for (i = 0; i < IRQS; wd7000_host[i++] = NULL);
for (i = 0; i < NUM_CONFIGS; biosptr[i++] = -1);
tpnt->proc_dir = &proc_scsi_wd7000;
tpnt->proc_info = &wd7000_proc_info;
for (pass = 0; pass < NUM_CONFIGS; pass++) {
short bios_match = 1;
#ifdef WD7000_DEBUG
printk ("%s: pass %d\n", __FUNCTION__, pass + 1);
#endif
for (biosaddr_ptr = 0; bios_match && (biosaddr_ptr < NUM_ADDRS); biosaddr_ptr++)
for (sig_ptr = 0; bios_match && (sig_ptr < NUM_SIGNATURES); sig_ptr++) {
for (i = 0; i < pass; i++)
if (biosptr[i] == biosaddr_ptr)
break;
if (i == pass) {
#if (LINUX_VERSION_CODE >= 0x020100)
char *biosaddr = (char *) ioremap (wd7000_biosaddr[biosaddr_ptr] +
signatures[sig_ptr].ofs,
signatures[sig_ptr].len);
#else
char *biosaddr = (char *) (wd7000_biosaddr[biosaddr_ptr] +
signatures[sig_ptr].ofs);
#endif
bios_match = memcmp (biosaddr, signatures[sig_ptr].sig,
signatures[sig_ptr].len);
#if (LINUX_VERSION_CODE >= 0x020100)
iounmap (biosaddr);
#else
#endif
if (! bios_match) {
biosptr[pass] = biosaddr_ptr;
#ifdef WD7000_DEBUG
printk ("WD-7000 SST BIOS detected at 0x%lx: checking...\n",
wd7000_biosaddr[biosaddr_ptr]);
#endif
}
}
}
#ifdef WD7000_DEBUG
if (bios_match)
printk ("WD-7000 SST BIOS not detected...\n");
#endif
if (configs[pass].irq < 0)
continue;
iobase = configs[pass].iobase;
#ifdef WD7000_DEBUG
printk ("%s: check IO 0x%x region...\n", __FUNCTION__, iobase);
#endif
if (! check_region (iobase, 4)) {
#ifdef WD7000_DEBUG
printk ("%s: ASC reset (IO 0x%x) ...", __FUNCTION__, iobase);
#endif
outb (ASC_RES, iobase + ASC_CONTROL);
delay (1);
outb (0, iobase + ASC_CONTROL);
if (WAIT (iobase + ASC_STAT, ASC_STATMASK, CMD_RDY, 0))
#ifdef WD7000_DEBUG
{
printk ("failed!\n");
continue;
}
else
printk ("ok!\n");
#else
continue;
#endif
if (inb (iobase + ASC_INTR_STAT) == 1) {
sh = scsi_register (tpnt, sizeof (Adapter));
host = (Adapter *) sh->hostdata;
#ifdef WD7000_DEBUG
printk ("%s: adapter allocated at 0x%x\n", __FUNCTION__, (int) host);
#endif
memset (host, 0, sizeof (Adapter));
host->irq = configs[pass].irq;
host->dma = configs[pass].dma;
host->iobase = iobase;
host->int_counter = 0;
host->bus_on = configs[pass].bus_on;
host->bus_off = configs[pass].bus_off;
host->sh = wd7000_host[host->irq - IRQ_MIN] = sh;
#ifdef WD7000_DEBUG
printk ("%s: Trying to init WD-7000 card at IO 0x%x, IRQ %d, DMA %d...\n",
__FUNCTION__, host->iobase, host->irq, host->dma);
#endif
if (! wd7000_init (host)) {
scsi_unregister (sh);
continue;
}
wd7000_revision (host);
request_region (host->iobase, 4, "wd7000");
if (host->rev1 < 6)
sh->sg_tablesize = SG_NONE;
present++;
printk ("Western Digital WD-7000 (rev %d.%d) ",
host->rev1, host->rev2);
printk ("using IO 0x%x, IRQ %d, DMA %d.\n",
host->iobase, host->irq, host->dma);
printk ("  BUS_ON time: %dns, BUS_OFF time: %dns\n",
host->bus_on * 125, host->bus_off * 125);
}
}
#ifdef WD7000_DEBUG
else
printk ("%s: IO 0x%x region is already allocated!\n", __FUNCTION__, iobase);
#endif
}
if (! present)
printk ("Failed initialization of WD-7000 SCSI card!\n");
return (present);
}
int wd7000_abort (Scsi_Cmnd *SCpnt)
{
Adapter *host = (Adapter *) SCpnt->host->hostdata;
if (inb (host->iobase + ASC_STAT) & INT_IM) {
printk ("%s: lost interrupt\n", __FUNCTION__);
wd7000_intr_handle (host->irq, NULL, NULL);
return (SCSI_ABORT_SUCCESS);
}
return (SCSI_ABORT_SNOOZE);
}
int wd7000_reset (Scsi_Cmnd *SCpnt, uint flags)
{
return (SCSI_RESET_PUNT);
}
int wd7000_biosparam (Disk *disk, kdev_t dev, int *ip)
{
#ifdef WD7000_DEBUG
printk ("%s: dev=%s, size=%d, ", __FUNCTION__, kdevname (dev), disk->capacity);
#endif
ip[0] = 64;
ip[1] = 32;
ip[2] = disk->capacity / (64 * 32);
if (ip[2] >= 1024) {
int info[3];
if ((scsicam_bios_param (disk, dev, info) < 0) ||
!(((info[0] == 64) && (info[1] == 32)) ||
((info[0] == 255) && (info[1] == 63)))) {
printk ("%s: unable to verify geometry for disk with >1GB.\n"
"                  using extended translation.\n",
__FUNCTION__);
ip[0] = 255;
ip[1] = 63;
ip[2] = disk->capacity / (255 * 63);
}
else {
ip[0] = info[0];
ip[1] = info[1];
ip[2] = info[2];
if (info[0] == 255)
printk ("%s: current partition table is using extended translation.\n",
__FUNCTION__);
}
}
#ifdef WD7000_DEBUG
printk ("bios geometry: head=%d, sec=%d, cyl=%d\n", ip[0], ip[1], ip[2]);
printk ("WARNING: check, if the bios geometry is correct.\n");
#endif
return (0);
}
#ifdef MODULE
Scsi_Host_Template driver_template = WD7000;
#include "scsi_module.c"
#endif