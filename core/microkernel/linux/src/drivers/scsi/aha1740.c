#ifdef MODULE
#include <linux/module.h>
#endif
#include <linux/kernel.h>
#include <linux/head.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/ioport.h>
#include <linux/proc_fs.h>
#include <linux/sched.h>
#include <asm/dma.h>
#include <asm/system.h>
#include <asm/io.h>
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "sd.h"
#include "aha1740.h"
#include<linux/stat.h>
struct proc_dir_entry proc_scsi_aha1740 = {
PROC_SCSI_AHA1740, 7, "aha1740",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#ifdef DEBUG
#define DEB(x) x
#else
#define DEB(x)
#endif
struct aha1740_hostdata {
unsigned int slot;
unsigned int translation;
unsigned int last_ecb_used;
struct ecb ecb[AHA1740_ECBS];
};
#define HOSTDATA(host) ((struct aha1740_hostdata *) &host->hostdata)
static struct Scsi_Host * aha_host[8] = {NULL, };
int aha1740_proc_info(char *buffer, char **start, off_t offset,
int length, int hostno, int inout)
{
int len;
struct Scsi_Host * shpnt;
struct aha1740_hostdata *host;
if (inout)
return(-ENOSYS);
for (len = 0; len < 8; len++) {
shpnt = aha_host[len];
if (shpnt && shpnt->host_no == hostno)
break;
}
host = HOSTDATA(shpnt);
len = sprintf(buffer, "aha174x at IO:%x, IRQ %d, SLOT %d.\n"
"Extended translation %sabled.\n",
shpnt->io_port, shpnt->irq, host->slot,
host->translation ? "en" : "dis");
if (offset > len) {
*start = buffer;
return 0;
}
*start = buffer + offset;
len -= offset;
if (len > length)
len = length;
return len;
}
int aha1740_makecode(unchar *sense, unchar *status)
{
struct statusword
{
ushort	don:1,
du:1,
:1,	qf:1,
sc:1,
dor:1,
ch:1,
intr:1,
asa:1,
sns:1,
:1,	ini:1,
me:1,
:1,	eca:1,
:1;
} status_word;
int retval = DID_OK;
status_word = * (struct statusword *) status;
#ifdef DEBUG
printk("makecode from %x,%x,%x,%x %x,%x,%x,%x",
status[0], status[1], status[2], status[3],
sense[0], sense[1], sense[2], sense[3]);
#endif
if (!status_word.don)
{
if ( (status[1]&0x18) || status_word.sc )
{
switch ( status[2] )
{
case 0x12:
if ( status_word.dor )
retval=DID_ERROR;
case 0x00:
break;
case 0x11:
case 0x21:
retval=DID_TIME_OUT;
break;
case 0x0a:
retval=DID_BAD_TARGET;
break;
case 0x04:
case 0x05:
retval=DID_ABORT;
break;
default:
retval=DID_ERROR;
}
}
else
{
if ( status_word.qf )
{
retval = DID_TIME_OUT;
printk("aha1740.c: WARNING: AHA1740 queue overflow!\n");
}
else if ( status[0]&0x60 )
{
retval = DID_ERROR;
}
}
}
return status[3] | retval << 16;
}
int aha1740_test_port(unsigned int base)
{
char name[4], tmp;
name[0]= 'A' -1 + ((tmp = inb(HID0(base))) >> 2);
name[1]= 'A' -1 + ((tmp & 3) << 3);
name[1]+= ((tmp = inb(HID1(base))) >> 5)&0x7;
name[2]= 'A' -1 + (tmp & 0x1f);
name[3]=0;
tmp = inb(HID2(base));
if ( strcmp ( name, HID_MFG ) || inb(HID2(base)) != HID_PRD )
return 0;
if ( inb(EBCNTRL(base)) != EBCNTRL_VALUE )
{
printk("aha174x: Board detected, but EBCNTRL = %x, so disabled it.\n",
inb(EBCNTRL(base)));
return 0;
}
if ( inb(PORTADR(base)) & PORTADDR_ENH )
return 1;
printk("aha174x: Board detected, but not in enhanced mode, so disabled it.\n");
return 0;
}
void aha1740_intr_handle(int irq, void *dev_id, struct pt_regs * regs)
{
void (*my_done)(Scsi_Cmnd *);
int errstatus, adapstat;
int number_serviced;
struct ecb *ecbptr;
Scsi_Cmnd *SCtmp;
unsigned int base;
if (!aha_host[irq - 9])
panic("aha1740.c: Irq from unknown host!\n");
base = aha_host[irq - 9]->io_port;
number_serviced = 0;
while(inb(G2STAT(base)) & G2STAT_INTPEND)
{
DEB(printk("aha1740_intr top of loop.\n"));
adapstat = inb(G2INTST(base));
ecbptr = (struct ecb *) bus_to_virt(inl(MBOXIN0(base)));
outb(G2CNTRL_IRST,G2CNTRL(base));
switch ( adapstat & G2INTST_MASK )
{
case	G2INTST_CCBRETRY:
case	G2INTST_CCBERROR:
case	G2INTST_CCBGOOD:
outb(G2CNTRL_HRDY,G2CNTRL(base));
if (!ecbptr)
{
printk("Aha1740 null ecbptr in interrupt (%x,%x,%x,%d)\n",
inb(G2STAT(base)),adapstat,
inb(G2INTST(base)), number_serviced++);
continue;
}
SCtmp = ecbptr->SCpnt;
if (!SCtmp)
{
printk("Aha1740 null SCtmp in interrupt (%x,%x,%x,%d)\n",
inb(G2STAT(base)),adapstat,
inb(G2INTST(base)), number_serviced++);
continue;
}
if (SCtmp->host_scribble)
scsi_free(SCtmp->host_scribble, 512);
if ( (adapstat & G2INTST_MASK) == G2INTST_CCBERROR )
{
memcpy(SCtmp->sense_buffer, ecbptr->sense,
sizeof(SCtmp->sense_buffer));
errstatus = aha1740_makecode(ecbptr->sense,ecbptr->status);
}
else
errstatus = 0;
DEB(if (errstatus) printk("aha1740_intr_handle: returning %6x\n",
errstatus));
SCtmp->result = errstatus;
my_done = ecbptr->done;
memset(ecbptr,0,sizeof(struct ecb));
if ( my_done )
my_done(SCtmp);
break;
case	G2INTST_HARDFAIL:
printk(KERN_ALERT "aha1740 hardware failure!\n");
panic("aha1740.c");
case	G2INTST_ASNEVENT:
printk("aha1740 asynchronous event: %02x %02x %02x %02x %02x\n",
adapstat, inb(MBOXIN0(base)), inb(MBOXIN1(base)),
inb(MBOXIN2(base)), inb(MBOXIN3(base)));
outb(G2CNTRL_HRDY,G2CNTRL(base));
break;
case	G2INTST_CMDGOOD:
break;
case	G2INTST_CMDERROR:
break;
}
number_serviced++;
}
}
int aha1740_queuecommand(Scsi_Cmnd * SCpnt, void (*done)(Scsi_Cmnd *))
{
unchar direction;
unchar *cmd = (unchar *) SCpnt->cmnd;
unchar target = SCpnt->target;
struct aha1740_hostdata *host = HOSTDATA(SCpnt->host);
unsigned long flags;
void *buff = SCpnt->request_buffer;
int bufflen = SCpnt->request_bufflen;
int ecbno;
DEB(int i);
if(*cmd == REQUEST_SENSE)
{
if (bufflen != sizeof(SCpnt->sense_buffer))
{
printk("Wrong buffer length supplied for request sense (%d)\n",
bufflen);
}
SCpnt->result = 0;
done(SCpnt);
return 0;
}
#ifdef DEBUG
if (*cmd == READ_10 || *cmd == WRITE_10)
i = xscsi2int(cmd+2);
else if (*cmd == READ_6 || *cmd == WRITE_6)
i = scsi2int(cmd+2);
else
i = -1;
printk("aha1740_queuecommand: dev %d cmd %02x pos %d len %d ",
target, *cmd, i, bufflen);
printk("scsi cmd:");
for (i = 0; i < SCpnt->cmd_len; i++) printk("%02x ", cmd[i]);
printk("\n");
#endif
save_flags(flags);
cli();
ecbno = host->last_ecb_used + 1;
if (ecbno >= AHA1740_ECBS)
ecbno = 0;
do {
if (!host->ecb[ecbno].cmdw)
break;
ecbno++;
if (ecbno >= AHA1740_ECBS)
ecbno = 0;
} while (ecbno != host->last_ecb_used);
if (host->ecb[ecbno].cmdw)
panic("Unable to find empty ecb for aha1740.\n");
host->ecb[ecbno].cmdw = AHA1740CMD_INIT;
host->last_ecb_used = ecbno;
restore_flags(flags);
#ifdef DEBUG
printk("Sending command (%d %x)...", ecbno, done);
#endif
host->ecb[ecbno].cdblen = SCpnt->cmd_len;
direction = 0;
if (*cmd == READ_10 || *cmd == READ_6)
direction = 1;
else if (*cmd == WRITE_10 || *cmd == WRITE_6)
direction = 0;
memcpy(host->ecb[ecbno].cdb, cmd, SCpnt->cmd_len);
if (SCpnt->use_sg)
{
struct scatterlist * sgpnt;
struct aha1740_chain * cptr;
int i;
DEB(unsigned char * ptr);
host->ecb[ecbno].sg = 1;
SCpnt->host_scribble = (unsigned char *) scsi_malloc(512);
sgpnt = (struct scatterlist *) SCpnt->request_buffer;
cptr = (struct aha1740_chain *) SCpnt->host_scribble;
if (cptr == NULL) panic("aha1740.c: unable to allocate DMA memory\n");
for(i=0; i<SCpnt->use_sg; i++)
{
cptr[i].datalen = sgpnt[i].length;
cptr[i].dataptr = virt_to_bus(sgpnt[i].address);
}
host->ecb[ecbno].datalen = SCpnt->use_sg * sizeof(struct aha1740_chain);
host->ecb[ecbno].dataptr = virt_to_bus(cptr);
#ifdef DEBUG
printk("cptr %x: ",cptr);
ptr = (unsigned char *) cptr;
for(i=0;i<24;i++) printk("%02x ", ptr[i]);
#endif
}
else
{
SCpnt->host_scribble = NULL;
host->ecb[ecbno].datalen = bufflen;
host->ecb[ecbno].dataptr = virt_to_bus(buff);
}
host->ecb[ecbno].lun = SCpnt->lun;
host->ecb[ecbno].ses = 1;
host->ecb[ecbno].dir = direction;
host->ecb[ecbno].ars = 1;
host->ecb[ecbno].senselen = 12;
host->ecb[ecbno].senseptr = virt_to_bus(host->ecb[ecbno].sense);
host->ecb[ecbno].statusptr = virt_to_bus(host->ecb[ecbno].status);
host->ecb[ecbno].done = done;
host->ecb[ecbno].SCpnt = SCpnt;
#ifdef DEBUG
{
int i;
printk("aha1740_command: sending.. ");
for (i = 0; i < sizeof(host->ecb[ecbno]) - 10; i++)
printk("%02x ", ((unchar *)&host->ecb[ecbno])[i]);
}
printk("\n");
#endif
if (done)
{
#define LOOPCNT_WARN 10
#define LOOPCNT_MAX 1000000
int loopcnt;
unsigned int base = SCpnt->host->io_port;
DEB(printk("aha1740[%d] critical section\n",ecbno));
save_flags(flags);
cli();
for (loopcnt = 0; ; loopcnt++) {
if (inb(G2STAT(base)) & G2STAT_MBXOUT) break;
if (loopcnt == LOOPCNT_WARN) {
printk("aha1740[%d]_mbxout wait!\n",ecbno);
cli();
}
if (loopcnt == LOOPCNT_MAX)
panic("aha1740.c: mbxout busy!\n");
}
outl(virt_to_bus(host->ecb + ecbno), MBOXOUT0(base));
for (loopcnt = 0; ; loopcnt++) {
if (! (inb(G2STAT(base)) & G2STAT_BUSY)) break;
if (loopcnt == LOOPCNT_WARN) {
printk("aha1740[%d]_attn wait!\n",ecbno);
cli();
}
if (loopcnt == LOOPCNT_MAX)
panic("aha1740.c: attn wait failed!\n");
}
outb(ATTN_START | (target & 7), ATTN(base));
restore_flags(flags);
DEB(printk("aha1740[%d] request queued.\n",ecbno));
}
else
printk(KERN_ALERT "aha1740_queuecommand: done can't be NULL\n");
return 0;
}
static void internal_done(Scsi_Cmnd * SCpnt)
{
SCpnt->SCp.Status++;
}
int aha1740_command(Scsi_Cmnd * SCpnt)
{
aha1740_queuecommand(SCpnt, internal_done);
SCpnt->SCp.Status = 0;
while (!SCpnt->SCp.Status)
barrier();
return SCpnt->result;
}
void aha1740_getconfig(unsigned int base, unsigned int *irq_level,
unsigned int *translation)
{
static int intab[] = { 9, 10, 11, 12, 0, 14, 15, 0 };
*irq_level = intab[inb(INTDEF(base)) & 0x7];
*translation = inb(RESV1(base)) & 0x1;
outb(inb(INTDEF(base)) | 0x10, INTDEF(base));
}
int aha1740_detect(Scsi_Host_Template * tpnt)
{
int count = 0, slot;
DEB(printk("aha1740_detect: \n"));
for ( slot=MINEISA; slot <= MAXEISA; slot++ )
{
int slotbase;
unsigned int irq_level, translation;
struct Scsi_Host *shpnt;
struct aha1740_hostdata *host;
slotbase = SLOTBASE(slot);
if (check_region(slotbase, SLOTSIZE))
continue;
if (!aha1740_test_port(slotbase))
continue;
aha1740_getconfig(slotbase,&irq_level,&translation);
if ((inb(G2STAT(slotbase)) &
(G2STAT_MBXOUT|G2STAT_BUSY)) != G2STAT_MBXOUT)
{
outb(G2CNTRL_HRST, G2CNTRL(slotbase));
outb(0, G2CNTRL(slotbase));
}
printk("Configuring aha174x at IO:%x, IRQ %d\n", slotbase, irq_level);
printk("aha174x: Extended translation %sabled.\n",
translation ? "en" : "dis");
DEB(printk("aha1740_detect: enable interrupt channel %d\n",irq_level));
if (request_irq(irq_level,aha1740_intr_handle,0,"aha1740",NULL)) {
printk("Unable to allocate IRQ for adaptec controller.\n");
continue;
}
shpnt = scsi_register(tpnt, sizeof(struct aha1740_hostdata));
request_region(slotbase, SLOTSIZE, "aha1740");
shpnt->base = 0;
shpnt->io_port = slotbase;
shpnt->n_io_port = SLOTSIZE;
shpnt->irq = irq_level;
shpnt->dma_channel = 0xff;
host = HOSTDATA(shpnt);
host->slot = slot;
host->translation = translation;
aha_host[irq_level - 9] = shpnt;
count++;
}
return count;
}
int aha1740_abort(Scsi_Cmnd * SCpnt)
{
DEB(printk("aha1740_abort called\n"));
return SCSI_ABORT_SNOOZE;
}
int aha1740_reset(Scsi_Cmnd * SCpnt, unsigned int ignored)
{
DEB(printk("aha1740_reset called\n"));
return SCSI_RESET_PUNT;
}
int aha1740_biosparam(Disk * disk, kdev_t dev, int* ip)
{
int size = disk->capacity;
int extended = HOSTDATA(disk->device->host)->translation;
DEB(printk("aha1740_biosparam\n"));
if (extended && (ip[2] > 1024))
{
ip[0] = 255;
ip[1] = 63;
ip[2] = size / (255 * 63);
}
else
{
ip[0] = 64;
ip[1] = 32;
ip[2] = size >> 11;
}
return 0;
}
#ifdef MODULE
Scsi_Host_Template driver_template = AHA1740;
#include "scsi_module.c"
#endif