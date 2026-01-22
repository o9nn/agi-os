#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/head.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/ioport.h>
#include <linux/delay.h>
#include <linux/sched.h>
#include <linux/proc_fs.h>
#include <asm/dma.h>
#include <asm/system.h>
#include <asm/io.h>
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "aha1542.h"
#include<linux/stat.h>
struct proc_dir_entry proc_scsi_aha1542 = {
PROC_SCSI_AHA1542, 7, "aha1542",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#ifdef DEBUG
#define DEB(x) x
#else
#define DEB(x)
#endif
#define MAXBOARDS 2
static unsigned int bases[MAXBOARDS]={0x330, 0x334};
static int setup_called[MAXBOARDS] = {0,0};
static int setup_buson[MAXBOARDS] = {0,0};
static int setup_busoff[MAXBOARDS] = {0,0};
static int setup_dmaspeed[MAXBOARDS] = {-1,-1};
static char *setup_str[MAXBOARDS] = {(char *)NULL,(char *)NULL};
#define BIOS_TRANSLATION_1632 0
#define BIOS_TRANSLATION_6432 1
#define BIOS_TRANSLATION_25563 2
struct aha1542_hostdata{
int bios_translation;
int aha1542_last_mbi_used;
int aha1542_last_mbo_used;
Scsi_Cmnd * SCint[AHA1542_MAILBOXES];
struct mailbox mb[2*AHA1542_MAILBOXES];
struct ccb ccb[AHA1542_MAILBOXES];
};
#define HOSTDATA(host) ((struct aha1542_hostdata *) &host->hostdata)
static struct Scsi_Host * aha_host[7] = {NULL,};
#define WAITnexttimeout 3000000
static void setup_mailboxes(int base_io, struct Scsi_Host * shpnt);
static int aha1542_restart(struct Scsi_Host * shost);
#define aha1542_intr_reset(base) outb(IRST, CONTROL(base))
#define WAIT(port, mask, allof, noneof) \
{ register int WAITbits; \
register int WAITtimeout = WAITnexttimeout; \
while (1) { \
WAITbits = inb(port) & (mask); \
if ((WAITbits & (allof)) == (allof) && ((WAITbits & (noneof)) == 0)) \
break; \
if (--WAITtimeout == 0) goto fail; \
} \
}
#define WAITd(port, mask, allof, noneof, timeout) \
{ register int WAITbits; \
register int WAITtimeout = timeout; \
while (1) { \
WAITbits = inb(port) & (mask); \
if ((WAITbits & (allof)) == (allof) && ((WAITbits & (noneof)) == 0)) \
break; \
udelay(1000); \
if (--WAITtimeout == 0) goto fail; \
} \
}
static void aha1542_stat(void)
{
}
static int aha1542_out(unsigned int base, unchar *cmdp, int len)
{
unsigned long flags = 0;
save_flags(flags);
if(len == 1) {
while(1==1){
WAIT(STATUS(base), CDF, 0, CDF);
cli();
if(inb(STATUS(base)) & CDF) {restore_flags(flags); continue;}
outb(*cmdp, DATA(base));
restore_flags(flags);
return 0;
}
} else {
cli();
while (len--)
{
WAIT(STATUS(base), CDF, 0, CDF);
outb(*cmdp++, DATA(base));
}
restore_flags(flags);
}
return 0;
fail:
restore_flags(flags);
printk("aha1542_out failed(%d): ", len+1); aha1542_stat();
return 1;
}
static int aha1542_in(unsigned int base, unchar *cmdp, int len)
{
unsigned long flags;
save_flags(flags);
cli();
while (len--)
{
WAIT(STATUS(base), DF, DF, 0);
*cmdp++ = inb(DATA(base));
}
restore_flags(flags);
return 0;
fail:
restore_flags(flags);
printk("aha1542_in failed(%d): ", len+1); aha1542_stat();
return 1;
}
static int aha1542_in1(unsigned int base, unchar *cmdp, int len)
{
unsigned long flags;
save_flags(flags);
cli();
while (len--)
{
WAITd(STATUS(base), DF, DF, 0, 100);
*cmdp++ = inb(DATA(base));
}
restore_flags(flags);
return 0;
fail:
restore_flags(flags);
return 1;
}
static int makecode(unsigned hosterr, unsigned scsierr)
{
switch (hosterr) {
case 0x0:
case 0xa:
case 0xb:
hosterr = 0;
break;
case 0x11:
hosterr = DID_TIME_OUT;
break;
case 0x12:
case 0x13:
case 0x15:
case 0x16:
case 0x17:
case 0x18:
case 0x19:
case 0x1a:
DEB(printk("Aha1542: %x %x\n", hosterr, scsierr));
hosterr = DID_ERROR;
break;
case 0x14:
hosterr = DID_RESET;
break;
default:
printk("makecode: unknown hoststatus %x\n", hosterr);
break;
}
return scsierr|(hosterr << 16);
}
static int aha1542_test_port(int bse, struct Scsi_Host * shpnt)
{
int i;
unchar inquiry_cmd[] = {CMD_INQUIRY };
unchar inquiry_result[4];
unchar *cmdp;
int len;
volatile int debug = 0;
if(inb(STATUS(bse)) == 0xff) return 0;
aha1542_intr_reset(bse);
outb(SRST|IRST, CONTROL(bse));
i = jiffies + 2;
while (i>jiffies);
debug = 1;
WAIT(STATUS(bse), STATMASK, INIT|IDLE, STST|DIAGF|INVDCMD|DF|CDF);
debug = 2;
if (inb(INTRFLAGS(bse))&INTRMASK) goto fail;
aha1542_out(bse, inquiry_cmd, 1);
debug = 3;
len = 4;
cmdp = &inquiry_result[0];
while (len--)
{
WAIT(STATUS(bse), DF, DF, 0);
*cmdp++ = inb(DATA(bse));
}
debug = 8;
if (inb(STATUS(bse)) & DF) goto fail;
debug = 9;
WAIT(INTRFLAGS(bse), HACC, HACC, 0);
debug = 10;
outb(IRST, CONTROL(bse));
debug = 11;
return debug;
fail:
return 0;
}
static void aha1542_intr_handle(int irq, void *dev_id, struct pt_regs *regs)
{
void (*my_done)(Scsi_Cmnd *) = NULL;
int errstatus, mbi, mbo, mbistatus;
int number_serviced;
unsigned long flags;
struct Scsi_Host * shost;
Scsi_Cmnd * SCtmp;
int flag;
int needs_restart;
struct mailbox * mb;
struct ccb *ccb;
shost = aha_host[irq - 9];
if(!shost) panic("Splunge!");
mb = HOSTDATA(shost)->mb;
ccb = HOSTDATA(shost)->ccb;
#ifdef DEBUG
{
flag = inb(INTRFLAGS(shost->io_port));
printk("aha1542_intr_handle: ");
if (!(flag&ANYINTR)) printk("no interrupt?");
if (flag&MBIF) printk("MBIF ");
if (flag&MBOA) printk("MBOF ");
if (flag&HACC) printk("HACC ");
if (flag&SCRD) printk("SCRD ");
printk("status %02x\n", inb(STATUS(shost->io_port)));
};
#endif
number_serviced = 0;
needs_restart = 0;
while(1==1){
flag = inb(INTRFLAGS(shost->io_port));
if (flag & ~MBIF) {
if (flag&MBOA) printk("MBOF ");
if (flag&HACC) printk("HACC ");
if (flag&SCRD) {
needs_restart = 1;
printk("SCRD ");
}
}
aha1542_intr_reset(shost->io_port);
save_flags(flags);
cli();
mbi = HOSTDATA(shost)->aha1542_last_mbi_used + 1;
if (mbi >= 2*AHA1542_MAILBOXES) mbi = AHA1542_MAILBOXES;
do{
if(mb[mbi].status != 0) break;
mbi++;
if (mbi >= 2*AHA1542_MAILBOXES) mbi = AHA1542_MAILBOXES;
} while (mbi != HOSTDATA(shost)->aha1542_last_mbi_used);
if(mb[mbi].status == 0){
restore_flags(flags);
if (!number_serviced && !needs_restart)
printk("aha1542.c: interrupt received, but no mail.\n");
if(needs_restart) aha1542_restart(shost);
return;
};
mbo = (scsi2int(mb[mbi].ccbptr) - ((unsigned int) &ccb[0])) / sizeof(struct ccb);
mbistatus = mb[mbi].status;
mb[mbi].status = 0;
HOSTDATA(shost)->aha1542_last_mbi_used = mbi;
restore_flags(flags);
#ifdef DEBUG
{
if (ccb[mbo].tarstat|ccb[mbo].hastat)
printk("aha1542_command: returning %x (status %d)\n",
ccb[mbo].tarstat + ((int) ccb[mbo].hastat << 16), mb[mbi].status);
};
#endif
if(mbistatus == 3) continue;
#ifdef DEBUG
printk("...done %d %d\n",mbo, mbi);
#endif
SCtmp = HOSTDATA(shost)->SCint[mbo];
if (!SCtmp || !SCtmp->scsi_done) {
printk("aha1542_intr_handle: Unexpected interrupt\n");
printk("tarstat=%x, hastat=%x idlun=%x ccb#=%d \n", ccb[mbo].tarstat,
ccb[mbo].hastat, ccb[mbo].idlun, mbo);
return;
}
my_done = SCtmp->scsi_done;
if (SCtmp->host_scribble) scsi_free(SCtmp->host_scribble, 512);
if (ccb[mbo].tarstat == 2)
memcpy(SCtmp->sense_buffer, &ccb[mbo].cdb[ccb[mbo].cdblen],
sizeof(SCtmp->sense_buffer));
if (mbistatus != 1)
errstatus = makecode(ccb[mbo].hastat, ccb[mbo].tarstat);
else
errstatus = 0;
#ifdef DEBUG
if(errstatus) printk("(aha1542 error:%x %x %x) ",errstatus,
ccb[mbo].hastat, ccb[mbo].tarstat);
#endif
if (ccb[mbo].tarstat == 2) {
#ifdef DEBUG
int i;
#endif
DEB(printk("aha1542_intr_handle: sense:"));
#ifdef DEBUG
for (i = 0; i < 12; i++)
printk("%02x ", ccb[mbo].cdb[ccb[mbo].cdblen+i]);
printk("\n");
#endif
}
DEB(if (errstatus) printk("aha1542_intr_handle: returning %6x\n", errstatus));
SCtmp->result = errstatus;
HOSTDATA(shost)->SCint[mbo] = NULL;
my_done(SCtmp);
number_serviced++;
};
}
int aha1542_queuecommand(Scsi_Cmnd * SCpnt, void (*done)(Scsi_Cmnd *))
{
unchar ahacmd = CMD_START_SCSI;
unchar direction;
unchar *cmd = (unchar *) SCpnt->cmnd;
unchar target = SCpnt->target;
unchar lun = SCpnt->lun;
unsigned long flags;
void *buff = SCpnt->request_buffer;
int bufflen = SCpnt->request_bufflen;
int mbo;
struct mailbox * mb;
struct ccb *ccb;
DEB(int i);
mb = HOSTDATA(SCpnt->host)->mb;
ccb = HOSTDATA(SCpnt->host)->ccb;
DEB(if (target > 1) {
SCpnt->result = DID_TIME_OUT << 16;
done(SCpnt); return 0;});
if(*cmd == REQUEST_SENSE){
#ifndef DEBUG
if (bufflen != sizeof(SCpnt->sense_buffer)) {
printk("Wrong buffer length supplied for request sense (%d)\n",bufflen);
};
#endif
SCpnt->result = 0;
done(SCpnt);
return 0;
};
#ifdef DEBUG
if (*cmd == READ_10 || *cmd == WRITE_10)
i = xscsi2int(cmd+2);
else if (*cmd == READ_6 || *cmd == WRITE_6)
i = scsi2int(cmd+2);
else
i = -1;
if (done)
printk("aha1542_queuecommand: dev %d cmd %02x pos %d len %d ", target, *cmd, i, bufflen);
else
printk("aha1542_command: dev %d cmd %02x pos %d len %d ", target, *cmd, i, bufflen);
aha1542_stat();
printk("aha1542_queuecommand: dumping scsi cmd:");
for (i = 0; i < SCpnt->cmd_len; i++) printk("%02x ", cmd[i]);
printk("\n");
if (*cmd == WRITE_10 || *cmd == WRITE_6)
return 0;
#endif
save_flags(flags);
cli();
mbo = HOSTDATA(SCpnt->host)->aha1542_last_mbo_used + 1;
if (mbo >= AHA1542_MAILBOXES) mbo = 0;
do{
if(mb[mbo].status == 0 && HOSTDATA(SCpnt->host)->SCint[mbo] == NULL)
break;
mbo++;
if (mbo >= AHA1542_MAILBOXES) mbo = 0;
} while (mbo != HOSTDATA(SCpnt->host)->aha1542_last_mbo_used);
if(mb[mbo].status || HOSTDATA(SCpnt->host)->SCint[mbo])
panic("Unable to find empty mailbox for aha1542.\n");
HOSTDATA(SCpnt->host)->SCint[mbo] = SCpnt;
HOSTDATA(SCpnt->host)->aha1542_last_mbo_used = mbo;
restore_flags(flags);
#ifdef DEBUG
printk("Sending command (%d %x)...",mbo, done);
#endif
any2scsi(mb[mbo].ccbptr, &ccb[mbo]);
memset(&ccb[mbo], 0, sizeof(struct ccb));
ccb[mbo].cdblen = SCpnt->cmd_len;
direction = 0;
if (*cmd == READ_10 || *cmd == READ_6)
direction = 8;
else if (*cmd == WRITE_10 || *cmd == WRITE_6)
direction = 16;
memcpy(ccb[mbo].cdb, cmd, ccb[mbo].cdblen);
if (SCpnt->use_sg) {
struct scatterlist * sgpnt;
struct chain * cptr;
#ifdef DEBUG
unsigned char * ptr;
#endif
int i;
ccb[mbo].op = 2;
SCpnt->host_scribble = (unsigned char *) scsi_malloc(512);
sgpnt = (struct scatterlist *) SCpnt->request_buffer;
cptr = (struct chain *) SCpnt->host_scribble;
if (cptr == NULL) panic("aha1542.c: unable to allocate DMA memory\n");
for(i=0; i<SCpnt->use_sg; i++) {
if(sgpnt[i].length == 0 || SCpnt->use_sg > 16 ||
(((int)sgpnt[i].address) & 1) || (sgpnt[i].length & 1)){
unsigned char * ptr;
printk("Bad segment list supplied to aha1542.c (%d, %d)\n",SCpnt->use_sg,i);
for(i=0;i<SCpnt->use_sg;i++){
printk("%d: %x %x %d\n",i,(unsigned int) sgpnt[i].address, (unsigned int) sgpnt[i].alt_address,
sgpnt[i].length);
};
printk("cptr %x: ",(unsigned int) cptr);
ptr = (unsigned char *) &cptr[i];
for(i=0;i<18;i++) printk("%02x ", ptr[i]);
panic("Foooooooood fight!");
};
any2scsi(cptr[i].dataptr, sgpnt[i].address);
if(((unsigned int) sgpnt[i].address) & 0xff000000) goto baddma;
any2scsi(cptr[i].datalen, sgpnt[i].length);
};
any2scsi(ccb[mbo].datalen, SCpnt->use_sg * sizeof(struct chain));
any2scsi(ccb[mbo].dataptr, cptr);
#ifdef DEBUG
printk("cptr %x: ",cptr);
ptr = (unsigned char *) cptr;
for(i=0;i<18;i++) printk("%02x ", ptr[i]);
#endif
} else {
ccb[mbo].op = 0;
SCpnt->host_scribble = NULL;
any2scsi(ccb[mbo].datalen, bufflen);
if(((unsigned int) buff & 0xff000000)) goto baddma;
any2scsi(ccb[mbo].dataptr, buff);
};
ccb[mbo].idlun = (target&7)<<5 | direction | (lun & 7);
ccb[mbo].rsalen = 16;
ccb[mbo].linkptr[0] = ccb[mbo].linkptr[1] = ccb[mbo].linkptr[2] = 0;
ccb[mbo].commlinkid = 0;
#ifdef DEBUG
{ int i;
printk("aha1542_command: sending.. ");
for (i = 0; i < sizeof(ccb[mbo])-10; i++)
printk("%02x ", ((unchar *)&ccb[mbo])[i]);
};
#endif
if (done) {
DEB(printk("aha1542_queuecommand: now waiting for interrupt "); aha1542_stat());
SCpnt->scsi_done = done;
mb[mbo].status = 1;
aha1542_out(SCpnt->host->io_port, &ahacmd, 1);
DEB(aha1542_stat());
}
else
printk("aha1542_queuecommand: done can't be NULL\n");
return 0;
baddma:
panic("Buffer at address  > 16Mb used for 1542B");
}
static void internal_done(Scsi_Cmnd * SCpnt)
{
SCpnt->SCp.Status++;
}
int aha1542_command(Scsi_Cmnd * SCpnt)
{
DEB(printk("aha1542_command: ..calling aha1542_queuecommand\n"));
aha1542_queuecommand(SCpnt, internal_done);
SCpnt->SCp.Status = 0;
while (!SCpnt->SCp.Status)
barrier();
return SCpnt->result;
}
static void setup_mailboxes(int bse, struct Scsi_Host * shpnt)
{
int i;
struct mailbox * mb;
struct ccb *ccb;
unchar cmd[5] = {CMD_MBINIT, AHA1542_MAILBOXES, 0, 0, 0};
mb = HOSTDATA(shpnt)->mb;
ccb = HOSTDATA(shpnt)->ccb;
for(i=0; i<AHA1542_MAILBOXES; i++){
mb[i].status = mb[AHA1542_MAILBOXES+i].status = 0;
any2scsi(mb[i].ccbptr, &ccb[i]);
};
aha1542_intr_reset(bse);
any2scsi((cmd+2), mb);
aha1542_out(bse, cmd, 5);
WAIT(INTRFLAGS(bse), INTRMASK, HACC, 0);
while (0) {
fail:
printk("aha1542_detect: failed setting up mailboxes\n");
}
aha1542_intr_reset(bse);
}
static int aha1542_getconfig(int base_io, unsigned char * irq_level, unsigned char * dma_chan, unsigned char * scsi_id)
{
unchar inquiry_cmd[] = {CMD_RETCONF };
unchar inquiry_result[3];
int i;
i = inb(STATUS(base_io));
if (i & DF) {
i = inb(DATA(base_io));
};
aha1542_out(base_io, inquiry_cmd, 1);
aha1542_in(base_io, inquiry_result, 3);
WAIT(INTRFLAGS(base_io), INTRMASK, HACC, 0);
while (0) {
fail:
printk("aha1542_detect: query board settings\n");
}
aha1542_intr_reset(base_io);
switch(inquiry_result[0]){
case 0x80:
*dma_chan = 7;
break;
case 0x40:
*dma_chan = 6;
break;
case 0x20:
*dma_chan = 5;
break;
case 0x01:
*dma_chan = 0;
break;
case 0:
*dma_chan = 0xFF;
break;
default:
printk("Unable to determine Adaptec DMA priority.  Disabling board\n");
return -1;
};
switch(inquiry_result[1]){
case 0x40:
*irq_level = 15;
break;
case 0x20:
*irq_level = 14;
break;
case 0x8:
*irq_level = 12;
break;
case 0x4:
*irq_level = 11;
break;
case 0x2:
*irq_level = 10;
break;
case 0x1:
*irq_level = 9;
break;
default:
printk("Unable to determine Adaptec IRQ level.  Disabling board\n");
return -1;
};
*scsi_id=inquiry_result[2] & 7;
return 0;
}
static int aha1542_mbenable(int base)
{
static unchar mbenable_cmd[3];
static unchar mbenable_result[2];
int retval;
retval = BIOS_TRANSLATION_6432;
mbenable_cmd[0]=CMD_EXTBIOS;
aha1542_out(base,mbenable_cmd,1);
if(aha1542_in1(base,mbenable_result,2))
return retval;
WAITd(INTRFLAGS(base),INTRMASK,HACC,0,100);
aha1542_intr_reset(base);
if ((mbenable_result[0] & 0x08) || mbenable_result[1]) {
mbenable_cmd[0]=CMD_MBENABLE;
mbenable_cmd[1]=0;
mbenable_cmd[2]=mbenable_result[1];
if((mbenable_result[0] & 0x08) && (mbenable_result[1] & 0x03)) retval = BIOS_TRANSLATION_25563;
aha1542_out(base,mbenable_cmd,3);
WAIT(INTRFLAGS(base),INTRMASK,HACC,0);
};
while(0) {
fail:
printk("aha1542_mbenable: Mailbox init failed\n");
}
aha1542_intr_reset(base);
return retval;
}
static int aha1542_query(int base_io, int * transl)
{
unchar inquiry_cmd[] = {CMD_INQUIRY };
unchar inquiry_result[4];
int i;
i = inb(STATUS(base_io));
if (i & DF) {
i = inb(DATA(base_io));
};
aha1542_out(base_io, inquiry_cmd, 1);
aha1542_in(base_io, inquiry_result, 4);
WAIT(INTRFLAGS(base_io), INTRMASK, HACC, 0);
while (0) {
fail:
printk("aha1542_detect: query card type\n");
}
aha1542_intr_reset(base_io);
*transl = BIOS_TRANSLATION_6432;
if (inquiry_result[0] == 0x43) {
printk("aha1542.c: Emulation mode not supported for AHA 174N hardware.\n");
return 1;
};
*transl = aha1542_mbenable(base_io);
return 0;
}
void aha1542_setup( char *str, int *ints)
{
const char *ahausage = "aha1542: usage: aha1542=<PORTBASE>[,<BUSON>,<BUSOFF>[,<DMASPEED>]]\n";
static int setup_idx = 0;
int setup_portbase;
if(setup_idx >= MAXBOARDS)
{
printk("aha1542: aha1542_setup called too many times! Bad LILO params ?\n");
printk("   Entryline 1: %s\n",setup_str[0]);
printk("   Entryline 2: %s\n",setup_str[1]);
printk("   This line:   %s\n",str);
return;
}
if (ints[0] < 1 || ints[0] > 4)
{
printk("aha1542: %s\n", str );
printk("%s", ahausage);
printk("aha1542: Wrong parameters may cause system malfunction.. We try anyway..\n");
}
setup_called[setup_idx]=ints[0];
setup_str[setup_idx]=str;
setup_portbase = ints[0] >= 1 ? ints[1] : 0;
setup_buson [setup_idx] = ints[0] >= 2 ? ints[2] : 7;
setup_busoff [setup_idx] = ints[0] >= 3 ? ints[3] : 5;
if (ints[0] >= 4) {
int atbt = -1;
switch (ints[4]) {
case 5:
atbt = 0x00;
break;
case 6:
atbt = 0x04;
break;
case 7:
atbt = 0x01;
break;
case 8:
atbt = 0x02;
break;
case 10:
atbt = 0x03;
break;
default:
printk("aha1542: %s\n", str );
printk("%s", ahausage);
printk("aha1542: Valid values for DMASPEED are 5-8, 10 MB/s.  Using jumper defaults.\n");
break;
}
setup_dmaspeed[setup_idx] = atbt;
}
if (setup_portbase != 0)
bases[setup_idx] = setup_portbase;
++setup_idx;
}
int aha1542_detect(Scsi_Host_Template * tpnt)
{
unsigned char dma_chan;
unsigned char irq_level;
unsigned char scsi_id;
unsigned long flags;
unsigned int base_io;
int trans;
struct Scsi_Host * shpnt = NULL;
int count = 0;
int indx;
DEB(printk("aha1542_detect: \n"));
tpnt->proc_dir = &proc_scsi_aha1542;
for(indx = 0; indx < sizeof(bases)/sizeof(bases[0]); indx++)
if(bases[indx] != 0 && !check_region(bases[indx], 4)) {
shpnt = scsi_register(tpnt,
sizeof(struct aha1542_hostdata));
if ((unsigned int) shpnt > 0xffffff) {
printk("Invalid address for shpnt with 1542.\n");
goto unregister;
}
if(!aha1542_test_port(bases[indx], shpnt)) goto unregister;
base_io = bases[indx];
{
unchar oncmd[] = {CMD_BUSON_TIME, 7};
unchar offcmd[] = {CMD_BUSOFF_TIME, 5};
if(setup_called[indx])
{
oncmd[1] = setup_buson[indx];
offcmd[1] = setup_busoff[indx];
}
aha1542_intr_reset(base_io);
aha1542_out(base_io, oncmd, 2);
WAIT(INTRFLAGS(base_io), INTRMASK, HACC, 0);
aha1542_intr_reset(base_io);
aha1542_out(base_io, offcmd, 2);
WAIT(INTRFLAGS(base_io), INTRMASK, HACC, 0);
if (setup_dmaspeed[indx] >= 0)
{
unchar dmacmd[] = {CMD_DMASPEED, 0};
dmacmd[1] = setup_dmaspeed[indx];
aha1542_intr_reset(base_io);
aha1542_out(base_io, dmacmd, 2);
WAIT(INTRFLAGS(base_io), INTRMASK, HACC, 0);
}
while (0) {
fail:
printk("aha1542_detect: setting bus on/off-time failed\n");
}
aha1542_intr_reset(base_io);
}
if(aha1542_query(base_io, &trans)) goto unregister;
if (aha1542_getconfig(base_io, &irq_level, &dma_chan, &scsi_id) == -1) goto unregister;
printk("Configuring Adaptec (SCSI-ID %d) at IO:%x, IRQ %d", scsi_id, base_io, irq_level);
if (dma_chan != 0xFF)
printk(", DMA priority %d", dma_chan);
printk("\n");
DEB(aha1542_stat());
setup_mailboxes(base_io, shpnt);
DEB(aha1542_stat());
DEB(printk("aha1542_detect: enable interrupt channel %d\n", irq_level));
save_flags(flags);
cli();
if (request_irq(irq_level,aha1542_intr_handle, 0, "aha1542", NULL)) {
printk("Unable to allocate IRQ for adaptec controller.\n");
restore_flags(flags);
goto unregister;
}
if (dma_chan != 0xFF) {
if (request_dma(dma_chan,"aha1542")) {
printk("Unable to allocate DMA channel for Adaptec.\n");
free_irq(irq_level, NULL);
restore_flags(flags);
goto unregister;
}
if (dma_chan == 0 || dma_chan >= 5) {
set_dma_mode(dma_chan, DMA_MODE_CASCADE);
enable_dma(dma_chan);
}
}
aha_host[irq_level - 9] = shpnt;
shpnt->this_id = scsi_id;
shpnt->unique_id = base_io;
shpnt->io_port = base_io;
shpnt->n_io_port = 4;
shpnt->dma_channel = dma_chan;
shpnt->irq = irq_level;
HOSTDATA(shpnt)->bios_translation = trans;
if(trans == BIOS_TRANSLATION_25563)
printk("aha1542.c: Using extended bios translation\n");
HOSTDATA(shpnt)->aha1542_last_mbi_used = (2*AHA1542_MAILBOXES - 1);
HOSTDATA(shpnt)->aha1542_last_mbo_used = (AHA1542_MAILBOXES - 1);
memset(HOSTDATA(shpnt)->SCint, 0, sizeof(HOSTDATA(shpnt)->SCint));
restore_flags(flags);
#if 0
DEB(printk(" *** READ CAPACITY ***\n"));
{
unchar buf[8];
static unchar cmd[] = { READ_CAPACITY, 0, 0, 0, 0, 0, 0, 0, 0, 0};
int i;
for (i = 0; i < sizeof(buf); ++i) buf[i] = 0x87;
for (i = 0; i < 2; ++i)
if (!aha1542_command(i, cmd, buf, sizeof(buf))) {
printk("aha_detect: LU %d sector_size %d device_size %d\n",
i, xscsi2int(buf+4), xscsi2int(buf));
}
}
DEB(printk(" *** NOW RUNNING MY OWN TEST *** \n"));
for (i = 0; i < 4; ++i)
{
unsigned char cmd[10];
static buffer[512];
cmd[0] = READ_10;
cmd[1] = 0;
xany2scsi(cmd+2, i);
cmd[6] = 0;
cmd[7] = 0;
cmd[8] = 1;
cmd[9] = 0;
aha1542_command(0, cmd, buffer, 512);
}
#endif
request_region(bases[indx], 4,"aha1542");
count++;
continue;
unregister:
scsi_unregister(shpnt);
continue;
};
return count;
}
static int aha1542_restart(struct Scsi_Host * shost)
{
int i;
int count = 0;
#if 0
unchar ahacmd = CMD_START_SCSI;
#endif
for(i=0; i< AHA1542_MAILBOXES; i++)
if(HOSTDATA(shost)->SCint[i] &&
!(HOSTDATA(shost)->SCint[i]->device->soft_reset))
{
#if 0
HOSTDATA(shost)->mb[i].status = 1;
#endif
count++;
}
printk("Potential to restart %d stalled commands...\n", count);
#if 0
if (count) aha1542_out(shost->io_port, &ahacmd, 1);
#endif
return 0;
}
int aha1542_abort(Scsi_Cmnd * SCpnt)
{
#if 0
unchar ahacmd = CMD_START_SCSI;
unsigned long flags;
struct mailbox * mb;
int mbi, mbo, i;
printk("In aha1542_abort: %x %x\n",
inb(STATUS(SCpnt->host->io_port)),
inb(INTRFLAGS(SCpnt->host->io_port)));
save_flags(flags);
cli();
mb = HOSTDATA(SCpnt->host)->mb;
mbi = HOSTDATA(SCpnt->host)->aha1542_last_mbi_used + 1;
if (mbi >= 2*AHA1542_MAILBOXES) mbi = AHA1542_MAILBOXES;
do{
if(mb[mbi].status != 0) break;
mbi++;
if (mbi >= 2*AHA1542_MAILBOXES) mbi = AHA1542_MAILBOXES;
} while (mbi != HOSTDATA(SCpnt->host)->aha1542_last_mbi_used);
restore_flags(flags);
if(mb[mbi].status) {
printk("Lost interrupt discovered on irq %d - attempting to recover\n",
SCpnt->host->irq);
aha1542_intr_handle(SCpnt->host->irq, NULL);
return 0;
}
for(i=0; i< AHA1542_MAILBOXES; i++)
if(HOSTDATA(SCpnt->host)->SCint[i])
{
if(HOSTDATA(SCpnt->host)->SCint[i] == SCpnt) {
printk("Timed out command pending for %s\n",
kdevname(SCpnt->request.rq_dev));
if (HOSTDATA(SCpnt->host)->mb[i].status) {
printk("OGMB still full - restarting\n");
aha1542_out(SCpnt->host->io_port, &ahacmd, 1);
};
} else
printk("Other pending command %s\n",
kdevname(SCpnt->request.rq_dev));
}
#endif
DEB(printk("aha1542_abort\n"));
#if 0
save_flags(flags);
cli();
for(mbo = 0; mbo < AHA1542_MAILBOXES; mbo++)
if (SCpnt == HOSTDATA(SCpnt->host)->SCint[mbo]){
mb[mbo].status = 2;
aha1542_out(SCpnt->host->io_port, &ahacmd, 1);
restore_flags(flags);
break;
};
#endif
return SCSI_ABORT_SNOOZE;
}
int aha1542_reset(Scsi_Cmnd * SCpnt, unsigned int reset_flags)
{
unchar ahacmd = CMD_START_SCSI;
int i;
if( reset_flags & SCSI_RESET_SUGGEST_BUS_RESET )
{
outb(HRST | SCRST, CONTROL(SCpnt->host->io_port));
WAIT(STATUS(SCpnt->host->io_port),
STATMASK, INIT|IDLE, STST|DIAGF|INVDCMD|DF|CDF);
setup_mailboxes(SCpnt->host->io_port, SCpnt->host);
printk("Sent BUS RESET to scsi host %d\n", SCpnt->host->host_no);
for(i=0; i< AHA1542_MAILBOXES; i++)
if(HOSTDATA(SCpnt->host)->SCint[i] != NULL)
{
Scsi_Cmnd * SCtmp;
SCtmp = HOSTDATA(SCpnt->host)->SCint[i];
SCtmp->result = DID_RESET << 16;
if (SCtmp->host_scribble) scsi_free(SCtmp->host_scribble, 512);
printk("Sending DID_RESET for target %d\n", SCpnt->target);
SCtmp->scsi_done(SCpnt);
HOSTDATA(SCpnt->host)->SCint[i] = NULL;
HOSTDATA(SCpnt->host)->mb[i].status = 0;
}
return (SCSI_RESET_SUCCESS | SCSI_RESET_BUS_RESET);
fail:
printk("aha1542.c: Unable to perform hard reset.\n");
printk("Power cycle machine to reset\n");
return (SCSI_RESET_ERROR | SCSI_RESET_BUS_RESET);
}
else
{
for(i=0; i< AHA1542_MAILBOXES; i++)
if(HOSTDATA(SCpnt->host)->SCint[i] == SCpnt)
{
HOSTDATA(SCpnt->host)->ccb[i].op = 0x81;
aha1542_out(SCpnt->host->io_port, &ahacmd, 1);
printk("Sent BUS DEVICE RESET to target %d\n", SCpnt->target);
for(i=0; i< AHA1542_MAILBOXES; i++)
if(HOSTDATA(SCpnt->host)->SCint[i] &&
HOSTDATA(SCpnt->host)->SCint[i]->target == SCpnt->target)
{
Scsi_Cmnd * SCtmp;
SCtmp = HOSTDATA(SCpnt->host)->SCint[i];
SCtmp->result = DID_RESET << 16;
if (SCtmp->host_scribble) scsi_free(SCtmp->host_scribble, 512);
printk("Sending DID_RESET for target %d\n", SCpnt->target);
SCtmp->scsi_done(SCpnt);
HOSTDATA(SCpnt->host)->SCint[i] = NULL;
HOSTDATA(SCpnt->host)->mb[i].status = 0;
}
return SCSI_RESET_SUCCESS;
}
}
return SCSI_RESET_PUNT;
}
#include "sd.h"
int aha1542_biosparam(Scsi_Disk * disk, kdev_t dev, int * ip)
{
int translation_algorithm;
int size = disk->capacity;
translation_algorithm = HOSTDATA(disk->device->host)->bios_translation;
if((size>>11) > 1024 && translation_algorithm == BIOS_TRANSLATION_25563) {
ip[0] = 255;
ip[1] = 63;
ip[2] = size /255/63;
} else {
ip[0] = 64;
ip[1] = 32;
ip[2] = size >> 11;
}
return 0;
}
#ifdef MODULE
Scsi_Host_Template driver_template = AHA1542;
#include "scsi_module.c"
#endif