#define QL_INT_ACTIVE_HIGH 2
#define QL_USE_IRQ 1
#define QL_TURBO_PDMA 1
#define QL_ENABLE_PARITY 1
#define QL_RESET_AT_START 0
#define XTALFREQ	40
#define SLOWCABLE 1
#define FASTSCSI 0
#define FASTCLK 0
#define SYNCXFRPD 5
#define SYNCOFFST 0
#ifdef PCMCIA
#undef QL_INT_ACTIVE_HIGH
#define QL_INT_ACTIVE_HIGH 0
#define MODULE
#endif
#include <linux/module.h>
#ifdef PCMCIA
#undef MODULE
#endif
#include <linux/blk.h>
#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/ioport.h>
#include <linux/sched.h>
#include <linux/proc_fs.h>
#include <linux/unistd.h>
#include <asm/io.h>
#include <asm/irq.h>
#include "sd.h"
#include "hosts.h"
#include "qlogicfas.h"
#include<linux/stat.h>
struct proc_dir_entry proc_scsi_qlogicfas = {
PROC_SCSI_QLOGICFAS, 6, "qlogicfas",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
static int	    qbase = 0;
static int	    qinitid;
static int	    qabort;
static int	    qlirq = -1;
static char	    qinfo[80];
static Scsi_Cmnd   *qlcmd;
static int	    qlcfg5 = ( XTALFREQ << 5 );
static int	    qlcfg6 = SYNCXFRPD;
static int	    qlcfg7 = SYNCOFFST;
static int	    qlcfg8 = ( SLOWCABLE << 7 ) | ( QL_ENABLE_PARITY << 4 );
static int	    qlcfg9 = ( ( XTALFREQ + 4 ) / 5 );
static int	    qlcfgc = ( FASTCLK << 3 ) | ( FASTSCSI << 4 );
#define REG0 ( outb( inb( qbase + 0xd ) & 0x7f , qbase + 0xd ), outb( 4 , qbase + 0xd ))
#define REG1 ( outb( inb( qbase + 0xd ) | 0x80 , qbase + 0xd ), outb( 0xb4 | QL_INT_ACTIVE_HIGH , qbase + 0xd ))
#define WATCHDOG 5000000
#if 0
#define rtrc(i) {inb(0x3da);outb(0x31,0x3c0);outb((i),0x3c0);}
#else
#define rtrc(i) {}
#endif
static void	ql_zap(void);
void	ql_zap()
{
int	x;
unsigned long	flags;
save_flags( flags );
cli();
x = inb(qbase + 0xd);
REG0;
outb(3, qbase + 3);
outb(2, qbase + 3);
if (x & 0x80)
REG1;
restore_flags( flags );
}
static int	ql_pdma(int phase, char *request, int reqlen)
{
int	j;
j = 0;
if (phase & 1) {
#if QL_TURBO_PDMA
rtrc(4)
if( reqlen >= 128 && (inb( qbase + 8 ) & 2) ) {
insl( qbase + 4, request, 32 );
reqlen -= 128;
request += 128;
}
while( reqlen >= 84 && !( j & 0xc0 ) )
if( (j=inb( qbase + 8 )) & 4 ) {
insl( qbase + 4, request, 21 );
reqlen -= 84;
request += 84;
}
if( reqlen >= 44 && (inb( qbase + 8 ) & 8) ) {
insl( qbase + 4, request, 11 );
reqlen -= 44;
request += 44;
}
#endif
rtrc(7)
j = 0;
while( reqlen && !( (j & 0x10) && (j & 0xc0) ) ) {
j &= 0xc0;
while ( reqlen && !( (j=inb(qbase + 8)) & 0x10 ) ) {
*request++ = inb(qbase + 4);
reqlen--;
}
if( j & 0x10 )
j = inb(qbase+8);
}
}
else {
#if QL_TURBO_PDMA
rtrc(4)
if( reqlen >= 128 && inb( qbase + 8 ) & 0x10 ) {
outsl(qbase + 4, request, 32 );
reqlen -= 128;
request += 128;
}
while( reqlen >= 84 && !( j & 0xc0 ) )
if( !((j=inb( qbase + 8 )) & 8) ) {
outsl( qbase + 4, request, 21 );
reqlen -= 84;
request += 84;
}
if( reqlen >= 40 && !(inb( qbase + 8 ) & 4 ) ) {
outsl( qbase + 4, request, 10 );
reqlen -= 40;
request += 40;
}
#endif
rtrc(7)
j = 0;
while( reqlen && !( (j & 2) && (j & 0xc0) ) ) {
while ( reqlen && !( (j=inb(qbase + 8)) & 2 ) ) {
outb(*request++, qbase + 4);
reqlen--;
}
if( j & 2 )
j = inb(qbase+8);
}
}
return inb( qbase + 8 ) & 0xc0;
}
static int	ql_wai(void)
{
int	i,k;
k = 0;
i = jiffies + WATCHDOG;
while ( i > jiffies && !qabort && !((k = inb(qbase + 4)) & 0xe0))
barrier();
if (i <= jiffies)
return (DID_TIME_OUT);
if (qabort)
return (qabort == 1 ? DID_ABORT : DID_RESET);
if (k & 0x60)
ql_zap();
if (k & 0x20)
return (DID_PARITY);
if (k & 0x40)
return (DID_ERROR);
return 0;
}
static void	ql_icmd(Scsi_Cmnd * cmd)
{
unsigned int	    i;
unsigned long	flags;
qabort = 0;
save_flags( flags );
cli();
REG0;
inb(qbase + 5);
if (inb(qbase + 5))
outb(2, qbase + 3);
else if (inb(qbase + 7) & 0x1f)
outb(1, qbase + 3);
while (inb(qbase + 5));
REG1;
outb(1, qbase + 8);
outb(0, qbase + 0xb);
inb(qbase + 8);
REG0;
outb(0x40, qbase + 0xb);
outb( qlcfgc , qbase + 0xc);
outb( 0x40 | qlcfg8 | qinitid, qbase + 8);
outb( qlcfg7 , qbase + 7 );
outb( qlcfg6 , qbase + 6 );
outb(qlcfg5, qbase + 5);
outb(qlcfg9 & 7, qbase + 9);
outb(cmd->target, qbase + 4);
for (i = 0; i < cmd->cmd_len; i++)
outb(cmd->cmnd[i], qbase + 2);
qlcmd = cmd;
outb(0x41, qbase + 3);
restore_flags( flags );
}
static unsigned int	ql_pcmd(Scsi_Cmnd * cmd)
{
unsigned int	i, j, k;
unsigned int	result;
unsigned int	status;
unsigned int	message;
unsigned int	phase;
unsigned int	reqlen;
struct scatterlist	*sglist;
unsigned int	sgcount;
rtrc(1)
j = inb(qbase + 6);
i = inb(qbase + 5);
if (i == 0x20) {
return (DID_NO_CONNECT << 16);
}
i |= inb(qbase + 5);
if (i != 0x18) {
printk("Ql:Bad Interrupt status:%02x\n", i);
ql_zap();
return (DID_BAD_INTR << 16);
}
j &= 7;
if(j != 3 && j != 4) {
printk("Ql:Bad sequence for command %d, int %02X, cmdleft = %d\n", j, i, inb( qbase+7 ) & 0x1f );
ql_zap();
return (DID_ERROR << 16);
}
result = DID_OK;
if (inb(qbase + 7) & 0x1f)
outb(1, qbase + 3);
reqlen = cmd->request_bufflen;
if (reqlen && !((phase = inb(qbase + 4)) & 6)) {
rtrc(2)
outb(reqlen, qbase);
outb(reqlen >> 8, qbase+1);
outb(reqlen >> 16, qbase + 0xe);
outb(0x90, qbase + 3);
REG1;
if (!cmd->use_sg)
ql_pdma(phase, cmd->request_buffer, cmd->request_bufflen);
else {
sgcount = cmd->use_sg;
sglist = cmd->request_buffer;
while (sgcount--) {
if (qabort) {
REG0;
return ((qabort == 1 ? DID_ABORT : DID_RESET) << 16);
}
if (ql_pdma(phase, sglist->address, sglist->length))
break;
sglist++;
}
}
REG0;
rtrc(2)
if ((k = ql_wai()))
return (k << 16);
k = inb(qbase + 5);
}
k = jiffies + WATCHDOG;
while ( k > jiffies && !qabort && !(inb(qbase + 4) & 6));
if ( k <= jiffies ) {
ql_zap();
return (DID_TIME_OUT << 16);
}
while (inb(qbase + 5));
if (qabort)
return ((qabort == 1 ? DID_ABORT : DID_RESET) << 16);
outb(0x11, qbase + 3);
if ((k = ql_wai()))
return (k << 16);
i = inb(qbase + 5);
j = inb(qbase + 7) & 0x1f;
status = inb(qbase + 2);
message = inb(qbase + 2);
if (!((i == 8 && j == 2) || (i == 0x10 && j == 1))) {
printk("Ql:Error during status phase, int=%02X, %d bytes recd\n", i, j);
result = DID_ERROR;
}
outb(0x12, qbase + 3);
rtrc(1)
if ((k = ql_wai()))
return (k << 16);
i = inb(qbase + 5);
while (!qabort && ((i & 0x20) != 0x20)) {
barrier();
i |= inb(qbase + 5);
}
rtrc(0)
if (qabort)
return ((qabort == 1 ? DID_ABORT : DID_RESET) << 16);
return (result << 16) | (message << 8) | (status & STATUS_MASK);
}
#if QL_USE_IRQ
static void	       ql_ihandl(int irq, void *dev_id, struct pt_regs * regs)
{
Scsi_Cmnd	   *icmd;
REG0;
if (!(inb(qbase + 4) & 0x80))
return;
if (qlcmd == NULL) {
int	i;
i = 16;
while (i-- && inb(qbase + 5));
return;
}
icmd = qlcmd;
icmd->result = ql_pcmd(icmd);
qlcmd = NULL;
(icmd->scsi_done) (icmd);
}
#endif
#if QL_USE_IRQ
static void	qlidone(Scsi_Cmnd * cmd) {};
#endif
int	qlogicfas_command(Scsi_Cmnd * cmd)
{
int	k;
#if QL_USE_IRQ
if (qlirq >= 0) {
qlogicfas_queuecommand(cmd, qlidone);
while (qlcmd != NULL);
return cmd->result;
}
#endif
if (cmd->target == qinitid)
return (DID_BAD_TARGET << 16);
ql_icmd(cmd);
if ((k = ql_wai()))
return (k << 16);
return ql_pcmd(cmd);
}
#if QL_USE_IRQ
int	qlogicfas_queuecommand(Scsi_Cmnd * cmd, void (*done) (Scsi_Cmnd *))
{
if(cmd->target == qinitid) {
cmd->result = DID_BAD_TARGET << 16;
done(cmd);
return 0;
}
cmd->scsi_done = done;
while (qlcmd != NULL)
barrier();
ql_icmd(cmd);
return 0;
}
#else
int	qlogicfas_queuecommand(Scsi_Cmnd * cmd, void (*done) (Scsi_Cmnd *))
{
return 1;
}
#endif
#ifdef PCMCIA
void	qlogicfas_preset(int port, int irq)
{
qbase=port;
qlirq=irq;
}
#endif
int	qlogicfas_detect(Scsi_Host_Template * host)
{
int	i, j;
int	qltyp;
struct	Scsi_Host	*hreg;
unsigned long	flags;
host->proc_dir =  &proc_scsi_qlogicfas;
if( !qbase ) {
for (qbase = 0x230; qbase < 0x430; qbase += 0x100) {
if( check_region( qbase , 0x10 ) )
continue;
REG1;
if ( ( (inb(qbase + 0xe) ^ inb(qbase + 0xe)) == 7 )
&& ( (inb(qbase + 0xe) ^ inb(qbase + 0xe)) == 7 ) )
break;
}
if (qbase == 0x430)
return 0;
}
else
printk( "Ql: Using preset base address of %03x\n", qbase );
qltyp = inb(qbase + 0xe) & 0xf8;
qinitid = host->this_id;
if (qinitid < 0)
qinitid = 7;
outb(1, qbase + 8);
REG0;
outb(0x40 | qlcfg8 | qinitid, qbase + 8);
outb(qlcfg5, qbase + 5);
outb(qlcfg9, qbase + 9);
#if QL_RESET_AT_START
outb( 3 , qbase + 3 );
REG1;
while( inb( qbase + 0xf ) & 4 );
REG0;
#endif
#if QL_USE_IRQ
if( qlirq == -1 ) {
save_flags( flags );
cli();
i = 0xffff;
j = 3;
outb(0x90, qbase + 3);
REG1;
outb(10, 0x20);
outb(10, 0xa0);
while (j--) {
outb(0xb0 | QL_INT_ACTIVE_HIGH , qbase + 0xd);
i &= ~(inb(0x20) | (inb(0xa0) << 8));
outb(0xb4 | QL_INT_ACTIVE_HIGH , qbase + 0xd);
i &= inb(0x20) | (inb(0xa0) << 8);
}
REG0;
while (inb(qbase + 5));
j = -1;
while (i)
i >>= 1, j++;
qlirq = j;
restore_flags( flags );
}
else
printk( "Ql: Using preset IRQ %d\n", qlirq );
if (qlirq >= 0 && !request_irq(qlirq, ql_ihandl, 0, "qlogicfas", NULL))
host->can_queue = 1;
#endif
request_region( qbase , 0x10 ,"qlogicfas");
hreg = scsi_register( host , 0 );
hreg->io_port = qbase;
hreg->n_io_port = 16;
hreg->dma_channel = -1;
if( qlirq != -1 )
hreg->irq = qlirq;
sprintf(qinfo, "Qlogicfas Driver version 0.45, chip %02X at %03X, IRQ %d, TPdma:%d",
qltyp, qbase, qlirq, QL_TURBO_PDMA );
host->name = qinfo;
return 1;
}
int	qlogicfas_biosparam(Disk * disk, kdev_t dev, int ip[])
{
ip[0] = 0x40;
ip[1] = 0x20;
ip[2] = disk->capacity / (ip[0] * ip[1]);
if (ip[2] > 1024) {
ip[0] = 0xff;
ip[1] = 0x3f;
ip[2] = disk->capacity / (ip[0] * ip[1]);
if (ip[2] > 1023)
ip[2] = 1023;
}
return 0;
}
int	qlogicfas_abort(Scsi_Cmnd * cmd)
{
qabort = 1;
ql_zap();
return 0;
}
int	qlogicfas_reset(Scsi_Cmnd * cmd, unsigned int flags)
{
qabort = 2;
ql_zap();
return 1;
}
const char	*qlogicfas_info(struct Scsi_Host * host)
{
return qinfo;
}
#ifdef MODULE
Scsi_Host_Template driver_template = QLOGICFAS;
#include "scsi_module.c"
#endif