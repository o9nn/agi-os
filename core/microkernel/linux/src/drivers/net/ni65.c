#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/ptrace.h>
#include <linux/errno.h>
#include <linux/ioport.h>
#include <linux/malloc.h>
#include <linux/interrupt.h>
#include <linux/delay.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/version.h>
#include <linux/module.h>
#include "ni65.h"
#undef XMT_VIA_SKB
#undef RCV_VIA_SKB
#define RCV_PARANOIA_CHECK
#define MID_PERFORMANCE
#if defined( LOW_PERFORMANCE )
static int isa0=7,isa1=7,csr80=0x0c10;
#elif defined( MID_PERFORMANCE )
static int isa0=5,isa1=5,csr80=0x2810;
#else
static int isa0=4,isa1=4,csr80=0x0017;
#endif
#define NI65_ID0 0x00
#define NI65_ID1 0x55
#define NI65_EB_ID0 0x52
#define NI65_EB_ID1 0x44
#define NE2100_ID0 0x57
#define NE2100_ID1 0x57
#define PORT p->cmdr_addr
#if 1
#define RMDNUM 16
#define RMDNUMMASK 0x80000000
#else
#define RMDNUM 8
#define RMDNUMMASK 0x60000000
#endif
#if 0
#define TMDNUM 1
#define TMDNUMMASK 0x00000000
#else
#define TMDNUM 4
#define TMDNUMMASK 0x40000000
#endif
#define R_BUF_SIZE 1544
#define T_BUF_SIZE 1544
#define L_DATAREG 0x00
#define L_ADDRREG 0x02
#define L_RESET 0x04
#define L_CONFIG 0x05
#define L_BUSIF 0x06
#define CSR0 0x00
#define CSR1 0x01
#define CSR2 0x02
#define CSR3 0x03
#define INIT_RING_BEFORE_START 0x1
#define FULL_RESET_ON_ERROR 0x2
#if 0
#define writereg(val,reg) {outw(reg,PORT+L_ADDRREG);inw(PORT+L_ADDRREG); \
outw(val,PORT+L_DATAREG);inw(PORT+L_DATAREG);}
#define readreg(reg) (outw(reg,PORT+L_ADDRREG),inw(PORT+L_ADDRREG),\
inw(PORT+L_DATAREG))
#if 0
#define writedatareg(val) {outw(val,PORT+L_DATAREG);inw(PORT+L_DATAREG);}
#else
#define writedatareg(val) { writereg(val,CSR0); }
#endif
#else
#define writereg(val,reg) {outw(reg,PORT+L_ADDRREG);outw(val,PORT+L_DATAREG);}
#define readreg(reg) (outw(reg,PORT+L_ADDRREG),inw(PORT+L_DATAREG))
#define writedatareg(val) { writereg(val,CSR0); }
#endif
static unsigned char ni_vendor[] = { 0x02,0x07,0x01 };
static struct card {
unsigned char id0,id1;
short id_offset;
short total_size;
short cmd_offset;
short addr_offset;
unsigned char *vendor_id;
char *cardname;
unsigned char config;
} cards[] = {
{ NI65_ID0,NI65_ID1,0x0e,0x10,0x0,0x8,ni_vendor,"ni6510", 0x1 } ,
{ NI65_EB_ID0,NI65_EB_ID1,0x0e,0x18,0x10,0x0,ni_vendor,"ni6510 EtherBlaster", 0x2 } ,
{ NE2100_ID0,NE2100_ID1,0x0e,0x18,0x10,0x0,NULL,"generic NE2100", 0x0 }
};
#define NUM_CARDS 3
struct priv
{
struct rmd rmdhead[RMDNUM];
struct tmd tmdhead[TMDNUM];
struct init_block ib;
int rmdnum;
int tmdnum,tmdlast;
#ifdef RCV_VIA_SKB
struct sk_buff *recv_skb[RMDNUM];
#else
void *recvbounce[RMDNUM];
#endif
#ifdef XMT_VIA_SKB
struct sk_buff *tmd_skb[TMDNUM];
#endif
void *tmdbounce[TMDNUM];
int tmdbouncenum;
int lock,xmit_queued;
struct enet_statistics stats;
void *self;
int cmdr_addr;
int cardno;
int features;
};
static int ni65_probe1(struct device *dev,int);
static void ni65_interrupt(int irq, void * dev_id, struct pt_regs *regs);
static void ni65_recv_intr(struct device *dev,int);
static void ni65_xmit_intr(struct device *dev,int);
static int ni65_open(struct device *dev);
static int ni65_lance_reinit(struct device *dev);
static void ni65_init_lance(struct priv *p,unsigned char*,int,int);
static int ni65_send_packet(struct sk_buff *skb, struct device *dev);
static int ni65_close(struct device *dev);
static int ni65_alloc_buffer(struct device *dev);
static void ni65_free_buffer(struct priv *p);
static struct enet_statistics *ni65_get_stats(struct device *);
static void set_multicast_list(struct device *dev);
static int irqtab[] = { 9,12,15,5 };
static int dmatab[] = { 0,3,5,6,7 };
static int debuglevel = 1;
static void ni65_set_performance(struct priv *p)
{
writereg(CSR0_STOP | CSR0_CLRALL,CSR0);
if( !(cards[p->cardno].config & 0x02) )
return;
outw(80,PORT+L_ADDRREG);
if(inw(PORT+L_ADDRREG) != 80)
return;
writereg( (csr80 & 0x3fff) ,80);
outw(0,PORT+L_ADDRREG);
outw((short)isa0,PORT+L_BUSIF);
outw(1,PORT+L_ADDRREG);
outw((short)isa1,PORT+L_BUSIF);
outw(CSR0,PORT+L_ADDRREG);
}
static int ni65_open(struct device *dev)
{
struct priv *p = (struct priv *) dev->priv;
int irqval = request_irq(dev->irq, &ni65_interrupt,0,
cards[p->cardno].cardname,NULL);
if (irqval) {
printk ("%s: unable to get IRQ %d (irqval=%d).\n",
dev->name,dev->irq, irqval);
return -EAGAIN;
}
irq2dev_map[dev->irq] = dev;
if(ni65_lance_reinit(dev))
{
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
MOD_INC_USE_COUNT;
return 0;
}
else
{
irq2dev_map[dev->irq] = NULL;
free_irq(dev->irq,NULL);
dev->start = 0;
return -EAGAIN;
}
}
static int ni65_close(struct device *dev)
{
struct priv *p = (struct priv *) dev->priv;
outw(inw(PORT+L_RESET),PORT+L_RESET);
#ifdef XMT_VIA_SKB
{
int i;
for(i=0;i<TMDNUM;i++)
{
if(p->tmd_skb[i]) {
dev_kfree_skb(p->tmd_skb[i],FREE_WRITE);
p->tmd_skb[i] = NULL;
}
}
}
#endif
irq2dev_map[dev->irq] = NULL;
free_irq(dev->irq,NULL);
dev->tbusy = 1;
dev->start = 0;
MOD_DEC_USE_COUNT;
return 0;
}
#ifdef MODULE
static
#endif
int ni65_probe(struct device *dev)
{
int *port;
static int ports[] = {0x360,0x300,0x320,0x340, 0};
if (dev->base_addr > 0x1ff)
return ni65_probe1(dev, dev->base_addr);
else if (dev->base_addr > 0)
return -ENXIO;
for (port = ports; *port; port++)
{
if (ni65_probe1(dev, *port) == 0)
return 0;
}
return -ENODEV;
}
static int ni65_probe1(struct device *dev,int ioaddr)
{
int i,j;
struct priv *p;
for(i=0;i<NUM_CARDS;i++) {
if(check_region(ioaddr, cards[i].total_size))
continue;
if(cards[i].id_offset >= 0) {
if(inb(ioaddr+cards[i].id_offset+0) != cards[i].id0 ||
inb(ioaddr+cards[i].id_offset+1) != cards[i].id1) {
continue;
}
}
if(cards[i].vendor_id) {
for(j=0;j<3;j++)
if(inb(ioaddr+cards[i].addr_offset+j) != cards[i].vendor_id[j])
continue;
}
break;
}
if(i == NUM_CARDS)
return -ENODEV;
for(j=0;j<6;j++)
dev->dev_addr[j] = inb(ioaddr+cards[i].addr_offset+j);
if( (j=ni65_alloc_buffer(dev)) < 0)
return j;
p = (struct priv *) dev->priv;
p->cmdr_addr = ioaddr + cards[i].cmd_offset;
p->cardno = i;
printk("%s: %s found at %#3x, ", dev->name, cards[p->cardno].cardname , ioaddr);
outw(inw(PORT+L_RESET),PORT+L_RESET);
if( (j=readreg(CSR0)) != 0x4) {
printk(KERN_ERR "can't RESET card: %04x\n",j);
ni65_free_buffer(p);
return -EAGAIN;
}
outw(88,PORT+L_ADDRREG);
if(inw(PORT+L_ADDRREG) == 88) {
unsigned long v;
v = inw(PORT+L_DATAREG);
v <<= 16;
outw(89,PORT+L_ADDRREG);
v |= inw(PORT+L_DATAREG);
printk("Version %#08lx, ",v);
p->features = INIT_RING_BEFORE_START;
}
else {
printk("ancient LANCE, ");
p->features = 0x0;
}
if(test_bit(0,&cards[i].config)) {
dev->irq = irqtab[(inw(ioaddr+L_CONFIG)>>2)&3];
dev->dma = dmatab[inw(ioaddr+L_CONFIG)&3];
printk("IRQ %d (from card), DMA %d (from card).\n",dev->irq,dev->dma);
}
else {
if(dev->dma == 0) {
int dma_channels = ((inb(DMA1_STAT_REG) >> 4) & 0x0f) | (inb(DMA2_STAT_REG) & 0xf0);
for(i=1;i<5;i++) {
int dma = dmatab[i];
if(test_bit(dma,&dma_channels) || request_dma(dma,"ni6510"))
continue;
disable_dma(dma);
set_dma_mode(dma,DMA_MODE_CASCADE);
enable_dma(dma);
ni65_init_lance(p,dev->dev_addr,0,0);
disable_dma(dma);
free_dma(dma);
if(readreg(CSR0) & CSR0_IDON)
break;
}
if(i == 5) {
printk("Can't detect DMA channel!\n");
ni65_free_buffer(p);
return -EAGAIN;
}
dev->dma = dmatab[i];
printk("DMA %d (autodetected), ",dev->dma);
}
else
printk("DMA %d (assigned), ",dev->dma);
if(dev->irq < 2)
{
ni65_init_lance(p,dev->dev_addr,0,0);
autoirq_setup(0);
writereg(CSR0_INIT|CSR0_INEA,CSR0);
if(!(dev->irq = autoirq_report(2)))
{
printk("Failed to detect IRQ line!\n");
ni65_free_buffer(p);
return -EAGAIN;
}
printk("IRQ %d (autodetected).\n",dev->irq);
}
else
printk("IRQ %d (assigned).\n",dev->irq);
}
if(request_dma(dev->dma, cards[p->cardno].cardname ) != 0)
{
printk("%s: Can't request dma-channel %d\n",dev->name,(int) dev->dma);
ni65_free_buffer(p);
return -EAGAIN;
}
request_region(ioaddr,cards[p->cardno].total_size,cards[p->cardno].cardname);
dev->base_addr = ioaddr;
dev->open = ni65_open;
dev->stop = ni65_close;
dev->hard_start_xmit = ni65_send_packet;
dev->get_stats = ni65_get_stats;
dev->set_multicast_list = set_multicast_list;
ether_setup(dev);
dev->interrupt = 0;
dev->tbusy = 0;
dev->start = 0;
return 0;
}
static void ni65_init_lance(struct priv *p,unsigned char *daddr,int filter,int mode)
{
int i;
u32 pib;
writereg(CSR0_CLRALL|CSR0_STOP,CSR0);
for(i=0;i<6;i++)
p->ib.eaddr[i] = daddr[i];
for(i=0;i<8;i++)
p->ib.filter[i] = filter;
p->ib.mode = mode;
p->ib.trp = (u32) virt_to_bus(p->tmdhead) | TMDNUMMASK;
p->ib.rrp = (u32) virt_to_bus(p->rmdhead) | RMDNUMMASK;
writereg(0,CSR3);
pib = (u32) virt_to_bus(&p->ib);
writereg(pib & 0xffff,CSR1);
writereg(pib >> 16,CSR2);
writereg(CSR0_INIT,CSR0);
for(i=0;i<32;i++)
{
udelay(4000);
if(inw(PORT+L_DATAREG) & (CSR0_IDON | CSR0_MERR) )
break;
}
}
static void *ni65_alloc_mem(struct device *dev,char *what,int size,int type)
{
struct sk_buff *skb=NULL;
unsigned char *ptr;
void *ret;
if(type) {
ret = skb = alloc_skb(2+16+size,GFP_KERNEL|GFP_DMA);
if(!skb) {
printk("%s: unable to allocate %s memory.\n",dev->name,what);
return NULL;
}
skb->dev = dev;
skb_reserve(skb,2+16);
skb_put(skb,R_BUF_SIZE);
ptr = skb->data;
}
else {
ret = ptr = kmalloc(T_BUF_SIZE,GFP_KERNEL | GFP_DMA);
if(!ret) {
printk("%s: unable to allocate %s memory.\n",dev->name,what);
return NULL;
}
}
if( (u32) virt_to_bus(ptr+size) > 0x1000000) {
printk("%s: unable to allocate %s memory in lower 16MB!\n",dev->name,what);
if(type)
kfree_skb(skb,FREE_WRITE);
else
kfree(ptr);
return NULL;
}
return ret;
}
static int ni65_alloc_buffer(struct device *dev)
{
unsigned char *ptr;
struct priv *p;
int i;
ptr = ni65_alloc_mem(dev,"BUFFER",sizeof(struct priv)+8,0);
if(!ptr)
return -ENOMEM;
p = dev->priv = (struct priv *) (((unsigned long) ptr + 7) & ~0x7);
memset((char *) dev->priv,0,sizeof(struct priv));
p->self = ptr;
for(i=0;i<TMDNUM;i++)
{
#ifdef XMT_VIA_SKB
p->tmd_skb[i] = NULL;
#endif
p->tmdbounce[i] = ni65_alloc_mem(dev,"XMIT",T_BUF_SIZE,0);
if(!p->tmdbounce[i]) {
ni65_free_buffer(p);
return -ENOMEM;
}
}
for(i=0;i<RMDNUM;i++)
{
#ifdef RCV_VIA_SKB
p->recv_skb[i] = ni65_alloc_mem(dev,"RECV",R_BUF_SIZE,1);
if(!p->recv_skb[i]) {
ni65_free_buffer(p);
return -ENOMEM;
}
#else
p->recvbounce[i] = ni65_alloc_mem(dev,"RECV",R_BUF_SIZE,0);
if(!p->recvbounce[i]) {
ni65_free_buffer(p);
return -ENOMEM;
}
#endif
}
return 0;
}
static void ni65_free_buffer(struct priv *p)
{
int i;
if(!p)
return;
for(i=0;i<TMDNUM;i++) {
if(p->tmdbounce[i])
kfree(p->tmdbounce[i]);
#ifdef XMT_VIA_SKB
if(p->tmd_skb[i])
dev_kfree_skb(p->tmd_skb[i],FREE_WRITE);
#endif
}
for(i=0;i<RMDNUM;i++)
{
#ifdef RCV_VIA_SKB
if(p->recv_skb[i])
dev_kfree_skb(p->recv_skb[i],FREE_WRITE);
#else
if(p->recvbounce[i])
kfree(p->recvbounce[i]);
#endif
}
if(p->self)
kfree(p->self);
}
static void ni65_stop_start(struct device *dev,struct priv *p)
{
int csr0 = CSR0_INEA;
writedatareg(CSR0_STOP);
if(debuglevel > 1)
printk("ni65_stop_start\n");
if(p->features & INIT_RING_BEFORE_START) {
int i;
#ifdef XMT_VIA_SKB
struct sk_buff *skb_save[TMDNUM];
#endif
unsigned long buffer[TMDNUM];
short blen[TMDNUM];
if(p->xmit_queued) {
while(1) {
if((p->tmdhead[p->tmdlast].u.s.status & XMIT_OWN))
break;
p->tmdlast = (p->tmdlast + 1) & (TMDNUM-1);
if(p->tmdlast == p->tmdnum)
break;
}
}
for(i=0;i<TMDNUM;i++) {
struct tmd *tmdp = p->tmdhead + i;
#ifdef XMT_VIA_SKB
skb_save[i] = p->tmd_skb[i];
#endif
buffer[i] = (u32) bus_to_virt(tmdp->u.buffer);
blen[i] = tmdp->blen;
tmdp->u.s.status = 0x0;
}
for(i=0;i<RMDNUM;i++) {
struct rmd *rmdp = p->rmdhead + i;
rmdp->u.s.status = RCV_OWN;
}
p->tmdnum = p->xmit_queued = 0;
writedatareg(CSR0_STRT | csr0);
for(i=0;i<TMDNUM;i++) {
int num = (i + p->tmdlast) & (TMDNUM-1);
p->tmdhead[i].u.buffer = (u32) virt_to_bus((char *)buffer[num]);
p->tmdhead[i].blen = blen[num];
if(p->tmdhead[i].u.s.status & XMIT_OWN) {
p->tmdnum = (p->tmdnum + 1) & (TMDNUM-1);
p->xmit_queued = 1;
writedatareg(CSR0_TDMD | CSR0_INEA | csr0);
}
#ifdef XMT_VIA_SKB
p->tmd_skb[i] = skb_save[num];
#endif
}
p->rmdnum = p->tmdlast = 0;
if(!p->lock)
dev->tbusy = (p->tmdnum || !p->xmit_queued) ? 0 : 1;
dev->trans_start = jiffies;
}
else
writedatareg(CSR0_STRT | csr0);
}
static int ni65_lance_reinit(struct device *dev)
{
int i;
struct priv *p = (struct priv *) dev->priv;
p->lock = 0;
p->xmit_queued = 0;
disable_dma(dev->dma);
set_dma_mode(dev->dma,DMA_MODE_CASCADE);
enable_dma(dev->dma);
outw(inw(PORT+L_RESET),PORT+L_RESET);
if( (i=readreg(CSR0) ) != 0x4)
{
printk(KERN_ERR "%s: can't RESET %s card: %04x\n",dev->name,
cards[p->cardno].cardname,(int) i);
disable_dma(dev->dma);
return 0;
}
p->rmdnum = p->tmdnum = p->tmdlast = p->tmdbouncenum = 0;
for(i=0;i<TMDNUM;i++)
{
struct tmd *tmdp = p->tmdhead + i;
#ifdef XMT_VIA_SKB
if(p->tmd_skb[i]) {
dev_kfree_skb(p->tmd_skb[i],FREE_WRITE);
p->tmd_skb[i] = NULL;
}
#endif
tmdp->u.buffer = 0x0;
tmdp->u.s.status = XMIT_START | XMIT_END;
tmdp->blen = tmdp->status2 = 0;
}
for(i=0;i<RMDNUM;i++)
{
struct rmd *rmdp = p->rmdhead + i;
#ifdef RCV_VIA_SKB
rmdp->u.buffer = (u32) virt_to_bus(p->recv_skb[i]->data);
#else
rmdp->u.buffer = (u32) virt_to_bus(p->recvbounce[i]);
#endif
rmdp->blen = -(R_BUF_SIZE-8);
rmdp->mlen = 0;
rmdp->u.s.status = RCV_OWN;
}
if(dev->flags & IFF_PROMISC)
ni65_init_lance(p,dev->dev_addr,0x00,M_PROM);
else if(dev->mc_count || dev->flags & IFF_ALLMULTI)
ni65_init_lance(p,dev->dev_addr,0xff,0x0);
else
ni65_init_lance(p,dev->dev_addr,0x00,0x00);
if(inw(PORT+L_DATAREG) & CSR0_IDON) {
ni65_set_performance(p);
writedatareg(CSR0_CLRALL | CSR0_INEA | CSR0_STRT);
return 1;
}
printk(KERN_ERR "%s: can't init lance, status: %04x\n",dev->name,(int) inw(PORT+L_DATAREG));
disable_dma(dev->dma);
return 0;
}
static void ni65_interrupt(int irq, void * dev_id, struct pt_regs * regs)
{
int csr0;
struct device *dev = (struct device *) irq2dev_map[irq];
struct priv *p;
int bcnt = 32;
if (dev == NULL) {
printk (KERN_ERR "ni65_interrupt(): irq %d for unknown device.\n", irq);
return;
}
if(set_bit(0,(int *) &dev->interrupt)) {
printk("ni65: oops .. interrupt while proceeding interrupt\n");
return;
}
p = (struct priv *) dev->priv;
while(--bcnt) {
csr0 = inw(PORT+L_DATAREG);
#if 0
writedatareg( (csr0 & CSR0_CLRALL) );
#else
writedatareg( (csr0 & CSR0_CLRALL) | CSR0_INEA );
#endif
if(!(csr0 & (CSR0_ERR | CSR0_RINT | CSR0_TINT)))
break;
if(csr0 & CSR0_RINT)
ni65_recv_intr(dev,csr0);
if(csr0 & CSR0_TINT)
ni65_xmit_intr(dev,csr0);
if(csr0 & CSR0_ERR)
{
struct priv *p = (struct priv *) dev->priv;
if(debuglevel > 1)
printk("%s: general error: %04x.\n",dev->name,csr0);
if(csr0 & CSR0_BABL)
p->stats.tx_errors++;
if(csr0 & CSR0_MISS) {
int i;
for(i=0;i<RMDNUM;i++)
printk("%02x ",p->rmdhead[i].u.s.status);
printk("\n");
p->stats.rx_errors++;
}
if(csr0 & CSR0_MERR) {
if(debuglevel > 1)
printk("%s: Ooops .. memory error: %04x.\n",dev->name,csr0);
ni65_stop_start(dev,p);
}
}
}
#ifdef RCV_PARANOIA_CHECK
{
int j;
for(j=0;j<RMDNUM;j++)
{
struct priv *p = (struct priv *) dev->priv;
int i,k,num1,num2;
for(i=RMDNUM-1;i>0;i--) {
num2 = (p->rmdnum + i) & (RMDNUM-1);
if(!(p->rmdhead[num2].u.s.status & RCV_OWN))
break;
}
if(i) {
for(k=0;k<RMDNUM;k++) {
num1 = (p->rmdnum + k) & (RMDNUM-1);
if(!(p->rmdhead[num1].u.s.status & RCV_OWN))
break;
}
if(!k)
break;
if(debuglevel > 0)
{
char buf[256],*buf1;
int k;
buf1 = buf;
for(k=0;k<RMDNUM;k++) {
sprintf(buf1,"%02x ",(p->rmdhead[k].u.s.status));
buf1 += 3;
}
*buf1 = 0;
printk(KERN_ERR "%s: Ooops, receive ring corrupted %2d %2d | %s\n",dev->name,p->rmdnum,i,buf);
}
p->rmdnum = num1;
ni65_recv_intr(dev,csr0);
if((p->rmdhead[num2].u.s.status & RCV_OWN))
break;
}
else
break;
}
}
#endif
if( (csr0 & (CSR0_RXON | CSR0_TXON)) != (CSR0_RXON | CSR0_TXON) ) {
printk("%s: RX or TX was offline -> restart\n",dev->name);
ni65_stop_start(dev,p);
}
else
writedatareg(CSR0_INEA);
dev->interrupt = 0;
return;
}
static void ni65_xmit_intr(struct device *dev,int csr0)
{
struct priv *p = (struct priv *) dev->priv;
while(p->xmit_queued)
{
struct tmd *tmdp = p->tmdhead + p->tmdlast;
int tmdstat = tmdp->u.s.status;
if(tmdstat & XMIT_OWN)
break;
if(tmdstat & XMIT_ERR)
{
#if 0
if(tmdp->status2 & XMIT_TDRMASK && debuglevel > 3)
printk(KERN_ERR "%s: tdr-problems (e.g. no resistor)\n",dev->name);
#endif
if(tmdp->status2 & XMIT_RTRY)
p->stats.tx_aborted_errors++;
if(tmdp->status2 & XMIT_LCAR)
p->stats.tx_carrier_errors++;
if(tmdp->status2 & (XMIT_BUFF | XMIT_UFLO )) {
p->stats.tx_fifo_errors++;
if(debuglevel > 0)
printk(KERN_ERR "%s: Xmit FIFO/BUFF error\n",dev->name);
if(p->features & INIT_RING_BEFORE_START) {
tmdp->u.s.status = XMIT_OWN | XMIT_START | XMIT_END;
ni65_stop_start(dev,p);
break;
}
else
ni65_stop_start(dev,p);
}
if(debuglevel > 2)
printk(KERN_ERR "%s: xmit-error: %04x %02x-%04x\n",dev->name,csr0,(int) tmdstat,(int) tmdp->status2);
if(!(csr0 & CSR0_BABL))
p->stats.tx_errors++;
tmdp->status2 = 0;
}
else
p->stats.tx_packets++;
#ifdef XMT_VIA_SKB
if(p->tmd_skb[p->tmdlast]) {
dev_kfree_skb(p->tmd_skb[p->tmdlast],FREE_WRITE);
p->tmd_skb[p->tmdlast] = NULL;
}
#endif
p->tmdlast = (p->tmdlast + 1) & (TMDNUM-1);
if(p->tmdlast == p->tmdnum)
p->xmit_queued = 0;
}
dev->tbusy = 0;
mark_bh(NET_BH);
}
static void ni65_recv_intr(struct device *dev,int csr0)
{
struct rmd *rmdp;
int rmdstat,len;
int cnt=0;
struct priv *p = (struct priv *) dev->priv;
rmdp = p->rmdhead + p->rmdnum;
while(!( (rmdstat = rmdp->u.s.status) & RCV_OWN))
{
cnt++;
if( (rmdstat & (RCV_START | RCV_END | RCV_ERR)) != (RCV_START | RCV_END) )
{
if(!(rmdstat & RCV_ERR)) {
if(rmdstat & RCV_START)
{
p->stats.rx_length_errors++;
printk(KERN_ERR "%s: recv, packet too long: %d\n",dev->name,rmdp->mlen & 0x0fff);
}
}
else {
if(debuglevel > 2)
printk(KERN_ERR "%s: receive-error: %04x, lance-status: %04x/%04x\n",
dev->name,(int) rmdstat,csr0,(int) inw(PORT+L_DATAREG) );
if(rmdstat & RCV_FRAM)
p->stats.rx_frame_errors++;
if(rmdstat & RCV_OFLO)
p->stats.rx_over_errors++;
if(rmdstat & RCV_CRC)
p->stats.rx_crc_errors++;
if(rmdstat & RCV_BUF_ERR)
p->stats.rx_fifo_errors++;
}
if(!(csr0 & CSR0_MISS))
p->stats.rx_errors++;
}
else if( (len = (rmdp->mlen & 0x0fff) - 4) >= 60)
{
#ifdef RCV_VIA_SKB
struct sk_buff *skb = alloc_skb(R_BUF_SIZE+2+16,GFP_ATOMIC);
if (skb)
skb_reserve(skb,16);
#else
struct sk_buff *skb = dev_alloc_skb(len+2);
#endif
if(skb)
{
skb_reserve(skb,2);
skb->dev = dev;
#ifdef RCV_VIA_SKB
if( (unsigned long) (skb->data + R_BUF_SIZE) > 0x1000000) {
skb_put(skb,len);
eth_copy_and_sum(skb, (unsigned char *)(p->recv_skb[p->rmdnum]->data),len,0);
}
else {
struct sk_buff *skb1 = p->recv_skb[p->rmdnum];
skb_put(skb,R_BUF_SIZE);
p->recv_skb[p->rmdnum] = skb;
rmdp->u.buffer = (u32) virt_to_bus(skb->data);
skb = skb1;
skb_trim(skb,len);
}
#else
skb_put(skb,len);
eth_copy_and_sum(skb, (unsigned char *) p->recvbounce[p->rmdnum],len,0);
#endif
p->stats.rx_packets++;
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
}
else
{
printk(KERN_ERR "%s: can't alloc new sk_buff\n",dev->name);
p->stats.rx_dropped++;
}
}
else {
printk(KERN_INFO "%s: received runt packet\n",dev->name);
p->stats.rx_errors++;
}
rmdp->blen = -(R_BUF_SIZE-8);
rmdp->mlen = 0;
rmdp->u.s.status = RCV_OWN;
p->rmdnum = (p->rmdnum + 1) & (RMDNUM-1);
rmdp = p->rmdhead + p->rmdnum;
}
}
static int ni65_send_packet(struct sk_buff *skb, struct device *dev)
{
struct priv *p = (struct priv *) dev->priv;
if(dev->tbusy)
{
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 50)
return 1;
printk(KERN_ERR "%s: xmitter timed out, try to restart!\n",dev->name);
{
int i;
for(i=0;i<TMDNUM;i++)
printk("%02x ",p->tmdhead[i].u.s.status);
printk("\n");
}
ni65_lance_reinit(dev);
dev->tbusy=0;
dev->trans_start = jiffies;
}
if(skb == NULL) {
dev_tint(dev);
return 0;
}
if (skb->len <= 0)
return 0;
if (set_bit(0, (void*)&dev->tbusy) != 0) {
printk(KERN_ERR "%s: Transmitter access conflict.\n", dev->name);
return 1;
}
if (set_bit(0, (void*)&p->lock)) {
printk(KERN_ERR "%s: Queue was locked.\n", dev->name);
return 1;
}
{
short len = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
struct tmd *tmdp;
long flags;
#ifdef XMT_VIA_SKB
if( (unsigned long) (skb->data + skb->len) > 0x1000000) {
#endif
memcpy((char *) p->tmdbounce[p->tmdbouncenum] ,(char *)skb->data,
(skb->len > T_BUF_SIZE) ? T_BUF_SIZE : skb->len);
dev_kfree_skb (skb, FREE_WRITE);
save_flags(flags);
cli();
tmdp = p->tmdhead + p->tmdnum;
tmdp->u.buffer = (u32) virt_to_bus(p->tmdbounce[p->tmdbouncenum]);
p->tmdbouncenum = (p->tmdbouncenum + 1) & (TMDNUM - 1);
#ifdef XMT_VIA_SKB
}
else {
save_flags(flags);
cli();
tmdp = p->tmdhead + p->tmdnum;
tmdp->u.buffer = (u32) virt_to_bus(skb->data);
p->tmd_skb[p->tmdnum] = skb;
}
#endif
tmdp->blen = -len;
tmdp->u.s.status = XMIT_OWN | XMIT_START | XMIT_END;
writedatareg(CSR0_TDMD | CSR0_INEA);
p->xmit_queued = 1;
p->tmdnum = (p->tmdnum + 1) & (TMDNUM-1);
dev->tbusy = (p->tmdnum == p->tmdlast) ? 1 : 0;
p->lock = 0;
dev->trans_start = jiffies;
restore_flags(flags);
}
return 0;
}
static struct enet_statistics *ni65_get_stats(struct device *dev)
{
#if 0
int i;
struct priv *p = (struct priv *) dev->priv;
for(i=0;i<RMDNUM;i++) {
struct rmd *rmdp = p->rmdhead + ((p->rmdnum + i) & (RMDNUM-1));
printk("%02x ",rmdp->u.s.status);
}
printk("\n");
#endif
return &((struct priv *) dev->priv)->stats;
}
static void set_multicast_list(struct device *dev)
{
if(!ni65_lance_reinit(dev))
printk(KERN_ERR "%s: Can't switch card into MC mode!\n",dev->name);
dev->tbusy = 0;
}
#ifdef MODULE
static struct device dev_ni65 = {
"        ",
0, 0, 0, 0,
0x360, 9,
0, 0, 0, NULL, ni65_probe };
static int irq=0;
static int io=0;
static int dma=0;
int init_module(void)
{
#if 0
if(io <= 0x0 || irq < 2) {
printk("ni65: Autoprobing not allowed for modules.\n");
printk("ni65: Set symbols 'io' 'irq' and 'dma'\n");
return -ENODEV;
}
#endif
dev_ni65.irq = irq;
dev_ni65.dma = dma;
dev_ni65.base_addr = io;
if (register_netdev(&dev_ni65) != 0)
return -EIO;
return 0;
}
void cleanup_module(void)
{
struct priv *p;
p = (struct priv *) dev_ni65.priv;
if(!p) {
printk("Ooops .. no privat struct\n");
return;
}
disable_dma(dev_ni65.dma);
free_dma(dev_ni65.dma);
release_region(dev_ni65.base_addr,cards[p->cardno].total_size);
ni65_free_buffer(p);
dev_ni65.priv = NULL;
unregister_netdev(&dev_ni65);
}
#endif