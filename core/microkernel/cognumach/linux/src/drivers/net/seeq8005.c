static const char *version =
"seeq8005.c:v1.00 8/07/95 Hamish Coleman (hamish@zot.apana.org.au)\n";
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/ioport.h>
#include <linux/in.h>
#include <linux/malloc.h>
#include <linux/string.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/errno.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include "seeq8005.h"
static unsigned int seeq8005_portlist[] =
{ 0x300, 0x320, 0x340, 0x360, 0};
#ifndef NET_DEBUG
#define NET_DEBUG 1
#endif
static unsigned int net_debug = NET_DEBUG;
struct net_local {
struct enet_statistics stats;
unsigned short receive_ptr;
long open_time;
};
#define SA_ADDR0 0x00
#define SA_ADDR1 0x80
#define SA_ADDR2 0x4b
extern int seeq8005_probe(struct device *dev);
static int seeq8005_probe1(struct device *dev, int ioaddr);
static int seeq8005_open(struct device *dev);
static int seeq8005_send_packet(struct sk_buff *skb, struct device *dev);
static void seeq8005_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void seeq8005_rx(struct device *dev);
static int seeq8005_close(struct device *dev);
static struct enet_statistics *seeq8005_get_stats(struct device *dev);
static void set_multicast_list(struct device *dev);
#define tx_done(dev)	(inw(SEEQ_STATUS) & SEEQSTAT_TX_ON)
extern void hardware_send_packet(struct device *dev, char *buf, int length);
extern void seeq8005_init(struct device *dev, int startp);
inline void wait_for_buffer(struct device *dev);
#ifdef HAVE_DEVLIST
struct netdev_entry seeq8005_drv =
{"seeq8005", seeq8005_probe1, SEEQ8005_IO_EXTENT, seeq8005_portlist};
#else
int
seeq8005_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return seeq8005_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; seeq8005_portlist[i]; i++) {
int ioaddr = seeq8005_portlist[i];
if (check_region(ioaddr, SEEQ8005_IO_EXTENT))
continue;
if (seeq8005_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
static int seeq8005_probe1(struct device *dev, int ioaddr)
{
static unsigned version_printed = 0;
int i,j;
unsigned char SA_prom[32];
int old_cfg1;
int old_cfg2;
int old_stat;
int old_dmaar;
int old_rear;
if (net_debug>1)
printk("seeq8005: probing at 0x%x\n",ioaddr);
old_stat = inw(SEEQ_STATUS);
if (old_stat == 0xffff)
return ENODEV;
if ( (old_stat & 0x1800) != 0x1800 ) {
if (net_debug>1) {
printk("seeq8005: reserved stat bits != 0x1800\n");
printk("          == 0x%04x\n",old_stat);
}
return ENODEV;
}
old_rear = inw(SEEQ_REA);
if (old_rear == 0xffff) {
outw(0,SEEQ_REA);
if (inw(SEEQ_REA) == 0xffff) {
return ENODEV;
}
} else if ((old_rear & 0xff00) != 0xff00) {
if (net_debug>1) {
printk("seeq8005: unused rear bits != 0xff00\n");
printk("          == 0x%04x\n",old_rear);
}
return ENODEV;
}
old_cfg2 = inw(SEEQ_CFG2);
old_cfg1 = inw(SEEQ_CFG1);
old_dmaar = inw(SEEQ_DMAAR);
if (net_debug>4) {
printk("seeq8005: stat = 0x%04x\n",old_stat);
printk("seeq8005: cfg1 = 0x%04x\n",old_cfg1);
printk("seeq8005: cfg2 = 0x%04x\n",old_cfg2);
printk("seeq8005: raer = 0x%04x\n",old_rear);
printk("seeq8005: dmaar= 0x%04x\n",old_dmaar);
}
outw( SEEQCMD_FIFO_WRITE | SEEQCMD_SET_ALL_OFF, SEEQ_CMD);
outw( 0, SEEQ_DMAAR);
outw( SEEQCFG1_BUFFER_PROM, SEEQ_CFG1);
j=0;
for(i=0; i <32; i++) {
j+= SA_prom[i] = inw(SEEQ_BUFFER) & 0xff;
}
#if 0
if ( (j&0xff) != 0 ) {
if (net_debug>1) {
printk("seeq8005: prom sum error\n");
}
outw( old_stat, SEEQ_STATUS);
outw( old_dmaar, SEEQ_DMAAR);
outw( old_cfg1, SEEQ_CFG1);
return ENODEV;
}
#endif
outw( SEEQCFG2_RESET, SEEQ_CFG2);
SLOW_DOWN_IO;
SLOW_DOWN_IO;
SLOW_DOWN_IO;
SLOW_DOWN_IO;
outw( SEEQCMD_SET_ALL_OFF, SEEQ_CMD);
if (net_debug) {
printk("seeq8005: prom sum = 0x%08x\n",j);
for(j=0; j<32; j+=16) {
printk("seeq8005: prom %02x: ",j);
for(i=0;i<16;i++) {
printk("%02x ",SA_prom[j|i]);
}
printk(" ");
for(i=0;i<16;i++) {
if ((SA_prom[j|i]>31)&&(SA_prom[j|i]<127)) {
printk("%c", SA_prom[j|i]);
} else {
printk(" ");
}
}
printk("\n");
}
}
#if 0
if (net_debug>1) {
printk("seeq8005: testing packet buffer ... ");
outw( SEEQCFG1_BUFFER_BUFFER, SEEQ_CFG1);
outw( SEEQCMD_FIFO_WRITE | SEEQCMD_SET_ALL_OFF, SEEQ_CMD);
outw( 0 , SEEQ_DMAAR);
for(i=0;i<32768;i++) {
outw(0x5a5a, SEEQ_BUFFER);
}
j=jiffies+HZ;
while ( ((inw(SEEQ_STATUS) & SEEQSTAT_FIFO_EMPTY) != SEEQSTAT_FIFO_EMPTY) && jiffies < j )
mb();
outw( 0 , SEEQ_DMAAR);
while ( ((inw(SEEQ_STATUS) & SEEQSTAT_WINDOW_INT) != SEEQSTAT_WINDOW_INT) && jiffies < j+HZ)
mb();
if ( (inw(SEEQ_STATUS) & SEEQSTAT_WINDOW_INT) == SEEQSTAT_WINDOW_INT)
outw( SEEQCMD_WINDOW_INT_ACK | (inw(SEEQ_STATUS)& SEEQCMD_INT_MASK), SEEQ_CMD);
outw( SEEQCMD_FIFO_READ | SEEQCMD_SET_ALL_OFF, SEEQ_CMD);
j=0;
for(i=0;i<32768;i++) {
if (inw(SEEQ_BUFFER) != 0x5a5a)
j++;
}
if (j) {
printk("%i\n",j);
} else {
printk("ok.\n");
}
}
#endif
if (dev == NULL)
dev = init_etherdev(0, sizeof(struct net_local));
if (net_debug  &&  version_printed++ == 0)
printk("%s", version);
printk("%s: %s found at %#3x, ", dev->name, "seeq8005", ioaddr);
dev->base_addr = ioaddr;
for (i = 0; i < 6; i++)
printk(" %2.2x", dev->dev_addr[i] = SA_prom[i+6]);
if (dev->irq == 0xff)
;
else if (dev->irq < 2) {
autoirq_setup(0);
outw( SEEQCMD_RX_INT_EN | SEEQCMD_SET_RX_ON | SEEQCMD_SET_RX_OFF, SEEQ_CMD );
dev->irq = autoirq_report(0);
if (net_debug >= 2)
printk(" autoirq is %d\n", dev->irq);
} else if (dev->irq == 2)
dev->irq = 9;
#if 0
{
int irqval = request_irq(dev->irq, &seeq8005_interrupt, 0, "seeq8005", NULL);
if (irqval) {
printk ("%s: unable to get IRQ %d (irqval=%d).\n", dev->name,
dev->irq, irqval);
return EAGAIN;
}
}
#endif
request_region(ioaddr, SEEQ8005_IO_EXTENT,"seeq8005");
dev->priv = kmalloc(sizeof(struct net_local), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct net_local));
dev->open		= seeq8005_open;
dev->stop		= seeq8005_close;
dev->hard_start_xmit = seeq8005_send_packet;
dev->get_stats	= seeq8005_get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
dev->flags &= ~IFF_MULTICAST;
return 0;
}
static int
seeq8005_open(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
{
int irqval = request_irq(dev->irq, &seeq8005_interrupt, 0, "seeq8005", NULL);
if (irqval) {
printk ("%s: unable to get IRQ %d (irqval=%d).\n", dev->name,
dev->irq, irqval);
return EAGAIN;
}
}
irq2dev_map[dev->irq] = dev;
seeq8005_init(dev, 1);
lp->open_time = jiffies;
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
return 0;
}
static int
seeq8005_send_packet(struct sk_buff *skb, struct device *dev)
{
int ioaddr = dev->base_addr;
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 5)
return 1;
printk("%s: transmit timed out, %s?\n", dev->name,
tx_done(dev) ? "IRQ conflict" : "network cable problem");
seeq8005_init(dev, 1);
dev->tbusy=0;
dev->trans_start = jiffies;
}
if (skb == NULL) {
dev_tint(dev);
return 0;
}
if (set_bit(0, (void*)&dev->tbusy) != 0)
printk("%s: Transmitter access conflict.\n", dev->name);
else {
short length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
unsigned char *buf = skb->data;
hardware_send_packet(dev, buf, length);
dev->trans_start = jiffies;
}
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
static void
seeq8005_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
struct net_local *lp;
int ioaddr, status, boguscount = 0;
if (dev == NULL) {
printk ("net_interrupt(): irq %d for unknown device.\n", irq);
return;
}
if (dev->interrupt)
printk ("%s: Re-entering the interrupt handler.\n", dev->name);
dev->interrupt = 1;
ioaddr = dev->base_addr;
lp = (struct net_local *)dev->priv;
status = inw(SEEQ_STATUS);
do {
if (net_debug >2) {
printk("%s: int, status=0x%04x\n",dev->name,status);
}
if (status & SEEQSTAT_WINDOW_INT) {
outw( SEEQCMD_WINDOW_INT_ACK | (status & SEEQCMD_INT_MASK), SEEQ_CMD);
if (net_debug) {
printk("%s: window int!\n",dev->name);
}
}
if (status & SEEQSTAT_TX_INT) {
outw( SEEQCMD_TX_INT_ACK | (status & SEEQCMD_INT_MASK), SEEQ_CMD);
lp->stats.tx_packets++;
dev->tbusy = 0;
mark_bh(NET_BH);
}
if (status & SEEQSTAT_RX_INT) {
seeq8005_rx(dev);
}
status = inw(SEEQ_STATUS);
} while ( (++boguscount < 10) && (status & SEEQSTAT_ANY_INT)) ;
if(net_debug>2) {
printk("%s: eoi\n",dev->name);
}
dev->interrupt = 0;
return;
}
static void
seeq8005_rx(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int boguscount = 10;
int pkt_hdr;
int ioaddr = dev->base_addr;
do {
int next_packet;
int pkt_len;
int i;
int status;
status = inw(SEEQ_STATUS);
outw( lp->receive_ptr, SEEQ_DMAAR);
outw(SEEQCMD_FIFO_READ | SEEQCMD_RX_INT_ACK | (status & SEEQCMD_INT_MASK), SEEQ_CMD);
wait_for_buffer(dev);
next_packet = ntohs(inw(SEEQ_BUFFER));
pkt_hdr = inw(SEEQ_BUFFER);
if (net_debug>2) {
printk("%s: 0x%04x recv next=0x%04x, hdr=0x%04x\n",dev->name,lp->receive_ptr,next_packet,pkt_hdr);
}
if ((next_packet == 0) || ((pkt_hdr & SEEQPKTH_CHAIN)==0)) {
return;
}
if ((pkt_hdr & SEEQPKTS_DONE)==0)
break;
if (next_packet < lp->receive_ptr) {
pkt_len = (next_packet + 0x10000 - ((DEFAULT_TEA+1)<<8)) - lp->receive_ptr - 4;
} else {
pkt_len = next_packet - lp->receive_ptr - 4;
}
if (next_packet < ((DEFAULT_TEA+1)<<8)) {
printk("%s: recv packet ring corrupt, resetting board\n",dev->name);
seeq8005_init(dev,1);
return;
}
lp->receive_ptr = next_packet;
if (net_debug>2) {
printk("%s: recv len=0x%04x\n",dev->name,pkt_len);
}
if (pkt_hdr & SEEQPKTS_ANY_ERROR) {
lp->stats.rx_errors++;
if (pkt_hdr & SEEQPKTS_SHORT) lp->stats.rx_frame_errors++;
if (pkt_hdr & SEEQPKTS_DRIB) lp->stats.rx_frame_errors++;
if (pkt_hdr & SEEQPKTS_OVERSIZE) lp->stats.rx_over_errors++;
if (pkt_hdr & SEEQPKTS_CRC_ERR) lp->stats.rx_crc_errors++;
outw( SEEQCMD_FIFO_WRITE | SEEQCMD_DMA_INT_ACK | (status & SEEQCMD_INT_MASK), SEEQ_CMD);
outw( (lp->receive_ptr & 0xff00)>>8, SEEQ_REA);
} else {
struct sk_buff *skb;
unsigned char *buf;
skb = dev_alloc_skb(pkt_len);
if (skb == NULL) {
printk("%s: Memory squeeze, dropping packet.\n", dev->name);
lp->stats.rx_dropped++;
break;
}
skb->dev = dev;
skb_reserve(skb, 2);
buf = skb_put(skb,pkt_len);
insw(SEEQ_BUFFER, buf, (pkt_len + 1) >> 1);
if (net_debug>2) {
char * p = buf;
printk("%s: recv ",dev->name);
for(i=0;i<14;i++) {
printk("%02x ",*(p++)&0xff);
}
printk("\n");
}
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
}
} while ((--boguscount) && (pkt_hdr & SEEQPKTH_CHAIN));
return;
}
static int
seeq8005_close(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
lp->open_time = 0;
dev->tbusy = 1;
dev->start = 0;
outw( SEEQCMD_SET_ALL_OFF, SEEQ_CMD);
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = 0;
return 0;
}
static struct enet_statistics *
seeq8005_get_stats(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
return &lp->stats;
}
static void
set_multicast_list(struct device *dev)
{
#if 0
int ioaddr = dev->base_addr;
if (num_addrs) {
outw( (inw(SEEQ_CFG1) & ~SEEQCFG1_MATCH_MASK)| SEEQCFG1_MATCH_ALL,  SEEQ_CFG1);
dev->flags|=IFF_PROMISC;
} else {
outw( (inw(SEEQ_CFG1) & ~SEEQCFG1_MATCH_MASK)| SEEQCFG1_MATCH_BROAD, SEEQ_CFG1);
}
#endif
}
void seeq8005_init(struct device *dev, int startp)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
int i;
outw(SEEQCFG2_RESET, SEEQ_CFG2);
SLOW_DOWN_IO;
SLOW_DOWN_IO;
SLOW_DOWN_IO;
SLOW_DOWN_IO;
outw( SEEQCMD_FIFO_WRITE | SEEQCMD_SET_ALL_OFF, SEEQ_CMD);
outw( 0, SEEQ_DMAAR);
outw( SEEQCFG1_BUFFER_MAC0, SEEQ_CFG1);
for(i=0;i<6;i++) {
outb(dev->dev_addr[i], SEEQ_BUFFER);
SLOW_DOWN_IO;
}
outw( SEEQCFG1_BUFFER_TEA, SEEQ_CFG1);
outb( DEFAULT_TEA, SEEQ_BUFFER);
lp->receive_ptr = (DEFAULT_TEA+1)<<8;
outw( lp->receive_ptr, SEEQ_RPR);
outw( 0x00ff, SEEQ_REA);
if (net_debug>4) {
printk("%s: SA0 = ",dev->name);
outw( SEEQCMD_FIFO_READ | SEEQCMD_SET_ALL_OFF, SEEQ_CMD);
outw( 0, SEEQ_DMAAR);
outw( SEEQCFG1_BUFFER_MAC0, SEEQ_CFG1);
for(i=0;i<6;i++) {
printk("%02x ",inb(SEEQ_BUFFER));
}
printk("\n");
}
outw( SEEQCFG1_MAC0_EN | SEEQCFG1_MATCH_BROAD | SEEQCFG1_BUFFER_BUFFER, SEEQ_CFG1);
outw( SEEQCFG2_AUTO_REA | SEEQCFG2_CTRLO, SEEQ_CFG2);
outw( SEEQCMD_SET_RX_ON | SEEQCMD_TX_INT_EN | SEEQCMD_RX_INT_EN, SEEQ_CMD);
if (net_debug>4) {
int old_cfg1;
old_cfg1 = inw(SEEQ_CFG1);
printk("%s: stat = 0x%04x\n",dev->name,inw(SEEQ_STATUS));
printk("%s: cfg1 = 0x%04x\n",dev->name,old_cfg1);
printk("%s: cfg2 = 0x%04x\n",dev->name,inw(SEEQ_CFG2));
printk("%s: raer = 0x%04x\n",dev->name,inw(SEEQ_REA));
printk("%s: dmaar= 0x%04x\n",dev->name,inw(SEEQ_DMAAR));
}
}
void hardware_send_packet(struct device * dev, char *buf, int length)
{
int ioaddr = dev->base_addr;
int status = inw(SEEQ_STATUS);
int transmit_ptr = 0;
int tmp;
if (net_debug>4) {
printk("%s: send 0x%04x\n",dev->name,length);
}
outw( SEEQCMD_FIFO_WRITE | (status & SEEQCMD_INT_MASK), SEEQ_CMD);
outw( transmit_ptr, SEEQ_DMAAR);
outw( htons(length + 4), SEEQ_BUFFER);
outw( SEEQPKTH_XMIT | SEEQPKTH_DATA_FOLLOWS | SEEQPKTH_XMIT_INT_EN, SEEQ_BUFFER );
outsw( SEEQ_BUFFER, buf, (length +1) >> 1);
outw( 0, SEEQ_BUFFER);
outw( 0, SEEQ_BUFFER);
outw( transmit_ptr, SEEQ_TPR);
tmp = jiffies;
while ( (((status=inw(SEEQ_STATUS)) & SEEQSTAT_FIFO_EMPTY) == 0) && (jiffies < tmp + HZ))
mb();
outw( SEEQCMD_WINDOW_INT_ACK | SEEQCMD_SET_TX_ON | (status & SEEQCMD_INT_MASK), SEEQ_CMD);
}
inline void wait_for_buffer(struct device * dev)
{
int ioaddr = dev->base_addr;
int tmp;
int status;
tmp = jiffies + HZ;
while ( ( ((status=inw(SEEQ_STATUS)) & SEEQSTAT_WINDOW_INT) != SEEQSTAT_WINDOW_INT) && jiffies < tmp)
mb();
if ( (status & SEEQSTAT_WINDOW_INT) == SEEQSTAT_WINDOW_INT)
outw( SEEQCMD_WINDOW_INT_ACK | (status & SEEQCMD_INT_MASK), SEEQ_CMD);
}