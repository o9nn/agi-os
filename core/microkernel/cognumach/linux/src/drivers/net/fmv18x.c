static const char *version =
"fmv18x.c:v1.3.71e 03/04/96  Yutaka TAMIYA (tamy@flab.fujitsu.co.jp)\n";
#include <linux/module.h>
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
#include <linux/delay.h>
static int fmv18x_probe_list[] =
{0x220, 0x240, 0x260, 0x280, 0x2a0, 0x2c0, 0x300, 0x340, 0};
#ifndef NET_DEBUG
#define NET_DEBUG 1
#endif
static unsigned int net_debug = NET_DEBUG;
typedef unsigned char uchar;
struct net_local {
struct enet_statistics stats;
long open_time;
uint tx_started:1;
uchar tx_queue;
ushort tx_queue_len;
};
#define STATUS 0
#define TX_STATUS 0
#define RX_STATUS 1
#define TX_INTR 2
#define RX_INTR 3
#define TX_MODE 4
#define RX_MODE 5
#define CONFIG_0 6
#define CONFIG_1 7
#define DATAPORT 8
#define TX_START 10
#define COL16CNTL 11
#define MODE13 13
#define FJ_STATUS0 0x10
#define FJ_STATUS1 0x11
#define FJ_CONFIG0 0x12
#define FJ_CONFIG1 0x13
#define FJ_MACADDR 0x14
#define FJ_BUFCNTL 0x1A
#define FJ_BUFDATA 0x1C
#define FMV18X_IO_EXTENT 32
extern int fmv18x_probe(struct device *dev);
static int fmv18x_probe1(struct device *dev, short ioaddr);
static int net_open(struct device *dev);
static int net_send_packet(struct sk_buff *skb, struct device *dev);
static void net_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void net_rx(struct device *dev);
static int net_close(struct device *dev);
static struct enet_statistics *net_get_stats(struct device *dev);
static void set_multicast_list(struct device *dev);
#ifdef HAVE_DEVLIST
struct netdev_entry fmv18x_drv =
{"fmv18x", fmv18x_probe1, FMV18X_IO_EXTENT, fmv18x_probe_list};
#else
int
fmv18x_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return fmv18x_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; fmv18x_probe_list[i]; i++) {
int ioaddr = fmv18x_probe_list[i];
if (check_region(ioaddr, FMV18X_IO_EXTENT))
continue;
if (fmv18x_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
int fmv18x_probe1(struct device *dev, short ioaddr)
{
char irqmap[4] = {3, 7, 10, 15};
unsigned int i, irq;
if (fmv18x_probe_list[inb(ioaddr + FJ_CONFIG0) & 0x07] != ioaddr
|| inb(ioaddr+FJ_MACADDR ) != 0x00
|| inb(ioaddr+FJ_MACADDR+1) != 0x00
|| inb(ioaddr+FJ_MACADDR+2) != 0x0e)
return -ENODEV;
irq = irqmap[(inb(ioaddr + FJ_CONFIG0)>>6) & 0x03];
if (request_irq(irq, &net_interrupt, 0, "fmv18x", NULL)) {
printk ("FMV-18x found at %#3x, but it's unusable due to a conflict on"
"IRQ %d.\n", ioaddr, irq);
return EAGAIN;
}
if (dev == NULL)
dev = init_etherdev(0, sizeof(struct net_local));
request_region(ioaddr, FMV18X_IO_EXTENT, "fmv18x");
printk("%s: FMV-18x found at %#3x, IRQ %d, address ", dev->name,
ioaddr, irq);
dev->base_addr = ioaddr;
dev->irq = irq;
irq2dev_map[irq] = dev;
for(i = 0; i < 6; i++) {
unsigned char val = inb(ioaddr + FJ_MACADDR + i);
printk("%02x", val);
dev->dev_addr[i] = val;
}
{
const char *porttype[] = {"auto-sense", "10baseT", "auto-sense", "10base2/5"};
ushort setup_value = inb(ioaddr + FJ_STATUS0);
switch( setup_value & 0x07 ){
case 0x01 :
case 0x02 : dev->if_port = 0x18; break;
case 0x04 : dev->if_port = 0x08; break;
default : dev->if_port = 0x00; break;
}
printk(" %s interface.\n", porttype[(dev->if_port>>3) & 3]);
}
outb(0xda, ioaddr + CONFIG_0);
outb(0x00, ioaddr + CONFIG_1);
outb(0x00, ioaddr + FJ_CONFIG1);
outb(0x00, ioaddr + FJ_BUFCNTL);
udelay(200);
outb(0x00, ioaddr + CONFIG_1);
for (i = 0; i < 6; i++)
outb(dev->dev_addr[i], ioaddr + 8 + i);
outb(0x04, ioaddr + CONFIG_1);
for (i = 0; i < 8; i++)
outb(0x00, ioaddr + 8 + i);
outb(0x08, ioaddr + CONFIG_1);
outb(dev->if_port, ioaddr + MODE13);
if (net_debug)
printk("%s", version);
dev->priv = kmalloc(sizeof(struct net_local), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct net_local));
dev->open = net_open;
dev->stop = net_close;
dev->hard_start_xmit = net_send_packet;
dev->get_stats = net_get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
return 0;
}
static int net_open(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
outb(0x5a, ioaddr + CONFIG_0);
outb(0xe8, ioaddr + CONFIG_1);
lp->tx_started = 0;
lp->tx_queue = 0;
lp->tx_queue_len = 0;
outb(0xff, ioaddr + TX_STATUS);
outb(0xff, ioaddr + RX_STATUS);
lp->open_time = jiffies;
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
outb(0x80, ioaddr + FJ_CONFIG1);
outw(0x8182, ioaddr+TX_INTR);
MOD_INC_USE_COUNT;
return 0;
}
static int
net_send_packet(struct sk_buff *skb, struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 10)
return 1;
printk("%s: transmit timed out with status %04x, %s?\n", dev->name,
htons(inw(ioaddr + TX_STATUS)),
inb(ioaddr + TX_STATUS) & 0x80
? "IRQ conflict" : "network cable problem");
printk("%s: timeout registers: %04x %04x %04x %04x %04x %04x %04x %04x.\n",
dev->name, htons(inw(ioaddr + 0)),
htons(inw(ioaddr + 2)), htons(inw(ioaddr + 4)),
htons(inw(ioaddr + 6)), htons(inw(ioaddr + 8)),
htons(inw(ioaddr +10)), htons(inw(ioaddr +12)),
htons(inw(ioaddr +14)));
printk("eth card: %04x %04x\n",
htons(inw(ioaddr+FJ_STATUS0)),
htons(inw(ioaddr+FJ_CONFIG0)));
lp->stats.tx_errors++;
cli();
outb(0xda, ioaddr + CONFIG_0);
outb(0x00, ioaddr + CONFIG_1);
outb(0x00, ioaddr + FJ_CONFIG1);
outb(0x00, ioaddr + FJ_BUFCNTL);
net_open(dev);
sti();
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
if (length > ETH_FRAME_LEN) {
if (net_debug)
printk("%s: Attempting to send a large packet (%d bytes).\n",
dev->name, length);
return 1;
}
if (net_debug > 4)
printk("%s: Transmitting a packet of length %lu.\n", dev->name,
(unsigned long)skb->len);
outw(0x0000, ioaddr + TX_INTR);
outw(length, ioaddr + DATAPORT);
outsw(ioaddr + DATAPORT, buf, (length + 1) >> 1);
lp->tx_queue++;
lp->tx_queue_len += length + 2;
if (lp->tx_started == 0) {
outb(0x80 | lp->tx_queue, ioaddr + TX_START);
lp->tx_queue = 0;
lp->tx_queue_len = 0;
dev->trans_start = jiffies;
lp->tx_started = 1;
dev->tbusy = 0;
} else if (lp->tx_queue_len < 4096 - 1502)
dev->tbusy = 0;
outw(0x8182, ioaddr + TX_INTR);
}
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
static void
net_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
struct net_local *lp;
int ioaddr, status;
if (dev == NULL) {
printk ("fmv18x_interrupt(): irq %d for unknown device.\n", irq);
return;
}
dev->interrupt = 1;
ioaddr = dev->base_addr;
lp = (struct net_local *)dev->priv;
outw(0x0000, ioaddr + TX_INTR);
status = inw(ioaddr + TX_STATUS);
outw(status, ioaddr + TX_STATUS);
if (net_debug > 4)
printk("%s: Interrupt with status %04x.\n", dev->name, status);
if (status & 0xff00
|| (inb(ioaddr + RX_MODE) & 0x40) == 0) {
net_rx(dev);
}
if (status & 0x00ff) {
if (status & 0x80) {
lp->stats.tx_packets++;
if (lp->tx_queue) {
outb(0x80 | lp->tx_queue, ioaddr + TX_START);
lp->tx_queue = 0;
lp->tx_queue_len = 0;
dev->trans_start = jiffies;
dev->tbusy = 0;
mark_bh(NET_BH);
} else {
lp->tx_started = 0;
dev->tbusy = 0;
mark_bh(NET_BH);
}
}
if (status & 0x02 ) {
if (net_debug > 4)
printk("%s: 16 Collision occur during Txing.\n", dev->name);
outb(0x02, ioaddr + COL16CNTL);
}
}
dev->interrupt = 0;
outw(0x8182, ioaddr + TX_INTR);
return;
}
static void
net_rx(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
int boguscount = 10;
while ((inb(ioaddr + RX_MODE) & 0x40) == 0) {
ushort status = inw(ioaddr + DATAPORT);
if (net_debug > 4)
printk("%s: Rxing packet mode %02x status %04x.\n",
dev->name, inb(ioaddr + RX_MODE), status);
#ifndef final_version
if (status == 0) {
outb(0x05, ioaddr + 14);
break;
}
#endif
if ((status & 0xF0) != 0x20) {
lp->stats.rx_errors++;
if (status & 0x08) lp->stats.rx_length_errors++;
if (status & 0x04) lp->stats.rx_frame_errors++;
if (status & 0x02) lp->stats.rx_crc_errors++;
if (status & 0x01) lp->stats.rx_over_errors++;
} else {
ushort pkt_len = inw(ioaddr + DATAPORT);
struct sk_buff *skb;
if (pkt_len > 1550) {
printk("%s: The FMV-18x claimed a very large packet, size %d.\n",
dev->name, pkt_len);
outb(0x05, ioaddr + 14);
lp->stats.rx_errors++;
break;
}
skb = dev_alloc_skb(pkt_len+3);
if (skb == NULL) {
printk("%s: Memory squeeze, dropping packet (len %d).\n",
dev->name, pkt_len);
outb(0x05, ioaddr + 14);
lp->stats.rx_dropped++;
break;
}
skb->dev = dev;
skb_reserve(skb,2);
insw(ioaddr + DATAPORT, skb_put(skb,pkt_len), (pkt_len + 1) >> 1);
if (net_debug > 5) {
int i;
printk("%s: Rxed packet of length %d: ", dev->name, pkt_len);
for (i = 0; i < 14; i++)
printk(" %02x", skb->data[i]);
printk(".\n");
}
skb->protocol=eth_type_trans(skb, dev);
netif_rx(skb);
lp->stats.rx_packets++;
}
if (--boguscount <= 0)
break;
}
{
int i;
for (i = 0; i < 20; i++) {
if ((inb(ioaddr + RX_MODE) & 0x40) == 0x40)
break;
(void)inw(ioaddr + DATAPORT);
outb(0x05, ioaddr + 14);
}
if (net_debug > 5 && i > 0)
printk("%s: Exint Rx packet with mode %02x after %d ticks.\n",
dev->name, inb(ioaddr + RX_MODE), i);
}
return;
}
static int net_close(struct device *dev)
{
int ioaddr = dev->base_addr;
((struct net_local *)dev->priv)->open_time = 0;
dev->tbusy = 1;
dev->start = 0;
outb(0xda, ioaddr + CONFIG_0);
outb(0x00, ioaddr + CONFIG_1);
MOD_DEC_USE_COUNT;
outb(0x00, ioaddr + FJ_CONFIG1);
return 0;
}
static struct enet_statistics *
net_get_stats(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
cli();
sti();
return &lp->stats;
}
static void
set_multicast_list(struct device *dev)
{
short ioaddr = dev->base_addr;
if (dev->mc_count || dev->flags&(IFF_PROMISC|IFF_ALLMULTI))
{
dev->flags|=IFF_PROMISC;
outb(3, ioaddr + RX_MODE);
}
else
outb(2, ioaddr + RX_MODE);
}
#ifdef MODULE
static char devicename[9] = { 0, };
static struct device dev_fmv18x = {
devicename,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, fmv18x_probe };
static int io = 0x220;
static int irq = 0;
int init_module(void)
{
if (io == 0)
printk("fmv18x: You should not use auto-probing with insmod!\n");
dev_fmv18x.base_addr = io;
dev_fmv18x.irq = irq;
if (register_netdev(&dev_fmv18x) != 0) {
printk("fmv18x: register_netdev() returned non-zero.\n");
return -EIO;
}
return 0;
}
void
cleanup_module(void)
{
unregister_netdev(&dev_fmv18x);
kfree(dev_fmv18x.priv);
dev_fmv18x.priv = NULL;
free_irq(dev_fmv18x.irq, NULL);
irq2dev_map[dev_fmv18x.irq] = NULL;
release_region(dev_fmv18x.base_addr, FMV18X_IO_EXTENT);
}
#endif