static const char *version =
"3c507.c:v1.10 9/23/94 Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/ioport.h>
#include <linux/in.h>
#include <linux/string.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/errno.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/malloc.h>
#ifndef NET_DEBUG
#define NET_DEBUG 1
#endif
static unsigned int net_debug = NET_DEBUG;
static unsigned int netcard_portlist[] =
{ 0x300, 0x320, 0x340, 0x280, 0};
static void init_rx_bufs(struct device *dev);
#define	 CUC_START	 0x0100
#define	 CUC_RESUME	 0x0200
#define	 CUC_SUSPEND 0x0300
#define	 RX_START	 0x0010
#define	 RX_RESUME	 0x0020
#define	 RX_SUSPEND	 0x0030
#define CMD_EOL		0x8000
#define CMD_SUSP	0x4000
#define CMD_INTR	0x2000
enum commands {
CmdNOp = 0, CmdSASetup = 1, CmdConfigure = 2, CmdMulticastList = 3,
CmdTx = 4, CmdTDR = 5, CmdDump = 6, CmdDiagnose = 7};
struct net_local {
struct enet_statistics stats;
int last_restart;
ushort rx_head;
ushort rx_tail;
ushort tx_head;
ushort tx_cmd_link;
ushort tx_reap;
};
#define	SA_DATA		0
#define MISC_CTRL	6
#define RESET_IRQ	10
#define SIGNAL_CA	11
#define ROM_CONFIG	13
#define MEM_CONFIG	14
#define IRQ_CONFIG	15
#define EL16_IO_EXTENT 16
#define ID_PORT		0x100
#define iSCB_STATUS	0x8
#define iSCB_CMD		0xA
#define iSCB_CBL		0xC
#define iSCB_RFA		0xE
#define SCB_BASE		((unsigned)64*1024 - (dev->mem_end - dev->mem_start))
#define CONFIG_CMD	0x0018
#define SET_SA_CMD	0x0024
#define SA_OFFSET	0x002A
#define IDLELOOP	0x30
#define TDR_CMD		0x38
#define TDR_TIME	0x3C
#define DUMP_CMD	0x40
#define DIAG_CMD	0x48
#define SET_MC_CMD	0x4E
#define DUMP_DATA	0x56
#define TX_BUF_START	0x0100
#define NUM_TX_BUFS 	4
#define TX_BUF_SIZE 	(1518+14+20+16)
#define RX_BUF_START	0x2000
#define RX_BUF_SIZE 	(1518+14+18)
#define RX_BUF_END		(dev->mem_end - dev->mem_start)
unsigned short init_words[] = {
0x0000,
0,0,
0x0000,0x0000,
0x0001,
0x0008,0,0,
0,0xf000|RX_START|CUC_START,
CONFIG_CMD,
RX_BUF_START,
0,0,0,0,
0, CmdConfigure,
SET_SA_CMD,
0x0804,
0x2e40,
0,
0, CmdSASetup,
SET_MC_CMD,
0xaa00,0xb000,0x0bad,
0, CmdNOp, IDLELOOP, 0 ,
0, CmdTDR, IDLELOOP, 0,
0, CmdDump, IDLELOOP, DUMP_DATA,
0, CmdDiagnose, IDLELOOP,
0, CmdMulticastList, IDLELOOP, 0,
};
extern int el16_probe(struct device *dev);
static int	el16_probe1(struct device *dev, int ioaddr);
static int	el16_open(struct device *dev);
static int	el16_send_packet(struct sk_buff *skb, struct device *dev);
static void	el16_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void el16_rx(struct device *dev);
static int	el16_close(struct device *dev);
static struct enet_statistics *el16_get_stats(struct device *dev);
static void hardware_send_packet(struct device *dev, void *buf, short length);
void init_82586_mem(struct device *dev);
#ifdef HAVE_DEVLIST
struct netdev_entry netcard_drv =
{"3c507", el16_probe1, EL16_IO_EXTENT, netcard_portlist};
#endif
int
el16_probe(struct device *dev)
{
int base_addr = dev ? dev->base_addr : 0;
int i;
if (base_addr > 0x1ff)
return el16_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; netcard_portlist[i]; i++) {
int ioaddr = netcard_portlist[i];
if (check_region(ioaddr, EL16_IO_EXTENT))
continue;
if (el16_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
int el16_probe1(struct device *dev, int ioaddr)
{
static unsigned char init_ID_done = 0, version_printed = 0;
int i, irq, irqval;
if (init_ID_done == 0) {
ushort lrs_state = 0xff;
outb(0x00, ID_PORT);
for(i = 0; i < 255; i++) {
outb(lrs_state, ID_PORT);
lrs_state <<= 1;
if (lrs_state & 0x100)
lrs_state ^= 0xe7;
}
outb(0x00, ID_PORT);
init_ID_done = 1;
}
if (inb(ioaddr) == '*' && inb(ioaddr+1) == '3'
&& inb(ioaddr+2) == 'C' && inb(ioaddr+3) == 'O')
;
else
return ENODEV;
if (dev == NULL)
dev = init_etherdev(0, sizeof(struct net_local));
if (net_debug  &&  version_printed++ == 0)
printk("%s", version);
printk("%s: 3c507 at %#x,", dev->name, ioaddr);
irq = inb(ioaddr + IRQ_CONFIG) & 0x0f;
irqval = request_irq(irq, &el16_interrupt, 0, "3c507", NULL);
if (irqval) {
printk ("unable to get IRQ %d (irqval=%d).\n", irq, irqval);
return EAGAIN;
}
request_region(ioaddr, EL16_IO_EXTENT, "3c507");
dev->base_addr = ioaddr;
outb(0x01, ioaddr + MISC_CTRL);
for (i = 0; i < 6; i++) {
dev->dev_addr[i] = inb(ioaddr + i);
printk(" %02x", dev->dev_addr[i]);
}
if ((dev->mem_start & 0xf) > 0)
net_debug = dev->mem_start & 7;
#ifdef MEM_BASE
dev->mem_start = MEM_BASE;
dev->mem_end = dev->mem_start + 0x10000;
#else
{
int base;
int size;
char mem_config = inb(ioaddr + MEM_CONFIG);
if (mem_config & 0x20) {
size = 64*1024;
base = 0xf00000 + (mem_config & 0x08 ? 0x080000
: ((mem_config & 3) << 17));
} else {
size = ((mem_config & 3) + 1) << 14;
base = 0x0c0000 + ( (mem_config & 0x18) << 12);
}
dev->mem_start = base;
dev->mem_end = base + size;
}
#endif
dev->if_port = (inb(ioaddr + ROM_CONFIG) & 0x80) ? 1 : 0;
dev->irq = inb(ioaddr + IRQ_CONFIG) & 0x0f;
printk(", IRQ %d, %sternal xcvr, memory %#lx-%#lx.\n", dev->irq,
dev->if_port ? "ex" : "in", dev->mem_start, dev->mem_end-1);
if (net_debug)
printk("%s", version);
dev->priv = kmalloc(sizeof(struct net_local), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct net_local));
dev->open		= el16_open;
dev->stop		= el16_close;
dev->hard_start_xmit = el16_send_packet;
dev->get_stats	= el16_get_stats;
ether_setup(dev);
dev->flags&=~IFF_MULTICAST;
return 0;
}
static int
el16_open(struct device *dev)
{
irq2dev_map[dev->irq] = dev;
init_82586_mem(dev);
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
MOD_INC_USE_COUNT;
return 0;
}
static int
el16_send_packet(struct sk_buff *skb, struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
short *shmem = (short*)dev->mem_start;
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 5)
return 1;
if (net_debug > 1)
printk("%s: transmit timed out, %s?  ", dev->name,
shmem[iSCB_STATUS>>1] & 0x8000 ? "IRQ conflict" :
"network cable problem");
if (lp->last_restart == lp->stats.tx_packets) {
if (net_debug > 1) printk("Resetting board.\n");
init_82586_mem(dev);
} else {
if (net_debug > 1) printk("Kicking board.\n");
shmem[iSCB_CMD>>1] = 0xf000|CUC_START|RX_START;
outb(0, ioaddr + SIGNAL_CA);
lp->last_restart = lp->stats.tx_packets;
}
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
outb(0x80, ioaddr + MISC_CTRL);
hardware_send_packet(dev, buf, length);
dev->trans_start = jiffies;
outb(0x84, ioaddr + MISC_CTRL);
}
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
static void
el16_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
struct net_local *lp;
int ioaddr, status, boguscount = 0;
ushort ack_cmd = 0;
ushort *shmem;
if (dev == NULL) {
printk ("net_interrupt(): irq %d for unknown device.\n", irq);
return;
}
dev->interrupt = 1;
ioaddr = dev->base_addr;
lp = (struct net_local *)dev->priv;
shmem = ((ushort*)dev->mem_start);
status = shmem[iSCB_STATUS>>1];
if (net_debug > 4) {
printk("%s: 3c507 interrupt, status %4.4x.\n", dev->name, status);
}
outb(0x80, ioaddr + MISC_CTRL);
while (lp->tx_reap != lp->tx_head) {
unsigned short tx_status = shmem[lp->tx_reap>>1];
if (tx_status == 0) {
if (net_debug > 5)  printk("Couldn't reap %#x.\n", lp->tx_reap);
break;
}
if (tx_status & 0x2000) {
lp->stats.tx_packets++;
lp->stats.collisions += tx_status & 0xf;
dev->tbusy = 0;
mark_bh(NET_BH);
} else {
lp->stats.tx_errors++;
if (tx_status & 0x0600)  lp->stats.tx_carrier_errors++;
if (tx_status & 0x0100)  lp->stats.tx_fifo_errors++;
if (!(tx_status & 0x0040))  lp->stats.tx_heartbeat_errors++;
if (tx_status & 0x0020)  lp->stats.tx_aborted_errors++;
}
if (net_debug > 5)
printk("Reaped %x, Tx status %04x.\n" , lp->tx_reap, tx_status);
lp->tx_reap += TX_BUF_SIZE;
if (lp->tx_reap > RX_BUF_START - TX_BUF_SIZE)
lp->tx_reap = TX_BUF_START;
if (++boguscount > 4)
break;
}
if (status & 0x4000) {
if (net_debug > 5)
printk("Received packet, rx_head %04x.\n", lp->rx_head);
el16_rx(dev);
}
ack_cmd = status & 0xf000;
if ((status & 0x0700) != 0x0200 && dev->start) {
if (net_debug)
printk("%s: Command unit stopped, status %04x, restarting.\n",
dev->name, status);
ack_cmd |= CUC_RESUME;
}
if ((status & 0x0070) != 0x0040  &&  dev->start) {
if (net_debug)
printk("%s: Rx unit stopped, status %04x, restarting.\n",
dev->name, status);
init_rx_bufs(dev);
shmem[iSCB_RFA >> 1] = RX_BUF_START;
ack_cmd |= RX_START;
}
shmem[iSCB_CMD>>1] = ack_cmd;
outb(0, ioaddr + SIGNAL_CA);
outb(0, ioaddr + RESET_IRQ);
outb(0x84, ioaddr + MISC_CTRL);
return;
}
static int
el16_close(struct device *dev)
{
int ioaddr = dev->base_addr;
ushort *shmem = (short*)dev->mem_start;
dev->tbusy = 1;
dev->start = 0;
shmem[iSCB_CMD >> 1] = RX_SUSPEND | CUC_SUSPEND;
outb(0, ioaddr + SIGNAL_CA);
outb(0x80, ioaddr + MISC_CTRL);
irq2dev_map[dev->irq] = 0;
MOD_DEC_USE_COUNT;
return 0;
}
static struct enet_statistics *
el16_get_stats(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
return &lp->stats;
}
static void
init_rx_bufs(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
unsigned short *write_ptr;
unsigned short SCB_base = SCB_BASE;
int cur_rxbuf = lp->rx_head = RX_BUF_START;
do {
write_ptr = (unsigned short *)(dev->mem_start + cur_rxbuf);
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = cur_rxbuf + RX_BUF_SIZE;
*write_ptr++ = cur_rxbuf + 22;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = -1;
*write_ptr++ = cur_rxbuf + 0x20 + SCB_base;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x8000 + RX_BUF_SIZE-0x20;
lp->rx_tail = cur_rxbuf;
cur_rxbuf += RX_BUF_SIZE;
} while (cur_rxbuf <= RX_BUF_END - RX_BUF_SIZE);
write_ptr = (unsigned short *)
(dev->mem_start + lp->rx_tail + 2);
*write_ptr++ = 0xC000;
*write_ptr++ = lp->rx_head;
}
void
init_82586_mem(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
short ioaddr = dev->base_addr;
ushort *shmem = (short*)dev->mem_start;
outb(0x20, ioaddr + MISC_CTRL);
init_words[3] = SCB_BASE;
init_words[7] = SCB_BASE;
memcpy((void*)dev->mem_end-10, init_words, 10);
memcpy((char*)dev->mem_start, init_words + 5, sizeof(init_words) - 10);
memcpy((char*)dev->mem_start+SA_OFFSET, dev->dev_addr,
sizeof(dev->dev_addr));
lp->tx_cmd_link = IDLELOOP + 4;
lp->tx_head = lp->tx_reap = TX_BUF_START;
init_rx_bufs(dev);
outb(0xA0, ioaddr + MISC_CTRL);
outb(0, ioaddr + SIGNAL_CA);
{
int boguscnt = 50;
while (shmem[iSCB_STATUS>>1] == 0)
if (--boguscnt == 0) {
printk("%s: i82586 initialization timed out with status %04x,"
"cmd %04x.\n", dev->name,
shmem[iSCB_STATUS>>1], shmem[iSCB_CMD>>1]);
break;
}
outb(0, ioaddr + SIGNAL_CA);
}
outb(0x84, ioaddr + MISC_CTRL);
if (net_debug > 4)
printk("%s: Initialized 82586, status %04x.\n", dev->name,
shmem[iSCB_STATUS>>1]);
return;
}
static void
hardware_send_packet(struct device *dev, void *buf, short length)
{
struct net_local *lp = (struct net_local *)dev->priv;
short ioaddr = dev->base_addr;
ushort tx_block = lp->tx_head;
ushort *write_ptr =	  (ushort *)(dev->mem_start + tx_block);
*write_ptr++ = 0x0000;
*write_ptr++ = CMD_INTR|CmdTx;
*write_ptr++ = tx_block+16;
*write_ptr++ = tx_block+8;
*write_ptr++ = length | 0x8000;
*write_ptr++ = -1;
*write_ptr++ = tx_block+22+SCB_BASE;
*write_ptr++ = 0x0000;
*write_ptr++ = 0x0000;
*write_ptr++ = CmdNOp;
*write_ptr++ = tx_block+16;
memcpy(write_ptr, buf, length);
*(ushort*)(dev->mem_start + lp->tx_cmd_link) = tx_block;
lp->tx_cmd_link = tx_block + 20;
lp->tx_head = tx_block + TX_BUF_SIZE;
if (lp->tx_head > RX_BUF_START - TX_BUF_SIZE)
lp->tx_head = TX_BUF_START;
if (net_debug > 4) {
printk("%s: 3c507 @%x send length = %d, tx_block %3x, next %3x.\n",
dev->name, ioaddr, length, tx_block, lp->tx_head);
}
if (lp->tx_head != lp->tx_reap)
dev->tbusy = 0;
}
static void
el16_rx(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
short *shmem = (short*)dev->mem_start;
ushort rx_head = lp->rx_head;
ushort rx_tail = lp->rx_tail;
ushort boguscount = 10;
short frame_status;
while ((frame_status = shmem[rx_head>>1]) < 0) {
ushort *read_frame =  (short *)(dev->mem_start + rx_head);
ushort rfd_cmd = read_frame[1];
ushort next_rx_frame = read_frame[2];
ushort data_buffer_addr = read_frame[3];
ushort *data_frame = (short *)(dev->mem_start + data_buffer_addr);
ushort pkt_len = data_frame[0];
if (rfd_cmd != 0 || data_buffer_addr != rx_head + 22
|| (pkt_len & 0xC000) != 0xC000) {
printk("%s: Rx frame at %#x corrupted, status %04x cmd %04x"
"next %04x data-buf @%04x %04x.\n", dev->name, rx_head,
frame_status, rfd_cmd, next_rx_frame, data_buffer_addr,
pkt_len);
} else if ((frame_status & 0x2000) == 0) {
lp->stats.rx_errors++;
if (frame_status & 0x0800) lp->stats.rx_crc_errors++;
if (frame_status & 0x0400) lp->stats.rx_frame_errors++;
if (frame_status & 0x0200) lp->stats.rx_fifo_errors++;
if (frame_status & 0x0100) lp->stats.rx_over_errors++;
if (frame_status & 0x0080) lp->stats.rx_length_errors++;
} else {
struct sk_buff *skb;
pkt_len &= 0x3fff;
skb = dev_alloc_skb(pkt_len+2);
if (skb == NULL) {
printk("%s: Memory squeeze, dropping packet.\n", dev->name);
lp->stats.rx_dropped++;
break;
}
skb_reserve(skb,2);
skb->dev = dev;
memcpy(skb_put(skb,pkt_len), data_frame + 5, pkt_len);
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
}
read_frame[0] = 0;
read_frame[1] = 0xC000;
*(short*)(dev->mem_start + rx_tail + 2) = 0x0000;
rx_tail = rx_head;
rx_head = next_rx_frame;
if (--boguscount == 0)
break;
}
lp->rx_head = rx_head;
lp->rx_tail = rx_tail;
}
#ifdef MODULE
static char devicename[9] = { 0, };
static struct device dev_3c507 = {
devicename,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, el16_probe
};
static int io = 0x300;
static int irq = 0;
int init_module(void)
{
if (io == 0)
printk("3c507: You should not use auto-probing with insmod!\n");
dev_3c507.base_addr = io;
dev_3c507.irq       = irq;
if (register_netdev(&dev_3c507) != 0) {
printk("3c507: register_netdev() returned non-zero.\n");
return -EIO;
}
return 0;
}
void
cleanup_module(void)
{
unregister_netdev(&dev_3c507);
kfree(dev_3c507.priv);
dev_3c507.priv = NULL;
free_irq(dev_3c507.irq, NULL);
release_region(dev_3c507.base_addr, EL16_IO_EXTENT);
}
#endif