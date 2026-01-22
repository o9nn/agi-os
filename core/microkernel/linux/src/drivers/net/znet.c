static const char *version = "znet.c:v1.02 9/23/94 becker@cesdis.gsfc.nasa.gov\n";
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/ptrace.h>
#include <linux/errno.h>
#include <linux/interrupt.h>
#include <linux/ioport.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/if_arp.h>
#ifndef ZNET_DEBUG
#define ZNET_DEBUG 1
#endif
static unsigned int znet_debug = ZNET_DEBUG;
#define DMA_RX_MODE 0x14
#define DMA_TX_MODE 0x18
#define dma_page_eq(ptr1, ptr2) ((long)(ptr1)>>17 == (long)(ptr2)>>17)
#define DMA_BUF_SIZE 8192
#define RX_BUF_SIZE 8192
#define TX_BUF_SIZE 8192
#define CMD0_CHNL_0 0x00
#define CMD0_CHNL_1 0x10
#define CMD0_NOP (CMD0_CHNL_0)
#define CMD0_PORT_1 CMD0_CHNL_1
#define CMD1_PORT_0 1
#define CMD0_IA_SETUP 1
#define CMD0_CONFIGURE 2
#define CMD0_MULTICAST_LIST 3
#define CMD0_TRANSMIT 4
#define CMD0_DUMP 6
#define CMD0_DIAGNOSE 7
#define CMD0_Rx_ENABLE 8
#define CMD0_Rx_DISABLE 10
#define CMD0_Rx_STOP 11
#define CMD0_RETRANSMIT 12
#define CMD0_ABORT 13
#define CMD0_RESET 14
#define CMD0_ACK 0x80
#define CMD0_STAT0 (0 << 5)
#define CMD0_STAT1 (1 << 5)
#define CMD0_STAT2 (2 << 5)
#define CMD0_STAT3 (3 << 5)
#define net_local znet_private
struct znet_private {
int rx_dma, tx_dma;
struct enet_statistics stats;
ushort *rx_start, *rx_cur, *rx_end;
ushort *tx_start, *tx_cur, *tx_end;
ushort tx_buf_len;
};
static struct znet_private zn;
static ushort dma_buffer1[DMA_BUF_SIZE/2];
static ushort dma_buffer2[DMA_BUF_SIZE/2];
static ushort dma_buffer3[DMA_BUF_SIZE/2 + 8];
static unsigned char i593_init[] = {
0xAA,
0x88,
0x2E,
0x00,
0x60,
0x00,
0xF2,
0x00,
0x00,
0x40,
0x5F,
0x00,
0x3F,
0x07,
0x31,
0x22,
};
struct netidblk {
char magic[8];
unsigned char netid[8];
char nettype, globalopt;
char vendor[8];
char product[8];
char irq1, irq2;
char dma1, dma2;
short dma_mem_misc[8];
short iobase1, iosize1;
short iobase2, iosize2;
char driver_options;
char pad;
};
int znet_probe(struct device *dev);
static int znet_open(struct device *dev);
static int znet_send_packet(struct sk_buff *skb, struct device *dev);
static void znet_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void znet_rx(struct device *dev);
static int znet_close(struct device *dev);
static struct enet_statistics *net_get_stats(struct device *dev);
static void set_multicast_list(struct device *dev);
static void hardware_init(struct device *dev);
static void update_stop_hit(short ioaddr, unsigned short rx_stop_offset);
#ifdef notdef
static struct sigaction znet_sigaction = { &znet_interrupt, 0, 0, NULL, };
#endif
int znet_probe(struct device *dev)
{
int i;
struct netidblk *netinfo;
char *p;
for(p = (char *)0xf0000; p < (char *)0x100000; p++)
if (*p == 'N' && strncmp(p, "NETIDBLK", 8) == 0)
break;
if (p >= (char *)0x100000) {
if (znet_debug > 1)
printk(KERN_INFO "No Z-Note ethernet adaptor found.\n");
return ENODEV;
}
netinfo = (struct netidblk *)p;
dev->base_addr = netinfo->iobase1;
dev->irq = netinfo->irq1;
printk(KERN_INFO "%s: ZNET at %#3lx,", dev->name, dev->base_addr);
for (i = 0; i < 6; i++)
printk(" %2.2x", dev->dev_addr[i] = netinfo->netid[i]);
printk(", using IRQ %d DMA %d and %d.\n", dev->irq, netinfo->dma1,
netinfo->dma2);
if (znet_debug > 1) {
printk(KERN_INFO "%s: vendor '%16.16s' IRQ1 %d IRQ2 %d DMA1 %d DMA2 %d.\n",
dev->name, netinfo->vendor,
netinfo->irq1, netinfo->irq2,
netinfo->dma1, netinfo->dma2);
printk(KERN_INFO "%s: iobase1 %#x size %d iobase2 %#x size %d net type %2.2x.\n",
dev->name, netinfo->iobase1, netinfo->iosize1,
netinfo->iobase2, netinfo->iosize2, netinfo->nettype);
}
if (znet_debug > 0)
printk("%s%s", KERN_INFO, version);
dev->priv = (void *) &zn;
zn.rx_dma = netinfo->dma1;
zn.tx_dma = netinfo->dma2;
if (request_irq(dev->irq, &znet_interrupt, 0, "ZNet", NULL)
|| request_dma(zn.rx_dma,"ZNet rx")
|| request_dma(zn.tx_dma,"ZNet tx")) {
printk(KERN_WARNING "%s: Not opened -- resource busy?!?\n", dev->name);
return EBUSY;
}
irq2dev_map[dev->irq] = dev;
if (dma_page_eq(dma_buffer1, &dma_buffer1[RX_BUF_SIZE/2-1]))
zn.rx_start = dma_buffer1;
else
zn.rx_start = dma_buffer2;
if (dma_page_eq(dma_buffer3, &dma_buffer3[RX_BUF_SIZE/2-1]))
zn.tx_start = dma_buffer3;
else
zn.tx_start = dma_buffer2;
zn.rx_end = zn.rx_start + RX_BUF_SIZE/2;
zn.tx_buf_len = TX_BUF_SIZE/2;
zn.tx_end = zn.tx_start + zn.tx_buf_len;
dev->open = &znet_open;
dev->hard_start_xmit = &znet_send_packet;
dev->stop = &znet_close;
dev->get_stats = net_get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
return 0;
}
static int znet_open(struct device *dev)
{
int ioaddr = dev->base_addr;
if (znet_debug > 2)
printk(KERN_DEBUG "%s: znet_open() called.\n", dev->name);
outb(0x10, 0xe6);
outb(inb(0xe7) | 0x84, 0xe7);
if (inb(ioaddr) != 0x10 && inb(ioaddr) != 0x00)
printk(KERN_WARNING "%s: Problem turning on the transceiver power.\n",
dev->name);
dev->tbusy = 0;
dev->interrupt = 0;
hardware_init(dev);
dev->start = 1;
return 0;
}
static int znet_send_packet(struct sk_buff *skb, struct device *dev)
{
int ioaddr = dev->base_addr;
if (znet_debug > 4)
printk(KERN_DEBUG "%s: ZNet_send_packet(%ld).\n", dev->name, dev->tbusy);
if (dev->tbusy) {
ushort event, tx_status, rx_offset, state;
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 10)
return 1;
outb(CMD0_STAT0, ioaddr); event = inb(ioaddr);
outb(CMD0_STAT1, ioaddr); tx_status = inw(ioaddr);
outb(CMD0_STAT2, ioaddr); rx_offset = inw(ioaddr);
outb(CMD0_STAT3, ioaddr); state = inb(ioaddr);
printk(KERN_WARNING "%s: transmit timed out, status %02x %04x %04x %02x,"
" resetting.\n", dev->name, event, tx_status, rx_offset, state);
if (tx_status == 0x0400)
printk(KERN_WARNING "%s: Tx carrier error, check transceiver cable.\n",
dev->name);
outb(CMD0_RESET, ioaddr);
hardware_init(dev);
}
if (skb == NULL) {
dev_tint(dev);
return 0;
}
outb(CMD0_STAT0, ioaddr);
if (inw(ioaddr) == 0x0010
&& inw(ioaddr) == 0x0000
&& inw(ioaddr) == 0x0010)
hardware_init(dev);
if (set_bit(0, (void*)&dev->tbusy) != 0)
printk(KERN_WARNING "%s: Transmitter access conflict.\n", dev->name);
else {
short length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
unsigned char *buf = (void *)skb->data;
ushort *tx_link = zn.tx_cur - 1;
ushort rnd_len = (length + 1)>>1;
{
short dma_port = ((zn.tx_dma&3)<<2) + IO_DMA2_BASE;
unsigned addr = inb(dma_port);
addr |= inb(dma_port) << 8;
addr <<= 1;
if (((int)zn.tx_cur & 0x1ffff) != addr)
printk(KERN_WARNING "Address mismatch at Tx: %#x vs %#x.\n",
(int)zn.tx_cur & 0xffff, addr);
zn.tx_cur = (ushort *)(((int)zn.tx_cur & 0xfe0000) | addr);
}
if (zn.tx_cur >= zn.tx_end)
zn.tx_cur = zn.tx_start;
*zn.tx_cur++ = length;
if (zn.tx_cur + rnd_len + 1 > zn.tx_end) {
int semi_cnt = (zn.tx_end - zn.tx_cur)<<1;
memcpy(zn.tx_cur, buf, semi_cnt);
rnd_len -= semi_cnt>>1;
memcpy(zn.tx_start, buf + semi_cnt, length - semi_cnt);
zn.tx_cur = zn.tx_start + rnd_len;
} else {
memcpy(zn.tx_cur, buf, skb->len);
zn.tx_cur += rnd_len;
}
*zn.tx_cur++ = 0;
cli(); {
*tx_link = CMD0_TRANSMIT + CMD0_CHNL_1;
outb(CMD0_TRANSMIT + CMD0_CHNL_1,ioaddr);
} sti();
dev->trans_start = jiffies;
if (znet_debug > 4)
printk(KERN_DEBUG "%s: Transmitter queued, length %d.\n", dev->name, length);
}
dev_kfree_skb(skb, FREE_WRITE);
return 0;
}
static void znet_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
struct device *dev = irq2dev_map[irq];
int ioaddr;
int boguscnt = 20;
if (dev == NULL) {
printk(KERN_WARNING "znet_interrupt(): IRQ %d for unknown device.\n", irq);
return;
}
dev->interrupt = 1;
ioaddr = dev->base_addr;
outb(CMD0_STAT0, ioaddr);
do {
ushort status = inb(ioaddr);
if (znet_debug > 5) {
ushort result, rx_ptr, running;
outb(CMD0_STAT1, ioaddr);
result = inw(ioaddr);
outb(CMD0_STAT2, ioaddr);
rx_ptr = inw(ioaddr);
outb(CMD0_STAT3, ioaddr);
running = inb(ioaddr);
printk(KERN_DEBUG "%s: interrupt, status %02x, %04x %04x %02x serial %d.\n",
dev->name, status, result, rx_ptr, running, boguscnt);
}
if ((status & 0x80) == 0)
break;
if ((status & 0x0F) == 4) {
struct net_local *lp = (struct net_local *)dev->priv;
int tx_status;
outb(CMD0_STAT1, ioaddr);
tx_status = inw(ioaddr);
if (tx_status & 0x2000) {
lp->stats.tx_packets++;
lp->stats.collisions += tx_status & 0xf;
} else {
if (tx_status & 0x0600) lp->stats.tx_carrier_errors++;
if (tx_status & 0x0100) lp->stats.tx_fifo_errors++;
if (!(tx_status & 0x0040)) lp->stats.tx_heartbeat_errors++;
if (tx_status & 0x0020) lp->stats.tx_aborted_errors++;
if ((tx_status | 0x0760) != 0x0760)
lp->stats.tx_errors++;
}
dev->tbusy = 0;
mark_bh(NET_BH);
}
if ((status & 0x40)
|| (status & 0x0f) == 11) {
znet_rx(dev);
}
outb(CMD0_ACK,ioaddr);
} while (boguscnt--);
dev->interrupt = 0;
return;
}
static void znet_rx(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
int boguscount = 1;
short next_frame_end_offset = 0;
short *cur_frame_end;
short cur_frame_end_offset;
outb(CMD0_STAT2, ioaddr);
cur_frame_end_offset = inw(ioaddr);
if (cur_frame_end_offset == zn.rx_cur - zn.rx_start) {
printk(KERN_WARNING "%s: Interrupted, but nothing to receive, offset %03x.\n",
dev->name, cur_frame_end_offset);
return;
}
while (zn.rx_start + cur_frame_end_offset != zn.rx_cur
&& ++boguscount < 5) {
unsigned short hi_cnt, lo_cnt, hi_status, lo_status;
int count, status;
if (cur_frame_end_offset < 4) {
memcpy(zn.rx_end, zn.rx_start, 8);
cur_frame_end_offset += (RX_BUF_SIZE/2);
}
cur_frame_end = zn.rx_start + cur_frame_end_offset - 4;
lo_status = *cur_frame_end++;
hi_status = *cur_frame_end++;
status = ((hi_status & 0xff) << 8) + (lo_status & 0xff);
lo_cnt = *cur_frame_end++;
hi_cnt = *cur_frame_end++;
count = ((hi_cnt & 0xff) << 8) + (lo_cnt & 0xff);
if (znet_debug > 5)
printk(KERN_DEBUG "Constructing trailer at location %03x, %04x %04x %04x %04x"
" count %#x status %04x.\n",
cur_frame_end_offset<<1, lo_status, hi_status, lo_cnt, hi_cnt,
count, status);
cur_frame_end[-4] = status;
cur_frame_end[-3] = next_frame_end_offset;
cur_frame_end[-2] = count;
next_frame_end_offset = cur_frame_end_offset;
cur_frame_end_offset -= ((count + 1)>>1) + 3;
if (cur_frame_end_offset < 0)
cur_frame_end_offset += RX_BUF_SIZE/2;
};
do {
ushort *this_rfp_ptr = zn.rx_start + next_frame_end_offset;
int status = this_rfp_ptr[-4];
int pkt_len = this_rfp_ptr[-2];
if (znet_debug > 5)
printk(KERN_DEBUG "Looking at trailer ending at %04x status %04x length %03x"
" next %04x.\n", next_frame_end_offset<<1, status, pkt_len,
this_rfp_ptr[-3]<<1);
if ( ! (status & 0x2000)) {
lp->stats.rx_errors++;
if (status & 0x0800) lp->stats.rx_crc_errors++;
if (status & 0x0400) lp->stats.rx_frame_errors++;
if (status & 0x0200) lp->stats.rx_over_errors++;
if (status & 0x0100) lp->stats.rx_fifo_errors++;
if (status & 0x0080) lp->stats.rx_length_errors++;
} else if (pkt_len > 1536) {
lp->stats.rx_length_errors++;
} else {
struct sk_buff *skb;
skb = dev_alloc_skb(pkt_len);
if (skb == NULL) {
if (znet_debug)
printk(KERN_WARNING "%s: Memory squeeze, dropping packet.\n", dev->name);
lp->stats.rx_dropped++;
break;
}
skb->dev = dev;
if (&zn.rx_cur[(pkt_len+1)>>1] > zn.rx_end) {
int semi_cnt = (zn.rx_end - zn.rx_cur)<<1;
memcpy(skb_put(skb,semi_cnt), zn.rx_cur, semi_cnt);
memcpy(skb_put(skb,pkt_len-semi_cnt), zn.rx_start,
pkt_len - semi_cnt);
} else {
memcpy(skb_put(skb,pkt_len), zn.rx_cur, pkt_len);
if (znet_debug > 6) {
unsigned int *packet = (unsigned int *) skb->data;
printk(KERN_DEBUG "Packet data is %08x %08x %08x %08x.\n", packet[0],
packet[1], packet[2], packet[3]);
}
}
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
}
zn.rx_cur = this_rfp_ptr;
if (zn.rx_cur >= zn.rx_end)
zn.rx_cur -= RX_BUF_SIZE/2;
update_stop_hit(ioaddr, (zn.rx_cur - zn.rx_start)<<1);
next_frame_end_offset = this_rfp_ptr[-3];
if (next_frame_end_offset == 0)
break;
this_rfp_ptr = zn.rx_start + next_frame_end_offset;
} while (--boguscount);
return;
}
static int znet_close(struct device *dev)
{
int ioaddr = dev->base_addr;
dev->tbusy = 1;
dev->start = 0;
outb(CMD0_RESET, ioaddr);
disable_dma(zn.rx_dma);
disable_dma(zn.tx_dma);
free_irq(dev->irq, NULL);
if (znet_debug > 1)
printk(KERN_DEBUG "%s: Shutting down ethercard.\n", dev->name);
outb(0x10, 0xe6);
outb(inb(0xe7) & ~0x84, 0xe7);
return 0;
}
static struct enet_statistics *net_get_stats(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
return &lp->stats;
}
static void set_multicast_list(struct device *dev)
{
short ioaddr = dev->base_addr;
if (dev->flags&IFF_PROMISC) {
i593_init[7] &= ~3; i593_init[7] |= 1;
i593_init[13] &= ~8; i593_init[13] |= 8;
} else if (dev->mc_list || (dev->flags&IFF_ALLMULTI)) {
i593_init[7] &= ~3; i593_init[7] |= 0;
i593_init[13] &= ~8; i593_init[13] |= 8;
} else {
i593_init[7] &= ~3; i593_init[7] |= 0;
i593_init[13] &= ~8; i593_init[13] |= 0;
}
*zn.tx_cur++ = sizeof(i593_init);
memcpy(zn.tx_cur, i593_init, sizeof(i593_init));
zn.tx_cur += sizeof(i593_init)/2;
outb(CMD0_CONFIGURE+CMD0_CHNL_1, ioaddr);
#ifdef not_tested
if (num_addrs > 0) {
int addrs_len = 6*num_addrs;
*zn.tx_cur++ = addrs_len;
memcpy(zn.tx_cur, addrs, addrs_len);
outb(CMD0_MULTICAST_LIST+CMD0_CHNL_1, ioaddr);
zn.tx_cur += addrs_len>>1;
}
#endif
}
void show_dma(void)
{
short dma_port = ((zn.tx_dma&3)<<2) + IO_DMA2_BASE;
unsigned addr = inb(dma_port);
addr |= inb(dma_port) << 8;
printk("Addr: %04x cnt:%3x...", addr<<1, get_dma_residue(zn.tx_dma));
}
static void hardware_init(struct device *dev)
{
short ioaddr = dev->base_addr;
zn.rx_cur = zn.rx_start;
zn.tx_cur = zn.tx_start;
outb(CMD0_RESET, ioaddr);
cli(); {
disable_dma(zn.rx_dma);
clear_dma_ff(zn.rx_dma);
set_dma_mode(zn.rx_dma, DMA_RX_MODE);
set_dma_addr(zn.rx_dma, (unsigned int) zn.rx_start);
set_dma_count(zn.rx_dma, RX_BUF_SIZE);
enable_dma(zn.rx_dma);
disable_dma(zn.tx_dma);
clear_dma_ff(zn.tx_dma);
set_dma_mode(zn.tx_dma, DMA_TX_MODE);
set_dma_addr(zn.tx_dma, (unsigned int) zn.tx_start);
set_dma_count(zn.tx_dma, zn.tx_buf_len<<1);
enable_dma(zn.tx_dma);
} sti();
if (znet_debug > 1)
printk(KERN_DEBUG "%s: Initializing the i82593, tx buf %p... ", dev->name,
zn.tx_start);
*zn.tx_cur++ = 0;
*zn.tx_cur++ = 0;
printk("stat:%02x ", inb(ioaddr)); show_dma();
outb(CMD0_CONFIGURE+CMD0_CHNL_1, ioaddr);
*zn.tx_cur++ = sizeof(i593_init);
memcpy(zn.tx_cur, i593_init, sizeof(i593_init));
zn.tx_cur += sizeof(i593_init)/2;
printk("stat:%02x ", inb(ioaddr)); show_dma();
outb(CMD0_CONFIGURE+CMD0_CHNL_1, ioaddr);
*zn.tx_cur++ = 6;
memcpy(zn.tx_cur, dev->dev_addr, 6);
zn.tx_cur += 3;
printk("stat:%02x ", inb(ioaddr)); show_dma();
outb(CMD0_IA_SETUP + CMD0_CHNL_1, ioaddr);
printk("stat:%02x ", inb(ioaddr)); show_dma();
update_stop_hit(ioaddr, 8192);
if (znet_debug > 1) printk("enabling Rx.\n");
outb(CMD0_Rx_ENABLE+CMD0_CHNL_0, ioaddr);
dev->tbusy = 0;
}
static void update_stop_hit(short ioaddr, unsigned short rx_stop_offset)
{
outb(CMD0_PORT_1, ioaddr);
if (znet_debug > 5)
printk(KERN_DEBUG "Updating stop hit with value %02x.\n",
(rx_stop_offset >> 6) | 0x80);
outb((rx_stop_offset >> 6) | 0x80, ioaddr);
outb(CMD1_PORT_0, ioaddr);
}