static const char *version =
"8390.c:v1.10 9/23/94 Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/fs.h>
#include <linux/types.h>
#include <linux/ptrace.h>
#include <linux/string.h>
#include <asm/system.h>
#include <asm/segment.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <linux/errno.h>
#include <linux/fcntl.h>
#include <linux/in.h>
#include <linux/interrupt.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include "8390.h"
#define ei_reset_8390 (ei_local->reset_8390)
#define ei_block_output (ei_local->block_output)
#define ei_block_input (ei_local->block_input)
#define ei_get_8390_hdr (ei_local->get_8390_hdr)
#ifdef EI_DEBUG
int ei_debug = EI_DEBUG;
#else
int ei_debug = 1;
#endif
static void ei_tx_intr(struct device *dev);
static void ei_tx_err(struct device *dev);
static void ei_receive(struct device *dev);
static void ei_rx_overrun(struct device *dev);
static void NS8390_trigger_send(struct device *dev, unsigned int length,
int start_page);
static void set_multicast_list(struct device *dev);
int ei_open(struct device *dev)
{
struct ei_device *ei_local = (struct ei_device *) dev->priv;
if (ei_local == NULL) {
printk(KERN_EMERG "%s: ei_open passed a non-existent device!\n", dev->name);
return -ENXIO;
}
irq2dev_map[dev->irq] = dev;
NS8390_init(dev, 1);
dev->start = 1;
ei_local->irqlock = 0;
return 0;
}
int ei_close(struct device *dev)
{
NS8390_init(dev, 0);
dev->start = 0;
return 0;
}
static int ei_start_xmit(struct sk_buff *skb, struct device *dev)
{
int e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
int length, send_length, output_page;
if (dev->tbusy) {
int txsr = inb(e8390_base+EN0_TSR), isr;
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < TX_TIMEOUT || (tickssofar < (TX_TIMEOUT+5) && ! (txsr & ENTSR_PTX))) {
return 1;
}
isr = inb(e8390_base+EN0_ISR);
if (dev->start == 0) {
printk("%s: xmit on stopped card\n", dev->name);
return 1;
}
printk(KERN_DEBUG "%s: Tx timed out, %s TSR=%#2x, ISR=%#2x, t=%d.\n",
dev->name, (txsr & ENTSR_ABT) ? "excess collisions." :
(isr) ? "lost interrupt?" : "cable problem?", txsr, isr, tickssofar);
if (!isr && !ei_local->stat.tx_packets) {
ei_local->interface_num ^= 1;
}
ei_reset_8390(dev);
NS8390_init(dev, 1);
dev->trans_start = jiffies;
}
if (skb == NULL) {
dev_tint(dev);
return 0;
}
length = skb->len;
if (skb->len <= 0)
return 0;
outb_p(0x00, e8390_base + EN0_IMR);
if (dev->interrupt) {
printk("%s: Tx request while isr active.\n",dev->name);
outb_p(ENISR_ALL, e8390_base + EN0_IMR);
return 1;
}
ei_local->irqlock = 1;
send_length = ETH_ZLEN < length ? length : ETH_ZLEN;
#ifdef EI_PINGPONG
if (ei_local->tx1 == 0) {
output_page = ei_local->tx_start_page;
ei_local->tx1 = send_length;
if (ei_debug && ei_local->tx2 > 0)
printk("%s: idle transmitter tx2=%d, lasttx=%d, txing=%d.\n",
dev->name, ei_local->tx2, ei_local->lasttx, ei_local->txing);
} else if (ei_local->tx2 == 0) {
output_page = ei_local->tx_start_page + TX_1X_PAGES;
ei_local->tx2 = send_length;
if (ei_debug && ei_local->tx1 > 0)
printk("%s: idle transmitter, tx1=%d, lasttx=%d, txing=%d.\n",
dev->name, ei_local->tx1, ei_local->lasttx, ei_local->txing);
} else {
if (ei_debug)
printk("%s: No Tx buffers free! irq=%d tx1=%d tx2=%d last=%d\n",
dev->name, dev->interrupt, ei_local->tx1, ei_local->tx2, ei_local->lasttx);
ei_local->irqlock = 0;
dev->tbusy = 1;
outb_p(ENISR_ALL, e8390_base + EN0_IMR);
return 1;
}
ei_block_output(dev, length, skb->data, output_page);
if (! ei_local->txing) {
ei_local->txing = 1;
NS8390_trigger_send(dev, send_length, output_page);
dev->trans_start = jiffies;
if (output_page == ei_local->tx_start_page) {
ei_local->tx1 = -1;
ei_local->lasttx = -1;
} else {
ei_local->tx2 = -1;
ei_local->lasttx = -2;
}
} else
ei_local->txqueue++;
dev->tbusy = (ei_local->tx1 && ei_local->tx2);
#else
ei_block_output(dev, length, skb->data, ei_local->tx_start_page);
ei_local->txing = 1;
NS8390_trigger_send(dev, send_length, ei_local->tx_start_page);
dev->trans_start = jiffies;
dev->tbusy = 1;
#endif
ei_local->irqlock = 0;
outb_p(ENISR_ALL, e8390_base + EN0_IMR);
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
void ei_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
int e8390_base;
int interrupts, nr_serviced = 0;
struct ei_device *ei_local;
if (dev == NULL) {
printk ("net_interrupt(): irq %d for unknown device.\n", irq);
return;
}
e8390_base = dev->base_addr;
ei_local = (struct ei_device *) dev->priv;
if (dev->interrupt || ei_local->irqlock) {
printk(ei_local->irqlock
? "%s: Interrupted while interrupts are masked! isr=%#2x imr=%#2x.\n"
: "%s: Reentering the interrupt handler! isr=%#2x imr=%#2x.\n",
dev->name, inb_p(e8390_base + EN0_ISR),
inb_p(e8390_base + EN0_IMR));
return;
}
dev->interrupt = 1;
outb_p(E8390_NODMA+E8390_PAGE0, e8390_base + E8390_CMD);
if (ei_debug > 3)
printk("%s: interrupt(isr=%#2.2x).\n", dev->name,
inb_p(e8390_base + EN0_ISR));
while ((interrupts = inb_p(e8390_base + EN0_ISR)) != 0
&& ++nr_serviced < MAX_SERVICE) {
if (dev->start == 0) {
printk("%s: interrupt from stopped card\n", dev->name);
interrupts = 0;
break;
}
if (interrupts & ENISR_OVER) {
ei_rx_overrun(dev);
} else if (interrupts & (ENISR_RX+ENISR_RX_ERR)) {
ei_receive(dev);
}
if (interrupts & ENISR_TX) {
ei_tx_intr(dev);
} else if (interrupts & ENISR_TX_ERR) {
ei_tx_err(dev);
}
if (interrupts & ENISR_COUNTERS) {
ei_local->stat.rx_frame_errors += inb_p(e8390_base + EN0_COUNTER0);
ei_local->stat.rx_crc_errors += inb_p(e8390_base + EN0_COUNTER1);
ei_local->stat.rx_missed_errors+= inb_p(e8390_base + EN0_COUNTER2);
outb_p(ENISR_COUNTERS, e8390_base + EN0_ISR);
}
if (interrupts & ENISR_RDC) {
outb_p(ENISR_RDC, e8390_base + EN0_ISR);
}
outb_p(E8390_NODMA+E8390_PAGE0+E8390_START, e8390_base + E8390_CMD);
}
if (interrupts && ei_debug) {
outb_p(E8390_NODMA+E8390_PAGE0+E8390_START, e8390_base + E8390_CMD);
if (nr_serviced >= MAX_SERVICE) {
printk("%s: Too much work at interrupt, status %#2.2x\n",
dev->name, interrupts);
outb_p(ENISR_ALL, e8390_base + EN0_ISR);
} else {
printk("%s: unknown interrupt %#2x\n", dev->name, interrupts);
outb_p(0xff, e8390_base + EN0_ISR);
}
}
dev->interrupt = 0;
return;
}
static void ei_tx_err(struct device *dev)
{
int e8390_base = dev->base_addr;
unsigned char txsr = inb_p(e8390_base+EN0_TSR);
unsigned char tx_was_aborted = txsr & (ENTSR_ABT+ENTSR_FU);
struct ei_device *ei_local = (struct ei_device *) dev->priv;
#ifdef VERBOSE_ERROR_DUMP
printk(KERN_DEBUG "%s: transmitter error (%#2x): ", dev->name, txsr);
if (txsr & ENTSR_ABT)
printk("excess-collisions ");
if (txsr & ENTSR_ND)
printk("non-deferral ");
if (txsr & ENTSR_CRS)
printk("lost-carrier ");
if (txsr & ENTSR_FU)
printk("FIFO-underrun ");
if (txsr & ENTSR_CDH)
printk("lost-heartbeat ");
printk("\n");
#endif
outb_p(ENISR_TX_ERR, e8390_base + EN0_ISR);
if (tx_was_aborted)
ei_tx_intr(dev);
if (txsr & ENTSR_ABT)
ei_local->stat.collisions += 16;
}
static void ei_tx_intr(struct device *dev)
{
int e8390_base = dev->base_addr;
int status = inb(e8390_base + EN0_TSR);
struct ei_device *ei_local = (struct ei_device *) dev->priv;
outb_p(ENISR_TX, e8390_base + EN0_ISR);
#ifdef EI_PINGPONG
ei_local->txqueue--;
if (ei_local->tx1 < 0) {
if (ei_local->lasttx != 1 && ei_local->lasttx != -1)
printk("%s: bogus last_tx_buffer %d, tx1=%d.\n",
ei_local->name, ei_local->lasttx, ei_local->tx1);
ei_local->tx1 = 0;
dev->tbusy = 0;
if (ei_local->tx2 > 0) {
ei_local->txing = 1;
NS8390_trigger_send(dev, ei_local->tx2, ei_local->tx_start_page + 6);
dev->trans_start = jiffies;
ei_local->tx2 = -1,
ei_local->lasttx = 2;
} else
ei_local->lasttx = 20, ei_local->txing = 0;
} else if (ei_local->tx2 < 0) {
if (ei_local->lasttx != 2 && ei_local->lasttx != -2)
printk("%s: bogus last_tx_buffer %d, tx2=%d.\n",
ei_local->name, ei_local->lasttx, ei_local->tx2);
ei_local->tx2 = 0;
dev->tbusy = 0;
if (ei_local->tx1 > 0) {
ei_local->txing = 1;
NS8390_trigger_send(dev, ei_local->tx1, ei_local->tx_start_page);
dev->trans_start = jiffies;
ei_local->tx1 = -1;
ei_local->lasttx = 1;
} else
ei_local->lasttx = 10, ei_local->txing = 0;
} else
printk("%s: unexpected TX-done interrupt, lasttx=%d.\n",
dev->name, ei_local->lasttx);
#else
ei_local->txing = 0;
dev->tbusy = 0;
#endif
if (status & ENTSR_COL)
ei_local->stat.collisions++;
if (status & ENTSR_PTX)
ei_local->stat.tx_packets++;
else {
ei_local->stat.tx_errors++;
if (status & ENTSR_ABT) ei_local->stat.tx_aborted_errors++;
if (status & ENTSR_CRS) ei_local->stat.tx_carrier_errors++;
if (status & ENTSR_FU) ei_local->stat.tx_fifo_errors++;
if (status & ENTSR_CDH) ei_local->stat.tx_heartbeat_errors++;
if (status & ENTSR_OWC) ei_local->stat.tx_window_errors++;
}
mark_bh (NET_BH);
}
static void ei_receive(struct device *dev)
{
int e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
unsigned char rxing_page, this_frame, next_frame;
unsigned short current_offset;
int rx_pkt_count = 0;
struct e8390_pkt_hdr rx_frame;
int num_rx_pages = ei_local->stop_page-ei_local->rx_start_page;
while (++rx_pkt_count < 10) {
int pkt_len;
outb_p(E8390_NODMA+E8390_PAGE1, e8390_base + E8390_CMD);
rxing_page = inb_p(e8390_base + EN1_CURPAG);
outb_p(E8390_NODMA+E8390_PAGE0, e8390_base + E8390_CMD);
this_frame = inb_p(e8390_base + EN0_BOUNDARY) + 1;
if (this_frame >= ei_local->stop_page)
this_frame = ei_local->rx_start_page;
if (ei_debug > 0 && this_frame != ei_local->current_page)
printk("%s: mismatched read page pointers %2x vs %2x.\n",
dev->name, this_frame, ei_local->current_page);
if (this_frame == rxing_page)
break;
current_offset = this_frame << 8;
ei_get_8390_hdr(dev, &rx_frame, this_frame);
pkt_len = rx_frame.count - sizeof(struct e8390_pkt_hdr);
next_frame = this_frame + 1 + ((pkt_len+4)>>8);
if (rx_frame.next != next_frame
&& rx_frame.next != next_frame + 1
&& rx_frame.next != next_frame - num_rx_pages
&& rx_frame.next != next_frame + 1 - num_rx_pages) {
ei_local->current_page = rxing_page;
outb(ei_local->current_page-1, e8390_base+EN0_BOUNDARY);
ei_local->stat.rx_errors++;
continue;
}
if (pkt_len < 60 || pkt_len > 1518) {
if (ei_debug)
printk("%s: bogus packet size: %d, status=%#2x nxpg=%#2x.\n",
dev->name, rx_frame.count, rx_frame.status,
rx_frame.next);
ei_local->stat.rx_errors++;
} else if ((rx_frame.status & 0x0F) == ENRSR_RXOK) {
struct sk_buff *skb;
skb = dev_alloc_skb(pkt_len+2);
if (skb == NULL) {
if (ei_debug > 1)
printk("%s: Couldn't allocate a sk_buff of size %d.\n",
dev->name, pkt_len);
ei_local->stat.rx_dropped++;
break;
} else {
skb_reserve(skb,2);
skb->dev = dev;
skb_put(skb, pkt_len);
ei_block_input(dev, pkt_len, skb, current_offset + sizeof(rx_frame));
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
ei_local->stat.rx_packets++;
}
} else {
int errs = rx_frame.status;
if (ei_debug)
printk("%s: bogus packet: status=%#2x nxpg=%#2x size=%d\n",
dev->name, rx_frame.status, rx_frame.next,
rx_frame.count);
if (errs & ENRSR_FO)
ei_local->stat.rx_fifo_errors++;
}
next_frame = rx_frame.next;
if (next_frame >= ei_local->stop_page) {
printk("%s: next frame inconsistency, %#2x\n", dev->name,
next_frame);
next_frame = ei_local->rx_start_page;
}
ei_local->current_page = next_frame;
outb_p(next_frame-1, e8390_base+EN0_BOUNDARY);
}
outb_p(ENISR_RX+ENISR_RX_ERR, e8390_base+EN0_ISR);
return;
}
static void ei_rx_overrun(struct device *dev)
{
int e8390_base = dev->base_addr;
unsigned long wait_start_time;
unsigned char was_txing, must_resend = 0;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
was_txing = inb_p(e8390_base+E8390_CMD) & E8390_TRANS;
outb_p(E8390_NODMA+E8390_PAGE0+E8390_STOP, e8390_base+E8390_CMD);
if (ei_debug > 1)
printk("%s: Receiver overrun.\n", dev->name);
ei_local->stat.rx_over_errors++;
wait_start_time = jiffies;
while (jiffies - wait_start_time <= 1*HZ/100)
barrier();
outb_p(0x00, e8390_base+EN0_RCNTLO);
outb_p(0x00, e8390_base+EN0_RCNTHI);
if (was_txing) {
unsigned char tx_completed = inb_p(e8390_base+EN0_ISR) & (ENISR_TX+ENISR_TX_ERR);
if (!tx_completed) must_resend = 1;
}
outb_p(E8390_TXOFF, e8390_base + EN0_TXCR);
outb_p(E8390_NODMA + E8390_PAGE0 + E8390_START, e8390_base + E8390_CMD);
ei_receive(dev);
outb_p(ENISR_OVER, e8390_base+EN0_ISR);
outb_p(E8390_TXCONFIG, e8390_base + EN0_TXCR);
if (must_resend)
outb_p(E8390_NODMA + E8390_PAGE0 + E8390_START + E8390_TRANS, e8390_base + E8390_CMD);
}
static struct enet_statistics *get_stats(struct device *dev)
{
short ioaddr = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
if (dev->start == 0) return &ei_local->stat;
ei_local->stat.rx_frame_errors += inb_p(ioaddr + EN0_COUNTER0);
ei_local->stat.rx_crc_errors += inb_p(ioaddr + EN0_COUNTER1);
ei_local->stat.rx_missed_errors+= inb_p(ioaddr + EN0_COUNTER2);
return &ei_local->stat;
}
static void set_multicast_list(struct device *dev)
{
short ioaddr = dev->base_addr;
if(dev->flags&IFF_PROMISC)
{
outb_p(E8390_RXCONFIG | 0x18, ioaddr + EN0_RXCR);
}
else if((dev->flags&IFF_ALLMULTI)||dev->mc_list)
{
outb_p(E8390_RXCONFIG | 0x08, ioaddr + EN0_RXCR);
}
else
outb_p(E8390_RXCONFIG, ioaddr + EN0_RXCR);
}
int ethdev_init(struct device *dev)
{
if (ei_debug > 1)
printk("%s", version);
if (dev->priv == NULL) {
dev->priv = kmalloc(sizeof(struct ei_device), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct ei_device));
}
dev->hard_start_xmit = &ei_start_xmit;
dev->get_stats = get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
return 0;
}
void NS8390_init(struct device *dev, int startp)
{
int e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
int i;
int endcfg = ei_local->word16 ? (0x48 | ENDCFG_WTS) : 0x48;
unsigned long flags;
outb_p(E8390_NODMA+E8390_PAGE0+E8390_STOP, e8390_base);
outb_p(endcfg, e8390_base + EN0_DCFG);
outb_p(0x00, e8390_base + EN0_RCNTLO);
outb_p(0x00, e8390_base + EN0_RCNTHI);
outb_p(E8390_RXOFF, e8390_base + EN0_RXCR);
outb_p(E8390_TXOFF, e8390_base + EN0_TXCR);
outb_p(ei_local->tx_start_page, e8390_base + EN0_TPSR);
ei_local->tx1 = ei_local->tx2 = 0;
outb_p(ei_local->rx_start_page, e8390_base + EN0_STARTPG);
outb_p(ei_local->stop_page-1, e8390_base + EN0_BOUNDARY);
ei_local->current_page = ei_local->rx_start_page;
outb_p(ei_local->stop_page, e8390_base + EN0_STOPPG);
outb_p(0xFF, e8390_base + EN0_ISR);
outb_p(0x00, e8390_base + EN0_IMR);
save_flags(flags);
cli();
outb_p(E8390_NODMA + E8390_PAGE1 + E8390_STOP, e8390_base);
for(i = 0; i < 6; i++) {
outb_p(dev->dev_addr[i], e8390_base + EN1_PHYS + i);
}
for(i = 0; i < 8; i++)
outb_p(0xff, e8390_base + EN1_MULT + i);
outb_p(ei_local->rx_start_page, e8390_base + EN1_CURPAG);
outb_p(E8390_NODMA+E8390_PAGE0+E8390_STOP, e8390_base);
restore_flags(flags);
dev->tbusy = 0;
dev->interrupt = 0;
ei_local->tx1 = ei_local->tx2 = 0;
ei_local->txing = 0;
if (startp) {
outb_p(0xff, e8390_base + EN0_ISR);
outb_p(ENISR_ALL, e8390_base + EN0_IMR);
outb_p(E8390_NODMA+E8390_PAGE0+E8390_START, e8390_base);
outb_p(E8390_TXCONFIG, e8390_base + EN0_TXCR);
outb_p(E8390_RXCONFIG, e8390_base + EN0_RXCR);
dev->set_multicast_list(dev);
}
return;
}
static void NS8390_trigger_send(struct device *dev, unsigned int length,
int start_page)
{
int e8390_base = dev->base_addr;
outb_p(E8390_NODMA+E8390_PAGE0, e8390_base);
if (inb_p(e8390_base) & E8390_TRANS) {
printk("%s: trigger_send() called with the transmitter busy.\n",
dev->name);
return;
}
outb_p(length & 0xff, e8390_base + EN0_TCNTLO);
outb_p(length >> 8, e8390_base + EN0_TCNTHI);
outb_p(start_page, e8390_base + EN0_TPSR);
outb_p(E8390_NODMA+E8390_TRANS+E8390_START, e8390_base);
return;
}
#ifdef MODULE
int init_module(void)
{
return 0;
}
void
cleanup_module(void)
{
}
#endif