static char *version = "3c509.c:1.16 2/3/98 becker@cesdis.gsfc.nasa.gov\n";
#define TX_TIMEOUT  (400*HZ/1000)
static int max_interrupt_work = 10;
#include <linux/module.h>
#include <linux/config.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/errno.h>
#include <linux/in.h>
#include <linux/malloc.h>
#include <linux/ioport.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/config.h>
#include <linux/delay.h>
#include <asm/bitops.h>
#include <asm/io.h>
#ifdef EL3_DEBUG
int el3_debug = EL3_DEBUG;
#else
int el3_debug = 2;
#endif
#define EL3_DATA 0x00
#define EL3_CMD 0x0e
#define EL3_STATUS 0x0e
#define	 EEPROM_READ 0x80
#define EL3_IO_EXTENT	16
#define EL3WINDOW(win_num) outw(SelectWindow + (win_num), ioaddr + EL3_CMD)
enum c509cmd {
TotalReset = 0<<11, SelectWindow = 1<<11, StartCoax = 2<<11,
RxDisable = 3<<11, RxEnable = 4<<11, RxReset = 5<<11, RxDiscard = 8<<11,
TxEnable = 9<<11, TxDisable = 10<<11, TxReset = 11<<11,
FakeIntr = 12<<11, AckIntr = 13<<11, SetIntrEnb = 14<<11,
SetStatusEnb = 15<<11, SetRxFilter = 16<<11, SetRxThreshold = 17<<11,
SetTxThreshold = 18<<11, SetTxStart = 19<<11, StatsEnable = 21<<11,
StatsDisable = 22<<11, StopCoax = 23<<11,};
enum c509status {
IntLatch = 0x0001, AdapterFailure = 0x0002, TxComplete = 0x0004,
TxAvailable = 0x0008, RxComplete = 0x0010, RxEarly = 0x0020,
IntReq = 0x0040, StatsFull = 0x0080, CmdBusy = 0x1000, };
enum RxFilter {
RxStation = 1, RxMulticast = 2, RxBroadcast = 4, RxProm = 8 };
#define TX_FIFO		0x00
#define RX_FIFO		0x00
#define RX_STATUS 	0x08
#define TX_STATUS 	0x0B
#define TX_FREE		0x0C
#define WN0_IRQ		0x08
#define WN4_MEDIA	0x0A
#define  MEDIA_TP	0x00C0
#define SKB_QUEUE_SIZE	64
struct el3_private {
struct enet_statistics stats;
struct device *next_dev;
int head, size;
struct sk_buff *queue[SKB_QUEUE_SIZE];
};
static int id_port = 0x110;
static struct device *el3_root_dev = NULL;
static ushort id_read_eeprom(int index);
static ushort read_eeprom(int ioaddr, int index);
static int el3_open(struct device *dev);
static int el3_start_xmit(struct sk_buff *skb, struct device *dev);
static void el3_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void update_stats(int addr, struct device *dev);
static struct enet_statistics *el3_get_stats(struct device *dev);
static int el3_rx(struct device *dev);
static int el3_close(struct device *dev);
static void set_multicast_list(struct device *dev);
int el3_probe(struct device *dev)
{
short lrs_state = 0xff, i;
int ioaddr, irq, if_port;
u16 phys_addr[3];
static int current_tag = 0;
if (EISA_bus) {
static int eisa_addr = 0x1000;
while (eisa_addr < 0x9000) {
ioaddr = eisa_addr;
eisa_addr += 0x1000;
if (inw(ioaddr + 0xC80) != 0x6d50)
continue;
outw(SelectWindow | 0, ioaddr + 0xC80 + EL3_CMD);
irq = inw(ioaddr + WN0_IRQ) >> 12;
if_port = inw(ioaddr + 6)>>14;
for (i = 0; i < 3; i++)
phys_addr[i] = htons(read_eeprom(ioaddr, i));
read_eeprom(ioaddr, 3);
goto found;
}
}
#ifdef CONFIG_MCA
if (MCA_bus) {
mca_adaptor_select_mode(1);
for (i = 0; i < 8; i++)
if ((mca_adaptor_id(i) | 1) == 0x627c) {
ioaddr = mca_pos_base_addr(i);
irq = inw(ioaddr + WN0_IRQ) >> 12;
if_port = inw(ioaddr + 6)>>14;
for (i = 0; i < 3; i++)
phys_addr[i] = htons(read_eeprom(ioaddr, i));
mca_adaptor_select_mode(0);
goto found;
}
mca_adaptor_select_mode(0);
}
#endif
outb(0x02, 0x279);
outb(0x02, 0xA79);
for ( ; id_port < 0x200; id_port += 0x10) {
if (check_region(id_port, 1))
continue;
outb(0x00, id_port);
outb(0xff, id_port);
if (inb(id_port) & 0x01)
break;
}
if (id_port >= 0x200) {
printk(" WARNING: No I/O port available for 3c509 activation.\n");
return -ENODEV;
}
outb(0x00, id_port);
outb(0x00, id_port);
for(i = 0; i < 255; i++) {
outb(lrs_state, id_port);
lrs_state <<= 1;
lrs_state = lrs_state & 0x100 ? lrs_state ^ 0xcf : lrs_state;
}
if (current_tag == 0)
outb(0xd0, id_port);
else
outb(0xd8, id_port);
if (id_read_eeprom(7) != 0x6d50) {
return -ENODEV;
}
for (i = 0; i < 3; i++) {
phys_addr[i] = htons(id_read_eeprom(i));
}
{
unsigned int iobase = id_read_eeprom(8);
if_port = iobase >> 14;
ioaddr = 0x200 + ((iobase & 0x1f) << 4);
}
irq = id_read_eeprom(9) >> 12;
if (dev) {
if (dev->irq > 1  &&  dev->irq < 16)
irq = dev->irq;
if (dev->base_addr) {
if (dev->mem_end == 0x3c509
&& dev->base_addr >= 0x200  &&  dev->base_addr <= 0x3e0)
ioaddr = dev->base_addr & 0x3f0;
else if (dev->base_addr != ioaddr)
return -ENODEV;
}
}
outb(0xd0 + ++current_tag, id_port);
outb((ioaddr >> 4) | 0xe0, id_port);
EL3WINDOW(0);
if (inw(ioaddr) != 0x6d50)
return -ENODEV;
outw(0x0f00, ioaddr + WN0_IRQ);
found:
if (dev == NULL) {
dev = init_etherdev(dev, sizeof(struct el3_private));
}
memcpy(dev->dev_addr, phys_addr, sizeof(phys_addr));
dev->base_addr = ioaddr;
dev->irq = irq;
dev->if_port = (dev->mem_start & 0x1f) ? dev->mem_start & 3 : if_port;
request_region(dev->base_addr, EL3_IO_EXTENT, "3c509");
{
const char *if_names[] = {"10baseT", "AUI", "undefined", "BNC"};
printk("%s: 3c509 at %#3.3lx tag %d, %s port, address ",
dev->name, dev->base_addr, current_tag, if_names[dev->if_port]);
}
for (i = 0; i < 6; i++)
printk(" %2.2x", dev->dev_addr[i]);
printk(", IRQ %d.\n", dev->irq);
if (dev->priv == NULL)
dev->priv = kmalloc(sizeof(struct el3_private), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct el3_private));
((struct el3_private *)dev->priv)->next_dev = el3_root_dev;
el3_root_dev = dev;
if (el3_debug > 0)
printk("%s", version);
dev->open = &el3_open;
dev->hard_start_xmit = &el3_start_xmit;
dev->stop = &el3_close;
dev->get_stats = &el3_get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
return 0;
}
static ushort read_eeprom(int ioaddr, int index)
{
outw(EEPROM_READ + index, ioaddr + 10);
udelay (500);
return inw(ioaddr + 12);
}
static ushort id_read_eeprom(int index)
{
int bit, word = 0;
outb(EEPROM_READ + index, id_port);
udelay (500);
for (bit = 15; bit >= 0; bit--)
word = (word << 1) + (inb(id_port) & 0x01);
if (el3_debug > 3)
printk("  3c509 EEPROM word %d %#4.4x.\n", index, word);
return word;
}
static int
el3_open(struct device *dev)
{
int ioaddr = dev->base_addr;
int i;
outw(TxReset, ioaddr + EL3_CMD);
outw(RxReset, ioaddr + EL3_CMD);
outw(SetStatusEnb | 0x00, ioaddr + EL3_CMD);
if (request_irq(dev->irq, &el3_interrupt, 0, "3c509", dev)) {
return -EAGAIN;
}
EL3WINDOW(0);
if (el3_debug > 3)
printk("%s: Opening, IRQ %d	 status@%x %4.4x.\n", dev->name,
dev->irq, ioaddr + EL3_STATUS, inw(ioaddr + EL3_STATUS));
outw(0x0001, ioaddr + 4);
outw((dev->irq << 12) | 0x0f00, ioaddr + WN0_IRQ);
EL3WINDOW(2);
for (i = 0; i < 6; i++)
outb(dev->dev_addr[i], ioaddr + i);
if (dev->if_port == 3)
outw(StartCoax, ioaddr + EL3_CMD);
else if (dev->if_port == 0) {
EL3WINDOW(4);
outw(inw(ioaddr + WN4_MEDIA) | MEDIA_TP, ioaddr + WN4_MEDIA);
}
outw(StatsDisable, ioaddr + EL3_CMD);
EL3WINDOW(6);
for (i = 0; i < 9; i++)
inb(ioaddr + i);
inw(ioaddr + 10);
inw(ioaddr + 12);
EL3WINDOW(1);
outw(SetRxFilter | RxStation | RxBroadcast, ioaddr + EL3_CMD);
outw(StatsEnable, ioaddr + EL3_CMD);
dev->interrupt = 0;
dev->tbusy = 0;
dev->start = 1;
outw(RxEnable, ioaddr + EL3_CMD);
outw(TxEnable, ioaddr + EL3_CMD);
outw(SetStatusEnb | 0xff, ioaddr + EL3_CMD);
outw(AckIntr | IntLatch | TxAvailable | RxEarly | IntReq,
ioaddr + EL3_CMD);
outw(SetIntrEnb | IntLatch|TxAvailable|TxComplete|RxComplete|StatsFull,
ioaddr + EL3_CMD);
if (el3_debug > 3)
printk("%s: Opened 3c509  IRQ %d  status %4.4x.\n",
dev->name, dev->irq, inw(ioaddr + EL3_STATUS));
MOD_INC_USE_COUNT;
return 0;
}
static int
el3_start_xmit(struct sk_buff *skb, struct device *dev)
{
struct el3_private *lp = (struct el3_private *)dev->priv;
int ioaddr = dev->base_addr;
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < TX_TIMEOUT)
return 1;
printk("%s: transmit timed out, Tx_status %2.2x status %4.4x "
"Tx FIFO room %d.\n",
dev->name, inb(ioaddr + TX_STATUS), inw(ioaddr + EL3_STATUS),
inw(ioaddr + TX_FREE));
lp->stats.tx_errors++;
dev->trans_start = jiffies;
outw(TxReset, ioaddr + EL3_CMD);
outw(TxEnable, ioaddr + EL3_CMD);
dev->tbusy = 0;
}
if (el3_debug > 4) {
printk("%s: el3_start_xmit(length = %ld) called, status %4.4x.\n",
dev->name, skb->len, inw(ioaddr + EL3_STATUS));
}
#if 0
#ifndef final_version
{
ushort status = inw(ioaddr + EL3_STATUS);
if (status & 0x0001
&& inw(ioaddr + EL3_STATUS) & 1) {
printk("%s: Missed interrupt, status then %04x now %04x"
"  Tx %2.2x Rx %4.4x.\n", dev->name, status,
inw(ioaddr + EL3_STATUS), inb(ioaddr + TX_STATUS),
inw(ioaddr + RX_STATUS));
outw(SetStatusEnb | 0x00, ioaddr + EL3_CMD);
outw(AckIntr | IntLatch | TxAvailable | RxEarly | IntReq,
ioaddr + EL3_CMD);
outw(SetStatusEnb | 0xff, ioaddr + EL3_CMD);
}
}
#endif
#endif
if (set_bit(0, (void*)&dev->tbusy) != 0)
printk("%s: Transmitter access conflict.\n", dev->name);
else {
outw(skb->len, ioaddr + TX_FIFO);
outw(0x00, ioaddr + TX_FIFO);
#ifdef  __powerpc__
outsl_unswapped(ioaddr + TX_FIFO, skb->data, (skb->len + 3) >> 2);
#else
outsl(ioaddr + TX_FIFO, skb->data, (skb->len + 3) >> 2);
#endif
dev->trans_start = jiffies;
if (inw(ioaddr + TX_FREE) > 1536) {
dev->tbusy = 0;
} else
outw(SetTxThreshold + 1536, ioaddr + EL3_CMD);
}
dev_kfree_skb (skb, FREE_WRITE);
{
short tx_status;
int i = 4;
while (--i > 0	&&	(tx_status = inb(ioaddr + TX_STATUS)) > 0) {
if (tx_status & 0x38) lp->stats.tx_aborted_errors++;
if (tx_status & 0x30) outw(TxReset, ioaddr + EL3_CMD);
if (tx_status & 0x3C) outw(TxEnable, ioaddr + EL3_CMD);
outb(0x00, ioaddr + TX_STATUS);
}
}
return 0;
}
static void
el3_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct device *dev = (struct device *)dev_id;
int ioaddr, status;
int i = max_interrupt_work;
if (dev == NULL) {
printk ("el3_interrupt(): irq %d for unknown device.\n", irq);
return;
}
if (dev->interrupt)
printk("%s: Re-entering the interrupt handler.\n", dev->name);
dev->interrupt = 1;
ioaddr = dev->base_addr;
status = inw(ioaddr + EL3_STATUS);
if (el3_debug > 4)
printk("%s: interrupt, status %4.4x.\n", dev->name, status);
while ((status = inw(ioaddr + EL3_STATUS)) &
(IntLatch | RxComplete | StatsFull)) {
if (status & RxComplete)
el3_rx(dev);
if (status & TxAvailable) {
if (el3_debug > 5)
printk("	TX room bit was handled.\n");
outw(AckIntr | TxAvailable, ioaddr + EL3_CMD);
dev->tbusy = 0;
mark_bh(NET_BH);
}
if (status & (AdapterFailure | RxEarly | StatsFull | TxComplete)) {
if (status & StatsFull)
update_stats(ioaddr, dev);
if (status & RxEarly) {
el3_rx(dev);
outw(AckIntr | RxEarly, ioaddr + EL3_CMD);
}
if (status & TxComplete) {
struct el3_private *lp = (struct el3_private *)dev->priv;
short tx_status;
int i = 4;
while (--i>0 && (tx_status = inb(ioaddr + TX_STATUS)) > 0) {
if (tx_status & 0x38) lp->stats.tx_aborted_errors++;
if (tx_status & 0x30) outw(TxReset, ioaddr + EL3_CMD);
if (tx_status & 0x3C) outw(TxEnable, ioaddr + EL3_CMD);
outb(0x00, ioaddr + TX_STATUS);
}
}
if (status & AdapterFailure) {
outw(RxReset, ioaddr + EL3_CMD);
outw(SetRxFilter | RxStation | RxBroadcast
| (dev->flags & IFF_ALLMULTI ? RxMulticast : 0)
| (dev->flags & IFF_PROMISC ? RxProm : 0),
ioaddr + EL3_CMD);
outw(RxEnable, ioaddr + EL3_CMD);
outw(AckIntr | AdapterFailure, ioaddr + EL3_CMD);
}
}
if (--i < 0) {
printk("%s: Infinite loop in interrupt, status %4.4x.\n",
dev->name, status);
outw(AckIntr | 0xFF, ioaddr + EL3_CMD);
break;
}
outw(AckIntr | IntReq | IntLatch, ioaddr + EL3_CMD);
}
if (el3_debug > 4) {
printk("%s: exiting interrupt, status %4.4x.\n", dev->name,
inw(ioaddr + EL3_STATUS));
}
dev->interrupt = 0;
return;
}
static struct enet_statistics *
el3_get_stats(struct device *dev)
{
struct el3_private *lp = (struct el3_private *)dev->priv;
unsigned long flags;
save_flags(flags);
cli();
update_stats(dev->base_addr, dev);
restore_flags(flags);
return &lp->stats;
}
static void update_stats(int ioaddr, struct device *dev)
{
struct el3_private *lp = (struct el3_private *)dev->priv;
if (el3_debug > 5)
printk("   Updating the statistics.\n");
outw(StatsDisable, ioaddr + EL3_CMD);
EL3WINDOW(6);
lp->stats.tx_carrier_errors 	+= inb(ioaddr + 0);
lp->stats.tx_heartbeat_errors	+= inb(ioaddr + 1);
inb(ioaddr + 2);
lp->stats.collisions			+= inb(ioaddr + 3);
lp->stats.tx_window_errors		+= inb(ioaddr + 4);
lp->stats.rx_fifo_errors		+= inb(ioaddr + 5);
lp->stats.tx_packets			+= inb(ioaddr + 6);
inb(ioaddr + 7);
inb(ioaddr + 8);
inw(ioaddr + 10);
inw(ioaddr + 12);
EL3WINDOW(1);
outw(StatsEnable, ioaddr + EL3_CMD);
return;
}
static int
el3_rx(struct device *dev)
{
struct el3_private *lp = (struct el3_private *)dev->priv;
int ioaddr = dev->base_addr;
short rx_status;
if (el3_debug > 5)
printk("   In rx_packet(), status %4.4x, rx_status %4.4x.\n",
inw(ioaddr+EL3_STATUS), inw(ioaddr+RX_STATUS));
while ((rx_status = inw(ioaddr + RX_STATUS)) > 0) {
if (rx_status & 0x4000) {
short error = rx_status & 0x3800;
outw(RxDiscard, ioaddr + EL3_CMD);
lp->stats.rx_errors++;
switch (error) {
case 0x0000:		lp->stats.rx_over_errors++; break;
case 0x0800:		lp->stats.rx_length_errors++; break;
case 0x1000:		lp->stats.rx_frame_errors++; break;
case 0x1800:		lp->stats.rx_length_errors++; break;
case 0x2000:		lp->stats.rx_frame_errors++; break;
case 0x2800:		lp->stats.rx_crc_errors++; break;
}
} else {
short pkt_len = rx_status & 0x7ff;
struct sk_buff *skb;
skb = dev_alloc_skb(pkt_len+5);
if (el3_debug > 4)
printk("Receiving packet size %d status %4.4x.\n",
pkt_len, rx_status);
if (skb != NULL) {
skb->dev = dev;
skb_reserve(skb, 2);
#ifdef  __powerpc__
insl_unswapped(ioaddr+RX_FIFO, skb_put(skb,pkt_len),
(pkt_len + 3) >> 2);
#else
insl(ioaddr + RX_FIFO, skb_put(skb,pkt_len),
(pkt_len + 3) >> 2);
#endif
outw(RxDiscard, ioaddr + EL3_CMD);
skb->protocol = eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
continue;
}
outw(RxDiscard, ioaddr + EL3_CMD);
lp->stats.rx_dropped++;
if (el3_debug)
printk("%s: Couldn't allocate a sk_buff of size %d.\n",
dev->name, pkt_len);
}
inw(ioaddr + EL3_STATUS);
while (inw(ioaddr + EL3_STATUS) & 0x1000)
printk("	Waiting for 3c509 to discard packet, status %x.\n",
inw(ioaddr + EL3_STATUS) );
}
return 0;
}
static void
set_multicast_list(struct device *dev)
{
int ioaddr = dev->base_addr;
if (el3_debug > 1) {
static int old = 0;
if (old != dev->mc_count) {
old = dev->mc_count;
printk("%s: Setting Rx mode to %d addresses.\n", dev->name, dev->mc_count);
}
}
if (dev->flags&IFF_PROMISC) {
outw(SetRxFilter | RxStation | RxMulticast | RxBroadcast | RxProm,
ioaddr + EL3_CMD);
}
else if (dev->mc_count || (dev->flags&IFF_ALLMULTI)) {
outw(SetRxFilter | RxStation | RxMulticast | RxBroadcast, ioaddr + EL3_CMD);
}
else
outw(SetRxFilter | RxStation | RxBroadcast, ioaddr + EL3_CMD);
}
static int
el3_close(struct device *dev)
{
int ioaddr = dev->base_addr;
if (el3_debug > 2)
printk("%s: Shutting down ethercard.\n", dev->name);
dev->tbusy = 1;
dev->start = 0;
outw(StatsDisable, ioaddr + EL3_CMD);
outw(RxDisable, ioaddr + EL3_CMD);
outw(TxDisable, ioaddr + EL3_CMD);
if (dev->if_port == 3)
outw(StopCoax, ioaddr + EL3_CMD);
else if (dev->if_port == 0) {
EL3WINDOW(4);
outw(inw(ioaddr + WN4_MEDIA) & ~MEDIA_TP, ioaddr + WN4_MEDIA);
}
free_irq(dev->irq, dev);
EL3WINDOW(0);
outw(0x0f00, ioaddr + WN0_IRQ);
update_stats(ioaddr, dev);
MOD_DEC_USE_COUNT;
return 0;
}
#ifdef MODULE
static int debug = -1;
static int irq[] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int xcvr[] = {-1, -1, -1, -1, -1, -1, -1, -1};
int
init_module(void)
{
int el3_cards = 0;
if (debug >= 0)
el3_debug = debug;
el3_root_dev = NULL;
while (el3_probe(0) == 0) {
if (irq[el3_cards] > 1)
el3_root_dev->irq = irq[el3_cards];
if (xcvr[el3_cards] >= 0)
el3_root_dev->if_port = xcvr[el3_cards];
el3_cards++;
}
return el3_cards ? 0 : -ENODEV;
}
void
cleanup_module(void)
{
struct device *next_dev;
while (el3_root_dev) {
next_dev = ((struct el3_private *)el3_root_dev->priv)->next_dev;
unregister_netdev(el3_root_dev);
release_region(el3_root_dev->base_addr, EL3_IO_EXTENT);
kfree(el3_root_dev);
el3_root_dev = next_dev;
}
}
#endif