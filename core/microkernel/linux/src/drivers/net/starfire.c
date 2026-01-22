static const char version1[] =
"starfire.c:v1.09 7/22/2003  Copyright by Donald Becker <becker@scyld.com>\n";
static const char version2[] =
" Updates and info at http:
static int interrupt_mitigation = 0x0;
static int debug = 2;
static int max_interrupt_work = 20;
static int multicast_filter_limit = 32;
static int rx_copybreak = 0;
#define MAX_UNITS 8
static int options[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int full_duplex[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
#define RX_RING_SIZE 256
#define TX_RING_SIZE 32
#define DONE_Q_SIZE 1024
#define TX_TIMEOUT (6*HZ)
#define PKT_BUF_SZ 1536
#ifndef __KERNEL__
#define __KERNEL__
#endif
#if !defined(__OPTIMIZE__)
#warning You must compile this file with the correct options!
#warning See the last lines of the source file.
#error You must compile this driver with "-O".
#endif
#include <linux/config.h>
#if defined(CONFIG_SMP) && ! defined(__SMP__)
#define __SMP__
#endif
#if defined(MODULE) && defined(CONFIG_MODVERSIONS) && ! defined(MODVERSIONS)
#define MODVERSIONS
#endif
#include <linux/version.h>
#if defined(MODVERSIONS)
#include <linux/modversions.h>
#endif
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/errno.h>
#include <linux/ioport.h>
#if LINUX_VERSION_CODE >= 0x20400
#include <linux/slab.h>
#else
#include <linux/malloc.h>
#endif
#include <linux/interrupt.h>
#include <linux/pci.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <asm/processor.h>
#include <asm/bitops.h>
#include <asm/io.h>
#ifdef INLINE_PCISCAN
#include "k_compat.h"
#else
#include "pci-scan.h"
#include "kern_compat.h"
#endif
#define virt_to_le32desc(addr) cpu_to_le32(virt_to_bus(addr))
#define le32desc_to_virt(addr) bus_to_virt(le32_to_cpu(addr))
#if (LINUX_VERSION_CODE >= 0x20100) && defined(MODULE)
char kernel_version[] = UTS_RELEASE;
#endif
MODULE_AUTHOR("Donald Becker <becker@scyld.com>");
MODULE_DESCRIPTION("Adaptec Starfire Ethernet driver");
MODULE_LICENSE("GPL");
MODULE_PARM(debug, "i");
MODULE_PARM(options, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(rx_copybreak, "i");
MODULE_PARM(full_duplex, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(multicast_filter_limit, "i");
MODULE_PARM(max_interrupt_work, "i");
MODULE_PARM_DESC(debug, "Driver message enable level (0-31)");
MODULE_PARM_DESC(options, "Force transceiver type or fixed speed+duplex");
MODULE_PARM_DESC(max_interrupt_work,
"Driver maximum events handled per interrupt");
MODULE_PARM_DESC(full_duplex,
"Non-zero to set forced full duplex (deprecated).");
MODULE_PARM_DESC(rx_copybreak,
"Breakpoint in bytes for copy-only-tiny-frames");
MODULE_PARM_DESC(multicast_filter_limit,
"Multicast addresses before switching to Rx-all-multicast");
static void *starfire_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int find_cnt);
static int starfire_pwr_event(void *dev_instance, int event);
enum chip_capability_flags {CanHaveMII=1, };
#define PCI_IOTYPE (PCI_USES_MASTER | PCI_USES_MEM | PCI_ADDR0)
#define MEM_ADDR_SZ 0x80000
#if 0 && (defined(__x86_64) || defined(__alpha__))
#define STARFIRE_ADDR_64BITS 1
#endif
static struct pci_id_info pci_id_tbl[] = {
{"Adaptec Starfire 6915", { 0x69159004, 0xffffffff, },
PCI_IOTYPE, MEM_ADDR_SZ, CanHaveMII},
{0,},
};
struct drv_id_info starfire_drv_id = {
"starfire", PCI_HOTSWAP, PCI_CLASS_NETWORK_ETHERNET<<8, pci_id_tbl,
starfire_probe1, starfire_pwr_event };
enum register_offsets {
PCIDeviceConfig=0x50040, GenCtrl=0x50070, IntrTimerCtrl=0x50074,
IntrClear=0x50080, IntrStatus=0x50084, IntrEnable=0x50088,
MIICtrl=0x52000, StationAddr=0x50120, EEPROMCtrl=0x51000,
TxDescCtrl=0x50090,
TxRingPtr=0x50098, HiPriTxRingPtr=0x50094,
TxRingHiAddr=0x5009C,
TxProducerIdx=0x500A0, TxConsumerIdx=0x500A4,
TxThreshold=0x500B0,
CompletionHiAddr=0x500B4, TxCompletionAddr=0x500B8,
RxCompletionAddr=0x500BC, RxCompletionQ2Addr=0x500C0,
CompletionQConsumerIdx=0x500C4,
RxDescQCtrl=0x500D4, RxDescQHiAddr=0x500DC, RxDescQAddr=0x500E0,
RxDescQIdx=0x500E8, RxDMAStatus=0x500F0, RxFilterMode=0x500F4,
TxMode=0x55000,
};
enum intr_status_bits {
IntrNormalSummary=0x8000, IntrAbnormalSummary=0x02000000,
IntrRxDone=0x0300, IntrRxEmpty=0x10040, IntrRxPCIErr=0x80000,
IntrTxDone=0x4000, IntrTxEmpty=0x1000, IntrTxPCIErr=0x80000,
StatsMax=0x08000000, LinkChange=0xf0000000,
IntrTxDataLow=0x00040000,
IntrPCIPin=0x01,
};
enum rx_mode_bits {
AcceptBroadcast=0x04, AcceptAllMulticast=0x02, AcceptAll=0x01,
AcceptMulticast=0x10, AcceptMyPhys=0xE040,
};
enum misc_bits {
ChipResetCmd=1,
PCIIntEnb=0x00800000,
TxEnable=0x0A, RxEnable=0x05, SoftIntr=0x100,
};
struct starfire_rx_desc {
u32 rxaddr;
#if defined(STARFIRE_ADDR_64BITS)
u32 rxaddr_hi;
#endif
};
enum rx_desc_bits {
RxDescValid=1, RxDescEndRing=2,
};
struct rx_done_desc {
u32 status;
#ifdef full_rx_status
u32 status2;
u16 vlanid;
u16 csum;
u32 timestamp;
#endif
};
enum rx_done_bits {
RxOK=0x20000000, RxFIFOErr=0x10000000, RxBufQ2=0x08000000,
};
struct starfire_tx_desc {
u32 status;
u32 addr;
};
enum tx_desc_bits {
TxDescID=0xB1010000,
TxDescIntr=0x08000000, TxRingWrap=0x04000000,
};
struct tx_done_report {
u32 status;
#if 0
u32 intrstatus;
#endif
};
#define PRIV_ALIGN 15
struct netdev_private {
struct starfire_rx_desc *rx_ring;
struct starfire_tx_desc *tx_ring;
struct net_device *next_module;
void *priv_addr;
const char *product_name;
struct sk_buff* rx_skbuff[RX_RING_SIZE];
struct sk_buff* tx_skbuff[TX_RING_SIZE];
u8 pad0[100];
struct rx_done_desc *rx_done_q __attribute__((aligned (L1_CACHE_BYTES)));
unsigned int rx_done;
struct tx_done_report *tx_done_q __attribute__((aligned (L1_CACHE_BYTES)));
unsigned int tx_done;
struct net_device_stats stats;
struct timer_list timer;
int msg_level;
int chip_id, drv_flags;
struct pci_dev *pci_dev;
int max_interrupt_work;
int intr_enable;
unsigned int restore_intr_enable:1;
unsigned int polling:1;
unsigned int cur_rx, dirty_rx;
unsigned int rx_buf_sz;
int rx_copybreak;
unsigned int cur_tx, dirty_tx;
unsigned int tx_full:1;
unsigned int full_duplex:1,
medialock:1,
rx_flowctrl:1,
tx_flowctrl:1;
unsigned int default_port;
u32 tx_mode;
u8 tx_threshold;
u32 cur_rx_mode;
u16 mc_filter[32];
int multicast_filter_limit;
int mii_cnt;
u16 advertising;
unsigned char phys[2];
};
static int mdio_read(struct net_device *dev, int phy_id, int location);
static void mdio_write(struct net_device *dev, int phy_id, int location,
int value);
static int netdev_open(struct net_device *dev);
static int change_mtu(struct net_device *dev, int new_mtu);
static void check_duplex(struct net_device *dev);
static void netdev_timer(unsigned long data);
static void tx_timeout(struct net_device *dev);
static void init_ring(struct net_device *dev);
static int start_tx(struct sk_buff *skb, struct net_device *dev);
static void intr_handler(int irq, void *dev_instance, struct pt_regs *regs);
static void netdev_error(struct net_device *dev, int intr_status);
static int netdev_rx(struct net_device *dev);
static void netdev_error(struct net_device *dev, int intr_status);
static void set_rx_mode(struct net_device *dev);
static struct net_device_stats *get_stats(struct net_device *dev);
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd);
static int netdev_close(struct net_device *dev);
static struct net_device *root_net_dev = NULL;
#ifndef MODULE
int starfire_probe(struct net_device *dev)
{
if (pci_drv_register(&starfire_drv_id, dev) < 0)
return -ENODEV;
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
return 0;
}
#endif
static void *starfire_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int card_idx)
{
struct net_device *dev;
struct netdev_private *np;
void *priv_mem;
int i, option = card_idx < MAX_UNITS ? options[card_idx] : 0;
dev = init_etherdev(init_dev, 0);
if (!dev)
return NULL;
printk(KERN_INFO "%s: %s at 0x%lx, ",
dev->name, pci_id_tbl[chip_idx].name, ioaddr);
for (i = 0; i < 6; i++)
dev->dev_addr[i] = readb(ioaddr + EEPROMCtrl + 20-i);
for (i = 0; i < 5; i++)
printk("%2.2x:", dev->dev_addr[i]);
printk("%2.2x, IRQ %d.\n", dev->dev_addr[i], irq);
priv_mem = kmalloc(sizeof(*np) + PRIV_ALIGN, GFP_KERNEL);
if (priv_mem == NULL)
return NULL;
writel(ChipResetCmd, ioaddr + PCIDeviceConfig);
dev->base_addr = ioaddr;
dev->irq = irq;
dev->priv = np = (void *)(((long)priv_mem + PRIV_ALIGN) & ~PRIV_ALIGN);
memset(np, 0, sizeof(*np));
np->priv_addr = priv_mem;
np->next_module = root_net_dev;
root_net_dev = dev;
np->pci_dev = pdev;
np->chip_id = chip_idx;
np->drv_flags = pci_id_tbl[chip_idx].drv_flags;
np->msg_level = (1 << debug) - 1;
np->rx_copybreak = rx_copybreak;
np->max_interrupt_work = max_interrupt_work;
np->multicast_filter_limit = multicast_filter_limit;
if (dev->mem_start)
option = dev->mem_start;
if (card_idx < MAX_UNITS && full_duplex[card_idx] > 0)
np->full_duplex = 1;
if (np->full_duplex) {
if (np->msg_level & NETIF_MSG_PROBE)
printk(KERN_INFO "%s: Set to forced full duplex, autonegotiation"
" disabled.\n", dev->name);
np->medialock = 1;
}
dev->open = &netdev_open;
dev->hard_start_xmit = &start_tx;
dev->stop = &netdev_close;
dev->get_stats = &get_stats;
dev->set_multicast_list = &set_rx_mode;
dev->do_ioctl = &mii_ioctl;
dev->change_mtu = &change_mtu;
if (np->drv_flags & CanHaveMII) {
int phy, phy_idx = 0;
for (phy = 0; phy < 32 && phy_idx < 4; phy++) {
int mii_status = mdio_read(dev, phy, 1);
if (mii_status != 0xffff && mii_status != 0x0000) {
np->phys[phy_idx++] = phy;
np->advertising = mdio_read(dev, phy, 4);
if (np->msg_level & NETIF_MSG_PROBE)
printk(KERN_INFO "%s: MII PHY found at address %d, status "
"0x%4.4x advertising %4.4x.\n",
dev->name, phy, mii_status, np->advertising);
}
}
np->mii_cnt = phy_idx;
}
if (option > 0) {
if (option & 0x220)
np->full_duplex = 1;
np->default_port = option & 0x3ff;
if (np->default_port & 0x330) {
np->medialock = 1;
if (np->msg_level & NETIF_MSG_PROBE)
printk(KERN_INFO "  Forcing %dMbs %s-duplex operation.\n",
(option & 0x300 ? 100 : 10),
(np->full_duplex ? "full" : "half"));
mdio_write(dev, np->phys[0], 0,
((option & 0x300) ? 0x2000 : 0) |
(np->full_duplex ? 0x0100 : 0));
}
}
return dev;
}
static int mdio_read(struct net_device *dev, int phy_id, int location)
{
long mdio_addr = dev->base_addr + MIICtrl + (phy_id<<7) + (location<<2);
int result, boguscnt=1000;
do
result = readl(mdio_addr);
while ((result & 0xC0000000) != 0x80000000 && --boguscnt >= 0);
return result & 0xffff;
}
static void mdio_write(struct net_device *dev, int phy_id, int location, int value)
{
long mdio_addr = dev->base_addr + MIICtrl + (phy_id<<7) + (location<<2);
writel(value, mdio_addr);
return;
}
static int netdev_open(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int i;
MOD_INC_USE_COUNT;
if (request_irq(dev->irq, &intr_handler, SA_SHIRQ, dev->name, dev)) {
MOD_DEC_USE_COUNT;
return -EAGAIN;
}
writel(0, ioaddr + GenCtrl);
writel(ChipResetCmd, ioaddr + PCIDeviceConfig);
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: netdev_open() irq %d.\n",
dev->name, dev->irq);
if (np->tx_done_q == 0)
np->tx_done_q = (struct tx_done_report *)get_free_page(GFP_KERNEL);
if (np->rx_done_q == 0)
np->rx_done_q = (struct rx_done_desc *)get_free_page(GFP_KERNEL);
if (np->tx_ring == 0)
np->tx_ring = (struct starfire_tx_desc *)get_free_page(GFP_KERNEL);
if (np->rx_ring == 0)
np->rx_ring = (struct starfire_rx_desc *)get_free_page(GFP_KERNEL);
if (np->tx_done_q == 0 || np->rx_done_q == 0
|| np->rx_ring == 0 || np->tx_ring == 0) {
MOD_DEC_USE_COUNT;
return -ENOMEM;
}
init_ring(dev);
writel((np->rx_buf_sz<<16) | 0xA000, ioaddr + RxDescQCtrl);
writel(0x02000401, ioaddr + TxDescCtrl);
#if defined(STARFIRE_ADDR_64BITS)
writel(virt_to_bus(np->rx_ring) >> 32, ioaddr + RxDescQHiAddr);
writel(virt_to_bus(np->tx_ring) >> 32, ioaddr + TxRingHiAddr);
#else
writel(0, ioaddr + RxDescQHiAddr);
writel(0, ioaddr + TxRingHiAddr);
writel(0, ioaddr + CompletionHiAddr);
#endif
writel(virt_to_bus(np->rx_ring), ioaddr + RxDescQAddr);
writel(virt_to_bus(np->tx_ring), ioaddr + TxRingPtr);
writel(virt_to_bus(np->tx_done_q), ioaddr + TxCompletionAddr);
writel(virt_to_bus(np->rx_done_q), ioaddr + RxCompletionAddr);
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s:  Filling in the station address.\n", dev->name);
for (i = 0; i < 6; i++)
writeb(dev->dev_addr[i], ioaddr + StationAddr + 5-i);
for (i = 0; i < 16; i++) {
u16 *eaddrs = (u16 *)dev->dev_addr;
long setup_frm = ioaddr + 0x56000 + i*16;
writew(cpu_to_be16(eaddrs[2]), setup_frm); setup_frm += 4;
writew(cpu_to_be16(eaddrs[1]), setup_frm); setup_frm += 4;
writew(cpu_to_be16(eaddrs[0]), setup_frm); setup_frm += 8;
}
np->tx_mode = 0;
np->tx_threshold = 4;
writel(np->tx_threshold, ioaddr + TxThreshold);
writel(interrupt_mitigation, ioaddr + IntrTimerCtrl);
if (dev->if_port == 0)
dev->if_port = np->default_port;
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s:  Setting the Rx and Tx modes.\n", dev->name);
set_rx_mode(dev);
np->advertising = mdio_read(dev, np->phys[0], 4);
check_duplex(dev);
netif_start_tx_queue(dev);
np->intr_enable = IntrRxDone | IntrRxEmpty | IntrRxPCIErr |
IntrTxDone | IntrTxEmpty | IntrTxPCIErr |
StatsMax | LinkChange | IntrNormalSummary | IntrAbnormalSummary
| 0x0010;
writel(np->intr_enable, ioaddr + IntrEnable);
writel(PCIIntEnb | readl(ioaddr + PCIDeviceConfig),
ioaddr + PCIDeviceConfig);
writel(TxEnable|RxEnable, ioaddr + GenCtrl);
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: Done netdev_open().\n",
dev->name);
init_timer(&np->timer);
np->timer.expires = jiffies + 3*HZ;
np->timer.data = (unsigned long)dev;
np->timer.function = &netdev_timer;
add_timer(&np->timer);
return 0;
}
static int change_mtu(struct net_device *dev, int new_mtu)
{
if ((new_mtu < 68) || (new_mtu > 17268))
return -EINVAL;
if (netif_running(dev))
return -EBUSY;
dev->mtu = new_mtu;
return 0;
}
static void check_duplex(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int new_tx_mode;
new_tx_mode = 0x0C04 | (np->tx_flowctrl ? 0x0800:0)
| (np->rx_flowctrl ? 0x0400:0);
if (np->medialock) {
if (np->full_duplex)
new_tx_mode |= 2;
} else {
int mii_reg5 = mdio_read(dev, np->phys[0], 5);
int negotiated = mii_reg5 & np->advertising;
int duplex = (negotiated & 0x0100) || (negotiated & 0x01C0) == 0x0040;
if (duplex)
new_tx_mode |= 2;
if (np->full_duplex != duplex) {
np->full_duplex = duplex;
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_INFO "%s: Setting %s-duplex based on MII #%d"
" negotiated capability %4.4x.\n", dev->name,
duplex ? "full" : "half", np->phys[0], negotiated);
}
}
if (new_tx_mode != np->tx_mode) {
np->tx_mode = new_tx_mode;
writel(np->tx_mode | 0x8000, ioaddr + TxMode);
writel(np->tx_mode, ioaddr + TxMode);
}
}
static void netdev_timer(unsigned long data)
{
struct net_device *dev = (struct net_device *)data;
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int status = readl(ioaddr + IntrStatus);
static long last_msg = 0;
np->timer.expires = jiffies + 60*HZ;
if (np->msg_level & NETIF_MSG_TIMER) {
printk(KERN_DEBUG "%s: Media selection timer tick, status %8.8x.\n",
dev->name, status);
}
if (status == 0xffffffff) {
if (jiffies - last_msg > 10*HZ) {
last_msg = jiffies;
printk(KERN_ERR "%s: The Starfire chip is missing!\n",
dev->name);
}
} else if (np->polling) {
if (status & IntrPCIPin) {
intr_handler(dev->irq, dev, 0);
if (jiffies - last_msg > 10*HZ) {
printk(KERN_ERR "%s: IRQ %d is still blocked!\n",
dev->name, dev->irq);
last_msg = jiffies;
}
} else if (jiffies - last_msg > 10*HZ)
np->polling = 0;
np->timer.expires = jiffies + 2;
} else if (status & IntrPCIPin) {
int new_status = readl(ioaddr + IntrStatus);
if (new_status & IntrPCIPin) {
printk(KERN_ERR "%s: IRQ %d is not raising an interrupt! "
"Status %8.8x/%8.8x.  \n",
dev->name, dev->irq, status, new_status);
intr_handler(dev->irq, dev, 0);
np->timer.expires = jiffies + 2;
np->polling = 1;
}
} else if (netif_queue_paused(dev) &&
np->cur_tx - np->dirty_tx > 1 &&
(jiffies - dev->trans_start) > TX_TIMEOUT) {
tx_timeout(dev);
}
check_duplex(dev);
add_timer(&np->timer);
}
static void tx_timeout(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
printk(KERN_WARNING "%s: Transmit timed out, status %8.8x,"
" resetting...\n", dev->name, (int)readl(ioaddr + IntrStatus));
#if defined(__i386__)
if (np->msg_level & NETIF_MSG_TX_ERR) {
int i;
printk("\n" KERN_DEBUG "  Tx ring %p: ", np->tx_ring);
for (i = 0; i < TX_RING_SIZE; i++)
printk(" %4.4x", np->tx_ring[i].status);
printk("\n" KERN_DEBUG "  Rx ring %p: ", np->rx_ring);
for (i = 0; i < RX_RING_SIZE; i++)
printk(" %8.8x", (unsigned int)np->rx_ring[i].rxaddr);
printk("\n");
}
#endif
dev->if_port = 0;
writel(0, ioaddr + GenCtrl);
writel(TxEnable|RxEnable, ioaddr + GenCtrl);
dev->trans_start = jiffies;
np->stats.tx_errors++;
return;
}
static void init_ring(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
int i;
np->tx_full = 0;
np->cur_rx = np->cur_tx = 0;
np->dirty_rx = np->rx_done = np->dirty_tx = np->tx_done = 0;
np->rx_buf_sz = (dev->mtu <= 1522 ? PKT_BUF_SZ :
(dev->mtu + 14 + 3) & ~3);
for (i = 0; i < RX_RING_SIZE; i++) {
struct sk_buff *skb = dev_alloc_skb(np->rx_buf_sz);
np->rx_skbuff[i] = skb;
if (skb == NULL)
break;
skb->dev = dev;
np->rx_ring[i].rxaddr =
virt_to_le32desc(skb->tail) | cpu_to_le32(RxDescValid);
}
writew(i - 1, dev->base_addr + RxDescQIdx);
np->dirty_rx = (unsigned int)(i - RX_RING_SIZE);
for ( ; i < RX_RING_SIZE; i++) {
np->rx_ring[i].rxaddr = 0;
np->rx_skbuff[i] = 0;
}
np->rx_ring[i-1].rxaddr |= cpu_to_le32(RxDescEndRing);
for (i = 0; i < DONE_Q_SIZE; i++) {
np->rx_done_q[i].status = 0;
np->tx_done_q[i].status = 0;
}
for (i = 0; i < TX_RING_SIZE; i++) {
np->tx_skbuff[i] = 0;
np->tx_ring[i].status = 0;
}
return;
}
static int start_tx(struct sk_buff *skb, struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
unsigned entry;
if (netif_pause_tx_queue(dev) != 0) {
if (jiffies - dev->trans_start > TX_TIMEOUT)
tx_timeout(dev);
return 1;
}
entry = np->cur_tx % TX_RING_SIZE;
np->tx_skbuff[entry] = skb;
np->tx_ring[entry].addr = virt_to_le32desc(skb->data);
np->tx_ring[entry].status = cpu_to_le32(skb->len | TxDescID);
#if 1
if (entry >= TX_RING_SIZE-1) {
np->tx_ring[entry].status |= cpu_to_le32(TxRingWrap | TxDescIntr);
entry = -1;
}
#endif
np->cur_tx++;
writel(++entry, dev->base_addr + TxProducerIdx);
if (np->cur_tx - np->dirty_tx >= TX_RING_SIZE - 1) {
np->tx_full = 1;
if (np->cur_tx - (volatile unsigned int)np->dirty_tx
< TX_RING_SIZE - 2) {
np->tx_full = 0;
netif_unpause_tx_queue(dev);
} else
netif_stop_tx_queue(dev);
} else
netif_unpause_tx_queue(dev);
dev->trans_start = jiffies;
if (np->msg_level & NETIF_MSG_TX_QUEUED) {
printk(KERN_DEBUG "%s: Tx frame #%d slot %d  %8.8x %8.8x.\n",
dev->name, np->cur_tx, entry,
np->tx_ring[entry].status, np->tx_ring[entry].addr);
}
return 0;
}
static void intr_handler(int irq, void *dev_instance, struct pt_regs *rgs)
{
struct net_device *dev = (struct net_device *)dev_instance;
struct netdev_private *np;
long ioaddr;
int boguscnt;
#ifndef final_version
if (dev == NULL) {
printk (KERN_ERR "Netdev interrupt handler(): IRQ %d for unknown "
"device.\n", irq);
return;
}
#endif
ioaddr = dev->base_addr;
np = (struct netdev_private *)dev->priv;
boguscnt = np->max_interrupt_work;
do {
u32 intr_status = readl(ioaddr + IntrClear);
if (np->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: Interrupt status %4.4x.\n",
dev->name, intr_status);
if (intr_status == 0 || intr_status == 0xffffffff)
break;
if (intr_status & IntrRxDone)
netdev_rx(dev);
{
int consumer = readl(ioaddr + TxConsumerIdx);
int tx_status;
if (np->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: Tx Consumer index is %d.\n",
dev->name, consumer);
#if 0
if (np->tx_done >= 250 || np->tx_done == 0)
printk(KERN_DEBUG "%s: Tx completion entry %d is %8.8x, "
"%d is %8.8x.\n", dev->name,
np->tx_done, np->tx_done_q[np->tx_done].status,
(np->tx_done+1) & (DONE_Q_SIZE-1),
np->tx_done_q[(np->tx_done+1)&(DONE_Q_SIZE-1)].status);
#endif
while ((tx_status = cpu_to_le32(np->tx_done_q[np->tx_done].status))
!= 0) {
if (np->msg_level & NETIF_MSG_TX_DONE)
printk(KERN_DEBUG "%s: Tx completion entry %d is %8.8x.\n",
dev->name, np->tx_done, tx_status);
if ((tx_status & 0xe0000000) == 0xa0000000) {
np->stats.tx_packets++;
} else if ((tx_status & 0xe0000000) == 0x80000000) {
u16 entry = tx_status;
entry >>= 3;
if (np->tx_skbuff[entry]) {
dev_free_skb_irq(np->tx_skbuff[entry]);
} else
printk(KERN_WARNING "%s: Null skbuff at entry %d!!!\n",
dev->name, entry);
np->tx_skbuff[entry] = 0;
np->dirty_tx++;
}
np->tx_done_q[np->tx_done].status = 0;
np->tx_done = (np->tx_done+1) & (DONE_Q_SIZE-1);
}
writew(np->tx_done, ioaddr + CompletionQConsumerIdx + 2);
}
if (np->tx_full && np->cur_tx - np->dirty_tx < TX_RING_SIZE - 4) {
np->tx_full = 0;
netif_resume_tx_queue(dev);
}
if (intr_status & IntrAbnormalSummary)
netdev_error(dev, intr_status);
if (--boguscnt < 0) {
printk(KERN_WARNING "%s: Too much work at interrupt, "
"status=0x%4.4x.\n",
dev->name, intr_status);
writel(0x0021, ioaddr + IntrTimerCtrl);
break;
}
} while (1);
if (np->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: exiting interrupt, status=%#4.4x.\n",
dev->name, (int)readl(ioaddr + IntrStatus));
return;
}
static int netdev_rx(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
int boguscnt = np->dirty_rx + RX_RING_SIZE - np->cur_rx;
u32 desc_status;
if (np->rx_done_q == 0) {
printk(KERN_ERR "%s:  rx_done_q is NULL!  rx_done is %d. %p.\n",
dev->name, np->rx_done, np->tx_done_q);
return 0;
}
while ((desc_status = le32_to_cpu(np->rx_done_q[np->rx_done].status)) != 0) {
if (np->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  netdev_rx() status of %d was %8.8x.\n",
np->rx_done, desc_status);
if (--boguscnt < 0)
break;
if ( ! (desc_status & RxOK)) {
if (np->msg_level & NETIF_MSG_RX_ERR)
printk(KERN_DEBUG "  netdev_rx() Rx error was %8.8x.\n",
desc_status);
np->stats.rx_errors++;
if (desc_status & RxFIFOErr)
np->stats.rx_fifo_errors++;
} else {
struct sk_buff *skb;
u16 pkt_len = desc_status;
int entry = (desc_status >> 16) & 0x7ff;
#ifndef final_version
if (np->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  netdev_rx() normal Rx pkt length %d"
", bogus_cnt %d.\n",
pkt_len, boguscnt);
#endif
if (pkt_len < rx_copybreak
&& (skb = dev_alloc_skb(pkt_len + 2)) != NULL) {
skb->dev = dev;
skb_reserve(skb, 2);
#if HAS_IP_COPYSUM
eth_copy_and_sum(skb, np->rx_skbuff[entry]->tail, pkt_len, 0);
skb_put(skb, pkt_len);
#else
memcpy(skb_put(skb, pkt_len), np->rx_skbuff[entry]->tail,
pkt_len);
#endif
} else {
char *temp = skb_put(skb = np->rx_skbuff[entry], pkt_len);
np->rx_skbuff[entry] = NULL;
#ifndef final_version
if (le32desc_to_virt(np->rx_ring[entry].rxaddr & ~3) != temp)
printk(KERN_ERR "%s: Internal fault: The skbuff addresses "
"do not match in netdev_rx: %p vs. %p / %p.\n",
dev->name,
le32desc_to_virt(np->rx_ring[entry].rxaddr),
skb->head, temp);
#endif
}
skb->protocol = eth_type_trans(skb, dev);
#ifdef full_rx_status
if (np->rx_done_q[np->rx_done].status2 & cpu_to_le32(0x01000000))
skb->ip_summed = CHECKSUM_UNNECESSARY;
#endif
netif_rx(skb);
dev->last_rx = jiffies;
np->stats.rx_packets++;
}
np->cur_rx++;
np->rx_done_q[np->rx_done].status = 0;
np->rx_done = (np->rx_done + 1) & (DONE_Q_SIZE-1);
}
writew(np->rx_done, dev->base_addr + CompletionQConsumerIdx);
for (; np->cur_rx - np->dirty_rx > 0; np->dirty_rx++) {
struct sk_buff *skb;
int entry = np->dirty_rx % RX_RING_SIZE;
if (np->rx_skbuff[entry] == NULL) {
skb = dev_alloc_skb(np->rx_buf_sz);
np->rx_skbuff[entry] = skb;
if (skb == NULL)
break;
skb->dev = dev;
np->rx_ring[entry].rxaddr =
virt_to_le32desc(skb->tail) | cpu_to_le32(RxDescValid);
}
if (entry == RX_RING_SIZE - 1)
np->rx_ring[entry].rxaddr |= cpu_to_le32(RxDescEndRing);
writew(entry, dev->base_addr + RxDescQIdx);
}
if ((np->msg_level & NETIF_MSG_RX_STATUS)
|| memcmp(np->pad0, np->pad0 + 1, sizeof(np->pad0) -1))
printk(KERN_DEBUG "  exiting netdev_rx() status of %d was %8.8x %d.\n",
np->rx_done, desc_status,
memcmp(np->pad0, np->pad0 + 1, sizeof(np->pad0) -1));
return 0;
}
static void netdev_error(struct net_device *dev, int intr_status)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
if (intr_status & LinkChange) {
int phy_num = np->phys[0];
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_NOTICE "%s: Link changed: Autonegotiation advertising"
" %4.4x  partner %4.4x.\n", dev->name,
mdio_read(dev, phy_num, 4),
mdio_read(dev, phy_num, 5));
mdio_read(dev, phy_num, 1);
if (mdio_read(dev, phy_num, 1) & 0x0004)
netif_link_up(dev);
else
netif_link_down(dev);
check_duplex(dev);
}
if (intr_status & StatsMax) {
get_stats(dev);
}
if (intr_status & IntrTxDataLow)
writel(++np->tx_threshold, dev->base_addr + TxThreshold);
if ((intr_status &
~(IntrAbnormalSummary|LinkChange|StatsMax|IntrTxDataLow| 0xFF01))
&& (np->msg_level & NETIF_MSG_DRV))
printk(KERN_ERR "%s: Something Wicked happened! %4.4x.\n",
dev->name, intr_status);
if (intr_status & IntrTxPCIErr)
np->stats.tx_fifo_errors++;
if (intr_status & IntrRxPCIErr)
np->stats.rx_fifo_errors++;
}
static struct net_device_stats *get_stats(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct netdev_private *np = (struct netdev_private *)dev->priv;
#if LINUX_VERSION_CODE > 0x20119
np->stats.tx_bytes = readl(ioaddr + 0x57010);
np->stats.rx_bytes = readl(ioaddr + 0x57044);
#endif
np->stats.tx_packets = readl(ioaddr + 0x57000);
np->stats.tx_aborted_errors =
readl(ioaddr + 0x57024) + readl(ioaddr + 0x57028);
np->stats.tx_window_errors = readl(ioaddr + 0x57018);
np->stats.collisions = readl(ioaddr + 0x57004) + readl(ioaddr + 0x57008);
np->stats.rx_dropped += readw(ioaddr + RxDMAStatus);
writew(0, ioaddr + RxDMAStatus);
np->stats.rx_crc_errors = readl(ioaddr + 0x5703C);
np->stats.rx_frame_errors = readl(ioaddr + 0x57040);
np->stats.rx_length_errors = readl(ioaddr + 0x57058);
np->stats.rx_missed_errors = readl(ioaddr + 0x5707C);
return &np->stats;
}
static unsigned const ethernet_polynomial_le = 0xedb88320U;
static inline unsigned ether_crc_le(int length, unsigned char *data)
{
unsigned int crc = ~0;
while(--length >= 0) {
unsigned char current_octet = *data++;
int bit;
for (bit = 8; --bit >= 0; current_octet >>= 1) {
if ((crc ^ current_octet) & 1) {
crc >>= 1;
crc ^= ethernet_polynomial_le;
} else
crc >>= 1;
}
}
return crc;
}
static void set_rx_mode(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
u32 rx_mode;
struct dev_mc_list *mclist;
int i;
if (dev->flags & IFF_PROMISC) {
printk(KERN_NOTICE "%s: Promiscuous mode enabled.\n", dev->name);
rx_mode = AcceptBroadcast|AcceptAllMulticast|AcceptAll|AcceptMyPhys;
} else if ((dev->mc_count > np->multicast_filter_limit)
|| (dev->flags & IFF_ALLMULTI)) {
rx_mode = AcceptBroadcast|AcceptAllMulticast|AcceptMyPhys;
} else if (dev->mc_count <= 15) {
long filter_addr = ioaddr + 0x56000 + 1*16;
for (i = 1, mclist = dev->mc_list; mclist && i <= dev->mc_count;
i++, mclist = mclist->next) {
u16 *eaddrs = (u16 *)mclist->dmi_addr;
writew(cpu_to_be16(eaddrs[2]), filter_addr); filter_addr += 4;
writew(cpu_to_be16(eaddrs[1]), filter_addr); filter_addr += 4;
writew(cpu_to_be16(eaddrs[0]), filter_addr); filter_addr += 8;
}
while (i++ < 16) {
writew(0xffff, filter_addr); filter_addr += 4;
writew(0xffff, filter_addr); filter_addr += 4;
writew(0xffff, filter_addr); filter_addr += 8;
}
rx_mode = AcceptBroadcast | AcceptMyPhys;
} else {
long filter_addr;
u16 mc_filter[32];
memset(mc_filter, 0, sizeof(mc_filter));
for (i = 0, mclist = dev->mc_list; mclist && i < dev->mc_count;
i++, mclist = mclist->next) {
set_bit(ether_crc_le(ETH_ALEN, mclist->dmi_addr) >> 23, mc_filter);
}
filter_addr = ioaddr + 0x56000 + 1*16;
for (i = 1; i < 16; i++) {
writew(0xffff, filter_addr); filter_addr += 4;
writew(0xffff, filter_addr); filter_addr += 4;
writew(0xffff, filter_addr); filter_addr += 8;
}
for (filter_addr=ioaddr + 0x56100, i=0; i < 32; filter_addr+= 16, i++){
np->mc_filter[i] = mc_filter[i];
writew(mc_filter[i], filter_addr);
}
rx_mode = AcceptBroadcast | AcceptMulticast | AcceptMyPhys;
}
writel(rx_mode, ioaddr + RxFilterMode);
}
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
u16 *data = (u16 *)&rq->ifr_data;
u32 *data32 = (void *)&rq->ifr_data;
switch(cmd) {
case 0x8947: case 0x89F0:
data[0] = np->phys[0] & 0x1f;
case 0x8948: case 0x89F1:
data[3] = mdio_read(dev, data[0] & 0x1f, data[1] & 0x1f);
return 0;
case 0x8949: case 0x89F2:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
if (data[0] == np->phys[0]) {
u16 value = data[2];
switch (data[1]) {
case 0:
np->medialock = (value & 0x9000) ? 0 : 1;
if (np->medialock)
np->full_duplex = (value & 0x0100) ? 1 : 0;
break;
case 4: np->advertising = value; break;
}
check_duplex(dev);
}
mdio_write(dev, data[0] & 0x1f, data[1] & 0x1f, data[2]);
return 0;
case SIOCGPARAMS:
data32[0] = np->msg_level;
data32[1] = np->multicast_filter_limit;
data32[2] = np->max_interrupt_work;
data32[3] = np->rx_copybreak;
return 0;
case SIOCSPARAMS:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
np->msg_level = data32[0];
np->multicast_filter_limit = data32[1];
np->max_interrupt_work = data32[2];
np->rx_copybreak = data32[3];
return 0;
default:
return -EOPNOTSUPP;
}
}
static int netdev_close(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct netdev_private *np = (struct netdev_private *)dev->priv;
int i;
netif_stop_tx_queue(dev);
if (np->msg_level & NETIF_MSG_IFDOWN) {
printk(KERN_DEBUG "%s: Shutting down ethercard, Intr status %4.4x.\n",
dev->name, (int)readl(ioaddr + IntrStatus));
printk(KERN_DEBUG "%s: Queue pointers were Tx %d / %d,  Rx %d / %d.\n",
dev->name, np->cur_tx, np->dirty_tx, np->cur_rx, np->dirty_rx);
}
writel(0, ioaddr + IntrEnable);
writel(0, ioaddr + GenCtrl);
del_timer(&np->timer);
#ifdef __i386__
if (np->msg_level & NETIF_MSG_IFDOWN) {
printk("\n"KERN_DEBUG"  Tx ring at %8.8x:\n",
(int)virt_to_bus(np->tx_ring));
for (i = 0; i < 8 ; i++)
printk(KERN_DEBUG " #%d desc. %8.8x %8.8x -> %8.8x.\n",
i, np->tx_ring[i].status, np->tx_ring[i].addr,
np->tx_done_q[i].status);
printk(KERN_DEBUG "  Rx ring at %8.8x -> %p:\n",
(int)virt_to_bus(np->rx_ring), np->rx_done_q);
if (np->rx_done_q)
for (i = 0; i < 8 ; i++) {
printk(KERN_DEBUG " #%d desc. %8.8x -> %8.8x\n",
i, np->rx_ring[i].rxaddr, np->rx_done_q[i].status);
}
}
#endif
free_irq(dev->irq, dev);
for (i = 0; i < RX_RING_SIZE; i++) {
np->rx_ring[i].rxaddr = 0xBADF00D0;
if (np->rx_skbuff[i]) {
#if LINUX_VERSION_CODE < 0x20100
np->rx_skbuff[i]->free = 1;
#endif
dev_free_skb(np->rx_skbuff[i]);
}
np->rx_skbuff[i] = 0;
}
for (i = 0; i < TX_RING_SIZE; i++) {
if (np->tx_skbuff[i])
dev_free_skb(np->tx_skbuff[i]);
np->tx_skbuff[i] = 0;
}
MOD_DEC_USE_COUNT;
return 0;
}
static int starfire_pwr_event(void *dev_instance, int event)
{
struct net_device *dev = dev_instance;
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_DEBUG "%s: Handling power event %d.\n", dev->name, event);
switch(event) {
case DRV_ATTACH:
MOD_INC_USE_COUNT;
break;
case DRV_SUSPEND:
writel(0x0000, ioaddr + IntrEnable);
writel(0, ioaddr + GenCtrl);
break;
case DRV_RESUME:
writel(np->tx_threshold, ioaddr + TxThreshold);
writel(interrupt_mitigation, ioaddr + IntrTimerCtrl);
set_rx_mode(dev);
writel(np->intr_enable, ioaddr + IntrEnable);
writel(TxEnable|RxEnable, ioaddr + GenCtrl);
break;
case DRV_DETACH: {
struct net_device **devp, **next;
if (dev->flags & IFF_UP) {
dev_close(dev);
dev->flags &= ~(IFF_UP|IFF_RUNNING);
}
unregister_netdev(dev);
release_region(dev->base_addr, pci_id_tbl[np->chip_id].io_size);
#ifndef USE_IO_OPS
iounmap((char *)dev->base_addr);
#endif
for (devp = &root_net_dev; *devp; devp = next) {
next = &((struct netdev_private *)(*devp)->priv)->next_module;
if (*devp == dev) {
*devp = *next;
break;
}
}
if (np->priv_addr)
kfree(np->priv_addr);
kfree(dev);
MOD_DEC_USE_COUNT;
break;
}
}
return 0;
}
#ifdef MODULE
int init_module(void)
{
if (debug >= NETIF_MSG_DRV)
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
if (pci_drv_register(&starfire_drv_id, NULL)) {
printk(KERN_INFO " No Starfire adapters detected, driver not loaded.\n");
return -ENODEV;
}
return 0;
}
void cleanup_module(void)
{
struct net_device *next_dev;
pci_drv_unregister(&starfire_drv_id);
while (root_net_dev) {
struct netdev_private *np = (void *)(root_net_dev->priv);
unregister_netdev(root_net_dev);
iounmap((char *)(root_net_dev->base_addr));
next_dev = np->next_module;
if (np->tx_done_q) free_page((long)np->tx_done_q);
if (np->rx_done_q) free_page((long)np->rx_done_q);
if (np->priv_addr) kfree(np->priv_addr);
kfree(root_net_dev);
root_net_dev = next_dev;
}
}
#endif