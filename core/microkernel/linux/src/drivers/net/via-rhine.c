static const char version1[] =
"via-rhine.c:v1.16 7/22/2003  Written by Donald Becker <becker@scyld.com>\n";
static const char version2[] =
"  http:
static int debug = 2;
static int max_interrupt_work = 20;
static int rx_copybreak = 0;
#define MAX_UNITS 8
static int options[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int full_duplex[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static const int multicast_filter_limit = 32;
#define TX_RING_SIZE	16
#define TX_QUEUE_LEN	10
#define RX_RING_SIZE	32
#define TX_TIMEOUT  (6*HZ)
#define PKT_BUF_SZ		1536
#ifndef __KERNEL__
#define __KERNEL__
#endif
#if !defined(__OPTIMIZE__)
#warning  You must compile this file with the correct options!
#warning  See the last lines of the source file.
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
#define virt_to_le32desc(addr)	cpu_to_le32(virt_to_bus(addr))
#define le32desc_to_virt(addr)	bus_to_virt(le32_to_cpu(addr))
#if defined(VIA_USE_MEMORY)
#warning Many adapters using the VIA Rhine chip are not configured to work
#warning with PCI memory space accesses.
#else
#define USE_IO_OPS
#undef readb
#undef readw
#undef readl
#undef writeb
#undef writew
#undef writel
#define readb inb
#define readw inw
#define readl inl
#define writeb outb
#define writew outw
#define writel outl
#endif
#if (LINUX_VERSION_CODE >= 0x20100)  &&  defined(MODULE)
char kernel_version[] = UTS_RELEASE;
#endif
MODULE_AUTHOR("Donald Becker <becker@scyld.com>");
MODULE_DESCRIPTION("VIA Rhine PCI Fast Ethernet driver");
MODULE_LICENSE("GPL");
MODULE_PARM(max_interrupt_work, "i");
MODULE_PARM(debug, "i");
MODULE_PARM(rx_copybreak, "i");
MODULE_PARM(options, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(full_duplex, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(multicast_filter_limit, "i");
MODULE_PARM_DESC(debug, "Driver message level (0-31)");
MODULE_PARM_DESC(options, "Force transceiver type or fixed speed+duplex");
MODULE_PARM_DESC(max_interrupt_work,
"Driver maximum events handled per interrupt");
MODULE_PARM_DESC(full_duplex, "Non-zero to set forced full duplex "
"(deprecated, use options[] instead).");
MODULE_PARM_DESC(rx_copybreak,
"Breakpoint in bytes for copy-only-tiny-frames");
MODULE_PARM_DESC(multicast_filter_limit,
"Multicast addresses before switching to Rx-all-multicast");
static void *via_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int find_cnt);
static int via_pwr_event(void *dev_instance, int event);
enum chip_capability_flags {
CanHaveMII=1, HasESIPhy=2, HasDavicomPhy=4, HasV1TxStat=8,
ReqTxAlign=0x10, HasWOL=0x20, HasIPChecksum=0x40, HasVLAN=0x80,
};
#if defined(VIA_USE_MEMORY)
#define RHINE_IOTYPE (PCI_USES_MEM | PCI_USES_MASTER | PCI_ADDR1)
#define RHINE_I_IOSIZE 128
#define RHINEII_IOSIZE 4096
#else
#define RHINE_IOTYPE (PCI_USES_IO  | PCI_USES_MASTER | PCI_ADDR0)
#define RHINE_I_IOSIZE 128
#define RHINEII_IOSIZE 256
#endif
static struct pci_id_info pci_tbl[] = {
{ "VIA VT3043 Rhine", { 0x30431106, 0xffffffff,},
RHINE_IOTYPE, RHINE_I_IOSIZE, CanHaveMII | ReqTxAlign | HasV1TxStat },
{ "VIA VT86C100A Rhine", { 0x61001106, 0xffffffff,},
RHINE_IOTYPE, RHINE_I_IOSIZE, CanHaveMII | ReqTxAlign | HasV1TxStat },
{ "VIA VT6102 Rhine-II", { 0x30651106, 0xffffffff,},
RHINE_IOTYPE, RHINEII_IOSIZE, CanHaveMII | HasWOL },
{ "VIA VT6105LOM Rhine-III (3106)", { 0x31061106, 0xffffffff,},
RHINE_IOTYPE, RHINEII_IOSIZE, CanHaveMII | HasWOL },
{ "VIA VT6105M Rhine-III (3106)", { 0x31061106, 0xffffffff,},
RHINE_IOTYPE, RHINEII_IOSIZE, CanHaveMII|HasWOL|HasIPChecksum|HasVLAN},
{ "VIA VT6105M Rhine-III (3053 prototype)", { 0x30531106, 0xffffffff,},
RHINE_IOTYPE, RHINEII_IOSIZE, CanHaveMII | HasWOL },
{0,},
};
struct drv_id_info via_rhine_drv_id = {
"via-rhine", PCI_HOTSWAP, PCI_CLASS_NETWORK_ETHERNET<<8, pci_tbl,
via_probe1, via_pwr_event
};
enum register_offsets {
StationAddr=0x00, RxConfig=0x06, TxConfig=0x07, ChipCmd=0x08,
IntrStatus=0x0C, IntrEnable=0x0E,
MulticastFilter0=0x10, MulticastFilter1=0x14,
RxRingPtr=0x18, TxRingPtr=0x1C,
MIIPhyAddr=0x6C, MIIStatus=0x6D, PCIBusConfig=0x6E,
MIICmd=0x70, MIIRegAddr=0x71, MIIData=0x72, MACRegEEcsr=0x74,
Config=0x78, ConfigA=0x7A, RxMissed=0x7C, RxCRCErrs=0x7E,
StickyHW=0x83, WOLcrClr=0xA4, WOLcgClr=0xA7, PwrcsrClr=0xAC,
};
enum intr_status_bits {
IntrRxDone=0x0001, IntrRxErr=0x0004, IntrRxEmpty=0x0020,
IntrTxDone=0x0002, IntrTxAbort=0x0008, IntrTxUnderrun=0x0010,
IntrPCIErr=0x0040,
IntrStatsMax=0x0080, IntrRxEarly=0x0100, IntrMIIChange=0x0200,
IntrRxOverflow=0x0400, IntrRxDropped=0x0800, IntrRxNoBuf=0x1000,
IntrTxAborted=0x2000, IntrLinkChange=0x4000,
IntrRxWakeUp=0x8000,
IntrNormalSummary=0x0003, IntrAbnormalSummary=0xC260,
};
struct rx_desc {
s32 rx_status;
u32 desc_length;
u32 addr;
u32 next_desc;
};
struct tx_desc {
s32 tx_status;
u32 desc_length;
u32 addr;
u32 next_desc;
};
enum rx_status_bits {
RxOK=0x8000, RxWholePkt=0x0300, RxErr=0x008F};
enum desc_status_bits {
DescOwn=0x80000000, DescEndPacket=0x4000, DescIntr=0x1000,
};
enum rx_info_bits {
RxTypeTag=0x00010000,
RxTypeUDP=0x00020000, RxTypeTCP=0x00040000, RxTypeIP=0x00080000,
RxTypeUTChksumOK=0x00100000, RxTypeIPChksumOK=0x00200000,
RxTypeCsumMask=0x003E0000,
RxTypeUDPSumOK=0x003A0000, RxTypeTCPSumOK=0x003C0000,
};
enum chip_cmd_bits {
CmdInit=0x0001, CmdStart=0x0002, CmdStop=0x0004, CmdRxOn=0x0008,
CmdTxOn=0x0010, CmdTxDemand=0x0020, CmdRxDemand=0x0040,
CmdEarlyRx=0x0100, CmdEarlyTx=0x0200, CmdFDuplex=0x0400,
CmdNoTxPoll=0x0800, CmdReset=0x8000,
};
#define PRIV_ALIGN	15
struct netdev_private {
struct rx_desc rx_ring[RX_RING_SIZE];
struct tx_desc tx_ring[TX_RING_SIZE];
struct sk_buff* rx_skbuff[RX_RING_SIZE];
struct sk_buff* tx_skbuff[TX_RING_SIZE];
unsigned char *tx_buf[TX_RING_SIZE];
unsigned char *tx_bufs;
struct net_device *next_module;
void *priv_addr;
struct net_device_stats stats;
struct timer_list timer;
int msg_level;
int max_interrupt_work;
int intr_enable;
int chip_id, drv_flags;
struct pci_dev *pci_dev;
struct rx_desc *rx_head_desc;
unsigned int cur_rx, dirty_rx;
unsigned int rx_buf_sz;
int rx_copybreak;
unsigned int cur_tx, dirty_tx;
u16 chip_cmd;
int multicast_filter_limit;
u32 mc_filter[2];
int rx_mode;
unsigned int tx_full:1;
unsigned int full_duplex:1;
unsigned int duplex_lock:1;
unsigned int medialock:1;
unsigned int default_port;
u8 tx_thresh, rx_thresh;
int mii_cnt;
u16 advertising;
unsigned char phys[2];
};
static int  mdio_read(struct net_device *dev, int phy_id, int location);
static void mdio_write(struct net_device *dev, int phy_id, int location, int value);
static int  netdev_open(struct net_device *dev);
static void check_duplex(struct net_device *dev);
static void netdev_timer(unsigned long data);
static void tx_timeout(struct net_device *dev);
static void init_ring(struct net_device *dev);
static int  start_tx(struct sk_buff *skb, struct net_device *dev);
static void intr_handler(int irq, void *dev_instance, struct pt_regs *regs);
static int  netdev_rx(struct net_device *dev);
static void netdev_error(struct net_device *dev, int intr_status);
static void set_rx_mode(struct net_device *dev);
static struct net_device_stats *get_stats(struct net_device *dev);
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd);
static int  netdev_close(struct net_device *dev);
static struct net_device *root_net_dev = NULL;
#ifndef MODULE
int via_rhine_probe(struct net_device *dev)
{
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
return pci_drv_register(&via_rhine_drv_id, dev);
}
#endif
static void *via_probe1(struct pci_dev *pdev, void *init_dev,
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
dev->name, pci_tbl[chip_idx].name, ioaddr);
for (i = 0; i < 6; i++)
dev->dev_addr[i] = readb(ioaddr + StationAddr + i);
if (memcmp(dev->dev_addr, "\0\0\0\0\0", 6) == 0) {
writeb(0x20, ioaddr + MACRegEEcsr);
for (i = 0; i < 150; i++)
if (! (readb(ioaddr + MACRegEEcsr) & 0x20))
break;
for (i = 0; i < 6; i++)
dev->dev_addr[i] = readb(ioaddr + StationAddr + i);
if (memcmp(dev->dev_addr, "\0\0\0\0\0", 6) == 0) {
printk(" (MISSING EEPROM ADDRESS)");
memcpy(dev->dev_addr, ">Linux", 6);
}
}
for (i = 0; i < 5; i++)
printk("%2.2x:", dev->dev_addr[i]);
printk("%2.2x, IRQ %d.\n", dev->dev_addr[i], irq);
priv_mem = kmalloc(sizeof(*np) + PRIV_ALIGN, GFP_KERNEL);
if (priv_mem == NULL)
return NULL;
#ifdef USE_IO_OPS
request_region(ioaddr, pci_tbl[chip_idx].io_size, dev->name);
#endif
writew(CmdReset, ioaddr + ChipCmd);
dev->base_addr = ioaddr;
dev->irq = irq;
dev->priv = np = (void *)(((long)priv_mem + PRIV_ALIGN) & ~PRIV_ALIGN);
memset(np, 0, sizeof(*np));
np->priv_addr = priv_mem;
np->next_module = root_net_dev;
root_net_dev = dev;
np->pci_dev = pdev;
np->chip_id = chip_idx;
np->drv_flags = pci_tbl[chip_idx].drv_flags;
np->msg_level = (1 << debug) - 1;
np->rx_copybreak = rx_copybreak;
np->max_interrupt_work = max_interrupt_work;
np->multicast_filter_limit = multicast_filter_limit;
if (dev->mem_start)
option = dev->mem_start;
if (option > 0) {
if (option & 0x220)
np->full_duplex = 1;
np->default_port = option & 15;
if (np->default_port)
np->medialock = 1;
}
if (card_idx < MAX_UNITS  &&  full_duplex[card_idx] > 0)
np->full_duplex = 1;
if (np->full_duplex) {
printk(KERN_INFO "%s: Set to forced full duplex, autonegotiation"
" disabled.\n", dev->name);
np->duplex_lock = 1;
}
dev->open = &netdev_open;
dev->hard_start_xmit = &start_tx;
dev->stop = &netdev_close;
dev->get_stats = &get_stats;
dev->set_multicast_list = &set_rx_mode;
dev->do_ioctl = &mii_ioctl;
if (np->drv_flags & CanHaveMII) {
int phy, phy_idx = 0;
np->phys[0] = 1;
for (phy = 1; phy < 32 && phy_idx < 4; phy++) {
int mii_status = mdio_read(dev, phy, 1);
if (mii_status != 0xffff  &&  mii_status != 0x0000) {
np->phys[phy_idx++] = phy;
np->advertising = mdio_read(dev, phy, 4);
printk(KERN_INFO "%s: MII PHY found at address %d, status "
"0x%4.4x advertising %4.4x Link %4.4x.\n",
dev->name, phy, mii_status, np->advertising,
mdio_read(dev, phy, 5));
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
printk(KERN_INFO "  Forcing %dMbs %s-duplex operation.\n",
(option & 0x300 ? 100 : 10),
(np->full_duplex ? "full" : "half"));
if (np->mii_cnt)
mdio_write(dev, np->phys[0], 0,
((option & 0x300) ? 0x2000 : 0) |
(np->full_duplex ? 0x0100 : 0));
}
}
return dev;
}
static int mdio_read(struct net_device *dev, int phy_id, int regnum)
{
long ioaddr = dev->base_addr;
int boguscnt = 1024;
while ((readb(ioaddr + MIICmd) & 0x60) && --boguscnt > 0)
;
writeb(0x00, ioaddr + MIICmd);
writeb(phy_id, ioaddr + MIIPhyAddr);
writeb(regnum, ioaddr + MIIRegAddr);
writeb(0x40, ioaddr + MIICmd);
boguscnt = 1024;
while ((readb(ioaddr + MIICmd) & 0x40) && --boguscnt > 0)
;
return readw(ioaddr + MIIData);
}
static void mdio_write(struct net_device *dev, int phy_id, int regnum, int value)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int boguscnt = 1024;
if (phy_id == np->phys[0]) {
switch (regnum) {
case 0:
if (value & 0x9000)
np->duplex_lock = 0;
else
np->full_duplex = (value & 0x0100) ? 1 : 0;
break;
case 4: np->advertising = value; break;
}
}
while ((readb(ioaddr + MIICmd) & 0x60) && --boguscnt > 0)
;
writeb(0x00, ioaddr + MIICmd);
writeb(phy_id, ioaddr + MIIPhyAddr);
writeb(regnum, ioaddr + MIIRegAddr);
writew(value, ioaddr + MIIData);
writeb(0x20, ioaddr + MIICmd);
return;
}
static int netdev_open(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int i;
writew(CmdReset, ioaddr + ChipCmd);
MOD_INC_USE_COUNT;
if (request_irq(dev->irq, &intr_handler, SA_SHIRQ, dev->name, dev)) {
MOD_DEC_USE_COUNT;
return -EAGAIN;
}
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: netdev_open() irq %d.\n",
dev->name, dev->irq);
init_ring(dev);
writel(virt_to_bus(np->rx_ring), ioaddr + RxRingPtr);
writel(virt_to_bus(np->tx_ring), ioaddr + TxRingPtr);
for (i = 0; i < 6; i++)
writeb(dev->dev_addr[i], ioaddr + StationAddr + i);
writew(0x0006, ioaddr + PCIBusConfig);
writeb(0x20, ioaddr + TxConfig);
np->tx_thresh = 0x20;
np->rx_thresh = 0x60;
if (dev->if_port == 0)
dev->if_port = np->default_port;
set_rx_mode(dev);
netif_start_tx_queue(dev);
np->intr_enable = IntrRxDone | IntrRxErr | IntrRxEmpty |
IntrRxOverflow| IntrRxDropped| IntrTxDone | IntrTxAbort |
IntrTxUnderrun | IntrPCIErr | IntrStatsMax | IntrLinkChange |
IntrMIIChange;
writew(np->intr_enable, ioaddr + IntrEnable);
np->chip_cmd = CmdStart|CmdTxOn|CmdRxOn|CmdNoTxPoll;
if (np->duplex_lock)
np->chip_cmd |= CmdFDuplex;
writew(np->chip_cmd, ioaddr + ChipCmd);
check_duplex(dev);
mdio_write(dev, np->phys[0], 0x17, mdio_read(dev, np->phys[0], 0x17) |
(np->drv_flags & HasESIPhy) ? 0x0080 : 0x0001);
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: Done netdev_open(), status %4.4x "
"MII status: %4.4x.\n",
dev->name, readw(ioaddr + ChipCmd),
mdio_read(dev, np->phys[0], 1));
init_timer(&np->timer);
np->timer.expires = jiffies + 2;
np->timer.data = (unsigned long)dev;
np->timer.function = &netdev_timer;
add_timer(&np->timer);
return 0;
}
static void check_duplex(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int mii_reg5 = mdio_read(dev, np->phys[0], 5);
int negotiated = mii_reg5 & np->advertising;
int duplex;
if (np->duplex_lock  ||  mii_reg5 == 0xffff)
return;
duplex = (negotiated & 0x0100) || (negotiated & 0x01C0) == 0x0040;
if (np->full_duplex != duplex) {
np->full_duplex = duplex;
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_INFO "%s: Setting %s-duplex based on MII #%d link"
" partner capability of %4.4x.\n", dev->name,
duplex ? "full" : "half", np->phys[0], mii_reg5);
if (duplex)
np->chip_cmd |= CmdFDuplex;
else
np->chip_cmd &= ~CmdFDuplex;
writew(np->chip_cmd, ioaddr + ChipCmd);
}
}
static void netdev_timer(unsigned long data)
{
struct net_device *dev = (struct net_device *)data;
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int next_tick = 10*HZ;
if (np->msg_level & NETIF_MSG_TIMER) {
printk(KERN_DEBUG "%s: VIA Rhine monitor tick, status %4.4x.\n",
dev->name, readw(ioaddr + IntrStatus));
}
if (netif_queue_paused(dev)
&& np->cur_tx - np->dirty_tx > 1
&& jiffies - dev->trans_start > TX_TIMEOUT)
tx_timeout(dev);
check_duplex(dev);
np->timer.expires = jiffies + next_tick;
add_timer(&np->timer);
}
static void tx_timeout(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
printk(KERN_WARNING "%s: Transmit timed out, status %4.4x, PHY status "
"%4.4x, resetting...\n",
dev->name, readw(ioaddr + IntrStatus),
mdio_read(dev, np->phys[0], 1));
dev->if_port = 0;
writel(virt_to_bus(np->tx_ring + (np->dirty_tx % TX_RING_SIZE)),
ioaddr + TxRingPtr);
writew(CmdTxDemand | np->chip_cmd, dev->base_addr + ChipCmd);
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
np->dirty_rx = np->dirty_tx = 0;
np->rx_buf_sz = dev->mtu + 14;
if (np->rx_buf_sz < PKT_BUF_SZ)
np->rx_buf_sz = PKT_BUF_SZ;
np->rx_head_desc = &np->rx_ring[0];
for (i = 0; i < RX_RING_SIZE; i++) {
np->rx_ring[i].rx_status = 0;
np->rx_ring[i].desc_length = cpu_to_le32(np->rx_buf_sz);
np->rx_ring[i].next_desc = virt_to_le32desc(&np->rx_ring[i+1]);
np->rx_skbuff[i] = 0;
}
np->rx_ring[i-1].next_desc = virt_to_le32desc(&np->rx_ring[0]);
for (i = 0; i < RX_RING_SIZE; i++) {
struct sk_buff *skb = dev_alloc_skb(np->rx_buf_sz);
np->rx_skbuff[i] = skb;
if (skb == NULL)
break;
skb->dev = dev;
np->rx_ring[i].addr = virt_to_le32desc(skb->tail);
np->rx_ring[i].rx_status = cpu_to_le32(DescOwn);
}
np->dirty_rx = (unsigned int)(i - RX_RING_SIZE);
for (i = 0; i < TX_RING_SIZE; i++) {
np->tx_skbuff[i] = 0;
np->tx_ring[i].tx_status = 0;
np->tx_ring[i].desc_length = cpu_to_le32(0x00e08000);
np->tx_ring[i].next_desc = virt_to_le32desc(&np->tx_ring[i+1]);
np->tx_buf[i] = 0;
}
np->tx_ring[i-1].next_desc = virt_to_le32desc(&np->tx_ring[0]);
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
if ((np->drv_flags & ReqTxAlign)  && ((long)skb->data & 3)) {
if (np->tx_buf[entry] == NULL &&
(np->tx_buf[entry] = kmalloc(PKT_BUF_SZ, GFP_KERNEL)) == NULL)
return 1;
memcpy(np->tx_buf[entry], skb->data, skb->len);
np->tx_ring[entry].addr = virt_to_le32desc(np->tx_buf[entry]);
} else
np->tx_ring[entry].addr = virt_to_le32desc(skb->data);
np->tx_ring[entry].desc_length =
cpu_to_le32(0x00E08000 | (skb->len >= ETH_ZLEN ? skb->len : ETH_ZLEN));
np->tx_ring[entry].tx_status = cpu_to_le32(DescOwn);
np->cur_tx++;
writew(CmdTxDemand | np->chip_cmd, dev->base_addr + ChipCmd);
if (np->cur_tx - np->dirty_tx >= TX_QUEUE_LEN - 1) {
np->tx_full = 1;
if (np->cur_tx - (volatile unsigned int)np->dirty_tx
< TX_QUEUE_LEN - 2) {
np->tx_full = 0;
netif_unpause_tx_queue(dev);
} else
netif_stop_tx_queue(dev);
} else
netif_unpause_tx_queue(dev);
dev->trans_start = jiffies;
if (np->msg_level & NETIF_MSG_TX_QUEUED) {
printk(KERN_DEBUG "%s: Transmit frame #%d queued in slot %d.\n",
dev->name, np->cur_tx, entry);
}
return 0;
}
static void intr_handler(int irq, void *dev_instance, struct pt_regs *rgs)
{
struct net_device *dev = (struct net_device *)dev_instance;
struct netdev_private *np = (void *)dev->priv;
long ioaddr = dev->base_addr;
int boguscnt = np->max_interrupt_work;
do {
u32 intr_status = readw(ioaddr + IntrStatus);
writew(intr_status & 0xffff, ioaddr + IntrStatus);
if (np->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: Interrupt, status %4.4x.\n",
dev->name, intr_status);
if (intr_status == 0)
break;
if (intr_status & (IntrRxDone | IntrRxErr | IntrRxDropped |
IntrRxWakeUp | IntrRxEmpty | IntrRxNoBuf))
netdev_rx(dev);
for (; np->cur_tx - np->dirty_tx > 0; np->dirty_tx++) {
int entry = np->dirty_tx % TX_RING_SIZE;
int txstatus = le32_to_cpu(np->tx_ring[entry].tx_status);
if (txstatus & DescOwn)
break;
if (np->msg_level & NETIF_MSG_TX_DONE)
printk(KERN_DEBUG "  Tx scavenge %d status %4.4x.\n",
entry, txstatus);
if (txstatus & 0x8000) {
if (np->msg_level & NETIF_MSG_TX_ERR)
printk(KERN_DEBUG "%s: Transmit error, Tx status %4.4x.\n",
dev->name, txstatus);
np->stats.tx_errors++;
if (txstatus & 0x0400) np->stats.tx_carrier_errors++;
if (txstatus & 0x0200) np->stats.tx_window_errors++;
if (txstatus & 0x0100) np->stats.tx_aborted_errors++;
if (txstatus & 0x0080) np->stats.tx_heartbeat_errors++;
if (txstatus & 0x0002) np->stats.tx_fifo_errors++;
#ifdef ETHER_STATS
if (txstatus & 0x0100) np->stats.collisions16++;
#endif
} else {
#ifdef ETHER_STATS
if (txstatus & 0x0001) np->stats.tx_deferred++;
#endif
if (np->drv_flags & HasV1TxStat)
np->stats.collisions += (txstatus >> 3) & 15;
else
np->stats.collisions += txstatus & 15;
#if defined(NETSTATS_VER2)
np->stats.tx_bytes += np->tx_skbuff[entry]->len;
#endif
np->stats.tx_packets++;
}
dev_free_skb_irq(np->tx_skbuff[entry]);
np->tx_skbuff[entry] = 0;
}
if (np->tx_full  &&  np->cur_tx - np->dirty_tx < TX_QUEUE_LEN - 4) {
np->tx_full = 0;
netif_resume_tx_queue(dev);
}
if (intr_status & (IntrPCIErr | IntrLinkChange | IntrMIIChange |
IntrStatsMax | IntrTxAbort | IntrTxUnderrun))
netdev_error(dev, intr_status);
if (--boguscnt < 0) {
printk(KERN_WARNING "%s: Too much work at interrupt, "
"status=0x%4.4x.\n",
dev->name, intr_status);
break;
}
} while (1);
if (np->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: exiting interrupt, status=%#4.4x.\n",
dev->name, (int)readw(ioaddr + IntrStatus));
return;
}
static int netdev_rx(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
int entry = np->cur_rx % RX_RING_SIZE;
int boguscnt = np->dirty_rx + RX_RING_SIZE - np->cur_rx;
if (np->msg_level & NETIF_MSG_RX_STATUS) {
printk(KERN_DEBUG " In netdev_rx(), entry %d status %8.8x.\n",
entry, np->rx_head_desc->rx_status);
}
while ( ! (np->rx_head_desc->rx_status & cpu_to_le32(DescOwn))) {
struct rx_desc *desc = np->rx_head_desc;
u32 desc_status = le32_to_cpu(desc->rx_status);
int data_size = desc_status >> 16;
if (np->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  netdev_rx() status is %4.4x.\n",
desc_status);
if (--boguscnt < 0)
break;
if ( (desc_status & (RxWholePkt | RxErr)) !=  RxWholePkt) {
if ((desc_status & RxWholePkt) !=  RxWholePkt) {
printk(KERN_WARNING "%s: Oversized Ethernet frame spanned "
"multiple buffers, entry %#x length %d status %4.4x!\n",
dev->name, np->cur_rx, data_size, desc_status);
printk(KERN_WARNING "%s: Oversized Ethernet frame %p vs %p.\n",
dev->name, np->rx_head_desc,
&np->rx_ring[np->cur_rx % RX_RING_SIZE]);
np->stats.rx_length_errors++;
} else if (desc_status & RxErr) {
if (np->msg_level & NETIF_MSG_RX_ERR)
printk(KERN_DEBUG "  netdev_rx() Rx error was %8.8x.\n",
desc_status);
np->stats.rx_errors++;
if (desc_status & 0x0030) np->stats.rx_length_errors++;
if (desc_status & 0x0048) np->stats.rx_fifo_errors++;
if (desc_status & 0x0004) np->stats.rx_frame_errors++;
if (desc_status & 0x0002) np->stats.rx_crc_errors++;
}
} else {
struct sk_buff *skb;
int pkt_len = data_size - 4;
if (pkt_len < np->rx_copybreak
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
skb_put(skb = np->rx_skbuff[entry], pkt_len);
np->rx_skbuff[entry] = NULL;
}
skb->protocol = eth_type_trans(skb, dev);
{
int rxtype = le32_to_cpu(desc->desc_length);
int csum_bits = rxtype & RxTypeCsumMask;
if (csum_bits == RxTypeUDPSumOK ||
csum_bits == RxTypeTCPSumOK)
skb->ip_summed = CHECKSUM_UNNECESSARY;
}
netif_rx(skb);
dev->last_rx = jiffies;
#if defined(NETSTATS_VER2)
np->stats.rx_bytes += pkt_len;
#endif
np->stats.rx_packets++;
}
entry = (++np->cur_rx) % RX_RING_SIZE;
np->rx_head_desc = &np->rx_ring[entry];
}
for (; np->cur_rx - np->dirty_rx > 0; np->dirty_rx++) {
struct sk_buff *skb;
entry = np->dirty_rx % RX_RING_SIZE;
if (np->rx_skbuff[entry] == NULL) {
skb = dev_alloc_skb(np->rx_buf_sz);
np->rx_skbuff[entry] = skb;
if (skb == NULL)
break;
skb->dev = dev;
np->rx_ring[entry].addr = virt_to_le32desc(skb->tail);
}
np->rx_ring[entry].rx_status = cpu_to_le32(DescOwn);
}
writew(CmdRxDemand | np->chip_cmd, dev->base_addr + ChipCmd);
return 0;
}
static void netdev_error(struct net_device *dev, int intr_status)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
if (intr_status & (IntrMIIChange | IntrLinkChange)) {
if (readb(ioaddr + MIIStatus) & 0x02) {
if (np->drv_flags & HasDavicomPhy)
mdio_write(dev, np->phys[0], 0, 0x3300);
netif_link_down(dev);
} else {
netif_link_up(dev);
check_duplex(dev);
}
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_ERR "%s: MII status changed: Autonegotiation "
"advertising %4.4x  partner %4.4x.\n", dev->name,
mdio_read(dev, np->phys[0], 4),
mdio_read(dev, np->phys[0], 5));
}
if (intr_status & IntrStatsMax) {
np->stats.rx_crc_errors	+= readw(ioaddr + RxCRCErrs);
np->stats.rx_missed_errors	+= readw(ioaddr + RxMissed);
writel(0, ioaddr + RxMissed);
}
if (intr_status & IntrTxAbort) {
writel(virt_to_bus(&np->tx_ring[np->dirty_tx % TX_RING_SIZE]),
ioaddr + TxRingPtr);
writew(CmdTxDemand | np->chip_cmd, dev->base_addr + ChipCmd);
}
if (intr_status & IntrTxUnderrun) {
if (np->tx_thresh < 0xE0)
writeb(np->tx_thresh += 0x20, ioaddr + TxConfig);
if (np->msg_level & NETIF_MSG_TX_ERR)
printk(KERN_INFO "%s: Transmitter underrun, increasing Tx "
"threshold setting to %2.2x.\n", dev->name, np->tx_thresh);
}
if ((intr_status & ~(IntrLinkChange | IntrMIIChange | IntrStatsMax |
IntrTxAbort|IntrTxAborted | IntrNormalSummary))
&& (np->msg_level & NETIF_MSG_DRV)) {
printk(KERN_ERR "%s: Something Wicked happened! %4.4x.\n",
dev->name, intr_status);
writew(CmdTxDemand | np->chip_cmd, dev->base_addr + ChipCmd);
}
}
static struct net_device_stats *get_stats(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
np->stats.rx_crc_errors	+= readw(ioaddr + RxCRCErrs);
np->stats.rx_missed_errors	+= readw(ioaddr + RxMissed);
writel(0, ioaddr + RxMissed);
return &np->stats;
}
static unsigned const ethernet_polynomial = 0x04c11db7U;
static inline u32 ether_crc(int length, unsigned char *data)
{
int crc = -1;
while(--length >= 0) {
unsigned char current_octet = *data++;
int bit;
for (bit = 0; bit < 8; bit++, current_octet >>= 1) {
crc = (crc << 1) ^
((crc < 0) ^ (current_octet & 1) ? ethernet_polynomial : 0);
}
}
return crc;
}
static void set_rx_mode(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
u32 mc_filter[2];
u8 rx_mode;
if (dev->flags & IFF_PROMISC) {
printk(KERN_NOTICE "%s: Promiscuous mode enabled.\n", dev->name);
rx_mode = 0x1C;
} else if ((dev->mc_count > np->multicast_filter_limit)
||  (dev->flags & IFF_ALLMULTI)) {
writel(0xffffffff, ioaddr + MulticastFilter0);
writel(0xffffffff, ioaddr + MulticastFilter1);
rx_mode = 0x0C;
} else {
struct dev_mc_list *mclist;
int i;
memset(mc_filter, 0, sizeof(mc_filter));
for (i = 0, mclist = dev->mc_list; mclist && i < dev->mc_count;
i++, mclist = mclist->next) {
set_bit(ether_crc(ETH_ALEN, mclist->dmi_addr) >> 26,
mc_filter);
}
writel(mc_filter[0], ioaddr + MulticastFilter0);
writel(mc_filter[1], ioaddr + MulticastFilter1);
rx_mode = 0x0C;
}
writeb(np->rx_thresh | rx_mode, ioaddr + RxConfig);
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
if (np->msg_level & NETIF_MSG_IFDOWN)
printk(KERN_DEBUG "%s: Shutting down ethercard, status was %4.4x.\n",
dev->name, readw(ioaddr + ChipCmd));
writeb(np->tx_thresh | 0x01, ioaddr + TxConfig);
writew(0x0000, ioaddr + IntrEnable);
np->chip_cmd = CmdStop;
writew(CmdStop, ioaddr + ChipCmd);
del_timer(&np->timer);
free_irq(dev->irq, dev);
for (i = 0; i < RX_RING_SIZE; i++) {
np->rx_ring[i].rx_status = 0;
np->rx_ring[i].addr = 0xBADF00D0;
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
if (np->tx_buf[i]) {
kfree(np->tx_buf[i]);
np->tx_buf[i] = 0;
}
}
MOD_DEC_USE_COUNT;
return 0;
}
static int via_pwr_event(void *dev_instance, int event)
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
writew(0x0000, ioaddr + IntrEnable);
writew(CmdStop, ioaddr + ChipCmd);
break;
case DRV_RESUME:
set_rx_mode(dev);
netif_start_tx_queue(dev);
writew(np->chip_cmd, ioaddr + ChipCmd);
writew(np->intr_enable, ioaddr + IntrEnable);
break;
case DRV_DETACH: {
struct net_device **devp, **next;
if (dev->flags & IFF_UP) {
dev_close(dev);
dev->flags &= ~(IFF_UP|IFF_RUNNING);
}
unregister_netdev(dev);
release_region(dev->base_addr, pci_tbl[np->chip_id].io_size);
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
return pci_drv_register(&via_rhine_drv_id, NULL);
}
void cleanup_module(void)
{
struct net_device *next_dev;
pci_drv_unregister(&via_rhine_drv_id);
while (root_net_dev) {
struct netdev_private *np = (void *)(root_net_dev->priv);
unregister_netdev(root_net_dev);
#ifdef USE_IO_OPS
release_region(root_net_dev->base_addr, pci_tbl[np->chip_id].io_size);
#else
iounmap((char *)(root_net_dev->base_addr));
#endif
next_dev = np->next_module;
if (np->priv_addr)
kfree(np->priv_addr);
kfree(root_net_dev);
root_net_dev = next_dev;
}
}
#endif