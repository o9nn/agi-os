static const char version1[] =
"myson803.c:v1.05 3/10/2003 Written by Donald Becker <becker@scyld.com>\n";
static const char version2[] =
"  http:
static int debug = 2;
static int max_interrupt_work = 40;
static int multicast_filter_limit = 32;
static int rx_copybreak = 0;
#define MAX_UNITS 8
static int options[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int full_duplex[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
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
#include <linux/delay.h>
#include <asm/processor.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/unaligned.h>
#ifdef INLINE_PCISCAN
#include "k_compat.h"
#else
#include "pci-scan.h"
#include "kern_compat.h"
#endif
#define virt_to_le32desc(addr)  cpu_to_le32(virt_to_bus(addr))
#define le32desc_to_virt(addr)  bus_to_virt(le32_to_cpu(addr))
#if (LINUX_VERSION_CODE >= 0x20100)  &&  defined(MODULE)
char kernel_version[] = UTS_RELEASE;
#endif
#if (LINUX_VERSION_CODE < 0x20100)  ||  ! defined(MODULE)
#define USE_IO_OPS
#endif
MODULE_AUTHOR("Donald Becker <becker@scyld.com>");
MODULE_DESCRIPTION("Myson mtd803 Ethernet driver");
MODULE_LICENSE("GPL");
MODULE_PARM(debug, "i");
MODULE_PARM(options, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(full_duplex, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(max_interrupt_work, "i");
MODULE_PARM(rx_copybreak, "i");
MODULE_PARM(multicast_filter_limit, "i");
MODULE_PARM_DESC(debug, "Driver message level (0-31)");
MODULE_PARM_DESC(options, "Force transceiver type or fixed speed+duplex");
MODULE_PARM_DESC(full_duplex, "Non-zero to force full duplex, "
"non-negotiated link (deprecated).");
MODULE_PARM_DESC(max_interrupt_work,
"Maximum events handled per interrupt");
MODULE_PARM_DESC(rx_copybreak,
"Breakpoint in bytes for copy-only-tiny-frames");
MODULE_PARM_DESC(multicast_filter_limit,
"Multicast addresses before switching to Rx-all-multicast");
static void *myson_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int find_cnt);
static int netdev_pwr_event(void *dev_instance, int event);
enum chip_capability_flags { HasMIIXcvr=1, HasChipXcvr=2 };
#ifdef USE_IO_OPS
#define PCI_IOTYPE (PCI_USES_MASTER | PCI_USES_IO  | PCI_ADDR0)
#define PCI_IOSIZE	256
#else
#define PCI_IOTYPE (PCI_USES_MASTER | PCI_USES_MEM | PCI_ADDR1)
#define PCI_IOSIZE	1024
#endif
static struct pci_id_info pci_id_tbl[] = {
{"Myson mtd803 Fast Ethernet", {0x08031516, 0xffffffff, },
PCI_IOTYPE, PCI_IOSIZE, HasChipXcvr},
{"Myson mtd891 Gigabit Ethernet", {0x08911516, 0xffffffff, },
PCI_IOTYPE, PCI_IOSIZE, HasChipXcvr},
{0,},
};
struct drv_id_info myson803_drv_id = {
"myson803", 0, PCI_CLASS_NETWORK_ETHERNET<<8, pci_id_tbl, myson_probe1,
netdev_pwr_event };
#ifdef USE_IO_OPS
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
enum register_offsets {
StationAddr=0x00, MulticastFilter0=0x08, MulticastFilter1=0x0C,
FlowCtrlAddr=0x10, RxConfig=0x18, TxConfig=0x1a, PCIBusCfg=0x1c,
TxStartDemand=0x20, RxStartDemand=0x24,
RxCurrentPtr=0x28, TxRingPtr=0x2c, RxRingPtr=0x30,
IntrStatus=0x34, IntrEnable=0x38,
FlowCtrlThreshold=0x3c,
MIICtrl=0x40, EECtrl=0x40, RxErrCnts=0x44, TxErrCnts=0x48,
PHYMgmt=0x4c,
};
enum intr_status_bits {
IntrRxErr=0x0002, IntrRxDone=0x0004, IntrTxDone=0x0008,
IntrTxEmpty=0x0010, IntrRxEmpty=0x0020, StatsMax=0x0040, RxEarly=0x0080,
TxEarly=0x0100, RxOverflow=0x0200, TxUnderrun=0x0400,
IntrPCIErr=0x2000, NWayDone=0x4000, LinkChange=0x8000,
};
enum rx_mode_bits {
RxEnable=0x01, RxFilter=0xfe,
AcceptErr=0x02, AcceptRunt=0x08, AcceptBroadcast=0x40,
AcceptMulticast=0x20, AcceptAllPhys=0x80, AcceptMyPhys=0x00,
RxFlowCtrl=0x2000,
TxEnable=0x40000, TxModeFDX=0x00100000, TxThreshold=0x00e00000,
};
enum misc_bits {
BCR_Reset=1,
TxThresholdInc=0x200000,
};
struct netdev_desc {
u32 status;
u32 ctrl_length;
u32 buf_addr;
u32 next_desc;
};
enum desc_status_bits {
DescOwn=0x80000000,
RxDescStartPacket=0x0800, RxDescEndPacket=0x0400, RxDescWholePkt=0x0c00,
RxDescErrSum=0x80, RxErrRunt=0x40, RxErrLong=0x20, RxErrFrame=0x10,
RxErrCRC=0x08, RxErrCode=0x04,
TxErrAbort=0x2000, TxErrCarrier=0x1000, TxErrLate=0x0800,
TxErr16Colls=0x0400, TxErrDefer=0x0200, TxErrHeartbeat=0x0100,
TxColls=0x00ff,
};
enum ctrl_length_bits {
TxIntrOnDone=0x80000000, TxIntrOnFIFO=0x40000000,
TxDescEndPacket=0x20000000, TxDescStartPacket=0x10000000,
TxAppendCRC=0x08000000, TxPadTo64=0x04000000, TxNormalPkt=0x3C000000,
};
#define PRIV_ALIGN	15
struct netdev_private {
struct netdev_desc rx_ring[RX_RING_SIZE];
struct netdev_desc tx_ring[TX_RING_SIZE];
struct net_device *next_module;
void *priv_addr;
struct sk_buff* rx_skbuff[RX_RING_SIZE];
struct sk_buff* tx_skbuff[TX_RING_SIZE];
struct net_device_stats stats;
struct timer_list timer;
int msg_level;
int max_interrupt_work;
int intr_enable;
int chip_id, drv_flags;
struct pci_dev *pci_dev;
struct netdev_desc *rx_head_desc;
unsigned int cur_rx, dirty_rx;
unsigned int rx_buf_sz;
int rx_copybreak;
unsigned int cur_tx, dirty_tx;
unsigned int tx_full:1;
unsigned int rx_died:1;
unsigned int txrx_config;
unsigned int full_duplex:1;
unsigned int duplex_lock:1;
unsigned int medialock:1;
unsigned int default_port;
unsigned int mcast_filter[2];
int multicast_filter_limit;
int mii_cnt;
u16 advertising;
unsigned char phys[2];
};
static int  eeprom_read(long ioaddr, int location);
static int  mdio_read(struct net_device *dev, int phy_id,
unsigned int location);
static void mdio_write(struct net_device *dev, int phy_id,
unsigned int location, int value);
static int  netdev_open(struct net_device *dev);
static void check_duplex(struct net_device *dev);
static void netdev_timer(unsigned long data);
static void tx_timeout(struct net_device *dev);
static void init_ring(struct net_device *dev);
static int  start_tx(struct sk_buff *skb, struct net_device *dev);
static void intr_handler(int irq, void *dev_instance, struct pt_regs *regs);
static void netdev_error(struct net_device *dev, int intr_status);
static int  netdev_rx(struct net_device *dev);
static void netdev_error(struct net_device *dev, int intr_status);
static void set_rx_mode(struct net_device *dev);
static struct net_device_stats *get_stats(struct net_device *dev);
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd);
static int  netdev_close(struct net_device *dev);
static struct net_device *root_net_dev = NULL;
#ifndef MODULE
int myson803_probe(struct net_device *dev)
{
if (pci_drv_register(&myson803_drv_id, dev) < 0)
return -ENODEV;
if (debug >= NETIF_MSG_DRV)
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
return 0;
}
#endif
static void *myson_probe1(struct pci_dev *pdev, void *init_dev,
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
for (i = 0; i < 3; i++)
((u16 *)dev->dev_addr)[i] = le16_to_cpu(eeprom_read(ioaddr, i + 8));
if (memcmp(dev->dev_addr, "\0\0\0\0\0", 6) == 0) {
memcpy(dev->dev_addr, ">Linux", 6);
}
for (i = 0; i < 5; i++)
printk("%2.2x:", dev->dev_addr[i]);
printk("%2.2x, IRQ %d.\n", dev->dev_addr[i], irq);
#if ! defined(final_version)
if (debug > 4)
for (i = 0; i < 0x40; i++)
printk("%4.4x%s",
eeprom_read(ioaddr, i), i % 16 != 15 ? " " : "\n");
#endif
priv_mem = kmalloc(sizeof(*np) + PRIV_ALIGN, GFP_KERNEL);
if (priv_mem == NULL)
return NULL;
#ifdef USE_IO_OPS
request_region(ioaddr, pci_id_tbl[chip_idx].io_size, dev->name);
#endif
writel(BCR_Reset, ioaddr + PCIBusCfg);
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
if (option > 0) {
if (option & 0x220)
np->full_duplex = 1;
np->default_port = option & 0x3ff;
if (np->default_port)
np->medialock = 1;
}
if (card_idx < MAX_UNITS  &&  full_duplex[card_idx] > 0)
np->full_duplex = 1;
if (np->full_duplex) {
if (np->msg_level & NETIF_MSG_PROBE)
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
if (np->drv_flags & HasMIIXcvr) {
int phy, phy_idx = 0;
for (phy = 0; phy < 32 && phy_idx < 4; phy++) {
int mii_status = mdio_read(dev, phy, 1);
if (mii_status != 0xffff  &&  mii_status != 0x0000) {
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
if (np->drv_flags & HasChipXcvr) {
np->phys[np->mii_cnt++] = 32;
printk(KERN_INFO "%s: Internal PHY status 0x%4.4x"
" advertising %4.4x.\n",
dev->name, mdio_read(dev, 32, 1), mdio_read(dev, 32, 4));
}
if (np->default_port & 0x330) {
np->medialock = 1;
if (option & 0x220)
np->full_duplex = 1;
printk(KERN_INFO "  Forcing %dMbs %s-duplex operation.\n",
(option & 0x300 ? 100 : 10),
(np->full_duplex ? "full" : "half"));
if (np->mii_cnt)
mdio_write(dev, np->phys[0], 0,
((option & 0x300) ? 0x2000 : 0) |
(np->full_duplex ? 0x0100 : 0));
}
return dev;
}
#define eeprom_delay(ee_addr)	readl(ee_addr); udelay(2); readl(ee_addr)
enum EEPROM_Ctrl_Bits {
EE_ShiftClk=0x04<<16, EE_ChipSelect=0x88<<16,
EE_DataOut=0x02<<16, EE_DataIn=0x01<<16,
EE_Write0=0x88<<16, EE_Write1=0x8a<<16,
};
enum EEPROM_Cmds { EE_WriteCmd=5, EE_ReadCmd=6, EE_EraseCmd=7, };
static int eeprom_read(long addr, int location)
{
int i;
int retval = 0;
long ee_addr = addr + EECtrl;
int read_cmd = location | (EE_ReadCmd<<6);
writel(EE_ChipSelect, ee_addr);
for (i = 10; i >= 0; i--) {
int dataval = (read_cmd & (1 << i)) ? EE_Write1 : EE_Write0;
writel(dataval, ee_addr);
eeprom_delay(ee_addr);
writel(dataval | EE_ShiftClk, ee_addr);
eeprom_delay(ee_addr);
}
writel(EE_ChipSelect, ee_addr);
eeprom_delay(ee_addr);
for (i = 16; i > 0; i--) {
writel(EE_ChipSelect | EE_ShiftClk, ee_addr);
eeprom_delay(ee_addr);
retval = (retval << 1) | ((readl(ee_addr) & EE_DataIn) ? 1 : 0);
writel(EE_ChipSelect, ee_addr);
eeprom_delay(ee_addr);
}
writel(EE_ChipSelect, ee_addr);
writel(0, ee_addr);
return retval;
}
#define mdio_in(mdio_addr) readl(mdio_addr)
#define mdio_out(value, mdio_addr) writel(value, mdio_addr)
#define mdio_delay(mdio_addr) readl(mdio_addr)
static char mii_preamble_required = 0;
enum mii_reg_bits {
MDIO_ShiftClk=0x0001, MDIO_Data=0x0002, MDIO_EnbOutput=0x0004,
};
#define MDIO_EnbIn  (0)
#define MDIO_WRITE0 (MDIO_EnbOutput)
#define MDIO_WRITE1 (MDIO_Data | MDIO_EnbOutput)
static void mdio_sync(long mdio_addr)
{
int bits = 32;
while (--bits >= 0) {
mdio_out(MDIO_WRITE1, mdio_addr);
mdio_delay(mdio_addr);
mdio_out(MDIO_WRITE1 | MDIO_ShiftClk, mdio_addr);
mdio_delay(mdio_addr);
}
}
static int mdio_read(struct net_device *dev, int phy_id, unsigned int location)
{
long ioaddr = dev->base_addr;
long mdio_addr = ioaddr + MIICtrl;
int mii_cmd = (0xf6 << 10) | (phy_id << 5) | location;
int i, retval = 0;
if (location >= 32)
return 0xffff;
if (phy_id >= 32) {
if (location < 6)
return readw(ioaddr + PHYMgmt + location*2);
else if (location == 16)
return readw(ioaddr + PHYMgmt + 6*2);
else if (location == 17)
return readw(ioaddr + PHYMgmt + 7*2);
else if (location == 18)
return readw(ioaddr + PHYMgmt + 10*2);
else
return 0;
}
if (mii_preamble_required)
mdio_sync(mdio_addr);
for (i = 15; i >= 0; i--) {
int dataval = (mii_cmd & (1 << i)) ? MDIO_WRITE1 : MDIO_WRITE0;
mdio_out(dataval, mdio_addr);
mdio_delay(mdio_addr);
mdio_out(dataval | MDIO_ShiftClk, mdio_addr);
mdio_delay(mdio_addr);
}
for (i = 19; i > 0; i--) {
mdio_out(MDIO_EnbIn, mdio_addr);
mdio_delay(mdio_addr);
retval = (retval << 1) | ((mdio_in(mdio_addr) & MDIO_Data) ? 1 : 0);
mdio_out(MDIO_EnbIn | MDIO_ShiftClk, mdio_addr);
mdio_delay(mdio_addr);
}
return (retval>>1) & 0xffff;
}
static void mdio_write(struct net_device *dev, int phy_id,
unsigned int location, int value)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
long mdio_addr = ioaddr + MIICtrl;
int mii_cmd = (0x5002 << 16) | (phy_id << 23) | (location<<18) | value;
int i;
if (location == 4  &&  phy_id == np->phys[0])
np->advertising = value;
else if (location >= 32)
return;
if (phy_id == 32) {
if (location < 6)
writew(value, ioaddr + PHYMgmt + location*2);
else if (location == 16)
writew(value, ioaddr + PHYMgmt + 6*2);
else if (location == 17)
writew(value, ioaddr + PHYMgmt + 7*2);
return;
}
if (mii_preamble_required)
mdio_sync(mdio_addr);
for (i = 31; i >= 0; i--) {
int dataval = (mii_cmd & (1 << i)) ? MDIO_WRITE1 : MDIO_WRITE0;
mdio_out(dataval, mdio_addr);
mdio_delay(mdio_addr);
mdio_out(dataval | MDIO_ShiftClk, mdio_addr);
mdio_delay(mdio_addr);
}
for (i = 2; i > 0; i--) {
mdio_out(MDIO_EnbIn, mdio_addr);
mdio_delay(mdio_addr);
mdio_out(MDIO_EnbIn | MDIO_ShiftClk, mdio_addr);
mdio_delay(mdio_addr);
}
return;
}
static int netdev_open(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
MOD_INC_USE_COUNT;
writel(~0, ioaddr + IntrStatus);
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
writel(cpu_to_le32(cpu_to_le32(get_unaligned((u32 *)dev->dev_addr))),
ioaddr + StationAddr);
writel(cpu_to_le16(cpu_to_le16(get_unaligned((u16 *)(dev->dev_addr+4)))),
ioaddr + StationAddr + 4);
writel(0x00c28001, ioaddr + FlowCtrlAddr);
writel(0x00000100, ioaddr + FlowCtrlAddr + 4);
writel(0x01f8, ioaddr + PCIBusCfg);
if (dev->if_port == 0)
dev->if_port = np->default_port;
np->txrx_config = TxEnable | RxEnable | RxFlowCtrl | 0x00600000;
np->mcast_filter[0] = np->mcast_filter[1] = 0;
np->rx_died = 0;
set_rx_mode(dev);
netif_start_tx_queue(dev);
np->intr_enable = IntrRxDone | IntrRxErr | IntrRxEmpty | IntrTxDone
| IntrTxEmpty | StatsMax | RxOverflow | TxUnderrun | IntrPCIErr
| NWayDone | LinkChange;
writel(np->intr_enable, ioaddr + IntrEnable);
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: Done netdev_open(), PHY status: %x %x.\n",
dev->name, (int)readw(ioaddr + PHYMgmt),
(int)readw(ioaddr + PHYMgmt + 2));
init_timer(&np->timer);
np->timer.expires = jiffies + 3*HZ;
np->timer.data = (unsigned long)dev;
np->timer.function = &netdev_timer;
add_timer(&np->timer);
return 0;
}
static void check_duplex(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int new_tx_mode = np->txrx_config;
if (np->medialock) {
} else {
int mii_reg5 = mdio_read(dev, np->phys[0], 5);
int negotiated = mii_reg5 & np->advertising;
int duplex = (negotiated & 0x0100) || (negotiated & 0x01C0) == 0x0040;
if (np->duplex_lock  ||  mii_reg5 == 0xffff)
return;
if (duplex)
new_tx_mode |= TxModeFDX;
if (np->full_duplex != duplex) {
np->full_duplex = duplex;
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_INFO "%s: Setting %s-duplex based on MII #%d"
" negotiated capability %4.4x.\n", dev->name,
duplex ? "full" : "half", np->phys[0], negotiated);
}
}
if (np->txrx_config != new_tx_mode)
writel(new_tx_mode, ioaddr + RxConfig);
}
static void netdev_timer(unsigned long data)
{
struct net_device *dev = (struct net_device *)data;
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int next_tick = 10*HZ;
if (np->msg_level & NETIF_MSG_TIMER) {
printk(KERN_DEBUG "%s: Media selection timer tick, status %8.8x.\n",
dev->name, (int)readw(ioaddr + PHYMgmt + 10));
}
if (netif_queue_paused(dev)  &&
np->cur_tx - np->dirty_tx > 1  &&
(jiffies - dev->trans_start) > TX_TIMEOUT) {
tx_timeout(dev);
}
if (np->rx_died)
netdev_rx(dev);
check_duplex(dev);
np->timer.expires = jiffies + next_tick;
add_timer(&np->timer);
}
static void tx_timeout(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
printk(KERN_WARNING "%s: Transmit timed out, status %8.8x,"
" resetting...\n", dev->name, (int)readl(ioaddr + IntrStatus));
if (np->msg_level & NETIF_MSG_TX_ERR) {
int i;
printk(KERN_DEBUG "  Rx ring %p: ", np->rx_ring);
for (i = 0; i < RX_RING_SIZE; i++)
printk(" %8.8x", (unsigned int)np->rx_ring[i].status);
printk("\n"KERN_DEBUG"  Tx ring %p: ", np->tx_ring);
for (i = 0; i < TX_RING_SIZE; i++)
printk(" %8.8x", np->tx_ring[i].status);
printk("\n");
}
writel(np->txrx_config & ~TxEnable, ioaddr + RxConfig);
writel(virt_to_bus(np->tx_ring + (np->dirty_tx%TX_RING_SIZE)),
ioaddr + TxRingPtr);
writel(np->txrx_config, ioaddr + RxConfig);
writel(0, dev->base_addr + TxStartDemand);
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
np->rx_buf_sz = (dev->mtu <= 1532 ? PKT_BUF_SZ : dev->mtu + 4);
np->rx_head_desc = &np->rx_ring[0];
for (i = 0; i < RX_RING_SIZE; i++) {
np->rx_ring[i].ctrl_length = cpu_to_le32(np->rx_buf_sz);
np->rx_ring[i].status = 0;
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
np->rx_ring[i].buf_addr = virt_to_le32desc(skb->tail);
np->rx_ring[i].status = cpu_to_le32(DescOwn);
}
np->dirty_rx = (unsigned int)(i - RX_RING_SIZE);
for (i = 0; i < TX_RING_SIZE; i++) {
np->tx_skbuff[i] = 0;
np->tx_ring[i].status = 0;
np->tx_ring[i].next_desc = virt_to_le32desc(&np->tx_ring[i+1]);
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
np->tx_ring[entry].buf_addr = virt_to_le32desc(skb->data);
np->tx_ring[entry].ctrl_length =
cpu_to_le32(TxIntrOnDone | TxNormalPkt | (skb->len << 11) | skb->len);
np->tx_ring[entry].status = cpu_to_le32(DescOwn);
np->cur_tx++;
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
writel(0, dev->base_addr + TxStartDemand);
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
#if defined(__i386__)  &&  LINUX_VERSION_CODE < 0x020300
if (test_and_set_bit(0, (void*)&dev->interrupt)) {
printk(KERN_ERR"%s: SMP simultaneous entry of an interrupt handler.\n",
dev->name);
dev->interrupt = 0;
return;
}
#endif
do {
u32 intr_status = readl(ioaddr + IntrStatus);
writel(intr_status, ioaddr + IntrStatus);
if (np->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: Interrupt, status %4.4x.\n",
dev->name, intr_status);
if (intr_status == 0)
break;
if (intr_status & IntrRxDone)
netdev_rx(dev);
for (; np->cur_tx - np->dirty_tx > 0; np->dirty_tx++) {
int entry = np->dirty_tx % TX_RING_SIZE;
int tx_status = le32_to_cpu(np->tx_ring[entry].status);
if (tx_status & DescOwn)
break;
if (np->msg_level & NETIF_MSG_TX_DONE)
printk(KERN_DEBUG "%s: Transmit done, Tx status %8.8x.\n",
dev->name, tx_status);
if (tx_status & (TxErrAbort | TxErrCarrier | TxErrLate
| TxErr16Colls | TxErrHeartbeat)) {
if (np->msg_level & NETIF_MSG_TX_ERR)
printk(KERN_DEBUG "%s: Transmit error, Tx status %8.8x.\n",
dev->name, tx_status);
np->stats.tx_errors++;
if (tx_status & TxErrCarrier) np->stats.tx_carrier_errors++;
if (tx_status & TxErrLate) np->stats.tx_window_errors++;
if (tx_status & TxErrHeartbeat) np->stats.tx_heartbeat_errors++;
#ifdef ETHER_STATS
if (tx_status & TxErr16Colls) np->stats.collisions16++;
if (tx_status & TxErrAbort) np->stats.tx_aborted_errors++;
#else
if (tx_status & (TxErr16Colls|TxErrAbort))
np->stats.tx_aborted_errors++;
#endif
} else {
np->stats.tx_packets++;
np->stats.collisions += tx_status & TxColls;
#if LINUX_VERSION_CODE > 0x20127
np->stats.tx_bytes += np->tx_skbuff[entry]->len;
#endif
#ifdef ETHER_STATS
if (tx_status & TxErrDefer) np->stats.tx_deferred++;
#endif
}
dev_free_skb_irq(np->tx_skbuff[entry]);
np->tx_skbuff[entry] = 0;
}
if (np->tx_full  &&  np->cur_tx - np->dirty_tx < TX_QUEUE_LEN - 4) {
np->tx_full = 0;
netif_resume_tx_queue(dev);
}
if (intr_status & (IntrRxErr | IntrRxEmpty | StatsMax | RxOverflow
| TxUnderrun | IntrPCIErr | NWayDone | LinkChange))
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
dev->name, (int)readl(ioaddr + IntrStatus));
#if defined(__i386__)  &&  LINUX_VERSION_CODE < 0x020300
clear_bit(0, (void*)&dev->interrupt);
#endif
return;
}
static int netdev_rx(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
int entry = np->cur_rx % RX_RING_SIZE;
int boguscnt = np->dirty_rx + RX_RING_SIZE - np->cur_rx;
int refilled = 0;
if (np->msg_level & NETIF_MSG_RX_STATUS) {
printk(KERN_DEBUG " In netdev_rx(), entry %d status %4.4x.\n",
entry, np->rx_ring[entry].status);
}
while ( ! (np->rx_head_desc->status & cpu_to_le32(DescOwn))) {
struct netdev_desc *desc = np->rx_head_desc;
u32 desc_status = le32_to_cpu(desc->status);
if (np->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  netdev_rx() status was %8.8x.\n",
desc_status);
if (--boguscnt < 0)
break;
if ((desc_status & RxDescWholePkt) != RxDescWholePkt) {
printk(KERN_WARNING "%s: Oversized Ethernet frame spanned "
"multiple buffers, entry %#x length %d status %4.4x!\n",
dev->name, np->cur_rx, desc_status >> 16, desc_status);
np->stats.rx_length_errors++;
} else if (desc_status & RxDescErrSum) {
if (np->msg_level & NETIF_MSG_RX_ERR)
printk(KERN_DEBUG "  netdev_rx() Rx error was %8.8x.\n",
desc_status);
np->stats.rx_errors++;
if (desc_status & (RxErrLong|RxErrRunt))
np->stats.rx_length_errors++;
if (desc_status & (RxErrFrame|RxErrCode))
np->stats.rx_frame_errors++;
if (desc_status & RxErrCRC)
np->stats.rx_crc_errors++;
} else {
struct sk_buff *skb;
u16 pkt_len = ((desc_status >> 16) & 0xfff) - 4;
#ifndef final_version
if (np->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  netdev_rx() normal Rx pkt length %d"
" of %d, bogus_cnt %d.\n",
pkt_len, pkt_len, boguscnt);
#endif
if (pkt_len < np->rx_copybreak
&& (skb = dev_alloc_skb(pkt_len + 2)) != NULL) {
skb->dev = dev;
skb_reserve(skb, 2);
eth_copy_and_sum(skb, np->rx_skbuff[entry]->tail, pkt_len, 0);
skb_put(skb, pkt_len);
} else {
skb_put(skb = np->rx_skbuff[entry], pkt_len);
np->rx_skbuff[entry] = NULL;
}
#ifndef final_version
if (np->msg_level & NETIF_MSG_PKTDATA)
printk(KERN_DEBUG "  Rx data %2.2x:%2.2x:%2.2x:%2.2x:%2.2x:"
"%2.2x %2.2x:%2.2x:%2.2x:%2.2x:%2.2x:%2.2x %2.2x%2.2x "
"%d.%d.%d.%d.\n",
skb->data[0], skb->data[1], skb->data[2], skb->data[3],
skb->data[4], skb->data[5], skb->data[6], skb->data[7],
skb->data[8], skb->data[9], skb->data[10],
skb->data[11], skb->data[12], skb->data[13],
skb->data[14], skb->data[15], skb->data[16],
skb->data[17]);
#endif
skb->mac.raw = skb->data;
if (0 && ntohs(skb->mac.ethernet->h_proto) >= 0x0800) {
struct ethhdr *eth = skb->mac.ethernet;
skb->protocol = eth->h_proto;
if (desc_status & 0x1000) {
if ((dev->flags & IFF_PROMISC) &&
memcmp(eth->h_dest, dev->dev_addr, ETH_ALEN))
skb->pkt_type = PACKET_OTHERHOST;
} else if (desc_status & 0x2000)
skb->pkt_type = PACKET_BROADCAST;
else if (desc_status & 0x4000)
skb->pkt_type = PACKET_MULTICAST;
} else
skb->protocol = eth_type_trans(skb, dev);
netif_rx(skb);
dev->last_rx = jiffies;
np->stats.rx_packets++;
#if LINUX_VERSION_CODE > 0x20127
np->stats.rx_bytes += pkt_len;
#endif
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
np->rx_ring[entry].buf_addr = virt_to_le32desc(skb->tail);
}
np->rx_ring[entry].ctrl_length = cpu_to_le32(np->rx_buf_sz);
np->rx_ring[entry].status = cpu_to_le32(DescOwn);
refilled++;
}
if (refilled) {
writel(0, dev->base_addr + RxStartDemand);
np->rx_died = 0;
}
return refilled;
}
static void netdev_error(struct net_device *dev, int intr_status)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
if (intr_status & (LinkChange | NWayDone)) {
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_NOTICE "%s: Link changed: Autonegotiation advertising"
" %4.4x  partner %4.4x.\n", dev->name,
mdio_read(dev, np->phys[0], 4),
mdio_read(dev, np->phys[0], 5));
readw(ioaddr + PHYMgmt + 2);
if (readw(ioaddr + PHYMgmt + 2) & 0x0004)
netif_link_up(dev);
else
netif_link_down(dev);
check_duplex(dev);
}
if ((intr_status & TxUnderrun)
&& (np->txrx_config & TxThreshold) != TxThreshold) {
np->txrx_config += TxThresholdInc;
writel(np->txrx_config, ioaddr + RxConfig);
np->stats.tx_fifo_errors++;
}
if (intr_status & IntrRxEmpty) {
printk(KERN_WARNING "%s: Out of receive buffers: no free memory.\n",
dev->name);
np->rx_died = 1;
netdev_rx(dev);
}
if (intr_status & RxOverflow) {
printk(KERN_WARNING "%s: Receiver overflow.\n", dev->name);
np->stats.rx_over_errors++;
netdev_rx(dev);
get_stats(dev);
}
if (intr_status & StatsMax) {
get_stats(dev);
}
if ((intr_status & ~(LinkChange|NWayDone|StatsMax|TxUnderrun|RxOverflow
|TxEarly|RxEarly|0x001e))
&& (np->msg_level & NETIF_MSG_DRV))
printk(KERN_ERR "%s: Something Wicked happened! %4.4x.\n",
dev->name, intr_status);
if (intr_status & IntrPCIErr) {
const char *const pcierr[4] =
{ "Parity Error", "Master Abort", "Target Abort", "Unknown Error" };
if (np->msg_level & NETIF_MSG_DRV)
printk(KERN_WARNING "%s: PCI Bus %s, %x.\n",
dev->name, pcierr[(intr_status>>11) & 3], intr_status);
}
}
static struct net_device_stats *get_stats(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct netdev_private *np = (struct netdev_private *)dev->priv;
unsigned int rxerrs = readl(ioaddr + RxErrCnts);
unsigned int txerrs = readl(ioaddr + TxErrCnts);
np->stats.rx_crc_errors	+= rxerrs >> 16;
np->stats.rx_missed_errors	+= rxerrs & 0xffff;
np->stats.tx_aborted_errors += txerrs >> 24;
np->stats.tx_window_errors += (txerrs >> 16) & 0xff;
np->stats.collisions += txerrs & 0xffff;
return &np->stats;
}
static unsigned const ethernet_polynomial = 0x04c11db7U;
static inline u32 ether_crc(int length, unsigned char *data)
{
u32 crc = ~0;
while(--length >= 0) {
unsigned char current_octet = *data++;
int bit;
for (bit = 0; bit < 8; bit++, current_octet >>= 1)
crc = (crc << 1) ^
((crc < 0) ^ (current_octet & 1) ? ethernet_polynomial : 0);
}
return crc;
}
static void set_rx_mode(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
u32 mc_filter[2];
u32 rx_mode;
if (dev->flags & IFF_PROMISC) {
printk(KERN_NOTICE "%s: Promiscuous mode enabled.\n", dev->name);
mc_filter[1] = mc_filter[0] = ~0;
rx_mode = AcceptBroadcast | AcceptMulticast | AcceptAllPhys
| AcceptMyPhys;
} else if ((dev->mc_count > np->multicast_filter_limit)
||  (dev->flags & IFF_ALLMULTI)) {
mc_filter[1] = mc_filter[0] = ~0;
rx_mode = AcceptBroadcast | AcceptMulticast | AcceptMyPhys;
} else {
struct dev_mc_list *mclist;
int i;
mc_filter[1] = mc_filter[0] = 0;
for (i = 0, mclist = dev->mc_list; mclist && i < dev->mc_count;
i++, mclist = mclist->next) {
set_bit((ether_crc(ETH_ALEN, mclist->dmi_addr) >> 26) & 0x3f,
mc_filter);
}
rx_mode = AcceptBroadcast | AcceptMulticast | AcceptMyPhys;
}
if (mc_filter[0] != np->mcast_filter[0]  ||
mc_filter[1] != np->mcast_filter[1]) {
writel(mc_filter[0], ioaddr + MulticastFilter0);
writel(mc_filter[1], ioaddr + MulticastFilter1);
np->mcast_filter[0] = mc_filter[0];
np->mcast_filter[1] = mc_filter[1];
}
if ((np->txrx_config & RxFilter) != rx_mode) {
np->txrx_config &= ~RxFilter;
np->txrx_config |= rx_mode;
writel(np->txrx_config, ioaddr + RxConfig);
}
}
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
u16 *data = (u16 *)&rq->ifr_data;
u32 *data32 = (void *)&rq->ifr_data;
switch(cmd) {
case 0x8947: case 0x89F0:
data[0] = np->phys[0];
case 0x8948: case 0x89F1:
data[3] = mdio_read(dev, data[0], data[1]);
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
}
mdio_write(dev, data[0], data[1], data[2]);
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
printk(KERN_DEBUG "%s: Shutting down ethercard, status was %8.8x.\n",
dev->name, (int)readl(ioaddr + RxConfig));
printk(KERN_DEBUG "%s: Queue pointers were Tx %d / %d,  Rx %d / %d.\n",
dev->name, np->cur_tx, np->dirty_tx, np->cur_rx, np->dirty_rx);
}
writel(0x0000, ioaddr + IntrEnable);
np->txrx_config = 0;
writel(0, ioaddr + RxConfig);
del_timer(&np->timer);
#ifdef __i386__
if (np->msg_level & NETIF_MSG_IFDOWN) {
printk("\n"KERN_DEBUG"  Tx ring at %8.8x:\n",
(int)virt_to_bus(np->tx_ring));
for (i = 0; i < TX_RING_SIZE; i++)
printk(" #%d desc. %x %x %8.8x.\n",
i, np->tx_ring[i].status, np->tx_ring[i].ctrl_length,
np->tx_ring[i].buf_addr);
printk("\n"KERN_DEBUG "  Rx ring %8.8x:\n",
(int)virt_to_bus(np->rx_ring));
for (i = 0; i < RX_RING_SIZE; i++) {
printk(KERN_DEBUG " #%d desc. %4.4x %4.4x %8.8x\n",
i, np->rx_ring[i].status, np->rx_ring[i].ctrl_length,
np->rx_ring[i].buf_addr);
}
}
#endif
free_irq(dev->irq, dev);
for (i = 0; i < RX_RING_SIZE; i++) {
np->rx_ring[i].status = 0;
np->rx_ring[i].buf_addr = 0xBADF00D0;
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
static int netdev_pwr_event(void *dev_instance, int event)
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
writel(0, ioaddr + IntrEnable);
writel(0, ioaddr + RxConfig);
break;
case DRV_RESUME:
set_rx_mode(dev);
writel(np->intr_enable, ioaddr + IntrEnable);
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
return pci_drv_register(&myson803_drv_id, NULL);
}
void cleanup_module(void)
{
struct net_device *next_dev;
pci_drv_unregister(&myson803_drv_id);
while (root_net_dev) {
struct netdev_private *np = (void *)(root_net_dev->priv);
unregister_netdev(root_net_dev);
#ifdef USE_IO_OPS
release_region(root_net_dev->base_addr,
pci_id_tbl[np->chip_id].io_size);
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