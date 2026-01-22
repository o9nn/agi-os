static const char version1[] =
"natsemi.c:v1.17a 8/09/2003  Written by Donald Becker <becker@scyld.com>\n";
static const char version2[] =
"  http:
static int debug = 2;
static int max_interrupt_work = 20;
static int multicast_filter_limit = 100;
static int rx_copybreak = 0;
#define MAX_UNITS 8
static int options[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int full_duplex[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
#define TX_RING_SIZE 16
#define TX_QUEUE_LEN 10
#define RX_RING_SIZE 32
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
MODULE_DESCRIPTION("National Semiconductor DP83810 series PCI Ethernet driver");
MODULE_LICENSE("GPL");
MODULE_PARM(debug, "i");
MODULE_PARM(options, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(rx_copybreak, "i");
MODULE_PARM(full_duplex, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(multicast_filter_limit, "i");
MODULE_PARM(max_interrupt_work, "i");
MODULE_PARM_DESC(debug, "Driver message level (0-31)");
MODULE_PARM_DESC(options, "Force transceiver type or fixed speed+duplex");
MODULE_PARM_DESC(max_interrupt_work,
"Driver maximum events handled per interrupt");
MODULE_PARM_DESC(full_duplex,
"Non-zero to force full duplex, non-negotiated link "
"(deprecated).");
MODULE_PARM_DESC(rx_copybreak,
"Breakpoint in bytes for copy-only-tiny-frames");
MODULE_PARM_DESC(multicast_filter_limit,
"Multicast addresses before switching to Rx-all-multicast");
static void *natsemi_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int find_cnt);
static int power_event(void *dev_instance, int event);
#ifdef USE_IO_OPS
#define PCI_IOTYPE (PCI_USES_MASTER | PCI_USES_IO | PCI_ADDR0)
#else
#define PCI_IOTYPE (PCI_USES_MASTER | PCI_USES_MEM | PCI_ADDR1)
#endif
static struct pci_id_info pci_id_tbl[] = {
{"Netgear FA311 (NatSemi DP83815)",
{ 0x0020100B, 0xffffffff, 0xf3111385, 0xffffffff, },
PCI_IOTYPE, 256, 0},
{"NatSemi DP83815", { 0x0020100B, 0xffffffff },
PCI_IOTYPE, 256, 0},
{0,},
};
struct drv_id_info natsemi_drv_id = {
"natsemi", PCI_HOTSWAP, PCI_CLASS_NETWORK_ETHERNET<<8, pci_id_tbl,
natsemi_probe1, power_event };
enum register_offsets {
ChipCmd=0x00, ChipConfig=0x04, EECtrl=0x08, PCIBusCfg=0x0C,
IntrStatus=0x10, IntrMask=0x14, IntrEnable=0x18,
TxRingPtr=0x20, TxConfig=0x24,
RxRingPtr=0x30, RxConfig=0x34, ClkRunCtrl=0x3C,
WOLCmd=0x40, PauseCmd=0x44, RxFilterAddr=0x48, RxFilterData=0x4C,
BootRomAddr=0x50, BootRomData=0x54, ChipRevReg=0x58,
StatsCtrl=0x5C, StatsData=0x60,
RxPktErrs=0x60, RxMissed=0x68, RxCRCErrs=0x64,
NS_Xcvr_Mgmt = 0x80, NS_MII_BMCR=0x80, NS_MII_BMSR=0x84,
NS_MII_Advert=0x90, NS_MIILinkPartner=0x94,
};
enum ChipCmdBits {
ChipReset=0x100, SoftIntr=0x80, RxReset=0x20, TxReset=0x10,
RxOff=0x08, RxOn=0x04, TxOff=0x02, TxOn=0x01,
};
enum ChipConfigBits {
CfgLinkGood=0x80000000, CfgFDX=0x20000000,
};
enum intr_status_bits {
IntrRxDone=0x0001, IntrRxIntr=0x0002, IntrRxErr=0x0004, IntrRxEarly=0x0008,
IntrRxIdle=0x0010, IntrRxOverrun=0x0020,
IntrTxDone=0x0040, IntrTxIntr=0x0080, IntrTxErr=0x0100,
IntrTxIdle=0x0200, IntrTxUnderrun=0x0400,
StatsMax=0x0800, IntrDrv=0x1000, WOLPkt=0x2000, LinkChange=0x4000,
RxStatusOverrun=0x10000,
RxResetDone=0x1000000, TxResetDone=0x2000000,
IntrPCIErr=0x00f00000,
IntrNormalSummary=0x0251, IntrAbnormalSummary=0xED20,
};
enum rx_mode_bits {
AcceptErr=0x20, AcceptRunt=0x10,
AcceptBroadcast=0xC0000000,
AcceptMulticast=0x00200000, AcceptAllMulticast=0x20000000,
AcceptAllPhys=0x10000000, AcceptMyPhys=0x08000000,
};
struct netdev_desc {
u32 next_desc;
s32 cmd_status;
u32 buf_addr;
u32 software_use;
};
enum desc_status_bits {
DescOwn=0x80000000, DescMore=0x40000000, DescIntr=0x20000000,
DescNoCRC=0x10000000,
DescPktOK=0x08000000, RxTooLong=0x00400000,
};
#define PRIV_ALIGN 15
struct netdev_private {
struct netdev_desc rx_ring[RX_RING_SIZE];
struct netdev_desc tx_ring[TX_RING_SIZE];
struct net_device *next_module;
void *priv_addr;
const char *product_name;
struct sk_buff* rx_skbuff[RX_RING_SIZE];
struct sk_buff* tx_skbuff[TX_RING_SIZE];
struct net_device_stats stats;
struct timer_list timer;
int msg_level;
int chip_id, drv_flags;
struct pci_dev *pci_dev;
long in_interrupt;
int max_interrupt_work;
int intr_enable;
unsigned int restore_intr_enable:1;
unsigned int rx_q_empty:1;
struct netdev_desc *rx_head_desc;
unsigned int cur_rx, dirty_rx;
unsigned int rx_buf_sz;
int rx_copybreak;
unsigned int cur_tx, dirty_tx;
unsigned int tx_full:1;
unsigned int full_duplex:1;
unsigned int duplex_lock:1;
unsigned int medialock:1;
unsigned int default_port;
u32 cur_rx_mode;
u16 rx_filter[32];
int multicast_filter_limit;
int tx_config, rx_config;
u16 advertising;
};
static int eeprom_read(long ioaddr, int location);
static int mdio_read(struct net_device *dev, int phy_id, int location);
static void mdio_write(struct net_device *dev, int phy_id, int location,
int value);
static int netdev_open(struct net_device *dev);
static void check_duplex(struct net_device *dev);
static void netdev_timer(unsigned long data);
static void tx_timeout(struct net_device *dev);
static int rx_ring_fill(struct net_device *dev);
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
int natsemi_probe(struct net_device *dev)
{
if (pci_drv_register(&natsemi_drv_id, dev) < 0)
return -ENODEV;
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
return 0;
}
#endif
static void *natsemi_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int card_idx)
{
struct net_device *dev;
struct netdev_private *np;
void *priv_mem;
int i, option = card_idx < MAX_UNITS ? options[card_idx] : 0;
int prev_eedata;
dev = init_etherdev(init_dev, 0);
if (!dev)
return NULL;
printk(KERN_INFO "%s: %s at 0x%lx, ",
dev->name, pci_id_tbl[chip_idx].name, ioaddr);
prev_eedata = eeprom_read(ioaddr, 6);
for (i = 0; i < 3; i++) {
int eedata = eeprom_read(ioaddr, i + 7);
dev->dev_addr[i*2] = (eedata << 1) + (prev_eedata >> 15);
dev->dev_addr[i*2+1] = eedata >> 7;
prev_eedata = eedata;
}
for (i = 0; i < 5; i++)
printk("%2.2x:", dev->dev_addr[i]);
printk("%2.2x, IRQ %d.\n", dev->dev_addr[i], irq);
writel(ChipReset, ioaddr + ChipCmd);
priv_mem = kmalloc(sizeof(*np) + PRIV_ALIGN, GFP_KERNEL);
if (priv_mem == NULL)
return NULL;
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
if (np->default_port & 0x330) {
np->medialock = 1;
if (np->msg_level & NETIF_MSG_PROBE)
printk(KERN_INFO "  Forcing %dMbs %s-duplex operation.\n",
(option & 0x300 ? 100 : 10),
(np->full_duplex ? "full" : "half"));
writew(((option & 0x300) ? 0x2000 : 0) |
(np->full_duplex ? 0x0100 : 0),
ioaddr + NS_MII_BMCR);
}
}
if (card_idx < MAX_UNITS && full_duplex[card_idx] > 0)
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
writel(0x8000, ioaddr + ClkRunCtrl);
if ((readl(ioaddr + ChipConfig) & 0xe000) != 0xe000) {
u32 chip_config = readl(ioaddr + ChipConfig);
if (np->msg_level & NETIF_MSG_PROBE)
printk(KERN_INFO "%s: Transceiver default autonegotiation %s "
"10%s %s duplex.\n",
dev->name, chip_config & 0x2000 ? "enabled, advertise"
: "disabled, force", chip_config & 0x4000 ? "0" : "",
chip_config & 0x8000 ? "full" : "half");
}
if (np->msg_level & NETIF_MSG_PROBE)
printk(KERN_INFO "%s: Transceiver status 0x%4.4x partner %4.4x.\n",
dev->name, (int)readl(ioaddr + NS_MII_BMSR),
(int)readl(ioaddr + NS_MIILinkPartner));
return dev;
}
#define eeprom_delay(ee_addr) readl(ee_addr)
enum EEPROM_Ctrl_Bits {
EE_ShiftClk=0x04, EE_DataIn=0x01, EE_ChipSelect=0x08, EE_DataOut=0x02,
};
#define EE_Write0 (EE_ChipSelect)
#define EE_Write1 (EE_ChipSelect | EE_DataIn)
enum EEPROM_Cmds {
EE_WriteCmd=(5 << 6), EE_ReadCmd=(6 << 6), EE_EraseCmd=(7 << 6),
};
static int eeprom_read(long addr, int location)
{
int i;
int retval = 0;
long ee_addr = addr + EECtrl;
int read_cmd = location | EE_ReadCmd;
writel(EE_Write0, ee_addr);
for (i = 10; i >= 0; i--) {
short dataval = (read_cmd & (1 << i)) ? EE_Write1 : EE_Write0;
writel(dataval, ee_addr);
eeprom_delay(ee_addr);
writel(dataval | EE_ShiftClk, ee_addr);
eeprom_delay(ee_addr);
}
writel(EE_ChipSelect, ee_addr);
eeprom_delay(ee_addr);
for (i = 0; i < 16; i++) {
writel(EE_ChipSelect | EE_ShiftClk, ee_addr);
eeprom_delay(ee_addr);
retval |= (readl(ee_addr) & EE_DataOut) ? 1 << i : 0;
writel(EE_ChipSelect, ee_addr);
eeprom_delay(ee_addr);
}
writel(EE_Write0, ee_addr);
writel(0, ee_addr);
return retval;
}
static int mdio_read(struct net_device *dev, int phy_id, int location)
{
if (phy_id == 1 && location < 32)
return readw(dev->base_addr + NS_Xcvr_Mgmt + (location<<2));
else
return 0xffff;
}
static void mdio_write(struct net_device *dev, int phy_id, int location,
int value)
{
if (phy_id == 1 && location < 32)
writew(value, dev->base_addr + NS_Xcvr_Mgmt + (location<<2));
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
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: netdev_open() irq %d.\n",
dev->name, dev->irq);
init_ring(dev);
writel(virt_to_bus(np->rx_ring), ioaddr + RxRingPtr);
writel(virt_to_bus(np->tx_ring), ioaddr + TxRingPtr);
for (i = 0; i < 6; i += 2) {
writel(i, ioaddr + RxFilterAddr);
writel(dev->dev_addr[i] + (dev->dev_addr[i+1] << 8),
ioaddr + RxFilterData);
}
if (readl(ioaddr + ChipRevReg) == 0x0203) {
writew(0x0001, ioaddr + 0xCC);
writew(0x18C9, ioaddr + 0xE4);
writew(0x0000, ioaddr + 0xFC);
writew(0x5040, ioaddr + 0xF4);
writew(0x008C, ioaddr + 0xF8);
}
if (readl(ioaddr + ChipConfig) & CfgFDX) {
np->tx_config = 0xD0801002;
np->rx_config = 0x10000020;
} else {
np->tx_config = 0x10801002;
np->rx_config = 0x0020;
}
if (dev->mtu > 1500)
np->rx_config |= 0x08000000;
writel(np->tx_config, ioaddr + TxConfig);
writel(np->rx_config, ioaddr + RxConfig);
if (dev->if_port == 0)
dev->if_port = np->default_port;
np->in_interrupt = 0;
check_duplex(dev);
set_rx_mode(dev);
netif_start_tx_queue(dev);
np->intr_enable = IntrNormalSummary | IntrAbnormalSummary | 0x1f;
writel(np->intr_enable, ioaddr + IntrMask);
writel(1, ioaddr + IntrEnable);
writel(RxOn | TxOn, ioaddr + ChipCmd);
writel(4, ioaddr + StatsCtrl);
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: Done netdev_open(), status: %x.\n",
dev->name, (int)readl(ioaddr + ChipCmd));
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
int duplex;
if (np->duplex_lock)
return;
duplex = readl(ioaddr + ChipConfig) & 0x20000000 ? 1 : 0;
if (np->full_duplex != duplex) {
np->full_duplex = duplex;
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_INFO "%s: Setting %s-duplex based on negotiated link"
" capability.\n", dev->name,
duplex ? "full" : "half");
if (duplex) {
np->rx_config |= 0x10000000;
np->tx_config |= 0xC0000000;
} else {
np->rx_config &= ~0x10000000;
np->tx_config &= ~0xC0000000;
}
writel(np->tx_config, ioaddr + TxConfig);
writel(np->rx_config, ioaddr + RxConfig);
}
}
static void netdev_timer(unsigned long data)
{
struct net_device *dev = (struct net_device *)data;
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int next_tick = 10*HZ;
if (np->msg_level & NETIF_MSG_TIMER)
printk(KERN_DEBUG "%s: Driver monitor timer tick, status %8.8x.\n",
dev->name, (int)readl(ioaddr + IntrStatus));
if (np->rx_q_empty) {
writel(SoftIntr, ioaddr + ChipCmd);
}
if (netif_queue_paused(dev) &&
np->cur_tx - np->dirty_tx > 1 &&
(jiffies - dev->trans_start) > TX_TIMEOUT) {
tx_timeout(dev);
}
check_duplex(dev);
np->timer.expires = jiffies + next_tick;
add_timer(&np->timer);
}
static void tx_timeout(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
printk(KERN_WARNING "%s: Transmit timed out, status %8.8x,"
" resetting...\n", dev->name, (int)readl(ioaddr + TxRingPtr));
if (np->msg_level & NETIF_MSG_TX_ERR) {
int i;
printk(KERN_DEBUG "  Rx ring %p: ", np->rx_ring);
for (i = 0; i < RX_RING_SIZE; i++)
printk(" %8.8x", (unsigned int)np->rx_ring[i].cmd_status);
printk("\n"KERN_DEBUG"  Tx ring %p: ", np->tx_ring);
for (i = 0; i < TX_RING_SIZE; i++)
printk(" %4.4x", np->tx_ring[i].cmd_status);
printk("\n");
}
dev->trans_start = jiffies;
np->stats.tx_errors++;
return;
}
static int rx_ring_fill(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
unsigned int entry;
for (; np->cur_rx - np->dirty_rx > 0; np->dirty_rx++) {
entry = np->dirty_rx % RX_RING_SIZE;
if (np->rx_skbuff[entry] == NULL) {
struct sk_buff *skb = dev_alloc_skb(np->rx_buf_sz);
np->rx_skbuff[entry] = skb;
if (skb == NULL)
return 1;
skb->dev = dev;
np->rx_ring[entry].buf_addr = virt_to_le32desc(skb->tail);
}
np->rx_ring[entry].cmd_status = cpu_to_le32(DescIntr | np->rx_buf_sz);
}
return 0;
}
static void init_ring(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
int i;
np->tx_full = 0;
np->cur_rx = np->cur_tx = 0;
np->dirty_rx = np->dirty_tx = 0;
np->rx_buf_sz = (dev->mtu <= 1532 ? PKT_BUF_SZ : dev->mtu + 8);
np->rx_head_desc = &np->rx_ring[0];
for (i = 0; i < RX_RING_SIZE; i++) {
np->rx_ring[i].next_desc = virt_to_le32desc(&np->rx_ring[i+1]);
np->rx_ring[i].cmd_status = cpu_to_le32(DescOwn);
np->rx_skbuff[i] = 0;
}
np->rx_ring[i-1].next_desc = virt_to_le32desc(&np->rx_ring[0]);
for (i = 0; i < TX_RING_SIZE; i++) {
np->tx_skbuff[i] = 0;
np->tx_ring[i].next_desc = virt_to_le32desc(&np->tx_ring[i+1]);
np->tx_ring[i].cmd_status = 0;
}
np->tx_ring[i-1].next_desc = virt_to_le32desc(&np->tx_ring[0]);
np->dirty_rx = (unsigned int)(0 - RX_RING_SIZE);
rx_ring_fill(dev);
return;
}
static int start_tx(struct sk_buff *skb, struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
unsigned int entry;
if (netif_pause_tx_queue(dev) != 0) {
if (jiffies - dev->trans_start > TX_TIMEOUT)
tx_timeout(dev);
return 1;
}
entry = np->cur_tx % TX_RING_SIZE;
np->tx_skbuff[entry] = skb;
np->tx_ring[entry].buf_addr = virt_to_le32desc(skb->data);
np->tx_ring[entry].cmd_status = cpu_to_le32(DescOwn|DescIntr | skb->len);
np->cur_tx++;
if (np->cur_tx - np->dirty_tx >= TX_QUEUE_LEN - 1) {
np->tx_full = 1;
if (np->cur_tx - (volatile unsigned int)np->dirty_tx
< TX_QUEUE_LEN - 4) {
np->tx_full = 0;
netif_unpause_tx_queue(dev);
} else
netif_stop_tx_queue(dev);
} else
netif_unpause_tx_queue(dev);
writel(TxOn, dev->base_addr + ChipCmd);
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
do {
u32 intr_status = readl(ioaddr + IntrStatus);
if (intr_status == 0 || intr_status == 0xffffffff)
break;
writel(intr_status & 0x001ffff, ioaddr + IntrStatus);
if (np->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: Interrupt, status %8.8x.\n",
dev->name, intr_status);
if (intr_status & (IntrRxDone | IntrRxIntr)) {
netdev_rx(dev);
np->rx_q_empty = rx_ring_fill(dev);
}
if (intr_status & (IntrRxIdle | IntrDrv)) {
unsigned int old_dirty_rx = np->dirty_rx;
if (rx_ring_fill(dev) == 0)
np->rx_q_empty = 0;
if (np->dirty_rx != old_dirty_rx)
writel(RxOn, dev->base_addr + ChipCmd);
}
for (; np->cur_tx - np->dirty_tx > 0; np->dirty_tx++) {
int entry = np->dirty_tx % TX_RING_SIZE;
int tx_status = le32_to_cpu(np->tx_ring[entry].cmd_status);
if (tx_status & DescOwn)
break;
if (np->msg_level & NETIF_MSG_TX_DONE)
printk(KERN_DEBUG "%s: Transmit done, Tx status %8.8x.\n",
dev->name, tx_status);
if (tx_status & 0x08000000) {
np->stats.tx_packets++;
#if LINUX_VERSION_CODE > 0x20127
np->stats.tx_bytes += np->tx_skbuff[entry]->len;
#endif
} else {
if (np->msg_level & NETIF_MSG_TX_ERR)
printk(KERN_DEBUG "%s: Transmit error, Tx status %8.8x.\n",
dev->name, tx_status);
if (tx_status & 0x04010000) np->stats.tx_aborted_errors++;
if (tx_status & 0x02000000) np->stats.tx_fifo_errors++;
if (tx_status & 0x01000000) np->stats.tx_carrier_errors++;
if (tx_status & 0x00200000) np->stats.tx_window_errors++;
np->stats.tx_errors++;
}
dev_free_skb_irq(np->tx_skbuff[entry]);
np->tx_skbuff[entry] = 0;
}
if (np->tx_full
&& np->cur_tx - np->dirty_tx < TX_QUEUE_LEN - 4) {
np->tx_full = 0;
netif_resume_tx_queue(dev);
}
if (intr_status & IntrAbnormalSummary)
netdev_error(dev, intr_status);
if (--boguscnt < 0) {
printk(KERN_WARNING "%s: Too much work at interrupt, "
"status=0x%4.4x.\n",
dev->name, intr_status);
np->restore_intr_enable = 1;
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
int entry = np->cur_rx % RX_RING_SIZE;
int boguscnt = np->dirty_rx + RX_RING_SIZE - np->cur_rx;
s32 desc_status = le32_to_cpu(np->rx_head_desc->cmd_status);
while (desc_status < 0) {
if (np->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  In netdev_rx() entry %d status was %8.8x.\n",
entry, desc_status);
if (--boguscnt < 0)
break;
if ((desc_status & (DescMore|DescPktOK|RxTooLong)) != DescPktOK) {
if (desc_status & DescMore) {
printk(KERN_WARNING "%s: Oversized(?) Ethernet frame spanned "
"multiple buffers, entry %#x status %x.\n",
dev->name, np->cur_rx, desc_status);
np->stats.rx_length_errors++;
} else {
if (np->msg_level & NETIF_MSG_RX_ERR)
printk(KERN_DEBUG "  netdev_rx() Rx error was %8.8x.\n",
desc_status);
np->stats.rx_errors++;
if (desc_status & 0x06000000) np->stats.rx_over_errors++;
if (desc_status & 0x00600000) np->stats.rx_length_errors++;
if (desc_status & 0x00140000) np->stats.rx_frame_errors++;
if (desc_status & 0x00080000) np->stats.rx_crc_errors++;
}
} else {
struct sk_buff *skb;
int pkt_len = (desc_status & 0x0fff) - 4;
if (pkt_len < np->rx_copybreak
&& (skb = dev_alloc_skb(pkt_len + 2)) != NULL) {
skb->dev = dev;
skb_reserve(skb, 2);
#if defined(HAS_IP_COPYSUM) || (LINUX_VERSION_CODE >= 0x20100)
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
netif_rx(skb);
dev->last_rx = jiffies;
np->stats.rx_packets++;
#if LINUX_VERSION_CODE > 0x20127
np->stats.rx_bytes += pkt_len;
#endif
}
entry = (++np->cur_rx) % RX_RING_SIZE;
np->rx_head_desc = &np->rx_ring[entry];
desc_status = le32_to_cpu(np->rx_head_desc->cmd_status);
}
return 0;
}
static void netdev_error(struct net_device *dev, int intr_status)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
if (intr_status & LinkChange) {
int chip_config = readl(ioaddr + ChipConfig);
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_NOTICE "%s: Link changed: Autonegotiation advertising"
" %4.4x  partner %4.4x.\n", dev->name,
(int)readw(ioaddr + NS_MII_Advert),
(int)readw(ioaddr + NS_MIILinkPartner));
if (chip_config & CfgLinkGood)
netif_link_up(dev);
else
netif_link_down(dev);
check_duplex(dev);
}
if (intr_status & StatsMax) {
get_stats(dev);
}
if (intr_status & IntrTxUnderrun) {
if ((np->tx_config & 0x3f) < 62)
np->tx_config += 2;
writel(np->tx_config, ioaddr + TxConfig);
}
if (intr_status & WOLPkt) {
int wol_status = readl(ioaddr + WOLCmd);
printk(KERN_NOTICE "%s: Link wake-up event %8.8x",
dev->name, wol_status);
}
if (intr_status & (RxStatusOverrun | IntrRxOverrun)) {
if (np->msg_level & NETIF_MSG_DRV)
printk(KERN_ERR "%s: Rx overflow! ns815 %8.8x.\n",
dev->name, intr_status);
np->stats.rx_fifo_errors++;
}
if (intr_status & ~(LinkChange|StatsMax|RxResetDone|TxResetDone|
RxStatusOverrun|0xA7ff)) {
if (np->msg_level & NETIF_MSG_DRV)
printk(KERN_ERR "%s: Something Wicked happened! natsemi %8.8x.\n",
dev->name, intr_status);
}
if (intr_status & IntrPCIErr) {
np->stats.tx_fifo_errors++;
np->stats.rx_fifo_errors++;
}
}
static struct net_device_stats *get_stats(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct netdev_private *np = (struct netdev_private *)dev->priv;
int crc_errs = readl(ioaddr + RxCRCErrs);
if (crc_errs != 0xffffffff) {
np->stats.rx_crc_errors += crc_errs;
np->stats.rx_missed_errors += readl(ioaddr + RxMissed);
}
return &np->stats;
}
static unsigned const ethernet_polynomial = 0x04c11db7U;
static inline u32 ether_crc(int length, unsigned char *data)
{
int crc = -1;
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
long ioaddr = dev->base_addr;
struct netdev_private *np = (struct netdev_private *)dev->priv;
u8 mc_filter[64];
u32 rx_mode;
if (dev->flags & IFF_PROMISC) {
printk(KERN_NOTICE "%s: Promiscuous mode enabled.\n", dev->name);
rx_mode = AcceptBroadcast | AcceptAllMulticast | AcceptAllPhys
| AcceptMyPhys;
} else if ((dev->mc_count > np->multicast_filter_limit)
|| (dev->flags & IFF_ALLMULTI)) {
rx_mode = AcceptBroadcast | AcceptAllMulticast | AcceptMyPhys;
} else {
struct dev_mc_list *mclist;
int i;
memset(mc_filter, 0, sizeof(mc_filter));
for (i = 0, mclist = dev->mc_list; mclist && i < dev->mc_count;
i++, mclist = mclist->next) {
int filterbit = ether_crc(ETH_ALEN, mclist->dmi_addr);
set_bit(filterbit & 0x1ff, mc_filter);
if (np->msg_level & NETIF_MSG_RXFILTER)
printk(KERN_INFO "%s: Added filter for %2.2x:%2.2x:%2.2x:"
"%2.2x:%2.2x:%2.2x  crc %8.8x bit %d.\n", dev->name,
mclist->dmi_addr[0], mclist->dmi_addr[1],
mclist->dmi_addr[2], mclist->dmi_addr[3],
mclist->dmi_addr[4], mclist->dmi_addr[5],
filterbit, filterbit & 0x1ff);
}
rx_mode = AcceptBroadcast | AcceptMulticast | AcceptMyPhys;
for (i = 0; i < 64; i += 2) {
u16 filterword = (mc_filter[i+1]<<8) + mc_filter[i];
if (filterword != np->rx_filter[i>>2]) {
writel(0x200 + i, ioaddr + RxFilterAddr);
writel(filterword, ioaddr + RxFilterData);
np->rx_filter[i>>2] = filterword;
}
}
}
writel(rx_mode, ioaddr + RxFilterAddr);
np->cur_rx_mode = rx_mode;
}
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
u16 *data = (u16 *)&rq->ifr_data;
u32 *data32 = (void *)&rq->ifr_data;
switch(cmd) {
case 0x8947: case 0x89F0:
data[0] = 1;
case 0x8948: case 0x89F1:
data[3] = mdio_read(dev, data[0] & 0x1f, data[1] & 0x1f);
return 0;
case 0x8949: case 0x89F2:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
if (data[0] == 1) {
u16 miireg = data[1] & 0x1f;
u16 value = data[2];
mdio_write(dev, 1, miireg, value);
switch (miireg) {
case 0:
np->duplex_lock = (value & 0x9000) ? 0 : 1;
if (np->duplex_lock)
np->full_duplex = (value & 0x0100) ? 1 : 0;
break;
case 4: np->advertising = value; break;
}
}
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
printk(KERN_DEBUG "%s: Shutting down ethercard, status was %4.4x "
"Int %2.2x.\n",
dev->name, (int)readl(ioaddr + ChipCmd),
(int)readl(ioaddr + IntrStatus));
printk(KERN_DEBUG "%s: Queue pointers were Tx %d / %d,  Rx %d / %d.\n",
dev->name, np->cur_tx, np->dirty_tx, np->cur_rx, np->dirty_rx);
}
del_timer(&np->timer);
writel(0, ioaddr + IntrMask);
writel(0, ioaddr + IntrEnable);
writel(2, ioaddr + StatsCtrl);
writel(RxOff | TxOff, ioaddr + ChipCmd);
get_stats(dev);
#ifdef __i386__
if (np->msg_level & NETIF_MSG_IFDOWN) {
printk("\n"KERN_DEBUG"  Tx ring at %8.8x:\n",
(int)virt_to_bus(np->tx_ring));
for (i = 0; i < TX_RING_SIZE; i++)
printk(" #%d desc. %8.8x %8.8x.\n",
i, np->tx_ring[i].cmd_status, (u32)np->tx_ring[i].buf_addr);
printk("\n"KERN_DEBUG "  Rx ring %8.8x:\n",
(int)virt_to_bus(np->rx_ring));
for (i = 0; i < RX_RING_SIZE; i++) {
printk(KERN_DEBUG " #%d desc. %8.8x %8.8x\n",
i, np->rx_ring[i].cmd_status, (u32)np->rx_ring[i].buf_addr);
}
}
#endif
free_irq(dev->irq, dev);
for (i = 0; i < RX_RING_SIZE; i++) {
np->rx_ring[i].cmd_status = 0;
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
#if 0
writel(0x0200, ioaddr + ChipConfig);
#endif
MOD_DEC_USE_COUNT;
return 0;
}
static int power_event(void *dev_instance, int event)
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
writel(2, ioaddr + StatsCtrl);
writel(RxOff | TxOff, ioaddr + ChipCmd);
break;
case DRV_RESUME:
set_rx_mode(dev);
writel(np->intr_enable, ioaddr + IntrEnable);
writel(1, ioaddr + IntrEnable);
writel(RxOn | TxOn, ioaddr + ChipCmd);
break;
case DRV_DETACH: {
struct net_device **devp, **next;
if (dev->flags & IFF_UP) {
dev_close(dev);
dev->flags &= ~(IFF_UP|IFF_RUNNING);
}
unregister_netdev(dev);
release_region(dev->base_addr, pci_id_tbl[np->chip_id].io_size);
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
#ifdef CARDBUS
register_driver(&etherdev_ops);
return 0;
#else
return pci_drv_register(&natsemi_drv_id, NULL);
#endif
}
void cleanup_module(void)
{
struct net_device *next_dev;
#ifdef CARDBUS
unregister_driver(&etherdev_ops);
#else
pci_drv_unregister(&natsemi_drv_id);
#endif
while (root_net_dev) {
struct netdev_private *np = (void *)(root_net_dev->priv);
unregister_netdev(root_net_dev);
iounmap((char *)root_net_dev->base_addr);
next_dev = np->next_module;
if (np->priv_addr)
kfree(np->priv_addr);
kfree(root_net_dev);
root_net_dev = next_dev;
}
}
#endif