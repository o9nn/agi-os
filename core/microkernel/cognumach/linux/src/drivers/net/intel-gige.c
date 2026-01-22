static const char version1[] =
"intel-gige.c:v0.14 11/17/2002 Written by Donald Becker <becker@scyld.com>\n";
static const char version2[] =
"  http:
static int debug = 2;
static int max_interrupt_work = 20;
static int multicast_filter_limit = 15;
static int rx_copybreak = 0;
#define MAX_UNITS 8
static int options[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int full_duplex[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int rx_intr_holdoff = 0;
static int tx_intr_holdoff = 128;
#if ! defined(final_version)
#define TX_RING_SIZE	8
#define TX_QUEUE_LEN	5
#define RX_RING_SIZE	4
#else
#define TX_RING_SIZE	16
#define TX_QUEUE_LEN	10
#define RX_RING_SIZE	32
#endif
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
#define virt_to_le32desc(addr)  cpu_to_le32(virt_to_bus(addr))
#define le32desc_to_virt(addr)  bus_to_virt(le32_to_cpu(addr))
#if (LINUX_VERSION_CODE >= 0x20100)  &&  defined(MODULE)
char kernel_version[] = UTS_RELEASE;
#endif
MODULE_AUTHOR("Donald Becker <becker@scyld.com>");
MODULE_DESCRIPTION("Intel Gigabit Ethernet driver");
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
"Non-zero to set forced full duplex (deprecated).");
MODULE_PARM_DESC(rx_copybreak,
"Breakpoint in bytes for copy-only-tiny-frames");
MODULE_PARM_DESC(multicast_filter_limit,
"Multicast addresses before switching to Rx-all-multicast");
static void *igige_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int find_cnt);
static int netdev_pwr_event(void *dev_instance, int event);
enum chip_capability_flags { CanHaveMII=1, };
#define PCI_IOTYPE ()
static struct pci_id_info pci_id_tbl[] = {
{"Intel Gigabit Ethernet adapter", {0x10008086, 0xffffffff, },
PCI_USES_MASTER | PCI_USES_MEM | PCI_ADDR0, 0x1ffff, 0},
{0,},
};
struct drv_id_info igige_drv_id = {
"intel-gige", PCI_HOTSWAP, PCI_CLASS_NETWORK_ETHERNET<<8, pci_id_tbl,
igige_probe1, netdev_pwr_event };
#ifdef USE_IO_OPS
#error This driver only works with PCI memory space access.
#endif
enum register_offsets {
ChipCtrl=0x00, ChipStatus=0x08, EECtrl=0x10,
FlowCtrlAddrLo=0x028, FlowCtrlAddrHi=0x02c, FlowCtrlType=0x030,
VLANetherType=0x38,
RxAddrCAM=0x040,
IntrStatus=0x0C0,
IntrEnable=0x0D0,
IntrDisable=0x0D8,
RxControl=0x100,
RxQ0IntrDelay=0x108,
RxRingPtr=0x110,
RxRingLen=0x118,
RxDescHead=0x120,
RxDescTail=0x128,
RxQ1IntrDelay=0x130,
RxRing1Ptr=0x138,
RxRing1Len=0x140,
RxDesc1Head=0x148,
RxDesc1Tail=0x150,
FlowCtrlTimer=0x170, FlowCtrlThrshHi=0x160, FlowCtrlThrshLo=0x168,
TxConfigReg=0x178,
RxConfigReg=0x180,
MulticastArray=0x200,
TxControl=0x400,
TxQState=0x408,
TxIPG=0x410,
TxRingPtr=0x420, TxRingLen=0x428,
TxDescHead=0x430, TxDescTail=0x438, TxIntrDelay=0x440,
RxCRCErrs=0x4000, RxMissed=0x4010,
TxStatus=0x408,
RxStatus=0x180,
};
enum intr_status_bits {
IntrTxDone=0x0001,
IntrLinkChange=0x0004,
IntrRxSErr=0x0008,
IntrRxEmpty=0x0010,
IntrRxQ1Empty=0x0020,
IntrRxDone=0x0080,
IntrRxDoneQ1=0x0100,
IntrPCIErr=0x0200,
IntrTxEmpty=0x0002,
StatsMax=0x1000,
};
enum rx_mode_bits {
RxCtrlReset=0x01, RxCtrlEnable=0x02, RxCtrlAllUnicast=0x08,
RxCtrlAllMulticast=0x10,
RxCtrlLoopback=0xC0,
RxCtrlAcceptBroadcast=0x8000,
AcceptAllPhys=0x08,	AcceptAllMulticast=0x10, AcceptBroadcast=0x8000,
AcceptMyPhys=0,
AcceptMulticast=0,
};
struct rx_desc {
u32 buf_addr;
u32 buf_addr_hi;
u32 csum_length;
u32 status;
};
struct tx_desc {
u32 buf_addr;
u32 buf_addr_hi;
u32 cmd_length;
u32 status;
};
enum tx_cmd_bits {
TxDescEndPacket=0x02000000, TxCmdIntrDelay=0x80000000,
TxCmdAddCRC=0x02000000, TxCmdDoTx=0x13000000,
};
enum tx_status_bits {
TxDescDone=0x0001, TxDescEndPkt=0x0002,
};
enum rx_status_bits {
RxDescDone=0x0001, RxDescEndPkt=0x0002,
};
#define PRIV_ALIGN	15
struct netdev_private {
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
int max_interrupt_work;
int intr_enable;
long in_interrupt;
struct rx_desc *rx_ring;
struct rx_desc *rx_head_desc;
unsigned int cur_rx, dirty_rx;
unsigned int rx_buf_sz;
int rx_copybreak;
struct tx_desc *tx_ring;
unsigned int cur_tx, dirty_tx;
unsigned int tx_full:1;
unsigned int rx_mode;
unsigned int tx_config;
int multicast_filter_limit;
unsigned int full_duplex:1;
unsigned int duplex_lock:1;
unsigned int medialock:1;
unsigned int default_port;
};
static int  eeprom_read(long ioaddr, int location);
static int  netdev_open(struct net_device *dev);
static int  change_mtu(struct net_device *dev, int new_mtu);
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
int igige_probe(struct net_device *dev)
{
if (pci_drv_register(&igige_drv_id, dev) < 0)
return -ENODEV;
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
return 0;
}
#endif
static void *igige_probe1(struct pci_dev *pdev, void *init_dev,
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
((u16*)dev->dev_addr)[i] = le16_to_cpu(eeprom_read(ioaddr, i));
for (i = 0; i < 5; i++)
printk("%2.2x:", dev->dev_addr[i]);
printk("%2.2x, IRQ %d.\n", dev->dev_addr[i], irq);
priv_mem = kmalloc(sizeof(*np) + PRIV_ALIGN, GFP_KERNEL);
if (priv_mem == NULL)
return NULL;
request_region(ioaddr, pci_id_tbl[chip_idx].io_size, dev->name);
writel(0x04000000, ioaddr + ChipCtrl);
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
if (option & 0x2220)
np->full_duplex = 1;
np->default_port = option & 0x3330;
if (np->default_port)
np->medialock = 1;
}
if (card_idx < MAX_UNITS  &&  full_duplex[card_idx] > 0)
np->full_duplex = 1;
if (np->full_duplex)
np->duplex_lock = 1;
#if ! defined(final_version)
if (np->msg_level & NETIF_MSG_MISC) {
int sum = 0;
for (i = 0; i < 0x40; i++) {
int eeval = eeprom_read(ioaddr, i);
printk("%4.4x%s", eeval, i % 16 != 15 ? " " : "\n");
sum += eeval;
}
printk(KERN_DEBUG "%s:  EEPROM checksum %4.4X (expected value 0xBABA).\n",
dev->name, sum & 0xffff);
}
#endif
dev->open = &netdev_open;
dev->hard_start_xmit = &start_tx;
dev->stop = &netdev_close;
dev->get_stats = &get_stats;
dev->set_multicast_list = &set_rx_mode;
dev->do_ioctl = &mii_ioctl;
dev->change_mtu = &change_mtu;
writel(0x04000000, ioaddr + VLANetherType);
for (i = 0x600; i < 0x800; i+=4)
writel(0, ioaddr + i);
np->tx_config = 0x80000020;
writel(np->tx_config, ioaddr + TxConfigReg);
{
int eeword10 = eeprom_read(ioaddr, 10);
writel(((eeword10 & 0x01e0) << 17) | ((eeword10 & 0x0010) << 3),
ioaddr + ChipCtrl);
}
return dev;
}
#define eeprom_delay(ee_addr)	readl(ee_addr)
enum EEPROM_Ctrl_Bits {
EE_ShiftClk=0x01, EE_ChipSelect=0x02, EE_DataIn=0x08, EE_DataOut=0x04,
};
#define EE_Write0 (EE_ChipSelect)
#define EE_Write1 (EE_ChipSelect | EE_DataOut)
enum EEPROM_Cmds { EE_WriteCmd=5, EE_ReadCmd=6, EE_EraseCmd=7, };
static int eeprom_read(long addr, int location)
{
int i;
int retval = 0;
long ee_addr = addr + EECtrl;
int read_cmd = ((EE_ReadCmd<<6) | location) << 16 ;
int cmd_len = 2+6+16;
u32 baseval = readl(ee_addr) & ~0x0f;
writel(EE_Write0 | baseval, ee_addr);
for (i = cmd_len; i >= 0; i--) {
int dataval = baseval |
((read_cmd & (1 << i)) ? EE_Write1 : EE_Write0);
writel(dataval, ee_addr);
eeprom_delay(ee_addr);
writel(dataval | EE_ShiftClk, ee_addr);
eeprom_delay(ee_addr);
retval = (retval << 1) | ((readl(ee_addr) & EE_DataIn) ? 1 : 0);
}
writel(baseval | EE_Write0, ee_addr);
writel(baseval & ~EE_ChipSelect, ee_addr);
return retval;
}
static int netdev_open(struct net_device *dev)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
MOD_INC_USE_COUNT;
if (np->tx_ring == 0)
np->tx_ring = (void *)get_free_page(GFP_KERNEL);
if (np->tx_ring == 0)
return -ENOMEM;
if (np->rx_ring == 0)
np->rx_ring = (void *)get_free_page(GFP_KERNEL);
if (np->tx_ring == 0) {
free_page((long)np->tx_ring);
return -ENOMEM;
}
if (request_irq(dev->irq, &intr_handler, SA_SHIRQ, dev->name, dev)) {
MOD_DEC_USE_COUNT;
return -EAGAIN;
}
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: netdev_open() irq %d.\n",
dev->name, dev->irq);
init_ring(dev);
writel(0, ioaddr + RxControl);
writel(virt_to_bus(np->rx_ring), ioaddr + RxRingPtr);
#if ADDRLEN == 64
writel(virt_to_bus(np->rx_ring) >> 32, ioaddr + RxRingPtr + 4);
#else
writel(0, ioaddr + RxRingPtr + 4);
#endif
writel(RX_RING_SIZE * sizeof(struct rx_desc), ioaddr + RxRingLen);
writel(0x80000000 | rx_intr_holdoff, ioaddr + RxQ0IntrDelay);
writel(0, ioaddr + RxDescHead);
writel(np->dirty_rx + RX_RING_SIZE, ioaddr + RxDescTail);
writel(0, ioaddr + RxQ1IntrDelay);
writel(0, ioaddr + RxRing1Ptr);
writel(0, ioaddr + RxRing1Ptr + 4);
writel(0, ioaddr + RxRing1Len);
writel(0, ioaddr + RxDesc1Head);
writel(0, ioaddr + RxDesc1Tail);
writel(0x000400FA, ioaddr + TxControl);
writel(virt_to_bus(np->tx_ring), ioaddr + TxRingPtr);
#if ADDRLEN == 64
writel(virt_to_bus(np->tx_ring) >> 32, ioaddr + TxRingPtr + 4);
#else
writel(0, ioaddr + TxRingPtr + 4);
#endif
writel(TX_RING_SIZE * sizeof(struct tx_desc), ioaddr + TxRingLen);
writel(0, ioaddr + TxDescHead);
writel(0, ioaddr + TxDescTail);
writel(0, ioaddr + TxQState);
writel(0, ioaddr + TxQState + 4);
writel(0x00A0080A, ioaddr + TxIPG);
writel(tx_intr_holdoff, ioaddr + TxIntrDelay);
writel(((u32*)dev->dev_addr)[0], ioaddr + RxAddrCAM);
writel(0x80000000 | ((((u32*)dev->dev_addr)[1]) & 0xffff),
ioaddr + RxAddrCAM + 4);
if (dev->if_port == 0)
dev->if_port = np->default_port;
np->in_interrupt = 0;
np->rx_mode = RxCtrlEnable;
set_rx_mode(dev);
np->tx_config = 0x80000020;
writel(np->tx_config, ioaddr + TxConfigReg);
writel(0x00C28001, ioaddr + FlowCtrlAddrLo);
writel(0x00000100, ioaddr + FlowCtrlAddrHi);
writel(0x8808, ioaddr + FlowCtrlType);
writel(0x0100, ioaddr + FlowCtrlTimer);
writel(0x8000, ioaddr + FlowCtrlThrshHi);
writel(0x4000, ioaddr + FlowCtrlThrshLo);
netif_start_tx_queue(dev);
writel(IntrTxDone | IntrLinkChange | IntrRxDone | IntrPCIErr
| IntrRxEmpty | IntrRxSErr, ioaddr + IntrEnable);
if (np->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: Done netdev_open(), status: %x Rx %x Tx %x.\n",
dev->name, (int)readl(ioaddr + ChipStatus),
(int)readl(ioaddr + RxStatus), (int)readl(ioaddr + TxStatus));
init_timer(&np->timer);
np->timer.expires = jiffies + 3*HZ;
np->timer.data = (unsigned long)dev;
np->timer.function = &netdev_timer;
add_timer(&np->timer);
return 0;
}
static int change_mtu(struct net_device *dev, int new_mtu)
{
if ((new_mtu < 68) || (new_mtu > 1500))
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
int chip_ctrl = readl(ioaddr + ChipCtrl);
int rx_cfg = readl(ioaddr + RxConfigReg);
int tx_cfg = readl(ioaddr + TxConfigReg);
#if 0
int chip_status = readl(ioaddr + ChipStatus);
#endif
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_DEBUG "%s:  Link changed status.  Ctrl %x rxcfg %8.8x "
"txcfg %8.8x.\n",
dev->name, chip_ctrl, rx_cfg, tx_cfg);
if (np->medialock) {
if (np->full_duplex)
;
}
}
static void netdev_timer(unsigned long data)
{
struct net_device *dev = (struct net_device *)data;
struct netdev_private *np = (struct netdev_private *)dev->priv;
long ioaddr = dev->base_addr;
int next_tick = 10*HZ;
if (np->msg_level & NETIF_MSG_TIMER) {
printk(KERN_DEBUG "%s: Media selection timer tick, status %8.8x, "
"Tx %x Rx %x.\n",
dev->name, (int)readl(ioaddr + ChipStatus),
(int)readl(ioaddr + TxStatus), (int)readl(ioaddr + RxStatus));
}
if ((jiffies - dev->trans_start) > TX_TIMEOUT  &&
(np->cur_tx - np->dirty_tx > 0  ||
netif_queue_paused(dev)) ) {
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
" resetting...\n", dev->name, (int)readl(ioaddr + ChipStatus));
#ifndef __alpha__
if (np->msg_level & NETIF_MSG_TX_ERR) {
int i;
printk(KERN_DEBUG "  Tx registers: ");
for (i = 0x400; i < 0x444; i += 8)
printk(" %8.8x", (int)readl(ioaddr + i));
printk("\n"KERN_DEBUG "  Rx ring %p: ", np->rx_ring);
for (i = 0; i < RX_RING_SIZE; i++)
printk(" %8.8x", (unsigned int)np->rx_ring[i].status);
printk("\n"KERN_DEBUG"  Tx ring %p: ", np->tx_ring);
for (i = 0; i < TX_RING_SIZE; i++)
printk(" %4.4x", np->tx_ring[i].status);
printk("\n");
}
#endif
dev->if_port = 0;
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
np->rx_buf_sz = (dev->mtu <= 1500 ? PKT_BUF_SZ : dev->mtu + 32);
np->rx_head_desc = &np->rx_ring[0];
for (i = 0; i < RX_RING_SIZE; i++) {
np->rx_skbuff[i] = 0;
}
for (i = 0; i < RX_RING_SIZE; i++) {
struct sk_buff *skb = dev_alloc_skb(np->rx_buf_sz);
np->rx_skbuff[i] = skb;
if (skb == NULL)
break;
skb->dev = dev;
skb_reserve(skb, 2);
np->rx_ring[i].buf_addr = virt_to_le32desc(skb->tail);
np->rx_ring[i].buf_addr_hi = 0;
np->rx_ring[i].status = 0;
}
np->dirty_rx = (unsigned int)(i - RX_RING_SIZE);
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
np->tx_ring[entry].buf_addr = virt_to_le32desc(skb->data);
np->tx_ring[entry].buf_addr_hi = 0;
np->tx_ring[entry].cmd_length = cpu_to_le32(TxCmdDoTx | skb->len);
np->tx_ring[entry].status = 0;
np->cur_tx++;
if (np->cur_tx - np->dirty_tx >= TX_QUEUE_LEN - 1) {
np->tx_full = 1;
if (np->cur_tx - (volatile int)np->dirty_tx < TX_QUEUE_LEN - 2) {
netif_unpause_tx_queue(dev);
np->tx_full = 0;
} else
netif_stop_tx_queue(dev);
} else
netif_unpause_tx_queue(dev);
if (np->msg_level & NETIF_MSG_TX_QUEUED)
printk(KERN_DEBUG "%s: Tx queued to slot %d, desc tail now %d "
"writing %d.\n",
dev->name, entry, (int)readl(dev->base_addr + TxDescTail),
np->cur_tx % TX_RING_SIZE);
writel(np->cur_tx % TX_RING_SIZE, dev->base_addr + TxDescTail);
dev->trans_start = jiffies;
if (np->msg_level & NETIF_MSG_TX_QUEUED) {
printk(KERN_DEBUG "%s: Transmit frame #%d (%x) queued in slot %d.\n",
dev->name, np->cur_tx, (int)virt_to_bus(&np->tx_ring[entry]),
entry);
}
return 0;
}
static void intr_handler(int irq, void *dev_instance, struct pt_regs *rgs)
{
struct net_device *dev = (struct net_device *)dev_instance;
struct netdev_private *np;
long ioaddr;
int work_limit;
ioaddr = dev->base_addr;
np = (struct netdev_private *)dev->priv;
work_limit = np->max_interrupt_work;
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
if (np->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: Interrupt, status %4.4x.\n",
dev->name, intr_status);
if (intr_status == 0 || intr_status == 0xffffffff)
break;
if (intr_status & IntrRxDone)
netdev_rx(dev);
for (; np->cur_tx - np->dirty_tx > 0; np->dirty_tx++) {
int entry = np->dirty_tx % TX_RING_SIZE;
if (np->tx_ring[entry].status == 0)
break;
if (np->msg_level & NETIF_MSG_TX_DONE)
printk(KERN_DEBUG "%s: Transmit done, Tx status %8.8x.\n",
dev->name, np->tx_ring[entry].status);
np->stats.tx_packets++;
#if LINUX_VERSION_CODE > 0x20127
np->stats.tx_bytes += np->tx_skbuff[entry]->len;
#endif
dev_free_skb_irq(np->tx_skbuff[entry]);
np->tx_skbuff[entry] = 0;
}
if (np->tx_full  &&  np->cur_tx - np->dirty_tx < TX_QUEUE_LEN - 4) {
np->tx_full = 0;
netif_resume_tx_queue(dev);
}
if (intr_status & (IntrPCIErr | IntrLinkChange | StatsMax))
netdev_error(dev, intr_status);
if (--work_limit < 0) {
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
if (np->msg_level & NETIF_MSG_RX_STATUS) {
printk(KERN_DEBUG " In netdev_rx(), entry %d status %4.4x.\n",
entry, np->rx_ring[entry].status);
}
while (np->rx_head_desc->status & cpu_to_le32(RxDescDone)) {
struct rx_desc *desc = np->rx_head_desc;
u32 desc_status = le32_to_cpu(desc->status);
int data_size = le32_to_cpu(desc->csum_length);
if (np->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  netdev_rx() status was %8.8x.\n",
desc_status);
if (--boguscnt < 0)
break;
if ( ! (desc_status & RxDescEndPkt)) {
printk(KERN_WARNING "%s: Oversized Ethernet frame spanned "
"multiple buffers, entry %#x length %d status %4.4x!\n",
dev->name, np->cur_rx, data_size, desc_status);
np->stats.rx_length_errors++;
} else {
struct sk_buff *skb;
int pkt_len = (data_size & 0xffff) - 4;
#ifndef final_version
if (np->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  netdev_rx() normal Rx pkt length %d"
" of %d, bogus_cnt %d.\n",
pkt_len, data_size, boguscnt);
#endif
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
char *temp = skb_put(skb = np->rx_skbuff[entry], pkt_len);
np->rx_skbuff[entry] = NULL;
#ifndef final_version
if (le32desc_to_virt(np->rx_ring[entry].buf_addr) != temp)
printk(KERN_ERR "%s: Internal fault: The skbuff addresses "
"do not match in netdev_rx: %p vs. %p / %p.\n",
dev->name,
le32desc_to_virt(np->rx_ring[entry].buf_addr),
skb->head, temp);
#endif
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
skb_reserve(skb, 2);
np->rx_ring[entry].buf_addr = virt_to_le32desc(skb->tail);
}
np->rx_ring[entry].status = 0;
}
return 0;
}
static void netdev_error(struct net_device *dev, int intr_status)
{
long ioaddr = dev->base_addr;
struct netdev_private *np = (struct netdev_private *)dev->priv;
if (intr_status & IntrLinkChange) {
int chip_ctrl = readl(ioaddr + ChipCtrl);
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_ERR "%s: Link changed: Autonegotiation on-going.\n",
dev->name);
if (chip_ctrl & 1)
netif_link_up(dev);
else
netif_link_down(dev);
check_duplex(dev);
}
if (intr_status & StatsMax) {
get_stats(dev);
}
if ((intr_status & ~(IntrLinkChange|StatsMax))
&& (np->msg_level & NETIF_MSG_DRV))
printk(KERN_ERR "%s: Something Wicked happened! %4.4x.\n",
dev->name, intr_status);
if (intr_status & IntrPCIErr)
np->stats.tx_fifo_errors++;
}
static struct net_device_stats *get_stats(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct netdev_private *np = (struct netdev_private *)dev->priv;
int crc_errs = readl(ioaddr + RxCRCErrs);
if (crc_errs != 0xffffffff) {
np->stats.rx_crc_errors	+= readl(ioaddr + RxCRCErrs);
np->stats.rx_missed_errors	+= readl(ioaddr + RxMissed);
}
return &np->stats;
}
static unsigned const ethernet_polynomial_le = 0xedb88320U;
static inline unsigned ether_crc_le(int length, unsigned char *data)
{
unsigned int crc = 0xffffffff;
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
long ioaddr = dev->base_addr;
struct netdev_private *np = (struct netdev_private *)dev->priv;
u32 new_mc_filter[128];
u32 new_rx_mode = np->rx_mode;
if (dev->flags & IFF_PROMISC) {
printk(KERN_NOTICE "%s: Promiscuous mode enabled.\n", dev->name);
new_rx_mode |=
RxCtrlAcceptBroadcast | RxCtrlAllMulticast | RxCtrlAllUnicast;
} else if ((dev->mc_count > np->multicast_filter_limit)
||  (dev->flags & IFF_ALLMULTI)) {
new_rx_mode &= ~RxCtrlAllUnicast;
new_rx_mode |= RxCtrlAcceptBroadcast | RxCtrlAllMulticast;
} else {
struct dev_mc_list *mclist;
int i;
memset(new_mc_filter, 0, sizeof(new_mc_filter));
for (i = 0, mclist = dev->mc_list; mclist && i < 15;
i++, mclist = mclist->next) {
writel(((u32*)mclist->dmi_addr)[0], ioaddr + RxAddrCAM + 8 + i*8);
writel((((u32*)mclist->dmi_addr)[1] & 0xffff) | 0x80000000,
ioaddr + RxAddrCAM + 12 + i*8);
}
for (; mclist && i < dev->mc_count; i++, mclist = mclist->next) {
set_bit(((u32*)mclist->dmi_addr)[1] & 0xfff,
new_mc_filter);
}
new_rx_mode &= ~RxCtrlAllUnicast | RxCtrlAllMulticast;
new_rx_mode |= RxCtrlAcceptBroadcast;
if (dev->mc_count > 15)
for (i = 0; i < 128; i++)
writel(new_mc_filter[i], ioaddr + MulticastArray + (i<<2));
}
if (np->rx_mode != new_rx_mode)
writel(np->rx_mode = new_rx_mode, ioaddr + RxControl);
}
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
struct netdev_private *np = (struct netdev_private *)dev->priv;
u32 *data32 = (void *)&rq->ifr_data;
switch(cmd) {
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
printk(KERN_DEBUG "%s: Shutting down ethercard, status was Tx %4.4x "
"Rx %4.4x Int %2.2x.\n",
dev->name, (int)readl(ioaddr + TxStatus),
(int)readl(ioaddr + RxStatus), (int)readl(ioaddr + IntrStatus));
printk(KERN_DEBUG "%s: Queue pointers were Tx %d / %d,  Rx %d / %d.\n",
dev->name, np->cur_tx, np->dirty_tx, np->cur_rx, np->dirty_rx);
}
writel(~0, ioaddr + IntrDisable);
readl(ioaddr + IntrStatus);
writel(0x04000000, ioaddr + ChipCtrl);
del_timer(&np->timer);
#ifdef __i386__
if (np->msg_level & NETIF_MSG_IFDOWN) {
printk("\n"KERN_DEBUG"  Tx ring at %8.8x:\n",
(int)virt_to_bus(np->tx_ring));
for (i = 0; i < TX_RING_SIZE; i++)
printk(" #%d desc. buf %8.8x, length %8.8x, status %8.8x.\n",
i, np->tx_ring[i].buf_addr, np->tx_ring[i].cmd_length,
np->tx_ring[i].status);
printk("\n"KERN_DEBUG "  Rx ring %8.8x:\n",
(int)virt_to_bus(np->rx_ring));
for (i = 0; i < RX_RING_SIZE; i++) {
printk(KERN_DEBUG " #%d desc. %4.4x %4.4x %8.8x\n",
i, np->rx_ring[i].csum_length,
np->rx_ring[i].status, np->rx_ring[i].buf_addr);
if (np->rx_ring[i].buf_addr) {
if (*(u8*)np->rx_skbuff[i]->tail != 0x69) {
u16 *pkt_buf = (void *)np->rx_skbuff[i]->tail;
int j;
for (j = 0; j < 0x50; j++)
printk(" %4.4x", pkt_buf[j]);
printk("\n");
}
}
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
writel(~0, ioaddr + IntrDisable);
break;
case DRV_RESUME:
set_rx_mode(dev);
break;
case DRV_DETACH: {
struct net_device **devp, **next;
if (dev->flags & IFF_UP) {
dev_close(dev);
dev->flags &= ~(IFF_UP|IFF_RUNNING);
}
unregister_netdev(dev);
release_region(dev->base_addr, pci_id_tbl[np->chip_id].io_size);
iounmap((char *)dev->base_addr);
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
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
return pci_drv_register(&igige_drv_id, NULL);
}
void cleanup_module(void)
{
struct net_device *next_dev;
pci_drv_unregister(&igige_drv_id);
while (root_net_dev) {
struct netdev_private *np = (void *)(root_net_dev->priv);
unregister_netdev(root_net_dev);
release_region(root_net_dev->base_addr,
pci_id_tbl[np->chip_id].io_size);
iounmap((char *)(root_net_dev->base_addr));
next_dev = np->next_module;
if (np->tx_ring == 0)
free_page((long)np->tx_ring);
if (np->rx_ring == 0)
free_page((long)np->rx_ring);
if (np->priv_addr)
kfree(np->priv_addr);
kfree(root_net_dev);
root_net_dev = next_dev;
}
}
#endif