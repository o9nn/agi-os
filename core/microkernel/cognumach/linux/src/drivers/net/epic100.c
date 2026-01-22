static const char version[] =
"epic100.c:v1.18 7/22/2003 Written by Donald Becker <becker@scyld.com>\n";
static const char version2[] =
"  http:
static int debug = 2;
static int max_interrupt_work = 32;
static int multicast_filter_limit = 32;
#define MAX_UNITS 8
static int options[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int full_duplex[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int rx_copybreak = 0;
#define TX_RING_SIZE 16
#define TX_QUEUE_LEN 10
#define RX_RING_SIZE 32
#define TX_TIMEOUT (6*HZ)
#define PKT_BUF_SZ 1536
#define TX_FIFO_THRESH 256
#define RX_FIFO_THRESH 1
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
#include <linux/delay.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <asm/bitops.h>
#include <asm/io.h>
#if LINUX_VERSION_CODE >= 0x20300
#include <linux/spinlock.h>
#elif LINUX_VERSION_CODE >= 0x20200
#include <asm/spinlock.h>
#endif
#ifdef INLINE_PCISCAN
#include "k_compat.h"
#else
#include "pci-scan.h"
#include "kern_compat.h"
#endif
#if (LINUX_VERSION_CODE >= 0x20100) && defined(MODULE)
char kernel_version[] = UTS_RELEASE;
#endif
MODULE_AUTHOR("Donald Becker <becker@scyld.com>");
MODULE_DESCRIPTION("SMC 83c170 EPIC series Ethernet driver");
MODULE_LICENSE("GPL");
MODULE_PARM(debug, "i");
MODULE_PARM(max_interrupt_work, "i");
MODULE_PARM(rx_copybreak, "i");
MODULE_PARM(options, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(full_duplex, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(multicast_filter_limit, "i");
MODULE_PARM_DESC(debug, "Driver message level (0-31)");
MODULE_PARM_DESC(options, "Force transceiver type or fixed speed+duplex.\n"
"Values are 0x10/0x20/0x100/0x200.");
MODULE_PARM_DESC(max_interrupt_work,
"Driver maximum events handled per interrupt");
MODULE_PARM_DESC(full_duplex, "Non-zero to set forced full duplex.");
MODULE_PARM_DESC(rx_copybreak,
"Breakpoint in bytes for copy-only-tiny-frames");
MODULE_PARM_DESC(multicast_filter_limit,
"Multicast addresses before switching to Rx-all-multicast");
static void *epic_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int find_cnt);
static int epic_pwr_event(void *dev_instance, int event);
enum chip_capability_flags { MII_PWRDWN=1, TYPE2_INTR=2, NO_MII=4 };
#define EPIC_TOTAL_SIZE 0x100
#ifdef USE_IO_OPS
#define EPIC_IOTYPE PCI_USES_MASTER|PCI_USES_IO|PCI_ADDR0
#else
#define EPIC_IOTYPE PCI_USES_MASTER|PCI_USES_MEM|PCI_ADDR1
#endif
static struct pci_id_info pci_id_tbl[] = {
{"SMSC EPIC 83c172", {0x000510B8, 0xffffffff, 0,0, 9,0xff},
EPIC_IOTYPE, EPIC_TOTAL_SIZE, TYPE2_INTR | MII_PWRDWN, },
{"SMSC EPIC 83c171", {0x000510B8, 0xffffffff, 0,0, 6,0xff},
EPIC_IOTYPE, EPIC_TOTAL_SIZE, TYPE2_INTR | MII_PWRDWN, },
{"SMSC EPIC/100 83c170", {0x000510B8, 0xffffffff, 0x0ab41092, 0xffffffff},
EPIC_IOTYPE, EPIC_TOTAL_SIZE, TYPE2_INTR | NO_MII | MII_PWRDWN, },
{"SMSC EPIC/100 83c170", {0x000510B8, 0xffffffff},
EPIC_IOTYPE, EPIC_TOTAL_SIZE, TYPE2_INTR, },
{"SMSC EPIC/C 83c175", {0x000610B8, 0xffffffff},
EPIC_IOTYPE, EPIC_TOTAL_SIZE, TYPE2_INTR | MII_PWRDWN, },
{0,},
};
struct drv_id_info epic_drv_id = {
"epic100", PCI_HOTSWAP, PCI_CLASS_NETWORK_ETHERNET<<8, pci_id_tbl,
epic_probe1, epic_pwr_event };
#ifndef USE_IO_OPS
#undef inb
#undef inw
#undef inl
#undef outb
#undef outw
#undef outl
#define inb readb
#define inw readw
#define inl readl
#define outb writeb
#define outw writew
#define outl writel
#endif
enum epic_registers {
COMMAND=0, INTSTAT=4, INTMASK=8, GENCTL=0x0C, NVCTL=0x10, EECTL=0x14,
PCIBurstCnt=0x18,
TEST1=0x1C, CRCCNT=0x20, ALICNT=0x24, MPCNT=0x28,
MIICtrl=0x30, MIIData=0x34, MIICfg=0x38,
LAN0=64,
MC0=80,
RxCtrl=96, TxCtrl=112, TxSTAT=0x74,
PRxCDAR=0x84, RxSTAT=0xA4, EarlyRx=0xB0, PTxCDAR=0xC4, TxThresh=0xDC,
};
enum IntrStatus {
TxIdle=0x40000, RxIdle=0x20000, IntrSummary=0x010000,
PCIBusErr170=0x7000, PCIBusErr175=0x1000, PhyEvent175=0x8000,
RxStarted=0x0800, RxEarlyWarn=0x0400, CntFull=0x0200, TxUnderrun=0x0100,
TxEmpty=0x0080, TxDone=0x0020, RxError=0x0010,
RxOverflow=0x0008, RxFull=0x0004, RxHeader=0x0002, RxDone=0x0001,
};
enum CommandBits {
StopRx=1, StartRx=2, TxQueued=4, RxQueued=8,
StopTxDMA=0x20, StopRxDMA=0x40, RestartTx=0x80,
};
struct epic_tx_desc {
u32 txstatus;
u32 bufaddr;
u32 buflength;
u32 next;
};
struct epic_rx_desc {
u32 rxstatus;
u32 bufaddr;
u32 buflength;
u32 next;
};
enum desc_status_bits {
DescOwn=0x8000,
};
#define PRIV_ALIGN 15
struct epic_private {
struct epic_rx_desc rx_ring[RX_RING_SIZE];
struct epic_tx_desc tx_ring[TX_RING_SIZE];
struct sk_buff* tx_skbuff[TX_RING_SIZE];
struct sk_buff* rx_skbuff[RX_RING_SIZE];
struct net_device *next_module;
void *priv_addr;
spinlock_t lock;
unsigned int cur_tx, dirty_tx;
struct descriptor *last_tx_desc;
unsigned int cur_rx, dirty_rx;
unsigned int rx_buf_sz;
struct descriptor *last_rx_desc;
long last_rx_time;
int rx_copybreak;
int msg_level;
int max_interrupt_work;
struct pci_dev *pci_dev;
int chip_id, chip_flags;
struct net_device_stats stats;
struct timer_list timer;
int tx_threshold;
int genctl;
u32 cur_rx_mode;
unsigned char mc_filter[8];
int multicast_filter_limit;
signed char phys[4];
u16 mii_bmcr;
u16 advertising;
int mii_phy_cnt;
unsigned int tx_full:1;
unsigned int full_duplex:1;
unsigned int duplex_lock:1;
unsigned int default_port;
unsigned int media2:4;
unsigned int medialock:1;
unsigned int mediasense:1;
};
static int epic_open(struct net_device *dev);
static int read_eeprom(long ioaddr, int location);
static int mdio_read(struct net_device *dev, int phy_id, int location);
static void mdio_write(struct net_device *dev, int phy_id, int loc, int val);
static void epic_start(struct net_device *dev, int restart);
static void check_media(struct net_device *dev);
static void epic_timer(unsigned long data);
static void epic_tx_timeout(struct net_device *dev);
static void epic_init_ring(struct net_device *dev);
static int epic_start_xmit(struct sk_buff *skb, struct net_device *dev);
static int epic_rx(struct net_device *dev);
static void epic_interrupt(int irq, void *dev_instance, struct pt_regs *regs);
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd);
static int epic_close(struct net_device *dev);
static struct net_device_stats *epic_get_stats(struct net_device *dev);
static void set_rx_mode(struct net_device *dev);
static struct net_device *root_epic_dev = NULL;
static void *epic_probe1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int card_idx)
{
struct net_device *dev;
struct epic_private *ep;
void *priv_mem;
int i, option = 0, duplex = 0;
dev = init_etherdev(init_dev, 0);
if (!dev)
return NULL;
if (dev->mem_start) {
option = dev->mem_start;
duplex = (dev->mem_start & 16) ? 1 : 0;
} else if (card_idx >= 0 && card_idx < MAX_UNITS) {
if (options[card_idx] >= 0)
option = options[card_idx];
if (full_duplex[card_idx] >= 0)
duplex = full_duplex[card_idx];
}
dev->base_addr = ioaddr;
dev->irq = irq;
printk(KERN_INFO "%s: %s at %#lx, %2.2x:%2.2x IRQ %d, ",
dev->name, pci_id_tbl[chip_idx].name, ioaddr,
pci_bus_number(pdev), pci_devfn(pdev)>>3, dev->irq);
outl(0x4200, ioaddr + GENCTL);
outl(0x0008, ioaddr + TEST1);
outl(0x12, ioaddr + MIICfg);
if (pci_id_tbl[chip_idx].drv_flags & NO_MII)
outl((inl(ioaddr + NVCTL) & ~0x003C) | 0x4800, ioaddr + NVCTL);
outl(0x0200, ioaddr + GENCTL);
if (((1 << debug) - 1) & NETIF_MSG_MISC) {
printk(KERN_DEBUG "%s: EEPROM contents\n", dev->name);
for (i = 0; i < 64; i++)
printk(" %4.4x%s", read_eeprom(ioaddr, i),
i % 16 == 15 ? "\n" : "");
}
for (i = 0; i < 3; i++)
((u16 *)dev->dev_addr)[i] = le16_to_cpu(inw(ioaddr + LAN0 + i*4));
for (i = 0; i < 5; i++)
printk("%2.2x:", dev->dev_addr[i]);
printk("%2.2x.\n", dev->dev_addr[i]);
priv_mem = kmalloc(sizeof(*ep) + PRIV_ALIGN, GFP_KERNEL);
if (priv_mem == NULL)
return NULL;
request_region(ioaddr, pci_id_tbl[chip_idx].io_size, dev->name);
dev->priv = ep = (void *)(((long)priv_mem + PRIV_ALIGN) & ~PRIV_ALIGN);
memset(ep, 0, sizeof(*ep));
ep->priv_addr = priv_mem;
ep->next_module = root_epic_dev;
root_epic_dev = dev;
ep->pci_dev = pdev;
ep->chip_id = chip_idx;
ep->chip_flags = pci_id_tbl[chip_idx].drv_flags;
ep->msg_level = (1 << debug) - 1;
ep->rx_copybreak = rx_copybreak;
ep->max_interrupt_work = max_interrupt_work;
ep->multicast_filter_limit = multicast_filter_limit;
if (option > 0) {
if (option & 0x220)
ep->duplex_lock = ep->full_duplex = 1;
ep->default_port = option & 0xFFFF;
ep->medialock = 1;
}
if (duplex) {
ep->duplex_lock = ep->full_duplex = 1;
printk(KERN_INFO "%s:  Forced full duplex operation requested.\n",
dev->name);
}
dev->if_port = ep->default_port;
{
int phy, phy_idx = 0;
for (phy = 1; phy < 32 && phy_idx < sizeof(ep->phys); phy++) {
int mii_status = mdio_read(dev, phy, 1);
if (mii_status != 0xffff && mii_status != 0x0000) {
ep->phys[phy_idx++] = phy;
printk(KERN_INFO "%s: Located MII transceiver #%d control "
"%4.4x status %4.4x.\n",
dev->name, phy, mdio_read(dev, phy, 0), mii_status);
}
}
ep->mii_phy_cnt = phy_idx;
}
if (ep->mii_phy_cnt == 0 && ! (ep->chip_flags & NO_MII)) {
printk(KERN_WARNING "%s: ***WARNING***: No MII transceiver found!\n",
dev->name);
ep->phys[0] = 3;
}
if (ep->mii_phy_cnt) {
int phy = ep->phys[0];
int xcvr = ep->default_port & 0x330;
if (xcvr) {
printk(KERN_INFO "  Forcing %dMbs %s-duplex operation.\n",
(xcvr & 0x300 ? 100 : 10),
(xcvr & 0x220 ? "full" : "half"));
ep->mii_bmcr = xcvr & 0x300 ? 0x2000 : 0;
ep->mii_bmcr |= xcvr & 0x220 ? 0x0100 : 0;
mdio_write(dev, phy, 0, ep->mii_bmcr);
} else {
ep->mii_bmcr = 0x3000;
ep->advertising = mdio_read(dev, phy, 4);
printk(KERN_INFO "%s: Autonegotiation advertising %4.4x link "
"partner %4.4x.\n",
dev->name, ep->advertising, mdio_read(dev, phy, 5));
}
}
#if EPIC_POWER_SAVE
if (ep->chip_flags & MII_PWRDWN)
outl(inl(ioaddr + NVCTL) & ~0x483C, ioaddr + NVCTL);
#endif
outl(0x0008, ioaddr + GENCTL);
dev->open = &epic_open;
dev->hard_start_xmit = &epic_start_xmit;
dev->stop = &epic_close;
dev->get_stats = &epic_get_stats;
dev->set_multicast_list = &set_rx_mode;
dev->do_ioctl = &mii_ioctl;
return dev;
}
#define EE_SHIFT_CLK 0x04
#define EE_CS 0x02
#define EE_DATA_WRITE 0x08
#define EE_WRITE_0 0x01
#define EE_WRITE_1 0x09
#define EE_DATA_READ 0x10
#define EE_ENB (0x0001 | EE_CS)
#define eeprom_delay() inl(ee_addr)
#define EE_WRITE_CMD (5 << 6)
#define EE_READ64_CMD (6 << 6)
#define EE_READ256_CMD (6 << 8)
#define EE_ERASE_CMD (7 << 6)
static int read_eeprom(long ioaddr, int location)
{
int i;
int retval = 0;
long ee_addr = ioaddr + EECTL;
int read_cmd = location |
(inl(ee_addr) & 0x40 ? EE_READ64_CMD : EE_READ256_CMD);
outl(EE_ENB & ~EE_CS, ee_addr);
outl(EE_ENB, ee_addr);
for (i = 12; i >= 0; i--) {
short dataval = (read_cmd & (1 << i)) ? EE_WRITE_1 : EE_WRITE_0;
outl(EE_ENB | dataval, ee_addr);
eeprom_delay();
outl(EE_ENB | dataval | EE_SHIFT_CLK, ee_addr);
eeprom_delay();
}
outl(EE_ENB, ee_addr);
for (i = 16; i > 0; i--) {
outl(EE_ENB | EE_SHIFT_CLK, ee_addr);
eeprom_delay();
retval = (retval << 1) | ((inl(ee_addr) & EE_DATA_READ) ? 1 : 0);
outl(EE_ENB, ee_addr);
eeprom_delay();
}
outl(EE_ENB & ~EE_CS, ee_addr);
return retval;
}
#define MII_READOP 1
#define MII_WRITEOP 2
static int mdio_read(struct net_device *dev, int phy_id, int location)
{
long ioaddr = dev->base_addr;
int read_cmd = (phy_id << 9) | (location << 4) | MII_READOP;
int i;
outl(read_cmd, ioaddr + MIICtrl);
for (i = 400; i > 0; i--)
if ((inl(ioaddr + MIICtrl) & MII_READOP) == 0) {
if (phy_id == 1 && location < 6
&& inw(ioaddr + MIIData) == 0xffff) {
outl(read_cmd, ioaddr + MIICtrl);
continue;
}
return inw(ioaddr + MIIData);
}
return 0xffff;
}
static void mdio_write(struct net_device *dev, int phy_id, int loc, int value)
{
long ioaddr = dev->base_addr;
int i;
outw(value, ioaddr + MIIData);
outl((phy_id << 9) | (loc << 4) | MII_WRITEOP, ioaddr + MIICtrl);
for (i = 10000; i > 0; i--) {
if ((inl(ioaddr + MIICtrl) & MII_WRITEOP) == 0)
break;
}
return;
}
static int epic_open(struct net_device *dev)
{
struct epic_private *ep = (struct epic_private *)dev->priv;
MOD_INC_USE_COUNT;
if (request_irq(dev->irq, &epic_interrupt, SA_SHIRQ, dev->name, dev)) {
MOD_DEC_USE_COUNT;
return -EAGAIN;
}
epic_init_ring(dev);
check_media(dev);
epic_start(dev, 0);
init_timer(&ep->timer);
ep->timer.expires = jiffies + 3*HZ;
ep->timer.data = (unsigned long)dev;
ep->timer.function = &epic_timer;
add_timer(&ep->timer);
return 0;
}
static void epic_pause(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct epic_private *ep = (struct epic_private *)dev->priv;
outl(0x00000000, ioaddr + INTMASK);
outw(StopRx | StopTxDMA | StopRxDMA, ioaddr + COMMAND);
if (inw(ioaddr + COMMAND) != 0xffff) {
ep->stats.rx_missed_errors += inb(ioaddr + MPCNT);
ep->stats.rx_frame_errors += inb(ioaddr + ALICNT);
ep->stats.rx_crc_errors += inb(ioaddr + CRCCNT);
}
epic_rx(dev);
}
static void epic_start(struct net_device *dev, int restart)
{
long ioaddr = dev->base_addr;
struct epic_private *ep = (struct epic_private *)dev->priv;
int i;
if (restart) {
outl(0x4001, ioaddr + GENCTL);
printk(KERN_DEBUG "%s: Restarting the EPIC chip, Rx %d/%d Tx %d/%d.\n",
dev->name, ep->cur_rx, ep->dirty_rx, ep->dirty_tx, ep->cur_tx);
udelay(1);
for (i = 16; i > 0; i--)
outl(0x0008, ioaddr + TEST1);
}
#if defined(__powerpc__) || defined(__sparc__) || defined(__BIG_ENDIAN)
ep->genctl = 0x0432 | (RX_FIFO_THRESH<<8);
#elif defined(__LITTLE_ENDIAN) || defined(__i386__)
ep->genctl = 0x0412 | (RX_FIFO_THRESH<<8);
#else
#error The byte order of this architecture is not defined.
#endif
if (ep->chip_flags & MII_PWRDWN)
outl((inl(ioaddr + NVCTL) & ~0x003C) | 0x4800, ioaddr + NVCTL);
if (restart) {
outl(ep->genctl | 0x4000, ioaddr + GENCTL);
inl(ioaddr + GENCTL);
}
outl(ep->genctl, ioaddr + GENCTL);
if (dev->if_port == 2 || dev->if_port == 5) {
outl(0x13, ioaddr + MIICfg);
printk(KERN_INFO "%s: Disabling MII PHY to use 10base2/AUI.\n",
dev->name);
mdio_write(dev, ep->phys[0], 0, 0x0C00);
} else {
outl(0x12, ioaddr + MIICfg);
mdio_write(dev, ep->phys[0], 0, ep->advertising);
mdio_write(dev, ep->phys[0], 0, ep->mii_bmcr);
check_media(dev);
}
for (i = 0; i < 3; i++)
outl(cpu_to_le16(((u16*)dev->dev_addr)[i]), ioaddr + LAN0 + i*4);
ep->tx_threshold = TX_FIFO_THRESH;
outl(ep->tx_threshold, ioaddr + TxThresh);
outl(ep->full_duplex ? 0x7F : 0x79, ioaddr + TxCtrl);
outl(virt_to_bus(&ep->rx_ring[ep->cur_rx % RX_RING_SIZE]),
ioaddr + PRxCDAR);
outl(virt_to_bus(&ep->tx_ring[ep->dirty_tx % TX_RING_SIZE]),
ioaddr + PTxCDAR);
set_rx_mode(dev);
outl(StartRx | RxQueued, ioaddr + COMMAND);
if ( ! restart)
netif_start_tx_queue(dev);
outl((ep->chip_flags & TYPE2_INTR ? PCIBusErr175 : PCIBusErr170)
| CntFull | TxUnderrun | TxDone | TxEmpty
| RxError | RxOverflow | RxFull | RxHeader | RxDone,
ioaddr + INTMASK);
if (ep->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: epic_start() done, cmd status %4.4x, "
"ctl %4.4x interrupt %4.4x.\n",
dev->name, (int)inl(ioaddr + COMMAND),
(int)inl(ioaddr + GENCTL), (int)inl(ioaddr + INTSTAT));
return;
}
static void check_media(struct net_device *dev)
{
struct epic_private *ep = (struct epic_private *)dev->priv;
long ioaddr = dev->base_addr;
int mii_reg5 = ep->mii_phy_cnt ? mdio_read(dev, ep->phys[0], 5) : 0;
int negotiated = mii_reg5 & ep->advertising;
int duplex = (negotiated & 0x0100) || (negotiated & 0x01C0) == 0x0040;
if (ep->duplex_lock)
return;
if (mii_reg5 == 0xffff)
return;
if (ep->full_duplex != duplex) {
ep->full_duplex = duplex;
printk(KERN_INFO "%s: Setting %s-duplex based on MII #%d link"
" partner capability of %4.4x.\n", dev->name,
ep->full_duplex ? "full" : "half", ep->phys[0], mii_reg5);
outl(ep->full_duplex ? 0x7F : 0x79, ioaddr + TxCtrl);
}
}
static void epic_timer(unsigned long data)
{
struct net_device *dev = (struct net_device *)data;
struct epic_private *ep = (struct epic_private *)dev->priv;
long ioaddr = dev->base_addr;
int next_tick = 5*HZ;
if (ep->msg_level & NETIF_MSG_TIMER) {
printk(KERN_DEBUG "%s: Media monitor tick, Tx status %8.8x.\n",
dev->name, (int)inl(ioaddr + TxSTAT));
printk(KERN_DEBUG "%s: Other registers are IntMask %4.4x "
"IntStatus %4.4x RxStatus %4.4x.\n",
dev->name, (int)inl(ioaddr + INTMASK),
(int)inl(ioaddr + INTSTAT), (int)inl(ioaddr + RxSTAT));
}
if (ep->cur_tx - ep->dirty_tx > 1 &&
jiffies - dev->trans_start > TX_TIMEOUT) {
printk(KERN_WARNING "%s: Tx hung, %d vs. %d.\n",
dev->name, ep->cur_tx, ep->dirty_tx);
epic_tx_timeout(dev);
}
check_media(dev);
ep->timer.expires = jiffies + next_tick;
add_timer(&ep->timer);
}
static void epic_tx_timeout(struct net_device *dev)
{
struct epic_private *ep = (struct epic_private *)dev->priv;
long ioaddr = dev->base_addr;
int tx_status = inw(ioaddr + TxSTAT);
printk(KERN_WARNING "%s: EPIC transmit timeout, Tx status %4.4x.\n",
dev->name, tx_status);
if (ep->msg_level & NETIF_MSG_TX_ERR)
printk(KERN_DEBUG "%s: Tx indices: dirty_tx %d, cur_tx %d.\n",
dev->name, ep->dirty_tx, ep->cur_tx);
if (tx_status & 0x10) {
ep->stats.tx_fifo_errors++;
outl(RestartTx, ioaddr + COMMAND);
} else {
epic_start(dev, 1);
outl(TxQueued, dev->base_addr + COMMAND);
}
dev->trans_start = jiffies;
ep->stats.tx_errors++;
return;
}
static void epic_init_ring(struct net_device *dev)
{
struct epic_private *ep = (struct epic_private *)dev->priv;
int i;
ep->tx_full = 0;
ep->lock = (spinlock_t) SPIN_LOCK_UNLOCKED;
ep->dirty_tx = ep->cur_tx = 0;
ep->cur_rx = ep->dirty_rx = 0;
ep->last_rx_time = jiffies;
ep->rx_buf_sz = (dev->mtu <= 1522 ? PKT_BUF_SZ : dev->mtu + 14);
for (i = 0; i < RX_RING_SIZE; i++) {
ep->rx_ring[i].rxstatus = 0;
ep->rx_ring[i].buflength = ep->rx_buf_sz;
ep->rx_ring[i].next = virt_to_bus(&ep->rx_ring[i+1]);
ep->rx_skbuff[i] = 0;
}
ep->rx_ring[i-1].next = virt_to_bus(&ep->rx_ring[0]);
for (i = 0; i < RX_RING_SIZE; i++) {
struct sk_buff *skb = dev_alloc_skb(ep->rx_buf_sz);
ep->rx_skbuff[i] = skb;
if (skb == NULL)
break;
skb->dev = dev;
skb_reserve(skb, 2);
ep->rx_ring[i].bufaddr = virt_to_bus(skb->tail);
ep->rx_ring[i].rxstatus = DescOwn;
}
ep->dirty_rx = (unsigned int)(i - RX_RING_SIZE);
for (i = 0; i < TX_RING_SIZE; i++) {
ep->tx_skbuff[i] = 0;
ep->tx_ring[i].txstatus = 0x0000;
ep->tx_ring[i].next = virt_to_bus(&ep->tx_ring[i+1]);
}
ep->tx_ring[i-1].next = virt_to_bus(&ep->tx_ring[0]);
return;
}
static int epic_start_xmit(struct sk_buff *skb, struct net_device *dev)
{
struct epic_private *ep = (struct epic_private *)dev->priv;
int entry, free_count;
u32 ctrl_word;
unsigned long flags;
if (netif_pause_tx_queue(dev) != 0) {
if (jiffies - dev->trans_start > TX_TIMEOUT)
epic_tx_timeout(dev);
return 1;
}
spin_lock_irqsave(&ep->lock, flags);
free_count = ep->cur_tx - ep->dirty_tx;
entry = ep->cur_tx % TX_RING_SIZE;
ep->tx_skbuff[entry] = skb;
ep->tx_ring[entry].bufaddr = virt_to_bus(skb->data);
if (free_count < TX_QUEUE_LEN/2) {
ctrl_word = 0x100000;
} else if (free_count == TX_QUEUE_LEN/2) {
ctrl_word = 0x140000;
} else if (free_count < TX_QUEUE_LEN - 1) {
ctrl_word = 0x100000;
} else {
ctrl_word = 0x140000;
ep->tx_full = 1;
}
ep->tx_ring[entry].buflength = ctrl_word | skb->len;
ep->tx_ring[entry].txstatus =
((skb->len >= ETH_ZLEN ? skb->len : ETH_ZLEN) << 16)
| DescOwn;
ep->cur_tx++;
if (ep->tx_full) {
if (ep->cur_tx - (volatile int)ep->dirty_tx < TX_QUEUE_LEN - 2) {
netif_unpause_tx_queue(dev);
ep->tx_full = 0;
} else
netif_stop_tx_queue(dev);
} else
netif_unpause_tx_queue(dev);
spin_unlock_irqrestore(&ep->lock, flags);
outl(TxQueued, dev->base_addr + COMMAND);
dev->trans_start = jiffies;
if (ep->msg_level & NETIF_MSG_TX_QUEUED)
printk(KERN_DEBUG "%s: Queued Tx packet size %d to slot %d, "
"flag %2.2x Tx status %8.8x.\n",
dev->name, (int)skb->len, entry, ctrl_word,
(int)inl(dev->base_addr + TxSTAT));
return 0;
}
static void epic_interrupt(int irq, void *dev_instance, struct pt_regs *regs)
{
struct net_device *dev = (struct net_device *)dev_instance;
struct epic_private *ep = (struct epic_private *)dev->priv;
long ioaddr = dev->base_addr;
int status, boguscnt = max_interrupt_work;
do {
status = inl(ioaddr + INTSTAT);
outl(status & 0x00007fff, ioaddr + INTSTAT);
if (ep->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: Interrupt, status=%#8.8x new "
"intstat=%#8.8x.\n",
dev->name, status, (int)inl(ioaddr + INTSTAT));
if ((status & IntrSummary) == 0)
break;
if (status & (RxDone | RxStarted | RxEarlyWarn | RxOverflow))
epic_rx(dev);
if (status & (TxEmpty | TxDone)) {
unsigned int dirty_tx, cur_tx;
spin_lock(&ep->lock);
cur_tx = ep->cur_tx;
dirty_tx = ep->dirty_tx;
for (; cur_tx - dirty_tx > 0; dirty_tx++) {
int entry = dirty_tx % TX_RING_SIZE;
int txstatus = ep->tx_ring[entry].txstatus;
if (txstatus & DescOwn)
break;
if ( ! (txstatus & 0x0001)) {
#ifndef final_version
if (ep->msg_level & NETIF_MSG_TX_ERR)
printk(KERN_DEBUG "%s: Transmit error, Tx status %8.8x.\n",
dev->name, txstatus);
#endif
ep->stats.tx_errors++;
if (txstatus & 0x1050) ep->stats.tx_aborted_errors++;
if (txstatus & 0x0008) ep->stats.tx_carrier_errors++;
if (txstatus & 0x0040) ep->stats.tx_window_errors++;
if (txstatus & 0x0010) ep->stats.tx_fifo_errors++;
#ifdef ETHER_STATS
if (txstatus & 0x1000) ep->stats.collisions16++;
#endif
} else {
if (ep->msg_level & NETIF_MSG_TX_DONE)
printk(KERN_DEBUG "%s: Transmit done, Tx status "
"%8.8x.\n", dev->name, txstatus);
#ifdef ETHER_STATS
if ((txstatus & 0x0002) != 0) ep->stats.tx_deferred++;
#endif
ep->stats.collisions += (txstatus >> 8) & 15;
ep->stats.tx_packets++;
#if LINUX_VERSION_CODE > 0x20127
ep->stats.tx_bytes += ep->tx_skbuff[entry]->len;
#endif
}
dev_free_skb_irq(ep->tx_skbuff[entry]);
ep->tx_skbuff[entry] = 0;
}
#ifndef final_version
if (cur_tx - dirty_tx > TX_RING_SIZE) {
printk(KERN_WARNING "%s: Out-of-sync dirty pointer, %d vs. %d, full=%d.\n",
dev->name, dirty_tx, cur_tx, ep->tx_full);
dirty_tx += TX_RING_SIZE;
}
#endif
ep->dirty_tx = dirty_tx;
if (ep->tx_full
&& cur_tx - dirty_tx < TX_QUEUE_LEN - 4) {
ep->tx_full = 0;
spin_unlock(&ep->lock);
netif_resume_tx_queue(dev);
} else
spin_unlock(&ep->lock);
}
if (status & (CntFull | TxUnderrun | RxOverflow | RxFull |
PCIBusErr170 | PCIBusErr175)) {
if (status == 0xffffffff)
break;
ep->stats.rx_missed_errors += inb(ioaddr + MPCNT);
ep->stats.rx_frame_errors += inb(ioaddr + ALICNT);
ep->stats.rx_crc_errors += inb(ioaddr + CRCCNT);
if (status & TxUnderrun) {
ep->stats.tx_fifo_errors++;
outl(ep->tx_threshold += 128, ioaddr + TxThresh);
outl(RestartTx, ioaddr + COMMAND);
}
if (status & RxOverflow) {
ep->stats.rx_errors++;
}
if (status & (RxOverflow | RxFull))
outw(RxQueued, ioaddr + COMMAND);
if (status & PCIBusErr170) {
printk(KERN_ERR "%s: PCI Bus Error!  EPIC status %4.4x.\n",
dev->name, status);
epic_pause(dev);
epic_start(dev, 1);
}
outl(status & 0x7f18, ioaddr + INTSTAT);
}
if (--boguscnt < 0) {
printk(KERN_WARNING "%s: Too much work at interrupt, "
"IntrStatus=0x%8.8x.\n",
dev->name, status);
outl(0x0001ffff, ioaddr + INTSTAT);
max_interrupt_work++;
break;
}
} while (1);
if (ep->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: Exiting interrupt, intr_status=%#4.4x.\n",
dev->name, status);
return;
}
static int epic_rx(struct net_device *dev)
{
struct epic_private *ep = (struct epic_private *)dev->priv;
int entry = ep->cur_rx % RX_RING_SIZE;
int rx_work_limit = ep->dirty_rx + RX_RING_SIZE - ep->cur_rx;
int work_done = 0;
if (ep->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG " In epic_rx(), entry %d %8.8x.\n", entry,
ep->rx_ring[entry].rxstatus);
while ((ep->rx_ring[entry].rxstatus & DescOwn) == 0) {
int status = ep->rx_ring[entry].rxstatus;
if (ep->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  epic_rx() status was %8.8x.\n", status);
if (--rx_work_limit < 0)
break;
if (status & 0x2006) {
if (ep->msg_level & NETIF_MSG_RX_ERR)
printk(KERN_DEBUG "%s: epic_rx() error status was %8.8x.\n",
dev->name, status);
if (status & 0x2000) {
printk(KERN_WARNING "%s: Oversized Ethernet frame spanned "
"multiple buffers, status %4.4x!\n", dev->name, status);
ep->stats.rx_length_errors++;
} else if (status & 0x0006)
ep->stats.rx_errors++;
} else {
short pkt_len = (status >> 16) - 4;
struct sk_buff *skb;
if (pkt_len > PKT_BUF_SZ - 4) {
printk(KERN_ERR "%s: Oversized Ethernet frame, status %x "
"%d bytes.\n",
dev->name, pkt_len, status);
pkt_len = 1514;
}
if (ep->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  netdev_rx() normal Rx pkt length %d"
", bogus_cnt %d.\n", pkt_len, rx_work_limit);
if (pkt_len < rx_copybreak
&& (skb = dev_alloc_skb(pkt_len + 2)) != NULL) {
skb->dev = dev;
skb_reserve(skb, 2);
#if 1
eth_copy_and_sum(skb, ep->rx_skbuff[entry]->tail, pkt_len, 0);
skb_put(skb, pkt_len);
#else
memcpy(skb_put(skb, pkt_len), ep->rx_skbuff[entry]->tail,
pkt_len);
#endif
} else {
skb_put(skb = ep->rx_skbuff[entry], pkt_len);
ep->rx_skbuff[entry] = NULL;
}
skb->protocol = eth_type_trans(skb, dev);
netif_rx(skb);
ep->stats.rx_packets++;
#if LINUX_VERSION_CODE > 0x20127
ep->stats.rx_bytes += pkt_len;
#endif
}
work_done++;
entry = (++ep->cur_rx) % RX_RING_SIZE;
}
for (; ep->cur_rx - ep->dirty_rx > 0; ep->dirty_rx++) {
entry = ep->dirty_rx % RX_RING_SIZE;
if (ep->rx_skbuff[entry] == NULL) {
struct sk_buff *skb;
skb = ep->rx_skbuff[entry] = dev_alloc_skb(ep->rx_buf_sz);
if (skb == NULL)
break;
skb->dev = dev;
skb_reserve(skb, 2);
ep->rx_ring[entry].bufaddr = virt_to_bus(skb->tail);
work_done++;
}
ep->rx_ring[entry].rxstatus = DescOwn;
}
return work_done;
}
static int epic_close(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct epic_private *ep = (struct epic_private *)dev->priv;
int i;
netif_stop_tx_queue(dev);
if (ep->msg_level & NETIF_MSG_IFDOWN)
printk(KERN_DEBUG "%s: Shutting down ethercard, status was %8.8x.\n",
dev->name, (int)inl(ioaddr + INTSTAT));
epic_pause(dev);
del_timer(&ep->timer);
free_irq(dev->irq, dev);
for (i = 0; i < RX_RING_SIZE; i++) {
struct sk_buff *skb = ep->rx_skbuff[i];
ep->rx_skbuff[i] = 0;
ep->rx_ring[i].rxstatus = 0;
ep->rx_ring[i].buflength = 0;
ep->rx_ring[i].bufaddr = 0xBADF00D0;
if (skb) {
#if LINUX_VERSION_CODE < 0x20100
skb->free = 1;
#endif
dev_free_skb(skb);
}
}
for (i = 0; i < TX_RING_SIZE; i++) {
if (ep->tx_skbuff[i])
dev_free_skb(ep->tx_skbuff[i]);
ep->tx_skbuff[i] = 0;
}
outl(0x440008, ioaddr + GENCTL);
MOD_DEC_USE_COUNT;
return 0;
}
static struct net_device_stats *epic_get_stats(struct net_device *dev)
{
struct epic_private *ep = (struct epic_private *)dev->priv;
long ioaddr = dev->base_addr;
if (netif_running(dev)) {
ep->stats.rx_missed_errors += inb(ioaddr + MPCNT);
ep->stats.rx_frame_errors += inb(ioaddr + ALICNT);
ep->stats.rx_crc_errors += inb(ioaddr + CRCCNT);
}
return &ep->stats;
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
struct epic_private *ep = (struct epic_private *)dev->priv;
unsigned char mc_filter[8];
u32 new_rx_mode;
int i;
if (dev->flags & IFF_PROMISC) {
new_rx_mode = 0x002C;
printk(KERN_INFO "%s: Promiscuous mode enabled.\n", dev->name);
memset(mc_filter, 0xff, sizeof(mc_filter));
} else if ((dev->mc_count > 0) || (dev->flags & IFF_ALLMULTI)) {
memset(mc_filter, 0xff, sizeof(mc_filter));
new_rx_mode = 0x000C;
} else if (dev->mc_count == 0) {
memset(mc_filter, 0, sizeof(mc_filter));
new_rx_mode = 0x0004;
} else {
struct dev_mc_list *mclist;
memset(mc_filter, 0, sizeof(mc_filter));
for (i = 0, mclist = dev->mc_list; mclist && i < dev->mc_count;
i++, mclist = mclist->next)
set_bit(ether_crc_le(ETH_ALEN, mclist->dmi_addr) & 0x3f,
mc_filter);
new_rx_mode = 0x000C;
}
if (ep->cur_rx_mode != new_rx_mode) {
ep->cur_rx_mode = new_rx_mode;
outl(new_rx_mode, ioaddr + RxCtrl);
}
if (memcmp(mc_filter, ep->mc_filter, sizeof(mc_filter))) {
for (i = 0; i < 4; i++)
outw(((u16 *)mc_filter)[i], ioaddr + MC0 + i*4);
memcpy(ep->mc_filter, mc_filter, sizeof(mc_filter));
}
return;
}
static int mii_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
struct epic_private *ep = (void *)dev->priv;
long ioaddr = dev->base_addr;
u16 *data = (u16 *)&rq->ifr_data;
u32 *data32 = (void *)&rq->ifr_data;
switch(cmd) {
case 0x8947: case 0x89F0:
data[0] = ep->phys[0] & 0x1f;
case 0x8948: case 0x89F1:
if (! netif_running(dev)) {
outl(0x0200, ioaddr + GENCTL);
outl((inl(ioaddr + NVCTL) & ~0x003C) | 0x4800, ioaddr + NVCTL);
}
data[3] = mdio_read(dev, data[0] & 0x1f, data[1] & 0x1f);
#if defined(PWRDWN_AFTER_IOCTL)
if (! netif_running(dev)) {
outl(0x0008, ioaddr + GENCTL);
outl((inl(ioaddr + NVCTL) & ~0x483C) | 0x0000, ioaddr + NVCTL);
}
#endif
return 0;
case 0x8949: case 0x89F2:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
if (! netif_running(dev)) {
outl(0x0200, ioaddr + GENCTL);
outl((inl(ioaddr + NVCTL) & ~0x003C) | 0x4800, ioaddr + NVCTL);
}
if (data[0] == ep->phys[0]) {
u16 value = data[2];
switch (data[1]) {
case 0:
ep->duplex_lock = (value & 0x9000) ? 0 : 1;
if (ep->duplex_lock)
ep->full_duplex = (value & 0x0100) ? 1 : 0;
break;
case 4: ep->advertising = value; break;
}
}
mdio_write(dev, data[0] & 0x1f, data[1] & 0x1f, data[2]);
#if defined(PWRDWN_AFTER_IOCTL)
if (! netif_running(dev)) {
outl(0x0008, ioaddr + GENCTL);
outl((inl(ioaddr + NVCTL) & ~0x483C) | 0x0000, ioaddr + NVCTL);
}
#endif
return 0;
case SIOCGPARAMS:
data32[0] = ep->msg_level;
data32[1] = ep->multicast_filter_limit;
data32[2] = ep->max_interrupt_work;
data32[3] = ep->rx_copybreak;
return 0;
case SIOCSPARAMS:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
ep->msg_level = data32[0];
ep->multicast_filter_limit = data32[1];
ep->max_interrupt_work = data32[2];
ep->rx_copybreak = data32[3];
return 0;
default:
return -EOPNOTSUPP;
}
}
static int epic_pwr_event(void *dev_instance, int event)
{
struct net_device *dev = dev_instance;
struct epic_private *ep = (struct epic_private *)dev->priv;
long ioaddr = dev->base_addr;
if (ep->msg_level & NETIF_MSG_LINK)
printk(KERN_DEBUG "%s: Handling power event %d.\n", dev->name, event);
switch(event) {
case DRV_SUSPEND:
epic_pause(dev);
outl(0x0008, ioaddr + GENCTL);
break;
case DRV_RESUME:
epic_start(dev, 1);
break;
case DRV_DETACH: {
struct net_device **devp, **next;
if (dev->flags & IFF_UP) {
dev_close(dev);
dev->flags &= ~(IFF_UP|IFF_RUNNING);
}
unregister_netdev(dev);
release_region(dev->base_addr, pci_id_tbl[ep->chip_id].io_size);
#ifndef USE_IO_OPS
iounmap((char *)dev->base_addr);
#endif
for (devp = &root_epic_dev; *devp; devp = next) {
next = &((struct epic_private *)(*devp)->priv)->next_module;
if (*devp == dev) {
*devp = *next;
break;
}
}
if (ep->priv_addr)
kfree(ep->priv_addr);
kfree(dev);
break;
}
}
return 0;
}
#ifdef CARDBUS
#include <pcmcia/driver_ops.h>
static dev_node_t *epic_attach(dev_locator_t *loc)
{
struct net_device *dev;
u16 dev_id;
u32 pciaddr;
u8 bus, devfn, irq;
long ioaddr;
if (loc->bus != LOC_PCI) return NULL;
bus = loc->b.pci.bus; devfn = loc->b.pci.devfn;
printk(KERN_DEBUG "epic_attach(bus %d, function %d)\n", bus, devfn);
#ifdef USE_IO_OPS
pcibios_read_config_dword(bus, devfn, PCI_BASE_ADDRESS_0, &pciaddr);
ioaddr = pciaddr & PCI_BASE_ADDRESS_IO_MASK;
#else
pcibios_read_config_dword(bus, devfn, PCI_BASE_ADDRESS_1, &pciaddr);
ioaddr = (long)ioremap(pciaddr & PCI_BASE_ADDRESS_MEM_MASK,
pci_id_tbl[1].io_size);
#endif
pcibios_read_config_byte(bus, devfn, PCI_INTERRUPT_LINE, &irq);
pcibios_read_config_word(bus, devfn, PCI_DEVICE_ID, &dev_id);
if (ioaddr == 0 || irq == 0) {
printk(KERN_ERR "The EPIC/C CardBus Ethernet interface at %d/%d was "
"not assigned an %s.\n"
KERN_ERR "  It will not be activated.\n",
bus, devfn, ioaddr == 0 ? "address" : "IRQ");
return NULL;
}
dev = epic_probe1(pci_find_slot(bus, devfn), NULL, ioaddr, irq, 1, 0);
if (dev) {
dev_node_t *node = kmalloc(sizeof(dev_node_t), GFP_KERNEL);
strcpy(node->dev_name, dev->name);
node->major = node->minor = 0;
node->next = NULL;
MOD_INC_USE_COUNT;
return node;
}
return NULL;
}
static void epic_suspend(dev_node_t *node)
{
struct net_device **devp, **next;
printk(KERN_INFO "epic_suspend(%s)\n", node->dev_name);
for (devp = &root_epic_dev; *devp; devp = next) {
next = &((struct epic_private *)(*devp)->priv)->next_module;
if (strcmp((*devp)->name, node->dev_name) == 0) break;
}
if (*devp) {
long ioaddr = (*devp)->base_addr;
epic_pause(*devp);
outl(0x0008, ioaddr + GENCTL);
}
}
static void epic_resume(dev_node_t *node)
{
struct net_device **devp, **next;
printk(KERN_INFO "epic_resume(%s)\n", node->dev_name);
for (devp = &root_epic_dev; *devp; devp = next) {
next = &((struct epic_private *)(*devp)->priv)->next_module;
if (strcmp((*devp)->name, node->dev_name) == 0) break;
}
if (*devp) {
epic_start(*devp, 1);
}
}
static void epic_detach(dev_node_t *node)
{
struct net_device **devp, **next;
printk(KERN_INFO "epic_detach(%s)\n", node->dev_name);
for (devp = &root_epic_dev; *devp; devp = next) {
next = &((struct epic_private *)(*devp)->priv)->next_module;
if (strcmp((*devp)->name, node->dev_name) == 0) break;
}
if (*devp) {
unregister_netdev(*devp);
release_region((*devp)->base_addr, EPIC_TOTAL_SIZE);
#ifndef USE_IO_OPS
iounmap((char *)(*devp)->base_addr);
#endif
kfree(*devp);
*devp = *next;
kfree(node);
MOD_DEC_USE_COUNT;
}
}
struct driver_operations epic_ops = {
"epic_cb", epic_attach, epic_suspend, epic_resume, epic_detach
};
#endif
#ifdef MODULE
int init_module(void)
{
printk(KERN_INFO "%s", version);
#ifdef CARDBUS
register_driver(&epic_ops);
return 0;
#else
return pci_drv_register(&epic_drv_id, NULL);
#endif
}
void cleanup_module(void)
{
struct net_device *next_dev;
#ifdef CARDBUS
unregister_driver(&epic_ops);
#else
pci_drv_unregister(&epic_drv_id);
#endif
while (root_epic_dev) {
struct epic_private *ep = (struct epic_private *)root_epic_dev->priv;
unregister_netdev(root_epic_dev);
release_region(root_epic_dev->base_addr, pci_id_tbl[ep->chip_id].io_size);
#ifndef USE_IO_OPS
iounmap((char *)root_epic_dev->base_addr);
#endif
next_dev = ep->next_module;
if (ep->priv_addr)
kfree(ep->priv_addr);
kfree(root_epic_dev);
root_epic_dev = next_dev;
}
}
#else
int epic100_probe(struct net_device *dev)
{
int retval = pci_drv_register(&epic_drv_id, dev);
if (retval >= 0)
printk(KERN_INFO "%s", version);
return retval;
}
#endif