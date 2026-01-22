static const char version1[] =
"eepro100.c:v1.28 7/22/2003 Donald Becker <becker@scyld.com>\n";
static const char version2[] =
"  http:
static int debug = 2;
static int congenb = 0;
static int txfifo = 8;
static int rxfifo = 8;
static int txdmacount = 128;
static int rxdmacount = 0;
static int rx_copybreak = 200;
static int max_interrupt_work = 20;
static int multicast_filter_limit = 64;
#define MAX_UNITS 8
static int options[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
static int full_duplex[MAX_UNITS] = {-1, -1, -1, -1, -1, -1, -1, -1};
#define TX_RING_SIZE 32
#define RX_RING_SIZE 32
#define TX_QUEUE_LIMIT 12
#define TX_QUEUE_UNFULL 8
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
#include <linux/delay.h>
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
#define virt_to_le32desc(addr) cpu_to_le32(virt_to_bus(addr))
#define le32desc_to_virt(addr) bus_to_virt(le32_to_cpu(addr))
#if (LINUX_VERSION_CODE >= 0x20100) && defined(MODULE)
char kernel_version[] = UTS_RELEASE;
#endif
MODULE_AUTHOR("Donald Becker <becker@scyld.com>");
MODULE_DESCRIPTION("Intel PCI EtherExpressPro 100 driver");
MODULE_LICENSE("GPL");
MODULE_PARM(debug, "i");
MODULE_PARM(options, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(full_duplex, "1-" __MODULE_STRING(MAX_UNITS) "i");
MODULE_PARM(congenb, "i");
MODULE_PARM(txfifo, "i");
MODULE_PARM(rxfifo, "i");
MODULE_PARM(txdmacount, "i");
MODULE_PARM(rxdmacount, "i");
MODULE_PARM(rx_copybreak, "i");
MODULE_PARM(max_interrupt_work, "i");
MODULE_PARM(multicast_filter_limit, "i");
#ifdef MODULE_PARM_DESC
MODULE_PARM_DESC(debug, "EEPro100 message level (0-31)");
MODULE_PARM_DESC(options,
"EEPro100: force fixed speed+duplex 0x10 0x20 0x100 0x200");
MODULE_PARM_DESC(max_interrupt_work,
"EEPro100 maximum events handled per interrupt");
MODULE_PARM_DESC(full_duplex, "EEPro100 set to forced full duplex when not 0"
" (deprecated)");
MODULE_PARM_DESC(rx_copybreak,
"EEPro100 copy breakpoint for copy-only-tiny-frames");
MODULE_PARM_DESC(multicast_filter_limit,
"EEPro100 breakpoint for switching to Rx-all-multicast");
#endif
static void *speedo_found1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int fnd_cnt);
static int speedo_pwr_event(void *dev_instance, int event);
enum chip_capability_flags { ResetMII=1, HasChksum=2};
#ifdef USE_IO_OPS
#define SPEEDO_IOTYPE PCI_USES_MASTER|PCI_USES_IO|PCI_ADDR1
#define SPEEDO_SIZE 32
#else
#define SPEEDO_IOTYPE PCI_USES_MASTER|PCI_USES_MEM|PCI_ADDR0
#define SPEEDO_SIZE 0x1000
#endif
struct pci_id_info static pci_id_tbl[] = {
{"Intel PCI EtherExpress Pro100 82865", { 0x12278086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel PCI EtherExpress Pro100 Smart (i960RP/RD)",
{ 0x12288086, 0xffffffff,}, SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel i82559 rev 8", { 0x12298086, ~0, 0,0, 8,0xff},
SPEEDO_IOTYPE, SPEEDO_SIZE, HasChksum, },
{"Intel PCI EtherExpress Pro100", { 0x12298086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel EtherExpress Pro/100+ i82559ER", { 0x12098086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, ResetMII, },
{"Intel EtherExpress Pro/100 type 1029", { 0x10298086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel EtherExpress Pro/100 type 1030", { 0x10308086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 V Network", { 0x24498086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel PCI LAN0 Controller 82801E", { 0x24598086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel PCI LAN1 Controller 82801E", { 0x245D8086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VE (type 1031)", { 0x10318086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VE (type 1032)", { 0x10328086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VE (type 1033)", { 0x10338086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VE (type 1034)", { 0x10348086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VE (type 1035)", { 0x10358086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VM (type 1038)", { 0x10388086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VM (type 1039)", { 0x10398086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VM (type 103a)", { 0x103a8086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"HP/Compaq D510 Intel Pro/100 VM",
{ 0x103b8086, 0xffffffff, 0x00120e11, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VM (type 103b)", { 0x103b8086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VE (type 103D)", { 0x103d8086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VE (type 103E)", { 0x103e8086, 0xffffffff,},
SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel EtherExpress Pro/100 865G Northbridge type 1051",
{ 0x10518086, 0xffffffff,}, SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel PCI to PCI Bridge EtherExpress Pro100 Server Adapter",
{ 0x52008086, 0xffffffff,}, SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel PCI EtherExpress Pro100 Server Adapter",
{ 0x52018086, 0xffffffff,}, SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 VM (unknown type series 1030)",
{ 0x10308086, 0xfff0ffff,}, SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{"Intel Pro/100 (unknown type series 1050)",
{ 0x10508086, 0xfff0ffff,}, SPEEDO_IOTYPE, SPEEDO_SIZE, 0, },
{0,},
};
struct drv_id_info eepro100_drv_id = {
"eepro100", PCI_HOTSWAP, PCI_CLASS_NETWORK_ETHERNET<<8, pci_id_tbl,
speedo_found1, speedo_pwr_event, };
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
enum speedo_offsets {
SCBStatus = 0, SCBCmd = 2,
SCBPointer = 4,
SCBPort = 8,
SCBflash = 12, SCBeeprom = 14,
SCBCtrlMDI = 16,
SCBEarlyRx = 20,
};
enum commands {
CmdNOp = 0, CmdIASetup = 0x10000, CmdConfigure = 0x20000,
CmdMulticastList = 0x30000, CmdTx = 0x40000, CmdTDR = 0x50000,
CmdDump = 0x60000, CmdDiagnose = 0x70000,
CmdSuspend = 0x40000000,
CmdIntr = 0x20000000,
CmdTxFlex = 0x00080000,
};
#if defined(__i386__)
#define clear_suspend(cmd) ((char *)(&(cmd)->cmd_status))[3] &= ~0x40
#elif defined(__alpha__) || defined(__x86_64) || defined(__ia64)
#define clear_suspend(cmd) clear_bit(30, &(cmd)->cmd_status)
#elif defined(__powerpc__) || defined(__sparc__) || (__BIG_ENDIAN)
#define clear_suspend(cmd) clear_bit(6, &(cmd)->cmd_status)
#else
#warning Undefined architecture.
#define clear_suspend(cmd) (cmd)->cmd_status &= cpu_to_le32(~CmdSuspend)
#endif
enum SCBCmdBits {
SCBMaskCmdDone=0x8000, SCBMaskRxDone=0x4000, SCBMaskCmdIdle=0x2000,
SCBMaskRxSuspend=0x1000, SCBMaskEarlyRx=0x0800, SCBMaskFlowCtl=0x0400,
SCBTriggerIntr=0x0200, SCBMaskAll=0x0100,
CUStart=0x0010, CUResume=0x0020, CUHiPriStart=0x0030, CUStatsAddr=0x0040,
CUShowStats=0x0050,
CUCmdBase=0x0060,
CUDumpStats=0x0070,
CUHiPriResume=0x00b0,
RxStart=0x0001, RxResume=0x0002, RxAbort=0x0004, RxAddrLoad=0x0006,
RxResumeNoResources=0x0007,
};
enum intr_status_bits {
IntrCmdDone=0x8000, IntrRxDone=0x4000, IntrCmdIdle=0x2000,
IntrRxSuspend=0x1000, IntrMIIDone=0x0800, IntrDrvrIntr=0x0400,
IntrAllNormal=0xfc00,
};
enum SCBPort_cmds {
PortReset=0, PortSelfTest=1, PortPartialReset=2, PortDump=3,
};
struct descriptor {
s32 cmd_status;
u32 link;
unsigned char params[0];
};
struct RxFD {
s32 status;
u32 link;
u32 rx_buf_addr;
u32 count;
};
enum RxFD_bits {
RxComplete=0x8000, RxOK=0x2000,
RxErrCRC=0x0800, RxErrAlign=0x0400, RxErrTooBig=0x0200, RxErrSymbol=0x0010,
RxEth2Type=0x0020, RxNoMatch=0x0004, RxNoIAMatch=0x0002,
TxUnderrun=0x1000, StatusComplete=0x8000,
};
struct TxFD {
s32 status;
u32 link;
u32 tx_desc_addr;
s32 count;
u32 tx_buf_addr0;
s32 tx_buf_size0;
u32 tx_buf_addr1;
s32 tx_buf_size1;
};
struct speedo_stats {
u32 tx_good_frames;
u32 tx_coll16_errs;
u32 tx_late_colls;
u32 tx_underruns;
u32 tx_lost_carrier;
u32 tx_deferred;
u32 tx_one_colls;
u32 tx_multi_colls;
u32 tx_total_colls;
u32 rx_good_frames;
u32 rx_crc_errs;
u32 rx_align_errs;
u32 rx_resource_errs;
u32 rx_overrun_errs;
u32 rx_colls_errs;
u32 rx_runt_errs;
u32 done_marker;
};
struct speedo_private {
struct TxFD tx_ring[TX_RING_SIZE];
struct RxFD *rx_ringp[RX_RING_SIZE];
struct speedo_stats lstats;
struct sk_buff* tx_skbuff[TX_RING_SIZE];
struct sk_buff* rx_skbuff[RX_RING_SIZE];
struct descriptor *last_cmd;
unsigned int cur_tx, dirty_tx;
spinlock_t lock;
u32 tx_threshold;
unsigned long last_cmd_time;
struct RxFD *last_rxf;
unsigned int cur_rx, dirty_rx;
unsigned int rx_buf_sz;
long last_rx_time;
int rx_copybreak;
int msg_level;
int max_interrupt_work;
struct net_device *next_module;
void *priv_addr;
struct net_device_stats stats;
int alloc_failures;
int chip_id, drv_flags;
struct pci_dev *pci_dev;
unsigned char acpi_pwr;
struct timer_list timer;
int mc_setup_frm_len;
struct descriptor *mc_setup_frm;
int mc_setup_busy;
int multicast_filter_limit;
int in_interrupt;
int rx_mode;
unsigned int tx_full:1;
unsigned int full_duplex:1;
unsigned int flow_ctrl:1;
unsigned int rx_bug:1;
unsigned int rx_bug10:1;
unsigned int rx_bug100:1;
unsigned int polling:1;
unsigned int medialock:1;
unsigned char default_port;
unsigned short phy[2];
unsigned short advertising;
unsigned short partner;
long last_reset;
};
enum rx_mode_bits {
AcceptAllMulticast=0x01, AcceptAllPhys=0x02,
AcceptErr=0x80, AcceptRunt=0x10,
AcceptBroadcast=0x08, AcceptMulticast=0x04,
AcceptMyPhys=0x01, RxInvalidMode=0x7f
};
const char i82557_config_cmd[22] = {
22, 0x08, 0, 0, 0, 0, 0x32, 0x03, 1,
0, 0x2E, 0, 0x60, 0,
0xf2, 0x48, 0, 0x40, 0xf2, 0x80,
0x3f, 0x05, };
const char i82558_config_cmd[22] = {
22, 0x08, 0, 1, 0, 0, 0x22, 0x03, 1,
0, 0x2E, 0, 0x60, 0x08, 0x88,
0x68, 0, 0x40, 0xf2, 0xBD,
0x31, 0x05, };
static const char *phys[] = {
"None", "i82553-A/B", "i82553-C", "i82503",
"DP83840", "80c240", "80c24", "i82555",
"unknown-8", "unknown-9", "DP83840A", "unknown-11",
"unknown-12", "unknown-13", "unknown-14", "unknown-15", };
enum phy_chips { NonSuchPhy=0, I82553AB, I82553C, I82503, DP83840, S80C240,
S80C24, I82555, DP83840A=10, };
static const char is_mii[] = { 0, 1, 1, 0, 1, 1, 0, 1 };
#define EE_READ_CMD (6)
static int do_eeprom_cmd(long ioaddr, int cmd, int cmd_len);
static int mdio_read(struct net_device *dev, int phy_id, int location);
static int mdio_write(long ioaddr, int phy_id, int location, int value);
static int speedo_open(struct net_device *dev);
static void speedo_resume(struct net_device *dev);
static void speedo_timer(unsigned long data);
static void speedo_init_rx_ring(struct net_device *dev);
static void speedo_tx_timeout(struct net_device *dev);
static int speedo_start_xmit(struct sk_buff *skb, struct net_device *dev);
static int speedo_rx(struct net_device *dev);
static void speedo_interrupt(int irq, void *dev_instance, struct pt_regs *regs);
static int speedo_close(struct net_device *dev);
static struct net_device_stats *speedo_get_stats(struct net_device *dev);
static int speedo_ioctl(struct net_device *dev, struct ifreq *rq, int cmd);
static void set_rx_mode(struct net_device *dev);
#ifdef honor_default_port
static int mii_ctrl[8] = { 0x3300, 0x3100, 0x0000, 0x0100,
0x2000, 0x2100, 0x0400, 0x3100};
#endif
static struct net_device *root_speedo_dev = NULL;
static void *speedo_found1(struct pci_dev *pdev, void *init_dev,
long ioaddr, int irq, int chip_idx, int card_idx)
{
struct net_device *dev;
struct speedo_private *sp;
void *priv_mem;
int i, option;
u16 eeprom[0x100];
int acpi_idle_state = 0;
dev = init_etherdev(init_dev, 0);
if (!dev)
return NULL;
if (dev->mem_start > 0)
option = dev->mem_start;
else if (card_idx >= 0 && options[card_idx] >= 0)
option = options[card_idx];
else
option = -1;
acpi_idle_state = acpi_set_pwr_state(pdev, ACPI_D0);
{
u16 sum = 0;
int j;
int read_cmd, ee_size;
if ((do_eeprom_cmd(ioaddr, EE_READ_CMD << 24, 27) & 0xffe0000)
== 0xffe0000) {
ee_size = 0x100;
read_cmd = EE_READ_CMD << 24;
} else {
ee_size = 0x40;
read_cmd = EE_READ_CMD << 22;
}
for (j = 0, i = 0; i < ee_size; i++) {
u16 value = do_eeprom_cmd(ioaddr, read_cmd | (i << 16), 27);
eeprom[i] = value;
sum += value;
if (i < 3) {
dev->dev_addr[j++] = value;
dev->dev_addr[j++] = value >> 8;
}
}
if (sum != 0xBABA)
printk(KERN_WARNING "%s: Invalid EEPROM checksum %#4.4x, "
"check settings before activating this device!\n",
dev->name, sum);
}
outl(PortReset, ioaddr + SCBPort);
printk(KERN_INFO "%s: %s%s at %#3lx, ", dev->name,
eeprom[3] & 0x0100 ? "OEM " : "", pci_id_tbl[chip_idx].name,
ioaddr);
for (i = 0; i < 5; i++)
printk("%2.2X:", dev->dev_addr[i]);
printk("%2.2X, IRQ %d.\n", dev->dev_addr[i], irq);
sp = priv_mem = kmalloc(sizeof(*sp), GFP_KERNEL);
if (priv_mem == NULL)
return NULL;
dev->base_addr = ioaddr;
dev->irq = irq;
#ifndef kernel_bloat
{
const char *connectors[] = {" RJ45", " BNC", " AUI", " MII"};
s32 *volatile self_test_results;
int boguscnt = 16000;
printk(KERN_INFO "  Board assembly %4.4x%2.2x-%3.3d, Physical"
" connectors present:",
eeprom[8], eeprom[9]>>8, eeprom[9] & 0xff);
for (i = 0; i < 4; i++)
if (eeprom[5] & (1<<i))
printk("%s", connectors[i]);
printk("\n"KERN_INFO"  Primary interface chip %s PHY #%d.\n",
phys[(eeprom[6]>>8)&15], eeprom[6] & 0x1f);
if (eeprom[7] & 0x0700)
printk(KERN_INFO "    Secondary interface chip %s.\n",
phys[(eeprom[7]>>8)&7]);
if (((eeprom[6]>>8) & 0x3f) == DP83840
|| ((eeprom[6]>>8) & 0x3f) == DP83840A) {
int mdi_reg23 = mdio_read(dev, eeprom[6] & 0x1f, 23) | 0x0422;
if (congenb)
mdi_reg23 |= 0x0100;
printk(KERN_INFO"  DP83840 specific setup, setting register 23 to %4.4x.\n",
mdi_reg23);
mdio_write(ioaddr, eeprom[6] & 0x1f, 23, mdi_reg23);
}
if ((option >= 0) && (option & 0x330)) {
printk(KERN_INFO "  Forcing %dMbs %s-duplex operation.\n",
(option & 0x300 ? 100 : 10),
(option & 0x220 ? "full" : "half"));
mdio_write(ioaddr, eeprom[6] & 0x1f, 0,
((option & 0x300) ? 0x2000 : 0) |
((option & 0x220) ? 0x0100 : 0));
} else {
int mii_bmcrctrl = mdio_read(dev, eeprom[6] & 0x1f, 0);
if ((mii_bmcrctrl & 0x3100) == 0)
mdio_write(ioaddr, eeprom[6] & 0x1f, 0, 0x8000);
}
if (eeprom[10] & 0x0002)
printk(KERN_INFO "\n" KERN_INFO "  ** The configuration "
"EEPROM enables Sleep Mode.\n" KERN_INFO "\n"
"  ** This will cause PCI bus errors!\n"
KERN_INFO "  ** Update the configuration EEPROM "
"with the eepro100-diag program.\n" );
if (eeprom[6] == 0)
printk(KERN_INFO "  ** The configuration EEPROM does not have a "
"transceiver type set.\n" KERN_INFO "\n"
"  ** This will cause configuration problems and prevent "
"monitoring the link!\n"
KERN_INFO "  ** Update the configuration EEPROM "
"with the eepro100-diag program.\n" );
self_test_results = (s32*)(&sp->lstats);
self_test_results[0] = 0;
self_test_results[1] = -1;
outl(virt_to_bus(self_test_results) | PortSelfTest, ioaddr + SCBPort);
do {
udelay(10);
} while (self_test_results[1] == -1 && --boguscnt >= 0);
if (boguscnt < 0) {
printk(KERN_ERR "Self test failed, status %8.8x:\n"
KERN_ERR " Failure to initialize the i82557.\n"
KERN_ERR " Verify that the card is a bus-master"
" capable slot.\n",
self_test_results[1]);
} else
printk(KERN_INFO "  General self-test: %s.\n"
KERN_INFO "  Serial sub-system self-test: %s.\n"
KERN_INFO "  Internal registers self-test: %s.\n"
KERN_INFO "  ROM checksum self-test: %s (%#8.8x).\n",
self_test_results[1] & 0x1000 ? "failed" : "passed",
self_test_results[1] & 0x0020 ? "failed" : "passed",
self_test_results[1] & 0x0008 ? "failed" : "passed",
self_test_results[1] & 0x0004 ? "failed" : "passed",
self_test_results[0]);
}
#endif
outl(PortReset, ioaddr + SCBPort);
acpi_set_pwr_state(pdev, acpi_idle_state);
request_region(ioaddr, pci_id_tbl[chip_idx].io_size, dev->name);
dev->priv = sp;
memset(sp, 0, sizeof(*sp));
sp->next_module = root_speedo_dev;
root_speedo_dev = dev;
sp->priv_addr = priv_mem;
sp->pci_dev = pdev;
sp->chip_id = chip_idx;
sp->drv_flags = pci_id_tbl[chip_idx].drv_flags;
sp->acpi_pwr = acpi_idle_state;
sp->msg_level = (1 << debug) - 1;
sp->rx_copybreak = rx_copybreak;
sp->max_interrupt_work = max_interrupt_work;
sp->multicast_filter_limit = multicast_filter_limit;
sp->full_duplex = option >= 0 && (option & 0x220) ? 1 : 0;
if (card_idx >= 0) {
if (full_duplex[card_idx] >= 0)
sp->full_duplex = full_duplex[card_idx];
}
sp->default_port = option >= 0 ? (option & 0x0f) : 0;
if (sp->full_duplex)
sp->medialock = 1;
sp->phy[0] = eeprom[6];
sp->phy[1] = eeprom[7];
sp->rx_bug = (eeprom[3] & 0x03) == 3 ? 0 : 1;
if (sp->rx_bug)
printk(KERN_INFO "  Receiver lock-up workaround activated.\n");
dev->open = &speedo_open;
dev->hard_start_xmit = &speedo_start_xmit;
dev->stop = &speedo_close;
dev->get_stats = &speedo_get_stats;
dev->set_multicast_list = &set_rx_mode;
dev->do_ioctl = &speedo_ioctl;
return dev;
}
static inline void wait_for_cmd_done(struct net_device *dev)
{
long cmd_ioaddr = dev->base_addr + SCBCmd;
int wait = 0;
int delayed_cmd;
do
if (inb(cmd_ioaddr) == 0) return;
while(++wait <= 100);
delayed_cmd = inb(cmd_ioaddr);
do
if (inb(cmd_ioaddr) == 0) break;
while(++wait <= 10000);
printk(KERN_ERR "%s: Command %2.2x was not immediately accepted, "
"%d ticks!\n",
dev->name, delayed_cmd, wait);
}
static void do_slow_command(struct net_device *dev, int cmd)
{
long cmd_ioaddr = dev->base_addr + SCBCmd;
int wait = 0;
do
if (inb(cmd_ioaddr) == 0) break;
while(++wait <= 200);
if (wait > 100)
printk(KERN_ERR "%s: Command %4.4x was never accepted (%d polls)!\n",
dev->name, inb(cmd_ioaddr), wait);
outb(cmd, cmd_ioaddr);
for (wait = 0; wait <= 100; wait++)
if (inb(cmd_ioaddr) == 0) return;
for (; wait <= 20000; wait++)
if (inb(cmd_ioaddr) == 0) return;
else udelay(1);
printk(KERN_ERR "%s: Command %4.4x was not accepted after %d polls!"
"  Current status %8.8x.\n",
dev->name, cmd, wait, (int)inl(dev->base_addr + SCBStatus));
}
#define EE_SHIFT_CLK 0x01
#define EE_CS 0x02
#define EE_DATA_WRITE 0x04
#define EE_DATA_READ 0x08
#define EE_ENB (0x4800 | EE_CS)
#define EE_WRITE_0 0x4802
#define EE_WRITE_1 0x4806
#define EE_OFFSET SCBeeprom
#ifndef USE_IO_OPS
#define eeprom_delay(ee_addr) writew(readw(ee_addr), ee_addr)
#else
#define eeprom_delay(ee_addr) inw(ee_addr)
#endif
static int do_eeprom_cmd(long ioaddr, int cmd, int cmd_len)
{
unsigned retval = 0;
long ee_addr = ioaddr + SCBeeprom;
outw(EE_ENB | EE_SHIFT_CLK, ee_addr);
do {
short dataval = (cmd & (1 << cmd_len)) ? EE_WRITE_1 : EE_WRITE_0;
outw(dataval, ee_addr);
eeprom_delay(ee_addr);
outw(dataval | EE_SHIFT_CLK, ee_addr);
eeprom_delay(ee_addr);
retval = (retval << 1) | ((inw(ee_addr) & EE_DATA_READ) ? 1 : 0);
} while (--cmd_len >= 0);
outw(EE_ENB, ee_addr);
outw(EE_ENB & ~EE_CS, ee_addr);
return retval;
}
static int mdio_read(struct net_device *dev, int phy_id, int location)
{
long ioaddr = dev->base_addr;
int val, boguscnt = 64*10;
outl(0x08000000 | (location<<16) | (phy_id<<21), ioaddr + SCBCtrlMDI);
do {
val = inl(ioaddr + SCBCtrlMDI);
if (--boguscnt < 0) {
printk(KERN_ERR "%s: mdio_read() timed out with val = %8.8x.\n",
dev->name, val);
break;
}
} while (! (val & 0x10000000));
return val & 0xffff;
}
static int mdio_write(long ioaddr, int phy_id, int location, int value)
{
int val, boguscnt = 64*10;
outl(0x04000000 | (location<<16) | (phy_id<<21) | value,
ioaddr + SCBCtrlMDI);
do {
val = inl(ioaddr + SCBCtrlMDI);
if (--boguscnt < 0) {
printk(KERN_ERR" mdio_write() timed out with val = %8.8x.\n", val);
break;
}
} while (! (val & 0x10000000));
return val & 0xffff;
}
static int
speedo_open(struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
MOD_INC_USE_COUNT;
acpi_set_pwr_state(sp->pci_dev, ACPI_D0);
if (sp->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: speedo_open() irq %d.\n", dev->name, dev->irq);
sp->cur_tx = 0;
sp->dirty_tx = 0;
sp->last_cmd = 0;
sp->tx_full = 0;
sp->lock = (spinlock_t) SPIN_LOCK_UNLOCKED;
sp->polling = sp->in_interrupt = 0;
dev->if_port = sp->default_port;
if ((sp->phy[0] & 0x8000) == 0)
sp->advertising = mdio_read(dev, sp->phy[0] & 0x1f, 4);
if ((sp->drv_flags & ResetMII) &&
(sp->phy[0] & 0x8000) == 0) {
int phy_addr = sp->phy[0] & 0x1f ;
#ifdef honor_default_port
mdio_write(ioaddr, phy_addr, 0, mii_ctrl[dev->default_port & 7]);
#else
mdio_write(ioaddr, phy_addr, 0, 0x3300);
#endif
}
if (request_irq(dev->irq, &speedo_interrupt, SA_SHIRQ, dev->name, dev)) {
MOD_DEC_USE_COUNT;
return -EAGAIN;
}
speedo_init_rx_ring(dev);
speedo_resume(dev);
netif_start_tx_queue(dev);
sp->mc_setup_frm = NULL;
sp->mc_setup_frm_len = 0;
sp->mc_setup_busy = 0;
sp->rx_mode = RxInvalidMode;
sp->flow_ctrl = sp->partner = 0;
set_rx_mode(dev);
if (sp->msg_level & NETIF_MSG_IFUP)
printk(KERN_DEBUG "%s: Done speedo_open(), status %8.8x.\n",
dev->name, (int)inw(ioaddr + SCBStatus));
init_timer(&sp->timer);
sp->timer.expires = jiffies + 3*HZ;
sp->timer.data = (unsigned long)dev;
sp->timer.function = &speedo_timer;
add_timer(&sp->timer);
if ((sp->phy[0] & 0x8000) == 0)
mdio_read(dev, sp->phy[0] & 0x1f, 0);
return 0;
}
static void speedo_resume(struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
outw(SCBMaskAll, ioaddr + SCBCmd);
sp->tx_threshold = 0x01208000;
wait_for_cmd_done(dev);
if (inb(ioaddr + SCBCmd)) {
outl(PortPartialReset, ioaddr + SCBPort);
udelay(10);
}
outl(0, ioaddr + SCBPointer);
inl(ioaddr + SCBPointer);
udelay(10);
do_slow_command(dev, RxAddrLoad);
do_slow_command(dev, CUCmdBase);
outl(virt_to_bus(&sp->lstats), ioaddr + SCBPointer);
inl(ioaddr + SCBPointer);
outb(CUStatsAddr, ioaddr + SCBCmd);
sp->lstats.done_marker = 0;
wait_for_cmd_done(dev);
outl(virt_to_bus(sp->rx_ringp[sp->cur_rx % RX_RING_SIZE]),
ioaddr + SCBPointer);
inl(ioaddr + SCBPointer);
do_slow_command(dev, RxStart);
do_slow_command(dev, CUDumpStats);
{
int entry = sp->cur_tx++ % TX_RING_SIZE;
struct descriptor *cur_cmd = (struct descriptor *)&sp->tx_ring[entry];
cur_cmd->cmd_status = cpu_to_le32((CmdSuspend | CmdIASetup) | 0xa000);
cur_cmd->link =
virt_to_le32desc(&sp->tx_ring[sp->cur_tx % TX_RING_SIZE]);
memcpy(cur_cmd->params, dev->dev_addr, 6);
if (sp->last_cmd)
clear_suspend(sp->last_cmd);
sp->last_cmd = cur_cmd;
}
outl(virt_to_bus(&sp->tx_ring[sp->dirty_tx % TX_RING_SIZE]),
ioaddr + SCBPointer);
outw(CUStart, ioaddr + SCBCmd);
}
static void speedo_timer(unsigned long data)
{
struct net_device *dev = (struct net_device *)data;
struct speedo_private *sp = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
int phy_num = sp->phy[0] & 0x1f;
int status = inw(ioaddr + SCBStatus);
if (sp->msg_level & NETIF_MSG_TIMER)
printk(KERN_DEBUG "%s: Interface monitor tick, chip status %4.4x.\n",
dev->name, status);
sp->timer.expires = jiffies + 2*HZ;
if (sp->polling) {
if (status & 0xfc00) {
speedo_interrupt(dev->irq, dev, 0);
if (jiffies - sp->last_reset > 10*HZ) {
printk(KERN_ERR "%s: IRQ %d is still blocked!\n",
dev->name, dev->irq);
sp->last_reset = jiffies;
}
} else if (jiffies - sp->last_reset > 10*HZ)
sp->polling = 0;
sp->timer.expires = jiffies + 2;
}
if ((sp->phy[0] & 0x8000) == 0) {
int partner = mdio_read(dev, phy_num, 5);
if (partner != sp->partner) {
int flow_ctrl = sp->advertising & partner & 0x0400 ? 1 : 0;
sp->partner = partner;
if (flow_ctrl != sp->flow_ctrl) {
sp->flow_ctrl = flow_ctrl;
sp->rx_mode = RxInvalidMode;
}
mdio_read(dev, phy_num, 1);
if (mdio_read(dev, phy_num, 1) & 0x0004)
netif_link_up(dev);
else
netif_link_down(dev);
}
}
if (sp->cur_tx - sp->dirty_tx > 1 &&
(jiffies - dev->trans_start) > TX_TIMEOUT &&
(jiffies - sp->last_cmd_time) > TX_TIMEOUT) {
if (status == 0xffff) {
if (jiffies - sp->last_reset > 10*HZ) {
sp->last_reset = jiffies;
printk(KERN_ERR "%s: The EEPro100 chip is missing!\n",
dev->name);
}
} else if (status & 0xfc00) {
if ( ! sp->polling) {
if (jiffies - sp->last_reset > 10*HZ) {
printk(KERN_ERR "%s: IRQ %d is physically blocked! (%4.4x)"
"Failing back to low-rate polling.\n",
dev->name, dev->irq, status);
sp->last_reset = jiffies;
}
sp->polling = 1;
}
speedo_interrupt(dev->irq, dev, 0);
sp->timer.expires = jiffies + 2;
} else {
speedo_tx_timeout(dev);
sp->last_reset = jiffies;
}
}
if (sp->rx_mode == RxInvalidMode ||
(sp->rx_bug && jiffies - sp->last_rx_time > 2*HZ)) {
set_rx_mode(dev);
}
add_timer(&sp->timer);
}
static void speedo_show_state(struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
int phy_num = sp->phy[0] & 0x1f;
int i;
if (sp->msg_level & NETIF_MSG_DRV) {
int i;
printk(KERN_DEBUG "%s: Tx ring dump,  Tx queue %d / %d:\n", dev->name,
sp->cur_tx, sp->dirty_tx);
for (i = 0; i < TX_RING_SIZE; i++)
printk(KERN_DEBUG "%s: %c%c%d %8.8x.\n", dev->name,
i == sp->dirty_tx % TX_RING_SIZE ? '*' : ' ',
i == sp->cur_tx % TX_RING_SIZE ? '=' : ' ',
i, sp->tx_ring[i].status);
}
printk(KERN_DEBUG "%s:Printing Rx ring (next to receive into %d).\n",
dev->name, sp->cur_rx);
for (i = 0; i < RX_RING_SIZE; i++)
printk(KERN_DEBUG "  Rx ring entry %d  %8.8x.\n",
i, sp->rx_ringp[i] ? (int)sp->rx_ringp[i]->status : 0);
for (i = 0; i < 16; i++) {
if (i == 6) i = 21;
printk(KERN_DEBUG "  PHY index %d register %d is %4.4x.\n",
phy_num, i, mdio_read(dev, phy_num, i));
}
}
static void
speedo_init_rx_ring(struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
struct RxFD *rxf, *last_rxf = NULL;
int i;
sp->cur_rx = 0;
#if defined(CONFIG_VLAN)
sp->rx_buf_sz = dev->mtu + 14 + sizeof(struct RxFD) + 4;
#else
sp->rx_buf_sz = dev->mtu + 14 + sizeof(struct RxFD);
#endif
if (sp->rx_buf_sz < PKT_BUF_SZ)
sp->rx_buf_sz = PKT_BUF_SZ;
for (i = 0; i < RX_RING_SIZE; i++) {
struct sk_buff *skb;
skb = dev_alloc_skb(sp->rx_buf_sz);
sp->rx_skbuff[i] = skb;
if (skb == NULL)
break;
skb->dev = dev;
rxf = (struct RxFD *)skb->tail;
sp->rx_ringp[i] = rxf;
skb_reserve(skb, sizeof(struct RxFD));
if (last_rxf)
last_rxf->link = virt_to_le32desc(rxf);
last_rxf = rxf;
rxf->status = cpu_to_le32(0x00000001);
rxf->link = 0;
#ifdef final_version
rxf->rx_buf_addr = 0xffffffff;
#else
rxf->rx_buf_addr = virt_to_bus(skb->tail);
#endif
rxf->count = cpu_to_le32((sp->rx_buf_sz - sizeof(struct RxFD)) << 16);
}
sp->dirty_rx = (unsigned int)(i - RX_RING_SIZE);
last_rxf->status = cpu_to_le32(0xC0000002);
sp->last_rxf = last_rxf;
}
static void speedo_tx_timeout(struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
int status = inw(ioaddr + SCBStatus);
printk(KERN_WARNING "%s: Transmit timed out: status %4.4x "
" %4.4x at %d/%d commands %8.8x %8.8x %8.8x.\n",
dev->name, status, (int)inw(ioaddr + SCBCmd),
sp->dirty_tx, sp->cur_tx,
sp->tx_ring[(sp->dirty_tx+0) % TX_RING_SIZE].status,
sp->tx_ring[(sp->dirty_tx+1) % TX_RING_SIZE].status,
sp->tx_ring[(sp->dirty_tx+2) % TX_RING_SIZE].status);
speedo_get_stats(dev);
speedo_show_state(dev);
if ((status & 0x00C0) != 0x0080
&& (status & 0x003C) == 0x0010 && 0) {
printk(KERN_WARNING "%s: Trying to restart the transmitter...\n",
dev->name);
outl(virt_to_bus(&sp->tx_ring[sp->dirty_tx % TX_RING_SIZE]),
ioaddr + SCBPointer);
outw(CUStart, ioaddr + SCBCmd);
} else {
printk(KERN_WARNING "%s: Restarting the chip...\n",
dev->name);
outl(PortReset, ioaddr + SCBPort);
if (sp->msg_level & NETIF_MSG_TX_ERR)
speedo_show_state(dev);
udelay(10);
speedo_resume(dev);
}
if ((sp->phy[0] & 0x8000) == 0) {
int phy_addr = sp->phy[0] & 0x1f;
int advertising = mdio_read(dev, phy_addr, 4);
int mii_bmcr = mdio_read(dev, phy_addr, 0);
mdio_write(ioaddr, phy_addr, 0, 0x0400);
mdio_write(ioaddr, phy_addr, 1, 0x0000);
mdio_write(ioaddr, phy_addr, 4, 0x0000);
mdio_write(ioaddr, phy_addr, 0, 0x8000);
#ifdef honor_default_port
mdio_write(ioaddr, phy_addr, 0, mii_ctrl[dev->default_port & 7]);
#else
mdio_read(dev, phy_addr, 0);
mdio_write(ioaddr, phy_addr, 0, mii_bmcr);
mdio_write(ioaddr, phy_addr, 4, advertising);
#endif
}
sp->stats.tx_errors++;
dev->trans_start = jiffies;
return;
}
static void speedo_intr_error(struct net_device *dev, int intr_status)
{
long ioaddr = dev->base_addr;
struct speedo_private *sp = (struct speedo_private *)dev->priv;
if (intr_status & IntrRxSuspend) {
if ((intr_status & 0x003c) == 0x0028)
outb(RxResumeNoResources, ioaddr + SCBCmd);
else if ((intr_status & 0x003c) == 0x0008) {
printk(KERN_DEBUG "%s: Unknown receiver error, status=%#4.4x.\n",
dev->name, intr_status);
outl(virt_to_bus(sp->rx_ringp[sp->cur_rx % RX_RING_SIZE]),
ioaddr + SCBPointer);
outb(RxStart, ioaddr + SCBCmd);
}
sp->stats.rx_errors++;
}
}
static int
speedo_start_xmit(struct sk_buff *skb, struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
int entry;
if (netif_pause_tx_queue(dev) != 0) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < TX_TIMEOUT - 2)
return 1;
if (tickssofar < TX_TIMEOUT) {
outw(SCBTriggerIntr, ioaddr + SCBCmd);
return 1;
}
speedo_tx_timeout(dev);
return 1;
}
{
unsigned long flags;
spin_lock_irqsave(&sp->lock, flags);
entry = sp->cur_tx % TX_RING_SIZE;
sp->tx_skbuff[entry] = skb;
sp->tx_ring[entry].status =
cpu_to_le32(CmdSuspend | CmdTx | CmdTxFlex);
sp->cur_tx++;
sp->tx_ring[entry].link =
virt_to_le32desc(&sp->tx_ring[sp->cur_tx % TX_RING_SIZE]);
sp->tx_ring[entry].tx_desc_addr =
virt_to_le32desc(&sp->tx_ring[entry].tx_buf_addr0);
sp->tx_ring[entry].count = cpu_to_le32(sp->tx_threshold);
sp->tx_ring[entry].tx_buf_addr0 = virt_to_le32desc(skb->data);
sp->tx_ring[entry].tx_buf_size0 = cpu_to_le32(skb->len);
{
struct descriptor *last_cmd = sp->last_cmd;
sp->last_cmd = (struct descriptor *)&sp->tx_ring[entry];
clear_suspend(last_cmd);
}
if (sp->cur_tx - sp->dirty_tx >= TX_QUEUE_LIMIT) {
sp->tx_full = 1;
netif_stop_tx_queue(dev);
} else
netif_unpause_tx_queue(dev);
spin_unlock_irqrestore(&sp->lock, flags);
}
wait_for_cmd_done(dev);
outb(CUResume, ioaddr + SCBCmd);
dev->trans_start = jiffies;
return 0;
}
static void speedo_interrupt(int irq, void *dev_instance, struct pt_regs *regs)
{
struct net_device *dev = (struct net_device *)dev_instance;
struct speedo_private *sp;
long ioaddr;
int work_limit;
u16 status;
ioaddr = dev->base_addr;
sp = (struct speedo_private *)dev->priv;
work_limit = sp->max_interrupt_work;
#ifndef final_version
if (test_and_set_bit(0, (void*)&sp->in_interrupt)) {
printk(KERN_ERR"%s: SMP simultaneous entry of an interrupt handler.\n",
dev->name);
sp->in_interrupt = 0;
return;
}
#endif
do {
status = inw(ioaddr + SCBStatus);
if ((status & IntrAllNormal) == 0 || status == 0xffff)
break;
outw(status & IntrAllNormal, ioaddr + SCBStatus);
if (sp->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: interrupt  status=%#4.4x.\n",
dev->name, status);
if (status & (IntrRxDone|IntrRxSuspend))
speedo_rx(dev);
if (status & (IntrCmdDone | IntrCmdIdle | IntrDrvrIntr)) {
unsigned int dirty_tx;
spin_lock(&sp->lock);
dirty_tx = sp->dirty_tx;
while (sp->cur_tx - dirty_tx > 0) {
int entry = dirty_tx % TX_RING_SIZE;
int status = le32_to_cpu(sp->tx_ring[entry].status);
if (sp->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG " scavenge candidate %d status %4.4x.\n",
entry, status);
if ((status & StatusComplete) == 0) {
if (sp->cur_tx - dirty_tx > 2 &&
(sp->tx_ring[(dirty_tx+1) % TX_RING_SIZE].status
& cpu_to_le32(StatusComplete))) {
printk(KERN_ERR "%s: Command unit failed to mark "
"command %8.8x as complete at %d.\n",
dev->name, status, dirty_tx);
} else
break;
}
if ((status & TxUnderrun) &&
(sp->tx_threshold < 0x01e08000)) {
sp->tx_threshold += 0x00040000;
if (sp->msg_level & NETIF_MSG_TX_ERR)
printk(KERN_DEBUG "%s: Tx threshold increased, "
"%#8.8x.\n", dev->name, sp->tx_threshold);
}
if (sp->tx_skbuff[entry]) {
sp->stats.tx_packets++;
#if LINUX_VERSION_CODE > 0x20127
sp->stats.tx_bytes += sp->tx_skbuff[entry]->len;
#endif
dev_free_skb_irq(sp->tx_skbuff[entry]);
sp->tx_skbuff[entry] = 0;
} else if ((status & 0x70000) == CmdNOp)
sp->mc_setup_busy = 0;
dirty_tx++;
}
#ifndef final_version
if (sp->cur_tx - dirty_tx > TX_RING_SIZE) {
printk(KERN_ERR "out-of-sync dirty pointer, %d vs. %d,"
" full=%d.\n",
dirty_tx, sp->cur_tx, sp->tx_full);
dirty_tx += TX_RING_SIZE;
}
#endif
sp->dirty_tx = dirty_tx;
if (sp->tx_full
&& sp->cur_tx - dirty_tx < TX_QUEUE_UNFULL) {
sp->tx_full = 0;
netif_resume_tx_queue(dev);
}
spin_unlock(&sp->lock);
}
if (status & IntrRxSuspend)
speedo_intr_error(dev, status);
if (--work_limit < 0) {
printk(KERN_ERR "%s: Too much work at interrupt, status=0x%4.4x.\n",
dev->name, status);
outl(0xfc00, ioaddr + SCBStatus);
break;
}
} while (1);
if (sp->msg_level & NETIF_MSG_INTR)
printk(KERN_DEBUG "%s: exiting interrupt, status=%#4.4x.\n",
dev->name, (int)inw(ioaddr + SCBStatus));
clear_bit(0, (void*)&sp->in_interrupt);
return;
}
static int
speedo_rx(struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
int entry = sp->cur_rx % RX_RING_SIZE;
int status;
int rx_work_limit = sp->dirty_rx + RX_RING_SIZE - sp->cur_rx;
if (sp->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG " In speedo_rx().\n");
while (sp->rx_ringp[entry] != NULL &&
(status = le32_to_cpu(sp->rx_ringp[entry]->status)) & RxComplete) {
int desc_count = le32_to_cpu(sp->rx_ringp[entry]->count);
int pkt_len = desc_count & 0x07ff;
if (--rx_work_limit < 0)
break;
if (sp->msg_level & NETIF_MSG_RX_STATUS)
printk(KERN_DEBUG "  speedo_rx() status %8.8x len %d.\n", status,
pkt_len);
if ((status & (RxErrTooBig|RxOK|0x0f90)) != RxOK) {
if (status & RxErrTooBig)
printk(KERN_ERR "%s: Ethernet frame overran the Rx buffer, "
"status %8.8x!\n", dev->name, status);
else if ( ! (status & RxOK)) {
sp->stats.rx_errors++;
printk(KERN_ERR "%s: Anomalous event in speedo_rx(), "
"status %8.8x.\n", dev->name, status);
}
} else {
struct sk_buff *skb;
if (sp->drv_flags & HasChksum)
pkt_len -= 2;
if (pkt_len < sp->rx_copybreak
&& (skb = dev_alloc_skb(pkt_len + 2)) != 0) {
skb->dev = dev;
skb_reserve(skb, 2);
eth_copy_and_sum(skb, sp->rx_skbuff[entry]->tail, pkt_len, 0);
skb_put(skb, pkt_len);
} else {
void *temp;
skb = sp->rx_skbuff[entry];
if (skb == NULL) {
printk(KERN_ERR "%s: Inconsistent Rx descriptor chain.\n",
dev->name);
break;
}
sp->rx_skbuff[entry] = NULL;
temp = skb_put(skb, pkt_len);
#if !defined(final_version) && !defined(__powerpc__)
if (bus_to_virt(sp->rx_ringp[entry]->rx_buf_addr) != temp)
printk(KERN_ERR "%s: Rx consistency error -- the skbuff "
"addresses do not match in speedo_rx: %p vs. %p "
"/ %p.\n", dev->name,
bus_to_virt(sp->rx_ringp[entry]->rx_buf_addr),
skb->head, temp);
#endif
sp->rx_ringp[entry] = NULL;
}
skb->protocol = eth_type_trans(skb, dev);
if (sp->drv_flags & HasChksum) {
#if 0
u16 csum = get_unaligned((u16*)(skb->head + pkt_len))
if (desc_count & 0x8000)
skb->ip_summed = CHECKSUM_UNNECESSARY;
#endif
}
netif_rx(skb);
sp->stats.rx_packets++;
#if LINUX_VERSION_CODE > 0x20127
sp->stats.rx_bytes += pkt_len;
#endif
}
entry = (++sp->cur_rx) % RX_RING_SIZE;
}
for (; sp->cur_rx - sp->dirty_rx > 0; sp->dirty_rx++) {
struct RxFD *rxf;
entry = sp->dirty_rx % RX_RING_SIZE;
if (sp->rx_skbuff[entry] == NULL) {
struct sk_buff *skb;
skb = dev_alloc_skb(sp->rx_buf_sz);
sp->rx_skbuff[entry] = skb;
if (skb == NULL) {
sp->rx_ringp[entry] = NULL;
sp->alloc_failures++;
break;
}
rxf = sp->rx_ringp[entry] = (struct RxFD *)skb->tail;
skb->dev = dev;
skb_reserve(skb, sizeof(struct RxFD));
rxf->rx_buf_addr = virt_to_le32desc(skb->tail);
} else {
rxf = sp->rx_ringp[entry];
}
rxf->status = cpu_to_le32(0xC0000001);
rxf->link = 0;
rxf->count = cpu_to_le32((sp->rx_buf_sz - sizeof(struct RxFD)) << 16);
sp->last_rxf->link = virt_to_le32desc(rxf);
sp->last_rxf->status &= cpu_to_le32(~0xC0000000);
sp->last_rxf = rxf;
}
sp->last_rx_time = jiffies;
return 0;
}
static int
speedo_close(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct speedo_private *sp = (struct speedo_private *)dev->priv;
int i;
netif_stop_tx_queue(dev);
if (sp->msg_level & NETIF_MSG_IFDOWN)
printk(KERN_DEBUG "%s: Shutting down ethercard, status was %4.4x.\n"
KERN_DEBUG "%s:   Cumlative allocation failures: %d.\n",
dev->name, (int)inw(ioaddr + SCBStatus),
dev->name, sp->alloc_failures);
del_timer(&sp->timer);
outl(PortPartialReset, ioaddr + SCBPort);
free_irq(dev->irq, dev);
for (i = 0; i < RX_RING_SIZE; i++) {
struct sk_buff *skb = sp->rx_skbuff[i];
sp->rx_skbuff[i] = 0;
if (skb) {
#if LINUX_VERSION_CODE < 0x20100
skb->free = 1;
#endif
dev_free_skb(skb);
}
}
for (i = 0; i < TX_RING_SIZE; i++) {
struct sk_buff *skb = sp->tx_skbuff[i];
sp->tx_skbuff[i] = 0;
if (skb)
dev_free_skb(skb);
}
if (sp->mc_setup_frm) {
kfree(sp->mc_setup_frm);
sp->mc_setup_frm_len = 0;
}
if (sp->msg_level & NETIF_MSG_IFDOWN)
speedo_show_state(dev);
acpi_set_pwr_state(sp->pci_dev, ACPI_D2);
MOD_DEC_USE_COUNT;
return 0;
}
static struct net_device_stats *speedo_get_stats(struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
if (sp->lstats.done_marker == le32_to_cpu(0xA007)) {
sp->stats.tx_aborted_errors += le32_to_cpu(sp->lstats.tx_coll16_errs);
sp->stats.tx_window_errors += le32_to_cpu(sp->lstats.tx_late_colls);
sp->stats.tx_fifo_errors += le32_to_cpu(sp->lstats.tx_underruns);
sp->stats.tx_fifo_errors += le32_to_cpu(sp->lstats.tx_lost_carrier);
sp->stats.collisions += le32_to_cpu(sp->lstats.tx_total_colls);
sp->stats.rx_crc_errors += le32_to_cpu(sp->lstats.rx_crc_errs);
sp->stats.rx_frame_errors += le32_to_cpu(sp->lstats.rx_align_errs);
sp->stats.rx_over_errors += le32_to_cpu(sp->lstats.rx_resource_errs);
sp->stats.rx_fifo_errors += le32_to_cpu(sp->lstats.rx_overrun_errs);
sp->stats.rx_length_errors += le32_to_cpu(sp->lstats.rx_runt_errs);
sp->lstats.done_marker = 0x0000;
if (netif_running(dev)) {
wait_for_cmd_done(dev);
outb(CUDumpStats, ioaddr + SCBCmd);
}
}
return &sp->stats;
}
static int speedo_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
u16 *data = (u16 *)&rq->ifr_data;
u32 *data32 = (void *)&rq->ifr_data;
int phy = sp->phy[0] & 0x1f;
int saved_acpi;
switch(cmd) {
case 0x8947: case 0x89F0:
data[0] = phy;
case 0x8948: case 0x89F1:
saved_acpi = acpi_set_pwr_state(sp->pci_dev, ACPI_D0);
data[3] = mdio_read(dev, data[0], data[1]);
acpi_set_pwr_state(sp->pci_dev, saved_acpi);
return 0;
case 0x8949: case 0x89F2:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
if (data[0] == sp->phy[0]) {
u16 value = data[2];
switch (data[1]) {
case 0:
sp->medialock = (value & 0x9000) ? 0 : 1;
if (sp->medialock) {
sp->full_duplex = (value & 0x0100) ? 1 : 0;
sp->rx_mode = RxInvalidMode;
}
break;
case 4: sp->advertising = value; break;
}
}
saved_acpi = acpi_set_pwr_state(sp->pci_dev, ACPI_D0);
mdio_write(ioaddr, data[0], data[1], data[2]);
acpi_set_pwr_state(sp->pci_dev, saved_acpi);
return 0;
case SIOCGPARAMS:
data32[0] = sp->msg_level;
data32[1] = sp->multicast_filter_limit;
data32[2] = sp->max_interrupt_work;
data32[3] = sp->rx_copybreak;
#if 0
data32[4] = txfifo;
data32[5] = rxfifo;
#endif
return 0;
case SIOCSPARAMS:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
sp->msg_level = data32[0];
sp->multicast_filter_limit = data32[1];
sp->max_interrupt_work = data32[2];
sp->rx_copybreak = data32[3];
#if 0
if (data32[4] < 16)
txfifo = data32[4];
if (data32[5] < 16)
rxfifo = data32[5];
#endif
return 0;
default:
return -EOPNOTSUPP;
}
}
static void set_rx_mode(struct net_device *dev)
{
struct speedo_private *sp = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
struct descriptor *last_cmd;
char new_rx_mode;
unsigned long flags;
int entry, i;
if (dev->flags & IFF_PROMISC) {
new_rx_mode = AcceptAllMulticast | AcceptAllPhys;
} else if ((dev->flags & IFF_ALLMULTI) ||
dev->mc_count > sp->multicast_filter_limit) {
new_rx_mode = AcceptAllMulticast;
} else
new_rx_mode = 0;
if (sp->cur_tx - sp->dirty_tx >= TX_RING_SIZE - 1) {
sp->rx_mode = RxInvalidMode;
return;
}
if (new_rx_mode != sp->rx_mode) {
u8 *config_cmd_data;
spin_lock_irqsave(&sp->lock, flags);
entry = sp->cur_tx % TX_RING_SIZE;
last_cmd = sp->last_cmd;
sp->last_cmd = (struct descriptor *)&sp->tx_ring[entry];
sp->tx_skbuff[entry] = 0;
sp->tx_ring[entry].status = cpu_to_le32(CmdSuspend | CmdConfigure);
sp->cur_tx++;
sp->tx_ring[entry].link =
virt_to_le32desc(&sp->tx_ring[(entry + 1) % TX_RING_SIZE]);
config_cmd_data = (void *)&sp->tx_ring[entry].tx_desc_addr;
memcpy(config_cmd_data, i82558_config_cmd, sizeof(i82558_config_cmd));
config_cmd_data[1] = (txfifo << 4) | rxfifo;
config_cmd_data[4] = rxdmacount;
config_cmd_data[5] = txdmacount + 0x80;
config_cmd_data[6] |= (new_rx_mode & AcceptErr) ? 0x80 : 0;
config_cmd_data[7] &= (new_rx_mode & AcceptRunt) ? ~0x01 : ~0;
if (sp->drv_flags & HasChksum)
config_cmd_data[9] |= 1;
config_cmd_data[15] |= (new_rx_mode & AcceptAllPhys) ? 1 : 0;
config_cmd_data[19] = sp->flow_ctrl ? 0xBD : 0x80;
config_cmd_data[19] |= sp->full_duplex ? 0x40 : 0;
config_cmd_data[21] = (new_rx_mode & AcceptAllMulticast) ? 0x0D : 0x05;
if (sp->phy[0] & 0x8000) {
config_cmd_data[15] |= 0x80;
config_cmd_data[8] = 0;
}
wait_for_cmd_done(dev);
clear_suspend(last_cmd);
outb(CUResume, ioaddr + SCBCmd);
spin_unlock_irqrestore(&sp->lock, flags);
sp->last_cmd_time = jiffies;
}
if (new_rx_mode == 0 && dev->mc_count < 4) {
struct dev_mc_list *mclist;
u16 *setup_params, *eaddrs;
spin_lock_irqsave(&sp->lock, flags);
entry = sp->cur_tx % TX_RING_SIZE;
last_cmd = sp->last_cmd;
sp->last_cmd = (struct descriptor *)&sp->tx_ring[entry];
sp->tx_skbuff[entry] = 0;
sp->tx_ring[entry].status = cpu_to_le32(CmdSuspend | CmdMulticastList);
sp->cur_tx++;
sp->tx_ring[entry].link =
virt_to_le32desc(&sp->tx_ring[(entry + 1) % TX_RING_SIZE]);
sp->tx_ring[entry].tx_desc_addr = 0;
setup_params = (u16 *)&sp->tx_ring[entry].tx_desc_addr;
*setup_params++ = cpu_to_le16(dev->mc_count*6);
for (i = 0, mclist = dev->mc_list; i < dev->mc_count;
i++, mclist = mclist->next) {
eaddrs = (u16 *)mclist->dmi_addr;
*setup_params++ = *eaddrs++;
*setup_params++ = *eaddrs++;
*setup_params++ = *eaddrs++;
}
wait_for_cmd_done(dev);
clear_suspend(last_cmd);
outb(CUResume, ioaddr + SCBCmd);
spin_unlock_irqrestore(&sp->lock, flags);
sp->last_cmd_time = jiffies;
} else if (new_rx_mode == 0) {
struct dev_mc_list *mclist;
u16 *setup_params, *eaddrs;
struct descriptor *mc_setup_frm = sp->mc_setup_frm;
int i;
if (sp->mc_setup_frm_len < 10 + dev->mc_count*6
|| sp->mc_setup_frm == NULL) {
if (sp->mc_setup_frm)
kfree(sp->mc_setup_frm);
sp->mc_setup_busy = 0;
sp->mc_setup_frm_len = 10 + sp->multicast_filter_limit*6;
sp->mc_setup_frm = kmalloc(sp->mc_setup_frm_len, GFP_ATOMIC);
if (sp->mc_setup_frm == NULL) {
printk(KERN_ERR "%s: Failed to allocate a setup frame.\n",
dev->name);
sp->rx_mode = RxInvalidMode;
return;
}
}
if (sp->mc_setup_busy) {
sp->rx_mode = RxInvalidMode;
return;
}
mc_setup_frm = sp->mc_setup_frm;
if (sp->msg_level & NETIF_MSG_RXFILTER)
printk(KERN_DEBUG "%s: Constructing a setup frame at %p, "
"%d bytes.\n",
dev->name, sp->mc_setup_frm, sp->mc_setup_frm_len);
mc_setup_frm->cmd_status =
cpu_to_le32(CmdSuspend | CmdIntr | CmdMulticastList);
setup_params = (u16 *)&mc_setup_frm->params;
*setup_params++ = cpu_to_le16(dev->mc_count*6);
for (i = 0, mclist = dev->mc_list; i < dev->mc_count;
i++, mclist = mclist->next) {
eaddrs = (u16 *)mclist->dmi_addr;
*setup_params++ = *eaddrs++;
*setup_params++ = *eaddrs++;
*setup_params++ = *eaddrs++;
}
spin_lock_irqsave(&sp->lock, flags);
entry = sp->cur_tx % TX_RING_SIZE;
last_cmd = sp->last_cmd;
sp->last_cmd = mc_setup_frm;
sp->mc_setup_busy++;
sp->tx_skbuff[entry] = 0;
sp->tx_ring[entry].status = cpu_to_le32(CmdNOp);
sp->cur_tx++;
sp->tx_ring[entry].link = virt_to_le32desc(mc_setup_frm);
mc_setup_frm->link =
virt_to_le32desc(&(sp->tx_ring[(entry+1) % TX_RING_SIZE]));
wait_for_cmd_done(dev);
clear_suspend(last_cmd);
outb(CUResume, ioaddr + SCBCmd);
spin_unlock_irqrestore(&sp->lock, flags);
sp->last_cmd_time = jiffies;
if (sp->msg_level & NETIF_MSG_RXFILTER)
printk(KERN_DEBUG " CmdMCSetup frame length %d in entry %d.\n",
dev->mc_count, entry);
}
sp->rx_mode = new_rx_mode;
}
static int speedo_pwr_event(void *dev_instance, int event)
{
struct net_device *dev = dev_instance;
struct speedo_private *np = (struct speedo_private *)dev->priv;
long ioaddr = dev->base_addr;
if (np->msg_level & NETIF_MSG_LINK)
printk(KERN_DEBUG "%s: Handling power event %d.\n", dev->name, event);
switch(event) {
case DRV_ATTACH:
MOD_INC_USE_COUNT;
break;
case DRV_SUSPEND:
outl(PortPartialReset, ioaddr + SCBPort);
break;
case DRV_RESUME:
speedo_resume(dev);
np->rx_mode = RxInvalidMode;
np->flow_ctrl = np->partner = 0;
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
#ifndef USE_IO_OPS
iounmap((char *)dev->base_addr);
#endif
for (devp = &root_speedo_dev; *devp; devp = next) {
next = &((struct speedo_private *)(*devp)->priv)->next_module;
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
case DRV_PWR_DOWN:
case DRV_PWR_UP:
acpi_set_pwr_state(np->pci_dev, event==DRV_PWR_DOWN ? ACPI_D3:ACPI_D0);
break;
case DRV_PWR_WakeOn:
default:
return -1;
}
return 0;
}
#if defined(MODULE) || (LINUX_VERSION_CODE >= 0x020400)
int init_module(void)
{
int cards_found;
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
cards_found = pci_drv_register(&eepro100_drv_id, NULL);
if (cards_found < 0)
printk(KERN_INFO "eepro100: No cards found, driver not installed.\n");
return cards_found;
}
void cleanup_module(void)
{
struct net_device *next_dev;
pci_drv_unregister(&eepro100_drv_id);
while (root_speedo_dev) {
struct speedo_private *sp = (void *)root_speedo_dev->priv;
unregister_netdev(root_speedo_dev);
#ifdef USE_IO_OPS
release_region(root_speedo_dev->base_addr,
pci_id_tbl[sp->chip_id].io_size);
#else
iounmap((char *)root_speedo_dev->base_addr);
#endif
acpi_set_pwr_state(sp->pci_dev, sp->acpi_pwr);
next_dev = sp->next_module;
if (sp->priv_addr)
kfree(sp->priv_addr);
kfree(root_speedo_dev);
root_speedo_dev = next_dev;
}
}
#if (LINUX_VERSION_CODE >= 0x020400) && 0
module_init(init_module);
module_exit(cleanup_module);
#endif
#else
int eepro100_probe(struct net_device *dev)
{
int cards_found = pci_drv_register(&eepro100_drv_id, dev);
if (cards_found >= 0)
printk(KERN_INFO "%s" KERN_INFO "%s", version1, version2);
return cards_found;
}
#endif