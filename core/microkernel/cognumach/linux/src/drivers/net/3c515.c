static char *version = "3c515.c:v0.99 4/7/98 becker@cesdis.gsfc.nasa.gov\n";
#define CORKSCREW 1
static const int rx_copybreak = 200;
static const int mtu = 1500;
static int max_interrupt_work = 20;
#define AUTOMEDIA 1
#define VORTEX_BUS_MASTER
#define TX_RING_SIZE	16
#define RX_RING_SIZE	16
#define PKT_BUF_SZ		1536
#ifdef MODULE
#ifdef MODVERSIONS
#include <linux/modversions.h>
#endif
#include <linux/module.h>
#include <linux/version.h>
#else
#define MOD_INC_USE_COUNT
#define MOD_DEC_USE_COUNT
#endif
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/ptrace.h>
#include <linux/errno.h>
#include <linux/in.h>
#include <linux/ioport.h>
#include <linux/malloc.h>
#include <linux/interrupt.h>
#include <linux/pci.h>
#include <linux/bios32.h>
#include <linux/timer.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#if (LINUX_VERSION_CODE >= 0x10344)
#define NEW_MULTICAST
#include <linux/delay.h>
#else
#define udelay(microsec)	do { int _i = 4*microsec; while (--_i > 0) { __SLOW_DOWN_IO; }} while (0)
#endif
#define RUN_AT(x) (jiffies + (x))
#define DEV_ALLOC_SKB(len) dev_alloc_skb(len + 2)
#define FREE_IRQ(irqnum, dev) free_irq(irqnum, dev)
#define REQUEST_IRQ(i,h,f,n, instance) request_irq(i,h,f,n, instance)
#define IRQ(irq, dev_id, pt_regs) (irq, dev_id, pt_regs)
#if (LINUX_VERSION_CODE < 0x20123)
#elif defined(MODULE)
MODULE_AUTHOR("Donald Becker <becker@cesdis.gsfc.nasa.gov>");
MODULE_DESCRIPTION("3Com 3c515 Corkscrew driver");
MODULE_PARM(debug, "i");
MODULE_PARM(options, "1-" __MODULE_STRING(8) "i");
MODULE_PARM(full_duplex, "1-" __MODULE_STRING(8) "i");
MODULE_PARM(rx_copybreak, "i");
MODULE_PARM(max_interrupt_work, "i");
#endif
#define DRIVER_DEBUG 1
static int rx_nocopy = 0, rx_copy = 0, queued_packet = 0;
#define WAIT_TX_AVAIL 200
#define TX_TIMEOUT  40
#define CORKSCREW_TOTAL_SIZE 0x20
#ifdef HAVE_DEVLIST
struct netdev_entry tc515_drv =
{"3c515", tc515_probe, CORKSCREW_TOTAL_SIZE, NULL};
#endif
#ifdef DRIVER_DEBUG
int vortex_debug = DRIVER_DEBUG;
#else
int vortex_debug = 1;
#endif
#define CORKSCREW_ID 10
#define EL3WINDOW(win_num) outw(SelectWindow + (win_num), ioaddr + EL3_CMD)
#define EL3_CMD 0x0e
#define EL3_STATUS 0x0e
enum vortex_cmd {
TotalReset = 0<<11, SelectWindow = 1<<11, StartCoax = 2<<11,
RxDisable = 3<<11, RxEnable = 4<<11, RxReset = 5<<11,
UpStall = 6<<11, UpUnstall = (6<<11)+1,
DownStall = (6<<11)+2, DownUnstall = (6<<11)+3,
RxDiscard = 8<<11, TxEnable = 9<<11, TxDisable = 10<<11, TxReset = 11<<11,
FakeIntr = 12<<11, AckIntr = 13<<11, SetIntrEnb = 14<<11,
SetStatusEnb = 15<<11, SetRxFilter = 16<<11, SetRxThreshold = 17<<11,
SetTxThreshold = 18<<11, SetTxStart = 19<<11,
StartDMAUp = 20<<11, StartDMADown = (20<<11)+1, StatsEnable = 21<<11,
StatsDisable = 22<<11, StopCoax = 23<<11,};
enum RxFilter {
RxStation = 1, RxMulticast = 2, RxBroadcast = 4, RxProm = 8 };
enum vortex_status {
IntLatch = 0x0001, AdapterFailure = 0x0002, TxComplete = 0x0004,
TxAvailable = 0x0008, RxComplete = 0x0010, RxEarly = 0x0020,
IntReq = 0x0040, StatsFull = 0x0080,
DMADone = 1<<8, DownComplete = 1<<9, UpComplete = 1<<10,
DMAInProgress = 1<<11,
CmdInProgress = 1<<12,
};
enum Window1 {
TX_FIFO = 0x10,  RX_FIFO = 0x10,  RxErrors = 0x14,
RxStatus = 0x18,  Timer=0x1A, TxStatus = 0x1B,
TxFree = 0x1C,
};
enum Window0 {
Wn0IRQ = 0x08,
#if defined(CORKSCREW)
Wn0EepromCmd = 0x200A,
Wn0EepromData = 0x200C,
#else
Wn0EepromCmd = 10,
Wn0EepromData = 12,
#endif
};
enum Win0_EEPROM_bits {
EEPROM_Read = 0x80, EEPROM_WRITE = 0x40, EEPROM_ERASE = 0xC0,
EEPROM_EWENB = 0x30,
EEPROM_EWDIS = 0x00,
};
enum eeprom_offset {
PhysAddr01=0, PhysAddr23=1, PhysAddr45=2, ModelID=3,
EtherLink3ID=7, };
enum Window3 {
Wn3_Config=0, Wn3_MAC_Ctrl=6, Wn3_Options=8,
};
union wn3_config {
int i;
struct w3_config_fields {
unsigned int ram_size:3, ram_width:1, ram_speed:2, rom_size:2;
int pad8:8;
unsigned int ram_split:2, pad18:2, xcvr:3, pad21:1, autoselect:1;
int pad24:7;
} u;
};
enum Window4 {
Wn4_NetDiag = 6, Wn4_Media = 10,
};
enum Win4_Media_bits {
Media_SQE = 0x0008,
Media_10TP = 0x00C0,
Media_Lnk = 0x0080,
Media_LnkBeat = 0x0800,
};
enum Window7 {
Wn7_MasterAddr = 0, Wn7_MasterLen = 6, Wn7_MasterStatus = 12,
};
enum MasterCtrl {
PktStatus = 0x400, DownListPtr = 0x404, FragAddr = 0x408, FragLen = 0x40c,
TxFreeThreshold = 0x40f, UpPktStatus = 0x410, UpListPtr = 0x418,
};
struct boom_rx_desc {
u32 next;
s32 status;
u32 addr;
s32 length;
};
enum rx_desc_status {
RxDComplete=0x00008000, RxDError=0x4000,
};
struct boom_tx_desc {
u32 next;
s32 status;
u32 addr;
s32 length;
};
struct vortex_private {
char devname[8];
const char *product_name;
struct device *next_module;
struct boom_rx_desc rx_ring[RX_RING_SIZE];
struct boom_tx_desc tx_ring[TX_RING_SIZE];
struct sk_buff* rx_skbuff[RX_RING_SIZE];
struct sk_buff* tx_skbuff[TX_RING_SIZE];
unsigned int cur_rx, cur_tx;
unsigned int dirty_rx, dirty_tx;
struct enet_statistics stats;
struct sk_buff *tx_skb;
struct timer_list timer;
int capabilities;
int options;
int last_rx_packets;
unsigned int available_media:8,
media_override:3,
default_media:3,
full_duplex:1, autoselect:1,
bus_master:1,
full_bus_master_tx:1, full_bus_master_rx:1,
tx_full:1;
};
enum xcvr_types {
XCVR_10baseT=0, XCVR_AUI, XCVR_10baseTOnly, XCVR_10base2, XCVR_100baseTx,
XCVR_100baseFx, XCVR_MII=6, XCVR_Default=8,
};
static struct media_table {
char *name;
unsigned int media_bits:16,
mask:8,
next:8;
short wait;
} media_tbl[] = {
{	"10baseT",   Media_10TP,0x08, XCVR_10base2, (14*HZ)/10},
{ "10Mbs AUI", Media_SQE, 0x20, XCVR_Default, (1*HZ)/10},
{ "undefined", 0,			0x80, XCVR_10baseT, 10000},
{ "10base2",   0,			0x10, XCVR_AUI,		(1*HZ)/10},
{ "100baseTX", Media_Lnk, 0x02, XCVR_100baseFx, (14*HZ)/10},
{ "100baseFX", Media_Lnk, 0x04, XCVR_MII,		(14*HZ)/10},
{ "MII",		 0,			0x40, XCVR_10baseT, 3*HZ },
{ "undefined", 0,			0x01, XCVR_10baseT, 10000},
{ "Default",	 0,			0xFF, XCVR_10baseT, 10000},
};
static int vortex_scan(struct device *dev);
static struct device *vortex_found_device(struct device *dev, int ioaddr,
int irq, int product_index,
int options);
static int vortex_probe1(struct device *dev);
static int vortex_open(struct device *dev);
static void vortex_timer(unsigned long arg);
static int vortex_start_xmit(struct sk_buff *skb, struct device *dev);
static int vortex_rx(struct device *dev);
static int boomerang_rx(struct device *dev);
static void vortex_interrupt IRQ(int irq, void *dev_id, struct pt_regs *regs);
static int vortex_close(struct device *dev);
static void update_stats(int addr, struct device *dev);
static struct enet_statistics *vortex_get_stats(struct device *dev);
static void set_rx_mode(struct device *dev);
static int options[8] = { -1, -1, -1, -1, -1, -1, -1, -1,};
#ifdef MODULE
static int debug = -1;
static struct device *root_vortex_dev = NULL;
int
init_module(void)
{
int cards_found;
if (debug >= 0)
vortex_debug = debug;
if (vortex_debug)
printk("%s", version);
root_vortex_dev = NULL;
cards_found = vortex_scan(0);
return cards_found ? 0 : -ENODEV;
}
#else
int tc515_probe(struct device *dev)
{
int cards_found = 0;
cards_found = vortex_scan(dev);
if (vortex_debug > 0  &&  cards_found)
printk("%s", version);
return cards_found ? 0 : -ENODEV;
}
#endif
static int vortex_scan(struct device *dev)
{
int cards_found = 0;
static int ioaddr = 0x100;
for (; ioaddr < 0x400; ioaddr += 0x20) {
int irq;
if (check_region(ioaddr, CORKSCREW_TOTAL_SIZE))
continue;
if ((inw(ioaddr + 0x2002) & 0x1f0) != (ioaddr & 0x1f0))
continue;
{
int timer;
outw(EEPROM_Read + 7, ioaddr + Wn0EepromCmd);
for (timer = 4; timer >= 0; timer--) {
udelay(162);
if ((inw(ioaddr + Wn0EepromCmd) & 0x0200) == 0)
break;
}
if (inw(ioaddr + Wn0EepromData) != 0x6d50)
continue;
}
printk("3c515 Resource configuraiton register %#4.4x, DCR %4.4x.\n",
inl(ioaddr + 0x2002), inw(ioaddr + 0x2000));
irq = inw(ioaddr + 0x2002) & 15;
vortex_found_device(dev, ioaddr, irq, CORKSCREW_ID, dev && dev->mem_start
? dev->mem_start : options[cards_found]);
dev = 0;
cards_found++;
}
if (vortex_debug)
printk("%d 3c515 cards found.\n", cards_found);
return cards_found;
}
static struct device *vortex_found_device(struct device *dev, int ioaddr,
int irq, int product_index,
int options)
{
struct vortex_private *vp;
#ifdef MODULE
int dev_size = sizeof(struct device) +
sizeof(struct vortex_private) + 15;
dev = (struct device *) kmalloc(dev_size, GFP_KERNEL);
memset(dev, 0, dev_size);
dev->priv = (void *)(((long)dev + sizeof(struct device) + 15) & ~15);
vp = (struct vortex_private *)dev->priv;
dev->name = vp->devname;
dev->base_addr = ioaddr;
dev->irq = irq;
dev->dma = (product_index == CORKSCREW_ID ? inw(ioaddr + 0x2000) & 7 : 0);
dev->init = vortex_probe1;
vp->product_name = "3c515";
vp->options = options;
if (options >= 0) {
vp->media_override = ((options & 7) == 2)  ?  0  :  options & 7;
vp->full_duplex = (options & 8) ? 1 : 0;
vp->bus_master = (options & 16) ? 1 : 0;
} else {
vp->media_override = 7;
vp->full_duplex = 0;
vp->bus_master = 0;
}
ether_setup(dev);
vp->next_module = root_vortex_dev;
root_vortex_dev = dev;
if (register_netdev(dev) != 0)
return 0;
#else
if (dev) {
dev->priv = kmalloc(sizeof (struct vortex_private), GFP_KERNEL);
memset(dev->priv, 0, sizeof (struct vortex_private));
}
dev = init_etherdev(dev, sizeof(struct vortex_private));
dev->base_addr = ioaddr;
dev->irq = irq;
dev->dma = (product_index == CORKSCREW_ID ? inw(ioaddr + 0x2000) & 7 : 0);
vp  = (struct vortex_private *)dev->priv;
vp->product_name = "3c515";
vp->options = options;
if (options >= 0) {
vp->media_override = ((options & 7) == 2)  ?  0  :  options & 7;
vp->full_duplex = (options & 8) ? 1 : 0;
vp->bus_master = (options & 16) ? 1 : 0;
} else {
vp->media_override = 7;
vp->full_duplex = 0;
vp->bus_master = 0;
}
vortex_probe1(dev);
#endif
return dev;
}
static int vortex_probe1(struct device *dev)
{
int ioaddr = dev->base_addr;
struct vortex_private *vp = (struct vortex_private *)dev->priv;
unsigned int eeprom[0x40], checksum = 0;
int i;
printk("%s: 3Com %s at %#3x,", dev->name,
vp->product_name, ioaddr);
EL3WINDOW(0);
for (i = 0; i < 0x18; i++) {
short *phys_addr = (short *)dev->dev_addr;
int timer;
outw(EEPROM_Read + i, ioaddr + Wn0EepromCmd);
for (timer = 4; timer >= 0; timer--) {
udelay(162);
if ((inw(ioaddr + Wn0EepromCmd) & 0x0200) == 0)
break;
}
eeprom[i] = inw(ioaddr + Wn0EepromData);
checksum ^= eeprom[i];
if (i < 3)
phys_addr[i] = htons(eeprom[i]);
}
checksum = (checksum ^ (checksum >> 8)) & 0xff;
if (checksum != 0x00)
printk(" ***INVALID CHECKSUM %4.4x*** ", checksum);
for (i = 0; i < 6; i++)
printk("%c%2.2x", i ? ':' : ' ', dev->dev_addr[i]);
if (eeprom[16] == 0x11c7) {
if (request_dma(dev->dma, "3c515")) {
printk(", DMA %d allocation failed", dev->dma);
dev->dma = 0;
} else
printk(", DMA %d", dev->dma);
}
printk(", IRQ %d\n", dev->irq);
if (vortex_debug && (dev->irq <= 0 || dev->irq > 15))
printk(" *** Warning: this IRQ is unlikely to work! ***\n");
{
char *ram_split[] = {"5:3", "3:1", "1:1", "3:5"};
union wn3_config config;
EL3WINDOW(3);
vp->available_media = inw(ioaddr + Wn3_Options);
config.i = inl(ioaddr + Wn3_Config);
if (vortex_debug > 1)
printk("  Internal config register is %4.4x, transceivers %#x.\n",
config.i, inw(ioaddr + Wn3_Options));
printk("  %dK %s-wide RAM %s Rx:Tx split, %s%s interface.\n",
8 << config.u.ram_size,
config.u.ram_width ? "word" : "byte",
ram_split[config.u.ram_split],
config.u.autoselect ? "autoselect/" : "",
media_tbl[config.u.xcvr].name);
dev->if_port = config.u.xcvr;
vp->default_media = config.u.xcvr;
vp->autoselect = config.u.autoselect;
}
if (vp->media_override != 7) {
printk("  Media override to transceiver type %d (%s).\n",
vp->media_override, media_tbl[vp->media_override].name);
dev->if_port = vp->media_override;
}
vp->capabilities = eeprom[16];
vp->full_bus_master_tx = (vp->capabilities & 0x20) ? 1 : 0;
vp->full_bus_master_rx = (vp->capabilities & 0x20) ? 1 : 0;
request_region(ioaddr, CORKSCREW_TOTAL_SIZE, vp->product_name);
dev->open = &vortex_open;
dev->hard_start_xmit = &vortex_start_xmit;
dev->stop = &vortex_close;
dev->get_stats = &vortex_get_stats;
dev->set_multicast_list = &set_rx_mode;
return 0;
}
static int
vortex_open(struct device *dev)
{
int ioaddr = dev->base_addr;
struct vortex_private *vp = (struct vortex_private *)dev->priv;
union wn3_config config;
int i;
EL3WINDOW(3);
if (vp->full_duplex)
outb(0x20, ioaddr + Wn3_MAC_Ctrl);
config.i = inl(ioaddr + Wn3_Config);
if (vp->media_override != 7) {
if (vortex_debug > 1)
printk("%s: Media override to transceiver %d (%s).\n",
dev->name, vp->media_override,
media_tbl[vp->media_override].name);
dev->if_port = vp->media_override;
} else if (vp->autoselect) {
dev->if_port = 4;
while (! (vp->available_media & media_tbl[dev->if_port].mask))
dev->if_port = media_tbl[dev->if_port].next;
if (vortex_debug > 1)
printk("%s: Initial media type %s.\n",
dev->name, media_tbl[dev->if_port].name);
init_timer(&vp->timer);
vp->timer.expires = RUN_AT(media_tbl[dev->if_port].wait);
vp->timer.data = (unsigned long)dev;
vp->timer.function = &vortex_timer;
add_timer(&vp->timer);
} else
dev->if_port = vp->default_media;
config.u.xcvr = dev->if_port;
outl(config.i, ioaddr + Wn3_Config);
if (vortex_debug > 1) {
printk("%s: vortex_open() InternalConfig %8.8x.\n",
dev->name, config.i);
}
outw(TxReset, ioaddr + EL3_CMD);
for (i = 20; i >= 0 ; i--)
if ( ! (inw(ioaddr + EL3_STATUS) & CmdInProgress))
break;
outw(RxReset, ioaddr + EL3_CMD);
for (i = 20; i >= 0 ; i--)
if ( ! (inw(ioaddr + EL3_STATUS) & CmdInProgress))
break;
outw(SetStatusEnb | 0x00, ioaddr + EL3_CMD);
if (vp->capabilities == 0x11c7) {
if (dev->irq == 0
|| dev->dma == 0
|| request_irq(dev->irq, &vortex_interrupt, 0,
vp->product_name, dev))
return -EAGAIN;
enable_dma(dev->dma);
set_dma_mode(dev->dma, DMA_MODE_CASCADE);
} else if (request_irq(dev->irq, &vortex_interrupt, SA_SHIRQ,
vp->product_name, dev)) {
return -EAGAIN;
}
if (vortex_debug > 1) {
EL3WINDOW(4);
printk("%s: vortex_open() irq %d media status %4.4x.\n",
dev->name, dev->irq, inw(ioaddr + Wn4_Media));
}
EL3WINDOW(2);
for (i = 0; i < 6; i++)
outb(dev->dev_addr[i], ioaddr + i);
for (; i < 12; i+=2)
outw(0, ioaddr + i);
if (dev->if_port == 3)
outw(StartCoax, ioaddr + EL3_CMD);
EL3WINDOW(4);
outw((inw(ioaddr + Wn4_Media) & ~(Media_10TP|Media_SQE)) |
media_tbl[dev->if_port].media_bits, ioaddr + Wn4_Media);
outw(StatsDisable, ioaddr + EL3_CMD);
EL3WINDOW(6);
for (i = 0; i < 10; i++)
inb(ioaddr + i);
inw(ioaddr + 10);
inw(ioaddr + 12);
EL3WINDOW(4);
inb(ioaddr + 12);
outw(0x0040, ioaddr + Wn4_NetDiag);
EL3WINDOW(7);
if (vp->full_bus_master_rx) {
vp->cur_rx = vp->dirty_rx = 0;
if (vortex_debug > 2)
printk("%s:  Filling in the Rx ring.\n", dev->name);
for (i = 0; i < RX_RING_SIZE; i++) {
struct sk_buff *skb;
if (i < (RX_RING_SIZE - 1))
vp->rx_ring[i].next = virt_to_bus(&vp->rx_ring[i+1]);
else
vp->rx_ring[i].next = 0;
vp->rx_ring[i].status = 0;
vp->rx_ring[i].length = PKT_BUF_SZ | 0x80000000;
skb = dev_alloc_skb(PKT_BUF_SZ);
vp->rx_skbuff[i] = skb;
if (skb == NULL)
break;
skb->dev = dev;
skb_reserve(skb, 2);
vp->rx_ring[i].addr = virt_to_bus(skb->tail);
}
vp->rx_ring[i-1].next = virt_to_bus(&vp->rx_ring[0]);
outl(virt_to_bus(&vp->rx_ring[0]), ioaddr + UpListPtr);
}
if (vp->full_bus_master_tx) {
vp->cur_tx = vp->dirty_tx = 0;
outb(PKT_BUF_SZ>>8, ioaddr + TxFreeThreshold);
for (i = 0; i < TX_RING_SIZE; i++)
vp->tx_skbuff[i] = 0;
outl(0, ioaddr + DownListPtr);
}
set_rx_mode(dev);
outw(StatsEnable, ioaddr + EL3_CMD);
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
outw(RxEnable, ioaddr + EL3_CMD);
outw(TxEnable, ioaddr + EL3_CMD);
outw(SetStatusEnb | AdapterFailure|IntReq|StatsFull |
(vp->full_bus_master_tx ? DownComplete : TxAvailable) |
(vp->full_bus_master_rx ? UpComplete : RxComplete) |
(vp->bus_master ? DMADone : 0),
ioaddr + EL3_CMD);
outw(AckIntr | IntLatch | TxAvailable | RxEarly | IntReq,
ioaddr + EL3_CMD);
outw(SetIntrEnb | IntLatch | TxAvailable | RxComplete | StatsFull
| (vp->bus_master ? DMADone : 0) | UpComplete | DownComplete,
ioaddr + EL3_CMD);
MOD_INC_USE_COUNT;
return 0;
}
static void vortex_timer(unsigned long data)
{
#ifdef AUTOMEDIA
struct device *dev = (struct device *)data;
struct vortex_private *vp = (struct vortex_private *)dev->priv;
int ioaddr = dev->base_addr;
unsigned long flags;
int ok = 0;
if (vortex_debug > 1)
printk("%s: Media selection timer tick happened, %s.\n",
dev->name, media_tbl[dev->if_port].name);
save_flags(flags);	cli(); {
int old_window = inw(ioaddr + EL3_CMD) >> 13;
int media_status;
EL3WINDOW(4);
media_status = inw(ioaddr + Wn4_Media);
switch (dev->if_port) {
case 0:  case 4:  case 5:
if (media_status & Media_LnkBeat) {
ok = 1;
if (vortex_debug > 1)
printk("%s: Media %s has link beat, %x.\n",
dev->name, media_tbl[dev->if_port].name, media_status);
} else if (vortex_debug > 1)
printk("%s: Media %s is has no link beat, %x.\n",
dev->name, media_tbl[dev->if_port].name, media_status);
break;
default:
if (vortex_debug > 1)
printk("%s: Media %s is has no indication, %x.\n",
dev->name, media_tbl[dev->if_port].name, media_status);
ok = 1;
}
if ( ! ok) {
union wn3_config config;
do {
dev->if_port = media_tbl[dev->if_port].next;
} while ( ! (vp->available_media & media_tbl[dev->if_port].mask));
if (dev->if_port == 8) {
dev->if_port = vp->default_media;
if (vortex_debug > 1)
printk("%s: Media selection failing, using default %s port.\n",
dev->name, media_tbl[dev->if_port].name);
} else {
if (vortex_debug > 1)
printk("%s: Media selection failed, now trying %s port.\n",
dev->name, media_tbl[dev->if_port].name);
vp->timer.expires = RUN_AT(media_tbl[dev->if_port].wait);
add_timer(&vp->timer);
}
outw((media_status & ~(Media_10TP|Media_SQE)) |
media_tbl[dev->if_port].media_bits, ioaddr + Wn4_Media);
EL3WINDOW(3);
config.i = inl(ioaddr + Wn3_Config);
config.u.xcvr = dev->if_port;
outl(config.i, ioaddr + Wn3_Config);
outw(dev->if_port == 3 ? StartCoax : StopCoax, ioaddr + EL3_CMD);
}
EL3WINDOW(old_window);
}   restore_flags(flags);
if (vortex_debug > 1)
printk("%s: Media selection timer finished, %s.\n",
dev->name, media_tbl[dev->if_port].name);
#endif
return;
}
static int
vortex_start_xmit(struct sk_buff *skb, struct device *dev)
{
struct vortex_private *vp = (struct vortex_private *)dev->priv;
int ioaddr = dev->base_addr;
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
int i;
if (tickssofar < 400*HZ/1000)
return 1;
printk("%s: transmit timed out, tx_status %2.2x status %4.4x.\n",
dev->name, inb(ioaddr + TxStatus),
inw(ioaddr + EL3_STATUS));
if ((inb(ioaddr + TxStatus) & 0x88) == 0x88)
printk("%s: Transmitter encountered 16 collisions -- network"
" network cable problem?\n", dev->name);
#ifndef final_version
printk("  Flags; bus-master %d, full %d; dirty %d current %d.\n",
vp->full_bus_master_tx, vp->tx_full, vp->dirty_tx, vp->cur_tx);
printk("  Down list %8.8x vs. %p.\n", inl(ioaddr + DownListPtr),
&vp->tx_ring[0]);
for (i = 0; i < TX_RING_SIZE; i++) {
printk("  %d: %p  length %8.8x status %8.8x\n", i,
&vp->tx_ring[i],
vp->tx_ring[i].length,
vp->tx_ring[i].status);
}
#endif
outw(TxReset, ioaddr + EL3_CMD);
for (i = 20; i >= 0 ; i--)
if ( ! (inw(ioaddr + EL3_STATUS) & CmdInProgress))
break;
outw(TxEnable, ioaddr + EL3_CMD);
dev->trans_start = jiffies;
vp->stats.tx_errors++;
vp->stats.tx_dropped++;
return 0;
}
if (test_and_set_bit(0, (void*)&dev->tbusy) != 0) {
printk("%s: Transmitter access conflict.\n", dev->name);
return 1;
}
if (vp->full_bus_master_tx) {
int entry = vp->cur_tx % TX_RING_SIZE;
struct boom_tx_desc *prev_entry;
unsigned long flags, i;
if (vp->tx_full)
return 1;
if (vp->cur_tx != 0)
prev_entry = &vp->tx_ring[(vp->cur_tx-1) % TX_RING_SIZE];
else
prev_entry = NULL;
if (vortex_debug > 3)
printk("%s: Trying to send a packet, Tx index %d.\n",
dev->name, vp->cur_tx);
vp->tx_skbuff[entry] = skb;
vp->tx_ring[entry].next = 0;
vp->tx_ring[entry].addr = virt_to_bus(skb->data);
vp->tx_ring[entry].length = skb->len | 0x80000000;
vp->tx_ring[entry].status = skb->len | 0x80000000;
save_flags(flags);
cli();
outw(DownStall, ioaddr + EL3_CMD);
for (i = 20; i >= 0 ; i--)
if ( (inw(ioaddr + EL3_STATUS) & CmdInProgress) == 0)
break;
if (prev_entry)
prev_entry->next = virt_to_bus(&vp->tx_ring[entry]);
if (inl(ioaddr + DownListPtr) == 0) {
outl(virt_to_bus(&vp->tx_ring[entry]), ioaddr + DownListPtr);
queued_packet++;
}
outw(DownUnstall, ioaddr + EL3_CMD);
restore_flags(flags);
vp->cur_tx++;
if (vp->cur_tx - vp->dirty_tx > TX_RING_SIZE - 1)
vp->tx_full = 1;
else {
if (prev_entry)
prev_entry->status &= ~0x80000000;
dev->tbusy = 0;
}
dev->trans_start = jiffies;
return 0;
}
outl(skb->len, ioaddr + TX_FIFO);
#ifdef VORTEX_BUS_MASTER
if (vp->bus_master) {
outl((int)(skb->data), ioaddr + Wn7_MasterAddr);
outw((skb->len + 3) & ~3, ioaddr + Wn7_MasterLen);
vp->tx_skb = skb;
outw(StartDMADown, ioaddr + EL3_CMD);
} else {
outsl(ioaddr + TX_FIFO, skb->data, (skb->len + 3) >> 2);
dev_kfree_skb (skb, FREE_WRITE);
if (inw(ioaddr + TxFree) > 1536) {
dev->tbusy = 0;
} else
outw(SetTxThreshold + (1536>>2), ioaddr + EL3_CMD);
}
#else
outsl(ioaddr + TX_FIFO, skb->data, (skb->len + 3) >> 2);
dev_kfree_skb (skb, FREE_WRITE);
if (inw(ioaddr + TxFree) > 1536) {
dev->tbusy = 0;
} else
outw(SetTxThreshold + (1536>>2), ioaddr + EL3_CMD);
#endif
dev->trans_start = jiffies;
{
short tx_status;
int i = 4;
while (--i > 0	&&	(tx_status = inb(ioaddr + TxStatus)) > 0) {
if (tx_status & 0x3C) {
if (vortex_debug > 2)
printk("%s: Tx error, status %2.2x.\n",
dev->name, tx_status);
if (tx_status & 0x04) vp->stats.tx_fifo_errors++;
if (tx_status & 0x38) vp->stats.tx_aborted_errors++;
if (tx_status & 0x30) {
int j;
outw(TxReset, ioaddr + EL3_CMD);
for (j = 20; j >= 0 ; j--)
if ( ! (inw(ioaddr + EL3_STATUS) & CmdInProgress))
break;
}
outw(TxEnable, ioaddr + EL3_CMD);
}
outb(0x00, ioaddr + TxStatus);
}
}
return 0;
}
static void vortex_interrupt IRQ(int irq, void *dev_id, struct pt_regs *regs)
{
struct device *dev = dev_id;
struct vortex_private *lp;
int ioaddr, status;
int latency;
int i = max_interrupt_work;
if (test_and_set_bit(0, (void*)&dev->interrupt)) {
printk("%s: Re-entering the interrupt handler.\n", dev->name);
return;
}
ioaddr = dev->base_addr;
latency = inb(ioaddr + Timer);
lp = (struct vortex_private *)dev->priv;
status = inw(ioaddr + EL3_STATUS);
if (vortex_debug > 4)
printk("%s: interrupt, status %4.4x, timer %d.\n", dev->name,
status, latency);
if ((status & 0xE000) != 0xE000) {
static int donedidthis=0;
if (donedidthis++ > 100) {
printk("%s: Bogus interrupt, bailing. Status %4.4x, start=%d.\n",
dev->name, status, dev->start);
FREE_IRQ(dev->irq, dev);
}
}
do {
if (vortex_debug > 5)
printk("%s: In interrupt loop, status %4.4x.\n",
dev->name, status);
if (status & RxComplete)
vortex_rx(dev);
if (status & TxAvailable) {
if (vortex_debug > 5)
printk("	TX room bit was handled.\n");
outw(AckIntr | TxAvailable, ioaddr + EL3_CMD);
dev->tbusy = 0;
mark_bh(NET_BH);
}
if (status & DownComplete) {
unsigned int dirty_tx = lp->dirty_tx;
while (lp->cur_tx - dirty_tx > 0) {
int entry = dirty_tx % TX_RING_SIZE;
if (inl(ioaddr + DownListPtr) ==
virt_to_bus(&lp->tx_ring[entry]))
break;
if (lp->tx_skbuff[entry]) {
dev_kfree_skb(lp->tx_skbuff[entry], FREE_WRITE);
lp->tx_skbuff[entry] = 0;
}
dirty_tx++;
}
lp->dirty_tx = dirty_tx;
outw(AckIntr | DownComplete, ioaddr + EL3_CMD);
if (lp->tx_full && (lp->cur_tx - dirty_tx <= TX_RING_SIZE - 1)) {
lp->tx_full= 0;
dev->tbusy = 0;
mark_bh(NET_BH);
}
}
#ifdef VORTEX_BUS_MASTER
if (status & DMADone) {
outw(0x1000, ioaddr + Wn7_MasterStatus);
dev->tbusy = 0;
dev_kfree_skb (lp->tx_skb, FREE_WRITE);
mark_bh(NET_BH);
}
#endif
if (status & UpComplete) {
boomerang_rx(dev);
outw(AckIntr | UpComplete, ioaddr + EL3_CMD);
}
if (status & (AdapterFailure | RxEarly | StatsFull)) {
if (status & RxEarly) {
vortex_rx(dev);
outw(AckIntr | RxEarly, ioaddr + EL3_CMD);
}
if (status & StatsFull) {
static int DoneDidThat = 0;
if (vortex_debug > 4)
printk("%s: Updating stats.\n", dev->name);
update_stats(ioaddr, dev);
if (DoneDidThat == 0  &&
inw(ioaddr + EL3_STATUS) & StatsFull) {
int win, reg;
printk("%s: Updating stats failed, disabling stats as an"
" interrupt source.\n", dev->name);
for (win = 0; win < 8; win++) {
EL3WINDOW(win);
printk("\n Vortex window %d:", win);
for (reg = 0; reg < 16; reg++)
printk(" %2.2x", inb(ioaddr+reg));
}
EL3WINDOW(7);
outw(SetIntrEnb | TxAvailable | RxComplete | AdapterFailure
| UpComplete | DownComplete | TxComplete,
ioaddr + EL3_CMD);
DoneDidThat++;
}
}
if (status & AdapterFailure) {
outw(RxReset, ioaddr + EL3_CMD);
set_rx_mode(dev);
outw(RxEnable, ioaddr + EL3_CMD);
outw(AckIntr | AdapterFailure, ioaddr + EL3_CMD);
}
}
if (--i < 0) {
printk("%s: Too much work in interrupt, status %4.4x.  "
"Disabling functions (%4.4x).\n",
dev->name, status, SetStatusEnb | ((~status) & 0x7FE));
outw(SetStatusEnb | ((~status) & 0x7FE), ioaddr + EL3_CMD);
outw(AckIntr | 0x7FF, ioaddr + EL3_CMD);
break;
}
outw(AckIntr | IntReq | IntLatch, ioaddr + EL3_CMD);
} while ((status = inw(ioaddr + EL3_STATUS)) & (IntLatch | RxComplete));
if (vortex_debug > 4)
printk("%s: exiting interrupt, status %4.4x.\n", dev->name, status);
dev->interrupt = 0;
return;
}
static int
vortex_rx(struct device *dev)
{
struct vortex_private *vp = (struct vortex_private *)dev->priv;
int ioaddr = dev->base_addr;
int i;
short rx_status;
if (vortex_debug > 5)
printk("   In rx_packet(), status %4.4x, rx_status %4.4x.\n",
inw(ioaddr+EL3_STATUS), inw(ioaddr+RxStatus));
while ((rx_status = inw(ioaddr + RxStatus)) > 0) {
if (rx_status & 0x4000) {
unsigned char rx_error = inb(ioaddr + RxErrors);
if (vortex_debug > 2)
printk(" Rx error: status %2.2x.\n", rx_error);
vp->stats.rx_errors++;
if (rx_error & 0x01)  vp->stats.rx_over_errors++;
if (rx_error & 0x02)  vp->stats.rx_length_errors++;
if (rx_error & 0x04)  vp->stats.rx_frame_errors++;
if (rx_error & 0x08)  vp->stats.rx_crc_errors++;
if (rx_error & 0x10)  vp->stats.rx_length_errors++;
} else {
short pkt_len = rx_status & 0x1fff;
struct sk_buff *skb;
skb = DEV_ALLOC_SKB(pkt_len + 5);
if (vortex_debug > 4)
printk("Receiving packet size %d status %4.4x.\n",
pkt_len, rx_status);
if (skb != NULL) {
skb->dev = dev;
#if LINUX_VERSION_CODE >= 0x10300
skb_reserve(skb, 2);
insl(ioaddr + RX_FIFO, skb_put(skb, pkt_len),
(pkt_len + 3) >> 2);
outw(RxDiscard, ioaddr + EL3_CMD);
skb->protocol = eth_type_trans(skb, dev);
#else
skb->len = pkt_len;
insl(ioaddr + RX_FIFO, skb->data, (pkt_len + 3) >> 2);
outw(RxDiscard, ioaddr + EL3_CMD);
#endif
netif_rx(skb);
dev->last_rx = jiffies;
vp->stats.rx_packets++;
for (i = 200; i >= 0; i--)
if ( ! (inw(ioaddr + EL3_STATUS) & CmdInProgress))
break;
continue;
} else if (vortex_debug)
printk("%s: Couldn't allocate a sk_buff of size %d.\n",
dev->name, pkt_len);
}
outw(RxDiscard, ioaddr + EL3_CMD);
vp->stats.rx_dropped++;
for (i = 200; i >= 0; i--)
if ( ! (inw(ioaddr + EL3_STATUS) & CmdInProgress))
break;
}
return 0;
}
static int
boomerang_rx(struct device *dev)
{
struct vortex_private *vp = (struct vortex_private *)dev->priv;
int entry = vp->cur_rx % RX_RING_SIZE;
int ioaddr = dev->base_addr;
int rx_status;
if (vortex_debug > 5)
printk("   In boomerang_rx(), status %4.4x, rx_status %4.4x.\n",
inw(ioaddr+EL3_STATUS), inw(ioaddr+RxStatus));
while ((rx_status = vp->rx_ring[entry].status) & RxDComplete) {
if (rx_status & RxDError) {
unsigned char rx_error = rx_status >> 16;
if (vortex_debug > 2)
printk(" Rx error: status %2.2x.\n", rx_error);
vp->stats.rx_errors++;
if (rx_error & 0x01)  vp->stats.rx_over_errors++;
if (rx_error & 0x02)  vp->stats.rx_length_errors++;
if (rx_error & 0x04)  vp->stats.rx_frame_errors++;
if (rx_error & 0x08)  vp->stats.rx_crc_errors++;
if (rx_error & 0x10)  vp->stats.rx_length_errors++;
} else {
short pkt_len = rx_status & 0x1fff;
struct sk_buff *skb;
if (vortex_debug > 4)
printk("Receiving packet size %d status %4.4x.\n",
pkt_len, rx_status);
if (pkt_len < rx_copybreak
&& (skb = DEV_ALLOC_SKB(pkt_len + 2)) != 0) {
skb->dev = dev;
skb_reserve(skb, 2);
memcpy(skb_put(skb, pkt_len),
bus_to_virt(vp->rx_ring[entry].addr),
pkt_len);
rx_copy++;
} else{
void *temp;
skb = vp->rx_skbuff[entry];
vp->rx_skbuff[entry] = NULL;
temp = skb_put(skb, pkt_len);
if (bus_to_virt(vp->rx_ring[entry].addr) != temp)
printk("%s: Warning -- the skbuff addresses do not match"
" in boomerang_rx: %p vs. %p / %p.\n", dev->name,
bus_to_virt(vp->rx_ring[entry].addr),
skb->head, temp);
rx_nocopy++;
}
#if LINUX_VERSION_CODE > 0x10300
skb->protocol = eth_type_trans(skb, dev);
#else
skb->len = pkt_len;
#endif
netif_rx(skb);
dev->last_rx = jiffies;
vp->stats.rx_packets++;
}
entry = (++vp->cur_rx) % RX_RING_SIZE;
}
for (; vp->dirty_rx < vp->cur_rx; vp->dirty_rx++) {
struct sk_buff *skb;
entry = vp->dirty_rx % RX_RING_SIZE;
if (vp->rx_skbuff[entry] == NULL) {
skb = dev_alloc_skb(PKT_BUF_SZ);
if (skb == NULL)
break;
skb->dev = dev;
#if LINUX_VERSION_CODE > 0x10300
skb_reserve(skb, 2);
vp->rx_ring[entry].addr = virt_to_bus(skb->tail);
#else
vp->rx_ring[entry].addr = virt_to_bus(skb->data);
#endif
vp->rx_skbuff[entry] = skb;
}
vp->rx_ring[entry].status = 0;
}
return 0;
}
static int
vortex_close(struct device *dev)
{
struct vortex_private *vp = (struct vortex_private *)dev->priv;
int ioaddr = dev->base_addr;
int i;
dev->start = 0;
dev->tbusy = 1;
if (vortex_debug > 1) {
printk("%s: vortex_close() status %4.4x, Tx status %2.2x.\n",
dev->name, inw(ioaddr + EL3_STATUS), inb(ioaddr + TxStatus));
printk("%s: vortex close stats: rx_nocopy %d rx_copy %d"
" tx_queued %d.\n",
dev->name, rx_nocopy, rx_copy, queued_packet);
}
del_timer(&vp->timer);
outw(StatsDisable, ioaddr + EL3_CMD);
outw(RxDisable, ioaddr + EL3_CMD);
outw(TxDisable, ioaddr + EL3_CMD);
if (dev->if_port == XCVR_10base2)
outw(StopCoax, ioaddr + EL3_CMD);
#ifdef SA_SHIRQ
free_irq(dev->irq, dev);
#else
free_irq(dev->irq);
irq2dev_map[dev->irq] = 0;
#endif
outw(SetIntrEnb | 0x0000, ioaddr + EL3_CMD);
update_stats(ioaddr, dev);
if (vp->full_bus_master_rx) {
outl(0, ioaddr + UpListPtr);
for (i = 0; i < RX_RING_SIZE; i++)
if (vp->rx_skbuff[i]) {
#if LINUX_VERSION_CODE < 0x20100
vp->rx_skbuff[i]->free = 1;
#endif
dev_kfree_skb (vp->rx_skbuff[i], FREE_WRITE);
vp->rx_skbuff[i] = 0;
}
}
if (vp->full_bus_master_tx) {
outl(0, ioaddr + DownListPtr);
for (i = 0; i < TX_RING_SIZE; i++)
if (vp->tx_skbuff[i]) {
dev_kfree_skb(vp->tx_skbuff[i], FREE_WRITE);
vp->tx_skbuff[i] = 0;
}
}
MOD_DEC_USE_COUNT;
return 0;
}
static struct enet_statistics *
vortex_get_stats(struct device *dev)
{
struct vortex_private *vp = (struct vortex_private *)dev->priv;
unsigned long flags;
if (dev->start) {
save_flags(flags);
cli();
update_stats(dev->base_addr, dev);
restore_flags(flags);
}
return &vp->stats;
}
static void update_stats(int ioaddr, struct device *dev)
{
struct vortex_private *vp = (struct vortex_private *)dev->priv;
EL3WINDOW(6);
vp->stats.tx_carrier_errors		+= inb(ioaddr + 0);
vp->stats.tx_heartbeat_errors	+= inb(ioaddr + 1);
inb(ioaddr + 2);
vp->stats.collisions			+= inb(ioaddr + 3);
vp->stats.tx_window_errors		+= inb(ioaddr + 4);
vp->stats.rx_fifo_errors		+= inb(ioaddr + 5);
vp->stats.tx_packets			+= inb(ioaddr + 6);
vp->stats.tx_packets			+= (inb(ioaddr + 9)&0x30) << 4;
inb(ioaddr + 7);
inb(ioaddr + 8);
inw(ioaddr + 10);
inw(ioaddr + 12);
EL3WINDOW(4);
inb(ioaddr + 12);
EL3WINDOW(7);
return;
}
static void
set_rx_mode(struct device *dev)
{
int ioaddr = dev->base_addr;
short new_mode;
if (dev->flags & IFF_PROMISC) {
if (vortex_debug > 3)
printk("%s: Setting promiscuous mode.\n", dev->name);
new_mode = SetRxFilter|RxStation|RxMulticast|RxBroadcast|RxProm;
} else	if ((dev->mc_list)  ||  (dev->flags & IFF_ALLMULTI)) {
new_mode = SetRxFilter|RxStation|RxMulticast|RxBroadcast;
} else
new_mode = SetRxFilter | RxStation | RxBroadcast;
outw(new_mode, ioaddr + EL3_CMD);
}
#ifdef MODULE
void
cleanup_module(void)
{
struct device *next_dev;
while (root_vortex_dev) {
next_dev = ((struct vortex_private *)root_vortex_dev->priv)->next_module;
if (root_vortex_dev->dma)
free_dma(root_vortex_dev->dma);
unregister_netdev(root_vortex_dev);
outw(TotalReset, root_vortex_dev->base_addr + EL3_CMD);
release_region(root_vortex_dev->base_addr, CORKSCREW_TOTAL_SIZE);
kfree(root_vortex_dev);
root_vortex_dev = next_dev;
}
}
#endif