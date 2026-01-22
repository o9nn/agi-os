static const char *version = "de4x5.c:V0.5351 1998/10/4 davies@maniac.ultranet.com\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/errno.h>
#include <linux/ioport.h>
#include <linux/malloc.h>
#include <linux/bios32.h>
#include <linux/pci.h>
#include <linux/delay.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <asm/byteorder.h>
#include <asm/unaligned.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/time.h>
#include <linux/types.h>
#include <linux/unistd.h>
#include <linux/ctype.h>
#include "de4x5.h"
#define c_char const char
#include <linux/version.h>
#if LINUX_VERSION_CODE < LinuxVersionCode(2,1,0)
# define __initfunc(__arginit) __arginit
# define net_device_stats enet_statistics
# define copy_to_user(a,b,c) memcpy_tofs(a,b,c)
# define copy_from_user(a,b,c) memcpy_fromfs(a,b,c)
# define le16_to_cpu(a) cpu_to_le16(a)
# define le32_to_cpu(a) cpu_to_le32(a)
# ifdef __powerpc__
# define cpu_to_le16(a) ((((a) & 0x00ffU) << 8) | (((a) & 0xff00U) >> 8))
# define cpu_to_le32(a) ((((a) & 0x000000ffU) << 24) |\
(((a) & 0x0000ff00U) << 8) |\
(((a) & 0x00ff0000U) >> 8) |\
(((a) & 0xff000000U) >> 24))
# else
# define cpu_to_le16(a) (a)
# define cpu_to_le32(a) (a)
# endif
# include <asm/segment.h>
#else
# include <asm/uaccess.h>
# include <linux/init.h>
#endif
#define TWIDDLE(a) (u_short)le16_to_cpu(get_unaligned((u_short *)(a)))
struct phy_table {
int reset;
int id;
int ta;
struct {
int reg;
int mask;
int value;
} spd;
};
struct mii_phy {
int reset;
int id;
int ta;
struct {
int reg;
int mask;
int value;
} spd;
int addr;
u_char *gep;
u_char *rst;
u_int mc;
u_int ana;
u_int fdx;
u_int ttm;
u_int mci;
};
#define DE4X5_MAX_PHY 8
struct sia_phy {
u_char mc;
u_char ext;
int csr13;
int csr14;
int csr15;
int gepc;
int gep;
};
static struct phy_table phy_info[] = {
{0, NATIONAL_TX, 1, {0x19, 0x40, 0x00}},
{1, BROADCOM_T4, 1, {0x10, 0x02, 0x02}},
{0, SEEQ_T4 , 1, {0x12, 0x10, 0x10}},
{0, CYPRESS_T4 , 1, {0x05, 0x20, 0x20}},
{0, 0x7810 , 1, {0x05, 0x0380, 0x0380}}
};
#define GENERIC_REG 0x05
#define GENERIC_MASK MII_ANLPA_100M
#define GENERIC_VALUE MII_ANLPA_100M
static c_char enet_det[][ETH_ALEN] = {
{0x00, 0x00, 0xc0, 0x00, 0x00, 0x00},
{0x00, 0x00, 0xe8, 0x00, 0x00, 0x00}
};
#define SMC 1
#define ACCTON 2
static c_char srom_repair_info[][100] = {
{0x00,0x1e,0x00,0x00,0x00,0x08,
0x1f,0x01,0x8f,0x01,0x00,0x01,0x00,0x02,
0x01,0x00,0x00,0x78,0xe0,0x01,0x00,0x50,
0x00,0x18,}
};
#ifdef DE4X5_DEBUG
static int de4x5_debug = DE4X5_DEBUG;
#else
static int de4x5_debug = (DEBUG_MEDIA | DEBUG_VERSION);
#endif
#ifdef DE4X5_PARM
static char *args = DE4X5_PARM;
#else
static char *args = NULL;
#endif
struct parameters {
int fdx;
int autosense;
};
#define DE4X5_AUTOSENSE_MS 250
#define DE4X5_NDA 0xffe0
#define PROBE_LENGTH 32
#define ETH_PROM_SIG 0xAA5500FFUL
#define PKT_BUF_SZ 1536
#define IEEE802_3_SZ 1518
#define MAX_PKT_SZ 1514
#define MAX_DAT_SZ 1500
#define MIN_DAT_SZ 1
#define PKT_HDR_LEN 14
#define FAKE_FRAME_LEN (MAX_PKT_SZ + 1)
#define QUEUE_PKT_TIMEOUT (3*HZ)
#define CRC_POLYNOMIAL_BE 0x04c11db7UL
#define CRC_POLYNOMIAL_LE 0xedb88320UL
#define DE4X5_EISA_IO_PORTS 0x0c00
#define DE4X5_EISA_TOTAL_SIZE 0x100
#define MAX_EISA_SLOTS 16
#define EISA_SLOT_INC 0x1000
#define EISA_ALLOWED_IRQ_LIST {5, 9, 10, 11}
#define DE4X5_SIGNATURE {"DE425","DE434","DE435","DE450","DE500"}
#define DE4X5_NAME_LENGTH 8
#define PROBE_LENGTH 32
#define ETH_PROM_SIG 0xAA5500FFUL
#define PCI_MAX_BUS_NUM 8
#define DE4X5_PCI_TOTAL_SIZE 0x80
#define DE4X5_CLASS_CODE 0x00020000
#define NO_MORE_PCI -2
#define ALIGN4 ((u_long)4 - 1)
#define ALIGN8 ((u_long)8 - 1)
#define ALIGN16 ((u_long)16 - 1)
#define ALIGN32 ((u_long)32 - 1)
#define ALIGN64 ((u_long)64 - 1)
#define ALIGN128 ((u_long)128 - 1)
#define ALIGN ALIGN32
#define CACHE_ALIGN CAL_16LONG
#define DESC_SKIP_LEN DSL_0
#define DESC_ALIGN
#ifndef DEC_ONLY
static int dec_only = 0;
#else
static int dec_only = 1;
#endif
#define ENABLE_IRQs { \
imr |= lp->irq_en;\
outl(imr, DE4X5_IMR); \
}
#define DISABLE_IRQs {\
imr = inl(DE4X5_IMR);\
imr &= ~lp->irq_en;\
outl(imr, DE4X5_IMR); \
}
#define UNMASK_IRQs {\
imr |= lp->irq_mask;\
outl(imr, DE4X5_IMR); \
}
#define MASK_IRQs {\
imr = inl(DE4X5_IMR);\
imr &= ~lp->irq_mask;\
outl(imr, DE4X5_IMR); \
}
#define START_DE4X5 {\
omr = inl(DE4X5_OMR);\
omr |= OMR_ST | OMR_SR;\
outl(omr, DE4X5_OMR); \
}
#define STOP_DE4X5 {\
omr = inl(DE4X5_OMR);\
omr &= ~(OMR_ST|OMR_SR);\
outl(omr, DE4X5_OMR); \
}
#define RESET_SIA outl(0, DE4X5_SICR);
#define DE4X5_AUTOSENSE_MS 250
struct de4x5_srom {
char sub_vendor_id[2];
char sub_system_id[2];
char reserved[12];
char id_block_crc;
char reserved2;
char version;
char num_controllers;
char ieee_addr[6];
char info[100];
short chksum;
};
#define SUB_VENDOR_ID 0x500a
#define NUM_RX_DESC 8
#define NUM_TX_DESC 32
#define RX_BUFF_SZ 1536
struct de4x5_desc {
volatile s32 status;
u32 des1;
u32 buf;
u32 next;
DESC_ALIGN
};
#define DE4X5_PKT_STAT_SZ 16
#define DE4X5_PKT_BIN_SZ 128
struct de4x5_private {
char adapter_name[80];
u_long interrupt;
struct de4x5_desc rx_ring[NUM_RX_DESC];
struct de4x5_desc tx_ring[NUM_TX_DESC];
struct sk_buff *tx_skb[NUM_TX_DESC];
struct sk_buff *rx_skb[NUM_RX_DESC];
int rx_new, rx_old;
int tx_new, tx_old;
char setup_frame[SETUP_FRAME_LEN];
char frame[64];
struct net_device_stats stats;
struct {
u_int bins[DE4X5_PKT_STAT_SZ];
u_int unicast;
u_int multicast;
u_int broadcast;
u_int excessive_collisions;
u_int tx_underruns;
u_int excessive_underruns;
u_int rx_runt_frames;
u_int rx_collision;
u_int rx_dribble;
u_int rx_overflow;
} pktStats;
char rxRingSize;
char txRingSize;
int bus;
int bus_num;
int device;
int state;
int chipset;
s32 irq_mask;
s32 irq_en;
int media;
int c_media;
int fdx;
int linkOK;
int autosense;
int tx_enable;
int setup_f;
int local_state;
struct mii_phy phy[DE4X5_MAX_PHY];
struct sia_phy sia;
int active;
int mii_cnt;
int timeout;
struct timer_list timer;
int tmp;
struct {
void *priv;
void *buf;
u_long lock;
s32 csr0;
s32 csr6;
s32 csr7;
s32 gep;
s32 gepc;
s32 csr13;
s32 csr14;
s32 csr15;
int save_cnt;
struct sk_buff *skb;
} cache;
struct de4x5_srom srom;
struct device *next_module;
int rx_ovf;
int useSROM;
int useMII;
int asBitValid;
int asPolarity;
int asBit;
int defMedium;
int tcount;
int infoblock_init;
int infoleaf_offset;
s32 infoblock_csr6;
int infoblock_media;
int (*infoleaf_fn)(struct device *);
u_char *rst;
u_char ibn;
struct parameters params;
};
static struct bus_type {
int bus;
int bus_num;
int device;
int chipset;
struct de4x5_srom srom;
int autosense;
int useSROM;
} bus;
static struct {
int chipset;
int bus;
int irq;
u_char addr[ETH_ALEN];
} last = {0,};
#define TX_BUFFS_AVAIL ((lp->tx_old<=lp->tx_new)?\
lp->tx_old+lp->txRingSize-lp->tx_new-1:\
lp->tx_old -lp->tx_new-1)
#define TX_PKT_PENDING (lp->tx_old != lp->tx_new)
static int de4x5_open(struct device *dev);
static int de4x5_queue_pkt(struct sk_buff *skb, struct device *dev);
static void de4x5_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static int de4x5_close(struct device *dev);
static struct net_device_stats *de4x5_get_stats(struct device *dev);
static void de4x5_local_stats(struct device *dev, char *buf, int pkt_len);
static void set_multicast_list(struct device *dev);
static int de4x5_ioctl(struct device *dev, struct ifreq *rq, int cmd);
static int de4x5_hw_init(struct device *dev, u_long iobase);
static int de4x5_init(struct device *dev);
static int de4x5_sw_reset(struct device *dev);
static int de4x5_rx(struct device *dev);
static int de4x5_tx(struct device *dev);
static int de4x5_ast(struct device *dev);
static int de4x5_txur(struct device *dev);
static int de4x5_rx_ovfc(struct device *dev);
static int autoconf_media(struct device *dev);
static void create_packet(struct device *dev, char *frame, int len);
static void de4x5_us_delay(u32 usec);
static void de4x5_ms_delay(u32 msec);
static void load_packet(struct device *dev, char *buf, u32 flags, struct sk_buff *skb);
static int dc21040_autoconf(struct device *dev);
static int dc21041_autoconf(struct device *dev);
static int dc21140m_autoconf(struct device *dev);
static int dc2114x_autoconf(struct device *dev);
static int srom_autoconf(struct device *dev);
static int de4x5_suspect_state(struct device *dev, int timeout, int prev_state, int (*fn)(struct device *, int), int (*asfn)(struct device *));
static int dc21040_state(struct device *dev, int csr13, int csr14, int csr15, int timeout, int next_state, int suspect_state, int (*fn)(struct device *, int));
static int test_media(struct device *dev, s32 irqs, s32 irq_mask, s32 csr13, s32 csr14, s32 csr15, s32 msec);
static int test_for_100Mb(struct device *dev, int msec);
static int wait_for_link(struct device *dev);
static int test_mii_reg(struct device *dev, int reg, int mask, int pol, long msec);
static int is_spd_100(struct device *dev);
static int is_100_up(struct device *dev);
static int is_10_up(struct device *dev);
static int is_anc_capable(struct device *dev);
static int ping_media(struct device *dev, int msec);
static struct sk_buff *de4x5_alloc_rx_buff(struct device *dev, int index, int len);
static void de4x5_free_rx_buffs(struct device *dev);
static void de4x5_free_tx_buffs(struct device *dev);
static void de4x5_save_skbs(struct device *dev);
static void de4x5_rst_desc_ring(struct device *dev);
static void de4x5_cache_state(struct device *dev, int flag);
static void de4x5_put_cache(struct device *dev, struct sk_buff *skb);
static void de4x5_putb_cache(struct device *dev, struct sk_buff *skb);
static struct sk_buff *de4x5_get_cache(struct device *dev);
static void de4x5_setup_intr(struct device *dev);
static void de4x5_init_connection(struct device *dev);
static int de4x5_reset_phy(struct device *dev);
static void reset_init_sia(struct device *dev, s32 sicr, s32 strr, s32 sigr);
static int test_ans(struct device *dev, s32 irqs, s32 irq_mask, s32 msec);
static int test_tp(struct device *dev, s32 msec);
static int EISA_signature(char *name, s32 eisa_id);
static int PCI_signature(char *name, struct bus_type *lp);
static void DevicePresent(u_long iobase);
static void enet_addr_rst(u_long aprom_addr);
static int de4x5_bad_srom(struct bus_type *lp);
static short srom_rd(u_long address, u_char offset);
static void srom_latch(u_int command, u_long address);
static void srom_command(u_int command, u_long address);
static void srom_address(u_int command, u_long address, u_char offset);
static short srom_data(u_int command, u_long address);
static void sendto_srom(u_int command, u_long addr);
static int getfrom_srom(u_long addr);
static int srom_map_media(struct device *dev);
static int srom_infoleaf_info(struct device *dev);
static void srom_init(struct device *dev);
static void srom_exec(struct device *dev, u_char *p);
static int mii_rd(u_char phyreg, u_char phyaddr, u_long ioaddr);
static void mii_wr(int data, u_char phyreg, u_char phyaddr, u_long ioaddr);
static int mii_rdata(u_long ioaddr);
static void mii_wdata(int data, int len, u_long ioaddr);
static void mii_ta(u_long rw, u_long ioaddr);
static int mii_swap(int data, int len);
static void mii_address(u_char addr, u_long ioaddr);
static void sendto_mii(u32 command, int data, u_long ioaddr);
static int getfrom_mii(u32 command, u_long ioaddr);
static int mii_get_oui(u_char phyaddr, u_long ioaddr);
static int mii_get_phy(struct device *dev);
static void SetMulticastFilter(struct device *dev);
static int get_hw_addr(struct device *dev);
static void srom_repair(struct device *dev, int card);
static int test_bad_enet(struct device *dev, int status);
static int an_exception(struct bus_type *lp);
#if !defined(__sparc_v9__) && !defined(__powerpc__) && !defined(__alpha__)
static void eisa_probe(struct device *dev, u_long iobase);
#endif
static void pci_probe(struct device *dev, u_long iobase);
static void srom_search(int index);
static char *build_setup_frame(struct device *dev, int mode);
static void disable_ast(struct device *dev);
static void enable_ast(struct device *dev, u32 time_out);
static long de4x5_switch_mac_port(struct device *dev);
static int gep_rd(struct device *dev);
static void gep_wr(s32 data, struct device *dev);
static void timeout(struct device *dev, void (*fn)(u_long data), u_long data, u_long msec);
static void yawn(struct device *dev, int state);
static void link_modules(struct device *dev, struct device *tmp);
static void de4x5_parse_params(struct device *dev);
static void de4x5_dbg_open(struct device *dev);
static void de4x5_dbg_mii(struct device *dev, int k);
static void de4x5_dbg_media(struct device *dev);
static void de4x5_dbg_srom(struct de4x5_srom *p);
static void de4x5_dbg_rx(struct sk_buff *skb, int len);
static int de4x5_strncmp(char *a, char *b, int n);
static int dc21041_infoleaf(struct device *dev);
static int dc21140_infoleaf(struct device *dev);
static int dc21142_infoleaf(struct device *dev);
static int dc21143_infoleaf(struct device *dev);
static int type0_infoblock(struct device *dev, u_char count, u_char *p);
static int type1_infoblock(struct device *dev, u_char count, u_char *p);
static int type2_infoblock(struct device *dev, u_char count, u_char *p);
static int type3_infoblock(struct device *dev, u_char count, u_char *p);
static int type4_infoblock(struct device *dev, u_char count, u_char *p);
static int type5_infoblock(struct device *dev, u_char count, u_char *p);
static int compact_infoblock(struct device *dev, u_char count, u_char *p);
#ifdef MODULE
int init_module(void);
void cleanup_module(void);
static struct device *unlink_modules(struct device *p);
static struct device *insert_device(struct device *dev, u_long iobase,
int (*init)(struct device *));
static int count_adapters(void);
static int loading_module = 1;
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,0)
MODULE_PARM(de4x5_debug, "i");
MODULE_PARM(dec_only, "i");
MODULE_PARM(args, "s");
#endif
# else
static int loading_module = 0;
#endif
static char name[DE4X5_NAME_LENGTH + 1];
#if !defined(__sparc_v9__) && !defined(__powerpc__) && !defined(__alpha__)
static u_char de4x5_irq[] = EISA_ALLOWED_IRQ_LIST;
static int lastEISA = 0;
#else
static int lastEISA = MAX_EISA_SLOTS;
#endif
static int num_de4x5s = 0;
static int cfrv = 0, useSROM = 0;
static int lastPCI = -1;
static struct device *lastModule = NULL;
struct InfoLeaf {
int chipset;
int (*fn)(struct device *);
};
static struct InfoLeaf infoleaf_array[] = {
{DC21041, dc21041_infoleaf},
{DC21140, dc21140_infoleaf},
{DC21142, dc21142_infoleaf},
{DC21143, dc21143_infoleaf}
};
#define INFOLEAF_SIZE (sizeof(infoleaf_array)/(sizeof(int)+sizeof(int *)))
static int (*dc_infoblock[])(struct device *dev, u_char, u_char *) = {
type0_infoblock,
type1_infoblock,
type2_infoblock,
type3_infoblock,
type4_infoblock,
type5_infoblock,
compact_infoblock
};
#define COMPACT (sizeof(dc_infoblock)/sizeof(int *) - 1)
#define RESET_DE4X5 {\
int i;\
i=inl(DE4X5_BMR);\
de4x5_ms_delay(1);\
outl(i | BMR_SWR, DE4X5_BMR);\
de4x5_ms_delay(1);\
outl(i, DE4X5_BMR);\
de4x5_ms_delay(1);\
for (i=0;i<5;i++) {inl(DE4X5_BMR); de4x5_ms_delay(1);}\
de4x5_ms_delay(1);\
}
#define PHY_HARD_RESET {\
outl(GEP_HRST, DE4X5_GEP); \
udelay(1000); \
outl(0x00, DE4X5_GEP);\
udelay(2000); \
}
__initfunc(int
de4x5_probe(struct device *dev))
{
u_long iobase = dev->base_addr;
#if !defined(__sparc_v9__) && !defined(__powerpc__) && !defined(__alpha__)
eisa_probe(dev, iobase);
#endif
if (lastEISA == MAX_EISA_SLOTS) {
pci_probe(dev, iobase);
}
return (dev->priv ? 0 : -ENODEV);
}
__initfunc(static int
de4x5_hw_init(struct device *dev, u_long iobase))
{
struct bus_type *lp = &bus;
int i, status=0;
char *tmp;
if (lp->bus == EISA) {
outb(WAKEUP, PCI_CFPM);
} else {
pcibios_write_config_byte(lp->bus_num, lp->device << 3,
PCI_CFDA_PSM, WAKEUP);
}
de4x5_ms_delay(10);
RESET_DE4X5;
if ((inl(DE4X5_STS) & (STS_TS | STS_RS)) != 0) {
return -ENXIO;
}
useSROM = FALSE;
if (lp->bus == PCI) {
PCI_signature(name, lp);
} else {
EISA_signature(name, EISA_ID0);
}
if (*name == '\0') {
return -ENXIO;
}
dev->base_addr = iobase;
if (lp->bus == EISA) {
printk("%s: %s at 0x%04lx (EISA slot %ld)",
dev->name, name, iobase, ((iobase>>12)&0x0f));
} else {
printk("%s: %s at 0x%04lx (PCI bus %d, device %d)", dev->name, name,
iobase, lp->bus_num, lp->device);
}
printk(", h/w address ");
status = get_hw_addr(dev);
for (i = 0; i < ETH_ALEN - 1; i++) {
printk("%2.2x:", dev->dev_addr[i]);
}
printk("%2.2x,\n", dev->dev_addr[i]);
if (status != 0) {
printk("      which has an Ethernet PROM CRC error.\n");
return -ENXIO;
} else {
struct de4x5_private *lp;
dev->priv = (void *) kmalloc(sizeof(struct de4x5_private) + ALIGN,
GFP_KERNEL);
if (dev->priv == NULL) {
return -ENOMEM;
}
tmp = dev->priv;
dev->priv = (void *)(((u_long)dev->priv + ALIGN) & ~ALIGN);
lp = (struct de4x5_private *)dev->priv;
memset(dev->priv, 0, sizeof(struct de4x5_private));
lp->bus = bus.bus;
lp->bus_num = bus.bus_num;
lp->device = bus.device;
lp->chipset = bus.chipset;
lp->cache.priv = tmp;
lp->cache.gepc = GEP_INIT;
lp->asBit = GEP_SLNK;
lp->asPolarity = GEP_SLNK;
lp->asBitValid = TRUE;
lp->timeout = -1;
lp->useSROM = useSROM;
memcpy((char *)&lp->srom,(char *)&bus.srom,sizeof(struct de4x5_srom));
de4x5_parse_params(dev);
lp->autosense = lp->params.autosense;
if (lp->chipset != DC21140) {
if ((lp->chipset==DC21040) && (lp->params.autosense&TP_NW)) {
lp->params.autosense = TP;
}
if ((lp->chipset==DC21041) && (lp->params.autosense&BNC_AUI)) {
lp->params.autosense = BNC;
}
}
lp->fdx = lp->params.fdx;
sprintf(lp->adapter_name,"%s (%s)", name, dev->name);
#if !defined(__alpha__) && !defined(__powerpc__) && !defined(__sparc_v9__) && !defined(DE4X5_DO_MEMCPY)
for (i=0; i<NUM_RX_DESC; i++) {
lp->rx_ring[i].status = 0;
lp->rx_ring[i].des1 = RX_BUFF_SZ;
lp->rx_ring[i].buf = 0;
lp->rx_ring[i].next = 0;
lp->rx_skb[i] = (struct sk_buff *) 1;
}
#else
if ((tmp = (void *)kmalloc(RX_BUFF_SZ * NUM_RX_DESC + ALIGN,
GFP_KERNEL)) == NULL) {
kfree(lp->cache.priv);
return -ENOMEM;
}
lp->cache.buf = tmp;
tmp = (char *)(((u_long) tmp + ALIGN) & ~ALIGN);
for (i=0; i<NUM_RX_DESC; i++) {
lp->rx_ring[i].status = 0;
lp->rx_ring[i].des1 = cpu_to_le32(RX_BUFF_SZ);
lp->rx_ring[i].buf = cpu_to_le32(virt_to_bus(tmp+i*RX_BUFF_SZ));
lp->rx_ring[i].next = 0;
lp->rx_skb[i] = (struct sk_buff *) 1;
}
#endif
barrier();
request_region(iobase, (lp->bus == PCI ? DE4X5_PCI_TOTAL_SIZE :
DE4X5_EISA_TOTAL_SIZE),
lp->adapter_name);
lp->rxRingSize = NUM_RX_DESC;
lp->txRingSize = NUM_TX_DESC;
lp->rx_ring[lp->rxRingSize - 1].des1 |= cpu_to_le32(RD_RER);
lp->tx_ring[lp->txRingSize - 1].des1 |= cpu_to_le32(TD_TER);
outl(virt_to_bus(lp->rx_ring), DE4X5_RRBA);
outl(virt_to_bus(lp->tx_ring), DE4X5_TRBA);
lp->irq_mask = IMR_RIM | IMR_TIM | IMR_TUM | IMR_UNM;
lp->irq_en = IMR_NIM | IMR_AIM;
create_packet(dev, lp->frame, sizeof(lp->frame));
i = cfrv & 0x000000fe;
if ((lp->chipset == DC21140) && (i == 0x20)) {
lp->rx_ovf = 1;
}
if (lp->useSROM) {
lp->state = INITIALISED;
if (srom_infoleaf_info(dev)) {
return -ENXIO;
}
srom_init(dev);
}
lp->state = CLOSED;
if ((lp->chipset != DC21040) && (lp->chipset != DC21041)) {
mii_get_phy(dev);
}
#ifndef __sparc_v9__
printk("      and requires IRQ%d (provided by %s).\n", dev->irq,
#else
printk("      and requires IRQ%x (provided by %s).\n", dev->irq,
#endif
((lp->bus == PCI) ? "PCI BIOS" : "EISA CNFG"));
}
if (de4x5_debug & DEBUG_VERSION) {
printk("%s", version);
}
dev->open = &de4x5_open;
dev->hard_start_xmit = &de4x5_queue_pkt;
dev->stop = &de4x5_close;
dev->get_stats = &de4x5_get_stats;
dev->set_multicast_list = &set_multicast_list;
dev->do_ioctl = &de4x5_ioctl;
dev->mem_start = 0;
ether_setup(dev);
yawn(dev, SLEEP);
return status;
}
static int
de4x5_open(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int i, status = 0;
s32 omr;
for (i=0; i<lp->rxRingSize; i++) {
if (de4x5_alloc_rx_buff(dev, i, 0) == NULL) {
de4x5_free_rx_buffs(dev);
return -EAGAIN;
}
}
yawn(dev, WAKEUP);
status = de4x5_init(dev);
lp->state = OPEN;
de4x5_dbg_open(dev);
if (request_irq(dev->irq, (void *)de4x5_interrupt, SA_SHIRQ,
lp->adapter_name, dev)) {
printk("de4x5_open(): Requested IRQ%d is busy - attemping FAST/SHARE...", dev->irq);
if (request_irq(dev->irq, de4x5_interrupt, SA_INTERRUPT | SA_SHIRQ,
lp->adapter_name, dev)) {
printk("\n              Cannot get IRQ- reconfigure your hardware.\n");
disable_ast(dev);
de4x5_free_rx_buffs(dev);
de4x5_free_tx_buffs(dev);
yawn(dev, SLEEP);
lp->state = CLOSED;
return -EAGAIN;
} else {
printk("\n              Succeeded, but you should reconfigure your hardware to avoid this.\n");
printk("WARNING: there may be IRQ related problems in heavily loaded systems.\n");
}
}
dev->tbusy = 0;
dev->start = 1;
lp->interrupt = UNMASK_INTERRUPTS;
dev->trans_start = jiffies;
START_DE4X5;
de4x5_setup_intr(dev);
if (de4x5_debug & DEBUG_OPEN) {
printk("\tsts:  0x%08x\n", inl(DE4X5_STS));
printk("\tbmr:  0x%08x\n", inl(DE4X5_BMR));
printk("\timr:  0x%08x\n", inl(DE4X5_IMR));
printk("\tomr:  0x%08x\n", inl(DE4X5_OMR));
printk("\tsisr: 0x%08x\n", inl(DE4X5_SISR));
printk("\tsicr: 0x%08x\n", inl(DE4X5_SICR));
printk("\tstrr: 0x%08x\n", inl(DE4X5_STRR));
printk("\tsigr: 0x%08x\n", inl(DE4X5_SIGR));
}
MOD_INC_USE_COUNT;
return status;
}
static int
de4x5_init(struct device *dev)
{
test_and_set_bit(0, (void *)&dev->tbusy);
de4x5_sw_reset(dev);
autoconf_media(dev);
return 0;
}
static int
de4x5_sw_reset(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int i, j, status = 0;
s32 bmr, omr;
if (!lp->useSROM) {
if (lp->phy[lp->active].id != 0) {
lp->infoblock_csr6 = OMR_SDP | OMR_PS | OMR_HBD;
} else {
lp->infoblock_csr6 = OMR_SDP | OMR_TTM;
}
de4x5_switch_mac_port(dev);
}
bmr = (lp->chipset==DC21140 ? PBL_8 : PBL_4) | DESC_SKIP_LEN | CACHE_ALIGN;
bmr |= ((lp->chipset & ~0x00ff)==DC2114x ? BMR_RML : 0);
outl(bmr, DE4X5_BMR);
omr = inl(DE4X5_OMR) & ~OMR_PR;
if (lp->chipset == DC21140) {
omr |= (OMR_SDP | OMR_SB);
}
lp->setup_f = PERFECT;
outl(virt_to_bus(lp->rx_ring), DE4X5_RRBA);
outl(virt_to_bus(lp->tx_ring), DE4X5_TRBA);
lp->rx_new = lp->rx_old = 0;
lp->tx_new = lp->tx_old = 0;
for (i = 0; i < lp->rxRingSize; i++) {
lp->rx_ring[i].status = cpu_to_le32(R_OWN);
}
for (i = 0; i < lp->txRingSize; i++) {
lp->tx_ring[i].status = cpu_to_le32(0);
}
barrier();
SetMulticastFilter(dev);
load_packet(dev, lp->setup_frame, PERFECT_F|TD_SET|SETUP_FRAME_LEN, NULL);
outl(omr|OMR_ST, DE4X5_OMR);
sti();
for (j=0, i=0;(i<500) && (j==0);i++) {
udelay(1000);
if ((s32)le32_to_cpu(lp->tx_ring[lp->tx_new].status) >= 0) j=1;
}
outl(omr, DE4X5_OMR);
if (j == 0) {
printk("%s: Setup frame timed out, status %08x\n", dev->name,
inl(DE4X5_STS));
status = -EIO;
}
lp->tx_new = (lp->tx_new + 1) % lp->txRingSize;
lp->tx_old = lp->tx_new;
return status;
}
static int
de4x5_queue_pkt(struct sk_buff *skb, struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int status = 0;
test_and_set_bit(0, (void*)&dev->tbusy);
if (lp->tx_enable == NO) {
return -1;
}
cli();
de4x5_tx(dev);
sti();
if (test_and_set_bit(0, (void *)&lp->cache.lock) && !lp->interrupt)
return -1;
if (dev->tbusy || lp->tx_skb[lp->tx_new]) {
if (lp->interrupt) {
de4x5_putb_cache(dev, skb);
} else {
de4x5_put_cache(dev, skb);
}
if (de4x5_debug & DEBUG_TX) {
printk("%s: transmit busy, lost media or stale skb found:\n  STS:%08x\n  tbusy:%ld\n  IMR:%08x\n  OMR:%08x\n Stale skb: %s\n",dev->name, inl(DE4X5_STS), dev->tbusy, inl(DE4X5_IMR), inl(DE4X5_OMR), (lp->tx_skb[lp->tx_new] ? "YES" : "NO"));
}
} else if (skb->len > 0) {
if (lp->cache.skb && !lp->interrupt) {
de4x5_put_cache(dev, skb);
skb = de4x5_get_cache(dev);
}
while (skb && !dev->tbusy && !lp->tx_skb[lp->tx_new]) {
cli();
test_and_set_bit(0, (void*)&dev->tbusy);
load_packet(dev, skb->data, TD_IC | TD_LS | TD_FS | skb->len, skb);
#if LINUX_VERSION_CODE >= ((2 << 16) | (1 << 8))
lp->stats.tx_bytes += skb->len;
#endif
outl(POLL_DEMAND, DE4X5_TPD);
lp->tx_new = (lp->tx_new + 1) % lp->txRingSize;
dev->trans_start = jiffies;
if (TX_BUFFS_AVAIL) {
dev->tbusy = 0;
}
skb = de4x5_get_cache(dev);
sti();
}
if (skb) de4x5_putb_cache(dev, skb);
}
lp->cache.lock = 0;
return status;
}
static void
de4x5_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct device *dev = (struct device *)dev_id;
struct de4x5_private *lp;
s32 imr, omr, sts, limit;
u_long iobase;
if (dev == NULL) {
printk ("de4x5_interrupt(): irq %d for unknown device.\n", irq);
return;
}
lp = (struct de4x5_private *)dev->priv;
iobase = dev->base_addr;
DISABLE_IRQs;
if (test_and_set_bit(MASK_INTERRUPTS, (void*) &lp->interrupt))
printk("%s: Re-entering the interrupt handler.\n", dev->name);
#if LINUX_VERSION_CODE >= ((2 << 16) | (1 << 8))
synchronize_irq();
#endif
for (limit=0; limit<8; limit++) {
sts = inl(DE4X5_STS);
outl(sts, DE4X5_STS);
if (!(sts & lp->irq_mask)) break;
if (sts & (STS_RI | STS_RU))
de4x5_rx(dev);
if (sts & (STS_TI | STS_TU))
de4x5_tx(dev);
if (sts & STS_LNF) {
lp->irq_mask &= ~IMR_LFM;
}
if (sts & STS_UNF) {
de4x5_txur(dev);
}
if (sts & STS_SE) {
STOP_DE4X5;
printk("%s: Fatal bus error occurred, sts=%#8x, device stopped.\n",
dev->name, sts);
return;
}
}
if (!test_and_set_bit(0, (void *)&lp->cache.lock)) {
while (lp->cache.skb && !dev->tbusy && lp->tx_enable) {
de4x5_queue_pkt(de4x5_get_cache(dev), dev);
}
lp->cache.lock = 0;
}
lp->interrupt = UNMASK_INTERRUPTS;
ENABLE_IRQs;
return;
}
static int
de4x5_rx(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int entry;
s32 status;
for (entry=lp->rx_new; (s32)le32_to_cpu(lp->rx_ring[entry].status)>=0;
entry=lp->rx_new) {
status = (s32)le32_to_cpu(lp->rx_ring[entry].status);
if (lp->rx_ovf) {
if (inl(DE4X5_MFC) & MFC_FOCM) {
de4x5_rx_ovfc(dev);
break;
}
}
if (status & RD_FS) {
lp->rx_old = entry;
}
if (status & RD_LS) {
if (lp->tx_enable) lp->linkOK++;
if (status & RD_ES) {
lp->stats.rx_errors++;
if (status & (RD_RF | RD_TL)) lp->stats.rx_frame_errors++;
if (status & RD_CE) lp->stats.rx_crc_errors++;
if (status & RD_OF) lp->stats.rx_fifo_errors++;
if (status & RD_TL) lp->stats.rx_length_errors++;
if (status & RD_RF) lp->pktStats.rx_runt_frames++;
if (status & RD_CS) lp->pktStats.rx_collision++;
if (status & RD_DB) lp->pktStats.rx_dribble++;
if (status & RD_OF) lp->pktStats.rx_overflow++;
} else {
struct sk_buff *skb;
short pkt_len = (short)(le32_to_cpu(lp->rx_ring[entry].status)
>> 16) - 4;
if ((skb = de4x5_alloc_rx_buff(dev, entry, pkt_len)) == NULL) {
printk("%s: Insufficient memory; nuking packet.\n",
dev->name);
lp->stats.rx_dropped++;
} else {
de4x5_dbg_rx(skb, pkt_len);
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
#if LINUX_VERSION_CODE >= ((2 << 16) | (1 << 8))
lp->stats.rx_bytes += pkt_len;
#endif
de4x5_local_stats(dev, skb->data, pkt_len);
}
}
for (;lp->rx_old!=entry;lp->rx_old=(lp->rx_old+1)%lp->rxRingSize) {
lp->rx_ring[lp->rx_old].status = cpu_to_le32(R_OWN);
barrier();
}
lp->rx_ring[entry].status = cpu_to_le32(R_OWN);
barrier();
}
lp->rx_new = (lp->rx_new + 1) % lp->rxRingSize;
}
return 0;
}
static int
de4x5_tx(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int entry;
s32 status;
for (entry = lp->tx_old; entry != lp->tx_new; entry = lp->tx_old) {
status = (s32)le32_to_cpu(lp->tx_ring[entry].status);
if (status < 0) {
break;
} else if (status != 0x7fffffff) {
if (status & TD_ES) {
lp->stats.tx_errors++;
if (status & TD_NC) lp->stats.tx_carrier_errors++;
if (status & TD_LC) lp->stats.tx_window_errors++;
if (status & TD_UF) lp->stats.tx_fifo_errors++;
if (status & TD_EC) lp->pktStats.excessive_collisions++;
if (status & TD_DE) lp->stats.tx_aborted_errors++;
if (TX_PKT_PENDING) {
outl(POLL_DEMAND, DE4X5_TPD);
}
} else {
lp->stats.tx_packets++;
if (lp->tx_enable) lp->linkOK++;
}
lp->stats.collisions += ((status & TD_EC) ? 16 :
((status & TD_CC) >> 3));
if (lp->tx_skb[entry] != NULL) {
dev_kfree_skb(lp->tx_skb[entry], FREE_WRITE);
lp->tx_skb[entry] = NULL;
}
}
lp->tx_old = (lp->tx_old + 1) % lp->txRingSize;
}
if (TX_BUFFS_AVAIL && dev->tbusy) {
dev->tbusy = 0;
if (lp->interrupt) mark_bh(NET_BH);
}
return 0;
}
static int
de4x5_ast(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int next_tick = DE4X5_AUTOSENSE_MS;
disable_ast(dev);
if (lp->useSROM) {
next_tick = srom_autoconf(dev);
} else if (lp->chipset == DC21140) {
next_tick = dc21140m_autoconf(dev);
} else if (lp->chipset == DC21041) {
next_tick = dc21041_autoconf(dev);
} else if (lp->chipset == DC21040) {
next_tick = dc21040_autoconf(dev);
}
lp->linkOK = 0;
enable_ast(dev, next_tick);
return 0;
}
static int
de4x5_txur(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int omr;
omr = inl(DE4X5_OMR);
if (!(omr & OMR_SF) || (lp->chipset==DC21041) || (lp->chipset==DC21040)) {
omr &= ~(OMR_ST|OMR_SR);
outl(omr, DE4X5_OMR);
while (inl(DE4X5_STS) & STS_TS);
if ((omr & OMR_TR) < OMR_TR) {
omr += 0x4000;
} else {
omr |= OMR_SF;
}
outl(omr | OMR_ST | OMR_SR, DE4X5_OMR);
}
return 0;
}
static int
de4x5_rx_ovfc(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int omr;
omr = inl(DE4X5_OMR);
outl(omr & ~OMR_SR, DE4X5_OMR);
while (inl(DE4X5_STS) & STS_RS);
for (; (s32)le32_to_cpu(lp->rx_ring[lp->rx_new].status)>=0;) {
lp->rx_ring[lp->rx_new].status = cpu_to_le32(R_OWN);
lp->rx_new = (lp->rx_new + 1) % lp->rxRingSize;
}
outl(omr, DE4X5_OMR);
return 0;
}
static int
de4x5_close(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
s32 imr, omr;
disable_ast(dev);
dev->start = 0;
dev->tbusy = 1;
if (de4x5_debug & DEBUG_CLOSE) {
printk("%s: Shutting down ethercard, status was %8.8x.\n",
dev->name, inl(DE4X5_STS));
}
DISABLE_IRQs;
STOP_DE4X5;
free_irq(dev->irq, dev);
lp->state = CLOSED;
de4x5_free_rx_buffs(dev);
de4x5_free_tx_buffs(dev);
MOD_DEC_USE_COUNT;
yawn(dev, SLEEP);
return 0;
}
static struct net_device_stats *
de4x5_get_stats(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
lp->stats.rx_missed_errors = (int)(inl(DE4X5_MFC) & (MFC_OVFL | MFC_CNTR));
return &lp->stats;
}
static void
de4x5_local_stats(struct device *dev, char *buf, int pkt_len)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int i;
for (i=1; i<DE4X5_PKT_STAT_SZ-1; i++) {
if (pkt_len < (i*DE4X5_PKT_BIN_SZ)) {
lp->pktStats.bins[i]++;
i = DE4X5_PKT_STAT_SZ;
}
}
if (buf[0] & 0x01) {
if ((*(s32 *)&buf[0] == -1) && (*(s16 *)&buf[4] == -1)) {
lp->pktStats.broadcast++;
} else {
lp->pktStats.multicast++;
}
} else if ((*(s32 *)&buf[0] == *(s32 *)&dev->dev_addr[0]) &&
(*(s16 *)&buf[4] == *(s16 *)&dev->dev_addr[4])) {
lp->pktStats.unicast++;
}
lp->pktStats.bins[0]++;
if (lp->pktStats.bins[0] == 0) {
memset((char *)&lp->pktStats, 0, sizeof(lp->pktStats));
}
return;
}
static void
load_packet(struct device *dev, char *buf, u32 flags, struct sk_buff *skb)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
lp->tx_ring[lp->tx_new].buf = cpu_to_le32(virt_to_bus(buf));
lp->tx_ring[lp->tx_new].des1 &= cpu_to_le32(TD_TER);
lp->tx_ring[lp->tx_new].des1 |= cpu_to_le32(flags);
lp->tx_skb[lp->tx_new] = skb;
barrier();
lp->tx_ring[lp->tx_new].status = cpu_to_le32(T_OWN);
barrier();
return;
}
static void
set_multicast_list(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if (lp->state == OPEN) {
if (dev->flags & IFF_PROMISC) {
u32 omr;
omr = inl(DE4X5_OMR);
omr |= OMR_PR;
outl(omr, DE4X5_OMR);
} else {
SetMulticastFilter(dev);
load_packet(dev, lp->setup_frame, TD_IC | PERFECT_F | TD_SET |
SETUP_FRAME_LEN, NULL);
lp->tx_new = (lp->tx_new + 1) % lp->txRingSize;
outl(POLL_DEMAND, DE4X5_TPD);
dev->trans_start = jiffies;
}
}
return;
}
static void
SetMulticastFilter(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
struct dev_mc_list *dmi=dev->mc_list;
u_long iobase = dev->base_addr;
int i, j, bit, byte;
u16 hashcode;
u32 omr, crc, poly = CRC_POLYNOMIAL_LE;
char *pa;
unsigned char *addrs;
omr = inl(DE4X5_OMR);
omr &= ~(OMR_PR | OMR_PM);
pa = build_setup_frame(dev, ALL);
if ((dev->flags & IFF_ALLMULTI) || (dev->mc_count > 14)) {
omr |= OMR_PM;
} else if (lp->setup_f == HASH_PERF) {
for (i=0;i<dev->mc_count;i++) {
addrs=dmi->dmi_addr;
dmi=dmi->next;
if ((*addrs & 0x01) == 1) {
crc = 0xffffffff;
for (byte=0;byte<ETH_ALEN;byte++) {
for (bit = *addrs++,j=0;j<8;j++, bit>>=1) {
crc = (crc >> 1) ^ (((crc ^ bit) & 0x01) ? poly : 0);
}
}
hashcode = crc & HASH_BITS;
byte = hashcode >> 3;
bit = 1 << (hashcode & 0x07);
byte <<= 1;
if (byte & 0x02) {
byte -= 1;
}
lp->setup_frame[byte] |= bit;
}
}
} else {
for (j=0; j<dev->mc_count; j++) {
addrs=dmi->dmi_addr;
dmi=dmi->next;
for (i=0; i<ETH_ALEN; i++) {
*(pa + (i&1)) = *addrs++;
if (i & 0x01) pa += 4;
}
}
}
outl(omr, DE4X5_OMR);
return;
}
#if !defined(__sparc_v9__) && !defined(__powerpc__) && !defined(__alpha__)
__initfunc(static void
eisa_probe(struct device *dev, u_long ioaddr))
{
int i, maxSlots, status, device;
u_char irq;
u_short vendor;
u32 cfid;
u_long iobase;
struct bus_type *lp = &bus;
char name[DE4X5_STRLEN];
if (lastEISA == MAX_EISA_SLOTS) return;
lp->bus = EISA;
if (ioaddr == 0) {
iobase = EISA_SLOT_INC;
i = 1;
maxSlots = MAX_EISA_SLOTS;
} else {
iobase = ioaddr;
i = (ioaddr >> 12);
maxSlots = i + 1;
}
for (status = -ENODEV; (i<maxSlots) && (dev!=NULL); i++, iobase+=EISA_SLOT_INC) {
if (EISA_signature(name, EISA_ID)) {
cfid = (u32) inl(PCI_CFID);
cfrv = (u_short) inl(PCI_CFRV);
device = (cfid >> 8) & 0x00ffff00;
vendor = (u_short) cfid;
irq = inb(EISA_REG0);
irq = de4x5_irq[(irq >> 1) & 0x03];
if (is_DC2114x) device |= (cfrv & CFRV_RN);
lp->chipset = device;
outl(PCI_COMMAND_IO | PCI_COMMAND_MASTER, PCI_CFCS);
outl(0x00006000, PCI_CFLT);
outl(iobase, PCI_CBIO);
DevicePresent(EISA_APROM);
if (check_region(iobase, DE4X5_EISA_TOTAL_SIZE) == 0) {
dev->irq = irq;
if ((status = de4x5_hw_init(dev, iobase)) == 0) {
num_de4x5s++;
if (loading_module) link_modules(lastModule, dev);
lastEISA = i;
return;
}
} else if (ioaddr != 0) {
printk("%s: region already allocated at 0x%04lx.\n", dev->name,iobase);
}
}
}
if (ioaddr == 0) lastEISA = i;
return;
}
#endif
#define PCI_DEVICE (dev_num << 3)
#define PCI_LAST_DEV 32
__initfunc(static void
pci_probe(struct device *dev, u_long ioaddr))
{
u_char pb, pbus, dev_num, dnum, dev_fn, timer, tirq;
u_short dev_id, vendor, index, status;
u_int tmp, irq = 0, device, class = DE4X5_CLASS_CODE;
u_long iobase = 0;
struct bus_type *lp = &bus;
if (lastPCI == NO_MORE_PCI) return;
if (!pcibios_present()) {
lastPCI = NO_MORE_PCI;
return;
}
lp->bus = PCI;
lp->bus_num = 0;
if ((ioaddr < 0x1000) && loading_module) {
pbus = (u_short)(ioaddr >> 8);
dnum = (u_short)(ioaddr & 0xff);
} else {
pbus = 0;
dnum = 0;
}
for (index=lastPCI+1;
(pcibios_find_class(class, index, &pb, &dev_fn)== PCIBIOS_SUCCESSFUL);
index++) {
dev_num = PCI_SLOT(dev_fn);
if ((!pbus && !dnum) || ((pbus == pb) && (dnum == dev_num))) {
#ifdef __sparc_v9__
struct pci_dev *pdev;
for (pdev = pci_devices; pdev; pdev = pdev->next) {
if ((pdev->bus->number==pb) && (pdev->devfn==dev_fn)) break;
}
#endif
device = 0;
pcibios_read_config_word(pb, PCI_DEVICE, PCI_VENDOR_ID, &vendor);
pcibios_read_config_word(pb, PCI_DEVICE, PCI_DEVICE_ID, &dev_id);
device = dev_id;
device <<= 8;
if (!(is_DC21040 || is_DC21041 || is_DC21140 || is_DC2114x)) {
continue;
}
if (lp->bus_num != pb) {
lp->bus_num = pb;
srom_search(index);
}
pcibios_read_config_dword(pb, PCI_DEVICE, PCI_REVISION_ID, &cfrv);
lp->device = dev_num;
lp->bus_num = pb;
if (is_DC2114x) device |= (cfrv & CFRV_RN);
lp->chipset = device;
#ifndef __sparc_v9__
pcibios_read_config_dword(pb, PCI_DEVICE, PCI_BASE_ADDRESS_0, &tmp);
iobase = tmp;
#else
iobase = pdev->base_address[0];
#endif
iobase &= CBIO_MASK;
#ifndef __sparc_v9__
pcibios_read_config_byte(pb, PCI_DEVICE, PCI_INTERRUPT_LINE, &tirq);
irq = tirq;
#else
irq = pdev->irq;
#endif
if ((irq == 0) || (irq == 0xff) || ((int)irq == -1)) continue;
pcibios_read_config_word(pb, PCI_DEVICE, PCI_COMMAND, &status);
#ifdef __powerpc__
if (!(status & PCI_COMMAND_IO)) {
status |= PCI_COMMAND_IO;
pcibios_write_config_word(pb, PCI_DEVICE, PCI_COMMAND, status);
pcibios_read_config_word(pb, PCI_DEVICE, PCI_COMMAND, &status);
}
#endif
if (!(status & PCI_COMMAND_IO)) continue;
if (!(status & PCI_COMMAND_MASTER)) {
status |= PCI_COMMAND_MASTER;
pcibios_write_config_word(pb, PCI_DEVICE, PCI_COMMAND, status);
pcibios_read_config_word(pb, PCI_DEVICE, PCI_COMMAND, &status);
}
if (!(status & PCI_COMMAND_MASTER)) continue;
pcibios_read_config_byte(pb, PCI_DEVICE, PCI_LATENCY_TIMER, &timer);
if (timer < 0x60) {
pcibios_write_config_byte(pb, PCI_DEVICE, PCI_LATENCY_TIMER, 0x60);
}
DevicePresent(DE4X5_APROM);
if (check_region(iobase, DE4X5_PCI_TOTAL_SIZE) == 0) {
dev->irq = irq;
if ((status = de4x5_hw_init(dev, iobase)) == 0) {
num_de4x5s++;
lastPCI = index;
if (loading_module) link_modules(lastModule, dev);
return;
}
} else if (ioaddr != 0) {
printk("%s: region already allocated at 0x%04lx.\n", dev->name,
iobase);
}
}
}
lastPCI = NO_MORE_PCI;
return;
}
__initfunc(static void
srom_search(int index))
{
u_char pb, dev_fn, tirq;
u_short dev_id, dev_num, vendor, status;
u_int tmp, irq = 0, device, class = DE4X5_CLASS_CODE;
u_long iobase = 0;
int i, j;
struct bus_type *lp = &bus;
for (;
(pcibios_find_class(class, index, &pb, &dev_fn)!= PCIBIOS_DEVICE_NOT_FOUND);
index++) {
if (lp->bus_num != pb) return;
dev_num = PCI_SLOT(dev_fn);
#ifdef __sparc_v9__
struct pci_dev *pdev;
for (pdev = pci_devices; pdev; pdev = pdev->next) {
if ((pdev->bus->number == pb) && (pdev->devfn == dev_fn)) break;
}
#endif
device = 0;
pcibios_read_config_word(pb, PCI_DEVICE, PCI_VENDOR_ID, &vendor);
pcibios_read_config_word(pb, PCI_DEVICE, PCI_DEVICE_ID, &dev_id);
device = dev_id;
device <<= 8;
if (!(is_DC21040 || is_DC21041 || is_DC21140 || is_DC2114x)) {
continue;
}
pcibios_read_config_dword(pb, PCI_DEVICE, PCI_REVISION_ID, &cfrv);
lp->device = dev_num;
lp->bus_num = pb;
if (is_DC2114x) device |= (cfrv & CFRV_RN);
lp->chipset = device;
#ifndef __sparc_v9__
pcibios_read_config_dword(pb, PCI_DEVICE, PCI_BASE_ADDRESS_0, &tmp);
iobase = tmp;
#else
iobase = pdev->base_address[0];
#endif
iobase &= CBIO_MASK;
#ifndef __sparc_v9__
pcibios_read_config_byte(pb, PCI_DEVICE, PCI_INTERRUPT_LINE, &tirq);
irq = tirq;
#else
irq = pdev->irq;
#endif
if ((irq == 0) || (irq == 0xff) || ((int)irq == -1)) continue;
pcibios_read_config_word(pb, PCI_DEVICE, PCI_COMMAND, &status);
if (!(status & PCI_COMMAND_IO)) continue;
DevicePresent(DE4X5_APROM);
for (j=0, i=0; i<ETH_ALEN; i++) {
j += (u_char) *((u_char *)&lp->srom + SROM_HWADD + i);
}
if ((j != 0) && (j != 0x5fa)) {
last.chipset = device;
last.bus = pb;
last.irq = irq;
for (i=0; i<ETH_ALEN; i++) {
last.addr[i] = (u_char)*((u_char *)&lp->srom + SROM_HWADD + i);
}
return;
}
}
return;
}
__initfunc(static void
link_modules(struct device *dev, struct device *tmp))
{
struct device *p=dev;
if (p) {
while (((struct de4x5_private *)(p->priv))->next_module) {
p = ((struct de4x5_private *)(p->priv))->next_module;
}
if (dev != tmp) {
((struct de4x5_private *)(p->priv))->next_module = tmp;
} else {
((struct de4x5_private *)(p->priv))->next_module = NULL;
}
}
return;
}
static int
autoconf_media(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int next_tick = DE4X5_AUTOSENSE_MS;
lp->linkOK = 0;
lp->c_media = AUTO;
disable_ast(dev);
inl(DE4X5_MFC);
lp->media = INIT;
lp->tcount = 0;
if (lp->useSROM) {
next_tick = srom_autoconf(dev);
} else if (lp->chipset == DC21040) {
next_tick = dc21040_autoconf(dev);
} else if (lp->chipset == DC21041) {
next_tick = dc21041_autoconf(dev);
} else if (lp->chipset == DC21140) {
next_tick = dc21140m_autoconf(dev);
}
enable_ast(dev, next_tick);
return (lp->media);
}
static int
dc21040_autoconf(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int next_tick = DE4X5_AUTOSENSE_MS;
s32 imr;
switch (lp->media) {
case INIT:
DISABLE_IRQs;
lp->tx_enable = NO;
lp->timeout = -1;
de4x5_save_skbs(dev);
if ((lp->autosense == AUTO) || (lp->autosense == TP)) {
lp->media = TP;
} else if ((lp->autosense == BNC) || (lp->autosense == AUI) || (lp->autosense == BNC_AUI)) {
lp->media = BNC_AUI;
} else if (lp->autosense == EXT_SIA) {
lp->media = EXT_SIA;
} else {
lp->media = NC;
}
lp->local_state = 0;
next_tick = dc21040_autoconf(dev);
break;
case TP:
next_tick = dc21040_state(dev, 0x8f01, 0xffff, 0x0000, 3000, BNC_AUI,
TP_SUSPECT, test_tp);
break;
case TP_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, TP, test_tp, dc21040_autoconf);
break;
case BNC:
case AUI:
case BNC_AUI:
next_tick = dc21040_state(dev, 0x8f09, 0x0705, 0x0006, 3000, EXT_SIA,
BNC_AUI_SUSPECT, ping_media);
break;
case BNC_AUI_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, BNC_AUI, ping_media, dc21040_autoconf);
break;
case EXT_SIA:
next_tick = dc21040_state(dev, 0x3041, 0x0000, 0x0006, 3000,
NC, EXT_SIA_SUSPECT, ping_media);
break;
case EXT_SIA_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, EXT_SIA, ping_media, dc21040_autoconf);
break;
case NC:
reset_init_sia(dev, 0x8f01, 0xffff, 0x0000);
if (lp->media != lp->c_media) {
de4x5_dbg_media(dev);
lp->c_media = lp->media;
}
lp->media = INIT;
lp->tx_enable = NO;
break;
}
return next_tick;
}
static int
dc21040_state(struct device *dev, int csr13, int csr14, int csr15, int timeout,
int next_state, int suspect_state,
int (*fn)(struct device *, int))
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int next_tick = DE4X5_AUTOSENSE_MS;
int linkBad;
switch (lp->local_state) {
case 0:
reset_init_sia(dev, csr13, csr14, csr15);
lp->local_state++;
next_tick = 500;
break;
case 1:
if (!lp->tx_enable) {
linkBad = fn(dev, timeout);
if (linkBad < 0) {
next_tick = linkBad & ~TIMER_CB;
} else {
if (linkBad && (lp->autosense == AUTO)) {
lp->local_state = 0;
lp->media = next_state;
} else {
de4x5_init_connection(dev);
}
}
} else if (!lp->linkOK && (lp->autosense == AUTO)) {
lp->media = suspect_state;
next_tick = 3000;
}
break;
}
return next_tick;
}
static int
de4x5_suspect_state(struct device *dev, int timeout, int prev_state,
int (*fn)(struct device *, int),
int (*asfn)(struct device *))
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int next_tick = DE4X5_AUTOSENSE_MS;
int linkBad;
switch (lp->local_state) {
case 1:
if (lp->linkOK) {
lp->media = prev_state;
} else {
lp->local_state++;
next_tick = asfn(dev);
}
break;
case 2:
linkBad = fn(dev, timeout);
if (linkBad < 0) {
next_tick = linkBad & ~TIMER_CB;
} else if (!linkBad) {
lp->local_state--;
lp->media = prev_state;
} else {
lp->media = INIT;
lp->tcount++;
}
}
return next_tick;
}
static int
dc21041_autoconf(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
s32 sts, irqs, irq_mask, imr, omr;
int next_tick = DE4X5_AUTOSENSE_MS;
switch (lp->media) {
case INIT:
DISABLE_IRQs;
lp->tx_enable = NO;
lp->timeout = -1;
de4x5_save_skbs(dev);
if ((lp->autosense == AUTO) || (lp->autosense == TP_NW)) {
lp->media = TP;
} else if (lp->autosense == TP) {
lp->media = TP;
} else if (lp->autosense == BNC) {
lp->media = BNC;
} else if (lp->autosense == AUI) {
lp->media = AUI;
} else {
lp->media = NC;
}
lp->local_state = 0;
next_tick = dc21041_autoconf(dev);
break;
case TP_NW:
if (lp->timeout < 0) {
omr = inl(DE4X5_OMR);
outl(omr | OMR_FDX, DE4X5_OMR);
}
irqs = STS_LNF | STS_LNP;
irq_mask = IMR_LFM | IMR_LPM;
sts = test_media(dev, irqs, irq_mask, 0xef01, 0xffff, 0x0008, 2400);
if (sts < 0) {
next_tick = sts & ~TIMER_CB;
} else {
if (sts & STS_LNP) {
lp->media = ANS;
} else {
lp->media = AUI;
}
next_tick = dc21041_autoconf(dev);
}
break;
case ANS:
if (!lp->tx_enable) {
irqs = STS_LNP;
irq_mask = IMR_LPM;
sts = test_ans(dev, irqs, irq_mask, 3000);
if (sts < 0) {
next_tick = sts & ~TIMER_CB;
} else {
if (!(sts & STS_LNP) && (lp->autosense == AUTO)) {
lp->media = TP;
next_tick = dc21041_autoconf(dev);
} else {
lp->local_state = 1;
de4x5_init_connection(dev);
}
}
} else if (!lp->linkOK && (lp->autosense == AUTO)) {
lp->media = ANS_SUSPECT;
next_tick = 3000;
}
break;
case ANS_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, ANS, test_tp, dc21041_autoconf);
break;
case TP:
if (!lp->tx_enable) {
if (lp->timeout < 0) {
omr = inl(DE4X5_OMR);
outl(omr & ~OMR_FDX, DE4X5_OMR);
}
irqs = STS_LNF | STS_LNP;
irq_mask = IMR_LFM | IMR_LPM;
sts = test_media(dev,irqs, irq_mask, 0xef01, 0xff3f, 0x0008, 2400);
if (sts < 0) {
next_tick = sts & ~TIMER_CB;
} else {
if (!(sts & STS_LNP) && (lp->autosense == AUTO)) {
if (inl(DE4X5_SISR) & SISR_NRA) {
lp->media = AUI;
} else {
lp->media = BNC;
}
next_tick = dc21041_autoconf(dev);
} else {
lp->local_state = 1;
de4x5_init_connection(dev);
}
}
} else if (!lp->linkOK && (lp->autosense == AUTO)) {
lp->media = TP_SUSPECT;
next_tick = 3000;
}
break;
case TP_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, TP, test_tp, dc21041_autoconf);
break;
case AUI:
if (!lp->tx_enable) {
if (lp->timeout < 0) {
omr = inl(DE4X5_OMR);
outl(omr & ~OMR_FDX, DE4X5_OMR);
}
irqs = 0;
irq_mask = 0;
sts = test_media(dev,irqs, irq_mask, 0xef09, 0xf73d, 0x000e, 1000);
if (sts < 0) {
next_tick = sts & ~TIMER_CB;
} else {
if (!(inl(DE4X5_SISR) & SISR_SRA) && (lp->autosense == AUTO)) {
lp->media = BNC;
next_tick = dc21041_autoconf(dev);
} else {
lp->local_state = 1;
de4x5_init_connection(dev);
}
}
} else if (!lp->linkOK && (lp->autosense == AUTO)) {
lp->media = AUI_SUSPECT;
next_tick = 3000;
}
break;
case AUI_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, AUI, ping_media, dc21041_autoconf);
break;
case BNC:
switch (lp->local_state) {
case 0:
if (lp->timeout < 0) {
omr = inl(DE4X5_OMR);
outl(omr & ~OMR_FDX, DE4X5_OMR);
}
irqs = 0;
irq_mask = 0;
sts = test_media(dev,irqs, irq_mask, 0xef09, 0xf73d, 0x0006, 1000);
if (sts < 0) {
next_tick = sts & ~TIMER_CB;
} else {
lp->local_state++;
next_tick = dc21041_autoconf(dev);
}
break;
case 1:
if (!lp->tx_enable) {
if ((sts = ping_media(dev, 3000)) < 0) {
next_tick = sts & ~TIMER_CB;
} else {
if (sts) {
lp->local_state = 0;
lp->media = NC;
} else {
de4x5_init_connection(dev);
}
}
} else if (!lp->linkOK && (lp->autosense == AUTO)) {
lp->media = BNC_SUSPECT;
next_tick = 3000;
}
break;
}
break;
case BNC_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, BNC, ping_media, dc21041_autoconf);
break;
case NC:
omr = inl(DE4X5_OMR);
outl(omr | OMR_FDX, DE4X5_OMR);
reset_init_sia(dev, 0xef01, 0xffff, 0x0008);
if (lp->media != lp->c_media) {
de4x5_dbg_media(dev);
lp->c_media = lp->media;
}
lp->media = INIT;
lp->tx_enable = NO;
break;
}
return next_tick;
}
static int
dc21140m_autoconf(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int ana, anlpa, cap, cr, slnk, sr;
int next_tick = DE4X5_AUTOSENSE_MS;
u_long imr, omr, iobase = dev->base_addr;
switch(lp->media) {
case INIT:
if (lp->timeout < 0) {
DISABLE_IRQs;
lp->tx_enable = FALSE;
lp->linkOK = 0;
de4x5_save_skbs(dev);
}
if ((next_tick = de4x5_reset_phy(dev)) < 0) {
next_tick &= ~TIMER_CB;
} else {
if (lp->useSROM) {
if (srom_map_media(dev) < 0) {
lp->tcount++;
return next_tick;
}
srom_exec(dev, lp->phy[lp->active].gep);
if (lp->infoblock_media == ANS) {
ana = lp->phy[lp->active].ana | MII_ANA_CSMA;
mii_wr(ana, MII_ANA, lp->phy[lp->active].addr, DE4X5_MII);
}
} else {
lp->tmp = MII_SR_ASSC;
SET_10Mb;
if (lp->autosense == _100Mb) {
lp->media = _100Mb;
} else if (lp->autosense == _10Mb) {
lp->media = _10Mb;
} else if ((lp->autosense == AUTO) &&
((sr=is_anc_capable(dev)) & MII_SR_ANC)) {
ana = (((sr >> 6) & MII_ANA_TAF) | MII_ANA_CSMA);
ana &= (lp->fdx ? ~0 : ~MII_ANA_FDAM);
mii_wr(ana, MII_ANA, lp->phy[lp->active].addr, DE4X5_MII);
lp->media = ANS;
} else if (lp->autosense == AUTO) {
lp->media = SPD_DET;
} else if (is_spd_100(dev) && is_100_up(dev)) {
lp->media = _100Mb;
} else {
lp->media = NC;
}
}
lp->local_state = 0;
next_tick = dc21140m_autoconf(dev);
}
break;
case ANS:
switch (lp->local_state) {
case 0:
if (lp->timeout < 0) {
mii_wr(MII_CR_ASSE | MII_CR_RAN, MII_CR, lp->phy[lp->active].addr, DE4X5_MII);
}
cr = test_mii_reg(dev, MII_CR, MII_CR_RAN, FALSE, 500);
if (cr < 0) {
next_tick = cr & ~TIMER_CB;
} else {
if (cr) {
lp->local_state = 0;
lp->media = SPD_DET;
} else {
lp->local_state++;
}
next_tick = dc21140m_autoconf(dev);
}
break;
case 1:
if ((sr=test_mii_reg(dev, MII_SR, MII_SR_ASSC, TRUE, 2000)) < 0) {
next_tick = sr & ~TIMER_CB;
} else {
lp->media = SPD_DET;
lp->local_state = 0;
if (sr) {
lp->tmp = MII_SR_ASSC;
anlpa = mii_rd(MII_ANLPA, lp->phy[lp->active].addr, DE4X5_MII);
ana = mii_rd(MII_ANA, lp->phy[lp->active].addr, DE4X5_MII);
if (!(anlpa & MII_ANLPA_RF) &&
(cap = anlpa & MII_ANLPA_TAF & ana)) {
if (cap & MII_ANA_100M) {
lp->fdx = ((ana & anlpa & MII_ANA_FDAM & MII_ANA_100M) ? TRUE : FALSE);
lp->media = _100Mb;
} else if (cap & MII_ANA_10M) {
lp->fdx = ((ana & anlpa & MII_ANA_FDAM & MII_ANA_10M) ? TRUE : FALSE);
lp->media = _10Mb;
}
}
}
next_tick = dc21140m_autoconf(dev);
}
break;
}
break;
case SPD_DET:
if (lp->timeout < 0) {
lp->tmp = (lp->phy[lp->active].id ? MII_SR_LKS :
(~gep_rd(dev) & GEP_LNP));
SET_100Mb_PDET;
}
if ((slnk = test_for_100Mb(dev, 6500)) < 0) {
next_tick = slnk & ~TIMER_CB;
} else {
if (is_spd_100(dev) && is_100_up(dev)) {
lp->media = _100Mb;
} else if ((!is_spd_100(dev) && (is_10_up(dev) & lp->tmp))) {
lp->media = _10Mb;
} else {
lp->media = NC;
}
next_tick = dc21140m_autoconf(dev);
}
break;
case _100Mb:
next_tick = 3000;
if (!lp->tx_enable) {
SET_100Mb;
de4x5_init_connection(dev);
} else {
if (!lp->linkOK && (lp->autosense == AUTO)) {
if (!is_100_up(dev) || (!lp->useSROM && !is_spd_100(dev))) {
lp->media = INIT;
lp->tcount++;
next_tick = DE4X5_AUTOSENSE_MS;
}
}
}
break;
case BNC:
case AUI:
case _10Mb:
next_tick = 3000;
if (!lp->tx_enable) {
SET_10Mb;
de4x5_init_connection(dev);
} else {
if (!lp->linkOK && (lp->autosense == AUTO)) {
if (!is_10_up(dev) || (!lp->useSROM && is_spd_100(dev))) {
lp->media = INIT;
lp->tcount++;
next_tick = DE4X5_AUTOSENSE_MS;
}
}
}
break;
case NC:
if (lp->media != lp->c_media) {
de4x5_dbg_media(dev);
lp->c_media = lp->media;
}
lp->media = INIT;
lp->tx_enable = FALSE;
break;
}
return next_tick;
}
static int
dc2114x_autoconf(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
s32 cr, anlpa, ana, cap, irqs, irq_mask, imr, omr, slnk, sr, sts;
int next_tick = DE4X5_AUTOSENSE_MS;
switch (lp->media) {
case INIT:
if (lp->timeout < 0) {
DISABLE_IRQs;
lp->tx_enable = FALSE;
lp->linkOK = 0;
lp->timeout = -1;
de4x5_save_skbs(dev);
if (lp->params.autosense & ~AUTO) {
srom_map_media(dev);
if (lp->media != lp->params.autosense) {
lp->tcount++;
lp->media = INIT;
return next_tick;
}
lp->media = INIT;
}
}
if ((next_tick = de4x5_reset_phy(dev)) < 0) {
next_tick &= ~TIMER_CB;
} else {
if (lp->autosense == _100Mb) {
lp->media = _100Mb;
} else if (lp->autosense == _10Mb) {
lp->media = _10Mb;
} else if (lp->autosense == TP) {
lp->media = TP;
} else if (lp->autosense == BNC) {
lp->media = BNC;
} else if (lp->autosense == AUI) {
lp->media = AUI;
} else {
lp->media = SPD_DET;
if ((lp->infoblock_media == ANS) &&
((sr=is_anc_capable(dev)) & MII_SR_ANC)) {
ana = (((sr >> 6) & MII_ANA_TAF) | MII_ANA_CSMA);
ana &= (lp->fdx ? ~0 : ~MII_ANA_FDAM);
mii_wr(ana, MII_ANA, lp->phy[lp->active].addr, DE4X5_MII);
lp->media = ANS;
}
}
lp->local_state = 0;
next_tick = dc2114x_autoconf(dev);
}
break;
case ANS:
switch (lp->local_state) {
case 0:
if (lp->timeout < 0) {
mii_wr(MII_CR_ASSE | MII_CR_RAN, MII_CR, lp->phy[lp->active].addr, DE4X5_MII);
}
cr = test_mii_reg(dev, MII_CR, MII_CR_RAN, FALSE, 500);
if (cr < 0) {
next_tick = cr & ~TIMER_CB;
} else {
if (cr) {
lp->local_state = 0;
lp->media = SPD_DET;
} else {
lp->local_state++;
}
next_tick = dc2114x_autoconf(dev);
}
break;
case 1:
if ((sr=test_mii_reg(dev, MII_SR, MII_SR_ASSC, TRUE, 2000)) < 0) {
next_tick = sr & ~TIMER_CB;
} else {
lp->media = SPD_DET;
lp->local_state = 0;
if (sr) {
lp->tmp = MII_SR_ASSC;
anlpa = mii_rd(MII_ANLPA, lp->phy[lp->active].addr, DE4X5_MII);
ana = mii_rd(MII_ANA, lp->phy[lp->active].addr, DE4X5_MII);
if (!(anlpa & MII_ANLPA_RF) &&
(cap = anlpa & MII_ANLPA_TAF & ana)) {
if (cap & MII_ANA_100M) {
lp->fdx = ((ana & anlpa & MII_ANA_FDAM & MII_ANA_100M) ? TRUE : FALSE);
lp->media = _100Mb;
} else if (cap & MII_ANA_10M) {
lp->fdx = ((ana & anlpa & MII_ANA_FDAM & MII_ANA_10M) ? TRUE : FALSE);
lp->media = _10Mb;
}
}
}
next_tick = dc2114x_autoconf(dev);
}
break;
}
break;
case AUI:
if (!lp->tx_enable) {
if (lp->timeout < 0) {
omr = inl(DE4X5_OMR);
outl(omr & ~OMR_FDX, DE4X5_OMR);
}
irqs = 0;
irq_mask = 0;
sts = test_media(dev,irqs, irq_mask, 0, 0, 0, 1000);
if (sts < 0) {
next_tick = sts & ~TIMER_CB;
} else {
if (!(inl(DE4X5_SISR) & SISR_SRA) && (lp->autosense == AUTO)) {
lp->media = BNC;
next_tick = dc2114x_autoconf(dev);
} else {
lp->local_state = 1;
de4x5_init_connection(dev);
}
}
} else if (!lp->linkOK && (lp->autosense == AUTO)) {
lp->media = AUI_SUSPECT;
next_tick = 3000;
}
break;
case AUI_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, AUI, ping_media, dc2114x_autoconf);
break;
case BNC:
switch (lp->local_state) {
case 0:
if (lp->timeout < 0) {
omr = inl(DE4X5_OMR);
outl(omr & ~OMR_FDX, DE4X5_OMR);
}
irqs = 0;
irq_mask = 0;
sts = test_media(dev,irqs, irq_mask, 0, 0, 0, 1000);
if (sts < 0) {
next_tick = sts & ~TIMER_CB;
} else {
lp->local_state++;
next_tick = dc2114x_autoconf(dev);
}
break;
case 1:
if (!lp->tx_enable) {
if ((sts = ping_media(dev, 3000)) < 0) {
next_tick = sts & ~TIMER_CB;
} else {
if (sts) {
lp->local_state = 0;
lp->tcount++;
lp->media = INIT;
} else {
de4x5_init_connection(dev);
}
}
} else if (!lp->linkOK && (lp->autosense == AUTO)) {
lp->media = BNC_SUSPECT;
next_tick = 3000;
}
break;
}
break;
case BNC_SUSPECT:
next_tick = de4x5_suspect_state(dev, 1000, BNC, ping_media, dc2114x_autoconf);
break;
case SPD_DET:
if (srom_map_media(dev) < 0) {
lp->tcount++;
lp->media = INIT;
return next_tick;
}
if (lp->media == _100Mb) {
if ((slnk = test_for_100Mb(dev, 6500)) < 0) {
lp->media = SPD_DET;
return (slnk & ~TIMER_CB);
}
} else {
if (wait_for_link(dev) < 0) {
lp->media = SPD_DET;
return PDET_LINK_WAIT;
}
}
if (lp->media == ANS) {
if (is_spd_100(dev)) {
lp->media = _100Mb;
} else {
lp->media = _10Mb;
}
next_tick = dc2114x_autoconf(dev);
} else if (((lp->media == _100Mb) && is_100_up(dev)) ||
(((lp->media == _10Mb) || (lp->media == TP) ||
(lp->media == BNC) || (lp->media == AUI)) &&
is_10_up(dev))) {
next_tick = dc2114x_autoconf(dev);
} else {
lp->tcount++;
lp->media = INIT;
}
break;
case _10Mb:
next_tick = 3000;
if (!lp->tx_enable) {
SET_10Mb;
de4x5_init_connection(dev);
} else {
if (!lp->linkOK && (lp->autosense == AUTO)) {
if (!is_10_up(dev) || (!lp->useSROM && is_spd_100(dev))) {
lp->media = INIT;
lp->tcount++;
next_tick = DE4X5_AUTOSENSE_MS;
}
}
}
break;
case _100Mb:
next_tick = 3000;
if (!lp->tx_enable) {
SET_100Mb;
de4x5_init_connection(dev);
} else {
if (!lp->linkOK && (lp->autosense == AUTO)) {
if (!is_100_up(dev) || (!lp->useSROM && !is_spd_100(dev))) {
lp->media = INIT;
lp->tcount++;
next_tick = DE4X5_AUTOSENSE_MS;
}
}
}
break;
default:
lp->tcount++;
printk("Huh?: media:%02x\n", lp->media);
lp->media = INIT;
break;
}
return next_tick;
}
static int
srom_autoconf(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
return lp->infoleaf_fn(dev);
}
static int
srom_map_media(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
lp->fdx = 0;
if (lp->infoblock_media == lp->media)
return 0;
switch(lp->infoblock_media) {
case SROM_10BASETF:
if (!lp->params.fdx) return -1;
lp->fdx = TRUE;
case SROM_10BASET:
if (lp->params.fdx && !lp->fdx) return -1;
if ((lp->chipset == DC21140) || ((lp->chipset & ~0x00ff) == DC2114x)) {
lp->media = _10Mb;
} else {
lp->media = TP;
}
break;
case SROM_10BASE2:
lp->media = BNC;
break;
case SROM_10BASE5:
lp->media = AUI;
break;
case SROM_100BASETF:
if (!lp->params.fdx) return -1;
lp->fdx = TRUE;
case SROM_100BASET:
if (lp->params.fdx && !lp->fdx) return -1;
lp->media = _100Mb;
break;
case SROM_100BASET4:
lp->media = _100Mb;
break;
case SROM_100BASEFF:
if (!lp->params.fdx) return -1;
lp->fdx = TRUE;
case SROM_100BASEF:
if (lp->params.fdx && !lp->fdx) return -1;
lp->media = _100Mb;
break;
case ANS:
lp->media = ANS;
break;
default:
printk("%s: Bad media code [%d] detected in SROM!\n", dev->name,
lp->infoblock_media);
return -1;
break;
}
return 0;
}
static void
de4x5_init_connection(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if (lp->media != lp->c_media) {
de4x5_dbg_media(dev);
lp->c_media = lp->media;
}
cli();
de4x5_rst_desc_ring(dev);
de4x5_setup_intr(dev);
lp->tx_enable = YES;
dev->tbusy = 0;
sti();
outl(POLL_DEMAND, DE4X5_TPD);
mark_bh(NET_BH);
return;
}
static int
de4x5_reset_phy(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int next_tick = 0;
if ((lp->useSROM) || (lp->phy[lp->active].id)) {
if (lp->timeout < 0) {
if (lp->useSROM) {
if (lp->phy[lp->active].rst) {
srom_exec(dev, lp->phy[lp->active].rst);
srom_exec(dev, lp->phy[lp->active].rst);
} else if (lp->rst) {
srom_exec(dev, lp->rst);
srom_exec(dev, lp->rst);
}
} else {
PHY_HARD_RESET;
}
if (lp->useMII) {
mii_wr(MII_CR_RST, MII_CR, lp->phy[lp->active].addr, DE4X5_MII);
}
}
if (lp->useMII) {
next_tick = test_mii_reg(dev, MII_CR, MII_CR_RST, FALSE, 500);
}
} else if (lp->chipset == DC21140) {
PHY_HARD_RESET;
}
return next_tick;
}
static int
test_media(struct device *dev, s32 irqs, s32 irq_mask, s32 csr13, s32 csr14, s32 csr15, s32 msec)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
s32 sts, csr12;
if (lp->timeout < 0) {
lp->timeout = msec/100;
if (!lp->useSROM) {
reset_init_sia(dev, csr13, csr14, csr15);
}
outl(irq_mask, DE4X5_IMR);
sts = inl(DE4X5_STS);
outl(sts, DE4X5_STS);
if ((lp->chipset == DC21041) || lp->useSROM) {
csr12 = inl(DE4X5_SISR);
outl(csr12, DE4X5_SISR);
}
}
sts = inl(DE4X5_STS) & ~TIMER_CB;
if (!(sts & irqs) && --lp->timeout) {
sts = 100 | TIMER_CB;
} else {
lp->timeout = -1;
}
return sts;
}
static int
test_tp(struct device *dev, s32 msec)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int sisr;
if (lp->timeout < 0) {
lp->timeout = msec/100;
}
sisr = (inl(DE4X5_SISR) & ~TIMER_CB) & (SISR_LKF | SISR_NCR);
if (sisr && --lp->timeout) {
sisr = 100 | TIMER_CB;
} else {
lp->timeout = -1;
}
return sisr;
}
#define SAMPLE_INTERVAL 500
#define SAMPLE_DELAY 2000
static int
test_for_100Mb(struct device *dev, int msec)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int gep = 0, ret = ((lp->chipset & ~0x00ff)==DC2114x? -1 :GEP_SLNK);
if (lp->timeout < 0) {
if ((msec/SAMPLE_INTERVAL) <= 0) return 0;
if (msec > SAMPLE_DELAY) {
lp->timeout = (msec - SAMPLE_DELAY)/SAMPLE_INTERVAL;
gep = SAMPLE_DELAY | TIMER_CB;
return gep;
} else {
lp->timeout = msec/SAMPLE_INTERVAL;
}
}
if (lp->phy[lp->active].id || lp->useSROM) {
gep = is_100_up(dev) | is_spd_100(dev);
} else {
gep = (~gep_rd(dev) & (GEP_SLNK | GEP_LNP));
}
if (!(gep & ret) && --lp->timeout) {
gep = SAMPLE_INTERVAL | TIMER_CB;
} else {
lp->timeout = -1;
}
return gep;
}
static int
wait_for_link(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
if (lp->timeout < 0) {
lp->timeout = 1;
}
if (lp->timeout--) {
return TIMER_CB;
} else {
lp->timeout = -1;
}
return 0;
}
static int
test_mii_reg(struct device *dev, int reg, int mask, int pol, long msec)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int test;
u_long iobase = dev->base_addr;
if (lp->timeout < 0) {
lp->timeout = msec/100;
}
if (pol) pol = ~0;
reg = mii_rd((u_char)reg, lp->phy[lp->active].addr, DE4X5_MII) & mask;
test = (reg ^ pol) & mask;
if (test && --lp->timeout) {
reg = 100 | TIMER_CB;
} else {
lp->timeout = -1;
}
return reg;
}
static int
is_spd_100(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int spd;
if (lp->useMII) {
spd = mii_rd(lp->phy[lp->active].spd.reg, lp->phy[lp->active].addr, DE4X5_MII);
spd = ~(spd ^ lp->phy[lp->active].spd.value);
spd &= lp->phy[lp->active].spd.mask;
} else if (!lp->useSROM) {
spd = ((~gep_rd(dev)) & GEP_SLNK);
} else {
if ((lp->ibn == 2) || !lp->asBitValid)
return ((lp->chipset == DC21143)?(~inl(DE4X5_SISR)&SISR_LS100):0);
spd = (lp->asBitValid & (lp->asPolarity ^ (gep_rd(dev) & lp->asBit))) |
(lp->linkOK & ~lp->asBitValid);
}
return spd;
}
static int
is_100_up(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if (lp->useMII) {
mii_rd(MII_SR, lp->phy[lp->active].addr, DE4X5_MII);
return (mii_rd(MII_SR, lp->phy[lp->active].addr, DE4X5_MII) & MII_SR_LKS);
} else if (!lp->useSROM) {
return ((~gep_rd(dev)) & GEP_SLNK);
} else {
if ((lp->ibn == 2) || !lp->asBitValid)
return ((lp->chipset == DC21143)?(~inl(DE4X5_SISR)&SISR_LS100):0);
return ((lp->asBitValid&(lp->asPolarity^(gep_rd(dev)&lp->asBit))) |
(lp->linkOK & ~lp->asBitValid));
}
}
static int
is_10_up(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if (lp->useMII) {
mii_rd(MII_SR, lp->phy[lp->active].addr, DE4X5_MII);
return (mii_rd(MII_SR, lp->phy[lp->active].addr, DE4X5_MII) & MII_SR_LKS);
} else if (!lp->useSROM) {
return ((~gep_rd(dev)) & GEP_LNP);
} else {
if ((lp->ibn == 2) || !lp->asBitValid)
return (((lp->chipset & ~0x00ff) == DC2114x) ?
(~inl(DE4X5_SISR)&SISR_LS10):
0);
return ((lp->asBitValid&(lp->asPolarity^(gep_rd(dev)&lp->asBit))) |
(lp->linkOK & ~lp->asBitValid));
}
}
static int
is_anc_capable(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if (lp->phy[lp->active].id && (!lp->useSROM || lp->useMII)) {
return (mii_rd(MII_SR, lp->phy[lp->active].addr, DE4X5_MII));
} else if ((lp->chipset & ~0x00ff) == DC2114x) {
return (inl(DE4X5_SISR) & SISR_LPN) >> 12;
} else {
return 0;
}
}
static int
ping_media(struct device *dev, int msec)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int sisr;
if (lp->timeout < 0) {
lp->timeout = msec/100;
lp->tmp = lp->tx_new;
load_packet(dev, lp->frame, TD_LS | TD_FS | sizeof(lp->frame), NULL);
lp->tx_new = (lp->tx_new + 1) % lp->txRingSize;
outl(POLL_DEMAND, DE4X5_TPD);
}
sisr = inl(DE4X5_SISR);
if ((!(sisr & SISR_NCR)) &&
((s32)le32_to_cpu(lp->tx_ring[lp->tmp].status) < 0) &&
(--lp->timeout)) {
sisr = 100 | TIMER_CB;
} else {
if ((!(sisr & SISR_NCR)) &&
!(le32_to_cpu(lp->tx_ring[lp->tmp].status) & (T_OWN | TD_ES)) &&
lp->timeout) {
sisr = 0;
} else {
sisr = 1;
}
lp->timeout = -1;
}
return sisr;
}
static struct sk_buff *
de4x5_alloc_rx_buff(struct device *dev, int index, int len)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
struct sk_buff *p;
#if !defined(__alpha__) && !defined(__powerpc__) && !defined(__sparc_v9__) && !defined(DE4X5_DO_MEMCPY)
struct sk_buff *ret;
u_long i=0, tmp;
p = dev_alloc_skb(IEEE802_3_SZ + ALIGN + 2);
if (!p) return NULL;
p->dev = dev;
tmp = virt_to_bus(p->data);
i = ((tmp + ALIGN) & ~ALIGN) - tmp;
skb_reserve(p, i);
lp->rx_ring[index].buf = tmp + i;
ret = lp->rx_skb[index];
lp->rx_skb[index] = p;
if ((u_long) ret > 1) {
skb_put(ret, len);
}
return ret;
#else
if (lp->state != OPEN) return (struct sk_buff *)1;
p = dev_alloc_skb(len + 2);
if (!p) return NULL;
p->dev = dev;
skb_reserve(p, 2);
if (index < lp->rx_old) {
short tlen = (lp->rxRingSize - lp->rx_old) * RX_BUFF_SZ;
memcpy(skb_put(p,tlen),
bus_to_virt(le32_to_cpu(lp->rx_ring[lp->rx_old].buf)),tlen);
memcpy(skb_put(p,len-tlen),
bus_to_virt(le32_to_cpu(lp->rx_ring[0].buf)), len-tlen);
} else {
memcpy(skb_put(p,len),
bus_to_virt(le32_to_cpu(lp->rx_ring[lp->rx_old].buf)),len);
}
return p;
#endif
}
static void
de4x5_free_rx_buffs(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int i;
for (i=0; i<lp->rxRingSize; i++) {
if ((u_long) lp->rx_skb[i] > 1) {
dev_kfree_skb(lp->rx_skb[i], FREE_WRITE);
}
lp->rx_ring[i].status = 0;
lp->rx_skb[i] = (struct sk_buff *)1;
}
return;
}
static void
de4x5_free_tx_buffs(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int i;
for (i=0; i<lp->txRingSize; i++) {
if (lp->tx_skb[i]) {
dev_kfree_skb(lp->tx_skb[i], FREE_WRITE);
lp->tx_skb[i] = NULL;
}
lp->tx_ring[i].status = 0;
}
while (lp->cache.skb) {
dev_kfree_skb(de4x5_get_cache(dev), FREE_WRITE);
}
return;
}
static void
de4x5_save_skbs(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
s32 omr;
if (!lp->cache.save_cnt) {
STOP_DE4X5;
de4x5_tx(dev);
de4x5_free_tx_buffs(dev);
de4x5_cache_state(dev, DE4X5_SAVE_STATE);
de4x5_sw_reset(dev);
de4x5_cache_state(dev, DE4X5_RESTORE_STATE);
lp->cache.save_cnt++;
START_DE4X5;
}
return;
}
static void
de4x5_rst_desc_ring(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int i;
s32 omr;
if (lp->cache.save_cnt) {
STOP_DE4X5;
outl(virt_to_bus(lp->rx_ring), DE4X5_RRBA);
outl(virt_to_bus(lp->tx_ring), DE4X5_TRBA);
lp->rx_new = lp->rx_old = 0;
lp->tx_new = lp->tx_old = 0;
for (i = 0; i < lp->rxRingSize; i++) {
lp->rx_ring[i].status = cpu_to_le32(R_OWN);
}
for (i = 0; i < lp->txRingSize; i++) {
lp->tx_ring[i].status = cpu_to_le32(0);
}
barrier();
lp->cache.save_cnt--;
START_DE4X5;
}
return;
}
static void
de4x5_cache_state(struct device *dev, int flag)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
switch(flag) {
case DE4X5_SAVE_STATE:
lp->cache.csr0 = inl(DE4X5_BMR);
lp->cache.csr6 = (inl(DE4X5_OMR) & ~(OMR_ST | OMR_SR));
lp->cache.csr7 = inl(DE4X5_IMR);
break;
case DE4X5_RESTORE_STATE:
outl(lp->cache.csr0, DE4X5_BMR);
outl(lp->cache.csr6, DE4X5_OMR);
outl(lp->cache.csr7, DE4X5_IMR);
if (lp->chipset == DC21140) {
gep_wr(lp->cache.gepc, dev);
gep_wr(lp->cache.gep, dev);
} else {
reset_init_sia(dev, lp->cache.csr13, lp->cache.csr14,
lp->cache.csr15);
}
break;
}
return;
}
static void
de4x5_put_cache(struct device *dev, struct sk_buff *skb)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
struct sk_buff *p;
if (lp->cache.skb) {
for (p=lp->cache.skb; p->next; p=p->next);
p->next = skb;
} else {
lp->cache.skb = skb;
}
skb->next = NULL;
return;
}
static void
de4x5_putb_cache(struct device *dev, struct sk_buff *skb)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
struct sk_buff *p = lp->cache.skb;
lp->cache.skb = skb;
skb->next = p;
return;
}
static struct sk_buff *
de4x5_get_cache(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
struct sk_buff *p = lp->cache.skb;
if (p) {
lp->cache.skb = p->next;
p->next = NULL;
}
return p;
}
static int
test_ans(struct device *dev, s32 irqs, s32 irq_mask, s32 msec)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
s32 sts, ans;
if (lp->timeout < 0) {
lp->timeout = msec/100;
outl(irq_mask, DE4X5_IMR);
sts = inl(DE4X5_STS);
outl(sts, DE4X5_STS);
}
ans = inl(DE4X5_SISR) & SISR_ANS;
sts = inl(DE4X5_STS) & ~TIMER_CB;
if (!(sts & irqs) && (ans ^ ANS_NWOK) && --lp->timeout) {
sts = 100 | TIMER_CB;
} else {
lp->timeout = -1;
}
return sts;
}
static void
de4x5_setup_intr(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
s32 imr, sts;
if (inl(DE4X5_OMR) & OMR_SR) {
imr = 0;
UNMASK_IRQs;
sts = inl(DE4X5_STS);
outl(sts, DE4X5_STS);
ENABLE_IRQs;
}
return;
}
static void
reset_init_sia(struct device *dev, s32 csr13, s32 csr14, s32 csr15)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
RESET_SIA;
if (lp->useSROM) {
if (lp->ibn == 3) {
srom_exec(dev, lp->phy[lp->active].rst);
srom_exec(dev, lp->phy[lp->active].gep);
outl(1, DE4X5_SICR);
return;
} else {
csr15 = lp->cache.csr15;
csr14 = lp->cache.csr14;
csr13 = lp->cache.csr13;
outl(csr15 | lp->cache.gepc, DE4X5_SIGR);
outl(csr15 | lp->cache.gep, DE4X5_SIGR);
}
} else {
outl(csr15, DE4X5_SIGR);
}
outl(csr14, DE4X5_STRR);
outl(csr13, DE4X5_SICR);
de4x5_ms_delay(10);
return;
}
static void
create_packet(struct device *dev, char *frame, int len)
{
int i;
char *buf = frame;
for (i=0; i<ETH_ALEN; i++) {
*buf++ = dev->dev_addr[i];
}
for (i=0; i<ETH_ALEN; i++) {
*buf++ = dev->dev_addr[i];
}
*buf++ = 0;
*buf++ = 1;
return;
}
static void
de4x5_us_delay(u32 usec)
{
udelay(usec);
return;
}
static void
de4x5_ms_delay(u32 msec)
{
u_int i;
for (i=0; i<msec; i++) {
de4x5_us_delay(1000);
}
return;
}
static int
EISA_signature(char *name, s32 eisa_id)
{
static c_char *signatures[] = DE4X5_SIGNATURE;
char ManCode[DE4X5_STRLEN];
union {
s32 ID;
char Id[4];
} Eisa;
int i, status = 0, siglen = sizeof(signatures)/sizeof(c_char *);
*name = '\0';
Eisa.ID = inl(eisa_id);
ManCode[0]=(((Eisa.Id[0]>>2)&0x1f)+0x40);
ManCode[1]=(((Eisa.Id[1]&0xe0)>>5)+((Eisa.Id[0]&0x03)<<3)+0x40);
ManCode[2]=(((Eisa.Id[2]>>4)&0x0f)+0x30);
ManCode[3]=((Eisa.Id[2]&0x0f)+0x30);
ManCode[4]=(((Eisa.Id[3]>>4)&0x0f)+0x30);
ManCode[5]='\0';
for (i=0;i<siglen;i++) {
if (strstr(ManCode, signatures[i]) != NULL) {
strcpy(name,ManCode);
status = 1;
break;
}
}
return status;
}
static int
PCI_signature(char *name, struct bus_type *lp)
{
static c_char *de4x5_signatures[] = DE4X5_SIGNATURE;
int i, status = 0, siglen = sizeof(de4x5_signatures)/sizeof(c_char *);
if (lp->chipset == DC21040) {
strcpy(name, "DE434/5");
return status;
} else {
int i = *((char *)&lp->srom + 19) * 3;
strncpy(name, (char *)&lp->srom + 26 + i, 8);
}
name[8] = '\0';
for (i=0; i<siglen; i++) {
if (strstr(name,de4x5_signatures[i])!=NULL) break;
}
if (i == siglen) {
if (dec_only) {
*name = '\0';
} else {
strcpy(name, (((lp->chipset == DC21040) ? "DC21040" :
((lp->chipset == DC21041) ? "DC21041" :
((lp->chipset == DC21140) ? "DC21140" :
((lp->chipset == DC21142) ? "DC21142" :
((lp->chipset == DC21143) ? "DC21143" : "UNKNOWN"
)))))));
}
if (lp->chipset != DC21041) {
useSROM = TRUE;
}
} else if ((lp->chipset & ~0x00ff) == DC2114x) {
useSROM = TRUE;
}
return status;
}
static void
DevicePresent(u_long aprom_addr)
{
int i, j=0;
struct bus_type *lp = &bus;
if (lp->chipset == DC21040) {
if (lp->bus == EISA) {
enet_addr_rst(aprom_addr);
} else {
outl(0, aprom_addr);
}
} else {
u_short tmp, *p = (short *)((char *)&lp->srom + SROM_HWADD);
for (i=0; i<(ETH_ALEN>>1); i++) {
tmp = srom_rd(aprom_addr, (SROM_HWADD>>1) + i);
*p = le16_to_cpu(tmp);
j += *p++;
}
if ((j == 0) || (j == 0x2fffd)) {
return;
}
p=(short *)&lp->srom;
for (i=0; i<(sizeof(struct de4x5_srom)>>1); i++) {
tmp = srom_rd(aprom_addr, i);
*p++ = le16_to_cpu(tmp);
}
de4x5_dbg_srom((struct de4x5_srom *)&lp->srom);
}
return;
}
static void
enet_addr_rst(u_long aprom_addr)
{
union {
struct {
u32 a;
u32 b;
} llsig;
char Sig[sizeof(u32) << 1];
} dev;
short sigLength=0;
s8 data;
int i, j;
dev.llsig.a = ETH_PROM_SIG;
dev.llsig.b = ETH_PROM_SIG;
sigLength = sizeof(u32) << 1;
for (i=0,j=0;j<sigLength && i<PROBE_LENGTH+sigLength-1;i++) {
data = inb(aprom_addr);
if (dev.Sig[j] == data) {
j++;
} else {
if (data == dev.Sig[0]) {
j=1;
} else {
j=0;
}
}
}
return;
}
static int
get_hw_addr(struct device *dev)
{
u_long iobase = dev->base_addr;
int broken, i, k, tmp, status = 0;
u_short j,chksum;
struct bus_type *lp = &bus;
broken = de4x5_bad_srom(lp);
for (i=0,k=0,j=0;j<3;j++) {
k <<= 1;
if (k > 0xffff) k-=0xffff;
if (lp->bus == PCI) {
if (lp->chipset == DC21040) {
while ((tmp = inl(DE4X5_APROM)) < 0);
k += (u_char) tmp;
dev->dev_addr[i++] = (u_char) tmp;
while ((tmp = inl(DE4X5_APROM)) < 0);
k += (u_short) (tmp << 8);
dev->dev_addr[i++] = (u_char) tmp;
} else if (!broken) {
dev->dev_addr[i] = (u_char) lp->srom.ieee_addr[i]; i++;
dev->dev_addr[i] = (u_char) lp->srom.ieee_addr[i]; i++;
} else if ((broken == SMC) || (broken == ACCTON)) {
dev->dev_addr[i] = *((u_char *)&lp->srom + i); i++;
dev->dev_addr[i] = *((u_char *)&lp->srom + i); i++;
}
} else {
k += (u_char) (tmp = inb(EISA_APROM));
dev->dev_addr[i++] = (u_char) tmp;
k += (u_short) ((tmp = inb(EISA_APROM)) << 8);
dev->dev_addr[i++] = (u_char) tmp;
}
if (k > 0xffff) k-=0xffff;
}
if (k == 0xffff) k=0;
if (lp->bus == PCI) {
if (lp->chipset == DC21040) {
while ((tmp = inl(DE4X5_APROM)) < 0);
chksum = (u_char) tmp;
while ((tmp = inl(DE4X5_APROM)) < 0);
chksum |= (u_short) (tmp << 8);
if ((k != chksum) && (dec_only)) status = -1;
}
} else {
chksum = (u_char) inb(EISA_APROM);
chksum |= (u_short) (inb(EISA_APROM) << 8);
if ((k != chksum) && (dec_only)) status = -1;
}
srom_repair(dev, broken);
#ifdef CONFIG_PMAC
if (dev->dev_addr[0] == 0 && dev->dev_addr[1] == 0xa0) {
for (i = 0; i < ETH_ALEN; ++i) {
int x = dev->dev_addr[i];
x = ((x & 0xf) << 4) + ((x & 0xf0) >> 4);
x = ((x & 0x33) << 2) + ((x & 0xcc) >> 2);
dev->dev_addr[i] = ((x & 0x55) << 1) + ((x & 0xaa) >> 1);
}
}
#endif
status = test_bad_enet(dev, status);
return status;
}
static int
de4x5_bad_srom(struct bus_type *lp)
{
int i, status = 0;
for (i=0; i<sizeof(enet_det)/ETH_ALEN; i++) {
if (!de4x5_strncmp((char *)&lp->srom, (char *)&enet_det[i], 3) &&
!de4x5_strncmp((char *)&lp->srom+0x10, (char *)&enet_det[i], 3)) {
if (i == 0) {
status = SMC;
} else if (i == 1) {
status = ACCTON;
}
break;
}
}
return status;
}
static int
de4x5_strncmp(char *a, char *b, int n)
{
int ret=0;
for (;n && !ret;n--) {
ret = *a++ - *b++;
}
return ret;
}
static void
srom_repair(struct device *dev, int card)
{
struct bus_type *lp = &bus;
switch(card) {
case SMC:
memset((char *)&bus.srom, 0, sizeof(struct de4x5_srom));
memcpy(lp->srom.ieee_addr, (char *)dev->dev_addr, ETH_ALEN);
memcpy(lp->srom.info, (char *)&srom_repair_info[SMC-1], 100);
useSROM = TRUE;
break;
}
return;
}
static int
test_bad_enet(struct device *dev, int status)
{
struct bus_type *lp = &bus;
int i, tmp;
for (tmp=0,i=0; i<ETH_ALEN; i++) tmp += (u_char)dev->dev_addr[i];
if ((tmp == 0) || (tmp == 0x5fa)) {
if ((lp->chipset == last.chipset) &&
(lp->bus_num == last.bus) && (lp->bus_num > 0)) {
for (i=0; i<ETH_ALEN; i++) dev->dev_addr[i] = last.addr[i];
for (i=ETH_ALEN-1; i>2; --i) {
dev->dev_addr[i] += 1;
if (dev->dev_addr[i] != 0) break;
}
for (i=0; i<ETH_ALEN; i++) last.addr[i] = dev->dev_addr[i];
if (!an_exception(lp)) {
dev->irq = last.irq;
}
status = 0;
}
} else if (!status) {
last.chipset = lp->chipset;
last.bus = lp->bus_num;
last.irq = dev->irq;
for (i=0; i<ETH_ALEN; i++) last.addr[i] = dev->dev_addr[i];
}
return status;
}
static int
an_exception(struct bus_type *lp)
{
if ((*(u_short *)lp->srom.sub_vendor_id == 0x00c0) &&
(*(u_short *)lp->srom.sub_system_id == 0x95e0)) {
return -1;
}
return 0;
}
static short
srom_rd(u_long addr, u_char offset)
{
sendto_srom(SROM_RD | SROM_SR, addr);
srom_latch(SROM_RD | SROM_SR | DT_CS, addr);
srom_command(SROM_RD | SROM_SR | DT_IN | DT_CS, addr);
srom_address(SROM_RD | SROM_SR | DT_CS, addr, offset);
return srom_data(SROM_RD | SROM_SR | DT_CS, addr);
}
static void
srom_latch(u_int command, u_long addr)
{
sendto_srom(command, addr);
sendto_srom(command | DT_CLK, addr);
sendto_srom(command, addr);
return;
}
static void
srom_command(u_int command, u_long addr)
{
srom_latch(command, addr);
srom_latch(command, addr);
srom_latch((command & 0x0000ff00) | DT_CS, addr);
return;
}
static void
srom_address(u_int command, u_long addr, u_char offset)
{
int i;
char a;
a = (char)(offset << 2);
for (i=0; i<6; i++, a <<= 1) {
srom_latch(command | ((a < 0) ? DT_IN : 0), addr);
}
de4x5_us_delay(1);
i = (getfrom_srom(addr) >> 3) & 0x01;
return;
}
static short
srom_data(u_int command, u_long addr)
{
int i;
short word = 0;
s32 tmp;
for (i=0; i<16; i++) {
sendto_srom(command | DT_CLK, addr);
tmp = getfrom_srom(addr);
sendto_srom(command, addr);
word = (word << 1) | ((tmp >> 3) & 0x01);
}
sendto_srom(command & 0x0000ff00, addr);
return word;
}
static void
sendto_srom(u_int command, u_long addr)
{
outl(command, addr);
udelay(1);
return;
}
static int
getfrom_srom(u_long addr)
{
s32 tmp;
tmp = inl(addr);
udelay(1);
return tmp;
}
static int
srom_infoleaf_info(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int i, count;
u_char *p;
for (i=0; i<INFOLEAF_SIZE; i++) {
if (lp->chipset == infoleaf_array[i].chipset) break;
}
if (i == INFOLEAF_SIZE) {
lp->useSROM = FALSE;
printk("%s: Cannot find correct chipset for SROM decoding!\n",
dev->name);
return -ENXIO;
}
lp->infoleaf_fn = infoleaf_array[i].fn;
count = *((u_char *)&lp->srom + 19);
p = (u_char *)&lp->srom + 26;
if (count > 1) {
for (i=count; i; --i, p+=3) {
if (lp->device == *p) break;
}
if (i == 0) {
lp->useSROM = FALSE;
printk("%s: Cannot find correct PCI device [%d] for SROM decoding!\n",
dev->name, lp->device);
return -ENXIO;
}
}
lp->infoleaf_offset = TWIDDLE(p+1);
return 0;
}
static void
srom_init(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char *p = (u_char *)&lp->srom + lp->infoleaf_offset;
u_char count;
p+=2;
if (lp->chipset == DC21140) {
lp->cache.gepc = (*p++ | GEP_CTRL);
gep_wr(lp->cache.gepc, dev);
}
count = *p++;
for (;count; --count) {
if (*p < 128) {
p += COMPACT_LEN;
} else if (*(p+1) == 5) {
type5_infoblock(dev, 1, p);
p += ((*p & BLOCK_LEN) + 1);
} else if (*(p+1) == 4) {
p += ((*p & BLOCK_LEN) + 1);
} else if (*(p+1) == 3) {
type3_infoblock(dev, 1, p);
p += ((*p & BLOCK_LEN) + 1);
} else if (*(p+1) == 2) {
p += ((*p & BLOCK_LEN) + 1);
} else if (*(p+1) == 1) {
type1_infoblock(dev, 1, p);
p += ((*p & BLOCK_LEN) + 1);
} else {
p += ((*p & BLOCK_LEN) + 1);
}
}
return;
}
static void
srom_exec(struct device *dev, u_char *p)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
u_char count = (p ? *p++ : 0);
u_short *w = (u_short *)p;
if (((lp->ibn != 1) && (lp->ibn != 3) && (lp->ibn != 5)) || !count) return;
if (lp->chipset != DC21140) RESET_SIA;
while (count--) {
gep_wr(((lp->chipset==DC21140) && (lp->ibn!=5) ?
*p++ : TWIDDLE(w++)), dev);
udelay(2000);
}
if (lp->chipset != DC21140) {
outl(lp->cache.csr14, DE4X5_STRR);
outl(lp->cache.csr13, DE4X5_SICR);
}
return;
}
static int
dc21041_infoleaf(struct device *dev)
{
return DE4X5_AUTOSENSE_MS;
}
static int
dc21140_infoleaf(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char count = 0;
u_char *p = (u_char *)&lp->srom + lp->infoleaf_offset;
int next_tick = DE4X5_AUTOSENSE_MS;
p+=2;
lp->cache.gepc = (*p++ | GEP_CTRL);
count = *p++;
if (*p < 128) {
next_tick = dc_infoblock[COMPACT](dev, count, p);
} else {
next_tick = dc_infoblock[*(p+1)](dev, count, p);
}
if (lp->tcount == count) {
lp->media = NC;
if (lp->media != lp->c_media) {
de4x5_dbg_media(dev);
lp->c_media = lp->media;
}
lp->media = INIT;
lp->tcount = 0;
lp->tx_enable = FALSE;
}
return next_tick & ~TIMER_CB;
}
static int
dc21142_infoleaf(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char count = 0;
u_char *p = (u_char *)&lp->srom + lp->infoleaf_offset;
int next_tick = DE4X5_AUTOSENSE_MS;
p+=2;
count = *p++;
if (*p < 128) {
next_tick = dc_infoblock[COMPACT](dev, count, p);
} else {
next_tick = dc_infoblock[*(p+1)](dev, count, p);
}
if (lp->tcount == count) {
lp->media = NC;
if (lp->media != lp->c_media) {
de4x5_dbg_media(dev);
lp->c_media = lp->media;
}
lp->media = INIT;
lp->tcount = 0;
lp->tx_enable = FALSE;
}
return next_tick & ~TIMER_CB;
}
static int
dc21143_infoleaf(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char count = 0;
u_char *p = (u_char *)&lp->srom + lp->infoleaf_offset;
int next_tick = DE4X5_AUTOSENSE_MS;
p+=2;
count = *p++;
if (*p < 128) {
next_tick = dc_infoblock[COMPACT](dev, count, p);
} else {
next_tick = dc_infoblock[*(p+1)](dev, count, p);
}
if (lp->tcount == count) {
lp->media = NC;
if (lp->media != lp->c_media) {
de4x5_dbg_media(dev);
lp->c_media = lp->media;
}
lp->media = INIT;
lp->tcount = 0;
lp->tx_enable = FALSE;
}
return next_tick & ~TIMER_CB;
}
static int
compact_infoblock(struct device *dev, u_char count, u_char *p)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char flags, csr6;
if (--count > lp->tcount) {
if (*(p+COMPACT_LEN) < 128) {
return dc_infoblock[COMPACT](dev, count, p+COMPACT_LEN);
} else {
return dc_infoblock[*(p+COMPACT_LEN+1)](dev, count, p+COMPACT_LEN);
}
}
if ((lp->media == INIT) && (lp->timeout < 0)) {
lp->ibn = COMPACT;
lp->active = 0;
gep_wr(lp->cache.gepc, dev);
lp->infoblock_media = (*p++) & COMPACT_MC;
lp->cache.gep = *p++;
csr6 = *p++;
flags = *p++;
lp->asBitValid = (flags & 0x80) ? 0 : -1;
lp->defMedium = (flags & 0x40) ? -1 : 0;
lp->asBit = 1 << ((csr6 >> 1) & 0x07);
lp->asPolarity = ((csr6 & 0x80) ? -1 : 0) & lp->asBit;
lp->infoblock_csr6 = OMR_DEF | ((csr6 & 0x71) << 18);
lp->useMII = FALSE;
de4x5_switch_mac_port(dev);
}
return dc21140m_autoconf(dev);
}
static int
type0_infoblock(struct device *dev, u_char count, u_char *p)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char flags, csr6, len = (*p & BLOCK_LEN)+1;
if (--count > lp->tcount) {
if (*(p+len) < 128) {
return dc_infoblock[COMPACT](dev, count, p+len);
} else {
return dc_infoblock[*(p+len+1)](dev, count, p+len);
}
}
if ((lp->media == INIT) && (lp->timeout < 0)) {
lp->ibn = 0;
lp->active = 0;
gep_wr(lp->cache.gepc, dev);
p+=2;
lp->infoblock_media = (*p++) & BLOCK0_MC;
lp->cache.gep = *p++;
csr6 = *p++;
flags = *p++;
lp->asBitValid = (flags & 0x80) ? 0 : -1;
lp->defMedium = (flags & 0x40) ? -1 : 0;
lp->asBit = 1 << ((csr6 >> 1) & 0x07);
lp->asPolarity = ((csr6 & 0x80) ? -1 : 0) & lp->asBit;
lp->infoblock_csr6 = OMR_DEF | ((csr6 & 0x71) << 18);
lp->useMII = FALSE;
de4x5_switch_mac_port(dev);
}
return dc21140m_autoconf(dev);
}
static int
type1_infoblock(struct device *dev, u_char count, u_char *p)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char len = (*p & BLOCK_LEN)+1;
if (--count > lp->tcount) {
if (*(p+len) < 128) {
return dc_infoblock[COMPACT](dev, count, p+len);
} else {
return dc_infoblock[*(p+len+1)](dev, count, p+len);
}
}
p += 2;
if (lp->state == INITIALISED) {
lp->ibn = 1;
lp->active = *p++;
lp->phy[lp->active].gep = (*p ? p : 0); p += (*p + 1);
lp->phy[lp->active].rst = (*p ? p : 0); p += (*p + 1);
lp->phy[lp->active].mc = TWIDDLE(p); p += 2;
lp->phy[lp->active].ana = TWIDDLE(p); p += 2;
lp->phy[lp->active].fdx = TWIDDLE(p); p += 2;
lp->phy[lp->active].ttm = TWIDDLE(p);
return 0;
} else if ((lp->media == INIT) && (lp->timeout < 0)) {
lp->ibn = 1;
lp->active = *p;
lp->infoblock_csr6 = OMR_MII_100;
lp->useMII = TRUE;
lp->infoblock_media = ANS;
de4x5_switch_mac_port(dev);
}
return dc21140m_autoconf(dev);
}
static int
type2_infoblock(struct device *dev, u_char count, u_char *p)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char len = (*p & BLOCK_LEN)+1;
if (--count > lp->tcount) {
if (*(p+len) < 128) {
return dc_infoblock[COMPACT](dev, count, p+len);
} else {
return dc_infoblock[*(p+len+1)](dev, count, p+len);
}
}
if ((lp->media == INIT) && (lp->timeout < 0)) {
lp->ibn = 2;
lp->active = 0;
p += 2;
lp->infoblock_media = (*p) & MEDIA_CODE;
if ((*p++) & EXT_FIELD) {
lp->cache.csr13 = TWIDDLE(p); p += 2;
lp->cache.csr14 = TWIDDLE(p); p += 2;
lp->cache.csr15 = TWIDDLE(p); p += 2;
} else {
lp->cache.csr13 = CSR13;
lp->cache.csr14 = CSR14;
lp->cache.csr15 = CSR15;
}
lp->cache.gepc = ((s32)(TWIDDLE(p)) << 16); p += 2;
lp->cache.gep = ((s32)(TWIDDLE(p)) << 16);
lp->infoblock_csr6 = OMR_SIA;
lp->useMII = FALSE;
de4x5_switch_mac_port(dev);
}
return dc2114x_autoconf(dev);
}
static int
type3_infoblock(struct device *dev, u_char count, u_char *p)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char len = (*p & BLOCK_LEN)+1;
if (--count > lp->tcount) {
if (*(p+len) < 128) {
return dc_infoblock[COMPACT](dev, count, p+len);
} else {
return dc_infoblock[*(p+len+1)](dev, count, p+len);
}
}
p += 2;
if (lp->state == INITIALISED) {
lp->ibn = 3;
lp->active = *p++;
lp->phy[lp->active].gep = (*p ? p : 0); p += (2 * (*p) + 1);
lp->phy[lp->active].rst = (*p ? p : 0); p += (2 * (*p) + 1);
lp->phy[lp->active].mc = TWIDDLE(p); p += 2;
lp->phy[lp->active].ana = TWIDDLE(p); p += 2;
lp->phy[lp->active].fdx = TWIDDLE(p); p += 2;
lp->phy[lp->active].ttm = TWIDDLE(p); p += 2;
lp->phy[lp->active].mci = *p;
return 0;
} else if ((lp->media == INIT) && (lp->timeout < 0)) {
lp->ibn = 3;
lp->active = *p;
lp->infoblock_csr6 = OMR_MII_100;
lp->useMII = TRUE;
lp->infoblock_media = ANS;
de4x5_switch_mac_port(dev);
}
return dc2114x_autoconf(dev);
}
static int
type4_infoblock(struct device *dev, u_char count, u_char *p)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char flags, csr6, len = (*p & BLOCK_LEN)+1;
if (--count > lp->tcount) {
if (*(p+len) < 128) {
return dc_infoblock[COMPACT](dev, count, p+len);
} else {
return dc_infoblock[*(p+len+1)](dev, count, p+len);
}
}
if ((lp->media == INIT) && (lp->timeout < 0)) {
lp->ibn = 4;
lp->active = 0;
p+=2;
lp->infoblock_media = (*p++) & MEDIA_CODE;
lp->cache.csr13 = CSR13;
lp->cache.csr14 = CSR14;
lp->cache.csr15 = CSR15;
lp->cache.gepc = ((s32)(TWIDDLE(p)) << 16); p += 2;
lp->cache.gep = ((s32)(TWIDDLE(p)) << 16); p += 2;
csr6 = *p++;
flags = *p++;
lp->asBitValid = (flags & 0x80) ? 0 : -1;
lp->defMedium = (flags & 0x40) ? -1 : 0;
lp->asBit = 1 << ((csr6 >> 1) & 0x07);
lp->asPolarity = ((csr6 & 0x80) ? -1 : 0) & lp->asBit;
lp->infoblock_csr6 = OMR_DEF | ((csr6 & 0x71) << 18);
lp->useMII = FALSE;
de4x5_switch_mac_port(dev);
}
return dc2114x_autoconf(dev);
}
static int
type5_infoblock(struct device *dev, u_char count, u_char *p)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_char len = (*p & BLOCK_LEN)+1;
if (--count > lp->tcount) {
if (*(p+len) < 128) {
return dc_infoblock[COMPACT](dev, count, p+len);
} else {
return dc_infoblock[*(p+len+1)](dev, count, p+len);
}
}
if ((lp->state == INITIALISED) || (lp->media == INIT)) {
p+=2;
lp->rst = p;
srom_exec(dev, lp->rst);
}
return DE4X5_AUTOSENSE_MS;
}
static int
mii_rd(u_char phyreg, u_char phyaddr, u_long ioaddr)
{
mii_wdata(MII_PREAMBLE, 2, ioaddr);
mii_wdata(MII_PREAMBLE, 32, ioaddr);
mii_wdata(MII_STRD, 4, ioaddr);
mii_address(phyaddr, ioaddr);
mii_address(phyreg, ioaddr);
mii_ta(MII_STRD, ioaddr);
return mii_rdata(ioaddr);
}
static void
mii_wr(int data, u_char phyreg, u_char phyaddr, u_long ioaddr)
{
mii_wdata(MII_PREAMBLE, 2, ioaddr);
mii_wdata(MII_PREAMBLE, 32, ioaddr);
mii_wdata(MII_STWR, 4, ioaddr);
mii_address(phyaddr, ioaddr);
mii_address(phyreg, ioaddr);
mii_ta(MII_STWR, ioaddr);
data = mii_swap(data, 16);
mii_wdata(data, 16, ioaddr);
return;
}
static int
mii_rdata(u_long ioaddr)
{
int i;
s32 tmp = 0;
for (i=0; i<16; i++) {
tmp <<= 1;
tmp |= getfrom_mii(MII_MRD | MII_RD, ioaddr);
}
return tmp;
}
static void
mii_wdata(int data, int len, u_long ioaddr)
{
int i;
for (i=0; i<len; i++) {
sendto_mii(MII_MWR | MII_WR, data, ioaddr);
data >>= 1;
}
return;
}
static void
mii_address(u_char addr, u_long ioaddr)
{
int i;
addr = mii_swap(addr, 5);
for (i=0; i<5; i++) {
sendto_mii(MII_MWR | MII_WR, addr, ioaddr);
addr >>= 1;
}
return;
}
static void
mii_ta(u_long rw, u_long ioaddr)
{
if (rw == MII_STWR) {
sendto_mii(MII_MWR | MII_WR, 1, ioaddr);
sendto_mii(MII_MWR | MII_WR, 0, ioaddr);
} else {
getfrom_mii(MII_MRD | MII_RD, ioaddr);
}
return;
}
static int
mii_swap(int data, int len)
{
int i, tmp = 0;
for (i=0; i<len; i++) {
tmp <<= 1;
tmp |= (data & 1);
data >>= 1;
}
return tmp;
}
static void
sendto_mii(u32 command, int data, u_long ioaddr)
{
u32 j;
j = (data & 1) << 17;
outl(command | j, ioaddr);
udelay(1);
outl(command | MII_MDC | j, ioaddr);
udelay(1);
return;
}
static int
getfrom_mii(u32 command, u_long ioaddr)
{
outl(command, ioaddr);
udelay(1);
outl(command | MII_MDC, ioaddr);
udelay(1);
return ((inl(ioaddr) >> 19) & 1);
}
static int
mii_get_oui(u_char phyaddr, u_long ioaddr)
{
int r2, r3;
r2 = mii_rd(MII_ID0, phyaddr, ioaddr);
r3 = mii_rd(MII_ID1, phyaddr, ioaddr);
return r2;
}
static int
mii_get_phy(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
int i, j, k, n, limit=sizeof(phy_info)/sizeof(struct phy_table);
int id;
lp->active = 0;
lp->useMII = TRUE;
for (n=0, lp->mii_cnt=0, i=1; !((i==1) && (n==1)); i=(i+1)%DE4X5_MAX_MII) {
lp->phy[lp->active].addr = i;
if (i==0) n++;
while (de4x5_reset_phy(dev)<0) udelay(100);
id = mii_get_oui(i, DE4X5_MII);
if ((id == 0) || (id == 65535)) continue;
for (j=0; j<limit; j++) {
if (id != phy_info[j].id) continue;
for (k=0; lp->phy[k].id && (k < DE4X5_MAX_PHY); k++);
if (k < DE4X5_MAX_PHY) {
memcpy((char *)&lp->phy[k],
(char *)&phy_info[j], sizeof(struct phy_table));
lp->phy[k].addr = i;
lp->mii_cnt++;
lp->active++;
} else {
goto purgatory;
}
break;
}
if ((j == limit) && (i < DE4X5_MAX_MII)) {
for (k=0; lp->phy[k].id && (k < DE4X5_MAX_PHY); k++);
lp->phy[k].addr = i;
lp->phy[k].id = id;
lp->phy[k].spd.reg = GENERIC_REG;
lp->phy[k].spd.mask = GENERIC_MASK;
lp->phy[k].spd.value = GENERIC_VALUE;
lp->mii_cnt++;
lp->active++;
printk("%s: Using generic MII device control. If the board doesn't operate, \nplease mail the following dump to the author:\n", dev->name);
j = de4x5_debug;
de4x5_debug |= DEBUG_MII;
de4x5_dbg_mii(dev, k);
de4x5_debug = j;
printk("\n");
}
}
purgatory:
lp->active = 0;
if (lp->phy[0].id) {
for (k=0; lp->phy[k].id && (k < DE4X5_MAX_PHY); k++) {
mii_wr(MII_CR_RST, MII_CR, lp->phy[k].addr, DE4X5_MII);
while (mii_rd(MII_CR, lp->phy[k].addr, DE4X5_MII) & MII_CR_RST);
de4x5_dbg_mii(dev, k);
}
}
if (!lp->mii_cnt) lp->useMII = FALSE;
return lp->mii_cnt;
}
static char *
build_setup_frame(struct device *dev, int mode)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int i;
char *pa = lp->setup_frame;
if (mode == ALL) {
memset(lp->setup_frame, 0, SETUP_FRAME_LEN);
}
if (lp->setup_f == HASH_PERF) {
for (pa=lp->setup_frame+IMPERF_PA_OFFSET, i=0; i<ETH_ALEN; i++) {
*(pa + i) = dev->dev_addr[i];
if (i & 0x01) pa += 2;
}
*(lp->setup_frame + (HASH_TABLE_LEN >> 3) - 3) = 0x80;
} else {
for (i=0; i<ETH_ALEN; i++) {
*(pa + (i&1)) = dev->dev_addr[i];
if (i & 0x01) pa += 4;
}
for (i=0; i<ETH_ALEN; i++) {
*(pa + (i&1)) = (char) 0xff;
if (i & 0x01) pa += 4;
}
}
return pa;
}
static void
enable_ast(struct device *dev, u32 time_out)
{
timeout(dev, (void *)&de4x5_ast, (u_long)dev, time_out);
return;
}
static void
disable_ast(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
del_timer(&lp->timer);
return;
}
static long
de4x5_switch_mac_port(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
s32 omr;
STOP_DE4X5;
omr = (inl(DE4X5_OMR) & ~(OMR_PS | OMR_HBD | OMR_TTM | OMR_PCS | OMR_SCR |
OMR_FDX));
omr |= lp->infoblock_csr6;
if (omr & OMR_PS) omr |= OMR_HBD;
outl(omr, DE4X5_OMR);
RESET_DE4X5;
if (lp->chipset == DC21140) {
gep_wr(lp->cache.gepc, dev);
gep_wr(lp->cache.gep, dev);
} else if ((lp->chipset & ~0x0ff) == DC2114x) {
reset_init_sia(dev, lp->cache.csr13, lp->cache.csr14, lp->cache.csr15);
}
outl(omr, DE4X5_OMR);
inl(DE4X5_MFC);
return omr;
}
static void
gep_wr(s32 data, struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if (lp->chipset == DC21140) {
outl(data, DE4X5_GEP);
} else if ((lp->chipset & ~0x00ff) == DC2114x) {
outl((data<<16) | lp->cache.csr15, DE4X5_SIGR);
}
return;
}
static int
gep_rd(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if (lp->chipset == DC21140) {
return inl(DE4X5_GEP);
} else if ((lp->chipset & ~0x00ff) == DC2114x) {
return (inl(DE4X5_SIGR) & 0x000fffff);
}
return 0;
}
static void
timeout(struct device *dev, void (*fn)(u_long data), u_long data, u_long msec)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int dt;
del_timer(&lp->timer);
dt = (msec * HZ) / 1000;
if (dt==0) dt=1;
lp->timer.expires = jiffies + dt;
lp->timer.function = fn;
lp->timer.data = data;
add_timer(&lp->timer);
return;
}
static void
yawn(struct device *dev, int state)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if ((lp->chipset == DC21040) || (lp->chipset == DC21140)) return;
if(lp->bus == EISA) {
switch(state) {
case WAKEUP:
outb(WAKEUP, PCI_CFPM);
de4x5_ms_delay(10);
break;
case SNOOZE:
outb(SNOOZE, PCI_CFPM);
break;
case SLEEP:
outl(0, DE4X5_SICR);
outb(SLEEP, PCI_CFPM);
break;
}
} else {
switch(state) {
case WAKEUP:
pcibios_write_config_byte(lp->bus_num, lp->device << 3,
PCI_CFDA_PSM, WAKEUP);
de4x5_ms_delay(10);
break;
case SNOOZE:
pcibios_write_config_byte(lp->bus_num, lp->device << 3,
PCI_CFDA_PSM, SNOOZE);
break;
case SLEEP:
outl(0, DE4X5_SICR);
pcibios_write_config_byte(lp->bus_num, lp->device << 3,
PCI_CFDA_PSM, SLEEP);
break;
}
}
return;
}
static void
de4x5_parse_params(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
char *p, *q, t;
lp->params.fdx = 0;
lp->params.autosense = AUTO;
if (args == NULL) return;
if ((p = strstr(args, dev->name))) {
if (!(q = strstr(p+strlen(dev->name), "eth"))) q = p + strlen(p);
t = *q;
*q = '\0';
if (strstr(p, "fdx") || strstr(p, "FDX")) lp->params.fdx = 1;
if (strstr(p, "autosense") || strstr(p, "AUTOSENSE")) {
if (strstr(p, "TP")) {
lp->params.autosense = TP;
} else if (strstr(p, "TP_NW")) {
lp->params.autosense = TP_NW;
} else if (strstr(p, "BNC")) {
lp->params.autosense = BNC;
} else if (strstr(p, "AUI")) {
lp->params.autosense = AUI;
} else if (strstr(p, "BNC_AUI")) {
lp->params.autosense = BNC;
} else if (strstr(p, "10Mb")) {
lp->params.autosense = _10Mb;
} else if (strstr(p, "100Mb")) {
lp->params.autosense = _100Mb;
} else if (strstr(p, "AUTO")) {
lp->params.autosense = AUTO;
}
}
*q = t;
}
return;
}
static void
de4x5_dbg_open(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
int i;
if (de4x5_debug & DEBUG_OPEN) {
printk("%s: de4x5 opening with irq %d\n",dev->name,dev->irq);
printk("\tphysical address: ");
for (i=0;i<6;i++) {
printk("%2.2x:",(short)dev->dev_addr[i]);
}
printk("\n");
printk("Descriptor head addresses:\n");
printk("\t0x%8.8lx  0x%8.8lx\n",(u_long)lp->rx_ring,(u_long)lp->tx_ring);
printk("Descriptor addresses:\nRX: ");
for (i=0;i<lp->rxRingSize-1;i++){
if (i < 3) {
printk("0x%8.8lx  ",(u_long)&lp->rx_ring[i].status);
}
}
printk("...0x%8.8lx\n",(u_long)&lp->rx_ring[i].status);
printk("TX: ");
for (i=0;i<lp->txRingSize-1;i++){
if (i < 3) {
printk("0x%8.8lx  ", (u_long)&lp->tx_ring[i].status);
}
}
printk("...0x%8.8lx\n", (u_long)&lp->tx_ring[i].status);
printk("Descriptor buffers:\nRX: ");
for (i=0;i<lp->rxRingSize-1;i++){
if (i < 3) {
printk("0x%8.8x  ",le32_to_cpu(lp->rx_ring[i].buf));
}
}
printk("...0x%8.8x\n",le32_to_cpu(lp->rx_ring[i].buf));
printk("TX: ");
for (i=0;i<lp->txRingSize-1;i++){
if (i < 3) {
printk("0x%8.8x  ", le32_to_cpu(lp->tx_ring[i].buf));
}
}
printk("...0x%8.8x\n", le32_to_cpu(lp->tx_ring[i].buf));
printk("Ring size: \nRX: %d\nTX: %d\n",
(short)lp->rxRingSize,
(short)lp->txRingSize);
}
return;
}
static void
de4x5_dbg_mii(struct device *dev, int k)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
u_long iobase = dev->base_addr;
if (de4x5_debug & DEBUG_MII) {
printk("\nMII device address: %d\n", lp->phy[k].addr);
printk("MII CR:  %x\n",mii_rd(MII_CR,lp->phy[k].addr,DE4X5_MII));
printk("MII SR:  %x\n",mii_rd(MII_SR,lp->phy[k].addr,DE4X5_MII));
printk("MII ID0: %x\n",mii_rd(MII_ID0,lp->phy[k].addr,DE4X5_MII));
printk("MII ID1: %x\n",mii_rd(MII_ID1,lp->phy[k].addr,DE4X5_MII));
if (lp->phy[k].id != BROADCOM_T4) {
printk("MII ANA: %x\n",mii_rd(0x04,lp->phy[k].addr,DE4X5_MII));
printk("MII ANC: %x\n",mii_rd(0x05,lp->phy[k].addr,DE4X5_MII));
}
printk("MII 16:  %x\n",mii_rd(0x10,lp->phy[k].addr,DE4X5_MII));
if (lp->phy[k].id != BROADCOM_T4) {
printk("MII 17:  %x\n",mii_rd(0x11,lp->phy[k].addr,DE4X5_MII));
printk("MII 18:  %x\n",mii_rd(0x12,lp->phy[k].addr,DE4X5_MII));
} else {
printk("MII 20:  %x\n",mii_rd(0x14,lp->phy[k].addr,DE4X5_MII));
}
}
return;
}
static void
de4x5_dbg_media(struct device *dev)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
if (lp->media != lp->c_media) {
if (de4x5_debug & DEBUG_MEDIA) {
printk("%s: media is %s%s\n", dev->name,
(lp->media == NC ? "unconnected, link down or incompatible connection" :
(lp->media == TP ? "TP" :
(lp->media == ANS ? "TP/Nway" :
(lp->media == BNC ? "BNC" :
(lp->media == AUI ? "AUI" :
(lp->media == BNC_AUI ? "BNC/AUI" :
(lp->media == EXT_SIA ? "EXT SIA" :
(lp->media == _100Mb ? "100Mb/s" :
(lp->media == _10Mb ? "10Mb/s" :
"???"
))))))))), (lp->fdx?" full duplex.":"."));
}
lp->c_media = lp->media;
}
return;
}
static void
de4x5_dbg_srom(struct de4x5_srom *p)
{
int i;
if (de4x5_debug & DEBUG_SROM) {
printk("Sub-system Vendor ID: %04x\n", *((u_short *)p->sub_vendor_id));
printk("Sub-system ID:        %04x\n", *((u_short *)p->sub_system_id));
printk("ID Block CRC:         %02x\n", (u_char)(p->id_block_crc));
printk("SROM version:         %02x\n", (u_char)(p->version));
printk("# controllers:         %02x\n", (u_char)(p->num_controllers));
printk("Hardware Address:     ");
for (i=0;i<ETH_ALEN-1;i++) {
printk("%02x:", (u_char)*(p->ieee_addr+i));
}
printk("%02x\n", (u_char)*(p->ieee_addr+i));
printk("CRC checksum:         %04x\n", (u_short)(p->chksum));
for (i=0; i<64; i++) {
printk("%3d %04x\n", i<<1, (u_short)*((u_short *)p+i));
}
}
return;
}
static void
de4x5_dbg_rx(struct sk_buff *skb, int len)
{
int i, j;
if (de4x5_debug & DEBUG_RX) {
printk("R: %02x:%02x:%02x:%02x:%02x:%02x <- %02x:%02x:%02x:%02x:%02x:%02x len/SAP:%02x%02x [%d]\n",
(u_char)skb->data[0],
(u_char)skb->data[1],
(u_char)skb->data[2],
(u_char)skb->data[3],
(u_char)skb->data[4],
(u_char)skb->data[5],
(u_char)skb->data[6],
(u_char)skb->data[7],
(u_char)skb->data[8],
(u_char)skb->data[9],
(u_char)skb->data[10],
(u_char)skb->data[11],
(u_char)skb->data[12],
(u_char)skb->data[13],
len);
if (de4x5_debug & DEBUG_RX) {
for (j=0; len>0;j+=16, len-=16) {
printk("    %03x: ",j);
for (i=0; i<16 && i<len; i++) {
printk("%02x ",(u_char)skb->data[i+j]);
}
printk("\n");
}
}
}
return;
}
static int
de4x5_ioctl(struct device *dev, struct ifreq *rq, int cmd)
{
struct de4x5_private *lp = (struct de4x5_private *)dev->priv;
struct de4x5_ioctl *ioc = (struct de4x5_ioctl *) &rq->ifr_data;
u_long iobase = dev->base_addr;
int i, j, status = 0;
s32 omr;
union {
u8 addr[144];
u16 sval[72];
u32 lval[36];
} tmp;
switch(ioc->cmd) {
case DE4X5_GET_HWADDR:
ioc->len = ETH_ALEN;
status = verify_area(VERIFY_WRITE, (void *)ioc->data, ioc->len);
if (status)
break;
for (i=0; i<ETH_ALEN; i++) {
tmp.addr[i] = dev->dev_addr[i];
}
copy_to_user(ioc->data, tmp.addr, ioc->len);
break;
case DE4X5_SET_HWADDR:
status = verify_area(VERIFY_READ, (void *)ioc->data, ETH_ALEN);
if (status)
break;
status = -EPERM;
if (!suser())
break;
status = 0;
copy_from_user(tmp.addr, ioc->data, ETH_ALEN);
for (i=0; i<ETH_ALEN; i++) {
dev->dev_addr[i] = tmp.addr[i];
}
build_setup_frame(dev, PHYS_ADDR_ONLY);
while (test_and_set_bit(0, (void *)&dev->tbusy) != 0);
load_packet(dev, lp->setup_frame, TD_IC | PERFECT_F | TD_SET |
SETUP_FRAME_LEN, NULL);
lp->tx_new = (lp->tx_new + 1) % lp->txRingSize;
outl(POLL_DEMAND, DE4X5_TPD);
dev->tbusy = 0;
break;
case DE4X5_SET_PROM:
if (suser()) {
omr = inl(DE4X5_OMR);
omr |= OMR_PR;
outl(omr, DE4X5_OMR);
dev->flags |= IFF_PROMISC;
} else {
status = -EPERM;
}
break;
case DE4X5_CLR_PROM:
if (suser()) {
omr = inl(DE4X5_OMR);
omr &= ~OMR_PR;
outb(omr, DE4X5_OMR);
dev->flags &= ~IFF_PROMISC;
} else {
status = -EPERM;
}
break;
case DE4X5_SAY_BOO:
printk("%s: Boo!\n", dev->name);
break;
case DE4X5_MCA_EN:
if (suser()) {
omr = inl(DE4X5_OMR);
omr |= OMR_PM;
outl(omr, DE4X5_OMR);
} else {
status = -EPERM;
}
break;
case DE4X5_GET_STATS:
ioc->len = sizeof(lp->pktStats);
status = verify_area(VERIFY_WRITE, (void *)ioc->data, ioc->len);
if (status)
break;
cli();
copy_to_user(ioc->data, &lp->pktStats, ioc->len);
sti();
break;
case DE4X5_CLR_STATS:
if (suser()) {
cli();
memset(&lp->pktStats, 0, sizeof(lp->pktStats));
sti();
} else {
status = -EPERM;
}
break;
case DE4X5_GET_OMR:
tmp.addr[0] = inl(DE4X5_OMR);
if (!(status = verify_area(VERIFY_WRITE, (void *)ioc->data, 1))) {
copy_to_user(ioc->data, tmp.addr, 1);
}
break;
case DE4X5_SET_OMR:
if (suser()) {
if (!(status = verify_area(VERIFY_READ, (void *)ioc->data, 1))) {
copy_from_user(tmp.addr, ioc->data, 1);
outl(tmp.addr[0], DE4X5_OMR);
}
} else {
status = -EPERM;
}
break;
case DE4X5_GET_REG:
j = 0;
tmp.lval[0] = inl(DE4X5_STS); j+=4;
tmp.lval[1] = inl(DE4X5_BMR); j+=4;
tmp.lval[2] = inl(DE4X5_IMR); j+=4;
tmp.lval[3] = inl(DE4X5_OMR); j+=4;
tmp.lval[4] = inl(DE4X5_SISR); j+=4;
tmp.lval[5] = inl(DE4X5_SICR); j+=4;
tmp.lval[6] = inl(DE4X5_STRR); j+=4;
tmp.lval[7] = inl(DE4X5_SIGR); j+=4;
ioc->len = j;
if (!(status = verify_area(VERIFY_WRITE, (void *)ioc->data, ioc->len))) {
copy_to_user(ioc->data, tmp.addr, ioc->len);
}
break;
#define DE4X5_DUMP 0x0f
default:
status = -EOPNOTSUPP;
}
return status;
}
#ifdef MODULE
#define LP(a) ((struct de4x5_private *)(a))
static struct device *mdev = NULL;
static int io=0x0;
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,0)
MODULE_PARM(io, "i");
#endif
int
init_module(void)
{
int i, num, status = -EIO;
struct device *p;
num = count_adapters();
for (i=0; i<num; i++) {
if ((p = insert_device(NULL, io, de4x5_probe)) == NULL)
return -ENOMEM;
if (!mdev) mdev = p;
if (register_netdev(p) != 0) {
kfree(p);
} else {
status = 0;
lastModule = p;
}
}
return status;
}
void
cleanup_module(void)
{
while (mdev != NULL) {
mdev = unlink_modules(mdev);
}
return;
}
static struct device *
unlink_modules(struct device *p)
{
struct device *next = NULL;
if (p->priv) {
struct de4x5_private *lp = (struct de4x5_private *)p->priv;
next = lp->next_module;
if (lp->cache.buf) {
kfree(lp->cache.buf);
}
kfree(lp->cache.priv);
release_region(p->base_addr, (lp->bus == PCI ?
DE4X5_PCI_TOTAL_SIZE :
DE4X5_EISA_TOTAL_SIZE));
}
unregister_netdev(p);
kfree(p);
return next;
}
static int
count_adapters(void)
{
int i, j=0;
u_char pb, dev_fn, dev_num;
u_short dev_id, vendor;
u_int class = DE4X5_CLASS_CODE;
u_int device;
#if !defined(__sparc_v9__) && !defined(__powerpc__) && !defined(__alpha__)
char name[DE4X5_STRLEN];
u_long iobase = 0x1000;
for (i=1; i<MAX_EISA_SLOTS; i++, iobase+=EISA_SLOT_INC) {
if (EISA_signature(name, EISA_ID)) j++;
}
#endif
if (!pcibios_present()) return j;
for (i=0;
(pcibios_find_class(class, i, &pb, &dev_fn)!= PCIBIOS_DEVICE_NOT_FOUND);
i++) {
dev_num = PCI_SLOT(dev_fn);
device = 0;
pcibios_read_config_word(pb, PCI_DEVICE, PCI_VENDOR_ID, &vendor);
pcibios_read_config_word(pb, PCI_DEVICE, PCI_DEVICE_ID, &dev_id);
device = dev_id;
device <<= 8;
if (is_DC21040 || is_DC21041 || is_DC21140 || is_DC2114x) j++;
}
return j;
}
__initfunc(static struct device *
insert_device(struct device *dev, u_long iobase, int (*init)(struct device *)))
{
struct device *new;
new = (struct device *)kmalloc(sizeof(struct device)+8, GFP_KERNEL);
if (new == NULL) {
printk("de4x5.c: Device not initialised, insufficient memory\n");
return NULL;
} else {
memset((char *)new, 0, sizeof(struct device)+8);
new->name = (char *)(new + 1);
new->base_addr = iobase;
new->init = init;
}
return new;
}
#endif