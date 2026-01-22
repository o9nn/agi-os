static const char *version =
"at1700.c:v1.15 4/7/98  Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/ioport.h>
#include <linux/in.h>
#include <linux/malloc.h>
#include <linux/string.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/errno.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#define MC_FILTERBREAK 64
static int at1700_probe_list[] =
{0x260, 0x280, 0x2a0, 0x240, 0x340, 0x320, 0x380, 0x300, 0};
static int fmv18x_probe_list[] =
{0x220, 0x240, 0x260, 0x280, 0x2a0, 0x2c0, 0x300, 0x340, 0};
#ifndef NET_DEBUG
#define NET_DEBUG 1
#endif
static unsigned int net_debug = NET_DEBUG;
typedef unsigned char uchar;
struct net_local {
struct enet_statistics stats;
unsigned char mc_filter[8];
uint jumpered:1;
uint tx_started:1;
uint invalid_irq:1;
uchar tx_queue;
ushort tx_queue_len;
};
#define STATUS 0
#define TX_STATUS 0
#define RX_STATUS 1
#define TX_INTR 2
#define RX_INTR 3
#define TX_MODE 4
#define RX_MODE 5
#define CONFIG_0 6
#define CONFIG_1 7
#define DATAPORT 8
#define TX_START 10
#define MODE13 13
#define EEPROM_Ctrl 16
#define EEPROM_Data 17
#define IOCONFIG 18
#define IOCONFIG1 19
#define SAPROM 20
#define RESET 31
#define AT1700_IO_EXTENT 32
extern int at1700_probe(struct device *dev);
static int at1700_probe1(struct device *dev, int ioaddr);
static int read_eeprom(int ioaddr, int location);
static int net_open(struct device *dev);
static int net_send_packet(struct sk_buff *skb, struct device *dev);
static void net_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void net_rx(struct device *dev);
static int net_close(struct device *dev);
static struct enet_statistics *net_get_stats(struct device *dev);
static void set_rx_mode(struct device *dev);
#ifdef HAVE_DEVLIST
struct netdev_entry at1700_drv =
{"at1700", at1700_probe1, AT1700_IO_EXTENT, at1700_probe_list};
#else
int
at1700_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return at1700_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; at1700_probe_list[i]; i++) {
int ioaddr = at1700_probe_list[i];
if (check_region(ioaddr, AT1700_IO_EXTENT))
continue;
if (at1700_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
int at1700_probe1(struct device *dev, int ioaddr)
{
char fmv_irqmap[4] = {3, 7, 10, 15};
char at1700_irqmap[8] = {3, 4, 5, 9, 10, 11, 14, 15};
unsigned int i, irq, is_fmv18x = 0, is_at1700 = 0;
#ifdef notdef
printk("at1700 probe at %#x, eeprom is %4.4x %4.4x %4.4x ctrl %4.4x.\n",
ioaddr, read_eeprom(ioaddr, 4), read_eeprom(ioaddr, 5),
read_eeprom(ioaddr, 6), inw(ioaddr + EEPROM_Ctrl));
#endif
if (at1700_probe_list[inb(ioaddr + IOCONFIG1) & 0x07] == ioaddr
&& read_eeprom(ioaddr, 4) == 0x0000
&& (read_eeprom(ioaddr, 5) & 0xff00) == 0xF400)
is_at1700 = 1;
else if (fmv18x_probe_list[inb(ioaddr + IOCONFIG) & 0x07] == ioaddr
&& inb(ioaddr + SAPROM ) == 0x00
&& inb(ioaddr + SAPROM + 1) == 0x00
&& inb(ioaddr + SAPROM + 2) == 0x0e)
is_fmv18x = 1;
else
return -ENODEV;
outb(0, ioaddr + RESET);
if (dev == NULL)
dev = init_etherdev(0, sizeof(struct net_local));
if (is_at1700)
irq = at1700_irqmap[(read_eeprom(ioaddr, 12)&0x04)
| (read_eeprom(ioaddr, 0)>>14)];
else
irq = fmv_irqmap[(inb(ioaddr + IOCONFIG)>>6) & 0x03];
request_region(ioaddr, AT1700_IO_EXTENT, dev->name);
printk("%s: AT1700 found at %#3x, IRQ %d, address ", dev->name,
ioaddr, irq);
dev->base_addr = ioaddr;
dev->irq = irq;
for(i = 0; i < 3; i++) {
unsigned short eeprom_val = read_eeprom(ioaddr, 4+i);
printk("%04x", eeprom_val);
((unsigned short *)dev->dev_addr)[i] = ntohs(eeprom_val);
}
{
const char *porttype[] = {"auto-sense", "10baseT", "auto-sense", "10base2"};
ushort setup_value = read_eeprom(ioaddr, 12);
dev->if_port = setup_value >> 8;
printk(" %s interface.\n", porttype[(dev->if_port>>3) & 3]);
}
outb(0xe0, ioaddr + CONFIG_1);
for (i = 0; i < 6; i++)
outb(dev->dev_addr[i], ioaddr + 8 + i);
outb(0xe4, ioaddr + CONFIG_1);
for (i = 0; i < 8; i++)
outb(0x00, ioaddr + 8 + i);
outb(0xda, ioaddr + CONFIG_0);
outb(0xe8, ioaddr + CONFIG_1);
outb(dev->if_port, MODE13);
outb(0x00, ioaddr + CONFIG_1);
if (net_debug)
printk("%s", version);
dev->priv = kmalloc(sizeof(struct net_local), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct net_local));
dev->open = net_open;
dev->stop = net_close;
dev->hard_start_xmit = net_send_packet;
dev->get_stats = net_get_stats;
dev->set_multicast_list = &set_rx_mode;
ether_setup(dev);
{
struct net_local *lp = (struct net_local *)dev->priv;
lp->jumpered = is_fmv18x;
if (request_irq(irq, &net_interrupt, 0, dev->name, dev)) {
printk ("  AT1700 at %#3x is unusable due to a conflict on"
"IRQ %d.\n", ioaddr, irq);
lp->invalid_irq = 1;
return 0;
}
}
return 0;
}
#define EE_SHIFT_CLK 0x40
#define EE_CS 0x20
#define EE_DATA_WRITE 0x80
#define EE_DATA_READ 0x80
#define eeprom_delay() do {} while (0);
#define EE_WRITE_CMD (5 << 6)
#define EE_READ_CMD (6 << 6)
#define EE_ERASE_CMD (7 << 6)
static int read_eeprom(int ioaddr, int location)
{
int i;
unsigned short retval = 0;
int ee_addr = ioaddr + EEPROM_Ctrl;
int ee_daddr = ioaddr + EEPROM_Data;
int read_cmd = location | EE_READ_CMD;
for (i = 9; i >= 0; i--) {
short dataval = (read_cmd & (1 << i)) ? EE_DATA_WRITE : 0;
outb(EE_CS, ee_addr);
outb(dataval, ee_daddr);
eeprom_delay();
outb(EE_CS | EE_SHIFT_CLK, ee_addr);
eeprom_delay();
}
outb(EE_DATA_WRITE, ee_daddr);
for (i = 16; i > 0; i--) {
outb(EE_CS, ee_addr);
eeprom_delay();
outb(EE_CS | EE_SHIFT_CLK, ee_addr);
eeprom_delay();
retval = (retval << 1) | ((inb(ee_daddr) & EE_DATA_READ) ? 1 : 0);
}
outb(EE_CS, ee_addr);
eeprom_delay();
outb(EE_SHIFT_CLK, ee_addr);
outb(0, ee_addr);
return retval;
}
static int net_open(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
int i;
outb(0xe0, ioaddr + CONFIG_1);
for (i = 0; i < 6; i++)
outb(dev->dev_addr[i], ioaddr + 8 + i);
outb(0xe4, ioaddr + CONFIG_1);
for (i = 0; i < 8; i++)
outb(0x00, ioaddr + 8 + i);
outb(0xda, ioaddr + CONFIG_0);
outw(0xe85a, ioaddr + CONFIG_0);
lp->tx_started = 0;
lp->tx_queue = 0;
lp->tx_queue_len = 0;
outb(0x00, ioaddr + TX_INTR);
outb(0x81, ioaddr + RX_INTR);
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
MOD_INC_USE_COUNT;
return 0;
}
static int
net_send_packet(struct sk_buff *skb, struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 10)
return 1;
printk("%s: transmit timed out with status %04x, %s?\n", dev->name,
inw(ioaddr + STATUS), inb(ioaddr + TX_STATUS) & 0x80
? "IRQ conflict" : "network cable problem");
printk("%s: timeout registers: %04x %04x %04x %04x %04x %04x %04x %04x.\n",
dev->name, inw(ioaddr + 0), inw(ioaddr + 2), inw(ioaddr + 4),
inw(ioaddr + 6), inw(ioaddr + 8), inw(ioaddr + 10),
inw(ioaddr + 12), inw(ioaddr + 14));
lp->stats.tx_errors++;
outw(0xffff, ioaddr + 24);
outw(0xffff, ioaddr + TX_STATUS);
outw(0xe85a, ioaddr + CONFIG_0);
outw(0x8100, ioaddr + TX_INTR);
dev->tbusy=0;
dev->trans_start = jiffies;
lp->tx_started = 0;
lp->tx_queue = 0;
lp->tx_queue_len = 0;
}
if (skb == NULL) {
dev_tint(dev);
return 0;
}
if (set_bit(0, (void*)&dev->tbusy) != 0)
printk("%s: Transmitter access conflict.\n", dev->name);
else {
short length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
unsigned char *buf = skb->data;
outb(0x00, ioaddr + TX_INTR);
outw(length, ioaddr + DATAPORT);
outsw(ioaddr + DATAPORT, buf, (length + 1) >> 1);
lp->tx_queue++;
lp->tx_queue_len += length + 2;
if (lp->tx_started == 0) {
outb(0x80 | lp->tx_queue, ioaddr + TX_START);
lp->tx_queue = 0;
lp->tx_queue_len = 0;
dev->trans_start = jiffies;
lp->tx_started = 1;
dev->tbusy = 0;
} else if (lp->tx_queue_len < 4096 - 1502)
dev->tbusy = 0;
outb(0x82, ioaddr + TX_INTR);
}
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
static void
net_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct device *dev = dev_id;
struct net_local *lp;
int ioaddr, status;
if (dev == NULL) {
printk ("at1700_interrupt(): irq %d for unknown device.\n", irq);
return;
}
dev->interrupt = 1;
ioaddr = dev->base_addr;
lp = (struct net_local *)dev->priv;
status = inw(ioaddr + TX_STATUS);
outw(status, ioaddr + TX_STATUS);
if (net_debug > 4)
printk("%s: Interrupt with status %04x.\n", dev->name, status);
if (status & 0xff00
|| (inb(ioaddr + RX_MODE) & 0x40) == 0) {
net_rx(dev);
}
if (status & 0x00ff) {
if (status & 0x80) {
lp->stats.tx_packets++;
if (lp->tx_queue) {
outb(0x80 | lp->tx_queue, ioaddr + TX_START);
lp->tx_queue = 0;
lp->tx_queue_len = 0;
dev->trans_start = jiffies;
dev->tbusy = 0;
mark_bh(NET_BH);
} else {
lp->tx_started = 0;
outb(0x00, ioaddr + TX_INTR);
dev->tbusy = 0;
mark_bh(NET_BH);
}
}
}
dev->interrupt = 0;
return;
}
static void
net_rx(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
int boguscount = 5;
while ((inb(ioaddr + RX_MODE) & 0x40) == 0) {
ushort status = inw(ioaddr + DATAPORT);
ushort pkt_len = inw(ioaddr + DATAPORT);
if (net_debug > 4)
printk("%s: Rxing packet mode %02x status %04x.\n",
dev->name, inb(ioaddr + RX_MODE), status);
#ifndef final_version
if (status == 0) {
outb(0x05, ioaddr + 14);
break;
}
#endif
if ((status & 0xF0) != 0x20) {
lp->stats.rx_errors++;
if (status & 0x08) lp->stats.rx_length_errors++;
if (status & 0x04) lp->stats.rx_frame_errors++;
if (status & 0x02) lp->stats.rx_crc_errors++;
if (status & 0x01) lp->stats.rx_over_errors++;
} else {
struct sk_buff *skb;
if (pkt_len > 1550) {
printk("%s: The AT1700 claimed a very large packet, size %d.\n",
dev->name, pkt_len);
inw(ioaddr + DATAPORT); inw(ioaddr + DATAPORT);
outb(0x05, ioaddr + 14);
lp->stats.rx_errors++;
break;
}
skb = dev_alloc_skb(pkt_len+3);
if (skb == NULL) {
printk("%s: Memory squeeze, dropping packet (len %d).\n",
dev->name, pkt_len);
inw(ioaddr + DATAPORT); inw(ioaddr + DATAPORT);
outb(0x05, ioaddr + 14);
lp->stats.rx_dropped++;
break;
}
skb->dev = dev;
skb_reserve(skb,2);
insw(ioaddr + DATAPORT, skb_put(skb,pkt_len), (pkt_len + 1) >> 1);
skb->protocol=eth_type_trans(skb, dev);
netif_rx(skb);
lp->stats.rx_packets++;
}
if (--boguscount <= 0)
break;
}
{
int i;
for (i = 0; i < 20; i++) {
if ((inb(ioaddr + RX_MODE) & 0x40) == 0x40)
break;
inw(ioaddr + DATAPORT);
outb(0x05, ioaddr + 14);
}
if (net_debug > 5)
printk("%s: Exint Rx packet with mode %02x after %d ticks.\n",
dev->name, inb(ioaddr + RX_MODE), i);
}
return;
}
static int net_close(struct device *dev)
{
#if 0
struct net_local *lp = (struct net_local *)dev->priv;
#endif
int ioaddr = dev->base_addr;
dev->tbusy = 1;
dev->start = 0;
outb(0xda, ioaddr + CONFIG_0);
#if 0
if (lp->jumpered) {
outb(0x00, ioaddr + IOCONFIG1);
free_irq(dev->irq, dev);
}
#endif
outb(0x00, ioaddr + CONFIG_1);
MOD_DEC_USE_COUNT;
return 0;
}
static struct enet_statistics *
net_get_stats(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
return &lp->stats;
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
static void
set_rx_mode(struct device *dev)
{
int ioaddr = dev->base_addr;
struct net_local *lp = (struct net_local *)dev->priv;
unsigned char mc_filter[8];
long flags;
int i;
if (dev->flags & IFF_PROMISC) {
printk("%s: Promiscuous mode enabled.\n", dev->name);
memset(mc_filter, 0xff, sizeof(mc_filter));
outb(3, ioaddr + RX_MODE);
} else if (dev->mc_count > MC_FILTERBREAK
|| (dev->flags & IFF_ALLMULTI)) {
memset(mc_filter, 0xff, sizeof(mc_filter));
outb(2, ioaddr + RX_MODE);
} else if (dev->mc_count == 0) {
memset(mc_filter, 0x00, sizeof(mc_filter));
outb(1, ioaddr + RX_MODE);
} else {
struct dev_mc_list *mclist;
int i;
memset(mc_filter, 0, sizeof(mc_filter));
for (i = 0, mclist = dev->mc_list; mclist && i < dev->mc_count;
i++, mclist = mclist->next)
set_bit(ether_crc_le(ETH_ALEN, mclist->dmi_addr) >> 26,
mc_filter);
}
save_flags(flags);
cli();
if (memcmp(mc_filter, lp->mc_filter, sizeof(mc_filter))) {
int saved_bank = inw(ioaddr + CONFIG_0);
outw((saved_bank & ~0x0C00) | 0x0480, ioaddr + CONFIG_0);
for (i = 0; i < 8; i++)
outb(mc_filter[i], ioaddr + 8 + i);
memcpy(lp->mc_filter, mc_filter, sizeof(mc_filter));
outw(saved_bank, ioaddr + CONFIG_0);
}
restore_flags(flags);
return;
}
#ifdef MODULE
static char devicename[9] = { 0, };
static struct device dev_at1700 = {
devicename,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, at1700_probe };
static int io = 0x260;
static int irq = 0;
int init_module(void)
{
if (io == 0)
printk("at1700: You should not use auto-probing with insmod!\n");
dev_at1700.base_addr = io;
dev_at1700.irq = irq;
if (register_netdev(&dev_at1700) != 0) {
printk("at1700: register_netdev() returned non-zero.\n");
return -EIO;
}
return 0;
}
void
cleanup_module(void)
{
unregister_netdev(&dev_at1700);
kfree(dev_at1700.priv);
dev_at1700.priv = NULL;
free_irq(dev_at1700.irq, NULL);
release_region(dev_at1700.base_addr, AT1700_IO_EXTENT);
}
#endif