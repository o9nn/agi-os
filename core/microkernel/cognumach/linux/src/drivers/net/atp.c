static const char *version =
"atp.c:v1.08 4/1/97 Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#define TX_TIMEOUT ((400*HZ)/1000)
#include <linux/config.h>
#ifdef MODULE
#include <linux/module.h>
#include <linux/version.h>
#else
#define MOD_INC_USE_COUNT
#define MOD_DEC_USE_COUNT
#endif
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
#include "atp.h"
#include <linux/version.h>
#if defined (LINUX_VERSION_CODE) && LINUX_VERSION_CODE < 0x10300
#define RUN_AT(x) (x)
#define DEV_ALLOC_SKB(len) alloc_skb(len, GFP_ATOMIC)
#define virt_to_bus(addr) ((unsigned long)addr)
#define bus_to_virt(addr) ((void*)addr)
#else
#define RUN_AT(x) (jiffies + (x))
#define DEV_ALLOC_SKB(len) dev_alloc_skb(len + 2)
#endif
#if defined (LINUX_VERSION_CODE) && LINUX_VERSION_CODE < 0x10338
#ifdef MODULE
#if !defined(CONFIG_MODVERSIONS) && !defined(__NO_VERSION__)
char kernel_version[] = UTS_RELEASE;
#endif
#else
#undef MOD_INC_USE_COUNT
#define MOD_INC_USE_COUNT
#undef MOD_DEC_USE_COUNT
#define MOD_DEC_USE_COUNT
#endif
#endif
#if (LINUX_VERSION_CODE >= 0x10344)
#define NEW_MULTICAST
#include <linux/delay.h>
#endif
#ifdef SA_SHIRQ
#define FREE_IRQ(irqnum, dev) free_irq(irqnum, dev)
#define REQUEST_IRQ(i,h,f,n, instance) request_irq(i,h,f,n, instance)
#define IRQ(irq, dev_id, pt_regs) (irq, dev_id, pt_regs)
#else
#define FREE_IRQ(irqnum, dev) free_irq(irqnum)
#define REQUEST_IRQ(i,h,f,n, instance) request_irq(i,h,f,n)
#define IRQ(irq, dev_id, pt_regs) (irq, pt_regs)
#endif
#ifndef NET_DEBUG
#define NET_DEBUG 1
#endif
static unsigned int net_debug = NET_DEBUG;
#define ETHERCARD_TOTAL_SIZE 3
static char mux_8012[] = { 0xff, 0xf7, 0xff, 0xfb, 0xf3, 0xfb, 0xff, 0xf7,};
#define TIMED_CHECKER (HZ/4)
#ifdef TIMED_CHECKER
#include <linux/timer.h>
static void atp_timed_checker(unsigned long ignored);
#endif
extern int atp_init(struct device *dev);
static int atp_probe1(struct device *dev, short ioaddr);
static void get_node_ID(struct device *dev);
static unsigned short eeprom_op(short ioaddr, unsigned int cmd);
static int net_open(struct device *dev);
static void hardware_init(struct device *dev);
static void write_packet(short ioaddr, int length, unsigned char *packet, int mode);
static void trigger_send(short ioaddr, int length);
static int net_send_packet(struct sk_buff *skb, struct device *dev);
static void net_interrupt IRQ(int irq, void *dev_id, struct pt_regs *regs);
static void net_rx(struct device *dev);
static void read_block(short ioaddr, int length, unsigned char *buffer, int data_mode);
static int net_close(struct device *dev);
static struct enet_statistics *net_get_stats(struct device *dev);
#ifdef NEW_MULTICAST
static void set_rx_mode_8002(struct device *dev);
static void set_rx_mode_8012(struct device *dev);
#else
static void set_rx_mode_8002(struct device *dev, int num_addrs, void *addrs);
static void set_rx_mode_8012(struct device *dev, int num_addrs, void *addrs);
#endif
static struct device *root_atp_dev = NULL;
int
atp_init(struct device *dev)
{
int *port, ports[] = {0x378, 0x278, 0x3bc, 0};
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return atp_probe1(dev, base_addr);
else if (base_addr == 1)
return ENXIO;
for (port = ports; *port; port++) {
int ioaddr = *port;
outb(0x57, ioaddr + PAR_DATA);
if (inb(ioaddr + PAR_DATA) != 0x57)
continue;
if (atp_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
static int atp_probe1(struct device *dev, short ioaddr)
{
struct net_local *lp;
int saved_ctrl_reg, status, i;
outb(0xff, ioaddr + PAR_DATA);
saved_ctrl_reg = inb(ioaddr + PAR_CONTROL);
outb(0x04, ioaddr + PAR_CONTROL);
for (i = 0; i < 8; i++)
outb(mux_8012[i], ioaddr + PAR_DATA);
write_reg_high(ioaddr, CMR1, CMR1h_RESET);
eeprom_delay(2048);
status = read_nibble(ioaddr, CMR1);
if ((status & 0x78) != 0x08) {
outb(saved_ctrl_reg, ioaddr + PAR_CONTROL);
return 1;
}
status = read_nibble(ioaddr, CMR2_h);
if ((status & 0x78) != 0x10) {
outb(saved_ctrl_reg, ioaddr + PAR_CONTROL);
return 1;
}
dev = init_etherdev(dev, sizeof(struct net_local));
write_reg_byte(ioaddr, CMR2, 0x01);
write_reg_high(ioaddr, CMR1, CMR1h_RxENABLE | CMR1h_TxENABLE);
if (ioaddr == 0x378)
dev->irq = 7;
else
dev->irq = 5;
write_reg_high(ioaddr, CMR1, CMR1h_TxRxOFF);
write_reg(ioaddr, CMR2, CMR2_NULL);
dev->base_addr = ioaddr;
get_node_ID(dev);
printk("%s: Pocket adapter found at %#3lx, IRQ %d, SAPROM "
"%02X:%02X:%02X:%02X:%02X:%02X.\n", dev->name, dev->base_addr,
dev->irq, dev->dev_addr[0], dev->dev_addr[1], dev->dev_addr[2],
dev->dev_addr[3], dev->dev_addr[4], dev->dev_addr[5]);
write_reg_high(ioaddr, CMR1, CMR1h_RESET | CMR1h_MUX);
if (net_debug)
printk(version);
ether_setup(dev);
if (dev->priv == NULL)
dev->priv = kmalloc(sizeof(struct net_local), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct net_local));
lp = (struct net_local *)dev->priv;
lp->chip_type = RTL8002;
lp->addr_mode = CMR2h_Normal;
lp->next_module = root_atp_dev;
root_atp_dev = dev;
dev->if_port = (dev->mem_start & 0xf) ? (dev->mem_start & 0x7) : 4;
if (dev->mem_end & 0xf)
net_debug = dev->mem_end & 7;
dev->open = net_open;
dev->stop = net_close;
dev->hard_start_xmit = net_send_packet;
dev->get_stats = net_get_stats;
dev->set_multicast_list =
lp->chip_type == RTL8002 ? &set_rx_mode_8002 : &set_rx_mode_8012;
return 0;
}
static void get_node_ID(struct device *dev)
{
short ioaddr = dev->base_addr;
int sa_offset = 0;
int i;
write_reg(ioaddr, CMR2, CMR2_EEPROM);
if (eeprom_op(ioaddr, EE_READ(0)) == 0xffff)
sa_offset = 15;
for (i = 0; i < 3; i++)
((unsigned short *)dev->dev_addr)[i] =
ntohs(eeprom_op(ioaddr, EE_READ(sa_offset + i)));
write_reg(ioaddr, CMR2, CMR2_NULL);
}
static unsigned short eeprom_op(short ioaddr, unsigned int cmd)
{
unsigned eedata_out = 0;
int num_bits = EE_CMD_SIZE;
while (--num_bits >= 0) {
char outval = test_bit(num_bits, &cmd) ? EE_DATA_WRITE : 0;
write_reg_high(ioaddr, PROM_CMD, outval | EE_CLK_LOW);
eeprom_delay(5);
write_reg_high(ioaddr, PROM_CMD, outval | EE_CLK_HIGH);
eedata_out <<= 1;
if (read_nibble(ioaddr, PROM_DATA) & EE_DATA_READ)
eedata_out++;
eeprom_delay(5);
}
write_reg_high(ioaddr, PROM_CMD, EE_CLK_LOW & ~EE_CS);
return eedata_out;
}
static int net_open(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
#ifndef SA_SHIRQ
if (irq2dev_map[dev->irq] != 0
|| (irq2dev_map[dev->irq] = dev) == 0
|| REQUEST_IRQ(dev->irq, &net_interrupt, 0, "ATP", dev)) {
return -EAGAIN;
}
#else
if (request_irq(dev->irq, &net_interrupt, 0, "ATP Ethernet", dev))
return -EAGAIN;
#endif
MOD_INC_USE_COUNT;
hardware_init(dev);
dev->start = 1;
init_timer(&lp->timer);
lp->timer.expires = RUN_AT(TIMED_CHECKER);
lp->timer.data = (unsigned long)dev;
lp->timer.function = &atp_timed_checker;
add_timer(&lp->timer);
return 0;
}
static void hardware_init(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
int i;
for (i = 0; i < 8; i++)
outb(mux_8012[i], ioaddr + PAR_DATA);
write_reg_high(ioaddr, CMR1, CMR1h_RESET);
for (i = 0; i < 6; i++)
write_reg_byte(ioaddr, PAR0 + i, dev->dev_addr[i]);
write_reg_high(ioaddr, CMR2, lp->addr_mode);
if (net_debug > 2) {
printk("%s: Reset: current Rx mode %d.\n", dev->name,
(read_nibble(ioaddr, CMR2_h) >> 3) & 0x0f);
}
write_reg(ioaddr, CMR2, CMR2_IRQOUT);
write_reg_high(ioaddr, CMR1, CMR1h_RxENABLE | CMR1h_TxENABLE);
outb(Ctrl_SelData + Ctrl_IRQEN, ioaddr + PAR_CONTROL);
write_reg(ioaddr, IMR, ISR_RxOK | ISR_TxErr | ISR_TxOK);
write_reg_high(ioaddr, IMR, ISRh_RxErr);
lp->tx_unit_busy = 0;
lp->pac_cnt_in_tx_buf = 0;
lp->saved_tx_size = 0;
dev->tbusy = 0;
dev->interrupt = 0;
}
static void trigger_send(short ioaddr, int length)
{
write_reg_byte(ioaddr, TxCNT0, length & 0xff);
write_reg(ioaddr, TxCNT1, length >> 8);
write_reg(ioaddr, CMR1, CMR1_Xmit);
}
static void write_packet(short ioaddr, int length, unsigned char *packet, int data_mode)
{
length = (length + 1) & ~1;
outb(EOC+MAR, ioaddr + PAR_DATA);
if ((data_mode & 1) == 0) {
outb(WrAddr+MAR, ioaddr + PAR_DATA);
do {
write_byte_mode0(ioaddr, *packet++);
} while (--length > 0) ;
} else {
unsigned char outbyte = *packet++;
outb(Ctrl_LNibWrite + Ctrl_IRQEN, ioaddr + PAR_CONTROL);
outb(WrAddr+MAR, ioaddr + PAR_DATA);
outb((outbyte & 0x0f)|0x40, ioaddr + PAR_DATA);
outb(outbyte & 0x0f, ioaddr + PAR_DATA);
outbyte >>= 4;
outb(outbyte & 0x0f, ioaddr + PAR_DATA);
outb(Ctrl_HNibWrite + Ctrl_IRQEN, ioaddr + PAR_CONTROL);
while (--length > 0)
write_byte_mode1(ioaddr, *packet++);
}
outb(0xff, ioaddr + PAR_DATA);
outb(Ctrl_HNibWrite | Ctrl_SelData | Ctrl_IRQEN, ioaddr + PAR_CONTROL);
}
static int
net_send_packet(struct sk_buff *skb, struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
#ifndef final_version
if (skb == NULL || skb->len <= 0) {
printk("%s: Obsolete driver layer request made: skbuff==NULL.\n",
dev->name);
dev_tint(dev);
return 0;
}
#endif
if (set_bit(0, (void*)&dev->tbusy) != 0) {
if (jiffies - dev->trans_start < TX_TIMEOUT)
return 1;
printk("%s: transmit timed out, %s?\n", dev->name,
inb(ioaddr + PAR_CONTROL) & 0x10 ? "network cable problem"
: "IRQ conflict");
lp->stats.tx_errors++;
hardware_init(dev);
dev->tbusy=0;
dev->trans_start = jiffies;
return 1;
} else {
short length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
unsigned char *buf = skb->data;
int flags;
save_flags(flags);
cli();
write_reg(ioaddr, IMR, 0);
write_reg_high(ioaddr, IMR, 0);
restore_flags(flags);
write_packet(ioaddr, length, buf, dev->if_port);
lp->pac_cnt_in_tx_buf++;
if (lp->tx_unit_busy == 0) {
trigger_send(ioaddr, length);
lp->saved_tx_size = 0;
lp->re_tx = 0;
lp->tx_unit_busy = 1;
} else
lp->saved_tx_size = length;
dev->trans_start = jiffies;
write_reg(ioaddr, IMR, ISR_RxOK | ISR_TxErr | ISR_TxOK);
write_reg_high(ioaddr, IMR, ISRh_RxErr);
}
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
static void
net_interrupt IRQ(int irq, void *dev_instance, struct pt_regs * regs)
{
#ifdef SA_SHIRQ
struct device *dev = (struct device *)dev_instance;
#else
struct device *dev = (struct device *)(irq2dev_map[irq]);
#endif
struct net_local *lp;
int ioaddr, status, boguscount = 20;
static int num_tx_since_rx = 0;
if (dev == NULL) {
printk ("ATP_interrupt(): irq %d for unknown device.\n", irq);
return;
}
dev->interrupt = 1;
ioaddr = dev->base_addr;
lp = (struct net_local *)dev->priv;
outb(Ctrl_SelData, ioaddr + PAR_CONTROL);
write_reg(ioaddr, CMR2, CMR2_NULL);
write_reg(ioaddr, IMR, 0);
if (net_debug > 5) printk("%s: In interrupt ", dev->name);
while (--boguscount > 0) {
status = read_nibble(ioaddr, ISR);
if (net_debug > 5) printk("loop status %02x..", status);
if (status & (ISR_RxOK<<3)) {
write_reg(ioaddr, ISR, ISR_RxOK);
do {
int read_status = read_nibble(ioaddr, CMR1);
if (net_debug > 6)
printk("handling Rx packet %02x..", read_status);
if (read_status & (CMR1_IRQ << 3)) {
lp->stats.rx_over_errors++;
write_reg_high(ioaddr, CMR2, CMR2h_OFF);
net_rx(dev);
write_reg_high(ioaddr, ISR, ISRh_RxErr);
write_reg_high(ioaddr, CMR2, lp->addr_mode);
} else if ((read_status & (CMR1_BufEnb << 3)) == 0) {
net_rx(dev);
dev->last_rx = jiffies;
num_tx_since_rx = 0;
} else
break;
} while (--boguscount > 0);
} else if (status & ((ISR_TxErr + ISR_TxOK)<<3)) {
if (net_debug > 6) printk("handling Tx done..");
write_reg(ioaddr, ISR, ISR_TxErr + ISR_TxOK);
if (status & (ISR_TxErr<<3)) {
lp->stats.collisions++;
if (++lp->re_tx > 15) {
lp->stats.tx_aborted_errors++;
hardware_init(dev);
break;
}
if (net_debug > 6) printk("attempting to ReTx");
write_reg(ioaddr, CMR1, CMR1_ReXmit + CMR1_Xmit);
} else {
lp->stats.tx_packets++;
lp->pac_cnt_in_tx_buf--;
if ( lp->saved_tx_size) {
trigger_send(ioaddr, lp->saved_tx_size);
lp->saved_tx_size = 0;
lp->re_tx = 0;
} else
lp->tx_unit_busy = 0;
dev->tbusy = 0;
mark_bh(NET_BH);
}
num_tx_since_rx++;
} else if (num_tx_since_rx > 8
&& jiffies > dev->last_rx + 100) {
if (net_debug > 2)
printk("%s: Missed packet? No Rx after %d Tx and %ld jiffies"
" status %02x  CMR1 %02x.\n", dev->name,
num_tx_since_rx, jiffies - dev->last_rx, status,
(read_nibble(ioaddr, CMR1) >> 3) & 15);
lp->stats.rx_missed_errors++;
hardware_init(dev);
num_tx_since_rx = 0;
break;
} else
break;
}
{
int i;
for (i = 0; i < 6; i++)
write_reg_byte(ioaddr, PAR0 + i, dev->dev_addr[i]);
}
write_reg(ioaddr, CMR2, CMR2_IRQOUT);
outb(Ctrl_SelData + Ctrl_IRQEN, ioaddr + PAR_CONTROL);
write_reg(ioaddr, IMR, ISR_RxOK | ISR_TxErr | ISR_TxOK);
write_reg_high(ioaddr, IMR, ISRh_RxErr);
if (net_debug > 5) printk("exiting interrupt.\n");
dev->interrupt = 0;
return;
}
#ifdef TIMED_CHECKER
static void atp_timed_checker(unsigned long data)
{
struct device *dev = (struct device *)data;
int ioaddr = dev->base_addr;
struct net_local *lp = (struct net_local *)dev->priv;
int tickssofar = jiffies - lp->last_rx_time;
int i;
if (tickssofar > 2*HZ && dev->interrupt == 0) {
#if 1
for (i = 0; i < 6; i++)
write_reg_byte(ioaddr, PAR0 + i, dev->dev_addr[i]);
lp->last_rx_time = jiffies;
#else
for (i = 0; i < 6; i++)
if (read_cmd_byte(ioaddr, PAR0 + i) != atp_timed_dev->dev_addr[i])
{
struct net_local *lp = (struct net_local *)atp_timed_dev->priv;
write_reg_byte(ioaddr, PAR0 + i, atp_timed_dev->dev_addr[i]);
if (i == 2)
lp->stats.tx_errors++;
else if (i == 3)
lp->stats.tx_dropped++;
else if (i == 4)
lp->stats.collisions++;
else
lp->stats.rx_errors++;
}
#endif
}
lp->timer.expires = RUN_AT(TIMED_CHECKER);
add_timer(&lp->timer);
}
#endif
static void net_rx(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
struct rx_header rx_head;
outb(EOC+MAR, ioaddr + PAR_DATA);
read_block(ioaddr, 8, (unsigned char*)&rx_head, dev->if_port);
if (net_debug > 5)
printk(" rx_count %04x %04x %04x %04x..", rx_head.pad,
rx_head.rx_count, rx_head.rx_status, rx_head.cur_addr);
if ((rx_head.rx_status & 0x77) != 0x01) {
lp->stats.rx_errors++;
if (rx_head.rx_status & 0x0004) lp->stats.rx_frame_errors++;
else if (rx_head.rx_status & 0x0002) lp->stats.rx_crc_errors++;
if (net_debug > 3) printk("%s: Unknown ATP Rx error %04x.\n",
dev->name, rx_head.rx_status);
if (rx_head.rx_status & 0x0020) {
lp->stats.rx_fifo_errors++;
write_reg_high(ioaddr, CMR1, CMR1h_TxENABLE);
write_reg_high(ioaddr, CMR1, CMR1h_RxENABLE | CMR1h_TxENABLE);
} else if (rx_head.rx_status & 0x0050)
hardware_init(dev);
return;
} else {
int pkt_len = (rx_head.rx_count & 0x7ff) - 4;
struct sk_buff *skb;
skb = DEV_ALLOC_SKB(pkt_len + 2);
if (skb == NULL) {
printk("%s: Memory squeeze, dropping packet.\n", dev->name);
lp->stats.rx_dropped++;
goto done;
}
skb->dev = dev;
#if LINUX_VERSION_CODE >= 0x10300
skb_reserve(skb, 2);
read_block(ioaddr, pkt_len, skb_put(skb,pkt_len), dev->if_port);
skb->protocol = eth_type_trans(skb, dev);
#else
read_block(ioaddr, pkt_len, skb->data, dev->if_port);
skb->len = pkt_len;
#endif
netif_rx(skb);
lp->stats.rx_packets++;
}
done:
write_reg(ioaddr, CMR1, CMR1_NextPkt);
lp->last_rx_time = jiffies;
return;
}
static void read_block(short ioaddr, int length, unsigned char *p, int data_mode)
{
if (data_mode <= 3) {
outb(Ctrl_LNibRead, ioaddr + PAR_CONTROL);
outb(length == 8 ? RdAddr | HNib | MAR : RdAddr | MAR,
ioaddr + PAR_DATA);
if (data_mode <= 1) {
do *p++ = read_byte_mode0(ioaddr); while (--length > 0);
} else
do *p++ = read_byte_mode2(ioaddr); while (--length > 0);
} else if (data_mode <= 5)
do *p++ = read_byte_mode4(ioaddr); while (--length > 0);
else
do *p++ = read_byte_mode6(ioaddr); while (--length > 0);
outb(EOC+HNib+MAR, ioaddr + PAR_DATA);
outb(Ctrl_SelData, ioaddr + PAR_CONTROL);
}
static int
net_close(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
dev->tbusy = 1;
dev->start = 0;
del_timer(&lp->timer);
lp->addr_mode = CMR2h_OFF;
write_reg_high(ioaddr, CMR2, CMR2h_OFF);
outb(0x00, ioaddr + PAR_CONTROL);
FREE_IRQ(dev->irq, dev);
#ifndef SA_SHIRQ
irq2dev_map[dev->irq] = 0;
#endif
write_reg_high(ioaddr, CMR1, CMR1h_RESET | CMR1h_MUX);
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
#ifdef NEW_MULTICAST
set_rx_mode_8002(struct device *dev)
#else
static void set_rx_mode_8002(struct device *dev, int num_addrs, void *addrs);
#endif
{
struct net_local *lp = (struct net_local *)dev->priv;
short ioaddr = dev->base_addr;
if ( dev->mc_count > 0 || (dev->flags & (IFF_ALLMULTI|IFF_PROMISC))) {
dev->flags|=IFF_PROMISC;
lp->addr_mode = CMR2h_PROMISC;
} else
lp->addr_mode = CMR2h_Normal;
write_reg_high(ioaddr, CMR2, lp->addr_mode);
}
static void
#ifdef NEW_MULTICAST
set_rx_mode_8012(struct device *dev)
#else
static void set_rx_mode_8012(struct device *dev, int num_addrs, void *addrs);
#endif
{
struct net_local *lp = (struct net_local *)dev->priv;
short ioaddr = dev->base_addr;
unsigned char new_mode, mc_filter[8];
int i;
if (dev->flags & IFF_PROMISC) {
new_mode = CMR2h_PROMISC;
} else if ((dev->mc_count > 1000) || (dev->flags & IFF_ALLMULTI)) {
memset(mc_filter, 0xff, sizeof(mc_filter));
new_mode = CMR2h_Normal;
} else {
struct dev_mc_list *mclist;
memset(mc_filter, 0, sizeof(mc_filter));
for (i = 0, mclist = dev->mc_list; mclist && i < dev->mc_count;
i++, mclist = mclist->next)
set_bit(ether_crc_le(ETH_ALEN, mclist->dmi_addr) & 0x3f,
mc_filter);
new_mode = CMR2h_Normal;
}
lp->addr_mode = new_mode;
write_reg(ioaddr, CMR2, CMR2_IRQOUT | 0x04);
for (i = 0; i < 8; i++)
write_reg_byte(ioaddr, i, mc_filter[i]);
if (net_debug > 2 || 1) {
lp->addr_mode = 1;
printk("%s: Mode %d, setting multicast filter to",
dev->name, lp->addr_mode);
for (i = 0; i < 8; i++)
printk(" %2.2x", mc_filter[i]);
printk(".\n");
}
write_reg_high(ioaddr, CMR2, lp->addr_mode);
write_reg(ioaddr, CMR2, CMR2_IRQOUT);
}
#ifdef MODULE
static int debug = 1;
int
init_module(void)
{
net_debug = debug;
root_atp_dev = NULL;
atp_init(0);
return 0;
}
void
cleanup_module(void)
{
struct device *next_dev;
while (root_atp_dev) {
next_dev = ((struct net_local *)root_atp_dev->priv)->next_module;
unregister_netdev(root_atp_dev);
kfree(root_atp_dev);
root_atp_dev = next_dev;
}
}
#endif