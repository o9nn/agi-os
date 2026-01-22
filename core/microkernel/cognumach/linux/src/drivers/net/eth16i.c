static char *version =
"eth16i.c: v0.33 10-09-98 Mika Kuoppala (miku@iki.fi)\n";
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
#include <linux/errno.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#ifndef LINUX_VERSION_CODE
#include <linux/version.h>
#endif
#if LINUX_VERSION_CODE >= 0x20123
#include <linux/init.h>
#else
#define __init
#define __initdata
#define __initfunc(x) x
#endif
#if LINUX_VERSION_CODE < 0x20138
#endif
#if LINUX_VERSION_CODE < 0x020100
typedef struct enet_statistics eth16i_stats_type;
#else
typedef struct net_device_stats eth16i_stats_type;
#endif
#define BIT(a)		       ( (1 << (a)) )
#define BITSET(ioaddr, bnum)   ((outb(((inb(ioaddr)) | (bnum)), ioaddr)))
#define BITCLR(ioaddr, bnum)   ((outb(((inb(ioaddr)) & (~(bnum))), ioaddr)))
#define ETH16I_IO_EXTENT       32
#define TX_TIMEOUT             (400*HZ/1000)
#define MAX_RX_LOOP            20
#define ETH16I_INTR_ON	       0xef8a
#define ETH16I_INTR_OFF	       0x0000
#define PKT_GOOD               BIT(5)
#define PKT_GOOD_RMT           BIT(4)
#define PKT_SHORT              BIT(3)
#define PKT_ALIGN_ERR          BIT(2)
#define PKT_CRC_ERR            BIT(1)
#define PKT_RX_BUF_OVERFLOW    BIT(0)
#define TX_STATUS_REG          0
#define TX_DONE                BIT(7)
#define NET_BUSY               BIT(6)
#define TX_PKT_RCD             BIT(5)
#define CR_LOST                BIT(4)
#define TX_JABBER_ERR	       BIT(3)
#define COLLISION              BIT(2)
#define COLLISIONS_16          BIT(1)
#define RX_STATUS_REG          1
#define RX_PKT                 BIT(7)
#define BUS_RD_ERR             BIT(6)
#define SHORT_PKT_ERR          BIT(3)
#define ALIGN_ERR              BIT(2)
#define CRC_ERR                BIT(1)
#define RX_BUF_OVERFLOW        BIT(0)
#define TX_INTR_REG            2
#define TX_INTR_DONE           BIT(7)
#define TX_INTR_COL            BIT(2)
#define TX_INTR_16_COL         BIT(1)
#define RX_INTR_REG            3
#define RX_INTR_RECEIVE        BIT(7)
#define RX_INTR_SHORT_PKT      BIT(3)
#define RX_INTR_CRC_ERR        BIT(1)
#define RX_INTR_BUF_OVERFLOW   BIT(0)
#define TRANSMIT_MODE_REG      4
#define LOOPBACK_CONTROL       BIT(1)
#define CONTROL_OUTPUT         BIT(2)
#define RECEIVE_MODE_REG       5
#define RX_BUFFER_EMPTY        BIT(6)
#define ACCEPT_BAD_PACKETS     BIT(5)
#define RECEIVE_SHORT_ADDR     BIT(4)
#define ACCEPT_SHORT_PACKETS   BIT(3)
#define REMOTE_RESET           BIT(2)
#define ADDRESS_FILTER_MODE    BIT(1) | BIT(0)
#define REJECT_ALL             0
#define ACCEPT_ALL             3
#define MODE_1                 1
#define MODE_2                 2
#define CONFIG_REG_0           6
#define DLC_EN                 BIT(7)
#define SRAM_CYCLE_TIME_100NS  BIT(6)
#define SYSTEM_BUS_WIDTH_8     BIT(5)
#define BUFFER_WIDTH_8         BIT(4)
#define TBS1                   BIT(3)
#define TBS0                   BIT(2)
#define SRAM_BS1               BIT(1)
#define SRAM_BS0               BIT(0)
#ifndef ETH16I_TX_BUF_SIZE
#define ETH16I_TX_BUF_SIZE     3
#endif
#define TX_BUF_1x2048          0
#define TX_BUF_2x2048          1
#define TX_BUF_2x4098          2
#define TX_BUF_2x8192          3
#define CONFIG_REG_1           7
#define POWERUP                BIT(5)
#define TRANSMIT_START_REG     10
#define TRANSMIT_START_RB      2
#define TX_START               BIT(7)
#define NODE_ID_0              8
#define NODE_ID_RB             0
#define HASH_TABLE_0           8
#define HASH_TABLE_RB          1
#define BUFFER_MEM_PORT_LB     8
#define DATAPORT               BUFFER_MEM_PORT_LB
#define BUFFER_MEM_PORT_HB     9
#define COL_16_REG             11
#define HALT_ON_16             0x00
#define RETRANS_AND_HALT_ON_16 0x02
#define MAX_COL_16	       10
#define TRANSCEIVER_MODE_REG   13
#define TRANSCEIVER_MODE_RB    2
#define IO_BASE_UNLOCK	       BIT(7)
#define LOWER_SQUELCH_TRESH    BIT(6)
#define LINK_TEST_DISABLE      BIT(5)
#define AUI_SELECT             BIT(4)
#define DIS_AUTO_PORT_SEL      BIT(3)
#define FILTER_SELF_RX_REG     14
#define SKIP_RX_PACKET         BIT(2)
#define FILTER_SELF_RECEIVE    BIT(0)
#define EEPROM_CTRL_REG        16
#define EEPROM_DATA_REG        17
#define CS_0                   0x00
#define CS_1                   0x20
#define SK_0                   0x00
#define SK_1                   0x40
#define DI_0                   0x00
#define DI_1                   0x80
#define EEPROM_READ            0x80
#define E_NODEID_0             0x02
#define E_NODEID_1             0x03
#define E_NODEID_2             0x04
#define E_PORT_SELECT          0x14
#define E_PORT_BNC           0x00
#define E_PORT_DIX           0x01
#define E_PORT_TP            0x02
#define E_PORT_AUTO          0x03
#define E_PORT_FROM_EPROM    0x04
#define E_PRODUCT_CFG          0x30
#define eeprom_slow_io() do { int _i = 40; while(--_i > 0) { inb(0x80); }}while(0)
#define JUMPERLESS_CONFIG      19
#define ID_ROM_0               24
#define ID_ROM_7               31
#define RESET                  ID_ROM_0
static unsigned int eth16i_portlist[] =
{ 0x260, 0x280, 0x2A0, 0x240, 0x340, 0x320, 0x380, 0x300, 0 };
static unsigned int eth32i_portlist[] =
{ 0x1000, 0x2000, 0x3000, 0x4000, 0x5000, 0x6000, 0x7000, 0x8000,
0x9000, 0xA000, 0xB000, 0xC000, 0xD000, 0xE000, 0xF000, 0 };
static unsigned int eth16i_irqmap[] = { 9, 10, 5, 15, 0 };
#define NUM_OF_ISA_IRQS    4
static unsigned int eth32i_irqmap[] = { 3, 5, 7, 9, 10, 11, 12, 15, 0 };
#define EISA_IRQ_REG	0xc89
#define NUM_OF_EISA_IRQS   8
static unsigned int eth16i_tx_buf_map[] = { 2048, 2048, 4096, 8192 };
static unsigned int boot = 1;
#ifndef ETH16I_DEBUG
#define ETH16I_DEBUG 0
#endif
static unsigned int eth16i_debug = ETH16I_DEBUG;
struct eth16i_local {
eth16i_stats_type stats;
unsigned char     tx_started;
unsigned char     tx_buf_busy;
unsigned short    tx_queue;
unsigned short    tx_queue_len;
unsigned int      tx_buf_size;
unsigned long     open_time;
unsigned long     tx_buffered_packets;
unsigned long     col_16;
};
extern int     eth16i_probe(struct device *dev);
static int     eth16i_probe1(struct device *dev, int ioaddr);
static int     eth16i_check_signature(int ioaddr);
static int     eth16i_probe_port(int ioaddr);
static void    eth16i_set_port(int ioaddr, int porttype);
static int     eth16i_send_probe_packet(int ioaddr, unsigned char *b, int l);
static int     eth16i_receive_probe_packet(int ioaddr);
static int     eth16i_get_irq(int ioaddr);
static int     eth16i_read_eeprom(int ioaddr, int offset);
static int     eth16i_read_eeprom_word(int ioaddr);
static void    eth16i_eeprom_cmd(int ioaddr, unsigned char command);
static int     eth16i_open(struct device *dev);
static int     eth16i_close(struct device *dev);
static int     eth16i_tx(struct sk_buff *skb, struct device *dev);
static void    eth16i_rx(struct device *dev);
static void    eth16i_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void    eth16i_reset(struct device *dev);
static void    eth16i_skip_packet(struct device *dev);
static void    eth16i_multicast(struct device *dev);
static void    eth16i_select_regbank(unsigned char regbank, int ioaddr);
static void    eth16i_initialize(struct device *dev);
#if 0
static int     eth16i_set_irq(struct device *dev);
#endif
#ifdef MODULE
static ushort  eth16i_parse_mediatype(const char* s);
#endif
static struct enet_statistics *eth16i_get_stats(struct device *dev);
static char *cardname = "ICL EtherTeam 16i/32";
#ifdef HAVE_DEVLIST
/struct netdev_entry eth16i_drv =
{"eth16i", eth16i_probe1, ETH16I_IO_EXTENT, eth16i_probe_list};
#else
__initfunc(int eth16i_probe(struct device *dev))
{
int i;
int ioaddr;
int base_addr = dev ? dev->base_addr : 0;
if(eth16i_debug > 4)
printk(KERN_DEBUG "Probing started for %s\n", cardname);
if(base_addr > 0x1ff)
return eth16i_probe1(dev, base_addr);
else if(base_addr != 0)
return ENXIO;
for(i = 0; (ioaddr = eth16i_portlist[i]) ; i++) {
if(check_region(ioaddr, ETH16I_IO_EXTENT))
continue;
if(eth16i_probe1(dev, ioaddr) == 0)
return 0;
}
for(i = 0; (ioaddr = eth32i_portlist[i]) ; i++) {
if(check_region(ioaddr, ETH16I_IO_EXTENT))
continue;
if(eth16i_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
__initfunc(static int eth16i_probe1(struct device *dev, int ioaddr))
{
static unsigned version_printed = 0;
boot = 1;
if(ioaddr < 0x1000) {
if(eth16i_portlist[(inb(ioaddr + JUMPERLESS_CONFIG) & 0x07)]
!= ioaddr)
return -ENODEV;
}
if(eth16i_check_signature(ioaddr) != 0)
return -ENODEV;
eth16i_select_regbank(TRANSCEIVER_MODE_RB, ioaddr);
outb(0x00, ioaddr + TRANSCEIVER_MODE_REG);
outb(0x00, ioaddr + RESET);
BITSET(ioaddr + CONFIG_REG_0, BIT(7));
if(dev == NULL)
dev = init_etherdev(0, 0);
if( (eth16i_debug & version_printed++) == 0)
printk(KERN_INFO "%s", version);
dev->base_addr = ioaddr;
#if 0
if(dev->irq) {
if(eth16i_set_irq(dev)) {
dev->irq = eth16i_get_irq(ioaddr);
}
}
else {
#endif
dev->irq = eth16i_get_irq(ioaddr);
if (request_irq(dev->irq, (void *)&eth16i_interrupt, 0, "eth16i", dev)) {
printk(KERN_WARNING "%s: %s at %#3x, but is unusable due conflicting IRQ %d.\n",
dev->name, cardname, ioaddr, dev->irq);
return -EAGAIN;
}
#if 0
irq2dev_map[dev->irq] = dev;
#endif
printk(KERN_INFO "%s: %s at %#3x, IRQ %d, ",
dev->name, cardname, ioaddr, dev->irq);
request_region(ioaddr, ETH16I_IO_EXTENT, "eth16i");
eth16i_select_regbank(TRANSCEIVER_MODE_RB, ioaddr);
outb(0x38, ioaddr + TRANSCEIVER_MODE_REG);
eth16i_initialize(dev);
BITCLR(ioaddr + CONFIG_REG_1, POWERUP);
if(dev->priv == NULL) {
dev->priv = kmalloc(sizeof(struct eth16i_local), GFP_KERNEL);
if(dev->priv == NULL)
return -ENOMEM;
}
memset(dev->priv, 0, sizeof(struct eth16i_local));
dev->open               = eth16i_open;
dev->stop               = eth16i_close;
dev->hard_start_xmit    = eth16i_tx;
dev->get_stats          = eth16i_get_stats;
dev->set_multicast_list = &eth16i_multicast;
ether_setup(dev);
boot = 0;
return 0;
}
static void eth16i_initialize(struct device *dev)
{
int ioaddr = dev->base_addr;
int i, node_w = 0;
unsigned char node_byte = 0;
eth16i_select_regbank(NODE_ID_RB, ioaddr);
for(i = 0 ; i < 3 ; i++) {
unsigned short node_val = eth16i_read_eeprom(ioaddr, E_NODEID_0 + i);
((unsigned short *)dev->dev_addr)[i] = ntohs(node_val);
}
for(i = 0; i < 6; i++) {
outb( ((unsigned char *)dev->dev_addr)[i], ioaddr + NODE_ID_0 + i);
if(boot) {
printk("%02x", inb(ioaddr + NODE_ID_0 + i));
if(i != 5)
printk(":");
}
}
eth16i_select_regbank(HASH_TABLE_RB, ioaddr);
for(i = 0; i < 8; i++)
outb(0x00, ioaddr + HASH_TABLE_0 + i);
eth16i_select_regbank(2, ioaddr);
node_byte = 0;
node_w = eth16i_read_eeprom(ioaddr, E_PRODUCT_CFG);
if( (node_w & 0xFF00) == 0x0800)
node_byte |= BUFFER_WIDTH_8;
node_byte |= SRAM_BS1;
if( (node_w & 0x00FF) == 64)
node_byte |= SRAM_BS0;
node_byte |= DLC_EN | SRAM_CYCLE_TIME_100NS | (ETH16I_TX_BUF_SIZE << 2);
outb(node_byte, ioaddr + CONFIG_REG_0);
outb(HALT_ON_16, ioaddr + COL_16_REG);
#ifdef MODULE
#else
dev->if_port = (dev->mem_start < E_PORT_FROM_EPROM) ?
dev->mem_start : E_PORT_FROM_EPROM;
#endif
if(boot) {
char *porttype[] = {"BNC", "DIX", "TP", "AUTO", "FROM_EPROM" };
switch(dev->if_port)
{
case E_PORT_FROM_EPROM:
dev->if_port = eth16i_read_eeprom(ioaddr, E_PORT_SELECT);
break;
case E_PORT_AUTO:
dev->if_port = eth16i_probe_port(ioaddr);
break;
case E_PORT_BNC:
case E_PORT_TP:
case E_PORT_DIX:
break;
}
printk(" %s interface.\n", porttype[dev->if_port]);
eth16i_set_port(ioaddr, dev->if_port);
}
outb(MODE_2, ioaddr + RECEIVE_MODE_REG);
}
static int eth16i_probe_port(int ioaddr)
{
int i;
int retcode;
unsigned char dummy_packet[64] = { 0 };
outb(0xc0 | POWERUP, ioaddr + CONFIG_REG_1);
BITSET(ioaddr + CONFIG_REG_0, DLC_EN);
eth16i_select_regbank(NODE_ID_RB, ioaddr);
for(i = 0; i < 6; i++) {
dummy_packet[i] = inb(ioaddr + NODE_ID_0 + i);
dummy_packet[i+6] = inb(ioaddr + NODE_ID_0 + i);
}
dummy_packet[12] = 0x00;
dummy_packet[13] = 0x04;
eth16i_select_regbank(2, ioaddr);
for(i = 0; i < 3; i++) {
BITSET(ioaddr + CONFIG_REG_0, DLC_EN);
BITCLR(ioaddr + CONFIG_REG_0, DLC_EN);
eth16i_set_port(ioaddr, i);
if(eth16i_debug > 1)
printk(KERN_DEBUG "Set port number %d\n", i);
retcode = eth16i_send_probe_packet(ioaddr, dummy_packet, 64);
if(retcode == 0) {
retcode = eth16i_receive_probe_packet(ioaddr);
if(retcode != -1) {
if(eth16i_debug > 1)
printk(KERN_DEBUG "Eth16i interface port found at %d\n", i);
return i;
}
}
else {
if(eth16i_debug > 1)
printk(KERN_DEBUG "TRANSMIT_DONE timeout when probing interface port\n");
}
}
if( eth16i_debug > 1)
printk(KERN_DEBUG "Using default port\n");
return E_PORT_BNC;
}
static void eth16i_set_port(int ioaddr, int porttype)
{
unsigned short temp = 0;
eth16i_select_regbank(TRANSCEIVER_MODE_RB, ioaddr);
outb(LOOPBACK_CONTROL, ioaddr + TRANSMIT_MODE_REG);
temp |= DIS_AUTO_PORT_SEL;
switch(porttype) {
case E_PORT_BNC :
temp |= AUI_SELECT;
break;
case E_PORT_TP :
break;
case E_PORT_DIX :
temp |= AUI_SELECT;
BITSET(ioaddr + TRANSMIT_MODE_REG, CONTROL_OUTPUT);
break;
}
outb(temp, ioaddr + TRANSCEIVER_MODE_REG);
if(eth16i_debug > 1) {
printk(KERN_DEBUG "TRANSMIT_MODE_REG = %x\n", inb(ioaddr + TRANSMIT_MODE_REG));
printk(KERN_DEBUG "TRANSCEIVER_MODE_REG = %x\n",
inb(ioaddr+TRANSCEIVER_MODE_REG));
}
}
static int eth16i_send_probe_packet(int ioaddr, unsigned char *b, int l)
{
int starttime;
outb(0xff, ioaddr + TX_STATUS_REG);
outw(l, ioaddr + DATAPORT);
outsw(ioaddr + DATAPORT, (unsigned short *)b, (l + 1) >> 1);
starttime = jiffies;
outb(TX_START | 1, ioaddr + TRANSMIT_START_REG);
while( (inb(ioaddr + TX_STATUS_REG) & 0x80) == 0) {
if( (jiffies - starttime) > TX_TIMEOUT) {
return -1;
}
}
return 0;
}
static int eth16i_receive_probe_packet(int ioaddr)
{
int starttime;
starttime = jiffies;
while((inb(ioaddr + TX_STATUS_REG) & 0x20) == 0) {
if( (jiffies - starttime) > TX_TIMEOUT) {
if(eth16i_debug > 1)
printk(KERN_DEBUG "Timeout occured waiting transmit packet received\n");
starttime = jiffies;
while((inb(ioaddr + RX_STATUS_REG) & 0x80) == 0) {
if( (jiffies - starttime) > TX_TIMEOUT) {
if(eth16i_debug > 1)
printk(KERN_DEBUG "Timeout occured waiting receive packet\n");
return -1;
}
}
if(eth16i_debug > 1)
printk(KERN_DEBUG "RECEIVE_PACKET\n");
return(0);
}
}
if(eth16i_debug > 1) {
printk(KERN_DEBUG "TRANSMIT_PACKET_RECEIVED %x\n", inb(ioaddr + TX_STATUS_REG));
printk(KERN_DEBUG "RX_STATUS_REG = %x\n", inb(ioaddr + RX_STATUS_REG));
}
return(0);
}
#if 0
static int eth16i_set_irq(struct device* dev)
{
const int ioaddr = dev->base_addr;
const int irq = dev->irq;
int i = 0;
if(ioaddr < 0x1000) {
while(eth16i_irqmap[i] && eth16i_irqmap[i] != irq)
i++;
if(i < NUM_OF_ISA_IRQS) {
u8 cbyte = inb(ioaddr + JUMPERLESS_CONFIG);
cbyte = (cbyte & 0x3F) | (i << 6);
outb(cbyte, ioaddr + JUMPERLESS_CONFIG);
return 0;
}
}
else {
printk(KERN_NOTICE "%s: EISA Interrupt cannot be set. Use EISA Configuration utility.\n", dev->name);
}
return -1;
}
#endif
static int eth16i_get_irq(int ioaddr)
{
unsigned char cbyte;
if( ioaddr < 0x1000) {
cbyte = inb(ioaddr + JUMPERLESS_CONFIG);
return( eth16i_irqmap[ ((cbyte & 0xC0) >> 6) ] );
} else {
unsigned short index = 0;
cbyte = inb(ioaddr + EISA_IRQ_REG);
while( (cbyte & 0x01) == 0) {
cbyte = cbyte >> 1;
index++;
}
return( eth32i_irqmap[ index ] );
}
}
static int eth16i_check_signature(int ioaddr)
{
int i;
unsigned char creg[4] = { 0 };
for(i = 0; i < 4 ; i++) {
creg[i] = inb(ioaddr + TRANSMIT_MODE_REG + i);
if(eth16i_debug > 1)
printk("eth16i: read signature byte %x at %x\n",
creg[i],
ioaddr + TRANSMIT_MODE_REG + i);
}
creg[0] &= 0x0F;
creg[2] &= 0x7F;
#if 0
if( ! ((creg[0] == 0x06) && (creg[1] == 0x41)) ) {
if(creg[1] != 0x42)
return -1;
}
#endif
if( !((creg[2] == 0x36) && (creg[3] == 0xE0)) ) {
creg[2] &= 0x40;
creg[3] &= 0x03;
if( !((creg[2] == 0x40) && (creg[3] == 0x00)) )
return -1;
}
if(eth16i_read_eeprom(ioaddr, E_NODEID_0) != 0)
return -1;
if((eth16i_read_eeprom(ioaddr, E_NODEID_1) & 0xFF00) != 0x4B00)
return -1;
return 0;
}
static int eth16i_read_eeprom(int ioaddr, int offset)
{
int data = 0;
eth16i_eeprom_cmd(ioaddr, EEPROM_READ | offset);
outb(CS_1, ioaddr + EEPROM_CTRL_REG);
data = eth16i_read_eeprom_word(ioaddr);
outb(CS_0 | SK_0, ioaddr + EEPROM_CTRL_REG);
return(data);
}
static int eth16i_read_eeprom_word(int ioaddr)
{
int i;
int data = 0;
for(i = 16; i > 0; i--) {
outb(CS_1 | SK_0, ioaddr + EEPROM_CTRL_REG);
eeprom_slow_io();
outb(CS_1 | SK_1, ioaddr + EEPROM_CTRL_REG);
eeprom_slow_io();
data = (data << 1) |
((inb(ioaddr + EEPROM_DATA_REG) & DI_1) ? 1 : 0);
eeprom_slow_io();
}
return(data);
}
static void eth16i_eeprom_cmd(int ioaddr, unsigned char command)
{
int i;
outb(CS_0 | SK_0, ioaddr + EEPROM_CTRL_REG);
outb(DI_0, ioaddr + EEPROM_DATA_REG);
outb(CS_1 | SK_0, ioaddr + EEPROM_CTRL_REG);
outb(DI_1, ioaddr + EEPROM_DATA_REG);
outb(CS_1 | SK_1, ioaddr + EEPROM_CTRL_REG);
for(i = 7; i >= 0; i--) {
short cmd = ( (command & (1 << i)) ? DI_1 : DI_0 );
outb(cmd, ioaddr + EEPROM_DATA_REG);
outb(CS_1 | SK_0, ioaddr + EEPROM_CTRL_REG);
eeprom_slow_io();
outb(CS_1 | SK_1, ioaddr + EEPROM_CTRL_REG);
eeprom_slow_io();
}
}
static int eth16i_open(struct device *dev)
{
struct eth16i_local *lp = (struct eth16i_local *)dev->priv;
int ioaddr = dev->base_addr;
outb(0xc0 | POWERUP, ioaddr + CONFIG_REG_1);
eth16i_initialize(dev);
lp->tx_buf_size = eth16i_tx_buf_map[ETH16I_TX_BUF_SIZE & 0x03];
if(eth16i_debug > 0)
printk(KERN_DEBUG "%s: transmit buffer size %d\n",
dev->name, lp->tx_buf_size);
BITCLR(ioaddr + CONFIG_REG_0, DLC_EN);
eth16i_select_regbank(2, ioaddr);
lp->open_time = jiffies;
lp->tx_started = 0;
lp->tx_queue = 0;
lp->tx_queue_len = 0;
outw(ETH16I_INTR_ON, ioaddr + TX_INTR_REG);
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
MOD_INC_USE_COUNT;
return 0;
}
static int eth16i_close(struct device *dev)
{
struct eth16i_local *lp = (struct eth16i_local *)dev->priv;
int ioaddr = dev->base_addr;
eth16i_reset(dev);
outw(ETH16I_INTR_OFF, ioaddr + TX_INTR_REG);
dev->start = 0;
dev->tbusy = 1;
lp->open_time = 0;
BITSET(ioaddr + CONFIG_REG_0, DLC_EN);
outb(0x00, ioaddr + CONFIG_REG_1);
MOD_DEC_USE_COUNT;
return 0;
}
static int eth16i_tx(struct sk_buff *skb, struct device *dev)
{
struct eth16i_local *lp = (struct eth16i_local *)dev->priv;
int ioaddr = dev->base_addr;
int status = 0;
if(dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if(tickssofar < TX_TIMEOUT)
return 1;
outw(ETH16I_INTR_OFF, ioaddr + TX_INTR_REG);
printk(KERN_WARNING "%s: transmit timed out with status %04x, %s ?\n",
dev->name,
inw(ioaddr + TX_STATUS_REG),
(inb(ioaddr + TX_STATUS_REG) & TX_DONE) ?
"IRQ conflict" : "network cable problem");
dev->trans_start = jiffies;
if(eth16i_debug > 0) {
printk(KERN_DEBUG "%s: timeout: %02x %02x %02x %02x %02x %02x %02x %02x.\n",
dev->name, inb(ioaddr + 0),
inb(ioaddr + 1), inb(ioaddr + 2),
inb(ioaddr + 3), inb(ioaddr + 4),
inb(ioaddr + 5),
inb(ioaddr + 6), inb(ioaddr + 7));
printk(KERN_DEBUG "%s: transmit start reg: %02x. collision reg %02x\n",
dev->name, inb(ioaddr + TRANSMIT_START_REG),
inb(ioaddr + COL_16_REG));
printk(KERN_DEBUG "lp->tx_queue = %d\n", lp->tx_queue);
printk(KERN_DEBUG "lp->tx_queue_len = %d\n", lp->tx_queue_len);
printk(KERN_DEBUG "lp->tx_started = %d\n", lp->tx_started);
}
lp->stats.tx_errors++;
eth16i_reset(dev);
dev->trans_start = jiffies;
outw(ETH16I_INTR_ON, ioaddr + TX_INTR_REG);
}
if(skb == NULL) {
#if LINUX_VERSION_CODE < 0x020100
dev_tint(dev);
#endif
if(eth16i_debug > 0)
printk(KERN_WARNING "%s: Missed tx-done interrupt.\n", dev->name);
return 0;
}
set_bit(0, (void *)&lp->tx_buf_busy);
outw(ETH16I_INTR_OFF, ioaddr + TX_INTR_REG);
if(test_and_set_bit(0, (void *)&dev->tbusy) != 0) {
printk(KERN_WARNING "%s: Transmitter access conflict.\n", dev->name);
status = -1;
}
else {
ushort length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
unsigned char *buf = skb->data;
if( (length + 2) > (lp->tx_buf_size - lp->tx_queue_len)) {
if(eth16i_debug > 0)
printk(KERN_WARNING "%s: Transmit buffer full.\n", dev->name);
}
else {
outw(length, ioaddr + DATAPORT);
if( ioaddr < 0x1000 )
outsw(ioaddr + DATAPORT, buf, (length + 1) >> 1);
else {
unsigned char frag = length % 4;
outsl(ioaddr + DATAPORT, buf, length >> 2);
if( frag != 0 ) {
outsw(ioaddr + DATAPORT, (buf + (length & 0xFFFC)), 1);
if( frag == 3 )
outsw(ioaddr + DATAPORT,
(buf + (length & 0xFFFC) + 2), 1);
}
}
lp->tx_buffered_packets++;
lp->tx_queue++;
lp->tx_queue_len += length + 2;
}
lp->tx_buf_busy = 0;
if(lp->tx_started == 0) {
outb(TX_START | lp->tx_queue, ioaddr + TRANSMIT_START_REG);
lp->tx_queue = 0;
lp->tx_queue_len = 0;
dev->trans_start = jiffies;
lp->tx_started = 1;
dev->tbusy = 0;
}
else if(lp->tx_queue_len < lp->tx_buf_size - (ETH_FRAME_LEN + 2)) {
dev->tbusy = 0;
}
outw(ETH16I_INTR_ON, ioaddr + TX_INTR_REG);
status = 0;
}
#if LINUX_VERSION_CODE >= 0x020100
dev_kfree_skb(skb);
#else
dev_kfree_skb(skb, FREE_WRITE);
#endif
return status;
}
static void eth16i_rx(struct device *dev)
{
struct eth16i_local *lp = (struct eth16i_local *)dev->priv;
int ioaddr = dev->base_addr;
int boguscount = MAX_RX_LOOP;
while( (inb(ioaddr + RECEIVE_MODE_REG) & RX_BUFFER_EMPTY) == 0) {
ushort status = inw(ioaddr + DATAPORT);
ushort pkt_len = inw(ioaddr + DATAPORT);
if(eth16i_debug > 4)
printk(KERN_DEBUG "%s: Receiving packet mode %02x status %04x.\n",
dev->name,
inb(ioaddr + RECEIVE_MODE_REG), status);
if( !(status & PKT_GOOD) ) {
lp->stats.rx_errors++;
if( (pkt_len < ETH_ZLEN) || (pkt_len > ETH_FRAME_LEN) ) {
lp->stats.rx_length_errors++;
eth16i_reset(dev);
return;
}
else {
eth16i_skip_packet(dev);
lp->stats.rx_dropped++;
}
}
else {
struct sk_buff *skb;
skb = dev_alloc_skb(pkt_len + 3);
if( skb == NULL ) {
printk(KERN_WARNING "%s: Could'n allocate memory for packet (len %d)\n",
dev->name, pkt_len);
eth16i_skip_packet(dev);
lp->stats.rx_dropped++;
break;
}
skb->dev = dev;
skb_reserve(skb,2);
if(ioaddr < 0x1000)
insw(ioaddr + DATAPORT, skb_put(skb, pkt_len),
(pkt_len + 1) >> 1);
else {
unsigned char *buf = skb_put(skb, pkt_len);
unsigned char frag = pkt_len % 4;
insl(ioaddr + DATAPORT, buf, pkt_len >> 2);
if(frag != 0) {
unsigned short rest[2];
rest[0] = inw( ioaddr + DATAPORT );
if(frag == 3)
rest[1] = inw( ioaddr + DATAPORT );
memcpy(buf + (pkt_len & 0xfffc), (char *)rest, frag);
}
}
skb->protocol=eth_type_trans(skb, dev);
netif_rx(skb);
lp->stats.rx_packets++;
if( eth16i_debug > 5 ) {
int i;
printk(KERN_DEBUG "%s: Received packet of length %d.\n",
dev->name, pkt_len);
for(i = 0; i < 14; i++)
printk(KERN_DEBUG " %02x", skb->data[i]);
printk(KERN_DEBUG ".\n");
}
}
if(--boguscount <= 0)
break;
}
#if 0
{
int i;
for(i = 0; i < 20; i++) {
if( (inb(ioaddr+RECEIVE_MODE_REG) & RX_BUFFER_EMPTY) ==
RX_BUFFER_EMPTY)
break;
inw(ioaddr + DATAPORT);
outb(SKIP_RX_PACKET, ioaddr + FILTER_SELF_RX_REG);
}
if(eth16i_debug > 1)
printk(KERN_DEBUG "%s: Flushed receive buffer.\n", dev->name);
}
#endif
return;
}
static void eth16i_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct device *dev = dev_id;
struct eth16i_local *lp;
int ioaddr = 0,
status;
if(dev == NULL) {
printk(KERN_WARNING "eth16i_interrupt(): irq %d for unknown device. \n", irq);
return;
}
outw(ETH16I_INTR_OFF, ioaddr + TX_INTR_REG);
set_bit(0, (void *)&dev->tbusy);
if(dev->interrupt)
printk(KERN_WARNING "%s: Re-entering the interrupt handler.\n", dev->name);
dev->interrupt = 1;
ioaddr = dev->base_addr;
lp = (struct eth16i_local *)dev->priv;
status = inw(ioaddr + TX_STATUS_REG);
outw(status, ioaddr + TX_STATUS_REG);
if(eth16i_debug > 3)
printk(KERN_DEBUG "%s: Interrupt with status %04x.\n", dev->name, status);
if( status & 0x7f00 ) {
lp->stats.rx_errors++;
if(status & (BUS_RD_ERR << 8) )
printk(KERN_WARNING "%s: Bus read error.\n",dev->name);
if(status & (SHORT_PKT_ERR << 8) )   lp->stats.rx_length_errors++;
if(status & (ALIGN_ERR << 8) )       lp->stats.rx_frame_errors++;
if(status & (CRC_ERR << 8) )	    lp->stats.rx_crc_errors++;
if(status & (RX_BUF_OVERFLOW << 8) ) lp->stats.rx_over_errors++;
}
if( status & 0x001a) {
lp->stats.tx_errors++;
if(status & CR_LOST) lp->stats.tx_carrier_errors++;
if(status & TX_JABBER_ERR) lp->stats.tx_window_errors++;
#if 0
if(status & COLLISION) {
lp->stats.collisions +=
((inb(ioaddr+TRANSMIT_MODE_REG) & 0xF0) >> 4);
}
#endif
if(status & COLLISIONS_16) {
if(lp->col_16 < MAX_COL_16) {
lp->col_16++;
lp->stats.collisions++;
outb(0x02, ioaddr + COL_16_REG);
}
else {
printk(KERN_WARNING "%s: bailing out due to many consecutive 16-in-a-row collisions. Network cable problem?\n", dev->name);
}
}
}
if( status & 0x00ff ) {
if(status & TX_DONE) {
lp->stats.tx_packets = lp->tx_buffered_packets;
lp->col_16 = 0;
if(lp->tx_queue) {
outb(TX_START | lp->tx_queue, ioaddr + TRANSMIT_START_REG);
lp->tx_queue = 0;
lp->tx_queue_len = 0;
lp->tx_started = 1;
dev->trans_start = jiffies;
mark_bh(NET_BH);
}
else {
lp->tx_started = 0;
mark_bh(NET_BH);
}
}
}
if( ( status & 0x8000 ) ||
( (inb(ioaddr + RECEIVE_MODE_REG) & RX_BUFFER_EMPTY) == 0) ) {
eth16i_rx(dev);
}
dev->interrupt = 0;
outw(ETH16I_INTR_ON, ioaddr + TX_INTR_REG);
if(lp->tx_queue_len < lp->tx_buf_size - (ETH_FRAME_LEN + 2)) {
dev->tbusy = 0;
}
return;
}
static void eth16i_skip_packet(struct device *dev)
{
int ioaddr = dev->base_addr;
inw(ioaddr + DATAPORT);
inw(ioaddr + DATAPORT);
inw(ioaddr + DATAPORT);
outb(SKIP_RX_PACKET, ioaddr + FILTER_SELF_RX_REG);
while( inb( ioaddr + FILTER_SELF_RX_REG ) != 0);
}
static void eth16i_reset(struct device *dev)
{
struct eth16i_local *lp = (struct eth16i_local *)dev->priv;
int ioaddr = dev->base_addr;
if(eth16i_debug > 1)
printk(KERN_DEBUG "%s: Resetting device.\n", dev->name);
BITSET(ioaddr + CONFIG_REG_0, DLC_EN);
outw(0xffff, ioaddr + TX_STATUS_REG);
eth16i_select_regbank(2, ioaddr);
lp->tx_started = 0;
lp->tx_buf_busy = 0;
lp->tx_queue = 0;
lp->tx_queue_len = 0;
dev->interrupt = 0;
dev->start = 1;
dev->tbusy = 0;
BITCLR(ioaddr + CONFIG_REG_0, DLC_EN);
}
static void eth16i_multicast(struct device *dev)
{
int ioaddr = dev->base_addr;
if(dev->mc_count || dev->flags&(IFF_ALLMULTI|IFF_PROMISC))
{
dev->flags|=IFF_PROMISC;
outb(3, ioaddr + RECEIVE_MODE_REG);
} else {
outb(2, ioaddr + RECEIVE_MODE_REG);
}
}
static struct enet_statistics *eth16i_get_stats(struct device *dev)
{
struct eth16i_local *lp = (struct eth16i_local *)dev->priv;
return &lp->stats;
}
static void eth16i_select_regbank(unsigned char banknbr, int ioaddr)
{
unsigned char data;
data = inb(ioaddr + CONFIG_REG_1);
outb( ((data & 0xF3) | ( (banknbr & 0x03) << 2)), ioaddr + CONFIG_REG_1);
}
#ifdef MODULE
static ushort eth16i_parse_mediatype(const char* s)
{
if(!s)
return E_PORT_FROM_EPROM;
if (!strncmp(s, "bnc", 3))
return E_PORT_BNC;
else if (!strncmp(s, "tp", 2))
return E_PORT_TP;
else if (!strncmp(s, "dix", 3))
return E_PORT_DIX;
else if (!strncmp(s, "auto", 4))
return E_PORT_AUTO;
else
return E_PORT_FROM_EPROM;
}
#define MAX_ETH16I_CARDS 4
#define NAMELEN          8
static char namelist[NAMELEN * MAX_ETH16I_CARDS] = { 0, };
static struct device dev_eth16i[MAX_ETH16I_CARDS] = {
{
NULL,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, NULL
},
};
static int ioaddr[MAX_ETH16I_CARDS] = { 0, };
#if 0
static int irq[MAX_ETH16I_CARDS] = { 0, };
#endif
static char* mediatype[MAX_ETH16I_CARDS] = { 0, };
static int debug = -1;
#if (LINUX_VERSION_CODE >= 0x20115)
MODULE_AUTHOR("Mika Kuoppala <miku@iki.fi>");
MODULE_DESCRIPTION("ICL EtherTeam 16i/32 driver");
MODULE_PARM(ioaddr, "1-" __MODULE_STRING(MAX_ETH16I_CARDS) "i");
MODULE_PARM_DESC(ioaddr, "eth16i io base address");
#if 0
MODULE_PARM(irq, "1-" __MODULE_STRING(MAX_ETH16I_CARDS) "i");
MODULE_PARM_DESC(irq, "eth16i interrupt request number");
#endif
MODULE_PARM(mediatype, "1-" __MODULE_STRING(MAX_ETH16I_CARDS) "s");
MODULE_PARM_DESC(mediatype, "eth16i interfaceport mediatype");
MODULE_PARM(debug, "i");
MODULE_PARM_DESC(debug, "eth16i debug level (0-4)");
#endif
int init_module(void)
{
int this_dev, found = 0;
for(this_dev = 0; this_dev < MAX_ETH16I_CARDS; this_dev++)
{
struct device *dev = &dev_eth16i[this_dev];
dev->name = namelist + (NAMELEN*this_dev);
dev->irq = 0;
dev->base_addr = ioaddr[this_dev];
dev->init = eth16i_probe;
if(debug != -1)
eth16i_debug = debug;
if(eth16i_debug > 1)
printk(KERN_NOTICE "eth16i(%d): interface type %s\n", this_dev, mediatype[this_dev] ? mediatype[this_dev] : "none" );
dev->if_port = eth16i_parse_mediatype(mediatype[this_dev]);
if(ioaddr[this_dev] == 0)
{
if(this_dev != 0) break;
printk(KERN_NOTICE "eth16i.c: Presently autoprobing (not recommended) for a single card.\n");
}
if(register_netdev(dev) != 0)
{
printk(KERN_WARNING "eth16i.c No Eth16i card found (i/o = 0x%x).\n",
ioaddr[this_dev]);
if(found != 0) return 0;
return -ENXIO;
}
found++;
}
return 0;
}
void cleanup_module(void)
{
int this_dev;
for(this_dev = 0; this_dev < MAX_ETH16I_CARDS; this_dev++)
{
struct device* dev = &dev_eth16i[this_dev];
if(dev->priv != NULL)
{
unregister_netdev(dev);
kfree(dev->priv);
dev->priv = NULL;
free_irq(dev->irq, dev);
release_region(dev->base_addr, ETH16I_IO_EXTENT);
}
}
}
#endif