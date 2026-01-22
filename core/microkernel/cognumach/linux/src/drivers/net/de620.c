static const char *version =
"de620.c: $Revision: 1.1 $,  Bjorn Ekwall <bj0rn@blox.se>\n";
#define DE620_CLONE 0
#ifndef READ_DELAY
#define READ_DELAY 100
#endif
#ifndef WRITE_DELAY
#define WRITE_DELAY 100
#endif
#ifdef LOWSPEED
#endif
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/string.h>
#include <linux/interrupt.h>
#include <linux/ioport.h>
#include <asm/io.h>
#include <linux/in.h>
#include <linux/ptrace.h>
#include <asm/system.h>
#include <linux/errno.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include "de620.h"
#define netstats enet_statistics
typedef unsigned char byte;
#ifndef DE620_IO
#define DE620_IO 0x378
#endif
#ifndef DE620_IRQ
#define DE620_IRQ	7
#endif
#define DATA_PORT	(dev->base_addr)
#define STATUS_PORT	(dev->base_addr + 1)
#define COMMAND_PORT	(dev->base_addr + 2)
#define RUNT 60
#define GIANT 1514
#ifdef DE620_DEBUG
#define PRINTK(x) if (de620_debug >= 2) printk x
#else
#define DE620_DEBUG 0
#define PRINTK(x)
#endif
static int bnc = 0;
static int utp = 0;
static int io  = DE620_IO;
static int irq = DE620_IRQ;
static int clone = DE620_CLONE;
static unsigned int de620_debug = DE620_DEBUG;
static int	de620_open(struct device *);
static int	de620_close(struct device *);
static struct netstats *get_stats(struct device *);
static void	de620_set_multicast_list(struct device *);
static int	de620_start_xmit(struct sk_buff *, struct device *);
static void	de620_interrupt(int, void *, struct pt_regs *);
static int	de620_rx_intr(struct device *);
static int	adapter_init(struct device *);
int		de620_probe(struct device *);
static int	read_eeprom(struct device *);
#define SCR_DEF NIBBLEMODE |INTON | SLEEP | AUTOTX
#define	TCR_DEF RXPB
#define DE620_RX_START_PAGE 12
#define DEF_NIC_CMD IRQEN | ICEN | DS1
static volatile byte	NIC_Cmd;
static volatile byte	next_rx_page;
static byte		first_rx_page;
static byte		last_rx_page;
static byte		EIPRegister;
static struct nic {
byte	NodeID[6];
byte	RAM_Size;
byte	Model;
byte	Media;
byte	SCR;
} nic_data;
#define de620_tx_buffs(dd) (inb(STATUS_PORT) & (TXBF0 | TXBF1))
#define de620_flip_ds(dd) NIC_Cmd ^= DS0 | DS1; outb(NIC_Cmd, COMMAND_PORT);
#ifdef COUNT_LOOPS
static int tot_cnt;
#endif
static inline byte
de620_ready(struct device *dev)
{
byte value;
register short int cnt = 0;
while ((((value = inb(STATUS_PORT)) & READY) == 0) && (cnt <= 1000))
++cnt;
#ifdef COUNT_LOOPS
tot_cnt += cnt;
#endif
return value & 0xf0;
}
static inline void
de620_send_command(struct device *dev, byte cmd)
{
de620_ready(dev);
if (cmd == W_DUMMY)
outb(NIC_Cmd, COMMAND_PORT);
outb(cmd, DATA_PORT);
outb(NIC_Cmd ^ CS0, COMMAND_PORT);
de620_ready(dev);
outb(NIC_Cmd, COMMAND_PORT);
}
static inline void
de620_put_byte(struct device *dev, byte value)
{
de620_ready(dev);
outb(value, DATA_PORT);
de620_flip_ds(dev);
}
static inline byte
de620_read_byte(struct device *dev)
{
byte value;
value = de620_ready(dev);
de620_flip_ds(dev);
value |= de620_ready(dev) >> 4;
return value;
}
static inline void
de620_write_block(struct device *dev, byte *buffer, int count)
{
#ifndef LOWSPEED
byte uflip = NIC_Cmd ^ (DS0 | DS1);
byte dflip = NIC_Cmd;
#else
#ifdef COUNT_LOOPS
int bytes = count;
#endif
#endif
#ifdef LOWSPEED
#ifdef COUNT_LOOPS
tot_cnt = 0;
#endif
for ( ; count > 0; --count, ++buffer) {
de620_put_byte(dev,*buffer);
}
de620_send_command(dev,W_DUMMY);
#ifdef COUNT_LOOPS
printk("WRITE(%d)\n", tot_cnt/((bytes?bytes:1)));
#endif
#else
for ( ; count > 0; count -=2) {
outb(*buffer++, DATA_PORT);
outb(uflip, COMMAND_PORT);
outb(*buffer++, DATA_PORT);
outb(dflip, COMMAND_PORT);
}
de620_send_command(dev,W_DUMMY);
#endif
}
static inline void
de620_read_block(struct device *dev, byte *data, int count)
{
#ifndef LOWSPEED
byte value;
byte uflip = NIC_Cmd ^ (DS0 | DS1);
byte dflip = NIC_Cmd;
#else
#ifdef COUNT_LOOPS
int bytes = count;
tot_cnt = 0;
#endif
#endif
#ifdef LOWSPEED
while (count-- > 0) {
*data++ = de620_read_byte(dev);
de620_flip_ds(dev);
}
#ifdef COUNT_LOOPS
printk("READ(%d)\n", tot_cnt/(2*(bytes?bytes:1)));
#endif
#else
while (count-- > 0) {
value = inb(STATUS_PORT) & 0xf0;
outb(uflip, COMMAND_PORT);
*data++ = value | inb(STATUS_PORT) >> 4;
outb(dflip , COMMAND_PORT);
}
#endif
}
static inline void
de620_set_delay(struct device *dev)
{
de620_ready(dev);
outb(W_DFR, DATA_PORT);
outb(NIC_Cmd ^ CS0, COMMAND_PORT);
de620_ready(dev);
#ifdef LOWSPEED
outb(WRITE_DELAY, DATA_PORT);
#else
outb(0, DATA_PORT);
#endif
de620_flip_ds(dev);
de620_ready(dev);
#ifdef LOWSPEED
outb(READ_DELAY, DATA_PORT);
#else
outb(0, DATA_PORT);
#endif
de620_flip_ds(dev);
}
static inline void
de620_set_register(struct device *dev, byte reg, byte value)
{
de620_ready(dev);
outb(reg, DATA_PORT);
outb(NIC_Cmd ^ CS0, COMMAND_PORT);
de620_put_byte(dev, value);
}
static inline byte
de620_get_register(struct device *dev, byte reg)
{
byte value;
de620_send_command(dev,reg);
value = de620_read_byte(dev);
de620_send_command(dev,W_DUMMY);
return value;
}
static int
de620_open(struct device *dev)
{
if (request_irq(dev->irq, de620_interrupt, 0, "de620", NULL)) {
printk ("%s: unable to get IRQ %d\n", dev->name, dev->irq);
return 1;
}
irq2dev_map[dev->irq] = dev;
MOD_INC_USE_COUNT;
if (adapter_init(dev)) {
return 1;
}
dev->start = 1;
return 0;
}
static int
de620_close(struct device *dev)
{
de620_set_register(dev, W_TCR, RXOFF);
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = NULL;
dev->start = 0;
MOD_DEC_USE_COUNT;
return 0;
}
static struct netstats *
get_stats(struct device *dev)
{
return (struct netstats *)(dev->priv);
}
static void de620_set_multicast_list(struct device *dev)
{
if (dev->mc_count || dev->flags&(IFF_ALLMULTI|IFF_PROMISC))
{
dev->flags|=IFF_PROMISC;
de620_set_register(dev, W_TCR, (TCR_DEF & ~RXPBM) | RXALL);
}
else
{
de620_set_register(dev, W_TCR, TCR_DEF);
}
}
static int
de620_start_xmit(struct sk_buff *skb, struct device *dev)
{
unsigned long flags;
int len;
int tickssofar;
byte *buffer = skb->data;
byte using_txbuf;
if (skb == NULL) {
dev_tint(dev);
return 0;
}
using_txbuf = de620_tx_buffs(dev);
dev->tbusy = (using_txbuf == (TXBF0 | TXBF1));
if (dev->tbusy) {
tickssofar = jiffies - dev->trans_start;
if (tickssofar < 5)
return 1;
printk("%s: transmit timed out (%d), %s?\n",
dev->name,
tickssofar,
"network cable problem"
);
if (adapter_init(dev))
return 1;
}
if ((len = skb->len) < RUNT)
len = RUNT;
if (len & 1)
++len;
save_flags(flags);
cli();
PRINTK(("de620_start_xmit: len=%d, bufs 0x%02x\n",
(int)skb->len, using_txbuf));
switch (using_txbuf) {
default:
case TXBF1:
de620_send_command(dev,W_CR | RW0);
using_txbuf |= TXBF0;
break;
case TXBF0:
de620_send_command(dev,W_CR | RW1);
using_txbuf |= TXBF1;
break;
case (TXBF0 | TXBF1):
printk("de620: Ouch! No tx-buffer available!\n");
restore_flags(flags);
return 1;
break;
}
de620_write_block(dev, buffer, len);
dev->trans_start = jiffies;
dev->tbusy = (using_txbuf == (TXBF0 | TXBF1));
((struct netstats *)(dev->priv))->tx_packets++;
restore_flags(flags);
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
static void
de620_interrupt(int irq_in, void *dev_id, struct pt_regs *regs)
{
struct device *dev = irq2dev_map[irq_in];
byte irq_status;
int bogus_count = 0;
int again = 0;
if ((dev == NULL) || (irq != irq_in)) {
printk("%s: bogus interrupt %d\n", dev?dev->name:"de620", irq_in);
return;
}
cli();
dev->interrupt = 1;
irq_status = de620_get_register(dev, R_STS);
PRINTK(("de620_interrupt (%2.2X)\n", irq_status));
if (irq_status & RXGOOD) {
do {
again = de620_rx_intr(dev);
PRINTK(("again=%d\n", again));
}
while (again && (++bogus_count < 100));
}
dev->tbusy = (de620_tx_buffs(dev) == (TXBF0 | TXBF1));
dev->interrupt = 0;
sti();
return;
}
static int
de620_rx_intr(struct device *dev)
{
struct header_buf {
byte		status;
byte		Rx_NextPage;
unsigned short	Rx_ByteCount;
} header_buf;
struct sk_buff *skb;
int size;
byte *buffer;
byte pagelink;
byte curr_page;
PRINTK(("de620_rx_intr: next_rx_page = %d\n", next_rx_page));
de620_send_command(dev, W_CR | RRN);
de620_set_register(dev, W_RSA1, next_rx_page);
de620_set_register(dev, W_RSA0, 0);
de620_read_block(dev, (byte *)&header_buf, sizeof(struct header_buf));
PRINTK(("page status=0x%02x, nextpage=%d, packetsize=%d\n",
header_buf.status, header_buf.Rx_NextPage, header_buf.Rx_ByteCount));
pagelink = header_buf.Rx_NextPage;
if ((pagelink < first_rx_page) || (last_rx_page < pagelink)) {
printk("%s: Ring overrun? Restoring...\n", dev->name);
adapter_init(dev);
((struct netstats *)(dev->priv))->rx_over_errors++;
return 0;
}
pagelink = next_rx_page +
((header_buf.Rx_ByteCount + (4 - 1 + 0x100)) >> 8);
if (pagelink > last_rx_page)
pagelink -= (last_rx_page - first_rx_page + 1);
if (pagelink != header_buf.Rx_NextPage) {
printk("%s: Page link out of sync! Restoring...\n", dev->name);
next_rx_page = header_buf.Rx_NextPage;
de620_send_command(dev, W_DUMMY);
de620_set_register(dev, W_NPRF, next_rx_page);
((struct netstats *)(dev->priv))->rx_over_errors++;
return 0;
}
next_rx_page = pagelink;
size = header_buf.Rx_ByteCount - 4;
if ((size < RUNT) || (GIANT < size)) {
printk("%s: Illegal packet size: %d!\n", dev->name, size);
}
else {
skb = dev_alloc_skb(size+2);
if (skb == NULL) {
printk("%s: Couldn't allocate a sk_buff of size %d.\n",
dev->name, size);
((struct netstats *)(dev->priv))->rx_dropped++;
}
else {
skb_reserve(skb,2);
skb->dev = dev;
skb->free = 1;
buffer = skb_put(skb,size);
de620_read_block(dev, buffer, size);
PRINTK(("Read %d bytes\n", size));
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
((struct netstats *)(dev->priv))->rx_packets++;
}
}
curr_page = de620_get_register(dev, R_CPR);
de620_set_register(dev, W_NPRF, next_rx_page);
PRINTK(("next_rx_page=%d CPR=%d\n", next_rx_page, curr_page));
return (next_rx_page != curr_page);
}
static int
adapter_init(struct device *dev)
{
int i;
static int was_down = 0;
if ((nic_data.Model == 3) || (nic_data.Model == 0)) {
EIPRegister = NCTL0;
if (nic_data.Media != 1)
EIPRegister |= NIS0;
}
else if (nic_data.Model == 2) {
EIPRegister = NCTL0 | NIS0;
}
if (utp)
EIPRegister = NCTL0 | NIS0;
if (bnc)
EIPRegister = NCTL0;
de620_send_command(dev, W_CR | RNOP | CLEAR);
de620_send_command(dev, W_CR | RNOP);
de620_set_register(dev, W_SCR, SCR_DEF);
de620_set_register(dev, W_TCR, RXOFF);
for (i = 0; i < 6; ++i) {
de620_set_register(dev, W_PAR0 + i, dev->dev_addr[i]);
}
de620_set_register(dev, W_EIP, EIPRegister);
next_rx_page = first_rx_page = DE620_RX_START_PAGE;
if (nic_data.RAM_Size)
last_rx_page = nic_data.RAM_Size - 1;
else
last_rx_page = 255;
de620_set_register(dev, W_SPR, first_rx_page);
de620_set_register(dev, W_EPR, last_rx_page);
de620_set_register(dev, W_CPR, first_rx_page);
de620_send_command(dev, W_NPR | first_rx_page);
de620_send_command(dev, W_DUMMY);
de620_set_delay(dev);
#define CHECK_MASK (  0 | TXSUC |  T16  |  0  | RXCRC | RXSHORT |  0  |  0  )
#define CHECK_OK   (  0 |   0   |  0    |  0  |   0   |   0     |  0  |  0  )
if (((i = de620_get_register(dev, R_STS)) & CHECK_MASK) != CHECK_OK) {
printk("Something has happened to the DE-620!  Please check it"
#ifdef SHUTDOWN_WHEN_LOST
" and do a new ifconfig"
#endif
"! (%02x)\n", i);
#ifdef SHUTDOWN_WHEN_LOST
dev->flags &= ~IFF_UP;
de620_close(dev);
#endif
was_down = 1;
return 1;
}
if (was_down) {
printk("Thanks, I feel much better now!\n");
was_down = 0;
}
de620_set_register(dev, W_TCR, TCR_DEF);
return 0;
}
int
de620_probe(struct device *dev)
{
static struct netstats de620_netstats;
int i;
byte checkbyte = 0xa5;
dev->base_addr = io;
dev->irq       = irq;
if (de620_debug)
printk("%s", version);
printk("D-Link DE-620 pocket adapter");
NIC_Cmd = DEF_NIC_CMD;
de620_set_register(dev, W_EIP, EIPRegister);
de620_set_register(dev, W_CPR, checkbyte);
checkbyte = de620_get_register(dev, R_CPR);
if ((checkbyte != 0xa5) || (read_eeprom(dev) != 0)) {
printk(" not identified in the printer port\n");
return ENODEV;
}
#if 0
if (check_region(dev->base_addr, 3)) {
printk(", port 0x%x busy\n", dev->base_addr);
return EBUSY;
}
#endif
request_region(dev->base_addr, 3, "de620");
printk(", Ethernet Address: %2.2X",
dev->dev_addr[0] = nic_data.NodeID[0]);
for (i = 1; i < ETH_ALEN; i++) {
printk(":%2.2X", dev->dev_addr[i] = nic_data.NodeID[i]);
dev->broadcast[i] = 0xff;
}
printk(" (%dk RAM,",
(nic_data.RAM_Size) ? (nic_data.RAM_Size >> 2) : 64);
if (nic_data.Media == 1)
printk(" BNC)\n");
else
printk(" UTP)\n");
dev->priv = &de620_netstats;
memset(dev->priv, 0, sizeof(struct netstats));
dev->get_stats = get_stats;
dev->open = de620_open;
dev->stop = de620_close;
dev->hard_start_xmit = &de620_start_xmit;
dev->set_multicast_list = &de620_set_multicast_list;
ether_setup(dev);
if (de620_debug) {
printk("\nEEPROM contents:\n");
printk("RAM_Size = 0x%02X\n", nic_data.RAM_Size);
printk("NodeID = %02X:%02X:%02X:%02X:%02X:%02X\n",
nic_data.NodeID[0], nic_data.NodeID[1],
nic_data.NodeID[2], nic_data.NodeID[3],
nic_data.NodeID[4], nic_data.NodeID[5]);
printk("Model = %d\n", nic_data.Model);
printk("Media = %d\n", nic_data.Media);
printk("SCR = 0x%02x\n", nic_data.SCR);
}
return 0;
}
#define sendit(dev,data) de620_set_register(dev, W_EIP, data | EIPRegister);
static unsigned short
ReadAWord(struct device *dev, int from)
{
unsigned short data;
int nbits;
sendit(dev, 0); sendit(dev, 1); sendit(dev, 5); sendit(dev, 4);
for (nbits = 9; nbits > 0; --nbits, from <<= 1) {
if (from & 0x0100) {
sendit(dev, 6); sendit(dev, 7); sendit(dev, 7); sendit(dev, 6);
}
else {
sendit(dev, 4); sendit(dev, 5); sendit(dev, 5); sendit(dev, 4);
}
}
for (data = 0, nbits = 16; nbits > 0; --nbits) {
sendit(dev, 4); sendit(dev, 5); sendit(dev, 5); sendit(dev, 4);
data = (data << 1) | ((de620_get_register(dev, R_STS) & EEDI) >> 7);
}
sendit(dev, 0); sendit(dev, 1); sendit(dev, 1); sendit(dev, 0);
return data;
}
static int
read_eeprom(struct device *dev)
{
unsigned short wrd;
wrd = ReadAWord(dev, 0x1aa);
if (!clone && (wrd != htons(0x0080)))
return -1;
nic_data.NodeID[0] = wrd & 0xff;
nic_data.NodeID[1] = wrd >> 8;
wrd = ReadAWord(dev, 0x1ab);
if (!clone && ((wrd & 0xff) != 0xc8))
return -1;
nic_data.NodeID[2] = wrd & 0xff;
nic_data.NodeID[3] = wrd >> 8;
wrd = ReadAWord(dev, 0x1ac);
nic_data.NodeID[4] = wrd & 0xff;
nic_data.NodeID[5] = wrd >> 8;
wrd = ReadAWord(dev, 0x1ad);
nic_data.RAM_Size = (wrd >> 8);
wrd = ReadAWord(dev, 0x1ae);
nic_data.Model = (wrd & 0xff);
wrd = ReadAWord(dev, 0x1af);
nic_data.Media = (wrd & 0xff);
wrd = ReadAWord(dev, 0x1a8);
nic_data.SCR = (wrd >> 8);
return 0;
}
#ifdef MODULE
static char nullname[8] = "";
static struct device de620_dev = {
nullname, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL, de620_probe };
int
init_module(void)
{
if (register_netdev(&de620_dev) != 0)
return -EIO;
return 0;
}
void
cleanup_module(void)
{
unregister_netdev(&de620_dev);
release_region(de620_dev.base_addr, 3);
}
#endif