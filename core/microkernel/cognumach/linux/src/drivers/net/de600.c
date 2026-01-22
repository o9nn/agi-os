static const char *version =
"de600.c: $Revision: 1.1 $,  Bjorn Ekwall (bj0rn@blox.se)\n";
#define DE600_SLOW_DOWN SLOW_DOWN_IO; SLOW_DOWN_IO; SLOW_DOWN_IO
#define SLOW_IO_BY_JUMPING
#define CHECK_LOST_DE600
#define SHUTDOWN_WHEN_LOST
#undef FAKE_SMALL_MAX
#ifdef DE600_DEBUG
#define PRINTK(x) if (de600_debug >= 2) printk x
#else
#define DE600_DEBUG 0
#define PRINTK(x)
#endif
unsigned int de600_debug = DE600_DEBUG;
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
#ifdef FAKE_SMALL_MAX
static unsigned long de600_rspace(struct sock *sk);
#include <net/sock.h>
#endif
#define netstats enet_statistics
typedef unsigned char byte;
#ifndef DE600_IO
#define DE600_IO 0x378
#endif
#define DATA_PORT	(DE600_IO)
#define STATUS_PORT	(DE600_IO + 1)
#define COMMAND_PORT	(DE600_IO + 2)
#ifndef DE600_IRQ
#define DE600_IRQ	7
#endif
#define SELECT_NIC	0x04
#define SELECT_PRN	0x1c
#define NML_PRN		0xec
#define IRQEN		0x10
#define RX_BUSY		0x80
#define RX_GOOD		0x40
#define TX_FAILED16	0x10
#define TX_BUSY		0x08
#define WRITE_DATA	0x00
#define READ_DATA	0x01
#define STATUS		0x02
#define COMMAND		0x03
#define NULL_COMMAND	0x04
#define RX_LEN		0x05
#define TX_ADDR		0x06
#define RW_ADDR		0x07
#define HI_NIBBLE	0x08
#define RX_ALL		0x01
#define RX_BP		0x02
#define RX_MBP		0x03
#define TX_ENABLE	0x04
#define RX_ENABLE	0x08
#define RESET		0x80
#define STOP_RESET	0x00
#define RX_PAGE2_SELECT	0x10
#define RX_BASE_PAGE	0x20
#define FLIP_IRQ	0x40
#define MEM_2K		0x0800
#define MEM_4K		0x1000
#define MEM_6K		0x1800
#define NODE_ADDRESS	0x2000
#define RUNT 60
static byte	de600_read_status(struct device *dev);
static byte	de600_read_byte(unsigned char type, struct device *dev);
static int	de600_open(struct device *dev);
static int	de600_close(struct device *dev);
static struct netstats *get_stats(struct device *dev);
static int	de600_start_xmit(struct sk_buff *skb, struct device *dev);
static void	de600_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static int	de600_tx_intr(struct device *dev, int irq_status);
static void	de600_rx_intr(struct device *dev);
static void	trigger_interrupt(struct device *dev);
int		de600_probe(struct device *dev);
static int	adapter_init(struct device *dev);
static volatile int		rx_page		= 0;
#define TX_PAGES 2
static volatile int		tx_fifo[TX_PAGES];
static volatile int		tx_fifo_in = 0;
static volatile int		tx_fifo_out = 0;
static volatile int		free_tx_pages = TX_PAGES;
static int			was_down = 0;
#define select_prn() outb_p(SELECT_PRN, COMMAND_PORT); DE600_SLOW_DOWN
#define select_nic() outb_p(SELECT_NIC, COMMAND_PORT); DE600_SLOW_DOWN
#define de600_put_byte(data) ( \
outb_p(((data) << 4)   | WRITE_DATA            , DATA_PORT), \
outb_p(((data) & 0xf0) | WRITE_DATA | HI_NIBBLE, DATA_PORT))
#define de600_put_command(cmd) ( \
outb_p(( rx_page        << 4)   | COMMAND            , DATA_PORT), \
outb_p(( rx_page        & 0xf0) | COMMAND | HI_NIBBLE, DATA_PORT), \
outb_p(((rx_page | cmd) << 4)   | COMMAND            , DATA_PORT), \
outb_p(((rx_page | cmd) & 0xf0) | COMMAND | HI_NIBBLE, DATA_PORT))
#define de600_setup_address(addr,type) ( \
outb_p((((addr) << 4) & 0xf0) | type            , DATA_PORT), \
outb_p(( (addr)       & 0xf0) | type | HI_NIBBLE, DATA_PORT), \
outb_p((((addr) >> 4) & 0xf0) | type            , DATA_PORT), \
outb_p((((addr) >> 8) & 0xf0) | type | HI_NIBBLE, DATA_PORT))
#define rx_page_adr() ((rx_page & RX_PAGE2_SELECT)?(MEM_6K):(MEM_4K))
#define next_rx_page() (rx_page ^= RX_PAGE2_SELECT)
#define tx_page_adr(a) (((a) + 1) * MEM_2K)
static inline byte
de600_read_status(struct device *dev)
{
byte status;
outb_p(STATUS, DATA_PORT);
status = inb(STATUS_PORT);
outb_p(NULL_COMMAND | HI_NIBBLE, DATA_PORT);
return status;
}
static inline byte
de600_read_byte(unsigned char type, struct device *dev) {
byte lo;
(void)outb_p((type), DATA_PORT);
lo = ((unsigned char)inb(STATUS_PORT)) >> 4;
(void)outb_p((type) | HI_NIBBLE, DATA_PORT);
return ((unsigned char)inb(STATUS_PORT) & (unsigned char)0xf0) | lo;
}
static int
de600_open(struct device *dev)
{
if (request_irq(DE600_IRQ, de600_interrupt, 0, "de600", NULL)) {
printk ("%s: unable to get IRQ %d\n", dev->name, DE600_IRQ);
return 1;
}
irq2dev_map[DE600_IRQ] = dev;
MOD_INC_USE_COUNT;
dev->start = 1;
if (adapter_init(dev)) {
return 1;
}
return 0;
}
static int
de600_close(struct device *dev)
{
select_nic();
rx_page = 0;
de600_put_command(RESET);
de600_put_command(STOP_RESET);
de600_put_command(0);
select_prn();
if (dev->start) {
free_irq(DE600_IRQ, NULL);
irq2dev_map[DE600_IRQ] = NULL;
dev->start = 0;
MOD_DEC_USE_COUNT;
}
return 0;
}
static struct netstats *
get_stats(struct device *dev)
{
return (struct netstats *)(dev->priv);
}
static inline void
trigger_interrupt(struct device *dev)
{
de600_put_command(FLIP_IRQ);
select_prn();
DE600_SLOW_DOWN;
select_nic();
de600_put_command(0);
}
static int
de600_start_xmit(struct sk_buff *skb, struct device *dev)
{
int	transmit_from;
int	len;
int	tickssofar;
byte	*buffer = skb->data;
if (skb == NULL) {
dev_tint(dev);
return 0;
}
if (free_tx_pages <= 0) {
tickssofar = jiffies - dev->trans_start;
if (tickssofar < 5)
return 1;
printk("%s: transmit timed out (%d), %s?\n",
dev->name,
tickssofar,
"network cable problem"
);
if (adapter_init(dev)) {
return 1;
}
}
PRINTK(("de600_start_xmit:len=%d, page %d/%d\n", skb->len, tx_fifo_in, free_tx_pages));
if ((len = skb->len) < RUNT)
len = RUNT;
cli();
select_nic();
tx_fifo[tx_fifo_in] = transmit_from = tx_page_adr(tx_fifo_in) - len;
tx_fifo_in = (tx_fifo_in + 1) % TX_PAGES;
#ifdef CHECK_LOST_DE600
de600_setup_address(NODE_ADDRESS, RW_ADDR);
de600_read_byte(READ_DATA, dev);
if (was_down || (de600_read_byte(READ_DATA, dev) != 0xde)) {
if (adapter_init(dev)) {
sti();
return 1;
}
}
#endif
de600_setup_address(transmit_from, RW_ADDR);
for ( ; len > 0; --len, ++buffer)
de600_put_byte(*buffer);
if (free_tx_pages-- == TX_PAGES) {
dev->trans_start = jiffies;
dev->tbusy = 0;
de600_setup_address(transmit_from, TX_ADDR);
de600_put_command(TX_ENABLE);
}
else {
dev->tbusy = !free_tx_pages;
select_prn();
}
sti();
#ifdef FAKE_SMALL_MAX
if (skb->sk && (skb->sk->protocol == IPPROTO_TCP) &&
(skb->sk->prot->rspace != &de600_rspace))
skb->sk->prot->rspace = de600_rspace;
#endif
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
static void
de600_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
struct device	*dev = irq2dev_map[irq];
byte		irq_status;
int		retrig = 0;
int		boguscount = 0;
if ((dev == NULL) || (dev->start == 0) || (DE600_IRQ != irq)) {
printk("%s: bogus interrupt %d\n", dev?dev->name:"DE-600", irq);
return;
}
dev->interrupt = 1;
select_nic();
irq_status = de600_read_status(dev);
do {
PRINTK(("de600_interrupt (%02X)\n", irq_status));
if (irq_status & RX_GOOD)
de600_rx_intr(dev);
else if (!(irq_status & RX_BUSY))
de600_put_command(RX_ENABLE);
if (free_tx_pages < TX_PAGES)
retrig = de600_tx_intr(dev, irq_status);
else
retrig = 0;
irq_status = de600_read_status(dev);
} while ( (irq_status & RX_GOOD) || ((++boguscount < 100) && retrig) );
dev->interrupt = 0;
select_prn();
if (retrig)
trigger_interrupt(dev);
sti();
return;
}
static int
de600_tx_intr(struct device *dev, int irq_status)
{
mark_bh(NET_BH);
if (irq_status & TX_BUSY)
return 1;
if (!(irq_status & TX_FAILED16)) {
tx_fifo_out = (tx_fifo_out + 1) % TX_PAGES;
++free_tx_pages;
((struct netstats *)(dev->priv))->tx_packets++;
dev->tbusy = 0;
}
if ((free_tx_pages < TX_PAGES) || (irq_status & TX_FAILED16)) {
dev->trans_start = jiffies;
de600_setup_address(tx_fifo[tx_fifo_out], TX_ADDR);
de600_put_command(TX_ENABLE);
return 1;
}
return 0;
}
static void
de600_rx_intr(struct device *dev)
{
struct sk_buff	*skb;
int		i;
int		read_from;
int		size;
register unsigned char	*buffer;
cli();
size = de600_read_byte(RX_LEN, dev);
size += (de600_read_byte(RX_LEN, dev) << 8);
size -= 4;
read_from = rx_page_adr();
next_rx_page();
de600_put_command(RX_ENABLE);
sti();
if ((size < 32)  ||  (size > 1535)) {
printk("%s: Bogus packet size %d.\n", dev->name, size);
if (size > 10000)
adapter_init(dev);
return;
}
skb = dev_alloc_skb(size+2);
sti();
if (skb == NULL) {
printk("%s: Couldn't allocate a sk_buff of size %d.\n",
dev->name, size);
return;
}
skb->dev = dev;
skb_reserve(skb,2);
buffer = skb_put(skb,size);
de600_setup_address(read_from, RW_ADDR);
for (i = size; i > 0; --i, ++buffer)
*buffer = de600_read_byte(READ_DATA, dev);
((struct netstats *)(dev->priv))->rx_packets++;
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
}
int
de600_probe(struct device *dev)
{
int	i;
static struct netstats de600_netstats;
printk("%s: D-Link DE-600 pocket adapter", dev->name);
if (de600_debug > 1)
printk("%s", version);
rx_page = 0;
select_nic();
(void)de600_read_status(dev);
de600_put_command(RESET);
de600_put_command(STOP_RESET);
if (de600_read_status(dev) & 0xf0) {
printk(": not at I/O %#3x.\n", DATA_PORT);
return ENODEV;
}
de600_setup_address(NODE_ADDRESS, RW_ADDR);
for (i = 0; i < ETH_ALEN; i++) {
dev->dev_addr[i] = de600_read_byte(READ_DATA, dev);
dev->broadcast[i] = 0xff;
}
if ((dev->dev_addr[1] == 0xde) && (dev->dev_addr[2] == 0x15)) {
dev->dev_addr[0] = 0x00;
dev->dev_addr[1] = 0x80;
dev->dev_addr[2] = 0xc8;
dev->dev_addr[3] &= 0x0f;
dev->dev_addr[3] |= 0x70;
} else {
printk(" not identified in the printer port\n");
return ENODEV;
}
#if 0
if (check_region(DE600_IO, 3)) {
printk(", port 0x%x busy\n", DE600_IO);
return EBUSY;
}
#endif
request_region(DE600_IO, 3, "de600");
printk(", Ethernet Address: %02X", dev->dev_addr[0]);
for (i = 1; i < ETH_ALEN; i++)
printk(":%02X",dev->dev_addr[i]);
printk("\n");
dev->priv = &de600_netstats;
memset(dev->priv, 0, sizeof(struct netstats));
dev->get_stats = get_stats;
dev->open = de600_open;
dev->stop = de600_close;
dev->hard_start_xmit = &de600_start_xmit;
ether_setup(dev);
dev->flags&=~IFF_MULTICAST;
select_prn();
return 0;
}
static int
adapter_init(struct device *dev)
{
int	i;
long flags;
save_flags(flags);
cli();
select_nic();
rx_page = 0;
de600_put_command(RESET);
de600_put_command(STOP_RESET);
#ifdef CHECK_LOST_DE600
de600_setup_address(NODE_ADDRESS, RW_ADDR);
de600_read_byte(READ_DATA, dev);
if ((de600_read_byte(READ_DATA, dev) != 0xde) ||
(de600_read_byte(READ_DATA, dev) != 0x15)) {
printk("Something has happened to the DE-600!  Please check it"
#ifdef SHUTDOWN_WHEN_LOST
" and do a new ifconfig"
#endif
"!\n");
#ifdef SHUTDOWN_WHEN_LOST
dev->flags &= ~IFF_UP;
de600_close(dev);
#endif
was_down = 1;
dev->tbusy = 1;
restore_flags(flags);
return 1;
}
#endif
if (was_down) {
printk("Thanks, I feel much better now!\n");
was_down = 0;
}
dev->tbusy = 0;
dev->interrupt = 0;
tx_fifo_in = 0;
tx_fifo_out = 0;
free_tx_pages = TX_PAGES;
de600_setup_address(NODE_ADDRESS, RW_ADDR);
for (i = 0; i < ETH_ALEN; i++)
de600_put_byte(dev->dev_addr[i]);
rx_page = RX_BP | RX_BASE_PAGE;
de600_setup_address(MEM_4K, RW_ADDR);
de600_put_command(RX_ENABLE);
select_prn();
restore_flags(flags);
return 0;
}
#ifdef FAKE_SMALL_MAX
#define DE600_MIN_WINDOW 1024
#define DE600_MAX_WINDOW 2048
#define DE600_TCP_WINDOW_DIFF 1024
#define min(a,b)	((a)<(b)?(a):(b))
static unsigned long
de600_rspace(struct sock *sk)
{
int amt;
if (sk != NULL) {
if (sk->rmem_alloc >= sk->rcvbuf-2*DE600_MIN_WINDOW) return(0);
amt = min((sk->rcvbuf-sk->rmem_alloc)/2, DE600_MAX_WINDOW);
if (amt < 0) return(0);
return(amt);
}
return(0);
}
#endif
#ifdef MODULE
static char nullname[8];
static struct device de600_dev = {
nullname, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL, de600_probe };
int
init_module(void)
{
if (register_netdev(&de600_dev) != 0)
return -EIO;
return 0;
}
void
cleanup_module(void)
{
unregister_netdev(&de600_dev);
release_region(DE600_IO, 3);
}
#endif