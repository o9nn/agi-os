static const char *version =
"smc9194.c:v0.12 03/06/96 by Erik Stahlman (erik@vt.edu)\n";
#ifdef MODULE
#include <linux/module.h>
#include <linux/version.h>
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
#include <linux/ioport.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <linux/errno.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include "smc9194.h"
#define REALLY_NEW_KERNEL
#ifndef REALLY_NEW_KERNEL
#define free_irq( x, y ) free_irq( x )
#define request_irq( x, y, z, u, v ) request_irq( x, y, z, u )
#endif
#define USE_32_BIT 1
static unsigned int smc_portlist[] =
{ 0x200, 0x220, 0x240, 0x260, 0x280, 0x2A0, 0x2C0, 0x2E0,
0x300, 0x320, 0x340, 0x360, 0x380, 0x3A0, 0x3C0, 0x3E0, 0};
#define MEMORY_WAIT_TIME 16
#define SMC_DEBUG 0
#if (SMC_DEBUG > 2 )
#define PRINTK3(x) printk x
#else
#define PRINTK3(x)
#endif
#if SMC_DEBUG > 1
#define PRINTK2(x) printk x
#else
#define PRINTK2(x)
#endif
#ifdef SMC_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x)
#endif
#ifdef SUPPORT_OLD_KERNEL
#define NO_AUTOPROBE
#endif
#define CARDNAME "SMC9194"
#ifdef SUPPORT_OLD_KERNEL
char kernel_version[] = UTS_RELEASE;
#endif
struct smc_local {
struct enet_statistics stats;
struct sk_buff * saved_skb;
int packets_waiting;
};
int smc_init(struct device *dev);
static int smc_open(struct device *dev);
static int smc_send_packet(struct sk_buff *skb, struct device *dev);
static int smc_close(struct device *dev);
static struct enet_statistics * smc_query_statistics( struct device *dev);
#ifdef SUPPORT_OLD_KERNEL
static void smc_set_multicast_list(struct device *dev, int num_addrs,
void *addrs);
#else
static void smc_set_multicast_list(struct device *dev);
#endif
#ifdef REALLY_NEW_KERNEL
static void smc_interrupt(int irq, void *, struct pt_regs *regs);
#else
static void smc_interrupt(int irq, struct pt_regs *regs);
#endif
inline static void smc_rcv( struct device *dev );
inline static void smc_tx( struct device * dev );
static int smc_probe( int ioaddr );
static int smc_initcard( struct device *, int ioaddr );
#if SMC_DEBUG > 2
static void print_packet( byte *, int );
#endif
#define tx_done(dev) 1
static void smc_hardware_send_packet( struct device * dev );
static int smc_wait_to_send_packet( struct sk_buff * skb, struct device *dev );
static void smc_reset( int ioaddr );
static void smc_enable( int ioaddr );
static void smc_shutdown( int ioaddr );
#ifndef NO_AUTOPROBE
static int smc_findirq( int ioaddr );
#endif
#ifndef SUPPORT_OLD_KERNEL
static void smc_setmulticast( int ioaddr, int count, struct dev_mc_list * );
static int crc32( char *, int );
#endif
#ifdef SUPPORT_OLD_KERNEL
extern struct device *init_etherdev(struct device *dev, int sizeof_private,
unsigned long *mem_startp );
#endif
static void smc_reset( int ioaddr )
{
SMC_SELECT_BANK( 0 );
outw( RCR_SOFTRESET, ioaddr + RCR );
SMC_DELAY( );
outw( RCR_CLEAR, ioaddr + RCR );
outw( TCR_CLEAR, ioaddr + TCR );
SMC_SELECT_BANK( 1 );
outw( inw( ioaddr + CONTROL ) | CTL_AUTO_RELEASE , ioaddr + CONTROL );
SMC_SELECT_BANK( 2 );
outw( MC_RESET, ioaddr + MMU_CMD );
outb( 0, ioaddr + INT_MASK );
}
static void smc_enable( int ioaddr )
{
SMC_SELECT_BANK( 0 );
outw( TCR_NORMAL, ioaddr + TCR );
outw( RCR_NORMAL, ioaddr + RCR );
SMC_SELECT_BANK( 2 );
outb( SMC_INTERRUPT_MASK, ioaddr + INT_MASK );
}
static void smc_shutdown( int ioaddr )
{
SMC_SELECT_BANK( 2 );
outb( 0, ioaddr + INT_MASK );
SMC_SELECT_BANK( 0 );
outb( RCR_CLEAR, ioaddr + RCR );
outb( TCR_CLEAR, ioaddr + TCR );
#if 0
SMC_SELECT_BANK( 1 );
outw( inw( ioaddr + CONTROL ), CTL_POWERDOWN, ioaddr + CONTROL );
#endif
}
#ifndef SUPPORT_OLD_KERNEL
static void smc_setmulticast( int ioaddr, int count, struct dev_mc_list * addrs ) {
int i;
unsigned char multicast_table[ 8 ];
struct dev_mc_list * cur_addr;
unsigned char invert3[] = { 0, 4, 2, 6, 1, 5, 3, 7 };
memset( multicast_table, 0, sizeof( multicast_table ) );
cur_addr = addrs;
for ( i = 0; i < count ; i ++, cur_addr = cur_addr->next ) {
int position;
if ( !cur_addr )
break;
if ( !( *cur_addr->dmi_addr & 1 ) )
continue;
position = crc32( cur_addr->dmi_addr, 6 ) & 0x3f;
multicast_table[invert3[position&7]] |=
(1<<invert3[(position>>3)&7]);
}
SMC_SELECT_BANK( 3 );
for ( i = 0; i < 8 ; i++ ) {
outb( multicast_table[i], ioaddr + MULTICAST1 + i );
}
}
static int crc32( char * s, int length ) {
int perByte;
int perBit;
const unsigned long poly = 0xedb88320;
unsigned long crc_value = 0xffffffff;
for ( perByte = 0; perByte < length; perByte ++ ) {
unsigned char c;
c = *(s++);
for ( perBit = 0; perBit < 8; perBit++ ) {
crc_value = (crc_value>>1)^
(((crc_value^c)&0x01)?poly:0);
c >>= 1;
}
}
return crc_value;
}
#endif
static int smc_wait_to_send_packet( struct sk_buff * skb, struct device * dev )
{
struct smc_local *lp = (struct smc_local *)dev->priv;
unsigned short ioaddr = dev->base_addr;
word length;
unsigned short numPages;
word time_out;
if ( lp->saved_skb) {
lp->stats.tx_aborted_errors++;
printk(CARDNAME": Bad Craziness - sent packet while busy.\n" );
return 1;
}
lp->saved_skb = skb;
length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
numPages = length / 256;
if (numPages > 7 ) {
printk(CARDNAME": Far too big packet error. \n");
dev_kfree_skb (skb, FREE_WRITE);
lp->saved_skb = NULL;
return 0;
}
lp->packets_waiting++;
SMC_SELECT_BANK( 2 );
outw( MC_ALLOC | numPages, ioaddr + MMU_CMD );
time_out = MEMORY_WAIT_TIME;
do {
word status;
status = inb( ioaddr + INTERRUPT );
if ( status & IM_ALLOC_INT ) {
outb( IM_ALLOC_INT, ioaddr + INTERRUPT );
break;
}
} while ( -- time_out );
if ( !time_out ) {
SMC_ENABLE_INT( IM_ALLOC_INT );
PRINTK2((CARDNAME": memory allocation deferred. \n"));
return 0;
}
smc_hardware_send_packet(dev);
return 0;
}
static void smc_hardware_send_packet( struct device * dev )
{
struct smc_local *lp = (struct smc_local *)dev->priv;
byte packet_no;
struct sk_buff * skb = lp->saved_skb;
word length;
unsigned short ioaddr;
byte * buf;
ioaddr = dev->base_addr;
if ( !skb ) {
PRINTK((CARDNAME": In XMIT with no packet to send \n"));
return;
}
length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
buf = skb->data;
packet_no = inb( ioaddr + PNR_ARR + 1 );
if ( packet_no & 0x80 ) {
printk(KERN_DEBUG CARDNAME": Memory allocation failed. \n");
kfree(skb);
lp->saved_skb = NULL;
dev->tbusy = 0;
return;
}
outb( packet_no, ioaddr + PNR_ARR );
outw( PTR_AUTOINC , ioaddr + POINTER );
PRINTK3((CARDNAME": Trying to xmit packet of length %x\n", length ));
#if SMC_DEBUG > 2
print_packet( buf, length );
#endif
#ifdef USE_32_BIT
outl( (length +6 ) << 16 , ioaddr + DATA_1 );
#else
outw( 0, ioaddr + DATA_1 );
outb( (length+6) & 0xFF,ioaddr + DATA_1 );
outb( (length+6) >> 8 , ioaddr + DATA_1 );
#endif
#ifdef USE_32_BIT
if ( length & 0x2 ) {
outsl(ioaddr + DATA_1, buf, length >> 2 );
outw( *((word *)(buf + (length & 0xFFFFFFFC))),ioaddr +DATA_1);
}
else
outsl(ioaddr + DATA_1, buf, length >> 2 );
#else
outsw(ioaddr + DATA_1 , buf, (length ) >> 1);
#endif
if ( (length & 1) == 0 ) {
outw( 0, ioaddr + DATA_1 );
} else {
outb( buf[length -1 ], ioaddr + DATA_1 );
outb( 0x20, ioaddr + DATA_1);
}
SMC_ENABLE_INT( (IM_TX_INT | IM_TX_EMPTY_INT) );
outw( MC_ENQUEUE , ioaddr + MMU_CMD );
PRINTK2((CARDNAME": Sent packet of length %d \n",length));
lp->saved_skb = NULL;
dev_kfree_skb (skb, FREE_WRITE);
dev->trans_start = jiffies;
dev->tbusy = 0;
return;
}
int smc_init(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff) {
int error;
error = smc_probe(base_addr);
if ( 0 == error ) {
return smc_initcard( dev, base_addr );
}
return error;
} else {
if ( 0 != base_addr ) {
return -ENXIO;
}
}
for (i = 0; smc_portlist[i]; i++) {
int ioaddr = smc_portlist[i];
if (check_region( ioaddr , SMC_IO_EXTENT))
continue;
if ( smc_probe( ioaddr ) == 0) {
return smc_initcard( dev, ioaddr );
}
}
return -ENODEV;
}
#ifndef NO_AUTOPROBE
int smc_findirq( int ioaddr )
{
int timeout = 20;
sti();
autoirq_setup( 0 );
SMC_SELECT_BANK(2);
outb( IM_ALLOC_INT, ioaddr + INT_MASK );
outw( MC_ALLOC | 1, ioaddr + MMU_CMD );
while ( timeout ) {
byte int_status;
int_status = inb( ioaddr + INTERRUPT );
if ( int_status & IM_ALLOC_INT )
break;
timeout--;
}
SMC_DELAY();
SMC_DELAY();
outb( 0, ioaddr + INT_MASK );
cli();
return autoirq_report( 0 );
}
#endif
static int smc_probe( int ioaddr )
{
unsigned int bank;
word revision_register;
word base_address_register;
bank = inw( ioaddr + BANK_SELECT );
if ( (bank & 0xFF00) != 0x3300 ) {
return -ENODEV;
}
outw( 0x0, ioaddr + BANK_SELECT );
bank = inw( ioaddr + BANK_SELECT );
if ( (bank & 0xFF00 ) != 0x3300 ) {
return -ENODEV;
}
SMC_SELECT_BANK(1);
base_address_register = inw( ioaddr + BASE );
if ( ioaddr != ( base_address_register >> 3 & 0x3E0 ) ) {
printk(CARDNAME ": IOADDR %x doesn't match configuration (%x)."
"Probably not a SMC chip\n",
ioaddr, base_address_register >> 3 & 0x3E0 );
return -ENODEV;
}
SMC_SELECT_BANK(3);
revision_register = inw( ioaddr + REVISION );
if ( !chip_ids[ ( revision_register >> 4 ) & 0xF ] ) {
printk(CARDNAME ": IO %x: Unrecognized revision register:"
" %x, Contact author. \n", ioaddr, revision_register );
return -ENODEV;
}
return 0;
}
static int smc_initcard(struct device *dev, int ioaddr)
{
int i;
static unsigned version_printed = 0;
word revision_register;
word configuration_register;
word memory_info_register;
word memory_cfg_register;
const char * version_string;
const char * if_string;
int memory;
int irqval;
if (dev == NULL) {
#ifdef SUPPORT_OLD_KERNEL
#ifndef MODULE
dev = init_etherdev( 0, sizeof( struct smc_local ), 0 );
#endif
#else
dev = init_etherdev(0, 0);
#endif
if (dev == NULL)
return -ENOMEM;
}
if (version_printed++ == 0)
printk("%s", version);
dev->base_addr = ioaddr;
SMC_SELECT_BANK( 1 );
for ( i = 0; i < 6; i += 2 ) {
word address;
address = inw( ioaddr + ADDR0 + i );
dev->dev_addr[ i + 1] = address >> 8;
dev->dev_addr[ i ] = address & 0xFF;
}
SMC_SELECT_BANK( 0 );
memory_info_register = inw( ioaddr + MIR );
memory_cfg_register = inw( ioaddr + MCR );
memory = ( memory_cfg_register >> 9 ) & 0x7;
memory *= 256 * ( memory_info_register & 0xFF );
SMC_SELECT_BANK(3);
revision_register = inw( ioaddr + REVISION );
version_string = chip_ids[ ( revision_register >> 4 ) & 0xF ];
if ( !version_string ) {
return -ENODEV;
}
if ( dev->if_port == 0 ) {
SMC_SELECT_BANK(1);
configuration_register = inw( ioaddr + CONFIG );
if ( configuration_register & CFG_AUI_SELECT )
dev->if_port = 2;
else
dev->if_port = 1;
}
if_string = interfaces[ dev->if_port - 1 ];
smc_reset( ioaddr );
#ifndef NO_AUTOPROBE
if ( dev->irq < 2 ) {
int trials;
trials = 3;
while ( trials-- ) {
dev->irq = smc_findirq( ioaddr );
if ( dev->irq )
break;
smc_reset( ioaddr );
}
}
if (dev->irq == 0 ) {
printk(CARDNAME": Couldn't autodetect your IRQ. Use irq=xx.\n");
return -ENODEV;
}
#else
if (dev->irq == 0 ) {
printk(CARDNAME
": Autoprobing IRQs is not supported for old kernels.\n");
return -ENODEV;
}
#endif
if (dev->irq == 2) {
dev->irq = 9;
}
printk(CARDNAME ": %s(r:%d) at %#3x IRQ:%d INTF:%s MEM:%db ",
version_string, revision_register & 0xF, ioaddr, dev->irq,
if_string, memory );
printk("ADDR: ");
for (i = 0; i < 5; i++)
printk("%2.2x:", dev->dev_addr[i] );
printk("%2.2x \n", dev->dev_addr[5] );
if (dev->priv == NULL) {
dev->priv = kmalloc(sizeof(struct smc_local), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
}
memset(dev->priv, 0, sizeof(struct smc_local));
ether_setup(dev);
irqval = request_irq(dev->irq, &smc_interrupt, 0, CARDNAME, NULL);
if (irqval) {
printk(CARDNAME": unable to get IRQ %d (irqval=%d).\n",
dev->irq, irqval);
return -EAGAIN;
}
irq2dev_map[dev->irq] = dev;
request_region(ioaddr, SMC_IO_EXTENT, CARDNAME);
dev->open = smc_open;
dev->stop = smc_close;
dev->hard_start_xmit = smc_send_packet;
dev->get_stats = smc_query_statistics;
#ifdef HAVE_MULTICAST
dev->set_multicast_list = &smc_set_multicast_list;
#endif
return 0;
}
#if SMC_DEBUG > 2
static void print_packet( byte * buf, int length )
{
#if 0
int i;
int remainder;
int lines;
printk("Packet of length %d \n", length );
lines = length / 16;
remainder = length % 16;
for ( i = 0; i < lines ; i ++ ) {
int cur;
for ( cur = 0; cur < 8; cur ++ ) {
byte a, b;
a = *(buf ++ );
b = *(buf ++ );
printk("%02x%02x ", a, b );
}
printk("\n");
}
for ( i = 0; i < remainder/2 ; i++ ) {
byte a, b;
a = *(buf ++ );
b = *(buf ++ );
printk("%02x%02x ", a, b );
}
printk("\n");
#endif
}
#endif
static int smc_open(struct device *dev)
{
int ioaddr = dev->base_addr;
int i;
memset(dev->priv, 0, sizeof(struct smc_local));
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
#ifdef MODULE
MOD_INC_USE_COUNT;
#endif
smc_reset( ioaddr );
smc_enable( ioaddr );
SMC_SELECT_BANK( 1 );
if ( dev->if_port == 1 ) {
outw( inw( ioaddr + CONFIG ) & ~CFG_AUI_SELECT,
ioaddr + CONFIG );
}
else if ( dev->if_port == 2 ) {
outw( inw( ioaddr + CONFIG ) | CFG_AUI_SELECT,
ioaddr + CONFIG );
}
SMC_SELECT_BANK( 1 );
for ( i = 0; i < 6; i += 2 ) {
word address;
address = dev->dev_addr[ i + 1 ] << 8 ;
address |= dev->dev_addr[ i ];
outw( address, ioaddr + ADDR0 + i );
}
return 0;
}
static int smc_send_packet(struct sk_buff *skb, struct device *dev)
{
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 5)
return 1;
printk(KERN_WARNING CARDNAME": transmit timed out, %s?\n",
tx_done(dev) ? "IRQ conflict" :
"network cable problem");
smc_reset( dev->base_addr );
smc_enable( dev->base_addr );
dev->tbusy = 0;
dev->trans_start = jiffies;
((struct smc_local *)dev->priv)->saved_skb = NULL;
}
if (skb == NULL) {
dev_tint(dev);
return 0;
}
if (set_bit(0, (void*)&dev->tbusy) != 0) {
printk(KERN_WARNING CARDNAME": Transmitter access conflict.\n");
dev_kfree_skb (skb, FREE_WRITE);
} else {
return smc_wait_to_send_packet( skb, dev );
}
return 0;
}
#ifdef REALLY_NEW_KERNEL
static void smc_interrupt(int irq, void * dev_id, struct pt_regs * regs)
#else
static void smc_interrupt(int irq, struct pt_regs * regs)
#endif
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
int ioaddr = dev->base_addr;
struct smc_local *lp = (struct smc_local *)dev->priv;
byte status;
word card_stats;
byte mask;
int timeout;
word saved_bank;
word saved_pointer;
PRINTK3((CARDNAME": SMC interrupt started \n"));
if (dev == NULL) {
printk(KERN_WARNING CARDNAME": irq %d for unknown device.\n",
irq);
return;
}
if ( dev->interrupt ) {
printk(KERN_WARNING CARDNAME": interrupt inside interrupt.\n");
return;
}
dev->interrupt = 1;
saved_bank = inw( ioaddr + BANK_SELECT );
SMC_SELECT_BANK(2);
saved_pointer = inw( ioaddr + POINTER );
mask = inb( ioaddr + INT_MASK );
outb( 0, ioaddr + INT_MASK );
timeout = 4;
PRINTK2((KERN_WARNING CARDNAME ": MASK IS %x \n", mask ));
do {
status = inb( ioaddr + INTERRUPT ) & mask;
if (!status )
break;
PRINTK3((KERN_WARNING CARDNAME
": Handling interrupt status %x \n", status ));
if (status & IM_RCV_INT) {
PRINTK2((KERN_WARNING CARDNAME
": Receive Interrupt\n"));
smc_rcv(dev);
} else if (status & IM_TX_INT ) {
PRINTK2((KERN_WARNING CARDNAME
": TX ERROR handled\n"));
smc_tx(dev);
outb(IM_TX_INT, ioaddr + INTERRUPT );
} else if (status & IM_TX_EMPTY_INT ) {
SMC_SELECT_BANK( 0 );
card_stats = inw( ioaddr + COUNTER );
lp->stats.collisions += card_stats & 0xF;
card_stats >>= 4;
lp->stats.collisions += card_stats & 0xF;
#if 0
card_stats >>= 4;
card_stats >>= 4;
#endif
SMC_SELECT_BANK( 2 );
PRINTK2((KERN_WARNING CARDNAME
": TX_BUFFER_EMPTY handled\n"));
outb( IM_TX_EMPTY_INT, ioaddr + INTERRUPT );
mask &= ~IM_TX_EMPTY_INT;
lp->stats.tx_packets += lp->packets_waiting;
lp->packets_waiting = 0;
} else if (status & IM_ALLOC_INT ) {
PRINTK2((KERN_DEBUG CARDNAME
": Allocation interrupt \n"));
mask &= ~IM_ALLOC_INT;
smc_hardware_send_packet( dev );
mask |= ( IM_TX_EMPTY_INT | IM_TX_INT );
mark_bh( NET_BH );
PRINTK2((CARDNAME": Handoff done successfully.\n"));
} else if (status & IM_RX_OVRN_INT ) {
lp->stats.rx_errors++;
lp->stats.rx_fifo_errors++;
outb( IM_RX_OVRN_INT, ioaddr + INTERRUPT );
} else if (status & IM_EPH_INT ) {
PRINTK((CARDNAME ": UNSUPPORTED: EPH INTERRUPT \n"));
} else if (status & IM_ERCV_INT ) {
PRINTK((CARDNAME ": UNSUPPORTED: ERCV INTERRUPT \n"));
outb( IM_ERCV_INT, ioaddr + INTERRUPT );
}
} while ( timeout -- );
SMC_SELECT_BANK( 2 );
outb( mask, ioaddr + INT_MASK );
PRINTK3(( KERN_WARNING CARDNAME ": MASK is now %x \n", mask ));
outw( saved_pointer, ioaddr + POINTER );
SMC_SELECT_BANK( saved_bank );
dev->interrupt = 0;
PRINTK3((CARDNAME ": Interrupt done\n"));
return;
}
static void smc_rcv(struct device *dev)
{
struct smc_local *lp = (struct smc_local *)dev->priv;
int ioaddr = dev->base_addr;
int packet_number;
word status;
word packet_length;
packet_number = inw( ioaddr + FIFO_PORTS );
if ( packet_number & FP_RXEMPTY ) {
PRINTK((CARDNAME ": WARNING: smc_rcv with nothing on FIFO. \n"));
return;
}
outw( PTR_READ | PTR_RCV | PTR_AUTOINC, ioaddr + POINTER );
status = inw( ioaddr + DATA_1 );
packet_length = inw( ioaddr + DATA_1 );
packet_length &= 0x07ff;
PRINTK2(("RCV: STATUS %4x LENGTH %4x\n", status, packet_length ));
packet_length -= 6;
if ( !(status & RS_ERRORS ) ){
struct sk_buff * skb;
byte * data;
if ( status & RS_ODDFRAME )
packet_length++;
if ( status & RS_MULTICAST )
lp->stats.multicast++;
#ifdef SUPPORT_OLD_KERNEL
skb = alloc_skb( packet_length + 5, GFP_ATOMIC );
#else
skb = dev_alloc_skb( packet_length + 5);
#endif
if ( skb == NULL ) {
printk(KERN_NOTICE CARDNAME
": Low memory, packet dropped.\n");
lp->stats.rx_dropped++;
}
#ifndef SUPPORT_OLD_KERNEL
skb_reserve( skb, 2 );
#endif
skb->dev = dev;
#ifdef SUPPORT_OLD_KERNEL
skb->len = packet_length;
data = skb->data;
#else
data = skb_put( skb, packet_length);
#endif
#ifdef USE_32_BIT
PRINTK3((" Reading %d dwords (and %d bytes) \n",
packet_length >> 2, packet_length & 3 ));
insl(ioaddr + DATA_1 , data, packet_length >> 2 );
insb( ioaddr + DATA_1, data + (packet_length & 0xFFFFFC),
packet_length & 0x3 );
#else
PRINTK3((" Reading %d words and %d byte(s) \n",
(packet_length >> 1 ), packet_length & 1 );
if ( packet_length & 1 )
*(data++) = inb( ioaddr + DATA_1 );
insw(ioaddr + DATA_1 , data, (packet_length + 1 ) >> 1);
if ( packet_length & 1 ) {
data += packet_length & ~1;
*((data++) = inb( ioaddr + DATA_1 );
}
#endif
#if SMC_DEBUG > 2
print_packet( data, packet_length );
#endif
#ifndef SUPPORT_OLD_KERNEL
skb->protocol = eth_type_trans(skb, dev );
#endif
netif_rx(skb);
lp->stats.rx_packets++;
} else {
lp->stats.rx_errors++;
if ( status & RS_ALGNERR ) lp->stats.rx_frame_errors++;
if ( status & (RS_TOOSHORT | RS_TOOLONG ) )
lp->stats.rx_length_errors++;
if ( status & RS_BADCRC) lp->stats.rx_crc_errors++;
}
outw( MC_RELEASE, ioaddr + MMU_CMD );
return;
}
static void smc_tx( struct device * dev )
{
int ioaddr = dev->base_addr;
struct smc_local *lp = (struct smc_local *)dev->priv;
byte saved_packet;
byte packet_no;
word tx_status;
saved_packet = inb( ioaddr + PNR_ARR );
packet_no = inw( ioaddr + FIFO_PORTS );
packet_no &= 0x7F;
outb( packet_no, ioaddr + PNR_ARR );
outw( PTR_AUTOINC | PTR_READ, ioaddr + POINTER );
tx_status = inw( ioaddr + DATA_1 );
PRINTK3((CARDNAME": TX DONE STATUS: %4x \n", tx_status ));
lp->stats.tx_errors++;
if ( tx_status & TS_LOSTCAR ) lp->stats.tx_carrier_errors++;
if ( tx_status & TS_LATCOL ) {
printk(KERN_DEBUG CARDNAME
": Late collision occurred on last xmit.\n");
lp->stats.tx_window_errors++;
}
#if 0
if ( tx_status & TS_16COL ) { ... }
#endif
if ( tx_status & TS_SUCCESS ) {
printk(CARDNAME": Successful packet caused interrupt \n");
}
SMC_SELECT_BANK( 0 );
outw( inw( ioaddr + TCR ) | TCR_ENABLE, ioaddr + TCR );
SMC_SELECT_BANK( 2 );
outw( MC_FREEPKT, ioaddr + MMU_CMD );
lp->packets_waiting--;
outb( saved_packet, ioaddr + PNR_ARR );
return;
}
static int smc_close(struct device *dev)
{
dev->tbusy = 1;
dev->start = 0;
smc_shutdown( dev->base_addr );
#ifdef MODULE
MOD_DEC_USE_COUNT;
#endif
return 0;
}
static struct enet_statistics * smc_query_statistics(struct device *dev) {
struct smc_local *lp = (struct smc_local *)dev->priv;
return &lp->stats;
}
#ifdef SUPPORT_OLD_KERNEL
static void smc_set_multicast_list( struct device * dev,
int num_addrs, void * addrs )
#else
static void smc_set_multicast_list(struct device *dev)
#endif
{
short ioaddr = dev->base_addr;
SMC_SELECT_BANK(0);
#ifdef SUPPORT_OLD_KERNEL
if ( num_addrs < 0 )
#else
if ( dev->flags & IFF_PROMISC )
#endif
outw( inw(ioaddr + RCR ) | RCR_PROMISC, ioaddr + RCR );
#ifdef SUPPORT_OLD_KERNEL
else if ( num_addrs > 20 )
#else
else if (dev->flags & IFF_ALLMULTI)
#endif
outw( inw(ioaddr + RCR ) | RCR_ALMUL, ioaddr + RCR );
#ifdef SUPPORT_OLD_KERNEL
else if (num_addrs > 0 ) {
outw( inw( ioaddr + RCR ) & ~RCR_PROMISC, ioaddr + RCR );
outw( inw( ioadddr + RCR ) | RCR_ALMUL, ioadddr + RCR );
}
#else
else if (dev->mc_count ) {
outw( inw( ioaddr + RCR ) & ~(RCR_PROMISC | RCR_ALMUL),
ioaddr + RCR );
smc_setmulticast( ioaddr, dev->mc_count, dev->mc_list );
}
#endif
else {
outw( inw( ioaddr + RCR ) & ~(RCR_PROMISC | RCR_ALMUL),
ioaddr + RCR );
SMC_SELECT_BANK( 3 );
outw( 0, ioaddr + MULTICAST1 );
outw( 0, ioaddr + MULTICAST2 );
outw( 0, ioaddr + MULTICAST3 );
outw( 0, ioaddr + MULTICAST4 );
}
}
#ifdef MODULE
static char devicename[9] = { 0, };
static struct device devSMC9194 = {
devicename,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, smc_init };
int io = 0;
int irq = 0;
int ifport = 0;
int init_module(void)
{
int result;
if (io == 0)
printk(KERN_WARNING
CARDNAME": You shouldn't use auto-probing with insmod!\n" );
devSMC9194.base_addr = io;
devSMC9194.irq = irq;
devSMC9194.if_port = ifport;
if ((result = register_netdev(&devSMC9194)) != 0)
return result;
return 0;
}
void cleanup_module(void)
{
unregister_netdev(&devSMC9194);
free_irq(devSMC9194.irq, NULL );
irq2dev_map[devSMC9194.irq] = NULL;
release_region(devSMC9194.base_addr, SMC_IO_EXTENT);
if (devSMC9194.priv)
kfree_s(devSMC9194.priv, sizeof(struct smc_local));
}
#endif