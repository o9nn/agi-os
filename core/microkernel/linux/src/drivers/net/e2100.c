static const char *version =
"e2100.c:v1.01 7/21/94 Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/ioport.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <asm/io.h>
#include <asm/system.h>
#include "8390.h"
static int e21_probe_list[] = {0x300, 0x280, 0x380, 0x220, 0};
#define E21_NIC_OFFSET 0
#define E21_ASIC 0x10
#define E21_MEM_ENABLE 0x10
#define E21_MEM_ON 0x05
#define E21_MEM_ON_8 0x07
#define E21_MEM_BASE 0x11
#define E21_IRQ_LOW 0x12
#define E21_IRQ_HIGH 0x14
#define E21_MEDIA 0x14
#define E21_ALT_IFPORT 0x02
#define E21_BIG_MEM 0x04
#define E21_SAPROM 0x10
#define E21_IO_EXTENT 0x20
static inline void mem_on(short port, volatile char *mem_base,
unsigned char start_page )
{
mem_base[start_page];
inb(port + E21_MEM_ENABLE);
outb(E21_MEM_ON, port + E21_MEM_ENABLE + E21_MEM_ON);
}
static inline void mem_off(short port)
{
inb(port + E21_MEM_ENABLE);
outb(0x00, port + E21_MEM_ENABLE);
}
#define E21_RX_START_PG 0x00
#define E21_RX_STOP_PG 0x30
#define E21_BIG_RX_STOP_PG 0xF0
#define E21_TX_START_PG E21_RX_STOP_PG
int e2100_probe(struct device *dev);
int e21_probe1(struct device *dev, int ioaddr);
static int e21_open(struct device *dev);
static void e21_reset_8390(struct device *dev);
static void e21_block_input(struct device *dev, int count,
struct sk_buff *skb, int ring_offset);
static void e21_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page);
static void e21_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
static int e21_close(struct device *dev);
int e2100_probe(struct device *dev)
{
int *port;
int base_addr = dev->base_addr;
if (base_addr > 0x1ff)
return e21_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (port = e21_probe_list; *port; port++) {
if (check_region(*port, E21_IO_EXTENT))
continue;
if (e21_probe1(dev, *port) == 0)
return 0;
}
return ENODEV;
}
int e21_probe1(struct device *dev, int ioaddr)
{
int i, status;
unsigned char *station_addr = dev->dev_addr;
static unsigned version_printed = 0;
if (inb(ioaddr + E21_SAPROM + 0) != 0x00
|| inb(ioaddr + E21_SAPROM + 1) != 0x00
|| inb(ioaddr + E21_SAPROM + 2) != 0x1d)
return ENODEV;
outb(E8390_NODMA + E8390_STOP, ioaddr);
SLOW_DOWN_IO;
status = inb(ioaddr);
if (status != 0x21 && status != 0x23)
return ENODEV;
for (i = 0; i < 6; i++)
station_addr[i] = inb(ioaddr + E21_SAPROM + i);
inb(ioaddr + E21_MEDIA);
outb(0, ioaddr + E21_ASIC);
if (ei_debug && version_printed++ == 0)
printk("%s", version);
if (dev == NULL) {
printk("e2100.c: Passed a NULL device.\n");
dev = init_etherdev(0, 0);
}
printk("%s: E21** at %#3x,", dev->name, ioaddr);
for (i = 0; i < 6; i++)
printk(" %02X", station_addr[i]);
if (dev->irq < 2) {
int irqlist[] = {15,11,10,12,5,9,3,4}, i;
for (i = 0; i < 8; i++)
if (request_irq (irqlist[i], NULL, 0, "bogus", NULL) != -EBUSY) {
dev->irq = irqlist[i];
break;
}
if (i >= 8) {
printk(" unable to get IRQ %d.\n", dev->irq);
return EAGAIN;
}
} else if (dev->irq == 2)
dev->irq = 9;
if (ethdev_init(dev)) {
printk (" unable to get memory for dev->priv.\n");
return -ENOMEM;
}
request_region(ioaddr, E21_IO_EXTENT, "e2100");
dev->base_addr = ioaddr;
ei_status.name = "E2100";
ei_status.word16 = 1;
ei_status.tx_start_page = E21_TX_START_PG;
ei_status.rx_start_page = E21_RX_START_PG;
ei_status.stop_page = E21_RX_STOP_PG;
ei_status.saved_irq = dev->irq;
if (dev->mem_end & 15)
dev->if_port = dev->mem_end & 7;
else {
dev->if_port = 0;
inb(ioaddr + E21_MEDIA);
for(i = 0; i < 6; i++)
if (station_addr[i] != inb(ioaddr + E21_SAPROM + 8 + i)) {
dev->if_port = 1;
break;
}
}
if (dev->mem_start == 0)
dev->mem_start = 0xd0000;
#ifdef notdef
dev->rmem_start = dev->mem_start + TX_PAGES*256;
dev->mem_end = dev->rmem_end = dev->mem_start + 2*1024;
#endif
printk(", IRQ %d, %s media, memory @ %#lx.\n", dev->irq,
dev->if_port ? "secondary" : "primary", dev->mem_start);
ei_status.reset_8390 = &e21_reset_8390;
ei_status.block_input = &e21_block_input;
ei_status.block_output = &e21_block_output;
ei_status.get_8390_hdr = &e21_get_8390_hdr;
dev->open = &e21_open;
dev->stop = &e21_close;
NS8390_init(dev, 0);
return 0;
}
static int
e21_open(struct device *dev)
{
short ioaddr = dev->base_addr;
if (request_irq(dev->irq, ei_interrupt, 0, "e2100", NULL)) {
return EBUSY;
}
irq2dev_map[dev->irq] = dev;
inb(ioaddr + E21_IRQ_LOW);
outb(0, ioaddr + E21_ASIC + (dev->irq & 7));
inb(ioaddr + E21_IRQ_HIGH);
outb(0, ioaddr + E21_ASIC + (dev->irq > 7 ? 1:0)
+ (dev->if_port ? E21_ALT_IFPORT : 0));
inb(ioaddr + E21_MEM_BASE);
outb(0, ioaddr + E21_ASIC + ((dev->mem_start >> 17) & 7));
ei_open(dev);
MOD_INC_USE_COUNT;
return 0;
}
static void
e21_reset_8390(struct device *dev)
{
short ioaddr = dev->base_addr;
outb(0x01, ioaddr);
if (ei_debug > 1) printk("resetting the E2180x3 t=%ld...", jiffies);
ei_status.txing = 0;
if (ei_debug > 1) printk("reset done\n");
return;
}
static void
e21_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr, int ring_page)
{
short ioaddr = dev->base_addr;
char *shared_mem = (char *)dev->mem_start;
mem_on(ioaddr, shared_mem, ring_page);
#ifdef notdef
memcpy_fromio(hdr, shared_mem, sizeof(struct e8390_pkt_hdr));
#else
((unsigned int*)hdr)[0] = readl(shared_mem);
#endif
mem_off(ioaddr);
}
static void
e21_block_input(struct device *dev, int count, struct sk_buff *skb, int ring_offset)
{
short ioaddr = dev->base_addr;
char *shared_mem = (char *)dev->mem_start;
mem_on(ioaddr, shared_mem, (ring_offset>>8));
eth_io_copy_and_sum(skb, dev->mem_start + (ring_offset & 0xff), count, 0);
mem_off(ioaddr);
}
static void
e21_block_output(struct device *dev, int count, const unsigned char *buf,
const int start_page)
{
short ioaddr = dev->base_addr;
volatile char *shared_mem = (char *)dev->mem_start;
readb(shared_mem + start_page);
mem_on(ioaddr, shared_mem, start_page);
memcpy_toio(shared_mem, buf, count);
mem_off(ioaddr);
}
static int
e21_close(struct device *dev)
{
short ioaddr = dev->base_addr;
if (ei_debug > 1)
printk("%s: Shutting down ethercard.\n", dev->name);
free_irq(dev->irq, NULL);
dev->irq = ei_status.saved_irq;
inb(ioaddr + E21_IRQ_LOW);
outb(0, ioaddr + E21_ASIC);
inb(ioaddr + E21_IRQ_HIGH);
outb(0, ioaddr + E21_ASIC);
irq2dev_map[dev->irq] = NULL;
ei_close(dev);
mem_off(ioaddr);
MOD_DEC_USE_COUNT;
return 0;
}
#ifdef HAVE_DEVLIST
struct netdev_entry e21_drv =
{"e21", e21_probe1, E21_IO_EXTENT, e21_probe_list};
#endif
#ifdef MODULE
#define MAX_E21_CARDS 4
#define NAMELEN 8
static char namelist[NAMELEN * MAX_E21_CARDS] = { 0, };
static struct device dev_e21[MAX_E21_CARDS] = {
{
NULL,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, NULL
},
};
static int io[MAX_E21_CARDS] = { 0, };
static int irq[MAX_E21_CARDS] = { 0, };
static int mem[MAX_E21_CARDS] = { 0, };
static int xcvr[MAX_E21_CARDS] = { 0, };
int
init_module(void)
{
int this_dev, found = 0;
for (this_dev = 0; this_dev < MAX_E21_CARDS; this_dev++) {
struct device *dev = &dev_e21[this_dev];
dev->name = namelist+(NAMELEN*this_dev);
dev->irq = irq[this_dev];
dev->base_addr = io[this_dev];
dev->mem_start = mem[this_dev];
dev->mem_end = xcvr[this_dev];
dev->init = e2100_probe;
if (io[this_dev] == 0) {
if (this_dev != 0) break;
printk(KERN_NOTICE "e2100.c: Presently autoprobing (not recommended) for a single card.\n");
}
if (register_netdev(dev) != 0) {
printk(KERN_WARNING "e2100.c: No E2100 card found (i/o = 0x%x).\n", io[this_dev]);
if (found != 0) return 0;
return -ENXIO;
}
found++;
}
return 0;
}
void
cleanup_module(void)
{
int this_dev;
for (this_dev = 0; this_dev < MAX_E21_CARDS; this_dev++) {
struct device *dev = &dev_e21[this_dev];
if (dev->priv != NULL) {
kfree(dev->priv);
dev->priv = NULL;
release_region(dev->base_addr, E21_IO_EXTENT);
unregister_netdev(dev);
}
}
}
#endif