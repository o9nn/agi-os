static const char *version =
"smc-ultra.c:v2.02 2/3/98 Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <asm/io.h>
#include <asm/system.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include "8390.h"
static unsigned int ultra_portlist[] =
{0x200, 0x220, 0x240, 0x280, 0x300, 0x340, 0x380, 0};
int ultra_probe(struct device *dev);
int ultra_probe1(struct device *dev, int ioaddr);
static int ultra_open(struct device *dev);
static void ultra_reset_8390(struct device *dev);
static void ultra_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
static void ultra_block_input(struct device *dev, int count,
struct sk_buff *skb, int ring_offset);
static void ultra_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page);
static void ultra_pio_get_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
static void ultra_pio_input(struct device *dev, int count,
struct sk_buff *skb, int ring_offset);
static void ultra_pio_output(struct device *dev, int count,
const unsigned char *buf, const int start_page);
static int ultra_close_card(struct device *dev);
#define START_PG		0x00
#define ULTRA_CMDREG	0
#define	 ULTRA_RESET	0x80
#define	 ULTRA_MEMENB	0x40
#define IOPD	0x02
#define IOPA	0x07
#define ULTRA_NIC_OFFSET  16
#define ULTRA_IO_EXTENT 32
#define EN0_ERWCNT		0x08
#ifdef HAVE_DEVLIST
struct netdev_entry ultra_drv =
{"ultra", ultra_probe1, NETCARD_IO_EXTENT, netcard_portlist};
#else
int ultra_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return ultra_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; ultra_portlist[i]; i++) {
int ioaddr = ultra_portlist[i];
if (check_region(ioaddr, ULTRA_IO_EXTENT))
continue;
if (ultra_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
int ultra_probe1(struct device *dev, int ioaddr)
{
int i;
int checksum = 0;
const char *model_name;
unsigned char eeprom_irq = 0;
static unsigned version_printed = 0;
unsigned char num_pages, irqreg, addr, piomode;
unsigned char idreg = inb(ioaddr + 7);
unsigned char reg4 = inb(ioaddr + 4) & 0x7f;
if ((idreg & 0xF0) != 0x20
&& (idreg & 0xF0) != 0x40)
return ENODEV;
outb(reg4, ioaddr + 4);
for (i = 0; i < 8; i++)
checksum += inb(ioaddr + 8 + i);
if ((checksum & 0xff) != 0xFF)
return ENODEV;
if (dev == NULL)
dev = init_etherdev(0, 0);
if (ei_debug  &&  version_printed++ == 0)
printk("%s", version);
model_name = (idreg & 0xF0) == 0x20 ? "SMC Ultra" : "SMC EtherEZ";
printk("%s: %s at %#3x,", dev->name, model_name, ioaddr);
for (i = 0; i < 6; i++)
printk(" %2.2X", dev->dev_addr[i] = inb(ioaddr + 8 + i));
outb(0x80 | reg4, ioaddr + 4);
outb(0x80 | inb(ioaddr + 0x0c), ioaddr + 0x0c);
piomode = inb(ioaddr + 0x8);
addr = inb(ioaddr + 0xb);
irqreg = inb(ioaddr + 0xd);
outb(reg4, ioaddr + 4);
if (dev->irq < 2) {
unsigned char irqmap[] = {0, 9, 3, 5, 7, 10, 11, 15};
int irq;
irq = irqmap[((irqreg & 0x40) >> 4) + ((irqreg & 0x0c) >> 2)];
if (irq == 0) {
printk(", failed to detect IRQ line.\n");
return -EAGAIN;
}
dev->irq = irq;
eeprom_irq = 1;
}
if (ethdev_init(dev)) {
printk (", no memory for dev->priv.\n");
return -ENOMEM;
}
request_region(ioaddr, ULTRA_IO_EXTENT, model_name);
dev->base_addr = ioaddr+ULTRA_NIC_OFFSET;
{
int addr_tbl[4] = {0x0C0000, 0x0E0000, 0xFC0000, 0xFE0000};
short num_pages_tbl[4] = {0x20, 0x40, 0x80, 0xff};
dev->mem_start = ((addr & 0x0f) << 13) + addr_tbl[(addr >> 6) & 3] ;
num_pages = num_pages_tbl[(addr >> 4) & 3];
}
ei_status.name = model_name;
ei_status.word16 = 1;
ei_status.tx_start_page = START_PG;
ei_status.rx_start_page = START_PG + TX_PAGES;
ei_status.stop_page = num_pages;
dev->rmem_start = dev->mem_start + TX_PAGES*256;
dev->mem_end = dev->rmem_end
= dev->mem_start + (ei_status.stop_page - START_PG)*256;
if (piomode) {
printk(",%s IRQ %d programmed-I/O mode.\n",
eeprom_irq ? "EEPROM" : "assigned ", dev->irq);
ei_status.block_input = &ultra_pio_input;
ei_status.block_output = &ultra_pio_output;
ei_status.get_8390_hdr = &ultra_pio_get_hdr;
} else {
printk(",%s IRQ %d memory %#lx-%#lx.\n", eeprom_irq ? "" : "assigned ",
dev->irq, dev->mem_start, dev->mem_end-1);
ei_status.block_input = &ultra_block_input;
ei_status.block_output = &ultra_block_output;
ei_status.get_8390_hdr = &ultra_get_8390_hdr;
}
ei_status.reset_8390 = &ultra_reset_8390;
dev->open = &ultra_open;
dev->stop = &ultra_close_card;
NS8390_init(dev, 0);
return 0;
}
static int
ultra_open(struct device *dev)
{
int ioaddr = dev->base_addr - ULTRA_NIC_OFFSET;
unsigned char irq2reg[] = {0, 0, 0x04, 0x08, 0, 0x0C, 0, 0x40,
0, 0x04, 0x44, 0x48, 0, 0, 0, 0x4C, };
if (request_irq(dev->irq, ei_interrupt, 0, ei_status.name, dev))
return -EAGAIN;
outb(0x00, ioaddr);
outb(0x80, ioaddr + 5);
outb(inb(ioaddr + 4) | 0x80, ioaddr + 4);
outb((inb(ioaddr + 13) & ~0x4C) | irq2reg[dev->irq], ioaddr + 13);
outb(inb(ioaddr + 4) & 0x7f, ioaddr + 4);
if (ei_status.block_input == &ultra_pio_input) {
outb(0x11, ioaddr + 6);
outb(0x01, ioaddr + 0x19);
} else
outb(0x01, ioaddr + 6);
outb_p(E8390_NODMA+E8390_PAGE0, dev->base_addr);
outb(0xff, dev->base_addr + EN0_ERWCNT);
ei_open(dev);
MOD_INC_USE_COUNT;
return 0;
}
static void
ultra_reset_8390(struct device *dev)
{
int cmd_port = dev->base_addr - ULTRA_NIC_OFFSET;
outb(ULTRA_RESET, cmd_port);
if (ei_debug > 1) printk("resetting Ultra, t=%ld...", jiffies);
ei_status.txing = 0;
outb(0x00, cmd_port);
outb(0x80, cmd_port + 5);
if (ei_status.block_input == &ultra_pio_input)
outb(0x11, cmd_port + 6);
else
outb(0x01, cmd_port + 6);
if (ei_debug > 1) printk("reset done\n");
return;
}
static void
ultra_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr, int ring_page)
{
unsigned long hdr_start = dev->mem_start + ((ring_page - START_PG)<<8);
outb(ULTRA_MEMENB, dev->base_addr - ULTRA_NIC_OFFSET);
#ifdef notdef
memcpy_fromio(hdr, hdr_start, sizeof(struct e8390_pkt_hdr));
#else
((unsigned int*)hdr)[0] = readl(hdr_start);
#endif
outb(0x00, dev->base_addr - ULTRA_NIC_OFFSET);
}
static void
ultra_block_input(struct device *dev, int count, struct sk_buff *skb, int ring_offset)
{
unsigned long xfer_start = dev->mem_start + ring_offset - (START_PG<<8);
outb(ULTRA_MEMENB, dev->base_addr - ULTRA_NIC_OFFSET);
if (xfer_start + count > dev->rmem_end) {
int semi_count = dev->rmem_end - xfer_start;
memcpy_fromio(skb->data, xfer_start, semi_count);
count -= semi_count;
memcpy_fromio(skb->data + semi_count, dev->rmem_start, count);
} else {
eth_io_copy_and_sum(skb, xfer_start, count, 0);
}
outb(0x00, dev->base_addr - ULTRA_NIC_OFFSET);
}
static void
ultra_block_output(struct device *dev, int count, const unsigned char *buf,
int start_page)
{
unsigned long shmem = dev->mem_start + ((start_page - START_PG)<<8);
outb(ULTRA_MEMENB, dev->base_addr - ULTRA_NIC_OFFSET);
memcpy_toio(shmem, buf, count);
outb(0x00, dev->base_addr - ULTRA_NIC_OFFSET);
}
static void ultra_pio_get_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page)
{
int ioaddr = dev->base_addr - ULTRA_NIC_OFFSET;
outb(0x00, ioaddr + IOPA);
outb(ring_page, ioaddr + IOPA);
insw(ioaddr + IOPD, hdr, sizeof(struct e8390_pkt_hdr)>>1);
}
static void ultra_pio_input(struct device *dev, int count,
struct sk_buff *skb, int ring_offset)
{
int ioaddr = dev->base_addr - ULTRA_NIC_OFFSET;
char *buf = skb->data;
outb(ring_offset, ioaddr + IOPA);
outb(ring_offset >> 8, ioaddr + IOPA);
insw(ioaddr + IOPD, buf, (count+1)>>1);
}
static void ultra_pio_output(struct device *dev, int count,
const unsigned char *buf, const int start_page)
{
int ioaddr = dev->base_addr - ULTRA_NIC_OFFSET;
outb(0x00, ioaddr + IOPA);
outb(start_page, ioaddr + IOPA);
outsw(ioaddr + IOPD, buf, (count+1)>>1);
}
static int
ultra_close_card(struct device *dev)
{
int ioaddr = dev->base_addr - ULTRA_NIC_OFFSET;
dev->start = 0;
dev->tbusy = 1;
if (ei_debug > 1)
printk("%s: Shutting down ethercard.\n", dev->name);
outb(0x00, ioaddr + 6);
free_irq(dev->irq, dev);
irq2dev_map[dev->irq] = 0;
NS8390_init(dev, 0);
MOD_DEC_USE_COUNT;
return 0;
}
#ifdef MODULE
#define MAX_ULTRA_CARDS	4
#define NAMELEN		8
static char namelist[NAMELEN * MAX_ULTRA_CARDS] = { 0, };
static struct device dev_ultra[MAX_ULTRA_CARDS] = {
{
NULL,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, NULL
},
};
static int io[MAX_ULTRA_CARDS] = { 0, };
static int irq[MAX_ULTRA_CARDS]  = { 0, };
int
init_module(void)
{
int this_dev, found = 0;
for (this_dev = 0; this_dev < MAX_ULTRA_CARDS; this_dev++) {
struct device *dev = &dev_ultra[this_dev];
dev->name = namelist+(NAMELEN*this_dev);
dev->irq = irq[this_dev];
dev->base_addr = io[this_dev];
dev->init = ultra_probe;
if (io[this_dev] == 0)  {
if (this_dev != 0) break;
printk(KERN_NOTICE "smc-ultra.c: Presently autoprobing (not recommended) for a single card.\n");
}
if (register_netdev(dev) != 0) {
printk(KERN_WARNING "smc-ultra.c: No SMC Ultra card found (i/o = 0x%x).\n", io[this_dev]);
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
for (this_dev = 0; this_dev < MAX_ULTRA_CARDS; this_dev++) {
struct device *dev = &dev_ultra[this_dev];
if (dev->priv != NULL) {
int ioaddr = dev->base_addr - ULTRA_NIC_OFFSET;
kfree(dev->priv);
dev->priv = NULL;
release_region(ioaddr, ULTRA_IO_EXTENT);
unregister_netdev(dev);
}
}
}
#endif