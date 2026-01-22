static const char *version = "smc-ultra32.c: 06/97 v1.00\n";
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
int ultra32_probe(struct device *dev);
int ultra32_probe1(struct device *dev, int ioaddr);
static int ultra32_open(struct device *dev);
static void ultra32_reset_8390(struct device *dev);
static void ultra32_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
static void ultra32_block_input(struct device *dev, int count,
struct sk_buff *skb, int ring_offset);
static void ultra32_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page);
static int ultra32_close(struct device *dev);
#define ULTRA32_CMDREG	0
#define	 ULTRA32_RESET	0x80
#define	 ULTRA32_MEMENB	0x40
#define ULTRA32_NIC_OFFSET 16
#define ULTRA32_IO_EXTENT 32
#define EN0_ERWCNT		0x08
#define ULTRA32_BASE	0xca0
#define ULTRA32_ID	0x1080a34d
#define ULTRA32_IDPORT	(-0x20)
#define ULTRA32_CFG1	0x04
#define ULTRA32_CFG2	0x05
#define ULTRA32_CFG3	(-0x18)
#define ULTRA32_CFG4	(-0x17)
#define ULTRA32_CFG5	(-0x16)
#define ULTRA32_CFG6	(-0x15)
#define ULTRA32_CFG7	0x0d
int ultra32_probe(struct device *dev)
{
const char *ifmap[] = {"UTP No Link", "", "UTP/AUI", "UTP/BNC"};
int ioaddr, edge, media;
if (!EISA_bus) return ENODEV;
for (ioaddr = 0x1000 + ULTRA32_BASE; ioaddr < 0x9000; ioaddr += 0x1000)
if (check_region(ioaddr, ULTRA32_IO_EXTENT) == 0 &&
inb(ioaddr + ULTRA32_IDPORT) != 0xff &&
inl(ioaddr + ULTRA32_IDPORT) == ULTRA32_ID) {
media = inb(ioaddr + ULTRA32_CFG7) & 0x03;
edge = inb(ioaddr + ULTRA32_CFG5) & 0x08;
printk("SMC Ultra32 in EISA Slot %d, Media: %s, %s IRQs.\n",
ioaddr >> 12, ifmap[media],
(edge ? "Edge Triggered" : "Level Sensitive"));
if (ultra32_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
int ultra32_probe1(struct device *dev, int ioaddr)
{
int i;
int checksum = 0;
const char *model_name;
static unsigned version_printed = 0;
unsigned char idreg = inb(ioaddr + 7);
unsigned char reg4 = inb(ioaddr + 4) & 0x7f;
if ((idreg & 0xf0) != 0x20)
return ENODEV;
outb(reg4, ioaddr + 4);
for (i = 0; i < 8; i++)
checksum += inb(ioaddr + 8 + i);
if ((checksum & 0xff) != 0xff)
return ENODEV;
if (dev == NULL) {
printk("smc-ultra32.c: Passed a NULL device.\n");
dev = init_etherdev(0, 0);
}
if (ei_debug  &&  version_printed++ == 0)
printk("%s", version);
model_name = "SMC Ultra32";
printk("%s: %s at 0x%X,", dev->name, model_name, ioaddr);
for (i = 0; i < 6; i++)
printk(" %2.2X", dev->dev_addr[i] = inb(ioaddr + 8 + i));
outb(0x80 | reg4, ioaddr + 4);
outb(0x80 | inb(ioaddr + 0x0c), ioaddr + 0x0c);
outb(0x00, ioaddr + 0x0b);
outb(reg4, ioaddr + 4);
if ((inb(ioaddr + ULTRA32_CFG5) & 0x40) == 0) {
printk("\nsmc-ultra32: Card RAM is disabled!  "
"Run EISA config utility.\n");
return ENODEV;
}
if ((inb(ioaddr + ULTRA32_CFG2) & 0x04) == 0)
printk("\nsmc-ultra32: Ignoring Bus-Master enable bit.  "
"Run EISA config utility.\n");
if (dev->irq < 2) {
unsigned char irqmap[] = {0, 9, 3, 5, 7, 10, 11, 15};
int irq = irqmap[inb(ioaddr + ULTRA32_CFG5) & 0x07];
if (irq == 0) {
printk(", failed to detect IRQ line.\n");
return -EAGAIN;
}
dev->irq = irq;
}
if (ethdev_init(dev)) {
printk (", no memory for dev->priv.\n");
return -ENOMEM;
}
request_region(ioaddr, ULTRA32_IO_EXTENT, model_name);
dev->base_addr = ioaddr + ULTRA32_NIC_OFFSET;
ei_status.reg0 = inb(ioaddr + ULTRA32_CFG3) & 0xfc;
dev->mem_start =  0xc0000 + ((ei_status.reg0 & 0x7c) << 11);
ei_status.name = model_name;
ei_status.word16 = 1;
ei_status.tx_start_page = 0;
ei_status.rx_start_page = TX_PAGES;
ei_status.stop_page = 128;
dev->rmem_start = dev->mem_start + TX_PAGES*256;
dev->mem_end = dev->rmem_end = dev->mem_start + 0x1fff;
printk(", IRQ %d, 32KB memory, 8KB window at 0x%lx-0x%lx.\n",
dev->irq, dev->mem_start, dev->mem_end);
ei_status.block_input = &ultra32_block_input;
ei_status.block_output = &ultra32_block_output;
ei_status.get_8390_hdr = &ultra32_get_8390_hdr;
ei_status.reset_8390 = &ultra32_reset_8390;
dev->open = &ultra32_open;
dev->stop = &ultra32_close;
NS8390_init(dev, 0);
return 0;
}
static int ultra32_open(struct device *dev)
{
int ioaddr = dev->base_addr - ULTRA32_NIC_OFFSET;
if (request_irq(dev->irq, ei_interrupt, 0, ei_status.name, dev))
return -EAGAIN;
outb(ULTRA32_MEMENB, ioaddr);
outb(0x80, ioaddr + ULTRA32_CFG6);
outb(0x84, ioaddr + 5);
outb(0x01, ioaddr + 6);
outb_p(E8390_NODMA+E8390_PAGE0, dev->base_addr);
outb(0xff, dev->base_addr + EN0_ERWCNT);
ei_open(dev);
MOD_INC_USE_COUNT;
return 0;
}
static int ultra32_close(struct device *dev)
{
int ioaddr = dev->base_addr - ULTRA32_NIC_OFFSET;
dev->start = 0;
dev->tbusy = 1;
if (ei_debug > 1)
printk("%s: Shutting down ethercard.\n", dev->name);
outb(0x00, ioaddr + ULTRA32_CFG6);
outb(0x00, ioaddr + 6);
free_irq(dev->irq, dev);
irq2dev_map[dev->irq] = 0;
NS8390_init(dev, 0);
MOD_DEC_USE_COUNT;
return 0;
}
static void ultra32_reset_8390(struct device *dev)
{
int ioaddr = dev->base_addr - ULTRA32_NIC_OFFSET;
outb(ULTRA32_RESET, ioaddr);
if (ei_debug > 1) printk("resetting Ultra32, t=%ld...", jiffies);
ei_status.txing = 0;
outb(ULTRA32_MEMENB, ioaddr);
outb(0x80, ioaddr + ULTRA32_CFG6);
outb(0x84, ioaddr + 5);
outb(0x01, ioaddr + 6);
if (ei_debug > 1) printk("reset done\n");
return;
}
static void ultra32_get_8390_hdr(struct device *dev,
struct e8390_pkt_hdr *hdr,
int ring_page)
{
unsigned long hdr_start = dev->mem_start + ((ring_page & 0x1f) << 8);
unsigned int RamReg = dev->base_addr - ULTRA32_NIC_OFFSET + ULTRA32_CFG3;
outb(ei_status.reg0 | ((ring_page & 0x60) >> 5), RamReg);
#ifdef notdef
memcpy_fromio(hdr, hdr_start, sizeof(struct e8390_pkt_hdr));
#else
((unsigned int*)hdr)[0] = readl(hdr_start);
#endif
}
static void ultra32_block_input(struct device *dev,
int count,
struct sk_buff *skb,
int ring_offset)
{
unsigned long xfer_start = dev->mem_start + (ring_offset & 0x1fff);
unsigned int RamReg = dev->base_addr - ULTRA32_NIC_OFFSET + ULTRA32_CFG3;
if ((ring_offset & ~0x1fff) != ((ring_offset + count - 1) & ~0x1fff)) {
int semi_count = 8192 - (ring_offset & 0x1FFF);
memcpy_fromio(skb->data, xfer_start, semi_count);
count -= semi_count;
if (ring_offset < 96*256) {
ring_offset += semi_count;
outb(ei_status.reg0 | ((ring_offset & 0x6000) >> 13), RamReg);
memcpy_fromio(skb->data + semi_count, dev->mem_start, count);
} else {
outb(ei_status.reg0, RamReg);
memcpy_fromio(skb->data + semi_count, dev->rmem_start, count);
}
} else {
eth_io_copy_and_sum(skb, xfer_start, count, 0);
}
}
static void ultra32_block_output(struct device *dev,
int count,
const unsigned char *buf,
int start_page)
{
unsigned long xfer_start = dev->mem_start + (start_page<<8);
unsigned int RamReg = dev->base_addr - ULTRA32_NIC_OFFSET + ULTRA32_CFG3;
outb(ei_status.reg0, RamReg);
memcpy_toio(xfer_start, buf, count);
}
#ifdef MODULE
#define MAX_ULTRA32_CARDS   4
#define NAMELEN		    8
static char namelist[NAMELEN * MAX_ULTRA32_CARDS] = { 0, };
static struct device dev_ultra[MAX_ULTRA32_CARDS] = {
{
NULL,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, NULL
},
};
int init_module(void)
{
int this_dev, found = 0;
for (this_dev = 0; this_dev < MAX_ULTRA32_CARDS; this_dev++) {
struct device *dev = &dev_ultra[this_dev];
dev->name = namelist+(NAMELEN*this_dev);
dev->init = ultra32_probe;
if (register_netdev(dev) != 0) {
if (found > 0) return 0;
printk(KERN_WARNING "smc-ultra32.c: No SMC Ultra32 found.\n");
return -ENXIO;
}
found++;
}
return 0;
}
void cleanup_module(void)
{
int this_dev;
for (this_dev = 0; this_dev < MAX_ULTRA32_CARDS; this_dev++) {
struct device *dev = &dev_ultra[this_dev];
if (dev->priv != NULL) {
int ioaddr = dev->base_addr - ULTRA32_NIC_OFFSET;
kfree(dev->priv);
dev->priv = NULL;
release_region(ioaddr, ULTRA32_IO_EXTENT);
unregister_netdev(dev);
}
}
}
#endif