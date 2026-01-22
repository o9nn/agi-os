static const char *version =
"hp.c:v1.10 9/23/94 Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/ioport.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <asm/system.h>
#include <asm/io.h>
#include "8390.h"
static unsigned int hppclan_portlist[] =
{ 0x300, 0x320, 0x340, 0x280, 0x2C0, 0x200, 0x240, 0};
#define HP_IO_EXTENT	32
#define HP_DATAPORT		0x0c
#define HP_ID			0x07
#define HP_CONFIGURE	0x08
#define	 HP_RUN			0x01
#define	 HP_IRQ			0x0E
#define	 HP_DATAON		0x10
#define NIC_OFFSET		0x10
#define HP_START_PG		0x00
#define HP_8BSTOP_PG	0x80
#define HP_16BSTOP_PG	0xFF
int hp_probe(struct device *dev);
int hp_probe1(struct device *dev, int ioaddr);
static int hp_open(struct device *dev);
static int hp_close(struct device *dev);
static void hp_reset_8390(struct device *dev);
static void hp_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
static void hp_block_input(struct device *dev, int count,
struct sk_buff *skb , int ring_offset);
static void hp_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page);
static void hp_init_card(struct device *dev);
static char irqmap[16] = { 0, 0, 4, 6, 8,10, 0,14, 0, 4, 2,12,0,0,0,0};
#ifdef HAVE_DEVLIST
struct netdev_entry netcard_drv =
{"hp", hp_probe1, HP_IO_EXTENT, hppclan_portlist};
#else
int hp_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return hp_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; hppclan_portlist[i]; i++) {
int ioaddr = hppclan_portlist[i];
if (check_region(ioaddr, HP_IO_EXTENT))
continue;
if (hp_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
int hp_probe1(struct device *dev, int ioaddr)
{
int i, board_id, wordmode;
const char *name;
static unsigned version_printed = 0;
if (inb(ioaddr) != 0x08
|| inb(ioaddr+1) != 0x00
|| inb(ioaddr+2) != 0x09
|| inb(ioaddr+14) == 0x57)
return ENODEV;
if ((board_id = inb(ioaddr + HP_ID)) & 0x80) {
name = "HP27247";
wordmode = 1;
} else {
name = "HP27250";
wordmode = 0;
}
if (dev == NULL) {
printk("hp.c: Passed a NULL device.\n");
dev = init_etherdev(0, 0);
}
if (ei_debug  &&  version_printed++ == 0)
printk("%s", version);
printk("%s: %s (ID %02x) at %#3x,", dev->name, name, board_id, ioaddr);
for(i = 0; i < ETHER_ADDR_LEN; i++)
printk(" %2.2x", dev->dev_addr[i] = inb(ioaddr + i));
if (dev->irq < 2) {
int irq_16list[] = { 11, 10, 5, 3, 4, 7, 9, 0};
int irq_8list[] = { 7, 5, 3, 4, 9, 0};
int *irqp = wordmode ? irq_16list : irq_8list;
do {
int irq = *irqp;
if (request_irq (irq, NULL, 0, "bogus", NULL) != -EBUSY) {
autoirq_setup(0);
outb_p(irqmap[irq] | HP_RUN, ioaddr + HP_CONFIGURE);
outb_p( 0x00 | HP_RUN, ioaddr + HP_CONFIGURE);
if (irq == autoirq_report(0)
&& request_irq (irq, &ei_interrupt, 0, "hp", NULL) == 0) {
printk(" selecting IRQ %d.\n", irq);
dev->irq = *irqp;
break;
}
}
} while (*++irqp);
if (*irqp == 0) {
printk(" no free IRQ lines.\n");
return EBUSY;
}
} else {
if (dev->irq == 2)
dev->irq = 9;
if (request_irq(dev->irq, ei_interrupt, 0, "hp", NULL)) {
printk (" unable to get IRQ %d.\n", dev->irq);
return EBUSY;
}
}
if (ethdev_init(dev)) {
printk (" unable to get memory for dev->priv.\n");
free_irq(dev->irq, NULL);
return -ENOMEM;
}
request_region(ioaddr, HP_IO_EXTENT,"hp");
dev->base_addr = ioaddr + NIC_OFFSET;
dev->open = &hp_open;
dev->stop = &hp_close;
ei_status.name = name;
ei_status.word16 = wordmode;
ei_status.tx_start_page = HP_START_PG;
ei_status.rx_start_page = HP_START_PG + TX_PAGES;
ei_status.stop_page = wordmode ? HP_16BSTOP_PG : HP_8BSTOP_PG;
ei_status.reset_8390 = &hp_reset_8390;
ei_status.get_8390_hdr = &hp_get_8390_hdr;
ei_status.block_input = &hp_block_input;
ei_status.block_output = &hp_block_output;
hp_init_card(dev);
return 0;
}
static int
hp_open(struct device *dev)
{
ei_open(dev);
MOD_INC_USE_COUNT;
return 0;
}
static int
hp_close(struct device *dev)
{
ei_close(dev);
MOD_DEC_USE_COUNT;
return 0;
}
static void
hp_reset_8390(struct device *dev)
{
int hp_base = dev->base_addr - NIC_OFFSET;
int saved_config = inb_p(hp_base + HP_CONFIGURE);
if (ei_debug > 1) printk("resetting the 8390 time=%ld...", jiffies);
outb_p(0x00, hp_base + HP_CONFIGURE);
ei_status.txing = 0;
SLOW_DOWN_IO;
SLOW_DOWN_IO;
outb_p(saved_config, hp_base + HP_CONFIGURE);
SLOW_DOWN_IO; SLOW_DOWN_IO;
if ((inb_p(hp_base+NIC_OFFSET+EN0_ISR) & ENISR_RESET) == 0)
printk("%s: hp_reset_8390() did not complete.\n", dev->name);
if (ei_debug > 1) printk("8390 reset done (%ld).", jiffies);
return;
}
static void
hp_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr, int ring_page)
{
int nic_base = dev->base_addr;
int saved_config = inb_p(nic_base - NIC_OFFSET + HP_CONFIGURE);
outb_p(saved_config | HP_DATAON, nic_base - NIC_OFFSET + HP_CONFIGURE);
outb_p(E8390_NODMA+E8390_PAGE0+E8390_START, nic_base);
outb_p(sizeof(struct e8390_pkt_hdr), nic_base + EN0_RCNTLO);
outb_p(0, nic_base + EN0_RCNTHI);
outb_p(0, nic_base + EN0_RSARLO);
outb_p(ring_page, nic_base + EN0_RSARHI);
outb_p(E8390_RREAD+E8390_START, nic_base);
if (ei_status.word16)
insw(nic_base - NIC_OFFSET + HP_DATAPORT, hdr, sizeof(struct e8390_pkt_hdr)>>1);
else
insb(nic_base - NIC_OFFSET + HP_DATAPORT, hdr, sizeof(struct e8390_pkt_hdr));
outb_p(saved_config & (~HP_DATAON), nic_base - NIC_OFFSET + HP_CONFIGURE);
}
static void
hp_block_input(struct device *dev, int count, struct sk_buff *skb, int ring_offset)
{
int nic_base = dev->base_addr;
int saved_config = inb_p(nic_base - NIC_OFFSET + HP_CONFIGURE);
int xfer_count = count;
char *buf = skb->data;
outb_p(saved_config | HP_DATAON, nic_base - NIC_OFFSET + HP_CONFIGURE);
outb_p(E8390_NODMA+E8390_PAGE0+E8390_START, nic_base);
outb_p(count & 0xff, nic_base + EN0_RCNTLO);
outb_p(count >> 8, nic_base + EN0_RCNTHI);
outb_p(ring_offset & 0xff, nic_base + EN0_RSARLO);
outb_p(ring_offset >> 8, nic_base + EN0_RSARHI);
outb_p(E8390_RREAD+E8390_START, nic_base);
if (ei_status.word16) {
insw(nic_base - NIC_OFFSET + HP_DATAPORT,buf,count>>1);
if (count & 0x01)
buf[count-1] = inb(nic_base - NIC_OFFSET + HP_DATAPORT), xfer_count++;
} else {
insb(nic_base - NIC_OFFSET + HP_DATAPORT, buf, count);
}
if (ei_debug > 0) {
int high = inb_p(nic_base + EN0_RSARHI);
int low = inb_p(nic_base + EN0_RSARLO);
int addr = (high << 8) + low;
if (((ring_offset + xfer_count) & 0xff) != (addr & 0xff))
printk("%s: RX transfer address mismatch, %#4.4x vs. %#4.4x (actual).\n",
dev->name, ring_offset + xfer_count, addr);
}
outb_p(saved_config & (~HP_DATAON), nic_base - NIC_OFFSET + HP_CONFIGURE);
}
static void
hp_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page)
{
int nic_base = dev->base_addr;
int saved_config = inb_p(nic_base - NIC_OFFSET + HP_CONFIGURE);
outb_p(saved_config | HP_DATAON, nic_base - NIC_OFFSET + HP_CONFIGURE);
if (ei_status.word16 && (count & 0x01))
count++;
outb_p(E8390_PAGE0+E8390_START+E8390_NODMA, nic_base);
#ifdef NE8390_RW_BUGFIX
outb_p(0x42, nic_base + EN0_RCNTLO);
outb_p(0,	nic_base + EN0_RCNTHI);
outb_p(0xff, nic_base + EN0_RSARLO);
outb_p(0x00, nic_base + EN0_RSARHI);
#define NE_CMD	 	0x00
outb_p(E8390_RREAD+E8390_START, nic_base + NE_CMD);
inb_p(0x61);
inb_p(0x61);
#endif
outb_p(count & 0xff, nic_base + EN0_RCNTLO);
outb_p(count >> 8,	 nic_base + EN0_RCNTHI);
outb_p(0x00, nic_base + EN0_RSARLO);
outb_p(start_page, nic_base + EN0_RSARHI);
outb_p(E8390_RWRITE+E8390_START, nic_base);
if (ei_status.word16) {
outsw(nic_base - NIC_OFFSET + HP_DATAPORT, buf, count>>1);
} else {
outsb(nic_base - NIC_OFFSET + HP_DATAPORT, buf, count);
}
if (ei_debug > 0) {
int high = inb_p(nic_base + EN0_RSARHI);
int low  = inb_p(nic_base + EN0_RSARLO);
int addr = (high << 8) + low;
if ((start_page << 8) + count != addr)
printk("%s: TX Transfer address mismatch, %#4.4x vs. %#4.4x.\n",
dev->name, (start_page << 8) + count, addr);
}
outb_p(saved_config & (~HP_DATAON), nic_base - NIC_OFFSET + HP_CONFIGURE);
return;
}
static void
hp_init_card(struct device *dev)
{
int irq = dev->irq;
NS8390_init(dev, 0);
outb_p(irqmap[irq&0x0f] | HP_RUN,
dev->base_addr - NIC_OFFSET + HP_CONFIGURE);
return;
}
#ifdef MODULE
#define MAX_HP_CARDS	4
#define NAMELEN		8
static char namelist[NAMELEN * MAX_HP_CARDS] = { 0, };
static struct device dev_hp[MAX_HP_CARDS] = {
{
NULL,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, NULL
},
};
static int io[MAX_HP_CARDS] = { 0, };
static int irq[MAX_HP_CARDS]  = { 0, };
int
init_module(void)
{
int this_dev, found = 0;
for (this_dev = 0; this_dev < MAX_HP_CARDS; this_dev++) {
struct device *dev = &dev_hp[this_dev];
dev->name = namelist+(NAMELEN*this_dev);
dev->irq = irq[this_dev];
dev->base_addr = io[this_dev];
dev->init = hp_probe;
if (io[this_dev] == 0)  {
if (this_dev != 0) break;
printk(KERN_NOTICE "hp.c: Presently autoprobing (not recommended) for a single card.\n");
}
if (register_netdev(dev) != 0) {
printk(KERN_WARNING "hp.c: No HP card found (i/o = 0x%x).\n", io[this_dev]);
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
for (this_dev = 0; this_dev < MAX_HP_CARDS; this_dev++) {
struct device *dev = &dev_hp[this_dev];
if (dev->priv != NULL) {
int ioaddr = dev->base_addr - NIC_OFFSET;
kfree(dev->priv);
dev->priv = NULL;
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = NULL;
release_region(ioaddr, HP_IO_EXTENT);
unregister_netdev(dev);
}
}
}
#endif