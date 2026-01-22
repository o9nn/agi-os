static const char *version =
"3c503.c:v1.10 9/23/93  Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/delay.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <asm/io.h>
#include <asm/system.h>
#include <asm/byteorder.h>
#include "8390.h"
#include "3c503.h"
#define WRD_COUNT 4
int el2_probe(struct device *dev);
int el2_pio_probe(struct device *dev);
int el2_probe1(struct device *dev, int ioaddr);
static unsigned int netcard_portlist[] =
{ 0x300,0x310,0x330,0x350,0x250,0x280,0x2a0,0x2e0,0};
#define EL2_IO_EXTENT	16
#ifdef HAVE_DEVLIST
struct netdev_entry el2_drv =
{"3c503", el2_probe, EL1_IO_EXTENT, 0};
struct netdev_entry el2pio_drv =
{"3c503pio", el2_pioprobe1, EL1_IO_EXTENT, netcard_portlist};
#endif
static int el2_open(struct device *dev);
static int el2_close(struct device *dev);
static void el2_reset_8390(struct device *dev);
static void el2_init_card(struct device *dev);
static void el2_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page);
static void el2_block_input(struct device *dev, int count, struct sk_buff *skb,
int ring_offset);
static void el2_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
int
el2_probe(struct device *dev)
{
int *addr, addrs[] = { 0xddffe, 0xd9ffe, 0xcdffe, 0xc9ffe, 0};
int base_addr = dev->base_addr;
if (base_addr > 0x1ff)
return el2_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (addr = addrs; *addr; addr++) {
int i;
unsigned int base_bits = readb(*addr);
for(i = 7; i >= 0; i--, base_bits >>= 1)
if (base_bits & 0x1)
break;
if (base_bits != 1)
continue;
if (check_region(netcard_portlist[i], EL2_IO_EXTENT))
continue;
if (el2_probe1(dev, netcard_portlist[i]) == 0)
return 0;
}
#if ! defined(no_probe_nonshared_memory) && ! defined (HAVE_DEVLIST)
return el2_pio_probe(dev);
#else
return ENODEV;
#endif
}
#ifndef HAVE_DEVLIST
int
el2_pio_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return el2_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; netcard_portlist[i]; i++) {
int ioaddr = netcard_portlist[i];
if (check_region(ioaddr, EL2_IO_EXTENT))
continue;
if (el2_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
int
el2_probe1(struct device *dev, int ioaddr)
{
int i, iobase_reg, membase_reg, saved_406, wordlength;
static unsigned version_printed = 0;
unsigned long vendor_id;
if (inb(ioaddr + 0x408) == 0xff) {
udelay(1000);
return ENODEV;
}
iobase_reg = inb(ioaddr+0x403);
membase_reg = inb(ioaddr+0x404);
if (   (iobase_reg  & (iobase_reg - 1))
|| (membase_reg & (membase_reg - 1))) {
return ENODEV;
}
saved_406 = inb_p(ioaddr + 0x406);
outb_p(ECNTRL_RESET|ECNTRL_THIN, ioaddr + 0x406);
outb_p(ECNTRL_THIN, ioaddr + 0x406);
outb(ECNTRL_SAPROM|ECNTRL_THIN, ioaddr + 0x406);
vendor_id = inb(ioaddr)*0x10000 + inb(ioaddr + 1)*0x100 + inb(ioaddr + 2);
if ((vendor_id != OLD_3COM_ID) && (vendor_id != NEW_3COM_ID)) {
outb(saved_406, ioaddr + 0x406);
return ENODEV;
}
if (dev == NULL) {
printk("3c503.c: Passed a NULL device.\n");
dev = init_etherdev(0, 0);
}
if (ei_debug  &&  version_printed++ == 0)
printk("%s", version);
dev->base_addr = ioaddr;
if (ethdev_init(dev)) {
printk ("3c503: unable to allocate memory for dev->priv.\n");
return -ENOMEM;
}
printk("%s: 3c503 at i/o base %#3x, node ", dev->name, ioaddr);
for (i = 0; i < 6; i++)
printk(" %2.2x", dev->dev_addr[i] = inb(ioaddr + i));
outb(ECNTRL_THIN, ioaddr + 0x406);
outb_p(E8390_PAGE0, ioaddr + E8390_CMD);
outb_p(0, ioaddr + EN0_DCFG);
outb_p(E8390_PAGE2, ioaddr + E8390_CMD);
wordlength = inb_p(ioaddr + EN0_DCFG) & ENDCFG_WTS;
outb_p(E8390_PAGE0, ioaddr + E8390_CMD);
if (ei_debug > 2) printk(" memory jumpers %2.2x ", membase_reg);
outb(EGACFR_NORM, ioaddr + 0x405);
#if defined(EI8390_THICK) || defined(EL2_AUI)
ei_status.interface_num = 1;
#else
ei_status.interface_num = dev->mem_end & 0xf;
#endif
printk(", using %sternal xcvr.\n", ei_status.interface_num == 0 ? "in" : "ex");
if ((membase_reg & 0xf0) == 0) {
dev->mem_start = 0;
ei_status.name = "3c503-PIO";
} else {
dev->mem_start = ((membase_reg & 0xc0) ? 0xD8000 : 0xC8000) +
((membase_reg & 0xA0) ? 0x4000 : 0);
#define EL2_MEMSIZE (EL2_MB1_STOP_PG - EL2_MB1_START_PG)*256
#ifdef EL2MEMTEST
{
unsigned long mem_base = dev->mem_start;
unsigned int test_val = 0xbbadf00d;
writel(0xba5eba5e, mem_base);
for (i = sizeof(test_val); i < EL2_MEMSIZE; i+=sizeof(test_val)) {
writel(test_val, mem_base + i);
if (readl(mem_base) != 0xba5eba5e
|| readl(mem_base + i) != test_val) {
printk("3c503: memory failure or memory address conflict.\n");
dev->mem_start = 0;
ei_status.name = "3c503-PIO";
break;
}
test_val += 0x55555555;
writel(0, mem_base + i);
}
}
#endif
dev->mem_end = dev->rmem_end = dev->mem_start + EL2_MEMSIZE;
if (wordlength) {
dev->rmem_start = dev->mem_start;
ei_status.name = "3c503/16";
} else {
dev->rmem_start = TX_PAGES*256 + dev->mem_start;
ei_status.name = "3c503";
}
}
if (wordlength) {
ei_status.tx_start_page = EL2_MB0_START_PG;
ei_status.rx_start_page = EL2_MB1_START_PG;
} else {
ei_status.tx_start_page = EL2_MB1_START_PG;
ei_status.rx_start_page = EL2_MB1_START_PG + TX_PAGES;
}
ei_status.stop_page = EL2_MB1_STOP_PG;
ei_status.word16 = wordlength;
ei_status.reset_8390 = &el2_reset_8390;
ei_status.get_8390_hdr = &el2_get_8390_hdr;
ei_status.block_input = &el2_block_input;
ei_status.block_output = &el2_block_output;
request_region(ioaddr, EL2_IO_EXTENT, ei_status.name);
if (dev->irq == 2)
dev->irq = 9;
else if (dev->irq > 5 && dev->irq != 9) {
printk("3c503: configured interrupt %d invalid, will use autoIRQ.\n",
dev->irq);
dev->irq = 0;
}
ei_status.saved_irq = dev->irq;
dev->start = 0;
dev->open = &el2_open;
dev->stop = &el2_close;
if (dev->mem_start)
printk("%s: %s - %dkB RAM, 8kB shared mem window at %#6lx-%#6lx.\n",
dev->name, ei_status.name, (wordlength+1)<<3,
dev->mem_start, dev->mem_end-1);
else
{
ei_status.tx_start_page = EL2_MB1_START_PG;
ei_status.rx_start_page = EL2_MB1_START_PG + TX_PAGES;
printk("\n%s: %s, %dkB RAM, using programmed I/O (REJUMPER for SHARED MEMORY).\n",
dev->name, ei_status.name, (wordlength+1)<<3);
}
return 0;
}
static int
el2_open(struct device *dev)
{
if (dev->irq < 2) {
int irqlist[] = {5, 9, 3, 4, 0};
int *irqp = irqlist;
outb(EGACFR_NORM, E33G_GACFR);
do {
if (request_irq (*irqp, NULL, 0, "bogus", NULL) != -EBUSY) {
autoirq_setup(0);
outb_p(0x04 << ((*irqp == 9) ? 2 : *irqp), E33G_IDCFR);
outb_p(0x00, E33G_IDCFR);
if (*irqp == autoirq_report(0)
&& request_irq (dev->irq = *irqp, &ei_interrupt, 0, ei_status.name, NULL) == 0)
break;
}
} while (*++irqp);
if (*irqp == 0) {
outb(EGACFR_IRQOFF, E33G_GACFR);
return -EAGAIN;
}
} else {
if (request_irq(dev->irq, &ei_interrupt, 0, ei_status.name, NULL)) {
return -EAGAIN;
}
}
el2_init_card(dev);
ei_open(dev);
MOD_INC_USE_COUNT;
return 0;
}
static int
el2_close(struct device *dev)
{
free_irq(dev->irq, NULL);
dev->irq = ei_status.saved_irq;
irq2dev_map[dev->irq] = NULL;
outb(EGACFR_IRQOFF, E33G_GACFR);
ei_close(dev);
MOD_DEC_USE_COUNT;
return 0;
}
static void
el2_reset_8390(struct device *dev)
{
if (ei_debug > 1) {
printk("%s: Resetting the 3c503 board...", dev->name);
printk("%#lx=%#02x %#lx=%#02x %#lx=%#02x...", E33G_IDCFR, inb(E33G_IDCFR),
E33G_CNTRL, inb(E33G_CNTRL), E33G_GACFR, inb(E33G_GACFR));
}
outb_p(ECNTRL_RESET|ECNTRL_THIN, E33G_CNTRL);
ei_status.txing = 0;
outb_p(ei_status.interface_num==0 ? ECNTRL_THIN : ECNTRL_AUI, E33G_CNTRL);
el2_init_card(dev);
if (ei_debug > 1) printk("done\n");
}
static void
el2_init_card(struct device *dev)
{
outb_p(ei_status.interface_num==0 ? ECNTRL_THIN : ECNTRL_AUI, E33G_CNTRL);
outb(ei_status.rx_start_page, E33G_STARTPG);
outb(ei_status.stop_page,  E33G_STOPPG);
outb(0xff, E33G_VP2);
outb(0xff, E33G_VP1);
outb(0x00, E33G_VP0);
outb_p(0x00,  dev->base_addr + EN0_IMR);
outb(EGACFR_NORM, E33G_GACFR);
outb_p((0x04 << (dev->irq == 9 ? 2 : dev->irq)), E33G_IDCFR);
outb_p((WRD_COUNT << 1), E33G_DRQCNT);
outb_p(0x20, E33G_DMAAH);
outb_p(0x00, E33G_DMAAL);
return;
}
static void
el2_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page)
{
unsigned short int *wrd;
int boguscount;
unsigned short word;
if (ei_status.word16)
outb(EGACFR_RSEL|EGACFR_TCM, E33G_GACFR);
else
outb(EGACFR_NORM, E33G_GACFR);
if (dev->mem_start) {
unsigned long dest_addr = dev->mem_start +
((start_page - ei_status.tx_start_page) << 8);
memcpy_toio(dest_addr, buf, count);
outb(EGACFR_NORM, E33G_GACFR);
return;
}
word = (unsigned short)start_page;
outb(word&0xFF, E33G_DMAAH);
outb(word>>8, E33G_DMAAL);
outb_p((ei_status.interface_num ? ECNTRL_AUI : ECNTRL_THIN ) | ECNTRL_OUTPUT
| ECNTRL_START, E33G_CNTRL);
wrd = (unsigned short int *) buf;
count  = (count + 1) >> 1;
for(;;)
{
boguscount = 0x1000;
while ((inb(E33G_STATUS) & ESTAT_DPRDY) == 0)
{
if(!boguscount--)
{
printk("%s: FIFO blocked in el2_block_output.\n", dev->name);
el2_reset_8390(dev);
goto blocked;
}
}
if(count > WRD_COUNT)
{
outsw(E33G_FIFOH, wrd, WRD_COUNT);
wrd   += WRD_COUNT;
count -= WRD_COUNT;
}
else
{
outsw(E33G_FIFOH, wrd, count);
break;
}
}
blocked:;
outb_p(ei_status.interface_num==0 ? ECNTRL_THIN : ECNTRL_AUI, E33G_CNTRL);
return;
}
static void
el2_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr, int ring_page)
{
int boguscount;
unsigned long hdr_start = dev->mem_start + ((ring_page - EL2_MB1_START_PG)<<8);
unsigned short word;
if (dev->mem_start) {
memcpy_fromio(hdr, hdr_start, sizeof(struct e8390_pkt_hdr));
return;
}
word = (unsigned short)ring_page;
outb(word&0xFF, E33G_DMAAH);
outb(word>>8, E33G_DMAAL);
outb_p((ei_status.interface_num == 0 ? ECNTRL_THIN : ECNTRL_AUI) | ECNTRL_INPUT
| ECNTRL_START, E33G_CNTRL);
boguscount = 0x1000;
while ((inb(E33G_STATUS) & ESTAT_DPRDY) == 0)
{
if(!boguscount--)
{
printk("%s: FIFO blocked in el2_get_8390_hdr.\n", dev->name);
memset(hdr, 0x00, sizeof(struct e8390_pkt_hdr));
el2_reset_8390(dev);
goto blocked;
}
}
insw(E33G_FIFOH, hdr, (sizeof(struct e8390_pkt_hdr))>> 1);
blocked:;
outb_p(ei_status.interface_num == 0 ? ECNTRL_THIN : ECNTRL_AUI, E33G_CNTRL);
}
static void
el2_block_input(struct device *dev, int count, struct sk_buff *skb, int ring_offset)
{
int boguscount = 0;
unsigned short int *buf;
unsigned short word;
int end_of_ring = dev->rmem_end;
if (dev->mem_start) {
ring_offset -= (EL2_MB1_START_PG<<8);
if (dev->mem_start + ring_offset + count > end_of_ring) {
int semi_count = end_of_ring - (dev->mem_start + ring_offset);
memcpy_fromio(skb->data, dev->mem_start + ring_offset, semi_count);
count -= semi_count;
memcpy_fromio(skb->data + semi_count, dev->rmem_start, count);
} else {
eth_io_copy_and_sum(skb, dev->mem_start + ring_offset, count, 0);
}
return;
}
word = (unsigned short) ring_offset;
outb(word>>8, E33G_DMAAH);
outb(word&0xFF, E33G_DMAAL);
outb_p((ei_status.interface_num == 0 ? ECNTRL_THIN : ECNTRL_AUI) | ECNTRL_INPUT
| ECNTRL_START, E33G_CNTRL);
buf =  (unsigned short int *) skb->data;
count =  (count + 1) >> 1;
for(;;)
{
boguscount = 0x1000;
while ((inb(E33G_STATUS) & ESTAT_DPRDY) == 0)
{
if(!boguscount--)
{
printk("%s: FIFO blocked in el2_block_input.\n", dev->name);
el2_reset_8390(dev);
goto blocked;
}
}
if(count > WRD_COUNT)
{
insw(E33G_FIFOH, buf, WRD_COUNT);
buf   += WRD_COUNT;
count -= WRD_COUNT;
}
else
{
insw(E33G_FIFOH, buf, count);
break;
}
}
blocked:;
outb_p(ei_status.interface_num == 0 ? ECNTRL_THIN : ECNTRL_AUI, E33G_CNTRL);
return;
}
#ifdef MODULE
#define MAX_EL2_CARDS	4
#define NAMELEN 	8
static char namelist[NAMELEN * MAX_EL2_CARDS] = { 0, };
static struct device dev_el2[MAX_EL2_CARDS] = {
{
NULL,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, NULL
},
};
static int io[MAX_EL2_CARDS] = { 0, };
static int irq[MAX_EL2_CARDS]  = { 0, };
static int xcvr[MAX_EL2_CARDS] = { 0, };
int
init_module(void)
{
int this_dev, found = 0;
for (this_dev = 0; this_dev < MAX_EL2_CARDS; this_dev++) {
struct device *dev = &dev_el2[this_dev];
dev->name = namelist+(NAMELEN*this_dev);
dev->irq = irq[this_dev];
dev->base_addr = io[this_dev];
dev->mem_end = xcvr[this_dev];
dev->init = el2_probe;
if (io[this_dev] == 0)  {
if (this_dev != 0) break;
printk(KERN_NOTICE "3c503.c: Presently autoprobing (not recommended) for a single card.\n");
}
if (register_netdev(dev) != 0) {
printk(KERN_WARNING "3c503.c: No 3c503 card found (i/o = 0x%x).\n", io[this_dev]);
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
for (this_dev = 0; this_dev < MAX_EL2_CARDS; this_dev++) {
struct device *dev = &dev_el2[this_dev];
if (dev->priv != NULL) {
kfree(dev->priv);
dev->priv = NULL;
release_region(dev->base_addr, EL2_IO_EXTENT);
unregister_netdev(dev);
}
}
}
#endif