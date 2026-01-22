static const char *version =
"hp-plus.c:v1.10 9/24/94 Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/string.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/ioport.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <asm/system.h>
#include <asm/io.h>
#include "8390.h"
static unsigned int hpplus_portlist[] =
{0x200, 0x240, 0x280, 0x2C0, 0x300, 0x320, 0x340, 0};
#define HP_ID 0x00
#define HP_PAGING 0x02
#define HPP_OPTION 0x04
#define HPP_OUT_ADDR 0x08
#define HPP_IN_ADDR 0x0A
#define HP_DATAPORT 0x0c
#define NIC_OFFSET 0x10
#define HP_IO_EXTENT 32
#define HP_START_PG 0x00
#define HP_STOP_PG 0x80
enum PageName {
Perf_Page = 0,
MAC_Page = 1,
HW_Page = 2,
LAN_Page = 4,
ID_Page = 6 };
enum HP_Option {
NICReset = 1, ChipReset = 2,
EnableIRQ = 4, FakeIntr = 8, BootROMEnb = 0x10, IOEnb = 0x20,
MemEnable = 0x40, ZeroWait = 0x80, MemDisable = 0x1000, };
int hp_plus_probe(struct device *dev);
int hpp_probe1(struct device *dev, int ioaddr);
static void hpp_reset_8390(struct device *dev);
static int hpp_open(struct device *dev);
static int hpp_close(struct device *dev);
static void hpp_mem_block_input(struct device *dev, int count,
struct sk_buff *skb, int ring_offset);
static void hpp_mem_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page);
static void hpp_mem_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
static void hpp_io_block_input(struct device *dev, int count,
struct sk_buff *skb, int ring_offset);
static void hpp_io_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page);
static void hpp_io_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
#ifdef HAVE_DEVLIST
struct netdev_entry hpplus_drv =
{"hpplus", hpp_probe1, HP_IO_EXTENT, hpplus_portlist};
#else
int hp_plus_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return hpp_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; hpplus_portlist[i]; i++) {
int ioaddr = hpplus_portlist[i];
if (check_region(ioaddr, HP_IO_EXTENT))
continue;
if (hpp_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
int hpp_probe1(struct device *dev, int ioaddr)
{
int i;
unsigned char checksum = 0;
const char *name = "HP-PC-LAN+";
int mem_start;
static unsigned version_printed = 0;
if (inw(ioaddr + HP_ID) != 0x4850
|| (inw(ioaddr + HP_PAGING) & 0xfff0) != 0x5300)
return ENODEV;
if (dev == NULL) {
printk("hp-plus.c: Passed a NULL device.\n");
dev = init_etherdev(0, 0);
}
if (ei_debug && version_printed++ == 0)
printk("%s", version);
printk("%s: %s at %#3x,", dev->name, name, ioaddr);
outw(MAC_Page, ioaddr + HP_PAGING);
for(i = 0; i < ETHER_ADDR_LEN; i++) {
unsigned char inval = inb(ioaddr + 8 + i);
dev->dev_addr[i] = inval;
checksum += inval;
printk(" %2.2x", inval);
}
checksum += inb(ioaddr + 14);
if (checksum != 0xff) {
printk(" bad checksum %2.2x.\n", checksum);
return ENODEV;
} else {
outw(ID_Page, ioaddr + HP_PAGING);
printk(" ID %4.4x", inw(ioaddr + 12));
}
if (ethdev_init(dev)) {
printk ("hp-plus.c: unable to allocate memory for dev->priv.\n");
return -ENOMEM;
}
request_region(ioaddr, HP_IO_EXTENT,"hp-plus");
outw(HW_Page, ioaddr + HP_PAGING);
{
int irq = inb(ioaddr + 13) & 0x0f;
int option = inw(ioaddr + HPP_OPTION);
dev->irq = irq;
if (option & MemEnable) {
mem_start = inw(ioaddr + 9) << 8;
printk(", IRQ %d, memory address %#x.\n", irq, mem_start);
} else {
mem_start = 0;
printk(", IRQ %d, programmed-I/O mode.\n", irq);
}
}
outw((HP_START_PG + TX_2X_PAGES) | ((HP_STOP_PG - 1) << 8), ioaddr + 14);
dev->base_addr = ioaddr + NIC_OFFSET;
dev->open = &hpp_open;
dev->stop = &hpp_close;
ei_status.name = name;
ei_status.word16 = 0;
ei_status.tx_start_page = HP_START_PG;
ei_status.rx_start_page = HP_START_PG + TX_2X_PAGES;
ei_status.stop_page = HP_STOP_PG;
ei_status.reset_8390 = &hpp_reset_8390;
ei_status.block_input = &hpp_io_block_input;
ei_status.block_output = &hpp_io_block_output;
ei_status.get_8390_hdr = &hpp_io_get_8390_hdr;
if (mem_start) {
ei_status.block_input = &hpp_mem_block_input;
ei_status.block_output = &hpp_mem_block_output;
ei_status.get_8390_hdr = &hpp_mem_get_8390_hdr;
dev->mem_start = mem_start;
dev->rmem_start = dev->mem_start + TX_2X_PAGES*256;
dev->mem_end = dev->rmem_end
= dev->mem_start + (HP_STOP_PG - HP_START_PG)*256;
}
outw(Perf_Page, ioaddr + HP_PAGING);
NS8390_init(dev, 0);
outw(inw(ioaddr + HPP_OPTION) & ~EnableIRQ, ioaddr + HPP_OPTION);
return 0;
}
static int
hpp_open(struct device *dev)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
int option_reg;
if (request_irq(dev->irq, &ei_interrupt, 0, "hp-plus", NULL)) {
return -EAGAIN;
}
option_reg = inw(ioaddr + HPP_OPTION);
outw(option_reg & ~(NICReset + ChipReset), ioaddr + HPP_OPTION);
SLOW_DOWN_IO; SLOW_DOWN_IO;
outw(option_reg | (EnableIRQ + NICReset + ChipReset), ioaddr + HPP_OPTION);
outw(HW_Page, ioaddr + HP_PAGING);
outw((HP_START_PG + TX_2X_PAGES) | ((HP_STOP_PG - 1) << 8), ioaddr + 14);
outw(Perf_Page, ioaddr + HP_PAGING);
ei_open(dev);
MOD_INC_USE_COUNT;
return 0;
}
static int
hpp_close(struct device *dev)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
int option_reg = inw(ioaddr + HPP_OPTION);
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = NULL;
ei_close(dev);
outw((option_reg & ~EnableIRQ) | MemDisable | NICReset | ChipReset,
ioaddr + HPP_OPTION);
MOD_DEC_USE_COUNT;
return 0;
}
static void
hpp_reset_8390(struct device *dev)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
int option_reg = inw(ioaddr + HPP_OPTION);
if (ei_debug > 1) printk("resetting the 8390 time=%ld...", jiffies);
outw(option_reg & ~(NICReset + ChipReset), ioaddr + HPP_OPTION);
SLOW_DOWN_IO;
SLOW_DOWN_IO;
ei_status.txing = 0;
outw(option_reg | (EnableIRQ + NICReset + ChipReset), ioaddr + HPP_OPTION);
SLOW_DOWN_IO; SLOW_DOWN_IO;
if ((inb_p(ioaddr+NIC_OFFSET+EN0_ISR) & ENISR_RESET) == 0)
printk("%s: hp_reset_8390() did not complete.\n", dev->name);
if (ei_debug > 1) printk("8390 reset done (%ld).", jiffies);
return;
}
static void
hpp_io_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr, int ring_page)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
outw((ring_page<<8), ioaddr + HPP_IN_ADDR);
insw(ioaddr + HP_DATAPORT, hdr, sizeof(struct e8390_pkt_hdr)>>1);
}
static void
hpp_io_block_input(struct device *dev, int count, struct sk_buff *skb, int ring_offset)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
char *buf = skb->data;
outw(ring_offset, ioaddr + HPP_IN_ADDR);
insw(ioaddr + HP_DATAPORT, buf, count>>1);
if (count & 0x01)
buf[count-1] = inw(ioaddr + HP_DATAPORT);
}
static void
hpp_mem_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr, int ring_page)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
int option_reg = inw(ioaddr + HPP_OPTION);
outw((ring_page<<8), ioaddr + HPP_IN_ADDR);
outw(option_reg & ~(MemDisable + BootROMEnb), ioaddr + HPP_OPTION);
memcpy_fromio(hdr, dev->mem_start, sizeof(struct e8390_pkt_hdr));
outw(option_reg, ioaddr + HPP_OPTION);
hdr->count = (hdr->count + 3) & ~3;
}
static void
hpp_mem_block_input(struct device *dev, int count, struct sk_buff *skb, int ring_offset)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
int option_reg = inw(ioaddr + HPP_OPTION);
outw(ring_offset, ioaddr + HPP_IN_ADDR);
outw(option_reg & ~(MemDisable + BootROMEnb), ioaddr + HPP_OPTION);
memcpy_fromio(skb->data, dev->mem_start, count);
outw(option_reg, ioaddr + HPP_OPTION);
}
static void
hpp_io_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
outw(start_page << 8, ioaddr + HPP_OUT_ADDR);
outsl(ioaddr + HP_DATAPORT, buf, (count+3)>>2);
return;
}
static void
hpp_mem_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page)
{
int ioaddr = dev->base_addr - NIC_OFFSET;
int option_reg = inw(ioaddr + HPP_OPTION);
outw(start_page << 8, ioaddr + HPP_OUT_ADDR);
outw(option_reg & ~(MemDisable + BootROMEnb), ioaddr + HPP_OPTION);
memcpy_toio(dev->mem_start, buf, (count + 3) & ~3);
outw(option_reg, ioaddr + HPP_OPTION);
return;
}
#ifdef MODULE
#define MAX_HPP_CARDS 4
#define NAMELEN 8
static char namelist[NAMELEN * MAX_HPP_CARDS] = { 0, };
static struct device dev_hpp[MAX_HPP_CARDS] = {
{
NULL,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, NULL
},
};
static int io[MAX_HPP_CARDS] = { 0, };
static int irq[MAX_HPP_CARDS] = { 0, };
int
init_module(void)
{
int this_dev, found = 0;
for (this_dev = 0; this_dev < MAX_HPP_CARDS; this_dev++) {
struct device *dev = &dev_hpp[this_dev];
dev->name = namelist+(NAMELEN*this_dev);
dev->irq = irq[this_dev];
dev->base_addr = io[this_dev];
dev->init = hp_plus_probe;
if (io[this_dev] == 0) {
if (this_dev != 0) break;
printk(KERN_NOTICE "hp-plus.c: Presently autoprobing (not recommended) for a single card.\n");
}
if (register_netdev(dev) != 0) {
printk(KERN_WARNING "hp-plus.c: No HP-Plus card found (i/o = 0x%x).\n", io[this_dev]);
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
for (this_dev = 0; this_dev < MAX_HPP_CARDS; this_dev++) {
struct device *dev = &dev_hpp[this_dev];
if (dev->priv != NULL) {
int ioaddr = dev->base_addr - NIC_OFFSET;
kfree(dev->priv);
dev->priv = NULL;
release_region(ioaddr, HP_IO_EXTENT);
unregister_netdev(dev);
}
}
}
#endif