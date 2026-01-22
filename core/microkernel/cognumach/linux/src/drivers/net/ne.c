static const char *version =
"ne.c:v1.10 9/23/94 Donald Becker (becker@cesdis.gsfc.nasa.gov)\n";
#include <linux/module.h>
#include <linux/config.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/pci.h>
#include <linux/bios32.h>
#include <asm/system.h>
#include <asm/io.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include "8390.h"
#define SUPPORT_NE_BAD_CLONES
#if defined(HAVE_DEVLIST) || !defined(MODULE)
static unsigned int netcard_portlist[] =
{ 0x300, 0x280, 0x320, 0x340, 0x360, 0};
#endif
#ifdef CONFIG_PCI
static struct { unsigned short vendor, dev_id;}
pci_clone_list[] = {
{PCI_VENDOR_ID_REALTEK, PCI_DEVICE_ID_REALTEK_8029},
{PCI_VENDOR_ID_WINBOND2, PCI_DEVICE_ID_WINBOND2_89C940},
{PCI_VENDOR_ID_COMPEX, PCI_DEVICE_ID_COMPEX_RL2000},
{PCI_VENDOR_ID_KTI, PCI_DEVICE_ID_KTI_ET32P2},
{PCI_VENDOR_ID_NETVIN, PCI_DEVICE_ID_NETVIN_NV5000SC},
{PCI_VENDOR_ID_VIA, PCI_DEVICE_ID_VIA_82C926},
{0,}
};
#endif
#ifdef SUPPORT_NE_BAD_CLONES
static struct { const char *name8, *name16; unsigned char SAprefix[4];}
bad_clone_list[] = {
{"DE100", "DE200", {0x00, 0xDE, 0x01,}},
{"DE120", "DE220", {0x00, 0x80, 0xc8,}},
{"DFI1000", "DFI2000", {'D', 'F', 'I',}},
{"EtherNext UTP8", "EtherNext UTP16", {0x00, 0x00, 0x79}},
{"NE1000","NE2000-invalid", {0x00, 0x00, 0xd8}},
{"NN1000", "NN2000", {0x08, 0x03, 0x08}},
{"4-DIM8","4-DIM16", {0x00,0x00,0x4d,}},
{"Con-Intl_8", "Con-Intl_16", {0x00, 0x00, 0x24}},
{"ET-100","ET-200", {0x00, 0x45, 0x54}},
{"COMPEX","COMPEX16",{0x00,0x80,0x48}},
{"E-LAN100", "E-LAN200", {0x00, 0x00, 0x5d}},
{"RealTek 8029", "RealTek 8029", {0x00, 0x3e, 0x4d}},
{0,}
};
#endif
#define NE_BASE (dev->base_addr)
#define NE_CMD 0x00
#define NE_DATAPORT 0x10
#define NE_RESET 0x1f
#define NE_IO_EXTENT 0x20
#define NE1SM_START_PG 0x20
#define NE1SM_STOP_PG 0x40
#define NESM_START_PG 0x40
#define NESM_STOP_PG 0x80
static unsigned char pci_irq_line = 0;
int ne_probe(struct device *dev);
#ifdef CONFIG_PCI
static int ne_probe_pci(struct device *dev);
#endif
static int ne_probe1(struct device *dev, int ioaddr);
static int ne_open(struct device *dev);
static int ne_close(struct device *dev);
static void ne_reset_8390(struct device *dev);
static void ne_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr,
int ring_page);
static void ne_block_input(struct device *dev, int count,
struct sk_buff *skb, int ring_offset);
static void ne_block_output(struct device *dev, const int count,
const unsigned char *buf, const int start_page);
#ifdef HAVE_DEVLIST
struct netdev_entry netcard_drv =
{"ne", ne_probe1, NE_IO_EXTENT, netcard_portlist};
#else
int ne_probe(struct device *dev)
{
#ifndef MODULE
int i;
#endif
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return ne_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
#ifdef CONFIG_PCI
if (pcibios_present() && (ne_probe_pci(dev) == 0))
return 0;
#endif
#ifndef MODULE
for (i = 0; netcard_portlist[i]; i++) {
int ioaddr = netcard_portlist[i];
if (check_region(ioaddr, NE_IO_EXTENT))
continue;
if (ne_probe1(dev, ioaddr) == 0)
return 0;
}
#endif
return ENODEV;
}
#endif
#ifdef CONFIG_PCI
static int ne_probe_pci(struct device *dev)
{
int i;
for (i = 0; pci_clone_list[i].vendor != 0; i++) {
unsigned char pci_bus, pci_device_fn;
unsigned int pci_ioaddr;
u16 pci_command, new_command;
int pci_index;
for (pci_index = 0; pci_index < 8; pci_index++) {
if (pcibios_find_device (pci_clone_list[i].vendor,
pci_clone_list[i].dev_id, pci_index,
&pci_bus, &pci_device_fn) != 0)
break;
pcibios_read_config_dword(pci_bus, pci_device_fn,
PCI_BASE_ADDRESS_0, &pci_ioaddr);
pci_ioaddr &= PCI_BASE_ADDRESS_IO_MASK;
if (check_region(pci_ioaddr, NE_IO_EXTENT))
continue;
pcibios_read_config_byte(pci_bus, pci_device_fn,
PCI_INTERRUPT_LINE, &pci_irq_line);
break;
}
if (pci_irq_line == 0) continue;
printk("ne.c: PCI BIOS reports NE 2000 clone at i/o %#x, irq %d.\n",
pci_ioaddr, pci_irq_line);
pcibios_read_config_word(pci_bus, pci_device_fn,
PCI_COMMAND, &pci_command);
new_command = pci_command | PCI_COMMAND_IO;
if (pci_command != new_command) {
printk(KERN_INFO "  The PCI BIOS has not enabled this"
" NE2k clone!  Updating PCI command %4.4x->%4.4x.\n",
pci_command, new_command);
pcibios_write_config_word(pci_bus, pci_device_fn,
PCI_COMMAND, new_command);
}
if (ne_probe1(dev, pci_ioaddr) != 0) {
printk(KERN_ERR "ne.c: Probe of PCI card at %#x failed.\n", pci_ioaddr);
pci_irq_line = 0;
return -ENXIO;
}
pci_irq_line = 0;
return 0;
}
return -ENODEV;
}
#endif
static int ne_probe1(struct device *dev, int ioaddr)
{
int i;
unsigned char SA_prom[32];
int wordlength = 2;
const char *name = NULL;
int start_page, stop_page;
int neX000, ctron, bad_card;
int reg0 = inb_p(ioaddr);
static unsigned version_printed = 0;
if (reg0 == 0xFF)
return ENODEV;
{ int regd;
outb_p(E8390_NODMA+E8390_PAGE1+E8390_STOP, ioaddr + E8390_CMD);
regd = inb_p(ioaddr + 0x0d);
outb_p(0xff, ioaddr + 0x0d);
outb_p(E8390_NODMA+E8390_PAGE0, ioaddr + E8390_CMD);
inb_p(ioaddr + EN0_COUNTER0);
if (inb_p(ioaddr + EN0_COUNTER0) != 0) {
outb_p(reg0, ioaddr);
outb_p(regd, ioaddr + 0x0d);
return ENODEV;
}
}
if (dev == NULL) {
printk(KERN_ERR "ne.c: Passed a NULL device.\n");
dev = init_etherdev(0, 0);
}
if (ei_debug && version_printed++ == 0)
printk("%s", version);
printk("NE*000 ethercard probe at %#3x:", ioaddr);
bad_card = ((dev->base_addr != 0) && (dev->mem_end == 0xbad));
{ unsigned long reset_start_time = jiffies;
outb(inb(ioaddr + NE_RESET), ioaddr + NE_RESET);
while ((inb_p(ioaddr + EN0_ISR) & ENISR_RESET) == 0)
if (jiffies - reset_start_time > 2*HZ/100) {
if (bad_card) {
printk(" (warning: no reset ack)");
break;
} else {
printk(" not found (no reset ack).\n");
return ENODEV;
}
}
outb_p(0xff, ioaddr + EN0_ISR);
}
{
struct {unsigned char value, offset; } program_seq[] = {
{E8390_NODMA+E8390_PAGE0+E8390_STOP, E8390_CMD},
{0x48, EN0_DCFG},
{0x00, EN0_RCNTLO},
{0x00, EN0_RCNTHI},
{0x00, EN0_IMR},
{0xFF, EN0_ISR},
{E8390_RXOFF, EN0_RXCR},
{E8390_TXOFF, EN0_TXCR},
{32, EN0_RCNTLO},
{0x00, EN0_RCNTHI},
{0x00, EN0_RSARLO},
{0x00, EN0_RSARHI},
{E8390_RREAD+E8390_START, E8390_CMD},
};
for (i = 0; i < sizeof(program_seq)/sizeof(program_seq[0]); i++)
outb_p(program_seq[i].value, ioaddr + program_seq[i].offset);
}
for(i = 0; i < 32 ; i+=2) {
SA_prom[i] = inb(ioaddr + NE_DATAPORT);
SA_prom[i+1] = inb(ioaddr + NE_DATAPORT);
if (SA_prom[i] != SA_prom[i+1])
wordlength = 1;
}
if (wordlength == 2)
for (i = 0; i < 16; i++)
SA_prom[i] = SA_prom[i+i];
if (pci_irq_line || ioaddr >= 0x400)
wordlength = 2;
if (wordlength == 2) {
outb_p(0x49, ioaddr + EN0_DCFG);
start_page = NESM_START_PG;
stop_page = NESM_STOP_PG;
} else {
start_page = NE1SM_START_PG;
stop_page = NE1SM_STOP_PG;
}
neX000 = (SA_prom[14] == 0x57 && SA_prom[15] == 0x57);
ctron = (SA_prom[0] == 0x00 && SA_prom[1] == 0x00 && SA_prom[2] == 0x1d);
if (neX000 || bad_card) {
name = (wordlength == 2) ? "NE2000" : "NE1000";
} else if (ctron) {
name = (wordlength == 2) ? "Ctron-8" : "Ctron-16";
start_page = 0x01;
stop_page = (wordlength == 2) ? 0x40 : 0x20;
} else {
#ifdef SUPPORT_NE_BAD_CLONES
for (i = 0; bad_clone_list[i].name8; i++) {
if (SA_prom[0] == bad_clone_list[i].SAprefix[0] &&
SA_prom[1] == bad_clone_list[i].SAprefix[1] &&
SA_prom[2] == bad_clone_list[i].SAprefix[2]) {
if (wordlength == 2) {
name = bad_clone_list[i].name16;
} else {
name = bad_clone_list[i].name8;
}
break;
}
}
if (bad_clone_list[i].name8 == NULL) {
printk(" not found (invalid signature %2.2x %2.2x).\n",
SA_prom[14], SA_prom[15]);
return ENXIO;
}
#else
printk(" not found.\n");
return ENXIO;
#endif
}
if (pci_irq_line)
dev->irq = pci_irq_line;
if (dev->irq < 2) {
autoirq_setup(0);
outb_p(0x50, ioaddr + EN0_IMR);
outb_p(0x00, ioaddr + EN0_RCNTLO);
outb_p(0x00, ioaddr + EN0_RCNTHI);
outb_p(E8390_RREAD+E8390_START, ioaddr);
outb_p(0x00, ioaddr + EN0_IMR);
dev->irq = autoirq_report(0);
if (ei_debug > 2)
printk(" autoirq is %d\n", dev->irq);
} else if (dev->irq == 2)
dev->irq = 9;
if (! dev->irq) {
printk(" failed to detect IRQ line.\n");
return EAGAIN;
}
{
int irqval = request_irq(dev->irq, ei_interrupt,
pci_irq_line ? SA_SHIRQ : 0, name, dev);
if (irqval) {
printk (" unable to get IRQ %d (irqval=%d).\n", dev->irq, irqval);
return EAGAIN;
}
}
dev->base_addr = ioaddr;
if (ethdev_init(dev)) {
printk (" unable to get memory for dev->priv.\n");
free_irq(dev->irq, NULL);
return -ENOMEM;
}
request_region(ioaddr, NE_IO_EXTENT, name);
for(i = 0; i < ETHER_ADDR_LEN; i++) {
printk(" %2.2x", SA_prom[i]);
dev->dev_addr[i] = SA_prom[i];
}
printk("\n%s: %s found at %#x, using IRQ %d.\n",
dev->name, name, ioaddr, dev->irq);
ei_status.name = name;
ei_status.tx_start_page = start_page;
ei_status.stop_page = stop_page;
ei_status.word16 = (wordlength == 2);
ei_status.rx_start_page = start_page + TX_PAGES;
#ifdef PACKETBUF_MEMSIZE
ei_status.stop_page = ei_status.tx_start_page + PACKETBUF_MEMSIZE;
#endif
ei_status.reset_8390 = &ne_reset_8390;
ei_status.block_input = &ne_block_input;
ei_status.block_output = &ne_block_output;
ei_status.get_8390_hdr = &ne_get_8390_hdr;
dev->open = &ne_open;
dev->stop = &ne_close;
NS8390_init(dev, 0);
return 0;
}
static int
ne_open(struct device *dev)
{
ei_open(dev);
MOD_INC_USE_COUNT;
return 0;
}
static int
ne_close(struct device *dev)
{
if (ei_debug > 1)
printk("%s: Shutting down ethercard.\n", dev->name);
ei_close(dev);
MOD_DEC_USE_COUNT;
return 0;
}
static void
ne_reset_8390(struct device *dev)
{
unsigned long reset_start_time = jiffies;
if (ei_debug > 1) printk("resetting the 8390 t=%ld...", jiffies);
outb(inb(NE_BASE + NE_RESET), NE_BASE + NE_RESET);
ei_status.txing = 0;
ei_status.dmaing = 0;
while ((inb_p(NE_BASE+EN0_ISR) & ENISR_RESET) == 0)
if (jiffies - reset_start_time > 2*HZ/100) {
printk("%s: ne_reset_8390() did not complete.\n", dev->name);
break;
}
outb_p(ENISR_RESET, NE_BASE + EN0_ISR);
}
static void
ne_get_8390_hdr(struct device *dev, struct e8390_pkt_hdr *hdr, int ring_page)
{
int nic_base = dev->base_addr;
if (ei_status.dmaing) {
printk("%s: DMAing conflict in ne_get_8390_hdr "
"[DMAstat:%d][irqlock:%d][intr:%d].\n",
dev->name, ei_status.dmaing, ei_status.irqlock,
dev->interrupt);
return;
}
ei_status.dmaing |= 0x01;
outb_p(E8390_NODMA+E8390_PAGE0+E8390_START, nic_base+ NE_CMD);
outb_p(sizeof(struct e8390_pkt_hdr), nic_base + EN0_RCNTLO);
outb_p(0, nic_base + EN0_RCNTHI);
outb_p(0, nic_base + EN0_RSARLO);
outb_p(ring_page, nic_base + EN0_RSARHI);
outb_p(E8390_RREAD+E8390_START, nic_base + NE_CMD);
if (ei_status.word16)
insw(NE_BASE + NE_DATAPORT, hdr, sizeof(struct e8390_pkt_hdr)>>1);
else
insb(NE_BASE + NE_DATAPORT, hdr, sizeof(struct e8390_pkt_hdr));
outb_p(ENISR_RDC, nic_base + EN0_ISR);
ei_status.dmaing &= ~0x01;
}
static void
ne_block_input(struct device *dev, int count, struct sk_buff *skb, int ring_offset)
{
#ifdef NE_SANITY_CHECK
int xfer_count = count;
#endif
int nic_base = dev->base_addr;
char *buf = skb->data;
if (ei_status.dmaing) {
printk("%s: DMAing conflict in ne_block_input "
"[DMAstat:%d][irqlock:%d][intr:%d].\n",
dev->name, ei_status.dmaing, ei_status.irqlock,
dev->interrupt);
return;
}
ei_status.dmaing |= 0x01;
outb_p(E8390_NODMA+E8390_PAGE0+E8390_START, nic_base+ NE_CMD);
outb_p(count & 0xff, nic_base + EN0_RCNTLO);
outb_p(count >> 8, nic_base + EN0_RCNTHI);
outb_p(ring_offset & 0xff, nic_base + EN0_RSARLO);
outb_p(ring_offset >> 8, nic_base + EN0_RSARHI);
outb_p(E8390_RREAD+E8390_START, nic_base + NE_CMD);
if (ei_status.word16) {
insw(NE_BASE + NE_DATAPORT,buf,count>>1);
if (count & 0x01) {
buf[count-1] = inb(NE_BASE + NE_DATAPORT);
#ifdef NE_SANITY_CHECK
xfer_count++;
#endif
}
} else {
insb(NE_BASE + NE_DATAPORT, buf, count);
}
#ifdef NE_SANITY_CHECK
if (ei_debug > 1) {
int addr, tries = 20;
do {
int high = inb_p(nic_base + EN0_RSARHI);
int low = inb_p(nic_base + EN0_RSARLO);
addr = (high << 8) + low;
if (((ring_offset + xfer_count) & 0xff) == low)
break;
} while (--tries > 0);
if (tries <= 0)
printk("%s: RX transfer address mismatch,"
"%#4.4x (expected) vs. %#4.4x (actual).\n",
dev->name, ring_offset + xfer_count, addr);
}
#endif
outb_p(ENISR_RDC, nic_base + EN0_ISR);
ei_status.dmaing &= ~0x01;
}
static void
ne_block_output(struct device *dev, int count,
const unsigned char *buf, const int start_page)
{
int nic_base = NE_BASE;
unsigned long dma_start;
#ifdef NE_SANITY_CHECK
int retries = 0;
#endif
if (ei_status.word16 && (count & 0x01))
count++;
if (ei_status.dmaing) {
printk("%s: DMAing conflict in ne_block_output."
"[DMAstat:%d][irqlock:%d][intr:%d]\n",
dev->name, ei_status.dmaing, ei_status.irqlock,
dev->interrupt);
return;
}
ei_status.dmaing |= 0x01;
outb_p(E8390_PAGE0+E8390_START+E8390_NODMA, nic_base + NE_CMD);
#ifdef NE_SANITY_CHECK
retry:
#endif
#ifdef NE8390_RW_BUGFIX
outb_p(0x42, nic_base + EN0_RCNTLO);
outb_p(0x00, nic_base + EN0_RCNTHI);
outb_p(0x42, nic_base + EN0_RSARLO);
outb_p(0x00, nic_base + EN0_RSARHI);
outb_p(E8390_RREAD+E8390_START, nic_base + NE_CMD);
SLOW_DOWN_IO;
SLOW_DOWN_IO;
SLOW_DOWN_IO;
#endif
outb_p(ENISR_RDC, nic_base + EN0_ISR);
outb_p(count & 0xff, nic_base + EN0_RCNTLO);
outb_p(count >> 8, nic_base + EN0_RCNTHI);
outb_p(0x00, nic_base + EN0_RSARLO);
outb_p(start_page, nic_base + EN0_RSARHI);
outb_p(E8390_RWRITE+E8390_START, nic_base + NE_CMD);
if (ei_status.word16) {
outsw(NE_BASE + NE_DATAPORT, buf, count>>1);
} else {
outsb(NE_BASE + NE_DATAPORT, buf, count);
}
dma_start = jiffies;
#ifdef NE_SANITY_CHECK
if (ei_debug > 1) {
int addr, tries = 20;
do {
int high = inb_p(nic_base + EN0_RSARHI);
int low = inb_p(nic_base + EN0_RSARLO);
addr = (high << 8) + low;
if ((start_page << 8) + count == addr)
break;
} while (--tries > 0);
if (tries <= 0) {
printk("%s: Tx packet transfer address mismatch,"
"%#4.4x (expected) vs. %#4.4x (actual).\n",
dev->name, (start_page << 8) + count, addr);
if (retries++ == 0)
goto retry;
}
}
#endif
while ((inb_p(nic_base + EN0_ISR) & ENISR_RDC) == 0)
if (jiffies - dma_start > 2*HZ/100) {
printk("%s: timeout waiting for Tx RDC.\n", dev->name);
ne_reset_8390(dev);
NS8390_init(dev,1);
break;
}
outb_p(ENISR_RDC, nic_base + EN0_ISR);
ei_status.dmaing &= ~0x01;
return;
}
#ifdef MODULE
#define MAX_NE_CARDS 4
#define NAMELEN 8
static char namelist[NAMELEN * MAX_NE_CARDS] = { 0, };
static struct device dev_ne[MAX_NE_CARDS] = {
{
NULL,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, NULL
},
};
static int io[MAX_NE_CARDS] = { 0, };
static int irq[MAX_NE_CARDS] = { 0, };
static int bad[MAX_NE_CARDS] = { 0, };
int
init_module(void)
{
int this_dev, found = 0;
for (this_dev = 0; this_dev < MAX_NE_CARDS; this_dev++) {
struct device *dev = &dev_ne[this_dev];
dev->name = namelist+(NAMELEN*this_dev);
dev->irq = irq[this_dev];
dev->base_addr = io[this_dev];
dev->init = ne_probe;
dev->mem_end = bad[this_dev];
if (register_netdev(dev) == 0) {
found++;
continue;
}
if (found != 0)
return 0;
if (io[this_dev] != 0)
printk(KERN_WARNING "ne.c: No NE*000 card found at i/o = %#x\n", io[this_dev]);
else
printk(KERN_NOTICE "ne.c: No PCI cards found. Use \"io=0xNNN\" value(s) for ISA cards.\n");
return -ENXIO;
}
return 0;
}
void
cleanup_module(void)
{
int this_dev;
for (this_dev = 0; this_dev < MAX_NE_CARDS; this_dev++) {
struct device *dev = &dev_ne[this_dev];
if (dev->priv != NULL) {
kfree(dev->priv);
dev->priv = NULL;
free_irq(dev->irq, dev);
irq2dev_map[dev->irq] = NULL;
release_region(dev->base_addr, NE_IO_EXTENT);
unregister_netdev(dev);
}
}
}
#endif