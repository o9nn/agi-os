static const char *version = "depca.c:v0.43 96/8/16 davies@maniac.ultranet.com\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/ptrace.h>
#include <linux/errno.h>
#include <linux/ioport.h>
#include <linux/malloc.h>
#include <linux/interrupt.h>
#include <linux/delay.h>
#include <asm/segment.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/time.h>
#include <linux/types.h>
#include <linux/unistd.h>
#include <linux/ctype.h>
#include "depca.h"
#ifdef DEPCA_DEBUG
static int depca_debug = DEPCA_DEBUG;
#else
static int depca_debug = 1;
#endif
#define DEPCA_NDA 0xffe0
#define PROBE_LENGTH 32
#define ETH_PROM_SIG 0xAA5500FFUL
#define NUM_RX_DESC 8
#define NUM_TX_DESC 8
#define RX_BUFF_SZ 1536
#define TX_BUFF_SZ 1536
#define CRC_POLYNOMIAL_BE 0x04c11db7UL
#define CRC_POLYNOMIAL_LE 0xedb88320UL
#define DEPCA_EISA_IO_PORTS 0x0c00
#define MAX_EISA_SLOTS 16
#define EISA_SLOT_INC 0x1000
#define DEPCA_RAM_BASE_ADDRESSES {0xc0000,0xd0000,0xe0000,0x00000}
#define DEPCA_IO_PORTS {0x300, 0x200, 0}
#define DEPCA_TOTAL_SIZE 0x10
static short mem_chkd = 0;
#define DEPCA_SIGNATURE {"DEPCA",\
"DE100","DE101",\
"DE200","DE201","DE202",\
"DE210",\
"DE422",\
""}
static enum {DEPCA, de100, de101, de200, de201, de202, de210, de422, unknown} adapter;
#define DEPCA_STRLEN 16
#define MAX_NUM_DEPCAS 2
#define ALIGN4 ((u_long)4 - 1)
#define ALIGN8 ((u_long)8 - 1)
#define ALIGN ALIGN8
struct depca_rx_desc {
volatile s32 base;
s16 buf_length;
s16 msg_length;
};
struct depca_tx_desc {
volatile s32 base;
s16 length;
s16 misc;
};
#define LA_MASK 0x0000ffff
struct depca_init {
u16 mode;
u8 phys_addr[ETH_ALEN];
u8 mcast_table[8];
u32 rx_ring;
u32 tx_ring;
};
#define DEPCA_PKT_STAT_SZ 16
#define DEPCA_PKT_BIN_SZ 128
struct depca_private {
char devname[DEPCA_STRLEN];
char adapter_name[DEPCA_STRLEN];
char adapter;
struct depca_rx_desc *rx_ring;
struct depca_tx_desc *tx_ring;
struct depca_init init_block;
char *rx_memcpy[NUM_RX_DESC];
char *tx_memcpy[NUM_TX_DESC];
u_long bus_offset;
u_long sh_mem;
u_long dma_buffs;
int rx_new, tx_new;
int rx_old, tx_old;
struct enet_statistics stats;
struct {
u32 bins[DEPCA_PKT_STAT_SZ];
u32 unicast;
u32 multicast;
u32 broadcast;
u32 excessive_collisions;
u32 tx_underruns;
u32 excessive_underruns;
} pktStats;
int txRingMask;
int rxRingMask;
s32 rx_rlen;
s32 tx_rlen;
};
#define TX_BUFFS_AVAIL ((lp->tx_old<=lp->tx_new)?\
lp->tx_old+lp->txRingMask-lp->tx_new:\
lp->tx_old -lp->tx_new-1)
static int depca_open(struct device *dev);
static int depca_start_xmit(struct sk_buff *skb, struct device *dev);
static void depca_interrupt(int irq, void *dev_id, struct pt_regs * regs);
static int depca_close(struct device *dev);
static int depca_ioctl(struct device *dev, struct ifreq *rq, int cmd);
static struct enet_statistics *depca_get_stats(struct device *dev);
static void set_multicast_list(struct device *dev);
static int depca_hw_init(struct device *dev, u_long ioaddr);
static void depca_init_ring(struct device *dev);
static int depca_rx(struct device *dev);
static int depca_tx(struct device *dev);
static void LoadCSRs(struct device *dev);
static int InitRestartDepca(struct device *dev);
static void DepcaSignature(char *name, u_long paddr);
static int DevicePresent(u_long ioaddr);
static int get_hw_addr(struct device *dev);
static int EISA_signature(char *name, s32 eisa_id);
static void SetMulticastFilter(struct device *dev);
static void isa_probe(struct device *dev, u_long iobase);
static void eisa_probe(struct device *dev, u_long iobase);
static struct device *alloc_device(struct device *dev, u_long iobase);
static int depca_dev_index(char *s);
static struct device *insert_device(struct device *dev, u_long iobase, int (*init)(struct device *));
static int load_packet(struct device *dev, struct sk_buff *skb);
static void depca_dbg_open(struct device *dev);
#ifdef MODULE
int init_module(void);
void cleanup_module(void);
static int autoprobed = 1, loading_module = 1;
# else
static u_char de1xx_irq[] = {2,3,4,5,7,9,0};
static u_char de2xx_irq[] = {5,9,10,11,15,0};
static u_char de422_irq[] = {5,9,10,11,0};
static u_char *depca_irq;
static int autoprobed = 0, loading_module = 0;
#endif
static char name[DEPCA_STRLEN];
static int num_depcas = 0, num_eth = 0;
static int mem=0;
static char *adapter_name = '\0';
#define STOP_DEPCA \
outw(CSR0, DEPCA_ADDR);\
outw(STOP, DEPCA_DATA)
int depca_probe(struct device *dev)
{
int tmp = num_depcas, status = -ENODEV;
u_long iobase = dev->base_addr;
if ((iobase == 0) && loading_module){
printk("Autoprobing is not supported when loading a module based driver.\n");
status = -EIO;
} else {
isa_probe(dev, iobase);
eisa_probe(dev, iobase);
if ((tmp == num_depcas) && (iobase != 0) && loading_module) {
printk("%s: depca_probe() cannot find device at 0x%04lx.\n", dev->name,
iobase);
}
for (; (dev->priv == NULL) && (dev->next != NULL); dev = dev->next);
if (dev->priv) status = 0;
if (iobase == 0) autoprobed = 1;
}
return status;
}
static int
depca_hw_init(struct device *dev, u_long ioaddr)
{
struct depca_private *lp;
int i, j, offset, netRAM, mem_len, status=0;
s16 nicsr;
u_long mem_start=0, mem_base[] = DEPCA_RAM_BASE_ADDRESSES;
STOP_DEPCA;
nicsr = inb(DEPCA_NICSR);
nicsr = ((nicsr & ~SHE & ~RBE & ~IEN) | IM);
outb(nicsr, DEPCA_NICSR);
if (inw(DEPCA_DATA) == STOP) {
do {
strcpy(name, (adapter_name ? adapter_name : ""));
mem_start = (mem ? mem & 0xf0000 : mem_base[mem_chkd++]);
DepcaSignature(name, mem_start);
} while (!mem && mem_base[mem_chkd] && (adapter == unknown));
if ((adapter != unknown) && mem_start) {
dev->base_addr = ioaddr;
if ((ioaddr&0x0fff)==DEPCA_EISA_IO_PORTS) {
printk("%s: %s at 0x%04lx (EISA slot %d)",
dev->name, name, ioaddr, (int)((ioaddr>>12)&0x0f));
} else {
printk("%s: %s at 0x%04lx", dev->name, name, ioaddr);
}
printk(", h/w address ");
status = get_hw_addr(dev);
for (i=0; i<ETH_ALEN - 1; i++) {
printk("%2.2x:", dev->dev_addr[i]);
}
printk("%2.2x", dev->dev_addr[i]);
if (status == 0) {
netRAM = ((adapter != DEPCA) ? 64 : 48);
if ((nicsr & _128KB) && (adapter == de422)) netRAM = 128;
offset = 0x0000;
if (nicsr & BUF) {
offset = 0x8000;
nicsr &= ~BS;
netRAM -= 32;
}
mem_start += offset;
if ((mem_len = (NUM_RX_DESC*(sizeof(struct depca_rx_desc)+RX_BUFF_SZ) +
NUM_TX_DESC*(sizeof(struct depca_tx_desc)+TX_BUFF_SZ) +
sizeof(struct depca_init))) <=
(netRAM<<10)) {
printk(",\n      has %dkB RAM at 0x%.5lx", netRAM, mem_start);
if (adapter != DEPCA) {
nicsr |= SHE;
outb(nicsr, DEPCA_NICSR);
}
dev->priv = (void *) kmalloc(sizeof(struct depca_private), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
lp = (struct depca_private *)dev->priv;
memset((char *)dev->priv, 0, sizeof(struct depca_private));
lp->adapter = adapter;
sprintf(lp->adapter_name,"%s (%s)", name, dev->name);
request_region(ioaddr, DEPCA_TOTAL_SIZE, lp->adapter_name);
lp->sh_mem = mem_start;
mem_start += sizeof(struct depca_init);
mem_start = (mem_start + ALIGN) & ~ALIGN;
lp->rx_ring = (struct depca_rx_desc *)mem_start;
mem_start += (sizeof(struct depca_rx_desc) * NUM_RX_DESC);
lp->tx_ring = (struct depca_tx_desc *)mem_start;
mem_start += (sizeof(struct depca_tx_desc) * NUM_TX_DESC);
lp->bus_offset = mem_start & 0x00ff0000;
mem_start &= LA_MASK;
lp->dma_buffs = mem_start;
lp->rxRingMask = NUM_RX_DESC - 1;
lp->txRingMask = NUM_TX_DESC - 1;
for (i=0, j = lp->rxRingMask; j>0; i++) {
j >>= 1;
}
lp->rx_rlen = (s32)(i << 29);
for (i=0, j = lp->txRingMask; j>0; i++) {
j >>= 1;
}
lp->tx_rlen = (s32)(i << 29);
depca_init_ring(dev);
LoadCSRs(dev);
nicsr = ((nicsr & ~IM)|IEN);
outb(nicsr, DEPCA_NICSR);
if (dev->irq < 2) {
#ifndef MODULE
unsigned char irqnum;
autoirq_setup(0);
switch (lp->adapter) {
case DEPCA:
case de100:
case de101:
depca_irq = de1xx_irq;
break;
case de200:
case de201:
case de202:
case de210:
depca_irq = de2xx_irq;
break;
case de422:
depca_irq = de422_irq;
break;
}
outw(INEA | INIT, DEPCA_DATA);
irqnum = autoirq_report(1);
if (!irqnum) {
printk(" and failed to detect IRQ line.\n");
status = -ENXIO;
} else {
for (dev->irq=0,i=0; (depca_irq[i]) && (!dev->irq); i++) {
if (irqnum == depca_irq[i]) {
dev->irq = irqnum;
printk(" and uses IRQ%d.\n", dev->irq);
}
}
if (!dev->irq) {
printk(" but incorrect IRQ line detected.\n");
status = -ENXIO;
}
}
#endif
} else {
printk(" and assigned IRQ%d.\n", dev->irq);
}
if (status) release_region(ioaddr, DEPCA_TOTAL_SIZE);
} else {
printk(",\n      requests %dkB RAM: only %dkB is available!\n",
(mem_len>>10), netRAM);
status = -ENXIO;
}
} else {
printk("      which has an Ethernet PROM CRC error.\n");
status = -ENXIO;
}
} else {
status = -ENXIO;
}
if (!status) {
if (depca_debug > 1) {
printk("%s", version);
}
dev->open = &depca_open;
dev->hard_start_xmit = &depca_start_xmit;
dev->stop = &depca_close;
dev->get_stats = &depca_get_stats;
dev->set_multicast_list = &set_multicast_list;
dev->do_ioctl = &depca_ioctl;
dev->mem_start = 0;
ether_setup(dev);
} else {
if (dev->priv) {
kfree_s(dev->priv, sizeof(struct depca_private));
dev->priv = NULL;
}
}
} else {
status = -ENXIO;
}
return status;
}
static int
depca_open(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
u_long ioaddr = dev->base_addr;
s16 nicsr;
int status = 0;
irq2dev_map[dev->irq] = dev;
STOP_DEPCA;
nicsr = inb(DEPCA_NICSR);
if (adapter != DEPCA) {
nicsr |= SHE;
outb(nicsr, DEPCA_NICSR);
}
depca_init_ring(dev);
LoadCSRs(dev);
depca_dbg_open(dev);
if (request_irq(dev->irq, &depca_interrupt, 0, lp->adapter_name, NULL)) {
printk("depca_open(): Requested IRQ%d is busy\n",dev->irq);
status = -EAGAIN;
} else {
nicsr = ((nicsr & ~IM & ~LED)|IEN);
outb(nicsr, DEPCA_NICSR);
outw(CSR0,DEPCA_ADDR);
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
status = InitRestartDepca(dev);
if (depca_debug > 1){
printk("CSR0: 0x%4.4x\n",inw(DEPCA_DATA));
printk("nicsr: 0x%02x\n",inb(DEPCA_NICSR));
}
}
MOD_INC_USE_COUNT;
return status;
}
static void
depca_init_ring(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
u_int i;
u_long p;
set_bit(0, (void *)&dev->tbusy);
lp->rx_new = lp->tx_new = 0;
lp->rx_old = lp->tx_old = 0;
for (i = 0; i <= lp->rxRingMask; i++) {
writel((p=lp->dma_buffs+i*RX_BUFF_SZ) | R_OWN, &lp->rx_ring[i].base);
writew(-RX_BUFF_SZ, &lp->rx_ring[i].buf_length);
lp->rx_memcpy[i]=(char *)(p+lp->bus_offset);
}
for (i = 0; i <= lp->txRingMask; i++) {
writel((p=lp->dma_buffs+(i+lp->txRingMask+1)*TX_BUFF_SZ) & 0x00ffffff,
&lp->tx_ring[i].base);
lp->tx_memcpy[i]=(char *)(p+lp->bus_offset);
}
lp->init_block.rx_ring = ((u32)((u_long)lp->rx_ring)&LA_MASK) | lp->rx_rlen;
lp->init_block.tx_ring = ((u32)((u_long)lp->tx_ring)&LA_MASK) | lp->tx_rlen;
SetMulticastFilter(dev);
for (i = 0; i < ETH_ALEN; i++) {
lp->init_block.phys_addr[i] = dev->dev_addr[i];
}
lp->init_block.mode = 0x0000;
return;
}
static int
depca_start_xmit(struct sk_buff *skb, struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
u_long ioaddr = dev->base_addr;
int status = 0;
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 1*HZ) {
status = -1;
} else {
printk("%s: transmit timed out, status %04x, resetting.\n",
dev->name, inw(DEPCA_DATA));
STOP_DEPCA;
depca_init_ring(dev);
LoadCSRs(dev);
dev->interrupt = UNMASK_INTERRUPTS;
dev->start = 1;
dev->tbusy=0;
dev->trans_start = jiffies;
InitRestartDepca(dev);
dev_kfree_skb(skb, FREE_WRITE);
}
return status;
} else if (skb == NULL) {
dev_tint(dev);
} else if (skb->len > 0) {
if (set_bit(0, (void*)&dev->tbusy) != 0) {
printk("%s: Transmitter access conflict.\n", dev->name);
status = -1;
} else {
if (TX_BUFFS_AVAIL) {
status = load_packet(dev, skb);
if (!status) {
outw(CSR0, DEPCA_ADDR);
outw(INEA | TDMD, DEPCA_DATA);
dev->trans_start = jiffies;
dev_kfree_skb(skb, FREE_WRITE);
}
if (TX_BUFFS_AVAIL) {
dev->tbusy=0;
}
} else {
status = -1;
}
}
}
return status;
}
static void
depca_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
struct depca_private *lp;
s16 csr0, nicsr;
u_long ioaddr;
if (dev == NULL) {
printk ("depca_interrupt(): irq %d for unknown device.\n", irq);
} else {
lp = (struct depca_private *)dev->priv;
ioaddr = dev->base_addr;
if (dev->interrupt)
printk("%s: Re-entering the interrupt handler.\n", dev->name);
dev->interrupt = MASK_INTERRUPTS;
nicsr = inb(DEPCA_NICSR);
nicsr |= (IM|LED);
outb(nicsr, DEPCA_NICSR);
outw(CSR0, DEPCA_ADDR);
csr0 = inw(DEPCA_DATA);
outw(csr0 & INTE, DEPCA_DATA);
if (csr0 & RINT)
depca_rx(dev);
if (csr0 & TINT)
depca_tx(dev);
if ((TX_BUFFS_AVAIL >= 0) && dev->tbusy) {
dev->tbusy = 0;
mark_bh(NET_BH);
}
nicsr = (nicsr & ~IM & ~LED);
outb(nicsr, DEPCA_NICSR);
dev->interrupt = UNMASK_INTERRUPTS;
}
return;
}
static int
depca_rx(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
int i, entry;
s32 status;
for (entry=lp->rx_new;
!(readl(&lp->rx_ring[entry].base) & R_OWN);
entry=lp->rx_new){
status = readl(&lp->rx_ring[entry].base) >> 16 ;
if (status & R_STP) {
lp->rx_old = entry;
}
if (status & R_ENP) {
if (status & R_ERR) {
lp->stats.rx_errors++;
if (status & R_FRAM) lp->stats.rx_frame_errors++;
if (status & R_OFLO) lp->stats.rx_over_errors++;
if (status & R_CRC) lp->stats.rx_crc_errors++;
if (status & R_BUFF) lp->stats.rx_fifo_errors++;
} else {
short len, pkt_len = readw(&lp->rx_ring[entry].msg_length);
struct sk_buff *skb;
skb = dev_alloc_skb(pkt_len+2);
if (skb != NULL) {
unsigned char *buf;
skb_reserve(skb,2);
buf = skb_put(skb,pkt_len);
skb->dev = dev;
if (entry < lp->rx_old) {
len = (lp->rxRingMask - lp->rx_old + 1) * RX_BUFF_SZ;
memcpy_fromio(buf, lp->rx_memcpy[lp->rx_old], len);
memcpy_fromio(buf + len, lp->rx_memcpy[0], pkt_len-len);
} else {
memcpy_fromio(buf, lp->rx_memcpy[lp->rx_old], pkt_len);
}
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
for (i=1; i<DEPCA_PKT_STAT_SZ-1; i++) {
if (pkt_len < (i*DEPCA_PKT_BIN_SZ)) {
lp->pktStats.bins[i]++;
i = DEPCA_PKT_STAT_SZ;
}
}
if (buf[0] & 0x01) {
if ((*(s16 *)&buf[0] == -1) &&
(*(s16 *)&buf[2] == -1) &&
(*(s16 *)&buf[4] == -1)) {
lp->pktStats.broadcast++;
} else {
lp->pktStats.multicast++;
}
} else if ((*(s16 *)&buf[0] == *(s16 *)&dev->dev_addr[0]) &&
(*(s16 *)&buf[2] == *(s16 *)&dev->dev_addr[2]) &&
(*(s16 *)&buf[4] == *(s16 *)&dev->dev_addr[4])) {
lp->pktStats.unicast++;
}
lp->pktStats.bins[0]++;
if (lp->pktStats.bins[0] == 0) {
memset((char *)&lp->pktStats, 0, sizeof(lp->pktStats));
}
} else {
printk("%s: Memory squeeze, deferring packet.\n", dev->name);
lp->stats.rx_dropped++;
break;
}
}
for (; lp->rx_old!=entry; lp->rx_old=(lp->rx_old+1)&lp->rxRingMask) {
writel(readl(&lp->rx_ring[lp->rx_old].base) | R_OWN,
&lp->rx_ring[lp->rx_old].base);
}
writel(readl(&lp->rx_ring[entry].base) | R_OWN, &lp->rx_ring[entry].base);
}
lp->rx_new = (lp->rx_new + 1) & lp->rxRingMask;
}
return 0;
}
static int
depca_tx(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
int entry;
s32 status;
u_long ioaddr = dev->base_addr;
for (entry = lp->tx_old; entry != lp->tx_new; entry = lp->tx_old) {
status = readl(&lp->tx_ring[entry].base) >> 16 ;
if (status < 0) {
break;
} else if (status & T_ERR) {
status = readl(&lp->tx_ring[entry].misc);
lp->stats.tx_errors++;
if (status & TMD3_RTRY) lp->stats.tx_aborted_errors++;
if (status & TMD3_LCAR) lp->stats.tx_carrier_errors++;
if (status & TMD3_LCOL) lp->stats.tx_window_errors++;
if (status & TMD3_UFLO) lp->stats.tx_fifo_errors++;
if (status & (TMD3_BUFF | TMD3_UFLO)) {
outw(CSR0, DEPCA_ADDR);
outw(INEA | TDMD, DEPCA_DATA);
}
} else if (status & (T_MORE | T_ONE)) {
lp->stats.collisions++;
} else {
lp->stats.tx_packets++;
}
lp->tx_old = (lp->tx_old + 1) & lp->txRingMask;
}
return 0;
}
static int
depca_close(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
s16 nicsr;
u_long ioaddr = dev->base_addr;
dev->start = 0;
dev->tbusy = 1;
outw(CSR0, DEPCA_ADDR);
if (depca_debug > 1) {
printk("%s: Shutting down ethercard, status was %2.2x.\n",
dev->name, inw(DEPCA_DATA));
}
outw(STOP, DEPCA_DATA);
if (lp->adapter != DEPCA) {
nicsr = inb(DEPCA_NICSR);
nicsr &= ~SHE;
outb(nicsr, DEPCA_NICSR);
}
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = NULL;
MOD_DEC_USE_COUNT;
return 0;
}
static void LoadCSRs(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
u_long ioaddr = dev->base_addr;
outw(CSR1, DEPCA_ADDR);
outw((u16)(lp->sh_mem & LA_MASK), DEPCA_DATA);
outw(CSR2, DEPCA_ADDR);
outw((u16)((lp->sh_mem & LA_MASK) >> 16), DEPCA_DATA);
outw(CSR3, DEPCA_ADDR);
outw(ACON, DEPCA_DATA);
outw(CSR0, DEPCA_ADDR);
return;
}
static int InitRestartDepca(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
u_long ioaddr = dev->base_addr;
int i, status=0;
memcpy_toio((char *)lp->sh_mem, &lp->init_block, sizeof(struct depca_init));
outw(CSR0, DEPCA_ADDR);
outw(INIT, DEPCA_DATA);
for (i=0;(i<100) && !(inw(DEPCA_DATA) & IDON); i++);
if (i!=100) {
outw(IDON | INEA | STRT, DEPCA_DATA);
if (depca_debug > 2) {
printk("%s: DEPCA open after %d ticks, init block 0x%08lx csr0 %4.4x.\n",
dev->name, i, lp->sh_mem, inw(DEPCA_DATA));
}
} else {
printk("%s: DEPCA unopen after %d ticks, init block 0x%08lx csr0 %4.4x.\n",
dev->name, i, lp->sh_mem, inw(DEPCA_DATA));
status = -1;
}
return status;
}
static struct enet_statistics *
depca_get_stats(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
return &lp->stats;
}
static void
set_multicast_list(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
u_long ioaddr = dev->base_addr;
if (irq2dev_map[dev->irq] != NULL) {
while(dev->tbusy);
set_bit(0, (void*)&dev->tbusy);
while(lp->tx_old != lp->tx_new);
STOP_DEPCA;
depca_init_ring(dev);
if (dev->flags & IFF_PROMISC) {
lp->init_block.mode |= PROM;
} else {
SetMulticastFilter(dev);
lp->init_block.mode &= ~PROM;
}
LoadCSRs(dev);
InitRestartDepca(dev);
dev->tbusy = 0;
}
}
static void SetMulticastFilter(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
struct dev_mc_list *dmi=dev->mc_list;
char *addrs;
int i, j, bit, byte;
u16 hashcode;
s32 crc, poly = CRC_POLYNOMIAL_BE;
if (dev->flags & IFF_ALLMULTI) {
for (i=0; i<(HASH_TABLE_LEN>>3); i++) {
lp->init_block.mcast_table[i] = (char)0xff;
}
} else {
for (i=0; i<(HASH_TABLE_LEN>>3); i++){
lp->init_block.mcast_table[i]=0;
}
for (i=0;i<dev->mc_count;i++) {
addrs=dmi->dmi_addr;
dmi=dmi->next;
if ((*addrs & 0x01) == 1) {
crc = 0xffffffff;
for (byte=0;byte<ETH_ALEN;byte++) {
for (bit = *addrs++,j=0;j<8;j++, bit>>=1) {
crc = (crc << 1) ^ ((((crc<0?1:0) ^ bit) & 0x01) ? poly : 0);
}
}
hashcode = (crc & 1);
for (j=0;j<5;j++) {
hashcode = (hashcode << 1) | ((crc>>=1) & 1);
}
byte = hashcode >> 3;
bit = 1 << (hashcode & 0x07);
lp->init_block.mcast_table[byte] |= bit;
}
}
}
return;
}
static void isa_probe(struct device *dev, u_long ioaddr)
{
int i = num_depcas, maxSlots;
s32 ports[] = DEPCA_IO_PORTS;
if (!ioaddr && autoprobed) return ;
if (ioaddr > 0x400) return;
if (i >= MAX_NUM_DEPCAS) return;
if (ioaddr == 0) {
maxSlots = MAX_NUM_DEPCAS;
} else {
ports[i] = ioaddr;
maxSlots = i + 1;
}
for (; (i<maxSlots) && (dev!=NULL) && ports[i]; i++) {
if (DevicePresent(ports[i]) == 0) {
if (check_region(ports[i], DEPCA_TOTAL_SIZE) == 0) {
if ((dev = alloc_device(dev, ports[i])) != NULL) {
if (depca_hw_init(dev, ports[i]) == 0) {
num_depcas++;
}
num_eth++;
}
} else if (autoprobed) {
printk("%s: region already allocated at 0x%04x.\n", dev->name,ports[i]);
}
}
}
return;
}
static void eisa_probe(struct device *dev, u_long ioaddr)
{
int i, maxSlots;
u_long iobase;
char name[DEPCA_STRLEN];
if (!ioaddr && autoprobed) return ;
if ((ioaddr < 0x400) && (ioaddr > 0)) return;
if (ioaddr == 0) {
iobase = EISA_SLOT_INC;
i = 1;
maxSlots = MAX_EISA_SLOTS;
} else {
iobase = ioaddr;
i = (ioaddr >> 12);
maxSlots = i + 1;
}
if ((iobase & 0x0fff) == 0) iobase += DEPCA_EISA_IO_PORTS;
for (; (i<maxSlots) && (dev!=NULL); i++, iobase+=EISA_SLOT_INC) {
if (EISA_signature(name, EISA_ID)) {
if (DevicePresent(iobase) == 0) {
if (check_region(iobase, DEPCA_TOTAL_SIZE) == 0) {
if ((dev = alloc_device(dev, iobase)) != NULL) {
if (depca_hw_init(dev, iobase) == 0) {
num_depcas++;
}
num_eth++;
}
} else if (autoprobed) {
printk("%s: region already allocated at 0x%04lx.\n",dev->name,iobase);
}
}
}
}
return;
}
static struct device *
alloc_device(struct device *dev, u_long iobase)
{
struct device *adev = NULL;
int fixed = 0, new_dev = 0;
num_eth = depca_dev_index(dev->name);
if (loading_module) return dev;
while (1) {
if (((dev->base_addr == DEPCA_NDA) || (dev->base_addr==0)) && !adev) {
adev=dev;
} else if ((dev->priv == NULL) && (dev->base_addr==iobase)) {
fixed = 1;
} else {
if (dev->next == NULL) {
new_dev = 1;
} else if (strncmp(dev->next->name, "eth", 3) != 0) {
new_dev = 1;
}
}
if ((dev->next == NULL) || new_dev || fixed) break;
dev = dev->next;
num_eth++;
}
if (adev && !fixed) {
dev = adev;
num_eth = depca_dev_index(dev->name);
new_dev = 0;
}
if (((dev->next == NULL) &&
((dev->base_addr != DEPCA_NDA) && (dev->base_addr != 0)) && !fixed) ||
new_dev) {
num_eth++;
dev = insert_device(dev, iobase, depca_probe);
}
return dev;
}
static struct device *
insert_device(struct device *dev, u_long iobase, int (*init)(struct device *))
{
struct device *new;
new = (struct device *)kmalloc(sizeof(struct device)+8, GFP_KERNEL);
if (new == NULL) {
printk("eth%d: Device not initialised, insufficient memory\n",num_eth);
return NULL;
} else {
new->next = dev->next;
dev->next = new;
dev = dev->next;
dev->name = (char *)(dev + 1);
if (num_eth > 9999) {
sprintf(dev->name,"eth????");
} else {
sprintf(dev->name,"eth%d", num_eth);
}
dev->base_addr = iobase;
dev->init = init;
}
return dev;
}
static int
depca_dev_index(char *s)
{
int i=0, j=0;
for (;*s; s++) {
if (isdigit(*s)) {
j=1;
i = (i * 10) + (*s - '0');
} else if (j) break;
}
return i;
}
static void DepcaSignature(char *name, u_long paddr)
{
u_int i,j,k;
const char *signatures[] = DEPCA_SIGNATURE;
char tmpstr[16];
for (i=0;i<16;i++) {
tmpstr[i] = readb(paddr+0xc000+i);
}
for (i=0;*signatures[i]!='\0';i++) {
for (j=0,k=0;j<16 && k<strlen(signatures[i]);j++) {
if (signatures[i][k] == tmpstr[j]) {
k++;
} else {
k=0;
}
}
if (k == strlen(signatures[i])) break;
}
if (*name && (i == unknown)) {
for (i=0;*signatures[i]!='\0';i++) {
if (strcmp(name,signatures[i]) == 0) break;
}
}
strcpy(name,signatures[i]);
adapter = i;
return;
}
static int DevicePresent(u_long ioaddr)
{
union {
struct {
u32 a;
u32 b;
} llsig;
char Sig[sizeof(u32) << 1];
} dev;
short sigLength=0;
s8 data;
s16 nicsr;
int i, j, status = 0;
data = inb(DEPCA_PROM);
data = inb(DEPCA_PROM);
if (data == 0x08) {
nicsr = inb(DEPCA_NICSR);
nicsr |= AAC;
outb(nicsr, DEPCA_NICSR);
}
dev.llsig.a = ETH_PROM_SIG;
dev.llsig.b = ETH_PROM_SIG;
sigLength = sizeof(u32) << 1;
for (i=0,j=0;j<sigLength && i<PROBE_LENGTH+sigLength-1;i++) {
data = inb(DEPCA_PROM);
if (dev.Sig[j] == data) {
j++;
} else {
if (data == dev.Sig[0]) {
j=1;
} else {
j=0;
}
}
}
if (j!=sigLength) {
status = -ENODEV;
}
return status;
}
static int get_hw_addr(struct device *dev)
{
u_long ioaddr = dev->base_addr;
int i, k, tmp, status = 0;
u_short j, x, chksum;
x = (((adapter == de100) || (adapter == de101)) ? 1 : 0);
for (i=0,k=0,j=0;j<3;j++) {
k <<= 1 ;
if (k > 0xffff) k-=0xffff;
k += (u_char) (tmp = inb(DEPCA_PROM + x));
dev->dev_addr[i++] = (u_char) tmp;
k += (u_short) ((tmp = inb(DEPCA_PROM + x)) << 8);
dev->dev_addr[i++] = (u_char) tmp;
if (k > 0xffff) k-=0xffff;
}
if (k == 0xffff) k=0;
chksum = (u_char) inb(DEPCA_PROM + x);
chksum |= (u_short) (inb(DEPCA_PROM + x) << 8);
if (k != chksum) status = -1;
return status;
}
static int load_packet(struct device *dev, struct sk_buff *skb)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
int i, entry, end, len, status = 0;
entry = lp->tx_new;
end = (entry + (skb->len - 1) / TX_BUFF_SZ) & lp->txRingMask;
if (!(readl(&lp->tx_ring[end].base) & T_OWN)) {
if (end < entry) {
len = (lp->txRingMask - entry + 1) * TX_BUFF_SZ;
memcpy_toio(lp->tx_memcpy[entry], skb->data, len);
memcpy_toio(lp->tx_memcpy[0], skb->data + len, skb->len - len);
} else {
memcpy_toio(lp->tx_memcpy[entry], skb->data, skb->len);
}
len = (skb->len < ETH_ZLEN) ? ETH_ZLEN : skb->len;
for (i = entry; i != end; i = (i + 1) & lp->txRingMask) {
writel(readl(&lp->tx_ring[i].base) & ~T_FLAGS, &lp->tx_ring[i].base);
writew(0x0000, &lp->tx_ring[i].misc);
writew(-TX_BUFF_SZ, &lp->tx_ring[i].length);
len -= TX_BUFF_SZ;
}
writel(readl(&lp->tx_ring[end].base) & ~T_FLAGS, &lp->tx_ring[end].base);
writew(0x0000, &lp->tx_ring[end].misc);
writew(-len, &lp->tx_ring[end].length);
writel(readl(&lp->tx_ring[entry].base) | T_STP, &lp->tx_ring[entry].base);
writel(readl(&lp->tx_ring[end].base) | T_ENP, &lp->tx_ring[end].base);
for (i=end; i!=entry; --i) {
writel(readl(&lp->tx_ring[i].base) | T_OWN, &lp->tx_ring[i].base);
if (i == 0) i=lp->txRingMask+1;
}
writel(readl(&lp->tx_ring[entry].base) | T_OWN, &lp->tx_ring[entry].base);
lp->tx_new = (++end) & lp->txRingMask;
} else {
status = -1;
}
return status;
}
static int EISA_signature(char *name, s32 eisa_id)
{
u_int i;
const char *signatures[] = DEPCA_SIGNATURE;
char ManCode[DEPCA_STRLEN];
union {
s32 ID;
char Id[4];
} Eisa;
int status = 0;
*name = '\0';
Eisa.ID = inl(eisa_id);
ManCode[0]=(((Eisa.Id[0]>>2)&0x1f)+0x40);
ManCode[1]=(((Eisa.Id[1]&0xe0)>>5)+((Eisa.Id[0]&0x03)<<3)+0x40);
ManCode[2]=(((Eisa.Id[2]>>4)&0x0f)+0x30);
ManCode[3]=(( Eisa.Id[2]&0x0f)+0x30);
ManCode[4]=(((Eisa.Id[3]>>4)&0x0f)+0x30);
ManCode[5]='\0';
for (i=0;(*signatures[i] != '\0') && (*name == '\0');i++) {
if (strstr(ManCode, signatures[i]) != NULL) {
strcpy(name,ManCode);
status = 1;
}
}
return status;
}
static void depca_dbg_open(struct device *dev)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
u_long ioaddr = dev->base_addr;
struct depca_init *p = (struct depca_init *)lp->sh_mem;
int i;
if (depca_debug > 1){
memcpy_toio((char *)lp->sh_mem,&lp->init_block,sizeof(struct depca_init));
printk("%s: depca open with irq %d\n",dev->name,dev->irq);
printk("Descriptor head addresses:\n");
printk("\t0x%lx  0x%lx\n",(u_long)lp->rx_ring, (u_long)lp->tx_ring);
printk("Descriptor addresses:\nRX: ");
for (i=0;i<lp->rxRingMask;i++){
if (i < 3) {
printk("0x%8.8lx ", (long) &lp->rx_ring[i].base);
}
}
printk("...0x%8.8lx\n", (long) &lp->rx_ring[i].base);
printk("TX: ");
for (i=0;i<lp->txRingMask;i++){
if (i < 3) {
printk("0x%8.8lx ", (long) &lp->tx_ring[i].base);
}
}
printk("...0x%8.8lx\n", (long) &lp->tx_ring[i].base);
printk("\nDescriptor buffers:\nRX: ");
for (i=0;i<lp->rxRingMask;i++){
if (i < 3) {
printk("0x%8.8x  ", readl(&lp->rx_ring[i].base));
}
}
printk("...0x%8.8x\n", readl(&lp->rx_ring[i].base));
printk("TX: ");
for (i=0;i<lp->txRingMask;i++){
if (i < 3) {
printk("0x%8.8x  ", readl(&lp->tx_ring[i].base));
}
}
printk("...0x%8.8x\n", readl(&lp->tx_ring[i].base));
printk("Initialisation block at 0x%8.8lx\n",lp->sh_mem);
printk("\tmode: 0x%4.4x\n",readw(&p->mode));
printk("\tphysical address: ");
for (i=0;i<ETH_ALEN-1;i++){
printk("%2.2x:",(u_char)readb(&p->phys_addr[i]));
}
printk("%2.2x\n",(u_char)readb(&p->phys_addr[i]));
printk("\tmulticast hash table: ");
for (i=0;i<(HASH_TABLE_LEN >> 3)-1;i++){
printk("%2.2x:",(u_char)readb(&p->mcast_table[i]));
}
printk("%2.2x\n",(u_char)readb(&p->mcast_table[i]));
printk("\trx_ring at: 0x%8.8x\n",readl(&p->rx_ring));
printk("\ttx_ring at: 0x%8.8x\n",readl(&p->tx_ring));
printk("dma_buffs: 0x%8.8lx\n",lp->dma_buffs);
printk("Ring size:\nRX: %d  Log2(rxRingMask): 0x%8.8x\n",
(int)lp->rxRingMask + 1,
lp->rx_rlen);
printk("TX: %d  Log2(txRingMask): 0x%8.8x\n",
(int)lp->txRingMask + 1,
lp->tx_rlen);
outw(CSR2,DEPCA_ADDR);
printk("CSR2&1: 0x%4.4x",inw(DEPCA_DATA));
outw(CSR1,DEPCA_ADDR);
printk("%4.4x\n",inw(DEPCA_DATA));
outw(CSR3,DEPCA_ADDR);
printk("CSR3: 0x%4.4x\n",inw(DEPCA_DATA));
}
return;
}
static int depca_ioctl(struct device *dev, struct ifreq *rq, int cmd)
{
struct depca_private *lp = (struct depca_private *)dev->priv;
struct depca_ioctl *ioc = (struct depca_ioctl *) &rq->ifr_data;
int i, status = 0;
u_long ioaddr = dev->base_addr;
union {
u8 addr[(HASH_TABLE_LEN * ETH_ALEN)];
u16 sval[(HASH_TABLE_LEN * ETH_ALEN) >> 1];
u32 lval[(HASH_TABLE_LEN * ETH_ALEN) >> 2];
} tmp;
switch(ioc->cmd) {
case DEPCA_GET_HWADDR:
for (i=0; i<ETH_ALEN; i++) {
tmp.addr[i] = dev->dev_addr[i];
}
ioc->len = ETH_ALEN;
if (!(status = verify_area(VERIFY_WRITE, (void *)ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, tmp.addr, ioc->len);
}
break;
case DEPCA_SET_HWADDR:
if (suser()) {
if (!(status = verify_area(VERIFY_READ, (void *)ioc->data, ETH_ALEN))) {
memcpy_fromfs(tmp.addr,ioc->data,ETH_ALEN);
for (i=0; i<ETH_ALEN; i++) {
dev->dev_addr[i] = tmp.addr[i];
}
while(dev->tbusy);
set_bit(0, (void*)&dev->tbusy);
while(lp->tx_old != lp->tx_new);
STOP_DEPCA;
depca_init_ring(dev);
LoadCSRs(dev);
InitRestartDepca(dev);
dev->tbusy = 0;
}
} else {
status = -EPERM;
}
break;
case DEPCA_SET_PROM:
if (suser()) {
while(dev->tbusy);
set_bit(0, (void*)&dev->tbusy);
while(lp->tx_old != lp->tx_new);
STOP_DEPCA;
depca_init_ring(dev);
lp->init_block.mode |= PROM;
LoadCSRs(dev);
InitRestartDepca(dev);
dev->tbusy = 0;
} else {
status = -EPERM;
}
break;
case DEPCA_CLR_PROM:
if (suser()) {
while(dev->tbusy);
set_bit(0, (void*)&dev->tbusy);
while(lp->tx_old != lp->tx_new);
STOP_DEPCA;
depca_init_ring(dev);
lp->init_block.mode &= ~PROM;
LoadCSRs(dev);
InitRestartDepca(dev);
dev->tbusy = 0;
} else {
status = -EPERM;
}
break;
case DEPCA_SAY_BOO:
printk("%s: Boo!\n", dev->name);
break;
case DEPCA_GET_MCA:
ioc->len = (HASH_TABLE_LEN >> 3);
if (!(status = verify_area(VERIFY_WRITE, ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, lp->init_block.mcast_table, ioc->len);
}
break;
case DEPCA_SET_MCA:
if (suser()) {
if (!(status=verify_area(VERIFY_READ, ioc->data, ETH_ALEN*ioc->len))) {
memcpy_fromfs(tmp.addr, ioc->data, ETH_ALEN * ioc->len);
set_multicast_list(dev);
}
} else {
status = -EPERM;
}
break;
case DEPCA_CLR_MCA:
if (suser()) {
set_multicast_list(dev);
} else {
status = -EPERM;
}
break;
case DEPCA_MCA_EN:
if (suser()) {
set_multicast_list(dev);
} else {
status = -EPERM;
}
break;
case DEPCA_GET_STATS:
cli();
ioc->len = sizeof(lp->pktStats);
if (!(status=verify_area(VERIFY_WRITE, ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, &lp->pktStats, ioc->len);
}
sti();
break;
case DEPCA_CLR_STATS:
if (suser()) {
cli();
memset(&lp->pktStats, 0, sizeof(lp->pktStats));
sti();
} else {
status = -EPERM;
}
break;
case DEPCA_GET_REG:
i=0;
tmp.sval[i++] = inw(DEPCA_NICSR);
outw(CSR0, DEPCA_ADDR);
tmp.sval[i++] = inw(DEPCA_DATA);
memcpy(&tmp.sval[i], &lp->init_block, sizeof(struct depca_init));
ioc->len = i+sizeof(struct depca_init);
if (!(status=verify_area(VERIFY_WRITE, ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, tmp.addr, ioc->len);
}
break;
default:
status = -EOPNOTSUPP;
}
return status;
}
#ifdef MODULE
static char devicename[9] = { 0, };
static struct device thisDepca = {
devicename,
0, 0, 0, 0,
0x200, 7,
0, 0, 0, NULL, depca_probe };
static int irq=7;
static int io=0x200;
int
init_module(void)
{
thisDepca.irq=irq;
thisDepca.base_addr=io;
if (register_netdev(&thisDepca) != 0)
return -EIO;
return 0;
}
void
cleanup_module(void)
{
if (thisDepca.priv) {
kfree(thisDepca.priv);
thisDepca.priv = NULL;
}
thisDepca.irq=0;
unregister_netdev(&thisDepca);
release_region(thisDepca.base_addr, DEPCA_TOTAL_SIZE);
}
#endif