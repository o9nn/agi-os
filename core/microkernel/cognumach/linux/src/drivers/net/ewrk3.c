static const char *version = "ewrk3.c:v0.43 96/8/16 davies@maniac.ultranet.com\n";
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
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <asm/segment.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/time.h>
#include <linux/types.h>
#include <linux/unistd.h>
#include <linux/ctype.h>
#include "ewrk3.h"
#ifdef EWRK3_DEBUG
static int ewrk3_debug = EWRK3_DEBUG;
#else
static int ewrk3_debug = 1;
#endif
#define EWRK3_NDA 0xffe0
#define PROBE_LENGTH 32
#define ETH_PROM_SIG 0xAA5500FFUL
#ifndef EWRK3_SIGNATURE
#define EWRK3_SIGNATURE {"DE203","DE204","DE205",""}
#define EWRK3_STRLEN 8
#endif
#ifndef EWRK3_RAM_BASE_ADDRESSES
#define EWRK3_RAM_BASE_ADDRESSES {0xc0000,0xd0000,0x00000}
#endif
#define EWRK3_IO_BASE 0x100
#define EWRK3_IOP_INC 0x20
#define EWRK3_TOTAL_SIZE 0x20
#ifndef MAX_NUM_EWRK3S
#define MAX_NUM_EWRK3S 21
#endif
#ifndef EWRK3_EISA_IO_PORTS
#define EWRK3_EISA_IO_PORTS 0x0c00
#endif
#ifndef MAX_EISA_SLOTS
#define MAX_EISA_SLOTS 16
#define EISA_SLOT_INC 0x1000
#endif
#define CRC_POLYNOMIAL_BE 0x04c11db7UL
#define CRC_POLYNOMIAL_LE 0xedb88320UL
#define QUEUE_PKT_TIMEOUT (1*HZ)
#define IO_ONLY 0x00
#define SHMEM_2K 0x800
#define SHMEM_32K 0x8000
#define SHMEM_64K 0x10000
#define ENABLE_IRQs { \
icr |= lp->irq_mask;\
outb(icr, EWRK3_ICR); \
}
#define DISABLE_IRQs { \
icr = inb(EWRK3_ICR);\
icr &= ~lp->irq_mask;\
outb(icr, EWRK3_ICR); \
}
#define START_EWRK3 { \
csr = inb(EWRK3_CSR);\
csr &= ~(CSR_TXD|CSR_RXD);\
outb(csr, EWRK3_CSR); \
}
#define STOP_EWRK3 { \
csr = (CSR_TXD|CSR_RXD);\
outb(csr, EWRK3_CSR); \
}
#define EWRK3_PKT_STAT_SZ 16
#define EWRK3_PKT_BIN_SZ 128
struct ewrk3_private {
char adapter_name[80];
u_long shmem_base;
u_long shmem_length;
struct enet_statistics stats;
struct {
u32 bins[EWRK3_PKT_STAT_SZ];
u32 unicast;
u32 multicast;
u32 broadcast;
u32 excessive_collisions;
u32 tx_underruns;
u32 excessive_underruns;
} pktStats;
u_char irq_mask;
u_char mPage;
u_char lemac;
u_char hard_strapped;
u_char lock;
u_char txc;
u_char *mctbl;
};
#define FORCE_2K_MODE { \
shmem_length = SHMEM_2K;\
outb(((mem_start - 0x80000) >> 11), EWRK3_MBR);\
}
static int ewrk3_open(struct device *dev);
static int ewrk3_queue_pkt(struct sk_buff *skb, struct device *dev);
static void ewrk3_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static int ewrk3_close(struct device *dev);
static struct enet_statistics *ewrk3_get_stats(struct device *dev);
static void set_multicast_list(struct device *dev);
static int ewrk3_ioctl(struct device *dev, struct ifreq *rq, int cmd);
static int ewrk3_hw_init(struct device *dev, u_long iobase);
static void ewrk3_init(struct device *dev);
static int ewrk3_rx(struct device *dev);
static int ewrk3_tx(struct device *dev);
static void EthwrkSignature(char * name, char *eeprom_image);
static int DevicePresent(u_long iobase);
static void SetMulticastFilter(struct device *dev);
static int EISA_signature(char *name, s32 eisa_id);
static int Read_EEPROM(u_long iobase, u_char eaddr);
static int Write_EEPROM(short data, u_long iobase, u_char eaddr);
static u_char get_hw_addr (struct device *dev, u_char *eeprom_image, char chipType);
static void isa_probe(struct device *dev, u_long iobase);
static void eisa_probe(struct device *dev, u_long iobase);
static struct device *alloc_device(struct device *dev, u_long iobase);
static int ewrk3_dev_index(char *s);
static struct device *insert_device(struct device *dev, u_long iobase, int (*init)(struct device *));
#ifdef MODULE
int init_module(void);
void cleanup_module(void);
static int autoprobed = 1, loading_module = 1;
# else
static u_char irq[] = {5,0,10,3,11,9,15,12};
static int autoprobed = 0, loading_module = 0;
#endif
static char name[EWRK3_STRLEN + 1];
static int num_ewrk3s = 0, num_eth = 0;
#define INIT_EWRK3 {\
outb(EEPROM_INIT, EWRK3_IOPR);\
udelay(1000);\
}
int ewrk3_probe(struct device *dev)
{
int tmp = num_ewrk3s, status = -ENODEV;
u_long iobase = dev->base_addr;
if ((iobase == 0) && loading_module){
printk("Autoprobing is not supported when loading a module based driver.\n");
status = -EIO;
} else {
isa_probe(dev, iobase);
eisa_probe(dev, iobase);
if ((tmp == num_ewrk3s) && (iobase != 0) && loading_module) {
printk("%s: ewrk3_probe() cannot find device at 0x%04lx.\n", dev->name,
iobase);
}
for (; (dev->priv == NULL) && (dev->next != NULL); dev = dev->next);
if (dev->priv) status = 0;
if (iobase == 0) autoprobed = 1;
}
return status;
}
static int
ewrk3_hw_init(struct device *dev, u_long iobase)
{
struct ewrk3_private *lp;
int i, status=0;
u_long mem_start, shmem_length;
u_char cr, cmr, icr, nicsr, lemac, hard_strapped = 0;
u_char eeprom_image[EEPROM_MAX], chksum, eisa_cr = 0;
if (iobase > 0x400) eisa_cr = inb(EISA_CR);
INIT_EWRK3;
nicsr = inb(EWRK3_CSR);
icr = inb(EWRK3_ICR);
icr &= 0x70;
outb(icr, EWRK3_ICR);
if (nicsr == (CSR_TXD|CSR_RXD)) {
for (chksum=0, i=0; i<EEPROM_MAX; i+=2) {
union {
short val;
char c[2];
} tmp;
tmp.val = (short)Read_EEPROM(iobase, (i>>1));
eeprom_image[i] = tmp.c[0];
eeprom_image[i+1] = tmp.c[1];
chksum += eeprom_image[i] + eeprom_image[i+1];
}
if (chksum != 0) {
printk("%s: Device has a bad on-board EEPROM.\n", dev->name);
status = -ENXIO;
} else {
EthwrkSignature(name, eeprom_image);
if (*name != '\0') {
dev->base_addr = iobase;
if (iobase > 0x400) {
outb(eisa_cr, EISA_CR);
}
lemac = eeprom_image[EEPROM_CHIPVER];
cmr = inb(EWRK3_CMR);
if (((lemac == LeMAC) && ((cmr & CMR_NO_EEPROM) != CMR_NO_EEPROM)) ||
((lemac == LeMAC2) && !(cmr & CMR_HS))) {
printk("%s: %s at %#4lx", dev->name, name, iobase);
hard_strapped = 1;
} else if ((iobase&0x0fff)==EWRK3_EISA_IO_PORTS) {
printk("%s: %s at %#4lx (EISA slot %ld)",
dev->name, name, iobase, ((iobase>>12)&0x0f));
} else {
printk("%s: %s at %#4lx", dev->name, name, iobase);
}
if (!status) {
printk(", h/w address ");
if (lemac!=LeMAC2) DevicePresent(iobase);
status = get_hw_addr(dev, eeprom_image, lemac);
for (i = 0; i < ETH_ALEN - 1; i++) {
printk("%2.2x:", dev->dev_addr[i]);
}
printk("%2.2x,\n", dev->dev_addr[i]);
if (status) {
printk("      which has an EEPROM CRC error.\n");
status = -ENXIO;
} else {
if (lemac == LeMAC2) {
cmr &= ~(CMR_RA | CMR_WB | CMR_LINK | CMR_POLARITY | CMR_0WS);
if (eeprom_image[EEPROM_MISC0] & READ_AHEAD) cmr |= CMR_RA;
if (eeprom_image[EEPROM_MISC0] & WRITE_BEHIND) cmr |= CMR_WB;
if (eeprom_image[EEPROM_NETMAN0] & NETMAN_POL) cmr |= CMR_POLARITY;
if (eeprom_image[EEPROM_NETMAN0] & NETMAN_LINK) cmr |= CMR_LINK;
if (eeprom_image[EEPROM_MISC0] & _0WS_ENA) cmr |= CMR_0WS;
}
if (eeprom_image[EEPROM_SETUP] & SETUP_DRAM) cmr |= CMR_DRAM;
outb(cmr, EWRK3_CMR);
cr = inb(EWRK3_CR);
cr |= eeprom_image[EEPROM_SETUP] & SETUP_APD;
if (cr & SETUP_APD) cr |= eeprom_image[EEPROM_SETUP] & SETUP_PS;
cr |= eeprom_image[EEPROM_MISC0] & FAST_BUS;
cr |= eeprom_image[EEPROM_MISC0] & ENA_16;
outb(cr, EWRK3_CR);
mem_start = inb(EWRK3_MBR);
shmem_length = 0;
if (mem_start != 0) {
if ((mem_start >= 0x0a) && (mem_start <= 0x0f)) {
mem_start *= SHMEM_64K;
shmem_length = SHMEM_64K;
} else if ((mem_start >= 0x14) && (mem_start <= 0x1f)) {
mem_start *= SHMEM_32K;
shmem_length = SHMEM_32K;
} else if ((mem_start >= 0x40) && (mem_start <= 0xff)) {
mem_start = mem_start * SHMEM_2K + 0x80000;
shmem_length = SHMEM_2K;
} else {
status = -ENXIO;
}
}
if (!status) {
if (hard_strapped) {
printk("      is hard strapped.\n");
} else if (mem_start) {
printk("      has a %dk RAM window", (int)(shmem_length >> 10));
printk(" at 0x%.5lx", mem_start);
} else {
printk("      is in I/O only mode");
}
dev->priv = (void *) kmalloc(sizeof(struct ewrk3_private),
GFP_KERNEL);
if (dev->priv == NULL) {
return -ENOMEM;
}
lp = (struct ewrk3_private *)dev->priv;
memset(dev->priv, 0, sizeof(struct ewrk3_private));
lp->shmem_base = mem_start;
lp->shmem_length = shmem_length;
lp->lemac = lemac;
lp->hard_strapped = hard_strapped;
lp->mPage = 64;
if (cmr & CMR_DRAM) lp->mPage <<= 1 ;
sprintf(lp->adapter_name,"%s (%s)", name, dev->name);
request_region(iobase, EWRK3_TOTAL_SIZE, lp->adapter_name);
lp->irq_mask = ICR_TNEM|ICR_TXDM|ICR_RNEM|ICR_RXDM;
if (!hard_strapped) {
icr |= ICR_IE;
outb(icr, EWRK3_ICR);
dev->dma = 0;
if (dev->irq < 2) {
#ifndef MODULE
u_char irqnum;
autoirq_setup(0);
icr |=ICR_TNEM;
outb(1,EWRK3_TDQ);
outb(icr, EWRK3_ICR);
irqnum = irq[((icr & IRQ_SEL) >> 4)];
dev->irq = autoirq_report(1);
if ((dev->irq) && (irqnum == dev->irq)) {
printk(" and uses IRQ%d.\n", dev->irq);
} else {
if (!dev->irq) {
printk(" and failed to detect IRQ line.\n");
} else if ((irqnum == 1) && (lemac == LeMAC2)) {
printk(" and an illegal IRQ line detected.\n");
} else {
printk(", but incorrect IRQ line detected.\n");
}
status = -ENXIO;
}
DISABLE_IRQs;
#endif
} else {
printk(" and requires IRQ%d.\n", dev->irq);
}
}
if (status) release_region(iobase, EWRK3_TOTAL_SIZE);
} else {
status = -ENXIO;
}
}
}
} else {
status = -ENXIO;
}
}
if (!status) {
if (ewrk3_debug > 1) {
printk("%s", version);
}
dev->open = &ewrk3_open;
dev->hard_start_xmit = &ewrk3_queue_pkt;
dev->stop = &ewrk3_close;
dev->get_stats = &ewrk3_get_stats;
dev->set_multicast_list = &set_multicast_list;
dev->do_ioctl = &ewrk3_ioctl;
dev->mem_start = 0;
ether_setup(dev);
}
} else {
status = -ENXIO;
}
return status;
}
static int
ewrk3_open(struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
u_long iobase = dev->base_addr;
int i, status = 0;
u_char icr, csr;
STOP_EWRK3;
if (!lp->hard_strapped) {
irq2dev_map[dev->irq] = dev;
if (request_irq(dev->irq, (void *)ewrk3_interrupt, 0, "ewrk3", NULL)) {
printk("ewrk3_open(): Requested IRQ%d is busy\n",dev->irq);
status = -EAGAIN;
} else {
ewrk3_init(dev);
if (ewrk3_debug > 1){
printk("%s: ewrk3 open with irq %d\n",dev->name,dev->irq);
printk("  physical address: ");
for (i=0;i<5;i++){
printk("%2.2x:",(u_char)dev->dev_addr[i]);
}
printk("%2.2x\n",(u_char)dev->dev_addr[i]);
if (lp->shmem_length == 0) {
printk("  no shared memory, I/O only mode\n");
} else {
printk("  start of shared memory: 0x%08lx\n",lp->shmem_base);
printk("  window length: 0x%04lx\n",lp->shmem_length);
}
printk("  # of DRAMS: %d\n",((inb(EWRK3_CMR) & 0x02) ? 2 : 1));
printk("  csr:  0x%02x\n", inb(EWRK3_CSR));
printk("  cr:   0x%02x\n", inb(EWRK3_CR));
printk("  icr:  0x%02x\n", inb(EWRK3_ICR));
printk("  cmr:  0x%02x\n", inb(EWRK3_CMR));
printk("  fmqc: 0x%02x\n", inb(EWRK3_FMQC));
}
dev->tbusy = 0;
dev->start = 1;
dev->interrupt = UNMASK_INTERRUPTS;
icr = inb(EWRK3_ICR);
ENABLE_IRQs;
}
} else {
dev->start = 0;
dev->tbusy = 1;
printk("%s: ewrk3 available for hard strapped set up only.\n", dev->name);
printk("      Run the 'ewrk3setup' utility or remove the hard straps.\n");
}
MOD_INC_USE_COUNT;
return status;
}
static void
ewrk3_init(struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
u_char csr, page;
u_long iobase = dev->base_addr;
set_multicast_list(dev);
while (inb(EWRK3_TQ));
while (inb(EWRK3_TDQ));
while (inb(EWRK3_RQ));
while (inb(EWRK3_FMQ));
for (page=1;page<lp->mPage;page++) {
outb(page, EWRK3_FMQ);
}
lp->lock = 0;
START_EWRK3;
}
static int
ewrk3_queue_pkt(struct sk_buff *skb, struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
u_long iobase = dev->base_addr;
int status = 0;
u_char icr, csr;
if (dev->tbusy || lp->lock) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < QUEUE_PKT_TIMEOUT) {
status = -1;
} else if (!lp->hard_strapped) {
printk("%s: transmit timed/locked out, status %04x, resetting.\n",
dev->name, inb(EWRK3_CSR));
DISABLE_IRQs;
STOP_EWRK3;
ewrk3_init(dev);
ENABLE_IRQs;
dev->tbusy=0;
dev->trans_start = jiffies;
dev_kfree_skb(skb, FREE_WRITE);
}
} else if (skb == NULL) {
dev_tint(dev);
} else if (skb->len > 0) {
if (set_bit(0, (void*)&dev->tbusy) != 0)
printk("%s: Transmitter access conflict.\n", dev->name);
DISABLE_IRQs;
if (inb(EWRK3_FMQC) > 0) {
u_long buf = 0;
u_char page;
if ((page = inb(EWRK3_FMQ)) < lp->mPage) {
while (set_bit(0, (void *)&lp->lock) != 0);
if (lp->shmem_length == IO_ONLY) {
outb(page, EWRK3_IOPR);
} else if (lp->shmem_length == SHMEM_2K) {
buf = lp->shmem_base;
outb(page, EWRK3_MPR);
} else if (lp->shmem_length == SHMEM_32K) {
buf = ((((short)page << 11) & 0x7800) + lp->shmem_base);
outb((page >> 4), EWRK3_MPR);
} else if (lp->shmem_length == SHMEM_64K) {
buf = ((((short)page << 11) & 0xf800) + lp->shmem_base);
outb((page >> 5), EWRK3_MPR);
} else {
status = -1;
printk("%s: Oops - your private data area is hosed!\n",dev->name);
}
if (!status) {
if (lp->shmem_length == IO_ONLY) {
int i;
u_char *p = skb->data;
outb((char)(TCR_QMODE | TCR_PAD | TCR_IFC), EWRK3_DATA);
outb((char)(skb->len & 0xff), EWRK3_DATA);
outb((char)((skb->len >> 8) & 0xff), EWRK3_DATA);
outb((char)0x04, EWRK3_DATA);
for (i=0; i<skb->len; i++) {
outb(*p++, EWRK3_DATA);
}
outb(page, EWRK3_TQ);
} else {
writeb((char)(TCR_QMODE|TCR_PAD|TCR_IFC), (char *)buf);
buf+=1;
writeb((char)(skb->len & 0xff), (char *)buf);
buf+=1;
if (lp->txc) {
writeb((char)(((skb->len >> 8) & 0xff) | XCT), (char *)buf);
buf+=1;
writeb(0x04, (char *)buf);
buf+=1;
writeb(0x00, (char *)(buf + skb->len));
memcpy_toio(buf, skb->data, PRELOAD);
outb(page, EWRK3_TQ);
memcpy_toio(buf+PRELOAD, skb->data+PRELOAD, skb->len-PRELOAD);
writeb(0xff, (char *)(buf + skb->len));
} else {
writeb((char)((skb->len >> 8) & 0xff), (char *)buf);
buf+=1;
writeb(0x04, (char *)buf);
buf+=1;
memcpy_toio((char *)buf, skb->data, skb->len);
outb(page, EWRK3_TQ);
}
}
dev->trans_start = jiffies;
dev_kfree_skb (skb, FREE_WRITE);
} else {
outb(page, EWRK3_FMQ);
}
lp->lock = 0;
} else {
printk("ewrk3_queue_pkt(): Invalid free memory page (%d).\n",
(u_char) page);
}
} else {
printk("ewrk3_queue_pkt(): No free resources...\n");
printk("ewrk3_queue_pkt(): CSR: %02x ICR: %02x FMQC: %02x\n",inb(EWRK3_CSR),inb(EWRK3_ICR),inb(EWRK3_FMQC));
}
if (inb(EWRK3_FMQC) > 0) {
dev->tbusy = 0;
}
ENABLE_IRQs;
}
return status;
}
static void
ewrk3_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
struct ewrk3_private *lp;
u_long iobase;
u_char icr, cr, csr;
if (dev == NULL) {
printk ("ewrk3_interrupt(): irq %d for unknown device.\n", irq);
} else {
lp = (struct ewrk3_private *)dev->priv;
iobase = dev->base_addr;
if (dev->interrupt)
printk("%s: Re-entering the interrupt handler.\n", dev->name);
dev->interrupt = MASK_INTERRUPTS;
csr = inb(EWRK3_CSR);
DISABLE_IRQs;
cr = inb(EWRK3_CR);
cr |= CR_LED;
outb(cr, EWRK3_CR);
if (csr & CSR_RNE)
ewrk3_rx(dev);
if (csr & CSR_TNE)
ewrk3_tx(dev);
if (inb(EWRK3_FMQC)) {
lp->irq_mask |= ICR_TXDM|ICR_RXDM;
csr &= ~(CSR_TXD|CSR_RXD);
outb(csr, EWRK3_CSR);
dev->tbusy = 0;
mark_bh(NET_BH);
} else {
lp->irq_mask &= ~(ICR_TXDM|ICR_RXDM);
}
cr &= ~CR_LED;
outb(cr, EWRK3_CR);
dev->interrupt = UNMASK_INTERRUPTS;
ENABLE_IRQs;
}
return;
}
static int
ewrk3_rx(struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
u_long iobase = dev->base_addr;
int i, status = 0;
u_char page, tmpPage = 0, tmpLock = 0;
u_long buf = 0;
while (inb(EWRK3_RQC) && !status) {
if ((page = inb(EWRK3_RQ)) < lp->mPage) {
if ((tmpLock = set_bit(0, (void *)&lp->lock)) == 1) {
if (lp->shmem_length == IO_ONLY) {
tmpPage = inb(EWRK3_IOPR);
} else {
tmpPage = inb(EWRK3_MPR);
}
}
if (lp->shmem_length == IO_ONLY) {
outb(page, EWRK3_IOPR);
} else if (lp->shmem_length == SHMEM_2K) {
buf = lp->shmem_base;
outb(page, EWRK3_MPR);
} else if (lp->shmem_length == SHMEM_32K) {
buf = ((((short)page << 11) & 0x7800) + lp->shmem_base);
outb((page >> 4), EWRK3_MPR);
} else if (lp->shmem_length == SHMEM_64K) {
buf = ((((short)page << 11) & 0xf800) + lp->shmem_base);
outb((page >> 5), EWRK3_MPR);
} else {
status = -1;
printk("%s: Oops - your private data area is hosed!\n",dev->name);
}
if (!status) {
char rx_status;
int pkt_len;
if (lp->shmem_length == IO_ONLY) {
rx_status = inb(EWRK3_DATA);
pkt_len = inb(EWRK3_DATA);
pkt_len |= ((u_short)inb(EWRK3_DATA) << 8);
} else {
rx_status = readb(buf);
buf+=1;
pkt_len = readw(buf);
buf+=3;
}
if (!(rx_status & R_ROK)) {
lp->stats.rx_errors++;
if (rx_status & R_DBE) lp->stats.rx_frame_errors++;
if (rx_status & R_CRC) lp->stats.rx_crc_errors++;
if (rx_status & R_PLL) lp->stats.rx_fifo_errors++;
} else {
struct sk_buff *skb;
if ((skb = dev_alloc_skb(pkt_len+2)) != NULL) {
unsigned char *p;
skb->dev = dev;
skb_reserve(skb,2);
p = skb_put(skb,pkt_len);
if (lp->shmem_length == IO_ONLY) {
*p = inb(EWRK3_DATA);
for (i=0; i<pkt_len; i++) {
*p++ = inb(EWRK3_DATA);
}
} else {
memcpy_fromio(p, buf, pkt_len);
}
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
for (i=1; i<EWRK3_PKT_STAT_SZ-1; i++) {
if (pkt_len < i*EWRK3_PKT_BIN_SZ) {
lp->pktStats.bins[i]++;
i = EWRK3_PKT_STAT_SZ;
}
}
p = skb->data;
if (p[0] & 0x01) {
if ((*(s32 *)&p[0] == -1) && (*(s16 *)&p[4] == -1)) {
lp->pktStats.broadcast++;
} else {
lp->pktStats.multicast++;
}
} else if ((*(s32 *)&p[0] == *(s32 *)&dev->dev_addr[0]) &&
(*(s16 *)&p[4] == *(s16 *)&dev->dev_addr[4])) {
lp->pktStats.unicast++;
}
lp->pktStats.bins[0]++;
if (lp->pktStats.bins[0] == 0) {
memset(&lp->pktStats, 0, sizeof(lp->pktStats));
}
} else {
printk("%s: Insufficient memory; nuking packet.\n", dev->name);
lp->stats.rx_dropped++;
break;
}
}
}
outb(page, EWRK3_FMQ);
if (tmpLock) {
if (lp->shmem_length == IO_ONLY) {
outb(tmpPage, EWRK3_IOPR);
} else {
outb(tmpPage, EWRK3_MPR);
}
}
lp->lock = 0;
} else {
printk("ewrk3_rx(): Illegal page number, page %d\n",page);
printk("ewrk3_rx(): CSR: %02x ICR: %02x FMQC: %02x\n",inb(EWRK3_CSR),inb(EWRK3_ICR),inb(EWRK3_FMQC));
}
}
return status;
}
static int
ewrk3_tx(struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
u_long iobase = dev->base_addr;
u_char tx_status;
while ((tx_status = inb(EWRK3_TDQ)) > 0) {
if (tx_status & T_VSTS) {
if (tx_status & T_TXE) {
lp->stats.tx_errors++;
if (tx_status & T_NCL) lp->stats.tx_carrier_errors++;
if (tx_status & T_LCL) lp->stats.tx_window_errors++;
if (tx_status & T_CTU) {
if ((tx_status & T_COLL) ^ T_XUR) {
lp->pktStats.tx_underruns++;
} else {
lp->pktStats.excessive_underruns++;
}
} else if (tx_status & T_COLL) {
if ((tx_status & T_COLL) ^ T_XCOLL) {
lp->stats.collisions++;
} else {
lp->pktStats.excessive_collisions++;
}
}
} else {
lp->stats.tx_packets++;
}
}
}
return 0;
}
static int
ewrk3_close(struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
u_long iobase = dev->base_addr;
u_char icr, csr;
dev->start = 0;
dev->tbusy = 1;
if (ewrk3_debug > 1) {
printk("%s: Shutting down ethercard, status was %2.2x.\n",
dev->name, inb(EWRK3_CSR));
}
DISABLE_IRQs;
STOP_EWRK3;
while (inb(EWRK3_TQ));
while (inb(EWRK3_TDQ));
while (inb(EWRK3_RQ));
if (!lp->hard_strapped) {
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = 0;
}
MOD_DEC_USE_COUNT;
return 0;
}
static struct enet_statistics *
ewrk3_get_stats(struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
return &lp->stats;
}
static void
set_multicast_list(struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
u_long iobase = dev->base_addr;
u_char csr;
if (irq2dev_map[dev->irq] != NULL) {
csr = inb(EWRK3_CSR);
if (lp->shmem_length == IO_ONLY) {
lp->mctbl = (char *) PAGE0_HTE;
} else {
lp->mctbl = (char *)(lp->shmem_base + PAGE0_HTE);
}
csr &= ~(CSR_PME | CSR_MCE);
if (dev->flags & IFF_PROMISC) {
csr |= CSR_PME;
outb(csr, EWRK3_CSR);
} else {
SetMulticastFilter(dev);
csr |= CSR_MCE;
outb(csr, EWRK3_CSR);
}
}
}
static void SetMulticastFilter(struct device *dev)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
struct dev_mc_list *dmi=dev->mc_list;
u_long iobase = dev->base_addr;
int i;
char *addrs, j, bit, byte;
short *p = (short *) lp->mctbl;
u16 hashcode;
s32 crc, poly = CRC_POLYNOMIAL_LE;
while (set_bit(0, (void *)&lp->lock) != 0);
if (lp->shmem_length == IO_ONLY) {
outb(0, EWRK3_IOPR);
outw(EEPROM_OFFSET(lp->mctbl), EWRK3_PIR1);
} else {
outb(0, EWRK3_MPR);
}
if (dev->flags & IFF_ALLMULTI) {
for (i=0; i<(HASH_TABLE_LEN >> 3); i++) {
if (lp->shmem_length == IO_ONLY) {
outb(0xff, EWRK3_DATA);
} else {
writew(0xffff, p);
p++; i++;
}
}
} else {
if (lp->shmem_length == IO_ONLY) {
for (i=0; i<(HASH_TABLE_LEN >> 4) - 1; i++) {
outb(0x00, EWRK3_DATA);
}
outb(0x80, EWRK3_DATA); i++;
for (; i<(HASH_TABLE_LEN >> 3); i++) {
outb(0x00, EWRK3_DATA);
}
} else {
memset_io(lp->mctbl, 0, (HASH_TABLE_LEN >> 3));
writeb(0x80, (char *)(lp->mctbl + (HASH_TABLE_LEN >> 4) - 1));
}
for (i=0;i<dev->mc_count;i++) {
addrs=dmi->dmi_addr;
dmi=dmi->next;
if ((*addrs & 0x01) == 1) {
crc = 0xffffffff;
for (byte=0;byte<ETH_ALEN;byte++) {
for (bit = *addrs++,j=0;j<8;j++, bit>>=1) {
crc = (crc >> 1) ^ (((crc ^ bit) & 0x01) ? poly : 0);
}
}
hashcode = crc & ((1 << 9) - 1);
byte = hashcode >> 3;
bit = 1 << (hashcode & 0x07);
if (lp->shmem_length == IO_ONLY) {
u_char tmp;
outw((short)((long)lp->mctbl) + byte, EWRK3_PIR1);
tmp = inb(EWRK3_DATA);
tmp |= bit;
outw((short)((long)lp->mctbl) + byte, EWRK3_PIR1);
outb(tmp, EWRK3_DATA);
} else {
writeb(readb(lp->mctbl + byte) | bit, lp->mctbl + byte);
}
}
}
}
lp->lock = 0;
return;
}
static void isa_probe(struct device *dev, u_long ioaddr)
{
int i = num_ewrk3s, maxSlots;
u_long iobase;
if (!ioaddr && autoprobed) return ;
if (ioaddr >= 0x400) return;
if (ioaddr == 0) {
iobase = EWRK3_IO_BASE;
maxSlots = 24;
} else {
iobase = ioaddr;
maxSlots = i + 1;
}
for (; (i<maxSlots) && (dev!=NULL);iobase+=EWRK3_IOP_INC, i++) {
if (!check_region(iobase, EWRK3_TOTAL_SIZE)) {
if (DevicePresent(iobase) == 0) {
if ((dev = alloc_device(dev, iobase)) != NULL) {
if (ewrk3_hw_init(dev, iobase) == 0) {
num_ewrk3s++;
}
num_eth++;
}
}
} else if (autoprobed) {
printk("%s: region already allocated at 0x%04lx.\n", dev->name, iobase);
}
}
return;
}
static void eisa_probe(struct device *dev, u_long ioaddr)
{
int i, maxSlots;
u_long iobase;
char name[EWRK3_STRLEN];
if (!ioaddr && autoprobed) return ;
if (ioaddr < 0x1000) return;
if (ioaddr == 0) {
iobase = EISA_SLOT_INC;
i = 1;
maxSlots = MAX_EISA_SLOTS;
} else {
iobase = ioaddr;
i = (ioaddr >> 12);
maxSlots = i + 1;
}
for (i=1; (i<maxSlots) && (dev!=NULL); i++, iobase+=EISA_SLOT_INC) {
if (EISA_signature(name, EISA_ID) == 0) {
if (!check_region(iobase, EWRK3_TOTAL_SIZE)) {
if (DevicePresent(iobase) == 0) {
if ((dev = alloc_device(dev, iobase)) != NULL) {
if (ewrk3_hw_init(dev, iobase) == 0) {
num_ewrk3s++;
}
num_eth++;
}
}
} else if (autoprobed) {
printk("%s: region already allocated at 0x%04lx.\n", dev->name, iobase);
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
num_eth = ewrk3_dev_index(dev->name);
if (loading_module) return dev;
while (1) {
if (((dev->base_addr == EWRK3_NDA) || (dev->base_addr==0)) && !adev) {
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
num_eth = ewrk3_dev_index(dev->name);
new_dev = 0;
}
if (((dev->next == NULL) &&
((dev->base_addr != EWRK3_NDA) && (dev->base_addr != 0)) && !fixed) ||
new_dev) {
num_eth++;
dev = insert_device(dev, iobase, ewrk3_probe);
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
ewrk3_dev_index(char *s)
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
static int Read_EEPROM(u_long iobase, u_char eaddr)
{
int i;
outb((eaddr & 0x3f), EWRK3_PIR1);
outb(EEPROM_RD, EWRK3_IOPR);
for (i=0;i<5000;i++) inb(EWRK3_CSR);
return inw(EWRK3_EPROM1);
}
static int Write_EEPROM(short data, u_long iobase, u_char eaddr)
{
int i;
outb(EEPROM_WR_EN, EWRK3_IOPR);
for (i=0;i<5000;i++) inb(EWRK3_CSR);
outw(data, EWRK3_EPROM1);
outb((eaddr & 0x3f), EWRK3_PIR1);
outb(EEPROM_WR, EWRK3_IOPR);
for (i=0;i<75000;i++) inb(EWRK3_CSR);
outb(EEPROM_WR_DIS, EWRK3_IOPR);
for (i=0;i<5000;i++) inb(EWRK3_CSR);
return 0;
}
static void EthwrkSignature(char *name, char *eeprom_image)
{
u_long i,j,k;
char *signatures[] = EWRK3_SIGNATURE;
strcpy(name, "");
for (i=0;*signatures[i] != '\0' && *name == '\0';i++) {
for (j=EEPROM_PNAME7,k=0;j<=EEPROM_PNAME0 && k<strlen(signatures[i]);j++) {
if (signatures[i][k] == eeprom_image[j]) {
k++;
} else {
k=0;
}
}
if (k == strlen(signatures[i])) {
for (k=0; k<EWRK3_STRLEN; k++) {
name[k] = eeprom_image[EEPROM_PNAME7 + k];
name[EWRK3_STRLEN] = '\0';
}
}
}
return;
}
static int DevicePresent(u_long iobase)
{
union {
struct {
u32 a;
u32 b;
} llsig;
char Sig[sizeof(u32) << 1];
} dev;
short sigLength;
char data;
int i, j, status = 0;
dev.llsig.a = ETH_PROM_SIG;
dev.llsig.b = ETH_PROM_SIG;
sigLength = sizeof(u32) << 1;
for (i=0,j=0;j<sigLength && i<PROBE_LENGTH+sigLength-1;i++) {
data = inb(EWRK3_APROM);
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
static u_char get_hw_addr(struct device *dev, u_char *eeprom_image, char chipType)
{
int i, j, k;
u_short chksum;
u_char crc, lfsr, sd, status = 0;
u_long iobase = dev->base_addr;
u16 tmp;
if (chipType == LeMAC2) {
for (crc=0x6a, j=0; j<ETH_ALEN; j++) {
sd = dev->dev_addr[j] = eeprom_image[EEPROM_PADDR0 + j];
outb(dev->dev_addr[j], EWRK3_PAR0 + j);
for (k=0; k<8; k++, sd >>= 1) {
lfsr = ((((crc & 0x02) >> 1) ^ (crc & 0x01)) ^ (sd & 0x01)) << 7;
crc = (crc >> 1) + lfsr;
}
}
if (crc != eeprom_image[EEPROM_PA_CRC]) status = -1;
} else {
for (i=0,k=0;i<ETH_ALEN;) {
k <<= 1 ;
if (k > 0xffff) k-=0xffff;
k += (u_char) (tmp = inb(EWRK3_APROM));
dev->dev_addr[i] = (u_char) tmp;
outb(dev->dev_addr[i], EWRK3_PAR0 + i);
i++;
k += (u_short) ((tmp = inb(EWRK3_APROM)) << 8);
dev->dev_addr[i] = (u_char) tmp;
outb(dev->dev_addr[i], EWRK3_PAR0 + i);
i++;
if (k > 0xffff) k-=0xffff;
}
if (k == 0xffff) k=0;
chksum = inb(EWRK3_APROM);
chksum |= (inb(EWRK3_APROM)<<8);
if (k != chksum) status = -1;
}
return status;
}
static int EISA_signature(char *name, s32 eisa_id)
{
u_long i;
char *signatures[] = EWRK3_SIGNATURE;
char ManCode[EWRK3_STRLEN];
union {
s32 ID;
char Id[4];
} Eisa;
int status = 0;
*name = '\0';
for (i=0; i<4; i++) {
Eisa.Id[i] = inb(eisa_id + i);
}
ManCode[0]=(((Eisa.Id[0]>>2)&0x1f)+0x40);
ManCode[1]=(((Eisa.Id[1]&0xe0)>>5)+((Eisa.Id[0]&0x03)<<3)+0x40);
ManCode[2]=(((Eisa.Id[2]>>4)&0x0f)+0x30);
ManCode[3]=((Eisa.Id[2]&0x0f)+0x30);
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
static int ewrk3_ioctl(struct device *dev, struct ifreq *rq, int cmd)
{
struct ewrk3_private *lp = (struct ewrk3_private *)dev->priv;
struct ewrk3_ioctl *ioc = (struct ewrk3_ioctl *) &rq->ifr_data;
u_long iobase = dev->base_addr;
int i, j, status = 0;
u_char csr;
union {
u_char addr[HASH_TABLE_LEN * ETH_ALEN];
u_short val[(HASH_TABLE_LEN * ETH_ALEN) >> 1];
} tmp;
switch(ioc->cmd) {
case EWRK3_GET_HWADDR:
for (i=0; i<ETH_ALEN; i++) {
tmp.addr[i] = dev->dev_addr[i];
}
ioc->len = ETH_ALEN;
if (!(status = verify_area(VERIFY_WRITE, (void *)ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, tmp.addr, ioc->len);
}
break;
case EWRK3_SET_HWADDR:
if (suser()) {
if (!(status = verify_area(VERIFY_READ, (void *)ioc->data, ETH_ALEN))) {
csr = inb(EWRK3_CSR);
csr |= (CSR_TXD|CSR_RXD);
outb(csr, EWRK3_CSR);
memcpy_fromfs(tmp.addr,ioc->data,ETH_ALEN);
for (i=0; i<ETH_ALEN; i++) {
dev->dev_addr[i] = tmp.addr[i];
outb(tmp.addr[i], EWRK3_PAR0 + i);
}
csr &= ~(CSR_TXD|CSR_RXD);
outb(csr, EWRK3_CSR);
}
} else {
status = -EPERM;
}
break;
case EWRK3_SET_PROM:
if (suser()) {
csr = inb(EWRK3_CSR);
csr |= CSR_PME;
csr &= ~CSR_MCE;
outb(csr, EWRK3_CSR);
} else {
status = -EPERM;
}
break;
case EWRK3_CLR_PROM:
if (suser()) {
csr = inb(EWRK3_CSR);
csr &= ~CSR_PME;
outb(csr, EWRK3_CSR);
} else {
status = -EPERM;
}
break;
case EWRK3_SAY_BOO:
printk("%s: Boo!\n", dev->name);
break;
case EWRK3_GET_MCA:
if (!(status = verify_area(VERIFY_WRITE, ioc->data, ioc->len))) {
while (set_bit(0, (void *)&lp->lock) != 0);
if (lp->shmem_length == IO_ONLY) {
outb(0, EWRK3_IOPR);
outw(PAGE0_HTE, EWRK3_PIR1);
for (i=0; i<(HASH_TABLE_LEN >> 3); i++) {
tmp.addr[i] = inb(EWRK3_DATA);
}
} else {
outb(0, EWRK3_MPR);
memcpy_fromio(tmp.addr, (char *)(lp->shmem_base + PAGE0_HTE), (HASH_TABLE_LEN >> 3));
}
ioc->len = (HASH_TABLE_LEN >> 3);
memcpy_tofs(ioc->data, tmp.addr, ioc->len);
}
lp->lock = 0;
break;
case EWRK3_SET_MCA:
if (suser()) {
if (!(status=verify_area(VERIFY_READ, ioc->data, ETH_ALEN*ioc->len))) {
memcpy_fromfs(tmp.addr, ioc->data, ETH_ALEN * ioc->len);
set_multicast_list(dev);
}
} else {
status = -EPERM;
}
break;
case EWRK3_CLR_MCA:
if (suser()) {
set_multicast_list(dev);
} else {
status = -EPERM;
}
break;
case EWRK3_MCA_EN:
if (suser()) {
csr = inb(EWRK3_CSR);
csr |= CSR_MCE;
csr &= ~CSR_PME;
outb(csr, EWRK3_CSR);
} else {
status = -EPERM;
}
break;
case EWRK3_GET_STATS:
cli();
ioc->len = sizeof(lp->pktStats);
if (!(status=verify_area(VERIFY_WRITE, ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, &lp->pktStats, ioc->len);
}
sti();
break;
case EWRK3_CLR_STATS:
if (suser()) {
cli();
memset(&lp->pktStats, 0, sizeof(lp->pktStats));
sti();
} else {
status = -EPERM;
}
break;
case EWRK3_GET_CSR:
tmp.addr[0] = inb(EWRK3_CSR);
ioc->len = 1;
if (!(status=verify_area(VERIFY_WRITE, ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, tmp.addr, ioc->len);
}
break;
case EWRK3_SET_CSR:
if (suser()) {
if (!(status=verify_area(VERIFY_READ, ioc->data, 1))) {
memcpy_fromfs(tmp.addr, ioc->data, 1);
outb(tmp.addr[0], EWRK3_CSR);
}
} else {
status = -EPERM;
}
break;
case EWRK3_GET_EEPROM:
if (suser()) {
for (i=0; i<(EEPROM_MAX>>1); i++) {
tmp.val[i] = (short)Read_EEPROM(iobase, i);
}
i = EEPROM_MAX;
tmp.addr[i++] = inb(EWRK3_CMR);
for (j=0;j<ETH_ALEN;j++) {
tmp.addr[i++] = inb(EWRK3_PAR0 + j);
}
ioc->len = EEPROM_MAX + 1 + ETH_ALEN;
if (!(status=verify_area(VERIFY_WRITE, ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, tmp.addr, ioc->len);
}
} else {
status = -EPERM;
}
break;
case EWRK3_SET_EEPROM:
if (suser()) {
if (!(status=verify_area(VERIFY_READ, ioc->data, EEPROM_MAX))) {
memcpy_fromfs(tmp.addr, ioc->data, EEPROM_MAX);
for (i=0; i<(EEPROM_MAX>>1); i++) {
Write_EEPROM(tmp.val[i], iobase, i);
}
}
} else {
status = -EPERM;
}
break;
case EWRK3_GET_CMR:
tmp.addr[0] = inb(EWRK3_CMR);
ioc->len = 1;
if (!(status=verify_area(VERIFY_WRITE, ioc->data, ioc->len))) {
memcpy_tofs(ioc->data, tmp.addr, ioc->len);
}
break;
case EWRK3_SET_TX_CUT_THRU:
if (suser()) {
lp->txc = 1;
} else {
status = -EPERM;
}
break;
case EWRK3_CLR_TX_CUT_THRU:
if (suser()) {
lp->txc = 0;
} else {
status = -EPERM;
}
break;
default:
status = -EOPNOTSUPP;
}
return status;
}
#ifdef MODULE
static char devicename[9] = { 0, };
static struct device thisEthwrk = {
devicename,
0, 0, 0, 0,
0x300, 5,
0, 0, 0, NULL, ewrk3_probe };
static int io=0x300;
static int irq=5;
int
init_module(void)
{
thisEthwrk.base_addr=io;
thisEthwrk.irq=irq;
if (register_netdev(&thisEthwrk) != 0)
return -EIO;
return 0;
}
void
cleanup_module(void)
{
if (thisEthwrk.priv) {
kfree(thisEthwrk.priv);
thisEthwrk.priv = NULL;
}
thisEthwrk.irq = 0;
unregister_netdev(&thisEthwrk);
release_region(thisEthwrk.base_addr, EWRK3_TOTAL_SIZE);
}
#endif