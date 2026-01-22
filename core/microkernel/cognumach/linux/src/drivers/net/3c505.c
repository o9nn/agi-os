#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/errno.h>
#include <linux/in.h>
#include <linux/malloc.h>
#include <linux/ioport.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include "3c505.h"
#define ELP_DMA      6
#define ELP_RX_PCBS  4
static const char *filename = __FILE__;
static const char *timeout_msg = "*** timeout at %s:%s (line %d) ***\n";
#define TIMEOUT_MSG(lineno) \
printk(timeout_msg, filename,__FUNCTION__,(lineno))
static const char *invalid_pcb_msg =
"*** invalid pcb length %d at %s:%s (line %d) ***\n";
#define INVALID_PCB_MSG(len) \
printk(invalid_pcb_msg, (len),filename,__FUNCTION__,__LINE__)
static const char *search_msg = "%s: Looking for 3c505 adapter at address %#x...";
static const char *stilllooking_msg = "still looking...";
static const char *found_msg = "found.\n";
static const char *notfound_msg = "not found (reason = %d)\n";
static const char *couldnot_msg = "%s: 3c505 not found\n";
#ifdef ELP_DEBUG
static const int elp_debug = ELP_DEBUG;
#else
static const int elp_debug = 0;
#endif
#ifndef	TRUE
#define	TRUE	1
#endif
#ifndef	FALSE
#define	FALSE	0
#endif
const int addr_list[] = {0x300, 0x280, 0x310, 0};
static inline int __get_order(unsigned long size)
{
int order;
size = (size - 1) >> (PAGE_SHIFT - 1);
order = -1;
do {
size >>= 1;
order++;
} while (size);
return order;
}
static unsigned long dma_mem_alloc(int size)
{
int order = __get_order(size);
return __get_dma_pages(GFP_KERNEL, order);
}
static inline unsigned char inb_status(unsigned int base_addr)
{
return inb(base_addr + PORT_STATUS);
}
static inline unsigned char inb_control(unsigned int base_addr)
{
return inb(base_addr + PORT_CONTROL);
}
static inline int inb_command(unsigned int base_addr)
{
return inb(base_addr + PORT_COMMAND);
}
static inline void outb_control(unsigned char val, unsigned int base_addr)
{
outb(val, base_addr + PORT_CONTROL);
}
static inline void outb_command(unsigned char val, unsigned int base_addr)
{
outb(val, base_addr + PORT_COMMAND);
}
static inline unsigned int inw_data(unsigned int base_addr)
{
return inw(base_addr + PORT_DATA);
}
static inline void outw_data(unsigned int val, unsigned int base_addr)
{
outw(val, base_addr + PORT_DATA);
}
#define DMA_BUFFER_SIZE  1600
#define BACKLOG_SIZE      4
typedef struct {
volatile short got[NUM_TRANSMIT_CMDS];
pcb_struct tx_pcb;
pcb_struct rx_pcb;
pcb_struct itx_pcb;
pcb_struct irx_pcb;
struct enet_statistics stats;
void *dma_buffer;
struct {
unsigned int length[BACKLOG_SIZE];
unsigned int in;
unsigned int out;
} rx_backlog;
struct {
unsigned int direction;
unsigned int length;
unsigned int copy_flag;
struct sk_buff *skb;
long int start_time;
} current_dma;
unsigned long send_pcb_semaphore;
unsigned int dmaing;
unsigned long busy;
unsigned int rx_active;
} elp_device;
static inline unsigned int backlog_next(unsigned int n)
{
return (n + 1) % BACKLOG_SIZE;
}
#define	GET_ASF(addr) \
(get_status(addr)&ASF_PCB_MASK)
static inline int get_status(unsigned int base_addr)
{
int timeout = jiffies + 10;
register int stat1;
do {
stat1 = inb_status(base_addr);
} while (stat1 != inb_status(base_addr) && jiffies < timeout);
if (jiffies >= timeout)
TIMEOUT_MSG(__LINE__);
return stat1;
}
static inline void set_hsf(unsigned int base_addr, int hsf)
{
cli();
outb_control((inb_control(base_addr) & ~HSF_PCB_MASK) | hsf, base_addr);
sti();
}
static int start_receive(struct device *, pcb_struct *);
inline static void adapter_reset(struct device *dev)
{
int timeout;
unsigned char orig_hcr = inb_control(dev->base_addr);
elp_device *adapter = dev->priv;
outb_control(0, dev->base_addr);
if (inb_status(dev->base_addr) & ACRF) {
do {
inb_command(dev->base_addr);
timeout = jiffies + 2;
while ((jiffies <= timeout) && !(inb_status(dev->base_addr) & ACRF));
} while (inb_status(dev->base_addr) & ACRF);
set_hsf(dev->base_addr, HSF_PCB_NAK);
}
outb_control(inb_control(dev->base_addr) | ATTN | DIR, dev->base_addr);
timeout = jiffies + 1;
while (jiffies <= timeout);
outb_control(inb_control(dev->base_addr) & ~ATTN, dev->base_addr);
timeout = jiffies + 1;
while (jiffies <= timeout);
outb_control(inb_control(dev->base_addr) | FLSH, dev->base_addr);
timeout = jiffies + 1;
while (jiffies <= timeout);
outb_control(inb_control(dev->base_addr) & ~FLSH, dev->base_addr);
timeout = jiffies + 1;
while (jiffies <= timeout);
outb_control(orig_hcr, dev->base_addr);
if (!start_receive(dev, &adapter->tx_pcb))
printk("%s: start receive command failed \n", dev->name);
}
static inline void check_dma(struct device *dev)
{
elp_device *adapter = dev->priv;
if (adapter->dmaing && (jiffies > (adapter->current_dma.start_time + 10))) {
unsigned long flags;
printk("%s: DMA %s timed out, %d bytes left\n", dev->name, adapter->current_dma.direction ? "download" : "upload", get_dma_residue(dev->dma));
save_flags(flags);
cli();
adapter->dmaing = 0;
adapter->busy = 0;
disable_dma(dev->dma);
if (adapter->rx_active)
adapter->rx_active--;
outb_control(inb_control(dev->base_addr) & ~(DMAE | TCEN | DIR), dev->base_addr);
restore_flags(flags);
}
}
static inline unsigned int send_pcb_slow(unsigned int base_addr, unsigned char byte)
{
unsigned int timeout;
outb_command(byte, base_addr);
for (timeout = jiffies + 5; jiffies < timeout;) {
if (inb_status(base_addr) & HCRE)
return FALSE;
}
printk("3c505: send_pcb_slow timed out\n");
return TRUE;
}
static inline unsigned int send_pcb_fast(unsigned int base_addr, unsigned char byte)
{
unsigned int timeout;
outb_command(byte, base_addr);
for (timeout = 0; timeout < 40000; timeout++) {
if (inb_status(base_addr) & HCRE)
return FALSE;
}
printk("3c505: send_pcb_fast timed out\n");
return TRUE;
}
static inline void prime_rx(struct device *dev)
{
elp_device *adapter = dev->priv;
while (adapter->rx_active < ELP_RX_PCBS && dev->start) {
if (!start_receive(dev, &adapter->itx_pcb))
break;
}
}
static int send_pcb(struct device *dev, pcb_struct * pcb)
{
int i;
int timeout;
elp_device *adapter = dev->priv;
check_dma(dev);
if (adapter->dmaing && adapter->current_dma.direction == 0)
return FALSE;
if (set_bit(1, &adapter->send_pcb_semaphore)) {
if (elp_debug >= 3) {
printk("%s: send_pcb entered while threaded\n", dev->name);
}
return FALSE;
}
set_hsf(dev->base_addr, 0);
if (send_pcb_slow(dev->base_addr, pcb->command))
goto abort;
cli();
if (send_pcb_fast(dev->base_addr, pcb->length))
goto sti_abort;
for (i = 0; i < pcb->length; i++) {
if (send_pcb_fast(dev->base_addr, pcb->data.raw[i]))
goto sti_abort;
}
outb_control(inb_control(dev->base_addr) | 3, dev->base_addr);
outb_command(2 + pcb->length, dev->base_addr);
sti();
for (timeout = jiffies + 5; jiffies < timeout;) {
switch (GET_ASF(dev->base_addr)) {
case ASF_PCB_ACK:
adapter->send_pcb_semaphore = 0;
return TRUE;
break;
case ASF_PCB_NAK:
printk("%s: send_pcb got NAK\n", dev->name);
goto abort;
break;
}
}
if (elp_debug >= 1)
printk("%s: timeout waiting for PCB acknowledge (status %02x)\n", dev->name, inb_status(dev->base_addr));
sti_abort:
sti();
abort:
adapter->send_pcb_semaphore = 0;
return FALSE;
}
static int receive_pcb(struct device *dev, pcb_struct * pcb)
{
int i, j;
int total_length;
int stat;
int timeout;
elp_device *adapter = dev->priv;
set_hsf(dev->base_addr, 0);
timeout = jiffies + 2;
while (((stat = get_status(dev->base_addr)) & ACRF) == 0 && jiffies < timeout);
if (jiffies >= timeout) {
TIMEOUT_MSG(__LINE__);
return FALSE;
}
pcb->command = inb_command(dev->base_addr);
timeout = jiffies + 3;
while (((stat = get_status(dev->base_addr)) & ACRF) == 0 && jiffies < timeout);
if (jiffies >= timeout) {
TIMEOUT_MSG(__LINE__);
printk("%s: status %02x\n", dev->name, stat);
return FALSE;
}
pcb->length = inb_command(dev->base_addr);
if (pcb->length > MAX_PCB_DATA) {
INVALID_PCB_MSG(pcb->length);
adapter_reset(dev);
return FALSE;
}
cli();
i = 0;
do {
j = 0;
while (((stat = get_status(dev->base_addr)) & ACRF) == 0 && j++ < 20000);
pcb->data.raw[i++] = inb_command(dev->base_addr);
if (i > MAX_PCB_DATA)
INVALID_PCB_MSG(i);
} while ((stat & ASF_PCB_MASK) != ASF_PCB_END && j < 20000);
sti();
if (j >= 20000) {
TIMEOUT_MSG(__LINE__);
return FALSE;
}
total_length = pcb->data.raw[--i];
if (total_length != (pcb->length + 2)) {
if (elp_debug >= 2)
printk("%s: mangled PCB received\n", dev->name);
set_hsf(dev->base_addr, HSF_PCB_NAK);
return FALSE;
}
if (pcb->command == CMD_RECEIVE_PACKET_COMPLETE) {
if (set_bit(0, (void *) &adapter->busy)) {
if (backlog_next(adapter->rx_backlog.in) == adapter->rx_backlog.out) {
set_hsf(dev->base_addr, HSF_PCB_NAK);
printk("%s: PCB rejected, transfer in progress and backlog full\n", dev->name);
pcb->command = 0;
return TRUE;
} else {
pcb->command = 0xff;
}
}
}
set_hsf(dev->base_addr, HSF_PCB_ACK);
return TRUE;
}
static int start_receive(struct device *dev, pcb_struct * tx_pcb)
{
int status;
elp_device *adapter = dev->priv;
if (elp_debug >= 3)
printk("%s: restarting receiver\n", dev->name);
tx_pcb->command = CMD_RECEIVE_PACKET;
tx_pcb->length = sizeof(struct Rcv_pkt);
tx_pcb->data.rcv_pkt.buf_seg
= tx_pcb->data.rcv_pkt.buf_ofs = 0;
tx_pcb->data.rcv_pkt.buf_len = 1600;
tx_pcb->data.rcv_pkt.timeout = 0;
status = send_pcb(dev, tx_pcb);
if (status)
adapter->rx_active++;
return status;
}
static void receive_packet(struct device *dev, int len)
{
int rlen;
elp_device *adapter = dev->priv;
unsigned long target;
struct sk_buff *skb;
rlen = (len + 1) & ~1;
skb = dev_alloc_skb(rlen + 2);
adapter->current_dma.copy_flag = 0;
if (!skb) {
printk("%s: memory squeeze, dropping packet\n", dev->name);
target = virt_to_bus(adapter->dma_buffer);
} else {
skb_reserve(skb, 2);
target = virt_to_bus(skb_put(skb, rlen));
if ((target + rlen) >= MAX_DMA_ADDRESS) {
target = virt_to_bus(adapter->dma_buffer);
adapter->current_dma.copy_flag = 1;
}
}
if (set_bit(0, (void *) &adapter->dmaing))
printk("%s: rx blocked, DMA in progress, dir %d\n", dev->name, adapter->current_dma.direction);
adapter->current_dma.direction = 0;
adapter->current_dma.length = rlen;
adapter->current_dma.skb = skb;
adapter->current_dma.start_time = jiffies;
outb_control(inb_control(dev->base_addr) | DIR | TCEN | DMAE, dev->base_addr);
disable_dma(dev->dma);
clear_dma_ff(dev->dma);
set_dma_mode(dev->dma, 0x04);
set_dma_addr(dev->dma, target);
set_dma_count(dev->dma, rlen);
enable_dma(dev->dma);
if (elp_debug >= 3) {
printk("%s: rx DMA transfer started\n", dev->name);
}
if (adapter->rx_active)
adapter->rx_active--;
if (!adapter->busy)
printk("%s: receive_packet called, busy not set.\n", dev->name);
}
static void elp_interrupt(int irq, void *dev_id, struct pt_regs *reg_ptr)
{
int len;
int dlen;
int icount = 0;
struct device *dev;
elp_device *adapter;
int timeout;
if (irq < 0 || irq > 15) {
printk("elp_interrupt(): illegal IRQ number found in interrupt routine (%i)\n", irq);
return;
}
dev = irq2dev_map[irq];
if (dev == NULL) {
printk("elp_interrupt(): irq %d for unknown device.\n", irq);
return;
}
adapter = (elp_device *) dev->priv;
if (dev->interrupt) {
printk("%s: re-entering the interrupt handler!\n", dev->name);
return;
}
dev->interrupt = 1;
do {
if (inb_status(dev->base_addr) & DONE) {
if (!adapter->dmaing) {
printk("%s: phantom DMA completed\n", dev->name);
}
if (elp_debug >= 3) {
printk("%s: %s DMA complete, status %02x\n", dev->name, adapter->current_dma.direction ? "tx" : "rx", inb_status(dev->base_addr));
}
outb_control(inb_control(dev->base_addr) & ~(DMAE | TCEN | DIR), dev->base_addr);
if (adapter->current_dma.direction) {
dev_kfree_skb(adapter->current_dma.skb, FREE_WRITE);
} else {
struct sk_buff *skb = adapter->current_dma.skb;
if (skb) {
skb->dev = dev;
if (adapter->current_dma.copy_flag) {
memcpy(skb_put(skb, adapter->current_dma.length), adapter->dma_buffer, adapter->current_dma.length);
}
skb->protocol = eth_type_trans(skb,dev);
netif_rx(skb);
}
}
adapter->dmaing = 0;
if (adapter->rx_backlog.in != adapter->rx_backlog.out) {
int t = adapter->rx_backlog.length[adapter->rx_backlog.out];
adapter->rx_backlog.out = backlog_next(adapter->rx_backlog.out);
if (elp_debug >= 2)
printk("%s: receiving backlogged packet (%d)\n", dev->name, t);
receive_packet(dev, t);
} else {
adapter->busy = 0;
}
} else {
check_dma(dev);
}
sti();
timeout = jiffies + 3;
while ((inb_status(dev->base_addr) & ACRF) != 0 && jiffies < timeout) {
if (receive_pcb(dev, &adapter->irx_pcb)) {
switch (adapter->irx_pcb.command) {
case 0:
break;
case 0xff:
case CMD_RECEIVE_PACKET_COMPLETE:
if (dev->start == 0)
break;
cli();
len = adapter->irx_pcb.data.rcv_resp.pkt_len;
dlen = adapter->irx_pcb.data.rcv_resp.buf_len;
if (adapter->irx_pcb.data.rcv_resp.timeout != 0) {
printk("%s: interrupt - packet not received correctly\n", dev->name);
sti();
} else {
if (elp_debug >= 3) {
sti();
printk("%s: interrupt - packet received of length %i (%i)\n", dev->name, len, dlen);
cli();
}
if (adapter->irx_pcb.command == 0xff) {
if (elp_debug >= 2)
printk("%s: adding packet to backlog (len = %d)\n", dev->name, dlen);
adapter->rx_backlog.length[adapter->rx_backlog.in] = dlen;
adapter->rx_backlog.in = backlog_next(adapter->rx_backlog.in);
} else {
receive_packet(dev, dlen);
}
sti();
if (elp_debug >= 3)
printk("%s: packet received\n", dev->name);
}
break;
case CMD_CONFIGURE_82586_RESPONSE:
adapter->got[CMD_CONFIGURE_82586] = 1;
if (elp_debug >= 3)
printk("%s: interrupt - configure response received\n", dev->name);
break;
case CMD_CONFIGURE_ADAPTER_RESPONSE:
adapter->got[CMD_CONFIGURE_ADAPTER_MEMORY] = 1;
if (elp_debug >= 3)
printk("%s: Adapter memory configuration %s.\n", dev->name,
adapter->irx_pcb.data.failed ? "failed" : "succeeded");
break;
case CMD_LOAD_MULTICAST_RESPONSE:
adapter->got[CMD_LOAD_MULTICAST_LIST] = 1;
if (elp_debug >= 3)
printk("%s: Multicast address list loading %s.\n", dev->name,
adapter->irx_pcb.data.failed ? "failed" : "succeeded");
break;
case CMD_SET_ADDRESS_RESPONSE:
adapter->got[CMD_SET_STATION_ADDRESS] = 1;
if (elp_debug >= 3)
printk("%s: Ethernet address setting %s.\n", dev->name,
adapter->irx_pcb.data.failed ? "failed" : "succeeded");
break;
case CMD_NETWORK_STATISTICS_RESPONSE:
adapter->stats.rx_packets += adapter->irx_pcb.data.netstat.tot_recv;
adapter->stats.tx_packets += adapter->irx_pcb.data.netstat.tot_xmit;
adapter->stats.rx_crc_errors += adapter->irx_pcb.data.netstat.err_CRC;
adapter->stats.rx_frame_errors += adapter->irx_pcb.data.netstat.err_align;
adapter->stats.rx_fifo_errors += adapter->irx_pcb.data.netstat.err_ovrrun;
adapter->stats.rx_over_errors += adapter->irx_pcb.data.netstat.err_res;
adapter->got[CMD_NETWORK_STATISTICS] = 1;
if (elp_debug >= 3)
printk("%s: interrupt - statistics response received\n", dev->name);
break;
case CMD_TRANSMIT_PACKET_COMPLETE:
if (elp_debug >= 3)
printk("%s: interrupt - packet sent\n", dev->name);
if (dev->start == 0)
break;
switch (adapter->irx_pcb.data.xmit_resp.c_stat) {
case 0xffff:
adapter->stats.tx_aborted_errors++;
printk(KERN_INFO "%s: transmit timed out, network cable problem?\n", dev->name);
break;
case 0xfffe:
adapter->stats.tx_fifo_errors++;
printk(KERN_INFO "%s: transmit timed out, FIFO underrun\n", dev->name);
break;
}
dev->tbusy = 0;
mark_bh(NET_BH);
break;
default:
printk(KERN_DEBUG "%s: unknown PCB received - %2.2x\n", dev->name, adapter->irx_pcb.command);
break;
}
} else {
printk("%s: failed to read PCB on interrupt\n", dev->name);
adapter_reset(dev);
}
}
} while (icount++ < 5 && (inb_status(dev->base_addr) & (ACRF | DONE)));
prime_rx(dev);
dev->interrupt = 0;
}
static int elp_open(struct device *dev)
{
elp_device *adapter;
adapter = dev->priv;
if (elp_debug >= 3)
printk("%s: request to open device\n", dev->name);
if (adapter == NULL) {
printk("%s: Opening a non-existent physical device\n", dev->name);
return -EAGAIN;
}
outb_control(0x00, dev->base_addr);
inb_command(dev->base_addr);
adapter_reset(dev);
dev->interrupt = 0;
dev->tbusy = 0;
adapter->rx_active = 0;
adapter->busy = 0;
adapter->send_pcb_semaphore = 0;
adapter->rx_backlog.in = 0;
adapter->rx_backlog.out = 0;
irq2dev_map[dev->irq] = dev;
if (request_irq(dev->irq, &elp_interrupt, 0, "3c505", NULL)) {
irq2dev_map[dev->irq] = NULL;
return -EAGAIN;
}
if (request_dma(dev->dma, "3c505")) {
printk("%s: could not allocate DMA channel\n", dev->name);
return -EAGAIN;
}
adapter->dma_buffer = (void *) dma_mem_alloc(DMA_BUFFER_SIZE);
if (!adapter->dma_buffer) {
printk("Could not allocate DMA buffer\n");
}
adapter->dmaing = 0;
outb_control(CMDE, dev->base_addr);
dev->start = 1;
if (elp_debug >= 3)
printk("%s: sending 3c505 memory configuration command\n", dev->name);
adapter->tx_pcb.command = CMD_CONFIGURE_ADAPTER_MEMORY;
adapter->tx_pcb.data.memconf.cmd_q = 10;
adapter->tx_pcb.data.memconf.rcv_q = 20;
adapter->tx_pcb.data.memconf.mcast = 10;
adapter->tx_pcb.data.memconf.frame = 20;
adapter->tx_pcb.data.memconf.rcv_b = 20;
adapter->tx_pcb.data.memconf.progs = 0;
adapter->tx_pcb.length = sizeof(struct Memconf);
adapter->got[CMD_CONFIGURE_ADAPTER_MEMORY] = 0;
if (!send_pcb(dev, &adapter->tx_pcb))
printk("%s: couldn't send memory configuration command\n", dev->name);
else {
int timeout = jiffies + TIMEOUT;
while (adapter->got[CMD_CONFIGURE_ADAPTER_MEMORY] == 0 && jiffies < timeout);
if (jiffies >= timeout)
TIMEOUT_MSG(__LINE__);
}
if (elp_debug >= 3)
printk("%s: sending 82586 configure command\n", dev->name);
adapter->tx_pcb.command = CMD_CONFIGURE_82586;
adapter->tx_pcb.data.configure = NO_LOOPBACK | RECV_BROAD;
adapter->tx_pcb.length = 2;
adapter->got[CMD_CONFIGURE_82586] = 0;
if (!send_pcb(dev, &adapter->tx_pcb))
printk("%s: couldn't send 82586 configure command\n", dev->name);
else {
int timeout = jiffies + TIMEOUT;
while (adapter->got[CMD_CONFIGURE_82586] == 0 && jiffies < timeout);
if (jiffies >= timeout)
TIMEOUT_MSG(__LINE__);
}
outb(0x1, dev->base_addr + PORT_AUXDMA);
prime_rx(dev);
if (elp_debug >= 3)
printk("%s: %d receive PCBs active\n", dev->name, adapter->rx_active);
MOD_INC_USE_COUNT;
return 0;
}
static int send_packet(struct device *dev, struct sk_buff *skb)
{
elp_device *adapter = dev->priv;
unsigned long target;
unsigned int nlen = (((skb->len < 60) ? 60 : skb->len) + 1) & (~1);
if (set_bit(0, (void *) &adapter->busy)) {
if (elp_debug >= 2)
printk("%s: transmit blocked\n", dev->name);
return FALSE;
}
adapter = dev->priv;
adapter->tx_pcb.command = CMD_TRANSMIT_PACKET;
adapter->tx_pcb.length = sizeof(struct Xmit_pkt);
adapter->tx_pcb.data.xmit_pkt.buf_ofs
= adapter->tx_pcb.data.xmit_pkt.buf_seg = 0;
adapter->tx_pcb.data.xmit_pkt.pkt_len = nlen;
if (!send_pcb(dev, &adapter->tx_pcb)) {
adapter->busy = 0;
return FALSE;
}
if (set_bit(0, (void *) &adapter->dmaing))
printk("%s: tx: DMA %d in progress\n", dev->name, adapter->current_dma.direction);
adapter->current_dma.direction = 1;
adapter->current_dma.start_time = jiffies;
target = virt_to_bus(skb->data);
if ((target + nlen) >= MAX_DMA_ADDRESS) {
memcpy(adapter->dma_buffer, skb->data, nlen);
target = virt_to_bus(adapter->dma_buffer);
}
adapter->current_dma.skb = skb;
cli();
disable_dma(dev->dma);
clear_dma_ff(dev->dma);
set_dma_mode(dev->dma, 0x08);
set_dma_addr(dev->dma, target);
set_dma_count(dev->dma, nlen);
enable_dma(dev->dma);
outb_control(inb_control(dev->base_addr) | DMAE | TCEN, dev->base_addr);
if (elp_debug >= 3)
printk("%s: DMA transfer started\n", dev->name);
return TRUE;
}
static int elp_start_xmit(struct sk_buff *skb, struct device *dev)
{
if (dev->interrupt) {
printk("%s: start_xmit aborted (in irq)\n", dev->name);
return 1;
}
check_dma(dev);
if (dev->tbusy) {
elp_device *adapter = dev->priv;
int tickssofar = jiffies - dev->trans_start;
int stat;
if (tickssofar < 1000)
return 1;
stat = inb_status(dev->base_addr);
printk("%s: transmit timed out, lost %s?\n", dev->name, (stat & ACRF) ? "interrupt" : "command");
if (elp_debug >= 1)
printk("%s: status %#02x\n", dev->name, stat);
dev->trans_start = jiffies;
dev->tbusy = 0;
adapter->stats.tx_dropped++;
}
if (skb == NULL) {
dev_tint(dev);
return 0;
}
if (skb->len <= 0)
return 0;
if (elp_debug >= 3)
printk("%s: request to send packet of length %d\n", dev->name, (int) skb->len);
if (set_bit(0, (void *) &dev->tbusy)) {
printk("%s: transmitter access conflict\n", dev->name);
return 1;
}
if (!send_packet(dev, skb)) {
if (elp_debug >= 2) {
printk("%s: failed to transmit packet\n", dev->name);
}
dev->tbusy = 0;
return 1;
}
if (elp_debug >= 3)
printk("%s: packet of length %d sent\n", dev->name, (int) skb->len);
dev->trans_start = jiffies;
prime_rx(dev);
return 0;
}
static struct enet_statistics *elp_get_stats(struct device *dev)
{
elp_device *adapter = (elp_device *) dev->priv;
if (elp_debug >= 3)
printk("%s: request for stats\n", dev->name);
if (!dev->start)
return &adapter->stats;
adapter->tx_pcb.command = CMD_NETWORK_STATISTICS;
adapter->tx_pcb.length = 0;
adapter->got[CMD_NETWORK_STATISTICS] = 0;
if (!send_pcb(dev, &adapter->tx_pcb))
printk("%s: couldn't send get statistics command\n", dev->name);
else {
int timeout = jiffies + TIMEOUT;
while (adapter->got[CMD_NETWORK_STATISTICS] == 0 && jiffies < timeout);
if (jiffies >= timeout) {
TIMEOUT_MSG(__LINE__);
return &adapter->stats;
}
}
return &adapter->stats;
}
static int elp_close(struct device *dev)
{
elp_device *adapter;
adapter = dev->priv;
if (elp_debug >= 3)
printk("%s: request to close device\n", dev->name);
(void) elp_get_stats(dev);
outb_control(0x00, dev->base_addr);
dev->tbusy = 1;
dev->start = 0;
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = 0;
free_dma(dev->dma);
free_pages((unsigned long) adapter->dma_buffer, __get_order(DMA_BUFFER_SIZE));
MOD_DEC_USE_COUNT;
return 0;
}
static void elp_set_mc_list(struct device *dev)
{
elp_device *adapter = (elp_device *) dev->priv;
struct dev_mc_list *dmi = dev->mc_list;
int i;
if (elp_debug >= 3)
printk("%s: request to set multicast list\n", dev->name);
if (!(dev->flags & (IFF_PROMISC | IFF_ALLMULTI))) {
adapter->tx_pcb.command = CMD_LOAD_MULTICAST_LIST;
adapter->tx_pcb.length = 6 * dev->mc_count;
for (i = 0; i < dev->mc_count; i++) {
memcpy(adapter->tx_pcb.data.multicast[i], dmi->dmi_addr, 6);
dmi = dmi->next;
}
adapter->got[CMD_LOAD_MULTICAST_LIST] = 0;
if (!send_pcb(dev, &adapter->tx_pcb))
printk("%s: couldn't send set_multicast command\n", dev->name);
else {
int timeout = jiffies + TIMEOUT;
while (adapter->got[CMD_LOAD_MULTICAST_LIST] == 0 && jiffies < timeout);
if (jiffies >= timeout) {
TIMEOUT_MSG(__LINE__);
}
}
if (dev->mc_count)
adapter->tx_pcb.data.configure = NO_LOOPBACK | RECV_BROAD | RECV_MULTI;
else
adapter->tx_pcb.data.configure = NO_LOOPBACK | RECV_BROAD;
} else
adapter->tx_pcb.data.configure = NO_LOOPBACK | RECV_PROMISC;
if (elp_debug >= 3)
printk("%s: sending 82586 configure command\n", dev->name);
adapter->tx_pcb.command = CMD_CONFIGURE_82586;
adapter->tx_pcb.length = 2;
adapter->got[CMD_CONFIGURE_82586] = 0;
if (!send_pcb(dev, &adapter->tx_pcb))
printk("%s: couldn't send 82586 configure command\n", dev->name);
else {
int timeout = jiffies + TIMEOUT;
while (adapter->got[CMD_CONFIGURE_82586] == 0 && jiffies < timeout);
if (jiffies >= timeout)
TIMEOUT_MSG(__LINE__);
}
}
static void elp_init(struct device *dev)
{
elp_device *adapter = dev->priv;
dev->open = elp_open;
dev->stop = elp_close;
dev->get_stats = elp_get_stats;
dev->hard_start_xmit = elp_start_xmit;
dev->set_multicast_list = elp_set_mc_list;
ether_setup(dev);
memset(&(adapter->stats), 0, sizeof(struct enet_statistics));
dev->mem_start = dev->mem_end = dev->rmem_end = dev->rmem_start = 0;
}
static int elp_sense(struct device *dev)
{
int timeout;
int addr = dev->base_addr;
const char *name = dev->name;
long flags;
byte orig_HCR, orig_HSR;
if (check_region(addr, 0xf))
return -1;
orig_HCR = inb_control(addr);
orig_HSR = inb_status(addr);
if (elp_debug > 0)
printk(search_msg, name, addr);
if (((orig_HCR == 0xff) && (orig_HSR == 0xff)) ||
((orig_HCR & DIR) != (orig_HSR & DIR))) {
if (elp_debug > 0)
printk(notfound_msg, 1);
return -1;
}
save_flags(flags);
sti();
if (elp_debug > 0)
printk("%s", stilllooking_msg);
if (orig_HCR & DIR) {
outb_control(orig_HCR & ~DIR, addr);
timeout = jiffies + 30;
while (jiffies < timeout);
restore_flags(flags);
if (inb_status(addr) & DIR) {
outb_control(orig_HCR, addr);
if (elp_debug > 0)
printk(notfound_msg, 2);
return -1;
}
} else {
outb_control(orig_HCR | DIR, addr);
timeout = jiffies + 30;
while (jiffies < timeout);
restore_flags(flags);
if (!(inb_status(addr) & DIR)) {
outb_control(orig_HCR, addr);
if (elp_debug > 0)
printk(notfound_msg, 3);
return -1;
}
}
if (elp_debug > 0)
printk("%s", found_msg);
return 0;
}
static int elp_autodetect(struct device *dev)
{
int idx = 0;
if (dev->base_addr != 0) {
if (elp_sense(dev) == 0)
return dev->base_addr;
} else
while ((dev->base_addr = addr_list[idx++])) {
if (elp_sense(dev) == 0)
return dev->base_addr;
}
if (elp_debug > 0)
printk(couldnot_msg, dev->name);
return 0;
}
int elplus_probe(struct device *dev)
{
elp_device *adapter;
int i, tries, tries1, timeout, okay;
dev->base_addr = elp_autodetect(dev);
if (!(dev->base_addr))
return -ENODEV;
adapter = (elp_device *) (dev->priv = kmalloc(sizeof(elp_device), GFP_KERNEL));
if (adapter == NULL) {
printk("%s: out of memory\n", dev->name);
return -ENODEV;
}
for (tries1 = 0; tries1 < 3; tries1++) {
outb_control((inb_control(dev->base_addr) | CMDE) & ~DIR, dev->base_addr);
timeout = jiffies + 5;
okay = 0;
while (jiffies < timeout && !(inb_status(dev->base_addr) & HCRE));
if ((inb_status(dev->base_addr) & HCRE)) {
outb_command(0, dev->base_addr);
timeout = jiffies + 5;
while (jiffies < timeout && !(inb_status(dev->base_addr) & HCRE));
if (inb_status(dev->base_addr) & HCRE)
okay = 1;
}
if (!okay) {
printk("%s: command register wouldn't drain, ", dev->name);
if ((inb_status(dev->base_addr) & 7) == 3) {
printk("assuming 3c505 still starting\n");
timeout = jiffies + 10 * HZ;
while (jiffies < timeout && (inb_status(dev->base_addr) & 7));
if (inb_status(dev->base_addr) & 7) {
printk("%s: 3c505 failed to start\n", dev->name);
} else {
okay = 1;
}
} else {
printk("3c505 is sulking\n");
}
}
for (tries = 0; tries < 5 && okay; tries++) {
adapter->tx_pcb.command = CMD_STATION_ADDRESS;
adapter->tx_pcb.length = 0;
autoirq_setup(0);
if (!send_pcb(dev, &adapter->tx_pcb)) {
printk("%s: could not send first PCB\n", dev->name);
autoirq_report(0);
continue;
}
if (!receive_pcb(dev, &adapter->rx_pcb)) {
printk("%s: could not read first PCB\n", dev->name);
autoirq_report(0);
continue;
}
if ((adapter->rx_pcb.command != CMD_ADDRESS_RESPONSE) ||
(adapter->rx_pcb.length != 6)) {
printk("%s: first PCB wrong (%d, %d)\n", dev->name, adapter->rx_pcb.command, adapter->rx_pcb.length);
autoirq_report(0);
continue;
}
goto okay;
}
printk(KERN_INFO "%s: resetting adapter\n", dev->name);
outb_control(inb_control(dev->base_addr) | FLSH | ATTN, dev->base_addr);
outb_control(inb_control(dev->base_addr) & ~(FLSH | ATTN), dev->base_addr);
}
printk("%s: failed to initialise 3c505\n", dev->name);
return -ENODEV;
okay:
if (dev->irq) {
int rpt = autoirq_report(0);
if (dev->irq != rpt) {
printk("%s: warning, irq %d configured but %d detected\n", dev->name, dev->irq, rpt);
return -ENODEV;
}
} else
dev->irq = autoirq_report(0);
switch (dev->irq) {
case 0:
printk("%s: No IRQ reported by autoirq_report().\n", dev->name);
printk("%s: Check the jumpers of your 3c505 board.\n", dev->name);
return -ENODEV;
case 1:
case 6:
case 8:
case 13:
printk("%s: Impossible IRQ %d reported by autoirq_report().\n",
dev->name, dev->irq);
return -ENODEV;
}
outb_control(inb_control(dev->base_addr) & ~CMDE, dev->base_addr);
for (i = 0; i < 6; i++)
dev->dev_addr[i] = adapter->rx_pcb.data.eth_addr[i];
dev->dma = ELP_DMA;
printk("%s: 3c505 at %#lx, irq %d, dma %d, ",
dev->name, dev->base_addr, dev->irq, dev->dma);
printk("addr %02x:%02x:%02x:%02x:%02x:%02x, ",
dev->dev_addr[0], dev->dev_addr[1], dev->dev_addr[2],
dev->dev_addr[3], dev->dev_addr[4], dev->dev_addr[5]);
adapter->tx_pcb.command = CMD_ADAPTER_INFO;
adapter->tx_pcb.length = 0;
if (!send_pcb(dev, &adapter->tx_pcb) ||
!receive_pcb(dev, &adapter->rx_pcb) ||
(adapter->rx_pcb.command != CMD_ADAPTER_INFO_RESPONSE) ||
(adapter->rx_pcb.length != 10)) {
printk("%s: not responding to second PCB\n", dev->name);
}
printk("rev %d.%d, %dk\n", adapter->rx_pcb.data.info.major_vers, adapter->rx_pcb.data.info.minor_vers, adapter->rx_pcb.data.info.RAM_sz);
adapter->tx_pcb.command = CMD_CONFIGURE_ADAPTER_MEMORY;
adapter->tx_pcb.length = 12;
adapter->tx_pcb.data.memconf.cmd_q = 8;
adapter->tx_pcb.data.memconf.rcv_q = 8;
adapter->tx_pcb.data.memconf.mcast = 10;
adapter->tx_pcb.data.memconf.frame = 10;
adapter->tx_pcb.data.memconf.rcv_b = 10;
adapter->tx_pcb.data.memconf.progs = 0;
if (!send_pcb(dev, &adapter->tx_pcb) ||
!receive_pcb(dev, &adapter->rx_pcb) ||
(adapter->rx_pcb.command != CMD_CONFIGURE_ADAPTER_RESPONSE) ||
(adapter->rx_pcb.length != 2)) {
printk("%s: could not configure adapter memory\n", dev->name);
}
if (adapter->rx_pcb.data.configure) {
printk("%s: adapter configuration failed\n", dev->name);
}
request_region(dev->base_addr, ELP_IO_EXTENT, "3c505");
elp_init(dev);
return 0;
}
#ifdef MODULE
static char devicename[9] = {0,};
static struct device dev_3c505 =
{
devicename,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, elplus_probe};
int io = 0x300;
int irq = 0;
int init_module(void)
{
if (io == 0)
printk("3c505: You should not use auto-probing with insmod!\n");
dev_3c505.base_addr = io;
dev_3c505.irq = irq;
if (register_netdev(&dev_3c505) != 0) {
return -EIO;
}
return 0;
}
void cleanup_module(void)
{
unregister_netdev(&dev_3c505);
kfree(dev_3c505.priv);
dev_3c505.priv = NULL;
release_region(dev_3c505.base_addr, ELP_IO_EXTENT);
}
#endif