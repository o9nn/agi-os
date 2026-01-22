static const char *version =
"3c501.c: 9/23/94 Donald Becker (becker@cesdis.gsfc.nasa.gov).\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/fcntl.h>
#include <linux/ioport.h>
#include <linux/interrupt.h>
#include <linux/malloc.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/config.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#define BLOCKOUT_2
static unsigned int netcard_portlist[] =
{ 0x280, 0x300, 0};
int el1_probe(struct device *dev);
static int el1_probe1(struct device *dev, int ioaddr);
static int el_open(struct device *dev);
static int el_start_xmit(struct sk_buff *skb, struct device *dev);
static void el_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void el_receive(struct device *dev);
static void el_reset(struct device *dev);
static int el1_close(struct device *dev);
static struct enet_statistics *el1_get_stats(struct device *dev);
static void set_multicast_list(struct device *dev);
#define EL1_IO_EXTENT 16
#ifndef EL_DEBUG
#define EL_DEBUG 0
#endif
static int el_debug = EL_DEBUG;
struct net_local
{
struct enet_statistics stats;
int tx_pkt_start;
int collisions;
int loading;
};
#define RX_STATUS (ioaddr + 0x06)
#define RX_CMD RX_STATUS
#define TX_STATUS (ioaddr + 0x07)
#define TX_CMD TX_STATUS
#define GP_LOW (ioaddr + 0x08)
#define GP_HIGH (ioaddr + 0x09)
#define RX_BUF_CLR (ioaddr + 0x0A)
#define RX_LOW (ioaddr + 0x0A)
#define RX_HIGH (ioaddr + 0x0B)
#define SAPROM (ioaddr + 0x0C)
#define AX_STATUS (ioaddr + 0x0E)
#define AX_CMD AX_STATUS
#define DATAPORT (ioaddr + 0x0F)
#define TX_RDY 0x08
#define EL1_DATAPTR 0x08
#define EL1_RXPTR 0x0A
#define EL1_SAPROM 0x0C
#define EL1_DATAPORT 0x0f
#define AX_OFF 0x00
#define AX_SYS 0x40
#define AX_XMIT 0x44
#define AX_RX 0x48
#define AX_LOOP 0x0C
#define AX_RESET 0x80
#define RX_NORM 0xA8
#define RX_PROM 0x68
#define RX_MULT 0xE8
#define TX_NORM 0x0A
#define TX_COLLISION 0x02
#define TX_16COLLISIONS 0x04
#define TX_READY 0x08
#define RX_RUNT 0x08
#define RX_MISSED 0x01
#define RX_GOOD 0x30
#ifdef HAVE_DEVLIST
struct netdev_entry el1_drv = {"3c501", el1_probe1, EL1_IO_EXTENT, netcard_portlist};
#else
int el1_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return el1_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; netcard_portlist[i]; i++)
{
int ioaddr = netcard_portlist[i];
if (check_region(ioaddr, EL1_IO_EXTENT))
continue;
if (el1_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
static int el1_probe1(struct device *dev, int ioaddr)
{
const char *mname;
unsigned char station_addr[6];
int autoirq = 0;
int i;
for (i = 0; i < 6; i++)
{
outw(i, ioaddr + EL1_DATAPTR);
station_addr[i] = inb(ioaddr + EL1_SAPROM);
}
if (station_addr[0] == 0x02 && station_addr[1] == 0x60
&& station_addr[2] == 0x8c)
{
mname = "3c501";
} else if (station_addr[0] == 0x00 && station_addr[1] == 0x80
&& station_addr[2] == 0xC8)
{
mname = "NP943";
}
else
return ENODEV;
request_region(ioaddr, EL1_IO_EXTENT,"3c501");
if (dev->irq < 2)
{
autoirq_setup(2);
inb(RX_STATUS);
inb(TX_STATUS);
outb(AX_LOOP + 1, AX_CMD);
outb(0x00, AX_CMD);
autoirq = autoirq_report(1);
if (autoirq == 0)
{
printk("%s probe at %#x failed to detect IRQ line.\n",
mname, ioaddr);
return EAGAIN;
}
}
outb(AX_RESET+AX_LOOP, AX_CMD);
dev->base_addr = ioaddr;
memcpy(dev->dev_addr, station_addr, ETH_ALEN);
if (dev->mem_start & 0xf)
el_debug = dev->mem_start & 0x7;
if (autoirq)
dev->irq = autoirq;
printk("%s: %s EtherLink at %#lx, using %sIRQ %d.\n", dev->name, mname, dev->base_addr,
autoirq ? "auto":"assigned ", dev->irq);
#ifdef CONFIG_IP_MULTICAST
printk("WARNING: Use of the 3c501 in a multicast kernel is NOT recommended.\n");
#endif
if (el_debug)
printk("%s", version);
dev->priv = kmalloc(sizeof(struct net_local), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct net_local));
dev->open = &el_open;
dev->hard_start_xmit = &el_start_xmit;
dev->stop = &el1_close;
dev->get_stats = &el1_get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
return 0;
}
static int el_open(struct device *dev)
{
int ioaddr = dev->base_addr;
if (el_debug > 2)
printk("%s: Doing el_open()...", dev->name);
if (request_irq(dev->irq, &el_interrupt, 0, "3c501", NULL))
return -EAGAIN;
irq2dev_map[dev->irq] = dev;
el_reset(dev);
dev->start = 1;
outb(AX_RX, AX_CMD);
MOD_INC_USE_COUNT;
return 0;
}
static int el_start_xmit(struct sk_buff *skb, struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
unsigned long flags;
if(dev->interrupt)
return 1;
if (dev->tbusy)
{
if (jiffies - dev->trans_start < 20)
{
if (el_debug > 2)
printk(" transmitter busy, deferred.\n");
return 1;
}
if (el_debug)
printk ("%s: transmit timed out, txsr %#2x axsr=%02x rxsr=%02x.\n",
dev->name, inb(TX_STATUS), inb(AX_STATUS), inb(RX_STATUS));
lp->stats.tx_errors++;
outb(TX_NORM, TX_CMD);
outb(RX_NORM, RX_CMD);
outb(AX_OFF, AX_CMD);
outb(AX_RX, AX_CMD);
dev->tbusy = 0;
dev->trans_start = jiffies;
}
if (skb == NULL)
{
dev_tint(dev);
return 0;
}
save_flags(flags);
cli();
if (set_bit(0, (void*)&dev->tbusy) != 0)
{
restore_flags(flags);
printk("%s: Transmitter access conflict.\n", dev->name);
}
else
{
int gp_start = 0x800 - (ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN);
unsigned char *buf = skb->data;
load_it_again_sam:
lp->tx_pkt_start = gp_start;
lp->collisions = 0;
#ifdef BLOCKOUT_1
disable_irq(dev->irq);
#endif
outb_p(AX_SYS, AX_CMD);
inb_p(RX_STATUS);
inb_p(TX_STATUS);
lp->loading=1;
restore_flags(flags);
outw(0x00, RX_BUF_CLR);
outw(gp_start, GP_LOW);
outsb(DATAPORT,buf,skb->len);
outw(gp_start, GP_LOW);
#ifndef BLOCKOUT_1
if(lp->loading==2)
{
if(el_debug>2)
printk("%s: burped during tx load.\n", dev->name);
goto load_it_again_sam;
}
#endif
outb(AX_XMIT, AX_CMD);
lp->loading=0;
#ifdef BLOCKOUT_1
enable_irq(dev->irq);
#endif
dev->trans_start = jiffies;
}
if (el_debug > 2)
printk(" queued xmit.\n");
dev_kfree_skb (skb, FREE_WRITE);
return 0;
}
static void el_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
struct net_local *lp;
int ioaddr;
int axsr;
if (dev == NULL || dev->irq != irq)
{
printk ("3c501 driver: irq %d for unknown device.\n", irq);
return;
}
ioaddr = dev->base_addr;
lp = (struct net_local *)dev->priv;
axsr = inb(AX_STATUS);
if (el_debug > 3)
printk("%s: el_interrupt() aux=%#02x", dev->name, axsr);
if (dev->interrupt)
printk("%s: Reentering the interrupt driver!\n", dev->name);
dev->interrupt = 1;
#ifndef BLOCKOUT_1
if(lp->loading==1 && !dev->tbusy)
printk("%s: Inconsistent state loading while not in tx\n",
dev->name);
#endif
#ifdef BLOCKOUT_3
lp->loading=2;
#endif
if (dev->tbusy)
{
int txsr = inb(TX_STATUS);
#ifdef BLOCKOUT_2
if(lp->loading==1)
{
if(el_debug > 2)
{
printk("%s: Interrupt while loading [", dev->name);
printk(" txsr=%02x gp=%04x rp=%04x]\n", txsr, inw(GP_LOW),inw(RX_LOW));
}
lp->loading=2;
dev->interrupt = 0;
return;
}
#endif
if (el_debug > 6)
printk(" txsr=%02x gp=%04x rp=%04x", txsr, inw(GP_LOW),inw(RX_LOW));
if ((axsr & 0x80) && (txsr & TX_READY) == 0)
{
if(el_debug>1)
printk("%s: Unusual interrupt during Tx, txsr=%02x axsr=%02x"
" gp=%03x rp=%03x.\n", dev->name, txsr, axsr,
inw(ioaddr + EL1_DATAPTR), inw(ioaddr + EL1_RXPTR));
dev->tbusy = 0;
mark_bh(NET_BH);
}
else if (txsr & TX_16COLLISIONS)
{
if (el_debug)
printk("%s: Transmit failed 16 times, ethernet jammed?\n",dev->name);
outb(AX_SYS, AX_CMD);
lp->stats.tx_aborted_errors++;
}
else if (txsr & TX_COLLISION)
{
if (el_debug > 6)
printk(" retransmitting after a collision.\n");
outb(AX_SYS, AX_CMD);
outw(lp->tx_pkt_start, GP_LOW);
outb(AX_XMIT, AX_CMD);
lp->stats.collisions++;
dev->interrupt = 0;
return;
}
else
{
lp->stats.tx_packets++;
if (el_debug > 6)
printk(" Tx succeeded %s\n",
(txsr & TX_RDY) ? "." : "but tx is busy!");
dev->tbusy = 0;
mark_bh(NET_BH);
}
}
else
{
int rxsr = inb(RX_STATUS);
if (el_debug > 5)
printk(" rxsr=%02x txsr=%02x rp=%04x", rxsr, inb(TX_STATUS),inw(RX_LOW));
if (rxsr & RX_MISSED)
lp->stats.rx_missed_errors++;
else if (rxsr & RX_RUNT)
{
lp->stats.rx_length_errors++;
if (el_debug > 5)
printk(" runt.\n");
}
else if (rxsr & RX_GOOD)
{
el_receive(dev);
}
else
{
if (el_debug > 2)
printk("%s: No packet seen, rxsr=%02x **resetting 3c501***\n",
dev->name, rxsr);
el_reset(dev);
}
if (el_debug > 3)
printk(".\n");
}
outb(AX_RX, AX_CMD);
outw(0x00, RX_BUF_CLR);
inb(RX_STATUS);
inb(TX_STATUS);
dev->interrupt = 0;
return;
}
static void el_receive(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
int ioaddr = dev->base_addr;
int pkt_len;
struct sk_buff *skb;
pkt_len = inw(RX_LOW);
if (el_debug > 4)
printk(" el_receive %d.\n", pkt_len);
if ((pkt_len < 60) || (pkt_len > 1536))
{
if (el_debug)
printk("%s: bogus packet, length=%d\n", dev->name, pkt_len);
lp->stats.rx_over_errors++;
return;
}
outb(AX_SYS, AX_CMD);
skb = dev_alloc_skb(pkt_len+2);
outw(0x00, GP_LOW);
if (skb == NULL)
{
printk("%s: Memory squeeze, dropping packet.\n", dev->name);
lp->stats.rx_dropped++;
return;
}
else
{
skb_reserve(skb,2);
skb->dev = dev;
insb(DATAPORT, skb_put(skb,pkt_len), pkt_len);
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
}
return;
}
static void el_reset(struct device *dev)
{
int ioaddr = dev->base_addr;
if (el_debug> 2)
printk("3c501 reset...");
outb(AX_RESET, AX_CMD);
outb(AX_LOOP, AX_CMD);
{
int i;
for (i = 0; i < 6; i++)
outb(dev->dev_addr[i], ioaddr + i);
}
outw(0, RX_BUF_CLR);
cli();
outb(TX_NORM, TX_CMD);
outb(RX_NORM, RX_CMD);
inb(RX_STATUS);
inb(TX_STATUS);
dev->interrupt = 0;
dev->tbusy = 0;
sti();
}
static int el1_close(struct device *dev)
{
int ioaddr = dev->base_addr;
if (el_debug > 2)
printk("%s: Shutting down ethercard at %#x.\n", dev->name, ioaddr);
dev->tbusy = 1;
dev->start = 0;
free_irq(dev->irq, NULL);
outb(AX_RESET, AX_CMD);
irq2dev_map[dev->irq] = 0;
MOD_DEC_USE_COUNT;
return 0;
}
static struct enet_statistics *el1_get_stats(struct device *dev)
{
struct net_local *lp = (struct net_local *)dev->priv;
return &lp->stats;
}
static void set_multicast_list(struct device *dev)
{
int ioaddr = dev->base_addr;
if(dev->flags&IFF_PROMISC)
{
outb(RX_PROM, RX_CMD);
inb(RX_STATUS);
}
else if (dev->mc_list || dev->flags&IFF_ALLMULTI)
{
outb(RX_MULT, RX_CMD);
inb(RX_STATUS);
}
else
{
outb(RX_NORM, RX_CMD);
inb(RX_STATUS);
}
}
#ifdef MODULE
static char devicename[9] = { 0, };
static struct device dev_3c501 =
{
devicename,
0, 0, 0, 0,
0x280, 5,
0, 0, 0, NULL, el1_probe
};
static int io=0x280;
static int irq=5;
int init_module(void)
{
dev_3c501.irq=irq;
dev_3c501.base_addr=io;
if (register_netdev(&dev_3c501) != 0)
return -EIO;
return 0;
}
void cleanup_module(void)
{
unregister_netdev(&dev_3c501);
kfree(dev_3c501.priv);
dev_3c501.priv = NULL;
release_region(dev_3c501.base_addr, EL1_IO_EXTENT);
}
#endif