static const char *rcsid = "$Id: sk_g16.c,v 1.1 1999/04/26 05:52:37 tb Exp $";
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/fcntl.h>
#include <linux/ioport.h>
#include <linux/interrupt.h>
#include <linux/malloc.h>
#include <linux/string.h>
#include <asm/system.h>
#include <asm/io.h>
#include <asm/bitops.h>
#include <linux/errno.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include "sk_g16.h"
#define SK_NAME "SK_G16"
#define SK_BOOT_ROM 1
#define SK_ADDR 0xcc000
#define POS_ADDR (rom_addr>>14)
#define SK_IO_PORTS { 0x100, 0x180, 0x208, 0x220, 0x288, 0x320, 0x328, 0x390, 0 }
#define SK_IRQS { 3, 5, 9, 11, 0 }
#define SK_BOOT_ROM_LOCATIONS { 0xc0000, 0xc4000, 0xc8000, 0xcc000, 0xd0000, 0xd4000, 0xd8000, 0xdc000, 0 }
#define SK_BOOT_ROM_ID { 0x55, 0xaa, 0x10, 0x50, 0x06, 0x33 }
#define SK_POS_SIZE 8
#define SK_POS0 ioaddr
#define SK_POS1 ioaddr+1
#define SK_POS2 ioaddr+2
#define SK_POS3 ioaddr+3
#define SK_POS4 ioaddr+4
#define SK_MAC0 0x00
#define SK_MAC1 0x00
#define SK_MAC2 0x5a
#define SK_IDLOW 0xfd
#define SK_IDHIGH 0x6a
#define SK_ROM_RAM_ON (POS2_CARD)
#define SK_ROM_RAM_OFF (POS2_EPROM)
#define SK_ROM_ON (inb(SK_POS2) & POS2_CARD)
#define SK_ROM_OFF (inb(SK_POS2) | POS2_EPROM)
#define SK_RAM_ON (inb(SK_POS2) | POS2_CARD)
#define SK_RAM_OFF (inb(SK_POS2) & POS2_EPROM)
#define POS2_CARD 0x0001
#define POS2_EPROM 0x0002
#define SK_IOREG (board->ioreg)
#define SK_PORT (board->port)
#define SK_IOCOM (board->iocom)
#define SK_IORUN 0x20
#define SK_IRQ 0x10
#define SK_RESET 0x08
#define SK_RW 0x02
#define SK_ADR 0x01
#define SK_RREG SK_RW
#define SK_WREG 0
#define SK_RAP SK_ADR
#define SK_RDATA 0
#define SK_DOIO 0x80
#define CSR0 0x00
#define CSR1 0x01
#define CSR2 0x02
#define CSR3 0x03
#define LC_LOG_TX_BUFFERS 1
#define LC_LOG_RX_BUFFERS 3
#define TMDNUM (1 << (LC_LOG_TX_BUFFERS))
#define RMDNUM (1 << (LC_LOG_RX_BUFFERS))
#define TMDNUMMASK (LC_LOG_TX_BUFFERS << 29)
#define RMDNUMMASK (LC_LOG_RX_BUFFERS << 29)
#define PKT_BUF_SZ 1518
#define ETHERCARD_TOTAL_SIZE SK_POS_SIZE
#ifndef HAVE_PORTRESERVE
#define check_region(ioaddr, size) 0
#define request_region(ioaddr, size,name) do ; while (0)
#endif
#undef SK_DEBUG
#undef SK_DEBUG2
#ifdef SK_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x)
#endif
#ifdef SK_DEBUG2
#define PRINTK2(x) printk x
#else
#define PRINTK2(x)
#endif
typedef struct
{
unsigned char ram[0x3fc0];
unsigned char rom[0x0020];
unsigned char res1[0x0010];
unsigned volatile short ioreg;
unsigned volatile char port;
unsigned char iocom;
} SK_RAM;
struct SK_ram
{
struct init_block ib;
struct tmd tmde[TMDNUM];
struct rmd rmde[RMDNUM];
char tmdbuf[TMDNUM][PKT_BUF_SZ];
char rmdbuf[RMDNUM][PKT_BUF_SZ];
};
struct priv
{
struct SK_ram *ram;
struct rmd *rmdhead;
struct tmd *tmdhead;
int rmdnum;
int tmdnum;
int tmdlast;
void *rmdbufs[RMDNUM];
void *tmdbufs[TMDNUM];
struct enet_statistics stats;
};
static SK_RAM *board;
int SK_init(struct device *dev);
static int SK_probe(struct device *dev, short ioaddr);
static int SK_open(struct device *dev);
static int SK_send_packet(struct sk_buff *skb, struct device *dev);
static void SK_interrupt(int irq, void *dev_id, struct pt_regs * regs);
static void SK_rxintr(struct device *dev);
static void SK_txintr(struct device *dev);
static int SK_close(struct device *dev);
static struct enet_statistics *SK_get_stats(struct device *dev);
unsigned int SK_rom_addr(void);
static void set_multicast_list(struct device *dev);
static int SK_lance_init(struct device *dev, unsigned short mode);
void SK_reset_board(void);
void SK_set_RAP(int reg_number);
int SK_read_reg(int reg_number);
int SK_rread_reg(void);
void SK_write_reg(int reg_number, int value);
void SK_print_pos(struct device *dev, char *text);
void SK_print_dev(struct device *dev, char *text);
void SK_print_ram(struct device *dev);
int SK_init(struct device *dev)
{
int ioaddr = 0;
int *port, ports[] = SK_IO_PORTS;
int base_addr = dev->base_addr;
PRINTK(("%s: %s", SK_NAME, rcsid));
rcsid = NULL;
if (base_addr > 0x0ff)
{
if ( (inb(SK_POS0) == SK_IDLOW) ||
(inb(SK_POS1) == SK_IDHIGH) )
{
return SK_probe(dev, base_addr);
}
return ENODEV;
}
else if (base_addr > 0)
{
return ENXIO;
}
for (port = &ports[0]; *port; port++)
{
ioaddr = *port;
if (check_region(ioaddr, ETHERCARD_TOTAL_SIZE))
{
continue;
}
if ( !(inb(SK_POS0) == SK_IDLOW) ||
!(inb(SK_POS1) == SK_IDHIGH) )
{
continue;
}
dev->base_addr = ioaddr;
if (SK_probe(dev, ioaddr) == 0)
{
return 0;
}
}
dev->base_addr = base_addr;
return ENODEV;
}
int SK_probe(struct device *dev, short ioaddr)
{
int i,j;
int sk_addr_flag = 0;
unsigned int rom_addr;
struct priv *p;
if (SK_ADDR & 0x3fff || SK_ADDR < 0xa0000)
{
sk_addr_flag = 1;
}
if (SK_BOOT_ROM)
{
PRINTK(("## %s: SK_BOOT_ROM is set.\n", SK_NAME));
rom_addr = SK_rom_addr();
if (rom_addr == 0)
{
if (sk_addr_flag)
{
printk("%s: SK_ADDR %#08x is not valid. Check configuration.\n",
dev->name, SK_ADDR);
return -1;
}
rom_addr = SK_ADDR;
PRINTK(("## %s: NO Bootrom found \n", SK_NAME));
outb(SK_ROM_RAM_OFF, SK_POS2);
outb(POS_ADDR, SK_POS3);
outb(SK_RAM_ON, SK_POS2);
}
else if (rom_addr == SK_ADDR)
{
printk("%s: RAM + ROM are set to the same address %#08x\n"
"   Check configuration. Now switching off Boot_ROM\n",
SK_NAME, rom_addr);
outb(SK_ROM_RAM_OFF, SK_POS2);
outb(POS_ADDR, SK_POS3);
outb(SK_RAM_ON, SK_POS2);
}
else
{
PRINTK(("## %s: Found ROM at %#08x\n", SK_NAME, rom_addr));
PRINTK(("## %s: Keeping Boot_ROM on\n", SK_NAME));
if (sk_addr_flag)
{
printk("%s: SK_ADDR %#08x is not valid. Check configuration.\n",
dev->name, SK_ADDR);
return -1;
}
rom_addr = SK_ADDR;
outb(SK_ROM_RAM_OFF, SK_POS2);
outb(POS_ADDR, SK_POS3);
outb(SK_ROM_RAM_ON, SK_POS2);
}
}
else
{
PRINTK(("## %s: SK_BOOT_ROM is not set.\n", SK_NAME));
if (sk_addr_flag)
{
printk("%s: SK_ADDR %#08x is not valid. Check configuration.\n",
dev->name, SK_ADDR);
return -1;
}
rom_addr = SK_rom_addr();
outb(SK_ROM_RAM_OFF, SK_POS2);
if (rom_addr)
{
printk("%s: We found Boot_ROM at %#08x. Now setting RAM on"
"that address\n", SK_NAME, rom_addr);
outb(POS_ADDR, SK_POS3);
}
else
{
if (sk_addr_flag)
{
printk("%s: SK_ADDR %#08x is not valid. Check configuration.\n",
dev->name, SK_ADDR);
return -1;
}
rom_addr = SK_ADDR;
outb(POS_ADDR, SK_POS3);
}
outb(SK_RAM_ON, SK_POS2);
}
#ifdef SK_DEBUG
SK_print_pos(dev, "POS registers after ROM, RAM config");
#endif
board = (SK_RAM *) rom_addr;
for (i = 0, j = 0; i < ETH_ALEN; i++, j+=2)
{
dev->dev_addr[i] = board->rom[j];
}
if (!(dev->dev_addr[0] == SK_MAC0 &&
dev->dev_addr[1] == SK_MAC1 &&
dev->dev_addr[2] == SK_MAC2) )
{
PRINTK(("## %s: We did not find SK_G16 at RAM location.\n",
SK_NAME));
return ENODEV;
}
printk("%s: %s found at %#3x, HW addr: %#04x:%02x:%02x:%02x:%02x:%02x\n",
dev->name,
"Schneider & Koch Netcard",
(unsigned int) dev->base_addr,
dev->dev_addr[0],
dev->dev_addr[1],
dev->dev_addr[2],
dev->dev_addr[3],
dev->dev_addr[4],
dev->dev_addr[5]);
p = dev->priv = (void *) kmalloc(sizeof(struct priv), GFP_KERNEL);
if (p == NULL) {
printk("%s: ERROR - no memory for driver data!\n", dev->name);
return -ENOMEM;
}
memset((char *) dev->priv, 0, sizeof(struct priv));
request_region(ioaddr, ETHERCARD_TOTAL_SIZE,"sk_g16");
dev->open = &SK_open;
dev->stop = &SK_close;
dev->hard_start_xmit = &SK_send_packet;
dev->get_stats = &SK_get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
dev->flags &= ~IFF_MULTICAST;
p->ram = (struct SK_ram *) rom_addr;
p->tmdhead = &(p->ram)->tmde[0];
p->rmdhead = &(p->ram)->rmde[0];
for (i = 0; i < TMDNUM; i++)
{
p->tmdbufs[i] = &(p->ram)->tmdbuf[i];
}
for (i = 0; i < RMDNUM; i++)
{
p->rmdbufs[i] = &(p->ram)->rmdbuf[i];
}
#ifdef SK_DEBUG
SK_print_pos(dev, "End of SK_probe");
SK_print_ram(dev);
#endif
return 0;
}
static int SK_open(struct device *dev)
{
int i = 0;
int irqval = 0;
int ioaddr = dev->base_addr;
int irqtab[] = SK_IRQS;
struct priv *p = (struct priv *)dev->priv;
PRINTK(("## %s: At beginning of SK_open(). CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
if (dev->irq == 0)
{
i = 0;
do
{
irqval = request_irq(irqtab[i], &SK_interrupt, 0, "sk_g16", NULL);
i++;
} while (irqval && irqtab[i]);
if (irqval)
{
printk("%s: unable to get an IRQ\n", dev->name);
return -EAGAIN;
}
dev->irq = irqtab[--i];
outb(i<<2, SK_POS4);
}
else if (dev->irq == 2)
{
if (request_irq(9, &SK_interrupt, 0, "sk_g16", NULL))
{
printk("%s: unable to get IRQ 9\n", dev->name);
return -EAGAIN;
}
dev->irq = 9;
outb(0x08, SK_POS4);
}
else
{
int i = 0;
if (request_irq(dev->irq, &SK_interrupt, 0, "sk_g16", NULL))
{
printk("%s: unable to get selected IRQ\n", dev->name);
return -EAGAIN;
}
switch(dev->irq)
{
case 3: i = 0;
break;
case 5: i = 1;
break;
case 2: i = 2;
break;
case 11:i = 3;
break;
default:
printk("%s: Preselected IRQ %d is invalid for %s boards",
dev->name,
dev->irq,
SK_NAME);
return -EAGAIN;
}
outb(i<<2, SK_POS4);
}
irq2dev_map[dev->irq] = dev;
printk("%s: Schneider & Koch G16 at %#3x, IRQ %d, shared mem at %#08x\n",
dev->name, (unsigned int)dev->base_addr,
(int) dev->irq, (unsigned int) p->ram);
if (!(i = SK_lance_init(dev, 0)))
{
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
#ifdef SK_DEBUG
printk("## %s: After lance init. CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0));
SK_write_reg(CSR0, CSR0_STOP);
printk("## %s: LANCE stopped. CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0));
SK_lance_init(dev, MODE_DTX | MODE_DRX);
printk("## %s: Reinit with DTX + DRX off. CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0));
SK_write_reg(CSR0, CSR0_STOP);
printk("## %s: LANCE stopped. CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0));
SK_lance_init(dev, MODE_NORMAL);
printk("## %s: LANCE back to normal mode. CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0));
SK_print_pos(dev, "POS regs before returning OK");
#endif
return 0;
}
else
{
PRINTK(("## %s: LANCE init failed: CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
dev->start = 0;
return -EAGAIN;
}
}
static int SK_lance_init(struct device *dev, unsigned short mode)
{
int i;
struct priv *p = (struct priv *) dev->priv;
struct tmd *tmdp;
struct rmd *rmdp;
PRINTK(("## %s: At beginning of LANCE init. CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
SK_reset_board();
p->tmdnum = 0;
p->tmdlast = 0;
for (i = 0; i < TMDNUM; i++)
{
tmdp = p->tmdhead + i;
tmdp->u.buffer = (unsigned long) p->tmdbufs[i];
tmdp->u.s.status = TX_STP | TX_ENP;
}
p->rmdnum = 0;
for (i = 0; i < RMDNUM; i++)
{
rmdp = p->rmdhead + i;
rmdp->u.buffer = (unsigned long) p->rmdbufs[i];
rmdp->u.s.status = RX_OWN;
rmdp->blen = -PKT_BUF_SZ;
rmdp->mlen = 0;
}
(p->ram)->ib.mode = mode;
for (i = 0; i < ETH_ALEN; i++)
{
(p->ram)->ib.paddr[i] = dev->dev_addr[i];
}
for (i = 0; i < 8; i++)
{
(p->ram)->ib.laddr[i] = 0;
}
(p->ram)->ib.rdrp = (int) p->rmdhead | RMDNUMMASK;
(p->ram)->ib.tdrp = (int) p->tmdhead | TMDNUMMASK;
cli();
SK_write_reg(CSR3, CSR3_ACON);
SK_write_reg(CSR1, 0);
SK_write_reg(CSR2, 0);
PRINTK(("## %s: After setting CSR1-3. CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
SK_write_reg(CSR0, CSR0_INIT);
sti();
SK_set_RAP(CSR0);
for (i = 0; (i < 100) && !(SK_rread_reg() & CSR0_IDON); i++)
;
if (i >= 100)
{
printk("%s: can't init am7990, status: %04x "
"init_block: %#08x\n",
dev->name, (int) SK_read_reg(CSR0),
(unsigned int) &(p->ram)->ib);
#ifdef SK_DEBUG
SK_print_pos(dev, "LANCE INIT failed");
SK_print_dev(dev,"Device Structure:");
#endif
return -1;
}
PRINTK(("## %s: init done after %d ticks\n", SK_NAME, i));
SK_write_reg(CSR0, CSR0_IDON | CSR0_INEA | CSR0_STRT);
PRINTK(("## %s: LANCE started. CSR0: %#06x\n", SK_NAME,
SK_read_reg(CSR0)));
return 0;
}
static int SK_send_packet(struct sk_buff *skb, struct device *dev)
{
struct priv *p = (struct priv *) dev->priv;
struct tmd *tmdp;
if (dev->tbusy)
{
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 15)
{
return 1;
}
printk("%s: xmitter timed out, try to restart!\n", dev->name);
SK_lance_init(dev, MODE_NORMAL);
dev->tbusy = 0;
dev->trans_start = jiffies;
}
if (skb == NULL)
{
dev_tint(dev);
return 0;
}
PRINTK2(("## %s: SK_send_packet() called, CSR0 %#04x.\n",
SK_NAME, SK_read_reg(CSR0)));
if (set_bit(0, (void *) &dev->tbusy) != 0)
{
printk("%s: Transmitter access conflict.\n", dev->name);
}
else
{
short len = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
tmdp = p->tmdhead + p->tmdnum;
memcpy((char *) (tmdp->u.buffer & 0x00ffffff), (char *)skb->data,
skb->len);
tmdp->blen = -len;
tmdp->u.s.status = TX_OWN | TX_STP | TX_ENP;
SK_write_reg(CSR0, CSR0_TDMD | CSR0_INEA);
dev->trans_start = jiffies;
p->tmdnum++;
p->tmdnum &= TMDNUM-1;
if (! ((p->tmdhead + p->tmdnum)->u.s.status & TX_OWN) )
{
dev->tbusy = 0;
}
}
dev_kfree_skb(skb, FREE_WRITE);
return 0;
}
static void SK_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
int csr0;
struct device *dev = (struct device *) irq2dev_map[irq];
struct priv *p = (struct priv *) dev->priv;
PRINTK2(("## %s: SK_interrupt(). status: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
if (dev == NULL)
{
printk("SK_interrupt(): IRQ %d for unknown device.\n", irq);
}
if (dev->interrupt)
{
printk("%s: Re-entering the interrupt handler.\n", dev->name);
}
csr0 = SK_read_reg(CSR0);
dev->interrupt = 1;
SK_write_reg(CSR0, csr0 & CSR0_CLRALL);
if (csr0 & CSR0_ERR)
{
printk("%s: error: %04x\n", dev->name, csr0);
if (csr0 & CSR0_MISS)
{
p->stats.rx_dropped++;
}
}
if (csr0 & CSR0_RINT)
{
SK_rxintr(dev);
}
if (csr0 & CSR0_TINT)
{
SK_txintr(dev);
}
SK_write_reg(CSR0, CSR0_INEA);
dev->interrupt = 0;
}
static void SK_txintr(struct device *dev)
{
int tmdstat;
struct tmd *tmdp;
struct priv *p = (struct priv *) dev->priv;
PRINTK2(("## %s: SK_txintr() status: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
tmdp = p->tmdhead + p->tmdlast;
p->tmdlast++;
p->tmdlast &= TMDNUM-1;
tmdstat = tmdp->u.s.status & 0xff00;
if (tmdstat & TX_ERR)
{
printk("%s: TX error: %04x %04x\n", dev->name, (int) tmdstat,
(int) tmdp->status2);
if (tmdp->status2 & TX_TDR)
{
printk("%s: tdr-problems \n", dev->name);
}
if (tmdp->status2 & TX_RTRY)
p->stats.tx_aborted_errors++;
if (tmdp->status2 & TX_LCOL)
p->stats.tx_window_errors++;
if (tmdp->status2 & TX_LCAR)
p->stats.tx_carrier_errors++;
if (tmdp->status2 & TX_UFLO)
{
p->stats.tx_fifo_errors++;
SK_lance_init(dev, MODE_NORMAL);
}
p->stats.tx_errors++;
tmdp->status2 = 0;
}
else if (tmdstat & TX_MORE)
{
p->stats.collisions++;
}
else
{
p->stats.tx_packets++;
}
dev->tbusy = 0;
mark_bh(NET_BH);
}
static void SK_rxintr(struct device *dev)
{
struct rmd *rmdp;
int rmdstat;
struct priv *p = (struct priv *) dev->priv;
PRINTK2(("## %s: SK_rxintr(). CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
rmdp = p->rmdhead + p->rmdnum;
while (!( (rmdstat = rmdp->u.s.status) & RX_OWN))
{
if ((rmdstat & (RX_STP | RX_ENP)) != (RX_STP | RX_ENP))
{
if (rmdstat & RX_STP)
{
p->stats.rx_errors++;
p->stats.rx_length_errors++;
printk("%s: packet too long\n", dev->name);
}
rmdp->u.s.status = RX_OWN;
}
else if (rmdstat & RX_ERR)
{
printk("%s: RX error: %04x\n", dev->name, (int) rmdstat);
p->stats.rx_errors++;
if (rmdstat & RX_FRAM) p->stats.rx_frame_errors++;
if (rmdstat & RX_CRC) p->stats.rx_crc_errors++;
rmdp->u.s.status = RX_OWN;
}
else
{
int len = (rmdp->mlen & 0x0fff);
struct sk_buff *skb;
skb = dev_alloc_skb(len+2);
if (skb == NULL)
{
rmdp->u.s.status = RX_OWN;
printk("%s: Couldn't allocate sk_buff, deferring packet.\n",
dev->name);
p->stats.rx_dropped++;
break;
}
skb->dev = dev;
skb_reserve(skb,2);
memcpy(skb_put(skb,len), (unsigned char *) (rmdp->u.buffer & 0x00ffffff),
len);
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
rmdp->u.s.status = RX_OWN;
p->stats.rx_packets++;
p->rmdnum++;
p->rmdnum %= RMDNUM;
rmdp = p->rmdhead + p->rmdnum;
}
}
}
static int SK_close(struct device *dev)
{
PRINTK(("## %s: SK_close(). CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
dev->tbusy = 1;
dev->start = 0;
printk("%s: Shutting %s down CSR0 %#06x\n", dev->name, SK_NAME,
(int) SK_read_reg(CSR0));
SK_write_reg(CSR0, CSR0_STOP);
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = 0;
return 0;
}
static struct enet_statistics *SK_get_stats(struct device *dev)
{
struct priv *p = (struct priv *) dev->priv;
PRINTK(("## %s: SK_get_stats(). CSR0: %#06x\n",
SK_NAME, SK_read_reg(CSR0)));
return &p->stats;
}
static void set_multicast_list(struct device *dev)
{
if (dev->flags&IFF_PROMISC)
{
SK_lance_init(dev, MODE_PROM);
}
else if (dev->mc_count==0 && !(dev->flags&IFF_ALLMULTI))
{
SK_lance_init(dev, MODE_NORMAL);
}
else
{
SK_lance_init(dev, MODE_NORMAL);
}
}
unsigned int SK_rom_addr(void)
{
int i,j;
int rom_found = 0;
unsigned int rom_location[] = SK_BOOT_ROM_LOCATIONS;
unsigned char rom_id[] = SK_BOOT_ROM_ID;
unsigned char *test_byte;
PRINTK(("## %s: Autodetection of Boot_ROM\n", SK_NAME));
for (i = 0; (rom_location[i] != 0) && (rom_found == 0); i++)
{
PRINTK(("##   Trying ROM location %#08x", rom_location[i]));
rom_found = 1;
for (j = 0; j < 6; j++)
{
test_byte = (unsigned char *) (rom_location[i]+j);
PRINTK((" %02x ", *test_byte));
if(!(*test_byte == rom_id[j]))
{
rom_found = 0;
}
}
PRINTK(("\n"));
}
if (rom_found == 1)
{
PRINTK(("## %s: Boot_ROM found at %#08x\n",
SK_NAME, rom_location[(i-1)]));
return (rom_location[--i]);
}
else
{
PRINTK(("%s: No Boot_ROM found\n", SK_NAME));
return 0;
}
}
void SK_reset_board(void)
{
int i;
SK_PORT = 0x00;
for (i = 0; i < 10 ; i++)
;
SK_PORT = SK_RESET;
}
void SK_set_RAP(int reg_number)
{
SK_IOREG = reg_number;
SK_PORT = SK_RESET | SK_RAP | SK_WREG;
SK_IOCOM = SK_DOIO;
while (SK_PORT & SK_IORUN)
;
}
int SK_read_reg(int reg_number)
{
SK_set_RAP(reg_number);
SK_PORT = SK_RESET | SK_RDATA | SK_RREG;
SK_IOCOM = SK_DOIO;
while (SK_PORT & SK_IORUN)
;
return (SK_IOREG);
}
int SK_rread_reg(void)
{
SK_PORT = SK_RESET | SK_RDATA | SK_RREG;
SK_IOCOM = SK_DOIO;
while (SK_PORT & SK_IORUN)
;
return (SK_IOREG);
}
void SK_write_reg(int reg_number, int value)
{
SK_set_RAP(reg_number);
SK_IOREG = value;
SK_PORT = SK_RESET | SK_RDATA | SK_WREG;
SK_IOCOM = SK_DOIO;
while (SK_PORT & SK_IORUN)
;
}
void SK_print_pos(struct device *dev, char *text)
{
int ioaddr = dev->base_addr;
unsigned char pos0 = inb(SK_POS0),
pos1 = inb(SK_POS1),
pos2 = inb(SK_POS2),
pos3 = inb(SK_POS3),
pos4 = inb(SK_POS4);
printk("## %s: %s.\n"
"##   pos0=%#4x pos1=%#4x pos2=%#04x pos3=%#08x pos4=%#04x\n",
SK_NAME, text, pos0, pos1, pos2, (pos3<<14), pos4);
}
void SK_print_dev(struct device *dev, char *text)
{
if (dev == NULL)
{
printk("## %s: Device Structure. %s\n", SK_NAME, text);
printk("## DEVICE == NULL\n");
}
else
{
printk("## %s: Device Structure. %s\n", SK_NAME, text);
printk("## Device Name: %s Base Address: %#06lx IRQ: %d\n",
dev->name, dev->base_addr, dev->irq);
printk("##   FLAGS: start: %d tbusy: %ld int: %d\n",
dev->start, dev->tbusy, dev->interrupt);
printk("## next device: %#08x init function: %#08x\n",
(int) dev->next, (int) dev->init);
}
}
void SK_print_ram(struct device *dev)
{
int i;
struct priv *p = (struct priv *) dev->priv;
printk("## %s: RAM Details.\n"
"##   RAM at %#08x tmdhead: %#08x rmdhead: %#08x initblock: %#08x\n",
SK_NAME,
(unsigned int) p->ram,
(unsigned int) p->tmdhead,
(unsigned int) p->rmdhead,
(unsigned int) &(p->ram)->ib);
printk("##   ");
for(i = 0; i < TMDNUM; i++)
{
if (!(i % 3))
{
printk("\n##   ");
}
printk("tmdbufs%d: %#08x ", (i+1), (int) p->tmdbufs[i]);
}
printk("##   ");
for(i = 0; i < RMDNUM; i++)
{
if (!(i % 3))
{
printk("\n##   ");
}
printk("rmdbufs%d: %#08x ", (i+1), (int) p->rmdbufs[i]);
}
printk("\n");
}