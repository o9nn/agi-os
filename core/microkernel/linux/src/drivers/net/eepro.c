static const char *version =
"eepro.c: v0.10c 9/28/98 Bao C. Ha (bao@hacom.net)\n";
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/ioport.h>
#include <linux/in.h>
#include <linux/malloc.h>
#include <linux/string.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/errno.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
static unsigned int eepro_portlist[] =
{ 0x300, 0x240, 0x280, 0x2C0, 0x200, 0x320, 0x340, 0x360, 0};
#ifndef NET_DEBUG
#define NET_DEBUG 1
#endif
static unsigned int net_debug = NET_DEBUG;
#define EEPRO_IO_EXTENT 16
#define LAN595 0
#define LAN595TX 1
#define LAN595FX 2
struct eepro_local {
struct enet_statistics stats;
unsigned rx_start;
unsigned tx_start;
int tx_last;
unsigned tx_end;
int eepro;
int version;
int stepping;
};
#define SA_ADDR0 0x00
#define SA_ADDR1 0xaa
#define SA_ADDR2 0x00
#define SA2_ADDR0 0x00
#define SA2_ADDR1 0xa0
#define SA2_ADDR2 0xc9
#define SA3_ADDR0 0x00
#define SA3_ADDR1 0xaa
#define SA3_ADDR2 0x00
#define SA3_ADDR3 0xc9
extern int eepro_probe(struct device *dev);
static int eepro_probe1(struct device *dev, short ioaddr);
static int eepro_open(struct device *dev);
static int eepro_send_packet(struct sk_buff *skb, struct device *dev);
static void eepro_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static void eepro_rx(struct device *dev);
static void eepro_transmit_interrupt(struct device *dev);
static int eepro_close(struct device *dev);
static struct enet_statistics *eepro_get_stats(struct device *dev);
static void set_multicast_list(struct device *dev);
static int read_eeprom(int ioaddr, int location);
static void hardware_send_packet(struct device *dev, void *buf, short length);
static int eepro_grab_irq(struct device *dev);
#define RAM_SIZE 0x8000
#define RCV_HEADER 8
#define RCV_RAM 0x6000
#define RCV_LOWER_LIMIT 0x00
#define RCV_UPPER_LIMIT (((rcv_ram) - 2) >> 8)
#define XMT_RAM (RAM_SIZE - (rcv_ram))
#define XMT_LOWER_LIMIT ((rcv_ram) >> 8)
#define XMT_UPPER_LIMIT ((RAM_SIZE - 2) >> 8)
#define XMT_HEADER 8
#define RCV_DONE 0x0008
#define RX_OK 0x2000
#define RX_ERROR 0x0d81
#define TX_DONE_BIT 0x0080
#define CHAIN_BIT 0x8000
#define XMT_STATUS 0x02
#define XMT_CHAIN 0x04
#define XMT_COUNT 0x06
#define BANK0_SELECT 0x00
#define BANK1_SELECT 0x40
#define BANK2_SELECT 0x80
#define COMMAND_REG 0x00
#define MC_SETUP 0x03
#define XMT_CMD 0x04
#define DIAGNOSE_CMD 0x07
#define RCV_ENABLE_CMD 0x08
#define RCV_DISABLE_CMD 0x0a
#define STOP_RCV_CMD 0x0b
#define RESET_CMD 0x0e
#define POWER_DOWN_CMD 0x18
#define RESUME_XMT_CMD 0x1c
#define SEL_RESET_CMD 0x1e
#define STATUS_REG 0x01
#define RX_INT 0x02
#define TX_INT 0x04
#define EXEC_STATUS 0x30
#define ID_REG 0x02
#define R_ROBIN_BITS 0xc0
#define ID_REG_MASK 0x2c
#define ID_REG_SIG 0x24
#define AUTO_ENABLE 0x10
#define INT_MASK_REG 0x03
#define RX_STOP_MASK 0x01
#define RX_MASK 0x02
#define TX_MASK 0x04
#define EXEC_MASK 0x08
#define ALL_MASK 0x0f
#define IO_32_BIT 0x10
#define RCV_BAR 0x04
#define RCV_STOP 0x06
#define XMT_BAR 0x0a
#define HOST_ADDRESS_REG 0x0c
#define IO_PORT 0x0e
#define IO_PORT_32_BIT 0x0c
#define REG1 0x01
#define WORD_WIDTH 0x02
#define INT_ENABLE 0x80
#define INT_NO_REG 0x02
#define RCV_LOWER_LIMIT_REG 0x08
#define RCV_UPPER_LIMIT_REG 0x09
#define XMT_LOWER_LIMIT_REG 0x0a
#define XMT_UPPER_LIMIT_REG 0x0b
#define XMT_Chain_Int 0x20
#define XMT_Chain_ErrStop 0x40
#define RCV_Discard_BadFrame 0x80
#define REG2 0x02
#define PRMSC_Mode 0x01
#define Multi_IA 0x20
#define REG3 0x03
#define TPE_BIT 0x04
#define BNC_BIT 0x20
#define REG13 0x0d
#define FDX 0x00
#define A_N_ENABLE 0x02
#define I_ADD_REG0 0x04
#define I_ADD_REG1 0x05
#define I_ADD_REG2 0x06
#define I_ADD_REG3 0x07
#define I_ADD_REG4 0x08
#define I_ADD_REG5 0x09
#define EEPROM_REG 0x0a
#define EESK 0x01
#define EECS 0x02
#define EEDI 0x04
#define EEDO 0x08
#ifdef HAVE_DEVLIST
struct netdev_entry netcard_drv =
{"eepro", eepro_probe1, EEPRO_IO_EXTENT, eepro_portlist};
#else
int
eepro_probe(struct device *dev)
{
int i;
int base_addr = dev ? dev->base_addr : 0;
if (base_addr > 0x1ff)
return eepro_probe1(dev, base_addr);
else if (base_addr != 0)
return ENXIO;
for (i = 0; eepro_portlist[i]; i++) {
int ioaddr = eepro_portlist[i];
if (check_region(ioaddr, EEPRO_IO_EXTENT))
continue;
if (eepro_probe1(dev, ioaddr) == 0)
return 0;
}
return ENODEV;
}
#endif
int
eepro_probe1(struct device *dev, short ioaddr)
{
unsigned short station_addr[6], id, counter;
int i;
int eepro;
const char *ifmap[] = {"AUI", "10Base2", "10BaseT"};
enum iftype { AUI=0, BNC=1, TPE=2 };
if (((id=inb(ioaddr + ID_REG)) & ID_REG_MASK) == ID_REG_SIG) {
counter = (id & R_ROBIN_BITS);
if (((id=inb(ioaddr+ID_REG)) & R_ROBIN_BITS) ==
(counter + 0x40)) {
station_addr[0] = read_eeprom(ioaddr, 2);
station_addr[1] = read_eeprom(ioaddr, 3);
station_addr[2] = read_eeprom(ioaddr, 4);
if ((station_addr[2] == 0x00aa) && (station_addr[1]!= 0x00c9)) {
eepro = 1;
printk("%s: Intel EtherExpress Pro/10 ISA at %#x,",
dev->name, ioaddr);
} else
if ( (station_addr[2] == 0x00a0)
|| ((station_addr[2] == 0x00aa) && (station_addr[1] == 0x00c9) )) {
eepro = 2;
printk("%s: Intel EtherExpress Pro/10+ ISA\n at %#x,",
dev->name, ioaddr);
}
else {
eepro = 0;
printk("%s: Intel 82595-based lan card at %#x,",
dev->name, ioaddr);
}
dev->base_addr = ioaddr;
for (i=0; i < 6; i++) {
dev->dev_addr[i] = ((unsigned char *) station_addr)[5-i];
printk("%c%02x", i ? ':' : ' ', dev->dev_addr[i]);
}
if ((dev->mem_end & 0x3f) < 3 ||
(dev->mem_end & 0x3f) > 29)
dev->mem_end = RCV_RAM;
else dev->mem_end = 1024*dev->mem_end;
if (net_debug > 3)
printk(", %dK RCV buffer", (int)(dev->mem_end)/1024);
outb(BANK2_SELECT, ioaddr);
id = inb(ioaddr + REG3);
if (id & TPE_BIT)
dev->if_port = TPE;
else dev->if_port = BNC;
if (net_debug>3)
printk("id: %x\n", id);
if (dev->irq < 2 && eepro) {
i = read_eeprom(ioaddr, 1);
if (eepro == 1)
switch (i & 0x07) {
case 0: dev->irq = 9; break;
case 1: dev->irq = 3; break;
case 2: dev->irq = 5; break;
case 3: dev->irq = 10; break;
case 4: dev->irq = 11; break;
default:
printk(" illegal interrupt vector stored in EEPROM.\n");
return ENODEV;
}
else switch (i & 0x07) {
case 0: dev->irq = 3; break;
case 1: dev->irq = 4; break;
case 2: dev->irq = 5; break;
case 3: dev->irq = 7; break;
case 4: dev->irq = 9; break;
case 5: dev->irq = 10; break;
case 6: dev->irq = 11; break;
case 7: dev->irq = 12; break;
}
}
else if (dev->irq == 2)
dev->irq = 9;
if (dev->irq > 2) {
printk(", IRQ %d, %s.\n", dev->irq,
ifmap[dev->if_port]);
if (request_irq(dev->irq, &eepro_interrupt, 0, "eepro", NULL)) {
printk("%s: unable to get IRQ %d.\n", dev->name, dev->irq);
return -EAGAIN;
}
}
else printk(", %s.\n", ifmap[dev->if_port]);
if ((dev->mem_start & 0xf) > 0)
net_debug = dev->mem_start & 7;
if (net_debug > 3) {
i = read_eeprom(ioaddr, 5);
if (i & 0x2000)
printk("%s: Concurrent Processing is enabled but not used!\n",
dev->name);
}
if (net_debug)
printk("%s", version);
request_region(ioaddr, EEPRO_IO_EXTENT, "eepro");
dev->priv = kmalloc(sizeof(struct eepro_local), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct eepro_local));
dev->open = eepro_open;
dev->stop = eepro_close;
dev->hard_start_xmit = eepro_send_packet;
dev->get_stats = eepro_get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
outb(RESET_CMD, ioaddr);
return 0;
}
else return ENODEV;
}
else if (net_debug > 3)
printk ("EtherExpress Pro probed failed!\n");
return ENODEV;
}
static char irqrmap[] = {-1,-1,0,1,-1,2,-1,-1,-1,0,3,4,-1,-1,-1,-1};
static char irqrmap2[] = {-1,-1,4,0,1,2,-1,3,-1,4,5,6,7,-1,-1,-1};
static int
eepro_grab_irq(struct device *dev)
{
int irqlist[] = { 3, 4, 5, 7, 9, 10, 11, 12 };
int *irqp = irqlist, temp_reg, ioaddr = dev->base_addr;
outb(BANK1_SELECT, ioaddr);
temp_reg = inb(ioaddr + REG1);
outb(temp_reg | INT_ENABLE, ioaddr + REG1);
outb(BANK0_SELECT, ioaddr);
outb(ALL_MASK, ioaddr + STATUS_REG);
outb(ALL_MASK & ~(EXEC_MASK), ioaddr + INT_MASK_REG);
do {
outb(BANK1_SELECT, ioaddr);
temp_reg = inb(ioaddr + INT_NO_REG);
outb((temp_reg & 0xf8) | irqrmap[*irqp], ioaddr + INT_NO_REG);
outb(BANK0_SELECT, ioaddr);
if (request_irq (*irqp, NULL, 0, "bogus", NULL) != EBUSY) {
autoirq_setup(0);
outb(DIAGNOSE_CMD, ioaddr);
if (*irqp == autoirq_report(2) &&
(request_irq(dev->irq = *irqp, &eepro_interrupt, 0, "eepro", NULL) == 0))
break;
outb(ALL_MASK, ioaddr + STATUS_REG);
}
} while (*++irqp);
outb(BANK1_SELECT, ioaddr);
temp_reg = inb(ioaddr + REG1);
outb(temp_reg & 0x7f, ioaddr + REG1);
outb(BANK0_SELECT, ioaddr);
outb(ALL_MASK, ioaddr + INT_MASK_REG);
outb(ALL_MASK, ioaddr + STATUS_REG);
return dev->irq;
}
static int
eepro_open(struct device *dev)
{
unsigned short temp_reg, old8, old9;
int i, ioaddr = dev->base_addr, rcv_ram = dev->mem_end;
struct eepro_local *lp = (struct eepro_local *)dev->priv;
if (net_debug > 3)
printk("eepro: entering eepro_open routine.\n");
if ((dev->dev_addr[0] == SA_ADDR0 &&
dev->dev_addr[1] == SA_ADDR1 &&
dev->dev_addr[2] == SA_ADDR2)&&
(dev->dev_addr[3] != SA3_ADDR3))
{
lp->eepro = 1;
if (net_debug > 3) printk("p->eepro = 1;\n");
}
else if ((dev->dev_addr[0] == SA2_ADDR0 &&
dev->dev_addr[1] == SA2_ADDR1 &&
dev->dev_addr[2] == SA2_ADDR2)||
(dev->dev_addr[0] == SA3_ADDR0 &&
dev->dev_addr[1] == SA3_ADDR1 &&
dev->dev_addr[2] == SA3_ADDR2 &&
dev->dev_addr[3] == SA3_ADDR3))
{
lp->eepro = 2;
if (net_debug > 3) printk("p->eepro = 2;\n");
}
else lp->eepro = 0;
if (dev->irq < 2 && eepro_grab_irq(dev) == 0) {
printk("%s: unable to get IRQ %d.\n", dev->name, dev->irq);
return -EAGAIN;
}
if (irq2dev_map[dev->irq] != 0
|| (irq2dev_map[dev->irq] = dev) == 0)
return -EAGAIN;
outb(BANK2_SELECT, ioaddr);
temp_reg = inb(ioaddr + EEPROM_REG);
lp->stepping = temp_reg >> 5;
if (net_debug > 3)
printk("The stepping of the 82595 is %d\n", lp->stepping);
if (temp_reg & 0x10)
outb(temp_reg & 0xef, ioaddr + EEPROM_REG);
for (i=0; i < 6; i++)
outb(dev->dev_addr[i] , ioaddr + I_ADD_REG0 + i);
temp_reg = inb(ioaddr + REG1);
outb(temp_reg | XMT_Chain_Int | XMT_Chain_ErrStop
| RCV_Discard_BadFrame, ioaddr + REG1);
temp_reg = inb(ioaddr + REG2);
outb(temp_reg | 0x14, ioaddr + REG2);
temp_reg = inb(ioaddr + REG3);
outb(temp_reg & 0x3f, ioaddr + REG3);
outb(BANK1_SELECT, ioaddr);
temp_reg = inb(ioaddr + INT_NO_REG);
if (lp->eepro == 2)
outb((temp_reg & 0xf8) | irqrmap2[dev->irq], ioaddr + INT_NO_REG);
else outb((temp_reg & 0xf8) | irqrmap[dev->irq], ioaddr + INT_NO_REG);
temp_reg = inb(ioaddr + INT_NO_REG);
if (lp->eepro == 2)
outb((temp_reg & 0xf0) | irqrmap2[dev->irq] | 0x08,ioaddr+INT_NO_REG);
else outb((temp_reg & 0xf8) | irqrmap[dev->irq], ioaddr + INT_NO_REG);
if (net_debug > 3)
printk("eepro_open: content of INT Reg is %x\n", temp_reg);
outb(RCV_LOWER_LIMIT, ioaddr + RCV_LOWER_LIMIT_REG);
outb(RCV_UPPER_LIMIT, ioaddr + RCV_UPPER_LIMIT_REG);
outb(XMT_LOWER_LIMIT, ioaddr + XMT_LOWER_LIMIT_REG);
outb(XMT_UPPER_LIMIT, ioaddr + XMT_UPPER_LIMIT_REG);
temp_reg = inb(ioaddr + REG1);
outb(temp_reg | INT_ENABLE, ioaddr + REG1);
outb(BANK0_SELECT, ioaddr);
outb(ALL_MASK & ~(RX_MASK | TX_MASK), ioaddr + INT_MASK_REG);
outb(ALL_MASK, ioaddr + STATUS_REG);
outw(RCV_LOWER_LIMIT << 8, ioaddr + RCV_BAR);
lp->rx_start = (RCV_LOWER_LIMIT << 8) ;
outw((RCV_UPPER_LIMIT << 8) | 0xfe, ioaddr + RCV_STOP);
outw(XMT_LOWER_LIMIT << 8, ioaddr + XMT_BAR);
old8 = inb(ioaddr + 8);
outb(~old8, ioaddr + 8);
if ((temp_reg = inb(ioaddr + 8)) == old8) {
if (net_debug > 3)
printk("i82595 detected!\n");
lp->version = LAN595;
}
else {
lp->version = LAN595TX;
outb(old8, ioaddr + 8);
old9 = inb(ioaddr + 9);
outb(~old9, ioaddr + 9);
if (((temp_reg = inb(ioaddr + 9)) == ( (~old9)&0xff) )) {
enum iftype { AUI=0, BNC=1, TPE=2 };
if (net_debug > 3) {
printk("temp_reg: %#x  ~old9: %#x\n",temp_reg, ~old9);
printk("i82595FX detected!\n");
}
lp->version = LAN595FX;
outb(old9, ioaddr + 9);
if (dev->if_port != TPE) {
outb(BANK2_SELECT, ioaddr);
temp_reg = inb(ioaddr + REG13);
outb(temp_reg & ~(FDX | A_N_ENABLE), REG13);
outb(BANK0_SELECT, ioaddr);
}
}
else if (net_debug > 3) {
printk("temp_reg: %#x  ~old9: %#x\n",temp_reg,((~old9)&0xff));
printk("i82595TX detected!\n");
}
}
outb(SEL_RESET_CMD, ioaddr);
SLOW_DOWN_IO;
SLOW_DOWN_IO;
lp->tx_start = lp->tx_end = XMT_LOWER_LIMIT << 8;
lp->tx_last = 0;
dev->tbusy = 0;
dev->interrupt = 0;
dev->start = 1;
if (net_debug > 3)
printk("eepro: exiting eepro_open routine.\n");
outb(RCV_ENABLE_CMD, ioaddr);
MOD_INC_USE_COUNT;
return 0;
}
static int
eepro_send_packet(struct sk_buff *skb, struct device *dev)
{
struct eepro_local *lp = (struct eepro_local *)dev->priv;
int ioaddr = dev->base_addr;
int rcv_ram = dev->mem_end;
if (net_debug > 5)
printk("eepro: entering eepro_send_packet routine.\n");
if (dev->tbusy) {
int tickssofar = jiffies - dev->trans_start;
if (tickssofar < 40)
return 1;
if (net_debug > 1)
printk("%s: transmit timed out, %s?\n", dev->name,
"network cable problem");
lp->stats.tx_errors++;
outb(SEL_RESET_CMD, ioaddr);
SLOW_DOWN_IO;
SLOW_DOWN_IO;
lp->tx_start = lp->tx_end = rcv_ram;
lp->tx_last = 0;
dev->tbusy=0;
dev->trans_start = jiffies;
outb(RCV_ENABLE_CMD, ioaddr);
}
if (skb == NULL) {
dev_tint(dev);
return 0;
}
if (set_bit(0, (void*)&dev->tbusy) != 0)
printk("%s: Transmitter access conflict.\n", dev->name);
else {
short length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
unsigned char *buf = skb->data;
hardware_send_packet(dev, buf, length);
dev->trans_start = jiffies;
}
dev_kfree_skb (skb, FREE_WRITE);
if (net_debug > 5)
printk("eepro: exiting eepro_send_packet routine.\n");
return 0;
}
static void
eepro_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
struct device *dev = (struct device *)(irq2dev_map[irq]);
int ioaddr, status, boguscount = 20;
if (net_debug > 5)
printk("eepro: entering eepro_interrupt routine.\n");
if (dev == NULL) {
printk ("eepro_interrupt(): irq %d for unknown device.\n", irq);
return;
}
dev->interrupt = 1;
ioaddr = dev->base_addr;
do {
status = inb(ioaddr + STATUS_REG);
if (status & RX_INT) {
if (net_debug > 4)
printk("eepro: packet received interrupt.\n");
outb(RX_INT, ioaddr + STATUS_REG);
eepro_rx(dev);
}
else if (status & TX_INT) {
if (net_debug > 4)
printk("eepro: packet transmit interrupt.\n");
outb(TX_INT, ioaddr + STATUS_REG);
eepro_transmit_interrupt(dev);
}
} while ((boguscount-- > 0) && (status & 0x06));
dev->interrupt = 0;
if (net_debug > 5)
printk("eepro: exiting eepro_interrupt routine.\n");
return;
}
static int
eepro_close(struct device *dev)
{
struct eepro_local *lp = (struct eepro_local *)dev->priv;
int ioaddr = dev->base_addr;
int rcv_ram = dev->mem_end;
short temp_reg;
dev->tbusy = 1;
dev->start = 0;
outb(BANK1_SELECT, ioaddr);
temp_reg = inb(ioaddr + REG1);
outb(temp_reg & 0x7f, ioaddr + REG1);
outb(BANK0_SELECT, ioaddr);
outb(STOP_RCV_CMD, ioaddr);
lp->tx_start = lp->tx_end = rcv_ram ;
lp->tx_last = 0;
outb(ALL_MASK, ioaddr + INT_MASK_REG);
outb(ALL_MASK, ioaddr + STATUS_REG);
outb(RESET_CMD, ioaddr);
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = 0;
SLOW_DOWN_IO;
SLOW_DOWN_IO;
MOD_DEC_USE_COUNT;
return 0;
}
static struct enet_statistics *
eepro_get_stats(struct device *dev)
{
struct eepro_local *lp = (struct eepro_local *)dev->priv;
return &lp->stats;
}
static void
set_multicast_list(struct device *dev)
{
struct eepro_local *lp = (struct eepro_local *)dev->priv;
short ioaddr = dev->base_addr;
unsigned short mode;
struct dev_mc_list *dmi=dev->mc_list;
if (dev->flags&(IFF_ALLMULTI|IFF_PROMISC) || dev->mc_count > 63)
{
dev->flags|=IFF_PROMISC;
outb(BANK2_SELECT, ioaddr);
mode = inb(ioaddr + REG2);
outb(mode | PRMSC_Mode, ioaddr + REG2);
mode = inb(ioaddr + REG3);
outb(mode, ioaddr + REG3);
outb(BANK0_SELECT, ioaddr);
printk("%s: promiscuous mode enabled.\n", dev->name);
}
else if (dev->mc_count==0 )
{
outb(BANK2_SELECT, ioaddr);
mode = inb(ioaddr + REG2);
outb(mode & 0xd6, ioaddr + REG2);
mode = inb(ioaddr + REG3);
outb(mode, ioaddr + REG3);
outb(BANK0_SELECT, ioaddr);
}
else
{
unsigned short status, *eaddrs;
int i, boguscount = 0;
outb(ALL_MASK, ioaddr + INT_MASK_REG);
outb(BANK2_SELECT, ioaddr);
mode = inb(ioaddr + REG2);
outb(mode | Multi_IA, ioaddr + REG2);
mode = inb(ioaddr + REG3);
outb(mode, ioaddr + REG3);
outb(BANK0_SELECT, ioaddr);
outw(lp->tx_end, ioaddr + HOST_ADDRESS_REG);
outw(MC_SETUP, ioaddr + IO_PORT);
outw(0, ioaddr + IO_PORT);
outw(0, ioaddr + IO_PORT);
outw(6*(dev->mc_count + 1), ioaddr + IO_PORT);
for (i = 0; i < dev->mc_count; i++)
{
eaddrs=(unsigned short *)dmi->dmi_addr;
dmi=dmi->next;
outw(*eaddrs++, ioaddr + IO_PORT);
outw(*eaddrs++, ioaddr + IO_PORT);
outw(*eaddrs++, ioaddr + IO_PORT);
}
eaddrs = (unsigned short *) dev->dev_addr;
outw(eaddrs[0], ioaddr + IO_PORT);
outw(eaddrs[1], ioaddr + IO_PORT);
outw(eaddrs[2], ioaddr + IO_PORT);
outw(lp->tx_end, ioaddr + XMT_BAR);
outb(MC_SETUP, ioaddr);
i = lp->tx_end + XMT_HEADER + 6*(dev->mc_count + 1);
if (lp->tx_start != lp->tx_end)
{
outw(lp->tx_last + XMT_CHAIN, ioaddr + HOST_ADDRESS_REG);
outw(i, ioaddr + IO_PORT);
outw(lp->tx_last + XMT_COUNT, ioaddr + HOST_ADDRESS_REG);
status = inw(ioaddr + IO_PORT);
outw(status | CHAIN_BIT, ioaddr + IO_PORT);
lp->tx_end = i ;
}
else {
lp->tx_start = lp->tx_end = i ;
}
do {
SLOW_DOWN_IO;
SLOW_DOWN_IO;
if (inb(ioaddr + STATUS_REG) & 0x08)
{
i = inb(ioaddr);
outb(0x08, ioaddr + STATUS_REG);
if (i & 0x20) {
printk("%s: multicast setup failed.\n",
dev->name);
break;
} else if ((i & 0x0f) == 0x03) {
printk("%s: set Rx mode to %d addresses.\n",
dev->name, dev->mc_count);
break;
}
}
} while (++boguscount < 100);
outb(ALL_MASK & ~(RX_MASK | TX_MASK), ioaddr + INT_MASK_REG);
}
outb(RCV_ENABLE_CMD, ioaddr);
}
#define eeprom_delay() { int _i = 40; while (--_i > 0) { __SLOW_DOWN_IO; }}
#define EE_READ_CMD (6 << 6)
int
read_eeprom(int ioaddr, int location)
{
int i;
unsigned short retval = 0;
short ee_addr = ioaddr + EEPROM_REG;
int read_cmd = location | EE_READ_CMD;
short ctrl_val = EECS ;
outb(BANK2_SELECT, ioaddr);
outb(ctrl_val, ee_addr);
for (i = 8; i >= 0; i--) {
short outval = (read_cmd & (1 << i)) ? ctrl_val | EEDI
: ctrl_val;
outb(outval, ee_addr);
outb(outval | EESK, ee_addr);
eeprom_delay();
outb(outval, ee_addr);
eeprom_delay();
}
outb(ctrl_val, ee_addr);
for (i = 16; i > 0; i--) {
outb(ctrl_val | EESK, ee_addr); eeprom_delay();
retval = (retval << 1) | ((inb(ee_addr) & EEDO) ? 1 : 0);
outb(ctrl_val, ee_addr); eeprom_delay();
}
ctrl_val &= ~EECS;
outb(ctrl_val | EESK, ee_addr);
eeprom_delay();
outb(ctrl_val, ee_addr);
eeprom_delay();
outb(BANK0_SELECT, ioaddr);
return retval;
}
static void
hardware_send_packet(struct device *dev, void *buf, short length)
{
struct eepro_local *lp = (struct eepro_local *)dev->priv;
short ioaddr = dev->base_addr;
int rcv_ram = dev->mem_end;
unsigned status, tx_available, last, end, boguscount = 100;
if (net_debug > 5)
printk("eepro: entering hardware_send_packet routine.\n");
while (boguscount-- > 0) {
outb(ALL_MASK, ioaddr + INT_MASK_REG);
if (dev->interrupt == 1) {
outb(ALL_MASK & ~(RX_MASK | TX_MASK), ioaddr + INT_MASK_REG);
continue;
}
if (lp->tx_end > lp->tx_start)
tx_available = XMT_RAM - (lp->tx_end - lp->tx_start);
else if (lp->tx_end < lp->tx_start)
tx_available = lp->tx_start - lp->tx_end;
else tx_available = XMT_RAM;
if (((((length + 3) >> 1) << 1) + 2*XMT_HEADER)
>= tx_available)
{
eepro_transmit_interrupt(dev);
outb(ALL_MASK & ~(RX_MASK | TX_MASK), ioaddr + INT_MASK_REG);
continue;
}
last = lp->tx_end;
end = last + (((length + 3) >> 1) << 1) + XMT_HEADER;
if (end >= RAM_SIZE) {
if ((RAM_SIZE - last) <= XMT_HEADER) {
last = rcv_ram;
end = last + (((length + 3) >> 1) << 1) + XMT_HEADER;
}
else end = rcv_ram + (end - RAM_SIZE);
}
outw(last, ioaddr + HOST_ADDRESS_REG);
outw(XMT_CMD, ioaddr + IO_PORT);
outw(0, ioaddr + IO_PORT);
outw(end, ioaddr + IO_PORT);
outw(length, ioaddr + IO_PORT);
if (lp->version == LAN595)
outsw(ioaddr + IO_PORT, buf, (length + 3) >> 1);
else {
unsigned short temp = inb(ioaddr + INT_MASK_REG);
outb(temp | IO_32_BIT, ioaddr + INT_MASK_REG);
outsl(ioaddr + IO_PORT_32_BIT, buf, (length + 3) >> 2);
outb(temp & ~(IO_32_BIT), ioaddr + INT_MASK_REG);
}
status = inw(ioaddr + IO_PORT);
if (lp->tx_start == lp->tx_end) {
outw(last, ioaddr + XMT_BAR);
outb(XMT_CMD, ioaddr);
lp->tx_start = last;
}
else {
if (lp->tx_end != last) {
outw(lp->tx_last + XMT_CHAIN, ioaddr + HOST_ADDRESS_REG);
outw(last, ioaddr + IO_PORT);
}
outw(lp->tx_last + XMT_COUNT, ioaddr + HOST_ADDRESS_REG);
status = inw(ioaddr + IO_PORT);
outw(status | CHAIN_BIT, ioaddr + IO_PORT);
outb(RESUME_XMT_CMD, ioaddr);
}
lp->tx_last = last;
lp->tx_end = end;
outb(ALL_MASK & ~(RX_MASK | TX_MASK), ioaddr + INT_MASK_REG);
if (dev->tbusy) {
dev->tbusy = 0;
}
if (net_debug > 5)
printk("eepro: exiting hardware_send_packet routine.\n");
return;
}
dev->tbusy = 1;
if (net_debug > 5)
printk("eepro: exiting hardware_send_packet routine.\n");
}
static void
eepro_rx(struct device *dev)
{
struct eepro_local *lp = (struct eepro_local *)dev->priv;
short ioaddr = dev->base_addr, rcv_ram = dev->mem_end;
short boguscount = 20;
short rcv_car = lp->rx_start;
unsigned rcv_event, rcv_status, rcv_next_frame, rcv_size;
if (net_debug > 5)
printk("eepro: entering eepro_rx routine.\n");
outw(rcv_car, ioaddr + HOST_ADDRESS_REG);
rcv_event = inw(ioaddr + IO_PORT);
while (rcv_event == RCV_DONE) {
rcv_status = inw(ioaddr + IO_PORT);
rcv_next_frame = inw(ioaddr + IO_PORT);
rcv_size = inw(ioaddr + IO_PORT);
if ((rcv_status & (RX_OK | RX_ERROR)) == RX_OK) {
struct sk_buff *skb;
rcv_size &= 0x3fff;
skb = dev_alloc_skb(rcv_size+5);
if (skb == NULL) {
printk("%s: Memory squeeze, dropping packet.\n", dev->name);
lp->stats.rx_dropped++;
break;
}
skb->dev = dev;
skb_reserve(skb,2);
if (lp->version == LAN595)
insw(ioaddr+IO_PORT, skb_put(skb,rcv_size), (rcv_size + 3) >> 1);
else {
unsigned short temp = inb(ioaddr + INT_MASK_REG);
outb(temp | IO_32_BIT, ioaddr + INT_MASK_REG);
insl(ioaddr+IO_PORT_32_BIT, skb_put(skb,rcv_size), (rcv_size + 3) >> 2);
outb(temp & ~(IO_32_BIT), ioaddr + INT_MASK_REG);
}
skb->protocol = eth_type_trans(skb,dev);
netif_rx(skb);
lp->stats.rx_packets++;
}
else {
lp->stats.rx_errors++;
if (rcv_status & 0x0100)
lp->stats.rx_over_errors++;
else if (rcv_status & 0x0400)
lp->stats.rx_frame_errors++;
else if (rcv_status & 0x0800)
lp->stats.rx_crc_errors++;
printk("%s: event = %#x, status = %#x, next = %#x, size = %#x\n",
dev->name, rcv_event, rcv_status, rcv_next_frame, rcv_size);
}
if (rcv_status & 0x1000)
lp->stats.rx_length_errors++;
if (--boguscount == 0)
break;
rcv_car = lp->rx_start + RCV_HEADER + rcv_size;
lp->rx_start = rcv_next_frame;
outw(rcv_next_frame, ioaddr + HOST_ADDRESS_REG);
rcv_event = inw(ioaddr + IO_PORT);
}
if (rcv_car == 0)
rcv_car = (RCV_UPPER_LIMIT << 8) | 0xff;
outw(rcv_car - 1, ioaddr + RCV_STOP);
if (net_debug > 5)
printk("eepro: exiting eepro_rx routine.\n");
}
static void
eepro_transmit_interrupt(struct device *dev)
{
struct eepro_local *lp = (struct eepro_local *)dev->priv;
short ioaddr = dev->base_addr;
short boguscount = 20;
short xmt_status;
while (lp->tx_start != lp->tx_end) {
outw(lp->tx_start, ioaddr + HOST_ADDRESS_REG);
xmt_status = inw(ioaddr+IO_PORT);
if ((xmt_status & TX_DONE_BIT) == 0) break;
xmt_status = inw(ioaddr+IO_PORT);
lp->tx_start = inw(ioaddr+IO_PORT);
dev->tbusy = 0;
mark_bh(NET_BH);
if (xmt_status & 0x2000)
lp->stats.tx_packets++;
else {
lp->stats.tx_errors++;
if (xmt_status & 0x0400)
lp->stats.tx_carrier_errors++;
printk("%s: XMT status = %#x\n",
dev->name, xmt_status);
}
if (xmt_status & 0x000f) {
lp->stats.collisions += (xmt_status & 0x000f);
}
if ((xmt_status & 0x0040) == 0x0) {
lp->stats.tx_heartbeat_errors++;
}
if (--boguscount == 0)
break;
}
}
#ifdef MODULE
static char devicename[9] = { 0, };
static struct device dev_eepro = {
devicename,
0, 0, 0, 0,
0, 0,
0, 0, 0, NULL, eepro_probe };
static int io = 0x200;
static int irq = 0;
static int mem = (RCV_RAM/1024);
int
init_module(void)
{
if (io == 0)
printk("eepro: You should not use auto-probing with insmod!\n");
dev_eepro.base_addr = io;
dev_eepro.irq = irq;
dev_eepro.mem_end = mem;
if (register_netdev(&dev_eepro) != 0)
return -EIO;
return 0;
}
void
cleanup_module(void)
{
unregister_netdev(&dev_eepro);
kfree_s(dev_eepro.priv,sizeof(struct eepro_local));
dev_eepro.priv=NULL;
release_region(dev_eepro.base_addr, EEPRO_IO_EXTENT);
}
#endif