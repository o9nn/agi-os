#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/slab.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/delay.h>
#include <linux/spinlock.h>
#include <asm/io.h>
#include <asm/system.h>
#include <asm/byteorder.h>
#include <asm/uaccess.h>
#include <linux/netdevice.h>
#include "ax8390.h"
#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/ciscode.h>
#include <pcmcia/ds.h>
#include <pcmcia/cisreg.h>
#define AXNET_CMD 0x00
#define AXNET_DATAPORT 0x10
#define AXNET_RESET 0x1f
#define AXNET_MII_EEP 0x14
#define AXNET_TEST 0x15
#define AXNET_GPIO 0x17
#define AXNET_START_PG 0x40
#define AXNET_STOP_PG 0x80
#define AXNET_RDC_TIMEOUT 0x02
#define IS_AX88190 0x0001
#define IS_AX88790 0x0002
MODULE_AUTHOR("David Hinds <dahinds@users.sourceforge.net>");
MODULE_DESCRIPTION("Asix AX88190 PCMCIA ethernet driver");
MODULE_LICENSE("GPL");
#define INT_MODULE_PARM(n, v) static int n = v; MODULE_PARM(n, "i")
INT_MODULE_PARM(irq_mask, 0xdeb8);
static int irq_list[4] = { -1 };
MODULE_PARM(irq_list, "1-4i");
#ifdef PCMCIA_DEBUG
INT_MODULE_PARM(pc_debug, PCMCIA_DEBUG);
#define DEBUG(n, args...) if (pc_debug>(n)) printk(KERN_DEBUG args)
static char *version =
"axnet_cs.c 1.31 2003/08/25 15:57:40 (David Hinds)";
#else
#define DEBUG(n, args...)
#endif
static void axnet_config(dev_link_t *link);
static void axnet_release(u_long arg);
static int axnet_event(event_t event, int priority,
event_callback_args_t *args);
static int axnet_open(struct net_device *dev);
static int axnet_close(struct net_device *dev);
static int axnet_ioctl(struct net_device *dev, struct ifreq *rq, int cmd);
static void ei_irq_wrapper(int irq, void *dev_id, struct pt_regs *regs);
static void ei_watchdog(u_long arg);
static void axnet_reset_8390(struct net_device *dev);
static int mdio_read(ioaddr_t addr, int phy_id, int loc);
static void mdio_write(ioaddr_t addr, int phy_id, int loc, int value);
static void get_8390_hdr(struct net_device *,
struct e8390_pkt_hdr *, int);
static void block_input(struct net_device *dev, int count,
struct sk_buff *skb, int ring_offset);
static void block_output(struct net_device *dev, int count,
const u_char *buf, const int start_page);
static dev_link_t *axnet_attach(void);
static void axnet_detach(dev_link_t *);
static dev_info_t dev_info = "axnet_cs";
static dev_link_t *dev_list;
static int axdev_init(struct net_device *dev);
static void AX88190_init(struct net_device *dev, int startp);
static int ax_open(struct net_device *dev);
static int ax_close(struct net_device *dev);
static void ax_interrupt(int irq, void *dev_id, struct pt_regs *regs);
typedef struct axnet_dev_t {
struct net_device dev;
dev_link_t link;
dev_node_t node;
caddr_t base;
struct timer_list watchdog;
int stale, fast_poll;
u_short link_status;
u_char duplex_flag;
int phy_id;
int flags;
} axnet_dev_t;
static void flush_stale_links(void)
{
dev_link_t *link, *next;
for (link = dev_list; link; link = next) {
next = link->next;
if (link->state & DEV_STALE_LINK)
axnet_detach(link);
}
}
static void cs_error(client_handle_t handle, int func, int ret)
{
error_info_t err = { func, ret };
CardServices(ReportError, handle, &err);
}
static int axnet_init(struct net_device *dev)
{
return 0;
}
static dev_link_t *axnet_attach(void)
{
axnet_dev_t *info;
dev_link_t *link;
struct net_device *dev;
client_reg_t client_reg;
int i, ret;
DEBUG(0, "axnet_attach()\n");
flush_stale_links();
info = kmalloc(sizeof(*info), GFP_KERNEL);
if (!info) return NULL;
memset(info, 0, sizeof(*info));
link = &info->link; dev = &info->dev;
link->priv = info;
init_timer(&link->release);
link->release.function = &axnet_release;
link->release.data = (u_long)link;
link->irq.Attributes = IRQ_TYPE_EXCLUSIVE;
link->irq.IRQInfo1 = IRQ_INFO2_VALID|IRQ_LEVEL_ID;
if (irq_list[0] == -1)
link->irq.IRQInfo2 = irq_mask;
else
for (i = 0; i < 4; i++)
link->irq.IRQInfo2 |= 1 << irq_list[i];
link->conf.Attributes = CONF_ENABLE_IRQ;
link->conf.IntType = INT_MEMORY_AND_IO;
axdev_init(dev);
init_dev_name(dev, info->node);
dev->init = &axnet_init;
dev->open = &axnet_open;
dev->stop = &axnet_close;
dev->do_ioctl = &axnet_ioctl;
link->next = dev_list;
dev_list = link;
client_reg.dev_info = &dev_info;
client_reg.Attributes = INFO_IO_CLIENT | INFO_CARD_SHARE;
client_reg.EventMask =
CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET |
CS_EVENT_PM_SUSPEND | CS_EVENT_PM_RESUME;
client_reg.event_handler = &axnet_event;
client_reg.Version = 0x0210;
client_reg.event_callback_args.client_data = link;
ret = CardServices(RegisterClient, &link->handle, &client_reg);
if (ret != CS_SUCCESS) {
cs_error(link->handle, RegisterClient, ret);
axnet_detach(link);
return NULL;
}
return link;
}
static void axnet_detach(dev_link_t *link)
{
axnet_dev_t *info = link->priv;
dev_link_t **linkp;
DEBUG(0, "axnet_detach(0x%p)\n", link);
for (linkp = &dev_list; *linkp; linkp = &(*linkp)->next)
if (*linkp == link) break;
if (*linkp == NULL)
return;
del_timer(&link->release);
if (link->state & DEV_CONFIG) {
axnet_release((u_long)link);
if (link->state & DEV_STALE_CONFIG) {
link->state |= DEV_STALE_LINK;
return;
}
}
if (link->handle)
CardServices(DeregisterClient, link->handle);
*linkp = link->next;
if (link->dev)
unregister_netdev(&info->dev);
kfree(info);
}
static int get_prom(dev_link_t *link)
{
struct net_device *dev = link->priv;
ioaddr_t ioaddr = dev->base_addr;
int i, j;
struct {
u_char value, offset;
} program_seq[] = {
{E8390_NODMA+E8390_PAGE0+E8390_STOP, E8390_CMD},
{0x01, EN0_DCFG},
{0x00, EN0_RCNTLO},
{0x00, EN0_RCNTHI},
{0x00, EN0_IMR},
{0xFF, EN0_ISR},
{E8390_RXOFF|0x40, EN0_RXCR},
{E8390_TXOFF, EN0_TXCR},
{0x10, EN0_RCNTLO},
{0x00, EN0_RCNTHI},
{0x00, EN0_RSARLO},
{0x04, EN0_RSARHI},
{E8390_RREAD+E8390_START, E8390_CMD},
};
if (link->conf.ConfigBase != 0x03c0)
return 0;
axnet_reset_8390(dev);
mdelay(10);
for (i = 0; i < sizeof(program_seq)/sizeof(program_seq[0]); i++)
outb_p(program_seq[i].value, ioaddr + program_seq[i].offset);
for (i = 0; i < 6; i += 2) {
j = inw(ioaddr + AXNET_DATAPORT);
dev->dev_addr[i] = j & 0xff;
dev->dev_addr[i+1] = j >> 8;
}
return 1;
}
#define CS_CHECK(fn, args...) \
while ((last_ret=CardServices(last_fn=(fn), args))!=0) goto cs_failed
#define CFG_CHECK(fn, args...) \
if (CardServices(fn, args) != 0) goto next_entry
static int try_io_port(dev_link_t *link)
{
int j, ret;
if (link->io.NumPorts1 == 32) {
link->io.Attributes1 = IO_DATA_PATH_WIDTH_AUTO;
if (link->io.NumPorts2 > 0) {
link->io.Attributes2 = IO_DATA_PATH_WIDTH_8;
link->irq.Attributes =
IRQ_TYPE_DYNAMIC_SHARING|IRQ_FIRST_SHARED;
}
} else {
link->io.Attributes1 = IO_DATA_PATH_WIDTH_8;
link->io.Attributes2 = IO_DATA_PATH_WIDTH_16;
}
if (link->io.BasePort1 == 0) {
link->io.IOAddrLines = 16;
for (j = 0; j < 0x400; j += 0x20) {
link->io.BasePort1 = j ^ 0x300;
link->io.BasePort2 = (j ^ 0x300) + 0x10;
ret = CardServices(RequestIO, link->handle, &link->io);
if (ret == CS_SUCCESS) return ret;
}
return ret;
} else {
return CardServices(RequestIO, link->handle, &link->io);
}
}
static void axnet_config(dev_link_t *link)
{
client_handle_t handle = link->handle;
axnet_dev_t *info = link->priv;
struct net_device *dev = &info->dev;
tuple_t tuple;
cisparse_t parse;
int i, j, last_ret, last_fn;
u_short buf[64];
config_info_t conf;
DEBUG(0, "axnet_config(0x%p)\n", link);
tuple.Attributes = 0;
tuple.TupleData = (cisdata_t *)buf;
tuple.TupleDataMax = sizeof(buf);
tuple.TupleOffset = 0;
tuple.DesiredTuple = CISTPL_CONFIG;
CS_CHECK(GetFirstTuple, handle, &tuple);
CS_CHECK(GetTupleData, handle, &tuple);
CS_CHECK(ParseTuple, handle, &tuple, &parse);
link->conf.ConfigBase = parse.config.base;
link->conf.Present = 0x63;
link->state |= DEV_CONFIG;
CS_CHECK(GetConfigurationInfo, handle, &conf);
link->conf.Vcc = conf.Vcc;
tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;
tuple.Attributes = 0;
CS_CHECK(GetFirstTuple, handle, &tuple);
while (last_ret == CS_SUCCESS) {
cistpl_cftable_entry_t *cfg = &(parse.cftable_entry);
cistpl_io_t *io = &(parse.cftable_entry.io);
CFG_CHECK(GetTupleData, handle, &tuple);
CFG_CHECK(ParseTuple, handle, &tuple, &parse);
if ((cfg->index == 0) || (cfg->io.nwin == 0))
goto next_entry;
link->conf.ConfigIndex = 0x05;
if (io->nwin > 1) {
i = (io->win[1].len > io->win[0].len);
link->io.BasePort2 = io->win[1-i].base;
link->io.NumPorts2 = io->win[1-i].len;
} else {
i = link->io.NumPorts2 = 0;
}
link->io.BasePort1 = io->win[i].base;
link->io.NumPorts1 = io->win[i].len;
link->io.IOAddrLines = io->flags & CISTPL_IO_LINES_MASK;
if (link->io.NumPorts1 + link->io.NumPorts2 >= 32) {
last_ret = try_io_port(link);
if (last_ret == CS_SUCCESS) break;
}
next_entry:
last_ret = CardServices(GetNextTuple, handle, &tuple);
}
if (last_ret != CS_SUCCESS) {
cs_error(handle, RequestIO, last_ret);
goto failed;
}
CS_CHECK(RequestIRQ, handle, &link->irq);
if (link->io.NumPorts2 == 8) {
link->conf.Attributes |= CONF_ENABLE_SPKR;
link->conf.Status = CCSR_AUDIO_ENA;
}
CS_CHECK(RequestConfiguration, handle, &link->conf);
dev->irq = link->irq.AssignedIRQ;
dev->base_addr = link->io.BasePort1;
if (register_netdev(dev) != 0) {
printk(KERN_NOTICE "axnet_cs: register_netdev() failed\n");
goto failed;
}
if (!get_prom(link)) {
printk(KERN_NOTICE "axnet_cs: this is not an AX88190 card!\n");
printk(KERN_NOTICE "axnet_cs: use pcnet_cs instead.\n");
unregister_netdev(dev);
goto failed;
}
ei_status.name = "AX88190";
ei_status.word16 = 1;
ei_status.tx_start_page = AXNET_START_PG;
ei_status.rx_start_page = AXNET_START_PG + TX_PAGES;
ei_status.stop_page = AXNET_STOP_PG;
ei_status.reset_8390 = &axnet_reset_8390;
ei_status.get_8390_hdr = &get_8390_hdr;
ei_status.block_input = &block_input;
ei_status.block_output = &block_output;
copy_dev_name(info->node, dev);
link->dev = &info->node;
if (inb(dev->base_addr + AXNET_TEST) != 0)
info->flags |= IS_AX88790;
else
info->flags |= IS_AX88190;
printk(KERN_INFO "%s: Asix AX88%d90: io %#3lx, irq %d, hw_addr ",
dev->name, ((info->flags & IS_AX88790) ? 7 : 1),
dev->base_addr, dev->irq);
for (i = 0; i < 6; i++)
printk("%02X%s", dev->dev_addr[i], ((i<5) ? ":" : "\n"));
if (info->flags & IS_AX88790)
outb(0x10, dev->base_addr + AXNET_GPIO);
for (i = 0; i < 32; i++) {
j = mdio_read(dev->base_addr + AXNET_MII_EEP, i, 1);
if ((j != 0) && (j != 0xffff)) break;
}
if (i == 32) {
conf_reg_t reg = { 0, CS_WRITE, CISREG_CCSR, 0x04 };
CardServices(AccessConfigurationRegister, link->handle, &reg);
for (i = 0; i < 32; i++) {
j = mdio_read(dev->base_addr + AXNET_MII_EEP, i, 1);
if ((j != 0) && (j != 0xffff)) break;
}
}
info->phy_id = (i < 32) ? i : -1;
if (i < 32) {
DEBUG(0, "  MII transceiver at index %d, status %x.\n", i, j);
} else {
printk(KERN_NOTICE "  No MII transceivers found!\n");
}
link->state &= ~DEV_CONFIG_PENDING;
return;
cs_failed:
cs_error(link->handle, last_fn, last_ret);
failed:
axnet_release((u_long)link);
link->state &= ~DEV_CONFIG_PENDING;
return;
}
static void axnet_release(u_long arg)
{
dev_link_t *link = (dev_link_t *)arg;
DEBUG(0, "axnet_release(0x%p)\n", link);
if (link->open) {
DEBUG(1, "axnet_cs: release postponed, '%s' still open\n",
((axnet_dev_t *)(link->priv))->node.dev_name);
link->state |= DEV_STALE_CONFIG;
return;
}
CardServices(ReleaseConfiguration, link->handle);
CardServices(ReleaseIO, link->handle, &link->io);
CardServices(ReleaseIRQ, link->handle, &link->irq);
link->state &= ~DEV_CONFIG;
}
static int axnet_event(event_t event, int priority,
event_callback_args_t *args)
{
dev_link_t *link = args->client_data;
axnet_dev_t *info = link->priv;
DEBUG(2, "axnet_event(0x%06x)\n", event);
switch (event) {
case CS_EVENT_CARD_REMOVAL:
link->state &= ~DEV_PRESENT;
if (link->state & DEV_CONFIG) {
netif_device_detach(&info->dev);
mod_timer(&link->release, jiffies + HZ/20);
}
break;
case CS_EVENT_CARD_INSERTION:
link->state |= DEV_PRESENT | DEV_CONFIG_PENDING;
axnet_config(link);
break;
case CS_EVENT_PM_SUSPEND:
link->state |= DEV_SUSPEND;
case CS_EVENT_RESET_PHYSICAL:
if (link->state & DEV_CONFIG) {
if (link->open)
netif_device_detach(&info->dev);
CardServices(ReleaseConfiguration, link->handle);
}
break;
case CS_EVENT_PM_RESUME:
link->state &= ~DEV_SUSPEND;
case CS_EVENT_CARD_RESET:
if (link->state & DEV_CONFIG) {
CardServices(RequestConfiguration, link->handle, &link->conf);
if (link->open) {
axnet_reset_8390(&info->dev);
AX88190_init(&info->dev, 1);
netif_device_attach(&info->dev);
}
}
break;
}
return 0;
}
#define MDIO_SHIFT_CLK 0x01
#define MDIO_DATA_WRITE0 0x00
#define MDIO_DATA_WRITE1 0x08
#define MDIO_DATA_READ 0x04
#define MDIO_MASK 0x0f
#define MDIO_ENB_IN 0x02
static void mdio_sync(ioaddr_t addr)
{
int bits;
for (bits = 0; bits < 32; bits++) {
outb_p(MDIO_DATA_WRITE1, addr);
outb_p(MDIO_DATA_WRITE1 | MDIO_SHIFT_CLK, addr);
}
}
static int mdio_read(ioaddr_t addr, int phy_id, int loc)
{
u_int cmd = (0xf6<<10)|(phy_id<<5)|loc;
int i, retval = 0;
mdio_sync(addr);
for (i = 14; i >= 0; i--) {
int dat = (cmd&(1<<i)) ? MDIO_DATA_WRITE1 : MDIO_DATA_WRITE0;
outb_p(dat, addr);
outb_p(dat | MDIO_SHIFT_CLK, addr);
}
for (i = 19; i > 0; i--) {
outb_p(MDIO_ENB_IN, addr);
retval = (retval << 1) | ((inb_p(addr) & MDIO_DATA_READ) != 0);
outb_p(MDIO_ENB_IN | MDIO_SHIFT_CLK, addr);
}
return (retval>>1) & 0xffff;
}
static void mdio_write(ioaddr_t addr, int phy_id, int loc, int value)
{
u_int cmd = (0x05<<28)|(phy_id<<23)|(loc<<18)|(1<<17)|value;
int i;
mdio_sync(addr);
for (i = 31; i >= 0; i--) {
int dat = (cmd&(1<<i)) ? MDIO_DATA_WRITE1 : MDIO_DATA_WRITE0;
outb_p(dat, addr);
outb_p(dat | MDIO_SHIFT_CLK, addr);
}
for (i = 1; i >= 0; i--) {
outb_p(MDIO_ENB_IN, addr);
outb_p(MDIO_ENB_IN | MDIO_SHIFT_CLK, addr);
}
}
static int axnet_open(struct net_device *dev)
{
axnet_dev_t *info = (axnet_dev_t *)dev;
dev_link_t *link = &info->link;
DEBUG(2, "axnet_open('%s')\n", dev->name);
if (!DEV_OK(link))
return -ENODEV;
link->open++;
MOD_INC_USE_COUNT;
request_irq(dev->irq, ei_irq_wrapper, SA_SHIRQ, dev_info, dev);
info->link_status = 0x00;
info->watchdog.function = &ei_watchdog;
info->watchdog.data = (u_long)info;
info->watchdog.expires = jiffies + HZ;
add_timer(&info->watchdog);
return ax_open(dev);
}
static int axnet_close(struct net_device *dev)
{
axnet_dev_t *info = (axnet_dev_t *)dev;
dev_link_t *link = &info->link;
DEBUG(2, "axnet_close('%s')\n", dev->name);
ax_close(dev);
free_irq(dev->irq, dev);
link->open--;
netif_stop_queue(dev);
netif_mark_down(dev);
del_timer(&info->watchdog);
if (link->state & DEV_STALE_CONFIG)
mod_timer(&link->release, jiffies + HZ/20);
MOD_DEC_USE_COUNT;
return 0;
}
static void axnet_reset_8390(struct net_device *dev)
{
ioaddr_t nic_base = dev->base_addr;
int i;
ei_status.txing = ei_status.dmaing = 0;
outb_p(E8390_NODMA+E8390_PAGE0+E8390_STOP, nic_base + E8390_CMD);
outb(inb(nic_base + AXNET_RESET), nic_base + AXNET_RESET);
for (i = 0; i < 100; i++) {
if ((inb_p(nic_base+EN0_ISR) & ENISR_RESET) != 0)
break;
udelay(100);
}
outb_p(ENISR_RESET, nic_base + EN0_ISR);
if (i == 100)
printk(KERN_ERR "%s: axnet_reset_8390() did not complete.\n",
dev->name);
}
static void ei_irq_wrapper(int irq, void *dev_id, struct pt_regs *regs)
{
axnet_dev_t *info = dev_id;
info->stale = 0;
ax_interrupt(irq, dev_id, regs);
}
static void ei_watchdog(u_long arg)
{
axnet_dev_t *info = (axnet_dev_t *)(arg);
struct net_device *dev = &info->dev;
ioaddr_t nic_base = dev->base_addr;
ioaddr_t mii_addr = nic_base + AXNET_MII_EEP;
u_short link;
if (!netif_device_present(dev)) goto reschedule;
if (info->stale++ && (inb_p(nic_base + EN0_ISR) & ENISR_ALL)) {
if (!info->fast_poll)
printk(KERN_INFO "%s: interrupt(s) dropped!\n", dev->name);
ei_irq_wrapper(dev->irq, dev, NULL);
info->fast_poll = HZ;
}
if (info->fast_poll) {
info->fast_poll--;
info->watchdog.expires = jiffies + 1;
add_timer(&info->watchdog);
return;
}
if (info->phy_id < 0)
goto reschedule;
link = mdio_read(mii_addr, info->phy_id, 1);
if (!link || (link == 0xffff)) {
printk(KERN_INFO "%s: MII is missing!\n", dev->name);
info->phy_id = -1;
goto reschedule;
}
link &= 0x0004;
if (link != info->link_status) {
u_short p = mdio_read(mii_addr, info->phy_id, 5);
printk(KERN_INFO "%s: %s link beat\n", dev->name,
(link) ? "found" : "lost");
if (link) {
info->duplex_flag = (p & 0x0140) ? 0x80 : 0x00;
if (p)
printk(KERN_INFO "%s: autonegotiation complete: "
"%sbaseT-%cD selected\n", dev->name,
((p & 0x0180) ? "100" : "10"),
((p & 0x0140) ? 'F' : 'H'));
else
printk(KERN_INFO "%s: link partner did not autonegotiate\n",
dev->name);
AX88190_init(dev, 1);
}
info->link_status = link;
}
reschedule:
info->watchdog.expires = jiffies + HZ;
add_timer(&info->watchdog);
}
static int axnet_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
axnet_dev_t *info = (axnet_dev_t *)dev;
u16 *data = (u16 *)&rq->ifr_data;
ioaddr_t mii_addr = dev->base_addr + AXNET_MII_EEP;
switch (cmd) {
case SIOCDEVPRIVATE:
data[0] = info->phy_id;
case SIOCDEVPRIVATE+1:
data[3] = mdio_read(mii_addr, data[0], data[1] & 0x1f);
return 0;
case SIOCDEVPRIVATE+2:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
mdio_write(mii_addr, data[0], data[1] & 0x1f, data[2]);
return 0;
}
return -EOPNOTSUPP;
}
static void get_8390_hdr(struct net_device *dev,
struct e8390_pkt_hdr *hdr,
int ring_page)
{
ioaddr_t nic_base = dev->base_addr;
outb_p(0, nic_base + EN0_RSARLO);
outb_p(ring_page, nic_base + EN0_RSARHI);
outb_p(E8390_RREAD+E8390_START, nic_base + AXNET_CMD);
insw(nic_base + AXNET_DATAPORT, hdr,
sizeof(struct e8390_pkt_hdr)>>1);
hdr->count = le16_to_cpu(hdr->count);
}
static void block_input(struct net_device *dev, int count,
struct sk_buff *skb, int ring_offset)
{
ioaddr_t nic_base = dev->base_addr;
int xfer_count = count;
char *buf = skb->data;
#ifdef PCMCIA_DEBUG
if ((ei_debug > 4) && (count != 4))
printk(KERN_DEBUG "%s: [bi=%d]\n", dev->name, count+4);
#endif
outb_p(ring_offset & 0xff, nic_base + EN0_RSARLO);
outb_p(ring_offset >> 8, nic_base + EN0_RSARHI);
outb_p(E8390_RREAD+E8390_START, nic_base + AXNET_CMD);
insw(nic_base + AXNET_DATAPORT,buf,count>>1);
if (count & 0x01)
buf[count-1] = inb(nic_base + AXNET_DATAPORT), xfer_count++;
}
static void block_output(struct net_device *dev, int count,
const u_char *buf, const int start_page)
{
ioaddr_t nic_base = dev->base_addr;
#ifdef PCMCIA_DEBUG
if (ei_debug > 4)
printk(KERN_DEBUG "%s: [bo=%d]\n", dev->name, count);
#endif
if (count & 0x01)
count++;
outb_p(0x00, nic_base + EN0_RSARLO);
outb_p(start_page, nic_base + EN0_RSARHI);
outb_p(E8390_RWRITE+E8390_START, nic_base + AXNET_CMD);
outsw(nic_base + AXNET_DATAPORT, buf, count>>1);
}
static int __init init_axnet_cs(void)
{
servinfo_t serv;
DEBUG(0, "%s\n", version);
CardServices(GetCardServicesInfo, &serv);
if (serv.Revision != CS_RELEASE_CODE) {
printk(KERN_NOTICE "axnet_cs: Card Services release "
"does not match!\n");
return -EINVAL;
}
register_pccard_driver(&dev_info, &axnet_attach, &axnet_detach);
return 0;
}
static void __exit exit_axnet_cs(void)
{
DEBUG(0, "axnet_cs: unloading\n");
unregister_pccard_driver(&dev_info);
while (dev_list != NULL)
axnet_detach(dev_list);
}
module_init(init_axnet_cs);
module_exit(exit_axnet_cs);
static const char *version_8390 =
"8390.c:v1.10cvs 9/23/94 Donald Becker (becker@scyld.com)\n";
#include <asm/uaccess.h>
#include <asm/bitops.h>
#include <asm/irq.h>
#include <linux/fcntl.h>
#include <linux/in.h>
#include <linux/interrupt.h>
#include <linux/etherdevice.h>
#define BUG_83C690
#define ei_reset_8390 (ei_local->reset_8390)
#define ei_block_output (ei_local->block_output)
#define ei_block_input (ei_local->block_input)
#define ei_get_8390_hdr (ei_local->get_8390_hdr)
#ifndef ei_debug
int ei_debug = 1;
#endif
static void ei_tx_intr(struct net_device *dev);
static void ei_tx_err(struct net_device *dev);
static void ei_tx_timeout(struct net_device *dev);
static void ei_receive(struct net_device *dev);
static void ei_rx_overrun(struct net_device *dev);
static void NS8390_trigger_send(struct net_device *dev, unsigned int length,
int start_page);
static void set_multicast_list(struct net_device *dev);
static void do_set_multicast_list(struct net_device *dev);
static int ax_open(struct net_device *dev)
{
unsigned long flags;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
if (ei_local == NULL)
{
printk(KERN_EMERG "%s: ax_open passed a non-existent device!\n", dev->name);
return -ENXIO;
}
#ifdef HAVE_TX_TIMEOUT
if (dev->tx_timeout == NULL)
dev->tx_timeout = ei_tx_timeout;
if (dev->watchdog_timeo <= 0)
dev->watchdog_timeo = TX_TIMEOUT;
#endif
spin_lock_irqsave(&ei_local->page_lock, flags);
AX88190_init(dev, 1);
netif_mark_up(dev);
netif_start_queue(dev);
spin_unlock_irqrestore(&ei_local->page_lock, flags);
ei_local->irqlock = 0;
return 0;
}
#define dev_lock(dev) (((struct ei_device *)(dev)->priv)->page_lock)
int ax_close(struct net_device *dev)
{
unsigned long flags;
spin_lock_irqsave(&dev_lock(dev), flags);
AX88190_init(dev, 0);
spin_unlock_irqrestore(&dev_lock(dev), flags);
netif_stop_queue(dev);
return 0;
}
void ei_tx_timeout(struct net_device *dev)
{
long e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
int txsr, isr, tickssofar = jiffies - dev->trans_start;
unsigned long flags;
ei_local->stat.tx_errors++;
spin_lock_irqsave(&ei_local->page_lock, flags);
txsr = inb(e8390_base+EN0_TSR);
isr = inb(e8390_base+EN0_ISR);
spin_unlock_irqrestore(&ei_local->page_lock, flags);
printk(KERN_DEBUG "%s: Tx timed out, %s TSR=%#2x, ISR=%#2x, t=%d.\n",
dev->name, (txsr & ENTSR_ABT) ? "excess collisions." :
(isr) ? "lost interrupt?" : "cable problem?", txsr, isr, tickssofar);
if (!isr && !ei_local->stat.tx_packets)
{
ei_local->interface_num ^= 1;
}
disable_irq_nosync(dev->irq);
spin_lock(&ei_local->page_lock);
ei_reset_8390(dev);
AX88190_init(dev, 1);
spin_unlock(&ei_local->page_lock);
enable_irq(dev->irq);
netif_wake_queue(dev);
}
static int ei_start_xmit(struct sk_buff *skb, struct net_device *dev)
{
long e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
int length, send_length, output_page;
unsigned long flags;
tx_timeout_check(dev, ei_tx_timeout);
skb_tx_check(dev, skb);
length = skb->len;
spin_lock_irqsave(&ei_local->page_lock, flags);
outb_p(0x00, e8390_base + EN0_IMR);
spin_unlock_irqrestore(&ei_local->page_lock, flags);
disable_irq_nosync(dev->irq);
spin_lock(&ei_local->page_lock);
ei_local->irqlock = 1;
send_length = ETH_ZLEN < length ? length : ETH_ZLEN;
if (ei_local->tx1 == 0)
{
output_page = ei_local->tx_start_page;
ei_local->tx1 = send_length;
if (ei_debug && ei_local->tx2 > 0)
printk(KERN_DEBUG "%s: idle transmitter tx2=%d, lasttx=%d, txing=%d.\n",
dev->name, ei_local->tx2, ei_local->lasttx, ei_local->txing);
}
else if (ei_local->tx2 == 0)
{
output_page = ei_local->tx_start_page + TX_1X_PAGES;
ei_local->tx2 = send_length;
if (ei_debug && ei_local->tx1 > 0)
printk(KERN_DEBUG "%s: idle transmitter, tx1=%d, lasttx=%d, txing=%d.\n",
dev->name, ei_local->tx1, ei_local->lasttx, ei_local->txing);
}
else
{
if (ei_debug)
printk(KERN_DEBUG "%s: No Tx buffers free! tx1=%d tx2=%d last=%d\n",
dev->name, ei_local->tx1, ei_local->tx2, ei_local->lasttx);
ei_local->irqlock = 0;
netif_stop_queue(dev);
outb_p(ENISR_ALL, e8390_base + EN0_IMR);
spin_unlock(&ei_local->page_lock);
enable_irq(dev->irq);
ei_local->stat.tx_errors++;
return 1;
}
ei_block_output(dev, length, skb->data, output_page);
if (! ei_local->txing)
{
ei_local->txing = 1;
NS8390_trigger_send(dev, send_length, output_page);
dev->trans_start = jiffies;
if (output_page == ei_local->tx_start_page)
{
ei_local->tx1 = -1;
ei_local->lasttx = -1;
}
else
{
ei_local->tx2 = -1;
ei_local->lasttx = -2;
}
}
else ei_local->txqueue++;
if (ei_local->tx1 && ei_local->tx2)
netif_stop_queue(dev);
else
netif_start_queue(dev);
ei_local->irqlock = 0;
outb_p(ENISR_ALL, e8390_base + EN0_IMR);
spin_unlock(&ei_local->page_lock);
enable_irq(dev->irq);
DEV_KFREE_SKB (skb);
add_tx_bytes(&ei_local->stat, send_length);
return 0;
}
static void ax_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
struct net_device *dev = dev_id;
long e8390_base;
int interrupts, nr_serviced = 0, i;
struct ei_device *ei_local;
if (dev == NULL)
{
printk ("net_interrupt(): irq %d for unknown device.\n", irq);
return;
}
e8390_base = dev->base_addr;
ei_local = (struct ei_device *) dev->priv;
spin_lock(&ei_local->page_lock);
if (ei_local->irqlock)
{
#if 1
printk(ei_local->irqlock
? "%s: Interrupted while interrupts are masked! isr=%#2x imr=%#2x.\n"
: "%s: Reentering the interrupt handler! isr=%#2x imr=%#2x.\n",
dev->name, inb_p(e8390_base + EN0_ISR),
inb_p(e8390_base + EN0_IMR));
#endif
spin_unlock(&ei_local->page_lock);
return;
}
if (ei_debug > 3)
printk(KERN_DEBUG "%s: interrupt(isr=%#2.2x).\n", dev->name,
inb_p(e8390_base + EN0_ISR));
outb_p(0x00, e8390_base + EN0_ISR);
ei_local->irqlock = 1;
while ((interrupts = inb_p(e8390_base + EN0_ISR)) != 0
&& ++nr_serviced < MAX_SERVICE)
{
if (!netif_running(dev) || (interrupts == 0xff)) {
if (ei_debug > 1)
printk(KERN_WARNING "%s: interrupt from stopped card\n", dev->name);
outb_p(interrupts, e8390_base + EN0_ISR);
interrupts = 0;
break;
}
outb_p(interrupts, e8390_base + EN0_ISR);
for (i = 0; i < 10; i++) {
if (!(inb(e8390_base + EN0_ISR) & interrupts))
break;
outb_p(0, e8390_base + EN0_ISR);
outb_p(interrupts, e8390_base + EN0_ISR);
}
if (interrupts & ENISR_OVER)
ei_rx_overrun(dev);
else if (interrupts & (ENISR_RX+ENISR_RX_ERR))
{
ei_receive(dev);
}
if (interrupts & ENISR_TX)
ei_tx_intr(dev);
else if (interrupts & ENISR_TX_ERR)
ei_tx_err(dev);
if (interrupts & ENISR_COUNTERS)
{
ei_local->stat.rx_frame_errors += inb_p(e8390_base + EN0_COUNTER0);
ei_local->stat.rx_crc_errors += inb_p(e8390_base + EN0_COUNTER1);
ei_local->stat.rx_missed_errors+= inb_p(e8390_base + EN0_COUNTER2);
}
}
if (interrupts && ei_debug)
{
if (nr_serviced >= MAX_SERVICE)
{
if(interrupts!=0xFF)
printk(KERN_WARNING "%s: Too much work at interrupt, status %#2.2x\n",
dev->name, interrupts);
outb_p(ENISR_ALL, e8390_base + EN0_ISR);
} else {
printk(KERN_WARNING "%s: unknown interrupt %#2x\n", dev->name, interrupts);
outb_p(0xff, e8390_base + EN0_ISR);
}
}
ei_local->irqlock = 0;
outb_p(ENISR_ALL, e8390_base + EN0_IMR);
spin_unlock(&ei_local->page_lock);
return;
}
static void ei_tx_err(struct net_device *dev)
{
long e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
unsigned char txsr = inb_p(e8390_base+EN0_TSR);
unsigned char tx_was_aborted = txsr & (ENTSR_ABT+ENTSR_FU);
#ifdef VERBOSE_ERROR_DUMP
printk(KERN_DEBUG "%s: transmitter error (%#2x): ", dev->name, txsr);
if (txsr & ENTSR_ABT)
printk("excess-collisions ");
if (txsr & ENTSR_ND)
printk("non-deferral ");
if (txsr & ENTSR_CRS)
printk("lost-carrier ");
if (txsr & ENTSR_FU)
printk("FIFO-underrun ");
if (txsr & ENTSR_CDH)
printk("lost-heartbeat ");
printk("\n");
#endif
if (tx_was_aborted)
ei_tx_intr(dev);
else
{
ei_local->stat.tx_errors++;
if (txsr & ENTSR_CRS) ei_local->stat.tx_carrier_errors++;
if (txsr & ENTSR_CDH) ei_local->stat.tx_heartbeat_errors++;
if (txsr & ENTSR_OWC) ei_local->stat.tx_window_errors++;
}
}
static void ei_tx_intr(struct net_device *dev)
{
long e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
int status = inb(e8390_base + EN0_TSR);
ei_local->txqueue--;
if (ei_local->tx1 < 0)
{
if (ei_local->lasttx != 1 && ei_local->lasttx != -1)
printk(KERN_ERR "%s: bogus last_tx_buffer %d, tx1=%d.\n",
ei_local->name, ei_local->lasttx, ei_local->tx1);
ei_local->tx1 = 0;
if (ei_local->tx2 > 0)
{
ei_local->txing = 1;
NS8390_trigger_send(dev, ei_local->tx2, ei_local->tx_start_page + 6);
dev->trans_start = jiffies;
ei_local->tx2 = -1,
ei_local->lasttx = 2;
}
else ei_local->lasttx = 20, ei_local->txing = 0;
}
else if (ei_local->tx2 < 0)
{
if (ei_local->lasttx != 2 && ei_local->lasttx != -2)
printk("%s: bogus last_tx_buffer %d, tx2=%d.\n",
ei_local->name, ei_local->lasttx, ei_local->tx2);
ei_local->tx2 = 0;
if (ei_local->tx1 > 0)
{
ei_local->txing = 1;
NS8390_trigger_send(dev, ei_local->tx1, ei_local->tx_start_page);
dev->trans_start = jiffies;
ei_local->tx1 = -1;
ei_local->lasttx = 1;
}
else
ei_local->lasttx = 10, ei_local->txing = 0;
}
if (status & ENTSR_COL)
ei_local->stat.collisions++;
if (status & ENTSR_PTX)
ei_local->stat.tx_packets++;
else
{
ei_local->stat.tx_errors++;
if (status & ENTSR_ABT)
{
ei_local->stat.tx_aborted_errors++;
ei_local->stat.collisions += 16;
}
if (status & ENTSR_CRS)
ei_local->stat.tx_carrier_errors++;
if (status & ENTSR_FU)
ei_local->stat.tx_fifo_errors++;
if (status & ENTSR_CDH)
ei_local->stat.tx_heartbeat_errors++;
if (status & ENTSR_OWC)
ei_local->stat.tx_window_errors++;
}
netif_wake_queue(dev);
}
static void ei_receive(struct net_device *dev)
{
long e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
unsigned char rxing_page, this_frame, next_frame;
unsigned short current_offset;
int rx_pkt_count = 0;
struct e8390_pkt_hdr rx_frame;
while (++rx_pkt_count < 10)
{
int pkt_len, pkt_stat;
rxing_page = inb_p(e8390_base + EN1_CURPAG -1);
this_frame = inb_p(e8390_base + EN0_BOUNDARY) + 1;
if (this_frame >= ei_local->stop_page)
this_frame = ei_local->rx_start_page;
if (ei_debug > 0 && this_frame != ei_local->current_page && (this_frame!=0x0 || rxing_page!=0xFF))
printk(KERN_ERR "%s: mismatched read page pointers %2x vs %2x.\n",
dev->name, this_frame, ei_local->current_page);
if (this_frame == rxing_page)
break;
current_offset = this_frame << 8;
ei_get_8390_hdr(dev, &rx_frame, this_frame);
pkt_len = rx_frame.count - sizeof(struct e8390_pkt_hdr);
pkt_stat = rx_frame.status;
next_frame = this_frame + 1 + ((pkt_len+4)>>8);
if (pkt_len < 60 || pkt_len > 1518)
{
if (ei_debug)
printk(KERN_DEBUG "%s: bogus packet size: %d, status=%#2x nxpg=%#2x.\n",
dev->name, rx_frame.count, rx_frame.status,
rx_frame.next);
ei_local->stat.rx_errors++;
ei_local->stat.rx_length_errors++;
}
else if ((pkt_stat & 0x0F) == ENRSR_RXOK)
{
struct sk_buff *skb;
skb = dev_alloc_skb(pkt_len+2);
if (skb == NULL)
{
if (ei_debug > 1)
printk(KERN_DEBUG "%s: Couldn't allocate a sk_buff of size %d.\n",
dev->name, pkt_len);
ei_local->stat.rx_dropped++;
break;
}
else
{
skb_reserve(skb,2);
skb->dev = dev;
skb_put(skb, pkt_len);
ei_block_input(dev, pkt_len, skb, current_offset + sizeof(rx_frame));
skb->protocol=eth_type_trans(skb,dev);
netif_rx(skb);
dev->last_rx = jiffies;
ei_local->stat.rx_packets++;
add_rx_bytes(&ei_local->stat, pkt_len);
if (pkt_stat & ENRSR_PHY)
ei_local->stat.multicast++;
}
}
else
{
if (ei_debug)
printk(KERN_DEBUG "%s: bogus packet: status=%#2x nxpg=%#2x size=%d\n",
dev->name, rx_frame.status, rx_frame.next,
rx_frame.count);
ei_local->stat.rx_errors++;
if (pkt_stat & ENRSR_FO)
ei_local->stat.rx_fifo_errors++;
}
next_frame = rx_frame.next;
if (next_frame >= ei_local->stop_page) {
printk("%s: next frame inconsistency, %#2x\n", dev->name,
next_frame);
next_frame = ei_local->rx_start_page;
}
ei_local->current_page = next_frame;
outb_p(next_frame-1, e8390_base+EN0_BOUNDARY);
}
return;
}
static void ei_rx_overrun(struct net_device *dev)
{
axnet_dev_t *info = (axnet_dev_t *)dev;
long e8390_base = dev->base_addr;
unsigned char was_txing, must_resend = 0;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
was_txing = inb_p(e8390_base+E8390_CMD) & E8390_TRANS;
outb_p(E8390_NODMA+E8390_PAGE0+E8390_STOP, e8390_base+E8390_CMD);
if (ei_debug > 1)
printk(KERN_DEBUG "%s: Receiver overrun.\n", dev->name);
ei_local->stat.rx_over_errors++;
mdelay(10);
outb_p(0x00, e8390_base+EN0_RCNTLO);
outb_p(0x00, e8390_base+EN0_RCNTHI);
if (was_txing)
{
unsigned char tx_completed = inb_p(e8390_base+EN0_ISR) & (ENISR_TX+ENISR_TX_ERR);
if (!tx_completed)
must_resend = 1;
}
outb_p(E8390_TXOFF, e8390_base + EN0_TXCR);
outb_p(E8390_NODMA + E8390_PAGE0 + E8390_START, e8390_base + E8390_CMD);
ei_receive(dev);
outb_p(E8390_TXCONFIG | info->duplex_flag, e8390_base + EN0_TXCR);
if (must_resend)
outb_p(E8390_NODMA + E8390_PAGE0 + E8390_START + E8390_TRANS, e8390_base + E8390_CMD);
}
static struct net_device_stats *get_stats(struct net_device *dev)
{
long ioaddr = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
unsigned long flags;
if (!netif_running(dev))
return &ei_local->stat;
spin_lock_irqsave(&ei_local->page_lock,flags);
ei_local->stat.rx_frame_errors += inb_p(ioaddr + EN0_COUNTER0);
ei_local->stat.rx_crc_errors += inb_p(ioaddr + EN0_COUNTER1);
ei_local->stat.rx_missed_errors+= inb_p(ioaddr + EN0_COUNTER2);
spin_unlock_irqrestore(&ei_local->page_lock, flags);
return &ei_local->stat;
}
static void do_set_multicast_list(struct net_device *dev)
{
long e8390_base = dev->base_addr;
if(dev->flags&IFF_PROMISC)
outb_p(E8390_RXCONFIG | 0x58, e8390_base + EN0_RXCR);
else if(dev->flags&IFF_ALLMULTI || dev->mc_list)
outb_p(E8390_RXCONFIG | 0x48, e8390_base + EN0_RXCR);
else
outb_p(E8390_RXCONFIG | 0x40, e8390_base + EN0_RXCR);
}
static void set_multicast_list(struct net_device *dev)
{
unsigned long flags;
spin_lock_irqsave(&dev_lock(dev), flags);
do_set_multicast_list(dev);
spin_unlock_irqrestore(&dev_lock(dev), flags);
}
static int axdev_init(struct net_device *dev)
{
if (ei_debug > 1)
printk("%s", version_8390);
if (dev->priv == NULL)
{
struct ei_device *ei_local;
dev->priv = kmalloc(sizeof(struct ei_device), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct ei_device));
ei_local = (struct ei_device *)dev->priv;
spin_lock_init(&ei_local->page_lock);
}
dev->hard_start_xmit = &ei_start_xmit;
dev->get_stats = get_stats;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
return 0;
}
static void AX88190_init(struct net_device *dev, int startp)
{
axnet_dev_t *info = (axnet_dev_t *)dev;
long e8390_base = dev->base_addr;
struct ei_device *ei_local = (struct ei_device *) dev->priv;
int i;
int endcfg = ei_local->word16 ? (0x48 | ENDCFG_WTS) : 0x48;
if(sizeof(struct e8390_pkt_hdr)!=4)
panic("8390.c: header struct mispacked\n");
outb_p(E8390_NODMA+E8390_PAGE0+E8390_STOP, e8390_base+E8390_CMD);
outb_p(endcfg, e8390_base + EN0_DCFG);
outb_p(0x00, e8390_base + EN0_RCNTLO);
outb_p(0x00, e8390_base + EN0_RCNTHI);
outb_p(E8390_RXOFF|0x40, e8390_base + EN0_RXCR);
outb_p(E8390_TXOFF, e8390_base + EN0_TXCR);
outb_p(ei_local->tx_start_page, e8390_base + EN0_TPSR);
ei_local->tx1 = ei_local->tx2 = 0;
outb_p(ei_local->rx_start_page, e8390_base + EN0_STARTPG);
outb_p(ei_local->stop_page-1, e8390_base + EN0_BOUNDARY);
ei_local->current_page = ei_local->rx_start_page;
outb_p(ei_local->stop_page, e8390_base + EN0_STOPPG);
outb_p(0xFF, e8390_base + EN0_ISR);
outb_p(0x00, e8390_base + EN0_IMR);
outb_p(E8390_NODMA + E8390_PAGE1 + E8390_STOP, e8390_base+E8390_CMD);
for(i = 0; i < 6; i++)
{
outb_p(dev->dev_addr[i], e8390_base + EN1_PHYS_SHIFT(i));
if(inb_p(e8390_base + EN1_PHYS_SHIFT(i))!=dev->dev_addr[i])
printk(KERN_ERR "Hw. address read/write mismap %d\n",i);
}
for (i = 0; i < 8; i++)
outb_p(0xff, e8390_base + EN1_MULT + i);
outb_p(ei_local->rx_start_page, e8390_base + EN1_CURPAG);
outb_p(E8390_NODMA+E8390_PAGE0+E8390_STOP, e8390_base+E8390_CMD);
netif_start_queue(dev);
ei_local->tx1 = ei_local->tx2 = 0;
ei_local->txing = 0;
if (startp)
{
outb_p(0xff, e8390_base + EN0_ISR);
outb_p(ENISR_ALL, e8390_base + EN0_IMR);
outb_p(E8390_NODMA+E8390_PAGE0+E8390_START, e8390_base+E8390_CMD);
outb_p(E8390_TXCONFIG | info->duplex_flag,
e8390_base + EN0_TXCR);
outb_p(E8390_RXCONFIG | 0x40, e8390_base + EN0_RXCR);
do_set_multicast_list(dev);
}
}
static void NS8390_trigger_send(struct net_device *dev, unsigned int length,
int start_page)
{
long e8390_base = dev->base_addr;
struct ei_device *ei_local __attribute((unused)) = (struct ei_device *) dev->priv;
if (inb_p(e8390_base) & E8390_TRANS)
{
printk(KERN_WARNING "%s: trigger_send() called with the transmitter busy.\n",
dev->name);
return;
}
outb_p(length & 0xff, e8390_base + EN0_TCNTLO);
outb_p(length >> 8, e8390_base + EN0_TCNTHI);
outb_p(start_page, e8390_base + EN0_TPSR);
outb_p(E8390_NODMA+E8390_TRANS+E8390_START, e8390_base+E8390_CMD);
}