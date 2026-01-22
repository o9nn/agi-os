#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/slab.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/interrupt.h>
#include <linux/in.h>
#include <linux/delay.h>
#include <asm/io.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/if_arp.h>
#include <linux/ioport.h>
#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/cisreg.h>
#include <pcmcia/ciscode.h>
#ifndef MANFID_COMPAQ
#define MANFID_COMPAQ 	   0x0138
#define MANFID_COMPAQ2	   0x0183
#endif
#include <pcmcia/ds.h>
#define TX_TIMEOUT	((400*HZ)/1000)
#define XIRCREG_CR  0
enum xirc_cr {
TransmitPacket = 0x01,
SoftReset = 0x02,
EnableIntr = 0x04,
ForceIntr  = 0x08,
ClearTxFIFO = 0x10,
ClearRxOvrun = 0x20,
RestartTx	 = 0x40
};
#define XIRCREG_ESR 0
enum xirc_esr {
FullPktRcvd = 0x01,
PktRejected = 0x04,
TxPktPend = 0x08,
IncorPolarity = 0x10,
MediaSelect = 0x20
};
#define XIRCREG_PR  1
#define XIRCREG_EDP 4
#define XIRCREG_ISR 6
enum xirc_isr {
TxBufOvr = 0x01,
PktTxed  = 0x02,
MACIntr  = 0x04,
TxResGrant = 0x08,
RxFullPkt = 0x20,
RxPktRej  = 0x40,
ForcedIntr= 0x80
};
#define XIRCREG1_IMR0 12
#define XIRCREG1_IMR1 13
#define XIRCREG0_TSO  8
#define XIRCREG0_TRS  10
#define XIRCREG0_DO   12
#define XIRCREG0_RSR  12
enum xirc_rsr {
PhyPkt = 0x01,
BrdcstPkt = 0x02,
PktTooLong = 0x04,
AlignErr = 0x10,
CRCErr = 0x20,
PktRxOk = 0x80
};
#define XIRCREG0_PTR 13
#define XIRCREG0_RBC 14
#define XIRCREG1_ECR 14
enum xirc_ecr {
FullDuplex = 0x04,
LongTPMode = 0x08,
DisablePolCor = 0x10,
DisableLinkPulse = 0x20,
DisableAutoTx = 0x40,
};
#define XIRCREG2_RBS 8
#define XIRCREG2_LED 10
#define XIRCREG2_MSR 12
#define XIRCREG4_GPR0 8
#define XIRCREG4_GPR1 9
#define XIRCREG2_GPR2 13
#define XIRCREG4_BOV 10
#define XIRCREG4_LMA 12
#define XIRCREG4_LMD 14
#define XIRCREG40_CMD0 8
enum xirc_cmd {
Transmit = 0x01,
EnableRecv = 0x04,
DisableRecv = 0x08,
Abort = 0x10,
Online = 0x20,
IntrAck = 0x40,
Offline = 0x80
};
#define XIRCREG5_RHSA0	10
#define XIRCREG40_RXST0 9
#define XIRCREG40_TXST0 11
#define XIRCREG40_TXST1 12
#define XIRCREG40_RMASK0 13
#define XIRCREG40_TMASK0 14
#define XIRCREG40_TMASK1 15
#define XIRCREG42_SWC0	8
#define XIRCREG42_SWC1	9
#define XIRCREG42_BOC	10
#define XIRCREG44_TDR0	8
#define XIRCREG44_TDR1	9
#define XIRCREG44_RXBC_LO 10
#define XIRCREG44_RXBC_HI 11
#define XIRCREG45_REV	 15
#define XIRCREG50_IA	8
static char *if_names[] = { "Auto", "10BaseT", "10Base2", "AUI", "100BaseT" };
#ifdef PCMCIA_DEBUG
static int pc_debug = PCMCIA_DEBUG;
MODULE_PARM(pc_debug, "i");
#define DEBUG(n, args...) if (pc_debug>(n)) printk(KDBG_XIRC args)
#else
#define DEBUG(n, args...)
#endif
static char *version =
"xirc2ps_cs.c 1.31 1998/12/09 19:32:55 (dd9jn+kvh)";
#define KDBG_XIRC KERN_DEBUG   "xirc2ps_cs: "
#define KERR_XIRC KERN_ERR     "xirc2ps_cs: "
#define KWRN_XIRC KERN_WARNING "xirc2ps_cs: "
#define KNOT_XIRC KERN_NOTICE  "xirc2ps_cs: "
#define KINF_XIRC KERN_INFO    "xirc2ps_cs: "
#define XIR_UNKNOWN  0
#define XIR_CE	     1
#define XIR_CE2      2
#define XIR_CE3      3
#define XIR_CEM      4
#define XIR_CEM2     5
#define XIR_CEM3     6
#define XIR_CEM33    7
#define XIR_CEM56M   8
#define XIR_CEM56    9
#define XIR_CM28    10
#define XIR_CM33    11
#define XIR_CM56    12
#define XIR_CG	    13
#define XIR_CBE     14
MODULE_DESCRIPTION("Xircom PCMCIA ethernet driver");
MODULE_LICENSE("Dual MPL/GPL");
#define INT_MODULE_PARM(n, v) static int n = v; MODULE_PARM(n, "i")
static int irq_list[4] = { -1 };
MODULE_PARM(irq_list, "1-4i");
INT_MODULE_PARM(irq_mask,	0xdeb8);
INT_MODULE_PARM(if_port,	0);
INT_MODULE_PARM(full_duplex,	0);
INT_MODULE_PARM(do_sound, 	1);
INT_MODULE_PARM(lockup_hack,	0);
static unsigned maxrx_bytes = 22000;
static void mii_idle(ioaddr_t ioaddr);
static void mii_putbit(ioaddr_t ioaddr, unsigned data);
static int  mii_getbit(ioaddr_t ioaddr);
static void mii_wbits(ioaddr_t ioaddr, unsigned data, int len);
static unsigned mii_rd(ioaddr_t ioaddr,	u_char phyaddr, u_char phyreg);
static void mii_wr(ioaddr_t ioaddr, u_char phyaddr, u_char phyreg,
unsigned data, int len);
static int has_ce2_string(dev_link_t * link);
static void xirc2ps_config(dev_link_t * link);
static void xirc2ps_release(u_long arg);
static int xirc2ps_event(event_t event, int priority,
event_callback_args_t * args);
static dev_link_t *xirc2ps_attach(void);
static void xirc2ps_detach(dev_link_t *);
static void xirc2ps_interrupt(int irq, void *dev_id, struct pt_regs *regs);
static dev_info_t dev_info = "xirc2ps_cs";
static dev_link_t *dev_list = NULL;
typedef struct local_info_t {
dev_link_t link;
struct net_device dev;
dev_node_t node;
struct net_device_stats stats;
int card_type;
int probe_port;
int silicon;
int mohawk;
int dingo;
int new_mii;
int modem;
caddr_t dingo_ccr;
unsigned last_ptr_value;
const char *manf_str;
} local_info_t;
static int do_start_xmit(struct sk_buff *skb, struct net_device *dev);
static void do_tx_timeout(struct net_device *dev);
static struct net_device_stats *do_get_stats(struct net_device *dev);
static void set_addresses(struct net_device *dev);
static void set_multicast_list(struct net_device *dev);
static int set_card_type(dev_link_t *link, const void *s);
static int do_config(struct net_device *dev, struct ifmap *map);
static int do_open(struct net_device *dev);
static int do_ioctl(struct net_device *dev, struct ifreq *rq, int cmd);
static void hardreset(struct net_device *dev);
static void do_reset(struct net_device *dev, int full);
static int init_mii(struct net_device *dev);
static void do_powerdown(struct net_device *dev);
static int do_stop(struct net_device *dev);
static void
flush_stale_links(void)
{
dev_link_t *link, *next;
for (link = dev_list; link; link = next) {
next = link->next;
if (link->state & DEV_STALE_LINK)
xirc2ps_detach(link);
}
}
static void
cs_error(client_handle_t handle, int func, int ret)
{
error_info_t err = { func, ret };
CardServices(ReportError, handle, &err);
}
static int
get_tuple_data(int fn, client_handle_t handle, tuple_t *tuple)
{
int err;
if ((err=CardServices(fn, handle, tuple)))
return err;
return CardServices(GetTupleData, handle, tuple);
}
static int
get_tuple(int fn, client_handle_t handle, tuple_t *tuple, cisparse_t *parse)
{
int err;
if ((err=get_tuple_data(fn, handle, tuple)))
return err;
return CardServices(ParseTuple, handle, tuple, parse);
}
#define first_tuple(a, b, c) get_tuple(GetFirstTuple, a, b, c)
#define next_tuple(a, b, c)  get_tuple(GetNextTuple, a, b, c)
#define SelectPage(pgnr)   outb((pgnr), ioaddr + XIRCREG_PR)
#define GetByte(reg)	   ((unsigned)inb(ioaddr + (reg)))
#define GetWord(reg)	   ((unsigned)inw(ioaddr + (reg)))
#define PutByte(reg,value) outb((value), ioaddr+(reg))
#define PutWord(reg,value) outw((value), ioaddr+(reg))
static void
busy_loop(u_long len)
{
#ifdef MACH
__udelay(1000000 / HZ *  len);
#else
if (in_interrupt()) {
u_long timeout = jiffies + len;
u_long flags;
save_flags(flags);
sti();
while (timeout >= jiffies)
;
restore_flags(flags);
} else {
__set_current_state(TASK_UNINTERRUPTIBLE);
schedule_timeout(len);
}
#endif
}
#if defined(PCMCIA_DEBUG) && 0
static void
PrintRegisters(struct net_device *dev)
{
ioaddr_t ioaddr = dev->base_addr;
if (pc_debug > 1) {
int i, page;
printk(KDBG_XIRC "Register  common: ");
for (i = 0; i < 8; i++)
printk(" %2.2x", GetByte(i));
printk("\n");
for (page = 0; page <= 8; page++) {
printk(KDBG_XIRC "Register page %2x: ", page);
SelectPage(page);
for (i = 8; i < 16; i++)
printk(" %2.2x", GetByte(i));
printk("\n");
}
for (page=0x40 ; page <= 0x5f; page++) {
if (page == 0x43 || (page >= 0x46 && page <= 0x4f)
|| (page >= 0x51 && page <=0x5e))
continue;
printk(KDBG_XIRC "Register page %2x: ", page);
SelectPage(page);
for (i = 8; i < 16; i++)
printk(" %2.2x", GetByte(i));
printk("\n");
}
}
}
#endif
static void
mii_idle(ioaddr_t ioaddr)
{
PutByte(XIRCREG2_GPR2, 0x04|0);
udelay(1);
PutByte(XIRCREG2_GPR2, 0x04|1);
udelay(1);
}
static void
mii_putbit(ioaddr_t ioaddr, unsigned data)
{
#if 1
if (data) {
PutByte(XIRCREG2_GPR2, 0x0c|2|0);
udelay(1);
PutByte(XIRCREG2_GPR2, 0x0c|2|1);
udelay(1);
} else {
PutByte(XIRCREG2_GPR2, 0x0c|0|0);
udelay(1);
PutByte(XIRCREG2_GPR2, 0x0c|0|1);
udelay(1);
}
#else
if (data) {
PutWord(XIRCREG2_GPR2-1, 0x0e0e);
udelay(1);
PutWord(XIRCREG2_GPR2-1, 0x0f0f);
udelay(1);
} else {
PutWord(XIRCREG2_GPR2-1, 0x0c0c);
udelay(1);
PutWord(XIRCREG2_GPR2-1, 0x0d0d);
udelay(1);
}
#endif
}
static int
mii_getbit(ioaddr_t ioaddr)
{
unsigned d;
PutByte(XIRCREG2_GPR2, 4|0);
udelay(1);
d = GetByte(XIRCREG2_GPR2);
PutByte(XIRCREG2_GPR2, 4|1);
udelay(1);
return d & 0x20;
}
static void
mii_wbits(ioaddr_t ioaddr, unsigned data, int len)
{
unsigned m = 1 << (len-1);
for (; m; m >>= 1)
mii_putbit(ioaddr, data & m);
}
static unsigned
mii_rd(ioaddr_t ioaddr,	u_char phyaddr, u_char phyreg)
{
int i;
unsigned data=0, m;
SelectPage(2);
for (i=0; i < 32; i++)
mii_putbit(ioaddr, 1);
mii_wbits(ioaddr, 0x06, 4);
mii_wbits(ioaddr, phyaddr, 5);
mii_wbits(ioaddr, phyreg, 5);
mii_idle(ioaddr);
mii_getbit(ioaddr);
for (m = 1<<15; m; m >>= 1)
if (mii_getbit(ioaddr))
data |= m;
mii_idle(ioaddr);
return data;
}
static void
mii_wr(ioaddr_t ioaddr, u_char phyaddr, u_char phyreg, unsigned data, int len)
{
int i;
SelectPage(2);
for (i=0; i < 32; i++)
mii_putbit(ioaddr, 1);
mii_wbits(ioaddr, 0x05, 4);
mii_wbits(ioaddr, phyaddr, 5);
mii_wbits(ioaddr, phyreg, 5);
mii_putbit(ioaddr, 1);
mii_putbit(ioaddr, 0);
mii_wbits(ioaddr, data, len);
mii_idle(ioaddr);
}
static dev_link_t *
xirc2ps_attach(void)
{
client_reg_t client_reg;
dev_link_t *link;
struct net_device *dev;
local_info_t *local;
int err;
DEBUG(0, "attach()\n");
flush_stale_links();
local = kmalloc(sizeof(*local), GFP_KERNEL);
if (!local) return NULL;
memset(local, 0, sizeof(*local));
link = &local->link; dev = &local->dev;
link->priv = dev->priv = local;
init_timer(&link->release);
link->release.function = &xirc2ps_release;
link->release.data = (u_long) link;
link->conf.Attributes = CONF_ENABLE_IRQ;
link->conf.Vcc = 50;
link->conf.IntType = INT_MEMORY_AND_IO;
link->conf.ConfigIndex = 1;
link->conf.Present = PRESENT_OPTION;
link->irq.Handler = xirc2ps_interrupt;
link->irq.Instance = dev;
dev->hard_start_xmit = &do_start_xmit;
dev->set_config = &do_config;
dev->get_stats = &do_get_stats;
dev->do_ioctl = &do_ioctl;
dev->set_multicast_list = &set_multicast_list;
ether_setup(dev);
init_dev_name(dev, local->node);
dev->open = &do_open;
dev->stop = &do_stop;
#ifdef HAVE_TX_TIMEOUT
dev->tx_timeout = do_tx_timeout;
dev->watchdog_timeo = TX_TIMEOUT;
#endif
link->next = dev_list;
dev_list = link;
client_reg.dev_info = &dev_info;
client_reg.Attributes = INFO_IO_CLIENT | INFO_CARD_SHARE;
client_reg.EventMask =
CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET |
CS_EVENT_PM_SUSPEND | CS_EVENT_PM_RESUME;
client_reg.event_handler = &xirc2ps_event;
client_reg.Version = 0x0210;
client_reg.event_callback_args.client_data = link;
if ((err = CardServices(RegisterClient, &link->handle, &client_reg))) {
cs_error(link->handle, RegisterClient, err);
xirc2ps_detach(link);
return NULL;
}
return link;
}
static void
xirc2ps_detach(dev_link_t * link)
{
local_info_t *local = link->priv;
dev_link_t **linkp;
DEBUG(0, "detach(0x%p)\n", link);
for (linkp = &dev_list; *linkp; linkp = &(*linkp)->next)
if (*linkp == link)
break;
if (!*linkp) {
DEBUG(0, "detach(0x%p): dev_link lost\n", link);
return;
}
del_timer(&link->release);
if (link->state & DEV_CONFIG) {
DEBUG(0, "detach postponed, '%s' still locked\n",
link->dev->dev_name);
link->state |= DEV_STALE_LINK;
return;
}
if (link->handle)
CardServices(DeregisterClient, link->handle);
*linkp = link->next;
if (link->dev)
unregister_netdev(&local->dev);
kfree(local);
}
static int
set_card_type(dev_link_t *link, const void *s)
{
local_info_t *local = link->priv;
#ifdef PCMCIA_DEBUG
unsigned cisrev = ((const unsigned char *)s)[2];
#endif
unsigned mediaid= ((const unsigned char *)s)[3];
unsigned prodid = ((const unsigned char *)s)[4];
DEBUG(0, "cisrev=%02x mediaid=%02x prodid=%02x\n",
cisrev, mediaid, prodid);
local->mohawk = 0;
local->dingo = 0;
local->modem = 0;
local->card_type = XIR_UNKNOWN;
if (!(prodid & 0x40)) {
printk(KNOT_XIRC "Ooops: Not a creditcard\n");
return 0;
}
if (!(mediaid & 0x01)) {
printk(KNOT_XIRC "Not an Ethernet card\n");
return 0;
}
if (mediaid & 0x10) {
local->modem = 1;
switch(prodid & 15) {
case 1: local->card_type = XIR_CEM   ; break;
case 2: local->card_type = XIR_CEM2  ; break;
case 3: local->card_type = XIR_CEM3  ; break;
case 4: local->card_type = XIR_CEM33 ; break;
case 5: local->card_type = XIR_CEM56M;
local->mohawk = 1;
break;
case 6:
case 7:
local->card_type = XIR_CEM56 ;
local->mohawk = 1;
local->dingo = 1;
break;
}
} else {
switch(prodid & 15) {
case 1: local->card_type = has_ce2_string(link)? XIR_CE2 : XIR_CE ;
break;
case 15:
case 2: local->card_type = XIR_CE2; break;
case 3: local->card_type = XIR_CE3;
local->mohawk = 1;
break;
}
}
if (local->card_type == XIR_CE || local->card_type == XIR_CEM) {
printk(KNOT_XIRC "Sorry, this is an old CE card\n");
return 0;
}
if (local->card_type == XIR_UNKNOWN)
printk(KNOT_XIRC "unknown card (mediaid=%02x prodid=%02x)\n",
mediaid, prodid);
return 1;
}
static int
has_ce2_string(dev_link_t * link)
{
client_handle_t handle = link->handle;
tuple_t tuple;
cisparse_t parse;
u_char buf[256];
tuple.Attributes = 0;
tuple.TupleData = buf;
tuple.TupleDataMax = 254;
tuple.TupleOffset = 0;
tuple.DesiredTuple = CISTPL_VERS_1;
if (!first_tuple(handle, &tuple, &parse) && parse.version_1.ns > 2) {
if (strstr(parse.version_1.str + parse.version_1.ofs[2], "CE2"))
return 1;
}
return 0;
}
static void
xirc2ps_config(dev_link_t * link)
{
client_handle_t handle = link->handle;
local_info_t *local = link->priv;
struct net_device *dev = &local->dev;
tuple_t tuple;
cisparse_t parse;
ioaddr_t ioaddr;
int err, i;
u_char buf[64];
cistpl_lan_node_id_t *node_id = (cistpl_lan_node_id_t*)parse.funce.data;
cistpl_cftable_entry_t *cf = &parse.cftable_entry;
local->dingo_ccr = 0;
DEBUG(0, "config(0x%p)\n", link);
tuple.Attributes = 0;
tuple.TupleData = buf;
tuple.TupleDataMax = 64;
tuple.TupleOffset = 0;
tuple.DesiredTuple = CISTPL_MANFID;
if ((err=first_tuple(handle, &tuple, &parse))) {
printk(KNOT_XIRC "manfid not found in CIS\n");
goto failure;
}
switch(parse.manfid.manf) {
case MANFID_XIRCOM:
local->manf_str = "Xircom";
break;
case MANFID_ACCTON:
local->manf_str = "Accton";
break;
case MANFID_COMPAQ:
case MANFID_COMPAQ2:
local->manf_str = "Compaq";
break;
case MANFID_INTEL:
local->manf_str = "Intel";
break;
case MANFID_TOSHIBA:
local->manf_str = "Toshiba";
break;
default:
printk(KNOT_XIRC "Unknown Card Manufacturer ID: 0x%04x\n",
(unsigned)parse.manfid.manf);
goto failure;
}
DEBUG(0, "found %s card\n", local->manf_str);
if (!set_card_type(link, buf)) {
printk(KNOT_XIRC "this card is not supported\n");
goto failure;
}
tuple.DesiredTuple = CISTPL_CONFIG;
if ((err=first_tuple(handle, &tuple, &parse)))
goto cis_error;
link->conf.ConfigBase = parse.config.base;
link->conf.Present =    parse.config.rmask[0];
tuple.DesiredTuple = CISTPL_FUNCE;
for (err = first_tuple(handle, &tuple, &parse); !err;
err = next_tuple(handle, &tuple, &parse)) {
if (parse.funce.type == CISTPL_FUNCE_LAN_NODE_ID
&& ((cistpl_lan_node_id_t *)parse.funce.data)->nb)
break;
}
if (err) {
tuple.DesiredTuple = 0x89;
if (!(err = get_tuple_data(GetFirstTuple, handle, &tuple))) {
if (tuple.TupleDataLen == 8 && *buf == CISTPL_FUNCE_LAN_NODE_ID)
memcpy(&parse, buf, 8);
else
err = -1;
}
}
if (err) {
tuple.DesiredTuple = CISTPL_FUNCE;
for (err = first_tuple(handle, &tuple, &parse); !err;
err = next_tuple(handle, &tuple, &parse)) {
if (parse.funce.type == 0x02 && parse.funce.data[0] == 1
&& parse.funce.data[1] == 6 && tuple.TupleDataLen == 13) {
buf[1] = 4;
memcpy(&parse, buf+1, 8);
break;
}
}
}
if (err) {
printk(KNOT_XIRC "node-id not found in CIS\n");
goto failure;
}
node_id = (cistpl_lan_node_id_t *)parse.funce.data;
if (node_id->nb != 6) {
printk(KNOT_XIRC "malformed node-id in CIS\n");
goto failure;
}
for (i=0; i < 6; i++)
dev->dev_addr[i] = node_id->id[i];
link->state |= DEV_CONFIG;
link->io.IOAddrLines =10;
link->io.Attributes1 = IO_DATA_PATH_WIDTH_16;
link->irq.Attributes = IRQ_HANDLE_PRESENT;
link->irq.IRQInfo1 = IRQ_INFO2_VALID | IRQ_LEVEL_ID;
if (irq_list[0] == -1)
link->irq.IRQInfo2 = irq_mask;
else {
for (i = 0; i < 4; i++)
link->irq.IRQInfo2 |= 1 << irq_list[i];
}
if (local->modem) {
int pass;
if (do_sound) {
link->conf.Attributes |= CONF_ENABLE_SPKR;
link->conf.Status |= CCSR_AUDIO_ENA;
}
link->irq.Attributes |= IRQ_TYPE_DYNAMIC_SHARING|IRQ_FIRST_SHARED ;
link->io.NumPorts2 = 8;
link->io.Attributes2 = IO_DATA_PATH_WIDTH_8;
if (local->dingo) {
link->io.NumPorts1 = 16;
tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;
for (err = first_tuple(handle, &tuple, &parse); !err;
err = next_tuple(handle, &tuple, &parse)) {
if (cf->io.nwin > 0  &&  (cf->io.win[0].base & 0xf) == 8) {
for (ioaddr = 0x300; ioaddr < 0x400; ioaddr += 0x10) {
link->conf.ConfigIndex = cf->index ;
link->io.BasePort2 = cf->io.win[0].base;
link->io.BasePort1 = ioaddr;
if (!(err=CardServices(RequestIO, link->handle,
&link->io)))
goto port_found;
}
}
}
} else {
link->io.NumPorts1 = 18;
for (pass=0; pass < 2; pass++) {
tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;
for (err = first_tuple(handle, &tuple, &parse); !err;
err = next_tuple(handle, &tuple, &parse)){
if (cf->io.nwin > 0  &&  (cf->io.win[0].base & 0xf) == 8){
link->conf.ConfigIndex = cf->index ;
link->io.BasePort2 = cf->io.win[0].base;
link->io.BasePort1 = link->io.BasePort2
+ (pass ? (cf->index & 0x20 ? -24:8)
: (cf->index & 0x20 ?   8:-24));
if (!(err=CardServices(RequestIO, link->handle,
&link->io)))
goto port_found;
}
}
}
}
printk(KNOT_XIRC "no ports available\n");
} else {
link->irq.Attributes |= IRQ_TYPE_EXCLUSIVE;
link->io.NumPorts1 = 16;
for (ioaddr = 0x300; ioaddr < 0x400; ioaddr += 0x10) {
link->io.BasePort1 = ioaddr;
if (!(err=CardServices(RequestIO, link->handle, &link->io)))
goto port_found;
}
link->io.BasePort1 = 0;
if ((err=CardServices(RequestIO, link->handle, &link->io))) {
cs_error(link->handle, RequestIO, err);
goto config_error;
}
}
port_found:
if (err)
goto config_error;
if ((err=CardServices(RequestIRQ, link->handle, &link->irq))) {
cs_error(link->handle, RequestIRQ, err);
goto config_error;
}
if ((err=CardServices(RequestConfiguration,
link->handle, &link->conf))) {
cs_error(link->handle, RequestConfiguration, err);
goto config_error;
}
if (local->dingo) {
conf_reg_t reg;
win_req_t req;
memreq_t mem;
reg.Action = CS_WRITE;
reg.Offset = CISREG_IOBASE_0;
reg.Value = link->io.BasePort2 & 0xff;
if ((err = CardServices(AccessConfigurationRegister, link->handle,
&reg))) {
cs_error(link->handle, AccessConfigurationRegister, err);
goto config_error;
}
reg.Action = CS_WRITE;
reg.Offset = CISREG_IOBASE_1;
reg.Value = (link->io.BasePort2 >> 8) & 0xff;
if ((err = CardServices(AccessConfigurationRegister, link->handle,
&reg))) {
cs_error(link->handle, AccessConfigurationRegister, err);
goto config_error;
}
req.Attributes = WIN_DATA_WIDTH_8|WIN_MEMORY_TYPE_AM|WIN_ENABLE;
req.Base = req.Size = 0;
req.AccessSpeed = 0;
link->win = (window_handle_t)link->handle;
if ((err = CardServices(RequestWindow, &link->win, &req))) {
cs_error(link->handle, RequestWindow, err);
goto config_error;
}
local->dingo_ccr = ioremap(req.Base,0x1000) + 0x0800;
mem.CardOffset = 0x0;
mem.Page = 0;
if ((err = CardServices(MapMemPage, link->win, &mem))) {
cs_error(link->handle, MapMemPage, err);
goto config_error;
}
writeb(0x47, local->dingo_ccr + CISREG_COR);
ioaddr = link->io.BasePort1;
writeb(ioaddr & 0xff	  , local->dingo_ccr + CISREG_IOBASE_0);
writeb((ioaddr >> 8)&0xff , local->dingo_ccr + CISREG_IOBASE_1);
#if 0
{
u_char tmp;
printk(KERN_INFO "ECOR:");
for (i=0; i < 7; i++) {
tmp = readb(local->dingo_ccr + i*2);
printk(" %02x", tmp);
}
printk("\n");
printk(KERN_INFO "DCOR:");
for (i=0; i < 4; i++) {
tmp = readb(local->dingo_ccr + 0x20 + i*2);
printk(" %02x", tmp);
}
printk("\n");
printk(KERN_INFO "SCOR:");
for (i=0; i < 10; i++) {
tmp = readb(local->dingo_ccr + 0x40 + i*2);
printk(" %02x", tmp);
}
printk("\n");
}
#endif
writeb(0x01, local->dingo_ccr + 0x20);
writeb(0x0c, local->dingo_ccr + 0x22);
writeb(0x00, local->dingo_ccr + 0x24);
writeb(0x00, local->dingo_ccr + 0x26);
writeb(0x00, local->dingo_ccr + 0x28);
}
local->probe_port=0;
if (!if_port) {
local->probe_port = dev->if_port = 1;
} else if ((if_port >= 1 && if_port <= 2) ||
(local->mohawk && if_port==4))
dev->if_port = if_port;
else
printk(KNOT_XIRC "invalid if_port requested\n");
dev->irq = link->irq.AssignedIRQ;
dev->base_addr = link->io.BasePort1;
if ((err=register_netdev(dev))) {
printk(KNOT_XIRC "register_netdev() failed\n");
goto config_error;
}
copy_dev_name(local->node, dev);
link->dev = &local->node;
link->state &= ~DEV_CONFIG_PENDING;
if (local->dingo)
do_reset(dev, 1);
printk(KERN_INFO "%s: %s: port %#3lx, irq %d, hwaddr",
dev->name, local->manf_str,(u_long)dev->base_addr, (int)dev->irq);
for (i = 0; i < 6; i++)
printk("%c%02X", i?':':' ', dev->dev_addr[i]);
printk("\n");
return;
config_error:
link->state &= ~DEV_CONFIG_PENDING;
xirc2ps_release((u_long)link);
return;
cis_error:
printk(KNOT_XIRC "unable to parse CIS\n");
failure:
link->state &= ~DEV_CONFIG_PENDING;
}
static void
xirc2ps_release(u_long arg)
{
dev_link_t *link = (dev_link_t *) arg;
local_info_t *local = link->priv;
struct net_device *dev = &local->dev;
DEBUG(0, "release(0x%p)\n", link);
if (link->open) {
DEBUG(0, "release postponed, '%s' "
"still open\n", link->dev->dev_name);
link->state |= DEV_STALE_CONFIG;
return;
}
if (link->win) {
local_info_t *local = dev->priv;
if (local->dingo)
iounmap(local->dingo_ccr - 0x0800);
CardServices(ReleaseWindow, link->win);
}
CardServices(ReleaseConfiguration, link->handle);
CardServices(ReleaseIO, link->handle, &link->io);
CardServices(ReleaseIRQ, link->handle, &link->irq);
link->state &= ~DEV_CONFIG;
}
static int
xirc2ps_event(event_t event, int priority,
event_callback_args_t * args)
{
dev_link_t *link = args->client_data;
local_info_t *lp = link->priv;
struct net_device *dev = &lp->dev;
DEBUG(0, "event(%d)\n", (int)event);
switch (event) {
case CS_EVENT_REGISTRATION_COMPLETE:
DEBUG(0, "registration complete\n");
break;
case CS_EVENT_CARD_REMOVAL:
link->state &= ~DEV_PRESENT;
if (link->state & DEV_CONFIG) {
netif_device_detach(dev);
mod_timer(&link->release, jiffies + HZ/20);
}
break;
case CS_EVENT_CARD_INSERTION:
link->state |= DEV_PRESENT | DEV_CONFIG_PENDING;
xirc2ps_config(link);
break;
case CS_EVENT_PM_SUSPEND:
link->state |= DEV_SUSPEND;
case CS_EVENT_RESET_PHYSICAL:
if (link->state & DEV_CONFIG) {
if (link->open) {
netif_device_detach(dev);
do_powerdown(dev);
}
CardServices(ReleaseConfiguration, link->handle);
}
break;
case CS_EVENT_PM_RESUME:
link->state &= ~DEV_SUSPEND;
case CS_EVENT_CARD_RESET:
if (link->state & DEV_CONFIG) {
CardServices(RequestConfiguration, link->handle, &link->conf);
if (link->open) {
do_reset(dev,1);
netif_device_attach(dev);
}
}
break;
}
return 0;
}
static void
xirc2ps_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct net_device *dev = (struct net_device *)dev_id;
local_info_t *lp = dev->priv;
ioaddr_t ioaddr;
u_char saved_page;
unsigned bytes_rcvd;
unsigned int_status, eth_status, rx_status, tx_status;
unsigned rsr, pktlen;
ulong start_ticks = jiffies;
if (!netif_device_present(dev))
return;
ioaddr = dev->base_addr;
if (lp->mohawk) {
PutByte(XIRCREG_CR, 0);
}
DEBUG(6, "%s: interrupt %d at %#x.\n", dev->name, irq, ioaddr);
saved_page = GetByte(XIRCREG_PR);
int_status = GetByte(XIRCREG_ISR);
bytes_rcvd = 0;
loop_entry:
if (int_status == 0xff) {
DEBUG(3, "%s: interrupt %d for dead card\n", dev->name, irq);
goto leave;
}
eth_status = GetByte(XIRCREG_ESR);
SelectPage(0x40);
rx_status  = GetByte(XIRCREG40_RXST0);
PutByte(XIRCREG40_RXST0, (~rx_status & 0xff));
tx_status = GetByte(XIRCREG40_TXST0);
tx_status |= GetByte(XIRCREG40_TXST1) << 8;
PutByte(XIRCREG40_TXST0, 0);
PutByte(XIRCREG40_TXST1, 0);
DEBUG(3, "%s: ISR=%#2.2x ESR=%#2.2x RSR=%#2.2x TSR=%#4.4x\n",
dev->name, int_status, eth_status, rx_status, tx_status);
SelectPage(0);
while (eth_status & FullPktRcvd) {
rsr = GetByte(XIRCREG0_RSR);
if (bytes_rcvd > maxrx_bytes && (rsr & PktRxOk)) {
lp->stats.rx_dropped++;
DEBUG(2, "%s: RX drop, too much done\n", dev->name);
} else if (rsr & PktRxOk) {
struct sk_buff *skb;
pktlen = GetWord(XIRCREG0_RBC);
bytes_rcvd += pktlen;
DEBUG(5, "rsr=%#02x packet_length=%u\n", rsr, pktlen);
skb = dev_alloc_skb(pktlen+3);
if (!skb) {
printk(KNOT_XIRC "low memory, packet dropped (size=%u)\n",
pktlen);
lp->stats.rx_dropped++;
} else {
skb_reserve(skb, 2);
if (lp->silicon == 0 ) {
unsigned rhsa;
SelectPage(5);
rhsa = GetWord(XIRCREG5_RHSA0);
SelectPage(0);
rhsa += 3;
if (rhsa >= 0x8000)
rhsa = 0;
if (rhsa + pktlen > 0x8000) {
unsigned i;
u_char *buf = skb_put(skb, pktlen);
for (i=0; i < pktlen ; i++, rhsa++) {
buf[i] = GetByte(XIRCREG_EDP);
if (rhsa == 0x8000) {
rhsa = 0;
i--;
}
}
} else {
insw(ioaddr+XIRCREG_EDP,
skb_put(skb, pktlen), (pktlen+1)>>1);
}
}
#if 0
else if (lp->mohawk) {
unsigned i;
u_long *p = skb_put(skb, pktlen);
register u_long a;
ioaddr_t edpreg = ioaddr+XIRCREG_EDP-2;
for (i=0; i < len ; i += 4, p++) {
a = inl(edpreg);
__asm__("rorl $16,%0\n\t"
:"=q" (a)
: "0" (a));
*p = a;
}
}
#endif
else {
insw(ioaddr+XIRCREG_EDP, skb_put(skb, pktlen),
(pktlen+1)>>1);
}
skb->protocol = eth_type_trans(skb, dev);
skb->dev = dev;
netif_rx(skb);
dev->last_rx = jiffies;
lp->stats.rx_packets++;
add_rx_bytes(&lp->stats, pktlen);
if (!(rsr & PhyPkt))
lp->stats.multicast++;
}
} else {
DEBUG(5, "rsr=%#02x\n", rsr);
}
if (rsr & PktTooLong) {
lp->stats.rx_frame_errors++;
DEBUG(3, "%s: Packet too long\n", dev->name);
}
if (rsr & CRCErr) {
lp->stats.rx_crc_errors++;
DEBUG(3, "%s: CRC error\n", dev->name);
}
if (rsr & AlignErr) {
lp->stats.rx_fifo_errors++;
DEBUG(3, "%s: Alignment error\n", dev->name);
}
PutWord(XIRCREG0_DO, 0x8000);
eth_status = GetByte(XIRCREG_ESR);
}
if (rx_status & 0x10) {
lp->stats.rx_over_errors++;
PutByte(XIRCREG_CR, ClearRxOvrun);
DEBUG(3, "receive overrun cleared\n");
}
if (int_status & PktTxed) {
unsigned n, nn;
n = lp->last_ptr_value;
nn = GetByte(XIRCREG0_PTR);
lp->last_ptr_value = nn;
if (nn < n)
lp->stats.tx_packets += 256 - n;
else if (n == nn) {
DEBUG(0, "PTR not changed?\n");
} else
lp->stats.tx_packets += lp->last_ptr_value - n;
netif_wake_queue(dev);
}
if (tx_status & 0x0002) {
DEBUG(0, "tx restarted due to execssive collissions\n");
PutByte(XIRCREG_CR, RestartTx);
}
if (tx_status & 0x0040)
lp->stats.tx_aborted_errors++;
if (bytes_rcvd > 1000) {
u_long duration = jiffies - start_ticks;
if (duration >= HZ/10) {
maxrx_bytes = (bytes_rcvd * (HZ/10)) / duration;
if (maxrx_bytes < 2000)
maxrx_bytes = 2000;
else if (maxrx_bytes > 22000)
maxrx_bytes = 22000;
DEBUG(1, "set maxrx=%u (rcvd=%u ticks=%lu)\n",
maxrx_bytes, bytes_rcvd, duration);
} else if (!duration && maxrx_bytes < 22000) {
maxrx_bytes += 2000;
if (maxrx_bytes > 22000)
maxrx_bytes = 22000;
DEBUG(1, "set maxrx=%u\n", maxrx_bytes);
}
}
leave:
if (lockup_hack) {
if (int_status != 0xff && (int_status = GetByte(XIRCREG_ISR)) != 0)
goto loop_entry;
}
SelectPage(saved_page);
PutByte(XIRCREG_CR, EnableIntr);
}
static void
do_tx_timeout(struct net_device *dev)
{
local_info_t *lp = dev->priv;
printk(KERN_NOTICE "%s: transmit timed out\n", dev->name);
lp->stats.tx_errors++;
do_reset(dev,1);
dev->trans_start = jiffies;
netif_wake_queue(dev);
}
static int
do_start_xmit(struct sk_buff *skb, struct net_device *dev)
{
local_info_t *lp = dev->priv;
ioaddr_t ioaddr = dev->base_addr;
int okay;
unsigned freespace;
unsigned pktlen = skb? skb->len : 0;
DEBUG(1, "do_start_xmit(skb=%p, dev=%p) len=%u\n",
skb, dev, pktlen);
tx_timeout_check(dev, do_tx_timeout);
skb_tx_check(dev, skb);
if (pktlen < ETH_ZLEN)
pktlen = ETH_ZLEN;
SelectPage(0);
PutWord(XIRCREG0_TRS, (u_short)pktlen+2);
freespace = GetWord(XIRCREG0_TSO);
okay = freespace & 0x8000;
freespace &= 0x7fff;
okay = pktlen +2 < freespace;
DEBUG(2 + (okay ? 2 : 0), "%s: avail. tx space=%u%s\n",
dev->name, freespace, okay ? " (okay)":" (not enough)");
if (!okay) {
return 1;
}
PutWord(XIRCREG_EDP, (u_short)pktlen);
outsw(ioaddr+XIRCREG_EDP, skb->data, pktlen>>1);
if (pktlen & 1)
PutByte(XIRCREG_EDP, skb->data[pktlen-1]);
if (lp->mohawk)
PutByte(XIRCREG_CR, TransmitPacket|EnableIntr);
DEV_KFREE_SKB (skb);
dev->trans_start = jiffies;
add_tx_bytes(&lp->stats, pktlen);
netif_start_queue(dev);
return 0;
}
static struct net_device_stats *
do_get_stats(struct net_device *dev)
{
local_info_t *lp = dev->priv;
return &lp->stats;
}
static void
set_addresses(struct net_device *dev)
{
ioaddr_t ioaddr = dev->base_addr;
local_info_t *lp = dev->priv;
struct dev_mc_list *dmi = dev->mc_list;
char *addr;
int i,j,k,n;
SelectPage(k=0x50);
for (i=0,j=8,n=0; ; i++, j++) {
if (i > 5) {
if (++n > 9)
break;
i = 0;
}
if (j > 15) {
j = 8;
k++;
SelectPage(k);
}
if (n && n <= dev->mc_count && dmi) {
addr = dmi->dmi_addr;
dmi = dmi->next;
} else
addr = dev->dev_addr;
if (lp->mohawk)
PutByte(j, addr[5-i]);
else
PutByte(j, addr[i]);
}
SelectPage(0);
}
static void
set_multicast_list(struct net_device *dev)
{
ioaddr_t ioaddr = dev->base_addr;
SelectPage(0x42);
if (dev->flags & IFF_PROMISC) {
PutByte(XIRCREG42_SWC1, 0x06);
} else if (dev->mc_count > 9 || (dev->flags & IFF_ALLMULTI)) {
PutByte(XIRCREG42_SWC1, 0x06);
} else if (dev->mc_count) {
PutByte(XIRCREG42_SWC1, 0x00);
SelectPage(0x40);
PutByte(XIRCREG40_CMD0, Offline);
set_addresses(dev);
SelectPage(0x40);
PutByte(XIRCREG40_CMD0, EnableRecv | Online);
} else {
PutByte(XIRCREG42_SWC1, 0x00);
}
SelectPage(0);
}
static int
do_config(struct net_device *dev, struct ifmap *map)
{
local_info_t *local = dev->priv;
DEBUG(0, "do_config(%p)\n", dev);
if (map->port != 255 && map->port != dev->if_port) {
if (map->port > 4)
return -EINVAL;
if (!map->port) {
local->probe_port = 1;
dev->if_port = 1;
} else {
local->probe_port = 0;
dev->if_port = map->port;
}
printk(KERN_INFO "%s: switching to %s port\n",
dev->name, if_names[dev->if_port]);
do_reset(dev,1);
}
return 0;
}
static int
do_open(struct net_device *dev)
{
local_info_t *lp = dev->priv;
dev_link_t *link = &lp->link;
DEBUG(0, "do_open(%p)\n", dev);
if (!DEV_OK(link))
return -ENODEV;
link->open++;
MOD_INC_USE_COUNT;
netif_start_queue(dev);
netif_mark_up(dev);
do_reset(dev,1);
return 0;
}
static int
do_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
local_info_t *local = dev->priv;
ioaddr_t ioaddr = dev->base_addr;
u16 *data = (u16 *)&rq->ifr_data;
DEBUG(1, "%s: ioctl(%-.6s, %#04x) %04x %04x %04x %04x\n",
dev->name, rq->ifr_ifrn.ifrn_name, cmd,
data[0], data[1], data[2], data[3]);
if (!local->mohawk)
return -EOPNOTSUPP;
switch(cmd) {
case SIOCDEVPRIVATE:
data[0] = 0;
case SIOCDEVPRIVATE+1:
data[3] = mii_rd(ioaddr, data[0] & 0x1f, data[1] & 0x1f);
break;
case SIOCDEVPRIVATE+2:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
mii_wr(ioaddr, data[0] & 0x1f, data[1] & 0x1f, data[2], 16);
break;
default:
return -EOPNOTSUPP;
}
return 0;
}
static void
hardreset(struct net_device *dev)
{
local_info_t *local = dev->priv;
ioaddr_t ioaddr = dev->base_addr;
SelectPage(4);
udelay(1);
PutByte(XIRCREG4_GPR1, 0);
busy_loop(HZ/25);
if (local->mohawk)
PutByte(XIRCREG4_GPR1, 1);
else
PutByte(XIRCREG4_GPR1, 1 | 4);
busy_loop(HZ/50);
}
static void
do_reset(struct net_device *dev, int full)
{
local_info_t *local = dev->priv;
ioaddr_t ioaddr = dev->base_addr;
unsigned value;
DEBUG(0, "%s: do_reset(%p,%d)\n", dev? dev->name:"eth?", dev, full);
hardreset(dev);
PutByte(XIRCREG_CR, SoftReset);
busy_loop(HZ/50);
PutByte(XIRCREG_CR, 0);
busy_loop(HZ/25);
if (local->mohawk) {
SelectPage(4);
PutByte(XIRCREG4_GPR0, 0x0e);
}
busy_loop(HZ/2);
local->last_ptr_value = 0;
local->silicon = local->mohawk ? (GetByte(XIRCREG4_BOV) & 0x70) >> 4
: (GetByte(XIRCREG4_BOV) & 0x30) >> 4;
if (local->probe_port) {
if (!local->mohawk) {
SelectPage(4);
PutByte(XIRCREG4_GPR0, 4);
local->probe_port = 0;
}
} else if (dev->if_port == 2) {
SelectPage(0x42);
PutByte(XIRCREG42_SWC1, 0xC0);
} else {
SelectPage(0x42);
PutByte(XIRCREG42_SWC1, 0x80);
}
busy_loop(HZ/25);
#ifdef PCMCIA_DEBUG
if (pc_debug) {
SelectPage(0);
value = GetByte(XIRCREG_ESR);
printk(KERN_DEBUG "%s: ESR is: %#02x\n", dev->name, value);
}
#endif
SelectPage(1);
PutByte(XIRCREG1_IMR0, 0xff);
PutByte(XIRCREG1_IMR1, 1	);
value = GetByte(XIRCREG1_ECR);
#if 0
if (local->mohawk)
value |= DisableLinkPulse;
PutByte(XIRCREG1_ECR, value);
#endif
DEBUG(0, "%s: ECR is: %#02x\n", dev->name, value);
SelectPage(0x42);
PutByte(XIRCREG42_SWC0, 0x20);
if (local->silicon != 1) {
SelectPage(2);
PutWord(XIRCREG2_RBS, 0x2000);
}
if (full)
set_addresses(dev);
SelectPage(0);
PutWord(XIRCREG0_DO, 0x2000);
SelectPage(0x40);
PutByte(XIRCREG40_RMASK0, 0xff);
PutByte(XIRCREG40_TMASK0, 0xff);
PutByte(XIRCREG40_TMASK1, 0xb0);
PutByte(XIRCREG40_RXST0,  0x00);
PutByte(XIRCREG40_TXST0,  0x00);
PutByte(XIRCREG40_TXST1,  0x00);
if (full && local->mohawk && init_mii(dev)) {
if (dev->if_port == 4 || local->dingo || local->new_mii) {
printk(KERN_INFO "%s: MII selected\n", dev->name);
SelectPage(2);
PutByte(XIRCREG2_MSR, GetByte(XIRCREG2_MSR) | 0x08);
busy_loop(HZ/50);
} else {
printk(KERN_INFO "%s: MII detected; using 10mbs\n",
dev->name);
SelectPage(0x42);
if (dev->if_port == 2)
PutByte(XIRCREG42_SWC1, 0xC0);
else
PutByte(XIRCREG42_SWC1, 0x80);
busy_loop(HZ/25);
}
if (full_duplex)
PutByte(XIRCREG1_ECR, GetByte(XIRCREG1_ECR | FullDuplex));
} else {
SelectPage(0);
value = GetByte(XIRCREG_ESR);
dev->if_port = (value & MediaSelect) ? 1 : 2;
}
SelectPage(2);
if (dev->if_port == 1 || dev->if_port == 4)
PutByte(XIRCREG2_LED, 0x3b);
else
PutByte(XIRCREG2_LED, 0x3a);
if (local->dingo)
PutByte(0x0b, 0x04);
if (full) {
SelectPage(0x40);
PutByte(XIRCREG40_CMD0, EnableRecv | Online);
}
SelectPage(1);
PutByte(XIRCREG1_IMR0, 0xff);
udelay(1);
SelectPage(0);
PutByte(XIRCREG_CR, EnableIntr);
if (local->modem && !local->dingo) {
if (!(GetByte(0x10) & 0x01))
PutByte(0x10, 0x11);
}
if (full)
printk(KERN_INFO "%s: media %s, silicon revision %d\n",
dev->name, if_names[dev->if_port], local->silicon);
SelectPage(0);
}
static int
init_mii(struct net_device *dev)
{
local_info_t *local = dev->priv;
ioaddr_t ioaddr = dev->base_addr;
unsigned control, status, linkpartner;
int i;
if (if_port == 4 || if_port == 1) {
dev->if_port = if_port;
local->probe_port = 0;
return 1;
}
status = mii_rd(ioaddr,  0, 1);
if ((status & 0xff00) != 0x7800)
return 0;
local->new_mii = (mii_rd(ioaddr, 0, 2) != 0xffff);
if (local->probe_port)
control = 0x1000;
else if (dev->if_port == 4)
control = 0x2000;
else
control = 0x0000;
mii_wr(ioaddr,  0, 0, control, 16);
udelay(100);
control = mii_rd(ioaddr, 0, 0);
if (control & 0x0400) {
printk(KERN_NOTICE "%s can't take PHY out of isolation mode\n",
dev->name);
local->probe_port = 0;
return 0;
}
if (local->probe_port) {
for (i=0; i < 35; i++) {
busy_loop(HZ/10);
status = mii_rd(ioaddr,  0, 1);
if ((status & 0x0020) && (status & 0x0004))
break;
}
if (!(status & 0x0020)) {
printk(KERN_INFO "%s: autonegotiation failed;"
" using 10mbs\n", dev->name);
if (!local->new_mii) {
control = 0x0000;
mii_wr(ioaddr,  0, 0, control, 16);
udelay(100);
SelectPage(0);
dev->if_port = (GetByte(XIRCREG_ESR) & MediaSelect) ? 1 : 2;
}
} else {
linkpartner = mii_rd(ioaddr, 0, 5);
printk(KERN_INFO "%s: MII link partner: %04x\n",
dev->name, linkpartner);
if (linkpartner & 0x0080) {
dev->if_port = 4;
} else
dev->if_port = 1;
}
}
return 1;
}
static void
do_powerdown(struct net_device *dev)
{
ioaddr_t ioaddr = dev->base_addr;
DEBUG(0, "do_powerdown(%p)\n", dev);
SelectPage(4);
PutByte(XIRCREG4_GPR1, 0);
SelectPage(0);
}
static int
do_stop(struct net_device *dev)
{
ioaddr_t ioaddr = dev->base_addr;
local_info_t *lp = dev->priv;
dev_link_t *link = &lp->link;
DEBUG(0, "do_stop(%p)\n", dev);
if (!link)
return -ENODEV;
netif_stop_queue(dev);
netif_mark_down(dev);
SelectPage(0);
PutByte(XIRCREG_CR, 0);
SelectPage(0x01);
PutByte(XIRCREG1_IMR0, 0x00);
SelectPage(4);
PutByte(XIRCREG4_GPR1, 0);
SelectPage(0);
link->open--;
if (link->state & DEV_STALE_CONFIG)
mod_timer(&link->release, jiffies + HZ/20);
MOD_DEC_USE_COUNT;
return 0;
}
static int __init
init_xirc2ps_cs(void)
{
servinfo_t serv;
printk(KERN_INFO "%s\n", version);
if (lockup_hack)
printk(KINF_XIRC "lockup hack is enabled\n");
CardServices(GetCardServicesInfo, &serv);
if (serv.Revision != CS_RELEASE_CODE) {
printk(KNOT_XIRC "Card Services release does not match!\n");
return -EINVAL;
}
DEBUG(0, "pc_debug=%d\n", pc_debug);
register_pccard_driver(&dev_info, &xirc2ps_attach, &xirc2ps_detach);
return 0;
}
static void __exit
exit_xirc2ps_cs(void)
{
DEBUG(0, "unloading\n");
unregister_pccard_driver(&dev_info);
while (dev_list) {
if (dev_list->state & DEV_CONFIG)
xirc2ps_release((u_long)dev_list);
if (dev_list)
xirc2ps_detach(dev_list);
}
}
module_init(init_xirc2ps_cs);
module_exit(exit_xirc2ps_cs);