#include <linux/config.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/slab.h>
#include <linux/string.h>
#include <linux/ioport.h>
#include <linux/netdevice.h>
#include <linux/if_arp.h>
#include <linux/etherdevice.h>
#include <linux/wireless.h>
#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/cisreg.h>
#include <pcmcia/ds.h>
#include <asm/uaccess.h>
#include <asm/io.h>
#include <asm/system.h>
#include "orinoco.h"
MODULE_AUTHOR("David Gibson <hermes@gibson.dropbear.id.au>");
MODULE_DESCRIPTION("Driver for PCMCIA Lucent Orinoco, Prism II based and similar wireless cards");
#ifdef MODULE_LICENSE
MODULE_LICENSE("Dual MPL/GPL");
#endif
static uint irq_mask = 0xdeb8;
static int irq_list[4] = { -1 };
static int ignore_cis_vcc;
MODULE_PARM(irq_mask, "i");
MODULE_PARM(irq_list, "1-4i");
MODULE_PARM(ignore_cis_vcc, "i");
static dev_info_t dev_info = "orinoco_cs";
struct orinoco_pccard {
dev_link_t link;
dev_node_t node;
unsigned long hard_reset_in_progress;
};
static dev_link_t *dev_list;
static int orinoco_cs_hard_reset(struct orinoco_private *priv);
static void orinoco_cs_config(dev_link_t * link);
static void orinoco_cs_release(u_long arg);
static int orinoco_cs_event(event_t event, int priority,
event_callback_args_t * args);
static dev_link_t *orinoco_cs_attach(void);
static void orinoco_cs_detach(dev_link_t *);
static int
orinoco_cs_hard_reset(struct orinoco_private *priv)
{
struct orinoco_pccard *card = priv->card;
dev_link_t *link = &card->link;
int err;
set_bit(0, &card->hard_reset_in_progress);
err = CardServices(ResetCard, link->handle, NULL);
if (err)
return err;
clear_bit(0, &card->hard_reset_in_progress);
return 0;
}
static void
orinoco_cs_error(client_handle_t handle, int func, int ret)
{
error_info_t err = { func, ret };
CardServices(ReportError, handle, &err);
}
static void
flush_stale_links(void)
{
dev_link_t *link, *next;
TRACE_ENTER("");
for (link = dev_list; link; link = next) {
next = link->next;
if (link->state & DEV_STALE_LINK) {
orinoco_cs_detach(link);
}
}
TRACE_EXIT("");
}
static dev_link_t *
orinoco_cs_attach(void)
{
struct net_device *dev;
struct orinoco_private *priv;
struct orinoco_pccard *card;
dev_link_t *link;
client_reg_t client_reg;
int ret, i;
flush_stale_links();
dev = alloc_orinocodev(sizeof(*card), orinoco_cs_hard_reset);
if (! dev)
return NULL;
priv = dev->priv;
card = priv->card;
link = &card->link;
link->priv = dev;
init_timer(&link->release);
link->release.function = &orinoco_cs_release;
link->release.data = (u_long) link;
link->irq.Attributes = IRQ_TYPE_EXCLUSIVE;
link->irq.IRQInfo1 = IRQ_INFO2_VALID | IRQ_LEVEL_ID;
if (irq_list[0] == -1)
link->irq.IRQInfo2 = irq_mask;
else
for (i = 0; i < 4; i++)
link->irq.IRQInfo2 |= 1 << irq_list[i];
link->irq.Handler = NULL;
link->conf.Attributes = 0;
link->conf.IntType = INT_MEMORY_AND_IO;
link->next = dev_list;
dev_list = link;
client_reg.dev_info = &dev_info;
client_reg.Attributes = INFO_IO_CLIENT | INFO_CARD_SHARE;
client_reg.EventMask =
CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET |
CS_EVENT_PM_SUSPEND | CS_EVENT_PM_RESUME;
client_reg.event_handler = &orinoco_cs_event;
client_reg.Version = 0x0210;
client_reg.event_callback_args.client_data = link;
ret = CardServices(RegisterClient, &link->handle, &client_reg);
if (ret != CS_SUCCESS) {
orinoco_cs_error(link->handle, RegisterClient, ret);
orinoco_cs_detach(link);
return NULL;
}
return link;
}
static void
orinoco_cs_detach(dev_link_t * link)
{
dev_link_t **linkp;
struct net_device *dev = link->priv;
for (linkp = &dev_list; *linkp; linkp = &(*linkp)->next)
if (*linkp == link)
break;
if (*linkp == NULL) {
BUG();
return;
}
if (link->state & DEV_CONFIG) {
orinoco_cs_release((u_long)link);
if (link->state & DEV_CONFIG) {
link->state |= DEV_STALE_LINK;
return;
}
}
if (link->handle)
CardServices(DeregisterClient, link->handle);
*linkp = link->next;
DEBUG(0, "orinoco_cs: detach: link=%p link->dev=%p\n", link, link->dev);
if (link->dev) {
DEBUG(0, "orinoco_cs: About to unregister net device %p\n",
dev);
unregister_netdev(dev);
}
kfree(dev);
}
#define CS_CHECK(fn, args...) \
while ((last_ret=CardServices(last_fn=(fn),args))!=0) goto cs_failed
#define CFG_CHECK(fn, args...) \
if (CardServices(fn, args) != 0) goto next_entry
static void
orinoco_cs_config(dev_link_t *link)
{
struct net_device *dev = link->priv;
client_handle_t handle = link->handle;
struct orinoco_private *priv = dev->priv;
struct orinoco_pccard *card = priv->card;
hermes_t *hw = &priv->hw;
int last_fn, last_ret;
u_char buf[64];
config_info_t conf;
cisinfo_t info;
tuple_t tuple;
cisparse_t parse;
CS_CHECK(ValidateCIS, handle, &info);
tuple.DesiredTuple = CISTPL_CONFIG;
tuple.Attributes = 0;
tuple.TupleData = buf;
tuple.TupleDataMax = sizeof(buf);
tuple.TupleOffset = 0;
CS_CHECK(GetFirstTuple, handle, &tuple);
CS_CHECK(GetTupleData, handle, &tuple);
CS_CHECK(ParseTuple, handle, &tuple, &parse);
link->conf.ConfigBase = parse.config.base;
link->conf.Present = parse.config.rmask[0];
link->state |= DEV_CONFIG;
CS_CHECK(GetConfigurationInfo, handle, &conf);
link->conf.Vcc = conf.Vcc;
tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;
CS_CHECK(GetFirstTuple, handle, &tuple);
while (1) {
cistpl_cftable_entry_t *cfg = &(parse.cftable_entry);
cistpl_cftable_entry_t dflt = { .index = 0 };
CFG_CHECK(GetTupleData, handle, &tuple);
CFG_CHECK(ParseTuple, handle, &tuple, &parse);
if (cfg->flags & CISTPL_CFTABLE_DEFAULT)
dflt = *cfg;
if (cfg->index == 0)
goto next_entry;
link->conf.ConfigIndex = cfg->index;
if (cfg->flags & CISTPL_CFTABLE_AUDIO) {
link->conf.Attributes |= CONF_ENABLE_SPKR;
link->conf.Status = CCSR_AUDIO_ENA;
}
if (cfg->vcc.present & (1 << CISTPL_POWER_VNOM)) {
if (conf.Vcc != cfg->vcc.param[CISTPL_POWER_VNOM] / 10000) {
DEBUG(2, "orinoco_cs_config: Vcc mismatch (conf.Vcc = %d, CIS = %d)\n", conf.Vcc, cfg->vcc.param[CISTPL_POWER_VNOM] / 10000);
if (!ignore_cis_vcc)
goto next_entry;
}
} else if (dflt.vcc.present & (1 << CISTPL_POWER_VNOM)) {
if (conf.Vcc != dflt.vcc.param[CISTPL_POWER_VNOM] / 10000) {
DEBUG(2, "orinoco_cs_config: Vcc mismatch (conf.Vcc = %d, CIS = %d)\n", conf.Vcc, dflt.vcc.param[CISTPL_POWER_VNOM] / 10000);
if(!ignore_cis_vcc)
goto next_entry;
}
}
if (cfg->vpp1.present & (1 << CISTPL_POWER_VNOM))
link->conf.Vpp1 = link->conf.Vpp2 =
cfg->vpp1.param[CISTPL_POWER_VNOM] / 10000;
else if (dflt.vpp1.present & (1 << CISTPL_POWER_VNOM))
link->conf.Vpp1 = link->conf.Vpp2 =
dflt.vpp1.param[CISTPL_POWER_VNOM] / 10000;
if (cfg->irq.IRQInfo1 || dflt.irq.IRQInfo1)
link->conf.Attributes |= CONF_ENABLE_IRQ;
link->io.NumPorts1 = link->io.NumPorts2 = 0;
if ((cfg->io.nwin > 0) || (dflt.io.nwin > 0)) {
cistpl_io_t *io =
(cfg->io.nwin) ? &cfg->io : &dflt.io;
link->io.Attributes1 = IO_DATA_PATH_WIDTH_AUTO;
if (!(io->flags & CISTPL_IO_8BIT))
link->io.Attributes1 =
IO_DATA_PATH_WIDTH_16;
if (!(io->flags & CISTPL_IO_16BIT))
link->io.Attributes1 =
IO_DATA_PATH_WIDTH_8;
link->io.IOAddrLines =
io->flags & CISTPL_IO_LINES_MASK;
link->io.BasePort1 = io->win[0].base;
link->io.NumPorts1 = io->win[0].len;
if (io->nwin > 1) {
link->io.Attributes2 =
link->io.Attributes1;
link->io.BasePort2 = io->win[1].base;
link->io.NumPorts2 = io->win[1].len;
}
CFG_CHECK(RequestIO, link->handle, &link->io);
}
break;
next_entry:
if (link->io.NumPorts1)
CardServices(ReleaseIO, link->handle, &link->io);
last_ret = CardServices(GetNextTuple, handle, &tuple);
if (last_ret == CS_NO_MORE_ITEMS) {
printk(KERN_ERR "GetNextTuple().  No matching CIS configuration, "
"maybe you need the ignore_cis_vcc=1 parameter.\n");
goto cs_failed;
}
}
if (link->conf.Attributes & CONF_ENABLE_IRQ) {
int i;
link->irq.Attributes = IRQ_TYPE_EXCLUSIVE | IRQ_HANDLE_PRESENT;
link->irq.IRQInfo1 = IRQ_INFO2_VALID | IRQ_LEVEL_ID;
if (irq_list[0] == -1)
link->irq.IRQInfo2 = irq_mask;
else
for (i=0; i<4; i++)
link->irq.IRQInfo2 |= 1 << irq_list[i];
link->irq.Handler = orinoco_interrupt;
link->irq.Instance = dev;
CS_CHECK(RequestIRQ, link->handle, &link->irq);
}
hermes_struct_init(hw, link->io.BasePort1,
HERMES_IO, HERMES_16BIT_REGSPACING);
CS_CHECK(RequestConfiguration, link->handle, &link->conf);
dev->base_addr = link->io.BasePort1;
dev->irq = link->irq.AssignedIRQ;
SET_MODULE_OWNER(dev);
card->node.major = card->node.minor = 0;
dev->name[0] = '\0';
if (register_netdev(dev) != 0) {
printk(KERN_ERR "orinoco_cs: register_netdev() failed\n");
goto failed;
}
strcpy(card->node.dev_name, dev->name);
link->dev = &card->node;
link->state &= ~DEV_CONFIG_PENDING;
printk(KERN_DEBUG "%s: index 0x%02x: Vcc %d.%d",
dev->name, link->conf.ConfigIndex,
link->conf.Vcc / 10, link->conf.Vcc % 10);
if (link->conf.Vpp1)
printk(", Vpp %d.%d", link->conf.Vpp1 / 10,
link->conf.Vpp1 % 10);
if (link->conf.Attributes & CONF_ENABLE_IRQ)
printk(", irq %d", link->irq.AssignedIRQ);
if (link->io.NumPorts1)
printk(", io 0x%04x-0x%04x", link->io.BasePort1,
link->io.BasePort1 + link->io.NumPorts1 - 1);
if (link->io.NumPorts2)
printk(" & 0x%04x-0x%04x", link->io.BasePort2,
link->io.BasePort2 + link->io.NumPorts2 - 1);
printk("\n");
return;
cs_failed:
orinoco_cs_error(link->handle, last_fn, last_ret);
failed:
orinoco_cs_release((u_long) link);
}
static void
orinoco_cs_release(u_long arg)
{
dev_link_t *link = (dev_link_t *) arg;
struct net_device *dev = link->priv;
struct orinoco_private *priv = dev->priv;
unsigned long flags;
spin_lock_irqsave(&priv->lock, flags);
priv->hw_unavailable++;
spin_unlock_irqrestore(&priv->lock, flags);
CardServices(ReleaseConfiguration, link->handle);
if (link->io.NumPorts1)
CardServices(ReleaseIO, link->handle, &link->io);
if (link->irq.AssignedIRQ)
CardServices(ReleaseIRQ, link->handle, &link->irq);
link->state &= ~DEV_CONFIG;
}
static int
orinoco_cs_event(event_t event, int priority,
event_callback_args_t * args)
{
dev_link_t *link = args->client_data;
struct net_device *dev = link->priv;
struct orinoco_private *priv = dev->priv;
struct orinoco_pccard *card = priv->card;
int err = 0;
unsigned long flags;
switch (event) {
case CS_EVENT_CARD_REMOVAL:
link->state &= ~DEV_PRESENT;
if (link->state & DEV_CONFIG) {
orinoco_lock(priv, &flags);
netif_device_detach(dev);
priv->hw_unavailable++;
orinoco_unlock(priv, &flags);
}
break;
case CS_EVENT_CARD_INSERTION:
link->state |= DEV_PRESENT | DEV_CONFIG_PENDING;
orinoco_cs_config(link);
break;
case CS_EVENT_PM_SUSPEND:
link->state |= DEV_SUSPEND;
case CS_EVENT_RESET_PHYSICAL:
if (link->state & DEV_CONFIG) {
if (! test_bit(0, &card->hard_reset_in_progress)) {
spin_lock_irqsave(&priv->lock, flags);
err = __orinoco_down(dev);
if (err)
printk(KERN_WARNING "%s: %s: Error %d downing interface\n",
dev->name,
event == CS_EVENT_PM_SUSPEND ? "SUSPEND" : "RESET_PHYSICAL",
err);
netif_device_detach(dev);
priv->hw_unavailable++;
spin_unlock_irqrestore(&priv->lock, flags);
}
CardServices(ReleaseConfiguration, link->handle);
}
break;
case CS_EVENT_PM_RESUME:
link->state &= ~DEV_SUSPEND;
case CS_EVENT_CARD_RESET:
if (link->state & DEV_CONFIG) {
CardServices(RequestConfiguration, link->handle,
&link->conf);
if (! test_bit(0, &card->hard_reset_in_progress)) {
err = orinoco_reinit_firmware(dev);
if (err) {
printk(KERN_ERR "%s: Error %d re-initializing firmware\n",
dev->name, err);
break;
}
spin_lock_irqsave(&priv->lock, flags);
netif_device_attach(dev);
priv->hw_unavailable--;
if (priv->open && ! priv->hw_unavailable) {
err = __orinoco_up(dev);
if (err)
printk(KERN_ERR "%s: Error %d restarting card\n",
dev->name, err);
}
spin_unlock_irqrestore(&priv->lock, flags);
}
}
break;
}
return err;
}
static char version[] __initdata = "orinoco_cs.c 0.13e (David Gibson <hermes@gibson.dropbear.id.au> and others)";
static int __init
init_orinoco_cs(void)
{
servinfo_t serv;
printk(KERN_DEBUG "%s\n", version);
CardServices(GetCardServicesInfo, &serv);
if (serv.Revision != CS_RELEASE_CODE) {
printk(KERN_NOTICE "orinoco_cs: Card Services release "
"does not match!\n");
return -EINVAL;
}
register_pccard_driver(&dev_info, &orinoco_cs_attach, &orinoco_cs_detach);
return 0;
}
static void __exit
exit_orinoco_cs(void)
{
unregister_pccard_driver(&dev_info);
if (dev_list)
DEBUG(0, "orinoco_cs: Removing leftover devices.\n");
while (dev_list != NULL) {
if (dev_list->state & DEV_CONFIG)
orinoco_cs_release((u_long) dev_list);
orinoco_cs_detach(dev_list);
}
}
module_init(init_orinoco_cs);
module_exit(exit_orinoco_cs);