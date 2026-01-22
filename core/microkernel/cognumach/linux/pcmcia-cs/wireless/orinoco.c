#include <linux/config.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/ptrace.h>
#include <linux/slab.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/ioport.h>
#include <linux/netdevice.h>
#include <linux/if_arp.h>
#include <linux/etherdevice.h>
#include <linux/wireless.h>
#include <asm/uaccess.h>
#include <asm/io.h>
#include <asm/system.h>
#include "hermes.h"
#include "hermes_rid.h"
#include "orinoco.h"
#include "ieee802_11.h"
MODULE_AUTHOR("David Gibson <hermes@gibson.dropbear.id.au>");
MODULE_DESCRIPTION("Driver for Lucent Orinoco, Prism II based and similar wireless cards");
#ifdef MODULE_LICENSE
MODULE_LICENSE("Dual MPL/GPL");
#endif
#ifdef ORINOCO_DEBUG
int orinoco_debug = ORINOCO_DEBUG;
MODULE_PARM(orinoco_debug, "i");
EXPORT_SYMBOL(orinoco_debug);
#endif
static int suppress_linkstatus;
MODULE_PARM(suppress_linkstatus, "i");
#ifndef SIOCIWFIRSTPRIV
#define SIOCIWFIRSTPRIV		SIOCDEVPRIVATE
#endif
#ifndef SIOCIWLASTPRIV
#define SIOCIWLASTPRIV		SIOCDEVPRIVATE+0xF
#endif
#ifdef WIRELESS_SPY
#define SPY_NUMBER(priv)	(priv->spy_number)
#else
#define SPY_NUMBER(priv)	0
#endif
#define ORINOCO_MIN_MTU		256
#define ORINOCO_MAX_MTU		(IEEE802_11_DATA_LEN - ENCAPS_OVERHEAD)
#define SYMBOL_MAX_VER_LEN	(14)
#define USER_BAP		0
#define IRQ_BAP			1
#define MAX_IRQLOOPS_PER_IRQ	10
#define MAX_IRQLOOPS_PER_JIFFY	(20000/HZ)
#define SMALL_KEY_SIZE		5
#define LARGE_KEY_SIZE		13
#define TX_NICBUF_SIZE_BUG	1585
#define DUMMY_FID		0xFFFF
#define RUP_EVEN(a) (((a) + 1) & (~1))
#define MAX_MULTICAST(priv)	(HERMES_MAX_MULTICAST)
#ifdef MACH
#undef copy_to_user
#define copy_to_user(a,b,c)     (memcpy(a,b,c), 0)
#define verify_area(a,b,c)      (0)
#define copy_from_user(a,b,c)   (memcpy(a,b,c), 0)
#endif
const long channel_frequency[] = {
2412, 2417, 2422, 2427, 2432, 2437, 2442,
2447, 2452, 2457, 2462, 2467, 2472, 2484
};
#define NUM_CHANNELS ( sizeof(channel_frequency) / sizeof(channel_frequency[0]) )
struct {
int bitrate;
int automatic;
u16 agere_txratectrl;
u16 intersil_txratectrl;
} bitrate_table[] = {
{110, 1,  3, 15},
{10,  0,  1,  1},
{10,  1,  1,  1},
{20,  0,  2,  2},
{20,  1,  6,  3},
{55,  0,  4,  4},
{55,  1,  7,  7},
{110, 0,  5,  8},
};
#define BITRATE_TABLE_SIZE (sizeof(bitrate_table) / sizeof(bitrate_table[0]))
struct header_struct {
u8 dest[ETH_ALEN];
u8 src[ETH_ALEN];
u16 len;
u8 dsap;
u8 ssap;
u8 ctrl;
u8 oui[3];
u16 ethertype;
} __attribute__ ((packed));
u8 encaps_hdr[] = {0xaa, 0xaa, 0x03, 0x00, 0x00, 0x00};
#define ENCAPS_OVERHEAD		(sizeof(encaps_hdr) + 2)
static void orinoco_stat_gather(struct net_device *dev,
struct sk_buff *skb,
struct hermes_rx_descriptor *desc);
static struct net_device_stats *orinoco_get_stats(struct net_device *dev);
static struct iw_statistics *orinoco_get_wireless_stats(struct net_device *dev);
static int __orinoco_program_rids(struct net_device *dev);
static int __orinoco_hw_set_bitrate(struct orinoco_private *priv);
static int __orinoco_hw_setup_wep(struct orinoco_private *priv);
static int orinoco_hw_get_bssid(struct orinoco_private *priv, char buf[ETH_ALEN]);
static int orinoco_hw_get_essid(struct orinoco_private *priv, int *active,
char buf[IW_ESSID_MAX_SIZE+1]);
static long orinoco_hw_get_freq(struct orinoco_private *priv);
static int orinoco_hw_get_bitratelist(struct orinoco_private *priv, int *numrates,
s32 *rates, int max);
static void __orinoco_set_multicast_list(struct net_device *dev);
static void __orinoco_ev_tick(struct net_device *dev, hermes_t *hw);
static void __orinoco_ev_wterr(struct net_device *dev, hermes_t *hw);
static void __orinoco_ev_infdrop(struct net_device *dev, hermes_t *hw);
static void __orinoco_ev_info(struct net_device *dev, hermes_t *hw);
static void __orinoco_ev_rx(struct net_device *dev, hermes_t *hw);
static void __orinoco_ev_txexc(struct net_device *dev, hermes_t *hw);
static void __orinoco_ev_tx(struct net_device *dev, hermes_t *hw);
static void __orinoco_ev_alloc(struct net_device *dev, hermes_t *hw);
static int orinoco_debug_dump_recs(struct net_device *dev);
int __orinoco_up(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
struct hermes *hw = &priv->hw;
int err;
err = __orinoco_program_rids(dev);
if (err) {
printk(KERN_ERR "%s: Error %d configuring card\n",
dev->name, err);
return err;
}
hermes_set_irqmask(hw, ORINOCO_INTEN);
err = hermes_enable_port(hw, 0);
if (err) {
printk(KERN_ERR "%s: Error %d enabling MAC port\n",
dev->name, err);
return err;
}
netif_start_queue(dev);
netif_mark_up(dev);
return 0;
}
int __orinoco_down(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
struct hermes *hw = &priv->hw;
int err;
netif_stop_queue(dev);
netif_mark_down(dev);
if (! priv->hw_unavailable) {
if (! priv->broken_disableport) {
err = hermes_disable_port(hw, 0);
if (err) {
printk(KERN_WARNING "%s: Error %d disabling MAC port\n",
dev->name, err);
priv->broken_disableport = 1;
}
}
hermes_set_irqmask(hw, 0);
hermes_write_regn(hw, EVACK, 0xffff);
}
priv->last_linkstatus = 0xffff;
priv->connected = 0;
return 0;
}
int orinoco_reinit_firmware(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
struct hermes *hw = &priv->hw;
int err;
err = hermes_init(hw);
if (err)
return err;
err = hermes_allocate(hw, priv->nicbuf_size, &priv->txfid);
if (err == -EIO) {
printk(KERN_WARNING "%s: firmware ALLOC bug detected "
"(old Symbol firmware?). Trying to work around... ",
dev->name);
priv->nicbuf_size = TX_NICBUF_SIZE_BUG;
err = hermes_allocate(hw, priv->nicbuf_size, &priv->txfid);
if (err)
printk("failed!\n");
else
printk("ok.\n");
}
return err;
}
static int orinoco_open(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
unsigned long flags;
int err;
err = orinoco_lock(priv, &flags);
if (err)
return err;
err = __orinoco_up(dev);
if (! err)
priv->open = 1;
orinoco_unlock(priv, &flags);
return err;
}
int orinoco_stop(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
int err = 0;
spin_lock_irq(&priv->lock);
priv->open = 0;
err = __orinoco_down(dev);
spin_unlock_irq(&priv->lock);
return err;
}
static int __orinoco_program_rids(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int err;
struct hermes_idstring idbuf;
err = hermes_write_ltv(hw, USER_BAP, HERMES_RID_CNFOWNMACADDR,
HERMES_BYTES_TO_RECLEN(ETH_ALEN), dev->dev_addr);
if (err) {
printk(KERN_ERR "%s: Error %d setting MAC address\n", dev->name, err);
return err;
}
err = hermes_write_wordrec(hw, USER_BAP, HERMES_RID_CNFPORTTYPE, priv->port_type);
if (err) {
printk(KERN_ERR "%s: Error %d setting port type\n", dev->name, err);
return err;
}
if (priv->channel == 0) {
printk(KERN_DEBUG "%s: Channel is 0 in __orinoco_program_rids()\n", dev->name);
if (priv->createibss)
priv->channel = 10;
}
err = hermes_write_wordrec(hw, USER_BAP, HERMES_RID_CNFOWNCHANNEL, priv->channel);
if (err) {
printk(KERN_ERR "%s: Error %d setting channel\n", dev->name, err);
return err;
}
if (priv->has_ibss) {
err = hermes_write_wordrec(hw, USER_BAP, HERMES_RID_CNFCREATEIBSS,
priv->createibss);
if (err) {
printk(KERN_ERR "%s: Error %d setting CREATEIBSS\n", dev->name, err);
return err;
}
if ((strlen(priv->desired_essid) == 0) && (priv->createibss)
&& (!priv->has_ibss_any)) {
printk(KERN_WARNING "%s: This firmware requires an \
ESSID in IBSS-Ad-Hoc mode.\n", dev->name);
}
}
idbuf.len = cpu_to_le16(strlen(priv->desired_essid));
memcpy(&idbuf.val, priv->desired_essid, sizeof(idbuf.val));
err = hermes_write_ltv(hw, USER_BAP, HERMES_RID_CNFOWNSSID,
HERMES_BYTES_TO_RECLEN(strlen(priv->desired_essid)+2),
&idbuf);
if (err) {
printk(KERN_ERR "%s: Error %d setting OWNSSID\n", dev->name, err);
return err;
}
err = hermes_write_ltv(hw, USER_BAP, HERMES_RID_CNFDESIREDSSID,
HERMES_BYTES_TO_RECLEN(strlen(priv->desired_essid)+2),
&idbuf);
if (err) {
printk(KERN_ERR "%s: Error %d setting DESIREDSSID\n", dev->name, err);
return err;
}
idbuf.len = cpu_to_le16(strlen(priv->nick));
memcpy(&idbuf.val, priv->nick, sizeof(idbuf.val));
err = hermes_write_ltv(hw, USER_BAP, HERMES_RID_CNFOWNNAME,
HERMES_BYTES_TO_RECLEN(strlen(priv->nick)+2),
&idbuf);
if (err) {
printk(KERN_ERR "%s: Error %d setting nickname\n", dev->name, err);
return err;
}
if (priv->has_sensitivity) {
err = hermes_write_wordrec(hw, USER_BAP, HERMES_RID_CNFSYSTEMSCALE,
priv->ap_density);
if (err) {
printk(KERN_WARNING "%s: Error %d setting SYSTEMSCALE.  "
"Disabling sensitivity control\n", dev->name, err);
priv->has_sensitivity = 0;
}
}
err = hermes_write_wordrec(hw, USER_BAP, HERMES_RID_CNFRTSTHRESHOLD, priv->rts_thresh);
if (err) {
printk(KERN_ERR "%s: Error %d setting RTS threshold\n", dev->name, err);
return err;
}
if (priv->has_mwo)
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFMWOROBUST_AGERE,
priv->mwo_robust);
else
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFFRAGMENTATIONTHRESHOLD,
priv->frag_thresh);
if (err) {
printk(KERN_ERR "%s: Error %d setting framentation\n", dev->name, err);
return err;
}
err = __orinoco_hw_set_bitrate(priv);
if (err) {
printk(KERN_ERR "%s: Error %d setting bitrate\n", dev->name, err);
return err;
}
if (priv->has_pm) {
err = hermes_write_wordrec(hw, USER_BAP, HERMES_RID_CNFPMENABLED,
priv->pm_on);
if (err) {
printk(KERN_ERR "%s: Error %d setting up PM\n",
dev->name, err);
return err;
}
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFMULTICASTRECEIVE,
priv->pm_mcast);
if (err) {
printk(KERN_ERR "%s: Error %d setting up PM\n",
dev->name, err);
return err;
}
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFMAXSLEEPDURATION,
priv->pm_period);
if (err) {
printk(KERN_ERR "%s: Error %d setting up PM\n",
dev->name, err);
return err;
}
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFPMHOLDOVERDURATION,
priv->pm_timeout);
if (err) {
printk(KERN_ERR "%s: Error %d setting up PM\n",
dev->name, err);
return err;
}
}
if (priv->has_preamble) {
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFPREAMBLE_SYMBOL,
priv->preamble);
if (err) {
printk(KERN_ERR "%s: Error %d setting preamble\n",
dev->name, err);
return err;
}
}
if (priv->has_wep) {
err = __orinoco_hw_setup_wep(priv);
if (err) {
printk(KERN_ERR "%s: Error %d activating WEP\n",
dev->name, err);
return err;
}
}
priv->promiscuous = 0;
priv->mc_count = 0;
__orinoco_set_multicast_list(dev);
return 0;
}
static int orinoco_reconfigure(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
struct hermes *hw = &priv->hw;
unsigned long flags;
int err = 0;
if (priv->broken_disableport) {
schedule_work(&priv->reset_work);
return 0;
}
err = orinoco_lock(priv, &flags);
if (err)
return err;
err = hermes_disable_port(hw, 0);
if (err) {
printk(KERN_WARNING "%s: Unable to disable port while reconfiguring card\n",
dev->name);
priv->broken_disableport = 1;
goto out;
}
err = __orinoco_program_rids(dev);
if (err) {
printk(KERN_WARNING "%s: Unable to reconfigure card\n",
dev->name);
goto out;
}
err = hermes_enable_port(hw, 0);
if (err) {
printk(KERN_WARNING "%s: Unable to enable port while reconfiguring card\n",
dev->name);
goto out;
}
out:
if (err) {
printk(KERN_WARNING "%s: Resetting instead...\n", dev->name);
schedule_work(&priv->reset_work);
err = 0;
}
orinoco_unlock(priv, &flags);
return err;
}
static void orinoco_reset(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
struct hermes *hw = &priv->hw;
int err;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return;
netif_stop_queue(dev);
hermes_set_irqmask(hw, 0);
hermes_write_regn(hw, EVACK, 0xffff);
priv->hw_unavailable++;
priv->last_linkstatus = 0xffff;
priv->connected = 0;
orinoco_unlock(priv, &flags);
if (priv->hard_reset)
err = (*priv->hard_reset)(priv);
if (err) {
printk(KERN_ERR "%s: orinoco_reset: Error %d performing hard reset\n",
dev->name, err);
return;
}
err = orinoco_reinit_firmware(dev);
if (err) {
printk(KERN_ERR "%s: orinoco_reset: Error %d re-initializing firmware\n",
dev->name, err);
return;
}
spin_lock_irq(&priv->lock);
priv->hw_unavailable--;
if (priv->open && (! priv->hw_unavailable)) {
err = __orinoco_up(dev);
if (err) {
printk(KERN_ERR "%s: orinoco_reset: Error %d reenabling card\n",
dev->name, err);
} else
dev->trans_start = jiffies;
}
spin_unlock_irq(&priv->lock);
return;
}
static inline void
set_port_type(struct orinoco_private *priv)
{
switch (priv->iw_mode) {
case IW_MODE_INFRA:
priv->port_type = 1;
priv->createibss = 0;
break;
case IW_MODE_ADHOC:
if (priv->prefer_port3) {
priv->port_type = 3;
priv->createibss = 0;
} else {
priv->port_type = priv->ibss_port;
priv->createibss = 1;
}
break;
default:
printk(KERN_ERR "%s: Invalid priv->iw_mode in set_port_type()\n",
priv->ndev->name);
}
}
static inline int
is_ethersnap(struct header_struct *hdr)
{
return (memcmp(&hdr->dsap, &encaps_hdr, 5) == 0)
&& ( (hdr->oui[2] == 0x00) || (hdr->oui[2] == 0xf8) );
}
static void
orinoco_set_multicast_list(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
unsigned long flags;
if (orinoco_lock(priv, &flags) != 0) {
printk(KERN_DEBUG "%s: orinoco_set_multicast_list() "
"called when hw_unavailable\n", dev->name);
return;
}
__orinoco_set_multicast_list(dev);
orinoco_unlock(priv, &flags);
}
static int __orinoco_hw_set_bitrate(struct orinoco_private *priv)
{
hermes_t *hw = &priv->hw;
int err = 0;
if (priv->bitratemode >= BITRATE_TABLE_SIZE) {
printk(KERN_ERR "%s: BUG: Invalid bitrate mode %d\n",
priv->ndev->name, priv->bitratemode);
return -EINVAL;
}
switch (priv->firmware_type) {
case FIRMWARE_TYPE_AGERE:
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFTXRATECONTROL,
bitrate_table[priv->bitratemode].agere_txratectrl);
break;
case FIRMWARE_TYPE_INTERSIL:
case FIRMWARE_TYPE_SYMBOL:
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFTXRATECONTROL,
bitrate_table[priv->bitratemode].intersil_txratectrl);
break;
default:
BUG();
}
return err;
}
static int __orinoco_hw_setup_wep(struct orinoco_private *priv)
{
hermes_t *hw = &priv->hw;
int err = 0;
int	master_wep_flag;
int	auth_flag;
switch (priv->firmware_type) {
case FIRMWARE_TYPE_AGERE:
if (priv->wep_on) {
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFTXKEY_AGERE,
priv->tx_key);
if (err)
return err;
err = HERMES_WRITE_RECORD(hw, USER_BAP,
HERMES_RID_CNFWEPKEYS_AGERE,
&priv->keys);
if (err)
return err;
}
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFWEPENABLED_AGERE,
priv->wep_on);
if (err)
return err;
break;
case FIRMWARE_TYPE_INTERSIL:
case FIRMWARE_TYPE_SYMBOL:
master_wep_flag = 0;
if (priv->wep_on) {
int keylen;
int i;
keylen = le16_to_cpu(priv->keys[priv->tx_key].len);
for(i = 0; i < ORINOCO_MAX_KEYS; i++) {
if (keylen > LARGE_KEY_SIZE) {
printk(KERN_ERR "%s: BUG: Key %d has oversize length %d.\n",
priv->ndev->name, i, keylen);
return -E2BIG;
}
err = hermes_write_ltv(hw, USER_BAP,
HERMES_RID_CNFDEFAULTKEY0 + i,
HERMES_BYTES_TO_RECLEN(keylen),
priv->keys[i].data);
if (err)
return err;
}
err = hermes_write_wordrec(hw, USER_BAP, HERMES_RID_CNFWEPDEFAULTKEYID,
priv->tx_key);
if (err)
return err;
if (priv->wep_restrict) {
auth_flag = 2;
master_wep_flag = 3;
} else {
auth_flag = 1;
if (priv->firmware_type == FIRMWARE_TYPE_SYMBOL)
master_wep_flag = 3;
else
master_wep_flag = 1;
}
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFAUTHENTICATION, auth_flag);
if (err)
return err;
}
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFWEPFLAGS_INTERSIL,
master_wep_flag);
if (err)
return err;
break;
default:
if (priv->wep_on) {
printk(KERN_ERR "%s: WEP enabled, although not supported!\n",
priv->ndev->name);
return -EINVAL;
}
}
return 0;
}
static int orinoco_hw_get_bssid(struct orinoco_private *priv,
char buf[ETH_ALEN])
{
hermes_t *hw = &priv->hw;
int err = 0;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
err = hermes_read_ltv(hw, USER_BAP, HERMES_RID_CURRENTBSSID,
ETH_ALEN, NULL, buf);
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_hw_get_essid(struct orinoco_private *priv, int *active,
char buf[IW_ESSID_MAX_SIZE+1])
{
hermes_t *hw = &priv->hw;
int err = 0;
struct hermes_idstring essidbuf;
char *p = (char *)(&essidbuf.val);
int len;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
if (strlen(priv->desired_essid) > 0) {
u16 rid;
*active = 1;
rid = (priv->port_type == 3) ? HERMES_RID_CNFOWNSSID :
HERMES_RID_CNFDESIREDSSID;
err = hermes_read_ltv(hw, USER_BAP, rid, sizeof(essidbuf),
NULL, &essidbuf);
if (err)
goto fail_unlock;
} else {
*active = 0;
err = hermes_read_ltv(hw, USER_BAP, HERMES_RID_CURRENTSSID,
sizeof(essidbuf), NULL, &essidbuf);
if (err)
goto fail_unlock;
}
len = le16_to_cpu(essidbuf.len);
memset(buf, 0, IW_ESSID_MAX_SIZE+1);
memcpy(buf, p, len);
buf[len] = '\0';
fail_unlock:
orinoco_unlock(priv, &flags);
return err;
}
static long orinoco_hw_get_freq(struct orinoco_private *priv)
{
hermes_t *hw = &priv->hw;
int err = 0;
u16 channel;
long freq = 0;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CURRENTCHANNEL, &channel);
if (err)
goto out;
if (channel == 0) {
err = -EBUSY;
goto out;
}
if ( (channel < 1) || (channel > NUM_CHANNELS) ) {
printk(KERN_WARNING "%s: Channel out of range (%d)!\n",
priv->ndev->name, channel);
err = -EBUSY;
goto out;
}
freq = channel_frequency[channel-1] * 100000;
out:
orinoco_unlock(priv, &flags);
if (err > 0)
err = -EBUSY;
return err ? err : freq;
}
static int orinoco_hw_get_bitratelist(struct orinoco_private *priv,
int *numrates, s32 *rates, int max)
{
hermes_t *hw = &priv->hw;
struct hermes_idstring list;
unsigned char *p = (unsigned char *)&list.val;
int err = 0;
int num;
int i;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
err = hermes_read_ltv(hw, USER_BAP, HERMES_RID_SUPPORTEDDATARATES,
sizeof(list), NULL, &list);
orinoco_unlock(priv, &flags);
if (err)
return err;
num = le16_to_cpu(list.len);
*numrates = num;
num = min(num, max);
for (i = 0; i < num; i++) {
rates[i] = (p[i] & 0x7f) * 500000;
}
return 0;
}
#if 0
static void show_rx_frame(struct orinoco_rxframe_hdr *frame)
{
printk(KERN_DEBUG "RX descriptor:\n");
printk(KERN_DEBUG "  status      = 0x%04x\n", frame->desc.status);
printk(KERN_DEBUG "  time        = 0x%08x\n", frame->desc.time);
printk(KERN_DEBUG "  silence     = 0x%02x\n", frame->desc.silence);
printk(KERN_DEBUG "  signal      = 0x%02x\n", frame->desc.signal);
printk(KERN_DEBUG "  rate        = 0x%02x\n", frame->desc.rate);
printk(KERN_DEBUG "  rxflow      = 0x%02x\n", frame->desc.rxflow);
printk(KERN_DEBUG "  reserved    = 0x%08x\n", frame->desc.reserved);
printk(KERN_DEBUG "IEEE 802.11 header:\n");
printk(KERN_DEBUG "  frame_ctl   = 0x%04x\n",
frame->p80211.frame_ctl);
printk(KERN_DEBUG "  duration_id = 0x%04x\n",
frame->p80211.duration_id);
printk(KERN_DEBUG "  addr1       = %02x:%02x:%02x:%02x:%02x:%02x\n",
frame->p80211.addr1[0], frame->p80211.addr1[1],
frame->p80211.addr1[2], frame->p80211.addr1[3],
frame->p80211.addr1[4], frame->p80211.addr1[5]);
printk(KERN_DEBUG "  addr2       = %02x:%02x:%02x:%02x:%02x:%02x\n",
frame->p80211.addr2[0], frame->p80211.addr2[1],
frame->p80211.addr2[2], frame->p80211.addr2[3],
frame->p80211.addr2[4], frame->p80211.addr2[5]);
printk(KERN_DEBUG "  addr3       = %02x:%02x:%02x:%02x:%02x:%02x\n",
frame->p80211.addr3[0], frame->p80211.addr3[1],
frame->p80211.addr3[2], frame->p80211.addr3[3],
frame->p80211.addr3[4], frame->p80211.addr3[5]);
printk(KERN_DEBUG "  seq_ctl     = 0x%04x\n",
frame->p80211.seq_ctl);
printk(KERN_DEBUG "  addr4       = %02x:%02x:%02x:%02x:%02x:%02x\n",
frame->p80211.addr4[0], frame->p80211.addr4[1],
frame->p80211.addr4[2], frame->p80211.addr4[3],
frame->p80211.addr4[4], frame->p80211.addr4[5]);
printk(KERN_DEBUG "  data_len    = 0x%04x\n",
frame->p80211.data_len);
printk(KERN_DEBUG "IEEE 802.3 header:\n");
printk(KERN_DEBUG "  dest        = %02x:%02x:%02x:%02x:%02x:%02x\n",
frame->p8023.h_dest[0], frame->p8023.h_dest[1],
frame->p8023.h_dest[2], frame->p8023.h_dest[3],
frame->p8023.h_dest[4], frame->p8023.h_dest[5]);
printk(KERN_DEBUG "  src         = %02x:%02x:%02x:%02x:%02x:%02x\n",
frame->p8023.h_source[0], frame->p8023.h_source[1],
frame->p8023.h_source[2], frame->p8023.h_source[3],
frame->p8023.h_source[4], frame->p8023.h_source[5]);
printk(KERN_DEBUG "  len         = 0x%04x\n", frame->p8023.h_proto);
printk(KERN_DEBUG "IEEE 802.2 LLC/SNAP header:\n");
printk(KERN_DEBUG "  DSAP        = 0x%02x\n", frame->p8022.dsap);
printk(KERN_DEBUG "  SSAP        = 0x%02x\n", frame->p8022.ssap);
printk(KERN_DEBUG "  ctrl        = 0x%02x\n", frame->p8022.ctrl);
printk(KERN_DEBUG "  OUI         = %02x:%02x:%02x\n",
frame->p8022.oui[0], frame->p8022.oui[1], frame->p8022.oui[2]);
printk(KERN_DEBUG "  ethertype  = 0x%04x\n", frame->ethertype);
}
#endif
irqreturn_t orinoco_interrupt(int irq, void *dev_id, struct pt_regs *regs)
{
struct net_device *dev = (struct net_device *)dev_id;
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int count = MAX_IRQLOOPS_PER_IRQ;
u16 evstat, events;
static int last_irq_jiffy = 0;
static int loops_this_jiffy = 0;
unsigned long flags;
if (orinoco_lock(priv, &flags) != 0) {
return IRQ_HANDLED;
}
evstat = hermes_read_regn(hw, EVSTAT);
events = evstat & hw->inten;
if (! events) {
orinoco_unlock(priv, &flags);
return IRQ_NONE;
}
if (jiffies != last_irq_jiffy)
loops_this_jiffy = 0;
last_irq_jiffy = jiffies;
while (events && count--) {
if (++loops_this_jiffy > MAX_IRQLOOPS_PER_JIFFY) {
printk(KERN_WARNING "%s: IRQ handler is looping too "
"much! Resetting.\n", dev->name);
hermes_set_irqmask(hw, 0);
schedule_work(&priv->reset_work);
break;
}
if (! hermes_present(hw)) {
DEBUG(0, "orinoco_interrupt(): card removed\n");
break;
}
if (events & HERMES_EV_TICK)
__orinoco_ev_tick(dev, hw);
if (events & HERMES_EV_WTERR)
__orinoco_ev_wterr(dev, hw);
if (events & HERMES_EV_INFDROP)
__orinoco_ev_infdrop(dev, hw);
if (events & HERMES_EV_INFO)
__orinoco_ev_info(dev, hw);
if (events & HERMES_EV_RX)
__orinoco_ev_rx(dev, hw);
if (events & HERMES_EV_TXEXC)
__orinoco_ev_txexc(dev, hw);
if (events & HERMES_EV_TX)
__orinoco_ev_tx(dev, hw);
if (events & HERMES_EV_ALLOC)
__orinoco_ev_alloc(dev, hw);
hermes_write_regn(hw, EVACK, events);
evstat = hermes_read_regn(hw, EVSTAT);
events = evstat & hw->inten;
};
orinoco_unlock(priv, &flags);
return IRQ_HANDLED;
}
static void __orinoco_ev_tick(struct net_device *dev, hermes_t *hw)
{
printk(KERN_DEBUG "%s: TICK\n", dev->name);
}
static void __orinoco_ev_wterr(struct net_device *dev, hermes_t *hw)
{
printk(KERN_DEBUG "%s: MAC controller error (WTERR). Ignoring.\n",
dev->name);
}
static void __orinoco_ev_infdrop(struct net_device *dev, hermes_t *hw)
{
printk(KERN_WARNING "%s: Information frame lost.\n", dev->name);
}
static void print_linkstatus(struct net_device *dev, u16 status)
{
char * s;
if (suppress_linkstatus)
return;
switch (status) {
case HERMES_LINKSTATUS_NOT_CONNECTED:
s = "Not Connected";
break;
case HERMES_LINKSTATUS_CONNECTED:
s = "Connected";
break;
case HERMES_LINKSTATUS_DISCONNECTED:
s = "Disconnected";
break;
case HERMES_LINKSTATUS_AP_CHANGE:
s = "AP Changed";
break;
case HERMES_LINKSTATUS_AP_OUT_OF_RANGE:
s = "AP Out of Range";
break;
case HERMES_LINKSTATUS_AP_IN_RANGE:
s = "AP In Range";
break;
case HERMES_LINKSTATUS_ASSOC_FAILED:
s = "Association Failed";
break;
default:
s = "UNKNOWN";
}
printk(KERN_INFO "%s: New link status: %s (%04x)\n",
dev->name, s, status);
}
static void __orinoco_ev_info(struct net_device *dev, hermes_t *hw)
{
struct orinoco_private *priv = dev->priv;
u16 infofid;
struct {
u16 len;
u16 type;
} __attribute__ ((packed)) info;
int len, type;
int err;
infofid = hermes_read_regn(hw, INFOFID);
err = hermes_bap_pread(hw, IRQ_BAP, &info, sizeof(info),
infofid, 0);
if (err) {
printk(KERN_ERR "%s: error %d reading info frame. "
"Frame dropped.\n", dev->name, err);
return;
}
len = HERMES_RECLEN_TO_BYTES(le16_to_cpu(info.len));
type = le16_to_cpu(info.type);
switch (type) {
case HERMES_INQ_TALLIES: {
struct hermes_tallies_frame tallies;
struct iw_statistics *wstats = &priv->wstats;
if (len > sizeof(tallies)) {
printk(KERN_WARNING "%s: Tallies frame too long (%d bytes)\n",
dev->name, len);
len = sizeof(tallies);
}
hermes_read_words(hw, HERMES_DATA1, (void *) &tallies,
len / 2);
wstats->discard.code +=
le16_to_cpu(tallies.RxWEPUndecryptable);
if (len == sizeof(tallies))
wstats->discard.code +=
le16_to_cpu(tallies.RxDiscards_WEPICVError) +
le16_to_cpu(tallies.RxDiscards_WEPExcluded);
wstats->discard.misc +=
le16_to_cpu(tallies.TxDiscardsWrongSA);
#if WIRELESS_EXT > 11
wstats->discard.fragment +=
le16_to_cpu(tallies.RxMsgInBadMsgFragments);
wstats->discard.retries +=
le16_to_cpu(tallies.TxRetryLimitExceeded);
#endif
}
break;
case HERMES_INQ_LINKSTATUS: {
struct hermes_linkstatus linkstatus;
u16 newstatus;
if (len != sizeof(linkstatus)) {
printk(KERN_WARNING "%s: Unexpected size for linkstatus frame (%d bytes)\n",
dev->name, len);
break;
}
hermes_read_words(hw, HERMES_DATA1, (void *) &linkstatus,
len / 2);
newstatus = le16_to_cpu(linkstatus.linkstatus);
if ( (newstatus == HERMES_LINKSTATUS_CONNECTED)
|| (newstatus == HERMES_LINKSTATUS_AP_CHANGE)
|| (newstatus == HERMES_LINKSTATUS_AP_IN_RANGE) )
priv->connected = 1;
else if ( (newstatus == HERMES_LINKSTATUS_NOT_CONNECTED)
|| (newstatus == HERMES_LINKSTATUS_DISCONNECTED)
|| (newstatus == HERMES_LINKSTATUS_AP_OUT_OF_RANGE)
|| (newstatus == HERMES_LINKSTATUS_ASSOC_FAILED) )
priv->connected = 0;
if (newstatus != priv->last_linkstatus)
print_linkstatus(dev, newstatus);
priv->last_linkstatus = newstatus;
}
break;
default:
printk(KERN_DEBUG "%s: Unknown information frame received (type %04x).\n",
dev->name, type);
break;
}
}
static void __orinoco_ev_rx(struct net_device *dev, hermes_t *hw)
{
struct orinoco_private *priv = dev->priv;
struct net_device_stats *stats = &priv->stats;
struct iw_statistics *wstats = &priv->wstats;
struct sk_buff *skb = NULL;
u16 rxfid, status;
int length, data_len, data_off;
char *p;
struct hermes_rx_descriptor desc;
struct header_struct hdr;
struct ethhdr *eh;
int err;
rxfid = hermes_read_regn(hw, RXFID);
err = hermes_bap_pread(hw, IRQ_BAP, &desc, sizeof(desc),
rxfid, 0);
if (err) {
printk(KERN_ERR "%s: error %d reading Rx descriptor. "
"Frame dropped.\n", dev->name, err);
stats->rx_errors++;
goto drop;
}
status = le16_to_cpu(desc.status);
if (status & HERMES_RXSTAT_ERR) {
if (status & HERMES_RXSTAT_UNDECRYPTABLE) {
wstats->discard.code++;
DEBUG(1, "%s: Undecryptable frame on Rx. Frame dropped.\n",
dev->name);
} else {
stats->rx_crc_errors++;
DEBUG(1, "%s: Bad CRC on Rx. Frame dropped.\n", dev->name);
}
stats->rx_errors++;
goto drop;
}
err = hermes_bap_pread(hw, IRQ_BAP, &hdr, sizeof(hdr),
rxfid, HERMES_802_3_OFFSET);
if (err) {
printk(KERN_ERR "%s: error %d reading frame header. "
"Frame dropped.\n", dev->name, err);
stats->rx_errors++;
goto drop;
}
length = ntohs(hdr.len);
if (length < 3) {
stats->rx_dropped++;
goto drop;
}
if (length > IEEE802_11_DATA_LEN) {
printk(KERN_WARNING "%s: Oversized frame received (%d bytes)\n",
dev->name, length);
stats->rx_length_errors++;
stats->rx_errors++;
goto drop;
}
skb = dev_alloc_skb(length+ETH_HLEN+2+1);
if (!skb) {
printk(KERN_WARNING "%s: Can't allocate skb for Rx\n",
dev->name);
goto drop;
}
skb_reserve(skb, 2);
if(((status & HERMES_RXSTAT_MSGTYPE) == HERMES_RXSTAT_1042) ||
((status & HERMES_RXSTAT_MSGTYPE) == HERMES_RXSTAT_TUNNEL) ||
is_ethersnap(&hdr)) {
if (length < ENCAPS_OVERHEAD) {
stats->rx_length_errors++;
goto drop;
}
data_len = length - ENCAPS_OVERHEAD;
data_off = HERMES_802_3_OFFSET + sizeof(hdr);
eh = (struct ethhdr *)skb_put(skb, ETH_HLEN);
memcpy(eh, &hdr, 2 * ETH_ALEN);
eh->h_proto = hdr.ethertype;
} else {
data_len = length;
data_off = HERMES_802_3_OFFSET;
}
p = skb_put(skb, data_len);
err = hermes_bap_pread(hw, IRQ_BAP, p, RUP_EVEN(data_len),
rxfid, data_off);
if (err) {
printk(KERN_ERR "%s: error %d reading frame. "
"Frame dropped.\n", dev->name, err);
stats->rx_errors++;
goto drop;
}
dev->last_rx = jiffies;
skb->dev = dev;
skb->protocol = eth_type_trans(skb, dev);
skb->ip_summed = CHECKSUM_NONE;
orinoco_stat_gather(dev, skb, &desc);
netif_rx(skb);
stats->rx_packets++;
return;
drop:
stats->rx_dropped++;
if (skb)
dev_kfree_skb_irq(skb);
return;
}
static void __orinoco_ev_txexc(struct net_device *dev, hermes_t *hw)
{
struct orinoco_private *priv = dev->priv;
struct net_device_stats *stats = &priv->stats;
u16 fid = hermes_read_regn(hw, TXCOMPLFID);
struct hermes_tx_descriptor desc;
int err = 0;
if (fid == DUMMY_FID)
return;
err = hermes_bap_pread(hw, IRQ_BAP, &desc, sizeof(desc), fid, 0);
if (err) {
printk(KERN_WARNING "%s: Unable to read descriptor on Tx error "
"(FID=%04X error %d)\n",
dev->name, fid, err);
} else {
DEBUG(1, "%s: Tx error, status %d\n",
dev->name, le16_to_cpu(desc.status));
}
stats->tx_errors++;
hermes_write_regn(hw, TXCOMPLFID, DUMMY_FID);
}
static void __orinoco_ev_tx(struct net_device *dev, hermes_t *hw)
{
struct orinoco_private *priv = dev->priv;
struct net_device_stats *stats = &priv->stats;
stats->tx_packets++;
hermes_write_regn(hw, TXCOMPLFID, DUMMY_FID);
}
static void __orinoco_ev_alloc(struct net_device *dev, hermes_t *hw)
{
struct orinoco_private *priv = dev->priv;
u16 fid = hermes_read_regn(hw, ALLOCFID);
if (fid != priv->txfid) {
if (fid != DUMMY_FID)
printk(KERN_WARNING "%s: Allocate event on unexpected fid (%04X)\n",
dev->name, fid);
return;
} else {
netif_wake_queue(dev);
}
hermes_write_regn(hw, ALLOCFID, DUMMY_FID);
}
struct sta_id {
u16 id, variant, major, minor;
} __attribute__ ((packed));
static int determine_firmware_type(struct net_device *dev, struct sta_id *sta_id)
{
unsigned int firmver = ((u32)sta_id->major << 16) | sta_id->minor;
if (sta_id->variant == 1)
return FIRMWARE_TYPE_AGERE;
else if ((sta_id->variant == 2) &&
((firmver == 0x10001) || (firmver == 0x20001)))
return FIRMWARE_TYPE_SYMBOL;
else
return FIRMWARE_TYPE_INTERSIL;
}
static void determine_firmware(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int err;
struct sta_id sta_id;
unsigned int firmver;
char tmp[SYMBOL_MAX_VER_LEN+1];
err = HERMES_READ_RECORD(hw, USER_BAP, HERMES_RID_STAID, &sta_id);
if (err) {
printk(KERN_WARNING "%s: Error %d reading firmware info. Wildly guessing capabilities...\n",
dev->name, err);
memset(&sta_id, 0, sizeof(sta_id));
}
le16_to_cpus(&sta_id.id);
le16_to_cpus(&sta_id.variant);
le16_to_cpus(&sta_id.major);
le16_to_cpus(&sta_id.minor);
printk(KERN_DEBUG "%s: Station identity %04x:%04x:%04x:%04x\n",
dev->name, sta_id.id, sta_id.variant,
sta_id.major, sta_id.minor);
if (! priv->firmware_type)
priv->firmware_type = determine_firmware_type(dev, &sta_id);
priv->has_sensitivity = 1;
priv->has_mwo = 0;
priv->has_preamble = 0;
priv->has_port3 = 1;
priv->has_ibss = 1;
priv->has_ibss_any = 0;
priv->has_wep = 0;
priv->has_big_wep = 0;
switch (priv->firmware_type) {
case FIRMWARE_TYPE_AGERE:
printk(KERN_DEBUG "%s: Looks like a Lucent/Agere firmware "
"version %d.%02d\n", dev->name,
sta_id.major, sta_id.minor);
firmver = ((unsigned long)sta_id.major << 16) | sta_id.minor;
priv->has_ibss = (firmver >= 0x60006);
priv->has_ibss_any = (firmver >= 0x60010);
priv->has_wep = (firmver >= 0x40020);
priv->has_big_wep = 1;
priv->has_mwo = (firmver >= 0x60000);
priv->has_pm = (firmver >= 0x40020);
priv->ibss_port = 1;
break;
case FIRMWARE_TYPE_SYMBOL:
memset(tmp, 0, sizeof(tmp));
err = hermes_read_ltv(hw, USER_BAP,
HERMES_RID_SECONDARYVERSION_SYMBOL,
SYMBOL_MAX_VER_LEN, NULL, &tmp);
if (err) {
printk(KERN_WARNING
"%s: Error %d reading Symbol firmware info. Wildly guessing capabilities...\n",
dev->name, err);
firmver = 0;
tmp[0] = '\0';
} else {
firmver = ((tmp[1] - '0') << 16) | ((tmp[3] - '0') << 12)
| ((tmp[4] - '0') << 8) | ((tmp[6] - '0') << 4)
| (tmp[7] - '0');
tmp[SYMBOL_MAX_VER_LEN] = '\0';
}
printk(KERN_DEBUG "%s: Looks like a Symbol firmware "
"version [%s] (parsing to %X)\n", dev->name,
tmp, firmver);
priv->has_ibss = (firmver >= 0x20000);
priv->has_wep = (firmver >= 0x15012);
priv->has_big_wep = (firmver >= 0x20000);
priv->has_pm = (firmver >= 0x20000) && (firmver < 0x22000);
priv->has_preamble = (firmver >= 0x20000);
priv->ibss_port = 4;
break;
case FIRMWARE_TYPE_INTERSIL:
printk(KERN_DEBUG "%s: Looks like an Intersil firmware "
"version %d.%d.%d\n", dev->name,
sta_id.major, sta_id.minor, sta_id.variant);
firmver = ((unsigned long)sta_id.major << 16) |
((unsigned long)sta_id.minor << 8) | sta_id.variant;
priv->has_ibss = (firmver >= 0x000700);
priv->has_big_wep = priv->has_wep = (firmver >= 0x000800);
priv->has_pm = (firmver >= 0x000700);
if (firmver >= 0x000800)
priv->ibss_port = 0;
else {
printk(KERN_NOTICE "%s: Intersil firmware earlier "
"than v0.8.x - several features not supported\n",
dev->name);
priv->ibss_port = 1;
}
break;
default:
break;
}
}
static int
orinoco_init(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int err = 0;
struct hermes_idstring nickbuf;
u16 reclen;
int len;
TRACE_ENTER(dev->name);
priv->nicbuf_size = IEEE802_11_FRAME_LEN + ETH_HLEN;
err = hermes_init(hw);
if (err != 0) {
printk(KERN_ERR "%s: failed to initialize firmware (err = %d)\n",
dev->name, err);
goto out;
}
determine_firmware(dev);
if (priv->has_port3)
printk(KERN_DEBUG "%s: Ad-hoc demo mode supported\n", dev->name);
if (priv->has_ibss)
printk(KERN_DEBUG "%s: IEEE standard IBSS ad-hoc mode supported\n",
dev->name);
if (priv->has_wep) {
printk(KERN_DEBUG "%s: WEP supported, ", dev->name);
if (priv->has_big_wep)
printk("104-bit key\n");
else
printk("40-bit key\n");
}
err = hermes_read_ltv(hw, USER_BAP, HERMES_RID_CNFOWNMACADDR,
ETH_ALEN, NULL, dev->dev_addr);
if (err) {
printk(KERN_WARNING "%s: failed to read MAC address!\n",
dev->name);
goto out;
}
printk(KERN_DEBUG "%s: MAC address %02X:%02X:%02X:%02X:%02X:%02X\n",
dev->name, dev->dev_addr[0], dev->dev_addr[1],
dev->dev_addr[2], dev->dev_addr[3], dev->dev_addr[4],
dev->dev_addr[5]);
err = hermes_read_ltv(hw, USER_BAP, HERMES_RID_CNFOWNNAME,
sizeof(nickbuf), &reclen, &nickbuf);
if (err) {
printk(KERN_ERR "%s: failed to read station name\n",
dev->name);
goto out;
}
if (nickbuf.len)
len = min(IW_ESSID_MAX_SIZE, (int)le16_to_cpu(nickbuf.len));
else
len = min(IW_ESSID_MAX_SIZE, 2 * reclen);
memcpy(priv->nick, &nickbuf.val, len);
priv->nick[len] = '\0';
printk(KERN_DEBUG "%s: Station name \"%s\"\n", dev->name, priv->nick);
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CHANNELLIST,
&priv->channel_mask);
if (err) {
printk(KERN_ERR "%s: failed to read channel list!\n",
dev->name);
goto out;
}
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFSYSTEMSCALE,
&priv->ap_density);
if (err || priv->ap_density < 1 || priv->ap_density > 3) {
priv->has_sensitivity = 0;
}
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFRTSTHRESHOLD,
&priv->rts_thresh);
if (err) {
printk(KERN_ERR "%s: failed to read RTS threshold!\n", dev->name);
goto out;
}
if (priv->has_mwo)
err = hermes_read_wordrec(hw, USER_BAP,
HERMES_RID_CNFMWOROBUST_AGERE,
&priv->mwo_robust);
else
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFFRAGMENTATIONTHRESHOLD,
&priv->frag_thresh);
if (err) {
printk(KERN_ERR "%s: failed to read fragmentation settings!\n", dev->name);
goto out;
}
if (priv->has_pm) {
priv->pm_on = 0;
priv->pm_mcast = 1;
err = hermes_read_wordrec(hw, USER_BAP,
HERMES_RID_CNFMAXSLEEPDURATION,
&priv->pm_period);
if (err) {
printk(KERN_ERR "%s: failed to read power management period!\n",
dev->name);
goto out;
}
err = hermes_read_wordrec(hw, USER_BAP,
HERMES_RID_CNFPMHOLDOVERDURATION,
&priv->pm_timeout);
if (err) {
printk(KERN_ERR "%s: failed to read power management timeout!\n",
dev->name);
goto out;
}
}
if (priv->has_preamble) {
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFPREAMBLE_SYMBOL,
&priv->preamble);
if (err)
goto out;
}
priv->iw_mode = IW_MODE_INFRA;
priv->prefer_port3 = priv->has_port3 && (! priv->has_ibss);
set_port_type(priv);
priv->channel = 10;
priv->promiscuous = 0;
priv->wep_on = 0;
priv->tx_key = 0;
err = hermes_allocate(hw, priv->nicbuf_size, &priv->txfid);
if (err == -EIO) {
printk(KERN_WARNING "%s: firmware ALLOC bug detected "
"(old Symbol firmware?). Trying to work around... ",
dev->name);
priv->nicbuf_size = TX_NICBUF_SIZE_BUG;
err = hermes_allocate(hw, priv->nicbuf_size, &priv->txfid);
if (err)
printk("failed!\n");
else
printk("ok.\n");
}
if (err) {
printk("%s: Error %d allocating Tx buffer\n", dev->name, err);
goto out;
}
spin_lock_irq(&priv->lock);
priv->hw_unavailable--;
spin_unlock_irq(&priv->lock);
printk(KERN_DEBUG "%s: ready\n", dev->name);
out:
TRACE_EXIT(dev->name);
return err;
}
struct net_device_stats *
orinoco_get_stats(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
return &priv->stats;
}
struct iw_statistics *
orinoco_get_wireless_stats(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
struct iw_statistics *wstats = &priv->wstats;
int err = 0;
unsigned long flags;
if (! netif_device_present(dev)) {
printk(KERN_WARNING "%s: get_wireless_stats() called while device not present\n",
dev->name);
return NULL;
}
err = orinoco_lock(priv, &flags);
if (err)
return NULL;
if (priv->iw_mode == IW_MODE_ADHOC) {
memset(&wstats->qual, 0, sizeof(wstats->qual));
if (SPY_NUMBER(priv)) {
wstats->qual.qual = priv->spy_stat[0].qual;
wstats->qual.level = priv->spy_stat[0].level;
wstats->qual.noise = priv->spy_stat[0].noise;
wstats->qual.updated = priv->spy_stat[0].updated;
}
} else {
struct {
u16 qual, signal, noise;
} __attribute__ ((packed)) cq;
err = HERMES_READ_RECORD(hw, USER_BAP,
HERMES_RID_COMMSQUALITY, &cq);
wstats->qual.qual = (int)le16_to_cpu(cq.qual);
wstats->qual.level = (int)le16_to_cpu(cq.signal) - 0x95;
wstats->qual.noise = (int)le16_to_cpu(cq.noise) - 0x95;
wstats->qual.updated = 7;
}
err = hermes_inquire(hw, HERMES_INQ_TALLIES);
orinoco_unlock(priv, &flags);
if (err)
return NULL;
return wstats;
}
static inline void orinoco_spy_gather(struct net_device *dev, u_char *mac,
int level, int noise)
{
struct orinoco_private *priv = (struct orinoco_private *)dev->priv;
int i;
for (i = 0; i < priv->spy_number; i++)
if (!memcmp(mac, priv->spy_address[i], ETH_ALEN)) {
priv->spy_stat[i].level = level - 0x95;
priv->spy_stat[i].noise = noise - 0x95;
priv->spy_stat[i].qual = (level > noise) ? (level - noise) : 0;
priv->spy_stat[i].updated = 7;
}
}
void
orinoco_stat_gather(struct net_device *dev,
struct sk_buff *skb,
struct hermes_rx_descriptor *desc)
{
struct orinoco_private *priv = (struct orinoco_private *)dev->priv;
if (SPY_NUMBER(priv)) {
orinoco_spy_gather(dev, skb->mac.raw + ETH_ALEN,
desc->signal, desc->silence);
}
}
static int
orinoco_xmit(struct sk_buff *skb, struct net_device *dev)
{
struct orinoco_private *priv = (struct orinoco_private *)dev->priv;
struct net_device_stats *stats = &priv->stats;
hermes_t *hw = &priv->hw;
int err = 0;
u16 txfid = priv->txfid;
char *p;
struct ethhdr *eh;
int len, data_len, data_off;
struct hermes_tx_descriptor desc;
unsigned long flags;
TRACE_ENTER(dev->name);
if (! netif_running(dev)) {
printk(KERN_ERR "%s: Tx on stopped device!\n",
dev->name);
TRACE_EXIT(dev->name);
return 1;
}
if (netif_queue_stopped(dev)) {
printk(KERN_DEBUG "%s: Tx while transmitter busy!\n",
dev->name);
TRACE_EXIT(dev->name);
return 1;
}
if (orinoco_lock(priv, &flags) != 0) {
printk(KERN_ERR "%s: orinoco_xmit() called while hw_unavailable\n",
dev->name);
TRACE_EXIT(dev->name);
return 1;
}
if (! priv->connected) {
stats->tx_errors++;
orinoco_unlock(priv, &flags);
dev_kfree_skb(skb, FREE_WRITE);
TRACE_EXIT(dev->name);
return 0;
}
len = max_t(int,skb->len - ETH_HLEN, ETH_ZLEN - ETH_HLEN);
eh = (struct ethhdr *)skb->data;
memset(&desc, 0, sizeof(desc));
desc.tx_control = cpu_to_le16(HERMES_TXCTRL_TX_OK | HERMES_TXCTRL_TX_EX);
err = hermes_bap_pwrite(hw, USER_BAP, &desc, sizeof(desc), txfid, 0);
if (err) {
printk(KERN_ERR "%s: Error %d writing Tx descriptor to BAP\n",
dev->name, err);
stats->tx_errors++;
goto fail;
}
hermes_clear_words(hw, HERMES_DATA0,
HERMES_802_3_OFFSET - HERMES_802_11_OFFSET);
if (ntohs(eh->h_proto) > 1500) {
struct header_struct hdr;
data_len = len;
data_off = HERMES_802_3_OFFSET + sizeof(hdr);
p = skb->data + ETH_HLEN;
memcpy(hdr.dest, eh->h_dest, ETH_ALEN);
memcpy(hdr.src, eh->h_source, ETH_ALEN);
hdr.len = htons(data_len + ENCAPS_OVERHEAD);
memcpy(&hdr.dsap, &encaps_hdr, sizeof(encaps_hdr));
hdr.ethertype = eh->h_proto;
err  = hermes_bap_pwrite(hw, USER_BAP, &hdr, sizeof(hdr),
txfid, HERMES_802_3_OFFSET);
if (err) {
printk(KERN_ERR "%s: Error %d writing packet header to BAP\n",
dev->name, err);
stats->tx_errors++;
goto fail;
}
} else {
data_len = len + ETH_HLEN;
data_off = HERMES_802_3_OFFSET;
p = skb->data;
}
err = hermes_bap_pwrite(hw, USER_BAP, p, RUP_EVEN(data_len), txfid, data_off);
if (err) {
printk(KERN_ERR "%s: Error %d writing packet to BAP\n",
dev->name, err);
stats->tx_errors++;
goto fail;
}
netif_stop_queue(dev);
err = hermes_docmd_wait(hw, HERMES_CMD_TX | HERMES_CMD_RECL, txfid, NULL);
if (err) {
netif_start_queue(dev);
printk(KERN_ERR "%s: Error %d transmitting packet\n", dev->name, err);
stats->tx_errors++;
goto fail;
}
dev->trans_start = jiffies;
orinoco_unlock(priv, &flags);
DEV_KFREE_SKB(skb);
TRACE_EXIT(dev->name);
return 0;
fail:
TRACE_EXIT(dev->name);
orinoco_unlock(priv, &flags);
return err;
}
#ifdef HAVE_TX_TIMEOUT
static void
orinoco_tx_timeout(struct net_device *dev)
{
struct orinoco_private *priv = (struct orinoco_private *)dev->priv;
struct net_device_stats *stats = &priv->stats;
struct hermes *hw = &priv->hw;
printk(KERN_WARNING "%s: Tx timeout! "
"ALLOCFID=%04x, TXCOMPLFID=%04x, EVSTAT=%04x\n",
dev->name, hermes_read_regn(hw, ALLOCFID),
hermes_read_regn(hw, TXCOMPLFID), hermes_read_regn(hw, EVSTAT));
stats->tx_errors++;
schedule_work(&priv->reset_work);
}
#endif
static int
orinoco_change_mtu(struct net_device *dev, int new_mtu)
{
struct orinoco_private *priv = dev->priv;
if ( (new_mtu < ORINOCO_MIN_MTU) || (new_mtu > ORINOCO_MAX_MTU) )
return -EINVAL;
if ( (new_mtu + ENCAPS_OVERHEAD + IEEE802_11_HLEN) >
(priv->nicbuf_size - ETH_HLEN) )
return -EINVAL;
dev->mtu = new_mtu;
return 0;
}
static void
__orinoco_set_multicast_list(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int err = 0;
int promisc, mc_count;
if ( (dev->flags & IFF_PROMISC) || (dev->flags & IFF_ALLMULTI) ||
(dev->mc_count > MAX_MULTICAST(priv)) ) {
promisc = 1;
mc_count = 0;
} else {
promisc = 0;
mc_count = dev->mc_count;
}
if (promisc != priv->promiscuous) {
err = hermes_write_wordrec(hw, USER_BAP,
HERMES_RID_CNFPROMISCUOUSMODE,
promisc);
if (err) {
printk(KERN_ERR "%s: Error %d setting PROMISCUOUSMODE to 1.\n",
dev->name, err);
} else
priv->promiscuous = promisc;
}
if (! promisc && (mc_count || priv->mc_count) ) {
struct dev_mc_list *p = dev->mc_list;
hermes_multicast_t mclist;
int i;
for (i = 0; i < mc_count; i++) {
if (! p)
BUG();
if (p->dmi_addrlen != ETH_ALEN)
BUG();
memcpy(mclist.addr[i], p->dmi_addr, ETH_ALEN);
p = p->next;
}
if (p)
printk(KERN_WARNING "Multicast list is longer than mc_count\n");
err = hermes_write_ltv(hw, USER_BAP, HERMES_RID_CNFGROUPADDRESSES,
HERMES_BYTES_TO_RECLEN(priv->mc_count * ETH_ALEN),
&mclist);
if (err)
printk(KERN_ERR "%s: Error %d setting multicast list.\n",
dev->name, err);
else
priv->mc_count = mc_count;
}
if (priv->promiscuous)
dev->flags |= IFF_PROMISC;
else
dev->flags &= ~IFF_PROMISC;
}
static int orinoco_ioctl_getiwrange(struct net_device *dev, struct iw_point *rrq)
{
struct orinoco_private *priv = dev->priv;
int err = 0;
int mode;
struct iw_range range;
int numrates;
int i, k;
unsigned long flags;
TRACE_ENTER(dev->name);
err = verify_area(VERIFY_WRITE, rrq->pointer, sizeof(range));
if (err)
return err;
rrq->length = sizeof(range);
err = orinoco_lock(priv, &flags);
if (err)
return err;
mode = priv->iw_mode;
orinoco_unlock(priv, &flags);
memset(&range, 0, sizeof(range));
#if WIRELESS_EXT > 10
range.we_version_compiled = WIRELESS_EXT;
range.we_version_source = 11;
#endif
range.min_nwid = range.max_nwid = 0;
range.num_channels = NUM_CHANNELS;
k = 0;
for (i = 0; i < NUM_CHANNELS; i++) {
if (priv->channel_mask & (1 << i)) {
range.freq[k].i = i + 1;
range.freq[k].m = channel_frequency[i] * 100000;
range.freq[k].e = 1;
k++;
}
if (k >= IW_MAX_FREQUENCIES)
break;
}
range.num_frequency = k;
range.sensitivity = 3;
if ((mode == IW_MODE_ADHOC) && (priv->spy_number == 0)){
range.max_qual.qual = 0;
range.max_qual.level = 0;
range.max_qual.noise = 0;
#if WIRELESS_EXT > 11
range.avg_qual.qual = 0;
range.avg_qual.level = 0;
range.avg_qual.noise = 0;
#endif
} else {
range.max_qual.qual = 0x8b - 0x2f;
range.max_qual.level = 0x2f - 0x95 - 1;
range.max_qual.noise = 0x2f - 0x95 - 1;
#if WIRELESS_EXT > 11
range.avg_qual.qual = 0x24;
range.avg_qual.level = 0xC2;
range.avg_qual.noise = 0x9E;
#endif
}
err = orinoco_hw_get_bitratelist(priv, &numrates,
range.bitrate, IW_MAX_BITRATES);
if (err)
return err;
range.num_bitrates = numrates;
if(numrates > 2)
range.throughput = 5 * 1000 * 1000;
else
range.throughput = 1.5 * 1000 * 1000;
range.min_rts = 0;
range.max_rts = 2347;
range.min_frag = 256;
range.max_frag = 2346;
err = orinoco_lock(priv, &flags);
if (err)
return err;
if (priv->has_wep) {
range.max_encoding_tokens = ORINOCO_MAX_KEYS;
range.encoding_size[0] = SMALL_KEY_SIZE;
range.num_encoding_sizes = 1;
if (priv->has_big_wep) {
range.encoding_size[1] = LARGE_KEY_SIZE;
range.num_encoding_sizes = 2;
}
} else {
range.num_encoding_sizes = 0;
range.max_encoding_tokens = 0;
}
orinoco_unlock(priv, &flags);
range.min_pmp = 0;
range.max_pmp = 65535000;
range.min_pmt = 0;
range.max_pmt = 65535 * 1000;
range.pmp_flags = IW_POWER_PERIOD;
range.pmt_flags = IW_POWER_TIMEOUT;
range.pm_capa = IW_POWER_PERIOD | IW_POWER_TIMEOUT | IW_POWER_UNICAST_R;
range.num_txpower = 1;
range.txpower[0] = 15;
range.txpower_capa = IW_TXPOW_DBM;
#if WIRELESS_EXT > 10
range.retry_capa = IW_RETRY_LIMIT | IW_RETRY_LIFETIME;
range.retry_flags = IW_RETRY_LIMIT;
range.r_time_flags = IW_RETRY_LIFETIME;
range.min_retry = 0;
range.max_retry = 65535;
range.min_r_time = 0;
range.max_r_time = 65535 * 1000;
#endif
if (copy_to_user(rrq->pointer, &range, sizeof(range)))
return -EFAULT;
TRACE_EXIT(dev->name);
return 0;
}
static int orinoco_ioctl_setiwencode(struct net_device *dev, struct iw_point *erq)
{
struct orinoco_private *priv = dev->priv;
int index = (erq->flags & IW_ENCODE_INDEX) - 1;
int setindex = priv->tx_key;
int enable = priv->wep_on;
int restricted = priv->wep_restrict;
u16 xlen = 0;
int err = 0;
char keybuf[ORINOCO_MAX_KEY_SIZE];
unsigned long flags;
if (erq->pointer) {
if ( (erq->length < SMALL_KEY_SIZE) || (erq->length > ORINOCO_MAX_KEY_SIZE) )
return -EINVAL;
if (copy_from_user(keybuf, erq->pointer, erq->length))
return -EFAULT;
}
err = orinoco_lock(priv, &flags);
if (err)
return err;
if (erq->pointer) {
if (erq->length > ORINOCO_MAX_KEY_SIZE) {
err = -E2BIG;
goto out;
}
if ( (erq->length > LARGE_KEY_SIZE)
|| ( ! priv->has_big_wep && (erq->length > SMALL_KEY_SIZE))  ) {
err = -EINVAL;
goto out;
}
if ((index < 0) || (index >= ORINOCO_MAX_KEYS))
index = priv->tx_key;
if (erq->length > SMALL_KEY_SIZE) {
xlen = LARGE_KEY_SIZE;
} else if (erq->length > 0) {
xlen = SMALL_KEY_SIZE;
} else
xlen = 0;
if ((!enable) && (xlen > 0)) {
setindex = index;
enable = 1;
}
} else {
if ((index < 0) || (index >= ORINOCO_MAX_KEYS)) {
if((index != -1) || (erq->flags == 0)) {
err = -EINVAL;
goto out;
}
} else {
if(priv->keys[index].len == 0) {
err = -EINVAL;
goto out;
}
setindex = index;
}
}
if (erq->flags & IW_ENCODE_DISABLED)
enable = 0;
if (erq->flags & IW_ENCODE_OPEN)
restricted = 0;
if (erq->flags & IW_ENCODE_RESTRICTED)
restricted = 1;
if (erq->pointer) {
priv->keys[index].len = cpu_to_le16(xlen);
memset(priv->keys[index].data, 0, sizeof(priv->keys[index].data));
memcpy(priv->keys[index].data, keybuf, erq->length);
}
priv->tx_key = setindex;
priv->wep_on = enable;
priv->wep_restrict = restricted;
out:
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_ioctl_getiwencode(struct net_device *dev, struct iw_point *erq)
{
struct orinoco_private *priv = dev->priv;
int index = (erq->flags & IW_ENCODE_INDEX) - 1;
u16 xlen = 0;
char keybuf[ORINOCO_MAX_KEY_SIZE];
int err;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
if ((index < 0) || (index >= ORINOCO_MAX_KEYS))
index = priv->tx_key;
erq->flags = 0;
if (! priv->wep_on)
erq->flags |= IW_ENCODE_DISABLED;
erq->flags |= index + 1;
if (priv->firmware_type != FIRMWARE_TYPE_AGERE) {
if(priv->wep_restrict)
erq->flags |= IW_ENCODE_RESTRICTED;
else
erq->flags |= IW_ENCODE_OPEN;
}
xlen = le16_to_cpu(priv->keys[index].len);
erq->length = xlen;
if (erq->pointer) {
memcpy(keybuf, priv->keys[index].data, ORINOCO_MAX_KEY_SIZE);
}
orinoco_unlock(priv, &flags);
if (erq->pointer) {
if (copy_to_user(erq->pointer, keybuf, xlen))
return -EFAULT;
}
return 0;
}
static int orinoco_ioctl_setessid(struct net_device *dev, struct iw_point *erq)
{
struct orinoco_private *priv = dev->priv;
char essidbuf[IW_ESSID_MAX_SIZE+1];
int err;
unsigned long flags;
memset(&essidbuf, 0, sizeof(essidbuf));
if (erq->flags) {
if (erq->length > IW_ESSID_MAX_SIZE)
return -E2BIG;
if (copy_from_user(&essidbuf, erq->pointer, erq->length))
return -EFAULT;
essidbuf[erq->length] = '\0';
}
err = orinoco_lock(priv, &flags);
if (err)
return err;
memcpy(priv->desired_essid, essidbuf, sizeof(priv->desired_essid));
orinoco_unlock(priv, &flags);
return 0;
}
static int orinoco_ioctl_getessid(struct net_device *dev, struct iw_point *erq)
{
struct orinoco_private *priv = dev->priv;
char essidbuf[IW_ESSID_MAX_SIZE+1];
int active;
int err = 0;
unsigned long flags;
TRACE_ENTER(dev->name);
if (netif_running(dev)) {
err = orinoco_hw_get_essid(priv, &active, essidbuf);
if (err)
return err;
} else {
err = orinoco_lock(priv, &flags);
if (err)
return err;
memcpy(essidbuf, priv->desired_essid, sizeof(essidbuf));
orinoco_unlock(priv, &flags);
}
erq->flags = 1;
erq->length = strlen(essidbuf) + 1;
if (erq->pointer)
if (copy_to_user(erq->pointer, essidbuf, erq->length))
return -EFAULT;
TRACE_EXIT(dev->name);
return 0;
}
static int orinoco_ioctl_setnick(struct net_device *dev, struct iw_point *nrq)
{
struct orinoco_private *priv = dev->priv;
char nickbuf[IW_ESSID_MAX_SIZE+1];
int err;
unsigned long flags;
if (nrq->length > IW_ESSID_MAX_SIZE)
return -E2BIG;
memset(nickbuf, 0, sizeof(nickbuf));
if (copy_from_user(nickbuf, nrq->pointer, nrq->length))
return -EFAULT;
nickbuf[nrq->length] = '\0';
err = orinoco_lock(priv, &flags);
if (err)
return err;
memcpy(priv->nick, nickbuf, sizeof(priv->nick));
orinoco_unlock(priv, &flags);
return 0;
}
static int orinoco_ioctl_getnick(struct net_device *dev, struct iw_point *nrq)
{
struct orinoco_private *priv = dev->priv;
char nickbuf[IW_ESSID_MAX_SIZE+1];
int err;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
memcpy(nickbuf, priv->nick, IW_ESSID_MAX_SIZE+1);
orinoco_unlock(priv, &flags);
nrq->length = strlen(nickbuf)+1;
#ifdef MACH
if(! nrq->pointer) {
printk(KERN_INFO "orinoco_ioctl_getnick: no nrq pointer.\n");
return -EFAULT;
}
#endif
if (copy_to_user(nrq->pointer, nickbuf, sizeof(nickbuf)))
return -EFAULT;
return 0;
}
static int orinoco_ioctl_setfreq(struct net_device *dev, struct iw_freq *frq)
{
struct orinoco_private *priv = dev->priv;
int chan = -1;
int err;
unsigned long flags;
if (priv->iw_mode != IW_MODE_ADHOC)
return -EOPNOTSUPP;
if ( (frq->e == 0) && (frq->m <= 1000) ) {
chan = frq->m;
} else {
int mult = 1;
int i;
for (i = 0; i < (6 - frq->e); i++)
mult *= 10;
for (i = 0; i < NUM_CHANNELS; i++)
if (frq->m == (channel_frequency[i] * mult))
chan = i+1;
}
if ( (chan < 1) || (chan > NUM_CHANNELS) ||
! (priv->channel_mask & (1 << (chan-1)) ) )
return -EINVAL;
err = orinoco_lock(priv, &flags);
if (err)
return err;
priv->channel = chan;
orinoco_unlock(priv, &flags);
return 0;
}
static int orinoco_ioctl_getsens(struct net_device *dev, struct iw_param *srq)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
u16 val;
int err;
unsigned long flags;
if (!priv->has_sensitivity)
return -EOPNOTSUPP;
err = orinoco_lock(priv, &flags);
if (err)
return err;
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFSYSTEMSCALE, &val);
orinoco_unlock(priv, &flags);
if (err)
return err;
srq->value = val;
srq->fixed = 0;
return 0;
}
static int orinoco_ioctl_setsens(struct net_device *dev, struct iw_param *srq)
{
struct orinoco_private *priv = dev->priv;
int val = srq->value;
int err;
unsigned long flags;
if (!priv->has_sensitivity)
return -EOPNOTSUPP;
if ((val < 1) || (val > 3))
return -EINVAL;
err = orinoco_lock(priv, &flags);
if (err)
return err;
priv->ap_density = val;
orinoco_unlock(priv, &flags);
return 0;
}
static int orinoco_ioctl_setrts(struct net_device *dev, struct iw_param *rrq)
{
struct orinoco_private *priv = dev->priv;
int val = rrq->value;
int err;
unsigned long flags;
if (rrq->disabled)
val = 2347;
if ( (val < 0) || (val > 2347) )
return -EINVAL;
err = orinoco_lock(priv, &flags);
if (err)
return err;
priv->rts_thresh = val;
orinoco_unlock(priv, &flags);
return 0;
}
static int orinoco_ioctl_setfrag(struct net_device *dev, struct iw_param *frq)
{
struct orinoco_private *priv = dev->priv;
int err = 0;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
if (priv->has_mwo) {
if (frq->disabled)
priv->mwo_robust = 0;
else {
if (frq->fixed)
printk(KERN_WARNING "%s: Fixed fragmentation not \
supported on this firmware. Using MWO robust instead.\n", dev->name);
priv->mwo_robust = 1;
}
} else {
if (frq->disabled)
priv->frag_thresh = 2346;
else {
if ( (frq->value < 256) || (frq->value > 2346) )
err = -EINVAL;
else
priv->frag_thresh = frq->value & ~0x1;
}
}
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_ioctl_getfrag(struct net_device *dev, struct iw_param *frq)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int err = 0;
u16 val;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
if (priv->has_mwo) {
err = hermes_read_wordrec(hw, USER_BAP,
HERMES_RID_CNFMWOROBUST_AGERE,
&val);
if (err)
val = 0;
frq->value = val ? 2347 : 0;
frq->disabled = ! val;
frq->fixed = 0;
} else {
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFFRAGMENTATIONTHRESHOLD,
&val);
if (err)
val = 0;
frq->value = val;
frq->disabled = (val >= 2346);
frq->fixed = 1;
}
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_ioctl_setrate(struct net_device *dev, struct iw_param *rrq)
{
struct orinoco_private *priv = dev->priv;
int err = 0;
int ratemode = -1;
int bitrate;
int i;
unsigned long flags;
if (rrq->value == -1)
bitrate = 110;
else {
if (rrq->value % 100000)
return -EINVAL;
bitrate = rrq->value / 100000;
}
if ( (bitrate != 10) && (bitrate != 20) &&
(bitrate != 55) && (bitrate != 110) )
return -EINVAL;
for (i = 0; i < BITRATE_TABLE_SIZE; i++)
if ( (bitrate_table[i].bitrate == bitrate) &&
(bitrate_table[i].automatic == ! rrq->fixed) ) {
ratemode = i;
break;
}
if (ratemode == -1)
return -EINVAL;
err = orinoco_lock(priv, &flags);
if (err)
return err;
priv->bitratemode = ratemode;
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_ioctl_getrate(struct net_device *dev, struct iw_param *rrq)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int err = 0;
int ratemode;
int i;
u16 val;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
ratemode = priv->bitratemode;
if ( (ratemode < 0) || (ratemode >= BITRATE_TABLE_SIZE) )
BUG();
rrq->value = bitrate_table[ratemode].bitrate * 100000;
rrq->fixed = ! bitrate_table[ratemode].automatic;
rrq->disabled = 0;
if (netif_running(dev)) {
err = hermes_read_wordrec(hw, USER_BAP,
HERMES_RID_CURRENTTXRATE, &val);
if (err)
goto out;
switch (priv->firmware_type) {
case FIRMWARE_TYPE_AGERE:
if (val == 6)
rrq->value = 5500000;
else
rrq->value = val * 1000000;
break;
case FIRMWARE_TYPE_INTERSIL:
case FIRMWARE_TYPE_SYMBOL:
for (i = 0; i < BITRATE_TABLE_SIZE; i++)
if (bitrate_table[i].intersil_txratectrl == val) {
ratemode = i;
break;
}
if (i >= BITRATE_TABLE_SIZE)
printk(KERN_INFO "%s: Unable to determine current bitrate (0x%04hx)\n",
dev->name, val);
rrq->value = bitrate_table[ratemode].bitrate * 100000;
break;
default:
BUG();
}
}
out:
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_ioctl_setpower(struct net_device *dev, struct iw_param *prq)
{
struct orinoco_private *priv = dev->priv;
int err = 0;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
if (prq->disabled) {
priv->pm_on = 0;
} else {
switch (prq->flags & IW_POWER_MODE) {
case IW_POWER_UNICAST_R:
priv->pm_mcast = 0;
priv->pm_on = 1;
break;
case IW_POWER_ALL_R:
priv->pm_mcast = 1;
priv->pm_on = 1;
break;
case IW_POWER_ON:
break;
default:
err = -EINVAL;
}
if (err)
goto out;
if (prq->flags & IW_POWER_TIMEOUT) {
priv->pm_on = 1;
priv->pm_timeout = prq->value / 1000;
}
if (prq->flags & IW_POWER_PERIOD) {
priv->pm_on = 1;
priv->pm_period = prq->value / 1000;
}
if(!priv->pm_on) {
err = -EINVAL;
goto out;
}
}
out:
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_ioctl_getpower(struct net_device *dev, struct iw_param *prq)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int err = 0;
u16 enable, period, timeout, mcast;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFPMENABLED, &enable);
if (err)
goto out;
err = hermes_read_wordrec(hw, USER_BAP,
HERMES_RID_CNFMAXSLEEPDURATION, &period);
if (err)
goto out;
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFPMHOLDOVERDURATION, &timeout);
if (err)
goto out;
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_CNFMULTICASTRECEIVE, &mcast);
if (err)
goto out;
prq->disabled = !enable;
if ((prq->flags & IW_POWER_TYPE) == IW_POWER_TIMEOUT) {
prq->flags = IW_POWER_TIMEOUT;
prq->value = timeout * 1000;
} else {
prq->flags = IW_POWER_PERIOD;
prq->value = period * 1000;
}
if (mcast)
prq->flags |= IW_POWER_ALL_R;
else
prq->flags |= IW_POWER_UNICAST_R;
out:
orinoco_unlock(priv, &flags);
return err;
}
#if WIRELESS_EXT > 10
static int orinoco_ioctl_getretry(struct net_device *dev, struct iw_param *rrq)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
int err = 0;
u16 short_limit, long_limit, lifetime;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_SHORTRETRYLIMIT,
&short_limit);
if (err)
goto out;
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_LONGRETRYLIMIT,
&long_limit);
if (err)
goto out;
err = hermes_read_wordrec(hw, USER_BAP, HERMES_RID_MAXTRANSMITLIFETIME,
&lifetime);
if (err)
goto out;
rrq->disabled = 0;
if ((rrq->flags & IW_RETRY_TYPE) == IW_RETRY_LIFETIME) {
rrq->flags = IW_RETRY_LIFETIME;
rrq->value = lifetime * 1000;
} else {
if ((rrq->flags & IW_RETRY_MAX)) {
rrq->flags = IW_RETRY_LIMIT | IW_RETRY_MAX;
rrq->value = long_limit;
} else {
rrq->flags = IW_RETRY_LIMIT;
rrq->value = short_limit;
if(short_limit != long_limit)
rrq->flags |= IW_RETRY_MIN;
}
}
out:
orinoco_unlock(priv, &flags);
return err;
}
#endif
static int orinoco_ioctl_setibssport(struct net_device *dev, struct iwreq *wrq)
{
struct orinoco_private *priv = dev->priv;
int val = *( (int *) wrq->u.name );
int err;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
priv->ibss_port = val ;
set_port_type(priv);
orinoco_unlock(priv, &flags);
return 0;
}
static int orinoco_ioctl_getibssport(struct net_device *dev, struct iwreq *wrq)
{
struct orinoco_private *priv = dev->priv;
int *val = (int *)wrq->u.name;
int err;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
*val = priv->ibss_port;
orinoco_unlock(priv, &flags);
return 0;
}
static int orinoco_ioctl_setport3(struct net_device *dev, struct iwreq *wrq)
{
struct orinoco_private *priv = dev->priv;
int val = *( (int *) wrq->u.name );
int err = 0;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
switch (val) {
case 0:
if (! priv->has_ibss) {
err = -EINVAL;
break;
}
priv->prefer_port3 = 0;
break;
case 1:
if (! priv->has_port3) {
err = -EINVAL;
break;
}
priv->prefer_port3 = 1;
break;
default:
err = -EINVAL;
}
if (! err)
set_port_type(priv);
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_ioctl_getport3(struct net_device *dev, struct iwreq *wrq)
{
struct orinoco_private *priv = dev->priv;
int *val = (int *)wrq->u.name;
int err;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
*val = priv->prefer_port3;
orinoco_unlock(priv, &flags);
return 0;
}
static int orinoco_ioctl_setspy(struct net_device *dev, struct iw_point *srq)
{
struct orinoco_private *priv = dev->priv;
struct sockaddr address[IW_MAX_SPY];
int number = srq->length;
int i;
int err = 0;
unsigned long flags;
if (number > IW_MAX_SPY)
return -E2BIG;
if (srq->pointer) {
if (copy_from_user(address, srq->pointer,
sizeof(struct sockaddr) * number))
return -EFAULT;
}
err = orinoco_lock(priv, &flags);
if (err)
return err;
priv->spy_number = 0;
if (number > 0) {
for (i = 0; i < number; i++)
memcpy(priv->spy_address[i], address[i].sa_data,
ETH_ALEN);
memset(priv->spy_stat, 0,
sizeof(struct iw_quality) * IW_MAX_SPY);
priv->spy_number = number;
}
orinoco_unlock(priv, &flags);
return err;
}
static int orinoco_ioctl_getspy(struct net_device *dev, struct iw_point *srq)
{
struct orinoco_private *priv = dev->priv;
struct sockaddr address[IW_MAX_SPY];
struct iw_quality spy_stat[IW_MAX_SPY];
int number;
int i;
int err;
unsigned long flags;
err = orinoco_lock(priv, &flags);
if (err)
return err;
number = priv->spy_number;
if ((number > 0) && (srq->pointer)) {
for (i = 0; i < number; i++) {
memcpy(address[i].sa_data, priv->spy_address[i],
ETH_ALEN);
address[i].sa_family = AF_UNIX;
}
memcpy(&spy_stat, priv->spy_stat,
sizeof(struct iw_quality) * IW_MAX_SPY);
for (i=0; i < number; i++)
priv->spy_stat[i].updated = 0;
}
orinoco_unlock(priv, &flags);
srq->length = number;
if(copy_to_user(srq->pointer, address,
sizeof(struct sockaddr) * number))
return -EFAULT;
if(copy_to_user(srq->pointer + (sizeof(struct sockaddr)*number),
&spy_stat, sizeof(struct iw_quality) * number))
return -EFAULT;
return 0;
}
static int
orinoco_ioctl(struct net_device *dev, struct ifreq *rq, int cmd)
{
struct orinoco_private *priv = dev->priv;
struct iwreq *wrq = (struct iwreq *)rq;
int err = 0;
int tmp;
int changed = 0;
unsigned long flags;
TRACE_ENTER(dev->name);
if (! netif_device_present(dev))
return -ENODEV;
switch (cmd) {
case SIOCGIWNAME:
strcpy(wrq->u.name, "IEEE 802.11-DS");
break;
case SIOCGIWAP:
wrq->u.ap_addr.sa_family = ARPHRD_ETHER;
err = orinoco_hw_get_bssid(priv, wrq->u.ap_addr.sa_data);
break;
case SIOCGIWRANGE:
err = orinoco_ioctl_getiwrange(dev, &wrq->u.data);
break;
case SIOCSIWMODE:
err = orinoco_lock(priv, &flags);
if (err)
return err;
switch (wrq->u.mode) {
case IW_MODE_ADHOC:
if (! (priv->has_ibss || priv->has_port3) )
err = -EINVAL;
else {
priv->iw_mode = IW_MODE_ADHOC;
changed = 1;
}
break;
case IW_MODE_INFRA:
priv->iw_mode = IW_MODE_INFRA;
changed = 1;
break;
default:
err = -EINVAL;
break;
}
set_port_type(priv);
orinoco_unlock(priv, &flags);
break;
case SIOCGIWMODE:
err = orinoco_lock(priv, &flags);
if (err)
return err;
wrq->u.mode = priv->iw_mode;
orinoco_unlock(priv, &flags);
break;
case SIOCSIWENCODE:
if (! priv->has_wep) {
err = -EOPNOTSUPP;
break;
}
err = orinoco_ioctl_setiwencode(dev, &wrq->u.encoding);
if (! err)
changed = 1;
break;
case SIOCGIWENCODE:
if (! priv->has_wep) {
err = -EOPNOTSUPP;
break;
}
if (! capable(CAP_NET_ADMIN)) {
err = -EPERM;
break;
}
err = orinoco_ioctl_getiwencode(dev, &wrq->u.encoding);
break;
case SIOCSIWESSID:
err = orinoco_ioctl_setessid(dev, &wrq->u.essid);
if (! err)
changed = 1;
break;
case SIOCGIWESSID:
err = orinoco_ioctl_getessid(dev, &wrq->u.essid);
break;
case SIOCSIWNICKN:
err = orinoco_ioctl_setnick(dev, &wrq->u.data);
if (! err)
changed = 1;
break;
case SIOCGIWNICKN:
err = orinoco_ioctl_getnick(dev, &wrq->u.data);
break;
case SIOCGIWFREQ:
tmp = orinoco_hw_get_freq(priv);
if (tmp < 0) {
err = tmp;
} else {
wrq->u.freq.m = tmp;
wrq->u.freq.e = 1;
}
break;
case SIOCSIWFREQ:
err = orinoco_ioctl_setfreq(dev, &wrq->u.freq);
if (! err)
changed = 1;
break;
case SIOCGIWSENS:
err = orinoco_ioctl_getsens(dev, &wrq->u.sens);
break;
case SIOCSIWSENS:
err = orinoco_ioctl_setsens(dev, &wrq->u.sens);
if (! err)
changed = 1;
break;
case SIOCGIWRTS:
wrq->u.rts.value = priv->rts_thresh;
wrq->u.rts.disabled = (wrq->u.rts.value == 2347);
wrq->u.rts.fixed = 1;
break;
case SIOCSIWRTS:
err = orinoco_ioctl_setrts(dev, &wrq->u.rts);
if (! err)
changed = 1;
break;
case SIOCSIWFRAG:
err = orinoco_ioctl_setfrag(dev, &wrq->u.frag);
if (! err)
changed = 1;
break;
case SIOCGIWFRAG:
err = orinoco_ioctl_getfrag(dev, &wrq->u.frag);
break;
case SIOCSIWRATE:
err = orinoco_ioctl_setrate(dev, &wrq->u.bitrate);
if (! err)
changed = 1;
break;
case SIOCGIWRATE:
err = orinoco_ioctl_getrate(dev, &wrq->u.bitrate);
break;
case SIOCSIWPOWER:
err = orinoco_ioctl_setpower(dev, &wrq->u.power);
if (! err)
changed = 1;
break;
case SIOCGIWPOWER:
err = orinoco_ioctl_getpower(dev, &wrq->u.power);
break;
case SIOCGIWTXPOW:
wrq->u.txpower.value = 15;
wrq->u.txpower.fixed = 1;
wrq->u.txpower.disabled = 0;
wrq->u.txpower.flags = IW_TXPOW_DBM;
break;
#if WIRELESS_EXT > 10
case SIOCSIWRETRY:
err = -EOPNOTSUPP;
break;
case SIOCGIWRETRY:
err = orinoco_ioctl_getretry(dev, &wrq->u.retry);
break;
#endif
case SIOCSIWSPY:
err = orinoco_ioctl_setspy(dev, &wrq->u.data);
break;
case SIOCGIWSPY:
err = orinoco_ioctl_getspy(dev, &wrq->u.data);
break;
case SIOCGIWPRIV:
if (wrq->u.data.pointer) {
struct iw_priv_args privtab[] = {
{ SIOCIWFIRSTPRIV + 0x0, 0, 0, "force_reset" },
{ SIOCIWFIRSTPRIV + 0x1, 0, 0, "card_reset" },
{ SIOCIWFIRSTPRIV + 0x2,
IW_PRIV_TYPE_INT | IW_PRIV_SIZE_FIXED | 1,
0, "set_port3" },
{ SIOCIWFIRSTPRIV + 0x3, 0,
IW_PRIV_TYPE_INT | IW_PRIV_SIZE_FIXED | 1,
"get_port3" },
{ SIOCIWFIRSTPRIV + 0x4,
IW_PRIV_TYPE_INT | IW_PRIV_SIZE_FIXED | 1,
0, "set_preamble" },
{ SIOCIWFIRSTPRIV + 0x5, 0,
IW_PRIV_TYPE_INT | IW_PRIV_SIZE_FIXED | 1,
"get_preamble" },
{ SIOCIWFIRSTPRIV + 0x6,
IW_PRIV_TYPE_INT | IW_PRIV_SIZE_FIXED | 1,
0, "set_ibssport" },
{ SIOCIWFIRSTPRIV + 0x7, 0,
IW_PRIV_TYPE_INT | IW_PRIV_SIZE_FIXED | 1,
"get_ibssport" },
{ SIOCIWLASTPRIV, 0, 0, "dump_recs" },
};
err = verify_area(VERIFY_WRITE, wrq->u.data.pointer, sizeof(privtab));
if (err)
break;
wrq->u.data.length = sizeof(privtab) / sizeof(privtab[0]);
if (copy_to_user(wrq->u.data.pointer, privtab, sizeof(privtab)))
err = -EFAULT;
}
break;
case SIOCIWFIRSTPRIV + 0x0:
case SIOCIWFIRSTPRIV + 0x1:
if (! capable(CAP_NET_ADMIN)) {
err = -EPERM;
break;
}
printk(KERN_DEBUG "%s: Force scheduling reset!\n", dev->name);
schedule_work(&priv->reset_work);
break;
case SIOCIWFIRSTPRIV + 0x2:
if (! capable(CAP_NET_ADMIN)) {
err = -EPERM;
break;
}
err = orinoco_ioctl_setport3(dev, wrq);
if (! err)
changed = 1;
break;
case SIOCIWFIRSTPRIV + 0x3:
err = orinoco_ioctl_getport3(dev, wrq);
break;
case SIOCIWFIRSTPRIV + 0x4:
if (! capable(CAP_NET_ADMIN)) {
err = -EPERM;
break;
}
if(priv->has_preamble) {
int val = *( (int *) wrq->u.name );
err = orinoco_lock(priv, &flags);
if (err)
return err;
if (val)
priv->preamble = 1;
else
priv->preamble = 0;
orinoco_unlock(priv, &flags);
changed = 1;
} else
err = -EOPNOTSUPP;
break;
case SIOCIWFIRSTPRIV + 0x5:
if(priv->has_preamble) {
int *val = (int *)wrq->u.name;
err = orinoco_lock(priv, &flags);
if (err)
return err;
*val = priv->preamble;
orinoco_unlock(priv, &flags);
} else
err = -EOPNOTSUPP;
break;
case SIOCIWFIRSTPRIV + 0x6:
if (! capable(CAP_NET_ADMIN)) {
err = -EPERM;
break;
}
err = orinoco_ioctl_setibssport(dev, wrq);
if (! err)
changed = 1;
break;
case SIOCIWFIRSTPRIV + 0x7:
err = orinoco_ioctl_getibssport(dev, wrq);
break;
case SIOCIWLASTPRIV:
err = orinoco_debug_dump_recs(dev);
if (err)
printk(KERN_ERR "%s: Unable to dump records (%d)\n",
dev->name, err);
break;
default:
err = -EOPNOTSUPP;
}
if (! err && changed && netif_running(dev)) {
err = orinoco_reconfigure(dev);
}
TRACE_EXIT(dev->name);
return err;
}
struct {
u16 rid;
char *name;
int displaytype;
#define DISPLAY_WORDS	0
#define DISPLAY_BYTES	1
#define DISPLAY_STRING	2
#define DISPLAY_XSTRING	3
} record_table[] = {
#define DEBUG_REC(name,type) { HERMES_RID_##name, #name, DISPLAY_##type }
DEBUG_REC(CNFPORTTYPE,WORDS),
DEBUG_REC(CNFOWNMACADDR,BYTES),
DEBUG_REC(CNFDESIREDSSID,STRING),
DEBUG_REC(CNFOWNCHANNEL,WORDS),
DEBUG_REC(CNFOWNSSID,STRING),
DEBUG_REC(CNFOWNATIMWINDOW,WORDS),
DEBUG_REC(CNFSYSTEMSCALE,WORDS),
DEBUG_REC(CNFMAXDATALEN,WORDS),
DEBUG_REC(CNFPMENABLED,WORDS),
DEBUG_REC(CNFPMEPS,WORDS),
DEBUG_REC(CNFMULTICASTRECEIVE,WORDS),
DEBUG_REC(CNFMAXSLEEPDURATION,WORDS),
DEBUG_REC(CNFPMHOLDOVERDURATION,WORDS),
DEBUG_REC(CNFOWNNAME,STRING),
DEBUG_REC(CNFOWNDTIMPERIOD,WORDS),
DEBUG_REC(CNFMULTICASTPMBUFFERING,WORDS),
DEBUG_REC(CNFWEPENABLED_AGERE,WORDS),
DEBUG_REC(CNFMANDATORYBSSID_SYMBOL,WORDS),
DEBUG_REC(CNFWEPDEFAULTKEYID,WORDS),
DEBUG_REC(CNFDEFAULTKEY0,BYTES),
DEBUG_REC(CNFDEFAULTKEY1,BYTES),
DEBUG_REC(CNFMWOROBUST_AGERE,WORDS),
DEBUG_REC(CNFDEFAULTKEY2,BYTES),
DEBUG_REC(CNFDEFAULTKEY3,BYTES),
DEBUG_REC(CNFWEPFLAGS_INTERSIL,WORDS),
DEBUG_REC(CNFWEPKEYMAPPINGTABLE,WORDS),
DEBUG_REC(CNFAUTHENTICATION,WORDS),
DEBUG_REC(CNFMAXASSOCSTA,WORDS),
DEBUG_REC(CNFKEYLENGTH_SYMBOL,WORDS),
DEBUG_REC(CNFTXCONTROL,WORDS),
DEBUG_REC(CNFROAMINGMODE,WORDS),
DEBUG_REC(CNFHOSTAUTHENTICATION,WORDS),
DEBUG_REC(CNFRCVCRCERROR,WORDS),
DEBUG_REC(CNFMMLIFE,WORDS),
DEBUG_REC(CNFALTRETRYCOUNT,WORDS),
DEBUG_REC(CNFBEACONINT,WORDS),
DEBUG_REC(CNFAPPCFINFO,WORDS),
DEBUG_REC(CNFSTAPCFINFO,WORDS),
DEBUG_REC(CNFPRIORITYQUSAGE,WORDS),
DEBUG_REC(CNFTIMCTRL,WORDS),
DEBUG_REC(CNFTHIRTY2TALLY,WORDS),
DEBUG_REC(CNFENHSECURITY,WORDS),
DEBUG_REC(CNFGROUPADDRESSES,BYTES),
DEBUG_REC(CNFCREATEIBSS,WORDS),
DEBUG_REC(CNFFRAGMENTATIONTHRESHOLD,WORDS),
DEBUG_REC(CNFRTSTHRESHOLD,WORDS),
DEBUG_REC(CNFTXRATECONTROL,WORDS),
DEBUG_REC(CNFPROMISCUOUSMODE,WORDS),
DEBUG_REC(CNFBASICRATES_SYMBOL,WORDS),
DEBUG_REC(CNFPREAMBLE_SYMBOL,WORDS),
DEBUG_REC(CNFSHORTPREAMBLE,WORDS),
DEBUG_REC(CNFWEPKEYS_AGERE,BYTES),
DEBUG_REC(CNFEXCLUDELONGPREAMBLE,WORDS),
DEBUG_REC(CNFTXKEY_AGERE,WORDS),
DEBUG_REC(CNFAUTHENTICATIONRSPTO,WORDS),
DEBUG_REC(CNFBASICRATES,WORDS),
DEBUG_REC(CNFSUPPORTEDRATES,WORDS),
DEBUG_REC(CNFTICKTIME,WORDS),
DEBUG_REC(CNFSCANREQUEST,WORDS),
DEBUG_REC(CNFJOINREQUEST,WORDS),
DEBUG_REC(CNFAUTHENTICATESTATION,WORDS),
DEBUG_REC(CNFCHANNELINFOREQUEST,WORDS),
DEBUG_REC(MAXLOADTIME,WORDS),
DEBUG_REC(DOWNLOADBUFFER,WORDS),
DEBUG_REC(PRIID,WORDS),
DEBUG_REC(PRISUPRANGE,WORDS),
DEBUG_REC(CFIACTRANGES,WORDS),
DEBUG_REC(NICSERNUM,XSTRING),
DEBUG_REC(NICID,WORDS),
DEBUG_REC(MFISUPRANGE,WORDS),
DEBUG_REC(CFISUPRANGE,WORDS),
DEBUG_REC(CHANNELLIST,WORDS),
DEBUG_REC(REGULATORYDOMAINS,WORDS),
DEBUG_REC(TEMPTYPE,WORDS),
DEBUG_REC(STAID,WORDS),
DEBUG_REC(CURRENTSSID,STRING),
DEBUG_REC(CURRENTBSSID,BYTES),
DEBUG_REC(COMMSQUALITY,WORDS),
DEBUG_REC(CURRENTTXRATE,WORDS),
DEBUG_REC(CURRENTBEACONINTERVAL,WORDS),
DEBUG_REC(CURRENTSCALETHRESHOLDS,WORDS),
DEBUG_REC(PROTOCOLRSPTIME,WORDS),
DEBUG_REC(SHORTRETRYLIMIT,WORDS),
DEBUG_REC(LONGRETRYLIMIT,WORDS),
DEBUG_REC(MAXTRANSMITLIFETIME,WORDS),
DEBUG_REC(MAXRECEIVELIFETIME,WORDS),
DEBUG_REC(CFPOLLABLE,WORDS),
DEBUG_REC(AUTHENTICATIONALGORITHMS,WORDS),
DEBUG_REC(PRIVACYOPTIONIMPLEMENTED,WORDS),
DEBUG_REC(OWNMACADDR,BYTES),
DEBUG_REC(SCANRESULTSTABLE,WORDS),
DEBUG_REC(PHYTYPE,WORDS),
DEBUG_REC(CURRENTCHANNEL,WORDS),
DEBUG_REC(CURRENTPOWERSTATE,WORDS),
DEBUG_REC(CCAMODE,WORDS),
DEBUG_REC(SUPPORTEDDATARATES,WORDS),
DEBUG_REC(BUILDSEQ,BYTES),
DEBUG_REC(FWID,XSTRING)
#undef DEBUG_REC
};
#define DEBUG_LTV_SIZE		128
static int orinoco_debug_dump_recs(struct net_device *dev)
{
struct orinoco_private *priv = dev->priv;
hermes_t *hw = &priv->hw;
u8 *val8;
u16 *val16;
int i,j;
u16 length;
int err;
val8 = kmalloc(DEBUG_LTV_SIZE + 2, GFP_ATOMIC);
if (! val8)
return -ENOMEM;
val16 = (u16 *)val8;
for (i = 0; i < ARRAY_SIZE(record_table); i++) {
u16 rid = record_table[i].rid;
int len;
memset(val8, 0, DEBUG_LTV_SIZE + 2);
err = hermes_read_ltv(hw, USER_BAP, rid, DEBUG_LTV_SIZE,
&length, val8);
if (err) {
DEBUG(0, "Error %d reading RID 0x%04x\n", err, rid);
continue;
}
val16 = (u16 *)val8;
if (length == 0)
continue;
printk(KERN_DEBUG "%-15s (0x%04x): length=%d (%d bytes)\tvalue=",
record_table[i].name,
rid, length, (length-1)*2);
len = min(((int)length-1)*2, DEBUG_LTV_SIZE);
switch (record_table[i].displaytype) {
case DISPLAY_WORDS:
for (j = 0; j < len / 2; j++)
printk("%04X-", le16_to_cpu(val16[j]));
break;
case DISPLAY_BYTES:
default:
for (j = 0; j < len; j++)
printk("%02X:", val8[j]);
break;
case DISPLAY_STRING:
len = min(len, le16_to_cpu(val16[0])+2);
val8[len] = '\0';
printk("\"%s\"", (char *)&val16[1]);
break;
case DISPLAY_XSTRING:
printk("'%s'", (char *)val8);
}
printk("\n");
}
kfree(val8);
return 0;
}
struct net_device *alloc_orinocodev(int sizeof_card, int (*hard_reset)(struct orinoco_private *))
{
struct net_device *dev;
struct orinoco_private *priv;
dev = alloc_etherdev(sizeof(struct orinoco_private) + sizeof_card);
priv = (struct orinoco_private *)dev->priv;
priv->ndev = dev;
if (sizeof_card)
priv->card = (void *)((unsigned long)dev->priv + sizeof(struct orinoco_private));
else
priv->card = NULL;
dev->init = orinoco_init;
dev->hard_start_xmit = orinoco_xmit;
#ifdef HAVE_TX_TIMEOUT
dev->tx_timeout = orinoco_tx_timeout;
dev->watchdog_timeo = HZ;
#endif
dev->get_stats = orinoco_get_stats;
dev->get_wireless_stats = orinoco_get_wireless_stats;
dev->do_ioctl = orinoco_ioctl;
dev->change_mtu = orinoco_change_mtu;
dev->set_multicast_list = orinoco_set_multicast_list;
dev->open = orinoco_open;
dev->stop = orinoco_stop;
priv->hard_reset = hard_reset;
spin_lock_init(&priv->lock);
priv->open = 0;
priv->hw_unavailable = 1;
INIT_WORK(&priv->reset_work, (void (*)(void *))orinoco_reset, dev);
priv->last_linkstatus = 0xffff;
priv->connected = 0;
return dev;
}
EXPORT_SYMBOL(alloc_orinocodev);
EXPORT_SYMBOL(__orinoco_up);
EXPORT_SYMBOL(__orinoco_down);
EXPORT_SYMBOL(orinoco_stop);
EXPORT_SYMBOL(orinoco_reinit_firmware);
EXPORT_SYMBOL(orinoco_interrupt);
static char version[] __initdata = "orinoco.c 0.13e (David Gibson <hermes@gibson.dropbear.id.au> and others)";
static int __init init_orinoco(void)
{
printk(KERN_DEBUG "%s\n", version);
return 0;
}
static void __exit exit_orinoco(void)
{
}
module_init(init_orinoco);
module_exit(exit_orinoco);