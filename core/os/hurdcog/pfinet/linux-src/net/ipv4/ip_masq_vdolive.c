#include <linux/config.h>
#include <linux/module.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <asm/system.h>
#include <linux/skbuff.h>
#include <linux/in.h>
#include <linux/ip.h>
#include <linux/init.h>
#include <net/protocol.h>
#include <net/tcp.h>
#include <net/ip_masq.h>
struct vdolive_priv_data {
unsigned short	origport;
unsigned short	masqport;
unsigned short	state;
};
static int ports[MAX_MASQ_APP_PORTS] = {7000};
struct ip_masq_app *masq_incarnations[MAX_MASQ_APP_PORTS];
#ifdef CONFIG_IP_MASQ_DEBUG
static int debug=0;
MODULE_PARM(debug, "i");
#endif
MODULE_PARM(ports, "1-" __MODULE_STRING(MAX_MASQ_APP_PORTS) "i");
static int
masq_vdolive_init_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_INC_USE_COUNT;
if ((ms->app_data = kmalloc(sizeof(struct vdolive_priv_data),
GFP_ATOMIC)) == NULL)
IP_MASQ_DEBUG(1-debug, "VDOlive: No memory for application data\n");
else
{
struct vdolive_priv_data *priv =
(struct vdolive_priv_data *)ms->app_data;
priv->origport = 0;
priv->masqport = 0;
priv->state = 0;
}
return 0;
}
static int
masq_vdolive_done_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_DEC_USE_COUNT;
if (ms->app_data)
kfree_s(ms->app_data, sizeof(struct vdolive_priv_data));
return 0;
}
int
masq_vdolive_out (struct ip_masq_app *mapp, struct ip_masq *ms, struct sk_buff **skb_p, __u32 maddr)
{
struct sk_buff *skb;
struct iphdr *iph;
struct tcphdr *th;
char *data, *data_limit;
unsigned int tagval;
struct ip_masq *n_ms;
struct vdolive_priv_data *priv =
(struct vdolive_priv_data *)ms->app_data;
if (!priv)
return 0;
if (priv->state == 3)
return 0;
skb = *skb_p;
iph = skb->nh.iph;
th = (struct tcphdr *)&(((char *)iph)[iph->ihl*4]);
data = (char *)&th[1];
data_limit = skb->h.raw + skb->len;
if (data+8 > data_limit) {
IP_MASQ_DEBUG(1-debug, "VDOlive: packet too short for ID %p %p\n", data, data_limit);
return 0;
}
memcpy(&tagval, data+4, 4);
IP_MASQ_DEBUG(1-debug, "VDOlive: packet seen, tag %ld, in initial state %d\n", ntohl(tagval), priv->state);
if ((ntohl(tagval) != 6) && (ntohl(tagval) != 1)) {
IP_MASQ_DEBUG(1-debug, "VDOlive: unrecognised tag %ld, in initial state %d\n", ntohl(tagval), priv->state);
return 0;
}
if ((ntohl(tagval) == 6) && (data+36 > data_limit)) {
IP_MASQ_DEBUG(1-debug, "VDOlive: initial packet too short %p %p\n", data, data_limit);
return 0;
} else if ((ntohl(tagval) == 1) && (data+20 > data_limit)) {
IP_MASQ_DEBUG(1-debug,"VDOlive: secondary packet too short %p %p\n", data, data_limit);
return 0;
}
if (ntohl(tagval) == 6) {
data += 24;
IP_MASQ_DEBUG(1-debug, "VDOlive: initial packet found\n");
} else {
data += 8;
IP_MASQ_DEBUG(1-debug, "VDOlive: secondary packet found\n");
}
if (memcmp(data, "VDO Live", 8) != 0) {
IP_MASQ_DEBUG(1-debug,"VDOlive: did not find tag\n");
return 0;
}
data += 10;
if (!priv->origport) {
memcpy(&priv->origport, data, 2);
IP_MASQ_DEBUG(1-debug, "VDOlive: found port %d\n", ntohs(priv->origport));
n_ms = ip_masq_new(IPPROTO_UDP,
maddr, 0,
ms->saddr, priv->origport,
ms->daddr, 0,
IP_MASQ_F_NO_DPORT);
if (n_ms==NULL) {
ip_masq_put(n_ms);
IP_MASQ_DEBUG(1-debug, "VDOlive: unable to build UDP tunnel for %x:%x\n", ms->saddr, priv->origport);
priv->origport = 0;
return 0;
}
ip_masq_listen(n_ms);
ip_masq_put(ms);
priv->masqport = n_ms->mport;
} else if (memcmp(data, &(priv->origport), 2)) {
IP_MASQ_DEBUG(1-debug, "VDOlive: ports do not match\n");
}
memcpy(data, &(priv->masqport), 2);
IP_MASQ_DEBUG(1-debug, "VDOlive: rewrote port %d to %d, server %08X\n", ntohs(priv->origport), ntohs(priv->masqport), ms->saddr);
priv->state |= (ntohl(tagval) == 6) ? 1 : 2;
return 0;
}
struct ip_masq_app ip_masq_vdolive = {
NULL,
"VDOlive",
0,
0,
masq_vdolive_init_1,
masq_vdolive_done_1,
masq_vdolive_out,
NULL
};
__initfunc(int ip_masq_vdolive_init(void))
{
int i, j;
for (i=0; (i<MAX_MASQ_APP_PORTS); i++) {
if (ports[i]) {
if ((masq_incarnations[i] = kmalloc(sizeof(struct ip_masq_app),
GFP_KERNEL)) == NULL)
return -ENOMEM;
memcpy(masq_incarnations[i], &ip_masq_vdolive, sizeof(struct ip_masq_app));
if ((j = register_ip_masq_app(masq_incarnations[i],
IPPROTO_TCP,
ports[i]))) {
return j;
}
IP_MASQ_DEBUG(1-debug, "RealAudio: loaded support on port[%d] = %d\n", i, ports[i]);
} else {
masq_incarnations[i] = NULL;
}
}
return 0;
}
int ip_masq_vdolive_done(void)
{
int i, j, k;
k=0;
for (i=0; (i<MAX_MASQ_APP_PORTS); i++) {
if (masq_incarnations[i]) {
if ((j = unregister_ip_masq_app(masq_incarnations[i]))) {
k = j;
} else {
kfree(masq_incarnations[i]);
masq_incarnations[i] = NULL;
IP_MASQ_DEBUG(1-debug,"VDOlive: unloaded support on port[%d] = %d\n", i, ports[i]);
}
}
}
return k;
}
#ifdef MODULE
EXPORT_NO_SYMBOLS;
int init_module(void)
{
if (ip_masq_vdolive_init() != 0)
return -EIO;
return 0;
}
void cleanup_module(void)
{
if (ip_masq_vdolive_done() != 0)
IP_MASQ_DEBUG(1-debug, "ip_masq_vdolive: can't remove module");
}
#endif