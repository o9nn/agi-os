#include <linux/config.h>
#include <linux/module.h>
#include <asm/system.h>
#include <linux/types.h>
#include <linux/ctype.h>
#include <linux/kernel.h>
#include <linux/skbuff.h>
#include <linux/in.h>
#include <linux/ip.h>
#include <linux/init.h>
#include <net/protocol.h>
#include <net/tcp.h>
#include <net/ip_masq.h>
#define TOLOWER(c) (((c) >= 'A' && (c) <= 'Z') ? ((c) - 'A' + 'a') : (c))
#define ISDIGIT(c) (((c) >= '0') && ((c) <= '9'))
struct raudio_priv_data {
struct ip_masq *data_conn;
struct ip_masq *error_conn;
short seen_start;
short is_rtsp;
};
int
masq_rtsp_out (struct ip_masq_app *mapp,
struct ip_masq *ms,
struct sk_buff **skb_p,
__u32 maddr);
int ports[MAX_MASQ_APP_PORTS] = {554, 7070, 0};
struct ip_masq_app *masq_incarnations[MAX_MASQ_APP_PORTS];
#ifdef CONFIG_IP_MASQ_DEBUG
static int debug=0;
MODULE_PARM(debug, "i");
#endif
MODULE_PARM(ports, "1-" __MODULE_STRING(MAX_MASQ_APP_PORTS) "i");
static int
masq_raudio_init_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_INC_USE_COUNT;
if ((ms->app_data = kmalloc(sizeof(struct raudio_priv_data),
GFP_ATOMIC)) == NULL)
printk(KERN_INFO "RealAudio: No memory for application data\n");
else
{
struct raudio_priv_data *priv =
(struct raudio_priv_data *)ms->app_data;
priv->seen_start = 0;
priv->data_conn = NULL;
priv->error_conn = NULL;
priv->is_rtsp = 0;
}
return 0;
}
static int
masq_raudio_done_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_DEC_USE_COUNT;
if (ms->app_data)
kfree_s(ms->app_data, sizeof(struct raudio_priv_data));
return 0;
}
int
masq_raudio_out (struct ip_masq_app *mapp, struct ip_masq *ms, struct sk_buff **skb_p, __u32 maddr)
{
struct sk_buff *skb;
struct iphdr *iph;
struct tcphdr *th;
char *p, *data, *data_limit;
struct ip_masq *n_ms;
unsigned short version, msg_id, msg_len, udp_port;
struct raudio_priv_data *priv =
(struct raudio_priv_data *)ms->app_data;
if (priv && priv->seen_start)
return 0;
if(priv && priv->is_rtsp)
return masq_rtsp_out(mapp, ms, skb_p, maddr);
skb = *skb_p;
iph = skb->nh.iph;
th = (struct tcphdr *)&(((char *)iph)[iph->ihl*4]);
data = (char *)&th[1];
data_limit = skb->h.raw + skb->len;
if(memcmp(data, "OPTIONS", 7) == 0 ||
memcmp(data, "DESCRIBE", 8) == 0)
{
IP_MASQ_DEBUG(1-debug, "RealAudio: Detected RTSP connection\n");
if(priv)
priv->is_rtsp = 1;
return masq_rtsp_out(mapp, ms, skb_p, maddr);
}
if (memcmp(data, "PNA", 3)) {
IP_MASQ_DEBUG(1-debug, "RealAudio: not initial protocol packet - ignored\n");
return(0);
}
data += 3;
memcpy(&version, data, 2);
IP_MASQ_DEBUG(1-debug, "RealAudio: initial seen - protocol version %d\n",
ntohs(version));
if (priv)
priv->seen_start = 1;
if (ntohs(version) >= 256)
{
printk(KERN_INFO "RealAudio: version (%d) not supported\n",
ntohs(version));
return 0;
}
data += 2;
while (data+4 < data_limit) {
memcpy(&msg_id, data, 2);
data += 2;
memcpy(&msg_len, data, 2);
data += 2;
if (ntohs(msg_id) == 0) {
IP_MASQ_DEBUG(1-debug, "RealAudio: packet end tag seen\n");
return 0;
}
IP_MASQ_DEBUG(1-debug, "RealAudio: msg %d - %d byte\n",
ntohs(msg_id), ntohs(msg_len));
if (ntohs(msg_id) == 0) {
return 0;
}
p = data;
data += ntohs(msg_len);
if (data > data_limit)
{
printk(KERN_INFO "RealAudio: Packet too short for data\n");
return 0;
}
if ((ntohs(msg_id) == 1) || (ntohs(msg_id) == 7)) {
memcpy(&udp_port, p, 2);
if (udp_port == 0)
continue;
n_ms = ip_masq_new(IPPROTO_UDP,
maddr, 0,
ms->saddr, udp_port,
ms->daddr, 0,
IP_MASQ_F_NO_DPORT);
if (n_ms==NULL)
return 0;
ip_masq_listen(n_ms);
ip_masq_control_add(n_ms, ms);
memcpy(p, &(n_ms->mport), 2);
IP_MASQ_DEBUG(1-debug, "RealAudio: rewrote UDP port %d -> %d in msg %d\n",
ntohs(udp_port), ntohs(n_ms->mport), ntohs(msg_id));
if (priv) {
if (ntohs(msg_id) == 1)
priv->data_conn = n_ms;
else
priv->error_conn = n_ms;
}
ip_masq_put(n_ms);
}
}
return 0;
}
int
masq_rtsp_out (struct ip_masq_app *mapp,
struct ip_masq *ms,
struct sk_buff **skb_p,
__u32 maddr)
{
struct sk_buff *skb;
struct iphdr *iph;
struct tcphdr *th;
char *data, *data_limit;
struct ip_masq *n_ms, *n_ms2;
unsigned short udp_port;
struct raudio_priv_data *priv =
(struct raudio_priv_data *)ms->app_data;
const char* srch = "transport:";
const char* srchpos = srch;
const char* srchend = srch + strlen(srch);
int state = 0;
char firstport[6];
int firstportpos = 0;
char secondport[6];
int secondportpos = 0;
char *portstart = NULL, *portend = NULL;
int diff;
if (priv && priv->seen_start)
return 0;
skb = *skb_p;
iph = skb->nh.iph;
th = (struct tcphdr *)&(((char *)iph)[iph->ihl*4]);
data = (char *)&th[1];
data_limit = skb->h.raw + skb->len;
firstport[0] = 0;
secondport[0] = 0;
while(data < data_limit && state >= 0)
{
switch(state)
{
case 0:
case 1:
if(TOLOWER(*data) == *srchpos)
{
srchpos++;
if(srchpos == srchend)
{
IP_MASQ_DEBUG(1-debug, "Found string %s in message\n",
srch);
state++;
if(state == 1)
{
srch = "client_port";
srchpos = srch;
srchend = srch + strlen(srch);
}
}
}
else
{
srchpos = srch;
}
break;
case 2:
if(*data == '=')
state = 3;
break;
case 3:
if(ISDIGIT(*data))
{
portstart = data;
firstportpos = 0;
firstport[firstportpos++] = *data;
state = 4;
}
break;
case 4:
if(*data == '-')
{
state = 5;
}
else if(*data == ';')
{
portend = data - 1;
firstport[firstportpos] = 0;
state = -1;
}
else if(ISDIGIT(*data))
{
firstport[firstportpos++] = *data;
}
else if(*data != ' ' && *data != '\t')
{
IP_MASQ_DEBUG(1-debug, "Badly formed RTSP Message\n");
return 0;
}
break;
case 5:
if(ISDIGIT(*data))
{
secondportpos = 0;
secondport[secondportpos++] = *data;
state = 6;
}
else if(*data == ';')
{
portend = data - 1;
secondport[secondportpos] = 0;
state = -1;
}
break;
case 6:
if(*data == ';')
{
portend = data - 1;
secondport[secondportpos] = 0;
state = -1;
}
else if(ISDIGIT(*data))
{
secondport[secondportpos++] = *data;
}
else if(*data != ' ' && *data != '\t')
{
IP_MASQ_DEBUG(1-debug, "Badly formed RTSP Message\n");
return 0;
}
break;
}
data++;
}
if(state >= 0)
return 0;
if(firstportpos > 0)
{
char newbuf[12];
char* tmpptr;
udp_port = htons(simple_strtoul(firstport, &tmpptr, 10));
n_ms = ip_masq_new(IPPROTO_UDP,
maddr, 0,
ms->saddr, udp_port,
ms->daddr, 0,
IP_MASQ_F_NO_DPORT);
if (n_ms==NULL)
return 0;
ip_masq_listen(n_ms);
ip_masq_control_add(n_ms, ms);
if(secondportpos > 0)
{
udp_port = htons(simple_strtoul(secondport, &tmpptr, 10));
n_ms2 = ip_masq_new(IPPROTO_UDP,
maddr, 0,
ms->saddr, udp_port,
ms->daddr, 0,
IP_MASQ_F_NO_DPORT);
if (n_ms2==NULL) {
ip_masq_put(n_ms);
return 0;
}
ip_masq_listen(n_ms2);
ip_masq_control_add(n_ms2, ms);
sprintf(newbuf, "%d-%d", ntohs(n_ms->mport),
ntohs(n_ms2->mport));
}
else
{
sprintf(newbuf, "%d", ntohs(n_ms->mport));
n_ms2 = NULL;
}
*skb_p = ip_masq_skb_replace(skb, GFP_ATOMIC,
portstart, portend - portstart + 1,
newbuf, strlen(newbuf));
IP_MASQ_DEBUG(1-debug, "RTSP: rewrote client_port to %s\n", newbuf);
diff = strlen(newbuf) - (portend - portstart);
}
else
{
return 0;
}
if(priv)
{
priv->seen_start = 1;
if(n_ms)
priv->data_conn = n_ms;
if(n_ms2)
priv->error_conn = n_ms2;
}
if (n_ms)
ip_masq_put(n_ms);
if (n_ms2)
ip_masq_put(n_ms2);
return diff;
}
struct ip_masq_app ip_masq_raudio = {
NULL,
"RealAudio",
0,
0,
masq_raudio_init_1,
masq_raudio_done_1,
masq_raudio_out,
NULL
};
__initfunc(int ip_masq_raudio_init(void))
{
int i, j;
for (i=0; (i<MAX_MASQ_APP_PORTS); i++) {
if (ports[i]) {
if ((masq_incarnations[i] = kmalloc(sizeof(struct ip_masq_app),
GFP_KERNEL)) == NULL)
return -ENOMEM;
memcpy(masq_incarnations[i], &ip_masq_raudio, sizeof(struct ip_masq_app));
if ((j = register_ip_masq_app(masq_incarnations[i],
IPPROTO_TCP,
ports[i]))) {
return j;
}
IP_MASQ_DEBUG(1-debug, "RealAudio: loaded support on port[%d] = %d\n",
i, ports[i]);
} else {
masq_incarnations[i] = NULL;
}
}
return 0;
}
int ip_masq_raudio_done(void)
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
IP_MASQ_DEBUG(1-debug, "RealAudio: unloaded support on port[%d] = %d\n",
i, ports[i]);
}
}
}
return k;
}
#ifdef MODULE
EXPORT_NO_SYMBOLS;
int init_module(void)
{
if (ip_masq_raudio_init() != 0)
return -EIO;
return 0;
}
void cleanup_module(void)
{
if (ip_masq_raudio_done() != 0)
printk(KERN_INFO "ip_masq_raudio: can't remove module");
}
#endif