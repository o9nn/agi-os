#include <linux/config.h>
#include <linux/module.h>
#include <asm/system.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/skbuff.h>
#include <linux/in.h>
#include <linux/ip.h>
#include <linux/init.h>
#include <net/protocol.h>
#include <net/tcp.h>
#include <net/ip_masq.h>
static int ports[MAX_MASQ_APP_PORTS] = {21};
struct ip_masq_app *masq_incarnations[MAX_MASQ_APP_PORTS];
#ifdef CONFIG_IP_MASQ_DEBUG
static int debug=0;
MODULE_PARM(debug, "i");
#endif
MODULE_PARM(ports, "1-" __MODULE_STRING(MAX_MASQ_APP_PORTS) "i");
static int masq_ftp_pasv;
static int
masq_ftp_init_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_INC_USE_COUNT;
return 0;
}
static int
masq_ftp_done_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_DEC_USE_COUNT;
return 0;
}
int
masq_ftp_out (struct ip_masq_app *mapp, struct ip_masq *ms, struct sk_buff **skb_p, __u32 maddr)
{
struct sk_buff *skb;
struct iphdr *iph;
struct tcphdr *th;
char *p, *data, *data_limit;
unsigned char p1,p2,p3,p4,p5,p6;
__u32 from;
__u16 port;
struct ip_masq *n_ms;
char buf[24];
unsigned buf_len;
int diff;
skb = *skb_p;
iph = skb->nh.iph;
th = (struct tcphdr *)&(((char *)iph)[iph->ihl*4]);
data = (char *)&th[1];
data_limit = skb->h.raw + skb->len - 18;
if (skb->len >= 6 && (memcmp(data, "PASV\r\n", 6) == 0 || memcmp(data, "pasv\r\n", 6) == 0))
ms->app_data = &masq_ftp_pasv;
while (data < data_limit)
{
if (memcmp(data,"PORT ",5) && memcmp(data,"port ",5))
{
data ++;
continue;
}
p = data+5;
p1 = simple_strtoul(data+5,&data,10);
if (*data!=',')
continue;
p2 = simple_strtoul(data+1,&data,10);
if (*data!=',')
continue;
p3 = simple_strtoul(data+1,&data,10);
if (*data!=',')
continue;
p4 = simple_strtoul(data+1,&data,10);
if (*data!=',')
continue;
p5 = simple_strtoul(data+1,&data,10);
if (*data!=',')
continue;
p6 = simple_strtoul(data+1,&data,10);
if (*data!='\r' && *data!='\n')
continue;
from = (p1<<24) | (p2<<16) | (p3<<8) | p4;
port = (p5<<8) | p6;
IP_MASQ_DEBUG(1-debug, "PORT %X:%X detected\n",from,port);
IP_MASQ_DEBUG(1-debug, "protocol %d %lX:%X %X:%X\n", iph->protocol, htonl(from), htons(port), iph->daddr, 0);
n_ms = ip_masq_out_get(iph->protocol,
htonl(from), htons(port),
iph->daddr, 0);
if (!n_ms) {
n_ms = ip_masq_new(IPPROTO_TCP,
maddr, 0,
htonl(from), htons(port),
iph->daddr, 0,
IP_MASQ_F_NO_DPORT);
if (n_ms==NULL)
return 0;
ip_masq_control_add(n_ms, ms);
}
from = ntohl(n_ms->maddr);
port = ntohs(n_ms->mport);
sprintf(buf,"%d,%d,%d,%d,%d,%d",
from>>24&255,from>>16&255,from>>8&255,from&255,
port>>8&255,port&255);
buf_len = strlen(buf);
IP_MASQ_DEBUG(1-debug, "new PORT %X:%X\n",from,port);
diff = buf_len - (data-p);
if (diff==0) {
memcpy(p,buf,buf_len);
} else {
*skb_p = ip_masq_skb_replace(skb, GFP_ATOMIC, p, data-p, buf, buf_len);
}
ip_masq_listen(n_ms);
ip_masq_put(n_ms);
return diff;
}
return 0;
}
int
masq_ftp_in (struct ip_masq_app *mapp, struct ip_masq *ms, struct sk_buff **skb_p, __u32 maddr)
{
struct sk_buff *skb;
struct iphdr *iph;
struct tcphdr *th;
char *data, *data_limit;
unsigned char p1,p2,p3,p4,p5,p6;
__u32 to;
__u16 port;
struct ip_masq *n_ms;
if (ms->app_data != &masq_ftp_pasv)
return 0;
skb = *skb_p;
iph = skb->nh.iph;
th = (struct tcphdr *)&(((char *)iph)[iph->ihl*4]);
data = (char *)&th[1];
data_limit = skb->h.raw + skb->len;
while (data < data_limit && *data != ' ')
++data;
while (data < data_limit && *data == ' ')
++data;
data += 22;
if (data >= data_limit || *data != '(')
return 0;
p1 = simple_strtoul(data+1, &data, 10);
if (data >= data_limit || *data != ',')
return 0;
p2 = simple_strtoul(data+1, &data, 10);
if (data >= data_limit || *data != ',')
return 0;
p3 = simple_strtoul(data+1, &data, 10);
if (data >= data_limit || *data != ',')
return 0;
p4 = simple_strtoul(data+1, &data, 10);
if (data >= data_limit || *data != ',')
return 0;
p5 = simple_strtoul(data+1, &data, 10);
if (data >= data_limit || *data != ',')
return 0;
p6 = simple_strtoul(data+1, &data, 10);
if (data >= data_limit || *data != ')')
return 0;
to = (p1<<24) | (p2<<16) | (p3<<8) | p4;
port = (p5<<8) | p6;
IP_MASQ_DEBUG(1-debug, "PASV response %lX:%X %X:%X detected\n", ntohl(ms->saddr), 0, to, port);
n_ms = ip_masq_out_get(iph->protocol,
ms->saddr, 0,
htonl(to), htons(port));
if (!n_ms) {
n_ms = ip_masq_new(IPPROTO_TCP,
maddr, 0,
ms->saddr, 0,
htonl(to), htons(port),
IP_MASQ_F_NO_SPORT);
if (n_ms==NULL)
return 0;
ip_masq_control_add(n_ms, ms);
}
#if 0
n_ms->timeout = ip_masq_expire->tcp_fin_timeout*3;
#endif
ms->app_data = NULL;
ip_masq_put(n_ms);
return 0;
}
struct ip_masq_app ip_masq_ftp = {
NULL,
"ftp",
0,
0,
masq_ftp_init_1,
masq_ftp_done_1,
masq_ftp_out,
masq_ftp_in,
};
__initfunc(int ip_masq_ftp_init(void))
{
int i, j;
for (i=0; (i<MAX_MASQ_APP_PORTS); i++) {
if (ports[i]) {
if ((masq_incarnations[i] = kmalloc(sizeof(struct ip_masq_app),
GFP_KERNEL)) == NULL)
return -ENOMEM;
memcpy(masq_incarnations[i], &ip_masq_ftp, sizeof(struct ip_masq_app));
if ((j = register_ip_masq_app(masq_incarnations[i],
IPPROTO_TCP,
ports[i]))) {
return j;
}
IP_MASQ_DEBUG(1-debug, "Ftp: loaded support on port[%d] = %d\n",
i, ports[i]);
} else {
masq_incarnations[i] = NULL;
}
}
return 0;
}
int ip_masq_ftp_done(void)
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
IP_MASQ_DEBUG(1-debug, "Ftp: unloaded support on port[%d] = %d\n",
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
if (ip_masq_ftp_init() != 0)
return -EIO;
return 0;
}
void cleanup_module(void)
{
if (ip_masq_ftp_done() != 0)
printk(KERN_INFO "ip_masq_ftp: can't remove module");
}
#endif