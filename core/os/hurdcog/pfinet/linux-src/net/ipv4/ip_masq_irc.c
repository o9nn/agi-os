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
int ports[MAX_MASQ_APP_PORTS] = {6667};
struct ip_masq_app *masq_incarnations[MAX_MASQ_APP_PORTS];
#ifdef CONFIG_IP_MASQ_DEBUG
static int debug=0;
MODULE_PARM(debug, "i");
#endif
MODULE_PARM(ports, "1-" __MODULE_STRING(MAX_MASQ_APP_PORTS) "i");
#define NUM_DCCPROTO 5
struct dccproto
{
char *match;
int matchlen;
};
struct dccproto dccprotos[NUM_DCCPROTO] = {
{ "SEND ", 5 },
{ "CHAT ", 5 },
{ "MOVE ", 5 },
{ "TSEND ", 6 },
{ "SCHAT ", 6 }
};
#define MAXMATCHLEN 6
static int
masq_irc_init_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_INC_USE_COUNT;
return 0;
}
static int
masq_irc_done_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_DEC_USE_COUNT;
return 0;
}
int
masq_irc_out (struct ip_masq_app *mapp, struct ip_masq *ms, struct sk_buff **skb_p, __u32 maddr)
{
struct sk_buff *skb;
struct iphdr *iph;
struct tcphdr *th;
char *data, *data_limit;
__u32 s_addr;
__u16 s_port;
struct ip_masq *n_ms;
char buf[20];
unsigned buf_len;
int diff;
char *dcc_p, *addr_beg_p, *addr_end_p;
skb = *skb_p;
iph = skb->nh.iph;
th = (struct tcphdr *)&(((char *)iph)[iph->ihl*4]);
data = (char *)&th[1];
data_limit = skb->h.raw + skb->len;
while (data < (data_limit - ( 22 + MAXMATCHLEN ) ) )
{
int i;
if (memcmp(data,"\1DCC ",5)) {
data ++;
continue;
}
dcc_p = data;
data += 5;
for(i=0; i<NUM_DCCPROTO; i++)
{
if( memcmp(data, dccprotos[i].match, dccprotos[i].matchlen ) == 0 )
{
data += dccprotos[i].matchlen;
while( *data++ != ' ')
if (data > (data_limit-12)) return 0;
addr_beg_p = data;
s_addr = simple_strtoul(data,&data,10);
if (*data++ !=' ')
continue;
s_port = simple_strtoul(data,&data,10);
addr_end_p = data;
n_ms = ip_masq_new(IPPROTO_TCP,
maddr, 0,
htonl(s_addr),htons(s_port),
0, 0,
IP_MASQ_F_NO_DPORT|IP_MASQ_F_NO_DADDR);
if (n_ms==NULL)
return 0;
buf_len = sprintf(buf,"%lu %u",
ntohl(n_ms->maddr),ntohs(n_ms->mport));
diff = buf_len - (addr_end_p-addr_beg_p);
*addr_beg_p = '\0';
IP_MASQ_DEBUG(1-debug, "masq_irc_out(): '%s' %X:%X detected (diff=%d)\n", dcc_p, s_addr,s_port, diff);
if (diff==0) {
memcpy(addr_beg_p,buf,buf_len);
} else {
*skb_p = ip_masq_skb_replace(skb, GFP_ATOMIC,
addr_beg_p, addr_end_p-addr_beg_p,
buf, buf_len);
}
ip_masq_listen(n_ms);
ip_masq_put(n_ms);
return diff;
}
}
}
return 0;
}
struct ip_masq_app ip_masq_irc = {
NULL,
"irc",
0,
0,
masq_irc_init_1,
masq_irc_done_1,
masq_irc_out,
NULL
};
__initfunc(int ip_masq_irc_init(void))
{
int i, j;
for (i=0; (i<MAX_MASQ_APP_PORTS); i++) {
if (ports[i]) {
if ((masq_incarnations[i] = kmalloc(sizeof(struct ip_masq_app),
GFP_KERNEL)) == NULL)
return -ENOMEM;
memcpy(masq_incarnations[i], &ip_masq_irc, sizeof(struct ip_masq_app));
if ((j = register_ip_masq_app(masq_incarnations[i],
IPPROTO_TCP,
ports[i]))) {
return j;
}
IP_MASQ_DEBUG(1-debug,
"Irc: loaded support on port[%d] = %d\n",
i, ports[i]);
} else {
masq_incarnations[i] = NULL;
}
}
return 0;
}
int ip_masq_irc_done(void)
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
IP_MASQ_DEBUG(1-debug, "Irc: unloaded support on port[%d] = %d\n",
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
if (ip_masq_irc_init() != 0)
return -EIO;
return 0;
}
void cleanup_module(void)
{
if (ip_masq_irc_done() != 0)
printk(KERN_INFO "ip_masq_irc: can't remove module");
}
#endif