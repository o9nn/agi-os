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
#include <net/udp.h>
#include <net/ip_masq.h>
#pragma pack(1)
typedef struct {
u_short 	dest_family;
u_short 	dest_port;
u_long  	dest_addr;
short 		family;
u_short 	port;
u_long 		addr;
u_long 		seq;
u_short 	msg;
u_short		data_type;
u_short		packet_len;
} cu_header;
typedef struct	{
cu_header	cu_head;
u_short 	client_count;
u_long		seq_no;
char		user_name[20];
char		stuff[4];
}oc_header;
typedef struct {
u_long		address;
char	       	stuff[8];
} client_info;
#pragma pack()
static int ports[MAX_MASQ_APP_PORTS] = {7648};
struct ip_masq_app *masq_incarnations[MAX_MASQ_APP_PORTS];
#ifdef CONFIG_IP_MASQ_DEBUG
static int debug=0;
MODULE_PARM(debug, "i");
#endif
MODULE_PARM(ports, "1-" __MODULE_STRING(MAX_MASQ_APP_PORTS) "i");
static int
masq_cuseeme_init_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_INC_USE_COUNT;
return 0;
}
static int
masq_cuseeme_done_1 (struct ip_masq_app *mapp, struct ip_masq *ms)
{
MOD_DEC_USE_COUNT;
return 0;
}
int
masq_cuseeme_out (struct ip_masq_app *mapp, struct ip_masq *ms, struct sk_buff **skb_p, __u32 maddr)
{
struct sk_buff *skb = *skb_p;
struct iphdr *iph = skb->nh.iph;
struct udphdr *uh = (struct udphdr *)&(((char *)iph)[iph->ihl*4]);
cu_header *cu_head;
char *data=(char *)&uh[1];
if (skb->len - ((unsigned char *) data - skb->h.raw) >= sizeof(cu_header))
{
cu_head         = (cu_header *) data;
if( cu_head->addr )
cu_head->addr = (u_long) maddr;
if(ntohs(cu_head->data_type) == 257)
IP_MASQ_DEBUG(1-debug, "Sending talk packet!\n");
}
return 0;
}
int
masq_cuseeme_in (struct ip_masq_app *mapp, struct ip_masq *ms, struct sk_buff **skb_p, __u32 maddr)
{
struct sk_buff *skb = *skb_p;
struct iphdr *iph = skb->nh.iph;
struct udphdr *uh = (struct udphdr *)&(((char *)iph)[iph->ihl*4]);
cu_header *cu_head;
oc_header	*oc;
client_info	*ci;
char *data=(char *)&uh[1];
u_short len = skb->len - ((unsigned char *) data - skb->h.raw);
int		i, off;
if (len >= sizeof(cu_header))
{
cu_head         = (cu_header *) data;
if(cu_head->dest_addr)
cu_head->dest_addr = (u_long) ms->saddr;
if(ntohs(cu_head->data_type)==101 && len > sizeof(oc_header))
{
oc = (oc_header * ) data;
off=sizeof(oc_header);
for(i=0;
(i < oc->client_count && off+sizeof(client_info) <= len);
i++)
{
ci=(client_info *)(data+off);
if(ci->address==(u_long) maddr)
{
ci->address = (u_long) ms->saddr;
break;
}
else
off+=sizeof(client_info);
}
}
}
return 0;
}
struct ip_masq_app ip_masq_cuseeme = {
NULL,
"cuseeme",
0,
0,
masq_cuseeme_init_1,
masq_cuseeme_done_1,
masq_cuseeme_out,
masq_cuseeme_in
};
__initfunc(int ip_masq_cuseeme_init(void))
{
int i, j;
for (i=0; (i<MAX_MASQ_APP_PORTS); i++) {
if (ports[i]) {
if ((masq_incarnations[i] = kmalloc(sizeof(struct ip_masq_app),
GFP_KERNEL)) == NULL)
return -ENOMEM;
memcpy(masq_incarnations[i], &ip_masq_cuseeme, sizeof(struct ip_masq_app));
if ((j = register_ip_masq_app(masq_incarnations[i],
IPPROTO_UDP,
ports[i]))) {
return j;
}
#if DEBUG_CONFIG_IP_MASQ_CUSEEME
IP_MASQ_DEBUG(1-debug, "CuSeeMe: loaded support on port[%d] = %d\n",
i, ports[i]);
#endif
} else {
masq_incarnations[i] = NULL;
}
}
return 0;
}
int ip_masq_cuseeme_done(void)
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
IP_MASQ_DEBUG(1-debug, "CuSeeMe: unloaded support on port[%d] = %d\n", i, ports[i]);
}
}
}
return k;
}
#ifdef MODULE
EXPORT_NO_SYMBOLS;
int init_module(void)
{
if (ip_masq_cuseeme_init() != 0)
return -EIO;
return 0;
}
void cleanup_module(void)
{
if (ip_masq_cuseeme_done() != 0)
IP_MASQ_DEBUG(1-debug, "ip_masq_cuseeme: can't remove module");
}
#endif