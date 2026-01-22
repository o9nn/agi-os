#include <linux/config.h>
#include <asm/uaccess.h>
#include <asm/system.h>
#include <linux/types.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/in.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/icmp.h>
#include <linux/udp.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/route.h>
#include <net/tcp.h>
#include <net/udp.h>
#include <net/sock.h>
#include <net/icmp.h>
#include <net/ip_masq.h>
#include <linux/netlink.h>
#include <linux/init.h>
#include <linux/firewall.h>
#include <linux/ip_fw.h>
#ifdef CONFIG_IP_MASQUERADE
#include <net/ip_masq.h>
#endif
#include <net/checksum.h>
#include <linux/proc_fs.h>
#include <linux/stat.h>
#ifdef CONFIG_IP_FIREWALL_NETLINK
static struct sock *ipfwsk;
#endif
#ifdef __SMP__
#define SLOT_NUMBER() (cpu_number_map[smp_processor_id()]*2 + !in_interrupt())
#else
#define SLOT_NUMBER() (!in_interrupt())
#endif
#define NUM_SLOTS (smp_num_cpus*2)
#define SIZEOF_STRUCT_IP_CHAIN (sizeof(struct ip_chain) \
+ NUM_SLOTS*sizeof(struct ip_reent))
#define SIZEOF_STRUCT_IP_FW_KERNEL (sizeof(struct ip_fwkernel) \
+ NUM_SLOTS*sizeof(struct ip_counters))
#ifdef DEBUG_IP_FIREWALL_LOCKING
static unsigned int fwc_rlocks, fwc_wlocks;
#define FWC_DEBUG_LOCK(d)			\
do {						\
FWC_DONT_HAVE_LOCK(d);			\
d |= (1 << SLOT_NUMBER());		\
} while (0)
#define FWC_DEBUG_UNLOCK(d)			\
do {						\
FWC_HAVE_LOCK(d);			\
d &= ~(1 << SLOT_NUMBER());		\
} while (0)
#define FWC_DONT_HAVE_LOCK(d)					\
do {								\
if ((d) & (1 << SLOT_NUMBER()))				\
printk("%s:%i: Got lock on %i already!\n", 	\
__FILE__, __LINE__, SLOT_NUMBER());	\
} while(0)
#define FWC_HAVE_LOCK(d)				\
do {							\
if (!((d) & (1 << SLOT_NUMBER())))		\
printk("%s:%i:No lock on %i!\n", 		\
__FILE__, __LINE__, SLOT_NUMBER());	\
} while (0)
#else
#define FWC_DEBUG_LOCK(d) do { } while(0)
#define FWC_DEBUG_UNLOCK(d) do { } while(0)
#define FWC_DONT_HAVE_LOCK(d) do { } while(0)
#define FWC_HAVE_LOCK(d) do { } while(0)
#endif
#define FWC_READ_LOCK(l) do { FWC_DEBUG_LOCK(fwc_rlocks); read_lock(l); } while (0)
#define FWC_WRITE_LOCK(l) do { FWC_DEBUG_LOCK(fwc_wlocks); write_lock(l); } while (0)
#define FWC_READ_LOCK_IRQ(l,f) do { FWC_DEBUG_LOCK(fwc_rlocks); read_lock_irqsave(l,f); } while (0)
#define FWC_WRITE_LOCK_IRQ(l,f) do { FWC_DEBUG_LOCK(fwc_wlocks); write_lock_irqsave(l,f); } while (0)
#define FWC_READ_UNLOCK(l) do { FWC_DEBUG_UNLOCK(fwc_rlocks); read_unlock(l); } while (0)
#define FWC_WRITE_UNLOCK(l) do { FWC_DEBUG_UNLOCK(fwc_wlocks); write_unlock(l); } while (0)
#define FWC_READ_UNLOCK_IRQ(l,f) do { FWC_DEBUG_UNLOCK(fwc_rlocks); read_unlock_irqrestore(l,f); } while (0)
#define FWC_WRITE_UNLOCK_IRQ(l,f) do { FWC_DEBUG_UNLOCK(fwc_wlocks); write_unlock_irqrestore(l,f); } while (0)
struct ip_chain;
struct ip_counters
{
__u64 pcnt, bcnt;
};
struct ip_fwkernel
{
struct ip_fw ipfw;
struct ip_fwkernel *next;
struct ip_chain *branch;
int simplebranch;
struct ip_counters counters[0];
};
struct ip_reent
{
struct ip_chain *prevchain;
struct ip_fwkernel *prevrule;
unsigned int count;
struct ip_counters counters;
};
struct ip_chain
{
ip_chainlabel label;
struct ip_chain *next;
struct ip_fwkernel *chain;
__u32 refcount;
int policy;
struct ip_reent reent[0];
};
#ifdef DEBUG_IP_FIREWALL
#define dprintf(format, args...)  printk(format , ## args)
#else
#define dprintf(format, args...)
#endif
#ifdef DEBUG_IP_FIREWALL_USER
#define duprintf(format, args...) printk(format , ## args)
#else
#define duprintf(format, args...)
#endif
rwlock_t ip_fw_lock = RW_LOCK_UNLOCKED;
static struct ip_chain *ip_fw_chains;
#define IP_FW_INPUT_CHAIN ip_fw_chains
#define IP_FW_FORWARD_CHAIN (ip_fw_chains->next)
#define IP_FW_OUTPUT_CHAIN (ip_fw_chains->next->next)
extern inline int port_match(__u16 min, __u16 max, __u16 port,
int frag, int invert)
{
if (frag)
return (min == 0 && max == 0xFFFF);
else return (port >= min && port <= max) ^ invert;
}
static int ip_rule_match(struct ip_fwkernel *f,
const char *ifname,
struct iphdr *ip,
char tcpsyn,
__u16 src_port, __u16 dst_port,
char isfrag)
{
#define FWINV(bool,invflg) ((bool) ^ !!(f->ipfw.fw_invflg & invflg))
if (FWINV((ip->saddr&f->ipfw.fw_smsk.s_addr) != f->ipfw.fw_src.s_addr,
IP_FW_INV_SRCIP)
|| FWINV((ip->daddr&f->ipfw.fw_dmsk.s_addr)!=f->ipfw.fw_dst.s_addr,
IP_FW_INV_DSTIP)) {
dprintf("Source or dest mismatch.\n");
dprintf("SRC: %u. Mask: %u. Target: %u.%s\n", ip->saddr,
f->ipfw.fw_smsk.s_addr, f->ipfw.fw_src.s_addr,
f->ipfw.fw_invflg & IP_FW_INV_SRCIP ? " (INV)" : "");
dprintf("DST: %u. Mask: %u. Target: %u.%s\n", ip->daddr,
f->ipfw.fw_dmsk.s_addr, f->ipfw.fw_dst.s_addr,
f->ipfw.fw_invflg & IP_FW_INV_DSTIP ? " (INV)" : "");
return 0;
}
if (f->ipfw.fw_flg & IP_FW_F_WILDIF) {
if (FWINV(strncmp(ifname, f->ipfw.fw_vianame,
strlen(f->ipfw.fw_vianame)) != 0,
IP_FW_INV_VIA)) {
dprintf("Wildcard interface mismatch.%s\n",
f->ipfw.fw_invflg & IP_FW_INV_VIA ? " (INV)" : "");
return 0;
}
}
else if (FWINV(strcmp(ifname, f->ipfw.fw_vianame) != 0,
IP_FW_INV_VIA)) {
dprintf("Interface name does not match.%s\n",
f->ipfw.fw_invflg & IP_FW_INV_VIA
? " (INV)" : "");
return 0;
}
if (FWINV((f->ipfw.fw_flg&IP_FW_F_FRAG) && !isfrag, IP_FW_INV_FRAG)) {
dprintf("Fragment rule but not fragment.%s\n",
f->ipfw.fw_invflg & IP_FW_INV_FRAG ? " (INV)" : "");
return 0;
}
if (FWINV((f->ipfw.fw_flg&IP_FW_F_TCPSYN) && !tcpsyn, IP_FW_INV_SYN)
|| (isfrag && (f->ipfw.fw_flg&IP_FW_F_TCPSYN))) {
dprintf("Rule requires SYN and packet has no SYN.%s\n",
f->ipfw.fw_invflg & IP_FW_INV_SYN ? " (INV)" : "");
return 0;
}
if (f->ipfw.fw_proto) {
if (FWINV(ip->protocol!=f->ipfw.fw_proto, IP_FW_INV_PROTO)) {
dprintf("Packet protocol %hi does not match %hi.%s\n",
ip->protocol, f->ipfw.fw_proto,
f->ipfw.fw_invflg&IP_FW_INV_PROTO ? " (INV)":"");
return 0;
}
if (!port_match(f->ipfw.fw_spts[0],
f->ipfw.fw_spts[1],
src_port, isfrag,
!!(f->ipfw.fw_invflg&IP_FW_INV_SRCPT))
|| !port_match(f->ipfw.fw_dpts[0],
f->ipfw.fw_dpts[1],
dst_port, isfrag,
!!(f->ipfw.fw_invflg
&IP_FW_INV_DSTPT))) {
dprintf("Port match failed.\n");
return 0;
}
}
dprintf("Match succeeded.\n");
return 1;
}
static const char *branchname(struct ip_chain *branch,int simplebranch)
{
if (branch)
return branch->label;
switch (simplebranch)
{
case FW_BLOCK: return IP_FW_LABEL_BLOCK;
case FW_ACCEPT: return IP_FW_LABEL_ACCEPT;
case FW_REJECT: return IP_FW_LABEL_REJECT;
case FW_REDIRECT: return IP_FW_LABEL_REDIRECT;
case FW_MASQUERADE: return IP_FW_LABEL_MASQUERADE;
case FW_SKIP: return "-";
case FW_SKIP+1: return IP_FW_LABEL_RETURN;
default:
return "UNKNOWN";
}
}
static void dump_packet(const struct iphdr *ip,
const char *ifname,
struct ip_fwkernel *f,
const ip_chainlabel chainlabel,
__u16 src_port,
__u16 dst_port,
unsigned int count,
int syn)
{
__u32 *opt = (__u32 *) (ip + 1);
int opti;
if (f)
{
printk(KERN_INFO "Packet log: %s ",chainlabel);
printk("%s ",branchname(f->branch,f->simplebranch));
if (f->simplebranch==FW_REDIRECT)
printk("%d ",f->ipfw.fw_redirpt);
}
printk("%s PROTO=%d %d.%d.%d.%d:%hu %d.%d.%d.%d:%hu"
" L=%hu S=0x%2.2hX I=%hu F=0x%4.4hX T=%hu",
ifname, ip->protocol,
(ntohl(ip->saddr)>>24)&0xFF,
(ntohl(ip->saddr)>>16)&0xFF,
(ntohl(ip->saddr)>>8)&0xFF,
(ntohl(ip->saddr))&0xFF,
src_port,
(ntohl(ip->daddr)>>24)&0xFF,
(ntohl(ip->daddr)>>16)&0xFF,
(ntohl(ip->daddr)>>8)&0xFF,
(ntohl(ip->daddr))&0xFF,
dst_port,
ntohs(ip->tot_len), ip->tos, ntohs(ip->id),
ntohs(ip->frag_off), ip->ttl);
for (opti = 0; opti < (ip->ihl - sizeof(struct iphdr) / 4); opti++)
printk(" O=0x%8.8X", *opt++);
printk(" %s(#%d)\n", syn ? "SYN " :  "", count);
}
static int check_label(ip_chainlabel label)
{
unsigned int i;
for (i = 0; i < IP_FW_MAX_LABEL_LENGTH + 1; i++)
if (label[i] == '\0') return 1;
return 0;
}
static struct ip_chain *find_label(ip_chainlabel label)
{
struct ip_chain *tmp;
FWC_HAVE_LOCK(fwc_rlocks | fwc_wlocks);
for (tmp = ip_fw_chains; tmp; tmp = tmp->next)
if (strcmp(tmp->label,label) == 0)
break;
return tmp;
}
static int find_special(ip_chainlabel label, int *answer)
{
if (label[0] == '\0') {
*answer = FW_SKIP;
return 1;
} else if (strcmp(label,IP_FW_LABEL_ACCEPT) == 0) {
*answer = FW_ACCEPT;
return 1;
} else if (strcmp(label,IP_FW_LABEL_BLOCK) == 0) {
*answer = FW_BLOCK;
return 1;
} else if (strcmp(label,IP_FW_LABEL_REJECT) == 0) {
*answer = FW_REJECT;
return 1;
#ifdef CONFIG_IP_TRANSPARENT_PROXY
} else if (strcmp(label,IP_FW_LABEL_REDIRECT) == 0) {
extern int sysctl_ip_always_defrag;
static int enabled = 0;
if(!enabled)
{
enabled=1;
sysctl_ip_always_defrag++;
}
*answer = FW_REDIRECT;
return 1;
#endif
#ifdef CONFIG_IP_MASQUERADE
} else if (strcmp(label,IP_FW_LABEL_MASQUERADE) == 0) {
*answer = FW_MASQUERADE;
return 1;
#endif
} else if (strcmp(label, IP_FW_LABEL_RETURN) == 0) {
*answer = FW_SKIP+1;
return 1;
} else {
return 0;
}
}
static void cleanup(struct ip_chain *chain,
const int verbose,
unsigned int slot)
{
struct ip_chain *tmpchain = chain->reent[slot].prevchain;
if (verbose)
printk(KERN_ERR "Chain backtrace: ");
while (tmpchain) {
if (verbose)
printk("%s<-",chain->label);
chain->reent[slot].prevchain = NULL;
chain = tmpchain;
tmpchain = chain->reent[slot].prevchain;
}
if (verbose)
printk("%s\n",chain->label);
}
static inline int
ip_fw_domatch(struct ip_fwkernel *f,
struct iphdr *ip,
const char *rif,
const ip_chainlabel label,
struct sk_buff *skb,
unsigned int slot,
__u16 src_port, __u16 dst_port,
unsigned int count,
int tcpsyn)
{
f->counters[slot].bcnt+=ntohs(ip->tot_len);
f->counters[slot].pcnt++;
if (f->ipfw.fw_flg & IP_FW_F_PRN) {
dump_packet(ip,rif,f,label,src_port,dst_port,count,tcpsyn);
}
ip->tos = (ip->tos & f->ipfw.fw_tosand) ^ f->ipfw.fw_tosxor;
if (f->ipfw.fw_flg & IP_FW_F_MARKABS)
skb->fwmark = f->ipfw.fw_mark;
else
skb->fwmark+=f->ipfw.fw_mark;
#ifdef CONFIG_IP_FIREWALL_NETLINK
if (f->ipfw.fw_flg & IP_FW_F_NETLINK) {
size_t len = min(f->ipfw.fw_outputsize, ntohs(ip->tot_len))
+ sizeof(__u32) + sizeof(skb->fwmark) + IFNAMSIZ;
struct sk_buff *outskb=alloc_skb(len, GFP_ATOMIC);
duprintf("Sending packet out NETLINK (length = %u).\n",
(unsigned int)len);
if (outskb) {
skb_put(outskb, len);
*((__u32 *)outskb->data) = (__u32)len;
*((__u32 *)(outskb->data+sizeof(__u32))) = skb->fwmark;
strcpy(outskb->data+sizeof(__u32)*2, rif);
memcpy(outskb->data+sizeof(__u32)*2+IFNAMSIZ, ip,
len-(sizeof(__u32)*2+IFNAMSIZ));
netlink_broadcast(ipfwsk, outskb, 0, ~0, GFP_KERNEL);
}
else {
if (net_ratelimit())
printk(KERN_WARNING "ip_fw: packet drop due to "
"netlink failure\n");
return 0;
}
}
#endif
return 1;
}
static int
ip_fw_check(struct iphdr *ip,
const char *rif,
__u16 *redirport,
struct ip_chain *chain,
struct sk_buff *skb,
unsigned int slot,
int testing)
{
struct tcphdr		*tcp=(struct tcphdr *)((__u32 *)ip+ip->ihl);
struct udphdr		*udp=(struct udphdr *)((__u32 *)ip+ip->ihl);
struct icmphdr		*icmp=(struct icmphdr *)((__u32 *)ip+ip->ihl);
__u32			src, dst;
__u16			src_port = 0xFFFF, dst_port = 0xFFFF;
char			tcpsyn=0;
__u16			offset;
unsigned char		oldtos;
struct ip_fwkernel	*f;
int			ret = FW_SKIP+2;
unsigned int		count;
offset = ntohs(ip->frag_off) & IP_OFFSET;
if (offset == 1 && ip->protocol == IPPROTO_TCP)	{
if (!testing && net_ratelimit()) {
printk("Suspect TCP fragment.\n");
dump_packet(ip,rif,NULL,NULL,0,0,0,0);
}
return FW_BLOCK;
}
if (offset == 0) {
unsigned int size_req;
switch (ip->protocol) {
case IPPROTO_TCP:
size_req = 16;
break;
case IPPROTO_UDP:
case IPPROTO_ICMP:
size_req = 8;
break;
default:
size_req = 0;
}
offset = (ntohs(ip->tot_len) < (ip->ihl<<2)+size_req);
if (offset && (ntohs(ip->frag_off) & IP_MF)) {
if (!testing && net_ratelimit()) {
printk("Suspect short first fragment.\n");
dump_packet(ip,rif,NULL,NULL,0,0,0,0);
}
return FW_BLOCK;
}
}
src = ip->saddr;
dst = ip->daddr;
oldtos = ip->tos;
dprintf("Packet ");
switch(ip->protocol)
{
case IPPROTO_TCP:
dprintf("TCP ");
if (!offset) {
src_port=ntohs(tcp->source);
dst_port=ntohs(tcp->dest);
if(tcp->syn && !(tcp->ack || tcp->rst))
tcpsyn=1;
}
break;
case IPPROTO_UDP:
dprintf("UDP ");
if (!offset) {
src_port=ntohs(udp->source);
dst_port=ntohs(udp->dest);
}
break;
case IPPROTO_ICMP:
if (!offset) {
src_port=(__u16)icmp->type;
dst_port=(__u16)icmp->code;
}
dprintf("ICMP ");
break;
default:
dprintf("p=%d ",ip->protocol);
break;
}
#ifdef DEBUG_IP_FIREWALL
print_ip(ip->saddr);
if (offset)
dprintf(":fragment (%i) ", ((int)offset)<<2);
else if (ip->protocol==IPPROTO_TCP || ip->protocol==IPPROTO_UDP
|| ip->protocol==IPPROTO_ICMP)
dprintf(":%hu:%hu", src_port, dst_port);
dprintf("\n");
#endif
if (!testing) FWC_READ_LOCK(&ip_fw_lock);
else FWC_HAVE_LOCK(fwc_rlocks);
f = chain->chain;
count = 0;
do {
for (; f; f = f->next) {
count++;
if (ip_rule_match(f,rif,ip,
tcpsyn,src_port,dst_port,offset)) {
if (!testing
&& !ip_fw_domatch(f, ip, rif, chain->label,
skb, slot,
src_port, dst_port,
count, tcpsyn)) {
ret = FW_BLOCK;
goto out;
}
break;
}
}
if (f) {
if (f->branch) {
if (f->branch->reent[slot].prevchain) {
if (!testing) {
printk(KERN_ERR
"IP firewall: "
"Loop detected "
"at `%s'.\n",
f->branch->label);
cleanup(chain, 1, slot);
ret = FW_BLOCK;
} else {
cleanup(chain, 0, slot);
ret = FW_SKIP+1;
}
}
else {
f->branch->reent[slot].prevchain
= chain;
f->branch->reent[slot].count = count;
f->branch->reent[slot].prevrule
= f->next;
chain = f->branch;
f = chain->chain;
count = 0;
}
}
else if (f->simplebranch == FW_SKIP)
f = f->next;
else if (f->simplebranch == FW_SKIP+1) {
goto fall_off_chain;
}
else {
cleanup(chain, 0, slot);
ret = f->simplebranch;
}
}
else {
fall_off_chain:
if (chain->reent[slot].prevchain) {
struct ip_chain *tmp = chain;
f = chain->reent[slot].prevrule;
count = chain->reent[slot].count;
chain = chain->reent[slot].prevchain;
tmp->reent[slot].prevchain = NULL;
}
else {
ret = chain->policy;
if (!testing) {
chain->reent[slot].counters.pcnt++;
chain->reent[slot].counters.bcnt
+= ntohs(ip->tot_len);
}
}
}
} while (ret == FW_SKIP+2);
out:
if (!testing) FWC_READ_UNLOCK(&ip_fw_lock);
if (ip->tos != oldtos
&& ret != FW_REJECT && ret != FW_BLOCK
&& !testing)
ip_send_check(ip);
#ifdef CONFIG_IP_TRANSPARENT_PROXY
if (ret == FW_REDIRECT && redirport) {
if ((*redirport = htons(f->ipfw.fw_redirpt)) == 0) {
*redirport = htons(dst_port);
}
}
#endif
#ifdef DEBUG_ALLOW_ALL
return (testing ? ret : FW_ACCEPT);
#else
return ret;
#endif
}
static int zero_fw_chain(struct ip_chain *chainptr)
{
struct ip_fwkernel *i;
FWC_HAVE_LOCK(fwc_wlocks);
for (i = chainptr->chain; i; i = i->next)
memset(i->counters, 0, sizeof(struct ip_counters)*NUM_SLOTS);
return 0;
}
static int clear_fw_chain(struct ip_chain *chainptr)
{
struct ip_fwkernel *i= chainptr->chain;
FWC_HAVE_LOCK(fwc_wlocks);
chainptr->chain=NULL;
while (i) {
struct ip_fwkernel *tmp = i->next;
if (i->branch)
i->branch->refcount--;
kfree(i);
i = tmp;
}
return 0;
}
static int replace_in_chain(struct ip_chain *chainptr,
struct ip_fwkernel *frwl,
__u32 position)
{
struct ip_fwkernel *f = chainptr->chain;
FWC_HAVE_LOCK(fwc_wlocks);
while (--position && f != NULL) f = f->next;
if (f == NULL)
return EINVAL;
if (f->branch) f->branch->refcount--;
if (frwl->branch) frwl->branch->refcount++;
frwl->next = f->next;
memcpy(f,frwl,sizeof(struct ip_fwkernel));
kfree(frwl);
return 0;
}
static int append_to_chain(struct ip_chain *chainptr, struct ip_fwkernel *rule)
{
struct ip_fwkernel *i;
FWC_HAVE_LOCK(fwc_wlocks);
if (chainptr->chain == NULL) {
chainptr->chain = rule;
if (rule->branch) rule->branch->refcount++;
return 0;
}
for (i = chainptr->chain; i->next; i = i->next);
i->next = rule;
if (rule->branch) rule->branch->refcount++;
return 0;
}
static int insert_in_chain(struct ip_chain *chainptr,
struct ip_fwkernel *frwl,
__u32 position)
{
struct ip_fwkernel *f = chainptr->chain;
FWC_HAVE_LOCK(fwc_wlocks);
if (position == 1) {
frwl->next = chainptr->chain;
if (frwl->branch) frwl->branch->refcount++;
chainptr->chain = frwl;
return 0;
}
position--;
while (--position && f != NULL) f = f->next;
if (f == NULL)
return EINVAL;
if (frwl->branch) frwl->branch->refcount++;
frwl->next = f->next;
f->next = frwl;
return 0;
}
static int del_num_from_chain(struct ip_chain *chainptr, __u32 rulenum)
{
struct ip_fwkernel *i=chainptr->chain,*tmp;
FWC_HAVE_LOCK(fwc_wlocks);
if (!chainptr->chain)
return ENOENT;
if (rulenum == 1) {
tmp = chainptr->chain;
if (chainptr->chain->branch) chainptr->chain->branch->refcount--;
chainptr->chain = chainptr->chain->next;
kfree(tmp);
} else {
rulenum--;
while (--rulenum && i->next ) i = i->next;
if (!i->next)
return ENOENT;
tmp = i->next;
if (i->next->branch)
i->next->branch->refcount--;
i->next = i->next->next;
kfree(tmp);
}
return 0;
}
static int del_rule_from_chain(struct ip_chain *chainptr,
struct ip_fwkernel *frwl)
{
struct ip_fwkernel *ltmp,*ftmp = chainptr->chain ;
int was_found;
FWC_HAVE_LOCK(fwc_wlocks);
for (ltmp=NULL, was_found=0;
!was_found && ftmp != NULL;
ltmp = ftmp,ftmp = ftmp->next) {
if (ftmp->ipfw.fw_src.s_addr!=frwl->ipfw.fw_src.s_addr
|| ftmp->ipfw.fw_dst.s_addr!=frwl->ipfw.fw_dst.s_addr
|| ftmp->ipfw.fw_smsk.s_addr!=frwl->ipfw.fw_smsk.s_addr
|| ftmp->ipfw.fw_dmsk.s_addr!=frwl->ipfw.fw_dmsk.s_addr
#if 0
|| ftmp->ipfw.fw_flg!=frwl->ipfw.fw_flg
#else
|| ((ftmp->ipfw.fw_flg & ~IP_FW_F_MARKABS)
!= (frwl->ipfw.fw_flg & ~IP_FW_F_MARKABS))
#endif
|| ftmp->ipfw.fw_invflg!=frwl->ipfw.fw_invflg
|| ftmp->ipfw.fw_proto!=frwl->ipfw.fw_proto
#if 0
|| ftmp->ipfw.fw_mark!=frwl->ipfw.fw_mark
#endif
|| ftmp->ipfw.fw_redirpt!=frwl->ipfw.fw_redirpt
|| ftmp->ipfw.fw_spts[0]!=frwl->ipfw.fw_spts[0]
|| ftmp->ipfw.fw_spts[1]!=frwl->ipfw.fw_spts[1]
|| ftmp->ipfw.fw_dpts[0]!=frwl->ipfw.fw_dpts[0]
|| ftmp->ipfw.fw_dpts[1]!=frwl->ipfw.fw_dpts[1]
|| ftmp->ipfw.fw_outputsize!=frwl->ipfw.fw_outputsize) {
duprintf("del_rule_from_chain: mismatch:"
"src:%u/%u dst:%u/%u smsk:%u/%u dmsk:%u/%u "
"flg:%hX/%hX invflg:%hX/%hX proto:%u/%u "
"mark:%u/%u "
"ports:%hu-%hu/%hu-%hu %hu-%hu/%hu-%hu "
"outputsize:%hu-%hu\n",
ftmp->ipfw.fw_src.s_addr,
frwl->ipfw.fw_src.s_addr,
ftmp->ipfw.fw_dst.s_addr,
frwl->ipfw.fw_dst.s_addr,
ftmp->ipfw.fw_smsk.s_addr,
frwl->ipfw.fw_smsk.s_addr,
ftmp->ipfw.fw_dmsk.s_addr,
frwl->ipfw.fw_dmsk.s_addr,
ftmp->ipfw.fw_flg,
frwl->ipfw.fw_flg,
ftmp->ipfw.fw_invflg,
frwl->ipfw.fw_invflg,
ftmp->ipfw.fw_proto,
frwl->ipfw.fw_proto,
ftmp->ipfw.fw_mark,
frwl->ipfw.fw_mark,
ftmp->ipfw.fw_spts[0],
frwl->ipfw.fw_spts[0],
ftmp->ipfw.fw_spts[1],
frwl->ipfw.fw_spts[1],
ftmp->ipfw.fw_dpts[0],
frwl->ipfw.fw_dpts[0],
ftmp->ipfw.fw_dpts[1],
frwl->ipfw.fw_dpts[1],
ftmp->ipfw.fw_outputsize,
frwl->ipfw.fw_outputsize);
continue;
}
if (strncmp(ftmp->ipfw.fw_vianame,
frwl->ipfw.fw_vianame,
IFNAMSIZ)) {
duprintf("del_rule_from_chain: if mismatch: %s/%s\n",
ftmp->ipfw.fw_vianame,
frwl->ipfw.fw_vianame);
continue;
}
if (ftmp->branch != frwl->branch) {
duprintf("del_rule_from_chain: branch mismatch: "
"%s/%s\n",
ftmp->branch?ftmp->branch->label:"(null)",
frwl->branch?frwl->branch->label:"(null)");
continue;
}
if (ftmp->branch == NULL
&& ftmp->simplebranch != frwl->simplebranch) {
duprintf("del_rule_from_chain: simplebranch mismatch: "
"%i/%i\n",
ftmp->simplebranch, frwl->simplebranch);
continue;
}
was_found = 1;
if (ftmp->branch)
ftmp->branch->refcount--;
if (ltmp)
ltmp->next = ftmp->next;
else
chainptr->chain = ftmp->next;
kfree(ftmp);
break;
}
if (was_found)
return 0;
else {
duprintf("del_rule_from_chain: no matching rule found\n");
return EINVAL;
}
}
static int del_chain(ip_chainlabel label)
{
struct ip_chain *tmp,*tmp2;
FWC_HAVE_LOCK(fwc_wlocks);
if (strcmp(label, ip_fw_chains->label) == 0)
return EBUSY;
for (tmp = ip_fw_chains; tmp->next; tmp = tmp->next)
if(strcmp(tmp->next->label,label) == 0)
break;
tmp2 = tmp->next;
if (!tmp2)
return ENOENT;
if (tmp2->refcount)
return EBUSY;
if (tmp2->chain)
return ENOTEMPTY;
tmp->next = tmp2->next;
kfree(tmp2);
return 0;
}
static struct ip_chain *ip_init_chain(ip_chainlabel name,
__u32 ref,
int policy)
{
unsigned int i;
struct ip_chain *label
= kmalloc(SIZEOF_STRUCT_IP_CHAIN, GFP_KERNEL);
if (label == NULL)
panic("Can't kmalloc for firewall chains.\n");
strcpy(label->label,name);
label->next = NULL;
label->chain = NULL;
label->refcount = ref;
label->policy = policy;
for (i = 0; i < smp_num_cpus*2; i++) {
label->reent[i].counters.pcnt = label->reent[i].counters.bcnt
= 0;
label->reent[i].prevchain = NULL;
label->reent[i].prevrule = NULL;
}
return label;
}
static int create_chain(ip_chainlabel label)
{
struct ip_chain *tmp;
if (!check_label(label))
return EINVAL;
FWC_HAVE_LOCK(fwc_wlocks);
for (tmp = ip_fw_chains; tmp->next; tmp = tmp->next)
if (strcmp(tmp->label,label) == 0)
return EEXIST;
if (strcmp(tmp->label,label) == 0)
return EEXIST;
tmp->next = ip_init_chain(label, 0, FW_SKIP);
return 0;
}
static int change_policy(struct ip_chain *chainptr, int policy)
{
FWC_HAVE_LOCK(fwc_wlocks);
chainptr->policy = policy;
return 0;
}
static struct ip_fwkernel *convert_ipfw(struct ip_fwuser *fwuser, int *errno)
{
struct ip_fwkernel *fwkern;
if ( (fwuser->ipfw.fw_flg & ~IP_FW_F_MASK) != 0 ) {
duprintf("convert_ipfw: undefined flag bits set (flags=%x)\n",
fwuser->ipfw.fw_flg);
*errno = EINVAL;
return NULL;
}
#ifdef DEBUG_IP_FIREWALL_USER
if ((fwuser->ipfw.fw_flg & IP_FW_F_TCPSYN)
&& ((fwuser->ipfw.fw_invflg & IP_FW_INV_PROTO)
|| fwuser->ipfw.fw_proto != IPPROTO_TCP)) {
duprintf("convert_ipfw: TCP SYN flag set but proto != TCP!\n");
*errno = EINVAL;
return NULL;
}
if (strcmp(fwuser->label, IP_FW_LABEL_REDIRECT) != 0
&& fwuser->ipfw.fw_redirpt != 0) {
duprintf("convert_ipfw: Target not REDIR but redirpt != 0!\n");
*errno = EINVAL;
return NULL;
}
if ((!(fwuser->ipfw.fw_flg & IP_FW_F_FRAG)
&& (fwuser->ipfw.fw_invflg & IP_FW_INV_FRAG))
|| (!(fwuser->ipfw.fw_flg & IP_FW_F_TCPSYN)
&& (fwuser->ipfw.fw_invflg & IP_FW_INV_SYN))) {
duprintf("convert_ipfw: Can't have INV flag if flag unset!\n");
*errno = EINVAL;
return NULL;
}
if (((fwuser->ipfw.fw_invflg & IP_FW_INV_SRCPT)
&& fwuser->ipfw.fw_spts[0] == 0
&& fwuser->ipfw.fw_spts[1] == 0xFFFF)
|| ((fwuser->ipfw.fw_invflg & IP_FW_INV_DSTPT)
&& fwuser->ipfw.fw_dpts[0] == 0
&& fwuser->ipfw.fw_dpts[1] == 0xFFFF)
|| ((fwuser->ipfw.fw_invflg & IP_FW_INV_VIA)
&& (fwuser->ipfw.fw_vianame)[0] == '\0')
|| ((fwuser->ipfw.fw_invflg & IP_FW_INV_SRCIP)
&& fwuser->ipfw.fw_smsk.s_addr == 0)
|| ((fwuser->ipfw.fw_invflg & IP_FW_INV_DSTIP)
&& fwuser->ipfw.fw_dmsk.s_addr == 0)) {
duprintf("convert_ipfw: INV flag makes rule unmatchable!\n");
*errno = EINVAL;
return NULL;
}
if ((fwuser->ipfw.fw_flg & IP_FW_F_FRAG)
&& !(fwuser->ipfw.fw_invflg & IP_FW_INV_FRAG)
&& (fwuser->ipfw.fw_spts[0] != 0
|| fwuser->ipfw.fw_spts[1] != 0xFFFF
|| fwuser->ipfw.fw_dpts[0] != 0
|| fwuser->ipfw.fw_dpts[1] != 0xFFFF
|| (fwuser->ipfw.fw_flg & IP_FW_F_TCPSYN))) {
duprintf("convert_ipfw: Can't test ports or SYN with frag!\n");
*errno = EINVAL;
return NULL;
}
#endif
if ((fwuser->ipfw.fw_spts[0] != 0
|| fwuser->ipfw.fw_spts[1] != 0xFFFF
|| fwuser->ipfw.fw_dpts[0] != 0
|| fwuser->ipfw.fw_dpts[1] != 0xFFFF)
&& ((fwuser->ipfw.fw_invflg & IP_FW_INV_PROTO)
|| (fwuser->ipfw.fw_proto != IPPROTO_TCP
&& fwuser->ipfw.fw_proto != IPPROTO_UDP
&& fwuser->ipfw.fw_proto != IPPROTO_ICMP))) {
duprintf("convert_ipfw: Can only test ports for TCP/UDP/ICMP!\n");
*errno = EINVAL;
return NULL;
}
fwkern = kmalloc(SIZEOF_STRUCT_IP_FW_KERNEL, GFP_KERNEL);
if (!fwkern) {
duprintf("convert_ipfw: kmalloc failed!\n");
*errno = ENOMEM;
return NULL;
}
memcpy(&fwkern->ipfw,&fwuser->ipfw,sizeof(struct ip_fw));
if (!find_special(fwuser->label, &fwkern->simplebranch)) {
fwkern->branch = find_label(fwuser->label);
if (!fwkern->branch) {
duprintf("convert_ipfw: chain doesn't exist `%s'.\n",
fwuser->label);
kfree(fwkern);
*errno = ENOENT;
return NULL;
} else if (fwkern->branch == IP_FW_INPUT_CHAIN
|| fwkern->branch == IP_FW_FORWARD_CHAIN
|| fwkern->branch == IP_FW_OUTPUT_CHAIN) {
duprintf("convert_ipfw: Can't branch to builtin chain `%s'.\n",
fwuser->label);
kfree(fwkern);
*errno = ENOENT;
return NULL;
}
} else
fwkern->branch = NULL;
memset(fwkern->counters, 0, sizeof(struct ip_counters)*NUM_SLOTS);
if ((fwkern->ipfw.fw_vianame)[0] == '\0')
fwkern->ipfw.fw_flg |= IP_FW_F_WILDIF;
fwkern->next = NULL;
return fwkern;
}
int ip_fw_ctl(int cmd, void *m, int len)
{
int ret;
struct ip_chain *chain;
unsigned long flags;
FWC_WRITE_LOCK_IRQ(&ip_fw_lock, flags);
switch (cmd) {
case IP_FW_FLUSH:
if (len != sizeof(ip_chainlabel) || !check_label(m))
ret = EINVAL;
else if ((chain = find_label(m)) == NULL)
ret = ENOENT;
else ret = clear_fw_chain(chain);
break;
case IP_FW_ZERO:
if (len != sizeof(ip_chainlabel) || !check_label(m))
ret = EINVAL;
else if ((chain = find_label(m)) == NULL)
ret = ENOENT;
else ret = zero_fw_chain(chain);
break;
case IP_FW_CHECK: {
struct ip_fwtest *new = m;
struct iphdr *ip;
FWC_WRITE_UNLOCK_IRQ(&ip_fw_lock, flags);
if (len != sizeof(struct ip_fwtest) || !check_label(m))
return EINVAL;
FWC_READ_LOCK(&ip_fw_lock);
if ((chain = find_label(new->fwt_label)) == NULL)
ret = ENOENT;
else {
ip = &(new->fwt_packet.fwp_iph);
if (ip->ihl != sizeof(struct iphdr) / sizeof(int)) {
duprintf("ip_fw_ctl: ip->ihl=%d, want %d\n",
ip->ihl,
sizeof(struct iphdr) / sizeof(int));
ret = EINVAL;
}
else {
ret = ip_fw_check(ip, new->fwt_packet.fwp_vianame,
NULL, chain,
NULL, SLOT_NUMBER(), 1);
switch (ret) {
case FW_ACCEPT:
ret = 0; break;
case FW_REDIRECT:
ret = ECONNABORTED; break;
case FW_MASQUERADE:
ret = ECONNRESET; break;
case FW_REJECT:
ret = ECONNREFUSED; break;
case FW_SKIP+1:
ret = ELOOP; break;
case FW_SKIP:
ret = ENFILE; break;
default:
ret = ETIMEDOUT; break;
}
}
}
FWC_READ_UNLOCK(&ip_fw_lock);
return ret;
}
case IP_FW_MASQ_TIMEOUTS: {
#ifdef CONFIG_IP_MASQUERADE
ret = ip_fw_masq_timeouts(m, len);
#else
ret = EINVAL;
#endif
}
break;
case IP_FW_REPLACE: {
struct ip_fwkernel *ip_fwkern;
struct ip_fwnew *new = m;
if (len != sizeof(struct ip_fwnew)
|| !check_label(new->fwn_label))
ret = EINVAL;
else if ((chain = find_label(new->fwn_label)) == NULL)
ret = ENOENT;
else if ((ip_fwkern = convert_ipfw(&new->fwn_rule, &ret))
!= NULL)
ret = replace_in_chain(chain, ip_fwkern,
new->fwn_rulenum);
}
break;
case IP_FW_APPEND: {
struct ip_fwchange *new = m;
struct ip_fwkernel *ip_fwkern;
if (len != sizeof(struct ip_fwchange)
|| !check_label(new->fwc_label))
ret = EINVAL;
else if ((chain = find_label(new->fwc_label)) == NULL)
ret = ENOENT;
else if ((ip_fwkern = convert_ipfw(&new->fwc_rule, &ret))
!= NULL)
ret = append_to_chain(chain, ip_fwkern);
}
break;
case IP_FW_INSERT: {
struct ip_fwkernel *ip_fwkern;
struct ip_fwnew *new = m;
if (len != sizeof(struct ip_fwnew)
|| !check_label(new->fwn_label))
ret = EINVAL;
else if ((chain = find_label(new->fwn_label)) == NULL)
ret = ENOENT;
else if ((ip_fwkern = convert_ipfw(&new->fwn_rule, &ret))
!= NULL)
ret = insert_in_chain(chain, ip_fwkern,
new->fwn_rulenum);
}
break;
case IP_FW_DELETE: {
struct ip_fwchange *new = m;
struct ip_fwkernel *ip_fwkern;
if (len != sizeof(struct ip_fwchange)
|| !check_label(new->fwc_label))
ret = EINVAL;
else if ((chain = find_label(new->fwc_label)) == NULL)
ret = ENOENT;
else if ((ip_fwkern = convert_ipfw(&new->fwc_rule, &ret))
!= NULL) {
ret = del_rule_from_chain(chain, ip_fwkern);
kfree(ip_fwkern);
}
}
break;
case IP_FW_DELETE_NUM: {
struct ip_fwdelnum *new = m;
if (len != sizeof(struct ip_fwdelnum)
|| !check_label(new->fwd_label))
ret = EINVAL;
else if ((chain = find_label(new->fwd_label)) == NULL)
ret = ENOENT;
else ret = del_num_from_chain(chain, new->fwd_rulenum);
}
break;
case IP_FW_CREATECHAIN: {
if (len != sizeof(ip_chainlabel)) {
duprintf("create_chain: bad size %i\n", len);
ret = EINVAL;
}
else ret = create_chain(m);
}
break;
case IP_FW_DELETECHAIN: {
if (len != sizeof(ip_chainlabel)) {
duprintf("delete_chain: bad size %i\n", len);
ret = EINVAL;
}
else ret = del_chain(m);
}
break;
case IP_FW_POLICY: {
struct ip_fwpolicy *new = m;
if (len != sizeof(struct ip_fwpolicy)
|| !check_label(new->fwp_label))
ret = EINVAL;
else if ((chain = find_label(new->fwp_label)) == NULL)
ret = ENOENT;
else if (chain != IP_FW_INPUT_CHAIN
&& chain != IP_FW_FORWARD_CHAIN
&& chain != IP_FW_OUTPUT_CHAIN) {
duprintf("change_policy: can't change policy on user"
" defined chain.\n");
ret = EINVAL;
}
else {
int pol = FW_SKIP;
find_special(new->fwp_policy, &pol);
switch(pol) {
case FW_MASQUERADE:
if (chain != IP_FW_FORWARD_CHAIN) {
ret = EINVAL;
break;
}
case FW_BLOCK:
case FW_ACCEPT:
case FW_REJECT:
ret = change_policy(chain, pol);
break;
default:
duprintf("change_policy: bad policy `%s'\n",
new->fwp_policy);
ret = EINVAL;
}
}
break;
}
default:
duprintf("ip_fw_ctl:  unknown request %d\n",cmd);
ret = EINVAL;
}
FWC_WRITE_UNLOCK_IRQ(&ip_fw_lock, flags);
return ret;
}
static int dump_rule(char *buffer,
const char *chainlabel,
const struct ip_fwkernel *rule)
{
int len;
unsigned int i;
__u64 packets = 0, bytes = 0;
FWC_HAVE_LOCK(fwc_wlocks);
for (i = 0; i < NUM_SLOTS; i++) {
packets += rule->counters[i].pcnt;
bytes += rule->counters[i].bcnt;
}
len=sprintf(buffer,
"%9s "
"%08X/%08X->%08X/%08X "
"%.16s "
"%X %X "
"%u "
"%-9u %-9u %-9u %-9u "
"%u-%u %u-%u "
"A%02X X%02X "
"%08X "
"%u "
"%u "
"%9s\n",
chainlabel,
ntohl(rule->ipfw.fw_src.s_addr),
ntohl(rule->ipfw.fw_smsk.s_addr),
ntohl(rule->ipfw.fw_dst.s_addr),
ntohl(rule->ipfw.fw_dmsk.s_addr),
(rule->ipfw.fw_vianame)[0] ? rule->ipfw.fw_vianame : "-",
rule->ipfw.fw_flg,
rule->ipfw.fw_invflg,
rule->ipfw.fw_proto,
(__u32)(packets >> 32), (__u32)packets,
(__u32)(bytes >> 32), (__u32)bytes,
rule->ipfw.fw_spts[0], rule->ipfw.fw_spts[1],
rule->ipfw.fw_dpts[0], rule->ipfw.fw_dpts[1],
rule->ipfw.fw_tosand, rule->ipfw.fw_tosxor,
rule->ipfw.fw_redirpt,
rule->ipfw.fw_mark,
rule->ipfw.fw_outputsize,
branchname(rule->branch,rule->simplebranch));
duprintf("dump_rule: %i bytes done.\n", len);
return len;
}
static int ip_chain_procinfo(char *buffer, char **start,
off_t offset, int length, int reset)
{
struct ip_chain *i;
struct ip_fwkernel *j = ip_fw_chains->chain;
unsigned long flags;
int len = 0;
int last_len = 0;
off_t upto = 0;
duprintf("Offset starts at %lu\n", offset);
duprintf("ip_fw_chains is 0x%0lX\n", (unsigned long int)ip_fw_chains);
FWC_WRITE_LOCK_IRQ(&ip_fw_lock, flags);
for (i = ip_fw_chains; i; i = i->next) {
for (j = i->chain; j; j = j->next) {
if (upto == offset) break;
duprintf("Skipping rule in chain `%s'\n",
i->label);
upto++;
}
if (upto == offset) break;
}
for (; i; (void)((i = i->next) && (j = i->chain))) {
duprintf("Dumping chain `%s'\n", i->label);
for (; j; j = j->next, upto++, last_len = len)
{
len += dump_rule(buffer+len, i->label, j);
if (len > length) {
duprintf("Dumped to %i (past %i).  "
"Moving back to %i.\n",
len, length, last_len);
len = last_len;
goto outside;
}
else if (reset)
memset(j->counters, 0,
sizeof(struct ip_counters)*NUM_SLOTS);
}
}
outside:
FWC_WRITE_UNLOCK_IRQ(&ip_fw_lock, flags);
buffer[len] = '\0';
duprintf("ip_chain_procinfo: Length = %i (of %i).  Offset = %li.\n",
len, length, upto);
*start=(char *)((unsigned int)upto-offset);
return len;
}
static int ip_chain_name_procinfo(char *buffer, char **start,
off_t offset, int length, int reset)
{
struct ip_chain *i;
int len = 0,last_len = 0;
off_t pos = 0,begin = 0;
unsigned long flags;
FWC_WRITE_LOCK_IRQ(&ip_fw_lock, flags);
for (i = ip_fw_chains; i; i = i->next)
{
unsigned int j;
__u32 packetsHi = 0, packetsLo = 0, bytesHi = 0, bytesLo = 0;
for (j = 0; j < NUM_SLOTS; j++) {
packetsLo += i->reent[j].counters.pcnt & 0xFFFFFFFF;
packetsHi += ((i->reent[j].counters.pcnt >> 32)
& 0xFFFFFFFF);
bytesLo += i->reent[j].counters.bcnt & 0xFFFFFFFF;
bytesHi += ((i->reent[j].counters.bcnt >> 32)
& 0xFFFFFFFF);
}
len+=sprintf(buffer+len,"%s %s %i %u %u %u %u\n",
i->label,branchname(NULL, i->policy),i->refcount,
packetsHi, packetsLo, bytesHi, bytesLo);
pos=begin+len;
if(pos<offset) {
len=0;
begin=pos;
}
else if(pos>offset+length) {
len = last_len;
break;
}
last_len = len;
}
FWC_WRITE_UNLOCK_IRQ(&ip_fw_lock, flags);
*start = buffer+(offset-begin);
len-=(offset-begin);
if(len>length)
len=length;
return len;
}
int ipfw_input_check(struct firewall_ops *this, int pf, struct device *dev,
void *phdr, void *arg, struct sk_buff **pskb)
{
return ip_fw_check(phdr, dev->name,
arg, IP_FW_INPUT_CHAIN, *pskb, SLOT_NUMBER(), 0);
}
int ipfw_output_check(struct firewall_ops *this, int pf, struct device *dev,
void *phdr, void *arg, struct sk_buff **pskb)
{
if (((struct iphdr *)phdr)->ihl * 4 < sizeof(struct iphdr)
|| (*pskb)->len < sizeof(struct iphdr))
return FW_ACCEPT;
return ip_fw_check(phdr, dev->name,
arg, IP_FW_OUTPUT_CHAIN, *pskb, SLOT_NUMBER(), 0);
}
int ipfw_forward_check(struct firewall_ops *this, int pf, struct device *dev,
void *phdr, void *arg, struct sk_buff **pskb)
{
return ip_fw_check(phdr, dev->name,
arg, IP_FW_FORWARD_CHAIN, *pskb, SLOT_NUMBER(), 0);
}
struct firewall_ops ipfw_ops=
{
NULL,
ipfw_forward_check,
ipfw_input_check,
ipfw_output_check,
PF_INET,
0
};
#ifdef CONFIG_PROC_FS
static struct proc_dir_entry proc_net_ipfwchains_chain = {
PROC_NET_IPFW_CHAINS, sizeof(IP_FW_PROC_CHAINS)-1,
IP_FW_PROC_CHAINS, S_IFREG | S_IRUSR | S_IWUSR, 1, 0, 0,
0, &proc_net_inode_operations, ip_chain_procinfo
};
static struct proc_dir_entry proc_net_ipfwchains_chainnames = {
PROC_NET_IPFW_CHAIN_NAMES, sizeof(IP_FW_PROC_CHAIN_NAMES)-1,
IP_FW_PROC_CHAIN_NAMES, S_IFREG | S_IRUSR | S_IWUSR, 1, 0, 0,
0, &proc_net_inode_operations, ip_chain_name_procinfo
};
#endif
__initfunc(void ip_fw_init(void))
{
#ifdef DEBUG_IP_FIRWALL_LOCKING
fwc_wlocks = fwc_rlocks = 0;
#endif
IP_FW_INPUT_CHAIN = ip_init_chain(IP_FW_LABEL_INPUT, 1, FW_ACCEPT);
IP_FW_FORWARD_CHAIN = ip_init_chain(IP_FW_LABEL_FORWARD, 1, FW_ACCEPT);
IP_FW_OUTPUT_CHAIN = ip_init_chain(IP_FW_LABEL_OUTPUT, 1, FW_ACCEPT);
if(register_firewall(PF_INET,&ipfw_ops)<0)
panic("Unable to register IP firewall.\n");
#ifdef CONFIG_PROC_FS
proc_net_register(&proc_net_ipfwchains_chain);
proc_net_register(&proc_net_ipfwchains_chainnames);
#endif
#ifdef CONFIG_IP_FIREWALL_NETLINK
ipfwsk = netlink_kernel_create(NETLINK_FIREWALL, NULL);
if (ipfwsk == NULL)
panic("ip_fw_init: cannot initialize netlink\n");
#endif
#if defined(DEBUG_IP_FIREWALL) || defined(DEBUG_IP_FIREWALL_USER)
printk("Firewall graphs enabled! Untested kernel coming thru. \n");
#endif
}