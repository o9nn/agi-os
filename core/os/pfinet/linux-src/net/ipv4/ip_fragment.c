#include <linux/types.h>
#include <linux/mm.h>
#include <linux/sched.h>
#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/icmp.h>
#include <linux/netdevice.h>
#include <net/sock.h>
#include <net/ip.h>
#include <net/icmp.h>
#include <linux/tcp.h>
#include <linux/udp.h>
#include <linux/inet.h>
#include <linux/firewall.h>
#include <linux/ip_fw.h>
int sysctl_ipfrag_high_thresh = 256*1024;
int sysctl_ipfrag_low_thresh = 192*1024;
int sysctl_ipfrag_time = IP_FRAG_TIME;
struct ipfrag {
int		offset;
int		end;
int		len;
struct sk_buff	*skb;
unsigned char	*ptr;
struct ipfrag	*next;
struct ipfrag	*prev;
};
struct ipq {
struct iphdr	*iph;
struct ipq	*next;
struct ipfrag	*fragments;
int		len;
short		ihlen;
struct timer_list timer;
struct ipq	**pprev;
struct device	*dev;
};
#define IPQ_HASHSZ	64
struct ipq *ipq_hash[IPQ_HASHSZ];
#define ipqhashfn(id, saddr, daddr, prot) \
((((id) >> 1) ^ (saddr) ^ (daddr) ^ (prot)) & (IPQ_HASHSZ - 1))
atomic_t ip_frag_mem = ATOMIC_INIT(0);
static __inline__ void frag_kfree_skb(struct sk_buff *skb)
{
atomic_sub(skb->truesize, &ip_frag_mem);
kfree_skb(skb);
}
static __inline__ void frag_kfree_s(void *ptr, int len)
{
atomic_sub(len, &ip_frag_mem);
kfree(ptr);
}
static __inline__ void *frag_kmalloc(int size, int pri)
{
void *vp = kmalloc(size, pri);
if(!vp)
return NULL;
atomic_add(size, &ip_frag_mem);
return vp;
}
static struct ipfrag *ip_frag_create(int offset, int end,
struct sk_buff *skb, unsigned char *ptr)
{
struct ipfrag *fp;
fp = (struct ipfrag *) frag_kmalloc(sizeof(struct ipfrag), GFP_ATOMIC);
if (fp == NULL)
goto out_nomem;
fp->offset = offset;
fp->end = end;
fp->len = end - offset;
fp->skb = skb;
fp->ptr = ptr;
fp->next = fp->prev = NULL;
atomic_add(skb->truesize, &ip_frag_mem);
return(fp);
out_nomem:
NETDEBUG(printk(KERN_ERR "IP: frag_create: no memory left !\n"));
return(NULL);
}
static inline struct ipq *ip_find(struct iphdr *iph, struct dst_entry *dst)
{
__u16 id = iph->id;
__u32 saddr = iph->saddr;
__u32 daddr = iph->daddr;
__u8 protocol = iph->protocol;
unsigned int hash = ipqhashfn(id, saddr, daddr, protocol);
struct ipq *qp;
for(qp = ipq_hash[hash]; qp; qp = qp->next) {
if(qp->iph->id == id		&&
qp->iph->saddr == saddr	&&
qp->iph->daddr == daddr	&&
qp->iph->protocol == protocol) {
del_timer(&qp->timer);
break;
}
}
return qp;
}
static void ip_free(struct ipq *qp)
{
struct ipfrag *fp;
del_timer(&qp->timer);
if(qp->next)
qp->next->pprev = qp->pprev;
*qp->pprev = qp->next;
fp = qp->fragments;
while (fp) {
struct ipfrag *xp = fp->next;
frag_kfree_skb(fp->skb);
frag_kfree_s(fp, sizeof(struct ipfrag));
fp = xp;
}
frag_kfree_s(qp->iph, 64 + 8);
frag_kfree_s(qp, sizeof(struct ipq));
}
static void ip_expire(unsigned long arg)
{
struct ipq *qp = (struct ipq *) arg;
if(!qp->fragments)
{
#ifdef IP_EXPIRE_DEBUG
printk("warning: possible ip-expire attack\n");
#endif
goto out;
}
ip_statistics.IpReasmTimeout++;
ip_statistics.IpReasmFails++;
icmp_send(qp->fragments->skb, ICMP_TIME_EXCEEDED, ICMP_EXC_FRAGTIME, 0);
out:
ip_free(qp);
}
static void ip_evictor(void)
{
int i, progress;
restart:
progress = 0;
for (i = 0; i < IPQ_HASHSZ; i++) {
struct ipq *qp;
if (atomic_read(&ip_frag_mem) <= sysctl_ipfrag_low_thresh)
return;
qp = ipq_hash[i];
if (qp) {
while (qp->next)
qp = qp->next;
ip_free(qp);
progress = 1;
}
}
if (progress)
goto restart;
panic("ip_evictor: memcount");
}
static struct ipq *ip_create(struct sk_buff *skb, struct iphdr *iph)
{
struct ipq *qp;
unsigned int hash;
int ihlen;
qp = (struct ipq *) frag_kmalloc(sizeof(struct ipq), GFP_ATOMIC);
if (qp == NULL)
goto out_nomem;
ihlen = iph->ihl * 4;
qp->iph = (struct iphdr *) frag_kmalloc(64 + 8, GFP_ATOMIC);
if (qp->iph == NULL)
goto out_free;
memcpy(qp->iph, iph, ihlen + 8);
qp->len = 0;
qp->ihlen = ihlen;
qp->fragments = NULL;
qp->dev = skb->dev;
init_timer(&qp->timer);
qp->timer.expires = 0;
qp->timer.data = (unsigned long) qp;
qp->timer.function = ip_expire;
hash = ipqhashfn(iph->id, iph->saddr, iph->daddr, iph->protocol);
if((qp->next = ipq_hash[hash]) != NULL)
qp->next->pprev = &qp->next;
ipq_hash[hash] = qp;
qp->pprev = &ipq_hash[hash];
return qp;
out_free:
frag_kfree_s(qp, sizeof(struct ipq));
out_nomem:
NETDEBUG(printk(KERN_ERR "IP: create: no memory left !\n"));
return(NULL);
}
static int ip_done(struct ipq *qp)
{
struct ipfrag *fp;
int offset;
if (qp->len == 0)
return 0;
fp = qp->fragments;
offset = 0;
while (fp) {
if (fp->offset > offset)
return(0);
offset = fp->end;
fp = fp->next;
}
return 1;
}
static struct sk_buff *ip_glue(struct ipq *qp)
{
struct sk_buff *skb;
struct iphdr *iph;
struct ipfrag *fp;
unsigned char *ptr;
int count, len;
len = qp->ihlen + qp->len;
if(len > 65535)
goto out_oversize;
skb = dev_alloc_skb(len);
if (!skb)
goto out_nomem;
skb->mac.raw = ptr = skb->data;
skb->nh.iph = iph = (struct iphdr *) skb_put(skb, len);
memcpy(ptr, qp->iph, qp->ihlen);
ptr += qp->ihlen;
fp = qp->fragments;
count = qp->ihlen;
while(fp) {
if ((fp->len <= 0) || ((count + fp->len) > skb->len))
goto out_invalid;
memcpy((ptr + fp->offset), fp->ptr, fp->len);
if (count == qp->ihlen) {
skb->dst = dst_clone(fp->skb->dst);
skb->dev = fp->skb->dev;
}
count += fp->len;
fp = fp->next;
}
skb->pkt_type = qp->fragments->skb->pkt_type;
skb->protocol = qp->fragments->skb->protocol;
skb->security = qp->fragments->skb->security;
iph = skb->nh.iph;
iph->frag_off = 0;
iph->tot_len = htons(count);
ip_statistics.IpReasmOKs++;
return skb;
out_invalid:
NETDEBUG(printk(KERN_ERR
"Invalid fragment list: Fragment over size.\n"));
kfree_skb(skb);
goto out_fail;
out_nomem:
NETDEBUG(printk(KERN_ERR
"IP: queue_glue: no memory for gluing queue %p\n",
qp));
goto out_fail;
out_oversize:
if (net_ratelimit())
printk(KERN_INFO
"Oversized IP packet from %d.%d.%d.%d.\n",
NIPQUAD(qp->iph->saddr));
out_fail:
ip_statistics.IpReasmFails++;
return NULL;
}
struct sk_buff *ip_defrag(struct sk_buff *skb)
{
struct iphdr *iph = skb->nh.iph;
struct ipfrag *prev, *next, *tmp, *tfp;
struct ipq *qp;
unsigned char *ptr;
int flags, offset;
int i, ihl, end;
ip_statistics.IpReasmReqds++;
if (atomic_read(&ip_frag_mem) > sysctl_ipfrag_high_thresh)
ip_evictor();
qp = ip_find(iph, skb->dst);
offset = ntohs(iph->frag_off);
flags = offset & ~IP_OFFSET;
offset &= IP_OFFSET;
offset <<= 3;
ihl = iph->ihl * 4;
if (qp) {
if (offset == 0) {
if ((flags & IP_MF) == 0)
goto out_freequeue;
qp->ihlen = ihl;
memcpy(qp->iph, iph, (ihl + 8));
}
} else {
if ((offset == 0) && ((flags & IP_MF) == 0))
goto out_skb;
qp = ip_create(skb, iph);
if (!qp)
goto out_freeskb;
}
if((ntohs(iph->tot_len) + ((int) offset)) > 65535)
goto out_oversize;
end = offset + ntohs(iph->tot_len) - ihl;
if ((flags & IP_MF) == 0)
qp->len = end;
prev = NULL;
for(next = qp->fragments; next != NULL; next = next->next) {
if (next->offset >= offset)
break;
prev = next;
}
ptr = skb->data + ihl;
if ((prev != NULL) && (offset < prev->end)) {
i = prev->end - offset;
offset += i;
ptr += i;
}
for (tmp = next; tmp != NULL; tmp = tfp) {
tfp = tmp->next;
if (tmp->offset >= end)
break;
i = end - next->offset;
tmp->len -= i;
tmp->offset += i;
tmp->ptr += i;
if (tmp->len <= 0) {
if (tmp->prev != NULL)
tmp->prev->next = tmp->next;
else
qp->fragments = tmp->next;
if (tmp->next != NULL)
tmp->next->prev = tmp->prev;
next = tfp;
frag_kfree_skb(tmp->skb);
frag_kfree_s(tmp, sizeof(struct ipfrag));
}
}
tfp = ip_frag_create(offset, end, skb, ptr);
if (!tfp)
goto out_freeskb;
tfp->prev = prev;
tfp->next = next;
if (prev != NULL)
prev->next = tfp;
else
qp->fragments = tfp;
if (next != NULL)
next->prev = tfp;
if (ip_done(qp)) {
skb = ip_glue(qp);
out_freequeue:
ip_free(qp);
out_skb:
return skb;
}
out_timer:
mod_timer(&qp->timer, jiffies + sysctl_ipfrag_time);
out:
return NULL;
out_oversize:
if (net_ratelimit())
printk(KERN_INFO "Oversized packet received from %d.%d.%d.%d\n",
NIPQUAD(iph->saddr));
out_freeskb:
kfree_skb(skb);
ip_statistics.IpReasmFails++;
if (qp)
goto out_timer;
goto out;
}