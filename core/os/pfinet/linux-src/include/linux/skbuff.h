#ifndef _LINUX_SKBUFF_H
#define _LINUX_SKBUFF_H
#include <linux/config.h>
#include <linux/time.h>
#include <asm/atomic.h>
#include <asm/types.h>
#include <asm/spinlock.h>
#define HAVE_ALLOC_SKB
#define HAVE_ALIGNABLE_SKB
#define SLAB_SKB
#define CHECKSUM_NONE 0
#define CHECKSUM_HW 1
#define CHECKSUM_UNNECESSARY 2
struct sk_buff_head {
struct sk_buff * next;
struct sk_buff * prev;
__u32 qlen;
};
struct sk_buff {
struct sk_buff * next;
struct sk_buff * prev;
struct sk_buff_head * list;
struct sock *sk;
struct timeval stamp;
struct device *dev;
union
{
struct tcphdr *th;
struct udphdr *uh;
struct icmphdr *icmph;
struct igmphdr *igmph;
struct iphdr *ipiph;
struct spxhdr *spxh;
unsigned char *raw;
} h;
union
{
struct iphdr *iph;
struct ipv6hdr *ipv6h;
struct arphdr *arph;
struct ipxhdr *ipxh;
unsigned char *raw;
} nh;
union
{
struct ethhdr *ethernet;
unsigned char *raw;
} mac;
struct dst_entry *dst;
char cb[48];
unsigned int len;
unsigned int csum;
volatile char used;
unsigned char is_clone,
cloned,
pkt_type,
pkt_bridged,
ip_summed;
__u32 priority;
atomic_t users;
unsigned short protocol;
unsigned short security;
unsigned int truesize;
unsigned char *head;
unsigned char *data;
unsigned char *tail;
unsigned char *end;
void (*destructor)(struct sk_buff *);
#ifdef CONFIG_IP_FIREWALL
__u32 fwmark;
#endif
#if defined(CONFIG_SHAPER) || defined(CONFIG_SHAPER_MODULE)
__u32 shapelatency;
__u32 shapeclock;
__u32 shapelen;
__u32 shapestamp;
__u16 shapepend;
#endif
#if defined(CONFIG_HIPPI)
union{
__u32 ifield;
} private;
#endif
};
#ifdef CONFIG_SKB_LARGE
#define SK_WMEM_MAX (256*1024)
#define SK_RMEM_MAX (256*1024)
#else
#define SK_WMEM_MAX 32767
#define SK_RMEM_MAX 32767
#endif
#ifdef __KERNEL__
#include <linux/malloc.h>
#include <asm/system.h>
extern void __kfree_skb(struct sk_buff *skb);
extern struct sk_buff * skb_peek_copy(struct sk_buff_head *list);
extern struct sk_buff * alloc_skb(unsigned int size, int priority);
extern void kfree_skbmem(struct sk_buff *skb);
extern struct sk_buff * skb_clone(struct sk_buff *skb, int priority);
extern struct sk_buff * skb_copy(struct sk_buff *skb, int priority);
extern struct sk_buff * skb_realloc_headroom(struct sk_buff *skb, int newheadroom);
#define dev_kfree_skb(a) kfree_skb(a)
extern void skb_over_panic(struct sk_buff *skb, int len, void *here);
extern void skb_under_panic(struct sk_buff *skb, int len, void *here);
static __inline__ atomic_t *skb_datarefp(struct sk_buff *skb)
{
return (atomic_t *)(skb->end);
}
static __inline__ int skb_queue_empty(struct sk_buff_head *list)
{
return (list->next == (struct sk_buff *) list);
}
static __inline__ void kfree_skb(struct sk_buff *skb)
{
if (atomic_dec_and_test(&skb->users))
__kfree_skb(skb);
}
static __inline__ void kfree_skb_fast(struct sk_buff *skb)
{
if (atomic_dec_and_test(&skb->users))
kfree_skbmem(skb);
}
static __inline__ int skb_cloned(struct sk_buff *skb)
{
return skb->cloned && atomic_read(skb_datarefp(skb)) != 1;
}
static __inline__ int skb_shared(struct sk_buff *skb)
{
return (atomic_read(&skb->users) != 1);
}
static __inline__ struct sk_buff *skb_unshare(struct sk_buff *skb, int pri)
{
struct sk_buff *nskb;
if(!skb_cloned(skb))
return skb;
nskb=skb_copy(skb, pri);
kfree_skb(skb);
return nskb;
}
static __inline__ struct sk_buff *skb_peek(struct sk_buff_head *list_)
{
struct sk_buff *list = ((struct sk_buff *)list_)->next;
if (list == (struct sk_buff *)list_)
list = NULL;
return list;
}
static __inline__ struct sk_buff *skb_peek_tail(struct sk_buff_head *list_)
{
struct sk_buff *list = ((struct sk_buff *)list_)->prev;
if (list == (struct sk_buff *)list_)
list = NULL;
return list;
}
static __inline__ __u32 skb_queue_len(struct sk_buff_head *list_)
{
return(list_->qlen);
}
static __inline__ void skb_queue_head_init(struct sk_buff_head *list)
{
list->prev = (struct sk_buff *)list;
list->next = (struct sk_buff *)list;
list->qlen = 0;
}
static __inline__ void __skb_queue_head(struct sk_buff_head *list, struct sk_buff *newsk)
{
struct sk_buff *prev, *next;
newsk->list = list;
list->qlen++;
prev = (struct sk_buff *)list;
next = prev->next;
newsk->next = next;
newsk->prev = prev;
next->prev = newsk;
prev->next = newsk;
}
extern spinlock_t skb_queue_lock;
static __inline__ void skb_queue_head(struct sk_buff_head *list, struct sk_buff *newsk)
{
unsigned long flags;
spin_lock_irqsave(&skb_queue_lock, flags);
__skb_queue_head(list, newsk);
spin_unlock_irqrestore(&skb_queue_lock, flags);
}
static __inline__ void __skb_queue_tail(struct sk_buff_head *list, struct sk_buff *newsk)
{
struct sk_buff *prev, *next;
newsk->list = list;
list->qlen++;
next = (struct sk_buff *)list;
prev = next->prev;
newsk->next = next;
newsk->prev = prev;
next->prev = newsk;
prev->next = newsk;
}
static __inline__ void skb_queue_tail(struct sk_buff_head *list, struct sk_buff *newsk)
{
unsigned long flags;
spin_lock_irqsave(&skb_queue_lock, flags);
__skb_queue_tail(list, newsk);
spin_unlock_irqrestore(&skb_queue_lock, flags);
}
static __inline__ struct sk_buff *__skb_dequeue(struct sk_buff_head *list)
{
struct sk_buff *next, *prev, *result;
prev = (struct sk_buff *) list;
next = prev->next;
result = NULL;
if (next != prev) {
result = next;
next = next->next;
list->qlen--;
next->prev = prev;
prev->next = next;
result->next = NULL;
result->prev = NULL;
result->list = NULL;
}
return result;
}
static __inline__ struct sk_buff *skb_dequeue(struct sk_buff_head *list)
{
long flags;
struct sk_buff *result;
spin_lock_irqsave(&skb_queue_lock, flags);
result = __skb_dequeue(list);
spin_unlock_irqrestore(&skb_queue_lock, flags);
return result;
}
static __inline__ void __skb_insert(struct sk_buff *newsk,
struct sk_buff * prev, struct sk_buff *next,
struct sk_buff_head * list)
{
newsk->next = next;
newsk->prev = prev;
next->prev = newsk;
prev->next = newsk;
newsk->list = list;
list->qlen++;
}
static __inline__ void skb_insert(struct sk_buff *old, struct sk_buff *newsk)
{
unsigned long flags;
spin_lock_irqsave(&skb_queue_lock, flags);
__skb_insert(newsk, old->prev, old, old->list);
spin_unlock_irqrestore(&skb_queue_lock, flags);
}
static __inline__ void __skb_append(struct sk_buff *old, struct sk_buff *newsk)
{
__skb_insert(newsk, old, old->next, old->list);
}
static __inline__ void skb_append(struct sk_buff *old, struct sk_buff *newsk)
{
unsigned long flags;
spin_lock_irqsave(&skb_queue_lock, flags);
__skb_append(old, newsk);
spin_unlock_irqrestore(&skb_queue_lock, flags);
}
static __inline__ void __skb_unlink(struct sk_buff *skb, struct sk_buff_head *list)
{
struct sk_buff * next, * prev;
list->qlen--;
next = skb->next;
prev = skb->prev;
skb->next = NULL;
skb->prev = NULL;
skb->list = NULL;
next->prev = prev;
prev->next = next;
}
static __inline__ void skb_unlink(struct sk_buff *skb)
{
unsigned long flags;
spin_lock_irqsave(&skb_queue_lock, flags);
if(skb->list)
__skb_unlink(skb, skb->list);
spin_unlock_irqrestore(&skb_queue_lock, flags);
}
static __inline__ struct sk_buff *__skb_dequeue_tail(struct sk_buff_head *list)
{
struct sk_buff *skb = skb_peek_tail(list);
if (skb)
__skb_unlink(skb, list);
return skb;
}
static __inline__ struct sk_buff *skb_dequeue_tail(struct sk_buff_head *list)
{
long flags;
struct sk_buff *result;
spin_lock_irqsave(&skb_queue_lock, flags);
result = __skb_dequeue_tail(list);
spin_unlock_irqrestore(&skb_queue_lock, flags);
return result;
}
static __inline__ unsigned char *__skb_put(struct sk_buff *skb, unsigned int len)
{
unsigned char *tmp=skb->tail;
skb->tail+=len;
skb->len+=len;
return tmp;
}
static __inline__ unsigned char *skb_put(struct sk_buff *skb, unsigned int len)
{
unsigned char *tmp=skb->tail;
skb->tail+=len;
skb->len+=len;
if(skb->tail>skb->end)
{
__label__ here;
skb_over_panic(skb, len, &&here);
here: ;
}
return tmp;
}
static __inline__ unsigned char *__skb_push(struct sk_buff *skb, unsigned int len)
{
skb->data-=len;
skb->len+=len;
return skb->data;
}
static __inline__ unsigned char *skb_push(struct sk_buff *skb, unsigned int len)
{
skb->data-=len;
skb->len+=len;
if(skb->data<skb->head)
{
__label__ here;
skb_under_panic(skb, len, &&here);
here: ;
}
return skb->data;
}
static __inline__ unsigned char *__skb_pull(struct sk_buff *skb, unsigned int len)
{
skb->len-=len;
return skb->data+=len;
}
static __inline__ unsigned char * skb_pull(struct sk_buff *skb, unsigned int len)
{
if (len > skb->len)
return NULL;
return __skb_pull(skb,len);
}
static __inline__ int skb_headroom(struct sk_buff *skb)
{
return skb->data-skb->head;
}
static __inline__ int skb_tailroom(struct sk_buff *skb)
{
return skb->end-skb->tail;
}
#ifndef NET_IP_ALIGN
#define NET_IP_ALIGN 2
#endif
static __inline__ void skb_reserve(struct sk_buff *skb, unsigned int len)
{
skb->data+=len;
skb->tail+=len;
}
static __inline__ void __skb_trim(struct sk_buff *skb, unsigned int len)
{
skb->len = len;
skb->tail = skb->data+len;
}
static __inline__ void skb_trim(struct sk_buff *skb, unsigned int len)
{
if (skb->len > len) {
__skb_trim(skb, len);
}
}
static __inline__ void skb_orphan(struct sk_buff *skb)
{
if (skb->destructor)
skb->destructor(skb);
skb->destructor = NULL;
skb->sk = NULL;
}
static __inline__ void skb_queue_purge(struct sk_buff_head *list)
{
struct sk_buff *skb;
while ((skb=skb_dequeue(list))!=NULL)
kfree_skb(skb);
}
static __inline__ struct sk_buff *dev_alloc_skb(unsigned int length)
{
struct sk_buff *skb;
skb = alloc_skb(length+16, GFP_ATOMIC);
if (skb)
skb_reserve(skb,16);
return skb;
}
static __inline__ struct sk_buff *
skb_cow(struct sk_buff *skb, unsigned int headroom)
{
headroom = (headroom+15)&~15;
if ((unsigned)skb_headroom(skb) < headroom || skb_cloned(skb)) {
struct sk_buff *skb2 = skb_realloc_headroom(skb, headroom);
kfree_skb(skb);
skb = skb2;
}
return skb;
}
extern struct sk_buff * skb_recv_datagram(struct sock *sk,unsigned flags,int noblock, int *err);
extern unsigned int datagram_poll(struct file *file, struct socket *sock, struct poll_table_struct *wait);
extern int skb_copy_datagram(struct sk_buff *from, int offset, char *to,int size);
extern int skb_copy_datagram_iovec(struct sk_buff *from, int offset, struct iovec *to,int size);
extern void skb_free_datagram(struct sock * sk, struct sk_buff *skb);
extern void skb_init(void);
extern void skb_add_mtu(int mtu);
#endif
#endif