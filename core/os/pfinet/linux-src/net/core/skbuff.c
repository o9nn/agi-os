#include <linux/config.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/interrupt.h>
#include <linux/in.h>
#include <linux/inet.h>
#include <linux/malloc.h>
#include <linux/netdevice.h>
#include <linux/string.h>
#include <linux/skbuff.h>
#include <linux/slab.h>
#include <linux/init.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/dst.h>
#include <net/tcp.h>
#include <net/udp.h>
#include <net/sock.h>
#include <asm/uaccess.h>
#include <asm/system.h>
spinlock_t skb_queue_lock = SPIN_LOCK_UNLOCKED;
static atomic_t net_skbcount = ATOMIC_INIT(0);
static atomic_t net_allocs = ATOMIC_INIT(0);
static atomic_t net_fails = ATOMIC_INIT(0);
extern atomic_t ip_frag_mem;
static kmem_cache_t *skbuff_head_cache;
void skb_over_panic(struct sk_buff *skb, int sz, void *here)
{
panic("skput:over: %p:%d put:%d dev:%s",
here, skb->len, sz, skb->dev ? skb->dev->name : "<NULL>");
}
void skb_under_panic(struct sk_buff *skb, int sz, void *here)
{
panic("skput:under: %p:%d put:%d dev:%s",
here, skb->len, sz, skb->dev ? skb->dev->name : "<NULL>");
}
void show_net_buffers(void)
{
printk("Networking buffers in use          : %u\n",
atomic_read(&net_skbcount));
printk("Total network buffer allocations   : %u\n",
atomic_read(&net_allocs));
printk("Total failed network buffer allocs : %u\n",
atomic_read(&net_fails));
#ifdef CONFIG_INET
printk("IP fragment buffer size            : %u\n",
atomic_read(&ip_frag_mem));
#endif
}
struct sk_buff *alloc_skb(unsigned int size,int gfp_mask)
{
struct sk_buff *skb;
u8 *data;
if (in_interrupt() && (gfp_mask & __GFP_WAIT)) {
static int count = 0;
if (++count < 5) {
printk(KERN_ERR "alloc_skb called nonatomically "
"from interrupt %p\n", __builtin_return_address(0));
}
gfp_mask &= ~__GFP_WAIT;
}
skb = kmem_cache_alloc(skbuff_head_cache, gfp_mask);
if (skb == NULL)
goto nohead;
size = ((size + 15) & ~15);
data = kmalloc(size + sizeof(atomic_t), gfp_mask);
if (data == NULL)
goto nodata;
atomic_inc(&net_allocs);
skb->truesize = size;
atomic_inc(&net_skbcount);
skb->head = data;
skb->data = data;
skb->tail = data;
skb->end = data + size;
skb->len = 0;
skb->is_clone = 0;
skb->cloned = 0;
atomic_set(&skb->users, 1);
atomic_set(skb_datarefp(skb), 1);
return skb;
nodata:
kmem_cache_free(skbuff_head_cache, skb);
nohead:
atomic_inc(&net_fails);
return NULL;
}
static inline void skb_headerinit(void *p, kmem_cache_t *cache,
unsigned long flags)
{
struct sk_buff *skb = p;
skb->destructor = NULL;
skb->pkt_type = PACKET_HOST;
skb->pkt_bridged = 0;
skb->prev = skb->next = NULL;
skb->list = NULL;
skb->sk = NULL;
skb->stamp.tv_sec=0;
skb->ip_summed = 0;
skb->security = 0;
skb->dst = NULL;
#ifdef CONFIG_IP_FIREWALL
skb->fwmark = 0;
#endif
memset(skb->cb, 0, sizeof(skb->cb));
skb->priority = 0;
}
void kfree_skbmem(struct sk_buff *skb)
{
if (!skb->cloned || atomic_dec_and_test(skb_datarefp(skb)))
kfree(skb->head);
kmem_cache_free(skbuff_head_cache, skb);
atomic_dec(&net_skbcount);
}
void __kfree_skb(struct sk_buff *skb)
{
if (skb->list)
printk(KERN_WARNING "Warning: kfree_skb passed an skb still "
"on a list (from %p).\n", __builtin_return_address(0));
dst_release(skb->dst);
if(skb->destructor)
skb->destructor(skb);
skb_headerinit(skb, NULL, 0);
kfree_skbmem(skb);
}
struct sk_buff *skb_clone(struct sk_buff *skb, int gfp_mask)
{
struct sk_buff *n;
n = kmem_cache_alloc(skbuff_head_cache, gfp_mask);
if (!n)
return NULL;
memcpy(n, skb, sizeof(*n));
atomic_inc(skb_datarefp(skb));
skb->cloned = 1;
atomic_inc(&net_allocs);
atomic_inc(&net_skbcount);
dst_clone(n->dst);
n->cloned = 1;
n->next = n->prev = NULL;
n->list = NULL;
n->sk = NULL;
n->is_clone = 1;
atomic_set(&n->users, 1);
n->destructor = NULL;
return n;
}
struct sk_buff *skb_copy(struct sk_buff *skb, int gfp_mask)
{
struct sk_buff *n;
unsigned long offset;
n=alloc_skb(skb->end - skb->head, gfp_mask);
if(n==NULL)
return NULL;
offset=n->head-skb->head;
skb_reserve(n,skb->data-skb->head);
skb_put(n,skb->len);
memcpy(n->head,skb->head,skb->end-skb->head);
n->csum = skb->csum;
n->list=NULL;
n->sk=NULL;
n->dev=skb->dev;
n->priority=skb->priority;
n->protocol=skb->protocol;
n->dst=dst_clone(skb->dst);
n->h.raw=skb->h.raw+offset;
n->nh.raw=skb->nh.raw+offset;
n->mac.raw=skb->mac.raw+offset;
memcpy(n->cb, skb->cb, sizeof(skb->cb));
n->used=skb->used;
n->is_clone=0;
atomic_set(&n->users, 1);
n->pkt_type=skb->pkt_type;
n->stamp=skb->stamp;
n->destructor = NULL;
n->security=skb->security;
#ifdef CONFIG_IP_FIREWALL
n->fwmark = skb->fwmark;
#endif
return n;
}
struct sk_buff *skb_realloc_headroom(struct sk_buff *skb, int newheadroom)
{
struct sk_buff *n;
unsigned long offset;
int headroom = skb_headroom(skb);
n=alloc_skb(skb->truesize+newheadroom-headroom, GFP_ATOMIC);
if(n==NULL)
return NULL;
skb_reserve(n,newheadroom);
offset=n->data-skb->data;
skb_put(n,skb->len);
memcpy(n->data,skb->data,skb->len);
n->list=NULL;
n->sk=NULL;
n->priority=skb->priority;
n->protocol=skb->protocol;
n->dev=skb->dev;
n->dst=dst_clone(skb->dst);
n->h.raw=skb->h.raw+offset;
n->nh.raw=skb->nh.raw+offset;
n->mac.raw=skb->mac.raw+offset;
memcpy(n->cb, skb->cb, sizeof(skb->cb));
n->used=skb->used;
n->is_clone=0;
atomic_set(&n->users, 1);
n->pkt_type=skb->pkt_type;
n->stamp=skb->stamp;
n->destructor = NULL;
n->security=skb->security;
#ifdef CONFIG_IP_FIREWALL
n->fwmark = skb->fwmark;
#endif
return n;
}
#if 0
void skb_add_mtu(int mtu)
{
mtu = ((mtu + 15) & ~15) + sizeof(atomic_t);
kmem_add_cache_size(mtu);
}
#endif
void __init skb_init(void)
{
skbuff_head_cache = kmem_cache_create("skbuff_head_cache",
sizeof(struct sk_buff),
0,
SLAB_HWCACHE_ALIGN,
skb_headerinit, NULL);
if (!skbuff_head_cache)
panic("cannot create skbuff cache");
}