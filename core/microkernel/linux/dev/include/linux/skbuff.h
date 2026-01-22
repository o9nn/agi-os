#ifndef _LINUX_SKBUFF_H
#define _LINUX_SKBUFF_H
#include <linux/config.h>
#include <linux/time.h>
#include <asm/atomic.h>
#include <asm/types.h>
#define CONFIG_SKB_CHECK 0
#define HAVE_ALLOC_SKB
#define HAVE_ALIGNABLE_SKB
#define FREE_READ 1
#define FREE_WRITE 0
#define CHECKSUM_NONE 0
#define CHECKSUM_HW 1
#define CHECKSUM_UNNECESSARY 2
struct sk_buff_head
{
struct sk_buff * next;
struct sk_buff * prev;
__u32 qlen;
#if CONFIG_SKB_CHECK
int magic_debug_cookie;
#endif
};
struct sk_buff
{
struct sk_buff * next;
struct sk_buff * prev;
struct sk_buff_head * list;
#if CONFIG_SKB_CHECK
int magic_debug_cookie;
#endif
struct sk_buff *link3;
struct sock *sk;
unsigned long when;
struct timeval stamp;
struct linux_device *dev;
union
{
struct tcphdr *th;
struct ethhdr *eth;
struct iphdr *iph;
struct udphdr *uh;
unsigned char *raw;
void *filp;
} h;
union
{
unsigned char *raw;
struct ethhdr *ethernet;
} mac;
struct iphdr *ip_hdr;
unsigned long len;
unsigned long csum;
__u32 saddr;
__u32 daddr;
__u32 raddr;
__u32 seq;
__u32 end_seq;
__u32 ack_seq;
unsigned char proto_priv[16];
volatile char acked,
used,
free,
arp;
unsigned char tries,
lock,
localroute,
pkt_type,
pkt_bridged,
ip_summed;
#define PACKET_HOST 0
#define PACKET_BROADCAST 1
#define PACKET_MULTICAST 2
#define PACKET_OTHERHOST 3
unsigned short users;
unsigned short protocol;
unsigned int truesize;
atomic_t count;
struct sk_buff *data_skb;
unsigned char *head;
unsigned char *data;
unsigned char *tail;
unsigned char *end;
void (*destructor)(struct sk_buff *);
__u16 redirport;
#ifdef MACH
#ifdef MACH_INCLUDE
ipc_port_t reply;
mach_msg_type_name_t reply_type;
vm_map_copy_t copy;
#else
void *reply;
unsigned reply_type;
void *copy;
#endif
#endif
};
#ifdef CONFIG_SKB_LARGE
#define SK_WMEM_MAX 65535
#define SK_RMEM_MAX 65535
#else
#define SK_WMEM_MAX 32767
#define SK_RMEM_MAX 32767
#endif
#if CONFIG_SKB_CHECK
#define SK_FREED_SKB 0x0DE2C0DE
#define SK_GOOD_SKB 0xDEC0DED1
#define SK_HEAD_SKB 0x12231298
#endif
#ifdef __KERNEL__
#include <linux/malloc.h>
#include <asm/system.h>
#if 0
extern void print_skb(struct sk_buff *);
#endif
extern void kfree_skb(struct sk_buff *skb, int rw);
extern void skb_queue_head_init(struct sk_buff_head *list);
extern void skb_queue_head(struct sk_buff_head *list,struct sk_buff *buf);
extern void skb_queue_tail(struct sk_buff_head *list,struct sk_buff *buf);
extern struct sk_buff * skb_dequeue(struct sk_buff_head *list);
extern void skb_insert(struct sk_buff *old,struct sk_buff *newsk);
extern void skb_append(struct sk_buff *old,struct sk_buff *newsk);
extern void skb_unlink(struct sk_buff *buf);
extern __u32 skb_queue_len(struct sk_buff_head *list);
extern struct sk_buff * skb_peek_copy(struct sk_buff_head *list);
extern struct sk_buff * alloc_skb(unsigned int size, int priority);
extern struct sk_buff * dev_alloc_skb(unsigned int size);
extern void kfree_skbmem(struct sk_buff *skb);
extern struct sk_buff * skb_clone(struct sk_buff *skb, int priority);
extern struct sk_buff * skb_copy(struct sk_buff *skb, int priority);
extern void skb_device_lock(struct sk_buff *skb);
extern void skb_device_unlock(struct sk_buff *skb);
extern void dev_kfree_skb(struct sk_buff *skb, int mode);
extern int skb_device_locked(struct sk_buff *skb);
extern unsigned char * skb_put(struct sk_buff *skb, int len);
extern unsigned char * skb_push(struct sk_buff *skb, int len);
extern unsigned char * skb_pull(struct sk_buff *skb, int len);
extern int skb_headroom(struct sk_buff *skb);
extern int skb_tailroom(struct sk_buff *skb);
extern void skb_reserve(struct sk_buff *skb, int len);
extern void skb_trim(struct sk_buff *skb, int len);
extern __inline__ int skb_queue_empty(struct sk_buff_head *list)
{
return (list->next == (struct sk_buff *) list);
}
extern __inline__ struct sk_buff *skb_peek(struct sk_buff_head *list_)
{
struct sk_buff *list = ((struct sk_buff *)list_)->next;
if (list == (struct sk_buff *)list_)
list = NULL;
return list;
}
extern __inline__ __u32 skb_queue_len(struct sk_buff_head *list_)
{
return(list_->qlen);
}
#if CONFIG_SKB_CHECK
extern int skb_check(struct sk_buff *skb,int,int, char *);
#define IS_SKB(skb) skb_check((skb), 0, __LINE__,__FILE__)
#define IS_SKB_HEAD(skb) skb_check((skb), 1, __LINE__,__FILE__)
#else
#define IS_SKB(skb)
#define IS_SKB_HEAD(skb)
extern __inline__ void skb_queue_head_init(struct sk_buff_head *list)
{
list->prev = (struct sk_buff *)list;
list->next = (struct sk_buff *)list;
list->qlen = 0;
}
extern __inline__ void __skb_queue_head(struct sk_buff_head *list, struct sk_buff *newsk)
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
extern __inline__ void skb_queue_head(struct sk_buff_head *list, struct sk_buff *newsk)
{
unsigned long flags;
save_flags(flags);
cli();
__skb_queue_head(list, newsk);
restore_flags(flags);
}
extern __inline__ void __skb_queue_tail(struct sk_buff_head *list, struct sk_buff *newsk)
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
extern __inline__ void skb_queue_tail(struct sk_buff_head *list, struct sk_buff *newsk)
{
unsigned long flags;
save_flags(flags);
cli();
__skb_queue_tail(list, newsk);
restore_flags(flags);
}
extern __inline__ struct sk_buff *__skb_dequeue(struct sk_buff_head *list)
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
extern __inline__ struct sk_buff *skb_dequeue(struct sk_buff_head *list)
{
long flags;
struct sk_buff *result;
save_flags(flags);
cli();
result = __skb_dequeue(list);
restore_flags(flags);
return result;
}
extern __inline__ void __skb_insert(struct sk_buff *newsk,
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
extern __inline__ void skb_insert(struct sk_buff *old, struct sk_buff *newsk)
{
unsigned long flags;
save_flags(flags);
cli();
__skb_insert(newsk, old->prev, old, old->list);
restore_flags(flags);
}
extern __inline__ void skb_append(struct sk_buff *old, struct sk_buff *newsk)
{
unsigned long flags;
save_flags(flags);
cli();
__skb_insert(newsk, old, old->next, old->list);
restore_flags(flags);
}
extern __inline__ void __skb_unlink(struct sk_buff *skb, struct sk_buff_head *list)
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
extern __inline__ void skb_unlink(struct sk_buff *skb)
{
unsigned long flags;
save_flags(flags);
cli();
if(skb->list)
__skb_unlink(skb, skb->list);
restore_flags(flags);
}
extern __inline__ unsigned char *skb_put(struct sk_buff *skb, int len)
{
unsigned char *tmp=skb->tail;
skb->tail+=len;
skb->len+=len;
if(skb->tail>skb->end)
{
panic("skput:over: %d", len);
}
return tmp;
}
extern __inline__ unsigned char *skb_push(struct sk_buff *skb, int len)
{
skb->data-=len;
skb->len+=len;
if(skb->data<skb->head)
{
panic("skpush:under: %d", len);
}
return skb->data;
}
extern __inline__ unsigned char * skb_pull(struct sk_buff *skb, int len)
{
if(len > skb->len)
return NULL;
skb->data+=len;
skb->len-=len;
return skb->data;
}
extern __inline__ int skb_headroom(struct sk_buff *skb)
{
return skb->data-skb->head;
}
extern __inline__ int skb_tailroom(struct sk_buff *skb)
{
return skb->end-skb->tail;
}
extern __inline__ void skb_reserve(struct sk_buff *skb, int len)
{
skb->data+=len;
skb->tail+=len;
}
extern __inline__ void skb_trim(struct sk_buff *skb, int len)
{
if(skb->len>len)
{
skb->len=len;
skb->tail=skb->data+len;
}
}
#endif
extern struct sk_buff * skb_recv_datagram(struct sock *sk,unsigned flags,int noblock, int *err);
extern int datagram_select(struct sock *sk, int sel_type, select_table *wait);
extern void skb_copy_datagram(struct sk_buff *from, int offset, char *to,int size);
extern void skb_copy_datagram_iovec(struct sk_buff *from, int offset, struct iovec *to,int size);
extern void skb_free_datagram(struct sock * sk, struct sk_buff *skb);
#endif
#endif