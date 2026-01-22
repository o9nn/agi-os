#ifndef _WIRELESS_GLUE_H
#define _WIRELESS_GLUE_H
#ifdef CONFIG_PCMCIA
#define PCMCIA_CLIENT
#include "pcmcia_glue.h"
#endif
#ifndef BUG
#define BUG() \
do { printk("kernel BUG at %s:%d!\n", __FILE__, __LINE__); \
*(int *)0=0; } while (0)
#endif
#include <kern/debug.h>
static inline int
schedule_task(struct tq_struct *task)
{
printk(KERN_INFO "schedule_task: not implemented, task=%p\n", task);
Debugger("schedule_task");
return 0;
}
#define min(x,y) ({ \
const typeof(x) _x = (x);       \
const typeof(y) _y = (y);       \
(void) (&_x == &_y);            \
_x < _y ? _x : _y; })
#define max(x,y) ({ \
const typeof(x) _x = (x);       \
const typeof(y) _y = (y);       \
(void) (&_x == &_y);            \
_x > _y ? _x : _y; })
#define min_t(type,x,y) \
({ type __x = (x); type __y = (y); __x < __y ? __x: __y; })
#define max_t(type,x,y) \
({ type __x = (x); type __y = (y); __x > __y ? __x: __y; })
#define DEV_KFREE_SKB(skb)      dev_kfree_skb(skb, FREE_WRITE)
#define le16_to_cpus(x)  do { } while(0)
#undef copy_to_user
#define copy_to_user(a,b,c)    ((memcpy_tofs(a,b,c)), 0)
#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))
#define PREPARE_TQUEUE(_tq, _routine, _data)                    \
do {                                                          \
(_tq)->routine = _routine;                                  \
(_tq)->data = _data;					\
} while (0)
#define INIT_TQUEUE(_tq, _routine, _data)                       \
do {								\
(_tq)->next = 0;						\
(_tq)->sync = 0;						\
PREPARE_TQUEUE((_tq), (_routine), (_data));			\
} while (0)
static inline struct net_device *
alloc_etherdev(int sz)
{
struct net_device *dev;
sz += sizeof(*dev) + 31;
if (!(dev = kmalloc(sz, GFP_KERNEL)))
return NULL;
memset(dev, 0, sz);
if (sz)
dev->priv = (void *)(((long)dev + sizeof(*dev) + 31) & ~31);
dev->name = kmalloc(8, GFP_KERNEL);
dev->name[0] = 0;
ether_setup(dev);
return dev;
}
#endif