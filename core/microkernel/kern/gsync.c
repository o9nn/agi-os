#include <kern/gsync.h>
#include <kern/kmutex.h>
#include <kern/sched_prim.h>
#include <kern/thread.h>
#include <kern/list.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <machine/locore.h>
struct gsync_hbucket
{
struct list entries;
struct kmutex lock;
};
union gsync_key
{
struct
{
vm_map_t map;
vm_offset_t addr;
} local;
struct
{
vm_object_t obj;
vm_offset_t off;
} shared;
struct
{
unsigned long u;
unsigned long v;
} any;
};
struct gsync_waiter
{
struct list link;
union gsync_key key;
thread_t waiter;
};
struct vm_args
{
vm_object_t obj;
vm_offset_t off;
};
#define GSYNC_NBUCKETS   512
static struct gsync_hbucket gsync_buckets[GSYNC_NBUCKETS];
void gsync_setup (void)
{
int i;
for (i = 0; i < GSYNC_NBUCKETS; ++i)
{
list_init (&gsync_buckets[i].entries);
kmutex_init (&gsync_buckets[i].lock);
}
}
static inline int
gsync_key_eq (const union gsync_key *lp,
const union gsync_key *rp)
{
return (lp->any.u == rp->any.u && lp->any.v == rp->any.v);
}
static inline int
gsync_key_lt (const union gsync_key *lp,
const union gsync_key *rp)
{
return (lp->any.u < rp->any.u ||
(lp->any.u == rp->any.u && lp->any.v < rp->any.v));
}
#define MIX2_LL(x, y)   ((((x) << 5) | ((x) >> 27)) ^ (y))
static inline unsigned int
gsync_key_hash (const union gsync_key *keyp)
{
unsigned int ret = sizeof (void *);
#ifndef __LP64__
ret = MIX2_LL (ret, keyp->any.u);
ret = MIX2_LL (ret, keyp->any.v);
#else
ret = MIX2_LL (ret, keyp->any.u & ~0U);
ret = MIX2_LL (ret, keyp->any.u >> 32);
ret = MIX2_LL (ret, keyp->any.v & ~0U);
ret = MIX2_LL (ret, keyp->any.v >> 32);
#endif
return (ret);
}
static int
probe_address (vm_map_t map, vm_offset_t addr,
int flags, struct vm_args *vap)
{
vm_prot_t prot = VM_PROT_READ |
((flags & GSYNC_MUTATE) ? VM_PROT_WRITE : 0);
vm_map_version_t ver;
vm_prot_t rprot;
boolean_t wired_p;
if (vm_map_lookup (&map, addr, prot, TRUE, &ver,
&vap->obj, &vap->off, &rprot, &wired_p) != KERN_SUCCESS)
return (-1);
else if ((rprot & prot) != prot)
{
vm_map_unlock_read (map);
vm_object_unlock (vap->obj);
return (-1);
}
return (0);
}
static int
gsync_prepare_key (task_t task, vm_offset_t addr, int flags,
union gsync_key *keyp, struct vm_args *vap)
{
if (probe_address (task->map, addr, flags, vap) < 0)
return (-1);
else if (flags & GSYNC_SHARED)
{
keyp->shared.obj = vap->obj;
keyp->shared.off = vap->off;
}
else
{
keyp->local.map = task->map;
keyp->local.addr = addr;
}
return ((int)(gsync_key_hash (keyp) % GSYNC_NBUCKETS));
}
static inline struct gsync_waiter*
node_to_waiter (struct list *nodep)
{
return (list_entry (nodep, struct gsync_waiter, link));
}
static inline struct list*
gsync_find_key (const struct list *entries,
const union gsync_key *keyp, int *exactp)
{
struct list *runp;
list_for_each (entries, runp)
{
struct gsync_waiter *p = node_to_waiter (runp);
if (gsync_key_lt (keyp, &p->key))
break;
else if (gsync_key_eq (keyp, &p->key))
{
if (exactp != 0)
*exactp = 1;
break;
}
}
return (runp);
}
static inline vm_offset_t
temp_mapping (struct vm_args *vap, vm_offset_t addr, vm_prot_t prot)
{
vm_offset_t paddr = VM_MIN_KERNEL_ADDRESS;
vm_offset_t off = vap->off - (addr - trunc_page (addr));
if (vm_map_enter (kernel_map, &paddr, PAGE_SIZE,
0, TRUE, vap->obj, off, FALSE, prot, VM_PROT_ALL,
VM_INHERIT_DEFAULT) != KERN_SUCCESS)
paddr = 0;
return (paddr);
}
kern_return_t gsync_wait (task_t task, vm_offset_t addr,
unsigned int lo, unsigned int hi, natural_t msec, int flags)
{
if (task == 0)
return (KERN_INVALID_TASK);
else if (addr % sizeof (int) != 0)
return (KERN_INVALID_ADDRESS);
struct gsync_waiter w;
struct vm_args va;
boolean_t remote = task != current_task ();
int bucket = gsync_prepare_key (task, addr, flags, &w.key, &va);
if (bucket < 0)
return (KERN_INVALID_ADDRESS);
else if (remote)
vm_object_reference_locked (va.obj);
vm_object_unlock (va.obj);
struct gsync_hbucket *hbp = gsync_buckets + bucket;
kmutex_lock (&hbp->lock, FALSE);
boolean_t equal;
if (! remote)
{
unsigned int value;
if (copyin ((const void *) addr, &value, 4))
{
vm_map_unlock_read (task->map);
kmutex_unlock (&hbp->lock);
return KERN_INVALID_ADDRESS;
}
equal = (value == lo);
if (flags & GSYNC_QUAD)
{
if (copyin ((const void *) (addr + 4), &value, 4))
{
vm_map_unlock_read (task->map);
kmutex_unlock (&hbp->lock);
return KERN_INVALID_ADDRESS;
}
equal = equal && (value == hi);
}
}
else
{
vm_offset_t paddr = temp_mapping (&va, addr, VM_PROT_READ);
if (unlikely (paddr == 0))
{
kmutex_unlock (&hbp->lock);
vm_map_unlock_read (task->map);
vm_object_deallocate (va.obj);
return (KERN_MEMORY_FAILURE);
}
vm_offset_t off = addr & (PAGE_SIZE - 1);
paddr += off;
equal = ((unsigned int *)paddr)[0] == lo &&
((flags & GSYNC_QUAD) == 0 ||
((unsigned int *)paddr)[1] == hi);
paddr -= off;
vm_map_remove (kernel_map, paddr, paddr + PAGE_SIZE);
}
vm_map_unlock_read (task->map);
if (! equal)
{
kmutex_unlock (&hbp->lock);
return (KERN_INVALID_ARGUMENT);
}
struct list *runp;
list_for_each (&hbp->entries, runp)
if (gsync_key_lt (&w.key, &node_to_waiter(runp)->key))
break;
list_add (runp->prev, runp, &w.link);
w.waiter = current_thread ();
if (flags & GSYNC_TIMED)
thread_will_wait_with_timeout (w.waiter, msec);
else
thread_will_wait (w.waiter);
kmutex_unlock (&hbp->lock);
thread_block (thread_no_continuation);
kern_return_t ret = KERN_SUCCESS;
if (current_thread()->wait_result != THREAD_AWAKENED)
{
kmutex_lock (&hbp->lock, FALSE);
if (!list_node_unlinked (&w.link))
list_remove (&w.link);
kmutex_unlock (&hbp->lock);
ret = current_thread()->wait_result == THREAD_INTERRUPTED ?
KERN_INTERRUPTED : KERN_TIMEDOUT;
}
return (ret);
}
static inline struct list*
dequeue_waiter (struct list *nodep)
{
struct list *nextp = list_next (nodep);
list_remove (nodep);
list_node_init (nodep);
clear_wait (node_to_waiter(nodep)->waiter,
THREAD_AWAKENED, FALSE);
return (nextp);
}
kern_return_t gsync_wake (task_t task,
vm_offset_t addr, unsigned int val, int flags)
{
if (task == 0)
return (KERN_INVALID_TASK);
else if (addr % sizeof (int) != 0)
return (KERN_INVALID_ADDRESS);
union gsync_key key;
struct vm_args va;
int bucket = gsync_prepare_key (task, addr, flags, &key, &va);
if (bucket < 0)
return (KERN_INVALID_ADDRESS);
else if (current_task () != task && (flags & GSYNC_MUTATE) != 0)
vm_object_reference_locked (va.obj);
vm_object_unlock (va.obj);
kern_return_t ret = KERN_INVALID_ARGUMENT;
struct gsync_hbucket *hbp = gsync_buckets + bucket;
kmutex_lock (&hbp->lock, FALSE);
if (flags & GSYNC_MUTATE)
{
if (task != current_task ())
{
vm_offset_t paddr = temp_mapping (&va, addr,
VM_PROT_READ | VM_PROT_WRITE);
if (paddr == 0)
{
kmutex_unlock (&hbp->lock);
vm_map_unlock_read (task->map);
vm_object_deallocate (va.obj);
return (KERN_MEMORY_FAILURE);
}
addr = paddr + (addr & (PAGE_SIZE - 1));
*(unsigned int *)addr = val;
vm_map_remove (kernel_map, addr, addr + sizeof (int));
}
else if (copyout (&val, (void *) addr, 4))
{
kmutex_unlock (&hbp->lock);
vm_map_unlock_read (task->map);
return KERN_INVALID_ADDRESS;
}
}
vm_map_unlock_read (task->map);
int found = 0;
struct list *runp = gsync_find_key (&hbp->entries, &key, &found);
if (found)
{
do
runp = dequeue_waiter (runp);
while ((flags & GSYNC_BROADCAST) &&
!list_end (&hbp->entries, runp) &&
gsync_key_eq (&node_to_waiter(runp)->key, &key));
ret = KERN_SUCCESS;
}
kmutex_unlock (&hbp->lock);
return (ret);
}
kern_return_t gsync_requeue (task_t task, vm_offset_t src,
vm_offset_t dst, boolean_t wake_one, int flags)
{
if (task == 0)
return (KERN_INVALID_TASK);
else if (src % sizeof (int) != 0 || dst % sizeof (int) != 0)
return (KERN_INVALID_ADDRESS);
union gsync_key src_k, dst_k;
struct vm_args va;
int src_bkt = gsync_prepare_key (task, src, flags, &src_k, &va);
if (src_bkt < 0)
return (KERN_INVALID_ADDRESS);
vm_map_unlock_read (task->map);
vm_object_unlock (va.obj);
int dst_bkt = gsync_prepare_key (task, dst, flags, &dst_k, &va);
if (dst_bkt < 0)
return (KERN_INVALID_ADDRESS);
vm_map_unlock_read (task->map);
vm_object_unlock (va.obj);
unsigned int nw = 1 + wake_one;
struct gsync_hbucket *bp1 = gsync_buckets + src_bkt;
struct gsync_hbucket *bp2 = gsync_buckets + dst_bkt;
if (bp1 == bp2)
kmutex_lock (&bp1->lock, FALSE);
else if ((unsigned long)bp1 < (unsigned long)bp2)
{
kmutex_lock (&bp1->lock, FALSE);
kmutex_lock (&bp2->lock, FALSE);
}
else
{
kmutex_lock (&bp2->lock, FALSE);
kmutex_lock (&bp1->lock, FALSE);
}
kern_return_t ret = KERN_SUCCESS;
int exact;
struct list *inp = gsync_find_key (&bp1->entries, &src_k, &exact);
if (! exact)
ret = KERN_INVALID_ARGUMENT;
else
{
struct list *outp = gsync_find_key (&bp2->entries, &dst_k, 0);
struct list *endp = inp;
do
{
node_to_waiter(endp)->key = dst_k;
endp = list_next (endp);
}
while (((flags & GSYNC_BROADCAST) || --nw != 0) &&
!list_end (&bp1->entries, endp) &&
gsync_key_eq (&node_to_waiter(endp)->key, &src_k));
inp->prev->next = endp;
endp->prev->next = outp->next;
endp->prev = inp->prev;
outp->next = inp;
inp->prev = outp;
if (wake_one)
(void)dequeue_waiter (inp);
}
kmutex_unlock (&bp1->lock);
if (bp1 != bp2)
kmutex_unlock (&bp2->lock);
return (ret);
}