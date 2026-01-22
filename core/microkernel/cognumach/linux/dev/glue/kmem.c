#include <sys/types.h>
#include <mach/mach_types.h>
#include <mach/vm_param.h>
#include <kern/assert.h>
#include <kern/kalloc.h>
#include <kern/printf.h>
#include <vm/vm_page.h>
#include <vm/vm_kern.h>
#define MACH_INCLUDE
#include <linux/sched.h>
#include <linux/malloc.h>
#include <linux/delay.h>
#include <asm/system.h>
#include <linux/dev/glue/glue.h>
#define MEM_CHUNK_SIZE (64 * 1024)
#define MEM_CHUNKS 32
#define MEM_DMA_LIMIT (16 * 1024 * 1024)
#define MIN_ALLOC 12
#ifndef NBPW
#define NBPW 32
#endif
struct blkhdr
{
unsigned short free;
unsigned short size;
};
struct pagehdr
{
unsigned size;
struct pagehdr *next;
};
struct chunkhdr
{
unsigned long start;
unsigned long end;
unsigned long bitmap;
};
static struct chunkhdr pages_free[MEM_CHUNKS];
static struct pagehdr *memlist;
int num_block_coalesce = 0;
int num_page_collect = 0;
int linux_mem_avail;
void
linux_kmem_init ()
{
int i, j;
vm_page_t p, pages;
for (i = 0; i < MEM_CHUNKS; i++)
{
pages_free[i].start = (unsigned long) alloc_contig_mem (MEM_CHUNK_SIZE,
MEM_DMA_LIMIT,
0xffff, &pages);
assert (pages_free[i].start);
assert ((pages_free[i].start & 0xffff) == 0);
for (p = pages, j = 0; j < MEM_CHUNK_SIZE - PAGE_SIZE; j += PAGE_SIZE)
{
assert (p->phys_addr < MEM_DMA_LIMIT);
assert (p->phys_addr + PAGE_SIZE == (p + 1)->phys_addr);
p++;
}
pages_free[i].end = pages_free[i].start + MEM_CHUNK_SIZE;
pages_free[i].bitmap = 0;
j = MEM_CHUNK_SIZE >> PAGE_SHIFT;
while (--j >= 0)
pages_free[i].bitmap |= 1 << j;
}
linux_mem_avail = (MEM_CHUNKS * MEM_CHUNK_SIZE) >> PAGE_SHIFT;
}
static unsigned long
get_page_order (int size)
{
unsigned long order;
for (order = 0; (PAGE_SIZE << order) < size; order++)
;
return order;
}
#ifdef LINUX_DEV_DEBUG
static void
check_page_list (int line)
{
unsigned size;
struct pagehdr *ph;
struct blkhdr *bh;
for (ph = memlist; ph; ph = ph->next)
{
if ((int) ph & PAGE_MASK)
panic ("%s:%d: page header not aligned", __FILE__, line);
size = 0;
bh = (struct blkhdr *) (ph + 1);
while (bh < (struct blkhdr *) ((void *) ph + ph->size))
{
size += bh->size + sizeof (struct blkhdr);
bh = (void *) (bh + 1) + bh->size;
}
if (size + sizeof (struct pagehdr) != ph->size)
panic ("%s:%d: memory list destroyed", __FILE__, line);
}
}
#else
#define check_page_list(line)
#endif
static void
coalesce_blocks ()
{
struct pagehdr *ph;
struct blkhdr *bh, *bhp, *ebh;
num_block_coalesce++;
for (ph = memlist; ph; ph = ph->next)
{
bh = (struct blkhdr *) (ph + 1);
ebh = (struct blkhdr *) ((void *) ph + ph->size);
while (1)
{
while (bh < ebh && !bh->free)
bh = (struct blkhdr *) ((void *) (bh + 1) + bh->size);
if (bh == ebh)
break;
while (1)
{
bhp = (struct blkhdr *) ((void *) (bh + 1) + bh->size);
if (bhp == ebh)
{
bh = bhp;
break;
}
if (!bhp->free)
{
bh = (struct blkhdr *) ((void *) (bhp + 1) + bhp->size);
break;
}
bh->size += bhp->size + sizeof (struct blkhdr);
}
}
}
}
void *
linux_kmalloc (unsigned int size, int priority)
{
int order, coalesced = 0;
unsigned long flags;
struct pagehdr *ph;
struct blkhdr *bh, *new_bh;
if (size < MIN_ALLOC)
size = MIN_ALLOC;
else
size = (size + sizeof (int) - 1) & ~(sizeof (int) - 1);
assert (size <= (MEM_CHUNK_SIZE
- sizeof (struct pagehdr)
- sizeof (struct blkhdr)));
save_flags (flags);
cli ();
again:
check_page_list (__LINE__);
for (ph = memlist; ph; ph = ph->next)
{
bh = (struct blkhdr *) (ph + 1);
while (bh < (struct blkhdr *) ((void *) ph + ph->size))
{
if (bh->free && bh->size >= size)
{
bh->free = 0;
if (bh->size - size >= MIN_ALLOC + sizeof (struct blkhdr))
{
new_bh = (void *) (bh + 1) + size;
new_bh->free = 1;
new_bh->size = bh->size - size - sizeof (struct blkhdr);
bh->size = size;
}
check_page_list (__LINE__);
restore_flags (flags);
return bh + 1;
}
bh = (void *) (bh + 1) + bh->size;
}
}
check_page_list (__LINE__);
if (!coalesced)
{
coalesce_blocks ();
coalesced = 1;
goto again;
}
order = get_page_order (size
+ sizeof (struct pagehdr)
+ sizeof (struct blkhdr));
ph = (struct pagehdr *) __get_free_pages (GFP_KERNEL, order, ~0UL);
if (!ph)
{
restore_flags (flags);
return NULL;
}
ph->size = PAGE_SIZE << order;
ph->next = memlist;
memlist = ph;
bh = (struct blkhdr *) (ph + 1);
bh->free = 0;
bh->size = ph->size - sizeof (struct pagehdr) - sizeof (struct blkhdr);
if (bh->size - size >= MIN_ALLOC + sizeof (struct blkhdr))
{
new_bh = (void *) (bh + 1) + size;
new_bh->free = 1;
new_bh->size = bh->size - size - sizeof (struct blkhdr);
bh->size = size;
}
check_page_list (__LINE__);
restore_flags (flags);
return bh + 1;
}
void
linux_kfree (void *p)
{
unsigned long flags;
struct blkhdr *bh;
struct pagehdr *ph;
assert (((int) p & (sizeof (int) - 1)) == 0);
save_flags (flags);
cli ();
check_page_list (__LINE__);
for (ph = memlist; ph; ph = ph->next)
if (p >= (void *) ph && p < (void *) ph + ph->size)
break;
assert (ph);
bh = (struct blkhdr *) p - 1;
assert (!bh->free);
assert (bh->size >= MIN_ALLOC);
assert ((bh->size & (sizeof (int) - 1)) == 0);
bh->free = 1;
check_page_list (__LINE__);
restore_flags (flags);
}
static void
collect_kmalloc_pages ()
{
struct blkhdr *bh;
struct pagehdr *ph, **prev_ph;
check_page_list (__LINE__);
coalesce_blocks ();
check_page_list (__LINE__);
ph = memlist;
prev_ph = &memlist;
while (ph)
{
bh = (struct blkhdr *) (ph + 1);
if (bh->free && (void *) (bh + 1) + bh->size == (void *) ph + ph->size)
{
*prev_ph = ph->next;
free_pages ((unsigned long) ph, get_page_order (ph->size));
ph = *prev_ph;
}
else
{
prev_ph = &ph->next;
ph = ph->next;
}
}
check_page_list (__LINE__);
}
unsigned long
__get_free_pages (int priority, unsigned long order, int dma)
{
int i, pages_collected = 0;
unsigned bits, off, j, len;
unsigned long flags;
assert ((PAGE_SIZE << order) <= MEM_CHUNK_SIZE);
bits = 0;
j = 0;
len = 0;
while (len < (PAGE_SIZE << order))
{
bits |= 1 << j++;
len += PAGE_SIZE;
}
save_flags (flags);
cli ();
again:
for (i = 0; i < MEM_CHUNKS; i++)
{
off = 0;
j = bits;
while (MEM_CHUNK_SIZE - off >= (PAGE_SIZE << order))
{
if ((pages_free[i].bitmap & j) == j)
{
pages_free[i].bitmap &= ~j;
linux_mem_avail -= order + 1;
restore_flags (flags);
return pages_free[i].start + off;
}
j <<= 1;
off += PAGE_SIZE;
}
}
if (!pages_collected)
{
num_page_collect++;
collect_kmalloc_pages ();
pages_collected = 1;
goto again;
}
printf ("%s:%d: __get_free_pages: ran out of pages\n", __FILE__, __LINE__);
restore_flags (flags);
return 0;
}
void
free_pages (unsigned long addr, unsigned long order)
{
int i;
unsigned bits, len, j;
unsigned long flags;
assert ((addr & PAGE_MASK) == 0);
for (i = 0; i < MEM_CHUNKS; i++)
if (addr >= pages_free[i].start && addr < pages_free[i].end)
break;
assert (i < MEM_CHUNKS);
len = 0;
j = 0;
bits = 0;
while (len < (PAGE_SIZE << order))
{
bits |= 1 << j++;
len += PAGE_SIZE;
}
bits <<= (addr - pages_free[i].start) >> PAGE_SHIFT;
save_flags (flags);
cli ();
assert ((pages_free[i].bitmap & bits) == 0);
pages_free[i].bitmap |= bits;
linux_mem_avail += order + 1;
restore_flags (flags);
}
struct vmalloc_struct
{
struct vmalloc_struct *prev;
struct vmalloc_struct *next;
vm_offset_t start;
vm_size_t size;
};
static struct vmalloc_struct
vmalloc_list = { &vmalloc_list, &vmalloc_list, 0, 0 };
static inline void
vmalloc_list_insert (vm_offset_t start, vm_size_t size)
{
struct vmalloc_struct *p;
p = (struct vmalloc_struct *) kalloc (sizeof (struct vmalloc_struct));
if (p == NULL)
panic ("kernel memory is exhausted");
p->prev = vmalloc_list.prev;
p->next = &vmalloc_list;
vmalloc_list.prev->next = p;
vmalloc_list.prev = p;
p->start = start;
p->size = size;
}
static struct vmalloc_struct *
vmalloc_list_lookup (vm_offset_t start)
{
struct vmalloc_struct *p;
for (p = vmalloc_list.next; p != &vmalloc_list; p = p->next)
{
if (p->start == start)
return p;
}
return NULL;
}
static inline void
vmalloc_list_remove (struct vmalloc_struct *p)
{
p->next->prev = p->prev;
p->prev->next = p->next;
kfree ((vm_offset_t) p, sizeof (struct vmalloc_struct));
}
void *
vmalloc (unsigned long size)
{
kern_return_t ret;
vm_offset_t addr;
ret = kmem_alloc_wired (kernel_map, &addr, round_page (size));
if (ret != KERN_SUCCESS)
return NULL;
vmalloc_list_insert (addr, round_page (size));
return (void *) addr;
}
void
vfree (void *addr)
{
struct vmalloc_struct *p;
p = vmalloc_list_lookup ((vm_offset_t) addr);
if (!p)
panic ("vmalloc_list_lookup failure");
kmem_free (kernel_map, (vm_offset_t) addr, p->size);
vmalloc_list_remove (p);
}
unsigned long
vmtophys (void *addr)
{
return kvtophys((vm_offset_t) addr);
}
#include <vm/pmap.h>
void *
vremap (unsigned long offset, unsigned long size)
{
vm_offset_t addr;
kern_return_t ret;
assert(page_aligned(offset));
ret = kmem_valloc (kernel_map, &addr, round_page (size));
if (ret != KERN_SUCCESS)
return NULL;
(void) pmap_map_bd (addr, offset, offset + round_page (size),
VM_PROT_READ | VM_PROT_WRITE);
vmalloc_list_insert (addr, round_page (size));
return (void *) addr;
}