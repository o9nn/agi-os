#ifndef _LINUX_MM_H
#define _LINUX_MM_H
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/kernel.h>
#ifdef __KERNEL__
#include <linux/string.h>
extern unsigned long high_memory;
#include <asm/page.h>
#include <asm/atomic.h>
#define VERIFY_READ 0
#define VERIFY_WRITE 1
extern int verify_area(int, const void *, unsigned long);
struct vm_area_struct {
struct mm_struct * vm_mm;
unsigned long vm_start;
unsigned long vm_end;
pgprot_t vm_page_prot;
unsigned short vm_flags;
short vm_avl_height;
struct vm_area_struct * vm_avl_left;
struct vm_area_struct * vm_avl_right;
struct vm_area_struct * vm_next;
struct vm_area_struct * vm_next_share;
struct vm_area_struct * vm_prev_share;
struct vm_operations_struct * vm_ops;
unsigned long vm_offset;
struct inode * vm_inode;
unsigned long vm_pte;
};
#define VM_READ		0x0001
#define VM_WRITE	0x0002
#define VM_EXEC		0x0004
#define VM_SHARED	0x0008
#define VM_MAYREAD	0x0010
#define VM_MAYWRITE	0x0020
#define VM_MAYEXEC	0x0040
#define VM_MAYSHARE	0x0080
#define VM_GROWSDOWN	0x0100
#define VM_GROWSUP	0x0200
#define VM_SHM		0x0400
#define VM_DENYWRITE	0x0800
#define VM_EXECUTABLE	0x1000
#define VM_LOCKED	0x2000
#define VM_STACK_FLAGS	0x0177
extern pgprot_t protection_map[16];
struct vm_operations_struct {
void (*open)(struct vm_area_struct * area);
void (*close)(struct vm_area_struct * area);
void (*unmap)(struct vm_area_struct *area, unsigned long, size_t);
void (*protect)(struct vm_area_struct *area, unsigned long, size_t, unsigned int newprot);
int (*sync)(struct vm_area_struct *area, unsigned long, size_t, unsigned int flags);
void (*advise)(struct vm_area_struct *area, unsigned long, size_t, unsigned int advise);
unsigned long (*nopage)(struct vm_area_struct * area, unsigned long address, int write_access);
unsigned long (*wppage)(struct vm_area_struct * area, unsigned long address,
unsigned long page);
int (*swapout)(struct vm_area_struct *,  unsigned long, pte_t *);
pte_t (*swapin)(struct vm_area_struct *, unsigned long, unsigned long);
};
typedef struct page {
struct page *next;
struct page *prev;
struct inode *inode;
unsigned long offset;
struct page *next_hash;
atomic_t count;
unsigned flags;
unsigned dirty:16,
age:8;
struct wait_queue *wait;
struct page *prev_hash;
struct buffer_head * buffers;
unsigned long swap_unlock_entry;
unsigned long map_nr;
} mem_map_t;
#define PG_locked		 0
#define PG_error		 1
#define PG_referenced		 2
#define PG_uptodate		 3
#define PG_free_after		 4
#define PG_decr_after		 5
#define PG_swap_unlock_after	 6
#define PG_DMA			 7
#define PG_reserved		31
#define PageLocked(page)	(test_bit(PG_locked, &(page)->flags))
#define PageError(page)		(test_bit(PG_error, &(page)->flags))
#define PageReferenced(page)	(test_bit(PG_referenced, &(page)->flags))
#define PageDirty(page)		(test_bit(PG_dirty, &(page)->flags))
#define PageUptodate(page)	(test_bit(PG_uptodate, &(page)->flags))
#define PageFreeAfter(page)	(test_bit(PG_free_after, &(page)->flags))
#define PageDecrAfter(page)	(test_bit(PG_decr_after, &(page)->flags))
#define PageSwapUnlockAfter(page) (test_bit(PG_swap_unlock_after, &(page)->flags))
#define PageDMA(page)		(test_bit(PG_DMA, &(page)->flags))
#define PageReserved(page)	(test_bit(PG_reserved, &(page)->flags))
extern mem_map_t * mem_map;
#define __get_free_page(priority) __get_free_pages((priority),0,0)
#define __get_dma_pages(priority, order) __get_free_pages((priority),(order),1)
extern unsigned long __get_free_pages(int priority, unsigned long gfporder, int dma);
extern inline unsigned long get_free_page(int priority)
{
unsigned long page;
page = __get_free_page(priority);
if (page)
memset((void *) page, 0, PAGE_SIZE);
return page;
}
#define free_page(addr) free_pages((addr),0)
extern void free_pages(unsigned long addr, unsigned long order);
extern void __free_page(struct page *);
extern void show_free_areas(void);
extern unsigned long put_dirty_page(struct task_struct * tsk,unsigned long page,
unsigned long address);
extern void free_page_tables(struct mm_struct * mm);
extern void clear_page_tables(struct task_struct * tsk);
extern int new_page_tables(struct task_struct * tsk);
extern int copy_page_tables(struct task_struct * to);
extern int zap_page_range(struct mm_struct *mm, unsigned long address, unsigned long size);
extern int copy_page_range(struct mm_struct *dst, struct mm_struct *src, struct vm_area_struct *vma);
extern int remap_page_range(unsigned long from, unsigned long to, unsigned long size, pgprot_t prot);
extern int zeromap_page_range(unsigned long from, unsigned long size, pgprot_t prot);
extern void vmtruncate(struct inode * inode, unsigned long offset);
extern void handle_mm_fault(struct vm_area_struct *vma, unsigned long address, int write_access);
extern void do_wp_page(struct task_struct * tsk, struct vm_area_struct * vma, unsigned long address, int write_access);
extern void do_no_page(struct task_struct * tsk, struct vm_area_struct * vma, unsigned long address, int write_access);
extern unsigned long paging_init(unsigned long start_mem, unsigned long end_mem);
extern void mem_init(unsigned long start_mem, unsigned long end_mem);
extern void show_mem(void);
extern void oom(struct task_struct * tsk);
extern void si_meminfo(struct sysinfo * val);
extern void * vmalloc(unsigned long size);
extern void * vremap(unsigned long offset, unsigned long size);
extern void vfree(void * addr);
extern int vread(char *buf, char *addr, int count);
extern unsigned long do_mmap(struct file * file, unsigned long addr, unsigned long len,
unsigned long prot, unsigned long flags, unsigned long off);
extern void merge_segments(struct mm_struct *, unsigned long, unsigned long);
extern void insert_vm_struct(struct mm_struct *, struct vm_area_struct *);
extern void remove_shared_vm_struct(struct vm_area_struct *);
extern void build_mmap_avl(struct mm_struct *);
extern void exit_mmap(struct mm_struct *);
extern int do_munmap(unsigned long, size_t);
extern unsigned long get_unmapped_area(unsigned long, unsigned long);
extern unsigned long page_unuse(unsigned long);
extern int shrink_mmap(int, int, int);
extern void truncate_inode_pages(struct inode *, unsigned long);
#define GFP_BUFFER	0x00
#define GFP_ATOMIC	0x01
#define GFP_USER	0x02
#define GFP_KERNEL	0x03
#define GFP_NOBUFFER	0x04
#define GFP_NFS		0x05
#define GFP_IO		0x06
#define GFP_DMA		0x80
#define GFP_LEVEL_MASK 0xf
static inline int expand_stack(struct vm_area_struct * vma, unsigned long address)
{
unsigned long grow;
address &= PAGE_MASK;
grow = vma->vm_start - address;
if (vma->vm_end - address
> (unsigned long) current->rlim[RLIMIT_STACK].rlim_cur ||
(vma->vm_mm->total_vm << PAGE_SHIFT) + grow
> (unsigned long) current->rlim[RLIMIT_AS].rlim_cur)
return -ENOMEM;
vma->vm_start = address;
vma->vm_offset -= grow;
vma->vm_mm->total_vm += grow >> PAGE_SHIFT;
if (vma->vm_flags & VM_LOCKED)
vma->vm_mm->locked_vm += grow >> PAGE_SHIFT;
return 0;
}
#define avl_empty	(struct vm_area_struct *) NULL
static inline struct vm_area_struct * find_vma(struct mm_struct * mm, unsigned long addr)
{
struct vm_area_struct * result = NULL;
if (mm) {
struct vm_area_struct * tree = mm->mmap_avl;
for (;;) {
if (tree == avl_empty)
break;
if (tree->vm_end > addr) {
result = tree;
if (tree->vm_start <= addr)
break;
tree = tree->vm_avl_left;
} else
tree = tree->vm_avl_right;
}
}
return result;
}
static inline struct vm_area_struct * find_vma_intersection(struct mm_struct * mm, unsigned long start_addr, unsigned long end_addr)
{
struct vm_area_struct * vma;
vma = find_vma(mm,start_addr);
if (vma && end_addr <= vma->vm_start)
vma = NULL;
return vma;
}
#endif
#endif