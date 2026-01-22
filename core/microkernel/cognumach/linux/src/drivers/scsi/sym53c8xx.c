#define SCSI_NCR_DRIVER_NAME	"sym53c8xx-1.7.1-20000726"
#define SCSI_NCR_DEBUG_FLAGS	(0)
#define NAME53C		"sym53c"
#define NAME53C8XX	"sym53c8xx"
#define LinuxVersionCode(v, p, s) (((v)<<16)+((p)<<8)+(s))
#ifdef MODULE
#include <linux/module.h>
#endif
#include <asm/dma.h>
#include <asm/io.h>
#include <asm/system.h>
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,3,17)
#include <linux/spinlock.h>
#elif LINUX_VERSION_CODE >= LinuxVersionCode(2,1,93)
#include <asm/spinlock.h>
#endif
#include <linux/delay.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/pci.h>
#include <linux/string.h>
#include <linux/malloc.h>
#include <linux/mm.h>
#include <linux/ioport.h>
#include <linux/time.h>
#include <linux/timer.h>
#include <linux/stat.h>
#include <linux/version.h>
#include <linux/blk.h>
#ifdef CONFIG_ALL_PPC
#include <asm/prom.h>
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,35)
#include <linux/init.h>
#endif
#ifndef	__init
#define	__init
#endif
#ifndef	__initdata
#define	__initdata
#endif
#if LINUX_VERSION_CODE <= LinuxVersionCode(2,1,92)
#include <linux/bios32.h>
#endif
#include "scsi.h"
#include "hosts.h"
#include "constants.h"
#include "sd.h"
#include <linux/types.h>
#ifndef	BITS_PER_LONG
#if (~0UL) == 0xffffffffUL
#define	BITS_PER_LONG	32
#else
#define	BITS_PER_LONG	64
#endif
#endif
typedef u32 u_int32;
typedef u64 u_int64;
#include "sym53c8xx.h"
#if 0
#define	SCSI_NCR_INTEGRITY_CHECKING
#endif
#define MIN(a,b)        (((a) < (b)) ? (a) : (b))
#define MAX(a,b)        (((a) > (b)) ? (a) : (b))
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,3,47)
#define SCSI_NCR_DYNAMIC_DMA_MAPPING
#endif
typedef struct xpt_quehead {
struct xpt_quehead *flink;
struct xpt_quehead *blink;
} XPT_QUEHEAD;
#define xpt_que_init(ptr) do { \
(ptr)->flink = (ptr); (ptr)->blink = (ptr); \
} while (0)
static inline void __xpt_que_add(struct xpt_quehead * new,
struct xpt_quehead * blink,
struct xpt_quehead * flink)
{
flink->blink	= new;
new->flink	= flink;
new->blink	= blink;
blink->flink	= new;
}
static inline void __xpt_que_del(struct xpt_quehead * blink,
struct xpt_quehead * flink)
{
flink->blink = blink;
blink->flink = flink;
}
static inline int xpt_que_empty(struct xpt_quehead *head)
{
return head->flink == head;
}
static inline void xpt_que_splice(struct xpt_quehead *list,
struct xpt_quehead *head)
{
struct xpt_quehead *first = list->flink;
if (first != list) {
struct xpt_quehead *last = list->blink;
struct xpt_quehead *at   = head->flink;
first->blink = head;
head->flink  = first;
last->flink = at;
at->blink   = last;
}
}
#define xpt_que_entry(ptr, type, member) \
((type *)((char *)(ptr)-(unsigned long)(&((type *)0)->member)))
#define xpt_insque(new, pos)		__xpt_que_add(new, pos, (pos)->flink)
#define xpt_remque(el)			__xpt_que_del((el)->blink, (el)->flink)
#define xpt_insque_head(new, head)	__xpt_que_add(new, head, (head)->flink)
static inline struct xpt_quehead *xpt_remque_head(struct xpt_quehead *head)
{
struct xpt_quehead *elem = head->flink;
if (elem != head)
__xpt_que_del(head, elem->flink);
else
elem = 0;
return elem;
}
#define xpt_insque_tail(new, head)	__xpt_que_add(new, (head)->blink, head)
static inline struct xpt_quehead *xpt_remque_tail(struct xpt_quehead *head)
{
struct xpt_quehead *elem = head->blink;
if (elem != head)
__xpt_que_del(elem->blink, head);
else
elem = 0;
return elem;
}
#ifndef SCSI_NCR_MYADDR
#define SCSI_NCR_MYADDR      (7)
#endif
#ifndef SCSI_NCR_MAX_TAGS
#define SCSI_NCR_MAX_TAGS    (8)
#endif
#if	SCSI_NCR_MAX_TAGS > 255
#define	MAX_TAGS	255
#else
#define	MAX_TAGS SCSI_NCR_MAX_TAGS
#endif
#if	MAX_TAGS > (512/4)
#define MAX_TASKS  (1024/4)
#elif	MAX_TAGS > (256/4)
#define MAX_TASKS  (512/4)
#else
#define MAX_TASKS  (256/4)
#endif
#define NO_TAG	(256)
#ifdef SCSI_NCR_MAX_TARGET
#define MAX_TARGET  (SCSI_NCR_MAX_TARGET)
#else
#define MAX_TARGET  (16)
#endif
#ifdef SCSI_NCR_MAX_LUN
#define MAX_LUN    64
#else
#define MAX_LUN    (1)
#endif
#ifndef SCSI_NCR_MIN_ASYNC
#define SCSI_NCR_MIN_ASYNC (40)
#endif
#ifdef SCSI_NCR_CAN_QUEUE
#define MAX_START   (SCSI_NCR_CAN_QUEUE + 4)
#else
#define MAX_START   (MAX_TARGET + 7 * MAX_TAGS)
#endif
#if	MAX_START > PAGE_SIZE/8
#undef	MAX_START
#define MAX_START (PAGE_SIZE/8)
#endif
#define MAX_SCATTER (SCSI_NCR_MAX_SCATTER)
#define	SCR_SG_SIZE	(2)
#define NCR_SNOOP_TIMEOUT (1000000)
#define u_char		unsigned char
#define u_short		unsigned short
#define u_int		unsigned int
#define u_long		unsigned long
#ifndef bcopy
#define bcopy(s, d, n)	memcpy((d), (s), (n))
#endif
#ifndef bzero
#define bzero(d, n)	memset((d), 0, (n))
#endif
#ifndef offsetof
#define offsetof(t, m)	((size_t) (&((t *)0)->m))
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,2,0)
typedef struct pci_dev *pcidev_t;
#define PCIDEV_NULL		(0)
#define PciBusNumber(d)		(d)->bus->number
#define PciDeviceFn(d)		(d)->devfn
#define PciVendorId(d)		(d)->vendor
#define PciDeviceId(d)		(d)->device
#define PciIrqLine(d)		(d)->irq
#if LINUX_VERSION_CODE > LinuxVersionCode(2,3,12)
static int __init
pci_get_base_address(struct pci_dev *pdev, int index, u_long *base)
{
*base = pdev->resource[index].start;
if ((pdev->resource[index].flags & 0x7) == 0x4)
++index;
return ++index;
}
#else
static int __init
pci_get_base_address(struct pci_dev *pdev, int index, u_long *base)
{
*base = pdev->base_address[index++];
if ((*base & 0x7) == 0x4) {
#if BITS_PER_LONG > 32
*base |= (((u_long)pdev->base_address[index]) << 32);
#endif
++index;
}
return index;
}
#endif
#else
typedef unsigned int pcidev_t;
#define PCIDEV_NULL		(~0u)
#define PciBusNumber(d)		((d)>>8)
#define PciDeviceFn(d)		((d)&0xff)
#define __PciDev(busn, devfn)	(((busn)<<8)+(devfn))
#define pci_present pcibios_present
#define pci_read_config_byte(d, w, v) \
pcibios_read_config_byte(PciBusNumber(d), PciDeviceFn(d), w, v)
#define pci_read_config_word(d, w, v) \
pcibios_read_config_word(PciBusNumber(d), PciDeviceFn(d), w, v)
#define pci_read_config_dword(d, w, v) \
pcibios_read_config_dword(PciBusNumber(d), PciDeviceFn(d), w, v)
#define pci_write_config_byte(d, w, v) \
pcibios_write_config_byte(PciBusNumber(d), PciDeviceFn(d), w, v)
#define pci_write_config_word(d, w, v) \
pcibios_write_config_word(PciBusNumber(d), PciDeviceFn(d), w, v)
#define pci_write_config_dword(d, w, v) \
pcibios_write_config_dword(PciBusNumber(d), PciDeviceFn(d), w, v)
static pcidev_t __init
pci_find_device(unsigned int vendor, unsigned int device, pcidev_t prev)
{
static unsigned short pci_index;
int retv;
unsigned char bus_number, device_fn;
if (prev == PCIDEV_NULL)
pci_index = 0;
else
++pci_index;
retv = pcibios_find_device (vendor, device, pci_index,
&bus_number, &device_fn);
return retv ? PCIDEV_NULL : __PciDev(bus_number, device_fn);
}
static u_short __init PciVendorId(pcidev_t dev)
{
u_short vendor_id;
pci_read_config_word(dev, PCI_VENDOR_ID, &vendor_id);
return vendor_id;
}
static u_short __init PciDeviceId(pcidev_t dev)
{
u_short device_id;
pci_read_config_word(dev, PCI_DEVICE_ID, &device_id);
return device_id;
}
static u_int __init PciIrqLine(pcidev_t dev)
{
u_char irq;
pci_read_config_byte(dev, PCI_INTERRUPT_LINE, &irq);
return irq;
}
static int __init
pci_get_base_address(pcidev_t dev, int offset, u_long *base)
{
u_int32 tmp;
pci_read_config_dword(dev, PCI_BASE_ADDRESS_0 + offset, &tmp);
*base = tmp;
offset += sizeof(u_int32);
if ((tmp & 0x7) == 0x4) {
#if BITS_PER_LONG > 32
pci_read_config_dword(dev, PCI_BASE_ADDRESS_0 + offset, &tmp);
*base |= (((u_long)tmp) << 32);
#endif
offset += sizeof(u_int32);
}
return offset;
}
#endif
#define DEBUG_ALLOC    (0x0001)
#define DEBUG_PHASE    (0x0002)
#define DEBUG_QUEUE    (0x0008)
#define DEBUG_RESULT   (0x0010)
#define DEBUG_POINTER  (0x0020)
#define DEBUG_SCRIPT   (0x0040)
#define DEBUG_TINY     (0x0080)
#define DEBUG_TIMING   (0x0100)
#define DEBUG_NEGO     (0x0200)
#define DEBUG_TAGS     (0x0400)
#define DEBUG_IC       (0x0800)
#ifdef SCSI_NCR_DEBUG_INFO_SUPPORT
static int ncr_debug = SCSI_NCR_DEBUG_FLAGS;
#define DEBUG_FLAGS ncr_debug
#else
#define DEBUG_FLAGS	SCSI_NCR_DEBUG_FLAGS
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,93)
spinlock_t sym53c8xx_lock = SPIN_LOCK_UNLOCKED;
#define	NCR_LOCK_DRIVER(flags)     spin_lock_irqsave(&sym53c8xx_lock, flags)
#define	NCR_UNLOCK_DRIVER(flags)   spin_unlock_irqrestore(&sym53c8xx_lock,flags)
#define NCR_INIT_LOCK_NCB(np)      spin_lock_init(&np->smp_lock);
#define	NCR_LOCK_NCB(np, flags)    spin_lock_irqsave(&np->smp_lock, flags)
#define	NCR_UNLOCK_NCB(np, flags)  spin_unlock_irqrestore(&np->smp_lock, flags)
#define	NCR_LOCK_SCSI_DONE(np, flags) \
spin_lock_irqsave(&io_request_lock, flags)
#define	NCR_UNLOCK_SCSI_DONE(np, flags) \
spin_unlock_irqrestore(&io_request_lock, flags)
#else
#define	NCR_LOCK_DRIVER(flags)     do { save_flags(flags); cli(); } while (0)
#define	NCR_UNLOCK_DRIVER(flags)   do { restore_flags(flags); } while (0)
#define	NCR_INIT_LOCK_NCB(np)      do { } while (0)
#define	NCR_LOCK_NCB(np, flags)    do { save_flags(flags); cli(); } while (0)
#define	NCR_UNLOCK_NCB(np, flags)  do { restore_flags(flags); } while (0)
#define	NCR_LOCK_SCSI_DONE(np, flags)    do {;} while (0)
#define	NCR_UNLOCK_SCSI_DONE(np, flags)  do {;} while (0)
#endif
#if LINUX_VERSION_CODE < LinuxVersionCode(2,1,0)
#define ioremap vremap
#define iounmap vfree
#endif
#ifdef __sparc__
#  include <asm/irq.h>
#  if LINUX_VERSION_CODE < LinuxVersionCode(2,3,0)
#    define ioremap(base, size)		((u_long) __va(base))
#    define iounmap(vaddr)
#  endif
#  define pcivtobus(p)			bus_dvma_to_mem(p)
#  define memcpy_to_pci(a, b, c)	memcpy_toio((void *)(a), (const void *)(b), (c))
#elif defined(__alpha__)
#  define pcivtobus(p)			((p) & 0xfffffffful)
#  define memcpy_to_pci(a, b, c)	memcpy_toio((a), (b), (c))
#else
#  define pcivtobus(p)			(p)
#  define memcpy_to_pci(a, b, c)	memcpy_toio((a), (b), (c))
#endif
#ifndef SCSI_NCR_PCI_MEM_NOT_SUPPORTED
static u_long __init remap_pci_mem(u_long base, u_long size)
{
u_long page_base	= ((u_long) base) & PAGE_MASK;
u_long page_offs	= ((u_long) base) - page_base;
u_long page_remapped	= (u_long) ioremap(page_base, page_offs+size);
return page_remapped? (page_remapped + page_offs) : 0UL;
}
static void __init unmap_pci_mem(u_long vaddr, u_long size)
{
if (vaddr)
iounmap((void *) (vaddr & PAGE_MASK));
}
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,105)
#define UDELAY udelay
#define MDELAY mdelay
#else
static void UDELAY(long us) { udelay(us); }
static void MDELAY(long ms) { while (ms--) UDELAY(1000); }
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,0)
#define __GetFreePages(flags, order) __get_free_pages(flags, order)
#else
#define __GetFreePages(flags, order) __get_free_pages(flags, order, 0)
#endif
#define MEMO_SHIFT	4
#if PAGE_SIZE >= 8192
#define MEMO_PAGE_ORDER	0
#else
#define MEMO_PAGE_ORDER	1
#endif
#define MEMO_FREE_UNUSED
#define MEMO_WARN	1
#define MEMO_GFP_FLAGS	GFP_ATOMIC
#define MEMO_CLUSTER_SHIFT	(PAGE_SHIFT+MEMO_PAGE_ORDER)
#define MEMO_CLUSTER_SIZE	(1UL << MEMO_CLUSTER_SHIFT)
#define MEMO_CLUSTER_MASK	(MEMO_CLUSTER_SIZE-1)
typedef u_long m_addr_t;
typedef pcidev_t m_bush_t;
typedef struct m_link {
struct m_link *next;
} m_link_s;
#ifdef	SCSI_NCR_DYNAMIC_DMA_MAPPING
typedef struct m_vtob {
struct m_vtob *next;
m_addr_t vaddr;
m_addr_t baddr;
} m_vtob_s;
#define VTOB_HASH_SHIFT		5
#define VTOB_HASH_SIZE		(1UL << VTOB_HASH_SHIFT)
#define VTOB_HASH_MASK		(VTOB_HASH_SIZE-1)
#define VTOB_HASH_CODE(m)	\
((((m_addr_t) (m)) >> MEMO_CLUSTER_SHIFT) & VTOB_HASH_MASK)
#endif
typedef struct m_pool {
#ifdef	SCSI_NCR_DYNAMIC_DMA_MAPPING
m_bush_t bush;
m_addr_t (*getp)(struct m_pool *);
void (*freep)(struct m_pool *, m_addr_t);
#define M_GETP()		mp->getp(mp)
#define M_FREEP(p)		mp->freep(mp, p)
#define GetPages()		__GetFreePages(MEMO_GFP_FLAGS, MEMO_PAGE_ORDER)
#define FreePages(p)		free_pages(p, MEMO_PAGE_ORDER)
int nump;
m_vtob_s *(vtob[VTOB_HASH_SIZE]);
struct m_pool *next;
#else
#define M_GETP()		__GetFreePages(MEMO_GFP_FLAGS, MEMO_PAGE_ORDER)
#define M_FREEP(p)		free_pages(p, MEMO_PAGE_ORDER)
#endif
struct m_link h[PAGE_SHIFT-MEMO_SHIFT+MEMO_PAGE_ORDER+1];
} m_pool_s;
static void *___m_alloc(m_pool_s *mp, int size)
{
int i = 0;
int s = (1 << MEMO_SHIFT);
int j;
m_addr_t a;
m_link_s *h = mp->h;
if (size > (PAGE_SIZE << MEMO_PAGE_ORDER))
return 0;
while (size > s) {
s <<= 1;
++i;
}
j = i;
while (!h[j].next) {
if (s == (PAGE_SIZE << MEMO_PAGE_ORDER)) {
h[j].next = (m_link_s *) M_GETP();
if (h[j].next)
h[j].next->next = 0;
break;
}
++j;
s <<= 1;
}
a = (m_addr_t) h[j].next;
if (a) {
h[j].next = h[j].next->next;
while (j > i) {
j -= 1;
s >>= 1;
h[j].next = (m_link_s *) (a+s);
h[j].next->next = 0;
}
}
#ifdef DEBUG
printk("___m_alloc(%d) = %p\n", size, (void *) a);
#endif
return (void *) a;
}
static void ___m_free(m_pool_s *mp, void *ptr, int size)
{
int i = 0;
int s = (1 << MEMO_SHIFT);
m_link_s *q;
m_addr_t a, b;
m_link_s *h = mp->h;
#ifdef DEBUG
printk("___m_free(%p, %d)\n", ptr, size);
#endif
if (size > (PAGE_SIZE << MEMO_PAGE_ORDER))
return;
while (size > s) {
s <<= 1;
++i;
}
a = (m_addr_t) ptr;
while (1) {
#ifdef MEMO_FREE_UNUSED
if (s == (PAGE_SIZE << MEMO_PAGE_ORDER)) {
M_FREEP(a);
break;
}
#endif
b = a ^ s;
q = &h[i];
while (q->next && q->next != (m_link_s *) b) {
q = q->next;
}
if (!q->next) {
((m_link_s *) a)->next = h[i].next;
h[i].next = (m_link_s *) a;
break;
}
q->next = q->next->next;
a = a & b;
s <<= 1;
++i;
}
}
static void *__m_calloc2(m_pool_s *mp, int size, char *name, int uflags)
{
void *p;
p = ___m_alloc(mp, size);
if (DEBUG_FLAGS & DEBUG_ALLOC)
printk ("new %-10s[%4d] @%p.\n", name, size, p);
if (p)
bzero(p, size);
else if (uflags & MEMO_WARN)
printk (NAME53C8XX ": failed to allocate %s[%d]\n", name, size);
return p;
}
#define __m_calloc(mp, s, n)	__m_calloc2(mp, s, n, MEMO_WARN)
static void __m_free(m_pool_s *mp, void *ptr, int size, char *name)
{
if (DEBUG_FLAGS & DEBUG_ALLOC)
printk ("freeing %-10s[%4d] @%p.\n", name, size, ptr);
___m_free(mp, ptr, size);
}
#ifndef	SCSI_NCR_DYNAMIC_DMA_MAPPING
static m_pool_s mp0;
#else
static m_addr_t ___mp0_getp(m_pool_s *mp)
{
m_addr_t m = GetPages();
if (m)
++mp->nump;
return m;
}
static void ___mp0_freep(m_pool_s *mp, m_addr_t m)
{
FreePages(m);
--mp->nump;
}
static m_pool_s mp0 = {0, ___mp0_getp, ___mp0_freep};
#endif
static void *m_calloc(int size, char *name)
{
u_long flags;
void *m;
NCR_LOCK_DRIVER(flags);
m = __m_calloc(&mp0, size, name);
NCR_UNLOCK_DRIVER(flags);
return m;
}
static void m_free(void *ptr, int size, char *name)
{
u_long flags;
NCR_LOCK_DRIVER(flags);
__m_free(&mp0, ptr, size, name);
NCR_UNLOCK_DRIVER(flags);
}
#ifndef	SCSI_NCR_DYNAMIC_DMA_MAPPING
#define __m_calloc_dma(b, s, n)		m_calloc(s, n)
#define __m_free_dma(b, p, s, n)	m_free(p, s, n)
#define __vtobus(b, p)			virt_to_bus(p)
#else
static m_addr_t ___dma_getp(m_pool_s *mp)
{
m_addr_t vp;
m_vtob_s *vbp;
vbp = __m_calloc(&mp0, sizeof(*vbp), "VTOB");
if (vbp) {
dma_addr_t daddr;
vp = (m_addr_t) pci_alloc_consistent(mp->bush,
PAGE_SIZE<<MEMO_PAGE_ORDER,
&daddr);
if (vp) {
int hc = VTOB_HASH_CODE(vp);
vbp->vaddr = vp;
vbp->baddr = daddr;
vbp->next = mp->vtob[hc];
mp->vtob[hc] = vbp;
++mp->nump;
return vp;
}
else
__m_free(&mp0, vbp, sizeof(*vbp), "VTOB");
}
return 0;
}
static void ___dma_freep(m_pool_s *mp, m_addr_t m)
{
m_vtob_s **vbpp, *vbp;
int hc = VTOB_HASH_CODE(m);
vbpp = &mp->vtob[hc];
while (*vbpp && (*vbpp)->vaddr != m)
vbpp = &(*vbpp)->next;
if (*vbpp) {
vbp = *vbpp;
*vbpp = (*vbpp)->next;
pci_free_consistent(mp->bush, PAGE_SIZE<<MEMO_PAGE_ORDER,
(void *)vbp->vaddr, (dma_addr_t)vbp->baddr);
__m_free(&mp0, vbp, sizeof(*vbp), "VTOB");
--mp->nump;
}
}
static inline m_pool_s *___get_dma_pool(m_bush_t bush)
{
m_pool_s *mp;
for (mp = mp0.next; mp && mp->bush != bush; mp = mp->next);
return mp;
}
static m_pool_s *___cre_dma_pool(m_bush_t bush)
{
m_pool_s *mp;
mp = __m_calloc(&mp0, sizeof(*mp), "MPOOL");
if (mp) {
bzero(mp, sizeof(*mp));
mp->bush = bush;
mp->getp = ___dma_getp;
mp->freep = ___dma_freep;
mp->next = mp0.next;
mp0.next = mp;
}
return mp;
}
static void ___del_dma_pool(m_pool_s *p)
{
struct m_pool **pp = &mp0.next;
while (*pp && *pp != p)
pp = &(*pp)->next;
if (*pp) {
*pp = (*pp)->next;
__m_free(&mp0, p, sizeof(*p), "MPOOL");
}
}
static void *__m_calloc_dma(m_bush_t bush, int size, char *name)
{
u_long flags;
struct m_pool *mp;
void *m = 0;
NCR_LOCK_DRIVER(flags);
mp = ___get_dma_pool(bush);
if (!mp)
mp = ___cre_dma_pool(bush);
if (mp)
m = __m_calloc(mp, size, name);
if (mp && !mp->nump)
___del_dma_pool(mp);
NCR_UNLOCK_DRIVER(flags);
return m;
}
static void __m_free_dma(m_bush_t bush, void *m, int size, char *name)
{
u_long flags;
struct m_pool *mp;
NCR_LOCK_DRIVER(flags);
mp = ___get_dma_pool(bush);
if (mp)
__m_free(mp, m, size, name);
if (mp && !mp->nump)
___del_dma_pool(mp);
NCR_UNLOCK_DRIVER(flags);
}
static m_addr_t __vtobus(m_bush_t bush, void *m)
{
u_long flags;
m_pool_s *mp;
int hc = VTOB_HASH_CODE(m);
m_vtob_s *vp = 0;
m_addr_t a = ((m_addr_t) m) & ~MEMO_CLUSTER_MASK;
NCR_LOCK_DRIVER(flags);
mp = ___get_dma_pool(bush);
if (mp) {
vp = mp->vtob[hc];
while (vp && (m_addr_t) vp->vaddr != a)
vp = vp->next;
}
NCR_UNLOCK_DRIVER(flags);
return vp ? vp->baddr + (((m_addr_t) m) - a) : 0;
}
#endif
#define _m_calloc_dma(np, s, n)		__m_calloc_dma(np->pdev, s, n)
#define _m_free_dma(np, p, s, n)	__m_free_dma(np->pdev, p, s, n)
#define m_calloc_dma(s, n)		_m_calloc_dma(np, s, n)
#define m_free_dma(p, s, n)		_m_free_dma(np, p, s, n)
#define _vtobus(np, p)			__vtobus(np->pdev, p)
#define vtobus(p)			_vtobus(np, p)
#ifndef SCSI_NCR_DYNAMIC_DMA_MAPPING
#define __unmap_scsi_data(pdev, cmd)	do {; } while (0)
#define __map_scsi_single_data(pdev, cmd) (__vtobus(pdev,(cmd)->request_buffer))
#define __map_scsi_sg_data(pdev, cmd)	((cmd)->use_sg)
#define __sync_scsi_data(pdev, cmd)	do {; } while (0)
#define scsi_sg_dma_address(sc)		vtobus((sc)->address)
#define scsi_sg_dma_len(sc)		((sc)->length)
#else
#define __data_mapped	SCp.phase
#define __data_mapping	SCp.have_data_in
static void __unmap_scsi_data(pcidev_t pdev, Scsi_Cmnd *cmd)
{
int dma_dir = scsi_to_pci_dma_dir(cmd->sc_data_direction);
switch(cmd->__data_mapped) {
case 2:
pci_unmap_sg(pdev, cmd->buffer, cmd->use_sg, dma_dir);
break;
case 1:
pci_unmap_single(pdev, cmd->__data_mapping,
cmd->request_bufflen, dma_dir);
break;
}
cmd->__data_mapped = 0;
}
static u_long __map_scsi_single_data(pcidev_t pdev, Scsi_Cmnd *cmd)
{
dma_addr_t mapping;
int dma_dir = scsi_to_pci_dma_dir(cmd->sc_data_direction);
if (cmd->request_bufflen == 0)
return 0;
mapping = pci_map_single(pdev, cmd->request_buffer,
cmd->request_bufflen, dma_dir);
cmd->__data_mapped = 1;
cmd->__data_mapping = mapping;
return mapping;
}
static int __map_scsi_sg_data(pcidev_t pdev, Scsi_Cmnd *cmd)
{
int use_sg;
int dma_dir = scsi_to_pci_dma_dir(cmd->sc_data_direction);
if (cmd->use_sg == 0)
return 0;
use_sg = pci_map_sg(pdev, cmd->buffer, cmd->use_sg, dma_dir);
cmd->__data_mapped = 2;
cmd->__data_mapping = use_sg;
return use_sg;
}
static void __sync_scsi_data(pcidev_t pdev, Scsi_Cmnd *cmd)
{
int dma_dir = scsi_to_pci_dma_dir(cmd->sc_data_direction);
switch(cmd->__data_mapped) {
case 2:
pci_dma_sync_sg(pdev, cmd->buffer, cmd->use_sg, dma_dir);
break;
case 1:
pci_dma_sync_single(pdev, cmd->__data_mapping,
cmd->request_bufflen, dma_dir);
break;
}
}
#define scsi_sg_dma_address(sc)		sg_dma_address(sc)
#define scsi_sg_dma_len(sc)		sg_dma_len(sc)
#endif
#define unmap_scsi_data(np, cmd)	__unmap_scsi_data(np->pdev, cmd)
#define map_scsi_single_data(np, cmd)	__map_scsi_single_data(np->pdev, cmd)
#define map_scsi_sg_data(np, cmd)	__map_scsi_sg_data(np->pdev, cmd)
#define sync_scsi_data(np, cmd)		__sync_scsi_data(np->pdev, cmd)
static void ncr_print_hex(u_char *p, int n)
{
while (n-- > 0)
printk (" %x", *p++);
}
static void ncr_printl_hex(char *label, u_char *p, int n)
{
printk("%s", label);
ncr_print_hex(p, n);
printk (".\n");
}
#ifdef	SCSI_DATA_UNKNOWN
#define scsi_data_direction(cmd)	(cmd->sc_data_direction)
#else
#define	SCSI_DATA_UNKNOWN	0
#define	SCSI_DATA_WRITE		1
#define	SCSI_DATA_READ		2
#define	SCSI_DATA_NONE		3
static __inline__ int scsi_data_direction(Scsi_Cmnd *cmd)
{
int direction;
switch((int) cmd->cmnd[0]) {
case 0x08:
case 0x28:
case 0xA8:
direction = SCSI_DATA_READ;
break;
case 0x0A:
case 0x2A:
case 0xAA:
direction = SCSI_DATA_WRITE;
break;
default:
direction = SCSI_DATA_UNKNOWN;
break;
}
return direction;
}
#endif
static struct Scsi_Host	*first_host = NULL;
#ifdef SCSI_NCR_PROC_INFO_SUPPORT
#if LINUX_VERSION_CODE < LinuxVersionCode(2,3,27)
static struct proc_dir_entry proc_scsi_sym53c8xx = {
PROC_SCSI_SYM53C8XX, 9, NAME53C8XX,
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#endif
static int sym53c8xx_proc_info(char *buffer, char **start, off_t offset,
int length, int hostno, int func);
#endif
static struct ncr_driver_setup
driver_setup			= SCSI_NCR_DRIVER_SETUP;
#ifdef	SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT
static struct ncr_driver_setup
driver_safe_setup __initdata	= SCSI_NCR_DRIVER_SAFE_SETUP;
# ifdef	MODULE
char *sym53c8xx = 0;
#  if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,30)
MODULE_PARM(sym53c8xx, "s");
#  endif
# endif
#endif
#define SetScsiResult(cmd, h_sts, s_sts) \
cmd->result = (((h_sts) << 16) + ((s_sts) & 0x7f))
#if 0
#define SetScsiAbortResult(cmd)	\
SetScsiResult(	\
cmd, 		\
(cmd)->abort_reason == DID_TIME_OUT ? DID_TIME_OUT : DID_ABORT, \
0xff)
#else
#define SetScsiAbortResult(cmd) SetScsiResult(cmd, DID_ABORT, 0xff)
#endif
static void sym53c8xx_select_queue_depths(
struct Scsi_Host *host, struct scsi_device *devlist);
static void sym53c8xx_intr(int irq, void *dev_id, struct pt_regs * regs);
static void sym53c8xx_timeout(unsigned long np);
#define initverbose (driver_setup.verbose)
#define bootverbose (np->verbose)
#ifdef SCSI_NCR_NVRAM_SUPPORT
static u_char Tekram_sync[16] __initdata =
{25,31,37,43, 50,62,75,125, 12,15,18,21, 6,7,9,10};
#endif
typedef struct {
int	bus;
u_char	device_fn;
u_long	base;
u_long	base_2;
u_long	io_port;
int	irq;
u_long	base_io;
volatile struct ncr_reg	*reg;
} ncr_slot;
typedef struct {
int type;
#define	SCSI_NCR_SYMBIOS_NVRAM	(1)
#define	SCSI_NCR_TEKRAM_NVRAM	(2)
#ifdef	SCSI_NCR_NVRAM_SUPPORT
union {
Symbios_nvram Symbios;
Tekram_nvram Tekram;
} data;
#endif
} ncr_nvram;
typedef struct {
pcidev_t  pdev;
ncr_slot  slot;
ncr_chip  chip;
ncr_nvram *nvram;
u_char host_id;
#ifdef	SCSI_NCR_PQS_PDS_SUPPORT
u_char pqs_pds;
#endif
int attach_done;
} ncr_device;
#define	assert(expression) { \
if (!(expression)) { \
(void)panic( \
"assertion \"%s\" failed: file \"%s\", line %d\n", \
#expression, \
__FILE__, __LINE__); \
} \
}
#define HS_IDLE		(0)
#define HS_BUSY		(1)
#define HS_NEGOTIATE	(2)
#define HS_DISCONNECT	(3)
#define HS_DONEMASK	(0x80)
#define HS_COMPLETE	(4|HS_DONEMASK)
#define HS_SEL_TIMEOUT	(5|HS_DONEMASK)
#define HS_RESET	(6|HS_DONEMASK)
#define HS_ABORTED	(7|HS_DONEMASK)
#define HS_TIMEOUT	(8|HS_DONEMASK)
#define HS_FAIL		(9|HS_DONEMASK)
#define HS_UNEXPECTED	(10|HS_DONEMASK)
#define DSA_INVALID 0xffffffff
#define	SIR_BAD_STATUS		(1)
#define	SIR_SEL_ATN_NO_MSG_OUT	(2)
#define	SIR_MSG_RECEIVED	(3)
#define	SIR_MSG_WEIRD		(4)
#define	SIR_NEGO_FAILED		(5)
#define	SIR_NEGO_PROTO		(6)
#define	SIR_SCRIPT_STOPPED	(7)
#define	SIR_REJECT_TO_SEND	(8)
#define	SIR_SWIDE_OVERRUN	(9)
#define	SIR_SODL_UNDERRUN	(10)
#define	SIR_RESEL_NO_MSG_IN	(11)
#define	SIR_RESEL_NO_IDENTIFY	(12)
#define	SIR_RESEL_BAD_LUN	(13)
#define	SIR_TARGET_SELECTED	(14)
#define	SIR_RESEL_BAD_I_T_L	(15)
#define	SIR_RESEL_BAD_I_T_L_Q	(16)
#define	SIR_ABORT_SENT		(17)
#define	SIR_RESEL_ABORTED	(18)
#define	SIR_MSG_OUT_DONE	(19)
#define	SIR_AUTO_SENSE_DONE	(20)
#define	SIR_DUMMY_INTERRUPT	(21)
#define	SIR_DATA_OVERRUN	(22)
#define	SIR_BAD_PHASE		(23)
#define	SIR_MAX			(23)
#define	XE_EXTRA_DATA	(1)
#define	XE_BAD_PHASE	(2)
#define	XE_PARITY_ERR	(4)
#define XE_SODL_UNRUN   (1<<3)
#define XE_SWIDE_OVRUN  (1<<4)
#define NS_NOCHANGE	(0)
#define NS_SYNC		(1)
#define NS_WIDE		(2)
#define NS_PPR		(4)
#define	QUIRK_AUTOSAVE	(0x01)
#define	INQ7_QUEUE	(0x02)
#define	INQ7_SYNC	(0x10)
#define	INQ7_WIDE16	(0x20)
#define CCB_HASH_SHIFT		8
#define CCB_HASH_SIZE		(1UL << CCB_HASH_SHIFT)
#define CCB_HASH_MASK		(CCB_HASH_SIZE-1)
#define CCB_HASH_CODE(dsa)	(((dsa) >> 11) & CCB_HASH_MASK)
struct tcb;
struct lcb;
struct ccb;
struct ncb;
struct script;
typedef struct ncb * ncb_p;
typedef struct tcb * tcb_p;
typedef struct lcb * lcb_p;
typedef struct ccb * ccb_p;
struct link {
ncrcmd	l_cmd;
ncrcmd	l_paddr;
};
struct	usrcmd {
u_long	target;
u_long	lun;
u_long	data;
u_long	cmd;
};
#define UC_SETSYNC      10
#define UC_SETTAGS	11
#define UC_SETDEBUG	12
#define UC_SETORDER	13
#define UC_SETWIDE	14
#define UC_SETFLAG	15
#define UC_SETVERBOSE	17
#define UC_RESETDEV	18
#define UC_CLEARDEV	19
#define	UF_TRACE	(0x01)
#define	UF_NODISC	(0x02)
#define	UF_NOSCAN	(0x04)
struct tcb {
u_int32		*luntbl;
u_int32		b_luntbl;
u_int32		b_lun0;
lcb_p		l0p;
#if MAX_LUN > 1
lcb_p		*lmp;
#endif
u_char		inq_done;
u_char		inq_byte7;
u_char		to_reset;
ccb_p   nego_cp;
u_char	minsync;
u_char	sval;
u_short	period;
u_char	maxoffs;
u_char	quirks;
u_char	widedone;
u_char	wval;
u_char	uval;
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
u_char ic_min_sync;
u_char ic_max_width;
u_char ic_done;
#endif
u_char ic_maximums_set;
u_char ppr_negotiation;
u_char	usrsync;
u_char	usrwide;
u_short	usrtags;
u_char	usrflag;
};
struct lcb {
u_int32		resel_task;
u_int32		tasktbl_0;
u_int32		*tasktbl;
u_int32		b_tasktbl;
XPT_QUEHEAD	busy_ccbq;
XPT_QUEHEAD	wait_ccbq;
u_short		busyccbs;
u_short		queuedccbs;
u_short		queuedepth;
u_short		scdev_depth;
u_short		maxnxs;
u_short		ia_tag;
u_short		if_tag;
u_char		*cb_tags;
u_char		inq_byte7;
u_char		usetags;
u_char		to_clear;
u_short		maxtags;
u_short		numtags;
u_short		num_good;
u_short		tags_sum[2];
u_char		tags_si;
u_long		tags_stime;
};
struct action {
u_int32		start;
u_int32		restart;
};
struct pm_ctx {
struct scr_tblmove sg;
u_int32	ret;
};
struct head {
struct action	go;
u_int32		savep;
u_int32		lastp;
u_int32		goalp;
u_int32		wlastp;
u_int32		wgoalp;
u_char		status[4];
};
#if MAX_LUN <= 1
#define ncr_lp(np, tp, lun) (!lun) ? (tp)->l0p : 0
#else
#define ncr_lp(np, tp, lun) \
(!lun) ? (tp)->l0p : (tp)->lmp ? (tp)->lmp[(lun)] : 0
#endif
#define  QU_REG	scr0
#define  HS_REG	scr1
#define  HS_PRT	nc_scr1
#define  SS_REG	scr2
#define  SS_PRT	nc_scr2
#define  HF_REG	scr3
#define  HF_PRT	nc_scr3
#define  actualquirks  phys.header.status[0]
#define  host_status   phys.header.status[1]
#define  scsi_status   phys.header.status[2]
#define  host_flags    phys.header.status[3]
#define HF_IN_PM0	1u
#define HF_IN_PM1	(1u<<1)
#define HF_ACT_PM	(1u<<2)
#define HF_DP_SAVED	(1u<<3)
#define HF_AUTO_SENSE	(1u<<4)
#define HF_DATA_IN	(1u<<5)
#define HF_PM_TO_C	(1u<<6)
#define HF_EXT_ERR	(1u<<7)
#ifdef SCSI_NCR_IARB_SUPPORT
#define HF_HINT_IARB	(1u<<7)
#endif
#define HF_DATA_ST	(1u<<7)
struct dsb {
struct head	header;
struct scr_tblsel  select;
struct scr_tblmove smsg  ;
struct scr_tblmove smsg_ext ;
struct scr_tblmove cmd   ;
struct scr_tblmove sense ;
struct scr_tblmove wresid;
struct scr_tblmove data [MAX_SCATTER];
struct pm_ctx pm0;
struct pm_ctx pm1;
};
struct ccb {
struct dsb	phys;
Scsi_Cmnd	*cmd;
u_char		cdb_buf[16];
u_char		sense_buf[64];
int		data_len;
int		segments;
u_char		scsi_smsg [12];
u_char		scsi_smsg2[12];
u_char		nego_status;
u_char		xerr_status;
u_int32		extra_bytes;
u_char		sv_scsi_status;
u_char		sv_xerr_status;
u_long		p_ccb;
u_char		sensecmd[6];
u_char		to_abort;
u_short		tag;
u_char		tags_si;
u_char		target;
u_char		lun;
u_short		queued;
ccb_p		link_ccb;
ccb_p		link_ccbh;
XPT_QUEHEAD	link_ccbq;
u_int32		startp;
u_int32		lastp0;
int		ext_sg;
int		ext_ofs;
int		resid;
};
#define CCB_PHYS(cp,lbl)	(cp->p_ccb + offsetof(struct ccb, lbl))
struct ncb {
struct action	idletask;
struct action	notask;
struct action	bad_i_t_l;
struct action	bad_i_t_l_q;
u_long		p_idletask;
u_long		p_notask;
u_long		p_bad_i_t_l;
u_long		p_bad_i_t_l_q;
u_int32		*badluntbl;
u_int32		resel_badlun;
u_int32		scr_ram_seg;
Scsi_Cmnd	*waiting_list;
Scsi_Cmnd	*done_list;
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,93)
spinlock_t	smp_lock;
#endif
int		unit;
char		chip_name[8];
char		inst_name[16];
u_char	sv_scntl0, sv_scntl3, sv_dmode, sv_dcntl, sv_ctest3, sv_ctest4,
sv_ctest5, sv_gpcntl, sv_stest2, sv_stest4, sv_stest1, sv_scntl4;
u_char	rv_scntl0, rv_scntl3, rv_dmode, rv_dcntl, rv_ctest3, rv_ctest4,
rv_ctest5, rv_stest2, rv_ccntl0, rv_ccntl1, rv_scntl4;
struct tcb	target[MAX_TARGET];
u_int32		*targtbl;
#ifndef SCSI_NCR_PCI_MEM_NOT_SUPPORTED
u_long		base_va;
u_long		base2_va;
#endif
u_long		base_ba;
u_long		base_io;
u_long		base_ws;
u_long		base2_ba;
u_long		base2_ws;
u_int		irq;
volatile
struct ncr_reg	*reg;
struct script	*script0;
struct scripth	*scripth0;
u_long		p_script;
u_long		p_scripth;
u_long		p_scripth0;
pcidev_t	pdev;
u_short		device_id;
u_char		revision_id;
u_char		bus;
u_char		device_fn;
u_char		myaddr;
u_char		maxburst;
u_char		maxwide;
u_char		minsync;
u_char		maxsync;
u_char		maxoffs;
u_char		multiplier;
u_char		clock_divn;
u_long		clock_khz;
u_int		features;
u_int		pciclock_min;
u_int		pciclock_max;
u_long		p_squeue;
u_int32		*squeue;
u_short		squeueput;
u_short		actccbs;
u_short		queuedepth;
u_short		dqueueget;
u_int32		*dqueue;
struct timer_list timer;
u_long		lasttime;
u_long		settle_time;
struct ncr_reg	regdump;
u_long		regtime;
u_char		msgout[12];
u_char		msgin [12];
u_int32		lastmsg;
u_char		scratch;
u_char		scsi_mode;
u_char		order;
u_char		verbose;
u_int32		ncr_cache;
u_long		p_ncb;
ccb_p ccbh[CCB_HASH_SIZE];
struct ccb	*ccbc;
XPT_QUEHEAD	free_ccbq;
#ifdef SCSI_NCR_IARB_SUPPORT
struct ccb	*last_cp;
u_short		iarb_max;
u_short		iarb_count;
#endif
XPT_QUEHEAD	b0_ccbq;
int (*scatter) (ncb_p, ccb_p, Scsi_Cmnd *);
u_char		abrt_msg[4];
struct scr_tblmove abrt_tbl;
struct scr_tblsel  abrt_sel;
u_char		istat_sem;
struct usrcmd	user;
volatile u_char	release_stage;
unsigned char  check_integrity;
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
unsigned char check_integ_par;
#endif
};
#define NCB_PHYS(np, lbl)	 (np->p_ncb + offsetof(struct ncb, lbl))
#define NCB_SCRIPT_PHYS(np,lbl)	 (np->p_script  + offsetof (struct script, lbl))
#define NCB_SCRIPTH_PHYS(np,lbl) (np->p_scripth + offsetof (struct scripth,lbl))
#define NCB_SCRIPTH0_PHYS(np,lbl) (np->p_scripth0+offsetof (struct scripth,lbl))
struct script {
ncrcmd	start		[ 14];
ncrcmd	getjob_begin	[  4];
ncrcmd	getjob_end	[  4];
ncrcmd	select		[  8];
ncrcmd	wf_sel_done	[  2];
ncrcmd	send_ident	[  2];
#ifdef SCSI_NCR_IARB_SUPPORT
ncrcmd	select2		[  8];
#else
ncrcmd	select2		[  2];
#endif
ncrcmd  command		[  2];
ncrcmd  dispatch	[ 28];
ncrcmd  sel_no_cmd	[ 10];
ncrcmd  init		[  6];
ncrcmd  clrack		[  4];
ncrcmd  disp_status	[  4];
ncrcmd  datai_done	[ 26];
ncrcmd  datao_done	[ 12];
ncrcmd  ign_i_w_r_msg	[  4];
ncrcmd  datai_phase	[  2];
ncrcmd  datao_phase	[  4];
ncrcmd  msg_in		[  2];
ncrcmd  msg_in2		[ 10];
#ifdef SCSI_NCR_IARB_SUPPORT
ncrcmd  status		[ 14];
#else
ncrcmd  status		[ 10];
#endif
ncrcmd  complete	[  8];
#ifdef SCSI_NCR_PCIQ_MAY_REORDER_WRITES
ncrcmd  complete2	[ 12];
#else
ncrcmd  complete2	[ 10];
#endif
#ifdef SCSI_NCR_PCIQ_SYNC_ON_INTR
ncrcmd	done		[ 18];
#else
ncrcmd	done		[ 14];
#endif
ncrcmd	done_end	[  2];
ncrcmd  save_dp		[  8];
ncrcmd  restore_dp	[  4];
ncrcmd  disconnect	[ 20];
#ifdef SCSI_NCR_IARB_SUPPORT
ncrcmd  idle		[  4];
#else
ncrcmd  idle		[  2];
#endif
#ifdef SCSI_NCR_IARB_SUPPORT
ncrcmd  ungetjob	[  6];
#else
ncrcmd  ungetjob	[  4];
#endif
ncrcmd	reselect	[  4];
ncrcmd	reselected	[ 20];
ncrcmd	resel_scntl4	[ 30];
#if   MAX_TASKS*4 > 512
ncrcmd	resel_tag	[ 18];
#elif MAX_TASKS*4 > 256
ncrcmd	resel_tag	[ 12];
#else
ncrcmd	resel_tag	[  8];
#endif
ncrcmd	resel_go	[  6];
ncrcmd	resel_notag	[  2];
ncrcmd	resel_dsa	[  8];
ncrcmd  data_in		[MAX_SCATTER * SCR_SG_SIZE];
ncrcmd  data_in2	[  4];
ncrcmd  data_out	[MAX_SCATTER * SCR_SG_SIZE];
ncrcmd  data_out2	[  4];
ncrcmd  pm0_data	[ 12];
ncrcmd  pm0_data_out	[  6];
ncrcmd  pm0_data_end	[  6];
ncrcmd  pm1_data	[ 12];
ncrcmd  pm1_data_out	[  6];
ncrcmd  pm1_data_end	[  6];
};
struct scripth {
ncrcmd	start64		[  2];
ncrcmd	no_data		[  2];
ncrcmd	sel_for_abort	[ 18];
ncrcmd	sel_for_abort_1	[  2];
ncrcmd	select_no_atn	[  8];
ncrcmd	wf_sel_done_no_atn [ 4];
ncrcmd	msg_in_etc	[ 14];
ncrcmd	msg_received	[  4];
ncrcmd	msg_weird_seen	[  4];
ncrcmd	msg_extended	[ 20];
ncrcmd  msg_bad		[  6];
ncrcmd	msg_weird	[  4];
ncrcmd	msg_weird1	[  8];
ncrcmd	wdtr_resp	[  6];
ncrcmd	send_wdtr	[  4];
ncrcmd	sdtr_resp	[  6];
ncrcmd	send_sdtr	[  4];
ncrcmd	ppr_resp	[  6];
ncrcmd	send_ppr	[  4];
ncrcmd	nego_bad_phase	[  4];
ncrcmd	msg_out		[  4];
ncrcmd	msg_out_done	[  4];
ncrcmd	data_ovrun	[  2];
ncrcmd	data_ovrun1	[ 22];
ncrcmd	data_ovrun2	[  8];
ncrcmd	abort_resel	[ 16];
ncrcmd	resend_ident	[  4];
ncrcmd	ident_break	[  4];
ncrcmd	ident_break_atn	[  4];
ncrcmd	sdata_in	[  6];
ncrcmd  data_io		[  2];
ncrcmd  data_io_com	[  8];
ncrcmd  data_io_out	[ 12];
ncrcmd	resel_bad_lun	[  4];
ncrcmd	bad_i_t_l	[  4];
ncrcmd	bad_i_t_l_q	[  4];
ncrcmd	bad_status	[  6];
ncrcmd	tweak_pmj	[ 12];
ncrcmd	pm_handle	[ 20];
ncrcmd	pm_handle1	[  4];
ncrcmd	pm_save		[  4];
ncrcmd	pm0_save	[ 14];
ncrcmd	pm1_save	[ 14];
#ifdef SYM_DEBUG_PM_WITH_WSR
ncrcmd  pm_wsr_handle	[ 44];
#else
ncrcmd  pm_wsr_handle	[ 42];
#endif
ncrcmd  wsr_ma_helper	[  4];
ncrcmd	zero		[  1];
ncrcmd	scratch		[  1];
ncrcmd	scratch1	[  1];
ncrcmd	pm0_data_addr	[  1];
ncrcmd	pm1_data_addr	[  1];
ncrcmd	saved_dsa	[  1];
ncrcmd	saved_drs	[  1];
ncrcmd	done_pos	[  1];
ncrcmd	startpos	[  1];
ncrcmd	targtbl		[  1];
#ifdef SCSI_NCR_PCI_MEM_NOT_SUPPORTED
ncrcmd	start_ram	[  1];
ncrcmd	script0_ba	[  4];
ncrcmd	start_ram64	[  3];
ncrcmd	script0_ba64	[  3];
ncrcmd	scripth0_ba64	[  6];
ncrcmd	ram_seg64	[  1];
#endif
ncrcmd	snooptest	[  6];
ncrcmd	snoopend	[  2];
};
static	ccb_p	ncr_alloc_ccb	(ncb_p np);
static	void	ncr_complete	(ncb_p np, ccb_p cp);
static	void	ncr_exception	(ncb_p np);
static	void	ncr_free_ccb	(ncb_p np, ccb_p cp);
static	ccb_p	ncr_ccb_from_dsa(ncb_p np, u_long dsa);
static	void	ncr_init_tcb	(ncb_p np, u_char tn);
static	lcb_p	ncr_alloc_lcb	(ncb_p np, u_char tn, u_char ln);
static	lcb_p	ncr_setup_lcb	(ncb_p np, u_char tn, u_char ln,
u_char *inq_data);
static	void	ncr_getclock	(ncb_p np, int mult);
static	u_int	ncr_getpciclock (ncb_p np);
static	void	ncr_selectclock	(ncb_p np, u_char scntl3);
static	ccb_p	ncr_get_ccb	(ncb_p np, u_char tn, u_char ln);
static	void	ncr_init	(ncb_p np, int reset, char * msg, u_long code);
static	void	ncr_int_sbmc	(ncb_p np);
static	void	ncr_int_par	(ncb_p np, u_short sist);
static	void	ncr_int_ma	(ncb_p np);
static	void	ncr_int_sir	(ncb_p np);
static  void    ncr_int_sto     (ncb_p np);
static  void    ncr_int_udc     (ncb_p np);
static	void	ncr_negotiate	(ncb_p np, tcb_p tp);
static	int	ncr_prepare_nego(ncb_p np, ccb_p cp, u_char *msgptr);
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
static	int	ncr_ic_nego(ncb_p np, ccb_p cp, Scsi_Cmnd *cmd, u_char *msgptr);
#endif
static	void	ncr_script_copy_and_bind
(ncb_p np, ncrcmd *src, ncrcmd *dst, int len);
static  void    ncr_script_fill (struct script * scr, struct scripth * scripth);
static	int	ncr_scatter_896R1 (ncb_p np, ccb_p cp, Scsi_Cmnd *cmd);
static	int	ncr_scatter	(ncb_p np, ccb_p cp, Scsi_Cmnd *cmd);
static	void	ncr_getsync	(ncb_p np, u_char sfac, u_char *fakp, u_char *scntl3p);
static  void    ncr_get_xfer_info(ncb_p np, tcb_p tp, u_char *factor, u_char *offset, u_char *width);
static	void	ncr_setsync	(ncb_p np, ccb_p cp, u_char scntl3, u_char sxfer, u_char scntl4);
static void 	ncr_set_sync_wide_status (ncb_p np, u_char target);
static	void	ncr_setup_tags	(ncb_p np, u_char tn, u_char ln);
static	void	ncr_setwide	(ncb_p np, ccb_p cp, u_char wide, u_char ack);
static	void	ncr_setsyncwide	(ncb_p np, ccb_p cp, u_char scntl3, u_char sxfer, u_char scntl4, u_char wide);
static	int	ncr_show_msg	(u_char * msg);
static	void	ncr_print_msg	(ccb_p cp, char *label, u_char * msg);
static	int	ncr_snooptest	(ncb_p np);
static	void	ncr_timeout	(ncb_p np);
static  void    ncr_wakeup      (ncb_p np, u_long code);
static  int     ncr_wakeup_done (ncb_p np);
static	void	ncr_start_next_ccb (ncb_p np, lcb_p lp, int maxn);
static	void	ncr_put_start_queue(ncb_p np, ccb_p cp);
static	void	ncr_chip_reset	(ncb_p np);
static	void	ncr_soft_reset	(ncb_p np);
static	void	ncr_start_reset	(ncb_p np);
static	int	ncr_reset_scsi_bus (ncb_p np, int enab_int, int settle_delay);
static	int	ncr_compute_residual (ncb_p np, ccb_p cp);
#ifdef SCSI_NCR_USER_COMMAND_SUPPORT
static	void	ncr_usercmd	(ncb_p np);
#endif
static int ncr_attach (Scsi_Host_Template *tpnt, int unit, ncr_device *device);
static void ncr_free_resources(ncb_p np);
static void insert_into_waiting_list(ncb_p np, Scsi_Cmnd *cmd);
static Scsi_Cmnd *retrieve_from_waiting_list(int to_remove, ncb_p np, Scsi_Cmnd *cmd);
static void process_waiting_list(ncb_p np, int sts);
#define remove_from_waiting_list(np, cmd) \
retrieve_from_waiting_list(1, (np), (cmd))
#define requeue_waiting_list(np) process_waiting_list((np), DID_OK)
#define reset_waiting_list(np) process_waiting_list((np), DID_RESET)
#ifdef SCSI_NCR_NVRAM_SUPPORT
static  void	ncr_get_nvram	       (ncr_device *devp, ncr_nvram *nvp);
static  int	sym_read_Tekram_nvram  (ncr_slot *np, u_short device_id,
Tekram_nvram *nvram);
static  int	sym_read_Symbios_nvram (ncr_slot *np, Symbios_nvram *nvram);
#endif
static inline char *ncr_name (ncb_p np)
{
return np->inst_name;
}
#define	RELOC_SOFTC	0x40000000
#define	RELOC_LABEL	0x50000000
#define	RELOC_REGISTER	0x60000000
#if 0
#define	RELOC_KVAR	0x70000000
#endif
#define	RELOC_LABELH	0x80000000
#define	RELOC_MASK	0xf0000000
#define	NADDR(label)	(RELOC_SOFTC | offsetof(struct ncb, label))
#define PADDR(label)    (RELOC_LABEL | offsetof(struct script, label))
#define PADDRH(label)   (RELOC_LABELH | offsetof(struct scripth, label))
#define	RADDR(label)	(RELOC_REGISTER | REG(label))
#define	FADDR(label,ofs)(RELOC_REGISTER | ((REG(label))+(ofs)))
#define	KVAR(which)	(RELOC_KVAR | (which))
#define SCR_DATA_ZERO	0xf00ff00f
#ifdef	RELOC_KVAR
#define	SCRIPT_KVAR_JIFFIES	(0)
#define	SCRIPT_KVAR_FIRST	SCRIPT_KVAR_JIFFIES
#define	SCRIPT_KVAR_LAST	SCRIPT_KVAR_JIFFIES
static void *script_kvars[] __initdata =
{ (void *)&jiffies };
#endif
static	struct script script0 __initdata = {
{
SCR_NO_OP,
0,
SCR_FROM_REG (ctest2),
0,
SCR_FROM_REG (istat),
0,
SCR_LOAD_ABS (scratcha, 4),
PADDRH (startpos),
SCR_INT ^ IFTRUE (MASK (SEM, SEM)),
SIR_SCRIPT_STOPPED,
SCR_LOAD_ABS (dsa, 4),
PADDRH (startpos),
SCR_LOAD_REL (temp, 4),
4,
},{
SCR_STORE_ABS (temp, 4),
PADDRH (startpos),
SCR_LOAD_REL (dsa, 4),
0,
},{
SCR_LOAD_REL (temp, 4),
0,
SCR_RETURN,
0,
},{
SCR_CLR (SCR_TRG),
0,
SCR_SEL_TBL_ATN ^ offsetof (struct dsb, select),
PADDR (ungetjob),
SCR_LOAD_REL (temp, 4),
offsetof (struct ccb, phys.header.savep),
SCR_LOAD_REL (scr0, 4),
offsetof (struct ccb, phys.header.status),
},{
SCR_INT ^ IFFALSE (WHEN (SCR_MSG_OUT)),
SIR_SEL_ATN_NO_MSG_OUT,
},{
SCR_MOVE_TBL ^ SCR_MSG_OUT,
offsetof (struct dsb, smsg),
},{
#ifdef SCSI_NCR_IARB_SUPPORT
SCR_FROM_REG (HF_REG),
0,
SCR_JUMPR ^ IFFALSE (MASK (HF_HINT_IARB, HF_HINT_IARB)),
8,
SCR_REG_REG (scntl1, SCR_OR, IARB),
0,
#endif
SCR_JUMP ^ IFFALSE (WHEN (SCR_COMMAND)),
PADDR (sel_no_cmd),
},{
SCR_MOVE_TBL ^ SCR_COMMAND,
offsetof (struct dsb, cmd),
},{
SCR_JUMP ^ IFTRUE (WHEN (SCR_MSG_IN)),
PADDR (msg_in),
SCR_JUMP ^ IFTRUE (IF (SCR_DATA_OUT)),
PADDR (datao_phase),
SCR_JUMP ^ IFTRUE (IF (SCR_DATA_IN)),
PADDR (datai_phase),
SCR_JUMP ^ IFTRUE (IF (SCR_STATUS)),
PADDR (status),
SCR_JUMP ^ IFTRUE (IF (SCR_COMMAND)),
PADDR (command),
SCR_JUMP ^ IFTRUE (IF (SCR_MSG_OUT)),
PADDRH (msg_out),
SCR_JUMPR ^ IFFALSE (WHEN (SCR_ILG_OUT)),
16,
SCR_MOVE_ABS (1) ^ SCR_ILG_OUT,
NADDR (scratch),
SCR_JUMPR ^ IFTRUE (WHEN (SCR_ILG_OUT)),
-16,
SCR_JUMPR ^ IFFALSE (WHEN (SCR_ILG_IN)),
16,
SCR_MOVE_ABS (1) ^ SCR_ILG_IN,
NADDR (scratch),
SCR_JUMPR ^ IFTRUE (WHEN (SCR_ILG_IN)),
-16,
SCR_INT,
SIR_BAD_PHASE,
SCR_JUMP,
PADDR (dispatch),
},{
SCR_JUMP ^ IFTRUE (WHEN (SCR_MSG_OUT)),
PADDRH (resend_ident),
SCR_JUMP ^ IFTRUE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_FROM_REG (HS_REG),
0,
SCR_INT ^ IFTRUE (DATA (HS_NEGOTIATE)),
SIR_NEGO_FAILED,
SCR_JUMP,
PADDR (dispatch),
},{
SCR_FROM_REG (sstat0),
0,
SCR_JUMPR ^ IFTRUE (MASK (IRST, IRST)),
-16,
SCR_JUMP,
PADDR (start),
},{
SCR_CLR (SCR_ACK),
0,
SCR_JUMP,
PADDR (dispatch),
},{
SCR_JUMP ^ IFTRUE (WHEN (SCR_STATUS)),
PADDR (status),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_JUMP ^ IFTRUE (WHEN (SCR_DATA_IN)),
PADDRH (data_ovrun),
SCR_FROM_REG (scntl2),
0,
SCR_JUMP ^ IFFALSE (MASK (WSR, WSR)),
PADDR (disp_status),
SCR_REG_REG (scntl2, SCR_OR, WSR),
0,
SCR_INT ^ IFFALSE (WHEN (SCR_MSG_IN)),
SIR_SWIDE_OVERRUN,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (disp_status),
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[0]),
SCR_INT ^ IFFALSE (DATA (M_IGN_RESIDUE)),
SIR_SWIDE_OVERRUN,
SCR_JUMP ^ IFFALSE (DATA (M_IGN_RESIDUE)),
PADDR (msg_in2),
SCR_CLR (SCR_ACK),
0,
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[1]),
SCR_CLR (SCR_ACK),
0,
SCR_JUMP,
PADDR (disp_status),
},{
SCR_JUMP ^ IFTRUE (WHEN (SCR_DATA_OUT)),
PADDRH (data_ovrun),
SCR_FROM_REG (scntl2),
0,
SCR_JUMP ^ IFFALSE (MASK (WSS, WSS)),
PADDR (disp_status),
SCR_REG_REG (scntl2, SCR_OR, WSS),
0,
SCR_INT,
SIR_SODL_UNDERRUN,
SCR_JUMP,
PADDR (dispatch),
},{
SCR_MOVE_ABS (2) ^ SCR_MSG_IN,
NADDR (scratch),
SCR_JUMP,
PADDR (clrack),
},{
SCR_RETURN,
0,
},{
SCR_NO_OP,
0,
SCR_RETURN,
0,
},{
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[0]),
},{
SCR_JUMP ^ IFTRUE (DATA (M_COMPLETE)),
PADDR (complete),
SCR_JUMP ^ IFTRUE (DATA (M_DISCONNECT)),
PADDR (disconnect),
SCR_JUMP ^ IFTRUE (DATA (M_SAVE_DP)),
PADDR (save_dp),
SCR_JUMP ^ IFTRUE (DATA (M_RESTORE_DP)),
PADDR (restore_dp),
SCR_JUMP,
PADDRH (msg_in_etc),
},{
SCR_MOVE_ABS (1) ^ SCR_STATUS,
NADDR (scratch),
#ifdef SCSI_NCR_IARB_SUPPORT
SCR_JUMPR ^ IFTRUE (DATA (S_GOOD)),
8,
SCR_REG_REG (scntl1, SCR_AND, ~IARB),
0,
#endif
SCR_TO_REG (SS_REG),
0,
SCR_LOAD_REG (HS_REG, HS_COMPLETE),
0,
SCR_JUMP ^ IFTRUE (WHEN (SCR_MSG_IN)),
PADDR (msg_in),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_STORE_REL (temp, 4),
offsetof (struct ccb, phys.header.lastp),
SCR_REG_REG (scntl2, SCR_AND, 0x7f),
0,
SCR_CLR (SCR_ACK|SCR_ATN),
0,
SCR_WAIT_DISC,
0,
},{
SCR_STORE_REL (scr0, 4),
offsetof (struct ccb, phys.header.status),
#ifdef SCSI_NCR_PCIQ_MAY_REORDER_WRITES
SCR_LOAD_REL (scr0, 4),
offsetof (struct ccb, phys.header.status),
#endif
SCR_FROM_REG (SS_REG),
0,
SCR_CALL ^ IFFALSE (DATA (S_GOOD)),
PADDRH (bad_status),
SCR_FROM_REG (HF_REG),
0,
SCR_INT ^ IFTRUE (MASK (HF_AUTO_SENSE, HF_AUTO_SENSE)),
SIR_AUTO_SENSE_DONE,
},{
#ifdef SCSI_NCR_PCIQ_SYNC_ON_INTR
SCR_FROM_REG (istat),
0,
SCR_INT ^ IFTRUE (MASK (INTF, INTF)),
SIR_DUMMY_INTERRUPT,
#endif
SCR_STORE_ABS (dsa, 4),
PADDRH (saved_dsa),
SCR_LOAD_ABS (dsa, 4),
PADDRH (done_pos),
SCR_LOAD_ABS (scratcha, 4),
PADDRH (saved_dsa),
SCR_STORE_REL (scratcha, 4),
0,
SCR_LOAD_REL (temp, 4),
4,
SCR_INT_FLY,
0,
SCR_STORE_ABS (temp, 4),
PADDRH (done_pos),
},{
SCR_JUMP,
PADDR (start),
},{
SCR_CLR (SCR_ACK),
0,
SCR_REG_REG (HF_REG, SCR_OR, HF_DP_SAVED),
0,
SCR_STORE_REL (temp, 4),
offsetof (struct ccb, phys.header.savep),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_LOAD_REL  (temp, 4),
offsetof (struct ccb, phys.header.savep),
SCR_JUMP,
PADDR (clrack),
},{
SCR_REG_REG (scntl2, SCR_AND, 0x7f),
0,
SCR_CLR (SCR_ACK|SCR_ATN),
0,
SCR_WAIT_DISC,
0,
SCR_LOAD_REG (HS_REG, HS_DISCONNECT),
0,
SCR_STORE_REL (scr0, 4),
offsetof (struct ccb, phys.header.status),
SCR_FROM_REG (QU_REG),
0,
SCR_JUMP ^ IFFALSE (MASK (QUIRK_AUTOSAVE, QUIRK_AUTOSAVE)),
PADDR (start),
SCR_REG_REG (HF_REG, SCR_OR, HF_DP_SAVED),
0,
SCR_STORE_REL (temp, 4),
offsetof (struct ccb, phys.header.savep),
SCR_JUMP,
PADDR (start),
},{
SCR_NO_OP,
0,
#ifdef SCSI_NCR_IARB_SUPPORT
SCR_JUMPR,
8,
#endif
},{
#ifdef SCSI_NCR_IARB_SUPPORT
SCR_REG_REG (scntl1, SCR_OR, IARB),
0,
#endif
SCR_LOAD_REG (dsa, 0xff),
0,
SCR_STORE_ABS (scratcha, 4),
PADDRH (startpos),
},{
SCR_CLR (SCR_TRG),
0,
SCR_WAIT_RESEL,
PADDR(start),
},{
SCR_NO_OP,
0,
SCR_REG_SFBR (ssid, SCR_AND, 0x8F),
0,
SCR_TO_REG (sdid),
0,
SCR_LOAD_ABS (dsa, 4),
PADDRH (targtbl),
SCR_SFBR_REG (dsa, SCR_SHL, 0),
0,
SCR_REG_REG (dsa, SCR_SHL, 0),
0,
SCR_REG_REG (dsa, SCR_AND, 0x3c),
0,
SCR_LOAD_REL (dsa, 4),
0,
SCR_LOAD_REL (scntl3, 1),
offsetof(struct tcb, wval),
SCR_LOAD_REL (sxfer, 1),
offsetof(struct tcb, sval),
},{
SCR_NO_OP,
0,
SCR_INT ^ IFFALSE (WHEN (SCR_MSG_IN)),
SIR_RESEL_NO_MSG_IN,
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin),
SCR_JUMPR ^ IFTRUE (MASK (0x80, 0xbf)),
56,
SCR_INT ^ IFFALSE (MASK (0x80, 0x80)),
SIR_RESEL_NO_IDENTIFY,
SCR_LOAD_REL (dsa, 4),
offsetof(struct tcb, b_luntbl),
SCR_SFBR_REG (dsa, SCR_SHL, 0),
0,
SCR_REG_REG (dsa, SCR_SHL, 0),
0,
SCR_REG_REG (dsa, SCR_AND, 0xfc),
0,
SCR_LOAD_REL (dsa, 4),
0,
SCR_JUMPR,
8,
SCR_LOAD_REL (dsa, 4),
offsetof(struct tcb, b_lun0),
SCR_LOAD_REL (temp, 4),
offsetof(struct lcb, resel_task),
SCR_LOAD_REL (dsa, 4),
offsetof(struct lcb, b_tasktbl),
SCR_RETURN,
0,
},{
SCR_CLR (SCR_ACK),
0,
SCR_MOVE_ABS (2) ^ SCR_MSG_IN,
NADDR (msgin),
SCR_REG_SFBR (sidl, SCR_SHL, 0),
0,
#if MAX_TASKS*4 > 512
SCR_JUMPR ^ IFFALSE (CARRYSET),
8,
SCR_REG_REG (dsa1, SCR_OR, 2),
0,
SCR_REG_REG (sfbr, SCR_SHL, 0),
0,
SCR_JUMPR ^ IFFALSE (CARRYSET),
8,
SCR_REG_REG (dsa1, SCR_OR, 1),
0,
#elif MAX_TASKS*4 > 256
SCR_JUMPR ^ IFFALSE (CARRYSET),
8,
SCR_REG_REG (dsa1, SCR_OR, 1),
0,
#endif
SCR_SFBR_REG (dsa, SCR_AND, 0xfc),
0,
},{
SCR_LOAD_REL (dsa, 4),
0,
SCR_LOAD_REL (temp, 4),
offsetof(struct ccb, phys.header.go.restart),
SCR_RETURN,
0,
},{
SCR_JUMP,
PADDR (resel_go),
},{
SCR_CLR (SCR_ACK),
0,
SCR_LOAD_REL (temp, 4),
offsetof (struct ccb, phys.header.savep),
SCR_LOAD_REL (scr0, 4),
offsetof (struct ccb, phys.header.status),
SCR_JUMP,
PADDR (dispatch),
},{
0
},{
SCR_CALL,
PADDR (datai_done),
SCR_JUMP,
PADDRH (data_ovrun),
},{
0
},{
SCR_CALL,
PADDR (datao_done),
SCR_JUMP,
PADDRH (data_ovrun),
},{
SCR_FROM_REG (HF_REG),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_DATA_IN)),
PADDR (pm0_data_out),
SCR_JUMP ^ IFFALSE (MASK (HF_DATA_IN, HF_DATA_IN)),
PADDRH (data_ovrun),
SCR_REG_REG (HF_REG, SCR_OR, HF_IN_PM0),
0,
SCR_CHMOV_TBL ^ SCR_DATA_IN,
offsetof (struct ccb, phys.pm0.sg),
SCR_JUMP,
PADDR (pm0_data_end),
},{
SCR_JUMP ^ IFTRUE (MASK (HF_DATA_IN, HF_DATA_IN)),
PADDRH (data_ovrun),
SCR_REG_REG (HF_REG, SCR_OR, HF_IN_PM0),
0,
SCR_CHMOV_TBL ^ SCR_DATA_OUT,
offsetof (struct ccb, phys.pm0.sg),
},{
SCR_REG_REG (HF_REG, SCR_AND, (~HF_IN_PM0)),
0,
SCR_LOAD_REL (temp, 4),
offsetof (struct ccb, phys.pm0.ret),
SCR_RETURN,
0,
},{
SCR_FROM_REG (HF_REG),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_DATA_IN)),
PADDR (pm1_data_out),
SCR_JUMP ^ IFFALSE (MASK (HF_DATA_IN, HF_DATA_IN)),
PADDRH (data_ovrun),
SCR_REG_REG (HF_REG, SCR_OR, HF_IN_PM1),
0,
SCR_CHMOV_TBL ^ SCR_DATA_IN,
offsetof (struct ccb, phys.pm1.sg),
SCR_JUMP,
PADDR (pm1_data_end),
},{
SCR_JUMP ^ IFTRUE (MASK (HF_DATA_IN, HF_DATA_IN)),
PADDRH (data_ovrun),
SCR_REG_REG (HF_REG, SCR_OR, HF_IN_PM1),
0,
SCR_CHMOV_TBL ^ SCR_DATA_OUT,
offsetof (struct ccb, phys.pm1.sg),
},{
SCR_REG_REG (HF_REG, SCR_AND, (~HF_IN_PM1)),
0,
SCR_LOAD_REL (temp, 4),
offsetof (struct ccb, phys.pm1.ret),
SCR_RETURN,
0,
}
};
static	struct scripth scripth0 __initdata = {
{
SCR_JUMP,
PADDR (init),
},{
SCR_JUMP,
PADDRH (data_ovrun),
},{
SCR_CLR (SCR_TRG),
0,
SCR_SEL_TBL_ATN ^ offsetof (struct ncb, abrt_sel),
PADDR (reselect),
SCR_JUMPR ^ IFFALSE (WHEN (SCR_MSG_OUT)),
-8,
SCR_INT,
SIR_TARGET_SELECTED,
SCR_REG_REG (scntl2, SCR_AND, 0x7f),
0,
SCR_MOVE_TBL ^ SCR_MSG_OUT,
offsetof (struct ncb, abrt_tbl),
SCR_CLR (SCR_ACK|SCR_ATN),
0,
SCR_WAIT_DISC,
0,
SCR_INT,
SIR_ABORT_SENT,
},{
SCR_JUMP,
PADDR (start),
},{
SCR_CLR (SCR_TRG),
0,
SCR_SEL_TBL ^ offsetof (struct dsb, select),
PADDR (ungetjob),
SCR_LOAD_REL (temp, 4),
offsetof (struct ccb, phys.header.savep),
SCR_LOAD_REL (scr0, 4),
offsetof (struct ccb, phys.header.status),
},{
SCR_JUMPR ^ IFFALSE (WHEN (SCR_MSG_OUT)),
0,
SCR_JUMP,
PADDR (select2),
},{
SCR_JUMP ^ IFTRUE (DATA (M_EXTENDED)),
PADDRH (msg_extended),
SCR_JUMP ^ IFTRUE (MASK (0x00, 0xf0)),
PADDRH (msg_received),
SCR_JUMP ^ IFTRUE (MASK (0x10, 0xf0)),
PADDRH (msg_received),
SCR_JUMP ^ IFFALSE (MASK (0x20, 0xf0)),
PADDRH (msg_weird_seen),
SCR_CLR (SCR_ACK),
0,
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[1]),
SCR_JUMP,
PADDRH (msg_received),
},{
SCR_LOAD_REL (scratcha, 4),
0,
SCR_INT,
SIR_MSG_RECEIVED,
},{
SCR_LOAD_REL (scratcha1, 4),
0,
SCR_INT,
SIR_MSG_WEIRD,
},{
SCR_CLR (SCR_ACK),
0,
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (msgin[1]),
SCR_JUMP ^ IFTRUE (DATA (0)),
PADDRH (msg_weird_seen),
SCR_TO_REG (scratcha),
0,
SCR_REG_REG (sfbr, SCR_ADD, (256-8)),
0,
SCR_JUMP ^ IFTRUE (CARRYSET),
PADDRH (msg_weird_seen),
SCR_STORE_REL (scratcha, 1),
offsetof (struct dsb, smsg_ext.size),
SCR_CLR (SCR_ACK),
0,
SCR_MOVE_TBL ^ SCR_MSG_IN,
offsetof (struct dsb, smsg_ext),
SCR_JUMP,
PADDRH (msg_received),
},{
SCR_INT,
SIR_REJECT_TO_SEND,
SCR_SET (SCR_ATN),
0,
SCR_JUMP,
PADDR (clrack),
},{
SCR_INT,
SIR_REJECT_TO_SEND,
SCR_SET (SCR_ATN),
0,
},{
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_IN)),
PADDR (dispatch),
SCR_MOVE_ABS (1) ^ SCR_MSG_IN,
NADDR (scratch),
SCR_JUMP,
PADDRH (msg_weird1),
},{
SCR_SET (SCR_ATN),
0,
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_OUT)),
PADDRH (nego_bad_phase),
},{
SCR_MOVE_ABS (4) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_JUMP,
PADDRH (msg_out_done),
},{
SCR_SET (SCR_ATN),
0,
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_OUT)),
PADDRH (nego_bad_phase),
},{
SCR_MOVE_ABS (5) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_JUMP,
PADDRH (msg_out_done),
},{
SCR_SET (SCR_ATN),
0,
SCR_CLR (SCR_ACK),
0,
SCR_JUMP ^ IFFALSE (WHEN (SCR_MSG_OUT)),
PADDRH (nego_bad_phase),
},{
SCR_MOVE_ABS (8) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_JUMP,
PADDRH (msg_out_done),
},{
SCR_INT,
SIR_NEGO_PROTO,
SCR_JUMP,
PADDR (dispatch),
},{
SCR_MOVE_ABS (1) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_JUMP ^ IFTRUE (WHEN (SCR_MSG_OUT)),
PADDRH (msg_out),
},{
SCR_INT,
SIR_MSG_OUT_DONE,
SCR_JUMP,
PADDR (dispatch),
},{
SCR_LOAD_ABS (scratcha, 4),
PADDRH (zero),
},{
SCR_JUMPR ^ IFFALSE (WHEN (SCR_DATA_OUT)),
16,
SCR_CHMOV_ABS (1) ^ SCR_DATA_OUT,
NADDR (scratch),
SCR_JUMP,
PADDRH (data_ovrun2),
SCR_FROM_REG (scntl2),
0,
SCR_JUMPR ^ IFFALSE (MASK (WSR, WSR)),
16,
SCR_REG_REG (scntl2, SCR_OR, WSR),
0,
SCR_JUMP,
PADDRH (data_ovrun2),
SCR_JUMPR ^ IFTRUE (WHEN (SCR_DATA_IN)),
16,
SCR_INT,
SIR_DATA_OVERRUN,
SCR_JUMP,
PADDR (dispatch),
SCR_CHMOV_ABS (1) ^ SCR_DATA_IN,
NADDR (scratch),
},{
SCR_REG_REG (scratcha,  SCR_ADD,  0x01),
0,
SCR_REG_REG (scratcha1, SCR_ADDC, 0),
0,
SCR_REG_REG (scratcha2, SCR_ADDC, 0),
0,
SCR_JUMP,
PADDRH (data_ovrun1),
},{
SCR_SET (SCR_ATN),
0,
SCR_CLR (SCR_ACK),
0,
SCR_REG_REG (scntl2, SCR_AND, 0x7f),
0,
SCR_MOVE_ABS (1) ^ SCR_MSG_OUT,
NADDR (msgout),
SCR_CLR (SCR_ACK|SCR_ATN),
0,
SCR_WAIT_DISC,
0,
SCR_INT,
SIR_RESEL_ABORTED,
SCR_JUMP,
PADDR (start),
},{
SCR_SET (SCR_ATN),
0,
SCR_JUMP,
PADDR (send_ident),
},{
SCR_CLR (SCR_ATN),
0,
SCR_JUMP,
PADDR (select2),
},{
SCR_SET (SCR_ATN),
0,
SCR_JUMP,
PADDR (select2),
},{
SCR_CHMOV_TBL ^ SCR_DATA_IN,
offsetof (struct dsb, sense),
SCR_CALL,
PADDR (datai_done),
SCR_JUMP,
PADDRH (data_ovrun),
},{
SCR_JUMP ^ IFTRUE (WHEN (SCR_DATA_OUT)),
PADDRH(data_io_out),
},{
SCR_LOAD_REL  (scratcha, 4),
offsetof (struct ccb, phys.header.lastp),
SCR_STORE_REL (scratcha, 4),
offsetof (struct ccb, phys.header.savep),
SCR_LOAD_REL  (temp, 4),
offsetof (struct ccb, phys.header.savep),
SCR_RETURN,
0,
},{
SCR_REG_REG (HF_REG, SCR_AND, (~HF_DATA_IN)),
0,
SCR_LOAD_REL  (scratcha, 4),
offsetof (struct ccb, phys.header.wlastp),
SCR_STORE_REL (scratcha, 4),
offsetof (struct ccb, phys.header.lastp),
SCR_LOAD_REL  (scratcha, 4),
offsetof (struct ccb, phys.header.wgoalp),
SCR_STORE_REL (scratcha, 4),
offsetof (struct ccb, phys.header.goalp),
SCR_JUMP,
PADDRH(data_io_com),
},{
SCR_INT,
SIR_RESEL_BAD_LUN,
SCR_JUMP,
PADDRH (abort_resel),
},{
SCR_INT,
SIR_RESEL_BAD_I_T_L,
SCR_JUMP,
PADDRH (abort_resel),
},{
SCR_INT,
SIR_RESEL_BAD_I_T_L_Q,
SCR_JUMP,
PADDRH (abort_resel),
},{
SCR_LOAD_ABS (scratcha, 4),
PADDRH (startpos),
SCR_INT ^ IFFALSE (DATA (S_COND_MET)),
SIR_BAD_STATUS,
SCR_RETURN,
0,
},{
SCR_FROM_REG(HF_REG),
0,
SCR_JUMPR ^ IFTRUE (MASK (HF_PM_TO_C, HF_PM_TO_C)),
16,
SCR_REG_REG (ccntl0, SCR_OR, ENPMJ),
0,
SCR_RETURN,
0,
SCR_REG_REG (ccntl0, SCR_AND, (~ENPMJ)),
0,
SCR_RETURN,
0,
},{
SCR_FROM_REG (HF_REG),
0,
SCR_JUMP ^ IFTRUE (MASK (0, (HF_IN_PM0 | HF_IN_PM1 | HF_DP_SAVED))),
PADDRH (pm_handle1),
SCR_JUMPR ^ IFFALSE (MASK (HF_DP_SAVED, HF_DP_SAVED)),
8,
SCR_REG_REG (sfbr, SCR_XOR, HF_ACT_PM),
0,
SCR_JUMP ^ IFTRUE (MASK (0, (HF_IN_PM0 | HF_IN_PM1))),
PADDRH (pm_handle1),
SCR_JUMPR ^ IFFALSE (MASK (HF_IN_PM0, HF_IN_PM0)),
16,
SCR_LOAD_REL (ia, 4),
offsetof(struct ccb, phys.pm0.ret),
SCR_JUMP,
PADDRH (pm_save),
SCR_LOAD_REL (ia, 4),
offsetof(struct ccb, phys.pm1.ret),
SCR_JUMP,
PADDRH (pm_save),
},{
SCR_REG_REG (ia, SCR_ADD, 8),
0,
SCR_REG_REG (ia1, SCR_ADDC, 0),
0,
},{
SCR_SFBR_REG (HF_REG, SCR_AND, (~(HF_IN_PM0|HF_IN_PM1|HF_DP_SAVED))),
0,
SCR_JUMP ^ IFTRUE (MASK (HF_ACT_PM, HF_ACT_PM)),
PADDRH (pm1_save),
},{
SCR_STORE_REL (ia, 4),
offsetof(struct ccb, phys.pm0.ret),
SCR_FROM_REG (scntl2),
0,
SCR_CALL ^ IFTRUE (MASK (WSR, WSR)),
PADDRH (pm_wsr_handle),
SCR_STORE_REL (rbc, 4),
offsetof(struct ccb, phys.pm0.sg.size),
SCR_STORE_REL (ua, 4),
offsetof(struct ccb, phys.pm0.sg.addr),
SCR_LOAD_ABS (temp, 4),
PADDRH (pm0_data_addr),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_STORE_REL (ia, 4),
offsetof(struct ccb, phys.pm1.ret),
SCR_FROM_REG (scntl2),
0,
SCR_CALL ^ IFTRUE (MASK (WSR, WSR)),
PADDRH (pm_wsr_handle),
SCR_STORE_REL (rbc, 4),
offsetof(struct ccb, phys.pm1.sg.size),
SCR_STORE_REL (ua, 4),
offsetof(struct ccb, phys.pm1.sg.addr),
SCR_LOAD_ABS (temp, 4),
PADDRH (pm1_data_addr),
SCR_JUMP,
PADDR (dispatch),
},{
#ifdef	SYM_DEBUG_PM_WITH_WSR
SCR_INT,
SIR_PM_WITH_WSR,
#endif
SCR_STORE_REL (ua, 4),
offsetof (struct ccb, phys.wresid.addr),
SCR_REG_REG (ua, SCR_ADD, 1),
0,
SCR_REG_REG (ua1, SCR_ADDC, 0),
0,
SCR_REG_REG (ua2, SCR_ADDC, 0),
0,
SCR_REG_REG (ua3, SCR_ADDC, 0),
0,
SCR_LOAD_ABS (scratcha, 4),
PADDRH (zero),
SCR_REG_REG (scratcha, SCR_OR, 1),
0,
SCR_FROM_REG (rbc3),
0,
SCR_TO_REG (scratcha3),
0,
SCR_STORE_REL (scratcha, 4),
offsetof (struct ccb, phys.wresid.size),
SCR_JUMPR ^ IFFALSE (WHEN (SCR_DATA_IN)),
0,
SCR_CHMOV_TBL ^ SCR_DATA_IN,
offsetof (struct ccb, phys.wresid),
SCR_FROM_REG (rbc),
0,
SCR_RETURN ^ IFFALSE (DATA (0)),
0,
SCR_FROM_REG (rbc1),
0,
SCR_RETURN ^ IFFALSE (DATA (0)),
0,
SCR_FROM_REG (rbc2),
0,
SCR_RETURN ^ IFFALSE (DATA (0)),
0,
SCR_STORE_ABS (ia, 4),
PADDRH (scratch),
SCR_LOAD_ABS (temp, 4),
PADDRH (scratch),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_CHMOV_TBL ^ SCR_DATA_IN,
offsetof (struct ccb, phys.wresid),
SCR_JUMP,
PADDR (dispatch),
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
},{
SCR_DATA_ZERO,
#ifdef SCSI_NCR_PCI_MEM_NOT_SUPPORTED
},{
SCR_COPY (sizeof (struct script)),
},{
0,
PADDR (start),
SCR_JUMP,
PADDR (init),
},{
SCR_LOAD_REL (mmws, 4),
offsetof (struct ncb, scr_ram_seg),
SCR_COPY (sizeof(struct script)),
},{
0,
PADDR (start),
SCR_COPY (sizeof(struct scripth)),
},{
0,
PADDRH  (start64),
SCR_LOAD_REL  (mmrs, 4),
offsetof (struct ncb, scr_ram_seg),
SCR_JUMP64,
PADDRH (start64),
},{
0,
#endif
},{
SCR_LOAD_REL (scratcha, 4),
offsetof(struct ncb, ncr_cache),
SCR_STORE_REL (temp, 4),
offsetof(struct ncb, ncr_cache),
SCR_LOAD_REL (temp, 4),
offsetof(struct ncb, ncr_cache),
},{
SCR_INT,
99,
}
};
void __init ncr_script_fill (struct script * scr, struct scripth * scrh)
{
int	i;
ncrcmd	*p;
p = scr->data_in;
for (i=0; i<MAX_SCATTER; i++) {
*p++ =SCR_CHMOV_TBL ^ SCR_DATA_IN;
*p++ =offsetof (struct dsb, data[i]);
};
assert ((u_long)p == (u_long)&scr->data_in + sizeof (scr->data_in));
p = scr->data_out;
for (i=0; i<MAX_SCATTER; i++) {
*p++ =SCR_CHMOV_TBL ^ SCR_DATA_OUT;
*p++ =offsetof (struct dsb, data[i]);
};
assert ((u_long)p == (u_long)&scr->data_out + sizeof (scr->data_out));
}
static void __init
ncr_script_copy_and_bind (ncb_p np,ncrcmd *src,ncrcmd *dst,int len)
{
ncrcmd  opcode, new, old, tmp1, tmp2;
ncrcmd	*start, *end;
int relocs;
int opchanged = 0;
start = src;
end = src + len/4;
while (src < end) {
opcode = *src++;
*dst++ = cpu_to_scr(opcode);
if (opcode == 0) {
printk (KERN_INFO "%s: ERROR0 IN SCRIPT at %d.\n",
ncr_name(np), (int) (src-start-1));
MDELAY (10000);
continue;
};
if (opcode == SCR_DATA_ZERO) {
dst[-1] = 0;
continue;
}
if (DEBUG_FLAGS & DEBUG_SCRIPT)
printk (KERN_INFO "%p:  <%x>\n",
(src-1), (unsigned)opcode);
switch (opcode >> 28) {
case 0xf:
relocs = 0;
break;
case 0xe:
relocs = 1;
break;
case 0xc:
relocs = 2;
tmp1 = src[0];
tmp2 = src[1];
#ifdef	RELOC_KVAR
if ((tmp1 & RELOC_MASK) == RELOC_KVAR)
tmp1 = 0;
if ((tmp2 & RELOC_MASK) == RELOC_KVAR)
tmp2 = 0;
#endif
if ((tmp1 ^ tmp2) & 3) {
printk (KERN_ERR"%s: ERROR1 IN SCRIPT at %d.\n",
ncr_name(np), (int) (src-start-1));
MDELAY (1000);
}
if ((opcode & SCR_NO_FLUSH) &&
!(np->features & FE_PFEN)) {
dst[-1] = cpu_to_scr(opcode & ~SCR_NO_FLUSH);
++opchanged;
}
break;
case 0x0:
if (!(np->features & FE_WIDE))
dst[-1] = cpu_to_scr(opcode | OPC_MOVE);
relocs = 1;
break;
case 0x1:
if (!(np->features & FE_WIDE))
dst[-1] = cpu_to_scr(opcode | OPC_MOVE);
relocs = 0;
break;
case 0x8:
if (opcode & 0x00800000)
relocs = 0;
else if ((opcode & 0xf8400000) == 0x80400000)
relocs = 2;
else
relocs = 1;
break;
case 0x4:
case 0x5:
case 0x6:
case 0x7:
relocs = 1;
break;
default:
relocs = 0;
break;
};
if (!relocs) {
*dst++ = cpu_to_scr(*src++);
continue;
}
while (relocs--) {
old = *src++;
switch (old & RELOC_MASK) {
case RELOC_REGISTER:
new = (old & ~RELOC_MASK) + pcivtobus(np->base_ba);
break;
case RELOC_LABEL:
new = (old & ~RELOC_MASK) + np->p_script;
break;
case RELOC_LABELH:
new = (old & ~RELOC_MASK) + np->p_scripth;
break;
case RELOC_SOFTC:
new = (old & ~RELOC_MASK) + np->p_ncb;
break;
#ifdef	RELOC_KVAR
case RELOC_KVAR:
new=0;
if (((old & ~RELOC_MASK) < SCRIPT_KVAR_FIRST) ||
((old & ~RELOC_MASK) > SCRIPT_KVAR_LAST))
panic("ncr KVAR out of range");
new = vtobus(script_kvars[old & ~RELOC_MASK]);
#endif
break;
case 0:
if (old == 0) {
new = old;
break;
}
default:
new = 0;
panic("ncr_script_copy_and_bind: "
"weird relocation %x\n", old);
break;
}
*dst++ = cpu_to_scr(new);
}
};
}
struct host_data {
struct ncb *ncb;
};
static void PRINT_TARGET(ncb_p np, int target)
{
printk(KERN_INFO "%s-<%d,*>: ", ncr_name(np), target);
}
static void PRINT_LUN(ncb_p np, int target, int lun)
{
printk(KERN_INFO "%s-<%d,%d>: ", ncr_name(np), target, lun);
}
static void PRINT_ADDR(Scsi_Cmnd *cmd)
{
struct host_data *host_data = (struct host_data *) cmd->host->hostdata;
PRINT_LUN(host_data->ncb, cmd->target, cmd->lun);
}
#define _5M 5000000
static u_long div_10M[] =
{2*_5M, 3*_5M, 4*_5M, 6*_5M, 8*_5M, 12*_5M, 16*_5M};
#define burst_length(bc) (!(bc))? 0 : 1 << (bc)
#define burst_code(dmode, ctest4, ctest5) \
(ctest4) & 0x80? 0 : (((dmode) & 0xc0) >> 6) + ((ctest5) & 0x04) + 1
static inline void ncr_init_burst(ncb_p np, u_char bc)
{
np->rv_ctest4	&= ~0x80;
np->rv_dmode	&= ~(0x3 << 6);
np->rv_ctest5	&= ~0x4;
if (!bc) {
np->rv_ctest4	|= 0x80;
}
else {
--bc;
np->rv_dmode	|= ((bc & 0x3) << 6);
np->rv_ctest5	|= (bc & 0x4);
}
}
#ifdef SCSI_NCR_NVRAM_SUPPORT
static void __init
ncr_Symbios_setup_target(ncb_p np, int target, Symbios_nvram *nvram)
{
tcb_p tp = &np->target[target];
Symbios_target *tn = &nvram->target[target];
tp->usrsync = tn->sync_period ? (tn->sync_period + 3) / 4 : 255;
tp->usrwide = tn->bus_width == 0x10 ? 1 : 0;
tp->usrtags =
(tn->flags & SYMBIOS_QUEUE_TAGS_ENABLED)? MAX_TAGS : 0;
if (!(tn->flags & SYMBIOS_DISCONNECT_ENABLE))
tp->usrflag |= UF_NODISC;
if (!(tn->flags & SYMBIOS_SCAN_AT_BOOT_TIME))
tp->usrflag |= UF_NOSCAN;
}
static void __init
ncr_Tekram_setup_target(ncb_p np, int target, Tekram_nvram *nvram)
{
tcb_p tp = &np->target[target];
struct Tekram_target *tn = &nvram->target[target];
int i;
if (tn->flags & TEKRAM_SYNC_NEGO) {
i = tn->sync_index & 0xf;
tp->usrsync = Tekram_sync[i];
}
tp->usrwide = (tn->flags & TEKRAM_WIDE_NEGO) ? 1 : 0;
if (tn->flags & TEKRAM_TAGGED_COMMANDS) {
tp->usrtags = 2 << nvram->max_tags_index;
}
if (!(tn->flags & TEKRAM_DISCONNECT_ENABLE))
tp->usrflag = UF_NODISC;
if (!(tn->flags & TEKRAM_PARITY_CHECK))
np->rv_scntl0  &= ~0x0a;
}
#endif
static void __init ncr_save_initial_setting(ncb_p np)
{
np->sv_scntl0	= INB(nc_scntl0) & 0x0a;
np->sv_dmode	= INB(nc_dmode)  & 0xce;
np->sv_dcntl	= INB(nc_dcntl)  & 0xa8;
np->sv_ctest3	= INB(nc_ctest3) & 0x01;
np->sv_ctest4	= INB(nc_ctest4) & 0x80;
np->sv_gpcntl	= INB(nc_gpcntl);
np->sv_stest2	= INB(nc_stest2) & 0x20;
np->sv_stest4	= INB(nc_stest4);
np->sv_stest1	= INB(nc_stest1);
np->sv_scntl3   = INB(nc_scntl3) & 0x07;
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66) ){
np->sv_ctest5 = INB(nc_ctest5) & 0x04 ;
np->sv_scntl4 = INB(nc_scntl4);
}
else {
np->sv_ctest5 = INB(nc_ctest5) & 0x24 ;
np->sv_scntl4 = 0;
}
}
static int __init ncr_prepare_setting(ncb_p np, ncr_nvram *nvram)
{
u_char	burst_max;
u_long	period;
int i;
np->maxwide	= (np->features & FE_WIDE)? 1 : 0;
if	(np->features & FE_QUAD)
np->multiplier	= 4;
else if	(np->features & FE_DBLR)
np->multiplier	= 2;
else
np->multiplier	= 1;
np->clock_khz	= (np->features & FE_CLK80)? 80000 : 40000;
np->clock_khz	*= np->multiplier;
if (np->clock_khz != 40000)
ncr_getclock(np, np->multiplier);
if ( (np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)) {
np->rv_scntl3 = 0;
}
else
{
i = np->clock_divn - 1;
while (--i >= 0) {
if (10ul * SCSI_NCR_MIN_ASYNC * np->clock_khz
> div_10M[i]) {
++i;
break;
}
}
np->rv_scntl3 = i+1;
}
np->rv_scntl4 = np->sv_scntl4;
period = (4 * div_10M[0] + np->clock_khz - 1) / np->clock_khz;
if	(period <= 250)		np->minsync = 10;
else if	(period <= 303)		np->minsync = 11;
else if	(period <= 500)		np->minsync = 12;
else				np->minsync = (period + 40 - 1) / 40;
if ((np->minsync == 10) && (np->features & FE_ULTRA3))
np->minsync = 9;
if	(np->minsync < 25 && !(np->features & (FE_ULTRA|FE_ULTRA2|FE_ULTRA3)))
np->minsync = 25;
else if	(np->minsync < 12 && (np->features & FE_ULTRA))
np->minsync = 12;
else if	(np->minsync < 10 && (np->features & FE_ULTRA2))
np->minsync = 10;
else if	(np->minsync < 9 && (np->features & FE_ULTRA3))
np->minsync = 9;
period = (11 * div_10M[np->clock_divn - 1]) / (4 * np->clock_khz);
np->maxsync = period > 2540 ? 254 : period / 10;
if (np->features & FE_64BIT)
#ifdef SCSI_NCR_USE_64BIT_DAC
np->rv_ccntl1	|= (XTIMOD | EXTIBMV);
#else
np->rv_ccntl1	|= (DDAC);
#endif
if (np->features & FE_NOPM)
np->rv_ccntl0	|= (ENPMJ);
#if defined SCSI_NCR_TRUST_BIOS_SETTING
np->rv_scntl0	= np->sv_scntl0;
np->rv_dmode	= np->sv_dmode;
np->rv_dcntl	= np->sv_dcntl;
np->rv_ctest3	= np->sv_ctest3;
np->rv_ctest4	= np->sv_ctest4;
np->rv_ctest5	= np->sv_ctest5;
burst_max	= burst_code(np->sv_dmode, np->sv_ctest4, np->sv_ctest5);
#else
burst_max	= driver_setup.burst_max;
if (burst_max == 255)
burst_max = burst_code(np->sv_dmode, np->sv_ctest4, np->sv_ctest5);
if (burst_max > 7)
burst_max = 7;
if (burst_max > np->maxburst)
burst_max = np->maxburst;
if ((np->device_id == PCI_DEVICE_ID_NCR_53C810 &&
np->revision_id >= 0x10 && np->revision_id <= 0x11) ||
(np->device_id == PCI_DEVICE_ID_NCR_53C860 &&
np->revision_id <= 0x1))
np->features &= ~(FE_WRIE|FE_ERL|FE_ERMP);
if ( ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) && (np->revision_id < 0x02) )
|| (np->device_id == PCI_DEVICE_ID_LSI_53C1010_66 ) )
np->rv_ccntl1  |=  0x10;
if (np->features & FE_ERL)
np->rv_dmode	|= ERL;
if (np->features & FE_BOF)
np->rv_dmode	|= BOF;
if (np->features & FE_ERMP)
np->rv_dmode	|= ERMP;
#if 1
if ((np->features & FE_PFEN) && !np->base2_ba)
#else
if (np->features & FE_PFEN)
#endif
np->rv_dcntl	|= PFEN;
if (np->features & FE_CLSE)
np->rv_dcntl	|= CLSE;
if (np->features & FE_WRIE)
np->rv_ctest3	|= WRIE;
if ( (np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66) &&
(np->features & FE_DFS))
np->rv_ctest5	|= DFS;
if (driver_setup.master_parity)
np->rv_ctest4	|= MPEE;
if (driver_setup.scsi_parity)
np->rv_scntl0	|= 0x0a;
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (nvram) {
switch(nvram->type) {
case SCSI_NCR_TEKRAM_NVRAM:
np->myaddr = nvram->data.Tekram.host_id & 0x0f;
break;
case SCSI_NCR_SYMBIOS_NVRAM:
if (!(nvram->data.Symbios.flags & SYMBIOS_PARITY_ENABLE))
np->rv_scntl0  &= ~0x0a;
np->myaddr = nvram->data.Symbios.host_id & 0x0f;
if (nvram->data.Symbios.flags & SYMBIOS_VERBOSE_MSGS)
np->verbose += 1;
break;
}
}
#endif
if (np->myaddr == 255) {
np->myaddr = INB(nc_scid) & 0x07;
if (!np->myaddr)
np->myaddr = SCSI_NCR_MYADDR;
}
#endif
ncr_init_burst(np, burst_max);
np->scsi_mode = SMODE_SE;
if	(np->features & (FE_ULTRA2 | FE_ULTRA3))
np->scsi_mode = (np->sv_stest4 & SMODE);
else if	(np->features & FE_DIFF) {
switch(driver_setup.diff_support) {
case 4:
if (np->sv_scntl3) {
if (np->sv_stest2 & 0x20)
np->scsi_mode = SMODE_HVD;
break;
}
case 3:
if (nvram && nvram->type != SCSI_NCR_SYMBIOS_NVRAM)
break;
if (INB(nc_gpreg) & 0x08)
break;
case 2:
np->scsi_mode = SMODE_HVD;
case 1:
if (np->sv_stest2 & 0x20)
np->scsi_mode = SMODE_HVD;
break;
default:
break;
}
}
if (np->scsi_mode == SMODE_HVD)
np->rv_stest2 |= 0x20;
if ((driver_setup.led_pin ||
(nvram && nvram->type == SCSI_NCR_SYMBIOS_NVRAM)) &&
!(np->features & FE_LEDC) && !(np->sv_gpcntl & 0x01))
np->features |= FE_LED0;
switch(driver_setup.irqm & 3) {
case 2:
np->rv_dcntl	|= IRQM;
break;
case 1:
np->rv_dcntl	|= (np->sv_dcntl & IRQM);
break;
default:
break;
}
for (i = 0 ; i < MAX_TARGET ; i++) {
tcb_p tp = &np->target[i];
tp->usrsync = 255;
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (nvram) {
switch(nvram->type) {
case SCSI_NCR_TEKRAM_NVRAM:
ncr_Tekram_setup_target(np, i, &nvram->data.Tekram);
break;
case SCSI_NCR_SYMBIOS_NVRAM:
ncr_Symbios_setup_target(np, i, &nvram->data.Symbios);
break;
}
if (driver_setup.use_nvram & 0x2)
tp->usrsync = driver_setup.default_sync;
if (driver_setup.use_nvram & 0x4)
tp->usrwide = driver_setup.max_wide;
if (driver_setup.use_nvram & 0x8)
tp->usrflag &= ~UF_NOSCAN;
}
else {
#else
if (1) {
#endif
tp->usrsync = driver_setup.default_sync;
tp->usrwide = driver_setup.max_wide;
tp->usrtags = MAX_TAGS;
if (!driver_setup.disconnection)
np->target[i].usrflag = UF_NODISC;
}
}
i = nvram ? nvram->type : 0;
printk(KERN_INFO "%s: %sID %d, Fast-%d%s%s\n", ncr_name(np),
i  == SCSI_NCR_SYMBIOS_NVRAM ? "Symbios format NVRAM, " :
(i == SCSI_NCR_TEKRAM_NVRAM  ? "Tekram format NVRAM, " : ""),
np->myaddr,
np->minsync < 10 ? 80 :
(np->minsync < 12 ? 40 : (np->minsync < 25 ? 20 : 10) ),
(np->rv_scntl0 & 0xa)	? ", Parity Checking"	: ", NO Parity",
(np->rv_stest2 & 0x20)	? ", Differential"	: "");
if (bootverbose > 1) {
printk (KERN_INFO "%s: initial SCNTL3/DMODE/DCNTL/CTEST3/4/5 = "
"(hex) %02x/%02x/%02x/%02x/%02x/%02x\n",
ncr_name(np), np->sv_scntl3, np->sv_dmode, np->sv_dcntl,
np->sv_ctest3, np->sv_ctest4, np->sv_ctest5);
printk (KERN_INFO "%s: final   SCNTL3/DMODE/DCNTL/CTEST3/4/5 = "
"(hex) %02x/%02x/%02x/%02x/%02x/%02x\n",
ncr_name(np), np->rv_scntl3, np->rv_dmode, np->rv_dcntl,
np->rv_ctest3, np->rv_ctest4, np->rv_ctest5);
}
if (bootverbose && np->base2_ba)
printk (KERN_INFO "%s: on-chip RAM at 0x%lx\n",
ncr_name(np), np->base2_ba);
return 0;
}
#ifdef SCSI_NCR_DEBUG_NVRAM
void __init ncr_display_Symbios_nvram(ncb_p np, Symbios_nvram *nvram)
{
int i;
printk(KERN_DEBUG "%s: HOST ID=%d%s%s%s%s%s\n",
ncr_name(np), nvram->host_id & 0x0f,
(nvram->flags  & SYMBIOS_SCAM_ENABLE)	? " SCAM"	:"",
(nvram->flags  & SYMBIOS_PARITY_ENABLE)	? " PARITY"	:"",
(nvram->flags  & SYMBIOS_VERBOSE_MSGS)	? " VERBOSE"	:"",
(nvram->flags  & SYMBIOS_CHS_MAPPING)	? " CHS_ALT"	:"",
(nvram->flags1 & SYMBIOS_SCAN_HI_LO)	? " HI_LO"	:"");
for (i = 0 ; i < 15 ; i++) {
struct Symbios_target *tn = &nvram->target[i];
printk(KERN_DEBUG "%s-%d:%s%s%s%s WIDTH=%d SYNC=%d TMO=%d\n",
ncr_name(np), i,
(tn->flags & SYMBIOS_DISCONNECT_ENABLE)	? " DISC"	: "",
(tn->flags & SYMBIOS_SCAN_AT_BOOT_TIME)	? " SCAN_BOOT"	: "",
(tn->flags & SYMBIOS_SCAN_LUNS)		? " SCAN_LUNS"	: "",
(tn->flags & SYMBIOS_QUEUE_TAGS_ENABLED)? " TCQ"	: "",
tn->bus_width,
tn->sync_period / 4,
tn->timeout);
}
}
static u_char Tekram_boot_delay[7] __initdata = {3, 5, 10, 20, 30, 60, 120};
void __init ncr_display_Tekram_nvram(ncb_p np, Tekram_nvram *nvram)
{
int i, tags, boot_delay;
char *rem;
tags = 2 << nvram->max_tags_index;
boot_delay = 0;
if (nvram->boot_delay_index < 6)
boot_delay = Tekram_boot_delay[nvram->boot_delay_index];
switch((nvram->flags & TEKRAM_REMOVABLE_FLAGS) >> 6) {
default:
case 0:	rem = "";			break;
case 1: rem = " REMOVABLE=boot device";	break;
case 2: rem = " REMOVABLE=all";		break;
}
printk(KERN_DEBUG
"%s: HOST ID=%d%s%s%s%s%s%s%s%s%s BOOT DELAY=%d tags=%d\n",
ncr_name(np), nvram->host_id & 0x0f,
(nvram->flags1 & SYMBIOS_SCAM_ENABLE)	? " SCAM"	:"",
(nvram->flags & TEKRAM_MORE_THAN_2_DRIVES) ? " >2DRIVES"	:"",
(nvram->flags & TEKRAM_DRIVES_SUP_1GB)	? " >1GB"	:"",
(nvram->flags & TEKRAM_RESET_ON_POWER_ON) ? " RESET"	:"",
(nvram->flags & TEKRAM_ACTIVE_NEGATION)	? " ACT_NEG"	:"",
(nvram->flags & TEKRAM_IMMEDIATE_SEEK)	? " IMM_SEEK"	:"",
(nvram->flags & TEKRAM_SCAN_LUNS)	? " SCAN_LUNS"	:"",
(nvram->flags1 & TEKRAM_F2_F6_ENABLED)	? " F2_F6"	:"",
rem, boot_delay, tags);
for (i = 0; i <= 15; i++) {
int sync, j;
struct Tekram_target *tn = &nvram->target[i];
j = tn->sync_index & 0xf;
sync = Tekram_sync[j];
printk(KERN_DEBUG "%s-%d:%s%s%s%s%s%s PERIOD=%d\n",
ncr_name(np), i,
(tn->flags & TEKRAM_PARITY_CHECK)	? " PARITY"	: "",
(tn->flags & TEKRAM_SYNC_NEGO)		? " SYNC"	: "",
(tn->flags & TEKRAM_DISCONNECT_ENABLE)	? " DISC"	: "",
(tn->flags & TEKRAM_START_CMD)		? " START"	: "",
(tn->flags & TEKRAM_TAGGED_COMMANDS)	? " TCQ"	: "",
(tn->flags & TEKRAM_WIDE_NEGO)		? " WIDE"	: "",
sync);
}
}
#endif
static int __init
ncr_attach (Scsi_Host_Template *tpnt, int unit, ncr_device *device)
{
struct host_data *host_data;
ncb_p np = 0;
struct Scsi_Host *instance = 0;
u_long flags = 0;
ncr_nvram *nvram = device->nvram;
int i;
printk(KERN_INFO NAME53C "%s-%d: rev 0x%x on pci bus %d device %d function %d "
#ifdef __sparc__
"irq %s\n",
#else
"irq %d\n",
#endif
device->chip.name, unit, device->chip.revision_id,
device->slot.bus, (device->slot.device_fn & 0xf8) >> 3,
device->slot.device_fn & 7,
#ifdef __sparc__
__irq_itoa(device->slot.irq));
#else
device->slot.irq);
#endif
if (!(instance = scsi_register(tpnt, sizeof(*host_data))))
goto attach_error;
host_data = (struct host_data *) instance->hostdata;
np = __m_calloc_dma(device->pdev, sizeof(struct ncb), "NCB");
if (!np)
goto attach_error;
NCR_INIT_LOCK_NCB(np);
np->pdev  = device->pdev;
np->p_ncb = vtobus(np);
host_data->ncb = np;
strncpy(np->chip_name, device->chip.name, sizeof(np->chip_name) - 1);
np->unit	= unit;
np->verbose	= driver_setup.verbose;
sprintf(np->inst_name, NAME53C "%s-%d", np->chip_name, np->unit);
np->device_id	= device->chip.device_id;
np->revision_id	= device->chip.revision_id;
np->bus		= device->slot.bus;
np->device_fn	= device->slot.device_fn;
np->features	= device->chip.features;
np->clock_divn	= device->chip.nr_divisor;
np->maxoffs	= device->chip.offset_max;
np->maxburst	= device->chip.burst_max;
np->myaddr	= device->host_id;
np->squeue = (ncrcmd *)
m_calloc_dma(sizeof(ncrcmd)*(MAX_START*2), "SQUEUE");
if (!np->squeue)
goto attach_error;
np->p_squeue = vtobus(np->squeue);
np->dqueue = (ncrcmd *)
m_calloc_dma(sizeof(ncrcmd)*(MAX_START*2), "DQUEUE");
if (!np->dqueue)
goto attach_error;
np->targtbl = (u_int32 *) m_calloc_dma(256, "TARGTBL");
if (!np->targtbl)
goto attach_error;
np->script0	= (struct script *)
m_calloc_dma(sizeof(struct script),  "SCRIPT");
if (!np->script0)
goto attach_error;
np->scripth0	= (struct scripth *)
m_calloc_dma(sizeof(struct scripth), "SCRIPTH");
if (!np->scripth0)
goto attach_error;
xpt_que_init(&np->free_ccbq);
xpt_que_init(&np->b0_ccbq);
if (!ncr_alloc_ccb(np))
goto attach_error;
init_timer(&np->timer);
np->timer.data     = (unsigned long) np;
np->timer.function = sym53c8xx_timeout;
np->base_ba	= device->slot.base;
np->base_ws	= (np->features & FE_IO256)? 256 : 128;
np->base2_ba	= (np->features & FE_RAM)? device->slot.base_2 : 0;
#ifndef SCSI_NCR_IOMAPPED
np->base_va = remap_pci_mem(np->base_ba, np->base_ws);
if (!np->base_va) {
printk(KERN_ERR "%s: can't map PCI MMIO region\n",ncr_name(np));
goto attach_error;
}
else if (bootverbose > 1)
printk(KERN_INFO "%s: using memory mapped IO\n", ncr_name(np));
np->reg = (struct ncr_reg *) np->base_va;
#endif
if (np->base2_ba && sizeof(struct script) > 4096) {
printk(KERN_ERR "%s: script too large.\n", ncr_name(np));
goto attach_error;
}
if (device->slot.io_port) {
request_region(device->slot.io_port, np->base_ws, NAME53C8XX);
np->base_io = device->slot.io_port;
}
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (nvram) {
switch(nvram->type) {
case SCSI_NCR_SYMBIOS_NVRAM:
#ifdef SCSI_NCR_DEBUG_NVRAM
ncr_display_Symbios_nvram(np, &nvram->data.Symbios);
#endif
break;
case SCSI_NCR_TEKRAM_NVRAM:
#ifdef SCSI_NCR_DEBUG_NVRAM
ncr_display_Tekram_nvram(np, &nvram->data.Tekram);
#endif
break;
default:
nvram = 0;
#ifdef SCSI_NCR_DEBUG_NVRAM
printk(KERN_DEBUG "%s: NVRAM: None or invalid data.\n", ncr_name(np));
#endif
}
}
#endif
ncr_save_initial_setting (np);
ncr_chip_reset (np);
(void) ncr_prepare_setting(np, nvram);
i = np->pciclock_max ? ncr_getpciclock(np) : 0;
if (i && (i < np->pciclock_min  || i > np->pciclock_max)) {
printk(KERN_ERR "%s: PCI clock (%u KHz) is out of range "
"[%u KHz - %u KHz].\n",
ncr_name(np), i, np->pciclock_min, np->pciclock_max);
goto attach_error;
}
ncr_script_fill (&script0, &scripth0);
np->p_script	= vtobus(np->script0);
np->p_scripth	= vtobus(np->scripth0);
np->p_scripth0	= np->p_scripth;
if (np->base2_ba) {
np->p_script	= pcivtobus(np->base2_ba);
if (np->features & FE_RAM8K) {
np->base2_ws = 8192;
np->p_scripth = np->p_script + 4096;
#if BITS_PER_LONG > 32
np->scr_ram_seg = cpu_to_scr(np->base2_ba >> 32);
#endif
}
else
np->base2_ws = 4096;
#ifndef SCSI_NCR_PCI_MEM_NOT_SUPPORTED
np->base2_va = remap_pci_mem(np->base2_ba, np->base2_ws);
if (!np->base2_va) {
printk(KERN_ERR "%s: can't map PCI MEMORY region\n",
ncr_name(np));
goto attach_error;
}
#endif
}
ncr_script_copy_and_bind (np, (ncrcmd *) &script0, (ncrcmd *) np->script0, sizeof(struct script));
ncr_script_copy_and_bind (np, (ncrcmd *) &scripth0, (ncrcmd *) np->scripth0, sizeof(struct scripth));
np->scripth0->pm0_data_addr[0] =
cpu_to_scr(NCB_SCRIPT_PHYS(np, pm0_data));
np->scripth0->pm1_data_addr[0] =
cpu_to_scr(NCB_SCRIPT_PHYS(np, pm1_data));
if (np->features & FE_ULTRA3) {
np->script0->resel_scntl4[0] = cpu_to_scr(SCR_LOAD_REL (scntl4, 1));
np->script0->resel_scntl4[1] = cpu_to_scr(offsetof(struct tcb, uval));
}
#ifdef SCSI_NCR_PCI_MEM_NOT_SUPPORTED
np->scripth0->script0_ba[0]	= cpu_to_scr(vtobus(np->script0));
np->scripth0->script0_ba64[0]	= cpu_to_scr(vtobus(np->script0));
np->scripth0->scripth0_ba64[0]	= cpu_to_scr(vtobus(np->scripth0));
np->scripth0->ram_seg64[0]	= np->scr_ram_seg;
#endif
np->idletask.start	= cpu_to_scr(NCB_SCRIPT_PHYS (np, idle));
np->idletask.restart	= cpu_to_scr(NCB_SCRIPTH_PHYS (np, bad_i_t_l));
np->p_idletask		= NCB_PHYS(np, idletask);
np->notask.start	= cpu_to_scr(NCB_SCRIPT_PHYS (np, idle));
np->notask.restart	= cpu_to_scr(NCB_SCRIPTH_PHYS (np, bad_i_t_l));
np->p_notask		= NCB_PHYS(np, notask);
np->bad_i_t_l.start	= cpu_to_scr(NCB_SCRIPT_PHYS (np, idle));
np->bad_i_t_l.restart	= cpu_to_scr(NCB_SCRIPTH_PHYS (np, bad_i_t_l));
np->p_bad_i_t_l		= NCB_PHYS(np, bad_i_t_l);
np->bad_i_t_l_q.start	= cpu_to_scr(NCB_SCRIPT_PHYS (np, idle));
np->bad_i_t_l_q.restart	= cpu_to_scr(NCB_SCRIPTH_PHYS (np,bad_i_t_l_q));
np->p_bad_i_t_l_q	= NCB_PHYS(np, bad_i_t_l_q);
np->badluntbl = m_calloc_dma(256, "BADLUNTBL");
if (!np->badluntbl)
goto attach_error;
assert (offsetof(struct lcb, resel_task) == 0);
np->resel_badlun = cpu_to_scr(NCB_SCRIPTH_PHYS(np, resel_bad_lun));
for (i = 0 ; i < 64 ; i++)
np->badluntbl[i] = cpu_to_scr(NCB_PHYS(np, resel_badlun));
np->scripth0->targtbl[0] = cpu_to_scr(vtobus(np->targtbl));
for (i = 0 ; i < MAX_TARGET ; i++) {
np->targtbl[i] = cpu_to_scr(NCB_PHYS(np, target[i]));
np->target[i].b_luntbl = cpu_to_scr(vtobus(np->badluntbl));
np->target[i].b_lun0   = cpu_to_scr(NCB_PHYS(np, resel_badlun));
}
if (np->features & FE_LED0) {
np->script0->idle[0]  =
cpu_to_scr(SCR_REG_REG(gpreg, SCR_OR,  0x01));
np->script0->reselected[0] =
cpu_to_scr(SCR_REG_REG(gpreg, SCR_AND, 0xfe));
np->script0->start[0] =
cpu_to_scr(SCR_REG_REG(gpreg, SCR_AND, 0xfe));
}
if (np->device_id == PCI_DEVICE_ID_LSI_53C1010_66){
np->script0->datao_phase[0] =
cpu_to_scr(SCR_REG_REG(scntl4, SCR_OR, 0x0c));
}
#ifdef SCSI_NCR_IARB_SUPPORT
if (!(driver_setup.iarb & 1))
np->script0->ungetjob[0] = cpu_to_scr(SCR_NO_OP);
np->iarb_max = (driver_setup.iarb >> 4);
#endif
if (np->device_id == PCI_DEVICE_ID_NCR_53C896 &&
np->revision_id <= 0x1 && (np->features & FE_NOPM)) {
np->scatter = ncr_scatter_896R1;
np->script0->datai_phase[0] = cpu_to_scr(SCR_JUMP);
np->script0->datai_phase[1] =
cpu_to_scr(NCB_SCRIPTH_PHYS (np, tweak_pmj));
np->script0->datao_phase[0] = cpu_to_scr(SCR_JUMP);
np->script0->datao_phase[1] =
cpu_to_scr(NCB_SCRIPTH_PHYS (np, tweak_pmj));
}
else
#ifdef DEBUG_896R1
np->scatter = ncr_scatter_896R1;
#else
np->scatter = ncr_scatter;
#endif
ncr_chip_reset(np);
if (ncr_snooptest (np)) {
printk (KERN_ERR "CACHE INCORRECTLY CONFIGURED.\n");
goto attach_error;
};
if (request_irq(device->slot.irq, sym53c8xx_intr,
#ifdef SCSI_NCR_PCIQ_SYNC_ON_INTR
((driver_setup.irqm & 0x20) ? 0 : SA_INTERRUPT),
#else
((driver_setup.irqm & 0x10) ? 0 : SA_SHIRQ) |
#if 0 && LINUX_VERSION_CODE < LinuxVersionCode(2,2,0)
((driver_setup.irqm & 0x20) ? 0 : SA_INTERRUPT),
#else
0,
#endif
#endif
NAME53C8XX, np)) {
printk(KERN_ERR "%s: request irq %d failure\n",
ncr_name(np), device->slot.irq);
goto attach_error;
}
np->irq = device->slot.irq;
NCR_LOCK_NCB(np, flags);
if (ncr_reset_scsi_bus(np, 0, driver_setup.settle_delay) != 0) {
printk(KERN_ERR "%s: FATAL ERROR: CHECK SCSI BUS - CABLES, TERMINATION, DEVICE POWER etc.!\n", ncr_name(np));
NCR_UNLOCK_NCB(np, flags);
goto attach_error;
}
ncr_exception (np);
if (driver_setup.settle_delay > 2) {
printk(KERN_INFO "%s: waiting %d seconds for scsi devices to settle...\n",
ncr_name(np), driver_setup.settle_delay);
MDELAY (1000 * driver_setup.settle_delay);
}
np->lasttime=0;
ncr_timeout (np);
#ifdef SCSI_NCR_ALWAYS_SIMPLE_TAG
np->order = M_SIMPLE_TAG;
#endif
if (!first_host)
first_host = instance;
instance->max_channel	= 0;
instance->this_id	= np->myaddr;
instance->max_id	= np->maxwide ? 16 : 8;
instance->max_lun	= MAX_LUN;
#ifndef SCSI_NCR_IOMAPPED
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,3,29)
instance->base		= (unsigned long) np->reg;
#else
instance->base		= (char *) np->reg;
#endif
#endif
instance->irq		= np->irq;
instance->unique_id	= np->base_io;
instance->io_port	= np->base_io;
instance->n_io_port	= np->base_ws;
instance->dma_channel	= 0;
instance->cmd_per_lun	= MAX_TAGS;
instance->can_queue	= (MAX_START-4);
np->check_integrity       = 0;
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
instance->check_integrity = 0;
#ifdef SCSI_NCR_ENABLE_INTEGRITY_CHECK
if ( !(driver_setup.bus_check & 0x04) ) {
np->check_integrity       = 1;
instance->check_integrity = 1;
}
#endif
#endif
instance->select_queue_depths = sym53c8xx_select_queue_depths;
NCR_UNLOCK_NCB(np, flags);
return 0;
attach_error:
if (!instance) return -1;
printk(KERN_INFO "%s: giving up ...\n", ncr_name(np));
if (np)
ncr_free_resources(np);
scsi_unregister(instance);
return -1;
}
static void ncr_free_resources(ncb_p np)
{
ccb_p cp;
tcb_p tp;
lcb_p lp;
int target, lun;
if (np->irq)
free_irq(np->irq, np);
if (np->base_io)
release_region(np->base_io, np->base_ws);
#ifndef SCSI_NCR_PCI_MEM_NOT_SUPPORTED
if (np->base_va)
unmap_pci_mem(np->base_va, np->base_ws);
if (np->base2_va)
unmap_pci_mem(np->base2_va, np->base2_ws);
#endif
if (np->scripth0)
m_free_dma(np->scripth0, sizeof(struct scripth), "SCRIPTH");
if (np->script0)
m_free_dma(np->script0, sizeof(struct script), "SCRIPT");
if (np->squeue)
m_free_dma(np->squeue, sizeof(ncrcmd)*(MAX_START*2), "SQUEUE");
if (np->dqueue)
m_free_dma(np->dqueue, sizeof(ncrcmd)*(MAX_START*2),"DQUEUE");
while ((cp = np->ccbc) != NULL) {
np->ccbc = cp->link_ccb;
m_free_dma(cp, sizeof(*cp), "CCB");
}
if (np->badluntbl)
m_free_dma(np->badluntbl, 256,"BADLUNTBL");
for (target = 0; target < MAX_TARGET ; target++) {
tp = &np->target[target];
for (lun = 0 ; lun < MAX_LUN ; lun++) {
lp = ncr_lp(np, tp, lun);
if (!lp)
continue;
if (lp->tasktbl != &lp->tasktbl_0)
m_free_dma(lp->tasktbl, MAX_TASKS*4, "TASKTBL");
if (lp->cb_tags)
m_free(lp->cb_tags, MAX_TAGS, "CB_TAGS");
m_free_dma(lp, sizeof(*lp), "LCB");
}
#if MAX_LUN > 1
if (tp->lmp)
m_free(tp->lmp, MAX_LUN * sizeof(lcb_p), "LMP");
if (tp->luntbl)
m_free_dma(tp->luntbl, 256, "LUNTBL");
#endif
}
if (np->targtbl)
m_free_dma(np->targtbl, 256, "TARGTBL");
m_free_dma(np, sizeof(*np), "NCB");
}
static inline void ncr_queue_done_cmd(ncb_p np, Scsi_Cmnd *cmd)
{
unmap_scsi_data(np, cmd);
cmd->host_scribble = (char *) np->done_list;
np->done_list = cmd;
}
static inline void ncr_flush_done_cmds(Scsi_Cmnd *lcmd)
{
Scsi_Cmnd *cmd;
while (lcmd) {
cmd = lcmd;
lcmd = (Scsi_Cmnd *) cmd->host_scribble;
cmd->scsi_done(cmd);
}
}
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
static int ncr_ic_nego(ncb_p np, ccb_p cp, Scsi_Cmnd *cmd, u_char *msgptr)
{
tcb_p tp = &np->target[cp->target];
int msglen = 0;
int nego = 0;
u_char new_width, new_offset, new_period;
u_char no_increase;
if (tp->ppr_negotiation == 1)
tp->ppr_negotiation = 2;
if (tp->inq_done) {
if (!tp->ic_maximums_set) {
tp->ic_maximums_set = 1;
if ( (tp->inq_byte7 & INQ7_WIDE16) &&
np->maxwide  && tp->usrwide)
tp->ic_max_width = 1;
else
tp->ic_max_width = 0;
if ((tp->inq_byte7 & INQ7_SYNC) && tp->maxoffs)
tp->ic_min_sync = (tp->minsync < np->minsync) ?
np->minsync : tp->minsync;
else
tp->ic_min_sync = 255;
tp->period   = 1;
tp->widedone = 1;
#if 0
if (tp->ic_max_width && (tp->ic_min_sync != 255 ))
tp->ppr_negotiation = 1;
#endif
tp->ppr_negotiation = 0;
if (np->features & FE_ULTRA3) {
if (tp->ic_max_width && (tp->ic_min_sync == 0x09))
tp->ppr_negotiation = 1;
}
if (!tp->ppr_negotiation)
cmd->ic_nego &= ~NS_PPR;
}
if (DEBUG_FLAGS & DEBUG_IC) {
printk("%s: cmd->ic_nego %d, 1st byte 0x%2X\n",
ncr_name(np), cmd->ic_nego, cmd->cmnd[0]);
}
if (np->check_integ_par) {
printk("%s: Parity Error. Target set to narrow.\n",
ncr_name(np));
tp->ic_max_width = 0;
tp->widedone = tp->period = 0;
}
if (!tp->ppr_negotiation &&  (cmd->ic_nego == NS_PPR )) {
tp->ppr_negotiation = 0;
cmd->ic_nego &= ~NS_PPR;
tp->widedone = tp->period = 1;
return msglen;
}
else if (( tp->ppr_negotiation && !(cmd->ic_nego & NS_PPR )) ||
(!tp->ppr_negotiation &&  (cmd->ic_nego & NS_PPR )) ) {
tp->ppr_negotiation = 0;
cmd->ic_nego &= ~NS_PPR;
}
if ((tp->ppr_negotiation) && (!(cmd->ic_nego & NS_PPR)))
tp->ppr_negotiation = 0;
no_increase = 0;
if (tp->ppr_negotiation && (!tp->widedone) && (!tp->period) ) {
cmd->ic_nego = NS_PPR;
tp->widedone = tp->period = 1;
no_increase = 1;
}
else if (!tp->widedone) {
cmd->ic_nego = NS_WIDE;
tp->widedone = 1;
no_increase = 1;
}
else if (!tp->period) {
cmd->ic_nego = NS_SYNC;
tp->period = 1;
no_increase = 1;
}
new_width = cmd->ic_nego_width & tp->ic_max_width;
switch (cmd->ic_nego_sync) {
case 2:
if (!no_increase) {
if (tp->ic_min_sync <= 0x09)
tp->ic_min_sync = 0x0A;
else if (tp->ic_min_sync <= 0x0A)
tp->ic_min_sync = 0x0C;
else if (tp->ic_min_sync <= 0x0C)
tp->ic_min_sync = 0x19;
else if (tp->ic_min_sync <= 0x19)
tp->ic_min_sync *= 2;
else  {
tp->ic_min_sync = 255;
cmd->ic_nego_sync = 0;
tp->maxoffs = 0;
}
}
new_period  = tp->maxoffs?tp->ic_min_sync:0;
new_offset  = tp->maxoffs;
break;
case 1:
new_period  = tp->maxoffs?tp->ic_min_sync:0;
new_offset  = tp->maxoffs;
break;
case 0:
default:
new_period = 0;
new_offset = 0;
break;
};
nego = NS_NOCHANGE;
if (tp->ppr_negotiation) {
u_char options_byte = 0;
if ( (new_period==0x09) && new_offset) {
if (new_width)
options_byte = 0x02;
else {
tp->ic_min_sync = 0x0A;
new_period = 0x0A;
cmd->ic_nego_width = 1;
new_width = 1;
new_offset &= 0x1f;
}
}
else if (new_period > 0x09)
new_offset &= 0x1f;
nego = NS_PPR;
msgptr[msglen++] = M_EXTENDED;
msgptr[msglen++] = 6;
msgptr[msglen++] = M_X_PPR_REQ;
msgptr[msglen++] = new_period;
msgptr[msglen++] = 0;
msgptr[msglen++] = new_offset;
msgptr[msglen++] = new_width;
msgptr[msglen++] = options_byte;
}
else {
switch (cmd->ic_nego & ~NS_PPR) {
case NS_WIDE:
cmd->ic_nego_width &= tp->ic_max_width;
if (tp->ic_max_width | np->check_integ_par) {
nego = NS_WIDE;
msgptr[msglen++] = M_EXTENDED;
msgptr[msglen++] = 2;
msgptr[msglen++] = M_X_WIDE_REQ;
msgptr[msglen++] = new_width;
}
break;
case NS_SYNC:
if (tp->inq_byte7 & INQ7_SYNC) {
if (new_offset && (new_period < 0x0A)) {
tp->ic_min_sync = 0x0A;
new_period = 0x0A;
}
nego = NS_SYNC;
msgptr[msglen++] = M_EXTENDED;
msgptr[msglen++] = 3;
msgptr[msglen++] = M_X_SYNC_REQ;
msgptr[msglen++] = new_period;
msgptr[msglen++] = new_offset & 0x1f;
}
else
cmd->ic_nego_sync = 0;
break;
case NS_NOCHANGE:
break;
}
}
};
cp->nego_status = nego;
np->check_integ_par = 0;
if (nego) {
tp->nego_cp = cp;
if (DEBUG_FLAGS & DEBUG_NEGO) {
ncr_print_msg(cp, nego == NS_WIDE ?
"wide/narrow msgout":
(nego == NS_SYNC ? "sync/async msgout" : "ppr msgout"),
msgptr);
};
};
return msglen;
}
#endif
static int ncr_prepare_nego(ncb_p np, ccb_p cp, u_char *msgptr)
{
tcb_p tp = &np->target[cp->target];
int msglen = 0;
int nego = 0;
u_char width, offset, factor, last_byte;
if (!np->check_integrity) {
if (tp->ppr_negotiation == 1)
tp->ppr_negotiation = 2;
if ((tp->inq_done) && (!tp->ic_maximums_set)) {
tp->ic_maximums_set = 1;
tp->ppr_negotiation = 0;
if ( (np->features & FE_ULTRA3) &&
(tp->usrwide) && (tp->maxoffs) &&
(tp->minsync == 0x09) )
tp->ppr_negotiation = 1;
}
}
if (tp->inq_done) {
ncr_get_xfer_info( np, tp, &factor,
&offset, &width);
if (!tp->widedone) {
if (tp->inq_byte7 & INQ7_WIDE16) {
if (tp->ppr_negotiation)
nego = NS_PPR;
else
nego = NS_WIDE;
width = tp->usrwide;
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
if (tp->ic_done)
width &= tp->ic_max_width;
#endif
} else
tp->widedone=1;
};
if ((nego != NS_WIDE) && !tp->period) {
if (tp->inq_byte7 & INQ7_SYNC) {
if (tp->ppr_negotiation)
nego = NS_PPR;
else
nego = NS_SYNC;
if (tp->maxoffs == 0) {
offset = 0;
factor = 0;
}
else {
offset = tp->maxoffs;
factor = tp->minsync;
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
if ((tp->ic_done) &&
(factor < tp->ic_min_sync))
factor = tp->ic_min_sync;
#endif
}
} else {
offset = 0;
factor = 0;
tp->period  =0xffff;
PRINT_TARGET(np, cp->target);
printk ("target did not report SYNC.\n");
};
};
};
switch (nego) {
case NS_PPR:
last_byte = 0;
if ( (factor==9) && offset) {
if (!width) {
factor = 0x0A;
offset &= 0x1f;
}
else
last_byte = 0x02;
}
else if (factor > 0x09)
offset &= 0x1f;
msgptr[msglen++] = M_EXTENDED;
msgptr[msglen++] = 6;
msgptr[msglen++] = M_X_PPR_REQ;
msgptr[msglen++] = factor;
msgptr[msglen++] = 0;
msgptr[msglen++] = offset;
msgptr[msglen++] = width;
msgptr[msglen++] = last_byte;
break;
case NS_SYNC:
if (offset && (factor < 0x0A)) {
factor = 0x0A;
tp->minsync = 0x0A;
}
msgptr[msglen++] = M_EXTENDED;
msgptr[msglen++] = 3;
msgptr[msglen++] = M_X_SYNC_REQ;
msgptr[msglen++] = factor;
msgptr[msglen++] = offset & 0x1f;
break;
case NS_WIDE:
msgptr[msglen++] = M_EXTENDED;
msgptr[msglen++] = 2;
msgptr[msglen++] = M_X_WIDE_REQ;
msgptr[msglen++] = width;
break;
};
cp->nego_status = nego;
if (nego) {
tp->nego_cp = cp;
if (DEBUG_FLAGS & DEBUG_NEGO) {
ncr_print_msg(cp, nego == NS_WIDE ?
"wide msgout":
(nego == NS_SYNC ? "sync msgout" : "ppr msgout"),
msgptr);
};
};
return msglen;
}
static int ncr_queue_command (ncb_p np, Scsi_Cmnd *cmd)
{
tcb_p tp                      = &np->target[cmd->target];
lcb_p lp		      = ncr_lp(np, tp, cmd->lun);
ccb_p cp;
u_char	idmsg, *msgptr;
u_int   msglen;
int	direction;
u_int32	lastp, goalp;
if ((cmd->target == np->myaddr	  ) ||
(cmd->target >= MAX_TARGET) ||
(cmd->lun    >= MAX_LUN   )) {
return(DID_BAD_TARGET);
}
if (cmd->cmnd[0] == 0 && (tp->usrflag & UF_NOSCAN)) {
tp->usrflag &= ~UF_NOSCAN;
return DID_BAD_TARGET;
}
if (DEBUG_FLAGS & DEBUG_TINY) {
PRINT_ADDR(cmd);
printk ("CMD=%x ", cmd->cmnd[0]);
}
if (np->settle_time && cmd->timeout_per_command >= HZ) {
u_long tlimit = ktime_get(cmd->timeout_per_command - HZ);
if (ktime_dif(np->settle_time, tlimit) > 0)
np->settle_time = tlimit;
}
if (np->settle_time || !(cp=ncr_get_ccb (np, cmd->target, cmd->lun))) {
insert_into_waiting_list(np, cmd);
return(DID_OK);
}
cp->cmd = cmd;
#if 0
if (lp && !lp->numtags && cmd->device && cmd->device->tagged_queue) {
lp->numtags = tp->usrtags;
ncr_setup_tags (np, cp->target, cp->lun);
}
#endif
idmsg = M_IDENTIFY | cp->lun;
if (cp ->tag != NO_TAG || (lp && !(tp->usrflag & UF_NODISC)))
idmsg |= 0x40;
msgptr = cp->scsi_smsg;
msglen = 0;
msgptr[msglen++] = idmsg;
if (cp->tag != NO_TAG) {
char order = np->order;
if (lp && ktime_exp(lp->tags_stime)) {
lp->tags_si = !(lp->tags_si);
if (lp->tags_sum[lp->tags_si]) {
order = M_ORDERED_TAG;
if ((DEBUG_FLAGS & DEBUG_TAGS)||bootverbose>0){
PRINT_ADDR(cmd);
printk("ordered tag forced.\n");
}
}
lp->tags_stime = ktime_get(3*HZ);
}
if (order == 0) {
switch (cmd->cmnd[0]) {
case 0x08:
case 0x28:
case 0xa8:
order = M_SIMPLE_TAG;
break;
default:
order = M_ORDERED_TAG;
}
}
msgptr[msglen++] = order;
#if MAX_TASKS > (512/4)
msgptr[msglen++] = cp->tag;
#else
msgptr[msglen++] = (cp->tag << 1) + 1;
#endif
}
cp->host_flags	= 0;
direction = scsi_data_direction(cmd);
if (direction != SCSI_DATA_NONE) {
cp->segments = np->scatter (np, cp, cp->cmd);
if (cp->segments < 0) {
ncr_free_ccb(np, cp);
return(DID_ERROR);
}
}
else {
cp->data_len = 0;
cp->segments = 0;
}
cp->nego_status = 0;
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
if ((np->check_integrity && tp->ic_done) || !np->check_integrity) {
if ((!tp->widedone || !tp->period) && !tp->nego_cp && lp) {
msglen += ncr_prepare_nego (np, cp, msgptr + msglen);
}
}
else if (np->check_integrity && (cmd->ic_in_progress)) {
msglen += ncr_ic_nego (np, cp, cmd, msgptr + msglen);
}
else if (np->check_integrity && cmd->ic_complete) {
u_long current_period;
u_char current_offset, current_width, current_factor;
ncr_get_xfer_info (np, tp, &current_factor,
&current_offset, &current_width);
tp->ic_max_width = current_width;
tp->ic_min_sync  = current_factor;
if      (current_factor == 9) 	current_period = 125;
else if (current_factor == 10) 	current_period = 250;
else if (current_factor == 11) 	current_period = 303;
else if (current_factor == 12) 	current_period = 500;
else  			current_period = current_factor * 40;
tp->period = current_period;
tp->widedone = 1;
tp->ic_done = 1;
printk("%s: Integrity Check Complete: \n", ncr_name(np));
printk("%s: %s %s SCSI", ncr_name(np),
current_offset?"SYNC":"ASYNC",
tp->ic_max_width?"WIDE":"NARROW");
if (current_offset) {
u_long mbs = 10000 * (tp->ic_max_width + 1);
printk(" %d.%d  MB/s",
(int) (mbs / current_period), (int) (mbs % current_period));
printk(" (%d ns, %d offset)\n",
(int) current_period/10, current_offset);
}
else
printk(" %d MB/s. \n ", (tp->ic_max_width+1)*5);
}
#else
if ((!tp->widedone || !tp->period) && !tp->nego_cp && lp) {
msglen += ncr_prepare_nego (np, cp, msgptr + msglen);
}
#endif
if (!cp->data_len)
direction = SCSI_DATA_NONE;
switch(direction) {
case SCSI_DATA_UNKNOWN:
case SCSI_DATA_WRITE:
goalp = NCB_SCRIPT_PHYS (np, data_out2) + 8;
lastp = goalp - 8 - (cp->segments * (SCR_SG_SIZE*4));
if (direction != SCSI_DATA_UNKNOWN)
break;
cp->phys.header.wgoalp	= cpu_to_scr(goalp);
cp->phys.header.wlastp	= cpu_to_scr(lastp);
case SCSI_DATA_READ:
cp->host_flags |= HF_DATA_IN;
goalp = NCB_SCRIPT_PHYS (np, data_in2) + 8;
lastp = goalp - 8 - (cp->segments * (SCR_SG_SIZE*4));
break;
default:
case SCSI_DATA_NONE:
lastp = goalp = NCB_SCRIPTH_PHYS (np, no_data);
break;
}
cp->phys.header.lastp = cpu_to_scr(lastp);
cp->phys.header.goalp = cpu_to_scr(goalp);
if (direction == SCSI_DATA_UNKNOWN)
cp->phys.header.savep =
cpu_to_scr(NCB_SCRIPTH_PHYS (np, data_io));
else
cp->phys.header.savep= cpu_to_scr(lastp);
cp->startp = cp->phys.header.savep;
cp->lastp0 = cp->phys.header.lastp;
cp->phys.header.go.start   = cpu_to_scr(NCB_SCRIPT_PHYS (np,select));
cp->phys.header.go.restart = cpu_to_scr(NCB_SCRIPT_PHYS (np,resel_dsa));
cp->phys.select.sel_id		= cp->target;
cp->phys.select.sel_scntl3	= tp->wval;
cp->phys.select.sel_sxfer	= tp->sval;
cp->phys.select.sel_scntl4	= tp->uval;
cp->phys.smsg.addr	= cpu_to_scr(CCB_PHYS (cp, scsi_smsg));
cp->phys.smsg.size	= cpu_to_scr(msglen);
memcpy(cp->cdb_buf, cmd->cmnd, MIN(cmd->cmd_len, sizeof(cp->cdb_buf)));
cp->phys.cmd.addr	= cpu_to_scr(CCB_PHYS (cp, cdb_buf[0]));
cp->phys.cmd.size	= cpu_to_scr(cmd->cmd_len);
cp->actualquirks	= tp->quirks;
cp->host_status		= cp->nego_status ? HS_NEGOTIATE : HS_BUSY;
cp->scsi_status		= S_ILLEGAL;
cp->xerr_status		= 0;
cp->extra_bytes		= 0;
cp->ext_sg  = -1;
cp->ext_ofs = 0;
if (lp)
ncr_start_next_ccb(np, lp, 2);
else
ncr_put_start_queue(np, cp);
return(DID_OK);
}
static void ncr_start_next_ccb(ncb_p np, lcb_p lp, int maxn)
{
XPT_QUEHEAD *qp;
ccb_p cp;
while (maxn-- && lp->queuedccbs < lp->queuedepth) {
qp = xpt_remque_head(&lp->wait_ccbq);
if (!qp)
break;
++lp->queuedccbs;
cp = xpt_que_entry(qp, struct ccb, link_ccbq);
xpt_insque_tail(qp, &lp->busy_ccbq);
lp->tasktbl[cp->tag == NO_TAG ? 0 : cp->tag] =
cpu_to_scr(cp->p_ccb);
ncr_put_start_queue(np, cp);
}
}
static void ncr_put_start_queue(ncb_p np, ccb_p cp)
{
u_short	qidx;
#ifdef SCSI_NCR_IARB_SUPPORT
if (np->last_cp && np->iarb_count < np->iarb_max) {
np->last_cp->host_flags |= HF_HINT_IARB;
++np->iarb_count;
}
else
np->iarb_count = 0;
np->last_cp = cp;
#endif
qidx = np->squeueput + 2;
if (qidx >= MAX_START*2) qidx = 0;
np->squeue [qidx]	   = cpu_to_scr(np->p_idletask);
MEMORY_BARRIER();
np->squeue [np->squeueput] = cpu_to_scr(cp->p_ccb);
np->squeueput = qidx;
cp->queued = 1;
if (DEBUG_FLAGS & DEBUG_QUEUE)
printk ("%s: queuepos=%d.\n", ncr_name (np), np->squeueput);
MEMORY_BARRIER();
OUTB (nc_istat, SIGP|np->istat_sem);
}
static void ncr_chip_reset (ncb_p np)
{
OUTB (nc_istat, SRST);
UDELAY (10);
OUTB (nc_istat, 0);
}
static void ncr_soft_reset(ncb_p np)
{
u_char istat;
int i;
OUTB (nc_istat, CABRT);
for (i = 1000000 ; i ; --i) {
istat = INB (nc_istat);
if (istat & SIP) {
INW (nc_sist);
continue;
}
if (istat & DIP) {
OUTB (nc_istat, 0);
INB (nc_dstat);
break;
}
}
if (!i)
printk("%s: unable to abort current chip operation.\n",
ncr_name(np));
ncr_chip_reset(np);
}
static void ncr_start_reset(ncb_p np)
{
(void) ncr_reset_scsi_bus(np, 1, driver_setup.settle_delay);
}
static int ncr_reset_scsi_bus(ncb_p np, int enab_int, int settle_delay)
{
u_int32 term;
int retv = 0;
np->settle_time	= ktime_get(settle_delay * HZ);
if (bootverbose > 1)
printk("%s: resetting, "
"command processing suspended for %d seconds\n",
ncr_name(np), settle_delay);
ncr_soft_reset(np);
UDELAY (2000);
if (enab_int)
OUTW (nc_sien, RST);
OUTB (nc_stest3, TE);
OUTB (nc_dcntl, (np->rv_dcntl & IRQM));
OUTB (nc_scntl1, CRST);
UDELAY (200);
if (!driver_setup.bus_check)
goto out;
term =	INB(nc_sstat0);
term =	((term & 2) << 7) + ((term & 1) << 17);
term |= ((INB(nc_sstat2) & 0x01) << 26) |
((INW(nc_sbdl) & 0xff)   << 9)  |
((INW(nc_sbdl) & 0xff00) << 10) |
INB(nc_sbcl);
if (!(np->features & FE_WIDE))
term &= 0x3ffff;
if (term != (2<<7)) {
printk("%s: suspicious SCSI data while resetting the BUS.\n",
ncr_name(np));
printk("%s: %sdp0,d7-0,rst,req,ack,bsy,sel,atn,msg,c/d,i/o = "
"0x%lx, expecting 0x%lx\n",
ncr_name(np),
(np->features & FE_WIDE) ? "dp1,d15-8," : "",
(u_long)term, (u_long)(2<<7));
if (driver_setup.bus_check == 1)
retv = 1;
}
out:
OUTB (nc_scntl1, 0);
return retv;
}
static int ncr_reset_bus (ncb_p np, Scsi_Cmnd *cmd, int sync_reset)
{
ccb_p cp;
int found;
if (np->settle_time) {
return SCSI_RESET_PUNT;
}
ncr_start_reset(np);
for (found=0, cp=np->ccbc; cp; cp=cp->link_ccb) {
if (cp->host_status == HS_IDLE) continue;
if (cp->cmd == cmd) {
found = 1;
break;
}
}
if (!found && retrieve_from_waiting_list(0, np, cmd))
found = 1;
reset_waiting_list(np);
ncr_wakeup(np, HS_RESET);
if (!found && sync_reset && !retrieve_from_waiting_list(0, np, cmd)) {
SetScsiResult(cmd, DID_RESET, 0);
ncr_queue_done_cmd(np, cmd);
}
return SCSI_RESET_SUCCESS;
}
static int ncr_abort_command (ncb_p np, Scsi_Cmnd *cmd)
{
ccb_p cp;
if (remove_from_waiting_list(np, cmd)) {
SetScsiAbortResult(cmd);
ncr_queue_done_cmd(np, cmd);
return SCSI_ABORT_SUCCESS;
}
for (cp=np->ccbc; cp; cp=cp->link_ccb) {
if (cp->host_status == HS_IDLE) continue;
if (cp->cmd == cmd)
break;
}
if (!cp) {
return SCSI_ABORT_NOT_RUNNING;
}
cp->to_abort = 1;
np->istat_sem = SEM;
OUTB (nc_istat, SIGP|SEM);
return SCSI_ABORT_PENDING;
}
#ifdef MODULE
static int ncr_detach(ncb_p np)
{
int i;
printk("%s: detaching ...\n", ncr_name(np));
np->release_stage = 1;
for (i = 50 ; i && np->release_stage != 2 ; i--) MDELAY (100);
if (np->release_stage != 2)
printk("%s: the timer seems to be already stopped\n",
ncr_name(np));
else np->release_stage = 2;
printk("%s: resetting chip\n", ncr_name(np));
ncr_chip_reset(np);
OUTB(nc_dmode,	np->sv_dmode);
OUTB(nc_dcntl,	np->sv_dcntl);
OUTB(nc_ctest3,	np->sv_ctest3);
OUTB(nc_ctest4,	np->sv_ctest4);
OUTB(nc_ctest5,	np->sv_ctest5);
OUTB(nc_gpcntl,	np->sv_gpcntl);
OUTB(nc_stest2,	np->sv_stest2);
ncr_selectclock(np, np->sv_scntl3);
ncr_free_resources(np);
return 1;
}
#endif
void ncr_complete (ncb_p np, ccb_p cp)
{
Scsi_Cmnd *cmd;
tcb_p tp;
lcb_p lp;
if (!cp || !cp->cmd)
return;
if (DEBUG_FLAGS & DEBUG_TINY)
printk ("CCB=%lx STAT=%x/%x\n", (unsigned long)cp,
cp->host_status,cp->scsi_status);
cmd = cp->cmd;
cp->cmd = NULL;
tp = &np->target[cp->target];
lp = ncr_lp(np, tp, cp->lun);
if (cp == tp->nego_cp)
tp->nego_cp = 0;
#ifdef SCSI_NCR_IARB_SUPPORT
if (cp == np->last_cp)
np->last_cp = 0;
#endif
if (cp->host_flags & HF_AUTO_SENSE) {
cp->scsi_status = cp->sv_scsi_status;
cp->xerr_status = cp->sv_xerr_status;
}
else {
cp->resid = 0;
if (cp->xerr_status ||
cp->phys.header.lastp != cp->phys.header.goalp)
cp->resid = ncr_compute_residual(np, cp);
}
if (cp->xerr_status) {
if (cp->xerr_status & XE_PARITY_ERR) {
PRINT_ADDR(cmd);
printk ("unrecovered SCSI parity error.\n");
}
if (cp->xerr_status & XE_EXTRA_DATA) {
PRINT_ADDR(cmd);
printk ("extraneous data discarded.\n");
}
if (cp->xerr_status & XE_BAD_PHASE) {
PRINT_ADDR(cmd);
printk ("illegal scsi phase (4/5).\n");
}
if (cp->xerr_status & XE_SODL_UNRUN) {
PRINT_ADDR(cmd);
printk ("ODD transfer in DATA OUT phase.\n");
}
if (cp->xerr_status & XE_SWIDE_OVRUN){
PRINT_ADDR(cmd);
printk ("ODD transfer in DATA IN phase.\n");
}
if (cp->host_status==HS_COMPLETE)
cp->host_status = HS_FAIL;
}
if (DEBUG_FLAGS & (DEBUG_RESULT|DEBUG_TINY)) {
if (cp->host_status!=HS_COMPLETE || cp->scsi_status!=S_GOOD ||
cp->resid) {
PRINT_ADDR(cmd);
printk ("ERROR: cmd=%x host_status=%x scsi_status=%x "
"data_len=%d residual=%d\n",
cmd->cmnd[0], cp->host_status, cp->scsi_status,
cp->data_len, cp->resid);
}
}
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,3,99)
cmd->resid = cp->resid;
#endif
if (   (cp->host_status == HS_COMPLETE)
&& (cp->scsi_status == S_GOOD ||
cp->scsi_status == S_COND_MET)) {
SetScsiResult(cmd, DID_OK, cp->scsi_status);
if (!lp)
ncr_alloc_lcb (np, cp->target, cp->lun);
if (cmd->cmnd[0] == 0x12 && !(cmd->cmnd[1] & 0x3) &&
cmd->cmnd[4] >= 7 && !cmd->use_sg) {
sync_scsi_data(np, cmd);
ncr_setup_lcb (np, cp->target, cp->lun,
(char *) cmd->request_buffer);
}
if (lp && lp->usetags && lp->numtags < lp->maxtags) {
++lp->num_good;
if (lp->num_good >= 1000) {
lp->num_good = 0;
++lp->numtags;
ncr_setup_tags (np, cp->target, cp->lun);
}
}
} else if ((cp->host_status == HS_COMPLETE)
&& (cp->scsi_status == S_CHECK_COND)) {
SetScsiResult(cmd, DID_OK, S_CHECK_COND);
if (DEBUG_FLAGS & (DEBUG_RESULT|DEBUG_TINY)) {
PRINT_ADDR(cmd);
ncr_printl_hex("sense data:", cmd->sense_buffer, 14);
}
} else if ((cp->host_status == HS_COMPLETE)
&& (cp->scsi_status == S_CONFLICT)) {
SetScsiResult(cmd, DID_OK, S_CONFLICT);
} else if ((cp->host_status == HS_COMPLETE)
&& (cp->scsi_status == S_BUSY ||
cp->scsi_status == S_QUEUE_FULL)) {
SetScsiResult(cmd, DID_OK, cp->scsi_status);
} else if ((cp->host_status == HS_SEL_TIMEOUT)
|| (cp->host_status == HS_TIMEOUT)) {
SetScsiResult(cmd, DID_TIME_OUT, cp->scsi_status);
} else if (cp->host_status == HS_RESET) {
SetScsiResult(cmd, DID_RESET, cp->scsi_status);
} else if (cp->host_status == HS_ABORTED) {
SetScsiAbortResult(cmd);
} else {
int did_status;
PRINT_ADDR(cmd);
printk ("COMMAND FAILED (%x %x) @%p.\n",
cp->host_status, cp->scsi_status, cp);
did_status = DID_ERROR;
if (cp->xerr_status & XE_PARITY_ERR)
did_status = DID_PARITY;
SetScsiResult(cmd, did_status, cp->scsi_status);
}
if (tp->usrflag & UF_TRACE) {
PRINT_ADDR(cmd);
printk (" CMD:");
ncr_print_hex(cmd->cmnd, cmd->cmd_len);
if (cp->host_status==HS_COMPLETE) {
switch (cp->scsi_status) {
case S_GOOD:
printk ("  GOOD");
break;
case S_CHECK_COND:
printk ("  SENSE:");
ncr_print_hex(cmd->sense_buffer, 14);
break;
default:
printk ("  STAT: %x\n", cp->scsi_status);
break;
}
} else printk ("  HOSTERROR: %x", cp->host_status);
printk ("\n");
}
ncr_free_ccb (np, cp);
if (lp && lp->queuedccbs < lp->queuedepth &&
!xpt_que_empty(&lp->wait_ccbq))
ncr_start_next_ccb(np, lp, 2);
if (np->waiting_list)
requeue_waiting_list(np);
ncr_queue_done_cmd(np, cmd);
}
int ncr_wakeup_done (ncb_p np)
{
ccb_p cp;
int i, n;
u_long dsa;
n = 0;
i = np->dqueueget;
while (1) {
dsa = scr_to_cpu(np->dqueue[i]);
if (!dsa)
break;
np->dqueue[i] = 0;
if ((i = i+2) >= MAX_START*2)
i = 0;
cp = ncr_ccb_from_dsa(np, dsa);
if (cp) {
MEMORY_BARRIER();
ncr_complete (np, cp);
++n;
}
else
printk (KERN_ERR "%s: bad DSA (%lx) in done queue.\n",
ncr_name(np), dsa);
}
np->dqueueget = i;
return n;
}
void ncr_wakeup (ncb_p np, u_long code)
{
ccb_p cp = np->ccbc;
while (cp) {
if (cp->host_status != HS_IDLE) {
cp->host_status = code;
ncr_complete (np, cp);
}
cp = cp->link_ccb;
}
}
void ncr_init (ncb_p np, int reset, char * msg, u_long code)
{
int	i;
u_long	phys;
if (reset)
ncr_soft_reset(np);
else {
OUTB (nc_stest3, TE|CSF);
OUTONB (nc_ctest3, CLF);
}
if (msg) printk (KERN_INFO "%s: restart (%s).\n", ncr_name (np), msg);
phys = np->p_squeue;
np->queuedepth = MAX_START - 1;
for (i = 0; i < MAX_START*2; i += 2) {
np->squeue[i]   = cpu_to_scr(np->p_idletask);
np->squeue[i+1] = cpu_to_scr(phys + (i+2)*4);
}
np->squeue[MAX_START*2-1] = cpu_to_scr(phys);
np->squeueput = 0;
np->scripth0->startpos[0] = cpu_to_scr(phys);
phys = vtobus(np->dqueue);
for (i = 0; i < MAX_START*2; i += 2) {
np->dqueue[i]   = 0;
np->dqueue[i+1] = cpu_to_scr(phys + (i+2)*4);
}
np->dqueue[MAX_START*2-1] = cpu_to_scr(phys);
np->scripth0->done_pos[0] = cpu_to_scr(phys);
np->dqueueget = 0;
ncr_wakeup (np, code);
OUTB (nc_istat,  0x00   );
UDELAY (2000);
OUTB (nc_scntl0, np->rv_scntl0 | 0xc0);
OUTB (nc_scntl1, 0x00);
ncr_selectclock(np, np->rv_scntl3);
OUTB (nc_scid  , RRE|np->myaddr);
OUTW (nc_respid, 1ul<<np->myaddr);
OUTB (nc_istat , SIGP	);
OUTB (nc_dmode , np->rv_dmode);
OUTB (nc_ctest5, np->rv_ctest5);
OUTB (nc_dcntl , NOCOM|np->rv_dcntl);
OUTB (nc_ctest3, np->rv_ctest3);
OUTB (nc_ctest4, np->rv_ctest4);
if ((np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66)){
OUTB (nc_stest2, EXT|np->rv_stest2);
}
OUTB (nc_stest3, TE);
OUTB (nc_stime0, 0x0c);
if (np->device_id == PCI_DEVICE_ID_NCR_53C875)
OUTB (nc_ctest0, (1<<5));
else if (np->device_id == PCI_DEVICE_ID_NCR_53C896  ||
np->device_id == PCI_DEVICE_ID_LSI_53C1010 ||
np->device_id == PCI_DEVICE_ID_LSI_53C1010_66 )
np->rv_ccntl0 |= DPR;
if (np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)
OUTB(nc_aipcntl1, (1<<3));
if (np->features & FE_64BIT) {
OUTB (nc_ccntl0, np->rv_ccntl0);
OUTB (nc_ccntl1, np->rv_ccntl1);
}
if (np->features & FE_NOPM) {
printk(KERN_INFO "%s: handling phase mismatch from SCRIPTS.\n",
ncr_name(np));
OUTL (nc_pmjad1, NCB_SCRIPTH_PHYS (np, pm_handle));
OUTL (nc_pmjad2, NCB_SCRIPTH_PHYS (np, pm_handle));
}
if (np->features & FE_LED0)
OUTB(nc_gpcntl, INB(nc_gpcntl) & ~0x01);
else if (np->features & FE_LEDC)
OUTB(nc_gpcntl, (INB(nc_gpcntl) & ~0x41) | 0x20);
OUTW (nc_sien , STO|HTH|MA|SGE|UDC|RST|PAR);
OUTB (nc_dien , MDPE|BF|SSI|SIR|IID);
if ( (np->features & FE_ULTRA2) || (np->features & FE_ULTRA3) ) {
OUTONW (nc_sien, SBMC);
np->scsi_mode = INB (nc_stest4) & SMODE;
}
for (i=0;i<MAX_TARGET;i++) {
tcb_p tp = &np->target[i];
tp->to_reset = 0;
tp->sval    = 0;
tp->wval    = np->rv_scntl3;
tp->uval    = np->rv_scntl4;
if (tp->usrsync != 255) {
if (tp->usrsync <= np->maxsync) {
if (tp->usrsync < np->minsync) {
tp->usrsync = np->minsync;
}
}
else
tp->usrsync = 255;
};
if (tp->usrwide > np->maxwide)
tp->usrwide = np->maxwide;
ncr_negotiate (np, tp);
}
if (np->base2_ba) {
if (bootverbose)
printk ("%s: Downloading SCSI SCRIPTS.\n",
ncr_name(np));
#ifdef SCSI_NCR_PCI_MEM_NOT_SUPPORTED
if (np->base2_ws == 8192)
phys = NCB_SCRIPTH0_PHYS (np, start_ram64);
else
phys = NCB_SCRIPTH_PHYS (np, start_ram);
#else
if (np->base2_ws == 8192) {
memcpy_to_pci(np->base2_va + 4096,
np->scripth0, sizeof(struct scripth));
OUTL (nc_mmws, np->scr_ram_seg);
OUTL (nc_mmrs, np->scr_ram_seg);
OUTL (nc_sfs,  np->scr_ram_seg);
phys = NCB_SCRIPTH_PHYS (np, start64);
}
else
phys = NCB_SCRIPT_PHYS (np, init);
memcpy_to_pci(np->base2_va, np->script0, sizeof(struct script));
#endif
}
else
phys = NCB_SCRIPT_PHYS (np, init);
np->istat_sem = 0;
OUTL (nc_dsa, np->p_ncb);
OUTL_DSP (phys);
}
static void ncr_negotiate (struct ncb* np, struct tcb* tp)
{
u_long minsync = tp->usrsync;
if (np->scsi_mode && np->scsi_mode == SMODE_SE) {
if (minsync < 12) minsync = 12;
}
if (minsync < np->minsync)
minsync = np->minsync;
if (minsync > np->maxsync)
minsync = 255;
tp->minsync = minsync;
tp->maxoffs = (minsync<255 ? np->maxoffs : 0);
tp->period=0;
tp->widedone=0;
}
static void ncr_getsync(ncb_p np, u_char sfac, u_char *fakp, u_char *scntl3p)
{
u_long	clk = np->clock_khz;
int	div = np->clock_divn;
u_long	fak;
u_long	per;
u_long	kpc;
if	(sfac <= 10)	per = 250;
else if	(sfac == 11)	per = 303;
else if	(sfac == 12)	per = 500;
else			per = 40 * sfac;
kpc = per * clk;
while (--div >= 0)
if (kpc >= (div_10M[div] << 2)) break;
fak = (kpc - 1) / div_10M[div] + 1;
#if 0
per = (fak * div_10M[div]) / clk;
if (div >= 1 && fak < 8) {
u_long fak2, per2;
fak2 = (kpc - 1) / div_10M[div-1] + 1;
per2 = (fak2 * div_10M[div-1]) / clk;
if (per2 < per && fak2 <= 8) {
fak = fak2;
per = per2;
--div;
}
}
#endif
if (fak < 4) fak = 4;
*fakp		= fak - 4;
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010)  ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)) {
*scntl3p	= (div+1) << 4;
*fakp		= 0;
}
else {
*scntl3p	= ((div+1) << 4) + (sfac < 25 ? 0x80 : 0);
*fakp		= fak - 4;
}
}
static void ncr_get_xfer_info(ncb_p np, tcb_p tp, u_char *factor,
u_char *offset, u_char *width)
{
u_char idiv;
u_long period;
*width = (tp->wval & EWS) ? 1 : 0;
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66))
*offset  = (tp->sval & 0x3f);
else
*offset  = (tp->sval & 0x1f);
idiv = (tp->wval>>4) & 0x07;
if ( *offset && idiv ) {
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)){
if (tp->uval & 0x80)
period = (2*div_10M[idiv-1])/np->clock_khz;
else
period = (4*div_10M[idiv-1])/np->clock_khz;
}
else
period = (((tp->sval>>5)+4)*div_10M[idiv-1])/np->clock_khz;
}
else
period = 0xffff;
if	(period <= 125)		*factor =   9;
else if	(period <= 250)		*factor =  10;
else if	(period <= 303)		*factor  = 11;
else if	(period <= 500)		*factor  = 12;
else				*factor  = (period + 40 - 1) / 40;
}
static void ncr_set_sync_wide_status (ncb_p np, u_char target)
{
ccb_p cp = np->ccbc;
tcb_p tp = &np->target[target];
OUTB (nc_sxfer, tp->sval);
OUTB (nc_scntl3, tp->wval);
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010)  ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66))
OUTB (nc_scntl4, tp->uval);
for (cp = np->ccbc; cp; cp = cp->link_ccb) {
if (cp->host_status == HS_IDLE)
continue;
if (cp->target != target)
continue;
cp->phys.select.sel_scntl3 = tp->wval;
cp->phys.select.sel_sxfer  = tp->sval;
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66))
cp->phys.select.sel_scntl4 = tp->uval;
};
}
static void ncr_setsync (ncb_p np, ccb_p cp, u_char scntl3, u_char sxfer,
u_char scntl4)
{
tcb_p tp;
u_char target = INB (nc_sdid) & 0x0f;
u_char idiv;
u_char offset;
assert (cp);
if (!cp) return;
assert (target == (cp->target & 0xf));
tp = &np->target[target];
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)) {
offset = sxfer & 0x3f;
scntl3 = (scntl3 & 0xf0) | (tp->wval & EWS);
scntl4 = (scntl4 & 0x80);
}
else {
offset = sxfer & 0x1f;
if (!scntl3 || !offset)
scntl3 = np->rv_scntl3;
scntl3 = (scntl3 & 0xf0) | (tp->wval & EWS) |
(np->rv_scntl3 & 0x07);
}
idiv = ((scntl3 >> 4) & 0x7);
if ( offset && idiv) {
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)) {
if (scntl4 & 0x80)
tp->period = (2*div_10M[idiv-1])/np->clock_khz;
else
tp->period = (4*div_10M[idiv-1])/np->clock_khz;
}
else
tp->period = (((sxfer>>5)+4)*div_10M[idiv-1])/np->clock_khz;
}
else
tp->period = 0xffff;
if (tp->sval == sxfer && tp->wval == scntl3 && tp->uval == scntl4) return;
tp->sval = sxfer;
tp->wval = scntl3;
tp->uval = scntl4;
if ( bootverbose < 2 && (cp->host_flags & HF_AUTO_SENSE))
goto next;
PRINT_TARGET(np, target);
if (offset) {
unsigned f10 = 100000 << (tp->widedone ? tp->widedone -1 : 0);
unsigned mb10 = (f10 + tp->period/2) / tp->period;
char *scsi;
if ((tp->period <= 2000) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66))
OUTOFFB (nc_stest2, EXT);
if	(tp->period < 250)	scsi = "FAST-80";
else if	(tp->period < 500)	scsi = "FAST-40";
else if	(tp->period < 1000)	scsi = "FAST-20";
else if	(tp->period < 2000)	scsi = "FAST-10";
else				scsi = "FAST-5";
printk ("%s %sSCSI %d.%d MB/s (%d ns, offset %d)\n", scsi,
tp->widedone > 1 ? "WIDE " : "",
mb10 / 10, mb10 % 10, tp->period / 10, offset);
} else
printk ("%sasynchronous.\n", tp->widedone > 1 ? "wide " : "");
next:
ncr_set_sync_wide_status(np, target);
}
static void ncr_setwide (ncb_p np, ccb_p cp, u_char wide, u_char ack)
{
u_short target = INB (nc_sdid) & 0x0f;
tcb_p tp;
u_char	scntl3;
u_char	sxfer;
assert (cp);
if (!cp) return;
assert (target == (cp->target & 0xf));
tp = &np->target[target];
tp->widedone  =  wide+1;
scntl3 = (tp->wval & (~EWS)) | (wide ? EWS : 0);
sxfer = ack ? 0 : tp->sval;
if (tp->sval == sxfer && tp->wval == scntl3) return;
tp->sval = sxfer;
tp->wval = scntl3;
if (bootverbose >= 2) {
PRINT_TARGET(np, target);
if (scntl3 & EWS)
printk ("WIDE SCSI (16 bit) enabled.\n");
else
printk ("WIDE SCSI disabled.\n");
}
ncr_set_sync_wide_status(np, target);
}
static void ncr_setsyncwide (ncb_p np, ccb_p cp, u_char scntl3, u_char sxfer,
u_char scntl4, u_char wide)
{
tcb_p tp;
u_char target = INB (nc_sdid) & 0x0f;
u_char idiv;
u_char offset;
assert (cp);
if (!cp) return;
assert (target == (cp->target & 0xf));
tp = &np->target[target];
tp->widedone  =  wide+1;
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)) {
offset = sxfer & 0x3f;
scntl3 = (scntl3 & 0xf0) | (wide ? EWS : 0);
scntl4 = (scntl4 & 0x80);
}
else {
offset = sxfer & 0x1f;
if (!scntl3 || !offset)
scntl3 = np->rv_scntl3;
scntl3 = (scntl3 & 0xf0) | (wide ? EWS : 0) |
(np->rv_scntl3 & 0x07);
}
idiv = ((scntl3 >> 4) & 0x7);
if ( offset && idiv) {
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)) {
if (scntl4 & 0x80)
tp->period = (2*div_10M[idiv-1])/np->clock_khz;
else
tp->period = (4*div_10M[idiv-1])/np->clock_khz;
}
else
tp->period = (((sxfer>>5)+4)*div_10M[idiv-1])/np->clock_khz;
}
else
tp->period = 0xffff;
if (tp->sval == sxfer && tp->wval == scntl3 && tp->uval == scntl4) return;
tp->sval = sxfer;
tp->wval = scntl3;
tp->uval = scntl4;
if ( bootverbose < 2 && (cp->host_flags & HF_AUTO_SENSE))
goto next;
PRINT_TARGET(np, target);
if (offset) {
unsigned f10 = 100000 << (tp->widedone ? tp->widedone -1 : 0);
unsigned mb10 = (f10 + tp->period/2) / tp->period;
char *scsi;
if ((tp->period <= 2000) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66))
OUTOFFB (nc_stest2, EXT);
if	(tp->period < 250)	scsi = "FAST-80";
else if	(tp->period < 500)	scsi = "FAST-40";
else if	(tp->period < 1000)	scsi = "FAST-20";
else if	(tp->period < 2000)	scsi = "FAST-10";
else				scsi = "FAST-5";
printk ("%s %sSCSI %d.%d MB/s (%d ns, offset %d)\n", scsi,
tp->widedone > 1 ? "WIDE " : "",
mb10 / 10, mb10 % 10, tp->period / 10, offset);
} else
printk ("%sasynchronous.\n", tp->widedone > 1 ? "wide " : "");
next:
ncr_set_sync_wide_status(np, target);
}
static void ncr_setup_tags (ncb_p np, u_char tn, u_char ln)
{
tcb_p tp = &np->target[tn];
lcb_p lp = ncr_lp(np, tp, ln);
u_short reqtags, maxdepth;
if ((!tp) || (!lp))
return;
if (!lp->scdev_depth)
return;
maxdepth = lp->scdev_depth;
if (maxdepth > lp->maxnxs)	maxdepth    = lp->maxnxs;
if (lp->maxtags > maxdepth)	lp->maxtags = maxdepth;
if (lp->numtags > maxdepth)	lp->numtags = maxdepth;
if ((lp->inq_byte7 & INQ7_QUEUE) && lp->numtags > 1) {
reqtags = lp->numtags;
} else {
reqtags = 1;
};
lp->numtags = reqtags;
if (lp->numtags > lp->maxtags)
lp->maxtags = lp->numtags;
if	(reqtags > 1 && lp->usetags) {
if (lp->queuedepth == reqtags)
return;
lp->queuedepth	= reqtags;
}
else if	(reqtags <= 1 && !lp->usetags) {
lp->queuedepth	= reqtags;
return;
}
else {
if (lp->busyccbs)
return;
lp->queuedepth	= reqtags;
lp->usetags	= reqtags > 1 ? 1 : 0;
}
lp->resel_task = lp->usetags?
cpu_to_scr(NCB_SCRIPT_PHYS(np, resel_tag)) :
cpu_to_scr(NCB_SCRIPT_PHYS(np, resel_notag));
if (bootverbose) {
PRINT_LUN(np, tn, ln);
if (lp->usetags)
printk("tagged command queue depth set to %d\n", reqtags);
else
printk("tagged command queueing disabled\n");
}
}
#ifdef SCSI_NCR_USER_COMMAND_SUPPORT
static void ncr_usercmd (ncb_p np)
{
u_char t;
tcb_p tp;
int ln;
u_long size;
switch (np->user.cmd) {
case 0: return;
case UC_SETDEBUG:
#ifdef SCSI_NCR_DEBUG_INFO_SUPPORT
ncr_debug = np->user.data;
#endif
break;
case UC_SETORDER:
np->order = np->user.data;
break;
case UC_SETVERBOSE:
np->verbose = np->user.data;
break;
default:
for (t = 0; t < MAX_TARGET; t++) {
if (!((np->user.target >> t) & 1))
continue;
tp = &np->target[t];
switch (np->user.cmd) {
case UC_SETSYNC:
tp->usrsync = np->user.data;
ncr_negotiate (np, tp);
break;
case UC_SETWIDE:
size = np->user.data;
if (size > np->maxwide)
size=np->maxwide;
tp->usrwide = size;
ncr_negotiate (np, tp);
break;
case UC_SETTAGS:
tp->usrtags = np->user.data;
for (ln = 0; ln < MAX_LUN; ln++) {
lcb_p lp;
lp = ncr_lp(np, tp, ln);
if (!lp)
continue;
lp->numtags = np->user.data;
lp->maxtags = lp->numtags;
ncr_setup_tags (np, t, ln);
}
break;
case UC_RESETDEV:
tp->to_reset = 1;
np->istat_sem = SEM;
OUTB (nc_istat, SIGP|SEM);
break;
case UC_CLEARDEV:
for (ln = 0; ln < MAX_LUN; ln++) {
lcb_p lp;
lp = ncr_lp(np, tp, ln);
if (lp)
lp->to_clear = 1;
}
np->istat_sem = SEM;
OUTB (nc_istat, SIGP|SEM);
break;
case UC_SETFLAG:
tp->usrflag = np->user.data;
break;
}
}
break;
}
np->user.cmd=0;
}
#endif
static void ncr_timeout (ncb_p np)
{
u_long	thistime = ktime_get(0);
if (np->release_stage) {
if (np->release_stage == 1) np->release_stage = 2;
return;
}
#ifdef SCSI_NCR_PCIQ_BROKEN_INTR
np->timer.expires = ktime_get((HZ+9)/10);
#else
np->timer.expires = ktime_get(SCSI_NCR_TIMER_INTERVAL);
#endif
add_timer(&np->timer);
if (np->settle_time) {
if (np->settle_time <= thistime) {
if (bootverbose > 1)
printk("%s: command processing resumed\n", ncr_name(np));
np->settle_time	= 0;
requeue_waiting_list(np);
}
return;
}
if (np->lasttime + 4*HZ < thistime) {
np->lasttime = thistime;
}
#ifdef SCSI_NCR_PCIQ_MAY_MISS_COMPLETIONS
ncr_wakeup_done(np);
#endif
#ifdef SCSI_NCR_PCIQ_BROKEN_INTR
if (INB(nc_istat) & (INTF|SIP|DIP)) {
if (DEBUG_FLAGS & DEBUG_TINY) printk ("{");
ncr_exception (np);
if (DEBUG_FLAGS & DEBUG_TINY) printk ("}");
}
#endif
}
static void ncr_log_hard_error(ncb_p np, u_short sist, u_char dstat)
{
u_int32	dsp;
int	script_ofs;
int	script_size;
char	*script_name;
u_char	*script_base;
int	i;
dsp	= INL (nc_dsp);
if (dsp > np->p_script && dsp <= np->p_script + sizeof(struct script)) {
script_ofs	= dsp - np->p_script;
script_size	= sizeof(struct script);
script_base	= (u_char *) np->script0;
script_name	= "script";
}
else if (np->p_scripth < dsp &&
dsp <= np->p_scripth + sizeof(struct scripth)) {
script_ofs	= dsp - np->p_scripth;
script_size	= sizeof(struct scripth);
script_base	= (u_char *) np->scripth0;
script_name	= "scripth";
} else {
script_ofs	= dsp;
script_size	= 0;
script_base	= 0;
script_name	= "mem";
}
printk ("%s:%d: ERROR (%x:%x) (%x-%x-%x) (%x/%x) @ (%s %x:%08x).\n",
ncr_name (np), (unsigned)INB (nc_sdid)&0x0f, dstat, sist,
(unsigned)INB (nc_socl), (unsigned)INB (nc_sbcl), (unsigned)INB (nc_sbdl),
(unsigned)INB (nc_sxfer),(unsigned)INB (nc_scntl3), script_name, script_ofs,
(unsigned)INL (nc_dbc));
if (((script_ofs & 3) == 0) &&
(unsigned)script_ofs < script_size) {
printk ("%s: script cmd = %08x\n", ncr_name(np),
scr_to_cpu((int) *(ncrcmd *)(script_base + script_ofs)));
}
printk ("%s: regdump:", ncr_name(np));
for (i=0; i<24;i++)
printk (" %02x", (unsigned)INB_OFF(i));
printk (".\n");
}
void ncr_exception (ncb_p np)
{
u_char	istat, istatc;
u_char	dstat;
u_short	sist;
int	i;
istat = INB (nc_istat);
if (istat & INTF) {
OUTB (nc_istat, (istat & SIGP) | INTF | np->istat_sem);
istat = INB (nc_istat);
if (DEBUG_FLAGS & DEBUG_TINY) printk ("F ");
(void)ncr_wakeup_done (np);
};
if (!(istat & (SIP|DIP)))
return;
#if 0
if (istat & CABRT)
OUTB (nc_istat, CABRT);
#endif
sist	= 0;
dstat	= 0;
istatc	= istat;
do {
if (istatc & SIP)
sist  |= INW (nc_sist);
if (istatc & DIP)
dstat |= INB (nc_dstat);
istatc = INB (nc_istat);
istat |= istatc;
} while (istatc & (SIP|DIP));
if (DEBUG_FLAGS & DEBUG_TINY)
printk ("<%d|%x:%x|%x:%x>",
(int)INB(nc_scr0),
dstat,sist,
(unsigned)INL(nc_dsp),
(unsigned)INL(nc_dbc));
MEMORY_BARRIER();
if (!(sist  & (STO|GEN|HTH|SGE|UDC|SBMC|RST)) &&
!(dstat & (MDPE|BF|ABRT|IID))) {
if	(sist & PAR)	ncr_int_par (np, sist);
else if (sist & MA)	ncr_int_ma (np);
else if (dstat & SIR)	ncr_int_sir (np);
else if (dstat & SSI)	OUTONB_STD ();
else			goto unknown_int;
return;
};
if (sist & RST) {
ncr_init (np, 1, bootverbose ? "scsi reset" : NULL, HS_RESET);
return;
};
OUTB (nc_ctest3, np->rv_ctest3 | CLF);
OUTB (nc_stest3, TE|CSF);
if (!(sist  & (GEN|HTH|SGE)) &&
!(dstat & (MDPE|BF|ABRT|IID))) {
if	(sist & SBMC)	ncr_int_sbmc (np);
else if (sist & STO)	ncr_int_sto (np);
else if (sist & UDC)	ncr_int_udc (np);
else			goto unknown_int;
return;
};
if (ktime_exp(np->regtime)) {
np->regtime = ktime_get(10*HZ);
for (i = 0; i<sizeof(np->regdump); i++)
((char*)&np->regdump)[i] = INB_OFF(i);
np->regdump.nc_dstat = dstat;
np->regdump.nc_sist  = sist;
};
ncr_log_hard_error(np, sist, dstat);
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)) {
u_char ctest4_o, ctest4_m;
u_char shadow;
ctest4_o = INB(nc_ctest4);
OUTB(nc_ctest4, ctest4_o | 0x10);
ctest4_m = INB(nc_ctest4);
shadow = INW_OFF(0x42);
OUTB(nc_ctest4, ctest4_o);
printk("%s: ctest4/sist original 0x%x/0x%X  mod: 0x%X/0x%x\n",
ncr_name(np), ctest4_o, sist, ctest4_m, shadow);
}
if ((sist & (GEN|HTH|SGE)) ||
(dstat & (MDPE|BF|ABRT|IID))) {
ncr_start_reset(np);
return;
};
unknown_int:
printk(	"%s: unknown interrupt(s) ignored, "
"ISTAT=0x%x DSTAT=0x%x SIST=0x%x\n",
ncr_name(np), istat, dstat, sist);
}
static void ncr_recover_scsi_int (ncb_p np, u_char hsts)
{
u_int32	dsp	= INL (nc_dsp);
u_int32	dsa	= INL (nc_dsa);
ccb_p cp	= ncr_ccb_from_dsa(np, dsa);
if ((!(dsp > NCB_SCRIPT_PHYS (np, getjob_begin) &&
dsp < NCB_SCRIPT_PHYS (np, getjob_end) + 1)) &&
(!(dsp > NCB_SCRIPT_PHYS (np, ungetjob) &&
dsp < NCB_SCRIPT_PHYS (np, reselect) + 1)) &&
(!(dsp > NCB_SCRIPTH_PHYS (np, sel_for_abort) &&
dsp < NCB_SCRIPTH_PHYS (np, sel_for_abort_1) + 1)) &&
(!(dsp > NCB_SCRIPT_PHYS (np, done) &&
dsp < NCB_SCRIPT_PHYS (np, done_end) + 1))) {
if (cp) {
cp->host_status = hsts;
ncr_complete (np, cp);
}
OUTL (nc_dsa, DSA_INVALID);
OUTB (nc_ctest3, np->rv_ctest3 | CLF);
OUTB (nc_stest3, TE|CSF);
OUTL_DSP (NCB_SCRIPT_PHYS (np, start));
}
else
goto reset_all;
return;
reset_all:
ncr_start_reset(np);
}
void ncr_int_sto (ncb_p np)
{
u_int32	dsp	= INL (nc_dsp);
if (DEBUG_FLAGS & DEBUG_TINY) printk ("T");
if (dsp == NCB_SCRIPT_PHYS (np, wf_sel_done) + 8 ||
!(driver_setup.recovery & 1))
ncr_recover_scsi_int(np, HS_SEL_TIMEOUT);
else
ncr_start_reset(np);
}
void ncr_int_udc (ncb_p np)
{
u_int32 dsa = INL (nc_dsa);
ccb_p   cp  = ncr_ccb_from_dsa(np, dsa);
tcb_p   tp  = &np->target[cp->target];
if (tp->ppr_negotiation == 1)
tp->ppr_negotiation = 0;
printk ("%s: unexpected disconnect\n", ncr_name(np));
ncr_recover_scsi_int(np, HS_UNEXPECTED);
}
static void ncr_int_sbmc (ncb_p np)
{
u_char scsi_mode = INB (nc_stest4) & SMODE;
printk("%s: SCSI bus mode change from %x to %x.\n",
ncr_name(np), np->scsi_mode, scsi_mode);
np->scsi_mode = scsi_mode;
np->settle_time	= ktime_get(1*HZ);
ncr_init (np, 0, bootverbose ? "scsi mode change" : NULL, HS_RESET);
}
static void ncr_int_par (ncb_p np, u_short sist)
{
u_char	hsts	= INB (HS_PRT);
u_int32	dsp	= INL (nc_dsp);
u_int32	dbc	= INL (nc_dbc);
u_int32	dsa	= INL (nc_dsa);
u_char	sbcl	= INB (nc_sbcl);
u_char	cmd	= dbc >> 24;
int phase	= cmd & 7;
ccb_p	cp	= ncr_ccb_from_dsa(np, dsa);
printk("%s: SCSI parity error detected: SCR1=%d DBC=%x SBCL=%x\n",
ncr_name(np), hsts, dbc, sbcl);
if (!(INB (nc_scntl1) & ISCON)) {
if (!(driver_setup.recovery & 1)) {
ncr_recover_scsi_int(np, HS_FAIL);
return;
}
goto reset_all;
}
if (!cp)
goto reset_all;
if ((cmd & 0xc0) || !(phase & 1) || !(sbcl & 0x8))
goto reset_all;
OUTONB (HF_PRT, HF_EXT_ERR);
cp->xerr_status |= XE_PARITY_ERR;
np->msgout[0] = (phase == 7) ? M_PARITY : M_ID_ERROR;
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
if (np->check_integrity)
np->check_integ_par = np->msgout[0];
#endif
if ((phase == 1) || (phase == 5)) {
if (dsp == NCB_SCRIPTH_PHYS (np, pm_handle))
OUTL_DSP (dsp);
else if (sist & MA)
ncr_int_ma (np);
else {
OUTL (nc_temp, dsp);
OUTL_DSP (NCB_SCRIPT_PHYS (np, dispatch));
}
}
else
OUTL_DSP (NCB_SCRIPT_PHYS (np, clrack));
return;
reset_all:
ncr_start_reset(np);
return;
}
static void ncr_int_ma (ncb_p np)
{
u_int32	dbc;
u_int32	rest;
u_int32	dsp;
u_int32	dsa;
u_int32	nxtdsp;
u_int32	*vdsp;
u_int32	oadr, olen;
u_int32	*tblp;
u_int32	newcmd;
u_int	delta;
u_char	cmd;
u_char	hflags, hflags0;
struct pm_ctx *pm;
ccb_p	cp;
dsp	= INL (nc_dsp);
dbc	= INL (nc_dbc);
dsa	= INL (nc_dsa);
cmd	= dbc >> 24;
rest	= dbc & 0xffffff;
delta	= 0;
cp = ncr_ccb_from_dsa(np, dsa);
if (DEBUG_FLAGS & DEBUG_PHASE)
printk("CCB = %2x %2x %2x %2x %2x %2x\n",
cp->cmd->cmnd[0], cp->cmd->cmnd[1], cp->cmd->cmnd[2],
cp->cmd->cmnd[3], cp->cmd->cmnd[4], cp->cmd->cmnd[5]);
if ((cmd & 7) != 1 && (cmd & 7) != 5) {
u_int32 dfifo;
u_char ss0, ss2;
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66))
delta = INL(nc_dfbc) & 0xffff;
else {
dfifo = INL(nc_dfifo);
if (dfifo & (DFS << 16))
delta = ((((dfifo >> 8) & 0x300) |
(dfifo & 0xff)) - rest) & 0x3ff;
else
delta = ((dfifo & 0xff) - rest) & 0x7f;
}
rest += delta;
ss0  = INB (nc_sstat0);
if (ss0 & OLF) rest++;
if ((np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66) && (ss0 & ORF))
rest++;
if (cp && (cp->phys.select.sel_scntl3 & EWS)) {
ss2 = INB (nc_sstat2);
if (ss2 & OLF1) rest++;
if ((np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66) && (ss2 & ORF))
rest++;
};
OUTB (nc_ctest3, np->rv_ctest3 | CLF);
OUTB (nc_stest3, TE|CSF);
}
if (DEBUG_FLAGS & (DEBUG_TINY|DEBUG_PHASE))
printk ("P%x%x RL=%d D=%d ", cmd&7, INB(nc_sbcl)&7,
(unsigned) rest, (unsigned) delta);
vdsp	= 0;
nxtdsp	= 0;
if	(dsp >  np->p_script &&
dsp <= np->p_script + sizeof(struct script)) {
vdsp = (u_int32 *)((char*)np->script0 + (dsp-np->p_script-8));
nxtdsp = dsp;
}
else if	(dsp >  np->p_scripth &&
dsp <= np->p_scripth + sizeof(struct scripth)) {
vdsp = (u_int32 *)((char*)np->scripth0 + (dsp-np->p_scripth-8));
nxtdsp = dsp;
}
if (DEBUG_FLAGS & DEBUG_PHASE) {
printk ("\nCP=%p DSP=%x NXT=%x VDSP=%p CMD=%x ",
cp, (unsigned)dsp, (unsigned)nxtdsp, vdsp, cmd);
};
if (!vdsp) {
printk ("%s: interrupted SCRIPT address not found.\n",
ncr_name (np));
goto reset_all;
}
if (!cp) {
printk ("%s: SCSI phase error fixup: CCB already dequeued.\n",
ncr_name (np));
goto reset_all;
}
oadr = scr_to_cpu(vdsp[1]);
if (cmd & 0x10) {
tblp = (u_int32 *) ((char*) &cp->phys + oadr);
olen = scr_to_cpu(tblp[0]);
oadr = scr_to_cpu(tblp[1]);
} else {
tblp = (u_int32 *) 0;
olen = scr_to_cpu(vdsp[0]) & 0xffffff;
};
if (DEBUG_FLAGS & DEBUG_PHASE) {
printk ("OCMD=%x\nTBLP=%p OLEN=%x OADR=%x\n",
(unsigned) (scr_to_cpu(vdsp[0]) >> 24),
tblp,
(unsigned) olen,
(unsigned) oadr);
};
if (((cmd & 2) ? cmd : (cmd & ~4)) != (scr_to_cpu(vdsp[0]) >> 24)) {
PRINT_ADDR(cp->cmd);
printk ("internal error: cmd=%02x != %02x=(vdsp[0] >> 24)\n",
(unsigned)cmd, (unsigned)scr_to_cpu(vdsp[0]) >> 24);
goto reset_all;
};
if (cmd & 0x02) {
PRINT_ADDR(cp->cmd);
printk ("phase change %x-%x %d@%08x resid=%d.\n",
cmd&7, INB(nc_sbcl)&7, (unsigned)olen,
(unsigned)oadr, (unsigned)rest);
goto unexpected_phase;
};
hflags0 = INB (HF_PRT);
hflags = hflags0;
if (hflags & (HF_IN_PM0 | HF_IN_PM1 | HF_DP_SAVED)) {
if (hflags & HF_IN_PM0)
nxtdsp = scr_to_cpu(cp->phys.pm0.ret);
else if	(hflags & HF_IN_PM1)
nxtdsp = scr_to_cpu(cp->phys.pm1.ret);
if (hflags & HF_DP_SAVED)
hflags ^= HF_ACT_PM;
}
if (!(hflags & HF_ACT_PM)) {
pm = &cp->phys.pm0;
newcmd = NCB_SCRIPT_PHYS(np, pm0_data);
}
else {
pm = &cp->phys.pm1;
newcmd = NCB_SCRIPT_PHYS(np, pm1_data);
}
hflags &= ~(HF_IN_PM0 | HF_IN_PM1 | HF_DP_SAVED);
if (hflags != hflags0)
OUTB (HF_PRT, hflags);
pm->sg.addr = cpu_to_scr(oadr + olen - rest);
pm->sg.size = cpu_to_scr(rest);
pm->ret     = cpu_to_scr(nxtdsp);
nxtdsp = NCB_SCRIPT_PHYS (np, dispatch);
if ( ((cmd & 7) == 1  || (cmd & 7) == 5)
&& cp && (cp->phys.select.sel_scntl3 & EWS) &&
(INB (nc_scntl2) & WSR)) {
u32 tmp;
#ifdef  SYM_DEBUG_PM_WITH_WSR
PRINT_ADDR(cp);
printf ("MA interrupt with WSR set - "
"pm->sg.addr=%x - pm->sg.size=%d\n",
pm->sg.addr, pm->sg.size);
#endif
tmp = scr_to_cpu(pm->sg.addr);
cp->phys.wresid.addr = cpu_to_scr(tmp);
pm->sg.addr = cpu_to_scr(tmp + 1);
tmp = scr_to_cpu(pm->sg.size);
cp->phys.wresid.size = cpu_to_scr((tmp&0xff000000) | 1);
pm->sg.size = cpu_to_scr(tmp - 1);
if ((tmp&0xffffff) == 1)
newcmd = pm->ret;
nxtdsp = NCB_SCRIPTH_PHYS (np, wsr_ma_helper);
}
if (DEBUG_FLAGS & DEBUG_PHASE) {
PRINT_ADDR(cp->cmd);
printk ("PM %x %x %x / %x %x %x.\n",
hflags0, hflags, newcmd,
(unsigned)scr_to_cpu(pm->sg.addr),
(unsigned)scr_to_cpu(pm->sg.size),
(unsigned)scr_to_cpu(pm->ret));
}
OUTL (nc_temp, newcmd);
OUTL_DSP (nxtdsp);
return;
unexpected_phase:
dsp -= 8;
nxtdsp = 0;
switch (cmd & 7) {
case 2:
nxtdsp = NCB_SCRIPT_PHYS (np, dispatch);
break;
#if 0
case 3:
nxtdsp = NCB_SCRIPT_PHYS (np, dispatch);
break;
#endif
case 6:
if	(dsp == NCB_SCRIPT_PHYS (np, send_ident)) {
if (cp->tag != NO_TAG && olen - rest <= 3) {
cp->host_status = HS_BUSY;
np->msgout[0] = M_IDENTIFY | cp->lun;
nxtdsp = NCB_SCRIPTH_PHYS (np, ident_break_atn);
}
else
nxtdsp = NCB_SCRIPTH_PHYS (np, ident_break);
}
else if	(dsp == NCB_SCRIPTH_PHYS (np, send_wdtr) ||
dsp == NCB_SCRIPTH_PHYS (np, send_sdtr) ||
dsp == NCB_SCRIPTH_PHYS (np, send_ppr)) {
nxtdsp = NCB_SCRIPTH_PHYS (np, nego_bad_phase);
}
break;
#if 0
case 7:
nxtdsp = NCB_SCRIPT_PHYS (np, clrack);
break;
#endif
}
if (nxtdsp) {
OUTL_DSP (nxtdsp);
return;
}
reset_all:
ncr_start_reset(np);
}
static void ncr_sir_to_redo(ncb_p np, int num, ccb_p cp)
{
Scsi_Cmnd *cmd	= cp->cmd;
tcb_p tp	= &np->target[cp->target];
lcb_p lp	= ncr_lp(np, tp, cp->lun);
ccb_p		cp2;
int		busyccbs = 1;
u_int32		startp;
u_char		s_status = INB (SS_PRT);
int		msglen;
int		i, j;
if (!lp)
goto next;
busyccbs = lp->queuedccbs;
i = (INL (nc_scratcha) - np->p_squeue) / 4;
j = i;
while (i != np->squeueput) {
cp2 = ncr_ccb_from_dsa(np, scr_to_cpu(np->squeue[i]));
assert(cp2);
#ifdef SCSI_NCR_IARB_SUPPORT
cp2->host_flags &= ~HF_HINT_IARB;
#endif
if (cp2 && cp2->target == cp->target && cp2->lun == cp->lun) {
xpt_remque(&cp2->link_ccbq);
xpt_insque_head(&cp2->link_ccbq, &lp->wait_ccbq);
--lp->queuedccbs;
cp2->queued = 0;
}
else {
if (i != j)
np->squeue[j] = np->squeue[i];
if ((j += 2) >= MAX_START*2) j = 0;
}
if ((i += 2) >= MAX_START*2) i = 0;
}
if (i != j)
np->squeue[j] = np->squeue[i];
np->squeueput = j;
xpt_remque(&cp->link_ccbq);
xpt_insque_head(&cp->link_ccbq, &lp->wait_ccbq);
--lp->queuedccbs;
cp->queued = 0;
next:
#ifdef SCSI_NCR_IARB_SUPPORT
cp->host_flags &= ~HF_HINT_IARB;
if (np->last_cp)
np->last_cp = 0;
#endif
OUTL_DSP (NCB_SCRIPT_PHYS (np, start));
switch(s_status) {
default:
case S_BUSY:
ncr_complete(np, cp);
break;
case S_QUEUE_FULL:
if (!lp || !lp->queuedccbs) {
ncr_complete(np, cp);
break;
}
if (bootverbose >= 1) {
PRINT_ADDR(cmd);
printk ("QUEUE FULL! %d busy, %d disconnected CCBs\n",
busyccbs, lp->queuedccbs);
}
if (lp->queuedccbs < lp->numtags) {
lp->numtags	= lp->queuedccbs;
lp->num_good	= 0;
ncr_setup_tags (np, cp->target, cp->lun);
}
cp->phys.header.savep	= cp->startp;
cp->phys.header.lastp	= cp->lastp0;
cp->host_status 	= HS_BUSY;
cp->scsi_status 	= S_ILLEGAL;
cp->xerr_status		= 0;
cp->extra_bytes		= 0;
cp->host_flags		&= (HF_PM_TO_C|HF_DATA_IN);
break;
case S_TERMINATED:
case S_CHECK_COND:
if (cp->host_flags & HF_AUTO_SENSE) {
ncr_complete(np, cp);
break;
}
cp->sv_scsi_status = cp->scsi_status;
cp->sv_xerr_status = cp->xerr_status;
cp->resid = ncr_compute_residual(np, cp);
cp->scsi_smsg2[0]	= M_IDENTIFY | cp->lun;
msglen = 1;
#ifdef	SCSI_NCR_INTEGRITY_CHECKING
if (DEBUG_FLAGS & DEBUG_IC) {
printk("%s: ncr_sir_to_redo: ic_done %2X, in_progress %2X\n",
ncr_name(np), tp->ic_done, cp->cmd->ic_in_progress);
}
if ( np->check_integ_par && np->check_integrity
&& cp->cmd->ic_in_progress ) {
cp->nego_status = 0;
msglen +=
ncr_ic_nego (np, cp, cmd ,&cp->scsi_smsg2[msglen]);
}
if (!np->check_integrity ||
(np->check_integrity &&
(!cp->cmd->ic_in_progress && !tp->ic_done)) ) {
ncr_negotiate(np, tp);
cp->nego_status = 0;
{
u_char sync_offset;
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66))
sync_offset = tp->sval & 0x3f;
else
sync_offset = tp->sval & 0x1f;
if ((tp->wval & EWS) || sync_offset)
msglen +=
ncr_prepare_nego (np, cp, &cp->scsi_smsg2[msglen]);
}
}
#else
ncr_negotiate(np, tp);
cp->nego_status = 0;
if ((tp->wval & EWS) || (tp->sval & 0x1f))
msglen +=
ncr_prepare_nego (np, cp, &cp->scsi_smsg2[msglen]);
#endif
cp->phys.smsg.addr	= cpu_to_scr(CCB_PHYS (cp, scsi_smsg2));
cp->phys.smsg.size	= cpu_to_scr(msglen);
cp->phys.cmd.addr	= cpu_to_scr(CCB_PHYS (cp, sensecmd));
cp->phys.cmd.size	= cpu_to_scr(6);
cp->sensecmd[0]		= 0x03;
cp->sensecmd[1]		= cp->lun << 5;
cp->sensecmd[4]		= sizeof(cp->sense_buf);
bzero(cp->sense_buf, sizeof(cp->sense_buf));
cp->phys.sense.addr	= cpu_to_scr(CCB_PHYS(cp,sense_buf[0]));
cp->phys.sense.size	= cpu_to_scr(sizeof(cp->sense_buf));
startp = NCB_SCRIPTH_PHYS (np, sdata_in);
cp->phys.header.savep	= cpu_to_scr(startp);
cp->phys.header.goalp	= cpu_to_scr(startp + 16);
cp->phys.header.lastp	= cpu_to_scr(startp);
cp->phys.header.wgoalp	= cpu_to_scr(startp + 16);
cp->phys.header.wlastp	= cpu_to_scr(startp);
cp->host_status	= cp->nego_status ? HS_NEGOTIATE : HS_BUSY;
cp->scsi_status = S_ILLEGAL;
cp->host_flags	= (HF_AUTO_SENSE|HF_DATA_IN);
cp->phys.header.go.start =
cpu_to_scr(NCB_SCRIPT_PHYS (np, select));
if (!lp)
ncr_put_start_queue(np, cp);
break;
}
if (lp)
ncr_start_next_ccb(np, lp, 1);
return;
}
static int ncr_clear_tasks(ncb_p np, u_char hsts,
int target, int lun, int task)
{
int i = 0;
ccb_p cp;
for (cp = np->ccbc; cp; cp = cp->link_ccb) {
if (cp->host_status != HS_DISCONNECT)
continue;
if (cp->target != target)
continue;
if (lun != -1 && cp->lun != lun)
continue;
if (task != -1 && cp->tag != NO_TAG && cp->scsi_smsg[2] != task)
continue;
cp->host_status = hsts;
cp->scsi_status = S_ILLEGAL;
ncr_complete(np, cp);
++i;
}
return i;
}
static void ncr_sir_task_recovery(ncb_p np, int num)
{
ccb_p cp;
tcb_p tp;
int target=-1, lun=-1, task;
int i, k;
u_char *p;
switch(num) {
case SIR_SCRIPT_STOPPED:
for (i = 0 ; i < MAX_TARGET ; i++) {
tp = &np->target[i];
if (tp->to_reset || (tp->l0p && tp->l0p->to_clear)) {
target = i;
break;
}
if (!tp->lmp)
continue;
for (k = 1 ; k < MAX_LUN ; k++) {
if (tp->lmp[k] && tp->lmp[k]->to_clear) {
target	= i;
break;
}
}
if (target != -1)
break;
}
if (target == -1) {
for (cp = np->ccbc; cp; cp = cp->link_ccb) {
if (cp->host_status != HS_DISCONNECT)
continue;
if (cp->to_abort) {
target = cp->target;
break;
}
}
}
if (target != -1) {
tp = &np->target[target];
np->abrt_sel.sel_id	= target;
np->abrt_sel.sel_scntl3 = tp->wval;
np->abrt_sel.sel_sxfer  = tp->sval;
np->abrt_sel.sel_scntl4 = tp->uval;
OUTL(nc_dsa, np->p_ncb);
OUTL_DSP (NCB_SCRIPTH_PHYS (np, sel_for_abort));
return;
}
np->istat_sem = 0;
OUTB (nc_istat, SIGP);
for (cp = np->ccbc; cp; cp = cp->link_ccb) {
if (cp->host_status != HS_BUSY &&
cp->host_status != HS_NEGOTIATE)
continue;
if (!cp->to_abort)
continue;
#ifdef SCSI_NCR_IARB_SUPPORT
if (cp == np->last_cp) {
cp->to_abort = 0;
continue;
}
#endif
i = (INL (nc_scratcha) - np->p_squeue) / 4;
k = -1;
while (1) {
if (i == np->squeueput)
break;
if (k == -1) {
if (cp == ncr_ccb_from_dsa(np,
scr_to_cpu(np->squeue[i])))
k = i;
}
else {
np->squeue[k] = np->squeue[i];
k += 2;
if (k >= MAX_START*2)
k = 0;
}
i += 2;
if (i >= MAX_START*2)
i = 0;
}
if (k != -1) {
np->squeue[k] = np->squeue[i];
np->squeueput = k;
}
cp->host_status = HS_ABORTED;
cp->scsi_status = S_ILLEGAL;
ncr_complete(np, cp);
}
break;
case SIR_TARGET_SELECTED:
target = (INB (nc_sdid) & 0xf);
tp = &np->target[target];
np->abrt_tbl.addr = vtobus(np->abrt_msg);
if (tp->to_reset) {
np->abrt_msg[0] = M_RESET;
np->abrt_tbl.size = 1;
tp->to_reset = 0;
break;
}
if (tp->l0p && tp->l0p->to_clear)
lun = 0;
else if (tp->lmp) {
for (k = 1 ; k < MAX_LUN ; k++) {
if (tp->lmp[k] && tp->lmp[k]->to_clear) {
lun = k;
break;
}
}
}
if (lun != -1) {
lcb_p lp = ncr_lp(np, tp, lun);
lp->to_clear = 0;
np->abrt_msg[0] = M_IDENTIFY | lun;
np->abrt_msg[1] = M_ABORT;
np->abrt_tbl.size = 2;
break;
}
for (cp = np->ccbc; cp; cp = cp->link_ccb) {
if (cp->host_status != HS_DISCONNECT)
continue;
if (cp->target != target)
continue;
if (cp->to_abort)
break;
}
if (!cp) {
np->abrt_msg[0] = M_ABORT;
np->abrt_tbl.size = 1;
break;
}
np->abrt_msg[0] = M_IDENTIFY | cp->lun;
if (cp->tag == NO_TAG) {
np->abrt_msg[1] = M_ABORT;
np->abrt_tbl.size = 2;
}
else {
np->abrt_msg[1] = cp->scsi_smsg[1];
np->abrt_msg[2] = cp->scsi_smsg[2];
np->abrt_msg[3] = M_ABORT_TAG;
np->abrt_tbl.size = 4;
}
cp->to_abort = 0;
break;
case SIR_ABORT_SENT:
target = (INB (nc_sdid) & 0xf);
tp = &np->target[target];
if (np->abrt_msg[0] == M_ABORT)
break;
lun = -1;
task = -1;
if (np->abrt_msg[0] == M_RESET) {
tp->sval = 0;
tp->wval = np->rv_scntl3;
tp->uval = np->rv_scntl4;
ncr_set_sync_wide_status(np, target);
ncr_negotiate(np, tp);
}
else {
lun = np->abrt_msg[0] & 0x3f;
if (np->abrt_msg[1] == M_ABORT_TAG)
task = np->abrt_msg[2];
}
(void) ncr_clear_tasks(np, HS_ABORTED, target, lun, task);
break;
case SIR_AUTO_SENSE_DONE:
cp = ncr_ccb_from_dsa(np, INL (nc_dsa));
if (!cp)
break;
memcpy(cp->cmd->sense_buffer, cp->sense_buf,
sizeof(cp->cmd->sense_buffer));
p  = &cp->cmd->sense_buffer[0];
if (p[0] != 0x70 || p[2] != 0x6 || p[12] != 0x29)
break;
#if 0
(void) ncr_clear_tasks(np, HS_RESET, cp->target, cp->lun, -1);
#endif
break;
}
if (num == SIR_TARGET_SELECTED) {
PRINT_TARGET(np, target);
ncr_printl_hex("control msgout:", np->abrt_msg,
np->abrt_tbl.size);
np->abrt_tbl.size = cpu_to_scr(np->abrt_tbl.size);
}
OUTONB_STD ();
}
static int ncr_evaluate_dp(ncb_p np, ccb_p cp, u_int32 scr, int *ofs)
{
u_int32	dp_scr;
int	dp_ofs, dp_sg, dp_sgmin;
int	tmp;
struct pm_ctx *pm;
dp_scr = scr;
dp_ofs = *ofs;
if	(dp_scr == NCB_SCRIPT_PHYS (np, pm0_data))
pm = &cp->phys.pm0;
else if (dp_scr == NCB_SCRIPT_PHYS (np, pm1_data))
pm = &cp->phys.pm1;
else
pm = 0;
if (pm) {
dp_scr  = scr_to_cpu(pm->ret);
dp_ofs -= scr_to_cpu(pm->sg.size);
}
tmp = scr_to_cpu(cp->phys.header.goalp);
dp_sg = MAX_SCATTER;
if (dp_scr != tmp)
dp_sg -= (tmp - 8 - (int)dp_scr) / (SCR_SG_SIZE*4);
dp_sgmin = MAX_SCATTER - cp->segments;
if (dp_ofs < 0) {
int n;
while (dp_sg > dp_sgmin) {
--dp_sg;
tmp = scr_to_cpu(cp->phys.data[dp_sg].size);
n = dp_ofs + (tmp & 0xffffff);
if (n > 0) {
++dp_sg;
break;
}
dp_ofs = n;
}
}
else if (dp_ofs > 0) {
while (dp_sg < MAX_SCATTER) {
tmp = scr_to_cpu(cp->phys.data[dp_sg].size);
dp_ofs -= (tmp & 0xffffff);
++dp_sg;
if (dp_ofs <= 0)
break;
}
}
if	(dp_sg < dp_sgmin || (dp_sg == dp_sgmin && dp_ofs < 0))
goto out_err;
else if	(dp_sg > MAX_SCATTER || (dp_sg == MAX_SCATTER && dp_ofs > 0))
goto out_err;
if (dp_sg > cp->ext_sg ||
(dp_sg == cp->ext_sg && dp_ofs > cp->ext_ofs)) {
cp->ext_sg  = dp_sg;
cp->ext_ofs = dp_ofs;
}
*ofs = dp_ofs;
return dp_sg;
out_err:
return -1;
}
static void ncr_modify_dp(ncb_p np, tcb_p tp, ccb_p cp, int ofs)
{
int dp_ofs	= ofs;
u_int32 dp_scr	= INL (nc_temp);
u_int32	dp_ret;
u_int32	tmp;
u_char	hflags;
int	dp_sg;
struct pm_ctx *pm;
if (cp->host_flags & HF_AUTO_SENSE)
goto out_reject;
dp_sg = ncr_evaluate_dp(np, cp, dp_scr, &dp_ofs);
if (dp_sg < 0)
goto out_reject;
dp_ret = cpu_to_scr(cp->phys.header.goalp);
dp_ret = dp_ret - 8 - (MAX_SCATTER - dp_sg) * (SCR_SG_SIZE*4);
if (dp_ofs == 0) {
dp_scr = dp_ret;
goto out_ok;
}
hflags = INB (HF_PRT);
if (hflags & HF_DP_SAVED)
hflags ^= HF_ACT_PM;
if (!(hflags & HF_ACT_PM)) {
pm  = &cp->phys.pm0;
dp_scr = NCB_SCRIPT_PHYS (np, pm0_data);
}
else {
pm = &cp->phys.pm1;
dp_scr = NCB_SCRIPT_PHYS (np, pm1_data);
}
hflags &= ~(HF_DP_SAVED);
OUTB (HF_PRT, hflags);
pm->ret = cpu_to_scr(dp_ret);
tmp  = scr_to_cpu(cp->phys.data[dp_sg-1].addr);
tmp += scr_to_cpu(cp->phys.data[dp_sg-1].size) + dp_ofs;
pm->sg.addr = cpu_to_scr(tmp);
pm->sg.size = cpu_to_scr(-dp_ofs);
out_ok:
OUTL (nc_temp, dp_scr);
OUTL_DSP (NCB_SCRIPT_PHYS (np, clrack));
return;
out_reject:
OUTL_DSP (NCB_SCRIPTH_PHYS (np, msg_bad));
}
static int ncr_compute_residual(ncb_p np, ccb_p cp)
{
int dp_sg, dp_sgmin, tmp;
int resid=0;
int dp_ofs = 0;
if (cp->xerr_status & (XE_EXTRA_DATA|XE_SODL_UNRUN|XE_SWIDE_OVRUN)) {
if (cp->xerr_status & XE_EXTRA_DATA)
resid -= cp->extra_bytes;
if (cp->xerr_status & XE_SODL_UNRUN)
++resid;
if (cp->xerr_status & XE_SWIDE_OVRUN)
--resid;
}
if (cp->phys.header.lastp == cp->phys.header.goalp)
return resid;
if (cp->phys.header.lastp == NCB_SCRIPTH_PHYS (np, data_io))
return cp->data_len;
if (cp->startp == cp->phys.header.lastp ||
ncr_evaluate_dp(np, cp, scr_to_cpu(cp->phys.header.lastp),
&dp_ofs) < 0) {
return cp->data_len;
}
dp_sgmin = MAX_SCATTER - cp->segments;
resid = -cp->ext_ofs;
for (dp_sg = cp->ext_sg; dp_sg < MAX_SCATTER; ++dp_sg) {
tmp = scr_to_cpu(cp->phys.data[dp_sg].size);
resid += (tmp & 0xffffff);
}
return resid;
}
static int ncr_show_msg (u_char * msg)
{
u_char i;
printk ("%x",*msg);
if (*msg==M_EXTENDED) {
for (i=1;i<8;i++) {
if (i-1>msg[1]) break;
printk ("-%x",msg[i]);
};
return (i+1);
} else if ((*msg & 0xf0) == 0x20) {
printk ("-%x",msg[1]);
return (2);
};
return (1);
}
static void ncr_print_msg (ccb_p cp, char *label, u_char *msg)
{
if (cp)
PRINT_ADDR(cp->cmd);
if (label)
printk ("%s: ", label);
(void) ncr_show_msg (msg);
printk (".\n");
}
static void ncr_sync_nego(ncb_p np, tcb_p tp, ccb_p cp)
{
u_char	scntl3, scntl4;
u_char	chg, ofs, per, fak;
if (DEBUG_FLAGS & DEBUG_NEGO) {
ncr_print_msg(cp, "sync msg in", np->msgin);
};
chg = 0;
per = np->msgin[3];
ofs = np->msgin[4];
if (ofs==0) per=255;
if (ofs)
tp->inq_byte7 |= INQ7_SYNC;
if (per < np->minsync)
{chg = 1; per = np->minsync;}
if (per < tp->minsync)
{chg = 1; per = tp->minsync;}
if (ofs > tp->maxoffs)
{chg = 1; ofs = tp->maxoffs;}
fak	= 7;
scntl3	= 0;
scntl4  = 0;
if (ofs != 0) {
ncr_getsync(np, per, &fak, &scntl3);
if (fak > 7) {
chg = 1;
ofs = 0;
}
}
if (ofs == 0) {
fak	= 7;
per	= 0;
scntl3	= 0;
scntl4  = 0;
tp->minsync = 0;
}
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printk ("sync: per=%d scntl3=0x%x scntl4=0x%x ofs=%d fak=%d chg=%d.\n",
per, scntl3, scntl4, ofs, fak, chg);
}
if (INB (HS_PRT) == HS_NEGOTIATE) {
OUTB (HS_PRT, HS_BUSY);
switch (cp->nego_status) {
case NS_SYNC:
if (chg) {
ncr_setsync (np, cp, 0, 0xe0, 0);
OUTL_DSP (NCB_SCRIPTH_PHYS (np, msg_bad));
} else {
if ((np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66))
ncr_setsync (np, cp, scntl3, (fak<<5)|ofs,0);
else
ncr_setsync (np, cp, scntl3, ofs, scntl4);
OUTL_DSP (NCB_SCRIPT_PHYS (np, clrack));
};
return;
case NS_WIDE:
ncr_setwide (np, cp, 0, 0);
break;
};
};
if ((np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66))
ncr_setsync (np, cp, scntl3, (fak<<5)|ofs,0);
else
ncr_setsync (np, cp, scntl3, ofs, scntl4);
np->msgout[0] = M_EXTENDED;
np->msgout[1] = 3;
np->msgout[2] = M_X_SYNC_REQ;
np->msgout[3] = per;
np->msgout[4] = ofs;
cp->nego_status = NS_SYNC;
if (DEBUG_FLAGS & DEBUG_NEGO) {
ncr_print_msg(cp, "sync msgout", np->msgout);
}
np->msgin [0] = M_NOOP;
if (!ofs)
OUTL_DSP (NCB_SCRIPTH_PHYS (np, msg_bad));
else
OUTL_DSP (NCB_SCRIPTH_PHYS (np, sdtr_resp));
}
static void ncr_wide_nego(ncb_p np, tcb_p tp, ccb_p cp)
{
u_char	chg, wide;
if (DEBUG_FLAGS & DEBUG_NEGO) {
ncr_print_msg(cp, "wide msgin", np->msgin);
};
chg  = 0;
wide = np->msgin[3];
if (wide)
tp->inq_byte7 |= INQ7_WIDE16;
if (wide > tp->usrwide)
{chg = 1; wide = tp->usrwide;}
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printk ("wide: wide=%d chg=%d.\n", wide, chg);
}
if (INB (HS_PRT) == HS_NEGOTIATE) {
OUTB (HS_PRT, HS_BUSY);
switch (cp->nego_status) {
case NS_WIDE:
if (chg) {
ncr_setwide (np, cp, 0, 1);
OUTL_DSP (NCB_SCRIPTH_PHYS (np, msg_bad));
} else {
ncr_setwide (np, cp, wide, 1);
OUTL_DSP (NCB_SCRIPT_PHYS (np, clrack));
};
return;
case NS_SYNC:
ncr_setsync (np, cp, 0, 0xe0, 0);
break;
};
};
ncr_setwide (np, cp, wide, 1);
np->msgout[0] = M_EXTENDED;
np->msgout[1] = 2;
np->msgout[2] = M_X_WIDE_REQ;
np->msgout[3] = wide;
np->msgin [0] = M_NOOP;
cp->nego_status = NS_WIDE;
if (DEBUG_FLAGS & DEBUG_NEGO) {
ncr_print_msg(cp, "wide msgout", np->msgout);
}
OUTL_DSP (NCB_SCRIPTH_PHYS (np, wdtr_resp));
}
static void ncr_ppr_nego(ncb_p np, tcb_p tp, ccb_p cp)
{
u_char	scntl3, scntl4;
u_char	chg, ofs, per, fak, wth, dt;
if (DEBUG_FLAGS & DEBUG_NEGO) {
ncr_print_msg(cp, "ppr msg in", np->msgin);
};
chg = 0;
per = np->msgin[3];
ofs = np->msgin[5];
wth = np->msgin[6];
dt  = np->msgin[7];
if (ofs==0) per=255;
if (ofs)
tp->inq_byte7 |= INQ7_SYNC;
if (wth)
tp->inq_byte7 |= INQ7_WIDE16;
if (wth > tp->usrwide)
{chg = 1; wth = tp->usrwide;}
if (per < np->minsync)
{chg = 1; per = np->minsync;}
if (per < tp->minsync)
{chg = 1; per = tp->minsync;}
if (ofs > tp->maxoffs)
{chg = 1; ofs = tp->maxoffs;}
fak	= 7;
scntl3	= 0;
scntl4  = 0;
if (ofs != 0) {
scntl4 = dt ? 0x80 : 0;
ncr_getsync(np, per, &fak, &scntl3);
if (fak > 7) {
chg = 1;
ofs = 0;
}
}
if (ofs == 0) {
fak	= 7;
per	= 0;
scntl3	= 0;
scntl4  = 0;
tp->minsync = 0;
}
if   ((per == 0x09) && ofs && (!wth || !dt))
chg = 1;
else if (( (per > 0x09) && dt) )
chg = 2;
if (DEBUG_FLAGS & DEBUG_NEGO) {
PRINT_ADDR(cp->cmd);
printk ("ppr: wth=%d per=%d scntl3=0x%x scntl4=0x%x ofs=%d fak=%d chg=%d.\n",
wth, per, scntl3, scntl4, ofs, fak, chg);
}
if (INB (HS_PRT) == HS_NEGOTIATE) {
OUTB (HS_PRT, HS_BUSY);
switch (cp->nego_status) {
case NS_PPR:
if (chg) {
if (chg == 2) {
tp->minsync = 0x0A;
tp->period = 0;
tp->widedone = 0;
}
ncr_setsyncwide (np, cp, 0, 0xe0, 0, 0);
OUTL_DSP (NCB_SCRIPTH_PHYS (np, msg_bad));
} else {
if ((np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66))
ncr_setsyncwide (np, cp, scntl3, (fak<<5)|ofs,0, wth);
else
ncr_setsyncwide (np, cp, scntl3, ofs, scntl4, wth);
OUTL_DSP (NCB_SCRIPT_PHYS (np, clrack));
};
return;
case NS_SYNC:
ncr_setsync (np, cp, 0, 0xe0, 0);
break;
case NS_WIDE:
ncr_setwide (np, cp, 0, 0);
break;
};
};
if  ((per == 0x09) && ofs && (!wth || !dt)) {
per = 0x0A;
dt = 0;
ofs &= 0x1f;
}
else if ( (per > 0x09) && dt) {
dt = 0;
ofs &= 0x1f;
}
if ((np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66))
ncr_setsyncwide (np, cp, scntl3, (fak<<5)|ofs,0, wth);
else
ncr_setsyncwide (np, cp, scntl3, ofs, scntl4, wth);
np->msgout[0] = M_EXTENDED;
np->msgout[1] = 6;
np->msgout[2] = M_X_PPR_REQ;
np->msgout[3] = per;
np->msgout[4] = 0;
np->msgout[5] = ofs;
np->msgout[6] = wth;
np->msgout[7] = dt;
cp->nego_status = NS_PPR;
if (DEBUG_FLAGS & DEBUG_NEGO) {
ncr_print_msg(cp, "ppr msgout", np->msgout);
}
np->msgin [0] = M_NOOP;
if (!ofs)
OUTL_DSP (NCB_SCRIPTH_PHYS (np, msg_bad));
else
OUTL_DSP (NCB_SCRIPTH_PHYS (np, ppr_resp));
}
static void ncr_nego_default(ncb_p np, tcb_p tp, ccb_p cp)
{
switch (cp->nego_status) {
case NS_SYNC:
ncr_setsync (np, cp, 0, 0xe0, 0);
break;
case NS_WIDE:
ncr_setwide (np, cp, 0, 0);
break;
case NS_PPR:
if (DEBUG_FLAGS & DEBUG_NEGO) {
tcb_p tp=&np->target[cp->target];
u_char factor, offset, width;
ncr_get_xfer_info ( np, tp, &factor, &offset, &width);
printk("Current factor %d offset %d width %d\n",
factor, offset, width);
}
if (tp->ppr_negotiation == 2)
ncr_setsyncwide (np, cp, 0, 0xe0, 0, 0);
else if (tp->ppr_negotiation == 1) {
tp->ppr_negotiation = 0;
}
else
{
tp->ppr_negotiation = 0;
ncr_setwide (np, cp, 0, 0);
}
break;
};
np->msgin [0] = M_NOOP;
np->msgout[0] = M_NOOP;
cp->nego_status = 0;
}
static void ncr_nego_rejected(ncb_p np, tcb_p tp, ccb_p cp)
{
ncr_nego_default(np, tp, cp);
OUTB (HS_PRT, HS_BUSY);
}
void ncr_int_sir (ncb_p np)
{
u_char	num	= INB (nc_dsps);
u_long	dsa	= INL (nc_dsa);
ccb_p	cp	= ncr_ccb_from_dsa(np, dsa);
u_char	target	= INB (nc_sdid) & 0x0f;
tcb_p	tp	= &np->target[target];
int	tmp;
if (DEBUG_FLAGS & DEBUG_TINY) printk ("I#%d", num);
switch (num) {
#ifdef SCSI_NCR_PCIQ_SYNC_ON_INTR
case SIR_DUMMY_INTERRUPT:
goto out;
#endif
case SIR_SCRIPT_STOPPED:
case SIR_TARGET_SELECTED:
case SIR_ABORT_SENT:
case SIR_AUTO_SENSE_DONE:
ncr_sir_task_recovery(np, num);
return;
case SIR_SEL_ATN_NO_MSG_OUT:
printk ("%s:%d: No MSG OUT phase after selection with ATN.\n",
ncr_name (np), target);
goto out_stuck;
case SIR_RESEL_NO_MSG_IN:
case SIR_RESEL_NO_IDENTIFY:
if (tp->l0p) {
OUTL (nc_dsa, scr_to_cpu(tp->l0p->tasktbl[0]));
OUTL_DSP (NCB_SCRIPT_PHYS (np, resel_go));
return;
}
case SIR_RESEL_BAD_LUN:
np->msgout[0] = M_RESET;
goto out;
case SIR_RESEL_BAD_I_T_L:
np->msgout[0] = M_ABORT;
goto out;
case SIR_RESEL_BAD_I_T_L_Q:
np->msgout[0] = M_ABORT_TAG;
goto out;
case SIR_RESEL_ABORTED:
np->lastmsg = np->msgout[0];
np->msgout[0] = M_NOOP;
printk ("%s:%d: message %x sent on bad reselection.\n",
ncr_name (np), target, np->lastmsg);
goto out;
case SIR_MSG_OUT_DONE:
np->lastmsg = np->msgout[0];
np->msgout[0] = M_NOOP;
if (np->lastmsg == M_PARITY || np->lastmsg == M_ID_ERROR) {
if (cp) {
cp->xerr_status &= ~XE_PARITY_ERR;
if (!cp->xerr_status)
OUTOFFB (HF_PRT, HF_EXT_ERR);
}
}
goto out;
case SIR_BAD_STATUS:
if (!cp)
goto out;
ncr_sir_to_redo(np, num, cp);
return;
case SIR_REJECT_TO_SEND:
ncr_print_msg(cp, "M_REJECT to send for ", np->msgin);
np->msgout[0] = M_REJECT;
goto out;
case SIR_SWIDE_OVERRUN:
if (cp) {
OUTONB (HF_PRT, HF_EXT_ERR);
cp->xerr_status |= XE_SWIDE_OVRUN;
}
goto out;
case SIR_SODL_UNDERRUN:
if (cp) {
OUTONB (HF_PRT, HF_EXT_ERR);
cp->xerr_status |= XE_SODL_UNRUN;
}
goto out;
case SIR_DATA_OVERRUN:
if (cp) {
OUTONB (HF_PRT, HF_EXT_ERR);
cp->xerr_status |= XE_EXTRA_DATA;
cp->extra_bytes += INL (nc_scratcha);
}
goto out;
case SIR_BAD_PHASE:
if (cp) {
OUTONB (HF_PRT, HF_EXT_ERR);
cp->xerr_status |= XE_BAD_PHASE;
}
goto out;
case SIR_MSG_RECEIVED:
if (!cp)
goto out_stuck;
switch (np->msgin [0]) {
case M_EXTENDED:
switch (np->msgin [2]) {
case M_X_MODIFY_DP:
if (DEBUG_FLAGS & DEBUG_POINTER)
ncr_print_msg(cp,"modify DP",np->msgin);
tmp = (np->msgin[3]<<24) + (np->msgin[4]<<16) +
(np->msgin[5]<<8)  + (np->msgin[6]);
ncr_modify_dp(np, tp, cp, tmp);
return;
case M_X_SYNC_REQ:
ncr_sync_nego(np, tp, cp);
return;
case M_X_WIDE_REQ:
ncr_wide_nego(np, tp, cp);
return;
case M_X_PPR_REQ:
ncr_ppr_nego(np, tp, cp);
return;
default:
goto out_reject;
}
break;
case M_IGN_RESIDUE:
if (DEBUG_FLAGS & DEBUG_POINTER)
ncr_print_msg(cp,"ign wide residue", np->msgin);
ncr_modify_dp(np, tp, cp, -1);
return;
case M_REJECT:
if (INB (HS_PRT) == HS_NEGOTIATE)
ncr_nego_rejected(np, tp, cp);
else {
PRINT_ADDR(cp->cmd);
printk ("M_REJECT received (%x:%x).\n",
scr_to_cpu(np->lastmsg), np->msgout[0]);
}
goto out_clrack;
break;
default:
goto out_reject;
}
break;
case SIR_MSG_WEIRD:
ncr_print_msg(cp, "WEIRD message received", np->msgin);
OUTL_DSP (NCB_SCRIPTH_PHYS (np, msg_weird));
return;
case SIR_NEGO_FAILED:
OUTB (HS_PRT, HS_BUSY);
case SIR_NEGO_PROTO:
ncr_nego_default(np, tp, cp);
goto out;
};
out:
OUTONB_STD ();
return;
out_reject:
OUTL_DSP (NCB_SCRIPTH_PHYS (np, msg_bad));
return;
out_clrack:
OUTL_DSP (NCB_SCRIPT_PHYS (np, clrack));
return;
out_stuck:
return;
}
static	ccb_p ncr_get_ccb (ncb_p np, u_char tn, u_char ln)
{
tcb_p tp = &np->target[tn];
lcb_p lp = ncr_lp(np, tp, ln);
u_short tag = NO_TAG;
XPT_QUEHEAD *qp;
ccb_p cp = (ccb_p) 0;
if (xpt_que_empty(&np->free_ccbq))
(void) ncr_alloc_ccb(np);
qp = xpt_remque_head(&np->free_ccbq);
if (!qp)
goto out;
cp = xpt_que_entry(qp, struct ccb, link_ccbq);
if (!lp) {
if (xpt_que_empty(&np->b0_ccbq))
xpt_insque_head(&cp->link_ccbq, &np->b0_ccbq);
else
goto out_free;
} else {
if (lp->queuedepth != lp->numtags) {
ncr_setup_tags(np, tn, ln);
}
if (lp->usetags) {
if (lp->busyccbs < lp->maxnxs) {
tag = lp->cb_tags[lp->ia_tag];
++lp->ia_tag;
if (lp->ia_tag == MAX_TAGS)
lp->ia_tag = 0;
cp->tags_si = lp->tags_si;
++lp->tags_sum[cp->tags_si];
}
else
goto out_free;
}
xpt_insque_tail(&cp->link_ccbq, &lp->wait_ccbq);
++lp->busyccbs;
}
cp->to_abort = 0;
cp->tag	   = tag;
cp->target = tn;
cp->lun    = ln;
if (DEBUG_FLAGS & DEBUG_TAGS) {
PRINT_LUN(np, tn, ln);
printk ("ccb @%p using tag %d.\n", cp, tag);
}
out:
return cp;
out_free:
xpt_insque_head(&cp->link_ccbq, &np->free_ccbq);
return (ccb_p) 0;
}
static void ncr_free_ccb (ncb_p np, ccb_p cp)
{
tcb_p tp = &np->target[cp->target];
lcb_p lp = ncr_lp(np, tp, cp->lun);
if (DEBUG_FLAGS & DEBUG_TAGS) {
PRINT_LUN(np, cp->target, cp->lun);
printk ("ccb @%p freeing tag %d.\n", cp, cp->tag);
}
if (lp) {
if (cp->tag != NO_TAG) {
lp->cb_tags[lp->if_tag++] = cp->tag;
if (lp->if_tag == MAX_TAGS)
lp->if_tag = 0;
--lp->tags_sum[cp->tags_si];
lp->tasktbl[cp->tag] = cpu_to_scr(np->p_bad_i_t_l_q);
} else {
lp->tasktbl[0] = cpu_to_scr(np->p_bad_i_t_l);
}
--lp->busyccbs;
if (cp->queued) {
--lp->queuedccbs;
}
}
xpt_remque(&cp->link_ccbq);
xpt_insque_head(&cp->link_ccbq, &np->free_ccbq);
cp -> host_status = HS_IDLE;
cp -> queued = 0;
}
static ccb_p ncr_alloc_ccb(ncb_p np)
{
ccb_p cp = 0;
int hcode;
cp = m_calloc_dma(sizeof(struct ccb), "CCB");
if (!cp)
return 0;
np->actccbs++;
cp->p_ccb 	   = vtobus(cp);
hcode = CCB_HASH_CODE(cp->p_ccb);
cp->link_ccbh = np->ccbh[hcode];
np->ccbh[hcode] = cp;
cp->phys.header.go.start   = cpu_to_scr(NCB_SCRIPT_PHYS (np, idle));
cp->phys.header.go.restart = cpu_to_scr(NCB_SCRIPTH_PHYS(np,bad_i_t_l));
cp->phys.smsg_ext.addr = cpu_to_scr(NCB_PHYS(np, msgin[2]));
cp->link_ccb	= np->ccbc;
np->ccbc	= cp;
xpt_insque_head(&cp->link_ccbq, &np->free_ccbq);
return cp;
}
static ccb_p ncr_ccb_from_dsa(ncb_p np, u_long dsa)
{
int hcode;
ccb_p cp;
hcode = CCB_HASH_CODE(dsa);
cp = np->ccbh[hcode];
while (cp) {
if (cp->p_ccb == dsa)
break;
cp = cp->link_ccbh;
}
return cp;
}
static void ncr_init_tcb (ncb_p np, u_char tn)
{
assert (( (offsetof(struct ncr_reg, nc_sxfer) ^
offsetof(struct tcb    , sval    )) &3) == 0);
assert (( (offsetof(struct ncr_reg, nc_scntl3) ^
offsetof(struct tcb    , wval    )) &3) == 0);
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)){
assert (( (offsetof(struct ncr_reg, nc_scntl4) ^
offsetof(struct tcb    , uval    )) &3) == 0);
}
}
static lcb_p ncr_alloc_lcb (ncb_p np, u_char tn, u_char ln)
{
tcb_p tp = &np->target[tn];
lcb_p lp = ncr_lp(np, tp, ln);
if (lp)
return lp;
ncr_init_tcb(np, tn);
if (ln && !tp->luntbl) {
int i;
tp->luntbl = m_calloc_dma(256, "LUNTBL");
if (!tp->luntbl)
goto fail;
for (i = 0 ; i < 64 ; i++)
tp->luntbl[i] = cpu_to_scr(NCB_PHYS(np, resel_badlun));
tp->b_luntbl = cpu_to_scr(vtobus(tp->luntbl));
}
if (ln && !tp->lmp) {
tp->lmp = m_calloc(MAX_LUN * sizeof(lcb_p), "LMP");
if (!tp->lmp)
goto fail;
}
lp = m_calloc_dma(sizeof(struct lcb), "LCB");
if (!lp)
goto fail;
if (ln) {
tp->lmp[ln] = lp;
tp->luntbl[ln] = cpu_to_scr(vtobus(lp));
}
else {
tp->l0p = lp;
tp->b_lun0 = cpu_to_scr(vtobus(lp));
}
xpt_que_init(&lp->busy_ccbq);
xpt_que_init(&lp->wait_ccbq);
lp->maxnxs	= 1;
lp->tasktbl	= &lp->tasktbl_0;
lp->b_tasktbl	= cpu_to_scr(vtobus(lp->tasktbl));
lp->tasktbl[0]	= cpu_to_scr(np->p_notask);
lp->resel_task	= cpu_to_scr(NCB_SCRIPT_PHYS(np, resel_notag));
lp->busyccbs	= 1;
lp->queuedccbs	= 1;
lp->queuedepth	= 1;
fail:
return lp;
}
static lcb_p ncr_setup_lcb (ncb_p np, u_char tn, u_char ln, u_char *inq_data)
{
tcb_p tp = &np->target[tn];
lcb_p lp = ncr_lp(np, tp, ln);
u_char inq_byte7;
int i;
if (!lp && !(lp = ncr_alloc_lcb(np, tn, ln)))
goto fail;
#if 0
tp->quirks = 0;
if (tp->quirks && bootverbose) {
PRINT_LUN(np, tn, ln);
printk ("quirks=%x.\n", tp->quirks);
}
#endif
inq_byte7 = 0;
if	((inq_data[2] & 0x7) >= 2 && (inq_data[3] & 0xf) == 2)
inq_byte7 = inq_data[7];
else if ((inq_data[2] & 0x7) == 1 && (inq_data[3] & 0xf) == 1)
inq_byte7 = INQ7_SYNC;
if ((inq_data[0] & 0xe0) > 0x20 || (inq_data[0] & 0x1f) == 0x1f)
inq_byte7 &= (INQ7_SYNC | INQ7_WIDE16);
if (driver_setup.force_sync_nego)
inq_byte7 |= INQ7_SYNC;
tp->inq_done = 1;
if ((inq_byte7 ^ tp->inq_byte7) & (INQ7_SYNC | INQ7_WIDE16)) {
tp->inq_byte7 = inq_byte7;
ncr_negotiate(np, tp);
}
if ((inq_byte7 & INQ7_QUEUE) && lp->tasktbl == &lp->tasktbl_0) {
lp->tasktbl = m_calloc_dma(MAX_TASKS*4, "TASKTBL");
if (!lp->tasktbl) {
lp->tasktbl = &lp->tasktbl_0;
goto fail;
}
lp->b_tasktbl = cpu_to_scr(vtobus(lp->tasktbl));
for (i = 0 ; i < MAX_TASKS ; i++)
lp->tasktbl[i] = cpu_to_scr(np->p_notask);
lp->cb_tags = m_calloc(MAX_TAGS, "CB_TAGS");
if (!lp->cb_tags)
goto fail;
for (i = 0 ; i < MAX_TAGS ; i++)
lp->cb_tags[i] = i;
lp->maxnxs = MAX_TAGS;
lp->tags_stime = ktime_get(3*HZ);
}
if ((inq_byte7 ^ lp->inq_byte7) & INQ7_QUEUE) {
lp->inq_byte7 = inq_byte7;
lp->numtags   = lp->maxtags;
ncr_setup_tags (np, tn, ln);
}
fail:
return lp;
}
#ifdef SCSI_NCR_USE_64BIT_DAC
#define SCATTER_ONE(data, badd, len)					\
(data)->addr = cpu_to_scr(badd);				\
(data)->size = cpu_to_scr((((badd) >> 8) & 0xff000000) + len);
#else
#define SCATTER_ONE(data, badd, len)		\
(data)->addr = cpu_to_scr(badd);	\
(data)->size = cpu_to_scr(len);
#endif
#define CROSS_16MB(p, n) (((((u_long) p) + n - 1) ^ ((u_long) p)) & ~0xffffff)
static	int ncr_scatter_no_sglist(ncb_p np, ccb_p cp, Scsi_Cmnd *cmd)
{
struct scr_tblmove *data = &cp->phys.data[MAX_SCATTER-1];
int segment;
cp->data_len = cmd->request_bufflen;
if (cmd->request_bufflen) {
u_long baddr = map_scsi_single_data(np, cmd);
SCATTER_ONE(data, baddr, cmd->request_bufflen);
if (CROSS_16MB(baddr, cmd->request_bufflen)) {
cp->host_flags |= HF_PM_TO_C;
#ifdef DEBUG_896R1
printk("He! we are crossing a 16 MB boundary (0x%lx, 0x%x)\n",
baddr, cmd->request_bufflen);
#endif
}
segment = 1;
}
else
segment = 0;
return segment;
}
static int ncr_scatter_896R1(ncb_p np, ccb_p cp, Scsi_Cmnd *cmd)
{
int segn;
int use_sg = (int) cmd->use_sg;
cp->data_len = 0;
if (!use_sg)
segn = ncr_scatter_no_sglist(np, cp, cmd);
else if (use_sg > MAX_SCATTER)
segn = -1;
else {
struct scatterlist *scatter = (struct scatterlist *)cmd->buffer;
struct scr_tblmove *data;
use_sg = map_scsi_sg_data(np, cmd);
data = &cp->phys.data[MAX_SCATTER - use_sg];
for (segn = 0; segn < use_sg; segn++) {
u_long baddr = scsi_sg_dma_address(&scatter[segn]);
unsigned int len = scsi_sg_dma_len(&scatter[segn]);
SCATTER_ONE(&data[segn],
baddr,
len);
if (CROSS_16MB(baddr, scatter[segn].length)) {
cp->host_flags |= HF_PM_TO_C;
#ifdef DEBUG_896R1
printk("He! we are crossing a 16 MB boundary (0x%lx, 0x%x)\n",
baddr, scatter[segn].length);
#endif
}
cp->data_len += len;
}
}
return segn;
}
static int ncr_scatter(ncb_p np, ccb_p cp, Scsi_Cmnd *cmd)
{
int segment;
int use_sg = (int) cmd->use_sg;
cp->data_len = 0;
if (!use_sg)
segment = ncr_scatter_no_sglist(np, cp, cmd);
else if (use_sg > MAX_SCATTER)
segment = -1;
else {
struct scatterlist *scatter = (struct scatterlist *)cmd->buffer;
struct scr_tblmove *data;
use_sg = map_scsi_sg_data(np, cmd);
data = &cp->phys.data[MAX_SCATTER - use_sg];
for (segment = 0; segment < use_sg; segment++) {
u_long baddr = scsi_sg_dma_address(&scatter[segment]);
unsigned int len = scsi_sg_dma_len(&scatter[segment]);
SCATTER_ONE(&data[segment],
baddr,
len);
cp->data_len += len;
}
}
return segment;
}
#ifndef SCSI_NCR_IOMAPPED
static int __init ncr_regtest (struct ncb* np)
{
register volatile u_int32 data;
data = 0xffffffff;
OUTL_OFF(offsetof(struct ncr_reg, nc_dstat), data);
data = INL_OFF(offsetof(struct ncr_reg, nc_dstat));
#if 1
if (data == 0xffffffff) {
#else
if ((data & 0xe2f0fffd) != 0x02000080) {
#endif
printk ("CACHE TEST FAILED: reg dstat-sstat2 readback %x.\n",
(unsigned) data);
return (0x10);
};
return (0);
}
#endif
static int __init ncr_snooptest (struct ncb* np)
{
u_int32	ncr_rd, ncr_wr, ncr_bk, host_rd, host_wr, pc;
int	i, err=0;
#ifndef SCSI_NCR_IOMAPPED
if (np->reg) {
err |= ncr_regtest (np);
if (err) return (err);
}
#endif
pc  = NCB_SCRIPTH0_PHYS (np, snooptest);
host_wr = 1;
ncr_wr  = 2;
np->ncr_cache = cpu_to_scr(host_wr);
OUTL (nc_temp, ncr_wr);
OUTL (nc_dsa, np->p_ncb);
OUTL_DSP (pc);
for (i=0; i<NCR_SNOOP_TIMEOUT; i++)
if (INB(nc_istat) & (INTF|SIP|DIP))
break;
pc = INL (nc_dsp);
host_rd = scr_to_cpu(np->ncr_cache);
ncr_rd  = INL (nc_scratcha);
ncr_bk  = INL (nc_temp);
if (i>=NCR_SNOOP_TIMEOUT) {
printk ("CACHE TEST FAILED: timeout.\n");
return (0x20);
};
if (pc != NCB_SCRIPTH0_PHYS (np, snoopend)+8) {
printk ("CACHE TEST FAILED: script execution failed.\n");
printk ("start=%08lx, pc=%08lx, end=%08lx\n",
(u_long) NCB_SCRIPTH0_PHYS (np, snooptest), (u_long) pc,
(u_long) NCB_SCRIPTH0_PHYS (np, snoopend) +8);
return (0x40);
};
if (host_wr != ncr_rd) {
printk ("CACHE TEST FAILED: host wrote %d, ncr read %d.\n",
(int) host_wr, (int) ncr_rd);
err |= 1;
};
if (host_rd != ncr_wr) {
printk ("CACHE TEST FAILED: ncr wrote %d, host read %d.\n",
(int) ncr_wr, (int) host_rd);
err |= 2;
};
if (ncr_bk != ncr_wr) {
printk ("CACHE TEST FAILED: ncr wrote %d, read back %d.\n",
(int) ncr_wr, (int) ncr_bk);
err |= 4;
};
return (err);
}
static void ncr_selectclock(ncb_p np, u_char scntl3)
{
if (np->multiplier < 2) {
OUTB(nc_scntl3,	scntl3);
return;
}
if (bootverbose >= 2)
printk ("%s: enabling clock multiplier\n", ncr_name(np));
OUTB(nc_stest1, DBLEN);
if ( (np->device_id != PCI_DEVICE_ID_LSI_53C1010) &&
(np->device_id != PCI_DEVICE_ID_LSI_53C1010_66) &&
(np->multiplier > 2)) {
int i = 20;
while (!(INB(nc_stest4) & LCKFRQ) && --i > 0)
UDELAY (20);
if (!i)
printk("%s: the chip cannot lock the frequency\n",
ncr_name(np));
} else
UDELAY (120);
OUTB(nc_stest3, HSC);
OUTB(nc_scntl3,	scntl3);
OUTB(nc_stest1, (DBLEN|DBLSEL));
OUTB(nc_stest3, 0x00);
}
static unsigned __init ncrgetfreq (ncb_p np, int gen)
{
unsigned int ms = 0;
unsigned int f;
int count;
OUTW (nc_sien , 0x0);
(void) INW (nc_sist);
OUTB (nc_dien , 0);
(void) INW (nc_sist);
OUTB (nc_scntl3, 4);
OUTB (nc_stime1, 0);
OUTB (nc_stime1, gen);
while (!(INW(nc_sist) & GEN) && ms++ < 100000) {
for (count = 0; count < 10; count++)
UDELAY (100);
}
OUTB (nc_stime1, 0);
OUTB (nc_scntl3, 0);
#if 0
if (np->device_id == PCI_DEVICE_ID_LSI_53C1010)
f = ms ? ((1 << gen) * 2866 ) / ms : 0;
else
#endif
f = ms ? ((1 << gen) * 4340) / ms : 0;
if (bootverbose >= 2)
printk ("%s: Delay (GEN=%d): %u msec, %u KHz\n",
ncr_name(np), gen, ms, f);
return f;
}
static unsigned __init ncr_getfreq (ncb_p np)
{
u_int f1, f2;
int gen = 11;
(void) ncrgetfreq (np, gen);
f1 = ncrgetfreq (np, gen);
f2 = ncrgetfreq (np, gen);
if (f1 > f2) f1 = f2;
return f1;
}
static void __init ncr_getclock (ncb_p np, int mult)
{
unsigned char scntl3 = np->sv_scntl3;
unsigned char stest1 = np->sv_stest1;
unsigned f1;
np->multiplier = 1;
f1 = 40000;
if (mult > 1 && (stest1 & (DBLEN+DBLSEL)) == DBLEN+DBLSEL) {
if (bootverbose >= 2)
printk ("%s: clock multiplier found\n", ncr_name(np));
np->multiplier = mult;
}
if ((np->device_id == PCI_DEVICE_ID_LSI_53C1010)  ||
(np->device_id == PCI_DEVICE_ID_LSI_53C1010_66)) {
f1=40000;
np->multiplier = mult;
if (bootverbose >= 2)
printk ("%s: clock multiplier assumed\n", ncr_name(np));
}
else if (np->multiplier != mult || (scntl3 & 7) < 3 || !(scntl3 & 1)) {
OUTB (nc_stest1, 0);
f1 = ncr_getfreq (np);
if (bootverbose)
printk ("%s: NCR clock is %uKHz\n", ncr_name(np), f1);
if	(f1 < 55000)		f1 =  40000;
else				f1 =  80000;
if (np->features & FE_66MHZ) {
np->pciclock_min = (66000*55+80-1)/80;
np->pciclock_max = (66000*55)/40;
}
else {
np->pciclock_min = (33000*55+80-1)/80;
np->pciclock_max = (33000*55)/40;
}
if (f1 == 40000 && mult > 1) {
if (bootverbose >= 2)
printk ("%s: clock multiplier assumed\n", ncr_name(np));
np->multiplier	= mult;
}
} else {
if	((scntl3 & 7) == 3)	f1 =  40000;
else if	((scntl3 & 7) == 5)	f1 =  80000;
else 				f1 = 160000;
f1 /= np->multiplier;
}
f1		*= np->multiplier;
np->clock_khz	= f1;
}
static u_int __init ncr_getpciclock (ncb_p np)
{
static u_int f;
OUTB (nc_stest1, SCLK);
f = ncr_getfreq (np);
OUTB (nc_stest1, 0);
return f;
}
#ifndef uchar
#define uchar unsigned char
#endif
#ifndef ushort
#define ushort unsigned short
#endif
#ifndef ulong
#define ulong unsigned long
#endif
#ifdef MODULE
#define	ARG_SEP	' '
#else
#define	ARG_SEP	','
#endif
#define OPT_TAGS		1
#define OPT_MASTER_PARITY	2
#define OPT_SCSI_PARITY		3
#define OPT_DISCONNECTION	4
#define OPT_SPECIAL_FEATURES	5
#define OPT_ULTRA_SCSI		6
#define OPT_FORCE_SYNC_NEGO	7
#define OPT_REVERSE_PROBE	8
#define OPT_DEFAULT_SYNC	9
#define OPT_VERBOSE		10
#define OPT_DEBUG		11
#define OPT_BURST_MAX		12
#define OPT_LED_PIN		13
#define OPT_MAX_WIDE		14
#define OPT_SETTLE_DELAY	15
#define OPT_DIFF_SUPPORT	16
#define OPT_IRQM		17
#define OPT_PCI_FIX_UP		18
#define OPT_BUS_CHECK		19
#define OPT_OPTIMIZE		20
#define OPT_RECOVERY		21
#define OPT_SAFE_SETUP		22
#define OPT_USE_NVRAM		23
#define OPT_EXCLUDE		24
#define OPT_HOST_ID		25
#ifdef SCSI_NCR_IARB_SUPPORT
#define OPT_IARB		26
#endif
static char setup_token[] __initdata =
"tags:"   "mpar:"
"spar:"   "disc:"
"specf:"  "ultra:"
"fsn:"    "revprob:"
"sync:"   "verb:"
"debug:"  "burst:"
"led:"    "wide:"
"settle:" "diff:"
"irqm:"   "pcifix:"
"buschk:" "optim:"
"recovery:"
"safe:"   "nvram:"
"excl:"   "hostid:"
#ifdef SCSI_NCR_IARB_SUPPORT
"iarb:"
#endif
;
#ifdef MODULE
#define	ARG_SEP	' '
#else
#define	ARG_SEP	','
#endif
static int __init get_setup_token(char *p)
{
char *cur = setup_token;
char *pc;
int i = 0;
while (cur != NULL && (pc = strchr(cur, ':')) != NULL) {
++pc;
++i;
if (!strncmp(p, cur, pc - cur))
return i;
cur = pc;
}
return 0;
}
int __init sym53c8xx_setup(char *str)
{
#ifdef SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT
char *cur = str;
char *pc, *pv;
unsigned long val;
int i,  c;
int xi = 0;
while (cur != NULL && (pc = strchr(cur, ':')) != NULL) {
char *pe;
val = 0;
pv = pc;
c = *++pv;
if	(c == 'n')
val = 0;
else if	(c == 'y')
val = 1;
else
val = (int) simple_strtoul(pv, &pe, 0);
switch (get_setup_token(cur)) {
case OPT_TAGS:
driver_setup.default_tags = val;
if (pe && *pe == '/') {
i = 0;
while (*pe && *pe != ARG_SEP &&
i < sizeof(driver_setup.tag_ctrl)-1) {
driver_setup.tag_ctrl[i++] = *pe++;
}
driver_setup.tag_ctrl[i] = '\0';
}
break;
case OPT_MASTER_PARITY:
driver_setup.master_parity = val;
break;
case OPT_SCSI_PARITY:
driver_setup.scsi_parity = val;
break;
case OPT_DISCONNECTION:
driver_setup.disconnection = val;
break;
case OPT_SPECIAL_FEATURES:
driver_setup.special_features = val;
break;
case OPT_ULTRA_SCSI:
driver_setup.ultra_scsi	= val;
break;
case OPT_FORCE_SYNC_NEGO:
driver_setup.force_sync_nego = val;
break;
case OPT_REVERSE_PROBE:
driver_setup.reverse_probe = val;
break;
case OPT_DEFAULT_SYNC:
driver_setup.default_sync = val;
break;
case OPT_VERBOSE:
driver_setup.verbose = val;
break;
case OPT_DEBUG:
driver_setup.debug = val;
break;
case OPT_BURST_MAX:
driver_setup.burst_max = val;
break;
case OPT_LED_PIN:
driver_setup.led_pin = val;
break;
case OPT_MAX_WIDE:
driver_setup.max_wide = val? 1:0;
break;
case OPT_SETTLE_DELAY:
driver_setup.settle_delay = val;
break;
case OPT_DIFF_SUPPORT:
driver_setup.diff_support = val;
break;
case OPT_IRQM:
driver_setup.irqm = val;
break;
case OPT_PCI_FIX_UP:
driver_setup.pci_fix_up	= val;
break;
case OPT_BUS_CHECK:
driver_setup.bus_check = val;
break;
case OPT_OPTIMIZE:
driver_setup.optimize = val;
break;
case OPT_RECOVERY:
driver_setup.recovery = val;
break;
case OPT_USE_NVRAM:
driver_setup.use_nvram = val;
break;
case OPT_SAFE_SETUP:
memcpy(&driver_setup, &driver_safe_setup,
sizeof(driver_setup));
break;
case OPT_EXCLUDE:
if (xi < SCSI_NCR_MAX_EXCLUDES)
driver_setup.excludes[xi++] = val;
break;
case OPT_HOST_ID:
driver_setup.host_id = val;
break;
#ifdef SCSI_NCR_IARB_SUPPORT
case OPT_IARB:
driver_setup.iarb = val;
break;
#endif
default:
printk("sym53c8xx_setup: unexpected boot option '%.*s' ignored\n", (int)(pc-cur+1), cur);
break;
}
if ((cur = strchr(cur, ARG_SEP)) != NULL)
++cur;
}
#endif
return 1;
}
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,3,13)
#ifndef MODULE
__setup("sym53c8xx=", sym53c8xx_setup);
#endif
#endif
static int
sym53c8xx_pci_init(Scsi_Host_Template *tpnt, pcidev_t pdev, ncr_device *device);
static void __init ncr_print_driver_setup(void)
{
#define YesNo(y)	y ? 'y' : 'n'
printk (NAME53C8XX ": setup=disc:%c,specf:%d,ultra:%d,tags:%d,sync:%d,"
"burst:%d,wide:%c,diff:%d,revprob:%c,buschk:0x%x\n",
YesNo(driver_setup.disconnection),
driver_setup.special_features,
driver_setup.ultra_scsi,
driver_setup.default_tags,
driver_setup.default_sync,
driver_setup.burst_max,
YesNo(driver_setup.max_wide),
driver_setup.diff_support,
YesNo(driver_setup.reverse_probe),
driver_setup.bus_check);
printk (NAME53C8XX ": setup=mpar:%c,spar:%c,fsn=%c,verb:%d,debug:0x%x,"
"led:%c,settle:%d,irqm:0x%x,nvram:0x%x,pcifix:0x%x\n",
YesNo(driver_setup.master_parity),
YesNo(driver_setup.scsi_parity),
YesNo(driver_setup.force_sync_nego),
driver_setup.verbose,
driver_setup.debug,
YesNo(driver_setup.led_pin),
driver_setup.settle_delay,
driver_setup.irqm,
driver_setup.use_nvram,
driver_setup.pci_fix_up);
#undef YesNo
}
static ncr_chip	ncr_chip_table[] __initdata	= SCSI_NCR_CHIP_TABLE;
static ushort	ncr_chip_ids[]   __initdata	= SCSI_NCR_CHIP_IDS;
#ifdef	SCSI_NCR_PQS_PDS_SUPPORT
#define	SCSI_NCR_MAX_PQS_BUS	16
static int pqs_bus[SCSI_NCR_MAX_PQS_BUS] __initdata = { 0 };
static void __init ncr_detect_pqs_pds(void)
{
short index;
pcidev_t dev = PCIDEV_NULL;
for(index=0; index < SCSI_NCR_MAX_PQS_BUS; index++) {
u_char tmp;
dev = pci_find_device(0x101a, 0x0009, dev);
if (dev == PCIDEV_NULL) {
pqs_bus[index] = -1;
break;
}
printk(KERN_INFO NAME53C8XX ": NCR PQS/PDS memory controller detected on bus %d\n", PciBusNumber(dev));
pci_read_config_byte(dev, 0x44, &tmp);
tmp |= 0x2;
pci_write_config_byte(dev, 0x44, tmp);
pci_read_config_byte(dev, 0x45, &tmp);
tmp |= 0x4;
pci_write_config_byte(dev, 0x45, tmp);
pqs_bus[index] = PciBusNumber(dev);
}
}
#endif
int __init sym53c8xx_detect(Scsi_Host_Template *tpnt)
{
pcidev_t pcidev;
int i, j, chips, hosts, count;
int attach_count = 0;
ncr_device *devtbl, *devp;
#ifdef SCSI_NCR_NVRAM_SUPPORT
ncr_nvram  nvram0, nvram, *nvp;
#endif
if (!pci_present())
return 0;
#ifdef SCSI_NCR_PROC_INFO_SUPPORT
#if LINUX_VERSION_CODE < LinuxVersionCode(2,3,27)
tpnt->proc_dir  = &proc_scsi_sym53c8xx;
#else
tpnt->proc_name = NAME53C8XX;
#endif
tpnt->proc_info = sym53c8xx_proc_info;
#endif
#if	defined(SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT) && defined(MODULE)
if (sym53c8xx)
sym53c8xx_setup(sym53c8xx);
#endif
#ifdef SCSI_NCR_DEBUG_INFO_SUPPORT
ncr_debug = driver_setup.debug;
#endif
if (initverbose >= 2)
ncr_print_driver_setup();
devtbl = m_calloc(PAGE_SIZE, "devtbl");
if (!devtbl)
return 0;
#ifdef	SCSI_NCR_PQS_PDS_SUPPORT
ncr_detect_pqs_pds();
#endif
chips	= sizeof(ncr_chip_ids)	/ sizeof(ncr_chip_ids[0]);
hosts	= PAGE_SIZE		/ sizeof(*devtbl);
#ifdef SCSI_NCR_NVRAM_SUPPORT
nvp = (driver_setup.use_nvram & 0x1) ? &nvram0 : 0;
#endif
j = 0;
count = 0;
pcidev = PCIDEV_NULL;
while (1) {
char *msg = "";
if (count >= hosts)
break;
if (j >= chips)
break;
i = driver_setup.reverse_probe ? chips - 1 - j : j;
pcidev = pci_find_device(PCI_VENDOR_ID_NCR, ncr_chip_ids[i],
pcidev);
if (pcidev == PCIDEV_NULL) {
++j;
continue;
}
for (i = 0; i < count ; i++) {
if (devtbl[i].slot.bus	     == PciBusNumber(pcidev) &&
devtbl[i].slot.device_fn == PciDeviceFn(pcidev))
break;
}
if (i != count)
continue;
devp = &devtbl[count];
devp->host_id = driver_setup.host_id;
devp->attach_done = 0;
if (sym53c8xx_pci_init(tpnt, pcidev, devp)) {
continue;
}
++count;
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (nvp) {
ncr_get_nvram(devp, nvp);
switch(nvp->type) {
case SCSI_NCR_SYMBIOS_NVRAM:
nvp = &nvram;
msg = "with Symbios NVRAM";
break;
case SCSI_NCR_TEKRAM_NVRAM:
msg = "with Tekram NVRAM";
break;
}
}
#endif
#ifdef	SCSI_NCR_PQS_PDS_SUPPORT
if (devp->pqs_pds)
msg = "(NCR PQS/PDS)";
#endif
printk(KERN_INFO NAME53C8XX ": 53c%s detected %s\n",
devp->chip.name, msg);
}
#ifdef SCSI_NCR_NVRAM_SUPPORT
if (!nvp || nvram0.type != SCSI_NCR_SYMBIOS_NVRAM)
goto next;
for (i = 0; i < 4; i++) {
Symbios_host *h = &nvram0.data.Symbios.host[i];
for (j = 0 ; j < count ; j++) {
devp = &devtbl[j];
if (h->device_fn != devp->slot.device_fn ||
h->bus_nr	 != devp->slot.bus	 ||
h->device_id != devp->chip.device_id)
continue;
if (devp->attach_done)
continue;
if (h->flags & SYMBIOS_INIT_SCAN_AT_BOOT) {
ncr_get_nvram(devp, nvp);
if (!ncr_attach (tpnt, attach_count, devp))
attach_count++;
}
else if (!(driver_setup.use_nvram & 0x80))
printk(KERN_INFO NAME53C8XX
": 53c%s state OFF thus not attached\n",
devp->chip.name);
else
continue;
devp->attach_done = 1;
break;
}
}
next:
#endif
for (i= 0; i < count; i++) {
devp = &devtbl[i];
if (!devp->attach_done) {
#ifdef SCSI_NCR_NVRAM_SUPPORT
ncr_get_nvram(devp, nvp);
#endif
if (!ncr_attach (tpnt, attach_count, devp))
attach_count++;
}
}
m_free(devtbl, PAGE_SIZE, "devtbl");
return attach_count;
}
static int __init
sym53c8xx_pci_init(Scsi_Host_Template *tpnt, pcidev_t pdev, ncr_device *device)
{
u_short vendor_id, device_id, command, status_reg;
u_char cache_line_size, latency_timer;
u_char suggested_cache_line_size = 0;
u_char pci_fix_up = driver_setup.pci_fix_up;
u_char revision;
u_int irq;
u_long base, base_2, io_port;
int i;
ncr_chip *chip;
printk(KERN_INFO NAME53C8XX ": at PCI bus %d, device %d, function %d\n",
PciBusNumber(pdev),
(int) (PciDeviceFn(pdev) & 0xf8) >> 3,
(int) (PciDeviceFn(pdev) & 7));
#ifdef SCSI_NCR_DYNAMIC_DMA_MAPPING
if (!pci_dma_supported(pdev, (dma_addr_t) (0xffffffffUL))) {
printk(KERN_WARNING NAME53C8XX
"32 BIT PCI BUS DMA ADDRESSING NOT SUPPORTED\n");
return -1;
}
#endif
vendor_id = PciVendorId(pdev);
device_id = PciDeviceId(pdev);
irq	  = PciIrqLine(pdev);
i =	0;
i =	pci_get_base_address(pdev, i, &io_port);
i =	pci_get_base_address(pdev, i, &base);
(void)	pci_get_base_address(pdev, i, &base_2);
pci_read_config_word(pdev, PCI_COMMAND,		&command);
pci_read_config_byte(pdev, PCI_CLASS_REVISION,	&revision);
pci_read_config_byte(pdev, PCI_CACHE_LINE_SIZE,	&cache_line_size);
pci_read_config_byte(pdev, PCI_LATENCY_TIMER,	&latency_timer);
pci_read_config_word(pdev, PCI_STATUS,		&status_reg);
#ifdef SCSI_NCR_PQS_PDS_SUPPORT
for(i = 0; i < SCSI_NCR_MAX_PQS_BUS && pqs_bus[i] != -1; i++) {
u_char tmp;
if (pqs_bus[i] == PciBusNumber(pdev)) {
pci_read_config_byte(pdev, 0x84, &tmp);
device->pqs_pds = 1;
device->host_id = tmp;
break;
}
}
#endif
for (i = 0 ; i < SCSI_NCR_MAX_EXCLUDES ; i++) {
if (driver_setup.excludes[i] ==
(io_port & PCI_BASE_ADDRESS_IO_MASK))
return -1;
}
chip = 0;
for (i = 0; i < sizeof(ncr_chip_table)/sizeof(ncr_chip_table[0]); i++) {
if (device_id != ncr_chip_table[i].device_id)
continue;
if (revision > ncr_chip_table[i].revision_id)
continue;
if (!(ncr_chip_table[i].features & FE_LDSTR))
break;
chip = &device->chip;
memcpy(chip, &ncr_chip_table[i], sizeof(*chip));
chip->revision_id = revision;
break;
}
#if defined(__i386__) && !defined(SCSI_NCR_PCI_MEM_NOT_SUPPORTED)
if (chip && (base_2 & PCI_BASE_ADDRESS_MEM_MASK)) {
unsigned int ram_size, ram_val;
u_long ram_ptr;
if (chip->features & FE_RAM8K)
ram_size = 8192;
else
ram_size = 4096;
ram_ptr = remap_pci_mem(base_2 & PCI_BASE_ADDRESS_MEM_MASK,
ram_size);
if (ram_ptr) {
ram_val = readl_raw(ram_ptr + ram_size - 16);
unmap_pci_mem(ram_ptr, ram_size);
if (ram_val == 0x52414944) {
printk(NAME53C8XX": not initializing, "
"driven by SISL RAID controller.\n");
return -1;
}
}
}
#endif
if (!chip) {
printk(NAME53C8XX ": not initializing, device not supported\n");
return -1;
}
#ifdef __powerpc__
if ((command & (PCI_COMMAND_IO | PCI_COMMAND_MEMORY))
!= (PCI_COMMAND_IO | PCI_COMMAND_MEMORY)) {
printk(NAME53C8XX ": setting%s%s...\n",
(command & PCI_COMMAND_IO)     ? "" : " PCI_COMMAND_IO",
(command & PCI_COMMAND_MEMORY) ? "" : " PCI_COMMAND_MEMORY");
command |= (PCI_COMMAND_IO | PCI_COMMAND_MEMORY);
pci_write_config_word(pdev, PCI_COMMAND, command);
}
#if LINUX_VERSION_CODE < LinuxVersionCode(2,2,0)
if ( is_prep ) {
if (io_port >= 0x10000000) {
printk(NAME53C8XX ": reallocating io_port (Wacky IBM)");
io_port = (io_port & 0x00FFFFFF) | 0x01000000;
pci_write_config_dword(pdev,
PCI_BASE_ADDRESS_0, io_port);
}
if (base >= 0x10000000) {
printk(NAME53C8XX ": reallocating base (Wacky IBM)");
base = (base & 0x00FFFFFF) | 0x01000000;
pci_write_config_dword(pdev,
PCI_BASE_ADDRESS_1, base);
}
if (base_2 >= 0x10000000) {
printk(NAME53C8XX ": reallocating base2 (Wacky IBM)");
base_2 = (base_2 & 0x00FFFFFF) | 0x01000000;
pci_write_config_dword(pdev,
PCI_BASE_ADDRESS_2, base_2);
}
}
#endif
#endif
#if defined(__sparc__) && (LINUX_VERSION_CODE < LinuxVersionCode(2,3,0))
base = __pa(base);
base_2 = __pa(base_2);
if (!cache_line_size)
suggested_cache_line_size = 16;
driver_setup.pci_fix_up |= 0x7;
#endif
#if defined(__i386__) && !defined(MODULE)
if (!cache_line_size) {
#if LINUX_VERSION_CODE < LinuxVersionCode(2,1,75)
extern char x86;
switch(x86) {
#else
switch(boot_cpu_data.x86) {
#endif
case 4:	suggested_cache_line_size = 4; break;
case 6:
case 5:	suggested_cache_line_size = 8; break;
}
}
#endif
#if 1
if (!(command & PCI_COMMAND_IO)) {
printk(NAME53C8XX ": I/O base address (0x%lx) disabled.\n",
(long) io_port);
io_port = 0;
}
#endif
if (!(command & PCI_COMMAND_MEMORY)) {
printk(NAME53C8XX ": PCI_COMMAND_MEMORY not set.\n");
base	= 0;
base_2	= 0;
}
io_port &= PCI_BASE_ADDRESS_IO_MASK;
base	&= PCI_BASE_ADDRESS_MEM_MASK;
base_2	&= PCI_BASE_ADDRESS_MEM_MASK;
#if 1
if (io_port && check_region (io_port, 128)) {
printk(NAME53C8XX ": IO region 0x%lx[0..127] is in use\n",
(long) io_port);
io_port = 0;
}
if (!io_port)
return -1;
#endif
#ifndef SCSI_NCR_IOMAPPED
if (!base) {
printk(NAME53C8XX ": MMIO base address disabled.\n");
return -1;
}
#endif
if ((command & (PCI_COMMAND_MASTER | PCI_COMMAND_PARITY))
!= (PCI_COMMAND_MASTER | PCI_COMMAND_PARITY)) {
printk(NAME53C8XX ": setting%s%s...(fix-up)\n",
(command & PCI_COMMAND_MASTER) ? "" : " PCI_COMMAND_MASTER",
(command & PCI_COMMAND_PARITY) ? "" : " PCI_COMMAND_PARITY");
command |= (PCI_COMMAND_MASTER | PCI_COMMAND_PARITY);
pci_write_config_word(pdev, PCI_COMMAND, command);
}
if (!(driver_setup.special_features & 1))
chip->features &= ~FE_SPECIAL_SET;
else {
if (driver_setup.special_features & 2)
chip->features &= ~FE_WRIE;
if (driver_setup.special_features & 4)
chip->features &= ~FE_NOPM;
}
if (chip->features & FE_66MHZ) {
if (!(status_reg & PCI_STATUS_66MHZ))
chip->features &= ~FE_66MHZ;
}
else {
if (status_reg & PCI_STATUS_66MHZ) {
status_reg = PCI_STATUS_66MHZ;
pci_write_config_word(pdev, PCI_STATUS, status_reg);
pci_read_config_word(pdev, PCI_STATUS, &status_reg);
}
}
if (driver_setup.ultra_scsi < 3 && (chip->features & FE_ULTRA3)) {
chip->features |=  FE_ULTRA2;
chip->features &= ~FE_ULTRA3;
}
if (driver_setup.ultra_scsi < 2 && (chip->features & FE_ULTRA2)) {
chip->features |=  FE_ULTRA;
chip->features &= ~FE_ULTRA2;
}
if (driver_setup.ultra_scsi < 1)
chip->features &= ~FE_ULTRA;
if (!driver_setup.max_wide)
chip->features &= ~FE_WIDE;
if (!driver_setup.max_wide && (chip->features & FE_ULTRA3)) {
chip->features |= FE_ULTRA2;
chip->features |= ~FE_ULTRA3;
}
if (device_id == PCI_DEVICE_ID_NCR_53C896 && revision <= 0x10) {
chip->features	|= (FE_WRIE | FE_CLSE);
pci_fix_up	|=  3;
}
#ifdef	SCSI_NCR_PCI_FIX_UP_SUPPORT
if ((pci_fix_up & 1) && (chip->features & FE_CLSE) &&
!cache_line_size && suggested_cache_line_size) {
cache_line_size = suggested_cache_line_size;
pci_write_config_byte(pdev,
PCI_CACHE_LINE_SIZE, cache_line_size);
printk(NAME53C8XX ": PCI_CACHE_LINE_SIZE set to %d (fix-up).\n",
cache_line_size);
}
if ((pci_fix_up & 2) && cache_line_size &&
(chip->features & FE_WRIE) && !(command & PCI_COMMAND_INVALIDATE)) {
printk(NAME53C8XX": setting PCI_COMMAND_INVALIDATE (fix-up)\n");
command |= PCI_COMMAND_INVALIDATE;
pci_write_config_word(pdev, PCI_COMMAND, command);
}
if (chip->burst_max && (latency_timer == 0 || (pci_fix_up & 4))) {
uchar lt = (1 << chip->burst_max) + 6 + 10;
if (latency_timer < lt) {
printk(NAME53C8XX
": changing PCI_LATENCY_TIMER from %d to %d.\n",
(int) latency_timer, (int) lt);
latency_timer = lt;
pci_write_config_byte(pdev,
PCI_LATENCY_TIMER, latency_timer);
}
}
#endif
device->pdev		= pdev;
device->slot.bus	= PciBusNumber(pdev);
device->slot.device_fn	= PciDeviceFn(pdev);
device->slot.base	= base;
device->slot.base_2	= base_2;
device->slot.io_port	= io_port;
device->slot.irq	= irq;
device->attach_done	= 0;
return 0;
}
#ifdef SCSI_NCR_NVRAM_SUPPORT
static void __init ncr_get_nvram(ncr_device *devp, ncr_nvram *nvp)
{
devp->nvram = nvp;
if (!nvp)
return;
#ifdef SCSI_NCR_IOMAPPED
request_region(devp->slot.io_port, 128, NAME53C8XX);
devp->slot.base_io = devp->slot.io_port;
#else
devp->slot.reg = (struct ncr_reg *) remap_pci_mem(devp->slot.base, 128);
if (!devp->slot.reg)
return;
#endif
if	(!sym_read_Symbios_nvram(&devp->slot, &nvp->data.Symbios))
nvp->type = SCSI_NCR_SYMBIOS_NVRAM;
else if	(!sym_read_Tekram_nvram(&devp->slot, devp->chip.device_id,
&nvp->data.Tekram))
nvp->type = SCSI_NCR_TEKRAM_NVRAM;
else {
nvp->type = 0;
devp->nvram = 0;
}
#ifdef SCSI_NCR_IOMAPPED
release_region(devp->slot.base_io, 128);
#else
unmap_pci_mem((u_long) devp->slot.reg, 128ul);
#endif
}
#endif
#define DEF_DEPTH	(driver_setup.default_tags)
#define ALL_TARGETS	-2
#define NO_TARGET	-1
#define ALL_LUNS	-2
#define NO_LUN		-1
static int device_queue_depth(ncb_p np, int target, int lun)
{
int c, h, t, u, v;
char *p = driver_setup.tag_ctrl;
char *ep;
h = -1;
t = NO_TARGET;
u = NO_LUN;
while ((c = *p++) != 0) {
v = simple_strtoul(p, &ep, 0);
switch(c) {
case '/':
++h;
t = ALL_TARGETS;
u = ALL_LUNS;
break;
case 't':
if (t != target)
t = (target == v) ? v : NO_TARGET;
u = ALL_LUNS;
break;
case 'u':
if (u != lun)
u = (lun == v) ? v : NO_LUN;
break;
case 'q':
if (h == np->unit &&
(t == ALL_TARGETS || t == target) &&
(u == ALL_LUNS    || u == lun))
return v;
break;
case '-':
t = ALL_TARGETS;
u = ALL_LUNS;
break;
default:
break;
}
p = ep;
}
return DEF_DEPTH;
}
static void sym53c8xx_select_queue_depths(struct Scsi_Host *host, struct scsi_device *devlist)
{
struct scsi_device *device;
for (device = devlist; device; device = device->next) {
ncb_p np;
tcb_p tp;
lcb_p lp;
int numtags;
if (device->host != host)
continue;
np = ((struct host_data *) host->hostdata)->ncb;
tp = &np->target[device->id];
lp = ncr_lp(np, tp, device->lun);
numtags = device_queue_depth(np, device->id, device->lun);
if (numtags > tp->usrtags)
numtags = tp->usrtags;
if (!device->tagged_supported)
numtags = 1;
device->queue_depth = numtags;
if (device->queue_depth < 2)
device->queue_depth = 2;
if (device->queue_depth > MAX_TAGS)
device->queue_depth = MAX_TAGS;
if (lp) {
lp->numtags = lp->maxtags = numtags;
lp->scdev_depth = device->queue_depth;
}
ncr_setup_tags (np, device->id, device->lun);
#ifdef DEBUG_SYM53C8XX
printk("sym53c8xx_select_queue_depth: host=%d, id=%d, lun=%d, depth=%d\n",
np->unit, device->id, device->lun, device->queue_depth);
#endif
}
}
const char *sym53c8xx_info (struct Scsi_Host *host)
{
return SCSI_NCR_DRIVER_NAME;
}
int sym53c8xx_queue_command (Scsi_Cmnd *cmd, void (* done)(Scsi_Cmnd *))
{
ncb_p np = ((struct host_data *) cmd->host->hostdata)->ncb;
unsigned long flags;
int sts;
#ifdef DEBUG_SYM53C8XX
printk("sym53c8xx_queue_command\n");
#endif
cmd->scsi_done     = done;
cmd->host_scribble = NULL;
cmd->SCp.ptr       = NULL;
cmd->SCp.buffer    = NULL;
#ifdef SCSI_NCR_DYNAMIC_DMA_MAPPING
cmd->__data_mapped = 0;
cmd->__data_mapping = 0;
#endif
NCR_LOCK_NCB(np, flags);
if ((sts = ncr_queue_command(np, cmd)) != DID_OK) {
SetScsiResult(cmd, sts, 0);
#ifdef DEBUG_SYM53C8XX
printk("sym53c8xx : command not queued - result=%d\n", sts);
#endif
}
#ifdef DEBUG_SYM53C8XX
else
printk("sym53c8xx : command successfully queued\n");
#endif
NCR_UNLOCK_NCB(np, flags);
if (sts != DID_OK) {
unmap_scsi_data(np, cmd);
done(cmd);
}
return sts;
}
static void sym53c8xx_intr(int irq, void *dev_id, struct pt_regs * regs)
{
unsigned long flags;
ncb_p np = (ncb_p) dev_id;
Scsi_Cmnd *done_list;
#ifdef DEBUG_SYM53C8XX
printk("sym53c8xx : interrupt received\n");
#endif
if (DEBUG_FLAGS & DEBUG_TINY) printk ("[");
NCR_LOCK_NCB(np, flags);
ncr_exception(np);
done_list     = np->done_list;
np->done_list = 0;
NCR_UNLOCK_NCB(np, flags);
if (DEBUG_FLAGS & DEBUG_TINY) printk ("]\n");
if (done_list) {
NCR_LOCK_SCSI_DONE(np, flags);
ncr_flush_done_cmds(done_list);
NCR_UNLOCK_SCSI_DONE(np, flags);
}
}
static void sym53c8xx_timeout(unsigned long npref)
{
ncb_p np = (ncb_p) npref;
unsigned long flags;
Scsi_Cmnd *done_list;
NCR_LOCK_NCB(np, flags);
ncr_timeout((ncb_p) np);
done_list     = np->done_list;
np->done_list = 0;
NCR_UNLOCK_NCB(np, flags);
if (done_list) {
NCR_LOCK_SCSI_DONE(np, flags);
ncr_flush_done_cmds(done_list);
NCR_UNLOCK_SCSI_DONE(np, flags);
}
}
#if defined SCSI_RESET_SYNCHRONOUS && defined SCSI_RESET_ASYNCHRONOUS
int sym53c8xx_reset(Scsi_Cmnd *cmd, unsigned int reset_flags)
#else
int sym53c8xx_reset(Scsi_Cmnd *cmd)
#endif
{
ncb_p np = ((struct host_data *) cmd->host->hostdata)->ncb;
int sts;
unsigned long flags;
Scsi_Cmnd *done_list;
#if defined SCSI_RESET_SYNCHRONOUS && defined SCSI_RESET_ASYNCHRONOUS
printk("sym53c8xx_reset: pid=%lu reset_flags=%x serial_number=%ld serial_number_at_timeout=%ld\n",
cmd->pid, reset_flags, cmd->serial_number, cmd->serial_number_at_timeout);
#else
printk("sym53c8xx_reset: command pid %lu\n", cmd->pid);
#endif
NCR_LOCK_NCB(np, flags);
#if defined SCSI_RESET_NOT_RUNNING
if (cmd->serial_number != cmd->serial_number_at_timeout) {
sts = SCSI_RESET_NOT_RUNNING;
goto out;
}
#endif
#if defined SCSI_RESET_SYNCHRONOUS && defined SCSI_RESET_ASYNCHRONOUS
sts = ncr_reset_bus(np, cmd,
(reset_flags & (SCSI_RESET_SYNCHRONOUS | SCSI_RESET_ASYNCHRONOUS)) == SCSI_RESET_SYNCHRONOUS);
#else
sts = ncr_reset_bus(np, cmd, 0);
#endif
#if defined SCSI_RESET_HOST_RESET
if (sts == SCSI_RESET_SUCCESS)
sts |= SCSI_RESET_HOST_RESET;
#endif
out:
done_list     = np->done_list;
np->done_list = 0;
NCR_UNLOCK_NCB(np, flags);
ncr_flush_done_cmds(done_list);
return sts;
}
int sym53c8xx_abort(Scsi_Cmnd *cmd)
{
ncb_p np = ((struct host_data *) cmd->host->hostdata)->ncb;
int sts;
unsigned long flags;
Scsi_Cmnd *done_list;
#if defined SCSI_RESET_SYNCHRONOUS && defined SCSI_RESET_ASYNCHRONOUS
printk("sym53c8xx_abort: pid=%lu serial_number=%ld serial_number_at_timeout=%ld\n",
cmd->pid, cmd->serial_number, cmd->serial_number_at_timeout);
#else
printk("sym53c8xx_abort: command pid %lu\n", cmd->pid);
#endif
NCR_LOCK_NCB(np, flags);
#if defined SCSI_RESET_SYNCHRONOUS && defined SCSI_RESET_ASYNCHRONOUS
if (cmd->serial_number != cmd->serial_number_at_timeout) {
sts = SCSI_ABORT_NOT_RUNNING;
goto out;
}
#endif
sts = ncr_abort_command(np, cmd);
out:
done_list     = np->done_list;
np->done_list = 0;
NCR_UNLOCK_NCB(np, flags);
ncr_flush_done_cmds(done_list);
return sts;
}
#ifdef MODULE
int sym53c8xx_release(struct Scsi_Host *host)
{
#ifdef DEBUG_SYM53C8XX
printk("sym53c8xx : release\n");
#endif
ncr_detach(((struct host_data *) host->hostdata)->ncb);
return 1;
}
#endif
#define next_wcmd host_scribble
static void insert_into_waiting_list(ncb_p np, Scsi_Cmnd *cmd)
{
Scsi_Cmnd *wcmd;
#ifdef DEBUG_WAITING_LIST
printk("%s: cmd %lx inserted into waiting list\n", ncr_name(np), (u_long) cmd);
#endif
cmd->next_wcmd = 0;
if (!(wcmd = np->waiting_list)) np->waiting_list = cmd;
else {
while ((wcmd->next_wcmd) != 0)
wcmd = (Scsi_Cmnd *) wcmd->next_wcmd;
wcmd->next_wcmd = (char *) cmd;
}
}
static Scsi_Cmnd *retrieve_from_waiting_list(int to_remove, ncb_p np, Scsi_Cmnd *cmd)
{
Scsi_Cmnd **pcmd = &np->waiting_list;
while (*pcmd) {
if (cmd == *pcmd) {
if (to_remove) {
*pcmd = (Scsi_Cmnd *) cmd->next_wcmd;
cmd->next_wcmd = 0;
}
#ifdef DEBUG_WAITING_LIST
printk("%s: cmd %lx retrieved from waiting list\n", ncr_name(np), (u_long) cmd);
#endif
return cmd;
}
pcmd = (Scsi_Cmnd **) &(*pcmd)->next_wcmd;
}
return 0;
}
static void process_waiting_list(ncb_p np, int sts)
{
Scsi_Cmnd *waiting_list, *wcmd;
waiting_list = np->waiting_list;
np->waiting_list = 0;
#ifdef DEBUG_WAITING_LIST
if (waiting_list) printk("%s: waiting_list=%lx processing sts=%d\n", ncr_name(np), (u_long) waiting_list, sts);
#endif
while ((wcmd = waiting_list) != 0) {
waiting_list = (Scsi_Cmnd *) wcmd->next_wcmd;
wcmd->next_wcmd = 0;
if (sts == DID_OK) {
#ifdef DEBUG_WAITING_LIST
printk("%s: cmd %lx trying to requeue\n", ncr_name(np), (u_long) wcmd);
#endif
sts = ncr_queue_command(np, wcmd);
}
if (sts != DID_OK) {
#ifdef DEBUG_WAITING_LIST
printk("%s: cmd %lx done forced sts=%d\n", ncr_name(np), (u_long) wcmd, sts);
#endif
SetScsiResult(wcmd, sts, 0);
ncr_queue_done_cmd(np, wcmd);
}
}
}
#undef next_wcmd
#ifdef SCSI_NCR_PROC_INFO_SUPPORT
#ifdef SCSI_NCR_USER_COMMAND_SUPPORT
#define is_digit(c)	((c) >= '0' && (c) <= '9')
#define digit_to_bin(c)	((c) - '0')
#define is_space(c)	((c) == ' ' || (c) == '\t')
static int skip_spaces(char *ptr, int len)
{
int cnt, c;
for (cnt = len; cnt > 0 && (c = *ptr++) && is_space(c); cnt--);
return (len - cnt);
}
static int get_int_arg(char *ptr, int len, u_long *pv)
{
int	cnt, c;
u_long	v;
for (v = 0, cnt = len; cnt > 0 && (c = *ptr++) && is_digit(c); cnt--) {
v = (v * 10) + digit_to_bin(c);
}
if (pv)
*pv = v;
return (len - cnt);
}
static int is_keyword(char *ptr, int len, char *verb)
{
int verb_len = strlen(verb);
if (len >= strlen(verb) && !memcmp(verb, ptr, verb_len))
return verb_len;
else
return 0;
}
#define SKIP_SPACES(min_spaces)						\
if ((arg_len = skip_spaces(ptr, len)) < (min_spaces))		\
return -EINVAL;						\
ptr += arg_len; len -= arg_len;
#define GET_INT_ARG(v)							\
if (!(arg_len = get_int_arg(ptr, len, &(v))))			\
return -EINVAL;						\
ptr += arg_len; len -= arg_len;
static int ncr_user_command(ncb_p np, char *buffer, int length)
{
char *ptr	= buffer;
int len		= length;
struct usrcmd	 *uc = &np->user;
int		arg_len;
u_long 		target;
bzero(uc, sizeof(*uc));
if (len > 0 && ptr[len-1] == '\n')
--len;
if	((arg_len = is_keyword(ptr, len, "setsync")) != 0)
uc->cmd = UC_SETSYNC;
else if	((arg_len = is_keyword(ptr, len, "settags")) != 0)
uc->cmd = UC_SETTAGS;
else if	((arg_len = is_keyword(ptr, len, "setorder")) != 0)
uc->cmd = UC_SETORDER;
else if	((arg_len = is_keyword(ptr, len, "setverbose")) != 0)
uc->cmd = UC_SETVERBOSE;
else if	((arg_len = is_keyword(ptr, len, "setwide")) != 0)
uc->cmd = UC_SETWIDE;
else if	((arg_len = is_keyword(ptr, len, "setdebug")) != 0)
uc->cmd = UC_SETDEBUG;
else if	((arg_len = is_keyword(ptr, len, "setflag")) != 0)
uc->cmd = UC_SETFLAG;
else if	((arg_len = is_keyword(ptr, len, "resetdev")) != 0)
uc->cmd = UC_RESETDEV;
else if	((arg_len = is_keyword(ptr, len, "cleardev")) != 0)
uc->cmd = UC_CLEARDEV;
else
arg_len = 0;
#ifdef DEBUG_PROC_INFO
printk("ncr_user_command: arg_len=%d, cmd=%ld\n", arg_len, uc->cmd);
#endif
if (!arg_len)
return -EINVAL;
ptr += arg_len; len -= arg_len;
switch(uc->cmd) {
case UC_SETSYNC:
case UC_SETTAGS:
case UC_SETWIDE:
case UC_SETFLAG:
case UC_RESETDEV:
case UC_CLEARDEV:
SKIP_SPACES(1);
if ((arg_len = is_keyword(ptr, len, "all")) != 0) {
ptr += arg_len; len -= arg_len;
uc->target = ~0;
} else {
GET_INT_ARG(target);
uc->target = (1<<target);
#ifdef DEBUG_PROC_INFO
printk("ncr_user_command: target=%ld\n", target);
#endif
}
break;
}
switch(uc->cmd) {
case UC_SETVERBOSE:
case UC_SETSYNC:
case UC_SETTAGS:
case UC_SETWIDE:
SKIP_SPACES(1);
GET_INT_ARG(uc->data);
#ifdef DEBUG_PROC_INFO
printk("ncr_user_command: data=%ld\n", uc->data);
#endif
break;
case UC_SETORDER:
SKIP_SPACES(1);
if	((arg_len = is_keyword(ptr, len, "simple")))
uc->data = M_SIMPLE_TAG;
else if	((arg_len = is_keyword(ptr, len, "ordered")))
uc->data = M_ORDERED_TAG;
else if	((arg_len = is_keyword(ptr, len, "default")))
uc->data = 0;
else
return -EINVAL;
break;
case UC_SETDEBUG:
while (len > 0) {
SKIP_SPACES(1);
if	((arg_len = is_keyword(ptr, len, "alloc")))
uc->data |= DEBUG_ALLOC;
else if	((arg_len = is_keyword(ptr, len, "phase")))
uc->data |= DEBUG_PHASE;
else if	((arg_len = is_keyword(ptr, len, "queue")))
uc->data |= DEBUG_QUEUE;
else if	((arg_len = is_keyword(ptr, len, "result")))
uc->data |= DEBUG_RESULT;
else if	((arg_len = is_keyword(ptr, len, "pointer")))
uc->data |= DEBUG_POINTER;
else if	((arg_len = is_keyword(ptr, len, "script")))
uc->data |= DEBUG_SCRIPT;
else if	((arg_len = is_keyword(ptr, len, "tiny")))
uc->data |= DEBUG_TINY;
else if	((arg_len = is_keyword(ptr, len, "timing")))
uc->data |= DEBUG_TIMING;
else if	((arg_len = is_keyword(ptr, len, "nego")))
uc->data |= DEBUG_NEGO;
else if	((arg_len = is_keyword(ptr, len, "tags")))
uc->data |= DEBUG_TAGS;
else
return -EINVAL;
ptr += arg_len; len -= arg_len;
}
#ifdef DEBUG_PROC_INFO
printk("ncr_user_command: data=%ld\n", uc->data);
#endif
break;
case UC_SETFLAG:
while (len > 0) {
SKIP_SPACES(1);
if	((arg_len = is_keyword(ptr, len, "trace")))
uc->data |= UF_TRACE;
else if	((arg_len = is_keyword(ptr, len, "no_disc")))
uc->data |= UF_NODISC;
else
return -EINVAL;
ptr += arg_len; len -= arg_len;
}
break;
default:
break;
}
if (len)
return -EINVAL;
else {
long flags;
NCR_LOCK_NCB(np, flags);
ncr_usercmd (np);
NCR_UNLOCK_NCB(np, flags);
}
return length;
}
#endif
#ifdef SCSI_NCR_USER_INFO_SUPPORT
struct info_str
{
char *buffer;
int length;
int offset;
int pos;
};
static void copy_mem_info(struct info_str *info, char *data, int len)
{
if (info->pos + len > info->length)
len = info->length - info->pos;
if (info->pos + len < info->offset) {
info->pos += len;
return;
}
if (info->pos < info->offset) {
data += (info->offset - info->pos);
len  -= (info->offset - info->pos);
}
if (len > 0) {
memcpy(info->buffer + info->pos, data, len);
info->pos += len;
}
}
static int copy_info(struct info_str *info, char *fmt, ...)
{
va_list args;
char buf[81];
int len;
va_start(args, fmt);
len = vsprintf(buf, fmt, args);
va_end(args);
copy_mem_info(info, buf, len);
return len;
}
static int ncr_host_info(ncb_p np, char *ptr, off_t offset, int len)
{
struct info_str info;
#ifdef CONFIG_ALL_PPC
struct device_node* of_node;
#endif
info.buffer	= ptr;
info.length	= len;
info.offset	= offset;
info.pos	= 0;
copy_info(&info, "General information:\n");
copy_info(&info, "  Chip " NAME53C "%s, device id 0x%x, "
"revision id 0x%x\n",
np->chip_name, np->device_id,	np->revision_id);
copy_info(&info, "  On PCI bus %d, device %d, function %d, "
#ifdef __sparc__
"IRQ %s\n",
#else
"IRQ %d\n",
#endif
np->bus, (np->device_fn & 0xf8) >> 3, np->device_fn & 7,
#ifdef __sparc__
__irq_itoa(np->irq));
#else
(int) np->irq);
#endif
#ifdef CONFIG_ALL_PPC
of_node = find_pci_device_OFnode(np->bus, np->device_fn);
if (of_node && of_node->full_name)
copy_info(&info, "PPC OpenFirmware path : %s\n", of_node->full_name);
#endif
copy_info(&info, "  Synchronous period factor %d, "
"max commands per lun %d\n",
(int) np->minsync, MAX_TAGS);
if (driver_setup.debug || driver_setup.verbose > 1) {
copy_info(&info, "  Debug flags 0x%x, verbosity level %d\n",
driver_setup.debug, driver_setup.verbose);
}
return info.pos > info.offset? info.pos - info.offset : 0;
}
#endif
static int sym53c8xx_proc_info(char *buffer, char **start, off_t offset,
int length, int hostno, int func)
{
struct Scsi_Host *host;
struct host_data *host_data;
ncb_p ncb = 0;
int retv;
#ifdef DEBUG_PROC_INFO
printk("sym53c8xx_proc_info: hostno=%d, func=%d\n", hostno, func);
#endif
for (host = first_host; host; host = host->next) {
if (host->hostt != first_host->hostt)
continue;
if (host->host_no == hostno) {
host_data = (struct host_data *) host->hostdata;
ncb = host_data->ncb;
break;
}
}
if (!ncb)
return -EINVAL;
if (func) {
#ifdef	SCSI_NCR_USER_COMMAND_SUPPORT
retv = ncr_user_command(ncb, buffer, length);
#else
retv = -EINVAL;
#endif
}
else {
if (start)
*start = buffer;
#ifdef SCSI_NCR_USER_INFO_SUPPORT
retv = ncr_host_info(ncb, buffer, offset, length);
#else
retv = -EINVAL;
#endif
}
return retv;
}
#endif
#ifdef SCSI_NCR_NVRAM_SUPPORT
#define SET_BIT 0
#define CLR_BIT 1
#define SET_CLK 2
#define CLR_CLK 3
static void __init
S24C16_set_bit(ncr_slot *np, u_char write_bit, u_char *gpreg, int bit_mode)
{
UDELAY (5);
switch (bit_mode){
case SET_BIT:
*gpreg |= write_bit;
break;
case CLR_BIT:
*gpreg &= 0xfe;
break;
case SET_CLK:
*gpreg |= 0x02;
break;
case CLR_CLK:
*gpreg &= 0xfd;
break;
}
OUTB (nc_gpreg, *gpreg);
UDELAY (5);
}
static void __init S24C16_start(ncr_slot *np, u_char *gpreg)
{
S24C16_set_bit(np, 1, gpreg, SET_BIT);
S24C16_set_bit(np, 0, gpreg, SET_CLK);
S24C16_set_bit(np, 0, gpreg, CLR_BIT);
S24C16_set_bit(np, 0, gpreg, CLR_CLK);
}
static void __init S24C16_stop(ncr_slot *np, u_char *gpreg)
{
S24C16_set_bit(np, 0, gpreg, SET_CLK);
S24C16_set_bit(np, 1, gpreg, SET_BIT);
}
static void __init
S24C16_do_bit(ncr_slot *np, u_char *read_bit, u_char write_bit, u_char *gpreg)
{
S24C16_set_bit(np, write_bit, gpreg, SET_BIT);
S24C16_set_bit(np, 0, gpreg, SET_CLK);
if (read_bit)
*read_bit = INB (nc_gpreg);
S24C16_set_bit(np, 0, gpreg, CLR_CLK);
S24C16_set_bit(np, 0, gpreg, CLR_BIT);
}
static void __init
S24C16_write_ack(ncr_slot *np, u_char write_bit, u_char *gpreg, u_char *gpcntl)
{
OUTB (nc_gpcntl, *gpcntl & 0xfe);
S24C16_do_bit(np, 0, write_bit, gpreg);
OUTB (nc_gpcntl, *gpcntl);
}
static void __init
S24C16_read_ack(ncr_slot *np, u_char *read_bit, u_char *gpreg, u_char *gpcntl)
{
OUTB (nc_gpcntl, *gpcntl | 0x01);
S24C16_do_bit(np, read_bit, 1, gpreg);
OUTB (nc_gpcntl, *gpcntl);
}
static void __init
S24C16_write_byte(ncr_slot *np, u_char *ack_data, u_char write_data,
u_char *gpreg, u_char *gpcntl)
{
int x;
for (x = 0; x < 8; x++)
S24C16_do_bit(np, 0, (write_data >> (7 - x)) & 0x01, gpreg);
S24C16_read_ack(np, ack_data, gpreg, gpcntl);
}
static void __init
S24C16_read_byte(ncr_slot *np, u_char *read_data, u_char ack_data,
u_char *gpreg, u_char *gpcntl)
{
int x;
u_char read_bit;
*read_data = 0;
for (x = 0; x < 8; x++) {
S24C16_do_bit(np, &read_bit, 1, gpreg);
*read_data |= ((read_bit & 0x01) << (7 - x));
}
S24C16_write_ack(np, ack_data, gpreg, gpcntl);
}
static int __init
sym_read_S24C16_nvram (ncr_slot *np, int offset, u_char *data, int len)
{
u_char	gpcntl, gpreg;
u_char	old_gpcntl, old_gpreg;
u_char	ack_data;
int	retv = 1;
int	x;
old_gpreg	= INB (nc_gpreg);
old_gpcntl	= INB (nc_gpcntl);
gpcntl		= old_gpcntl & 0xfc;
OUTB (nc_gpreg,  old_gpreg);
OUTB (nc_gpcntl, gpcntl);
gpreg = old_gpreg;
S24C16_set_bit(np, 0, &gpreg, CLR_CLK);
S24C16_set_bit(np, 0, &gpreg, CLR_BIT);
S24C16_stop(np, &gpreg);
S24C16_start(np, &gpreg);
S24C16_write_byte(np, &ack_data,
0xa0 | ((offset >> 7) & 0x0e), &gpreg, &gpcntl);
if (ack_data & 0x01)
goto out;
S24C16_write_byte(np, &ack_data,
offset & 0xff, &gpreg, &gpcntl);
if (ack_data & 0x01)
goto out;
S24C16_start(np, &gpreg);
S24C16_write_byte(np, &ack_data,
0xa1 | ((offset >> 7) & 0x0e), &gpreg, &gpcntl);
if (ack_data & 0x01)
goto out;
gpcntl |= 0x01;
OUTB (nc_gpcntl, gpcntl);
for (x = 0; x < len; x++)
S24C16_read_byte(np, &data[x], (x == (len-1)), &gpreg, &gpcntl);
gpcntl &= 0xfe;
OUTB (nc_gpcntl, gpcntl);
S24C16_stop(np, &gpreg);
retv = 0;
out:
OUTB (nc_gpcntl, old_gpcntl);
OUTB (nc_gpreg,  old_gpreg);
return retv;
}
#undef SET_BIT
#undef CLR_BIT
#undef SET_CLK
#undef CLR_CLK
static int __init sym_read_Symbios_nvram (ncr_slot *np, Symbios_nvram *nvram)
{
static u_char Symbios_trailer[6] = {0xfe, 0xfe, 0, 0, 0, 0};
u_char *data = (u_char *) nvram;
int len  = sizeof(*nvram);
u_short	csum;
int x;
if (sym_read_S24C16_nvram (np, SYMBIOS_NVRAM_ADDRESS, data, len))
return 1;
if (nvram->type != 0 ||
memcmp(nvram->trailer, Symbios_trailer, 6) ||
nvram->byte_count != len - 12)
return 1;
for (x = 6, csum = 0; x < len - 6; x++)
csum += data[x];
if (csum != nvram->checksum)
return 1;
return 0;
}
static void __init T93C46_Clk(ncr_slot *np, u_char *gpreg)
{
OUTB (nc_gpreg, *gpreg | 0x04);
UDELAY (2);
OUTB (nc_gpreg, *gpreg);
}
static void __init T93C46_Read_Bit(ncr_slot *np, u_char *read_bit, u_char *gpreg)
{
UDELAY (2);
T93C46_Clk(np, gpreg);
*read_bit = INB (nc_gpreg);
}
static void __init T93C46_Write_Bit(ncr_slot *np, u_char write_bit, u_char *gpreg)
{
if (write_bit & 0x01)
*gpreg |= 0x02;
else
*gpreg &= 0xfd;
*gpreg |= 0x10;
OUTB (nc_gpreg, *gpreg);
UDELAY (2);
T93C46_Clk(np, gpreg);
}
static void __init T93C46_Stop(ncr_slot *np, u_char *gpreg)
{
*gpreg &= 0xef;
OUTB (nc_gpreg, *gpreg);
UDELAY (2);
T93C46_Clk(np, gpreg);
}
static void __init
T93C46_Send_Command(ncr_slot *np, u_short write_data,
u_char *read_bit, u_char *gpreg)
{
int x;
for (x = 0; x < 9; x++)
T93C46_Write_Bit(np, (u_char) (write_data >> (8 - x)), gpreg);
*read_bit = INB (nc_gpreg);
}
static void __init
T93C46_Read_Word(ncr_slot *np, u_short *nvram_data, u_char *gpreg)
{
int x;
u_char read_bit;
*nvram_data = 0;
for (x = 0; x < 16; x++) {
T93C46_Read_Bit(np, &read_bit, gpreg);
if (read_bit & 0x01)
*nvram_data |=  (0x01 << (15 - x));
else
*nvram_data &= ~(0x01 << (15 - x));
}
}
static int __init
T93C46_Read_Data(ncr_slot *np, u_short *data,int len,u_char *gpreg)
{
u_char	read_bit;
int	x;
for (x = 0; x < len; x++)  {
T93C46_Send_Command(np, 0x180 | x, &read_bit, gpreg);
if (read_bit & 0x01)
return 1;
T93C46_Read_Word(np, &data[x], gpreg);
T93C46_Stop(np, gpreg);
}
return 0;
}
static int __init
sym_read_T93C46_nvram (ncr_slot *np, Tekram_nvram *nvram)
{
u_char gpcntl, gpreg;
u_char old_gpcntl, old_gpreg;
int retv = 1;
old_gpreg	= INB (nc_gpreg);
old_gpcntl	= INB (nc_gpcntl);
gpreg = old_gpreg & 0xe9;
OUTB (nc_gpreg, gpreg);
gpcntl = (old_gpcntl & 0xe9) | 0x09;
OUTB (nc_gpcntl, gpcntl);
retv = T93C46_Read_Data(np, (u_short *) nvram,
sizeof(*nvram) / sizeof(short), &gpreg);
OUTB (nc_gpcntl, old_gpcntl);
OUTB (nc_gpreg,  old_gpreg);
return retv;
}
static int __init
sym_read_Tekram_nvram (ncr_slot *np, u_short device_id, Tekram_nvram *nvram)
{
u_char *data = (u_char *) nvram;
int len = sizeof(*nvram);
u_short	csum;
int x;
switch (device_id) {
case PCI_DEVICE_ID_NCR_53C885:
case PCI_DEVICE_ID_NCR_53C895:
case PCI_DEVICE_ID_NCR_53C896:
x = sym_read_S24C16_nvram(np, TEKRAM_24C16_NVRAM_ADDRESS,
data, len);
break;
case PCI_DEVICE_ID_NCR_53C875:
x = sym_read_S24C16_nvram(np, TEKRAM_24C16_NVRAM_ADDRESS,
data, len);
if (!x)
break;
default:
x = sym_read_T93C46_nvram(np, nvram);
break;
}
if (x)
return 1;
for (x = 0, csum = 0; x < len - 1; x += 2)
csum += data[x] + (data[x+1] << 8);
if (csum != 0x1234)
return 1;
return 0;
}
#endif
#ifdef MODULE
Scsi_Host_Template driver_template = SYM53C8XX;
#include "scsi_module.c"
#endif