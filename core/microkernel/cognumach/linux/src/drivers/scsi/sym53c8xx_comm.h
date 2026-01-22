#define MIN(a,b)        (((a) < (b)) ? (a) : (b))
#define MAX(a,b)        (((a) > (b)) ? (a) : (b))
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,3,47)
#define SCSI_NCR_DYNAMIC_DMA_MAPPING
#endif
#define u_char		unsigned char
#define u_short		unsigned short
#define u_int		unsigned int
#define u_long		unsigned long
#ifndef bcopy
#define bcopy(s, d, n)	memcpy((d), (s), (n))
#endif
#ifndef bcmp
#define bcmp(s, d, n)	memcmp((d), (s), (n))
#endif
#ifndef bzero
#define bzero(d, n)	memset((d), 0, (n))
#endif
#ifndef offsetof
#define offsetof(t, m)	((size_t) (&((t *)0)->m))
#endif
#define	assert(expression) { \
if (!(expression)) { \
(void)panic( \
"assertion \"%s\" failed: file \"%s\", line %d\n", \
#expression, \
__FILE__, __LINE__); \
} \
}
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
#define DEBUG_SCATTER  (0x0800)
#define DEBUG_IC        (0x1000)
#ifdef SCSI_NCR_DEBUG_INFO_SUPPORT
static int ncr_debug = SCSI_NCR_DEBUG_FLAGS;
#define DEBUG_FLAGS ncr_debug
#else
#define DEBUG_FLAGS	SCSI_NCR_DEBUG_FLAGS
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
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,93)
spinlock_t DRIVER_SMP_LOCK = SPIN_LOCK_UNLOCKED;
#define	NCR_LOCK_DRIVER(flags)     spin_lock_irqsave(&DRIVER_SMP_LOCK, flags)
#define	NCR_UNLOCK_DRIVER(flags)   \
spin_unlock_irqrestore(&DRIVER_SMP_LOCK, flags)
#define NCR_INIT_LOCK_NCB(np)      spin_lock_init(&np->smp_lock)
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
#  define pcivtobus(p)			bus_dvma_to_mem(p)
#  define memcpy_to_pci(a, b, c)	memcpy_toio((a), (b), (c))
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
}
if (vbp)
__m_free(&mp0, vbp, sizeof(*vbp), "VTOB");
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
static struct ncr_driver_setup
driver_setup			= SCSI_NCR_DRIVER_SETUP;
#ifdef	SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT
static struct ncr_driver_setup
driver_safe_setup __initdata	= SCSI_NCR_DRIVER_SAFE_SETUP;
#endif
#define initverbose (driver_setup.verbose)
#define bootverbose (np->verbose)
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
static int ncr_attach (Scsi_Host_Template *tpnt, int unit, ncr_device *device);
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
#undef SET_BIT 0
#undef CLR_BIT 1
#undef SET_CLK 2
#undef CLR_CLK 3
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
#ifdef	SCSI_NCR_DEBUG_NVRAM
static void __init ncr_display_Symbios_nvram(Symbios_nvram *nvram)
{
int i;
printk(KERN_DEBUG NAME53C8XX ": HOST ID=%d%s%s%s%s%s\n",
nvram->host_id & 0x0f,
(nvram->flags  & SYMBIOS_SCAM_ENABLE)	? " SCAM"	:"",
(nvram->flags  & SYMBIOS_PARITY_ENABLE)	? " PARITY"	:"",
(nvram->flags  & SYMBIOS_VERBOSE_MSGS)	? " VERBOSE"	:"",
(nvram->flags  & SYMBIOS_CHS_MAPPING)	? " CHS_ALT"	:"",
(nvram->flags1 & SYMBIOS_SCAN_HI_LO)	? " HI_LO"	:"");
for (i = 0 ; i < 15 ; i++) {
struct Symbios_target *tn = &nvram->target[i];
printk(KERN_DEBUG NAME53C8XX
"-%d:%s%s%s%s WIDTH=%d SYNC=%d TMO=%d\n",
i,
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
static void __init ncr_display_Tekram_nvram(Tekram_nvram *nvram)
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
printk(KERN_DEBUG NAME53C8XX
": HOST ID=%d%s%s%s%s%s%s%s%s%s BOOT DELAY=%d tags=%d\n",
nvram->host_id & 0x0f,
(nvram->flags1 & SYMBIOS_SCAM_ENABLE)	? " SCAM"	:"",
(nvram->flags & TEKRAM_MORE_THAN_2_DRIVES) ? " >2DRIVES":"",
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
printk(KERN_DEBUG NAME53C8XX "-%d:%s%s%s%s%s%s PERIOD=%d\n",
i,
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
static int __init sym53c8xx__setup(char *str)
{
#ifdef SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT
char *cur = str;
char *pc, *pv;
int i, val, c;
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
#define DEF_DEPTH	(driver_setup.default_tags)
#define ALL_TARGETS	-2
#define NO_TARGET	-1
#define ALL_LUNS	-2
#define NO_LUN		-1
static int device_queue_depth(int unit, int target, int lun)
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
if (h == unit &&
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
static int __init
sym53c8xx_pci_init(Scsi_Host_Template *tpnt, pcidev_t pdev, ncr_device *device)
{
u_short vendor_id, device_id, command;
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
if ((device_id == PCI_DEVICE_ID_LSI_53C1010) ||
(device_id == PCI_DEVICE_ID_LSI_53C1010_66)){
printk(NAME53C8XX ": not initializing, device not supported\n");
return -1;
}
chip = 0;
for (i = 0; i < sizeof(ncr_chip_table)/sizeof(ncr_chip_table[0]); i++) {
if (device_id != ncr_chip_table[i].device_id)
continue;
if (revision > ncr_chip_table[i].revision_id)
continue;
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
if (!(command & PCI_COMMAND_MASTER)) {
if (initverbose >= 2)
printk("ncr53c8xx: setting PCI_COMMAND_MASTER bit (fixup)\n");
command |= PCI_COMMAND_MASTER;
pcibios_write_config_word(bus, device_fn, PCI_COMMAND, command);
pcibios_read_config_word(bus, device_fn, PCI_COMMAND, &command);
}
if ((chip->features & FE_WRIE) && !(command & PCI_COMMAND_INVALIDATE)) {
if (initverbose >= 2)
printk("ncr53c8xx: setting PCI_COMMAND_INVALIDATE bit (fixup)\n");
command |= PCI_COMMAND_INVALIDATE;
pcibios_write_config_word(bus, device_fn, PCI_COMMAND, command);
pcibios_read_config_word(bus, device_fn, PCI_COMMAND, &command);
}
if ((chip->features & FE_CLSE) && !cache_line_size) {
cache_line_size = 64 / sizeof(u_int32);
if (initverbose >= 2)
printk("ncr53c8xx: setting PCI_CACHE_LINE_SIZE to %d (fixup)\n",
cache_line_size);
pcibios_write_config_byte(bus, device_fn,
PCI_CACHE_LINE_SIZE, cache_line_size);
pcibios_read_config_byte(bus, device_fn,
PCI_CACHE_LINE_SIZE, &cache_line_size);
}
if (!latency_timer) {
unsigned char min_gnt;
pcibios_read_config_byte(bus, device_fn,
PCI_MIN_GNT, &min_gnt);
if (min_gnt == 0)
latency_timer = 128;
else
latency_timer = ((min_gnt << 3) & 0xff);
printk("ncr53c8xx: setting PCI_LATENCY_TIMER to %d bus clocks (fixup)\n", latency_timer);
pcibios_write_config_byte(bus, device_fn,
PCI_LATENCY_TIMER, latency_timer);
pcibios_read_config_byte(bus, device_fn,
PCI_LATENCY_TIMER, &latency_timer);
}
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
#if 0
if ((command & (PCI_COMMAND_MASTER | PCI_COMMAND_PARITY))
!= (PCI_COMMAND_MASTER | PCI_COMMAND_PARITY)) {
printk(NAME53C8XX ": setting%s%s...(fix-up)\n",
(command & PCI_COMMAND_MASTER) ? "" : " PCI_COMMAND_MASTER",
(command & PCI_COMMAND_PARITY) ? "" : " PCI_COMMAND_PARITY");
command |= (PCI_COMMAND_MASTER | PCI_COMMAND_PARITY);
pci_write_config_word(pdev, PCI_COMMAND, command);
}
#else
if ((command & PCI_COMMAND_MASTER) != PCI_COMMAND_MASTER) {
printk(NAME53C8XX ": setting PCI_COMMAND_MASTER...(fix-up)\n");
command |= PCI_COMMAND_MASTER;
pci_write_config_word(pdev, PCI_COMMAND, command);
}
#endif
if (!(driver_setup.special_features & 1))
chip->features &= ~FE_SPECIAL_SET;
else {
if (driver_setup.special_features & 2)
chip->features &= ~FE_WRIE;
if (driver_setup.special_features & 4)
chip->features &= ~FE_NOPM;
}
if (driver_setup.ultra_scsi < 2 && (chip->features & FE_ULTRA2)) {
chip->features |=  FE_ULTRA;
chip->features &= ~FE_ULTRA2;
}
if (driver_setup.ultra_scsi < 1)
chip->features &= ~FE_ULTRA;
if (!driver_setup.max_wide)
chip->features &= ~FE_WIDE;
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
u_char lt = (1 << chip->burst_max) + 6 + 10;
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
static int __init
sym53c8xx__detect(Scsi_Host_Template *tpnt, u_short ncr_chip_ids[], int chips)
{
pcidev_t pcidev;
int i, j, hosts, count;
int attach_count = 0;
ncr_device *devtbl, *devp;
#ifdef SCSI_NCR_NVRAM_SUPPORT
ncr_nvram  nvram0, nvram, *nvp;
#endif
if (!pci_present())
return 0;
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
#if 0
else if (!(driver_setup.use_nvram & 0x80))
printk(KERN_INFO NAME53C8XX
": 53c%s state OFF thus not attached\n",
devp->chip.name);
#endif
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