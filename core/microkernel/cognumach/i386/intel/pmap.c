#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifdef __x86_64__
#ifndef PAE
#define PAE 1
#endif
#endif
#include <string.h>
#include <mach/machine/vm_types.h>
#include <mach/boolean.h>
#include <kern/debug.h>
#include <kern/printf.h>
#include <kern/thread.h>
#include <kern/slab.h>
#include <kern/lock.h>
#include <vm/pmap.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <i386/vm_param.h>
#include <mach/vm_prot.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/vm_user.h>
#include <mach/machine/vm_param.h>
#include <mach/xen.h>
#include <machine/thread.h>
#include <i386/cpu_number.h>
#include <i386/proc_reg.h>
#include <i386/locore.h>
#include <i386/model_dep.h>
#include <i386/spl.h>
#include <i386at/biosmem.h>
#include <i386at/model_dep.h>
#if NCPUS > 1
#include <i386/mp_desc.h>
#endif
#include <ddb/db_output.h>
#include <machine/db_machdep.h>
#ifdef MACH_PSEUDO_PHYS
#define WRITE_PTE(pte_p, pte_entry) *(pte_p) = pte_entry?pa_to_ma(pte_entry):0;
#else
#define WRITE_PTE(pte_p, pte_entry) *(pte_p) = (pte_entry);
#endif
typedef struct pv_entry {
struct pv_entry *next;
pmap_t pmap;
vm_offset_t va;
} *pv_entry_t;
#define PV_ENTRY_NULL ((pv_entry_t) 0)
pv_entry_t pv_head_table;
pv_entry_t pv_free_list;
def_simple_lock_data(static, pv_free_list_lock)
#define PV_ALLOC(pv_e) \
MACRO_BEGIN \
simple_lock(&pv_free_list_lock); \
if ((pv_e = pv_free_list) != 0) { \
pv_free_list = pv_e->next; \
} \
simple_unlock(&pv_free_list_lock); \
MACRO_END
#define PV_FREE(pv_e) \
MACRO_BEGIN \
simple_lock(&pv_free_list_lock); \
pv_e->next = pv_free_list; \
pv_free_list = pv_e; \
simple_unlock(&pv_free_list_lock); \
MACRO_END
struct kmem_cache pv_list_cache;
char *pv_lock_table;
#define pv_lock_table_size(n) (((n)+BYTE_SIZE-1)/BYTE_SIZE)
boolean_t pmap_initialized = FALSE;
vm_offset_t kernel_virtual_start;
vm_offset_t kernel_virtual_end;
#define pa_index(pa) vm_page_table_index(pa)
#define pai_to_pvh(pai) (&pv_head_table[pai])
#define lock_pvh_pai(pai) (bit_lock(pai, pv_lock_table))
#define unlock_pvh_pai(pai) (bit_unlock(pai, pv_lock_table))
char *pmap_phys_attributes;
#define PHYS_MODIFIED INTEL_PTE_MOD
#define PHYS_REFERENCED INTEL_PTE_REF
#define PDE_MAPPED_SIZE (pdenum2lin(1))
vm_object_t pmap_object = VM_OBJECT_NULL;
#if NCPUS > 1
#define SPLVM(spl) \
MACRO_BEGIN \
spl = splvm(); \
i_bit_clear(cpu_number(), &cpus_active); \
MACRO_END
#define SPLX(spl) \
MACRO_BEGIN \
i_bit_set(cpu_number(), &cpus_active); \
splx(spl); \
MACRO_END
lock_data_t pmap_system_lock;
#define PMAP_READ_LOCK(pmap, spl) \
MACRO_BEGIN \
SPLVM(spl); \
lock_read(&pmap_system_lock); \
simple_lock(&(pmap)->lock); \
MACRO_END
#define PMAP_WRITE_LOCK(spl) \
MACRO_BEGIN \
SPLVM(spl); \
lock_write(&pmap_system_lock); \
MACRO_END
#define PMAP_READ_UNLOCK(pmap, spl) \
MACRO_BEGIN \
simple_unlock(&(pmap)->lock); \
lock_read_done(&pmap_system_lock); \
SPLX(spl); \
MACRO_END
#define PMAP_WRITE_UNLOCK(spl) \
MACRO_BEGIN \
lock_write_done(&pmap_system_lock); \
SPLX(spl); \
MACRO_END
#define PMAP_WRITE_TO_READ_LOCK(pmap) \
MACRO_BEGIN \
simple_lock(&(pmap)->lock); \
lock_write_to_read(&pmap_system_lock); \
MACRO_END
#define LOCK_PVH(index) (lock_pvh_pai(index))
#define UNLOCK_PVH(index) (unlock_pvh_pai(index))
#define PMAP_UPDATE_TLBS(pmap, s, e) \
MACRO_BEGIN \
cpu_set cpu_mask = 1 << cpu_number(); \
cpu_set users; \
\
\
\
\
\
users = (pmap)->cpus_using & ~cpu_mask; \
if (users) { \
\
\
signal_cpus(users, (pmap), (s), (e)); \
while ((pmap)->cpus_using & cpus_active & ~cpu_mask) \
cpu_pause(); \
} \
\
\
if ((pmap)->cpus_using & cpu_mask) { \
INVALIDATE_TLB((pmap), (s), (e)); \
} \
MACRO_END
#else
#define SPLVM(spl) ((void)(spl))
#define SPLX(spl) ((void)(spl))
#define PMAP_READ_LOCK(pmap, spl) SPLVM(spl)
#define PMAP_WRITE_LOCK(spl) SPLVM(spl)
#define PMAP_READ_UNLOCK(pmap, spl) SPLX(spl)
#define PMAP_WRITE_UNLOCK(spl) SPLX(spl)
#define PMAP_WRITE_TO_READ_LOCK(pmap)
#define LOCK_PVH(index)
#define UNLOCK_PVH(index)
#define PMAP_UPDATE_TLBS(pmap, s, e) \
MACRO_BEGIN \
\
if ((pmap)->cpus_using) { \
INVALIDATE_TLB((pmap), (s), (e)); \
} \
MACRO_END
#endif
#ifdef MACH_PV_PAGETABLES
#define INVALIDATE_TLB(pmap, s, e) \
MACRO_BEGIN \
if (__builtin_constant_p((e) - (s)) \
&& (e) - (s) == PAGE_SIZE) \
hyp_invlpg((pmap) == kernel_pmap ? kvtolin(s) : (s)); \
else \
hyp_mmuext_op_void(MMUEXT_TLB_FLUSH_LOCAL); \
MACRO_END
#else
#define INVALIDATE_TLB(pmap, s, e) \
MACRO_BEGIN \
if (__builtin_constant_p((e) - (s)) \
&& (e) - (s) == PAGE_SIZE) \
invlpg_linear((pmap) == kernel_pmap ? kvtolin(s) : (s)); \
else \
flush_tlb(); \
MACRO_END
#endif
#if NCPUS > 1
#define UPDATE_LIST_SIZE 4
struct pmap_update_item {
pmap_t pmap;
vm_offset_t start;
vm_offset_t end;
} ;
typedef struct pmap_update_item *pmap_update_item_t;
struct pmap_update_list {
decl_simple_lock_data(, lock)
int count;
struct pmap_update_item item[UPDATE_LIST_SIZE];
} ;
typedef struct pmap_update_list *pmap_update_list_t;
struct pmap_update_list cpu_update_list[NCPUS];
cpu_set cpus_active;
cpu_set cpus_idle;
volatile
boolean_t cpu_update_needed[NCPUS];
#endif
#define current_pmap() (vm_map_pmap(current_thread()->task->map))
#define pmap_in_use(pmap, cpu) (((pmap)->cpus_using & (1 << (cpu))) != 0)
struct pmap kernel_pmap_store;
pmap_t kernel_pmap;
struct kmem_cache pmap_cache;
struct kmem_cache pt_cache;
struct kmem_cache pd_cache;
#if PAE
struct kmem_cache pdpt_cache;
#ifdef __x86_64__
struct kmem_cache l4_cache;
#endif
#endif
boolean_t pmap_debug = FALSE;
#if 0
int ptes_per_vm_page;
#else
#define ptes_per_vm_page 1
#endif
unsigned int inuse_ptepages_count = 0;
pt_entry_t *kernel_page_dir;
static pmap_mapwindow_t mapwindows[PMAP_NMAPWINDOWS * NCPUS];
#define MAPWINDOW_SIZE (PMAP_NMAPWINDOWS * NCPUS * PAGE_SIZE)
#ifdef __x86_64__
static inline pt_entry_t *
pmap_l4base(const pmap_t pmap, vm_offset_t lin_addr)
{
return &pmap->l4base[lin2l4num(lin_addr)];
}
#endif
#ifdef PAE
static inline pt_entry_t *
pmap_ptp(const pmap_t pmap, vm_offset_t lin_addr)
{
pt_entry_t *pdp_table;
#ifdef __x86_64__
pt_entry_t *l4_table;
l4_table = pmap_l4base(pmap, lin_addr);
if (l4_table == PT_ENTRY_NULL)
return(PT_ENTRY_NULL);
pt_entry_t pdp = *l4_table;
if ((pdp & INTEL_PTE_VALID) == 0)
return PT_ENTRY_NULL;
pdp_table = (pt_entry_t *) ptetokv(pdp);
#else
pdp_table = pmap->pdpbase;
#endif
return &pdp_table[lin2pdpnum(lin_addr)];
}
#endif
static inline pt_entry_t *
pmap_pde(const pmap_t pmap, vm_offset_t addr)
{
pt_entry_t *page_dir;
if (pmap == kernel_pmap)
addr = kvtolin(addr);
#if PAE
pt_entry_t *pdp_table;
pdp_table = pmap_ptp(pmap, addr);
if (pdp_table == PT_ENTRY_NULL)
return(PT_ENTRY_NULL);
pt_entry_t pde = *pdp_table;
if ((pde & INTEL_PTE_VALID) == 0)
return PT_ENTRY_NULL;
page_dir = (pt_entry_t *) ptetokv(pde);
#else
#ifdef __x86_64__
#error "Invalid configuration: x86_64 requires PAE"
#else
page_dir = pmap->dirbase;
#endif
#endif
return &page_dir[lin2pdenum(addr)];
}
pt_entry_t *
pmap_pte(const pmap_t pmap, vm_offset_t addr)
{
pt_entry_t *ptp;
pt_entry_t pte;
#ifdef __x86_64__
if (pmap->l4base == 0)
return(PT_ENTRY_NULL);
#elif PAE
if (pmap->pdpbase == 0)
return(PT_ENTRY_NULL);
#else
if (pmap->dirbase == 0)
return(PT_ENTRY_NULL);
#endif
ptp = pmap_pde(pmap, addr);
if (ptp == 0)
return(PT_ENTRY_NULL);
pte = *ptp;
if ((pte & INTEL_PTE_VALID) == 0)
return(PT_ENTRY_NULL);
ptp = (pt_entry_t *)ptetokv(pte);
return(&ptp[ptenum(addr)]);
}
#define DEBUG_PTE_PAGE 0
#if DEBUG_PTE_PAGE
void ptep_check(ptep_t ptep)
{
pt_entry_t *pte, *epte;
int ctu, ctw;
if (ptep == PTE_PAGE_NULL)
return;
pte = pmap_pte(ptep->pmap, ptep->va);
epte = pte + INTEL_PGBYTES/sizeof(pt_entry_t);
ctu = 0;
ctw = 0;
while (pte < epte) {
if (pte->pfn != 0) {
ctu++;
if (pte->wired)
ctw++;
}
pte += ptes_per_vm_page;
}
if (ctu != ptep->use_count || ctw != ptep->wired_count) {
printf("use %d wired %d - actual use %d wired %d\n",
ptep->use_count, ptep->wired_count, ctu, ctw);
panic("pte count");
}
}
#endif
vm_offset_t pmap_map_bd(
vm_offset_t virt,
phys_addr_t start,
phys_addr_t end,
vm_prot_t prot)
{
pt_entry_t template;
pt_entry_t *pte;
int spl;
#ifdef MACH_PV_PAGETABLES
int n, i = 0;
struct mmu_update update[HYP_BATCH_MMU_UPDATES];
#endif
template = pa_to_pte(start)
| INTEL_PTE_NCACHE|INTEL_PTE_WTHRU
| INTEL_PTE_VALID;
if (CPU_HAS_FEATURE(CPU_FEATURE_PGE))
template |= INTEL_PTE_GLOBAL;
if (prot & VM_PROT_WRITE)
template |= INTEL_PTE_WRITE;
PMAP_READ_LOCK(kernel_pmap, spl);
while (start < end) {
pte = pmap_pte(kernel_pmap, virt);
if (pte == PT_ENTRY_NULL)
panic("pmap_map_bd: Invalid kernel address\n");
#ifdef MACH_PV_PAGETABLES
update[i].ptr = kv_to_ma(pte);
update[i].val = pa_to_ma(template);
i++;
if (i == HYP_BATCH_MMU_UPDATES) {
hyp_mmu_update(kvtolin(&update), i, kvtolin(&n), DOMID_SELF);
if (n != i)
panic("couldn't pmap_map_bd\n");
i = 0;
}
#else
WRITE_PTE(pte, template)
#endif
pte_increment_pa(template);
virt += PAGE_SIZE;
start += PAGE_SIZE;
}
#ifdef MACH_PV_PAGETABLES
if (i > HYP_BATCH_MMU_UPDATES)
panic("overflowed array in pmap_map_bd");
hyp_mmu_update(kvtolin(&update), i, kvtolin(&n), DOMID_SELF);
if (n != i)
panic("couldn't pmap_map_bd\n");
#endif
PMAP_READ_UNLOCK(kernel_pmap, spl);
return(virt);
}
#ifdef PAE
static void pmap_bootstrap_pae(void)
{
vm_offset_t addr;
pt_entry_t *pdp_kernel;
#ifdef __x86_64__
#ifdef MACH_HYP
kernel_pmap->user_l4base = NULL;
kernel_pmap->user_pdpbase = NULL;
#endif
kernel_pmap->l4base = (pt_entry_t*)phystokv(pmap_grab_page());
memset(kernel_pmap->l4base, 0, INTEL_PGBYTES);
#else
const int PDPNUM_KERNEL = PDPNUM;
#endif
init_alloc_aligned(PDPNUM_KERNEL * INTEL_PGBYTES, &addr);
kernel_page_dir = (pt_entry_t*)phystokv(addr);
memset(kernel_page_dir, 0, PDPNUM_KERNEL * INTEL_PGBYTES);
pdp_kernel = (pt_entry_t*)phystokv(pmap_grab_page());
memset(pdp_kernel, 0, INTEL_PGBYTES);
for (int i = 0; i < PDPNUM_KERNEL; i++) {
int pdp_index = i;
#ifdef __x86_64__
pdp_index += lin2pdpnum(VM_MIN_KERNEL_ADDRESS);
#endif
WRITE_PTE(&pdp_kernel[pdp_index],
pa_to_pte(_kvtophys((void *) kernel_page_dir
+ i * INTEL_PGBYTES))
| INTEL_PTE_VALID
#if (defined(__x86_64__) && !defined(MACH_HYP)) || defined(MACH_PV_PAGETABLES)
| INTEL_PTE_WRITE
#endif
);
}
#ifdef __x86_64__
WRITE_PTE(&kernel_pmap->l4base[lin2l4num(VM_MIN_KERNEL_ADDRESS)],
pa_to_pte(_kvtophys(pdp_kernel)) | INTEL_PTE_VALID | INTEL_PTE_WRITE);
#ifdef MACH_PV_PAGETABLES
pmap_set_page_readonly_init(kernel_pmap->l4base);
#endif
#else
kernel_pmap->pdpbase = pdp_kernel;
#endif
}
#endif
#ifdef MACH_PV_PAGETABLES
#ifdef PAE
#define NSUP_L1 4
#else
#define NSUP_L1 1
#endif
static void pmap_bootstrap_xen(pt_entry_t *l1_map[NSUP_L1])
{
hyp_vm_assist(VMASST_CMD_enable, VMASST_TYPE_pae_extended_cr3);
vm_offset_t la;
int n_l1map;
for (n_l1map = 0, la = VM_MIN_KERNEL_ADDRESS; la >= VM_MIN_KERNEL_ADDRESS; la += NPTES * PAGE_SIZE) {
pt_entry_t *base = (pt_entry_t*) boot_info.pt_base;
#ifdef PAE
#ifdef __x86_64__
base = (pt_entry_t*) ptetokv(base[0]);
#endif
pt_entry_t *l2_map = (pt_entry_t*) ptetokv(base[lin2pdpnum(la)]);
#else
pt_entry_t *l2_map = base;
#endif
l2_map += (la >> PDESHIFT) & PDEMASK;
if (!(*l2_map & INTEL_PTE_VALID)) {
struct mmu_update update;
unsigned j, n;
l1_map[n_l1map] = (pt_entry_t*) phystokv(pmap_grab_page());
for (j = 0; j < NPTES; j++)
l1_map[n_l1map][j] = (((pt_entry_t)pfn_to_mfn(lin2pdenum(la - VM_MIN_KERNEL_ADDRESS) * NPTES + j)) << PAGE_SHIFT) | INTEL_PTE_VALID | INTEL_PTE_WRITE;
pmap_set_page_readonly_init(l1_map[n_l1map]);
if (!hyp_mmuext_op_mfn (MMUEXT_PIN_L1_TABLE, kv_to_mfn (l1_map[n_l1map])))
panic("couldn't pin page %p(%lx)", l1_map[n_l1map],
(long unsigned int) kv_to_ma (l1_map[n_l1map]));
update.ptr = kv_to_ma(l2_map);
update.val = kv_to_ma(l1_map[n_l1map]) | INTEL_PTE_VALID | INTEL_PTE_WRITE;
hyp_mmu_update(kv_to_la(&update), 1, kv_to_la(&n), DOMID_SELF);
if (n != 1)
panic("couldn't complete bootstrap map");
if (++n_l1map >= NSUP_L1)
break;
}
}
}
#endif
void pmap_bootstrap(void)
{
#if 0
ptes_per_vm_page = PAGE_SIZE / INTEL_PGBYTES;
#endif
kernel_pmap = &kernel_pmap_store;
#if NCPUS > 1
lock_init(&pmap_system_lock, FALSE);
#endif
simple_lock_init(&kernel_pmap->lock);
kernel_pmap->ref_count = 1;
kernel_virtual_start = phystokv(biosmem_directmap_end());
kernel_virtual_end = kernel_virtual_start + VM_KERNEL_MAP_SIZE;
if (kernel_virtual_end < kernel_virtual_start
|| kernel_virtual_end > VM_MAX_KERNEL_ADDRESS - PAGE_SIZE)
kernel_virtual_end = VM_MAX_KERNEL_ADDRESS - PAGE_SIZE;
#if PAE
pmap_bootstrap_pae();
#else
#ifdef __x86_64__
#error "Invalid configuration: x86_64 requires PAE"
#else
kernel_pmap->dirbase = kernel_page_dir = (pt_entry_t*)phystokv(pmap_grab_page());
{
unsigned i;
for (i = 0; i < NPDES; i++)
kernel_page_dir[i] = 0;
}
#endif
#endif
#ifdef MACH_PV_PAGETABLES
pt_entry_t *l1_map[NSUP_L1];
pmap_bootstrap_xen(l1_map);
#endif
{
vm_offset_t va;
pt_entry_t global = CPU_HAS_FEATURE(CPU_FEATURE_PGE) ? INTEL_PTE_GLOBAL : 0;
for (va = phystokv(0); va >= phystokv(0) && va < kernel_virtual_end; )
{
pt_entry_t *pde = kernel_page_dir + lin2pdenum_cont(kvtolin(va));
pt_entry_t *ptable = (pt_entry_t*)phystokv(pmap_grab_page());
pt_entry_t *pte;
WRITE_PTE(pde, pa_to_pte((vm_offset_t)_kvtophys(ptable))
| INTEL_PTE_VALID | INTEL_PTE_WRITE);
for (pte = ptable; (va < phystokv(biosmem_directmap_end())) && (pte < ptable+NPTES); pte++)
{
if ((pte - ptable) < ptenum(va))
{
WRITE_PTE(pte, 0);
}
else
#ifdef MACH_PV_PAGETABLES
if (va == (vm_offset_t) &hyp_shared_info)
{
*pte = boot_info.shared_info | INTEL_PTE_VALID | INTEL_PTE_WRITE;
va += INTEL_PGBYTES;
}
else
#endif
{
extern char _start[], etext[];
if (((va >= (vm_offset_t) _start)
&& (va + INTEL_PGBYTES <= (vm_offset_t)etext))
#ifdef MACH_PV_PAGETABLES
|| (va >= (vm_offset_t) boot_info.pt_base
&& (va + INTEL_PGBYTES <=
(vm_offset_t) ptable + INTEL_PGBYTES))
#endif
)
{
WRITE_PTE(pte, pa_to_pte(_kvtophys(va))
| INTEL_PTE_VALID | global);
}
else
{
#ifdef MACH_PV_PAGETABLES
int i;
for (i = 0; i < NSUP_L1; i++)
if (va == (vm_offset_t) l1_map[i]) {
WRITE_PTE(pte, pa_to_pte(_kvtophys(va))
| INTEL_PTE_VALID | global);
break;
}
if (i == NSUP_L1)
#endif
WRITE_PTE(pte, pa_to_pte(_kvtophys(va))
| INTEL_PTE_VALID | INTEL_PTE_WRITE | global)
}
va += INTEL_PGBYTES;
}
}
for (; pte < ptable+NPTES; pte++)
{
if (va >= kernel_virtual_end - MAPWINDOW_SIZE && va < kernel_virtual_end)
{
pmap_mapwindow_t *win = &mapwindows[atop(va - (kernel_virtual_end - MAPWINDOW_SIZE))];
win->entry = pte;
win->vaddr = va;
}
WRITE_PTE(pte, 0);
va += INTEL_PGBYTES;
}
#ifdef MACH_PV_PAGETABLES
pmap_set_page_readonly_init(ptable);
if (!hyp_mmuext_op_mfn (MMUEXT_PIN_L1_TABLE, kv_to_mfn (ptable)))
panic("couldn't pin page %p(%lx)\n", ptable, (unsigned long) kv_to_ma (ptable));
#endif
}
}
}
#ifdef MACH_PV_PAGETABLES
void pmap_set_page_readwrite(void *_vaddr) {
vm_offset_t vaddr = (vm_offset_t) _vaddr;
phys_addr_t paddr = kvtophys(vaddr);
vm_offset_t canon_vaddr = phystokv(paddr);
if (hyp_do_update_va_mapping (kvtolin(vaddr), pa_to_pte (pa_to_ma(paddr)) | INTEL_PTE_VALID | INTEL_PTE_WRITE, UVMF_NONE))
panic("couldn't set hiMMU readwrite for addr %lx(%lx)\n", (unsigned long) vaddr,
(unsigned long) pa_to_ma (paddr));
if (canon_vaddr != vaddr)
if (hyp_do_update_va_mapping (kvtolin(canon_vaddr), pa_to_pte (pa_to_ma(paddr)) | INTEL_PTE_VALID | INTEL_PTE_WRITE, UVMF_NONE))
panic("couldn't set hiMMU readwrite for paddr %lx(%lx)\n",
(unsigned long) canon_vaddr, (unsigned long) pa_to_ma (paddr));
}
void pmap_set_page_readonly(void *_vaddr) {
vm_offset_t vaddr = (vm_offset_t) _vaddr;
phys_addr_t paddr = kvtophys(vaddr);
vm_offset_t canon_vaddr = phystokv(paddr);
if (*pmap_pde(kernel_pmap, vaddr) & INTEL_PTE_VALID) {
if (hyp_do_update_va_mapping (kvtolin(vaddr), pa_to_pte (pa_to_ma(paddr)) | INTEL_PTE_VALID, UVMF_NONE))
panic("couldn't set hiMMU readonly for vaddr %lx(%lx)\n",
(unsigned long) vaddr, (unsigned long) pa_to_ma (paddr));
}
if (canon_vaddr != vaddr &&
*pmap_pde(kernel_pmap, canon_vaddr) & INTEL_PTE_VALID) {
if (hyp_do_update_va_mapping (kvtolin(canon_vaddr), pa_to_pte (pa_to_ma(paddr)) | INTEL_PTE_VALID, UVMF_NONE))
panic("couldn't set hiMMU readonly for vaddr %lx canon_vaddr %lx paddr %lx (%lx)\n",
(unsigned long) vaddr, (unsigned long) canon_vaddr,
(unsigned long) paddr, (unsigned long) pa_to_ma (paddr));
}
}
void pmap_set_page_readonly_init(void *_vaddr) {
vm_offset_t vaddr = (vm_offset_t) _vaddr;
#if PAE
pt_entry_t *pdpbase = (void*) boot_info.pt_base;
#ifdef __x86_64__
pdpbase = (pt_entry_t *) ptetokv(pdpbase[lin2l4num(vaddr)]);
#endif
pt_entry_t *dirbase = (void*) ptetokv(pdpbase[lin2pdpnum(vaddr)]);
#else
pt_entry_t *dirbase = (void*) boot_info.pt_base;
#endif
pt_entry_t *pte = &dirbase[lin2pdenum(vaddr) & PTEMASK];
if (*pmap_pde(kernel_pmap, vaddr) & INTEL_PTE_VALID) {
if (!hyp_mmu_update_la (kvtolin(vaddr), pa_to_pte (kv_to_ma(vaddr)) | INTEL_PTE_VALID))
panic("couldn't set hiMMU readonly for vaddr %lx(%lx)\n",
(unsigned long) vaddr, (unsigned long) kv_to_ma (vaddr));
}
if (*pte & INTEL_PTE_VALID) {
if (hyp_do_update_va_mapping (vaddr, pa_to_pte (kv_to_ma(vaddr)) | INTEL_PTE_VALID, UVMF_NONE))
panic("couldn't set MMU readonly for vaddr %lx(%lx)\n",
(unsigned long) vaddr, (unsigned long) kv_to_ma (vaddr));
}
}
void pmap_clear_bootstrap_pagetable(pt_entry_t *base) {
unsigned i;
pt_entry_t *dir;
vm_offset_t va = 0;
#ifdef __x86_64__
int l4i, l3i;
#else
#if PAE
unsigned j;
#endif
#endif
if (!hyp_mmuext_op_mfn (MMUEXT_UNPIN_TABLE, kv_to_mfn(base)))
panic("pmap_clear_bootstrap_pagetable: couldn't unpin page %p(%lx)\n", base,
(unsigned long) kv_to_ma(base));
#ifdef __x86_64__
for (l4i = 0; l4i < NPTES && va < HYP_VIRT_START && va < 0x0000800000000000UL; l4i++) {
pt_entry_t l4e = base[l4i];
pt_entry_t *l3;
if (!(l4e & INTEL_PTE_VALID)) {
va += NPTES * NPTES * NPTES * INTEL_PGBYTES;
continue;
}
l3 = (pt_entry_t *) ptetokv(l4e);
for (l3i = 0; l3i < NPTES && va < HYP_VIRT_START; l3i++) {
pt_entry_t l3e = l3[l3i];
if (!(l3e & INTEL_PTE_VALID)) {
va += NPTES * NPTES * INTEL_PGBYTES;
continue;
}
dir = (pt_entry_t *) ptetokv(l3e);
#else
#if PAE
for (j = 0; j < PDPNUM && va < HYP_VIRT_START; j++)
{
pt_entry_t pdpe = base[j];
if (!(pdpe & INTEL_PTE_VALID)) {
va += NPTES * NPTES * INTEL_PGBYTES;
continue;
}
dir = (pt_entry_t *) ptetokv(pdpe);
#else
dir = base;
#endif
#endif
for (i = 0; i < NPTES && va < HYP_VIRT_START; i++) {
pt_entry_t pde = dir[i];
unsigned long pfn = atop(pte_to_pa(pde));
void *pgt = (void*) phystokv(ptoa(pfn));
if (pde & INTEL_PTE_VALID)
hyp_free_page(pfn, pgt);
va += NPTES * INTEL_PGBYTES;
}
#ifndef __x86_64__
#if PAE
hyp_free_page(atop(_kvtophys(dir)), dir);
}
#endif
#else
hyp_free_page(atop(_kvtophys(dir)), dir);
}
hyp_free_page(atop(_kvtophys(l3)), l3);
}
#endif
hyp_free_page(atop(_kvtophys(base)), base);
}
#endif
pmap_mapwindow_t *pmap_get_mapwindow(pt_entry_t entry)
{
pmap_mapwindow_t *map;
int cpu = cpu_number();
assert(entry != 0);
for (map = &mapwindows[cpu * PMAP_NMAPWINDOWS]; map < &mapwindows[(cpu+1) * PMAP_NMAPWINDOWS]; map++)
if (!(*map->entry))
break;
assert(map < &mapwindows[(cpu+1) * PMAP_NMAPWINDOWS]);
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmu_update_pte(kv_to_ma(map->entry), pa_to_ma(entry)))
panic("pmap_get_mapwindow");
#else
WRITE_PTE(map->entry, entry);
#endif
INVALIDATE_TLB(kernel_pmap, map->vaddr, map->vaddr + PAGE_SIZE);
return map;
}
void pmap_put_mapwindow(pmap_mapwindow_t *map)
{
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmu_update_pte(kv_to_ma(map->entry), 0))
panic("pmap_put_mapwindow");
#else
WRITE_PTE(map->entry, 0);
#endif
INVALIDATE_TLB(kernel_pmap, map->vaddr, map->vaddr + PAGE_SIZE);
}
void pmap_virtual_space(
vm_offset_t *startp,
vm_offset_t *endp)
{
*startp = kernel_virtual_start;
*endp = kernel_virtual_end - MAPWINDOW_SIZE;
}
void pmap_init(void)
{
unsigned long npages;
vm_offset_t addr;
vm_size_t s;
#if NCPUS > 1
int i;
#endif
npages = vm_page_table_size();
s = (vm_size_t) (sizeof(struct pv_entry) * npages
+ pv_lock_table_size(npages)
+ npages);
s = round_page(s);
if (kmem_alloc_wired(kernel_map, &addr, s) != KERN_SUCCESS)
panic("pmap_init");
memset((void *) addr, 0, s);
pv_head_table = (pv_entry_t) addr;
addr = (vm_offset_t) (pv_head_table + npages);
pv_lock_table = (char *) addr;
addr = (vm_offset_t) (pv_lock_table + pv_lock_table_size(npages));
pmap_phys_attributes = (char *) addr;
s = (vm_size_t) sizeof(struct pmap);
kmem_cache_init(&pmap_cache, "pmap", s, 0, NULL, 0);
kmem_cache_init(&pt_cache, "pmap_L1",
INTEL_PGBYTES, INTEL_PGBYTES, NULL,
KMEM_CACHE_PHYSMEM);
kmem_cache_init(&pd_cache, "pmap_L2",
INTEL_PGBYTES, INTEL_PGBYTES, NULL,
KMEM_CACHE_PHYSMEM);
#if PAE
kmem_cache_init(&pdpt_cache, "pmap_L3",
INTEL_PGBYTES, INTEL_PGBYTES, NULL,
KMEM_CACHE_PHYSMEM);
#ifdef __x86_64__
kmem_cache_init(&l4_cache, "pmap_L4",
INTEL_PGBYTES, INTEL_PGBYTES, NULL,
KMEM_CACHE_PHYSMEM);
#endif
#endif
s = (vm_size_t) sizeof(struct pv_entry);
kmem_cache_init(&pv_list_cache, "pv_entry", s, 0, NULL, 0);
#if NCPUS > 1
for (i = 0; i < NCPUS; i++) {
pmap_update_list_t up = &cpu_update_list[i];
simple_lock_init(&up->lock);
up->count = 0;
}
#endif
pmap_initialized = TRUE;
}
static inline boolean_t
valid_page(phys_addr_t addr)
{
struct vm_page *p;
if (!pmap_initialized)
return FALSE;
p = vm_page_lookup_pa(addr);
return (p != NULL);
}
#ifdef MACH_XEN
static vm_offset_t
pmap_page_table_page_alloc(void)
{
vm_page_t m;
phys_addr_t pa;
check_simple_locks();
if (pmap_object == VM_OBJECT_NULL)
pmap_object = vm_object_allocate(vm_page_table_size() * PAGE_SIZE);
while ((m = vm_page_grab(VM_PAGE_DIRECTMAP)) == VM_PAGE_NULL)
VM_PAGE_WAIT((void (*)()) 0);
pa = m->phys_addr;
assert(pa == (vm_offset_t) pa);
vm_object_lock(pmap_object);
vm_page_insert(m, pmap_object, pa);
vm_page_lock_queues();
vm_page_wire(m);
inuse_ptepages_count++;
vm_page_unlock_queues();
vm_object_unlock(pmap_object);
memset((void *)phystokv(pa), 0, PAGE_SIZE);
return pa;
}
#endif
#ifdef MACH_XEN
void pmap_map_mfn(void *_addr, unsigned long mfn) {
vm_offset_t addr = (vm_offset_t) _addr;
pt_entry_t *pte, *pdp;
vm_offset_t ptp;
pt_entry_t ma = ((pt_entry_t) mfn) << PAGE_SHIFT;
if ((pte = pmap_pte(kernel_pmap, addr)) == PT_ENTRY_NULL) {
ptp = phystokv(pmap_page_table_page_alloc());
#ifdef MACH_PV_PAGETABLES
pmap_set_page_readonly((void*) ptp);
if (!hyp_mmuext_op_mfn (MMUEXT_PIN_L1_TABLE, pa_to_mfn(ptp)))
panic("couldn't pin page %lx(%lx)\n", (unsigned long) ptp,
(unsigned long) kv_to_ma(ptp));
#endif
pdp = pmap_pde(kernel_pmap, addr);
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmu_update_pte(kv_to_ma(pdp),
pa_to_pte(kv_to_ma(ptp)) | INTEL_PTE_VALID
#ifndef __x86_64__
| INTEL_PTE_USER
#endif
| INTEL_PTE_WRITE))
panic("%s:%d could not set pde %llx(%lx) to %lx(%lx)\n",__FILE__,__LINE__,
(long long unsigned int) kvtophys((vm_offset_t)pdp),
(unsigned long) kv_to_ma(pdp), (unsigned long) ptp,
(unsigned long) pa_to_ma(ptp));
#else
*pdp = pa_to_pte(kvtophys(ptp)) | INTEL_PTE_VALID
#ifndef __x86_64__
| INTEL_PTE_USER
#endif
| INTEL_PTE_WRITE;
#endif
pte = pmap_pte(kernel_pmap, addr);
}
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmu_update_pte(kv_to_ma(pte), ma | INTEL_PTE_VALID | INTEL_PTE_WRITE))
panic("%s:%d could not set pte %p(%lx) to %llx(%llx)\n",
__FILE__,__LINE__,pte,(unsigned long) kv_to_ma(pte),
(uint64_t) ma, (uint64_t) ma_to_pa(ma));
#else
WRITE_PTE(pte, ma | INTEL_PTE_VALID | INTEL_PTE_WRITE);
#endif
}
#endif
#ifdef MACH_XEN
static void
pmap_page_table_page_dealloc(vm_offset_t pa)
{
vm_page_t m;
vm_object_lock(pmap_object);
m = vm_page_lookup(pmap_object, pa);
vm_page_lock_queues();
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmuext_op_mfn (MMUEXT_UNPIN_TABLE, pa_to_mfn(pa)))
panic("couldn't unpin page %llx(%lx)\n", (uint64_t) pa, (unsigned long) kv_to_ma(pa));
pmap_set_page_readwrite((void*) phystokv(pa));
#endif
vm_page_free(m);
inuse_ptepages_count--;
vm_page_unlock_queues();
vm_object_unlock(pmap_object);
}
#endif
pmap_t pmap_create(vm_size_t size)
{
#ifdef __x86_64__
const int PDPNUM = PDPNUM_KERNEL;
#endif
pt_entry_t *page_dir[PDPNUM];
int i;
pmap_t p;
pmap_statistics_t stats;
if (size != 0) {
return(PMAP_NULL);
}
p = (pmap_t) kmem_cache_alloc(&pmap_cache);
if (p == PMAP_NULL)
return PMAP_NULL;
for (i = 0; i < PDPNUM; i++) {
page_dir[i] = (pt_entry_t *) kmem_cache_alloc(&pd_cache);
if (page_dir[i] == NULL) {
i -= 1;
while (i >= 0) {
kmem_cache_free(&pd_cache,
(vm_address_t) page_dir[i]);
i -= 1;
}
kmem_cache_free(&pmap_cache, (vm_address_t) p);
return PMAP_NULL;
}
memcpy(page_dir[i],
(void *) kernel_page_dir + i * INTEL_PGBYTES,
INTEL_PGBYTES);
}
#ifdef LINUX_DEV
#if VM_MIN_KERNEL_ADDRESS != 0
page_dir
#if PAE
[lin2pdpnum(LINEAR_MIN_KERNEL_ADDRESS - VM_MIN_KERNEL_ADDRESS)]
#else
[0]
#endif
[lin2pdenum(LINEAR_MIN_KERNEL_ADDRESS - VM_MIN_KERNEL_ADDRESS)]
= 0;
#endif
#endif
#ifdef MACH_PV_PAGETABLES
{
for (i = 0; i < PDPNUM; i++)
pmap_set_page_readonly((void *) page_dir[i]);
}
#endif
#if PAE
pt_entry_t *pdp_kernel = (pt_entry_t *) kmem_cache_alloc(&pdpt_cache);
if (pdp_kernel == NULL) {
for (i = 0; i < PDPNUM; i++)
kmem_cache_free(&pd_cache, (vm_address_t) page_dir[i]);
kmem_cache_free(&pmap_cache, (vm_address_t) p);
return PMAP_NULL;
}
memset(pdp_kernel, 0, INTEL_PGBYTES);
{
for (i = 0; i < PDPNUM; i++) {
int pdp_index = i;
#ifdef __x86_64__
pdp_index += lin2pdpnum(VM_MIN_KERNEL_ADDRESS);
#endif
WRITE_PTE(&pdp_kernel[pdp_index],
pa_to_pte(kvtophys((vm_offset_t) page_dir[i]))
| INTEL_PTE_VALID
#if (defined(__x86_64__) && !defined(MACH_HYP)) || defined(MACH_PV_PAGETABLES)
| INTEL_PTE_WRITE
#endif
);
}
}
#ifdef __x86_64__
p->l4base = (pt_entry_t *) kmem_cache_alloc(&l4_cache);
if (p->l4base == NULL)
panic("pmap_create");
memset(p->l4base, 0, INTEL_PGBYTES);
WRITE_PTE(&p->l4base[lin2l4num(VM_MIN_KERNEL_ADDRESS)],
pa_to_pte(kvtophys((vm_offset_t) pdp_kernel)) | INTEL_PTE_VALID | INTEL_PTE_WRITE);
#ifdef MACH_PV_PAGETABLES
if (kmem_alloc_wired(kernel_map,
(vm_offset_t *)&p->user_pdpbase, INTEL_PGBYTES)
!= KERN_SUCCESS)
panic("pmap_create");
memset(p->user_pdpbase, 0, INTEL_PGBYTES);
{
int i;
for (i = 0; i < lin2pdpnum(VM_MAX_USER_ADDRESS); i++)
WRITE_PTE(&p->user_pdpbase[i], pa_to_pte(kvtophys((vm_offset_t) page_dir[i])) | INTEL_PTE_VALID | INTEL_PTE_WRITE);
}
if (kmem_alloc_wired(kernel_map,
(vm_offset_t *)&p->user_l4base, INTEL_PGBYTES)
!= KERN_SUCCESS)
panic("pmap_create");
memset(p->user_l4base, 0, INTEL_PGBYTES);
WRITE_PTE(&p->user_l4base[0], pa_to_pte(kvtophys((vm_offset_t) p->user_pdpbase)) | INTEL_PTE_VALID | INTEL_PTE_WRITE);
#endif
#else
p->pdpbase = pdp_kernel;
#endif
#ifdef MACH_PV_PAGETABLES
#ifdef __x86_64__
pmap_set_page_readonly(p->l4base);
pmap_set_page_readonly(p->user_l4base);
pmap_set_page_readonly(p->user_pdpbase);
#else
pmap_set_page_readonly(p->pdpbase);
#endif
#endif
#else
#ifdef __x86_64__
#error "Invalid configuration: x86_64 requires PAE"
#else
p->dirbase = page_dir[0];
#endif
#endif
p->ref_count = 1;
simple_lock_init(&p->lock);
p->cpus_using = 0;
stats = &p->stats;
stats->resident_count = 0;
stats->wired_count = 0;
return(p);
}
void pmap_destroy(pmap_t p)
{
int c, s;
if (p == PMAP_NULL)
return;
SPLVM(s);
simple_lock(&p->lock);
c = --p->ref_count;
simple_unlock(&p->lock);
SPLX(s);
if (c != 0) {
return;
}
#if PAE
#ifdef __x86_64__
for (int l4i = 0; l4i < NPTES; l4i++) {
pt_entry_t pdp = (pt_entry_t) p->l4base[l4i];
if (!(pdp & INTEL_PTE_VALID))
continue;
pt_entry_t *pdpbase = (pt_entry_t*) ptetokv(pdp);
#else
pt_entry_t *pdpbase = p->pdpbase;
#endif
for (int l3i = 0; l3i < NPTES; l3i++) {
pt_entry_t pde = (pt_entry_t) pdpbase[l3i];
if (!(pde & INTEL_PTE_VALID))
continue;
pt_entry_t *pdebase = (pt_entry_t*) ptetokv(pde);
if (
#ifdef __x86_64__
l4i < lin2l4num(VM_MAX_USER_ADDRESS) ||
(l4i == lin2l4num(VM_MAX_USER_ADDRESS) && l3i < lin2pdpnum(VM_MAX_USER_ADDRESS))
#else
l3i < lin2pdpnum(VM_MAX_USER_ADDRESS)
#endif
)
for (int l2i = 0; l2i < NPTES; l2i++)
#else
#ifdef __x86_64__
#error "Invalid configuration: x86_64 requires PAE"
#else
pt_entry_t *pdebase = p->dirbase;
for (int l2i = 0; l2i < lin2pdenum(VM_MAX_USER_ADDRESS); l2i++)
#endif
#endif
{
pt_entry_t pte = (pt_entry_t) pdebase[l2i];
if (!(pte & INTEL_PTE_VALID))
continue;
kmem_cache_free(&pt_cache, (vm_offset_t)ptetokv(pte));
}
kmem_cache_free(&pd_cache, (vm_offset_t)pdebase);
#if PAE
}
kmem_cache_free(&pdpt_cache, (vm_offset_t)pdpbase);
#ifdef __x86_64__
}
kmem_cache_free(&l4_cache, (vm_offset_t) p->l4base);
#endif
#endif
kmem_cache_free(&pmap_cache, (vm_offset_t) p);
}
void pmap_reference(pmap_t p)
{
int s;
if (p != PMAP_NULL) {
SPLVM(s);
simple_lock(&p->lock);
p->ref_count++;
simple_unlock(&p->lock);
SPLX(s);
}
}
static
void pmap_remove_range(
pmap_t pmap,
vm_offset_t va,
pt_entry_t *spte,
pt_entry_t *epte)
{
pt_entry_t *cpte;
unsigned long num_removed, num_unwired;
unsigned long pai;
phys_addr_t pa;
#ifdef MACH_PV_PAGETABLES
int n, ii = 0;
struct mmu_update update[HYP_BATCH_MMU_UPDATES];
#endif
if (pmap == kernel_pmap && (va < kernel_virtual_start || va + (epte-spte)*PAGE_SIZE > kernel_virtual_end))
panic("pmap_remove_range(%lx-%lx) falls in physical memory area!\n", (unsigned long) va, (unsigned long) va + (epte-spte)*PAGE_SIZE);
#if DEBUG_PTE_PAGE
if (pmap != kernel_pmap)
ptep_check(get_pte_page(spte));
#endif
num_removed = 0;
num_unwired = 0;
for (cpte = spte; cpte < epte;
cpte += ptes_per_vm_page, va += PAGE_SIZE) {
if (*cpte == 0)
continue;
assert(*cpte & INTEL_PTE_VALID);
pa = pte_to_pa(*cpte);
num_removed++;
if (*cpte & INTEL_PTE_WIRED)
num_unwired++;
if (!valid_page(pa)) {
int i = ptes_per_vm_page;
pt_entry_t *lpte = cpte;
do {
#ifdef MACH_PV_PAGETABLES
update[ii].ptr = kv_to_ma(lpte);
update[ii].val = 0;
ii++;
if (ii == HYP_BATCH_MMU_UPDATES) {
hyp_mmu_update(kvtolin(&update), ii, kvtolin(&n), DOMID_SELF);
if (n != ii)
panic("couldn't pmap_remove_range\n");
ii = 0;
}
#else
*lpte = 0;
#endif
lpte++;
} while (--i > 0);
continue;
}
pai = pa_index(pa);
LOCK_PVH(pai);
{
int i;
pt_entry_t *lpte;
i = ptes_per_vm_page;
lpte = cpte;
do {
pmap_phys_attributes[pai] |=
*lpte & (PHYS_MODIFIED|PHYS_REFERENCED);
#ifdef MACH_PV_PAGETABLES
update[ii].ptr = kv_to_ma(lpte);
update[ii].val = 0;
ii++;
if (ii == HYP_BATCH_MMU_UPDATES) {
hyp_mmu_update(kvtolin(&update), ii, kvtolin(&n), DOMID_SELF);
if (n != ii)
panic("couldn't pmap_remove_range\n");
ii = 0;
}
#else
*lpte = 0;
#endif
lpte++;
} while (--i > 0);
}
{
pv_entry_t pv_h, prev, cur;
pv_h = pai_to_pvh(pai);
if (pv_h->pmap == PMAP_NULL) {
panic("pmap_remove: null pv_list for pai %lx at va %lx!", pai, (unsigned long) va);
}
if (pv_h->va == va && pv_h->pmap == pmap) {
cur = pv_h->next;
if (cur != PV_ENTRY_NULL) {
*pv_h = *cur;
PV_FREE(cur);
}
else {
pv_h->pmap = PMAP_NULL;
}
}
else {
cur = pv_h;
do {
prev = cur;
if ((cur = prev->next) == PV_ENTRY_NULL) {
panic("pmap-remove: mapping not in pv_list!");
}
} while (cur->va != va || cur->pmap != pmap);
prev->next = cur->next;
PV_FREE(cur);
}
UNLOCK_PVH(pai);
}
}
#ifdef MACH_PV_PAGETABLES
if (ii > HYP_BATCH_MMU_UPDATES)
panic("overflowed array in pmap_remove_range");
hyp_mmu_update(kvtolin(&update), ii, kvtolin(&n), DOMID_SELF);
if (n != ii)
panic("couldn't pmap_remove_range\n");
#endif
pmap->stats.resident_count -= num_removed;
pmap->stats.wired_count -= num_unwired;
}
void pmap_remove(
pmap_t map,
vm_offset_t s,
vm_offset_t e)
{
int spl;
pt_entry_t *spte, *epte;
vm_offset_t l;
vm_offset_t _s = s;
if (map == PMAP_NULL)
return;
PMAP_READ_LOCK(map, spl);
while (s < e) {
pt_entry_t *pde = pmap_pde(map, s);
l = (s + PDE_MAPPED_SIZE) & ~(PDE_MAPPED_SIZE-1);
if (l > e || l < s)
l = e;
if (pde && (*pde & INTEL_PTE_VALID)) {
spte = (pt_entry_t *)ptetokv(*pde);
spte = &spte[ptenum(s)];
epte = &spte[intel_btop(l-s)];
pmap_remove_range(map, s, spte, epte);
}
s = l;
}
PMAP_UPDATE_TLBS(map, _s, e);
PMAP_READ_UNLOCK(map, spl);
}
void pmap_page_protect(
phys_addr_t phys,
vm_prot_t prot)
{
pv_entry_t pv_h, prev;
pv_entry_t pv_e;
pt_entry_t *pte;
unsigned long pai;
pmap_t pmap;
int spl;
boolean_t remove;
assert(phys != vm_page_fictitious_addr);
if (!valid_page(phys)) {
return;
}
switch (prot) {
case VM_PROT_READ:
case VM_PROT_READ|VM_PROT_EXECUTE:
remove = FALSE;
break;
case VM_PROT_ALL:
return;
default:
remove = TRUE;
break;
}
PMAP_WRITE_LOCK(spl);
pai = pa_index(phys);
pv_h = pai_to_pvh(pai);
if (pv_h->pmap != PMAP_NULL) {
prev = pv_e = pv_h;
do {
vm_offset_t va;
pmap = pv_e->pmap;
simple_lock(&pmap->lock);
va = pv_e->va;
pte = pmap_pte(pmap, va);
assert(*pte & INTEL_PTE_VALID);
assert(pte_to_pa(*pte) == phys);
if (remove || pmap == kernel_pmap) {
if (*pte & INTEL_PTE_WIRED) {
pmap->stats.wired_count--;
}
{
int i = ptes_per_vm_page;
do {
pmap_phys_attributes[pai] |=
*pte & (PHYS_MODIFIED|PHYS_REFERENCED);
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmu_update_pte(kv_to_ma(pte++), 0))
panic("%s:%d could not clear pte %p\n",__FILE__,__LINE__,pte-1);
#else
*pte++ = 0;
#endif
} while (--i > 0);
}
pmap->stats.resident_count--;
if (pv_e == pv_h) {
pv_h->pmap = PMAP_NULL;
}
else {
prev->next = pv_e->next;
PV_FREE(pv_e);
}
}
else {
int i = ptes_per_vm_page;
do {
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmu_update_pte(kv_to_ma(pte), *pte & ~INTEL_PTE_WRITE))
panic("%s:%d could not disable write on pte %p\n",__FILE__,__LINE__,pte);
#else
*pte &= ~INTEL_PTE_WRITE;
#endif
pte++;
} while (--i > 0);
prev = pv_e;
}
PMAP_UPDATE_TLBS(pmap, va, va + PAGE_SIZE);
simple_unlock(&pmap->lock);
} while ((pv_e = prev->next) != PV_ENTRY_NULL);
if (pv_h->pmap == PMAP_NULL) {
pv_e = pv_h->next;
if (pv_e != PV_ENTRY_NULL) {
*pv_h = *pv_e;
PV_FREE(pv_e);
}
}
}
PMAP_WRITE_UNLOCK(spl);
}
void pmap_protect(
pmap_t map,
vm_offset_t s,
vm_offset_t e,
vm_prot_t prot)
{
pt_entry_t *spte, *epte;
vm_offset_t l;
int spl;
vm_offset_t _s = s;
if (map == PMAP_NULL)
return;
switch (prot) {
case VM_PROT_READ:
case VM_PROT_READ|VM_PROT_EXECUTE:
break;
case VM_PROT_READ|VM_PROT_WRITE:
case VM_PROT_ALL:
return;
default:
pmap_remove(map, s, e);
return;
}
#if (__i386__ && !(__i486__ || __i586__ || __i686__))
if (map == kernel_pmap) {
pmap_remove(map, s, e);
return;
}
#endif
SPLVM(spl);
simple_lock(&map->lock);
while (s < e) {
pt_entry_t *pde = pde = pmap_pde(map, s);
l = (s + PDE_MAPPED_SIZE) & ~(PDE_MAPPED_SIZE-1);
if (l > e || l < s)
l = e;
if (pde && (*pde & INTEL_PTE_VALID)) {
spte = (pt_entry_t *)ptetokv(*pde);
spte = &spte[ptenum(s)];
epte = &spte[intel_btop(l-s)];
#ifdef MACH_PV_PAGETABLES
int n, i = 0;
struct mmu_update update[HYP_BATCH_MMU_UPDATES];
#endif
while (spte < epte) {
if (*spte & INTEL_PTE_VALID) {
#ifdef MACH_PV_PAGETABLES
update[i].ptr = kv_to_ma(spte);
update[i].val = *spte & ~INTEL_PTE_WRITE;
i++;
if (i == HYP_BATCH_MMU_UPDATES) {
hyp_mmu_update(kvtolin(&update), i, kvtolin(&n), DOMID_SELF);
if (n != i)
panic("couldn't pmap_protect\n");
i = 0;
}
#else
*spte &= ~INTEL_PTE_WRITE;
#endif
}
spte++;
}
#ifdef MACH_PV_PAGETABLES
if (i > HYP_BATCH_MMU_UPDATES)
panic("overflowed array in pmap_protect");
hyp_mmu_update(kvtolin(&update), i, kvtolin(&n), DOMID_SELF);
if (n != i)
panic("couldn't pmap_protect\n");
#endif
}
s = l;
}
PMAP_UPDATE_TLBS(map, _s, e);
simple_unlock(&map->lock);
SPLX(spl);
}
typedef pt_entry_t* (*pmap_level_getter_t)(const pmap_t pmap, vm_offset_t addr);
static inline pt_entry_t* pmap_expand_level(pmap_t pmap, vm_offset_t v, int spl,
pmap_level_getter_t pmap_level,
pmap_level_getter_t pmap_level_upper,
int n_per_vm_page,
struct kmem_cache *cache)
{
pt_entry_t *pte;
while ((pte = pmap_level(pmap, v)) == PT_ENTRY_NULL) {
vm_offset_t ptp;
pt_entry_t *pdp;
int i;
if (pmap == kernel_pmap) {
panic("pmap_expand kernel pmap to %#zx", v);
}
PMAP_READ_UNLOCK(pmap, spl);
while (!(ptp = kmem_cache_alloc(cache)))
VM_PAGE_WAIT((void (*)()) 0);
memset((void *)ptp, 0, PAGE_SIZE);
PMAP_READ_LOCK(pmap, spl);
if (pmap_level(pmap, v) != PT_ENTRY_NULL) {
PMAP_READ_UNLOCK(pmap, spl);
kmem_cache_free(cache, ptp);
PMAP_READ_LOCK(pmap, spl);
continue;
}
i = n_per_vm_page;
pdp = pmap_level_upper(pmap, v);
do {
#ifdef MACH_PV_PAGETABLES
pmap_set_page_readonly((void *) ptp);
if (!hyp_mmuext_op_mfn (MMUEXT_PIN_L1_TABLE, kv_to_mfn(ptp)))
panic("couldn't pin page %lx(%lx)\n",(unsigned long) ptp,
(unsigned long) kv_to_ma(ptp));
if (!hyp_mmu_update_pte(pa_to_ma(kvtophys((vm_offset_t)pdp)),
pa_to_pte(pa_to_ma(kvtophys(ptp))) | INTEL_PTE_VALID
| (pmap != kernel_pmap ? INTEL_PTE_USER : 0)
| INTEL_PTE_WRITE))
panic("%s:%d could not set pde %p(%llx,%lx) to %lx(%llx,%lx) %lx\n",__FILE__,__LINE__,
pdp, (uint64_t) kvtophys((vm_offset_t)pdp),
(unsigned long) pa_to_ma(kvtophys((vm_offset_t)pdp)),
(unsigned long) ptp, (uint64_t) kvtophys(ptp),
(unsigned long) pa_to_ma(kvtophys(ptp)),
(unsigned long) pa_to_pte(kv_to_ma(ptp)));
#else
*pdp = pa_to_pte(kvtophys(ptp)) | INTEL_PTE_VALID
| (pmap != kernel_pmap ? INTEL_PTE_USER : 0)
| INTEL_PTE_WRITE;
#endif
pdp++;
ptp += INTEL_PGBYTES;
} while (--i > 0);
continue;
}
return pte;
}
static inline pt_entry_t* pmap_expand(pmap_t pmap, vm_offset_t v, int spl)
{
#ifdef PAE
#ifdef __x86_64__
pmap_expand_level(pmap, v, spl, pmap_ptp, pmap_l4base, 1, &pdpt_cache);
#endif
pmap_expand_level(pmap, v, spl, pmap_pde, pmap_ptp, 1, &pd_cache);
#endif
return pmap_expand_level(pmap, v, spl, pmap_pte, pmap_pde, ptes_per_vm_page, &pt_cache);
}
void pmap_enter(
pmap_t pmap,
vm_offset_t v,
phys_addr_t pa,
vm_prot_t prot,
boolean_t wired)
{
boolean_t is_physmem;
pt_entry_t *pte;
pv_entry_t pv_h;
unsigned long i, pai;
pv_entry_t pv_e;
pt_entry_t template;
int spl;
phys_addr_t old_pa;
assert(pa != vm_page_fictitious_addr);
if (pmap_debug) printf("pmap(%zx, %llx)\n", v, (unsigned long long) pa);
if (pmap == PMAP_NULL)
return;
if (pmap == kernel_pmap && (v < kernel_virtual_start || v >= kernel_virtual_end))
panic("pmap_enter(%lx, %llx) falls in physical memory area!\n", (unsigned long) v, (unsigned long long) pa);
#if (__i386__ && !(__i486__ || __i586__ || __i686__))
if (pmap == kernel_pmap && (prot & VM_PROT_WRITE) == 0
&& !wired ) {
PMAP_READ_LOCK(pmap, spl);
pte = pmap_pte(pmap, v);
if (pte != PT_ENTRY_NULL && *pte != 0) {
pmap_remove_range(pmap, v, pte,
pte + ptes_per_vm_page);
PMAP_UPDATE_TLBS(pmap, v, v + PAGE_SIZE);
}
PMAP_READ_UNLOCK(pmap, spl);
return;
}
#endif
pv_e = PV_ENTRY_NULL;
Retry:
PMAP_READ_LOCK(pmap, spl);
pte = pmap_expand(pmap, v, spl);
if (vm_page_ready())
is_physmem = (vm_page_lookup_pa(pa) != NULL);
else
is_physmem = (pa < biosmem_directmap_end());
old_pa = pte_to_pa(*pte);
if (*pte && old_pa == pa) {
if (wired && !(*pte & INTEL_PTE_WIRED))
pmap->stats.wired_count++;
else if (!wired && (*pte & INTEL_PTE_WIRED))
pmap->stats.wired_count--;
template = pa_to_pte(pa) | INTEL_PTE_VALID;
if (pmap != kernel_pmap)
template |= INTEL_PTE_USER;
if (prot & VM_PROT_WRITE)
template |= INTEL_PTE_WRITE;
if (machine_slot[cpu_number()].cpu_type >= CPU_TYPE_I486
&& !is_physmem)
template |= INTEL_PTE_NCACHE|INTEL_PTE_WTHRU;
if (wired)
template |= INTEL_PTE_WIRED;
i = ptes_per_vm_page;
do {
if (*pte & INTEL_PTE_MOD)
template |= INTEL_PTE_MOD;
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmu_update_pte(kv_to_ma(pte), pa_to_ma(template)))
panic("%s:%d could not set pte %p to %llx\n",__FILE__,__LINE__,pte,template);
#else
WRITE_PTE(pte, template)
#endif
pte++;
pte_increment_pa(template);
} while (--i > 0);
PMAP_UPDATE_TLBS(pmap, v, v + PAGE_SIZE);
}
else {
if (*pte) {
pmap_remove_range(pmap, v, pte,
pte + ptes_per_vm_page);
PMAP_UPDATE_TLBS(pmap, v, v + PAGE_SIZE);
}
if (valid_page(pa)) {
pai = pa_index(pa);
LOCK_PVH(pai);
pv_h = pai_to_pvh(pai);
if (pv_h->pmap == PMAP_NULL) {
pv_h->va = v;
pv_h->pmap = pmap;
pv_h->next = PV_ENTRY_NULL;
}
else {
#if DEBUG
{
pv_entry_t e = pv_h;
while (e != PV_ENTRY_NULL) {
if (e->pmap == pmap && e->va == v)
panic("pmap_enter: already in pv_list");
e = e->next;
}
}
#endif
if (pv_e == PV_ENTRY_NULL) {
PV_ALLOC(pv_e);
if (pv_e == PV_ENTRY_NULL) {
UNLOCK_PVH(pai);
PMAP_READ_UNLOCK(pmap, spl);
pv_e = (pv_entry_t) kmem_cache_alloc(&pv_list_cache);
goto Retry;
}
}
pv_e->va = v;
pv_e->pmap = pmap;
pv_e->next = pv_h->next;
pv_h->next = pv_e;
pv_e = PV_ENTRY_NULL;
}
UNLOCK_PVH(pai);
}
pmap->stats.resident_count++;
if (wired)
pmap->stats.wired_count++;
template = pa_to_pte(pa) | INTEL_PTE_VALID;
if (pmap != kernel_pmap)
template |= INTEL_PTE_USER;
if (prot & VM_PROT_WRITE)
template |= INTEL_PTE_WRITE;
if (machine_slot[cpu_number()].cpu_type >= CPU_TYPE_I486
&& !is_physmem)
template |= INTEL_PTE_NCACHE|INTEL_PTE_WTHRU;
if (wired)
template |= INTEL_PTE_WIRED;
i = ptes_per_vm_page;
do {
#ifdef MACH_PV_PAGETABLES
if (!(hyp_mmu_update_pte(kv_to_ma(pte), pa_to_ma(template))))
panic("%s:%d could not set pte %p to %llx\n",__FILE__,__LINE__,pte,template);
#else
WRITE_PTE(pte, template)
#endif
pte++;
pte_increment_pa(template);
} while (--i > 0);
}
if (pv_e != PV_ENTRY_NULL) {
PV_FREE(pv_e);
}
PMAP_READ_UNLOCK(pmap, spl);
}
void pmap_change_wiring(
pmap_t map,
vm_offset_t v,
boolean_t wired)
{
pt_entry_t *pte;
int i;
int spl;
PMAP_READ_LOCK(map, spl);
if ((pte = pmap_pte(map, v)) == PT_ENTRY_NULL)
panic("pmap_change_wiring: pte missing");
if (wired && !(*pte & INTEL_PTE_WIRED)) {
map->stats.wired_count++;
i = ptes_per_vm_page;
do {
*pte++ |= INTEL_PTE_WIRED;
} while (--i > 0);
}
else if (!wired && (*pte & INTEL_PTE_WIRED)) {
map->stats.wired_count--;
i = ptes_per_vm_page;
do {
#ifdef MACH_PV_PAGETABLES
if (!(hyp_mmu_update_pte(kv_to_ma(pte), *pte & ~INTEL_PTE_WIRED)))
panic("%s:%d could not wire down pte %p\n",__FILE__,__LINE__,pte);
#else
*pte &= ~INTEL_PTE_WIRED;
#endif
pte++;
} while (--i > 0);
}
PMAP_READ_UNLOCK(map, spl);
}
phys_addr_t pmap_extract(
pmap_t pmap,
vm_offset_t va)
{
pt_entry_t *pte;
phys_addr_t pa;
int spl;
SPLVM(spl);
simple_lock(&pmap->lock);
if ((pte = pmap_pte(pmap, va)) == PT_ENTRY_NULL)
pa = 0;
else if (!(*pte & INTEL_PTE_VALID))
pa = 0;
else
pa = pte_to_pa(*pte) + (va & INTEL_OFFMASK);
simple_unlock(&pmap->lock);
SPLX(spl);
return(pa);
}
#if 0
void pmap_copy(
pmap_t dst_pmap,
pmap_t src_pmap,
vm_offset_t dst_addr,
vm_size_t len,
vm_offset_t src_addr)
{
}
#endif
void pmap_collect(pmap_t p)
{
pt_entry_t *ptp;
pt_entry_t *eptp;
phys_addr_t pa;
int spl, wired;
if (p == PMAP_NULL)
return;
if (p == kernel_pmap)
return;
PMAP_READ_LOCK(p, spl);
#if PAE
#ifdef __x86_64__
for (int l4i = 0; l4i < lin2l4num(VM_MAX_USER_ADDRESS); l4i++) {
pt_entry_t pdp = (pt_entry_t) p->l4base[l4i];
if (!(pdp & INTEL_PTE_VALID))
continue;
pt_entry_t *pdpbase = (pt_entry_t*) ptetokv(pdp);
for (int l3i = 0; l3i < NPTES; l3i++)
#else
pt_entry_t *pdpbase = p->pdpbase;
for (int l3i = 0; l3i < lin2pdpnum(VM_MAX_USER_ADDRESS); l3i++)
#endif
{
pt_entry_t pde = (pt_entry_t ) pdpbase[l3i];
if (!(pde & INTEL_PTE_VALID))
continue;
pt_entry_t *pdebase = (pt_entry_t*) ptetokv(pde);
for (int l2i = 0; l2i < NPTES; l2i++)
#else
#ifdef __x86_64__
#error "Invalid configuration: x86_64 requires PAE"
#else
pt_entry_t *pdebase = p->dirbase;
for (int l2i = 0; l2i < lin2pdenum(VM_MAX_USER_ADDRESS); l2i++)
#endif
#endif
{
pt_entry_t pte = (pt_entry_t) pdebase[l2i];
if (!(pte & INTEL_PTE_VALID))
continue;
pa = pte_to_pa(pte);
ptp = (pt_entry_t *)phystokv(pa);
eptp = ptp + NPTES*ptes_per_vm_page;
wired = 0;
{
pt_entry_t *ptep;
for (ptep = ptp; ptep < eptp; ptep++) {
if (*ptep & INTEL_PTE_WIRED) {
wired = 1;
break;
}
}
}
if (!wired) {
{
vm_offset_t va = pagenum2lin(l4i, l3i, l2i, 0);
if (p == kernel_pmap)
va = lintokv(va);
pmap_remove_range(p, va, ptp, eptp);
}
{
int i = ptes_per_vm_page;
pt_entry_t *pdep = &pdebase[l2i];
do {
#ifdef MACH_PV_PAGETABLES
unsigned long pte = *pdep;
void *ptable = (void*) ptetokv(pte);
if (!(hyp_mmu_update_pte(pa_to_ma(kvtophys((vm_offset_t)pdep++)), 0)))
panic("%s:%d could not clear pde %p\n",__FILE__,__LINE__,pdep-1);
if (!hyp_mmuext_op_mfn (MMUEXT_UNPIN_TABLE, kv_to_mfn(ptable)))
panic("couldn't unpin page %p(%lx)\n", ptable,
(unsigned long) pa_to_ma(kvtophys((vm_offset_t)ptable)));
pmap_set_page_readwrite(ptable);
#else
*pdep++ = 0;
#endif
} while (--i > 0);
}
PMAP_READ_UNLOCK(p, spl);
kmem_cache_free(&pt_cache, (vm_offset_t)ptetokv(pte));
PMAP_READ_LOCK(p, spl);
}
}
#if PAE
}
#ifdef __x86_64__
}
#endif
#endif
PMAP_UPDATE_TLBS(p, VM_MIN_USER_ADDRESS, VM_MAX_USER_ADDRESS);
PMAP_READ_UNLOCK(p, spl);
return;
}
#if MACH_KDB
int pmap_whatis(pmap_t p, vm_offset_t a)
{
pt_entry_t *ptp;
phys_addr_t pa;
int spl;
int ret = 0;
if (p == PMAP_NULL)
return 0;
PMAP_READ_LOCK(p, spl);
#if PAE
#ifdef __x86_64__
if (a >= (vm_offset_t) p->l4base && a < (vm_offset_t) (&p->l4base[NPTES])) {
db_printf("L4 for pmap %p\n", p);
ret = 1;
}
for (int l4i = 0; l4i < NPTES; l4i++) {
pt_entry_t pdp = (pt_entry_t) p->l4base[l4i];
if (!(pdp & INTEL_PTE_VALID))
continue;
pt_entry_t *pdpbase = (pt_entry_t*) ptetokv(pdp);
#else
int l4i = 0;
pt_entry_t *pdpbase = p->pdpbase;
#endif
if (a >= (vm_offset_t) pdpbase && a < (vm_offset_t) (&pdpbase[NPTES])) {
db_printf("PDP %d for pmap %p\n", l4i, p);
ret = 1;
}
for (int l3i = 0; l3i < NPTES; l3i++)
{
pt_entry_t pde = (pt_entry_t ) pdpbase[l3i];
if (!(pde & INTEL_PTE_VALID))
continue;
pt_entry_t *pdebase = (pt_entry_t*) ptetokv(pde);
#else
int l4i = 0, l3i = 0;
#ifdef __x86_64__
#error "Invalid configuration: x86_64 requires PAE"
#else
pt_entry_t *pdebase = p->dirbase;
#endif
#endif
if (a >= (vm_offset_t) pdebase && a < (vm_offset_t) (&pdebase[NPTES])) {
db_printf("PDE %d %d for pmap %p\n", l4i, l3i, p);
ret = 1;
}
for (int l2i = 0; l2i < NPTES; l2i++)
{
pt_entry_t pte = (pt_entry_t) pdebase[l2i];
if (!(pte & INTEL_PTE_VALID))
continue;
pa = pte_to_pa(pte);
ptp = (pt_entry_t *)phystokv(pa);
if (a >= (vm_offset_t) ptp && a < (vm_offset_t) (&ptp[NPTES*ptes_per_vm_page])) {
db_printf("PTP %d %d %d for pmap %p\n", l4i, l3i, l2i, p);
ret = 1;
}
}
#if PAE
}
#ifdef __x86_64__
}
#endif
#endif
PMAP_READ_UNLOCK(p, spl);
if (p == kernel_pmap) {
phys_addr_t pa;
if (DB_VALID_KERN_ADDR(a))
pa = kvtophys(a);
else
pa = pmap_extract(current_task()->map->pmap, a);
if (valid_page(pa)) {
unsigned long pai;
pv_entry_t pv_h;
pai = pa_index(pa);
for (pv_h = pai_to_pvh(pai);
pv_h && pv_h->pmap;
pv_h = pv_h->next)
db_printf("pmap %p at %llx\n", pv_h->pmap, pv_h->va);
}
}
return ret;
}
#endif
#if 0
void pmap_activate(pmap_t my_pmap, thread_t th, int my_cpu)
{
PMAP_ACTIVATE(my_pmap, th, my_cpu);
}
#endif
#if 0
void pmap_deactivate(pmap_t pmap, thread_t th, int which_cpu)
{
PMAP_DEACTIVATE(pmap, th, which_cpu);
}
#endif
#if 0
pmap_t pmap_kernel()
{
return (kernel_pmap);
}
#endif
#if 0
pmap_zero_page(vm_offset_t phys)
{
int i;
assert(phys != vm_page_fictitious_addr);
i = PAGE_SIZE / INTEL_PGBYTES;
phys = intel_pfn(phys);
while (i--)
zero_phys(phys++);
}
#endif
#if 0
pmap_copy_page(vm_offset_t src, vm_offset_t dst)
{
int i;
assert(src != vm_page_fictitious_addr);
assert(dst != vm_page_fictitious_addr);
i = PAGE_SIZE / INTEL_PGBYTES;
while (i--) {
copy_phys(intel_pfn(src), intel_pfn(dst));
src += INTEL_PGBYTES;
dst += INTEL_PGBYTES;
}
}
#endif
void
pmap_pageable(
pmap_t pmap,
vm_offset_t start,
vm_offset_t end,
boolean_t pageable)
{
}
static void
phys_attribute_clear(
phys_addr_t phys,
int bits)
{
pv_entry_t pv_h;
pv_entry_t pv_e;
pt_entry_t *pte;
unsigned long pai;
pmap_t pmap;
int spl;
assert(phys != vm_page_fictitious_addr);
if (!valid_page(phys)) {
return;
}
PMAP_WRITE_LOCK(spl);
pai = pa_index(phys);
pv_h = pai_to_pvh(pai);
if (pv_h->pmap != PMAP_NULL) {
for (pv_e = pv_h; pv_e != PV_ENTRY_NULL; pv_e = pv_e->next) {
vm_offset_t va;
pmap = pv_e->pmap;
simple_lock(&pmap->lock);
va = pv_e->va;
pte = pmap_pte(pmap, va);
assert(*pte & INTEL_PTE_VALID);
assert(pte_to_pa(*pte) == phys);
{
int i = ptes_per_vm_page;
do {
#ifdef MACH_PV_PAGETABLES
if (!(hyp_mmu_update_pte(kv_to_ma(pte), *pte & ~bits)))
panic("%s:%d could not clear bits %x from pte %p\n",__FILE__,__LINE__,bits,pte);
#else
*pte &= ~bits;
#endif
} while (--i > 0);
}
PMAP_UPDATE_TLBS(pmap, va, va + PAGE_SIZE);
simple_unlock(&pmap->lock);
}
}
pmap_phys_attributes[pai] &= ~bits;
PMAP_WRITE_UNLOCK(spl);
}
static boolean_t
phys_attribute_test(
phys_addr_t phys,
int bits)
{
pv_entry_t pv_h;
pv_entry_t pv_e;
pt_entry_t *pte;
unsigned long pai;
pmap_t pmap;
int spl;
assert(phys != vm_page_fictitious_addr);
if (!valid_page(phys)) {
return (FALSE);
}
PMAP_WRITE_LOCK(spl);
pai = pa_index(phys);
pv_h = pai_to_pvh(pai);
if (pmap_phys_attributes[pai] & bits) {
PMAP_WRITE_UNLOCK(spl);
return (TRUE);
}
if (pv_h->pmap != PMAP_NULL) {
for (pv_e = pv_h; pv_e != PV_ENTRY_NULL; pv_e = pv_e->next) {
pmap = pv_e->pmap;
simple_lock(&pmap->lock);
{
vm_offset_t va;
va = pv_e->va;
pte = pmap_pte(pmap, va);
assert(*pte & INTEL_PTE_VALID);
assert(pte_to_pa(*pte) == phys);
}
{
int i = ptes_per_vm_page;
do {
if (*pte & bits) {
simple_unlock(&pmap->lock);
PMAP_WRITE_UNLOCK(spl);
return (TRUE);
}
} while (--i > 0);
}
simple_unlock(&pmap->lock);
}
}
PMAP_WRITE_UNLOCK(spl);
return (FALSE);
}
void pmap_clear_modify(phys_addr_t phys)
{
phys_attribute_clear(phys, PHYS_MODIFIED);
}
boolean_t pmap_is_modified(phys_addr_t phys)
{
return (phys_attribute_test(phys, PHYS_MODIFIED));
}
void pmap_clear_reference(phys_addr_t phys)
{
phys_attribute_clear(phys, PHYS_REFERENCED);
}
boolean_t pmap_is_referenced(phys_addr_t phys)
{
return (phys_attribute_test(phys, PHYS_REFERENCED));
}
#if NCPUS > 1
void signal_cpus(
cpu_set use_list,
pmap_t pmap,
vm_offset_t start,
vm_offset_t end)
{
int which_cpu, j;
pmap_update_list_t update_list_p;
while ((which_cpu = __builtin_ffs(use_list)) != 0) {
which_cpu -= 1;
update_list_p = &cpu_update_list[which_cpu];
simple_lock(&update_list_p->lock);
j = update_list_p->count;
if (j >= UPDATE_LIST_SIZE) {
update_list_p->item[UPDATE_LIST_SIZE-1].pmap = kernel_pmap;
update_list_p->item[UPDATE_LIST_SIZE-1].start = VM_MIN_USER_ADDRESS;
update_list_p->item[UPDATE_LIST_SIZE-1].end = VM_MAX_KERNEL_ADDRESS;
}
else {
update_list_p->item[j].pmap = pmap;
update_list_p->item[j].start = start;
update_list_p->item[j].end = end;
update_list_p->count = j+1;
}
cpu_update_needed[which_cpu] = TRUE;
simple_unlock(&update_list_p->lock);
__sync_synchronize();
if (((cpus_idle & (1 << which_cpu)) == 0))
interrupt_processor(which_cpu);
use_list &= ~(1 << which_cpu);
}
}
void process_pmap_updates(pmap_t my_pmap)
{
int my_cpu = cpu_number();
pmap_update_list_t update_list_p;
int j;
pmap_t pmap;
update_list_p = &cpu_update_list[my_cpu];
assert_splvm();
simple_lock_nocheck(&update_list_p->lock);
for (j = 0; j < update_list_p->count; j++) {
pmap = update_list_p->item[j].pmap;
if (pmap == my_pmap ||
pmap == kernel_pmap) {
INVALIDATE_TLB(pmap,
update_list_p->item[j].start,
update_list_p->item[j].end);
}
}
update_list_p->count = 0;
cpu_update_needed[my_cpu] = FALSE;
simple_unlock_nocheck(&update_list_p->lock);
}
void pmap_update_interrupt(void)
{
int my_cpu;
pmap_t my_pmap;
int s;
my_cpu = cpu_number();
if (cpus_idle & (1 << my_cpu))
return;
if (current_thread() == THREAD_NULL)
my_pmap = kernel_pmap;
else {
my_pmap = current_pmap();
if (!pmap_in_use(my_pmap, my_cpu))
my_pmap = kernel_pmap;
}
s = splvm();
do {
i_bit_clear(my_cpu, &cpus_active);
while (my_pmap->lock.lock_data ||
kernel_pmap->lock.lock_data)
cpu_pause();
process_pmap_updates(my_pmap);
i_bit_set(my_cpu, &cpus_active);
} while (cpu_update_needed[my_cpu]);
splx(s);
}
#else
void pmap_update_interrupt(void)
{
}
#endif
#if defined(__i386__) || defined (__x86_64__)
void
pmap_unmap_page_zero (void)
{
int *pte;
printf("Unmapping the zero page.  Some BIOS functions may not be working any more.\n");
pte = (int *) pmap_pte (kernel_pmap, 0);
if (!pte)
return;
assert (pte);
#ifdef MACH_PV_PAGETABLES
if (!hyp_mmu_update_pte(kv_to_ma(pte), 0))
printf("couldn't unmap page 0\n");
#else
*pte = 0;
INVALIDATE_TLB(kernel_pmap, 0, PAGE_SIZE);
#endif
}
#endif
void
pmap_make_temporary_mapping(void)
{
#if INIT_VM_MIN_KERNEL_ADDRESS != LINEAR_MIN_KERNEL_ADDRESS
int i;
vm_offset_t delta = INIT_VM_MIN_KERNEL_ADDRESS - LINEAR_MIN_KERNEL_ADDRESS;
if ((vm_offset_t)(-delta) < delta)
delta = (vm_offset_t)(-delta);
int nb_direct = delta >> PDESHIFT;
for (i = 0; i < nb_direct; i++)
kernel_page_dir[lin2pdenum_cont(INIT_VM_MIN_KERNEL_ADDRESS) + i] =
kernel_page_dir[lin2pdenum_cont(LINEAR_MIN_KERNEL_ADDRESS) + i];
#endif
#ifdef LINUX_DEV
#if VM_MIN_KERNEL_ADDRESS != 0
kernel_page_dir[lin2pdenum_cont(LINEAR_MIN_KERNEL_ADDRESS - VM_MIN_KERNEL_ADDRESS)] =
kernel_page_dir[lin2pdenum_cont(LINEAR_MIN_KERNEL_ADDRESS)];
#endif
#endif
#ifdef MACH_PV_PAGETABLES
int i;
#ifndef __x86_64__
const int PDPNUM_KERNEL = PDPNUM;
#endif
for (i = 0; i < PDPNUM_KERNEL; i++)
pmap_set_page_readonly_init((void*) kernel_page_dir + i * INTEL_PGBYTES);
#if PAE
#ifndef __x86_64__
pmap_set_page_readonly_init(kernel_pmap->pdpbase);
#endif
#endif
#endif
}
void
pmap_set_page_dir(void)
{
#if PAE
#ifdef __x86_64__
set_cr3((unsigned long)_kvtophys(kernel_pmap->l4base));
#else
set_cr3((unsigned long)_kvtophys(kernel_pmap->pdpbase));
#endif
#ifndef MACH_HYP
if (!CPU_HAS_FEATURE(CPU_FEATURE_PAE))
panic("CPU doesn't have support for PAE.");
set_cr4(get_cr4() | CR4_PAE);
#endif
#else
set_cr3((unsigned long)_kvtophys(kernel_page_dir));
#endif
}
void
pmap_remove_temporary_mapping(void)
{
#if INIT_VM_MIN_KERNEL_ADDRESS != LINEAR_MIN_KERNEL_ADDRESS
int i;
vm_offset_t delta = INIT_VM_MIN_KERNEL_ADDRESS - LINEAR_MIN_KERNEL_ADDRESS;
if ((vm_offset_t)(-delta) < delta)
delta = (vm_offset_t)(-delta);
int nb_direct = delta >> PDESHIFT;
for (i = 0 ; i < nb_direct; i++) {
#ifdef MACH_XEN
#ifdef MACH_PSEUDO_PHYS
if (!hyp_mmu_update_pte(kv_to_ma(&kernel_page_dir[lin2pdenum_cont(VM_MIN_KERNEL_ADDRESS) + i]), 0))
#else
if (hyp_do_update_va_mapping(VM_MIN_KERNEL_ADDRESS + i * INTEL_PGBYTES, 0, UVMF_INVLPG | UVMF_ALL))
#endif
printf("couldn't unmap frame %d\n", i);
#else
kernel_page_dir[lin2pdenum_cont(INIT_VM_MIN_KERNEL_ADDRESS) + i] = 0;
#endif
}
#endif
#ifdef LINUX_DEV
#if VM_MIN_KERNEL_ADDRESS != 0
kernel_page_dir[lin2pdenum_cont(LINEAR_MIN_KERNEL_ADDRESS - VM_MIN_KERNEL_ADDRESS)] =
kernel_page_dir[lin2pdenum_cont(LINEAR_MIN_KERNEL_ADDRESS)];
#endif
#endif
#ifdef MACH_XEN
hyp_free_page(0, (void*) VM_MIN_KERNEL_ADDRESS);
#endif
flush_tlb();
}