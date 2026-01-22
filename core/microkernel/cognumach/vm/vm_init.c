#include <mach/machine/vm_types.h>
#include <kern/slab.h>
#include <kern/kalloc.h>
#include <vm/vm_fault.h>
#include <vm/vm_init.h>
#include <vm/vm_object.h>
#include <vm/vm_map.h>
#include <vm/vm_page.h>
#include <vm/vm_kern.h>
#include <vm/memory_object.h>
#include <vm/memory_object_proxy.h>
#include <vm/vm_block_cache.h>
void vm_mem_bootstrap(void)
{
vm_offset_t	start, end;
vm_page_bootstrap(&start, &end);
slab_bootstrap();
vm_object_bootstrap();
vm_block_cache_init();
vm_map_init();
kmem_init(start, end);
pmap_init();
slab_init();
kalloc_init();
vm_fault_init();
vm_page_module_init();
memory_manager_default_init();
}
void vm_mem_init(void)
{
vm_object_init();
memory_object_proxy_init();
vm_page_info_all();
}