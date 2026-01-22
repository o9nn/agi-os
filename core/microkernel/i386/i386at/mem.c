#include <device/io_req.h>
#include <i386/model_dep.h>
#include <i386at/biosmem.h>
#include <i386at/mem.h>
vm_offset_t
memmmap(dev_t dev, vm_offset_t off, vm_prot_t prot)
{
if (biosmem_addr_available(off))
return -1;
return i386_btop(off);
}