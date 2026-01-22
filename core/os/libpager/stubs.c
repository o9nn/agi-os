#include "priv.h"
#include "memory_object_S.h"
#include <stdio.h>
kern_return_t __attribute__((weak))
_pager_S_memory_object_copy (struct pager *p,
memory_object_control_t obj_ctl,
vm_offset_t off,
vm_size_t len,
mach_port_t new)
{
printf ("m_o_copy called\n");
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
_pager_S_memory_object_supply_completed (struct pager *p,
mach_port_t ctl,
vm_offset_t off,
vm_size_t len,
kern_return_t result,
vm_offset_t err_off)
{
printf ("m_o_supply_completed called\n");
return EOPNOTSUPP;
}