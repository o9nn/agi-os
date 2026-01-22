#include <link.h>
#include <dlfcn.h>
#include <hurd.h>
#include <error.h>
#include <elf.h>
#include <mach/gnumach.h>
#include <mach/vm_param.h>
static error_t
wire_segment_internal (vm_address_t start,
vm_size_t len,
host_priv_t host_priv)
{
vm_address_t addr;
vm_size_t size;
vm_prot_t protection;
vm_prot_t max_protection;
vm_inherit_t inheritance;
boolean_t shared;
mach_port_t object_name;
vm_offset_t offset;
error_t err;
volatile char *poke;
do
{
addr = start;
err = vm_region (mach_task_self (), &addr, &size, &protection,
&max_protection, &inheritance, &shared, &object_name,
&offset);
if (err == KERN_NO_SPACE)
return 0;
if (err)
return err;
mach_port_deallocate (mach_task_self (), object_name);
if (protection != VM_PROT_NONE)
{
if (addr + size > start + len)
size = len - (addr - start);
if (!(protection & VM_PROT_WRITE))
{
err = vm_protect (mach_task_self (), addr, size, 0, max_protection);
if (err)
return err;
}
for (poke = (char *) addr;
(vm_address_t) poke < addr + size;
poke += vm_page_size)
*poke = *poke;
err = vm_wire (host_priv, mach_task_self (), addr, size, protection);
if (err)
return err;
if (!(protection & VM_PROT_WRITE))
{
err = vm_protect (mach_task_self (), addr, size, 0, protection);
if (err)
return err;
}
}
len -= (addr - start) + size;
start = addr + size;
}
while (len);
return err;
}
error_t
wire_segment (vm_address_t start,
vm_size_t len)
{
mach_port_t host, device;
error_t err;
err = get_privileged_ports (&host, &device);
if (err)
return err;
err = wire_segment_internal (start, len, host);
mach_port_deallocate (mach_task_self (), host);
mach_port_deallocate (mach_task_self (), device);
return err;
}
error_t
wire_task_self (void)
{
mach_port_t host, device;
error_t err;
err = get_privileged_ports (&host, &device);
if (err)
return err;
err = wire_segment_internal (0, (vm_size_t) -1, host);
if (err)
goto out;
err = vm_wire_all (host, mach_task_self (), VM_WIRE_ALL);
out:
mach_port_deallocate (mach_task_self (), host);
mach_port_deallocate (mach_task_self (), device);
return err;
}