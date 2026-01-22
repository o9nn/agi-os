#include <string.h>
#include <kern/assert.h>
#include <kern/debug.h>
#include <kern/kalloc.h>
#include <kern/host.h>
#include <mach/host_info.h>
#include <mach/kern_return.h>
#include <mach/machine.h>
#include <mach/port.h>
#include <kern/processor.h>
#include <kern/ipc_host.h>
#include <kern/mach_clock.h>
#include <kern/mach_host.server.h>
#include <mach/vm_param.h>
host_data_t	realhost;
kern_return_t host_processors(
const host_t		host,
processor_array_t	*processor_list,
natural_t		*countp)
{
unsigned		i;
processor_t		*tp;
vm_offset_t		addr;
unsigned int		count;
if (host == HOST_NULL)
return KERN_INVALID_ARGUMENT;
count = 0;
for (i = 0; i < NCPUS; i++)
if (machine_slot[i].is_cpu)
count++;
if (count == 0)
panic("host_processors");
addr = kalloc((vm_size_t) (count * sizeof(mach_port_t)));
if (addr == 0)
return KERN_RESOURCE_SHORTAGE;
tp = (processor_t *) addr;
for (i = 0; i < NCPUS; i++)
if (machine_slot[i].is_cpu)
*tp++ = cpu_to_processor(i);
*countp = count;
*processor_list = (mach_port_t *) addr;
tp = (processor_t *) addr;
for (i = 0; i < count; i++)
((mach_port_t *) tp)[i] =
(mach_port_t)convert_processor_to_port(tp[i]);
return KERN_SUCCESS;
}
kern_return_t	host_info(
const host_t	host,
int		flavor,
host_info_t	info,
natural_t	*count)
{
integer_t	i, *slot_ptr;
if (host == HOST_NULL)
return KERN_INVALID_ARGUMENT;
switch(flavor) {
case HOST_BASIC_INFO:
{
host_basic_info_t	basic_info;
if (*count < HOST_BASIC_INFO_COUNT)
return KERN_FAILURE;
basic_info = (host_basic_info_t) info;
basic_info->max_cpus = machine_info.max_cpus;
basic_info->avail_cpus = machine_info.avail_cpus;
basic_info->memory_size = machine_info.memory_size;
basic_info->cpu_type =
machine_slot[master_processor->slot_num].cpu_type;
basic_info->cpu_subtype =
machine_slot[master_processor->slot_num].cpu_subtype;
*count = HOST_BASIC_INFO_COUNT;
return KERN_SUCCESS;
}
case HOST_PROCESSOR_SLOTS:
if (*count < NCPUS)
return KERN_INVALID_ARGUMENT;
slot_ptr = (integer_t *)info;
*count = 0;
for (i = 0; i < NCPUS; i++) {
if (machine_slot[i].is_cpu &&
machine_slot[i].running) {
*slot_ptr++ = i;
(*count)++;
}
}
return KERN_SUCCESS;
case HOST_SCHED_INFO:
{
host_sched_info_t	sched_info;
extern int	min_quantum;
if (*count < HOST_SCHED_INFO_COUNT)
return(KERN_FAILURE);
sched_info = (host_sched_info_t) info;
sched_info->min_timeout = tick / 1000;
sched_info->min_quantum = min_quantum * tick / 1000;
*count = HOST_SCHED_INFO_COUNT;
return KERN_SUCCESS;
}
case HOST_LOAD_INFO:
{
host_load_info_t load_info;
extern long avenrun[3], mach_factor[3];
if (*count < HOST_LOAD_INFO_COUNT)
return KERN_FAILURE;
load_info = (host_load_info_t) info;
memcpy(load_info->avenrun,
avenrun,
sizeof avenrun);
memcpy(load_info->mach_factor,
mach_factor,
sizeof mach_factor);
*count = HOST_LOAD_INFO_COUNT;
return KERN_SUCCESS;
}
default:
return KERN_INVALID_ARGUMENT;
}
}
kern_return_t host_get_kernel_version(
const host_t		host,
kernel_version_t	out_version)
{
extern char	version[];
if (host == HOST_NULL)
return KERN_INVALID_ARGUMENT;
(void) strncpy(out_version, version, sizeof(kernel_version_t));
return KERN_SUCCESS;
}
#if defined(__i386__) || (defined(__x86_64__) && defined(USER32))
kern_return_t host_kernel_version(
const host_t		host,
kernel_version_t	out_version)
{
return host_get_kernel_version(host, out_version);
}
#endif
#if	MACH_HOST
kern_return_t
host_processor_sets(
const host_t			host,
processor_set_name_array_t	*pset_list,
natural_t			*count)
{
unsigned int actual;
processor_set_t pset;
processor_set_t *psets;
int i;
vm_size_t size;
vm_size_t size_needed;
vm_offset_t addr;
if (host == HOST_NULL)
return KERN_INVALID_ARGUMENT;
size = 0; addr = 0;
for (;;) {
simple_lock(&all_psets_lock);
actual = all_psets_count;
size_needed = actual * sizeof(mach_port_t);
if (size_needed <= size)
break;
simple_unlock(&all_psets_lock);
if (size != 0)
kfree(addr, size);
assert(size_needed > 0);
size = size_needed;
addr = kalloc(size);
if (addr == 0)
return KERN_RESOURCE_SHORTAGE;
}
psets = (processor_set_t *) addr;
for (i = 0, pset = (processor_set_t) queue_first(&all_psets);
i < actual;
i++, pset = (processor_set_t) queue_next(&pset->all_psets)) {
pset_reference(pset);
psets[i] = pset;
}
assert(queue_end(&all_psets, (queue_entry_t) pset));
simple_unlock(&all_psets_lock);
assert(actual > 0);
if (size_needed < size) {
vm_offset_t newaddr;
newaddr = kalloc(size_needed);
if (newaddr == 0) {
for (i = 0; i < actual; i++)
pset_deallocate(psets[i]);
kfree(addr, size);
return KERN_RESOURCE_SHORTAGE;
}
memcpy((void *) newaddr, (void *) addr, size_needed);
kfree(addr, size);
psets = (processor_set_t *) newaddr;
}
*pset_list = (mach_port_t *) psets;
*count = actual;
for (i = 0; i < actual; i++)
((mach_port_t *) psets)[i] =
(mach_port_t)convert_pset_name_to_port(psets[i]);
return KERN_SUCCESS;
}
#else
kern_return_t
host_processor_sets(
const host_t			host,
processor_set_name_array_t	*pset_list,
natural_t			*count)
{
vm_offset_t addr;
if (host == HOST_NULL)
return KERN_INVALID_ARGUMENT;
addr = kalloc((vm_size_t) sizeof(mach_port_t));
if (addr == 0)
return KERN_RESOURCE_SHORTAGE;
pset_reference(&default_pset);
mach_port_t *port_list = (mach_port_t *) addr;
port_list[0] = (mach_port_t) convert_pset_name_to_port(&default_pset);
*pset_list = port_list;
*count = 1;
return KERN_SUCCESS;
}
#endif
kern_return_t
host_processor_set_priv(
const host_t	host,
processor_set_t	pset_name,
processor_set_t	*pset)
{
if ((host == HOST_NULL) || (pset_name == PROCESSOR_SET_NULL)) {
*pset = PROCESSOR_SET_NULL;
return KERN_INVALID_ARGUMENT;
}
*pset = pset_name;
pset_reference(*pset);
return KERN_SUCCESS;
}