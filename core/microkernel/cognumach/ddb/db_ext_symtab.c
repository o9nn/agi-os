#if MACH_KDB
#if	MACH_DEBUG
#include <mach/mach_types.h>
#include <mach/std_types.h>
#include <mach/vm_param.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <vm/vm_user.h>
#include <kern/host.h>
#include <kern/mach_debug.server.h>
#include <kern/task.h>
#include <ddb/db_sym.h>
kern_return_t
host_load_symbol_table(
host_t		host,
task_t		task,
const char *	name,
pointer_t	symtab,
unsigned int	symtab_count)
{
kern_return_t	result;
vm_offset_t	symtab_start;
vm_offset_t	symtab_end;
vm_map_t	map;
vm_map_copy_t	symtab_copy_object;
if (host == HOST_NULL)
return (KERN_INVALID_ARGUMENT);
symtab_copy_object = (vm_map_copy_t) symtab;
result = vm_map_copyout(
kernel_map,
&symtab_start,
vm_map_copy_copy(symtab_copy_object));
if (result != KERN_SUCCESS)
return (result);
symtab_end = symtab_start + symtab_count;
if (task == TASK_NULL)
map = VM_MAP_NULL;
else
map = task->map;
if (!X_db_sym_init((char *)symtab_start,
(char *)symtab_end,
name,
(char *)map))
{
(void) vm_deallocate(kernel_map,
symtab_start,
symtab_count);
return (KERN_FAILURE);
}
(void) vm_map_pageable(kernel_map,
symtab_start,
round_page(symtab_end),
VM_PROT_READ|VM_PROT_WRITE,
TRUE, TRUE);
vm_map_copy_discard(symtab_copy_object);
return (KERN_SUCCESS);
}
#endif
#endif