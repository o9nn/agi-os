#include <string.h>
#include <mach/error.h>
#include <mach/vm_param.h>
#include <kern/syscall_emulation.h>
#include <kern/task.h>
#include <kern/kalloc.h>
#include <kern/mach.server.h>
#include <vm/vm_kern.h>
#define syscall_emulation_sync(task)
#define	base_size	(sizeof(struct eml_dispatch) - sizeof(eml_routine_t))
#define	count_to_size(count) \
(base_size + sizeof(vm_offset_t) * (count))
#define	size_to_count(size) \
( ((size) - base_size) / sizeof(vm_offset_t) )
void eml_init(void)
{
}
void eml_task_reference(
task_t	task,
task_t	parent)
{
eml_dispatch_t	eml;
if (parent == TASK_NULL)
eml = EML_DISPATCH_NULL;
else
eml = parent->eml_dispatch;
if (eml != EML_DISPATCH_NULL) {
simple_lock(&eml->lock);
eml->ref_count++;
simple_unlock(&eml->lock);
}
task->eml_dispatch = eml;
}
void eml_task_deallocate(const task_t task)
{
eml_dispatch_t	eml;
eml = task->eml_dispatch;
if (eml != EML_DISPATCH_NULL) {
int count;
simple_lock(&eml->lock);
count = --eml->ref_count;
simple_unlock(&eml->lock);
if (count == 0)
kfree((vm_offset_t)eml, count_to_size(eml->disp_count));
}
}
static kern_return_t
task_set_emulation_vector_internal(
task_t 			task,
int			vector_start,
emulation_vector_t	emulation_vector,
unsigned int		emulation_vector_count)
{
eml_dispatch_t	cur_eml, new_eml, old_eml;
vm_size_t	new_size;
int		cur_start, cur_end;
int		new_start = 0, new_end = 0;
int		vector_end;
if (task == TASK_NULL)
return EML_BAD_TASK;
vector_end = vector_start + emulation_vector_count;
old_eml = EML_DISPATCH_NULL;
new_eml = EML_DISPATCH_NULL;
for (;;) {
task_lock(task);
cur_eml = task->eml_dispatch;
if (cur_eml != EML_DISPATCH_NULL) {
cur_start = cur_eml->disp_min;
cur_end   = cur_eml->disp_count + cur_start;
simple_lock(&cur_eml->lock);
if (cur_eml->ref_count == 1 &&
cur_start <= vector_start &&
cur_end >= vector_end)
{
simple_unlock(&cur_eml->lock);
old_eml = new_eml;
break;
}
if (new_eml != EML_DISPATCH_NULL &&
new_start <= cur_start &&
new_end >= cur_end)
{
memcpy(&new_eml->disp_vector[cur_start-new_start],
&cur_eml->disp_vector[0],
cur_eml->disp_count * sizeof(vm_offset_t));
if (--cur_eml->ref_count == 0)
old_eml = cur_eml;
simple_unlock(&cur_eml->lock);
task->eml_dispatch = new_eml;
syscall_emulation_sync(task);
cur_eml = new_eml;
break;
}
simple_unlock(&cur_eml->lock);
new_start = vector_start;
if (new_start > cur_start)
new_start = cur_start;
new_end = vector_end;
if (new_end < cur_end)
new_end = cur_end;
}
else {
if (new_eml != EML_DISPATCH_NULL) {
task->eml_dispatch = new_eml;
cur_eml = new_eml;
break;
}
new_start = vector_start;
new_end = vector_end;
}
task_unlock(task);
if (new_eml != EML_DISPATCH_NULL)
kfree((vm_offset_t)new_eml, count_to_size(new_eml->disp_count));
new_size = count_to_size(new_end - new_start);
new_eml = (eml_dispatch_t) kalloc(new_size);
memset(new_eml, 0, new_size);
simple_lock_init(&new_eml->lock);
new_eml->ref_count = 1;
new_eml->disp_min   = new_start;
new_eml->disp_count = new_end - new_start;
continue;
}
memcpy(&cur_eml->disp_vector[vector_start - cur_eml->disp_min],
&emulation_vector[0],
emulation_vector_count * sizeof(vm_offset_t));
task_unlock(task);
if (old_eml)
kfree((vm_offset_t) old_eml, count_to_size(old_eml->disp_count));
return KERN_SUCCESS;
}
kern_return_t
task_set_emulation_vector(
task_t 			task,
int			vector_start,
emulation_vector_t	emulation_vector,
unsigned int		emulation_vector_count)
{
kern_return_t		kr;
vm_offset_t		emul_vector_addr;
if (task == TASK_NULL)
return EML_BAD_TASK;
kr = vm_map_copyout(ipc_kernel_map, &emul_vector_addr,
(vm_map_copy_t) emulation_vector);
if (kr != KERN_SUCCESS)
return kr;
kr = task_set_emulation_vector_internal(
task,
vector_start,
(emulation_vector_t) emul_vector_addr,
emulation_vector_count);
(void) kmem_free(ipc_kernel_map,
emul_vector_addr,
emulation_vector_count * sizeof(eml_dispatch_t));
return kr;
}
kern_return_t
task_get_emulation_vector(
task_t			task,
int			*vector_start,
emulation_vector_t	*emulation_vector,
unsigned int		*emulation_vector_count)
{
eml_dispatch_t		eml;
vm_size_t		vector_size, size;
vm_offset_t		addr;
if (task == TASK_NULL)
return EML_BAD_TASK;
addr = 0;
size = 0;
for(;;) {
vm_size_t	size_needed;
task_lock(task);
eml = task->eml_dispatch;
if (eml == EML_DISPATCH_NULL) {
task_unlock(task);
if (addr)
(void) kmem_free(ipc_kernel_map, addr, size);
*vector_start = 0;
*emulation_vector = 0;
*emulation_vector_count = 0;
return KERN_SUCCESS;
}
vector_size = eml->disp_count * sizeof(vm_offset_t);
size_needed = round_page(vector_size);
if (size_needed <= size)
break;
task_unlock(task);
if (size != 0)
kmem_free(ipc_kernel_map, addr, size);
size = size_needed;
if (kmem_alloc(ipc_kernel_map, &addr, size) != KERN_SUCCESS)
return KERN_RESOURCE_SHORTAGE;
}
*vector_start = eml->disp_min;
*emulation_vector_count = eml->disp_count;
memcpy((void *)addr,
eml->disp_vector,
vector_size);
task_unlock(task);
{
vm_size_t	size_used, size_left;
vm_map_copy_t	memory;
size_used = round_page(vector_size);
if (size_used != size)
(void) kmem_free(ipc_kernel_map,
addr + size_used,
size - size_used);
size_left = size_used - vector_size;
if (size_left > 0)
memset((char *)addr + vector_size, 0, size_left);
(void) vm_map_copyin(ipc_kernel_map, addr, vector_size, TRUE, &memory);
*emulation_vector = (emulation_vector_t) memory;
}
return KERN_SUCCESS;
}
kern_return_t task_set_emulation(
task_t		task,
vm_offset_t 	routine_entry_pt,
int		routine_number)
{
return task_set_emulation_vector_internal(task, routine_number,
&routine_entry_pt, 1);
}