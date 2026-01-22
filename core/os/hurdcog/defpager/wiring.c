error_t
__wrap___syscall_vm_allocate (task_t target_task,
vm_address_t *address,
vm_size_t size,
boolean_t anywhere)
{
error_t err;
err = __real___syscall_vm_allocate (target_task, address, size, anywhere);
if (!err && target_task == mach_task_self ())
wire_segment (*address, size);
return err;
}
error_t
__wrap___vm_allocate_rpc (task_t target_task,
vm_address_t *address,
vm_size_t size,
boolean_t anywhere)
{
error_t err;
err = __real___vm_allocate_rpc (target_task, address, size, anywhere);
if (!err && target_task == mach_task_self ())
wire_segment (*address, size);
return err;
}
error_t
__wrap___syscall_vm_map (mach_port_t target_task,
vm_address_t *address,
vm_size_t size,
vm_address_t mask,
boolean_t anywhere,
mach_port_t memory_object,
vm_offset_t offset,
boolean_t copy,
vm_prot_t cur_protection,
vm_prot_t max_protection,
vm_inherit_t inheritance)
{
error_t err;
err = __real___syscall_vm_map (target_task, address, size, mask, anywhere,
memory_object, offset, copy, cur_protection,
max_protection, inheritance);
if (!err && target_task == mach_task_self ())
wire_segment (*address, size);
return err;
}
error_t
__wrap___vm_map_rpc (mach_port_t target_task,
vm_address_t *address,
vm_size_t size,
vm_address_t mask,
boolean_t anywhere,
mach_port_t memory_object,
vm_offset_t offset,
boolean_t copy,
vm_prot_t cur_protection,
vm_prot_t max_protection,
vm_inherit_t inheritance)
{
error_t err;
err = __real___vm_map_rpc (target_task, address, size, mask, anywhere,
memory_object, offset, copy, cur_protection,
mak_protection, inheritance);
if (!err && target_task == mach_task_self ())
wire_segment (*address, size);
return err;
}
#define weak_alias(name,aliasname) \
extern typeof (name) aliasname __attribute__ ((weak, alias (#name)));
weak_alias (__wrap___vm_map_rpc, __wrap_vm_map_rpc)
weak_alias (__wrap___syscall_vm_map, __wrap_syscall_vm_map)
weak_alias (__wrap___vm_allocate_rpc, __wrap_vm_allocate_rpc)
weak_alias (__wrap___syscall_vm_allocate, __wrap_syscall_vm_allocate)