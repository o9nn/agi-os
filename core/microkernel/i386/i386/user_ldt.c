#include <string.h>
#include <kern/kalloc.h>
#include <kern/thread.h>
#include <vm/vm_kern.h>
#include <i386/pcb.h>
#include <i386/seg.h>
#include <i386/thread.h>
#include <i386/user_ldt.h>
#include <i386/constants.h>
#include <i386/i386/mach_i386.server.h>
#include <stddef.h>
#include "ldt.h"
#include "vm_param.h"
kern_return_t
i386_set_ldt(
thread_t thread,
int first_selector,
const struct descriptor *descriptor_list,
unsigned int count,
boolean_t desc_list_inline)
{
struct real_descriptor* desc_list = (struct real_descriptor *)descriptor_list;
user_ldt_t new_ldt, old_ldt, temp;
struct real_descriptor *dp;
unsigned i;
unsigned min_selector = 0;
pcb_t pcb;
vm_size_t ldt_size_needed;
unsigned first_desc = sel_idx(first_selector);
vm_map_copy_t old_copy_object = NULL;
if (thread == THREAD_NULL)
return KERN_INVALID_ARGUMENT;
if (thread == current_thread())
min_selector = LDTSZ;
if (first_desc < min_selector || first_desc > 8191)
return KERN_INVALID_ARGUMENT;
if (first_desc + count >= 8192)
return KERN_INVALID_ARGUMENT;
if (!desc_list_inline) {
kern_return_t kr;
vm_offset_t dst_addr;
old_copy_object = (vm_map_copy_t) desc_list;
kr = vm_map_copyout(ipc_kernel_map, &dst_addr,
vm_map_copy_copy(old_copy_object));
if (kr != KERN_SUCCESS)
return kr;
(void) vm_map_pageable(ipc_kernel_map,
dst_addr,
dst_addr + count * sizeof(struct real_descriptor),
VM_PROT_READ|VM_PROT_WRITE, TRUE, TRUE);
desc_list = (struct real_descriptor *)dst_addr;
}
for (i = 0, dp = desc_list;
i < count;
i++, dp++)
{
switch (dp->access & ~ACC_A) {
case 0:
case ACC_P:
break;
case ACC_P | ACC_CALL_GATE:
*dp = *(struct real_descriptor *)
&ldt[sel_idx(USER_SCALL)];
break;
case ACC_P | ACC_PL_U | ACC_DATA:
case ACC_P | ACC_PL_U | ACC_DATA_W:
case ACC_P | ACC_PL_U | ACC_DATA_E:
case ACC_P | ACC_PL_U | ACC_DATA_EW:
case ACC_P | ACC_PL_U | ACC_CODE:
case ACC_P | ACC_PL_U | ACC_CODE_R:
case ACC_P | ACC_PL_U | ACC_CODE_C:
case ACC_P | ACC_PL_U | ACC_CODE_CR:
case ACC_P | ACC_PL_U | ACC_CALL_GATE_16:
case ACC_P | ACC_PL_U | ACC_CALL_GATE:
break;
default:
return KERN_INVALID_ARGUMENT;
}
}
ldt_size_needed = sizeof(struct real_descriptor)
* (first_desc + count);
pcb = thread->pcb;
new_ldt = 0;
Retry:
simple_lock(&pcb->lock);
old_ldt = pcb->ims.ldt;
if (old_ldt == 0 ||
old_ldt->desc.limit_low + 1 < ldt_size_needed)
{
if (new_ldt == 0) {
simple_unlock(&pcb->lock);
#ifdef MACH_PV_DESCRIPTORS
vm_offset_t alloc = kalloc(ldt_size_needed + PAGE_SIZE + offsetof(struct user_ldt, ldt));
new_ldt = (user_ldt_t) (round_page((alloc + offsetof(struct user_ldt, ldt))) - offsetof(struct user_ldt, ldt));
new_ldt->alloc = alloc;
#else
new_ldt = (user_ldt_t)
kalloc(ldt_size_needed
+ sizeof(struct real_descriptor));
#endif
{
vm_offset_t ldt_base;
ldt_base = kvtolin(&new_ldt->ldt[0]);
new_ldt->desc.limit_low = ldt_size_needed - 1;
new_ldt->desc.limit_high = 0;
new_ldt->desc.base_low = ldt_base & WORD_MASK;
new_ldt->desc.base_med = (ldt_base >> 16) & BYTE_MASK;
new_ldt->desc.base_high = ldt_base >> 24;
new_ldt->desc.access = ACC_P | ACC_LDT;
new_ldt->desc.granularity = 0;
}
goto Retry;
}
if (old_ldt) {
memcpy(&new_ldt->ldt[0],
&old_ldt->ldt[0],
old_ldt->desc.limit_low + 1);
}
else {
struct real_descriptor template = {0, 0, 0, ACC_P, 0, 0 ,0};
for (dp = &new_ldt->ldt[0], i = 0; i < first_desc; i++, dp++) {
if (i < LDTSZ)
*dp = *(struct real_descriptor *) &ldt[i];
else
*dp = template;
}
}
temp = old_ldt;
old_ldt = new_ldt;
new_ldt = temp;
pcb->ims.ldt = old_ldt;
if (thread == current_thread())
switch_ktss(pcb);
}
memcpy(&old_ldt->ldt[first_desc],
desc_list,
count * sizeof(struct real_descriptor));
simple_unlock(&pcb->lock);
if (new_ldt)
#ifdef MACH_PV_DESCRIPTORS
{
#ifdef MACH_PV_PAGETABLES
for (i=0; i<(new_ldt->desc.limit_low + 1)/sizeof(struct real_descriptor); i+=PAGE_SIZE/sizeof(struct real_descriptor))
pmap_set_page_readwrite(&new_ldt->ldt[i]);
#endif
kfree(new_ldt->alloc, new_ldt->desc.limit_low + 1
+ PAGE_SIZE + offsetof(struct user_ldt, ldt));
}
#else
kfree((vm_offset_t)new_ldt,
new_ldt->desc.limit_low + 1
+ sizeof(struct real_descriptor));
#endif
if (!desc_list_inline) {
(void) kmem_free(ipc_kernel_map,
(vm_offset_t) desc_list,
count * sizeof(struct real_descriptor));
vm_map_copy_discard(old_copy_object);
}
return KERN_SUCCESS;
}
kern_return_t
i386_get_ldt(const thread_t thread,
int first_selector,
int selector_count,
struct descriptor **descriptor_list,
unsigned int *count
)
{
struct real_descriptor** desc_list = (struct real_descriptor **)descriptor_list;
struct user_ldt *user_ldt;
pcb_t pcb;
int first_desc = sel_idx(first_selector);
unsigned ldt_count;
vm_size_t ldt_size;
vm_size_t size, size_needed;
vm_offset_t addr;
if (thread == THREAD_NULL)
return KERN_INVALID_ARGUMENT;
if (first_desc < 0 || first_desc > 8191)
return KERN_INVALID_ARGUMENT;
if (first_desc + selector_count >= 8192)
return KERN_INVALID_ARGUMENT;
pcb = thread->pcb;
addr = 0;
size = 0;
for (;;) {
simple_lock(&pcb->lock);
user_ldt = pcb->ims.ldt;
if (user_ldt == 0) {
simple_unlock(&pcb->lock);
if (addr)
kmem_free(ipc_kernel_map, addr, size);
*count = 0;
return KERN_SUCCESS;
}
ldt_count = (user_ldt->desc.limit_low + 1) /
sizeof (struct real_descriptor);
ldt_count -= first_desc;
if (ldt_count > selector_count)
ldt_count = selector_count;
ldt_size = ldt_count * sizeof(struct real_descriptor);
if (ldt_count <= *count)
break;
size_needed = round_page(ldt_size);
if (size_needed <= size)
break;
simple_unlock(&pcb->lock);
if (size != 0)
kmem_free(ipc_kernel_map, addr, size);
size = size_needed;
if (kmem_alloc(ipc_kernel_map, &addr, size)
!= KERN_SUCCESS)
return KERN_RESOURCE_SHORTAGE;
}
memcpy(*desc_list,
&user_ldt->ldt[first_desc],
ldt_size);
*count = ldt_count;
simple_unlock(&pcb->lock);
if (addr) {
vm_size_t size_used, size_left;
vm_map_copy_t memory;
size_used = round_page(ldt_size);
if (size_used != size)
kmem_free(ipc_kernel_map,
addr + size_used, size - size_used);
size_left = size_used - ldt_size;
if (size_left > 0)
memset((char *)addr + ldt_size, 0, size_left);
(void) vm_map_copyin(ipc_kernel_map, addr, size_used,
TRUE, &memory);
*desc_list = (struct real_descriptor *)memory;
}
return KERN_SUCCESS;
}
void
user_ldt_free(user_ldt_t user_ldt)
{
#ifdef MACH_PV_DESCRIPTORS
unsigned i;
#ifdef MACH_PV_PAGETABLES
for (i=0; i<(user_ldt->desc.limit_low + 1)/sizeof(struct real_descriptor); i+=PAGE_SIZE/sizeof(struct real_descriptor))
pmap_set_page_readwrite(&user_ldt->ldt[i]);
#endif
kfree(user_ldt->alloc, user_ldt->desc.limit_low + 1
+ PAGE_SIZE + offsetof(struct user_ldt, ldt));
#else
kfree((vm_offset_t)user_ldt,
user_ldt->desc.limit_low + 1
+ sizeof(struct real_descriptor));
#endif
}
kern_return_t
i386_set_gdt (thread_t thread, int *selector, struct descriptor descriptor)
{
const struct real_descriptor *desc = (struct real_descriptor *)&descriptor;
int idx;
if (thread == THREAD_NULL)
return KERN_INVALID_ARGUMENT;
if (*selector == -1)
{
for (idx = 0; idx < USER_GDT_SLOTS; ++idx)
if ((thread->pcb->ims.user_gdt[idx].access & ACC_P) == 0)
{
*selector = ((idx + sel_idx(USER_GDT)) << 3) | SEL_PL_U;
break;
}
if (idx == USER_GDT_SLOTS)
return KERN_NO_SPACE;
}
else if ((*selector & (SEL_LDT|SEL_PL)) != SEL_PL_U
|| sel_idx (*selector) < sel_idx(USER_GDT)
|| sel_idx (*selector) >= sel_idx(USER_GDT) + USER_GDT_SLOTS)
return KERN_INVALID_ARGUMENT;
else
idx = sel_idx (*selector) - sel_idx(USER_GDT);
if ((desc->access & ACC_P) == 0)
memset (&thread->pcb->ims.user_gdt[idx], 0,
sizeof thread->pcb->ims.user_gdt[idx]);
else if ((desc->access & (ACC_TYPE_USER|ACC_PL)) != (ACC_TYPE_USER|ACC_PL_U) || (desc->granularity & SZ_64))
return KERN_INVALID_ARGUMENT;
else
memcpy (&thread->pcb->ims.user_gdt[idx], desc, sizeof (struct descriptor));
if (thread == current_thread())
switch_ktss(thread->pcb);
return KERN_SUCCESS;
}
kern_return_t
i386_get_gdt (const thread_t thread, int selector, struct descriptor *descriptor)
{
struct real_descriptor *desc = (struct real_descriptor *)descriptor;
if (thread == THREAD_NULL)
return KERN_INVALID_ARGUMENT;
if ((selector & (SEL_LDT|SEL_PL)) != SEL_PL_U
|| sel_idx (selector) < sel_idx(USER_GDT)
|| sel_idx (selector) >= sel_idx(USER_GDT) + USER_GDT_SLOTS)
return KERN_INVALID_ARGUMENT;
*desc = thread->pcb->ims.user_gdt[sel_idx (selector) - sel_idx(USER_GDT)];
return KERN_SUCCESS;
}