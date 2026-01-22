#include <kern/vdso.h>
#include <kern/kalloc.h>
#include <kern/printf.h>
#include <kern/thread.h>
#include <kern/task.h>
#include <kern/sched_prim.h>
#include <mach/time_value.h>
#include <kern/mach_clock.h>
#include <vm/vm_kern.h>
#include <vm/vm_map.h>
#include <vm/vm_page.h>
#include <i386/i386/locore.h>
#include <string.h>
static vdso_state_t vdso_state = {
.vdso_page = 0,
.vdso_size = VDSO_PAGE_SIZE,
.initialized = FALSE,
.header = NULL
};
static vdso_symbol_t vdso_symbols_template[] = {
{ "gettimeofday", 0x100, TRUE },
{ "clock_gettime", 0x200, TRUE },
{ "time", 0x300, TRUE },
{ "getpid", 0x400, TRUE },
};
void
vdso_init(void)
{
kern_return_t result;
if (vdso_state.initialized) {
printf("VDSO: Already initialized\n");
return;
}
printf("VDSO: Initializing Virtual Dynamic Shared Object support\n");
result = vdso_bootstrap();
if (result != KERN_SUCCESS) {
printf("VDSO: Bootstrap failed with error %d\n", result);
return;
}
vdso_arch_init();
vdso_state.initialized = TRUE;
printf("VDSO: Initialization complete\n");
}
kern_return_t
vdso_bootstrap(void)
{
vm_offset_t page;
vdso_header_t *header;
int i;
if (kmem_alloc(kernel_map, &page, VDSO_PAGE_SIZE) != KERN_SUCCESS) {
printf("VDSO: Failed to allocate VDSO page\n");
return KERN_RESOURCE_SHORTAGE;
}
memset((void *)page, 0, VDSO_PAGE_SIZE);
header = (vdso_header_t *)page;
header->magic = VDSO_MAGIC;
header->version = VDSO_VERSION;
header->symbol_count = sizeof(vdso_symbols_template) / sizeof(vdso_symbol_t);
header->reserved = 0;
for (i = 0; i < header->symbol_count && i < VDSO_MAX_SYMBOLS; i++) {
header->symbols[i] = vdso_symbols_template[i];
}
vdso_state.vdso_page = page;
vdso_state.header = header;
return vdso_arch_setup_page(page);
}
kern_return_t
vdso_map_into_task(task_t task, vm_offset_t *address)
{
kern_return_t result;
vm_offset_t user_addr;
if (!vdso_state.initialized) {
return KERN_FAILURE;
}
if (!task || !address) {
return KERN_INVALID_ARGUMENT;
}
user_addr = VDSO_USER_BASE;
result = vm_map(task->map,
&user_addr,
VDSO_PAGE_SIZE,
0,
FALSE,
VM_OBJECT_NULL,
0,
FALSE,
VM_PROT_READ | VM_PROT_EXECUTE,
VM_PROT_READ | VM_PROT_EXECUTE,
VM_INHERIT_COPY);
if (result != KERN_SUCCESS) {
printf("VDSO: Failed to map VDSO into task (error %d)\n", result);
return result;
}
*address = user_addr;
return KERN_SUCCESS;
}
void
vdso_unmap_from_task(task_t task)
{
if (!task || !vdso_state.initialized) {
return;
}
vm_deallocate(task->map, VDSO_USER_BASE, VDSO_PAGE_SIZE);
}
vm_offset_t
vdso_lookup_symbol(vdso_symbol_type_t symbol)
{
vdso_header_t *header;
if (!vdso_state.initialized || symbol >= VDSO_SYM_COUNT) {
return 0;
}
header = vdso_state.header;
if (!header || symbol >= header->symbol_count) {
return 0;
}
if (!header->symbols[symbol].available) {
return 0;
}
return vdso_state.vdso_page + header->symbols[symbol].offset;
}
kern_return_t
vdso_add_symbol(vdso_symbol_type_t type, const char *name, vm_offset_t offset)
{
vdso_header_t *header;
if (!vdso_state.initialized || type >= VDSO_SYM_COUNT || !name) {
return KERN_INVALID_ARGUMENT;
}
header = vdso_state.header;
if (!header || type >= header->symbol_count) {
return KERN_INVALID_ARGUMENT;
}
header->symbols[type].offset = offset;
header->symbols[type].available = TRUE;
strncpy((char *)header->symbols[type].name, name, 31);
return KERN_SUCCESS;
}
int
vdso_gettimeofday(time_value_t *tv)
{
if (!tv) {
return -1;
}
tv->seconds = 1234567890;
tv->microseconds = 0;
return 0;
}
int
vdso_clock_gettime(int clock_id, time_value_t *tp)
{
if (!tp) {
return -1;
}
if (clock_id != 0) {
return -1;
}
tp->seconds = 1234567890;
tp->microseconds = 0;
return 0;
}
long
vdso_time(long *t)
{
long current_time = 1234567890;
if (t) {
*t = current_time;
}
return current_time;
}
int
vdso_getpid(void)
{
task_t task;
task = current_task();
if (!task) {
return -1;
}
return (int)(uintptr_t)task;
}
vdso_state_t *
vdso_get_state(void)
{
return &vdso_state;
}
boolean_t
vdso_is_available(void)
{
return vdso_state.initialized;
}
__attribute__((weak)) void
vdso_arch_init(void)
{
printf("VDSO: Using default architecture initialization\n");
}
__attribute__((weak)) kern_return_t
vdso_arch_setup_page(vm_offset_t page)
{
printf("VDSO: Using default page setup\n");
return KERN_SUCCESS;
}