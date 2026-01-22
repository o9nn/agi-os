#include <mach/valgrind.h>
#include <kern/printf.h>
#include <kern/assert.h>
#include <kern/lock.h>
#include <kern/mem_track.h>
#include <mach/kern_return.h>
#include <string.h>
#if CONFIG_VALGRIND_SUPPORT
static boolean_t valgrind_enabled = FALSE;
static simple_lock_data_t valgrind_lock;
static uint32_t next_alloc_id = 1;
static struct valgrind_mem_record memory_records[VALGRIND_MAX_RECORDS];
static uint32_t record_count = 0;
static uint32_t record_index = 0;
static uint32_t total_allocations = 0;
static uint32_t total_frees = 0;
static uint32_t memory_errors_detected = 0;
static uint32_t client_requests = 0;
static vm_address_t get_caller_pc(void)
{
vm_address_t pc;
pc = (vm_address_t)__builtin_return_address(1);
return pc ? pc : (vm_address_t)__builtin_return_address(0);
}
static struct valgrind_mem_record *find_memory_record(vm_address_t addr)
{
uint32_t i;
for (i = 0; i < record_count; i++) {
struct valgrind_mem_record *rec = &memory_records[i];
if (addr >= rec->start_addr &&
addr < rec->start_addr + rec->size) {
return rec;
}
}
return NULL;
}
static kern_return_t add_memory_record(vm_address_t addr, vm_size_t size,
valgrind_mem_state_t state,
vm_address_t caller_pc)
{
struct valgrind_mem_record *rec;
rec = find_memory_record(addr);
if (!rec) {
if (record_count < VALGRIND_MAX_RECORDS) {
rec = &memory_records[record_count++];
} else {
rec = &memory_records[record_index];
record_index = (record_index + 1) % VALGRIND_MAX_RECORDS;
}
}
rec->start_addr = addr;
rec->size = size;
rec->state = state;
rec->alloc_id = next_alloc_id++;
rec->caller_pc = caller_pc;
rec->timestamp = 0;
return KERN_SUCCESS;
}
static kern_return_t remove_memory_record(vm_address_t addr)
{
struct valgrind_mem_record *rec = find_memory_record(addr);
if (rec) {
rec->state = VALGRIND_MEM_NOACCESS;
return KERN_SUCCESS;
}
return KERN_INVALID_ADDRESS;
}
kern_return_t valgrind_init(void)
{
simple_lock_init(&valgrind_lock);
memset(memory_records, 0, sizeof(memory_records));
record_count = 0;
record_index = 0;
next_alloc_id = 1;
total_allocations = 0;
total_frees = 0;
memory_errors_detected = 0;
client_requests = 0;
valgrind_enabled = FALSE;
printf("Valgrind support initialized\n");
return KERN_SUCCESS;
}
kern_return_t valgrind_enable(boolean_t enable)
{
simple_lock(&valgrind_lock);
valgrind_enabled = enable;
simple_unlock(&valgrind_lock);
printf("Valgrind support %s\n", enable ? "enabled" : "disabled");
return KERN_SUCCESS;
}
boolean_t valgrind_is_enabled(void)
{
return valgrind_enabled;
}
kern_return_t valgrind_track_alloc(vm_address_t addr, vm_size_t size,
vm_address_t caller_pc)
{
kern_return_t ret;
if (!valgrind_enabled)
return KERN_SUCCESS;
if (caller_pc == 0)
caller_pc = get_caller_pc();
simple_lock(&valgrind_lock);
ret = add_memory_record(addr, size, VALGRIND_MEM_ADDRESSABLE, caller_pc);
if (ret == KERN_SUCCESS) {
total_allocations++;
mem_track_alloc(MEM_TYPE_GENERAL, size);
}
simple_unlock(&valgrind_lock);
return ret;
}
kern_return_t valgrind_track_free(vm_address_t addr)
{
kern_return_t ret;
struct valgrind_mem_record *rec;
if (!valgrind_enabled)
return KERN_SUCCESS;
simple_lock(&valgrind_lock);
rec = find_memory_record(addr);
if (rec) {
mem_track_free(MEM_TYPE_GENERAL, rec->size);
ret = remove_memory_record(addr);
if (ret == KERN_SUCCESS) {
total_frees++;
}
} else {
ret = KERN_INVALID_ADDRESS;
memory_errors_detected++;
valgrind_report_error("Invalid free", addr, 0,
"Attempt to free untracked or already freed memory");
}
simple_unlock(&valgrind_lock);
return ret;
}
kern_return_t valgrind_track_realloc(vm_address_t old_addr, vm_address_t new_addr,
vm_size_t new_size)
{
kern_return_t ret;
vm_address_t caller_pc;
if (!valgrind_enabled)
return KERN_SUCCESS;
caller_pc = get_caller_pc();
if (old_addr != 0) {
ret = valgrind_track_free(old_addr);
if (ret != KERN_SUCCESS) {
return ret;
}
}
if (new_addr != 0 && new_size > 0) {
ret = valgrind_track_alloc(new_addr, new_size, caller_pc);
if (ret != KERN_SUCCESS) {
return ret;
}
}
return KERN_SUCCESS;
}
kern_return_t valgrind_make_mem_defined(vm_address_t addr, vm_size_t size)
{
struct valgrind_mem_record *rec;
if (!valgrind_enabled)
return KERN_SUCCESS;
simple_lock(&valgrind_lock);
rec = find_memory_record(addr);
if (rec && addr >= rec->start_addr &&
addr + size <= rec->start_addr + rec->size) {
rec->state = VALGRIND_MEM_DEFINED;
}
simple_unlock(&valgrind_lock);
return KERN_SUCCESS;
}
kern_return_t valgrind_make_mem_undefined(vm_address_t addr, vm_size_t size)
{
struct valgrind_mem_record *rec;
if (!valgrind_enabled)
return KERN_SUCCESS;
simple_lock(&valgrind_lock);
rec = find_memory_record(addr);
if (rec && addr >= rec->start_addr &&
addr + size <= rec->start_addr + rec->size) {
rec->state = VALGRIND_MEM_UNDEFINED;
}
simple_unlock(&valgrind_lock);
return KERN_SUCCESS;
}
kern_return_t valgrind_make_mem_noaccess(vm_address_t addr, vm_size_t size)
{
struct valgrind_mem_record *rec;
if (!valgrind_enabled)
return KERN_SUCCESS;
simple_lock(&valgrind_lock);
rec = find_memory_record(addr);
if (rec && addr >= rec->start_addr &&
addr + size <= rec->start_addr + rec->size) {
rec->state = VALGRIND_MEM_NOACCESS;
}
simple_unlock(&valgrind_lock);
return KERN_SUCCESS;
}
valgrind_mem_state_t valgrind_check_mem_state(vm_address_t addr)
{
struct valgrind_mem_record *rec;
valgrind_mem_state_t state = VALGRIND_MEM_NOACCESS;
if (!valgrind_enabled)
return VALGRIND_MEM_DEFINED;
simple_lock(&valgrind_lock);
rec = find_memory_record(addr);
if (rec) {
state = rec->state;
}
simple_unlock(&valgrind_lock);
return state;
}
boolean_t valgrind_is_mem_defined(vm_address_t addr, vm_size_t size)
{
vm_address_t end_addr = addr + size;
vm_address_t check_addr;
if (!valgrind_enabled)
return TRUE;
for (check_addr = addr; check_addr < end_addr; check_addr++) {
valgrind_mem_state_t state = valgrind_check_mem_state(check_addr);
if (state != VALGRIND_MEM_DEFINED) {
return FALSE;
}
}
return TRUE;
}
boolean_t valgrind_is_mem_addressable(vm_address_t addr, vm_size_t size)
{
vm_address_t end_addr = addr + size;
vm_address_t check_addr;
if (!valgrind_enabled)
return TRUE;
for (check_addr = addr; check_addr < end_addr; check_addr++) {
valgrind_mem_state_t state = valgrind_check_mem_state(check_addr);
if (state == VALGRIND_MEM_NOACCESS) {
return FALSE;
}
}
return TRUE;
}
void valgrind_report_error(const char *error_type, vm_address_t addr,
vm_size_t size, const char *description)
{
simple_lock(&valgrind_lock);
memory_errors_detected++;
simple_unlock(&valgrind_lock);
printf("VALGRIND ERROR: %s at address 0x%lx (size %lu)\n",
error_type, (unsigned long)addr, (unsigned long)size);
printf("  Description: %s\n", description);
valgrind_print_backtrace(get_caller_pc());
}
void valgrind_print_backtrace(vm_address_t pc)
{
printf("  at 0x%lx\n", (unsigned long)pc);
}
kern_return_t valgrind_handle_client_request(uint32_t request,
vm_address_t arg1, vm_address_t arg2,
vm_address_t arg3, vm_address_t arg4,
vm_address_t arg5)
{
kern_return_t ret = KERN_SUCCESS;
if (!valgrind_enabled)
return KERN_INVALID_ARGUMENT;
simple_lock(&valgrind_lock);
client_requests++;
simple_unlock(&valgrind_lock);
switch (request) {
case VG_USERREQ_MAKE_MEM_NOACCESS:
ret = valgrind_make_mem_noaccess(arg1, arg2);
break;
case VG_USERREQ_MAKE_MEM_UNDEFINED:
ret = valgrind_make_mem_undefined(arg1, arg2);
break;
case VG_USERREQ_MAKE_MEM_DEFINED:
ret = valgrind_make_mem_defined(arg1, arg2);
break;
case VG_USERREQ_CHECK_MEM_IS_ADDRESSABLE:
ret = valgrind_is_mem_addressable(arg1, arg2) ? KERN_SUCCESS : KERN_FAILURE;
break;
case VG_USERREQ_CHECK_MEM_IS_DEFINED:
ret = valgrind_is_mem_defined(arg1, arg2) ? KERN_SUCCESS : KERN_FAILURE;
break;
case VG_USERREQ_COUNT_ERRORS:
ret = (kern_return_t)memory_errors_detected;
break;
default:
ret = KERN_INVALID_ARGUMENT;
break;
}
return ret;
}
#else
kern_return_t valgrind_init(void)
{
return KERN_SUCCESS;
}
kern_return_t valgrind_enable(boolean_t enable)
{
return KERN_NOT_SUPPORTED;
}
boolean_t valgrind_is_enabled(void)
{
return FALSE;
}
kern_return_t valgrind_track_alloc(vm_address_t addr, vm_size_t size,
vm_address_t caller_pc)
{
return KERN_SUCCESS;
}
kern_return_t valgrind_track_free(vm_address_t addr)
{
return KERN_SUCCESS;
}
kern_return_t valgrind_track_realloc(vm_address_t old_addr, vm_address_t new_addr,
vm_size_t new_size)
{
return KERN_SUCCESS;
}
kern_return_t valgrind_make_mem_defined(vm_address_t addr, vm_size_t size)
{
return KERN_SUCCESS;
}
kern_return_t valgrind_make_mem_undefined(vm_address_t addr, vm_size_t size)
{
return KERN_SUCCESS;
}
kern_return_t valgrind_make_mem_noaccess(vm_address_t addr, vm_size_t size)
{
return KERN_SUCCESS;
}
valgrind_mem_state_t valgrind_check_mem_state(vm_address_t addr)
{
return VALGRIND_MEM_DEFINED;
}
boolean_t valgrind_is_mem_defined(vm_address_t addr, vm_size_t size)
{
return TRUE;
}
boolean_t valgrind_is_mem_addressable(vm_address_t addr, vm_size_t size)
{
return TRUE;
}
void valgrind_report_error(const char *error_type, vm_address_t addr,
vm_size_t size, const char *description)
{
}
void valgrind_print_backtrace(vm_address_t pc)
{
}
kern_return_t valgrind_handle_client_request(uint32_t request,
vm_address_t arg1, vm_address_t arg2,
vm_address_t arg3, vm_address_t arg4,
vm_address_t arg5)
{
return KERN_NOT_SUPPORTED;
}
#endif