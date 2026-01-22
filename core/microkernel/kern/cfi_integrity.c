#include "cfi_integrity.h"
#include <mach/mach_security.h>
#include "security_monitor.h"
#include <kern/printf.h>
#include <mach/machine.h>
#include <string.h>
extern void clock_get_uptime(time_value_t *);
#define CFI_MAX_CALL_DEPTH      64
#define CFI_VALID_CODE_START    0x100000
#define CFI_VALID_CODE_END      0x800000
static struct cfi_call_stack {
uintptr_t addresses[CFI_MAX_CALL_DEPTH];
int depth;
} cfi_call_stack;
static struct cfi_function_table {
uintptr_t *entries;
int count;
int capacity;
} function_table;
void
cfi_init_context(struct cfi_context *ctx, uintptr_t stack_base, uintptr_t stack_limit)
{
if (!ctx) {
return;
}
ctx->expected_return = 0;
ctx->call_site = 0;
ctx->stack_base = stack_base;
ctx->stack_limit = stack_limit;
ctx->magic = CFI_RETURN_MAGIC;
}
cfi_result_t
cfi_validate_return(uintptr_t return_addr, uintptr_t expected)
{
if (return_addr < CFI_VALID_CODE_START || return_addr > CFI_VALID_CODE_END) {
security_event_log(SEC_EVENT_CFI_VIOLATION, return_addr, "invalid_return_region");
return CFI_INVALID_RETURN_ADDR;
}
if (expected != 0 && return_addr != expected) {
security_event_log(SEC_EVENT_CFI_VIOLATION, return_addr, "return_mismatch");
return CFI_INVALID_RETURN_ADDR;
}
if ((return_addr & 0x3) != 0) {
security_event_log(SEC_EVENT_CFI_VIOLATION, return_addr, "unaligned_return");
return CFI_INVALID_RETURN_ADDR;
}
return CFI_VALID;
}
cfi_result_t
cfi_validate_call_target(uintptr_t target)
{
if (target < CFI_VALID_CODE_START || target > CFI_VALID_CODE_END) {
security_event_log(SEC_EVENT_CFI_VIOLATION, target, "invalid_call_target");
return CFI_INVALID_CALL_TARGET;
}
if ((target & 0x3) != 0) {
security_event_log(SEC_EVENT_CFI_VIOLATION, target, "unaligned_call");
return CFI_INVALID_CALL_TARGET;
}
if (function_table.entries && function_table.count > 0) {
for (int i = 0; i < function_table.count; i++) {
if (function_table.entries[i] == target) {
return CFI_VALID;
}
}
security_event_log(SEC_EVENT_CFI_VIOLATION, target, "unlisted_function");
return CFI_INVALID_CALL_TARGET;
}
return CFI_VALID;
}
cfi_result_t
cfi_check_stack_integrity(struct cfi_context *ctx)
{
uintptr_t current_sp;
if (!ctx || ctx->magic != CFI_RETURN_MAGIC) {
return CFI_STACK_CORRUPTION;
}
#if defined(__x86_64__)
asm volatile("movq %%rsp, %0" : "=r"(current_sp));
#elif defined(__i386__)
asm volatile("movl %%esp, %0" : "=r"(current_sp));
#else
#error "Unsupported architecture for stack pointer read"
#endif
if (current_sp < ctx->stack_limit || current_sp > ctx->stack_base) {
security_event_log(SEC_EVENT_STACK_SMASH, current_sp, "stack_bounds");
return CFI_STACK_CORRUPTION;
}
if (current_sp < ctx->stack_limit + 1024) {
security_event_log(SEC_EVENT_STACK_SMASH, current_sp, "stack_overflow");
return CFI_BUFFER_OVERFLOW;
}
return CFI_VALID;
}
static void
cfi_push_call(uintptr_t return_addr)
{
if (cfi_call_stack.depth < CFI_MAX_CALL_DEPTH) {
cfi_call_stack.addresses[cfi_call_stack.depth] = return_addr;
cfi_call_stack.depth++;
} else {
security_event_log(SEC_EVENT_CFI_VIOLATION, return_addr, "call_stack_full");
}
}
static uintptr_t
cfi_pop_call(void)
{
if (cfi_call_stack.depth > 0) {
cfi_call_stack.depth--;
return cfi_call_stack.addresses[cfi_call_stack.depth];
}
return 0;
}
cfi_result_t
cfi_protected_call(uintptr_t target, uintptr_t return_site)
{
cfi_result_t result;
result = cfi_validate_call_target(target);
if (result != CFI_VALID) {
return result;
}
cfi_push_call(return_site);
return CFI_VALID;
}
cfi_result_t
cfi_protected_return(uintptr_t return_addr)
{
uintptr_t expected_return;
cfi_result_t result;
expected_return = cfi_pop_call();
result = cfi_validate_return(return_addr, expected_return);
if (result != CFI_VALID) {
return result;
}
return CFI_VALID;
}
void
cfi_init(void)
{
memset(&cfi_call_stack, 0, sizeof(cfi_call_stack));
function_table.entries = NULL;
function_table.count = 0;
function_table.capacity = 0;
printf("CFI integrity checking initialized\n");
}
kern_return_t
cfi_add_function(uintptr_t entry_point)
{
return KERN_SUCCESS;
}
kern_return_t
cfi_remove_function(uintptr_t entry_point)
{
return KERN_SUCCESS;
}
int
cfi_get_call_depth(void)
{
return cfi_call_stack.depth;
}
void
cfi_dump_call_stack(void)
{
int i;
printf("CFI Call Stack (depth %d):\n", cfi_call_stack.depth);
for (i = cfi_call_stack.depth - 1; i >= 0; i--) {
printf("  %d: 0x%x\n", i, (unsigned int)cfi_call_stack.addresses[i]);
}
}