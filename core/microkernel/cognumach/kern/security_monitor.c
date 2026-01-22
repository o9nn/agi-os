#include "security_monitor.h"
#include <mach/mach_security.h>
#include <mach/mach_safety.h>
#include <kern/printf.h>
#include <kern/lock.h>
#include <mach/time_value.h>
#include <mach/machine.h>
#include <string.h>
extern void clock_get_uptime(time_value_t *);
boolean_t security_monitoring_enabled = FALSE;
static struct security_stats global_security_stats;
static decl_simple_lock_data(, security_stats_lock)
#define SECURITY_EVENT_BUFFER_SIZE 1024
static struct security_event {
security_event_t type;
uintptr_t address;
time_value_t timestamp;
char context[64];
} security_event_buffer[SECURITY_EVENT_BUFFER_SIZE];
static int security_event_head = 0;
static int security_event_count = 0;
static decl_simple_lock_data(, security_event_lock)
static uint32_t stack_canary_value = 0;
void
security_monitor_init(void)
{
simple_lock_init(&security_stats_lock);
simple_lock_init(&security_event_lock);
memset(&global_security_stats, 0, sizeof(global_security_stats));
stack_canary_init();
memset(security_event_buffer, 0, sizeof(security_event_buffer));
security_event_head = 0;
security_event_count = 0;
security_monitoring_enabled = TRUE;
printf("Security monitoring initialized\n");
}
void
security_event_log(security_event_t event, uintptr_t addr, const char *context)
{
struct security_event *entry;
time_value_t current_time;
if (!security_monitoring_enabled) {
return;
}
clock_get_uptime(&current_time);
simple_lock(&security_stats_lock);
switch (event) {
case SEC_EVENT_CFI_VIOLATION:
global_security_stats.cfi_violations++;
break;
case SEC_EVENT_BUFFER_OVERFLOW:
global_security_stats.buffer_overflows++;
break;
case SEC_EVENT_STACK_SMASH:
global_security_stats.stack_smashes++;
break;
case SEC_EVENT_ROP_ATTEMPT:
global_security_stats.rop_attempts++;
break;
case SEC_EVENT_PRIVILEGE_ESCALATION:
global_security_stats.privilege_escalations++;
break;
case SEC_EVENT_RESOURCE_EXHAUSTION:
global_security_stats.resource_exhaustions++;
break;
}
global_security_stats.total_events++;
global_security_stats.last_event_time = current_time.seconds;
simple_unlock(&security_stats_lock);
simple_lock(&security_event_lock);
entry = &security_event_buffer[security_event_head];
entry->type = event;
entry->address = addr;
entry->timestamp = current_time;
if (context) {
strncpy(entry->context, context, sizeof(entry->context) - 1);
entry->context[sizeof(entry->context) - 1] = '\0';
} else {
entry->context[0] = '\0';
}
security_event_head = (security_event_head + 1) % SECURITY_EVENT_BUFFER_SIZE;
if (security_event_count < SECURITY_EVENT_BUFFER_SIZE) {
security_event_count++;
}
simple_unlock(&security_event_lock);
if (event == SEC_EVENT_CFI_VIOLATION ||
event == SEC_EVENT_BUFFER_OVERFLOW ||
event == SEC_EVENT_STACK_SMASH) {
printf("SECURITY: Event %d at 0x%lx in %s\n", event, addr,
context ? context : "unknown");
}
}
kern_return_t
security_get_stats(struct security_stats *stats)
{
if (!stats) {
return KERN_INVALID_ARGUMENT;
}
simple_lock(&security_stats_lock);
*stats = global_security_stats;
simple_unlock(&security_stats_lock);
return KERN_SUCCESS;
}
void
security_reset_stats(void)
{
simple_lock(&security_stats_lock);
memset(&global_security_stats, 0, sizeof(global_security_stats));
simple_unlock(&security_stats_lock);
simple_lock(&security_event_lock);
security_event_head = 0;
security_event_count = 0;
simple_unlock(&security_event_lock);
}
boolean_t
security_detect_rop_chain(uintptr_t *addresses, int count)
{
int i;
int suspicious_patterns = 0;
if (!addresses || count < 3) {
return FALSE;
}
for (i = 0; i < count - 1; i++) {
uintptr_t current = addresses[i];
uintptr_t next = addresses[i + 1];
if (current > next && current - next < 32) {
suspicious_patterns++;
}
if ((current & 0x3) != 0) {
suspicious_patterns++;
}
}
return (suspicious_patterns > count / 2);
}
boolean_t
security_detect_stack_pivot(uintptr_t old_sp, uintptr_t new_sp)
{
uintptr_t diff;
if (new_sp > old_sp) {
diff = new_sp - old_sp;
} else {
diff = old_sp - new_sp;
}
return (diff > 0x100000) || (new_sp < 0x1000);
}
void
buffer_guard_init(buffer_guard_t *guard, void *buffer, size_t size)
{
if (!guard || !buffer || size == 0) {
return;
}
guard->buffer_start = (uintptr_t)buffer;
guard->buffer_size = size;
guard->canary_value = stack_canary_get();
if (size >= sizeof(uint32_t)) {
uint32_t *canary_location = (uint32_t *)((char *)buffer + size - sizeof(uint32_t));
*canary_location = guard->canary_value;
}
}
boolean_t
buffer_guard_check(buffer_guard_t *guard)
{
uint32_t *canary_location;
if (!guard || guard->buffer_size < sizeof(uint32_t)) {
return FALSE;
}
canary_location = (uint32_t *)((char *)guard->buffer_start +
guard->buffer_size - sizeof(uint32_t));
return (*canary_location == guard->canary_value);
}
kern_return_t
memory_safety_check(void *ptr, size_t size, int access_type)
{
uintptr_t addr = (uintptr_t)ptr;
if (!ptr) {
return KERN_INVALID_ADDRESS;
}
if (!MACH_VALIDATE_REGION(addr, size)) {
return KERN_INVALID_ARGUMENT;
}
if (addr < 0x1000) {
return KERN_INVALID_ADDRESS;
}
if (addr >= 0xC0000000 && access_type == 1 ) {
return KERN_PROTECTION_FAILURE;
}
return KERN_SUCCESS;
}
void
stack_canary_init(void)
{
time_value_t current_time;
clock_get_uptime(&current_time);
stack_canary_value = (uint32_t)(current_time.seconds ^ current_time.microseconds);
stack_canary_value ^= 0xDEADBEEF;
if (stack_canary_value == 0) {
stack_canary_value = 0xCAFEBABE;
}
}
uint32_t
stack_canary_get(void)
{
return stack_canary_value;
}
boolean_t
stack_canary_validate(uint32_t canary)
{
return (canary == stack_canary_value);
}