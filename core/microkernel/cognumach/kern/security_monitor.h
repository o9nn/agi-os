#ifndef _KERN_SECURITY_MONITOR_H_
#define _KERN_SECURITY_MONITOR_H_
#include <mach/mach_security.h>
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/time_value.h>
extern void security_monitor_init(void);
extern void security_event_log(security_event_t event, uintptr_t addr, const char *context);
extern kern_return_t security_get_stats(struct security_stats *stats);
extern void security_reset_stats(void);
extern boolean_t security_detect_rop_chain(uintptr_t *addresses, int count);
extern boolean_t security_detect_stack_pivot(uintptr_t old_sp, uintptr_t new_sp);
extern void buffer_guard_init(buffer_guard_t *guard, void *buffer, size_t size);
extern boolean_t buffer_guard_check(buffer_guard_t *guard);
extern kern_return_t memory_safety_check(void *ptr, size_t size, int access_type);
extern void stack_canary_init(void);
extern uint32_t stack_canary_get(void);
extern boolean_t stack_canary_validate(uint32_t canary);
extern boolean_t security_monitoring_enabled;
#endif