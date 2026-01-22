#ifndef _KERN_NEW_FEATURE_H_
#define _KERN_NEW_FEATURE_H_
#include <kern/kern_types.h>
#include <kern/lock.h>
#include <mach/kern_return.h>
typedef enum {
FEATURE_STATE_DISABLED = 0,
FEATURE_STATE_INITIALIZING,
FEATURE_STATE_ENABLED,
FEATURE_STATE_ERROR,
FEATURE_STATE_MAX
} feature_state_t;
#define FEATURE_CAP_MEMORY_EFFICIENT 0x01
#define FEATURE_CAP_LOW_LATENCY 0x02
#define FEATURE_CAP_X86_SUPPORT 0x04
#define FEATURE_CAP_X86_64_SUPPORT 0x08
#define FEATURE_CAP_QEMU_SUPPORT 0x10
struct kernel_feature {
feature_state_t state;
uint32_t capabilities;
uint32_t enabled_count;
uint32_t error_count;
uint64_t last_operation_time;
simple_lock_data_t lock;
};
struct feature_stats {
uint64_t init_calls;
uint64_t enable_calls;
uint64_t disable_calls;
uint64_t total_operations;
uint64_t avg_latency_us;
uint64_t max_latency_us;
};
extern struct kernel_feature global_kernel_feature;
extern struct feature_stats global_feature_stats;
void feature_init(void);
kern_return_t feature_enable(void);
kern_return_t feature_disable(void);
feature_state_t feature_get_state(void);
struct feature_stats *feature_get_stats(void);
boolean_t feature_is_enabled(void);
void feature_reset_stats(void);
#endif