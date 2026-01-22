#ifndef _MACH_LTTNG_H_
#define _MACH_LTTNG_H_
#include <mach/boolean.h>
#include <stdint.h>
#ifdef __KERNEL__
#ifndef __uint8_t_defined
typedef unsigned char   uint8_t;
typedef unsigned short  uint16_t;
typedef unsigned int    uint32_t;
typedef unsigned long long uint64_t;
#define __uint8_t_defined
#endif
#else
#include <stdint.h>
#endif
#ifndef CONFIG_MACH_TRACING
#define CONFIG_MACH_TRACING 1
#endif
#define MACH_TRACE_BUF_SIZE 8192
typedef enum {
MACH_TRACE_IPC = 0,
MACH_TRACE_SCHED,
MACH_TRACE_VM,
MACH_TRACE_KERN,
MACH_TRACE_DEBUG,
MACH_TRACE_MAX_CATEGORY
} mach_trace_category_t;
typedef enum {
MACH_TRACE_LEVEL_EMERG = 0,
MACH_TRACE_LEVEL_ALERT,
MACH_TRACE_LEVEL_CRIT,
MACH_TRACE_LEVEL_ERR,
MACH_TRACE_LEVEL_WARNING,
MACH_TRACE_LEVEL_NOTICE,
MACH_TRACE_LEVEL_INFO,
MACH_TRACE_LEVEL_DEBUG
} mach_trace_level_t;
struct mach_trace_event {
uint32_t timestamp_hi;
uint32_t timestamp_lo;
uint16_t category;
uint16_t level;
uint32_t event_id;
uint32_t cpu_id;
uint32_t task_id;
uint32_t thread_id;
char data[64];
} __attribute__((packed));
extern struct mach_trace_buffer_impl *mach_trace_buf_ptr;
extern boolean_t mach_tracing_enabled;
void mach_trace_init(void);
void mach_trace_enable(boolean_t enable);
boolean_t mach_trace_is_enabled(void);
void mach_trace_event(mach_trace_category_t category,
mach_trace_level_t level,
uint32_t event_id,
const char *fmt, ...);
#if CONFIG_MACH_TRACING
#define MACH_TRACEPOINT_DECLARE(category, name) \
extern void __mach_trace_##category##_##name(void)
#define MACH_TRACEPOINT_DEFINE(category, name, level, id, fmt, ...) \
static void __attribute__((unused)) __mach_trace_##category##_##name(void) { \
if (mach_tracing_enabled) \
mach_trace_event(MACH_TRACE_##category, level, id, fmt, ##__VA_ARGS__); \
}
#define MACH_TRACEPOINT(category, name) \
do { \
if (mach_tracing_enabled) \
__mach_trace_##category##_##name(); \
} while (0)
#define TRACE_IPC(name) MACH_TRACEPOINT(IPC, name)
#define TRACE_SCHED(name) MACH_TRACEPOINT(SCHED, name)
#define TRACE_VM(name) MACH_TRACEPOINT(VM, name)
#define TRACE_KERN(name) MACH_TRACEPOINT(KERN, name)
#define TRACE_DEBUG(name) MACH_TRACEPOINT(DEBUG, name)
#define MACH_TRACE_EVENT_IPC_BASE    0x1000
#define MACH_TRACE_EVENT_SCHED_BASE  0x2000
#define MACH_TRACE_EVENT_VM_BASE     0x3000
#define MACH_TRACE_EVENT_KERN_BASE   0x4000
#define MACH_TRACE_EVENT_DEBUG_BASE  0x5000
#else
#define MACH_TRACEPOINT_DECLARE(category, name)
#define MACH_TRACEPOINT_DEFINE(category, name, level, id, fmt, ...)
#define MACH_TRACEPOINT(category, name) do { } while (0)
#define TRACE_IPC(name) do { } while (0)
#define TRACE_SCHED(name) do { } while (0)
#define TRACE_VM(name) do { } while (0)
#define TRACE_KERN(name) do { } while (0)
#define TRACE_DEBUG(name) do { } while (0)
#endif
struct mach_trace_read_request {
uint32_t max_events;
uint32_t timeout_ms;
};
struct mach_trace_read_response {
uint32_t num_events;
uint32_t dropped_events;
struct mach_trace_event events[];
};
#define MACH_TRACE_ENABLE_CALL    3500
#define MACH_TRACE_READ_CALL      3501
#define MACH_TRACE_STATUS_CALL    3502
#endif