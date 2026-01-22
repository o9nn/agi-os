#ifndef _KERN_DTRACE_H_
#define _KERN_DTRACE_H_
#include <kern/kern_types.h>
#include <kern/lock.h>
#ifndef MACH_DTRACE
#define MACH_DTRACE 1
#endif
#if MACH_DTRACE
typedef enum {
DTRACE_PROBE_FUNCTION_ENTRY = 1,
DTRACE_PROBE_FUNCTION_EXIT  = 2,
DTRACE_PROBE_SYSCALL_ENTRY  = 3,
DTRACE_PROBE_SYSCALL_EXIT   = 4,
DTRACE_PROBE_IPC_SEND       = 5,
DTRACE_PROBE_IPC_RECEIVE    = 6,
DTRACE_PROBE_VM_FAULT       = 7,
DTRACE_PROBE_THREAD_SWITCH  = 8,
DTRACE_PROBE_TIMER_TICK     = 9,
DTRACE_PROBE_CUSTOM         = 10
} dtrace_probe_type_t;
typedef struct dtrace_probe {
uint32_t             id;
dtrace_probe_type_t  type;
const char          *name;
const char          *function;
boolean_t            enabled;
uint64_t             fire_count;
uint64_t             total_time;
uint32_t             arg_count;
void                *handler;
} dtrace_probe_t;
typedef struct dtrace_event {
uint32_t    probe_id;
uint64_t    timestamp;
uint32_t    cpu_id;
uint32_t    thread_id;
uint32_t    task_id;
uint64_t    args[6];
} dtrace_event_t;
#define DTRACE_BUFFER_SIZE 1024
typedef struct dtrace_buffer {
dtrace_event_t   events[DTRACE_BUFFER_SIZE];
uint32_t         head;
uint32_t         tail;
uint32_t         count;
uint32_t         overruns;
simple_lock_irq_data_t lock;
} dtrace_buffer_t;
typedef struct dtrace_metrics {
uint64_t total_probes_fired;
uint64_t total_events_captured;
uint64_t buffer_overruns;
uint64_t probe_overhead_ns;
uint32_t active_probes;
uint32_t max_probes;
} dtrace_metrics_t;
typedef struct dtrace_state {
dtrace_probe_t     *probes;
uint32_t            probe_count;
uint32_t            max_probes;
dtrace_buffer_t     buffer;
dtrace_metrics_t    metrics;
boolean_t           enabled;
simple_lock_irq_data_t probe_lock;
} dtrace_state_t;
typedef void (*dtrace_handler_t)(dtrace_probe_t *probe, uint64_t arg0,
uint64_t arg1, uint64_t arg2, uint64_t arg3,
uint64_t arg4, uint64_t arg5);
#define DTRACE_FUNCTION_ENTRY(name) \
dtrace_probe_fire(DTRACE_PROBE_FUNCTION_ENTRY, name, \
(uint64_t)__builtin_return_address(0), 0, 0, 0, 0, 0)
#define DTRACE_FUNCTION_EXIT(name) \
dtrace_probe_fire(DTRACE_PROBE_FUNCTION_EXIT, name, \
(uint64_t)__builtin_return_address(0), 0, 0, 0, 0, 0)
#define DTRACE_SYSCALL_ENTRY(name, arg0) \
dtrace_probe_fire(DTRACE_PROBE_SYSCALL_ENTRY, name, \
(uint64_t)(arg0), 0, 0, 0, 0, 0)
#define DTRACE_SYSCALL_EXIT(name, retval) \
dtrace_probe_fire(DTRACE_PROBE_SYSCALL_EXIT, name, \
(uint64_t)(retval), 0, 0, 0, 0, 0)
#define DTRACE_IPC_SEND(port, size) \
dtrace_probe_fire(DTRACE_PROBE_IPC_SEND, "ipc_send", \
(uint64_t)(port), (uint64_t)(size), 0, 0, 0, 0)
#define DTRACE_IPC_RECEIVE(port, size) \
dtrace_probe_fire(DTRACE_PROBE_IPC_RECEIVE, "ipc_receive", \
(uint64_t)(port), (uint64_t)(size), 0, 0, 0, 0)
#define DTRACE_VM_FAULT(addr, type) \
dtrace_probe_fire(DTRACE_PROBE_VM_FAULT, "vm_fault", \
(uint64_t)(addr), (uint64_t)(type), 0, 0, 0, 0)
#define DTRACE_THREAD_SWITCH(old_thread, new_thread) \
dtrace_probe_fire(DTRACE_PROBE_THREAD_SWITCH, "thread_switch", \
(uint64_t)(old_thread), (uint64_t)(new_thread), 0, 0, 0, 0)
#define DTRACE_CUSTOM(name, arg0, arg1, arg2, arg3, arg4, arg5) \
dtrace_probe_fire(DTRACE_PROBE_CUSTOM, name, \
(uint64_t)(arg0), (uint64_t)(arg1), (uint64_t)(arg2), \
(uint64_t)(arg3), (uint64_t)(arg4), (uint64_t)(arg5))
void dtrace_init(void);
void dtrace_shutdown(void);
uint32_t dtrace_probe_register(dtrace_probe_type_t type, const char *name,
const char *function, dtrace_handler_t handler);
boolean_t dtrace_probe_enable(uint32_t probe_id);
boolean_t dtrace_probe_disable(uint32_t probe_id);
boolean_t dtrace_probe_remove(uint32_t probe_id);
void dtrace_probe_fire(dtrace_probe_type_t type, const char *name,
uint64_t arg0, uint64_t arg1, uint64_t arg2,
uint64_t arg3, uint64_t arg4, uint64_t arg5);
uint32_t dtrace_buffer_read(dtrace_event_t *events, uint32_t max_events);
void dtrace_buffer_clear(void);
void dtrace_get_metrics(dtrace_metrics_t *metrics);
uint32_t dtrace_get_probe_count(void);
boolean_t dtrace_get_probe_info(uint32_t index, dtrace_probe_t *probe_info);
void dtrace_enable(void);
void dtrace_disable(void);
boolean_t dtrace_is_enabled(void);
uint64_t dtrace_gethrtime(void);
#else
#define DTRACE_FUNCTION_ENTRY(name)              do { } while (0)
#define DTRACE_FUNCTION_EXIT(name)               do { } while (0)
#define DTRACE_SYSCALL_ENTRY(name, arg0)         do { } while (0)
#define DTRACE_SYSCALL_EXIT(name, retval)        do { } while (0)
#define DTRACE_IPC_SEND(port, size)              do { } while (0)
#define DTRACE_IPC_RECEIVE(port, size)           do { } while (0)
#define DTRACE_VM_FAULT(addr, type)              do { } while (0)
#define DTRACE_THREAD_SWITCH(old_thread, new_thread) do { } while (0)
#define DTRACE_CUSTOM(name, arg0, arg1, arg2, arg3, arg4, arg5) do { } while (0)
static inline void dtrace_init(void) { }
static inline void dtrace_shutdown(void) { }
static inline void dtrace_enable(void) { }
static inline void dtrace_disable(void) { }
static inline boolean_t dtrace_is_enabled(void) { return FALSE; }
#endif
#endif