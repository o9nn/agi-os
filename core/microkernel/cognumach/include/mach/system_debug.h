#ifndef _MACH_SYSTEM_DEBUG_H_
#define _MACH_SYSTEM_DEBUG_H_
#include <mach/boolean.h>
#include <mach/mach_types.h>
#include <mach/kern_return.h>
typedef enum {
SYSDEBUG_SUBSYSTEM_KERNEL = 0,
SYSDEBUG_SUBSYSTEM_VM = 1,
SYSDEBUG_SUBSYSTEM_IPC = 2,
SYSDEBUG_SUBSYSTEM_SCHED = 3,
SYSDEBUG_SUBSYSTEM_DEVICE = 4,
SYSDEBUG_SUBSYSTEM_NET = 5,
SYSDEBUG_SUBSYSTEM_MAX = 6
} sysdebug_subsystem_t;
typedef enum {
SYSDEBUG_EVENT_NONE = 0,
SYSDEBUG_EVENT_INIT = 1,
SYSDEBUG_EVENT_SHUTDOWN = 2,
SYSDEBUG_EVENT_ERROR = 3,
SYSDEBUG_EVENT_WARNING = 4,
SYSDEBUG_EVENT_TRACE = 5,
SYSDEBUG_EVENT_INTERACTION = 6,
SYSDEBUG_EVENT_RESOURCE = 7,
SYSDEBUG_EVENT_STATE = 8,
SYSDEBUG_EVENT_MAX = 9
} sysdebug_event_t;
typedef enum {
SYSDEBUG_LEVEL_NONE = 0,
SYSDEBUG_LEVEL_MINIMAL = 1,
SYSDEBUG_LEVEL_NORMAL = 2,
SYSDEBUG_LEVEL_VERBOSE = 3,
SYSDEBUG_LEVEL_TRACE = 4
} sysdebug_level_t;
struct sysdebug_context {
boolean_t enabled;
sysdebug_level_t global_level;
sysdebug_level_t subsystem_levels[SYSDEBUG_SUBSYSTEM_MAX];
unsigned long event_count[SYSDEBUG_EVENT_MAX];
unsigned long subsystem_events[SYSDEBUG_SUBSYSTEM_MAX][SYSDEBUG_EVENT_MAX];
void *cross_component_context;
};
struct sysdebug_cross_component {
boolean_t tracking_enabled;
struct {
sysdebug_subsystem_t from_subsystem;
sysdebug_subsystem_t to_subsystem;
sysdebug_event_t event_type;
unsigned long timestamp;
void *context_data;
} interactions[32];
int interaction_head;
int interaction_count;
};
extern void sysdebug_init(void);
extern void sysdebug_enable(boolean_t enable);
extern boolean_t sysdebug_is_enabled(void);
extern void sysdebug_set_global_level(sysdebug_level_t level);
extern void sysdebug_set_subsystem_level(sysdebug_subsystem_t subsystem,
sysdebug_level_t level);
extern sysdebug_level_t sysdebug_get_subsystem_level(sysdebug_subsystem_t subsystem);
extern void sysdebug_report_event(sysdebug_subsystem_t subsystem,
sysdebug_event_t event,
const char *message,
...);
extern void sysdebug_track_interaction(sysdebug_subsystem_t from_subsystem,
sysdebug_subsystem_t to_subsystem,
sysdebug_event_t event_type,
const void *context_data);
extern void sysdebug_enable_cross_component_tracking(boolean_t enable);
extern boolean_t sysdebug_is_cross_component_tracking_enabled(void);
extern void sysdebug_dump_system_state(void);
extern void sysdebug_dump_subsystem_state(sysdebug_subsystem_t subsystem);
extern void sysdebug_dump_cross_component_interactions(void);
extern void sysdebug_get_statistics(sysdebug_subsystem_t subsystem,
unsigned long *event_counts);
extern void sysdebug_reset_statistics(void);
#define SYSDEBUG_ENABLED() sysdebug_is_enabled()
#define SYSDEBUG_LOG(subsystem, event, message, ...) \
do { \
if (sysdebug_is_enabled()) { \
sysdebug_report_event((subsystem), (event), (message), ##__VA_ARGS__); \
} \
} while (0)
#define SYSDEBUG_TRACE_INTERACTION(from, to, event, context) \
do { \
if (sysdebug_is_cross_component_tracking_enabled()) { \
sysdebug_track_interaction((from), (to), (event), (context)); \
} \
} while (0)
#define SYSDEBUG_VM_LOG(event, message, ...) \
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_VM, (event), (message), ##__VA_ARGS__)
#define SYSDEBUG_IPC_LOG(event, message, ...) \
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_IPC, (event), (message), ##__VA_ARGS__)
#define SYSDEBUG_SCHED_LOG(event, message, ...) \
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_SCHED, (event), (message), ##__VA_ARGS__)
#define SYSDEBUG_DEVICE_LOG(event, message, ...) \
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_DEVICE, (event), (message), ##__VA_ARGS__)
#ifdef DEBUG
#define SYSDEBUG_DEBUG_ASSERT(condition, subsystem, message) \
do { \
if (!(condition)) { \
SYSDEBUG_LOG((subsystem), SYSDEBUG_EVENT_ERROR, \
"Debug assertion failed: %s: %s", #condition, (message)); \
} \
} while (0)
#else
#define SYSDEBUG_DEBUG_ASSERT(condition, subsystem, message) do {} while (0)
#endif
extern void sysdebug_gdb_integration_init(void);
extern void sysdebug_gdb_break_on_event(sysdebug_subsystem_t subsystem,
sysdebug_event_t event);
extern void sysdebug_gdb_break_on_interaction(sysdebug_subsystem_t from,
sysdebug_subsystem_t to);
#endif