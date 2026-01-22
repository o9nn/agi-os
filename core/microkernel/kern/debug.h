#ifndef _mach_debug__debug_
#define _mach_debug__debug_
#include <kern/assert.h>
#ifndef NDEBUG
#define here() printf("@ %s:%d\n", __FILE__, __LINE__)
#define message(args) ({ printf("@ %s:%d: ", __FILE__, __LINE__); printf args; printf("\n"); })
#define otsan() panic("%s:%d: off the straight and narrow!", __FILE__, __LINE__)
#define struct_id_decl unsigned struct_id;
#define struct_id_init(p,id) ((p)->struct_id = (id))
#define struct_id_denit(p) ((p)->struct_id = 0)
#define struct_id_verify(p,id) \
({ if ((p)->struct_id != (id)) \
panic("%s:%d: "#p" (%08x) struct_id should be "#id" (%08x), is %08x\n", \
__FILE__, __LINE__, (p), (id), (p->struct_id)); \
})
#else
#define otsan()
#define struct_id_decl
#define struct_id_init(p,id)
#define struct_id_denit(p)
#define struct_id_verify(p,id)
#endif
extern void log (int level, const char *fmt, ...);
extern void panic_init(void);
extern void Panic (const char *file, int line, const char *fun,
const char *s, ...)
__attribute__ ((noreturn, format (printf, 4, 5)));
#define panic(s, ...) \
Panic (__FILE__, __LINE__, __FUNCTION__, s, ##__VA_ARGS__)
extern void SoftDebugger (const char *message);
extern void Debugger (const char *message) __attribute__ ((noreturn));
#include <mach/system_debug.h>
#ifndef NDEBUG
#define sysdebug_here(subsystem) \
do { \
printf("@ %s:%d\n", __FILE__, __LINE__); \
SYSDEBUG_LOG((subsystem), SYSDEBUG_EVENT_TRACE, \
"Debug marker at %s:%d", __FILE__, __LINE__); \
} while (0)
#define sysdebug_message(subsystem, args) \
do { \
printf("@ %s:%d: ", __FILE__, __LINE__); \
printf args; \
printf("\n"); \
SYSDEBUG_LOG((subsystem), SYSDEBUG_EVENT_TRACE, \
"Debug message at %s:%d", __FILE__, __LINE__); \
} while (0)
#define sysdebug_panic(subsystem, s, ...) \
do { \
SYSDEBUG_LOG((subsystem), SYSDEBUG_EVENT_ERROR, \
"Panic in %s:%d: " s, __FILE__, __LINE__, ##__VA_ARGS__); \
Panic(__FILE__, __LINE__, __FUNCTION__, s, ##__VA_ARGS__); \
} while (0)
#else
#define sysdebug_here(subsystem) do {} while (0)
#define sysdebug_message(subsystem, args) do {} while (0)
#define sysdebug_panic(subsystem, s, ...) \
panic(s, ##__VA_ARGS__)
#endif
#define SYSDEBUG_ENTER_SUBSYSTEM(subsystem) \
SYSDEBUG_LOG((subsystem), SYSDEBUG_EVENT_TRACE, \
"Entering %s", __FUNCTION__)
#define SYSDEBUG_EXIT_SUBSYSTEM(subsystem) \
SYSDEBUG_LOG((subsystem), SYSDEBUG_EVENT_TRACE, \
"Exiting %s", __FUNCTION__)
#define SYSDEBUG_CALL_SUBSYSTEM(from, to, function_name) \
do { \
SYSDEBUG_TRACE_INTERACTION((from), (to), SYSDEBUG_EVENT_INTERACTION, \
(function_name)); \
SYSDEBUG_LOG((from), SYSDEBUG_EVENT_TRACE, \
"Calling %s->%s", #from, #to); \
} while (0)
#endif