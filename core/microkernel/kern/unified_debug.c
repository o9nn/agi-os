#include <mach/unified_debug.h>
#include <mach/system_debug.h>
#include <kern/debug.h>
#include <kern/printf.h>
#include <string.h>
#if MACH_KDB
#include <gdb_stub.h>
#endif
static struct unified_debug_state global_unified_debug = {
.initialized = FALSE,
.traditional_debug_enabled = FALSE,
.system_debug_enabled = FALSE,
.gdb_debug_enabled = FALSE,
.cross_component_tracking = FALSE
};
struct debug_performance_measurement {
char name[64];
unsigned long start_time;
unsigned long end_time;
boolean_t active;
};
#define MAX_PERFORMANCE_MEASUREMENTS 16
static struct debug_performance_measurement perf_measurements[MAX_PERFORMANCE_MEASUREMENTS];
static int perf_measurement_count = 0;
struct debug_session {
char name[64];
boolean_t active;
unsigned long start_time;
unsigned long event_count;
};
#define MAX_DEBUG_SESSIONS 8
static struct debug_session debug_sessions[MAX_DEBUG_SESSIONS];
static int debug_session_count = 0;
void unified_debug_init(void)
{
if (global_unified_debug.initialized) {
return;
}
printf("[UNIFIED_DEBUG] Initializing unified debugging interface\n");
sysdebug_init();
panic_init();
#if MACH_KDB
gdb_stub_init();
sysdebug_gdb_integration_init();
global_unified_debug.gdb_debug_enabled = gdb_stub_is_enabled();
#endif
global_unified_debug.traditional_debug_enabled = TRUE;
global_unified_debug.system_debug_enabled = sysdebug_is_enabled();
global_unified_debug.cross_component_tracking =
sysdebug_is_cross_component_tracking_enabled();
global_unified_debug.initialized = TRUE;
printf("[UNIFIED_DEBUG] Unified debugging interface initialized\n");
printf("  - Traditional debugging: %s\n",
global_unified_debug.traditional_debug_enabled ? "enabled" : "disabled");
printf("  - System debugging: %s\n",
global_unified_debug.system_debug_enabled ? "enabled" : "disabled");
printf("  - GDB integration: %s\n",
global_unified_debug.gdb_debug_enabled ? "enabled" : "disabled");
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_INIT,
"Unified debugging interface initialized");
}
void unified_debug_enable_all(boolean_t enable)
{
if (!global_unified_debug.initialized) {
unified_debug_init();
}
printf("[UNIFIED_DEBUG] %s all debugging subsystems\n",
enable ? "Enabling" : "Disabling");
sysdebug_enable(enable);
global_unified_debug.system_debug_enabled = enable;
sysdebug_enable_cross_component_tracking(enable);
global_unified_debug.cross_component_tracking = enable;
#if MACH_KDB
gdb_stub_enable(enable);
global_unified_debug.gdb_debug_enabled = enable;
#endif
global_unified_debug.traditional_debug_enabled = TRUE;
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_STATE,
"All debugging subsystems %s", enable ? "enabled" : "disabled");
}
void unified_debug_disable_all(void)
{
unified_debug_enable_all(FALSE);
}
boolean_t unified_debug_is_any_enabled(void)
{
return global_unified_debug.traditional_debug_enabled ||
global_unified_debug.system_debug_enabled ||
global_unified_debug.gdb_debug_enabled;
}
void unified_debug_get_state(struct unified_debug_state *state)
{
if (state) {
*state = global_unified_debug;
}
}
void unified_debug_dump_all_state(void)
{
printf("\n=== UNIFIED DEBUGGING STATE DUMP ===\n");
printf("Unified Debug Status:\n");
printf("  - Initialized: %s\n", global_unified_debug.initialized ? "YES" : "NO");
printf("  - Traditional Debug: %s\n",
global_unified_debug.traditional_debug_enabled ? "enabled" : "disabled");
printf("  - System Debug: %s\n",
global_unified_debug.system_debug_enabled ? "enabled" : "disabled");
printf("  - GDB Debug: %s\n",
global_unified_debug.gdb_debug_enabled ? "enabled" : "disabled");
printf("  - Cross-Component: %s\n",
global_unified_debug.cross_component_tracking ? "enabled" : "disabled");
if (global_unified_debug.system_debug_enabled) {
sysdebug_dump_system_state();
}
if (global_unified_debug.cross_component_tracking) {
sysdebug_dump_cross_component_interactions();
}
unified_debug_performance_dump();
printf("=== END UNIFIED DEBUGGING STATE DUMP ===\n\n");
}
void unified_debug_vm_init(void)
{
UNIFIED_DEBUG_INIT("Virtual Memory");
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_VM, SYSDEBUG_EVENT_INIT,
"VM debugging integration initialized");
}
void unified_debug_vm_operation(const char *operation, void *address, vm_size_t size)
{
if (!unified_debug_is_any_enabled()) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_VM, SYSDEBUG_EVENT_TRACE,
"VM operation: %s at %p, size %zu", operation, address, size);
SYSDEBUG_TRACE_INTERACTION(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_SUBSYSTEM_VM,
SYSDEBUG_EVENT_INTERACTION, operation);
}
void unified_debug_vm_fault(vm_offset_t fault_addr, kern_return_t result)
{
if (!unified_debug_is_any_enabled()) {
return;
}
if (result == KERN_SUCCESS) {
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_VM, SYSDEBUG_EVENT_TRACE,
"VM fault resolved at address 0x%lx", (unsigned long)fault_addr);
} else {
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_VM, SYSDEBUG_EVENT_ERROR,
"VM fault failed at address 0x%lx, result=%d",
(unsigned long)fault_addr, result);
}
}
void unified_debug_ipc_init(void)
{
UNIFIED_DEBUG_INIT("Inter-Process Communication");
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_IPC, SYSDEBUG_EVENT_INIT,
"IPC debugging integration initialized");
}
void unified_debug_ipc_message(mach_msg_header_t *msg, boolean_t send)
{
if (!unified_debug_is_any_enabled() || !msg) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_IPC, SYSDEBUG_EVENT_TRACE,
"IPC message %s: id=0x%x, size=%u",
send ? "send" : "receive", msg->msgh_id, msg->msgh_size);
SYSDEBUG_TRACE_INTERACTION(SYSDEBUG_SUBSYSTEM_IPC, SYSDEBUG_SUBSYSTEM_KERNEL,
SYSDEBUG_EVENT_INTERACTION, "message_transfer");
}
void unified_debug_ipc_port_operation(const char *operation, mach_port_t port)
{
if (!unified_debug_is_any_enabled()) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_IPC, SYSDEBUG_EVENT_TRACE,
"IPC port operation: %s on port 0x%x", operation, port);
}
void unified_debug_thread_init(void)
{
UNIFIED_DEBUG_INIT("Thread Management");
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_SCHED, SYSDEBUG_EVENT_INIT,
"Thread debugging integration initialized");
}
void unified_debug_thread_create(thread_t thread)
{
if (!unified_debug_is_any_enabled()) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_SCHED, SYSDEBUG_EVENT_RESOURCE,
"Thread created: %p", thread);
#if MACH_KDB
gdb_stub_thread_create(thread);
#endif
}
void unified_debug_thread_destroy(thread_t thread)
{
if (!unified_debug_is_any_enabled()) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_SCHED, SYSDEBUG_EVENT_RESOURCE,
"Thread destroyed: %p", thread);
#if MACH_KDB
gdb_stub_thread_destroy(thread);
#endif
}
void unified_debug_thread_switch(thread_t old_thread, thread_t new_thread)
{
if (!unified_debug_should_trace_subsystem(SYSDEBUG_SUBSYSTEM_SCHED)) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_SCHED, SYSDEBUG_EVENT_STATE,
"Thread switch: %p -> %p", old_thread, new_thread);
#if MACH_KDB
gdb_stub_thread_switch(old_thread, new_thread);
#endif
}
void unified_debug_task_create(task_t task)
{
if (!unified_debug_is_any_enabled()) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_SCHED, SYSDEBUG_EVENT_RESOURCE,
"Task created: %p", task);
}
void unified_debug_task_destroy(task_t task)
{
if (!unified_debug_is_any_enabled()) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_SCHED, SYSDEBUG_EVENT_RESOURCE,
"Task destroyed: %p", task);
}
void unified_debug_device_init(void)
{
UNIFIED_DEBUG_INIT("Device Management");
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_DEVICE, SYSDEBUG_EVENT_INIT,
"Device debugging integration initialized");
}
void unified_debug_device_operation(const char *device_name, const char *operation)
{
if (!unified_debug_is_any_enabled()) {
return;
}
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_DEVICE, SYSDEBUG_EVENT_TRACE,
"Device operation: %s on %s", operation, device_name);
SYSDEBUG_TRACE_INTERACTION(SYSDEBUG_SUBSYSTEM_DEVICE, SYSDEBUG_SUBSYSTEM_KERNEL,
SYSDEBUG_EVENT_INTERACTION, operation);
}
void unified_debug_session_start(const char *session_name)
{
int i;
if (!unified_debug_is_any_enabled() || debug_session_count >= MAX_DEBUG_SESSIONS) {
return;
}
for (i = 0; i < MAX_DEBUG_SESSIONS; i++) {
if (!debug_sessions[i].active) {
strncpy(debug_sessions[i].name, session_name,
sizeof(debug_sessions[i].name) - 1);
debug_sessions[i].name[sizeof(debug_sessions[i].name) - 1] = '\0';
debug_sessions[i].active = TRUE;
debug_sessions[i].start_time = 0;
debug_sessions[i].event_count = 0;
debug_session_count++;
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_INIT,
"Debug session started: %s", session_name);
break;
}
}
}
void unified_debug_session_end(const char *session_name)
{
int i;
if (!unified_debug_is_any_enabled()) {
return;
}
for (i = 0; i < MAX_DEBUG_SESSIONS; i++) {
if (debug_sessions[i].active &&
strcmp(debug_sessions[i].name, session_name) == 0) {
debug_sessions[i].active = FALSE;
debug_session_count--;
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_SHUTDOWN,
"Debug session ended: %s (events: %lu)",
session_name, debug_sessions[i].event_count);
break;
}
}
}
void unified_debug_performance_start(const char *measurement_name)
{
int i;
if (!unified_debug_is_any_enabled() ||
perf_measurement_count >= MAX_PERFORMANCE_MEASUREMENTS) {
return;
}
for (i = 0; i < MAX_PERFORMANCE_MEASUREMENTS; i++) {
if (!perf_measurements[i].active) {
strncpy(perf_measurements[i].name, measurement_name,
sizeof(perf_measurements[i].name) - 1);
perf_measurements[i].name[sizeof(perf_measurements[i].name) - 1] = '\0';
perf_measurements[i].start_time = 0;
perf_measurements[i].end_time = 0;
perf_measurements[i].active = TRUE;
perf_measurement_count++;
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_TRACE,
"Performance measurement started: %s", measurement_name);
break;
}
}
}
void unified_debug_performance_end(const char *measurement_name)
{
int i;
if (!unified_debug_is_any_enabled()) {
return;
}
for (i = 0; i < MAX_PERFORMANCE_MEASUREMENTS; i++) {
if (perf_measurements[i].active &&
strcmp(perf_measurements[i].name, measurement_name) == 0) {
perf_measurements[i].end_time = 0;
perf_measurements[i].active = FALSE;
perf_measurement_count--;
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_TRACE,
"Performance measurement ended: %s", measurement_name);
break;
}
}
}
void unified_debug_performance_dump(void)
{
int i;
printf("\n--- Performance Measurements ---\n");
printf("Active measurements: %d\n", perf_measurement_count);
for (i = 0; i < MAX_PERFORMANCE_MEASUREMENTS; i++) {
if (perf_measurements[i].active ||
(perf_measurements[i].end_time > perf_measurements[i].start_time)) {
printf("  %s: %s", perf_measurements[i].name,
perf_measurements[i].active ? "ACTIVE" : "COMPLETED");
if (!perf_measurements[i].active && perf_measurements[i].end_time > 0) {
printf(" (duration: %lu)",
perf_measurements[i].end_time - perf_measurements[i].start_time);
}
printf("\n");
}
}
}
#if MACH_KDB
void unified_debug_gdb_setup_system_breakpoints(void)
{
if (!global_unified_debug.gdb_debug_enabled) {
return;
}
printf("[UNIFIED_DEBUG] Setting up system-wide GDB breakpoints\n");
sysdebug_gdb_break_on_event(SYSDEBUG_SUBSYSTEM_VM, SYSDEBUG_EVENT_INIT);
sysdebug_gdb_break_on_event(SYSDEBUG_SUBSYSTEM_IPC, SYSDEBUG_EVENT_INIT);
sysdebug_gdb_break_on_event(SYSDEBUG_SUBSYSTEM_SCHED, SYSDEBUG_EVENT_INIT);
sysdebug_gdb_break_on_event(SYSDEBUG_SUBSYSTEM_VM, SYSDEBUG_EVENT_ERROR);
sysdebug_gdb_break_on_event(SYSDEBUG_SUBSYSTEM_IPC, SYSDEBUG_EVENT_ERROR);
}
void unified_debug_gdb_enable_cross_component_breaks(boolean_t enable)
{
if (!global_unified_debug.gdb_debug_enabled) {
return;
}
if (enable) {
printf("[UNIFIED_DEBUG] Enabling cross-component GDB breakpoints\n");
sysdebug_gdb_break_on_interaction(SYSDEBUG_SUBSYSTEM_VM, SYSDEBUG_SUBSYSTEM_IPC);
sysdebug_gdb_break_on_interaction(SYSDEBUG_SUBSYSTEM_IPC, SYSDEBUG_SUBSYSTEM_SCHED);
sysdebug_gdb_break_on_interaction(SYSDEBUG_SUBSYSTEM_SCHED, SYSDEBUG_SUBSYSTEM_VM);
} else {
printf("[UNIFIED_DEBUG] Disabling cross-component GDB breakpoints\n");
}
}
#else
void unified_debug_gdb_setup_system_breakpoints(void)
{
}
void unified_debug_gdb_enable_cross_component_breaks(boolean_t enable)
{
}
#endif
void unified_debug_script_command(const char *command)
{
if (!unified_debug_is_any_enabled() || !command) {
return;
}
printf("[UNIFIED_DEBUG] Executing debug command: %s\n", command);
if (strncmp(command, "dump", 4) == 0) {
unified_debug_dump_all_state();
} else if (strncmp(command, "enable", 6) == 0) {
unified_debug_enable_all(TRUE);
} else if (strncmp(command, "disable", 7) == 0) {
unified_debug_disable_all();
} else if (strncmp(command, "reset", 5) == 0) {
sysdebug_reset_statistics();
} else {
printf("[UNIFIED_DEBUG] Unknown debug command: %s\n", command);
}
}
void unified_debug_generate_gdb_scripts(void)
{
printf("[UNIFIED_DEBUG] Generating GDB scripts for system debugging\n");
SYSDEBUG_LOG(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_TRACE,
"GDB script generation requested");
}