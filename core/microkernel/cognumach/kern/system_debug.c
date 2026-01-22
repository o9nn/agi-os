#include <mach/system_debug.h>
#include <kern/debug.h>
#include <kern/printf.h>
#include <string.h>
#include <stdarg.h>
#if MACH_KDB
#include <gdb_stub.h>
#endif
static struct sysdebug_context global_sysdebug_context = {
.enabled = FALSE,
.global_level = SYSDEBUG_LEVEL_NORMAL,
.subsystem_levels = { SYSDEBUG_LEVEL_NORMAL },
.event_count = { 0 },
.subsystem_events = { { 0 } },
.cross_component_context = NULL
};
static struct sysdebug_cross_component cross_component_state = {
.tracking_enabled = FALSE,
.interactions = { { 0 } },
.interaction_head = 0,
.interaction_count = 0
};
static const char *sysdebug_subsystem_names[SYSDEBUG_SUBSYSTEM_MAX] = {
"KERNEL",
"VM",
"IPC",
"SCHED",
"DEVICE",
"NET"
};
static const char *sysdebug_event_names[SYSDEBUG_EVENT_MAX] = {
"NONE",
"INIT",
"SHUTDOWN",
"ERROR",
"WARNING",
"TRACE",
"INTERACTION",
"RESOURCE",
"STATE"
};
void sysdebug_init(void)
{
int i, j;
printf("[SYSDEBUG] Initializing system-wide debugging infrastructure\n");
global_sysdebug_context.enabled = FALSE;
global_sysdebug_context.global_level = SYSDEBUG_LEVEL_NORMAL;
for (i = 0; i < SYSDEBUG_SUBSYSTEM_MAX; i++) {
global_sysdebug_context.subsystem_levels[i] = SYSDEBUG_LEVEL_NORMAL;
}
for (i = 0; i < SYSDEBUG_EVENT_MAX; i++) {
global_sysdebug_context.event_count[i] = 0;
}
for (i = 0; i < SYSDEBUG_SUBSYSTEM_MAX; i++) {
for (j = 0; j < SYSDEBUG_EVENT_MAX; j++) {
global_sysdebug_context.subsystem_events[i][j] = 0;
}
}
cross_component_state.tracking_enabled = FALSE;
cross_component_state.interaction_head = 0;
cross_component_state.interaction_count = 0;
global_sysdebug_context.cross_component_context = &cross_component_state;
printf("[SYSDEBUG] System-wide debugging infrastructure initialized\n");
sysdebug_report_event(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_INIT,
"System debugging infrastructure initialized");
}
void sysdebug_enable(boolean_t enable)
{
global_sysdebug_context.enabled = enable;
if (enable) {
printf("[SYSDEBUG] System-wide debugging enabled (level: %d)\n",
global_sysdebug_context.global_level);
sysdebug_report_event(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_STATE,
"System debugging enabled");
} else {
printf("[SYSDEBUG] System-wide debugging disabled\n");
sysdebug_report_event(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_STATE,
"System debugging disabled");
}
}
boolean_t sysdebug_is_enabled(void)
{
return global_sysdebug_context.enabled;
}
void sysdebug_set_global_level(sysdebug_level_t level)
{
if (level >= SYSDEBUG_LEVEL_NONE && level <= SYSDEBUG_LEVEL_TRACE) {
global_sysdebug_context.global_level = level;
printf("[SYSDEBUG] Global debugging level set to %d\n", level);
sysdebug_report_event(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_STATE,
"Global debug level changed to %d", level);
}
}
void sysdebug_set_subsystem_level(sysdebug_subsystem_t subsystem,
sysdebug_level_t level)
{
if (subsystem >= 0 && subsystem < SYSDEBUG_SUBSYSTEM_MAX &&
level >= SYSDEBUG_LEVEL_NONE && level <= SYSDEBUG_LEVEL_TRACE) {
global_sysdebug_context.subsystem_levels[subsystem] = level;
printf("[SYSDEBUG] %s subsystem debugging level set to %d\n",
sysdebug_subsystem_names[subsystem], level);
sysdebug_report_event(subsystem, SYSDEBUG_EVENT_STATE,
"Subsystem debug level changed to %d", level);
}
}
sysdebug_level_t sysdebug_get_subsystem_level(sysdebug_subsystem_t subsystem)
{
if (subsystem >= 0 && subsystem < SYSDEBUG_SUBSYSTEM_MAX) {
return global_sysdebug_context.subsystem_levels[subsystem];
}
return SYSDEBUG_LEVEL_NONE;
}
void sysdebug_report_event(sysdebug_subsystem_t subsystem,
sysdebug_event_t event,
const char *message,
...)
{
va_list args;
char formatted_message[256];
sysdebug_level_t effective_level;
if (!global_sysdebug_context.enabled) {
return;
}
if (subsystem >= SYSDEBUG_SUBSYSTEM_MAX || event >= SYSDEBUG_EVENT_MAX) {
return;
}
effective_level = global_sysdebug_context.subsystem_levels[subsystem];
if (effective_level == SYSDEBUG_LEVEL_NONE) {
return;
}
va_start(args, message);
vsnprintf(formatted_message, sizeof(formatted_message), message, args);
va_end(args);
global_sysdebug_context.event_count[event]++;
global_sysdebug_context.subsystem_events[subsystem][event]++;
printf("[SYSDEBUG:%s:%s] %s\n",
sysdebug_subsystem_names[subsystem],
sysdebug_event_names[event],
formatted_message);
if (event == SYSDEBUG_EVENT_ERROR) {
log(0, "[SYSDEBUG] Error in %s: %s",
sysdebug_subsystem_names[subsystem], formatted_message);
}
}
void sysdebug_track_interaction(sysdebug_subsystem_t from_subsystem,
sysdebug_subsystem_t to_subsystem,
sysdebug_event_t event_type,
const void *context_data)
{
struct sysdebug_cross_component *cc_state;
int next_slot;
if (!global_sysdebug_context.enabled ||
!cross_component_state.tracking_enabled) {
return;
}
if (from_subsystem >= SYSDEBUG_SUBSYSTEM_MAX ||
to_subsystem >= SYSDEBUG_SUBSYSTEM_MAX ||
event_type >= SYSDEBUG_EVENT_MAX) {
return;
}
cc_state = &cross_component_state;
next_slot = cc_state->interaction_head;
cc_state->interaction_head = (cc_state->interaction_head + 1) % 32;
cc_state->interactions[next_slot].from_subsystem = from_subsystem;
cc_state->interactions[next_slot].to_subsystem = to_subsystem;
cc_state->interactions[next_slot].event_type = event_type;
cc_state->interactions[next_slot].timestamp = 0;
cc_state->interactions[next_slot].context_data = (void *)context_data;
if (cc_state->interaction_count < 32) {
cc_state->interaction_count++;
}
if (global_sysdebug_context.global_level >= SYSDEBUG_LEVEL_VERBOSE) {
printf("[SYSDEBUG:INTERACTION] %s -> %s (%s)\n",
sysdebug_subsystem_names[from_subsystem],
sysdebug_subsystem_names[to_subsystem],
sysdebug_event_names[event_type]);
}
}
void sysdebug_enable_cross_component_tracking(boolean_t enable)
{
cross_component_state.tracking_enabled = enable;
if (enable) {
printf("[SYSDEBUG] Cross-component tracking enabled\n");
sysdebug_report_event(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_STATE,
"Cross-component tracking enabled");
} else {
printf("[SYSDEBUG] Cross-component tracking disabled\n");
sysdebug_report_event(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_STATE,
"Cross-component tracking disabled");
}
}
boolean_t sysdebug_is_cross_component_tracking_enabled(void)
{
return cross_component_state.tracking_enabled;
}
void sysdebug_dump_system_state(void)
{
int i, j;
printf("\n=== SYSTEM-WIDE DEBUGGING STATE ===\n");
printf("Enabled: %s\n", global_sysdebug_context.enabled ? "YES" : "NO");
printf("Global Level: %d\n", global_sysdebug_context.global_level);
printf("Cross-component tracking: %s\n",
cross_component_state.tracking_enabled ? "YES" : "NO");
printf("\n--- Subsystem Debug Levels ---\n");
for (i = 0; i < SYSDEBUG_SUBSYSTEM_MAX; i++) {
printf("  %s: Level %d\n",
sysdebug_subsystem_names[i],
global_sysdebug_context.subsystem_levels[i]);
}
printf("\n--- Global Event Statistics ---\n");
for (i = 0; i < SYSDEBUG_EVENT_MAX; i++) {
if (global_sysdebug_context.event_count[i] > 0) {
printf("  %s: %lu events\n",
sysdebug_event_names[i],
global_sysdebug_context.event_count[i]);
}
}
printf("\n--- Per-Subsystem Event Statistics ---\n");
for (i = 0; i < SYSDEBUG_SUBSYSTEM_MAX; i++) {
boolean_t has_events = FALSE;
for (j = 0; j < SYSDEBUG_EVENT_MAX; j++) {
if (global_sysdebug_context.subsystem_events[i][j] > 0) {
has_events = TRUE;
break;
}
}
if (has_events) {
printf("  %s:\n", sysdebug_subsystem_names[i]);
for (j = 0; j < SYSDEBUG_EVENT_MAX; j++) {
if (global_sysdebug_context.subsystem_events[i][j] > 0) {
printf("    %s: %lu\n",
sysdebug_event_names[j],
global_sysdebug_context.subsystem_events[i][j]);
}
}
}
}
printf("=== END SYSTEM DEBUG STATE ===\n\n");
}
void sysdebug_dump_subsystem_state(sysdebug_subsystem_t subsystem)
{
int j;
if (subsystem >= SYSDEBUG_SUBSYSTEM_MAX) {
printf("Invalid subsystem ID: %d\n", subsystem);
return;
}
printf("\n=== %s SUBSYSTEM DEBUG STATE ===\n",
sysdebug_subsystem_names[subsystem]);
printf("Debug Level: %d\n",
global_sysdebug_context.subsystem_levels[subsystem]);
printf("Event Statistics:\n");
for (j = 0; j < SYSDEBUG_EVENT_MAX; j++) {
if (global_sysdebug_context.subsystem_events[subsystem][j] > 0) {
printf("  %s: %lu\n",
sysdebug_event_names[j],
global_sysdebug_context.subsystem_events[subsystem][j]);
}
}
printf("=== END %s SUBSYSTEM STATE ===\n\n",
sysdebug_subsystem_names[subsystem]);
}
void sysdebug_dump_cross_component_interactions(void)
{
struct sysdebug_cross_component *cc_state = &cross_component_state;
int i, slot;
printf("\n=== CROSS-COMPONENT INTERACTIONS ===\n");
printf("Tracking Enabled: %s\n", cc_state->tracking_enabled ? "YES" : "NO");
printf("Total Interactions Recorded: %d\n", cc_state->interaction_count);
if (cc_state->interaction_count > 0) {
printf("\nRecent Interactions (newest first):\n");
for (i = 0; i < cc_state->interaction_count; i++) {
slot = (cc_state->interaction_head - 1 - i + 32) % 32;
printf("  %d: %s -> %s (%s)\n",
i + 1,
sysdebug_subsystem_names[cc_state->interactions[slot].from_subsystem],
sysdebug_subsystem_names[cc_state->interactions[slot].to_subsystem],
sysdebug_event_names[cc_state->interactions[slot].event_type]);
}
}
printf("=== END CROSS-COMPONENT INTERACTIONS ===\n\n");
}
void sysdebug_get_statistics(sysdebug_subsystem_t subsystem,
unsigned long *event_counts)
{
int i;
if (subsystem >= SYSDEBUG_SUBSYSTEM_MAX || !event_counts) {
return;
}
for (i = 0; i < SYSDEBUG_EVENT_MAX; i++) {
event_counts[i] = global_sysdebug_context.subsystem_events[subsystem][i];
}
}
void sysdebug_reset_statistics(void)
{
int i, j;
printf("[SYSDEBUG] Resetting statistics\n");
for (i = 0; i < SYSDEBUG_EVENT_MAX; i++) {
global_sysdebug_context.event_count[i] = 0;
}
for (i = 0; i < SYSDEBUG_SUBSYSTEM_MAX; i++) {
for (j = 0; j < SYSDEBUG_EVENT_MAX; j++) {
global_sysdebug_context.subsystem_events[i][j] = 0;
}
}
cross_component_state.interaction_count = 0;
cross_component_state.interaction_head = 0;
sysdebug_report_event(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_STATE,
"Statistics reset");
}
#if MACH_KDB
void sysdebug_gdb_integration_init(void)
{
if (!gdb_stub_is_enabled()) {
return;
}
printf("[SYSDEBUG] Initializing GDB integration for system-wide debugging\n");
sysdebug_report_event(SYSDEBUG_SUBSYSTEM_KERNEL, SYSDEBUG_EVENT_INIT,
"GDB integration initialized");
}
void sysdebug_gdb_break_on_event(sysdebug_subsystem_t subsystem,
sysdebug_event_t event)
{
if (!gdb_stub_is_enabled() || !global_sysdebug_context.enabled) {
return;
}
printf("[SYSDEBUG] Setting GDB breakpoint for %s:%s events\n",
sysdebug_subsystem_names[subsystem],
sysdebug_event_names[event]);
}
void sysdebug_gdb_break_on_interaction(sysdebug_subsystem_t from,
sysdebug_subsystem_t to)
{
if (!gdb_stub_is_enabled() || !global_sysdebug_context.enabled) {
return;
}
printf("[SYSDEBUG] Setting GDB breakpoint for %s->%s interactions\n",
sysdebug_subsystem_names[from],
sysdebug_subsystem_names[to]);
}
#else
void sysdebug_gdb_integration_init(void)
{
}
void sysdebug_gdb_break_on_event(sysdebug_subsystem_t subsystem,
sysdebug_event_t event)
{
}
void sysdebug_gdb_break_on_interaction(sysdebug_subsystem_t from,
sysdebug_subsystem_t to)
{
}
#endif