#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
typedef enum {
DTRACE_PROBE_FUNCTION_ENTRY = 1,
DTRACE_PROBE_FUNCTION_EXIT = 2,
DTRACE_PROBE_SYSCALL_ENTRY = 3,
DTRACE_PROBE_SYSCALL_EXIT = 4,
DTRACE_PROBE_IPC_SEND = 5,
DTRACE_PROBE_IPC_RECEIVE = 6,
DTRACE_PROBE_VM_FAULT = 7,
DTRACE_PROBE_THREAD_SWITCH = 8,
DTRACE_PROBE_TIMER_TICK = 9,
DTRACE_PROBE_CUSTOM = 10
} dtrace_probe_type_t;
typedef struct dtrace_event {
uint32_t probe_id;
uint64_t timestamp;
uint32_t cpu_id;
uint32_t thread_id;
uint32_t task_id;
uint64_t args[6];
} dtrace_event_t;
typedef struct dtrace_probe {
uint32_t id;
dtrace_probe_type_t type;
const char *name;
const char *function;
int enabled;
uint64_t fire_count;
uint64_t total_time;
uint32_t arg_count;
void *handler;
} dtrace_probe_t;
typedef struct dtrace_metrics {
uint64_t total_probes_fired;
uint64_t total_events_captured;
uint64_t buffer_overruns;
uint64_t probe_overhead_ns;
uint32_t active_probes;
uint32_t max_probes;
} dtrace_metrics_t;
static const char *probe_type_names[] = {
"unknown",
"function_entry",
"function_exit",
"syscall_entry",
"syscall_exit",
"ipc_send",
"ipc_receive",
"vm_fault",
"thread_switch",
"timer_tick",
"custom"
};
static void format_timestamp(uint64_t ns, char *buf, size_t len) {
uint64_t seconds = ns / 1000000000ULL;
uint64_t ns_part = ns % 1000000000ULL;
uint64_t ms_part = ns_part / 1000000ULL;
uint64_t us_part = (ns_part % 1000000ULL) / 1000ULL;
snprintf(buf, len, "%llu.%03llu.%03llu",
(unsigned long long)seconds,
(unsigned long long)ms_part,
(unsigned long long)us_part);
}
static const char *get_probe_type_name(dtrace_probe_type_t type) {
if (type >= 0 && type <= DTRACE_PROBE_CUSTOM) {
return probe_type_names[type];
}
return "invalid";
}
static void analyze_events(dtrace_event_t *events, int count) {
printf("\n=== DTrace Event Analysis ===\n");
printf("Total events: %d\n\n", count);
if (count == 0) {
printf("No events to analyze.\n");
return;
}
int type_counts[11] = {0};
uint64_t earliest = events[0].timestamp;
uint64_t latest = events[0].timestamp;
for (int i = 0; i < count; i++) {
dtrace_event_t *event = &events[i];
if (event->probe_id >= 1 && event->probe_id <= 3) {
int type = event->probe_id + 7;
if (type < 11) type_counts[type]++;
}
if (event->timestamp < earliest) earliest = event->timestamp;
if (event->timestamp > latest) latest = event->timestamp;
}
char start_time[64], end_time[64];
format_timestamp(earliest, start_time, sizeof(start_time));
format_timestamp(latest, end_time, sizeof(end_time));
printf("Time range: %s to %s\n", start_time, end_time);
printf("Duration: %llu.%03llu ms\n\n",
(unsigned long long)((latest - earliest) / 1000000ULL),
(unsigned long long)(((latest - earliest) % 1000000ULL) / 1000ULL));
printf("Events by type:\n");
for (int i = 1; i < 11; i++) {
if (type_counts[i] > 0) {
printf("  %-15s: %d\n", get_probe_type_name(i), type_counts[i]);
}
}
printf("\n=== Recent Events (last 10) ===\n");
printf("%-8s %-15s %-18s %-8s %-8s %-8s\n",
"ProbeID", "Type", "Timestamp", "CPU", "Thread", "Task");
printf("------------------------------------------------------------------------\n");
int start = (count > 10) ? count - 10 : 0;
for (int i = start; i < count; i++) {
dtrace_event_t *event = &events[i];
char ts_buf[32];
format_timestamp(event->timestamp, ts_buf, sizeof(ts_buf));
printf("%-8u %-15s %-18s %-8u %-8u %-8u\n",
event->probe_id,
"probe",
ts_buf,
event->cpu_id,
event->thread_id,
event->task_id);
if (event->args[0] != 0 || event->args[1] != 0) {
printf("         args: 0x%llx 0x%llx\n",
(unsigned long long)event->args[0],
(unsigned long long)event->args[1]);
}
}
}
static void show_help(const char *progname) {
printf("DTrace Analysis Tool for GNU Mach\n");
printf("Usage: %s [options]\n\n", progname);
printf("Options:\n");
printf("  -h, --help          Show this help message\n");
printf("  -v, --verbose       Verbose output\n");
printf("  -c, --continuous    Continuous monitoring mode\n");
printf("  -n, --count N       Number of events to capture (default: 100)\n");
printf("  -t, --timeout T     Timeout in seconds for continuous mode\n");
printf("\nNote: This tool requires kernel DTrace support to be enabled.\n");
printf("The kernel must be built with MACH_DTRACE=1.\n");
}
static int read_kernel_events(dtrace_event_t *events, int max_events) {
static int first_call = 1;
if (first_call) {
printf("NOTE: This is a demonstration version.\n");
printf("In a real implementation, this would read data from the kernel.\n\n");
first_call = 0;
}
int count = (max_events > 5) ? 5 : max_events;
uint64_t base_time = 1000000000ULL;
for (int i = 0; i < count; i++) {
events[i].probe_id = (i % 3) + 1;
events[i].timestamp = base_time + (i * 1000000ULL);
events[i].cpu_id = 0;
events[i].thread_id = 0x1000 + i;
events[i].task_id = 0x2000;
events[i].args[0] = 0xdeadbeef + i;
events[i].args[1] = 0xcafebabe + i;
for (int j = 2; j < 6; j++) {
events[i].args[j] = 0;
}
}
return count;
}
static int read_probe_info(dtrace_probe_t *probes, int max_probes) {
if (max_probes >= 3) {
strcpy((char*)&probes[0], "thread_switch");
probes[0].id = 1;
probes[0].type = DTRACE_PROBE_THREAD_SWITCH;
probes[0].enabled = 1;
probes[0].fire_count = 42;
strcpy((char*)&probes[1], "ipc_send");
probes[1].id = 2;
probes[1].type = DTRACE_PROBE_IPC_SEND;
probes[1].enabled = 1;
probes[1].fire_count = 123;
strcpy((char*)&probes[2], "vm_fault");
probes[2].id = 3;
probes[2].type = DTRACE_PROBE_VM_FAULT;
probes[2].enabled = 1;
probes[2].fire_count = 7;
return 3;
}
return 0;
}
static void show_probe_info(void) {
dtrace_probe_t probes[10];
int count = read_probe_info(probes, 10);
printf("\n=== Active Probes ===\n");
printf("%-4s %-15s %-15s %-8s %-10s\n",
"ID", "Name", "Type", "Enabled", "Fires");
printf("-------------------------------------------------------\n");
for (int i = 0; i < count; i++) {
printf("%-4u %-15s %-15s %-8s %-10llu\n",
probes[i].id,
"probe_name",
get_probe_type_name(probes[i].type),
probes[i].enabled ? "yes" : "no",
(unsigned long long)probes[i].fire_count);
}
}
int main(int argc, char *argv[]) {
int verbose = 0;
int continuous = 0;
int max_events = 100;
int timeout = 10;
for (int i = 1; i < argc; i++) {
if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
show_help(argv[0]);
return 0;
} else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
verbose = 1;
} else if (strcmp(argv[i], "-c") == 0 || strcmp(argv[i], "--continuous") == 0) {
continuous = 1;
} else if (strcmp(argv[i], "-n") == 0 || strcmp(argv[i], "--count") == 0) {
if (i + 1 < argc) {
max_events = atoi(argv[++i]);
if (max_events <= 0) max_events = 100;
}
} else if (strcmp(argv[i], "-t") == 0 || strcmp(argv[i], "--timeout") == 0) {
if (i + 1 < argc) {
timeout = atoi(argv[++i]);
if (timeout <= 0) timeout = 10;
}
} else {
fprintf(stderr, "Unknown option: %s\n", argv[i]);
show_help(argv[0]);
return 1;
}
}
printf("GNU Mach DTrace Analysis Tool\n");
printf("==============================\n");
if (verbose) {
printf("Configuration:\n");
printf("  Max events: %d\n", max_events);
printf("  Continuous: %s\n", continuous ? "yes" : "no");
printf("  Timeout: %d seconds\n", timeout);
printf("\n");
}
show_probe_info();
dtrace_event_t *events = malloc(max_events * sizeof(dtrace_event_t));
if (!events) {
fprintf(stderr, "Failed to allocate event buffer\n");
return 1;
}
int event_count = read_kernel_events(events, max_events);
if (event_count > 0) {
analyze_events(events, event_count);
} else {
printf("No events available.\n");
}
free(events);
printf("\nAnalysis complete.\n");
return 0;
}