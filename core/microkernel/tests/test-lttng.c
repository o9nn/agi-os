#include <testlib.h>
#include <mach/lttng.h>
static char *test_name = "tracing";
static void
test_tracing_basic()
{
mach_trace_init();
assert(!mach_trace_is_enabled());
mach_trace_enable(TRUE);
assert(mach_trace_is_enabled());
mach_trace_event(MACH_TRACE_KERN, MACH_TRACE_LEVEL_INFO,
MACH_TRACE_EVENT_KERN_BASE + 100,
"Test event #%d", 1);
mach_trace_event(MACH_TRACE_IPC, MACH_TRACE_LEVEL_DEBUG,
MACH_TRACE_EVENT_IPC_BASE + 100,
"IPC test event");
mach_trace_event(MACH_TRACE_SCHED, MACH_TRACE_LEVEL_INFO,
MACH_TRACE_EVENT_SCHED_BASE + 100,
"Scheduler test event");
printf("Generated 3 test trace events\n");
mach_trace_print_stats();
mach_trace_enable(FALSE);
assert(!mach_trace_is_enabled());
printf("Basic tracing test passed\n");
}
static void
test_tracepoint_macros()
{
mach_trace_enable(TRUE);
TRACE_KERN(startup);
TRACE_IPC(msg_send);
TRACE_SCHED(thread_switch);
TRACE_VM(page_fault);
TRACE_DEBUG(warning);
printf("Generated 5 tracepoint events\n");
mach_trace_print_stats();
mach_trace_enable(FALSE);
printf("Tracepoint macro test passed\n");
}
static void
test_tracing_performance()
{
int i;
uint64_t start_time, end_time;
mach_trace_enable(TRUE);
start_time = timer_read();
for (i = 0; i < 1000; i++) {
mach_trace_event(MACH_TRACE_KERN, MACH_TRACE_LEVEL_DEBUG,
MACH_TRACE_EVENT_KERN_BASE + 200,
"Performance test event %d", i);
}
end_time = timer_read();
printf("Generated 1000 events in %llu timer ticks\n",
end_time - start_time);
mach_trace_print_stats();
mach_trace_enable(FALSE);
printf("Performance test completed\n");
}
static void
test_tracing_disabled()
{
uint64_t start_time, end_time;
int i;
mach_trace_enable(FALSE);
assert(!mach_trace_is_enabled());
start_time = timer_read();
for (i = 0; i < 1000; i++) {
mach_trace_event(MACH_TRACE_KERN, MACH_TRACE_LEVEL_DEBUG,
MACH_TRACE_EVENT_KERN_BASE + 300,
"Disabled event %d", i);
}
end_time = timer_read();
printf("1000 disabled trace calls took %llu timer ticks\n",
end_time - start_time);
printf("Disabled tracing test passed\n");
}
int
main(int argc, char *argv[], int envc, char *envp[])
{
int err = 0;
printf("Starting LTTng-style tracing tests...\n");
test_tracing_basic();
test_tracepoint_macros();
test_tracing_performance();
test_tracing_disabled();
printf("All tracing tests passed!\n");
printf("%s: PASS\n", test_name);
return 0;
}