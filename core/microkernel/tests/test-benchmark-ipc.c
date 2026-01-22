#include <mach/message.h>
#include <mach/mach_types.h>
#include <mach/vm_param.h>
#include <syscalls.h>
#include <testlib.h>
#include <mach.user.h>
#include <mach_port.user.h>
static mach_port_t test_port;
void port_allocation_test(void *arg)
{
mach_port_t port;
kern_return_t kr;
kr = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &port);
if (kr == KERN_SUCCESS) {
mach_port_deallocate(mach_task_self(), port);
}
}
void message_send_test(void *arg)
{
mach_msg_header_t msg;
kern_return_t kr;
msg.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, 0);
msg.msgh_size = sizeof(msg);
msg.msgh_remote_port = test_port;
msg.msgh_local_port = MACH_PORT_NULL;
msg.msgh_id = 1000;
kr = mach_msg(&msg, MACH_SEND_MSG | MACH_SEND_TIMEOUT,
sizeof(msg), 0, MACH_PORT_NULL, 100, MACH_PORT_NULL);
}
void benchmark_port_operations(void)
{
benchmark_t bench;
printf("=== IPC Port Operations Benchmark ===\n");
benchmark_start(&bench, "Port Allocation/Deallocation");
benchmark_iterations(&bench, 1000, port_allocation_test, NULL);
benchmark_end(&bench);
benchmark_report(&bench, "port ops/sec");
printf("Port allocation benchmark completed\n");
}
void benchmark_message_operations(void)
{
benchmark_t bench;
kern_return_t kr;
printf("=== IPC Message Operations Benchmark ===\n");
kr = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &test_port);
ASSERT_RET(kr, "failed to create test port");
benchmark_start(&bench, "Message Send Operations");
benchmark_iterations(&bench, 500, message_send_test, NULL);
benchmark_end(&bench);
benchmark_report(&bench, "messages/sec");
mach_port_deallocate(mach_task_self(), test_port);
printf("Message operations benchmark completed\n");
}
void benchmark_task_info_operations(void)
{
benchmark_t bench;
struct task_basic_info info;
mach_msg_type_number_t count;
kern_return_t kr;
printf("=== Task Info Operations Benchmark ===\n");
benchmark_start(&bench, "Task Info Retrieval");
for (uint64_t i = 0; i < 1000; i++) {
count = TASK_BASIC_INFO_COUNT;
kr = task_info(mach_task_self(), TASK_BASIC_INFO,
(task_info_t)&info, &count);
if (kr != KERN_SUCCESS) break;
}
bench.iterations = 1000;
benchmark_end(&bench);
benchmark_report(&bench, "task_info calls/sec");
printf("Task info benchmark completed\n");
}
int main(int argc, char *argv[], int envc, char *envp[])
{
printf("Starting IPC Performance Benchmarks\n");
benchmark_port_operations();
benchmark_message_operations();
benchmark_task_info_operations();
printf("All IPC benchmarks completed successfully\n");
return 0;
}