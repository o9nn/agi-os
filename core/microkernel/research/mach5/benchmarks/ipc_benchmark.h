#ifndef _MACH5_IPC_BENCHMARK_H_
#define _MACH5_IPC_BENCHMARK_H_
#include <mach/mach.h>
#include <mach/message.h>
#include <mach/port.h>
#include <sys/time.h>
#define IPC_BENCHMARK_MAX_ITERATIONS 10000
#define IPC_BENCHMARK_MAX_MESSAGE_SIZE 65536
#define IPC_BENCHMARK_MIN_MESSAGE_SIZE 64
typedef enum {
IPC_BENCH_LATENCY,
IPC_BENCH_THROUGHPUT,
IPC_BENCH_BANDWIDTH,
IPC_BENCH_SCALABILITY,
IPC_BENCH_MEMORY_USAGE
} ipc_benchmark_type_t;
typedef enum {
IPC_MSG_TINY = 64,
IPC_MSG_SMALL = 256,
IPC_MSG_MEDIUM = 4096,
IPC_MSG_LARGE = 16384,
IPC_MSG_HUGE = 65536
} ipc_message_size_t;
typedef struct {
ipc_benchmark_type_t type;
ipc_message_size_t message_size;
unsigned int iterations;
struct timeval start_time;
struct timeval end_time;
unsigned long total_microseconds;
double average_latency_us;
double messages_per_second;
double bytes_per_second;
double cpu_utilization;
unsigned long memory_allocated;
unsigned long memory_peak;
unsigned int page_faults;
unsigned int failed_sends;
unsigned int failed_receives;
unsigned int timeouts;
} ipc_benchmark_result_t;
typedef struct {
mach_port_t server_port;
mach_port_t client_port;
mach_port_t reply_port;
task_t server_task;
task_t client_task;
void *message_buffer;
mach_msg_size_t buffer_size;
unsigned int num_iterations;
ipc_benchmark_type_t test_type;
} ipc_benchmark_context_t;
kern_return_t ipc_benchmark_init(ipc_benchmark_context_t *context,
ipc_benchmark_type_t type,
unsigned int iterations);
kern_return_t ipc_benchmark_cleanup(ipc_benchmark_context_t *context);
kern_return_t ipc_benchmark_latency(ipc_benchmark_context_t *context,
ipc_message_size_t msg_size,
ipc_benchmark_result_t *result);
kern_return_t ipc_benchmark_throughput(ipc_benchmark_context_t *context,
ipc_message_size_t msg_size,
ipc_benchmark_result_t *result);
kern_return_t ipc_benchmark_bandwidth(ipc_benchmark_context_t *context,
ipc_message_size_t msg_size,
ipc_benchmark_result_t *result);
kern_return_t ipc_benchmark_scalability(ipc_benchmark_context_t *context,
unsigned int num_tasks,
ipc_benchmark_result_t *result);
void ipc_benchmark_print_results(const ipc_benchmark_result_t *result);
void ipc_benchmark_save_results(const ipc_benchmark_result_t *result,
const char *filename);
double ipc_benchmark_calculate_latency(const struct timeval *start,
const struct timeval *end,
unsigned int iterations);
kern_return_t ipc_benchmark_memory_analysis(ipc_benchmark_context_t *context,
ipc_benchmark_result_t *result);
kern_return_t ipc_benchmark_zero_copy(ipc_benchmark_context_t *context,
ipc_message_size_t msg_size,
ipc_benchmark_result_t *result);
kern_return_t ipc_benchmark_shared_memory(ipc_benchmark_context_t *context,
ipc_message_size_t msg_size,
ipc_benchmark_result_t *result);
kern_return_t ipc_benchmark_async_ipc(ipc_benchmark_context_t *context,
ipc_message_size_t msg_size,
ipc_benchmark_result_t *result);
typedef struct {
ipc_benchmark_result_t traditional_ipc;
ipc_benchmark_result_t zero_copy_ipc;
ipc_benchmark_result_t shared_memory_ipc;
ipc_benchmark_result_t async_ipc;
double latency_improvement;
double throughput_improvement;
double memory_efficiency;
} ipc_benchmark_comparison_t;
kern_return_t ipc_benchmark_compare_mechanisms(ipc_benchmark_context_t *context,
ipc_message_size_t msg_size,
ipc_benchmark_comparison_t *comparison);
void ipc_benchmark_print_comparison(const ipc_benchmark_comparison_t *comparison);
#endif