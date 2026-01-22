#ifndef _MACH5_ZERO_COPY_IPC_H_
#define _MACH5_ZERO_COPY_IPC_H_
#include <mach/mach.h>
#include <mach/vm_map.h>
#include <mach/vm_param.h>
#define ZCOPY_IPC_MIN_SIZE 4096
#define ZCOPY_IPC_MAX_SIZE (64*1024)
#define ZCOPY_IPC_MAX_PAGES 16
typedef enum {
ZCOPY_MSG_SMALL,
ZCOPY_MSG_LARGE,
ZCOPY_MSG_SHARED,
ZCOPY_MSG_MAPPED
} zcopy_message_type_t;
typedef struct {
vm_address_t page_addr;
vm_size_t page_size;
vm_prot_t protection;
unsigned int ref_count;
boolean_t is_wired;
} zcopy_page_desc_t;
typedef struct {
mach_msg_header_t header;
zcopy_message_type_t msg_type;
unsigned int page_count;
vm_size_t total_size;
unsigned int checksum;
zcopy_page_desc_t pages[ZCOPY_IPC_MAX_PAGES];
} zcopy_message_header_t;
typedef struct {
task_t sender_task;
task_t receiver_task;
mach_port_t shared_port;
vm_address_t shared_base;
vm_size_t shared_size;
unsigned int *allocation_bitmap;
unsigned long total_messages;
unsigned long zero_copy_messages;
unsigned long bytes_saved;
double avg_latency_us;
} zcopy_ipc_context_t;
kern_return_t zcopy_ipc_init(zcopy_ipc_context_t *context,
task_t sender,
task_t receiver);
kern_return_t zcopy_ipc_cleanup(zcopy_ipc_context_t *context);
kern_return_t zcopy_allocate_shared_memory(zcopy_ipc_context_t *context,
vm_size_t size);
kern_return_t zcopy_deallocate_shared_memory(zcopy_ipc_context_t *context);
vm_address_t zcopy_allocate_pages(zcopy_ipc_context_t *context,
unsigned int page_count);
kern_return_t zcopy_deallocate_pages(zcopy_ipc_context_t *context,
vm_address_t addr,
unsigned int page_count);
kern_return_t zcopy_send_message(zcopy_ipc_context_t *context,
void *data,
vm_size_t size,
mach_port_t dest_port);
kern_return_t zcopy_receive_message(zcopy_ipc_context_t *context,
zcopy_message_header_t *msg,
vm_size_t *received_size);
kern_return_t zcopy_map_pages_to_receiver(zcopy_ipc_context_t *context,
zcopy_page_desc_t *pages,
unsigned int page_count);
kern_return_t zcopy_unmap_pages_from_sender(zcopy_ipc_context_t *context,
zcopy_page_desc_t *pages,
unsigned int page_count);
kern_return_t zcopy_decide_mechanism(vm_size_t message_size,
zcopy_message_type_t *recommended_type);
kern_return_t zcopy_optimize_page_alignment(void *data,
vm_size_t size,
vm_address_t *aligned_addr,
vm_size_t *aligned_size);
unsigned int zcopy_calculate_checksum(const void *data, vm_size_t size);
kern_return_t zcopy_verify_message_integrity(const zcopy_message_header_t *msg);
kern_return_t zcopy_enforce_access_control(zcopy_ipc_context_t *context,
task_t requester_task);
kern_return_t zcopy_measure_performance(zcopy_ipc_context_t *context,
vm_size_t message_size,
unsigned int iterations,
double *latency_us,
double *bandwidth_mbps);
void zcopy_print_statistics(const zcopy_ipc_context_t *context);
boolean_t zcopy_is_page_aligned(vm_address_t addr);
vm_address_t zcopy_align_to_page(vm_address_t addr);
vm_size_t zcopy_round_to_page(vm_size_t size);
kern_return_t zcopy_setup_cow_mapping(zcopy_ipc_context_t *context,
vm_address_t src_addr,
vm_size_t size,
vm_address_t *cow_addr);
typedef struct {
vm_address_t addr;
vm_size_t size;
} zcopy_sg_element_t;
kern_return_t zcopy_send_scatter_gather(zcopy_ipc_context_t *context,
zcopy_sg_element_t *elements,
unsigned int element_count,
mach_port_t dest_port);
typedef void (*zcopy_completion_callback_t)(kern_return_t result, void *context);
kern_return_t zcopy_send_async(zcopy_ipc_context_t *context,
void *data,
vm_size_t size,
mach_port_t dest_port,
zcopy_completion_callback_t callback,
void *callback_context);
typedef struct {
double traditional_latency_us;
double zero_copy_latency_us;
double latency_improvement;
unsigned long traditional_copies;
unsigned long zero_copy_copies;
double copy_reduction;
vm_size_t traditional_memory_usage;
vm_size_t zero_copy_memory_usage;
double memory_efficiency;
} zcopy_comparison_result_t;
kern_return_t zcopy_compare_with_traditional(zcopy_ipc_context_t *context,
vm_size_t message_size,
unsigned int iterations,
zcopy_comparison_result_t *result);
typedef struct {
unsigned int total_pages;
unsigned int allocated_pages;
unsigned int free_pages;
unsigned int fragmented_pages;
double fragmentation_ratio;
} zcopy_fragmentation_info_t;
kern_return_t zcopy_analyze_fragmentation(zcopy_ipc_context_t *context,
zcopy_fragmentation_info_t *info);
kern_return_t zcopy_test_multicore_scalability(zcopy_ipc_context_t *context,
unsigned int num_cores,
vm_size_t message_size,
double *scalability_factor);
#endif