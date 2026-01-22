#ifndef HURD_COGNITIVE_API_H
#define HURD_COGNITIVE_API_H
#include "cognitive_kernels.h"
#include "tensor_signatures.h"
#include <stdint.h>
#include <stdbool.h>
typedef enum {
HURD_PRIMITIVE_PORT,
HURD_PRIMITIVE_TASK,
HURD_PRIMITIVE_THREAD,
HURD_PRIMITIVE_MEMORY,
HURD_PRIMITIVE_IPC,
HURD_PRIMITIVE_TRANSLATOR,
HURD_PRIMITIVE_COUNT
} hurd_primitive_type_t;
struct hurd_primitive {
hurd_primitive_type_t type;
uint64_t handle_id;
void* native_handle;
cognitive_tensor_t* cognitive_state;
bool is_cognitive;
};
typedef enum {
HURD_COGNITIVE_OP_ALLOCATE,
HURD_COGNITIVE_OP_DEALLOCATE,
HURD_COGNITIVE_OP_COMMUNICATE,
HURD_COGNITIVE_OP_TRANSLATE,
HURD_COGNITIVE_OP_SCHEDULE,
HURD_COGNITIVE_OP_PREDICT,
HURD_COGNITIVE_OP_OPTIMIZE,
HURD_COGNITIVE_OP_COUNT
} hurd_cognitive_op_t;
typedef struct {
uint64_t sender_id;
uint64_t receiver_id;
void* message_data;
size_t message_size;
cognitive_tensor_t* cognitive_context;
float priority;
} hurd_cognitive_message_t;
typedef struct {
float predicted_latency_ms;
float predicted_throughput;
float confidence_score;
cognitive_tensor_t* prediction_basis;
} hurd_performance_prediction_t;
int hurd_cognitive_api_init(void);
void hurd_cognitive_api_shutdown(void);
struct hurd_primitive* hurd_create_cognitive_primitive(
hurd_primitive_type_t type,
void* native_handle
);
void hurd_destroy_cognitive_primitive(struct hurd_primitive* primitive);
int hurd_enable_cognitive(struct hurd_primitive* primitive);
int hurd_disable_cognitive(struct hurd_primitive* primitive);
cognitive_result_t hurd_cognitive_operation(
struct hurd_primitive* primitive,
hurd_cognitive_op_t operation,
cognitive_kernel_config_t config
);
int hurd_integrate_result(cognitive_result_t result);
struct hurd_primitive* hurd_cognitive_port_allocate(
cognitive_tensor_t* allocation_hint
);
int hurd_cognitive_ipc_send(
struct hurd_primitive* sender,
struct hurd_primitive* receiver,
hurd_cognitive_message_t* message
);
hurd_cognitive_message_t* hurd_cognitive_ipc_receive(
struct hurd_primitive* receiver,
cognitive_kernel_config_t config
);
hurd_performance_prediction_t hurd_predict_performance(
struct hurd_primitive* primitive,
hurd_cognitive_op_t operation,
cognitive_kernel_config_t config
);
cognitive_result_t hurd_optimize_operation(
struct hurd_primitive* primitive,
cognitive_tensor_t* historical_data,
cognitive_kernel_config_t config
);
int hurd_learn_from_execution(
struct hurd_primitive* primitive,
hurd_cognitive_op_t operation,
uint64_t execution_time_ns,
bool success
);
cognitive_tensor_t* hurd_get_cognitive_state(
struct hurd_primitive* primitive
);
int hurd_update_cognitive_state(
struct hurd_primitive* primitive,
cognitive_tensor_t* new_state
);
cognitive_result_t hurd_cognitive_memory_manage(
struct hurd_primitive* memory_primitive,
size_t required_size,
cognitive_kernel_config_t config
);
cognitive_result_t hurd_cognitive_schedule(
struct hurd_primitive** threads,
size_t thread_count,
cognitive_kernel_config_t config
);
cognitive_result_t hurd_cognitive_translate(
struct hurd_primitive* translator,
void* input_data,
size_t input_size,
cognitive_kernel_config_t config
);
void hurd_free_cognitive_message(hurd_cognitive_message_t* message);
void hurd_free_performance_prediction(hurd_performance_prediction_t* prediction);
#endif