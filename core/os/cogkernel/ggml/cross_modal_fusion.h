#ifndef CROSS_MODAL_FUSION_H
#define CROSS_MODAL_FUSION_H
#include "cognitive_kernels.h"
#include "tensor_signatures.h"
#include "atomspace_bridge.h"
#include <stdint.h>
#include <stdbool.h>
typedef enum {
COGNITIVE_SUBSYSTEM_MEMORY,
COGNITIVE_SUBSYSTEM_REASONING,
COGNITIVE_SUBSYSTEM_LEARNING,
COGNITIVE_SUBSYSTEM_ATTENTION,
COGNITIVE_SUBSYSTEM_COUNT
} cognitive_subsystem_t;
typedef enum {
FUSION_STRATEGY_SEQUENTIAL,
FUSION_STRATEGY_PARALLEL,
FUSION_STRATEGY_HIERARCHICAL,
FUSION_STRATEGY_ADAPTIVE,
FUSION_STRATEGY_COUNT
} fusion_strategy_t;
typedef struct {
cognitive_tensor_t* memory_state;
cognitive_tensor_t* reasoning_state;
cognitive_tensor_t* learning_state;
cognitive_tensor_t* attention_state;
fusion_strategy_t strategy;
float subsystem_weights[COGNITIVE_SUBSYSTEM_COUNT];
uint32_t fusion_cycles;
float convergence_threshold;
uint64_t total_fusions;
uint64_t successful_fusions;
uint64_t total_time_ns;
float average_confidence;
void* atomspace_handle;
void* pln_engine_handle;
void* moses_optimizer_handle;
void* ecan_allocator_handle;
} cross_modal_fusion_context_t;
typedef struct {
cognitive_tensor_t* unified_representation;
float confidence_score;
uint64_t processing_time_ns;
uint32_t subsystems_converged;
bool convergence_achieved;
float memory_contribution;
float reasoning_contribution;
float learning_contribution;
float attention_contribution;
cognitive_tensor_t* feedback_tensor;
} fusion_result_t;
typedef struct {
cognitive_tensor_t* updated_reasoning_strategy;
cognitive_tensor_t* evolved_program;
float fitness_improvement;
uint32_t learning_iterations;
bool strategy_evolved;
} feedback_loop_result_t;
cross_modal_fusion_context_t* fusion_context_init(
fusion_strategy_t strategy,
cognitive_kernel_config_t config
);
void fusion_context_destroy(cross_modal_fusion_context_t* context);
cognitive_result_t fusion_create_shared_representation(
cross_modal_fusion_context_t* context,
cognitive_tensor_t** input_tensors,
size_t tensor_count,
cognitive_kernel_config_t config
);
feedback_loop_result_t fusion_pln_moses_feedback(
cross_modal_fusion_context_t* context,
cognitive_tensor_t* reasoning_result,
cognitive_tensor_t* current_program,
cognitive_kernel_config_t config
);
cognitive_result_t fusion_meta_learning(
cross_modal_fusion_context_t* context,
cognitive_tensor_t** reasoning_history,
size_t history_size,
cognitive_kernel_config_t config
);
cognitive_result_t fusion_evolve_reasoning_strategy(
cross_modal_fusion_context_t* context,
cognitive_tensor_t* strategy_genome,
cognitive_tensor_t* performance_data,
cognitive_kernel_config_t config
);
fusion_result_t fusion_unified_process(
cross_modal_fusion_context_t* context,
cognitive_tensor_t* input,
cognitive_kernel_config_t config
);
void fusion_update_subsystem_weight(
cross_modal_fusion_context_t* context,
cognitive_subsystem_t subsystem,
float performance_delta
);
void fusion_get_statistics(
cross_modal_fusion_context_t* context,
uint64_t* total_fusions,
uint64_t* successful_fusions,
float* average_confidence,
float* average_latency_ms
);
void fusion_reset_statistics(cross_modal_fusion_context_t* context);
void fusion_result_destroy(fusion_result_t* result);
void feedback_loop_result_destroy(feedback_loop_result_t* result);
#endif