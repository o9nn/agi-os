/**
 * Time Crystal Integration for Cognitive Kernels
 * 
 * This header provides the integration layer between the Time Crystal Daemon
 * and the existing cognitive kernel infrastructure in agi-os.
 * 
 * The Time Crystal Daemon implements hierarchical temporal organization
 * based on Nanobrain time crystal models, exposing cognitive services
 * through a typed IDL interface with optional LLM natural language access.
 */

#ifndef TIME_CRYSTAL_INTEGRATION_H
#define TIME_CRYSTAL_INTEGRATION_H

#include "cognitive_kernels.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 * TIME CRYSTAL HIERARCHY CONSTANTS
 * ======================================================================== */

#define TC_LEVEL_QUANTUM_RESONANCE      0   /* 1μs - Quantum effects */
#define TC_LEVEL_PROTEIN_DYNAMICS       1   /* 8ms - Protein channels */
#define TC_LEVEL_ION_CHANNEL_GATING     2   /* 26ms - Ion channels */
#define TC_LEVEL_MEMBRANE_DYNAMICS      3   /* 52ms - Membrane */
#define TC_LEVEL_AXON_INITIAL_SEGMENT   4   /* 110ms - AIS */
#define TC_LEVEL_DENDRITIC_INTEGRATION  5   /* 160ms - Dendrites */
#define TC_LEVEL_SYNAPTIC_PLASTICITY    6   /* 250ms - Synapses */
#define TC_LEVEL_SOMA_PROCESSING        7   /* 330ms - Soma */
#define TC_LEVEL_NETWORK_SYNC           8   /* 500ms - Network */
#define TC_LEVEL_GLOBAL_RHYTHM          9   /* 1s - Global */
#define TC_LEVEL_CIRCADIAN_MODULATION   10  /* 1min - Circadian */
#define TC_LEVEL_HOMEOSTATIC_REGULATION 11  /* 1hr - Homeostatic */
#define TC_LEVEL_COUNT                  12

/* ========================================================================
 * TIME CRYSTAL STRUCTURES
 * ======================================================================== */

/**
 * Time Crystal Level State
 */
typedef struct {
    uint8_t level_id;
    const char* name;
    double period_ms;
    double current_phase;  /* 0.0 to 1.0 */
    uint64_t atom_count;
} tc_level_state_t;

/**
 * Time Crystal Hierarchy State
 */
typedef struct {
    tc_level_state_t levels[TC_LEVEL_COUNT];
    uint64_t start_time_ns;
    uint64_t version;
} tc_hierarchy_state_t;

/**
 * Time Crystal Daemon Configuration
 */
typedef struct {
    const char* socket_path;
    bool enable_llm_interface;
    const char* llm_model;
    cognitive_perf_config_t perf_config;
} tc_daemon_config_t;

/* ========================================================================
 * TIME CRYSTAL DAEMON API
 * ======================================================================== */

/**
 * Initialize the Time Crystal Daemon
 * 
 * @param config Daemon configuration
 * @return 0 on success, negative error code on failure
 */
int tc_daemon_init(tc_daemon_config_t* config);

/**
 * Shutdown the Time Crystal Daemon
 */
void tc_daemon_shutdown(void);

/**
 * Get the current hierarchy state
 * 
 * @param state Output: current hierarchy state
 * @return 0 on success, negative error code on failure
 */
int tc_get_hierarchy_state(tc_hierarchy_state_t* state);

/**
 * Update all oscillator phases
 * 
 * This should be called periodically to advance the time crystal hierarchy.
 * 
 * @return Number of phase transitions that occurred
 */
int tc_update_phases(void);

/**
 * Assign an atom to a time crystal level
 * 
 * @param atom_handle The atom handle
 * @param level_id The target level (0-11)
 * @return 0 on success, negative error code on failure
 */
int tc_assign_atom_level(uint64_t atom_handle, uint8_t level_id);

/**
 * Get the appropriate level for a given period
 * 
 * @param period_ms The desired period in milliseconds
 * @return The closest matching level ID
 */
uint8_t tc_get_level_for_period(double period_ms);

/* ========================================================================
 * COGNITIVE KERNEL INTEGRATION
 * ======================================================================== */

/**
 * Execute a cognitive operation at a specific time crystal level
 * 
 * This function schedules the operation to execute during the appropriate
 * phase of the specified time crystal level.
 * 
 * @param level_id The time crystal level
 * @param operation The cognitive operation to execute
 * @param input Input tensor
 * @param config Kernel configuration
 * @return Result of the operation
 */
cognitive_result_t tc_execute_at_level(
    uint8_t level_id,
    cognitive_result_t (*operation)(cognitive_tensor_t*, cognitive_kernel_config_t),
    cognitive_tensor_t* input,
    cognitive_kernel_config_t config
);

/**
 * Synchronize attention allocation with time crystal hierarchy
 * 
 * This function redistributes attention based on the current phase
 * of each time crystal level, implementing hierarchical attention.
 * 
 * @param attention_tensor The attention tensor to synchronize
 * @return 0 on success, negative error code on failure
 */
int tc_sync_attention(cognitive_tensor_t* attention_tensor);

/* ========================================================================
 * LLM INTERFACE INTEGRATION
 * ======================================================================== */

/**
 * Process a natural language command through the LLM interface
 * 
 * @param query The natural language query
 * @param access_level "technician" or "engineer"
 * @param response Output buffer for the response
 * @param response_size Size of the response buffer
 * @return 0 on success, negative error code on failure
 */
int tc_llm_process_query(
    const char* query,
    const char* access_level,
    char* response,
    size_t response_size
);

/**
 * Check if the LLM interface is available
 * 
 * @return true if LLM interface is enabled and connected
 */
bool tc_llm_is_available(void);

#ifdef __cplusplus
}
#endif

#endif /* TIME_CRYSTAL_INTEGRATION_H */
