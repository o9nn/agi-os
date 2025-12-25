/**
 * @file nars_integration.h
 * @brief OpenNARS Integration for AGI-OS Cognitive Synergy Bridge
 * 
 * This header provides the integration layer between the Cognitive Synergy
 * Bridge and OpenNARS for Applications (ONA).
 * 
 * @copyright AGI-OS Project
 * @license GPL-3.0
 */

#ifndef AGI_OS_NARS_INTEGRATION_H
#define AGI_OS_NARS_INTEGRATION_H

#include "../cognitive_synergy_bridge.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * NARS Configuration
 * ============================================================================ */

/**
 * @brief NARS configuration parameters
 */
typedef struct {
    int decision_threshold;      /**< Threshold for decision making (0-100) */
    int truth_projection_decay;  /**< Truth projection decay rate */
    int anticipation_confidence; /**< Confidence for anticipations */
    int motor_babbling_chance;   /**< Chance for motor babbling (0-100) */
    bool allow_reinforcement;    /**< Enable reinforcement learning */
    bool allow_anticipation;     /**< Enable anticipation */
    bool allow_motor_babbling;   /**< Enable motor babbling */
} nars_config_t;

/**
 * @brief Default NARS configuration
 */
#define NARS_CONFIG_DEFAULT { \
    .decision_threshold = 50, \
    .truth_projection_decay = 1, \
    .anticipation_confidence = 90, \
    .motor_babbling_chance = 10, \
    .allow_reinforcement = true, \
    .allow_anticipation = true, \
    .allow_motor_babbling = false \
}

/* ============================================================================
 * NARS Initialization
 * ============================================================================ */

/**
 * @brief Initialize NARS subsystem
 * @param ctx Cognitive context
 * @param config NARS configuration (NULL for defaults)
 * @return 0 on success, negative on error
 */
int nars_init(cog_context_t* ctx, const nars_config_t* config);

/**
 * @brief Shutdown NARS subsystem
 * @param ctx Cognitive context
 */
void nars_shutdown(cog_context_t* ctx);

/* ============================================================================
 * NARS Input/Output
 * ============================================================================ */

/**
 * @brief Submit Narsese input to NARS
 * @param ctx Cognitive context
 * @param narsese Narsese string
 * @return Task ID or 0 on failure
 */
uint64_t nars_input(cog_context_t* ctx, const char* narsese);

/**
 * @brief Submit a belief to NARS
 * @param ctx Cognitive context
 * @param term Term string
 * @param strength Truth strength (0.0-1.0)
 * @param confidence Truth confidence (0.0-1.0)
 * @return Task ID or 0 on failure
 */
uint64_t nars_add_belief(cog_context_t* ctx, 
                          const char* term,
                          float strength,
                          float confidence);

/**
 * @brief Submit a goal to NARS
 * @param ctx Cognitive context
 * @param term Goal term string
 * @param strength Desire strength (0.0-1.0)
 * @param confidence Desire confidence (0.0-1.0)
 * @return Task ID or 0 on failure
 */
uint64_t nars_add_goal(cog_context_t* ctx,
                        const char* term,
                        float strength,
                        float confidence);

/**
 * @brief Submit a question to NARS
 * @param ctx Cognitive context
 * @param term Question term string
 * @return Task ID or 0 on failure
 */
uint64_t nars_ask(cog_context_t* ctx, const char* term);

/* ============================================================================
 * NARS Reasoning Cycles
 * ============================================================================ */

/**
 * @brief Run NARS reasoning cycles
 * @param ctx Cognitive context
 * @param cycles Number of cycles to run
 * @return Number of derivations made
 */
int nars_cycle(cog_context_t* ctx, int cycles);

/**
 * @brief Run NARS until quiescence or max cycles
 * @param ctx Cognitive context
 * @param max_cycles Maximum cycles
 * @param timeout_ms Timeout in milliseconds (0 for no timeout)
 * @return Number of cycles executed
 */
int nars_run(cog_context_t* ctx, int max_cycles, int timeout_ms);

/* ============================================================================
 * NARS Output Handling
 * ============================================================================ */

/**
 * @brief NARS output types
 */
typedef enum {
    NARS_OUTPUT_DERIVED,     /**< Derived belief */
    NARS_OUTPUT_ANSWER,      /**< Answer to question */
    NARS_OUTPUT_EXECUTION,   /**< Operation execution */
    NARS_OUTPUT_ANTICIPATION /**< Anticipation */
} nars_output_type_t;

/**
 * @brief NARS output structure
 */
typedef struct {
    nars_output_type_t type;
    char* term;
    float strength;
    float confidence;
    uint64_t timestamp;
} nars_output_t;

/**
 * @brief NARS output callback type
 */
typedef void (*nars_output_callback_t)(cog_context_t* ctx,
                                        const nars_output_t* output,
                                        void* user_data);

/**
 * @brief Register NARS output callback
 * @param ctx Cognitive context
 * @param callback Callback function
 * @param user_data User data passed to callback
 * @return 0 on success
 */
int nars_register_output_callback(cog_context_t* ctx,
                                   nars_output_callback_t callback,
                                   void* user_data);

/* ============================================================================
 * NARS-AtomSpace Bridge
 * ============================================================================ */

/**
 * @brief Convert NARS term to AtomSpace atom
 * @param ctx Cognitive context
 * @param narsese Narsese term string
 * @return Atom handle or 0 on failure
 */
cog_atom_t nars_term_to_atom(cog_context_t* ctx, const char* narsese);

/**
 * @brief Convert AtomSpace atom to NARS term
 * @param ctx Cognitive context
 * @param atom Atom handle
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Length of term string or negative on error
 */
int nars_atom_to_term(cog_context_t* ctx,
                       cog_atom_t atom,
                       char* buffer,
                       size_t buffer_size);

/**
 * @brief Synchronize NARS beliefs with AtomSpace
 * @param ctx Cognitive context
 * @param direction 1 = NARS->AtomSpace, -1 = AtomSpace->NARS, 0 = bidirectional
 * @return Number of items synchronized
 */
int nars_sync_atomspace(cog_context_t* ctx, int direction);

/* ============================================================================
 * NARS Operations
 * ============================================================================ */

/**
 * @brief NARS operation callback type
 */
typedef bool (*nars_operation_t)(cog_context_t* ctx,
                                  const char* op_name,
                                  const char** args,
                                  int arg_count,
                                  void* user_data);

/**
 * @brief Register NARS operation
 * @param ctx Cognitive context
 * @param op_name Operation name (e.g., "^left")
 * @param operation Operation callback
 * @param user_data User data passed to callback
 * @return 0 on success
 */
int nars_register_operation(cog_context_t* ctx,
                             const char* op_name,
                             nars_operation_t operation,
                             void* user_data);

/* ============================================================================
 * NARS Statistics
 * ============================================================================ */

/**
 * @brief NARS statistics structure
 */
typedef struct {
    uint64_t total_cycles;
    uint64_t total_inputs;
    uint64_t total_derivations;
    uint64_t total_answers;
    uint64_t total_executions;
    uint64_t concept_count;
    uint64_t belief_count;
    uint64_t goal_count;
} nars_stats_t;

/**
 * @brief Get NARS statistics
 * @param ctx Cognitive context
 * @param stats Output statistics structure
 * @return 0 on success
 */
int nars_get_stats(cog_context_t* ctx, nars_stats_t* stats);

#ifdef __cplusplus
}
#endif

#endif /* AGI_OS_NARS_INTEGRATION_H */
