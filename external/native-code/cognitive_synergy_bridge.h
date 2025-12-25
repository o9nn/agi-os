/**
 * @file cognitive_synergy_bridge.h
 * @brief AGI-OS Cognitive Synergy Bridge
 * 
 * This header provides the unified interface for integrating multiple
 * cognitive computing systems into a coherent AGI-OS framework.
 * 
 * Components integrated:
 * - OpenNARS: Non-Axiomatic Reasoning System
 * - Hyperon/MeTTa: Meta Type Talk language and AtomSpace
 * - GGML: Tensor operations for neural processing
 * - DAS: Distributed AtomSpace for scalable knowledge
 * - Soar: Cognitive architecture patterns
 * - Inferno: Distributed OS primitives
 * 
 * @copyright AGI-OS Project
 * @license GPL-3.0
 */

#ifndef AGI_OS_COGNITIVE_SYNERGY_BRIDGE_H
#define AGI_OS_COGNITIVE_SYNERGY_BRIDGE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* ============================================================================
 * Version Information
 * ============================================================================ */

#define AGI_OS_VERSION_MAJOR 1
#define AGI_OS_VERSION_MINOR 0
#define AGI_OS_VERSION_PATCH 0
#define AGI_OS_VERSION_STRING "1.0.0"

/* ============================================================================
 * Core Types
 * ============================================================================ */

/**
 * @brief Cognitive atom handle - universal knowledge unit
 */
typedef uint64_t cog_atom_t;

/**
 * @brief Truth value for probabilistic/fuzzy reasoning
 */
typedef struct {
    float strength;     /**< Confidence in truth (0.0 - 1.0) */
    float confidence;   /**< Amount of evidence (0.0 - 1.0) */
} cog_truth_value_t;

/**
 * @brief Attention value for ECAN (Economic Attention Networks)
 */
typedef struct {
    int16_t sti;        /**< Short-term importance */
    int16_t lti;        /**< Long-term importance */
    int16_t vlti;       /**< Very long-term importance */
} cog_attention_value_t;

/**
 * @brief Cognitive event for inter-system communication
 */
typedef struct {
    uint64_t timestamp;
    uint32_t event_type;
    cog_atom_t source;
    cog_atom_t target;
    void* payload;
    size_t payload_size;
} cog_event_t;

/**
 * @brief Cognitive context for processing
 */
typedef struct cog_context cog_context_t;

/* ============================================================================
 * Initialization and Lifecycle
 * ============================================================================ */

/**
 * @brief Initialize the cognitive synergy bridge
 * @return 0 on success, negative error code on failure
 */
int cog_bridge_init(void);

/**
 * @brief Shutdown the cognitive synergy bridge
 */
void cog_bridge_shutdown(void);

/**
 * @brief Create a new cognitive context
 * @param name Context identifier
 * @return Context handle or NULL on failure
 */
cog_context_t* cog_context_create(const char* name);

/**
 * @brief Destroy a cognitive context
 * @param ctx Context to destroy
 */
void cog_context_destroy(cog_context_t* ctx);

/* ============================================================================
 * Knowledge Representation (AtomSpace Integration)
 * ============================================================================ */

/**
 * @brief Create a concept node
 * @param ctx Cognitive context
 * @param name Concept name
 * @return Atom handle
 */
cog_atom_t cog_create_concept(cog_context_t* ctx, const char* name);

/**
 * @brief Create a predicate node
 * @param ctx Cognitive context
 * @param name Predicate name
 * @return Atom handle
 */
cog_atom_t cog_create_predicate(cog_context_t* ctx, const char* name);

/**
 * @brief Create an inheritance link
 * @param ctx Cognitive context
 * @param child Child concept
 * @param parent Parent concept
 * @param tv Truth value
 * @return Link atom handle
 */
cog_atom_t cog_create_inheritance(cog_context_t* ctx, 
                                   cog_atom_t child, 
                                   cog_atom_t parent,
                                   cog_truth_value_t tv);

/**
 * @brief Create an evaluation link
 * @param ctx Cognitive context
 * @param predicate Predicate atom
 * @param arguments Array of argument atoms
 * @param arg_count Number of arguments
 * @param tv Truth value
 * @return Link atom handle
 */
cog_atom_t cog_create_evaluation(cog_context_t* ctx,
                                  cog_atom_t predicate,
                                  cog_atom_t* arguments,
                                  size_t arg_count,
                                  cog_truth_value_t tv);

/* ============================================================================
 * Reasoning (NARS/PLN Integration)
 * ============================================================================ */

/**
 * @brief Perform forward inference
 * @param ctx Cognitive context
 * @param premise Input premise atom
 * @param max_steps Maximum inference steps
 * @return Number of conclusions derived
 */
int cog_forward_inference(cog_context_t* ctx, 
                          cog_atom_t premise, 
                          int max_steps);

/**
 * @brief Perform backward inference (goal-directed)
 * @param ctx Cognitive context
 * @param goal Goal atom to prove
 * @param max_steps Maximum inference steps
 * @return Truth value of goal (or NULL if not provable)
 */
cog_truth_value_t* cog_backward_inference(cog_context_t* ctx,
                                           cog_atom_t goal,
                                           int max_steps);

/**
 * @brief Submit a NARS task
 * @param ctx Cognitive context
 * @param narsese Narsese string representation
 * @return Task handle or 0 on failure
 */
uint64_t cog_nars_submit(cog_context_t* ctx, const char* narsese);

/* ============================================================================
 * Neural Processing (GGML Integration)
 * ============================================================================ */

/**
 * @brief Create a tensor for neural processing
 * @param ctx Cognitive context
 * @param dims Dimension array
 * @param ndims Number of dimensions
 * @return Tensor handle
 */
uint64_t cog_tensor_create(cog_context_t* ctx, 
                           const int64_t* dims, 
                           int ndims);

/**
 * @brief Embed an atom into vector space
 * @param ctx Cognitive context
 * @param atom Atom to embed
 * @param embedding Output embedding vector
 * @param dim Embedding dimension
 * @return 0 on success
 */
int cog_atom_embed(cog_context_t* ctx,
                   cog_atom_t atom,
                   float* embedding,
                   size_t dim);

/**
 * @brief Compute attention weights using neural attention
 * @param ctx Cognitive context
 * @param query Query atom
 * @param keys Array of key atoms
 * @param num_keys Number of keys
 * @param weights Output attention weights
 * @return 0 on success
 */
int cog_neural_attention(cog_context_t* ctx,
                         cog_atom_t query,
                         cog_atom_t* keys,
                         size_t num_keys,
                         float* weights);

/* ============================================================================
 * MeTTa Integration
 * ============================================================================ */

/**
 * @brief Execute MeTTa code
 * @param ctx Cognitive context
 * @param metta_code MeTTa source code
 * @return Result atom or 0 on failure
 */
cog_atom_t cog_metta_eval(cog_context_t* ctx, const char* metta_code);

/**
 * @brief Load MeTTa module
 * @param ctx Cognitive context
 * @param module_path Path to MeTTa module
 * @return 0 on success
 */
int cog_metta_load_module(cog_context_t* ctx, const char* module_path);

/* ============================================================================
 * Distributed AtomSpace (DAS Integration)
 * ============================================================================ */

/**
 * @brief Connect to distributed AtomSpace
 * @param ctx Cognitive context
 * @param endpoint DAS endpoint URL
 * @return 0 on success
 */
int cog_das_connect(cog_context_t* ctx, const char* endpoint);

/**
 * @brief Query distributed AtomSpace
 * @param ctx Cognitive context
 * @param query Query pattern
 * @param results Output array for results
 * @param max_results Maximum results to return
 * @return Number of results found
 */
int cog_das_query(cog_context_t* ctx,
                  const char* query,
                  cog_atom_t* results,
                  size_t max_results);

/* ============================================================================
 * Cognitive Cycle (Soar-inspired)
 * ============================================================================ */

/**
 * @brief Run one cognitive cycle
 * @param ctx Cognitive context
 * @return 0 on success, positive for state change, negative for error
 */
int cog_cycle_step(cog_context_t* ctx);

/**
 * @brief Run cognitive cycles until quiescence
 * @param ctx Cognitive context
 * @param max_cycles Maximum cycles to run
 * @return Number of cycles executed
 */
int cog_cycle_run(cog_context_t* ctx, int max_cycles);

/**
 * @brief Set cognitive goal
 * @param ctx Cognitive context
 * @param goal Goal atom
 * @return 0 on success
 */
int cog_set_goal(cog_context_t* ctx, cog_atom_t goal);

/* ============================================================================
 * Event System
 * ============================================================================ */

/**
 * @brief Event callback type
 */
typedef void (*cog_event_callback_t)(cog_context_t* ctx, 
                                      const cog_event_t* event,
                                      void* user_data);

/**
 * @brief Register event callback
 * @param ctx Cognitive context
 * @param event_type Event type to listen for (0 for all)
 * @param callback Callback function
 * @param user_data User data passed to callback
 * @return 0 on success
 */
int cog_register_callback(cog_context_t* ctx,
                          uint32_t event_type,
                          cog_event_callback_t callback,
                          void* user_data);

/**
 * @brief Emit cognitive event
 * @param ctx Cognitive context
 * @param event Event to emit
 * @return 0 on success
 */
int cog_emit_event(cog_context_t* ctx, const cog_event_t* event);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * @brief Get atom name/string representation
 * @param ctx Cognitive context
 * @param atom Atom handle
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Length of name or negative on error
 */
int cog_atom_to_string(cog_context_t* ctx,
                       cog_atom_t atom,
                       char* buffer,
                       size_t buffer_size);

/**
 * @brief Get truth value of atom
 * @param ctx Cognitive context
 * @param atom Atom handle
 * @return Truth value
 */
cog_truth_value_t cog_get_truth_value(cog_context_t* ctx, cog_atom_t atom);

/**
 * @brief Set truth value of atom
 * @param ctx Cognitive context
 * @param atom Atom handle
 * @param tv New truth value
 * @return 0 on success
 */
int cog_set_truth_value(cog_context_t* ctx, 
                        cog_atom_t atom, 
                        cog_truth_value_t tv);

/**
 * @brief Get attention value of atom
 * @param ctx Cognitive context
 * @param atom Atom handle
 * @return Attention value
 */
cog_attention_value_t cog_get_attention_value(cog_context_t* ctx, 
                                               cog_atom_t atom);

/**
 * @brief Stimulate atom (increase STI)
 * @param ctx Cognitive context
 * @param atom Atom handle
 * @param stimulus Stimulus amount
 * @return 0 on success
 */
int cog_stimulate(cog_context_t* ctx, cog_atom_t atom, int16_t stimulus);

#ifdef __cplusplus
}
#endif

#endif /* AGI_OS_COGNITIVE_SYNERGY_BRIDGE_H */
