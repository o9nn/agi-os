/**
 * @file metta_integration.h
 * @brief MeTTa/Hyperon Integration for AGI-OS Cognitive Synergy Bridge
 * 
 * This header provides the integration layer between the Cognitive Synergy
 * Bridge and the Hyperon MeTTa language runtime.
 * 
 * MeTTa (Meta Type Talk) is a type-theoretic programming language designed
 * for AGI, providing meta-programming capabilities and integration with
 * the Distributed AtomSpace (DAS).
 * 
 * @copyright AGI-OS Project
 * @license GPL-3.0
 */

#ifndef AGI_OS_METTA_INTEGRATION_H
#define AGI_OS_METTA_INTEGRATION_H

#include "../cognitive_synergy_bridge.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * MeTTa Configuration
 * ============================================================================ */

/**
 * @brief MeTTa runtime configuration
 */
typedef struct {
    size_t max_atoms;           /**< Maximum atoms in space */
    size_t max_recursion;       /**< Maximum recursion depth */
    int timeout_ms;             /**< Execution timeout in milliseconds */
    bool enable_stdlib;         /**< Load standard library */
    bool enable_das;            /**< Enable DAS integration */
    const char* das_endpoint;   /**< DAS endpoint URL (if enabled) */
} metta_config_t;

/**
 * @brief Default MeTTa configuration
 */
#define METTA_CONFIG_DEFAULT { \
    .max_atoms = 1000000, \
    .max_recursion = 1000, \
    .timeout_ms = 30000, \
    .enable_stdlib = true, \
    .enable_das = false, \
    .das_endpoint = NULL \
}

/* ============================================================================
 * MeTTa Types
 * ============================================================================ */

/**
 * @brief MeTTa atom types
 */
typedef enum {
    METTA_ATOM_SYMBOL,      /**< Symbol atom */
    METTA_ATOM_VARIABLE,    /**< Variable atom */
    METTA_ATOM_EXPRESSION,  /**< Expression (list) atom */
    METTA_ATOM_GROUNDED     /**< Grounded (external) atom */
} metta_atom_type_t;

/**
 * @brief MeTTa space handle
 */
typedef uint64_t metta_space_t;

/**
 * @brief MeTTa atom handle (within MeTTa space)
 */
typedef uint64_t metta_atom_t;

/**
 * @brief MeTTa module handle
 */
typedef uint64_t metta_module_t;

/**
 * @brief MeTTa runner handle
 */
typedef uint64_t metta_runner_t;

/* ============================================================================
 * MeTTa Initialization
 * ============================================================================ */

/**
 * @brief Initialize MeTTa subsystem
 * @param ctx Cognitive context
 * @param config MeTTa configuration (NULL for defaults)
 * @return 0 on success, negative on error
 */
int metta_init(cog_context_t* ctx, const metta_config_t* config);

/**
 * @brief Shutdown MeTTa subsystem
 * @param ctx Cognitive context
 */
void metta_shutdown(cog_context_t* ctx);

/**
 * @brief Get MeTTa version string
 * @return Version string
 */
const char* metta_version(void);

/* ============================================================================
 * MeTTa Space Operations
 * ============================================================================ */

/**
 * @brief Create a new MeTTa space
 * @param ctx Cognitive context
 * @param name Space name (optional)
 * @return Space handle or 0 on failure
 */
metta_space_t metta_space_new(cog_context_t* ctx, const char* name);

/**
 * @brief Free a MeTTa space
 * @param ctx Cognitive context
 * @param space Space handle
 */
void metta_space_free(cog_context_t* ctx, metta_space_t space);

/**
 * @brief Get the default space
 * @param ctx Cognitive context
 * @return Default space handle
 */
metta_space_t metta_space_default(cog_context_t* ctx);

/**
 * @brief Add atom to space
 * @param ctx Cognitive context
 * @param space Space handle
 * @param atom MeTTa atom handle
 * @return 0 on success
 */
int metta_space_add(cog_context_t* ctx, metta_space_t space, metta_atom_t atom);

/**
 * @brief Remove atom from space
 * @param ctx Cognitive context
 * @param space Space handle
 * @param atom MeTTa atom handle
 * @return 0 on success
 */
int metta_space_remove(cog_context_t* ctx, metta_space_t space, metta_atom_t atom);

/**
 * @brief Query space with pattern
 * @param ctx Cognitive context
 * @param space Space handle
 * @param pattern Pattern atom
 * @param results Output array for results
 * @param max_results Maximum results to return
 * @return Number of results found
 */
int metta_space_query(cog_context_t* ctx,
                       metta_space_t space,
                       metta_atom_t pattern,
                       metta_atom_t* results,
                       size_t max_results);

/* ============================================================================
 * MeTTa Atom Creation
 * ============================================================================ */

/**
 * @brief Create a symbol atom
 * @param ctx Cognitive context
 * @param name Symbol name
 * @return MeTTa atom handle
 */
metta_atom_t metta_atom_sym(cog_context_t* ctx, const char* name);

/**
 * @brief Create a variable atom
 * @param ctx Cognitive context
 * @param name Variable name
 * @return MeTTa atom handle
 */
metta_atom_t metta_atom_var(cog_context_t* ctx, const char* name);

/**
 * @brief Create an expression atom
 * @param ctx Cognitive context
 * @param children Array of child atoms
 * @param count Number of children
 * @return MeTTa atom handle
 */
metta_atom_t metta_atom_expr(cog_context_t* ctx,
                              const metta_atom_t* children,
                              size_t count);

/**
 * @brief Parse MeTTa code into atom
 * @param ctx Cognitive context
 * @param code MeTTa source code
 * @return MeTTa atom handle or 0 on parse error
 */
metta_atom_t metta_parse(cog_context_t* ctx, const char* code);

/**
 * @brief Convert atom to string representation
 * @param ctx Cognitive context
 * @param atom MeTTa atom handle
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Length of string or negative on error
 */
int metta_atom_to_string(cog_context_t* ctx,
                          metta_atom_t atom,
                          char* buffer,
                          size_t buffer_size);

/**
 * @brief Get atom type
 * @param ctx Cognitive context
 * @param atom MeTTa atom handle
 * @return Atom type
 */
metta_atom_type_t metta_atom_type(cog_context_t* ctx, metta_atom_t atom);

/* ============================================================================
 * MeTTa Execution
 * ============================================================================ */

/**
 * @brief Create a MeTTa runner
 * @param ctx Cognitive context
 * @param space Space to run in
 * @return Runner handle or 0 on failure
 */
metta_runner_t metta_runner_new(cog_context_t* ctx, metta_space_t space);

/**
 * @brief Free a MeTTa runner
 * @param ctx Cognitive context
 * @param runner Runner handle
 */
void metta_runner_free(cog_context_t* ctx, metta_runner_t runner);

/**
 * @brief Execute MeTTa code
 * @param ctx Cognitive context
 * @param runner Runner handle
 * @param code MeTTa source code
 * @param results Output array for results
 * @param max_results Maximum results to return
 * @return Number of results or negative on error
 */
int metta_run(cog_context_t* ctx,
               metta_runner_t runner,
               const char* code,
               metta_atom_t* results,
               size_t max_results);

/**
 * @brief Evaluate MeTTa atom
 * @param ctx Cognitive context
 * @param runner Runner handle
 * @param atom Atom to evaluate
 * @param results Output array for results
 * @param max_results Maximum results to return
 * @return Number of results or negative on error
 */
int metta_eval(cog_context_t* ctx,
                metta_runner_t runner,
                metta_atom_t atom,
                metta_atom_t* results,
                size_t max_results);

/**
 * @brief Step-by-step execution
 * @param ctx Cognitive context
 * @param runner Runner handle
 * @return 0 if more steps available, 1 if complete, negative on error
 */
int metta_step(cog_context_t* ctx, metta_runner_t runner);

/* ============================================================================
 * MeTTa Modules
 * ============================================================================ */

/**
 * @brief Load MeTTa module from file
 * @param ctx Cognitive context
 * @param path Path to .metta file
 * @return Module handle or 0 on failure
 */
metta_module_t metta_load_module(cog_context_t* ctx, const char* path);

/**
 * @brief Load MeTTa module from string
 * @param ctx Cognitive context
 * @param name Module name
 * @param code Module source code
 * @return Module handle or 0 on failure
 */
metta_module_t metta_load_module_string(cog_context_t* ctx,
                                         const char* name,
                                         const char* code);

/**
 * @brief Import module into space
 * @param ctx Cognitive context
 * @param space Target space
 * @param module Module to import
 * @return 0 on success
 */
int metta_import_module(cog_context_t* ctx,
                         metta_space_t space,
                         metta_module_t module);

/* ============================================================================
 * MeTTa-AtomSpace Bridge
 * ============================================================================ */

/**
 * @brief Convert MeTTa atom to cognitive atom
 * @param ctx Cognitive context
 * @param metta_atom MeTTa atom handle
 * @return Cognitive atom handle
 */
cog_atom_t metta_to_cog_atom(cog_context_t* ctx, metta_atom_t metta_atom);

/**
 * @brief Convert cognitive atom to MeTTa atom
 * @param ctx Cognitive context
 * @param cog_atom Cognitive atom handle
 * @return MeTTa atom handle
 */
metta_atom_t cog_to_metta_atom(cog_context_t* ctx, cog_atom_t cog_atom);

/**
 * @brief Synchronize MeTTa space with AtomSpace
 * @param ctx Cognitive context
 * @param space MeTTa space
 * @param direction 1 = MeTTa->AtomSpace, -1 = AtomSpace->MeTTa, 0 = bidirectional
 * @return Number of atoms synchronized
 */
int metta_sync_atomspace(cog_context_t* ctx, metta_space_t space, int direction);

/* ============================================================================
 * MeTTa Type System
 * ============================================================================ */

/**
 * @brief Define a type
 * @param ctx Cognitive context
 * @param space Target space
 * @param type_name Type name
 * @param type_def Type definition atom
 * @return 0 on success
 */
int metta_define_type(cog_context_t* ctx,
                       metta_space_t space,
                       const char* type_name,
                       metta_atom_t type_def);

/**
 * @brief Get type of atom
 * @param ctx Cognitive context
 * @param space Space for type lookup
 * @param atom Atom to type-check
 * @return Type atom or 0 if untyped
 */
metta_atom_t metta_get_type(cog_context_t* ctx,
                             metta_space_t space,
                             metta_atom_t atom);

/**
 * @brief Check if atom matches type
 * @param ctx Cognitive context
 * @param space Space for type lookup
 * @param atom Atom to check
 * @param type Expected type
 * @return 1 if matches, 0 if not, negative on error
 */
int metta_check_type(cog_context_t* ctx,
                      metta_space_t space,
                      metta_atom_t atom,
                      metta_atom_t type);

/* ============================================================================
 * MeTTa Grounded Operations
 * ============================================================================ */

/**
 * @brief Grounded operation callback type
 */
typedef metta_atom_t (*metta_grounded_op_t)(cog_context_t* ctx,
                                             const metta_atom_t* args,
                                             size_t arg_count,
                                             void* user_data);

/**
 * @brief Register grounded operation
 * @param ctx Cognitive context
 * @param space Target space
 * @param name Operation name
 * @param op Operation callback
 * @param user_data User data passed to callback
 * @return 0 on success
 */
int metta_register_grounded_op(cog_context_t* ctx,
                                metta_space_t space,
                                const char* name,
                                metta_grounded_op_t op,
                                void* user_data);

/* ============================================================================
 * MeTTa Statistics
 * ============================================================================ */

/**
 * @brief MeTTa statistics structure
 */
typedef struct {
    uint64_t atoms_created;
    uint64_t atoms_in_space;
    uint64_t evaluations;
    uint64_t reductions;
    uint64_t unifications;
    double eval_time_ms;
} metta_stats_t;

/**
 * @brief Get MeTTa statistics
 * @param ctx Cognitive context
 * @param stats Output statistics structure
 * @return 0 on success
 */
int metta_get_stats(cog_context_t* ctx, metta_stats_t* stats);

/* ============================================================================
 * Standard Library Functions
 * ============================================================================ */

/**
 * @brief Load MeTTa standard library
 * @param ctx Cognitive context
 * @param space Target space
 * @return 0 on success
 */
int metta_load_stdlib(cog_context_t* ctx, metta_space_t space);

/**
 * @brief Load PLN (Probabilistic Logic Networks) module
 * @param ctx Cognitive context
 * @param space Target space
 * @return 0 on success
 */
int metta_load_pln(cog_context_t* ctx, metta_space_t space);

/**
 * @brief Load NARS integration module
 * @param ctx Cognitive context
 * @param space Target space
 * @return 0 on success
 */
int metta_load_nars(cog_context_t* ctx, metta_space_t space);

#ifdef __cplusplus
}
#endif

#endif /* AGI_OS_METTA_INTEGRATION_H */
