/**
 * @file unified_cog.h
 * @brief Unified Cognitive Interface for AGI-OS
 * 
 * This header defines the unified cognitive interface that works across
 * all AGI-OS paradigms (Inferno, CogPlan9, HurdCog, CogNumach, OpenCog, DAS).
 * All cognitive operations are exposed via the 9P protocol as file operations.
 * 
 * @author AGI-OS Architecture Team
 * @date December 14, 2025
 */

#ifndef UNIFIED_COG_H
#define UNIFIED_COG_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 * Core Types
 * ======================================================================== */

/**
 * @brief Handle type for cognitive objects (atoms, patterns, etc.)
 * 
 * Handles are 64-bit identifiers that are consistent across all systems.
 * They can be used to reference atoms in OpenCog, DAS, CogPlan9, etc.
 */
typedef uint64_t CogHandle;

/**
 * @brief Truth value representation
 * 
 * Compatible with OpenCog's SimpleTruthValue and CogPlan9's TruthValue.
 */
typedef struct {
    double strength;    /**< Probability/strength [0.0, 1.0] */
    double confidence;  /**< Confidence in the strength [0.0, 1.0] */
} CogTruthValue;

/**
 * @brief Attention value representation
 * 
 * Compatible with OpenCog's AttentionValue and ECAN.
 */
typedef struct {
    int16_t sti;   /**< Short-term importance [-32768, 32767] */
    int16_t lti;   /**< Long-term importance [-32768, 32767] */
    int16_t vlti;  /**< Very long-term importance [-32768, 32767] */
} CogAttentionValue;

/**
 * @brief Atom type enumeration
 */
typedef enum {
    COG_NODE = 1,
    COG_LINK = 2,
    COG_CONCEPT_NODE = 3,
    COG_PREDICATE_NODE = 4,
    COG_INHERITANCE_LINK = 5,
    COG_SIMILARITY_LINK = 6,
    COG_IMPLICATION_LINK = 7,
    COG_EVALUATION_LINK = 8,
    COG_EXECUTION_LINK = 9,
    /* Add more types as needed */
} CogAtomType;

/**
 * @brief Atom structure
 */
typedef struct {
    CogHandle handle;
    CogAtomType type;
    char* name;                    /**< For nodes */
    CogHandle* outgoing;           /**< For links */
    size_t outgoing_count;         /**< Number of outgoing atoms */
    CogTruthValue tv;
    CogAttentionValue av;
} CogAtom;

/* ========================================================================
 * 9P File System Interface
 * ======================================================================== */

/**
 * @brief Cognitive file system paths
 * 
 * All cognitive operations are accessible via these 9P paths:
 * 
 * /cog/atoms/create         - Create new atoms
 * /cog/atoms/read           - Read atom by handle
 * /cog/atoms/update         - Update atom properties
 * /cog/atoms/delete         - Delete atom
 * /cog/atoms/list           - List all atoms
 * 
 * /cog/query/pattern        - Pattern matching queries
 * /cog/query/traversal      - Graph traversal queries
 * /cog/query/distributed    - Distributed queries (via DAS)
 * 
 * /cog/reason/pln           - PLN inference
 * /cog/reason/ure           - Unified Rule Engine
 * /cog/reason/forward       - Forward chaining
 * /cog/reason/backward      - Backward chaining
 * 
 * /cog/attention/sti        - Short-term importance operations
 * /cog/attention/lti        - Long-term importance operations
 * /cog/attention/allocate   - Attention allocation
 * /cog/attention/spread     - Attention spreading
 * 
 * /cog/learn/mine           - Pattern mining
 * /cog/learn/evolve         - Evolution agent
 * /cog/learn/create_links   - Link creation agent
 * 
 * /cog/distributed/sync     - Synchronize distributed atomspace
 * /cog/distributed/query    - Distributed query
 * /cog/distributed/status   - Cluster status
 */

/* ========================================================================
 * Atom Operations
 * ======================================================================== */

/**
 * @brief Create a new atom
 * 
 * @param type Atom type
 * @param name Atom name (for nodes) or NULL (for links)
 * @param outgoing Array of outgoing atom handles (for links) or NULL
 * @param outgoing_count Number of outgoing atoms
 * @param tv Truth value
 * @return Handle to the created atom, or 0 on error
 * 
 * 9P equivalent: echo "type name tv.strength tv.confidence" > /cog/atoms/create
 */
CogHandle cog_atom_create(
    CogAtomType type,
    const char* name,
    const CogHandle* outgoing,
    size_t outgoing_count,
    CogTruthValue tv
);

/**
 * @brief Read an atom by handle
 * 
 * @param handle Atom handle
 * @param atom Output atom structure
 * @return true on success, false on error
 * 
 * 9P equivalent: cat /cog/atoms/read/HANDLE
 */
bool cog_atom_read(CogHandle handle, CogAtom* atom);

/**
 * @brief Update atom truth value
 * 
 * @param handle Atom handle
 * @param tv New truth value
 * @return true on success, false on error
 * 
 * 9P equivalent: echo "strength confidence" > /cog/atoms/update/HANDLE/tv
 */
bool cog_atom_update_tv(CogHandle handle, CogTruthValue tv);

/**
 * @brief Update atom attention value
 * 
 * @param handle Atom handle
 * @param av New attention value
 * @return true on success, false on error
 * 
 * 9P equivalent: echo "sti lti vlti" > /cog/atoms/update/HANDLE/av
 */
bool cog_atom_update_av(CogHandle handle, CogAttentionValue av);

/**
 * @brief Delete an atom
 * 
 * @param handle Atom handle
 * @return true on success, false on error
 * 
 * 9P equivalent: echo "HANDLE" > /cog/atoms/delete
 */
bool cog_atom_delete(CogHandle handle);

/* ========================================================================
 * Pattern Matching and Queries
 * ======================================================================== */

/**
 * @brief Pattern matching query
 * 
 * @param pattern Pattern to match (in Atomese syntax)
 * @param results Output array of matching atom handles
 * @param max_results Maximum number of results
 * @return Number of results found
 * 
 * 9P equivalent: echo "pattern" > /cog/query/pattern && cat /cog/query/pattern
 */
size_t cog_query_pattern(
    const char* pattern,
    CogHandle* results,
    size_t max_results
);

/**
 * @brief Distributed query (via DAS)
 * 
 * @param query Query specification
 * @param results Output array of matching atom handles
 * @param max_results Maximum number of results
 * @return Number of results found
 * 
 * 9P equivalent: echo "query" > /cog/distributed/query && cat /cog/distributed/query
 */
size_t cog_query_distributed(
    const char* query,
    CogHandle* results,
    size_t max_results
);

/* ========================================================================
 * Reasoning and Inference
 * ======================================================================== */

/**
 * @brief PLN inference
 * 
 * @param target Target atom to infer
 * @param max_steps Maximum inference steps
 * @param results Output array of inferred atom handles
 * @param max_results Maximum number of results
 * @return Number of inferences made
 * 
 * 9P equivalent: echo "target max_steps" > /cog/reason/pln && cat /cog/reason/pln
 */
size_t cog_reason_pln(
    CogHandle target,
    int max_steps,
    CogHandle* results,
    size_t max_results
);

/**
 * @brief Forward chaining inference
 * 
 * @param premises Array of premise atom handles
 * @param premise_count Number of premises
 * @param results Output array of inferred atom handles
 * @param max_results Maximum number of results
 * @return Number of inferences made
 */
size_t cog_reason_forward(
    const CogHandle* premises,
    size_t premise_count,
    CogHandle* results,
    size_t max_results
);

/**
 * @brief Backward chaining inference
 * 
 * @param goal Goal atom handle
 * @param results Output array of supporting atom handles
 * @param max_results Maximum number of results
 * @return Number of supporting atoms found
 */
size_t cog_reason_backward(
    CogHandle goal,
    CogHandle* results,
    size_t max_results
);

/* ========================================================================
 * Attention Allocation (ECAN)
 * ======================================================================== */

/**
 * @brief Allocate attention to an atom
 * 
 * @param handle Atom handle
 * @param sti_delta Change in short-term importance
 * @return true on success, false on error
 * 
 * 9P equivalent: echo "HANDLE sti_delta" > /cog/attention/allocate
 */
bool cog_attention_allocate(CogHandle handle, int16_t sti_delta);

/**
 * @brief Spread attention from an atom
 * 
 * @param handle Source atom handle
 * @param amount Amount of attention to spread
 * @return true on success, false on error
 * 
 * 9P equivalent: echo "HANDLE amount" > /cog/attention/spread
 */
bool cog_attention_spread(CogHandle handle, int16_t amount);

/**
 * @brief Get atoms with highest STI
 * 
 * @param results Output array of atom handles
 * @param max_results Maximum number of results
 * @return Number of atoms returned
 * 
 * 9P equivalent: cat /cog/attention/sti/top
 */
size_t cog_attention_get_top(CogHandle* results, size_t max_results);

/* ========================================================================
 * Learning and Mining
 * ======================================================================== */

/**
 * @brief Mine patterns from the atomspace
 * 
 * @param min_support Minimum support threshold
 * @param results Output array of pattern atom handles
 * @param max_results Maximum number of results
 * @return Number of patterns found
 * 
 * 9P equivalent: echo "min_support" > /cog/learn/mine && cat /cog/learn/mine
 */
size_t cog_learn_mine(
    double min_support,
    CogHandle* results,
    size_t max_results
);

/* ========================================================================
 * Distributed Operations
 * ======================================================================== */

/**
 * @brief Synchronize distributed atomspace
 * 
 * @return true on success, false on error
 * 
 * 9P equivalent: echo "sync" > /cog/distributed/sync
 */
bool cog_distributed_sync(void);

/**
 * @brief Get distributed cluster status
 * 
 * @param status_buffer Output buffer for status information
 * @param buffer_size Size of status buffer
 * @return Number of bytes written
 * 
 * 9P equivalent: cat /cog/distributed/status
 */
size_t cog_distributed_status(char* status_buffer, size_t buffer_size);

/* ========================================================================
 * System Initialization
 * ======================================================================== */

/**
 * @brief Initialize unified cognitive interface
 * 
 * @param config_path Path to configuration file
 * @return true on success, false on error
 */
bool cog_init(const char* config_path);

/**
 * @brief Shutdown unified cognitive interface
 */
void cog_shutdown(void);

/**
 * @brief Connect to cognitive services via 9P
 * 
 * @param mount_point 9P mount point (e.g., "/n/cog")
 * @return true on success, false on error
 */
bool cog_connect_9p(const char* mount_point);

#ifdef __cplusplus
}
#endif

#endif /* UNIFIED_COG_H */
