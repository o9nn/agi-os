/**
 * @file ggml_integration.h
 * @brief GGML Tensor Integration for AGI-OS Cognitive Synergy Bridge
 * 
 * This header provides the integration layer between the Cognitive Synergy
 * Bridge and the GGML tensor library for neural processing.
 * 
 * @copyright AGI-OS Project
 * @license GPL-3.0
 */

#ifndef AGI_OS_GGML_INTEGRATION_H
#define AGI_OS_GGML_INTEGRATION_H

#include "../cognitive_synergy_bridge.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * GGML Configuration
 * ============================================================================ */

/**
 * @brief GGML backend types
 */
typedef enum {
    GGML_BACKEND_CPU,       /**< CPU backend (default) */
    GGML_BACKEND_CUDA,      /**< NVIDIA CUDA backend */
    GGML_BACKEND_OPENCL,    /**< OpenCL backend */
    GGML_BACKEND_METAL,     /**< Apple Metal backend */
    GGML_BACKEND_VULKAN     /**< Vulkan backend */
} ggml_backend_type_t;

/**
 * @brief GGML configuration parameters
 */
typedef struct {
    ggml_backend_type_t backend;  /**< Compute backend */
    size_t mem_size;              /**< Memory pool size in bytes */
    int n_threads;                /**< Number of threads for CPU backend */
    bool use_mmap;                /**< Use memory-mapped files */
    bool use_mlock;               /**< Lock memory to prevent swapping */
} ggml_config_t;

/**
 * @brief Default GGML configuration
 */
#define GGML_CONFIG_DEFAULT { \
    .backend = GGML_BACKEND_CPU, \
    .mem_size = 512 * 1024 * 1024, \
    .n_threads = 4, \
    .use_mmap = true, \
    .use_mlock = false \
}

/* ============================================================================
 * Tensor Types
 * ============================================================================ */

/**
 * @brief Tensor data types
 */
typedef enum {
    GGML_TYPE_F32,      /**< 32-bit float */
    GGML_TYPE_F16,      /**< 16-bit float */
    GGML_TYPE_Q4_0,     /**< 4-bit quantized */
    GGML_TYPE_Q4_1,     /**< 4-bit quantized with scale */
    GGML_TYPE_Q5_0,     /**< 5-bit quantized */
    GGML_TYPE_Q5_1,     /**< 5-bit quantized with scale */
    GGML_TYPE_Q8_0,     /**< 8-bit quantized */
    GGML_TYPE_Q8_1,     /**< 8-bit quantized with scale */
    GGML_TYPE_I8,       /**< 8-bit integer */
    GGML_TYPE_I16,      /**< 16-bit integer */
    GGML_TYPE_I32       /**< 32-bit integer */
} ggml_dtype_t;

/**
 * @brief Tensor handle
 */
typedef uint64_t ggml_tensor_handle_t;

/**
 * @brief Computation graph handle
 */
typedef uint64_t ggml_graph_handle_t;

/* ============================================================================
 * GGML Initialization
 * ============================================================================ */

/**
 * @brief Initialize GGML subsystem
 * @param ctx Cognitive context
 * @param config GGML configuration (NULL for defaults)
 * @return 0 on success, negative on error
 */
int ggml_cog_init(cog_context_t* ctx, const ggml_config_t* config);

/**
 * @brief Shutdown GGML subsystem
 * @param ctx Cognitive context
 */
void ggml_cog_shutdown(cog_context_t* ctx);

/**
 * @brief Get available backends
 * @param backends Output array of available backends
 * @param max_backends Maximum backends to return
 * @return Number of available backends
 */
int ggml_cog_get_backends(ggml_backend_type_t* backends, int max_backends);

/* ============================================================================
 * Tensor Operations
 * ============================================================================ */

/**
 * @brief Create a new tensor
 * @param ctx Cognitive context
 * @param dtype Data type
 * @param dims Dimension array
 * @param ndims Number of dimensions (1-4)
 * @return Tensor handle or 0 on failure
 */
ggml_tensor_handle_t ggml_cog_tensor_new(cog_context_t* ctx,
                                          ggml_dtype_t dtype,
                                          const int64_t* dims,
                                          int ndims);

/**
 * @brief Create tensor from data
 * @param ctx Cognitive context
 * @param dtype Data type
 * @param dims Dimension array
 * @param ndims Number of dimensions
 * @param data Source data
 * @return Tensor handle or 0 on failure
 */
ggml_tensor_handle_t ggml_cog_tensor_from_data(cog_context_t* ctx,
                                                ggml_dtype_t dtype,
                                                const int64_t* dims,
                                                int ndims,
                                                const void* data);

/**
 * @brief Free a tensor
 * @param ctx Cognitive context
 * @param tensor Tensor handle
 */
void ggml_cog_tensor_free(cog_context_t* ctx, ggml_tensor_handle_t tensor);

/**
 * @brief Get tensor data pointer
 * @param ctx Cognitive context
 * @param tensor Tensor handle
 * @return Pointer to tensor data or NULL
 */
void* ggml_cog_tensor_data(cog_context_t* ctx, ggml_tensor_handle_t tensor);

/**
 * @brief Get tensor shape
 * @param ctx Cognitive context
 * @param tensor Tensor handle
 * @param dims Output dimension array (must have space for 4 elements)
 * @return Number of dimensions
 */
int ggml_cog_tensor_shape(cog_context_t* ctx,
                           ggml_tensor_handle_t tensor,
                           int64_t* dims);

/* ============================================================================
 * Computation Graph
 * ============================================================================ */

/**
 * @brief Create a new computation graph
 * @param ctx Cognitive context
 * @return Graph handle or 0 on failure
 */
ggml_graph_handle_t ggml_cog_graph_new(cog_context_t* ctx);

/**
 * @brief Free a computation graph
 * @param ctx Cognitive context
 * @param graph Graph handle
 */
void ggml_cog_graph_free(cog_context_t* ctx, ggml_graph_handle_t graph);

/**
 * @brief Execute computation graph
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @return 0 on success
 */
int ggml_cog_graph_compute(cog_context_t* ctx, ggml_graph_handle_t graph);

/* ============================================================================
 * Neural Network Operations
 * ============================================================================ */

/**
 * @brief Matrix multiplication: C = A @ B
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param a First matrix
 * @param b Second matrix
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_matmul(cog_context_t* ctx,
                                      ggml_graph_handle_t graph,
                                      ggml_tensor_handle_t a,
                                      ggml_tensor_handle_t b);

/**
 * @brief Element-wise addition: C = A + B
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param a First tensor
 * @param b Second tensor
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_add(cog_context_t* ctx,
                                   ggml_graph_handle_t graph,
                                   ggml_tensor_handle_t a,
                                   ggml_tensor_handle_t b);

/**
 * @brief Element-wise multiplication: C = A * B
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param a First tensor
 * @param b Second tensor
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_mul(cog_context_t* ctx,
                                   ggml_graph_handle_t graph,
                                   ggml_tensor_handle_t a,
                                   ggml_tensor_handle_t b);

/**
 * @brief ReLU activation
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param x Input tensor
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_relu(cog_context_t* ctx,
                                    ggml_graph_handle_t graph,
                                    ggml_tensor_handle_t x);

/**
 * @brief GELU activation
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param x Input tensor
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_gelu(cog_context_t* ctx,
                                    ggml_graph_handle_t graph,
                                    ggml_tensor_handle_t x);

/**
 * @brief Softmax
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param x Input tensor
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_softmax(cog_context_t* ctx,
                                       ggml_graph_handle_t graph,
                                       ggml_tensor_handle_t x);

/**
 * @brief Layer normalization
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param x Input tensor
 * @param eps Epsilon for numerical stability
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_layer_norm(cog_context_t* ctx,
                                          ggml_graph_handle_t graph,
                                          ggml_tensor_handle_t x,
                                          float eps);

/* ============================================================================
 * Attention Mechanisms
 * ============================================================================ */

/**
 * @brief Scaled dot-product attention
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param q Query tensor [batch, heads, seq_len, head_dim]
 * @param k Key tensor [batch, heads, seq_len, head_dim]
 * @param v Value tensor [batch, heads, seq_len, head_dim]
 * @param mask Optional attention mask (0 for no mask)
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_attention(cog_context_t* ctx,
                                         ggml_graph_handle_t graph,
                                         ggml_tensor_handle_t q,
                                         ggml_tensor_handle_t k,
                                         ggml_tensor_handle_t v,
                                         ggml_tensor_handle_t mask);

/**
 * @brief Multi-head attention
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param x Input tensor
 * @param wq Query weight
 * @param wk Key weight
 * @param wv Value weight
 * @param wo Output projection weight
 * @param n_heads Number of attention heads
 * @return Result tensor handle
 */
ggml_tensor_handle_t ggml_cog_multi_head_attention(cog_context_t* ctx,
                                                    ggml_graph_handle_t graph,
                                                    ggml_tensor_handle_t x,
                                                    ggml_tensor_handle_t wq,
                                                    ggml_tensor_handle_t wk,
                                                    ggml_tensor_handle_t wv,
                                                    ggml_tensor_handle_t wo,
                                                    int n_heads);

/* ============================================================================
 * Atom Embedding Operations
 * ============================================================================ */

/**
 * @brief Create embedding table for atoms
 * @param ctx Cognitive context
 * @param vocab_size Number of unique atoms
 * @param embed_dim Embedding dimension
 * @return Tensor handle for embedding table
 */
ggml_tensor_handle_t ggml_cog_create_embedding_table(cog_context_t* ctx,
                                                      int vocab_size,
                                                      int embed_dim);

/**
 * @brief Lookup atom embeddings
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param table Embedding table tensor
 * @param atoms Array of atom handles
 * @param n_atoms Number of atoms
 * @return Tensor of embeddings [n_atoms, embed_dim]
 */
ggml_tensor_handle_t ggml_cog_embed_atoms(cog_context_t* ctx,
                                           ggml_graph_handle_t graph,
                                           ggml_tensor_handle_t table,
                                           const cog_atom_t* atoms,
                                           int n_atoms);

/**
 * @brief Compute atom similarity using embeddings
 * @param ctx Cognitive context
 * @param graph Graph handle
 * @param embed_a First atom embedding
 * @param embed_b Second atom embedding
 * @return Similarity score tensor
 */
ggml_tensor_handle_t ggml_cog_atom_similarity(cog_context_t* ctx,
                                               ggml_graph_handle_t graph,
                                               ggml_tensor_handle_t embed_a,
                                               ggml_tensor_handle_t embed_b);

/* ============================================================================
 * Model Loading
 * ============================================================================ */

/**
 * @brief Load model weights from file
 * @param ctx Cognitive context
 * @param path Path to model file (GGUF format)
 * @return 0 on success
 */
int ggml_cog_load_model(cog_context_t* ctx, const char* path);

/**
 * @brief Save model weights to file
 * @param ctx Cognitive context
 * @param path Path to output file
 * @return 0 on success
 */
int ggml_cog_save_model(cog_context_t* ctx, const char* path);

/* ============================================================================
 * Statistics
 * ============================================================================ */

/**
 * @brief GGML statistics structure
 */
typedef struct {
    size_t memory_used;
    size_t memory_total;
    uint64_t tensors_created;
    uint64_t graphs_computed;
    uint64_t flops_total;
    double compute_time_ms;
} ggml_stats_t;

/**
 * @brief Get GGML statistics
 * @param ctx Cognitive context
 * @param stats Output statistics structure
 * @return 0 on success
 */
int ggml_cog_get_stats(cog_context_t* ctx, ggml_stats_t* stats);

#ifdef __cplusplus
}
#endif

#endif /* AGI_OS_GGML_INTEGRATION_H */
