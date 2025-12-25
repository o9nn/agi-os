/**
 * @file ggml_integration.c
 * @brief GGML Tensor Integration Implementation for AGI-OS
 * 
 * @copyright AGI-OS Project
 * @license GPL-3.0
 */

#include "ggml_integration.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <pthread.h>

/* Include GGML headers when available */
#ifdef HAVE_GGML
#include "ggml.h"
#include "ggml-alloc.h"
#include "ggml-backend.h"
#endif

/* ============================================================================
 * Internal Structures
 * ============================================================================ */

#define MAX_TENSORS 4096
#define MAX_GRAPHS 64

typedef struct {
    ggml_tensor_handle_t handle;
    ggml_dtype_t dtype;
    int64_t dims[4];
    int ndims;
    void* data;
    size_t data_size;
    bool allocated;
} tensor_entry_t;

typedef struct {
    ggml_graph_handle_t handle;
    ggml_tensor_handle_t* nodes;
    int node_count;
    int node_capacity;
    bool computed;
} graph_entry_t;

typedef struct {
    bool initialized;
    ggml_config_t config;
    
    /* Tensor storage */
    tensor_entry_t tensors[MAX_TENSORS];
    uint64_t next_tensor_handle;
    pthread_mutex_t tensor_mutex;
    
    /* Graph storage */
    graph_entry_t graphs[MAX_GRAPHS];
    uint64_t next_graph_handle;
    pthread_mutex_t graph_mutex;
    
    /* Memory pool */
    void* mem_pool;
    size_t mem_used;
    
    /* Statistics */
    ggml_stats_t stats;
    pthread_mutex_t stats_mutex;
    
#ifdef HAVE_GGML
    struct ggml_context* ggml_ctx;
    struct ggml_backend* backend;
#endif
} ggml_state_t;

static __thread ggml_state_t* tls_ggml_state = NULL;

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static ggml_state_t* get_ggml_state(cog_context_t* ctx) {
    (void)ctx;
    return tls_ggml_state;
}

static size_t dtype_size(ggml_dtype_t dtype) {
    switch (dtype) {
        case GGML_TYPE_F32: return 4;
        case GGML_TYPE_F16: return 2;
        case GGML_TYPE_Q4_0:
        case GGML_TYPE_Q4_1: return 1;  /* Approximate */
        case GGML_TYPE_Q5_0:
        case GGML_TYPE_Q5_1: return 1;
        case GGML_TYPE_Q8_0:
        case GGML_TYPE_Q8_1: return 1;
        case GGML_TYPE_I8: return 1;
        case GGML_TYPE_I16: return 2;
        case GGML_TYPE_I32: return 4;
        default: return 4;
    }
}

static size_t tensor_size(ggml_dtype_t dtype, const int64_t* dims, int ndims) {
    size_t nelems = 1;
    for (int i = 0; i < ndims; i++) {
        nelems *= dims[i];
    }
    return nelems * dtype_size(dtype);
}

static tensor_entry_t* find_tensor(ggml_state_t* state, ggml_tensor_handle_t handle) {
    for (int i = 0; i < MAX_TENSORS; i++) {
        if (state->tensors[i].allocated && state->tensors[i].handle == handle) {
            return &state->tensors[i];
        }
    }
    return NULL;
}

static graph_entry_t* find_graph(ggml_state_t* state, ggml_graph_handle_t handle) {
    for (int i = 0; i < MAX_GRAPHS; i++) {
        if (state->graphs[i].handle == handle && state->graphs[i].nodes != NULL) {
            return &state->graphs[i];
        }
    }
    return NULL;
}

/* ============================================================================
 * GGML Initialization
 * ============================================================================ */

int ggml_cog_init(cog_context_t* ctx, const ggml_config_t* config) {
    if (!ctx) return -1;
    
    ggml_state_t* state = calloc(1, sizeof(ggml_state_t));
    if (!state) return -1;
    
    /* Apply configuration */
    if (config) {
        state->config = *config;
    } else {
        ggml_config_t defaults = GGML_CONFIG_DEFAULT;
        state->config = defaults;
    }
    
    /* Initialize mutexes */
    pthread_mutex_init(&state->tensor_mutex, NULL);
    pthread_mutex_init(&state->graph_mutex, NULL);
    pthread_mutex_init(&state->stats_mutex, NULL);
    
    /* Allocate memory pool */
    state->mem_pool = malloc(state->config.mem_size);
    if (!state->mem_pool) {
        free(state);
        return -1;
    }
    state->mem_used = 0;
    
    state->next_tensor_handle = 1;
    state->next_graph_handle = 1;
    
#ifdef HAVE_GGML
    /* Initialize GGML context */
    struct ggml_init_params params = {
        .mem_size = state->config.mem_size,
        .mem_buffer = state->mem_pool,
        .no_alloc = false
    };
    state->ggml_ctx = ggml_init(params);
    
    /* Initialize backend */
    switch (state->config.backend) {
        case GGML_BACKEND_CUDA:
            state->backend = ggml_backend_cuda_init(0);
            break;
        case GGML_BACKEND_METAL:
            state->backend = ggml_backend_metal_init();
            break;
        default:
            state->backend = ggml_backend_cpu_init();
            ggml_backend_cpu_set_n_threads(state->backend, state->config.n_threads);
            break;
    }
#endif
    
    state->stats.memory_total = state->config.mem_size;
    state->initialized = true;
    tls_ggml_state = state;
    
    fprintf(stderr, "[GGML] Initialized with %zu MB memory pool, %d threads\n",
            state->config.mem_size / (1024 * 1024), state->config.n_threads);
    
    return 0;
}

void ggml_cog_shutdown(cog_context_t* ctx) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return;
    
#ifdef HAVE_GGML
    if (state->backend) {
        ggml_backend_free(state->backend);
    }
    if (state->ggml_ctx) {
        ggml_free(state->ggml_ctx);
    }
#endif
    
    /* Free all tensors */
    for (int i = 0; i < MAX_TENSORS; i++) {
        if (state->tensors[i].allocated && state->tensors[i].data) {
            free(state->tensors[i].data);
        }
    }
    
    /* Free all graphs */
    for (int i = 0; i < MAX_GRAPHS; i++) {
        if (state->graphs[i].nodes) {
            free(state->graphs[i].nodes);
        }
    }
    
    free(state->mem_pool);
    
    pthread_mutex_destroy(&state->tensor_mutex);
    pthread_mutex_destroy(&state->graph_mutex);
    pthread_mutex_destroy(&state->stats_mutex);
    
    free(state);
    tls_ggml_state = NULL;
    
    fprintf(stderr, "[GGML] Shutdown complete\n");
}

int ggml_cog_get_backends(ggml_backend_type_t* backends, int max_backends) {
    int count = 0;
    
    if (count < max_backends) backends[count++] = GGML_BACKEND_CPU;
    
#ifdef HAVE_CUDA
    if (count < max_backends) backends[count++] = GGML_BACKEND_CUDA;
#endif
    
#ifdef HAVE_METAL
    if (count < max_backends) backends[count++] = GGML_BACKEND_METAL;
#endif
    
#ifdef HAVE_OPENCL
    if (count < max_backends) backends[count++] = GGML_BACKEND_OPENCL;
#endif
    
    return count;
}

/* ============================================================================
 * Tensor Operations
 * ============================================================================ */

ggml_tensor_handle_t ggml_cog_tensor_new(cog_context_t* ctx,
                                          ggml_dtype_t dtype,
                                          const int64_t* dims,
                                          int ndims) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state || !dims || ndims <= 0 || ndims > 4) return 0;
    
    pthread_mutex_lock(&state->tensor_mutex);
    
    /* Find free slot */
    tensor_entry_t* entry = NULL;
    for (int i = 0; i < MAX_TENSORS; i++) {
        if (!state->tensors[i].allocated) {
            entry = &state->tensors[i];
            break;
        }
    }
    
    if (!entry) {
        pthread_mutex_unlock(&state->tensor_mutex);
        return 0;
    }
    
    /* Allocate tensor data */
    size_t size = tensor_size(dtype, dims, ndims);
    entry->data = calloc(1, size);
    if (!entry->data) {
        pthread_mutex_unlock(&state->tensor_mutex);
        return 0;
    }
    
    entry->handle = state->next_tensor_handle++;
    entry->dtype = dtype;
    entry->ndims = ndims;
    entry->data_size = size;
    entry->allocated = true;
    
    for (int i = 0; i < ndims; i++) {
        entry->dims[i] = dims[i];
    }
    for (int i = ndims; i < 4; i++) {
        entry->dims[i] = 1;
    }
    
    state->stats.tensors_created++;
    state->stats.memory_used += size;
    
    pthread_mutex_unlock(&state->tensor_mutex);
    
    return entry->handle;
}

ggml_tensor_handle_t ggml_cog_tensor_from_data(cog_context_t* ctx,
                                                ggml_dtype_t dtype,
                                                const int64_t* dims,
                                                int ndims,
                                                const void* data) {
    ggml_tensor_handle_t handle = ggml_cog_tensor_new(ctx, dtype, dims, ndims);
    if (!handle) return 0;
    
    ggml_state_t* state = get_ggml_state(ctx);
    tensor_entry_t* entry = find_tensor(state, handle);
    if (entry && data) {
        memcpy(entry->data, data, entry->data_size);
    }
    
    return handle;
}

void ggml_cog_tensor_free(cog_context_t* ctx, ggml_tensor_handle_t tensor) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return;
    
    pthread_mutex_lock(&state->tensor_mutex);
    
    tensor_entry_t* entry = find_tensor(state, tensor);
    if (entry) {
        if (entry->data) {
            free(entry->data);
            state->stats.memory_used -= entry->data_size;
        }
        entry->allocated = false;
        entry->data = NULL;
    }
    
    pthread_mutex_unlock(&state->tensor_mutex);
}

void* ggml_cog_tensor_data(cog_context_t* ctx, ggml_tensor_handle_t tensor) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return NULL;
    
    tensor_entry_t* entry = find_tensor(state, tensor);
    return entry ? entry->data : NULL;
}

int ggml_cog_tensor_shape(cog_context_t* ctx,
                           ggml_tensor_handle_t tensor,
                           int64_t* dims) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state || !dims) return -1;
    
    tensor_entry_t* entry = find_tensor(state, tensor);
    if (!entry) return -1;
    
    for (int i = 0; i < 4; i++) {
        dims[i] = entry->dims[i];
    }
    
    return entry->ndims;
}

/* ============================================================================
 * Computation Graph
 * ============================================================================ */

ggml_graph_handle_t ggml_cog_graph_new(cog_context_t* ctx) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    pthread_mutex_lock(&state->graph_mutex);
    
    /* Find free slot */
    graph_entry_t* entry = NULL;
    for (int i = 0; i < MAX_GRAPHS; i++) {
        if (state->graphs[i].nodes == NULL) {
            entry = &state->graphs[i];
            break;
        }
    }
    
    if (!entry) {
        pthread_mutex_unlock(&state->graph_mutex);
        return 0;
    }
    
    entry->handle = state->next_graph_handle++;
    entry->node_capacity = 256;
    entry->nodes = calloc(entry->node_capacity, sizeof(ggml_tensor_handle_t));
    entry->node_count = 0;
    entry->computed = false;
    
    pthread_mutex_unlock(&state->graph_mutex);
    
    return entry->handle;
}

void ggml_cog_graph_free(cog_context_t* ctx, ggml_graph_handle_t graph) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return;
    
    pthread_mutex_lock(&state->graph_mutex);
    
    graph_entry_t* entry = find_graph(state, graph);
    if (entry) {
        free(entry->nodes);
        entry->nodes = NULL;
        entry->handle = 0;
    }
    
    pthread_mutex_unlock(&state->graph_mutex);
}

int ggml_cog_graph_compute(cog_context_t* ctx, ggml_graph_handle_t graph) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return -1;
    
    graph_entry_t* entry = find_graph(state, graph);
    if (!entry) return -1;
    
    /* Execute graph operations */
    /* In full implementation, this would use GGML's graph execution */
    
    entry->computed = true;
    
    pthread_mutex_lock(&state->stats_mutex);
    state->stats.graphs_computed++;
    pthread_mutex_unlock(&state->stats_mutex);
    
    return 0;
}

/* ============================================================================
 * Neural Network Operations (Simplified implementations)
 * ============================================================================ */

static ggml_tensor_handle_t add_graph_node(ggml_state_t* state,
                                            graph_entry_t* graph,
                                            ggml_tensor_handle_t tensor) {
    if (graph->node_count >= graph->node_capacity) {
        graph->node_capacity *= 2;
        graph->nodes = realloc(graph->nodes, 
                               graph->node_capacity * sizeof(ggml_tensor_handle_t));
    }
    graph->nodes[graph->node_count++] = tensor;
    return tensor;
}

ggml_tensor_handle_t ggml_cog_matmul(cog_context_t* ctx,
                                      ggml_graph_handle_t graph,
                                      ggml_tensor_handle_t a,
                                      ggml_tensor_handle_t b) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* ta = find_tensor(state, a);
    tensor_entry_t* tb = find_tensor(state, b);
    if (!ta || !tb) return 0;
    
    /* Result shape: [a.dims[0], b.dims[1]] */
    int64_t dims[2] = {ta->dims[0], tb->dims[1]};
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, GGML_TYPE_F32, dims, 2);
    
    /* Simplified matrix multiplication */
    float* ra = (float*)ta->data;
    float* rb = (float*)tb->data;
    tensor_entry_t* tr = find_tensor(state, result);
    float* rc = (float*)tr->data;
    
    int m = ta->dims[0];
    int k = ta->dims[1];
    int n = tb->dims[1];
    
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            float sum = 0.0f;
            for (int l = 0; l < k; l++) {
                sum += ra[i * k + l] * rb[l * n + j];
            }
            rc[i * n + j] = sum;
        }
    }
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

ggml_tensor_handle_t ggml_cog_add(cog_context_t* ctx,
                                   ggml_graph_handle_t graph,
                                   ggml_tensor_handle_t a,
                                   ggml_tensor_handle_t b) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* ta = find_tensor(state, a);
    tensor_entry_t* tb = find_tensor(state, b);
    if (!ta || !tb) return 0;
    
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, ta->dtype, ta->dims, ta->ndims);
    tensor_entry_t* tr = find_tensor(state, result);
    
    float* ra = (float*)ta->data;
    float* rb = (float*)tb->data;
    float* rc = (float*)tr->data;
    
    size_t nelems = ta->data_size / sizeof(float);
    for (size_t i = 0; i < nelems; i++) {
        rc[i] = ra[i] + rb[i];
    }
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

ggml_tensor_handle_t ggml_cog_mul(cog_context_t* ctx,
                                   ggml_graph_handle_t graph,
                                   ggml_tensor_handle_t a,
                                   ggml_tensor_handle_t b) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* ta = find_tensor(state, a);
    tensor_entry_t* tb = find_tensor(state, b);
    if (!ta || !tb) return 0;
    
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, ta->dtype, ta->dims, ta->ndims);
    tensor_entry_t* tr = find_tensor(state, result);
    
    float* ra = (float*)ta->data;
    float* rb = (float*)tb->data;
    float* rc = (float*)tr->data;
    
    size_t nelems = ta->data_size / sizeof(float);
    for (size_t i = 0; i < nelems; i++) {
        rc[i] = ra[i] * rb[i];
    }
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

ggml_tensor_handle_t ggml_cog_relu(cog_context_t* ctx,
                                    ggml_graph_handle_t graph,
                                    ggml_tensor_handle_t x) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* tx = find_tensor(state, x);
    if (!tx) return 0;
    
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, tx->dtype, tx->dims, tx->ndims);
    tensor_entry_t* tr = find_tensor(state, result);
    
    float* rx = (float*)tx->data;
    float* rc = (float*)tr->data;
    
    size_t nelems = tx->data_size / sizeof(float);
    for (size_t i = 0; i < nelems; i++) {
        rc[i] = rx[i] > 0 ? rx[i] : 0;
    }
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

ggml_tensor_handle_t ggml_cog_gelu(cog_context_t* ctx,
                                    ggml_graph_handle_t graph,
                                    ggml_tensor_handle_t x) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* tx = find_tensor(state, x);
    if (!tx) return 0;
    
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, tx->dtype, tx->dims, tx->ndims);
    tensor_entry_t* tr = find_tensor(state, result);
    
    float* rx = (float*)tx->data;
    float* rc = (float*)tr->data;
    
    /* GELU approximation: 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3))) */
    const float sqrt_2_over_pi = 0.7978845608f;
    
    size_t nelems = tx->data_size / sizeof(float);
    for (size_t i = 0; i < nelems; i++) {
        float xi = rx[i];
        float inner = sqrt_2_over_pi * (xi + 0.044715f * xi * xi * xi);
        rc[i] = 0.5f * xi * (1.0f + tanhf(inner));
    }
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

ggml_tensor_handle_t ggml_cog_softmax(cog_context_t* ctx,
                                       ggml_graph_handle_t graph,
                                       ggml_tensor_handle_t x) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* tx = find_tensor(state, x);
    if (!tx) return 0;
    
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, tx->dtype, tx->dims, tx->ndims);
    tensor_entry_t* tr = find_tensor(state, result);
    
    float* rx = (float*)tx->data;
    float* rc = (float*)tr->data;
    
    size_t nelems = tx->data_size / sizeof(float);
    
    /* Find max for numerical stability */
    float max_val = rx[0];
    for (size_t i = 1; i < nelems; i++) {
        if (rx[i] > max_val) max_val = rx[i];
    }
    
    /* Compute exp and sum */
    float sum = 0.0f;
    for (size_t i = 0; i < nelems; i++) {
        rc[i] = expf(rx[i] - max_val);
        sum += rc[i];
    }
    
    /* Normalize */
    for (size_t i = 0; i < nelems; i++) {
        rc[i] /= sum;
    }
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

ggml_tensor_handle_t ggml_cog_layer_norm(cog_context_t* ctx,
                                          ggml_graph_handle_t graph,
                                          ggml_tensor_handle_t x,
                                          float eps) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* tx = find_tensor(state, x);
    if (!tx) return 0;
    
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, tx->dtype, tx->dims, tx->ndims);
    tensor_entry_t* tr = find_tensor(state, result);
    
    float* rx = (float*)tx->data;
    float* rc = (float*)tr->data;
    
    size_t nelems = tx->data_size / sizeof(float);
    
    /* Compute mean */
    float mean = 0.0f;
    for (size_t i = 0; i < nelems; i++) {
        mean += rx[i];
    }
    mean /= nelems;
    
    /* Compute variance */
    float var = 0.0f;
    for (size_t i = 0; i < nelems; i++) {
        float diff = rx[i] - mean;
        var += diff * diff;
    }
    var /= nelems;
    
    /* Normalize */
    float inv_std = 1.0f / sqrtf(var + eps);
    for (size_t i = 0; i < nelems; i++) {
        rc[i] = (rx[i] - mean) * inv_std;
    }
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

/* ============================================================================
 * Attention Mechanisms
 * ============================================================================ */

ggml_tensor_handle_t ggml_cog_attention(cog_context_t* ctx,
                                         ggml_graph_handle_t graph,
                                         ggml_tensor_handle_t q,
                                         ggml_tensor_handle_t k,
                                         ggml_tensor_handle_t v,
                                         ggml_tensor_handle_t mask) {
    /* Simplified attention: softmax(Q @ K^T / sqrt(d)) @ V */
    (void)mask;  /* Mask handling would be added in full implementation */
    
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* tq = find_tensor(state, q);
    if (!tq) return 0;
    
    float scale = 1.0f / sqrtf((float)tq->dims[tq->ndims - 1]);
    
    /* Q @ K^T */
    ggml_tensor_handle_t scores = ggml_cog_matmul(ctx, graph, q, k);
    
    /* Scale */
    tensor_entry_t* ts = find_tensor(state, scores);
    float* rs = (float*)ts->data;
    size_t nelems = ts->data_size / sizeof(float);
    for (size_t i = 0; i < nelems; i++) {
        rs[i] *= scale;
    }
    
    /* Softmax */
    ggml_tensor_handle_t attn = ggml_cog_softmax(ctx, graph, scores);
    
    /* @ V */
    return ggml_cog_matmul(ctx, graph, attn, v);
}

ggml_tensor_handle_t ggml_cog_multi_head_attention(cog_context_t* ctx,
                                                    ggml_graph_handle_t graph,
                                                    ggml_tensor_handle_t x,
                                                    ggml_tensor_handle_t wq,
                                                    ggml_tensor_handle_t wk,
                                                    ggml_tensor_handle_t wv,
                                                    ggml_tensor_handle_t wo,
                                                    int n_heads) {
    (void)n_heads;  /* Head splitting would be added in full implementation */
    
    /* Project Q, K, V */
    ggml_tensor_handle_t q = ggml_cog_matmul(ctx, graph, x, wq);
    ggml_tensor_handle_t k = ggml_cog_matmul(ctx, graph, x, wk);
    ggml_tensor_handle_t v = ggml_cog_matmul(ctx, graph, x, wv);
    
    /* Attention */
    ggml_tensor_handle_t attn = ggml_cog_attention(ctx, graph, q, k, v, 0);
    
    /* Output projection */
    return ggml_cog_matmul(ctx, graph, attn, wo);
}

/* ============================================================================
 * Atom Embedding Operations
 * ============================================================================ */

ggml_tensor_handle_t ggml_cog_create_embedding_table(cog_context_t* ctx,
                                                      int vocab_size,
                                                      int embed_dim) {
    int64_t dims[2] = {vocab_size, embed_dim};
    ggml_tensor_handle_t table = ggml_cog_tensor_new(ctx, GGML_TYPE_F32, dims, 2);
    
    /* Initialize with random values */
    ggml_state_t* state = get_ggml_state(ctx);
    tensor_entry_t* entry = find_tensor(state, table);
    if (entry) {
        float* data = (float*)entry->data;
        float scale = 1.0f / sqrtf((float)embed_dim);
        for (int i = 0; i < vocab_size * embed_dim; i++) {
            data[i] = ((float)rand() / RAND_MAX - 0.5f) * 2.0f * scale;
        }
    }
    
    return table;
}

ggml_tensor_handle_t ggml_cog_embed_atoms(cog_context_t* ctx,
                                           ggml_graph_handle_t graph,
                                           ggml_tensor_handle_t table,
                                           const cog_atom_t* atoms,
                                           int n_atoms) {
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* tt = find_tensor(state, table);
    if (!tt) return 0;
    
    int embed_dim = tt->dims[1];
    int64_t dims[2] = {n_atoms, embed_dim};
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, GGML_TYPE_F32, dims, 2);
    
    tensor_entry_t* tr = find_tensor(state, result);
    float* table_data = (float*)tt->data;
    float* result_data = (float*)tr->data;
    int vocab_size = tt->dims[0];
    
    for (int i = 0; i < n_atoms; i++) {
        int idx = atoms[i] % vocab_size;  /* Simple hash-based lookup */
        memcpy(&result_data[i * embed_dim], 
               &table_data[idx * embed_dim],
               embed_dim * sizeof(float));
    }
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

ggml_tensor_handle_t ggml_cog_atom_similarity(cog_context_t* ctx,
                                               ggml_graph_handle_t graph,
                                               ggml_tensor_handle_t embed_a,
                                               ggml_tensor_handle_t embed_b) {
    /* Cosine similarity */
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return 0;
    
    tensor_entry_t* ta = find_tensor(state, embed_a);
    tensor_entry_t* tb = find_tensor(state, embed_b);
    if (!ta || !tb) return 0;
    
    int64_t dims[1] = {1};
    ggml_tensor_handle_t result = ggml_cog_tensor_new(ctx, GGML_TYPE_F32, dims, 1);
    tensor_entry_t* tr = find_tensor(state, result);
    
    float* ra = (float*)ta->data;
    float* rb = (float*)tb->data;
    float* rc = (float*)tr->data;
    
    size_t nelems = ta->data_size / sizeof(float);
    
    float dot = 0.0f, norm_a = 0.0f, norm_b = 0.0f;
    for (size_t i = 0; i < nelems; i++) {
        dot += ra[i] * rb[i];
        norm_a += ra[i] * ra[i];
        norm_b += rb[i] * rb[i];
    }
    
    rc[0] = dot / (sqrtf(norm_a) * sqrtf(norm_b) + 1e-8f);
    
    graph_entry_t* g = find_graph(state, graph);
    if (g) add_graph_node(state, g, result);
    
    return result;
}

/* ============================================================================
 * Model Loading (Placeholder)
 * ============================================================================ */

int ggml_cog_load_model(cog_context_t* ctx, const char* path) {
    (void)ctx;
    fprintf(stderr, "[GGML] Loading model from: %s\n", path);
    /* Full implementation would parse GGUF format */
    return 0;
}

int ggml_cog_save_model(cog_context_t* ctx, const char* path) {
    (void)ctx;
    fprintf(stderr, "[GGML] Saving model to: %s\n", path);
    /* Full implementation would write GGUF format */
    return 0;
}

/* ============================================================================
 * Statistics
 * ============================================================================ */

int ggml_cog_get_stats(cog_context_t* ctx, ggml_stats_t* stats) {
    if (!stats) return -1;
    
    ggml_state_t* state = get_ggml_state(ctx);
    if (!state) return -1;
    
    pthread_mutex_lock(&state->stats_mutex);
    *stats = state->stats;
    pthread_mutex_unlock(&state->stats_mutex);
    
    return 0;
}
