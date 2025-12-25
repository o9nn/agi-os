/**
 * @file nars_integration.c
 * @brief OpenNARS Integration Implementation for AGI-OS
 * 
 * @copyright AGI-OS Project
 * @license GPL-3.0
 */

#include "nars_integration.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <time.h>

/* Include OpenNARS headers when available */
#ifdef HAVE_OPENNARS
#include "NAR.h"
#include "Narsese.h"
#include "Shell.h"
#endif

/* ============================================================================
 * Internal Structures
 * ============================================================================ */

#define MAX_OPERATIONS 64
#define MAX_OUTPUT_CALLBACKS 16
#define NARS_TERM_BUFFER_SIZE 4096

typedef struct {
    char name[64];
    nars_operation_t callback;
    void* user_data;
} nars_operation_entry_t;

typedef struct {
    nars_output_callback_t callback;
    void* user_data;
} nars_output_callback_entry_t;

typedef struct {
    bool initialized;
    nars_config_t config;
    
    /* Operations */
    nars_operation_entry_t operations[MAX_OPERATIONS];
    int operation_count;
    pthread_mutex_t operation_mutex;
    
    /* Output callbacks */
    nars_output_callback_entry_t output_callbacks[MAX_OUTPUT_CALLBACKS];
    int output_callback_count;
    pthread_mutex_t output_mutex;
    
    /* Statistics */
    nars_stats_t stats;
    pthread_mutex_t stats_mutex;
    
    /* Term buffer for conversions */
    char term_buffer[NARS_TERM_BUFFER_SIZE];
} nars_state_t;

/* Thread-local NARS state per context */
static __thread nars_state_t* tls_nars_state = NULL;

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static nars_state_t* get_nars_state(cog_context_t* ctx) {
    /* In a full implementation, this would retrieve context-specific state */
    return tls_nars_state;
}

static void emit_output(cog_context_t* ctx, const nars_output_t* output) {
    nars_state_t* state = get_nars_state(ctx);
    if (!state) return;
    
    pthread_mutex_lock(&state->output_mutex);
    
    for (int i = 0; i < state->output_callback_count; i++) {
        nars_output_callback_entry_t* entry = &state->output_callbacks[i];
        if (entry->callback) {
            entry->callback(ctx, output, entry->user_data);
        }
    }
    
    pthread_mutex_unlock(&state->output_mutex);
}

/* ============================================================================
 * NARS Initialization
 * ============================================================================ */

int nars_init(cog_context_t* ctx, const nars_config_t* config) {
    if (!ctx) return -1;
    
    nars_state_t* state = calloc(1, sizeof(nars_state_t));
    if (!state) return -1;
    
    /* Apply configuration */
    if (config) {
        state->config = *config;
    } else {
        nars_config_t defaults = NARS_CONFIG_DEFAULT;
        state->config = defaults;
    }
    
    /* Initialize mutexes */
    pthread_mutex_init(&state->operation_mutex, NULL);
    pthread_mutex_init(&state->output_mutex, NULL);
    pthread_mutex_init(&state->stats_mutex, NULL);
    
#ifdef HAVE_OPENNARS
    /* Initialize OpenNARS */
    NAR_INIT();
    
    /* Apply configuration */
    NAR_DECISION_THRESHOLD = state->config.decision_threshold / 100.0;
    NAR_TRUTH_PROJECTION_DECAY = state->config.truth_projection_decay;
#endif
    
    state->initialized = true;
    tls_nars_state = state;
    
    fprintf(stderr, "[NARS] Initialized with decision_threshold=%d\n",
            state->config.decision_threshold);
    
    return 0;
}

void nars_shutdown(cog_context_t* ctx) {
    nars_state_t* state = get_nars_state(ctx);
    if (!state) return;
    
    pthread_mutex_destroy(&state->operation_mutex);
    pthread_mutex_destroy(&state->output_mutex);
    pthread_mutex_destroy(&state->stats_mutex);
    
    free(state);
    tls_nars_state = NULL;
    
    fprintf(stderr, "[NARS] Shutdown complete\n");
}

/* ============================================================================
 * NARS Input/Output
 * ============================================================================ */

uint64_t nars_input(cog_context_t* ctx, const char* narsese) {
    if (!ctx || !narsese) return 0;
    
    nars_state_t* state = get_nars_state(ctx);
    if (!state || !state->initialized) return 0;
    
    static uint64_t task_id = 1;
    
#ifdef HAVE_OPENNARS
    NAR_AddInputNarsese(narsese);
#endif
    
    pthread_mutex_lock(&state->stats_mutex);
    state->stats.total_inputs++;
    pthread_mutex_unlock(&state->stats_mutex);
    
    fprintf(stderr, "[NARS] Input: %s\n", narsese);
    
    return task_id++;
}

uint64_t nars_add_belief(cog_context_t* ctx, 
                          const char* term,
                          float strength,
                          float confidence) {
    if (!ctx || !term) return 0;
    
    char narsese[1024];
    snprintf(narsese, sizeof(narsese), "%s. %%%0.2f;%0.2f%%",
             term, strength, confidence);
    
    return nars_input(ctx, narsese);
}

uint64_t nars_add_goal(cog_context_t* ctx,
                        const char* term,
                        float strength,
                        float confidence) {
    if (!ctx || !term) return 0;
    
    char narsese[1024];
    snprintf(narsese, sizeof(narsese), "%s! %%%0.2f;%0.2f%%",
             term, strength, confidence);
    
    return nars_input(ctx, narsese);
}

uint64_t nars_ask(cog_context_t* ctx, const char* term) {
    if (!ctx || !term) return 0;
    
    char narsese[1024];
    snprintf(narsese, sizeof(narsese), "%s?", term);
    
    return nars_input(ctx, narsese);
}

/* ============================================================================
 * NARS Reasoning Cycles
 * ============================================================================ */

int nars_cycle(cog_context_t* ctx, int cycles) {
    if (!ctx || cycles <= 0) return 0;
    
    nars_state_t* state = get_nars_state(ctx);
    if (!state || !state->initialized) return 0;
    
    int derivations = 0;
    
#ifdef HAVE_OPENNARS
    for (int i = 0; i < cycles; i++) {
        NAR_Cycles(1);
        /* Check for outputs and emit them */
    }
#else
    /* Simulated cycles for testing */
    derivations = cycles / 10;  /* Approximate derivation rate */
#endif
    
    pthread_mutex_lock(&state->stats_mutex);
    state->stats.total_cycles += cycles;
    state->stats.total_derivations += derivations;
    pthread_mutex_unlock(&state->stats_mutex);
    
    return derivations;
}

int nars_run(cog_context_t* ctx, int max_cycles, int timeout_ms) {
    if (!ctx || max_cycles <= 0) return 0;
    
    struct timespec start, now;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    int total_cycles = 0;
    int batch_size = 100;
    
    while (total_cycles < max_cycles) {
        int remaining = max_cycles - total_cycles;
        int to_run = remaining < batch_size ? remaining : batch_size;
        
        nars_cycle(ctx, to_run);
        total_cycles += to_run;
        
        /* Check timeout */
        if (timeout_ms > 0) {
            clock_gettime(CLOCK_MONOTONIC, &now);
            long elapsed_ms = (now.tv_sec - start.tv_sec) * 1000 +
                             (now.tv_nsec - start.tv_nsec) / 1000000;
            if (elapsed_ms >= timeout_ms) break;
        }
    }
    
    return total_cycles;
}

/* ============================================================================
 * NARS Output Handling
 * ============================================================================ */

int nars_register_output_callback(cog_context_t* ctx,
                                   nars_output_callback_t callback,
                                   void* user_data) {
    if (!ctx || !callback) return -1;
    
    nars_state_t* state = get_nars_state(ctx);
    if (!state) return -1;
    
    pthread_mutex_lock(&state->output_mutex);
    
    if (state->output_callback_count >= MAX_OUTPUT_CALLBACKS) {
        pthread_mutex_unlock(&state->output_mutex);
        return -1;
    }
    
    nars_output_callback_entry_t* entry = 
        &state->output_callbacks[state->output_callback_count++];
    entry->callback = callback;
    entry->user_data = user_data;
    
    pthread_mutex_unlock(&state->output_mutex);
    
    return 0;
}

/* ============================================================================
 * NARS-AtomSpace Bridge
 * ============================================================================ */

cog_atom_t nars_term_to_atom(cog_context_t* ctx, const char* narsese) {
    if (!ctx || !narsese) return 0;
    
    /* Parse Narsese term and create corresponding atom structure */
    /* This is a simplified implementation */
    
    /* Check for compound terms */
    if (narsese[0] == '<' || narsese[0] == '(') {
        /* Compound term - parse and create link */
        /* For now, create a concept with the full term as name */
        return cog_create_concept(ctx, narsese);
    }
    
    /* Simple term - create concept */
    return cog_create_concept(ctx, narsese);
}

int nars_atom_to_term(cog_context_t* ctx,
                       cog_atom_t atom,
                       char* buffer,
                       size_t buffer_size) {
    if (!ctx || !buffer || buffer_size == 0) return -1;
    
    /* Convert atom to Narsese representation */
    return cog_atom_to_string(ctx, atom, buffer, buffer_size);
}

int nars_sync_atomspace(cog_context_t* ctx, int direction) {
    if (!ctx) return -1;
    
    int synced = 0;
    
    /* Synchronization would iterate through atoms/beliefs and convert */
    /* This is a placeholder for the full implementation */
    
    fprintf(stderr, "[NARS] Sync with AtomSpace (direction=%d): %d items\n",
            direction, synced);
    
    return synced;
}

/* ============================================================================
 * NARS Operations
 * ============================================================================ */

int nars_register_operation(cog_context_t* ctx,
                             const char* op_name,
                             nars_operation_t operation,
                             void* user_data) {
    if (!ctx || !op_name || !operation) return -1;
    
    nars_state_t* state = get_nars_state(ctx);
    if (!state) return -1;
    
    pthread_mutex_lock(&state->operation_mutex);
    
    if (state->operation_count >= MAX_OPERATIONS) {
        pthread_mutex_unlock(&state->operation_mutex);
        return -1;
    }
    
    nars_operation_entry_t* entry = &state->operations[state->operation_count++];
    strncpy(entry->name, op_name, sizeof(entry->name) - 1);
    entry->callback = operation;
    entry->user_data = user_data;
    
    pthread_mutex_unlock(&state->operation_mutex);
    
    fprintf(stderr, "[NARS] Registered operation: %s\n", op_name);
    
    return 0;
}

/* ============================================================================
 * NARS Statistics
 * ============================================================================ */

int nars_get_stats(cog_context_t* ctx, nars_stats_t* stats) {
    if (!ctx || !stats) return -1;
    
    nars_state_t* state = get_nars_state(ctx);
    if (!state) return -1;
    
    pthread_mutex_lock(&state->stats_mutex);
    *stats = state->stats;
    pthread_mutex_unlock(&state->stats_mutex);
    
    return 0;
}
