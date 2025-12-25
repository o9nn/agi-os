/**
 * @file metta_integration.c
 * @brief MeTTa/Hyperon Integration Implementation for AGI-OS
 * 
 * @copyright AGI-OS Project
 * @license GPL-3.0
 */

#include "metta_integration.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

/* Include Hyperon C API when available */
#ifdef HAVE_HYPERON
#include "hyperon.h"
#endif

/* ============================================================================
 * Internal Structures
 * ============================================================================ */

#define MAX_SPACES 64
#define MAX_ATOMS_PER_SPACE 100000
#define MAX_RUNNERS 32
#define MAX_MODULES 128
#define MAX_GROUNDED_OPS 256

typedef struct metta_atom_internal {
    metta_atom_t handle;
    metta_atom_type_t type;
    char* name;
    struct metta_atom_internal** children;
    size_t child_count;
    void* grounded_data;
} metta_atom_internal_t;

typedef struct {
    metta_space_t handle;
    char name[256];
    metta_atom_internal_t** atoms;
    size_t atom_count;
    size_t atom_capacity;
    pthread_mutex_t mutex;
} metta_space_internal_t;

typedef struct {
    metta_runner_t handle;
    metta_space_t space;
    bool running;
} metta_runner_internal_t;

typedef struct {
    char name[128];
    metta_grounded_op_t callback;
    void* user_data;
} grounded_op_entry_t;

typedef struct {
    bool initialized;
    metta_config_t config;
    
    /* Spaces */
    metta_space_internal_t spaces[MAX_SPACES];
    uint64_t next_space_handle;
    metta_space_t default_space;
    pthread_mutex_t space_mutex;
    
    /* Atoms */
    uint64_t next_atom_handle;
    pthread_mutex_t atom_mutex;
    
    /* Runners */
    metta_runner_internal_t runners[MAX_RUNNERS];
    uint64_t next_runner_handle;
    pthread_mutex_t runner_mutex;
    
    /* Grounded operations */
    grounded_op_entry_t grounded_ops[MAX_GROUNDED_OPS];
    int grounded_op_count;
    pthread_mutex_t grounded_mutex;
    
    /* Statistics */
    metta_stats_t stats;
    pthread_mutex_t stats_mutex;
    
#ifdef HAVE_HYPERON
    hyperon_metta_t* hyperon_metta;
#endif
} metta_state_t;

static __thread metta_state_t* tls_metta_state = NULL;

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static metta_state_t* get_metta_state(cog_context_t* ctx) {
    (void)ctx;
    return tls_metta_state;
}

static metta_space_internal_t* find_space(metta_state_t* state, metta_space_t handle) {
    for (int i = 0; i < MAX_SPACES; i++) {
        if (state->spaces[i].handle == handle && state->spaces[i].atoms != NULL) {
            return &state->spaces[i];
        }
    }
    return NULL;
}

static metta_runner_internal_t* find_runner(metta_state_t* state, metta_runner_t handle) {
    for (int i = 0; i < MAX_RUNNERS; i++) {
        if (state->runners[i].handle == handle) {
            return &state->runners[i];
        }
    }
    return NULL;
}

static metta_atom_internal_t* create_atom(metta_state_t* state, metta_atom_type_t type) {
    metta_atom_internal_t* atom = calloc(1, sizeof(metta_atom_internal_t));
    if (!atom) return NULL;
    
    pthread_mutex_lock(&state->atom_mutex);
    atom->handle = state->next_atom_handle++;
    state->stats.atoms_created++;
    pthread_mutex_unlock(&state->atom_mutex);
    
    atom->type = type;
    return atom;
}

static void free_atom(metta_atom_internal_t* atom) {
    if (!atom) return;
    
    free(atom->name);
    if (atom->children) {
        for (size_t i = 0; i < atom->child_count; i++) {
            free_atom(atom->children[i]);
        }
        free(atom->children);
    }
    free(atom);
}

/* ============================================================================
 * MeTTa Initialization
 * ============================================================================ */

int metta_init(cog_context_t* ctx, const metta_config_t* config) {
    if (!ctx) return -1;
    
    metta_state_t* state = calloc(1, sizeof(metta_state_t));
    if (!state) return -1;
    
    /* Apply configuration */
    if (config) {
        state->config = *config;
    } else {
        metta_config_t defaults = METTA_CONFIG_DEFAULT;
        state->config = defaults;
    }
    
    /* Initialize mutexes */
    pthread_mutex_init(&state->space_mutex, NULL);
    pthread_mutex_init(&state->atom_mutex, NULL);
    pthread_mutex_init(&state->runner_mutex, NULL);
    pthread_mutex_init(&state->grounded_mutex, NULL);
    pthread_mutex_init(&state->stats_mutex, NULL);
    
    state->next_space_handle = 1;
    state->next_atom_handle = 1;
    state->next_runner_handle = 1;
    
#ifdef HAVE_HYPERON
    /* Initialize Hyperon MeTTa runtime */
    state->hyperon_metta = hyperon_metta_new();
#endif
    
    state->initialized = true;
    tls_metta_state = state;
    
    /* Create default space */
    state->default_space = metta_space_new(ctx, "default");
    
    /* Load standard library if enabled */
    if (state->config.enable_stdlib) {
        metta_load_stdlib(ctx, state->default_space);
    }
    
    fprintf(stderr, "[METTA] Initialized with max_atoms=%zu, stdlib=%s\n",
            state->config.max_atoms,
            state->config.enable_stdlib ? "enabled" : "disabled");
    
    return 0;
}

void metta_shutdown(cog_context_t* ctx) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return;
    
#ifdef HAVE_HYPERON
    if (state->hyperon_metta) {
        hyperon_metta_free(state->hyperon_metta);
    }
#endif
    
    /* Free all spaces */
    for (int i = 0; i < MAX_SPACES; i++) {
        if (state->spaces[i].atoms) {
            for (size_t j = 0; j < state->spaces[i].atom_count; j++) {
                free_atom(state->spaces[i].atoms[j]);
            }
            free(state->spaces[i].atoms);
            pthread_mutex_destroy(&state->spaces[i].mutex);
        }
    }
    
    pthread_mutex_destroy(&state->space_mutex);
    pthread_mutex_destroy(&state->atom_mutex);
    pthread_mutex_destroy(&state->runner_mutex);
    pthread_mutex_destroy(&state->grounded_mutex);
    pthread_mutex_destroy(&state->stats_mutex);
    
    free(state);
    tls_metta_state = NULL;
    
    fprintf(stderr, "[METTA] Shutdown complete\n");
}

const char* metta_version(void) {
    return "0.1.0-agi-os";
}

/* ============================================================================
 * MeTTa Space Operations
 * ============================================================================ */

metta_space_t metta_space_new(cog_context_t* ctx, const char* name) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return 0;
    
    pthread_mutex_lock(&state->space_mutex);
    
    /* Find free slot */
    metta_space_internal_t* space = NULL;
    for (int i = 0; i < MAX_SPACES; i++) {
        if (state->spaces[i].atoms == NULL) {
            space = &state->spaces[i];
            break;
        }
    }
    
    if (!space) {
        pthread_mutex_unlock(&state->space_mutex);
        return 0;
    }
    
    space->handle = state->next_space_handle++;
    strncpy(space->name, name ? name : "unnamed", sizeof(space->name) - 1);
    space->atom_capacity = 1024;
    space->atoms = calloc(space->atom_capacity, sizeof(metta_atom_internal_t*));
    space->atom_count = 0;
    pthread_mutex_init(&space->mutex, NULL);
    
    pthread_mutex_unlock(&state->space_mutex);
    
    return space->handle;
}

void metta_space_free(cog_context_t* ctx, metta_space_t space_handle) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return;
    
    pthread_mutex_lock(&state->space_mutex);
    
    metta_space_internal_t* space = find_space(state, space_handle);
    if (space) {
        pthread_mutex_lock(&space->mutex);
        for (size_t i = 0; i < space->atom_count; i++) {
            free_atom(space->atoms[i]);
        }
        free(space->atoms);
        space->atoms = NULL;
        space->handle = 0;
        pthread_mutex_unlock(&space->mutex);
        pthread_mutex_destroy(&space->mutex);
    }
    
    pthread_mutex_unlock(&state->space_mutex);
}

metta_space_t metta_space_default(cog_context_t* ctx) {
    metta_state_t* state = get_metta_state(ctx);
    return state ? state->default_space : 0;
}

int metta_space_add(cog_context_t* ctx, metta_space_t space_handle, metta_atom_t atom) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return -1;
    
    metta_space_internal_t* space = find_space(state, space_handle);
    if (!space) return -1;
    
    pthread_mutex_lock(&space->mutex);
    
    /* Expand if needed */
    if (space->atom_count >= space->atom_capacity) {
        space->atom_capacity *= 2;
        space->atoms = realloc(space->atoms, 
                               space->atom_capacity * sizeof(metta_atom_internal_t*));
    }
    
    /* Create a placeholder atom entry */
    metta_atom_internal_t* new_atom = create_atom(state, METTA_ATOM_SYMBOL);
    new_atom->handle = atom;
    space->atoms[space->atom_count++] = new_atom;
    
    pthread_mutex_lock(&state->stats_mutex);
    state->stats.atoms_in_space++;
    pthread_mutex_unlock(&state->stats_mutex);
    
    pthread_mutex_unlock(&space->mutex);
    
    return 0;
}

int metta_space_remove(cog_context_t* ctx, metta_space_t space_handle, metta_atom_t atom) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return -1;
    
    metta_space_internal_t* space = find_space(state, space_handle);
    if (!space) return -1;
    
    pthread_mutex_lock(&space->mutex);
    
    for (size_t i = 0; i < space->atom_count; i++) {
        if (space->atoms[i] && space->atoms[i]->handle == atom) {
            free_atom(space->atoms[i]);
            /* Shift remaining atoms */
            memmove(&space->atoms[i], &space->atoms[i + 1],
                   (space->atom_count - i - 1) * sizeof(metta_atom_internal_t*));
            space->atom_count--;
            
            pthread_mutex_lock(&state->stats_mutex);
            state->stats.atoms_in_space--;
            pthread_mutex_unlock(&state->stats_mutex);
            
            pthread_mutex_unlock(&space->mutex);
            return 0;
        }
    }
    
    pthread_mutex_unlock(&space->mutex);
    return -1;
}

int metta_space_query(cog_context_t* ctx,
                       metta_space_t space_handle,
                       metta_atom_t pattern,
                       metta_atom_t* results,
                       size_t max_results) {
    (void)pattern;  /* Pattern matching would be implemented fully */
    
    metta_state_t* state = get_metta_state(ctx);
    if (!state || !results) return -1;
    
    metta_space_internal_t* space = find_space(state, space_handle);
    if (!space) return -1;
    
    pthread_mutex_lock(&space->mutex);
    
    /* Simple query - return all atoms (pattern matching would filter) */
    size_t count = 0;
    for (size_t i = 0; i < space->atom_count && count < max_results; i++) {
        if (space->atoms[i]) {
            results[count++] = space->atoms[i]->handle;
        }
    }
    
    pthread_mutex_unlock(&space->mutex);
    
    return count;
}

/* ============================================================================
 * MeTTa Atom Creation
 * ============================================================================ */

metta_atom_t metta_atom_sym(cog_context_t* ctx, const char* name) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state || !name) return 0;
    
    metta_atom_internal_t* atom = create_atom(state, METTA_ATOM_SYMBOL);
    if (!atom) return 0;
    
    atom->name = strdup(name);
    return atom->handle;
}

metta_atom_t metta_atom_var(cog_context_t* ctx, const char* name) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state || !name) return 0;
    
    metta_atom_internal_t* atom = create_atom(state, METTA_ATOM_VARIABLE);
    if (!atom) return 0;
    
    atom->name = strdup(name);
    return atom->handle;
}

metta_atom_t metta_atom_expr(cog_context_t* ctx,
                              const metta_atom_t* children,
                              size_t count) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return 0;
    
    metta_atom_internal_t* atom = create_atom(state, METTA_ATOM_EXPRESSION);
    if (!atom) return 0;
    
    if (count > 0 && children) {
        atom->children = calloc(count, sizeof(metta_atom_internal_t*));
        atom->child_count = count;
        /* Note: In full implementation, would link to actual child atoms */
    }
    
    return atom->handle;
}

metta_atom_t metta_parse(cog_context_t* ctx, const char* code) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state || !code) return 0;
    
    /* Simple parser - handles basic cases */
    while (*code == ' ' || *code == '\n' || *code == '\t') code++;
    
    if (*code == '(') {
        /* Expression */
        return metta_atom_expr(ctx, NULL, 0);
    } else if (*code == '$') {
        /* Variable */
        return metta_atom_var(ctx, code + 1);
    } else {
        /* Symbol */
        char name[256];
        int i = 0;
        while (code[i] && code[i] != ' ' && code[i] != ')' && i < 255) {
            name[i] = code[i];
            i++;
        }
        name[i] = '\0';
        return metta_atom_sym(ctx, name);
    }
}

int metta_atom_to_string(cog_context_t* ctx,
                          metta_atom_t atom,
                          char* buffer,
                          size_t buffer_size) {
    (void)ctx;
    (void)atom;
    
    if (!buffer || buffer_size == 0) return -1;
    
    /* Placeholder - would traverse atom structure */
    snprintf(buffer, buffer_size, "(atom %lu)", atom);
    return strlen(buffer);
}

metta_atom_type_t metta_atom_type(cog_context_t* ctx, metta_atom_t atom) {
    (void)ctx;
    (void)atom;
    return METTA_ATOM_SYMBOL;  /* Placeholder */
}

/* ============================================================================
 * MeTTa Execution
 * ============================================================================ */

metta_runner_t metta_runner_new(cog_context_t* ctx, metta_space_t space) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return 0;
    
    pthread_mutex_lock(&state->runner_mutex);
    
    /* Find free slot */
    metta_runner_internal_t* runner = NULL;
    for (int i = 0; i < MAX_RUNNERS; i++) {
        if (state->runners[i].handle == 0) {
            runner = &state->runners[i];
            break;
        }
    }
    
    if (!runner) {
        pthread_mutex_unlock(&state->runner_mutex);
        return 0;
    }
    
    runner->handle = state->next_runner_handle++;
    runner->space = space;
    runner->running = false;
    
    pthread_mutex_unlock(&state->runner_mutex);
    
    return runner->handle;
}

void metta_runner_free(cog_context_t* ctx, metta_runner_t runner_handle) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return;
    
    pthread_mutex_lock(&state->runner_mutex);
    
    metta_runner_internal_t* runner = find_runner(state, runner_handle);
    if (runner) {
        runner->handle = 0;
        runner->running = false;
    }
    
    pthread_mutex_unlock(&state->runner_mutex);
}

int metta_run(cog_context_t* ctx,
               metta_runner_t runner_handle,
               const char* code,
               metta_atom_t* results,
               size_t max_results) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state || !code || !results) return -1;
    
    metta_runner_internal_t* runner = find_runner(state, runner_handle);
    if (!runner) return -1;
    
    fprintf(stderr, "[METTA] Running: %s\n", code);
    
    /* Parse and evaluate */
    metta_atom_t parsed = metta_parse(ctx, code);
    if (!parsed) return -1;
    
    return metta_eval(ctx, runner_handle, parsed, results, max_results);
}

int metta_eval(cog_context_t* ctx,
                metta_runner_t runner_handle,
                metta_atom_t atom,
                metta_atom_t* results,
                size_t max_results) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state || !results || max_results == 0) return -1;
    
    metta_runner_internal_t* runner = find_runner(state, runner_handle);
    if (!runner) return -1;
    
    pthread_mutex_lock(&state->stats_mutex);
    state->stats.evaluations++;
    pthread_mutex_unlock(&state->stats_mutex);
    
    /* Simple evaluation - return atom as-is */
    /* Full implementation would perform reduction */
    results[0] = atom;
    
    return 1;
}

int metta_step(cog_context_t* ctx, metta_runner_t runner_handle) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return -1;
    
    metta_runner_internal_t* runner = find_runner(state, runner_handle);
    if (!runner) return -1;
    
    /* Single step execution */
    pthread_mutex_lock(&state->stats_mutex);
    state->stats.reductions++;
    pthread_mutex_unlock(&state->stats_mutex);
    
    return 1;  /* Complete */
}

/* ============================================================================
 * MeTTa Modules
 * ============================================================================ */

metta_module_t metta_load_module(cog_context_t* ctx, const char* path) {
    if (!path) return 0;
    
    fprintf(stderr, "[METTA] Loading module: %s\n", path);
    
    /* Read file and load */
    FILE* f = fopen(path, "r");
    if (!f) return 0;
    
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    char* code = malloc(size + 1);
    if (!code) {
        fclose(f);
        return 0;
    }
    
    size_t read = fread(code, 1, size, f);
    code[read] = '\0';
    fclose(f);
    
    /* Extract module name from path */
    const char* name = strrchr(path, '/');
    name = name ? name + 1 : path;
    
    metta_module_t module = metta_load_module_string(ctx, name, code);
    free(code);
    
    return module;
}

metta_module_t metta_load_module_string(cog_context_t* ctx,
                                         const char* name,
                                         const char* code) {
    metta_state_t* state = get_metta_state(ctx);
    if (!state || !name || !code) return 0;
    
    static uint64_t next_module_handle = 1;
    
    fprintf(stderr, "[METTA] Loading module '%s' from string\n", name);
    
    /* Parse and add to default space */
    metta_atom_t atom = metta_parse(ctx, code);
    if (atom) {
        metta_space_add(ctx, state->default_space, atom);
    }
    
    return next_module_handle++;
}

int metta_import_module(cog_context_t* ctx,
                         metta_space_t space,
                         metta_module_t module) {
    (void)ctx;
    (void)space;
    (void)module;
    
    fprintf(stderr, "[METTA] Importing module %lu into space %lu\n", module, space);
    return 0;
}

/* ============================================================================
 * MeTTa-AtomSpace Bridge
 * ============================================================================ */

cog_atom_t metta_to_cog_atom(cog_context_t* ctx, metta_atom_t metta_atom) {
    /* Convert MeTTa atom to cognitive atom */
    char buffer[256];
    metta_atom_to_string(ctx, metta_atom, buffer, sizeof(buffer));
    return cog_create_concept(ctx, buffer);
}

metta_atom_t cog_to_metta_atom(cog_context_t* ctx, cog_atom_t cog_atom) {
    /* Convert cognitive atom to MeTTa atom */
    char buffer[256];
    cog_atom_to_string(ctx, cog_atom, buffer, sizeof(buffer));
    return metta_atom_sym(ctx, buffer);
}

int metta_sync_atomspace(cog_context_t* ctx, metta_space_t space, int direction) {
    (void)ctx;
    (void)space;
    
    fprintf(stderr, "[METTA] Sync with AtomSpace (direction=%d)\n", direction);
    return 0;
}

/* ============================================================================
 * MeTTa Type System
 * ============================================================================ */

int metta_define_type(cog_context_t* ctx,
                       metta_space_t space,
                       const char* type_name,
                       metta_atom_t type_def) {
    (void)ctx;
    (void)space;
    (void)type_def;
    
    fprintf(stderr, "[METTA] Defining type: %s\n", type_name);
    return 0;
}

metta_atom_t metta_get_type(cog_context_t* ctx,
                             metta_space_t space,
                             metta_atom_t atom) {
    (void)ctx;
    (void)space;
    (void)atom;
    return 0;  /* Untyped */
}

int metta_check_type(cog_context_t* ctx,
                      metta_space_t space,
                      metta_atom_t atom,
                      metta_atom_t type) {
    (void)ctx;
    (void)space;
    (void)atom;
    (void)type;
    return 1;  /* Assume matches */
}

/* ============================================================================
 * MeTTa Grounded Operations
 * ============================================================================ */

int metta_register_grounded_op(cog_context_t* ctx,
                                metta_space_t space,
                                const char* name,
                                metta_grounded_op_t op,
                                void* user_data) {
    (void)space;
    
    metta_state_t* state = get_metta_state(ctx);
    if (!state || !name || !op) return -1;
    
    pthread_mutex_lock(&state->grounded_mutex);
    
    if (state->grounded_op_count >= MAX_GROUNDED_OPS) {
        pthread_mutex_unlock(&state->grounded_mutex);
        return -1;
    }
    
    grounded_op_entry_t* entry = &state->grounded_ops[state->grounded_op_count++];
    strncpy(entry->name, name, sizeof(entry->name) - 1);
    entry->callback = op;
    entry->user_data = user_data;
    
    pthread_mutex_unlock(&state->grounded_mutex);
    
    fprintf(stderr, "[METTA] Registered grounded op: %s\n", name);
    return 0;
}

/* ============================================================================
 * MeTTa Statistics
 * ============================================================================ */

int metta_get_stats(cog_context_t* ctx, metta_stats_t* stats) {
    if (!stats) return -1;
    
    metta_state_t* state = get_metta_state(ctx);
    if (!state) return -1;
    
    pthread_mutex_lock(&state->stats_mutex);
    *stats = state->stats;
    pthread_mutex_unlock(&state->stats_mutex);
    
    return 0;
}

/* ============================================================================
 * Standard Library Functions
 * ============================================================================ */

int metta_load_stdlib(cog_context_t* ctx, metta_space_t space) {
    fprintf(stderr, "[METTA] Loading standard library\n");
    
    /* Define basic types */
    metta_atom_t type_atom = metta_atom_sym(ctx, "Type");
    metta_space_add(ctx, space, type_atom);
    
    /* Define basic operations */
    const char* stdlib_ops[] = {
        "if", "let", "let*", "match", "case",
        "=", "==", "!=", "<", ">", "<=", ">=",
        "+", "-", "*", "/", "%",
        "and", "or", "not",
        "cons", "car", "cdr", "list",
        "get-type", "get-metatype",
        NULL
    };
    
    for (int i = 0; stdlib_ops[i]; i++) {
        metta_atom_t op = metta_atom_sym(ctx, stdlib_ops[i]);
        metta_space_add(ctx, space, op);
    }
    
    return 0;
}

int metta_load_pln(cog_context_t* ctx, metta_space_t space) {
    fprintf(stderr, "[METTA] Loading PLN module\n");
    
    /* Define PLN types and rules */
    const char* pln_symbols[] = {
        "TruthValue", "Strength", "Confidence",
        "Inheritance", "Similarity", "Implication",
        "deduction", "induction", "abduction",
        "modus-ponens", "modus-tollens",
        NULL
    };
    
    for (int i = 0; pln_symbols[i]; i++) {
        metta_atom_t sym = metta_atom_sym(ctx, pln_symbols[i]);
        metta_space_add(ctx, space, sym);
    }
    
    return 0;
}

int metta_load_nars(cog_context_t* ctx, metta_space_t space) {
    fprintf(stderr, "[METTA] Loading NARS integration module\n");
    
    /* Define NARS-compatible symbols */
    const char* nars_symbols[] = {
        "nars-belief", "nars-goal", "nars-question",
        "nars-cycle", "nars-input",
        "-->", "<->", "==>", "<=>",
        "&", "|", "-", "~",
        NULL
    };
    
    for (int i = 0; nars_symbols[i]; i++) {
        metta_atom_t sym = metta_atom_sym(ctx, nars_symbols[i]);
        metta_space_add(ctx, space, sym);
    }
    
    return 0;
}
