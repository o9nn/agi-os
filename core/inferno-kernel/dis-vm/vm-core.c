/**
 * Dis Virtual Machine - Core Implementation
 * 
 * This is the core of the Dis VM, responsible for:
 * - Bytecode interpretation
 * - Instruction dispatch
 * - Stack management
 * - Module loading
 * - Garbage collection coordination
 * 
 * The Dis VM is a register-based virtual machine designed for
 * the Limbo programming language in Inferno OS.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <pthread.h>

/* Dis VM version */
#define DIS_VM_VERSION_MAJOR 1
#define DIS_VM_VERSION_MINOR 0
#define DIS_VM_VERSION_PATCH 0

/* VM state */
typedef enum {
    DIS_VM_STATE_UNINITIALIZED,
    DIS_VM_STATE_INITIALIZED,
    DIS_VM_STATE_RUNNING,
    DIS_VM_STATE_SUSPENDED,
    DIS_VM_STATE_TERMINATED
} dis_vm_state_t;

/* VM context */
typedef struct dis_vm_context {
    dis_vm_state_t state;
    void *heap;
    size_t heap_size;
    void *stack;
    size_t stack_size;
    pthread_mutex_t lock;
} dis_vm_context_t;

/* Global VM instance */
static dis_vm_context_t *global_vm = NULL;

/**
 * Initialize the Dis VM
 */
int dis_vm_init(void) {
    if (global_vm != NULL) {
        fprintf(stderr, "Dis VM already initialized\n");
        return -1;
    }
    
    global_vm = (dis_vm_context_t *)malloc(sizeof(dis_vm_context_t));
    if (global_vm == NULL) {
        fprintf(stderr, "Failed to allocate VM context\n");
        return -1;
    }
    
    memset(global_vm, 0, sizeof(dis_vm_context_t));
    
    /* Initialize heap (1MB default) */
    global_vm->heap_size = 1024 * 1024;
    global_vm->heap = malloc(global_vm->heap_size);
    if (global_vm->heap == NULL) {
        fprintf(stderr, "Failed to allocate VM heap\n");
        free(global_vm);
        global_vm = NULL;
        return -1;
    }
    
    /* Initialize stack (64KB default) */
    global_vm->stack_size = 64 * 1024;
    global_vm->stack = malloc(global_vm->stack_size);
    if (global_vm->stack == NULL) {
        fprintf(stderr, "Failed to allocate VM stack\n");
        free(global_vm->heap);
        free(global_vm);
        global_vm = NULL;
        return -1;
    }
    
    /* Initialize mutex */
    pthread_mutex_init(&global_vm->lock, NULL);
    
    global_vm->state = DIS_VM_STATE_INITIALIZED;
    
    printf("Dis VM initialized (version %d.%d.%d)\n",
           DIS_VM_VERSION_MAJOR, DIS_VM_VERSION_MINOR, DIS_VM_VERSION_PATCH);
    printf("  Heap size: %zu bytes\n", global_vm->heap_size);
    printf("  Stack size: %zu bytes\n", global_vm->stack_size);
    
    return 0;
}

/**
 * Shutdown the Dis VM
 */
void dis_vm_shutdown(void) {
    if (global_vm == NULL) {
        return;
    }
    
    pthread_mutex_lock(&global_vm->lock);
    
    global_vm->state = DIS_VM_STATE_TERMINATED;
    
    if (global_vm->heap != NULL) {
        free(global_vm->heap);
        global_vm->heap = NULL;
    }
    
    if (global_vm->stack != NULL) {
        free(global_vm->stack);
        global_vm->stack = NULL;
    }
    
    pthread_mutex_unlock(&global_vm->lock);
    pthread_mutex_destroy(&global_vm->lock);
    
    free(global_vm);
    global_vm = NULL;
    
    printf("Dis VM shutdown complete\n");
}

/**
 * Get VM state
 */
dis_vm_state_t dis_vm_get_state(void) {
    if (global_vm == NULL) {
        return DIS_VM_STATE_UNINITIALIZED;
    }
    return global_vm->state;
}

/**
 * Execute bytecode (stub implementation)
 */
int dis_vm_execute(const uint8_t *bytecode, size_t bytecode_size) {
    if (global_vm == NULL) {
        fprintf(stderr, "Dis VM not initialized\n");
        return -1;
    }
    
    if (bytecode == NULL || bytecode_size == 0) {
        fprintf(stderr, "Invalid bytecode\n");
        return -1;
    }
    
    pthread_mutex_lock(&global_vm->lock);
    
    global_vm->state = DIS_VM_STATE_RUNNING;
    
    /* TODO: Implement bytecode interpretation */
    printf("Dis VM: Executing %zu bytes of bytecode (stub)\n", bytecode_size);
    
    global_vm->state = DIS_VM_STATE_INITIALIZED;
    
    pthread_mutex_unlock(&global_vm->lock);
    
    return 0;
}

/**
 * Get VM version
 */
void dis_vm_get_version(int *major, int *minor, int *patch) {
    if (major != NULL) *major = DIS_VM_VERSION_MAJOR;
    if (minor != NULL) *minor = DIS_VM_VERSION_MINOR;
    if (patch != NULL) *patch = DIS_VM_VERSION_PATCH;
}
