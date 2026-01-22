#ifndef gsalloc_INCLUDED
#  define gsalloc_INCLUDED
#ifndef gs_ref_memory_DEFINED
#  define gs_ref_memory_DEFINED
typedef struct gs_ref_memory_s gs_ref_memory_t;
#endif
typedef struct gs_memory_gc_status_s {
long vm_threshold;
long max_vm;
int *psignal;
int signal_value;
bool enabled;
long requested;
} gs_memory_gc_status_t;
void gs_memory_gc_status(const gs_ref_memory_t *, gs_memory_gc_status_t *);
void gs_memory_set_gc_status(gs_ref_memory_t *, const gs_memory_gc_status_t *);
void gs_memory_set_vm_threshold(gs_ref_memory_t * mem, long val);
void gs_memory_set_vm_reclaim(gs_ref_memory_t * mem, bool enabled);
gs_ref_memory_t *ialloc_alloc_state(gs_memory_t *, uint);
int ialloc_add_chunk(gs_ref_memory_t *, ulong, client_name_t);
void ialloc_gc_prepare(gs_ref_memory_t *);
void ialloc_reset(gs_ref_memory_t *);
void ialloc_reset_free(gs_ref_memory_t *);
void ialloc_set_limit(gs_ref_memory_t *);
void ialloc_consolidate_free(gs_ref_memory_t *);
#endif