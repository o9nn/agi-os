#ifndef igc_INCLUDED
#  define igc_INCLUDED
#include "istruct.h"
extern vm_reclaim_proc(gs_gc_reclaim);
struct struct_shared_procs_s {
#define gc_proc_clear_reloc(proc)\
void proc(obj_header_t *pre, uint size)
gc_proc_clear_reloc((*clear_reloc));
#define gc_proc_set_reloc(proc)\
bool proc(obj_header_t *pre, uint reloc, uint size)
gc_proc_set_reloc((*set_reloc));
#define gc_proc_compact(proc)\
void proc(const gs_memory_t *cmem, obj_header_t *pre, obj_header_t *dpre, uint size)
gc_proc_compact((*compact));
};
#ifndef name_table_DEFINED
#  define name_table_DEFINED
typedef struct name_table_s name_table;
#endif
struct gc_state_s {
const gc_procs_with_refs_t *procs;
chunk_locator_t loc;
vm_spaces spaces;
int min_collect;
bool relocating_untraced;
gs_memory_t *heap;
name_table *ntable;
#ifdef DEBUG
chunk_t *container;
#endif
};
ptr_proc_unmark(ptr_ref_unmark);
ptr_proc_mark(ptr_ref_mark);
void ialloc_validate_memory(const gs_ref_memory_t *, gc_state_t *);
void ialloc_validate_chunk(const chunk_t *, gc_state_t *);
void ialloc_validate_object(const obj_header_t *, const chunk_t *,
gc_state_t *);
const gs_memory_t * gcst_get_memory_ptr(gc_state_t *gcst);
const void *print_reloc_proc(const void *obj, const char *cname,
const void *robj);
#ifdef DEBUG
#  define print_reloc(obj, cname, nobj)\
(gs_debug_c('9') ? print_reloc_proc(obj, cname, nobj) : nobj)
#else
#  define print_reloc(obj, cname, nobj) (nobj)
#endif
#endif