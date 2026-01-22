#ifndef imemory_INCLUDED
#  define imemory_INCLUDED
#include "ivmspace.h"
#include "gsalloc.h"
#ifndef gs_ref_memory_DEFINED
#  define gs_ref_memory_DEFINED
typedef struct gs_ref_memory_s gs_ref_memory_t;
#endif
int gs_alloc_ref_array(gs_ref_memory_t * mem, ref * paref,
uint attrs, uint num_refs, client_name_t cname);
int gs_resize_ref_array(gs_ref_memory_t * mem, ref * paref,
uint new_num_refs, client_name_t cname);
void gs_free_ref_array(gs_ref_memory_t * mem, ref * paref,
client_name_t cname);
int gs_alloc_string_ref(gs_ref_memory_t * mem, ref * psref,
uint attrs, uint nbytes, client_name_t cname);
int gs_register_ref_root(gs_memory_t *mem, gs_gc_root_t *root,
void **pp, client_name_t cname);
#ifndef gs_dual_memory_DEFINED
#  define gs_dual_memory_DEFINED
typedef struct gs_dual_memory_s gs_dual_memory_t;
#endif
struct gs_dual_memory_s {
gs_ref_memory_t *current;
vm_spaces spaces;
uint current_space;
int (*reclaim) (gs_dual_memory_t *, int);
uint test_mask;
uint new_mask;
};
#define public_st_gs_dual_memory()	\
gs_public_st_simple(st_gs_dual_memory, gs_dual_memory_t, "gs_dual_memory_t")
#define st_gs_dual_memory_num_ptrs 0
#endif