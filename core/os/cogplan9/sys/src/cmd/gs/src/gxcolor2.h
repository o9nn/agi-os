#ifndef gxcolor2_INCLUDED
#  define gxcolor2_INCLUDED
#include "gscolor2.h"
#include "gsmatrix.h"
#include "gsrefct.h"
#include "gxbitmap.h"
struct gs_indexed_map_s {
rc_header rc;
union {
int (*lookup_index)(const gs_indexed_params *, int, float *);
int (*tint_transform)(const gs_separation_params *, floatp, float *);
} proc;
void *proc_data;
uint num_values;
float *values;
};
#define private_st_indexed_map() \
gs_private_st_ptrs2(st_indexed_map, gs_indexed_map, "gs_indexed_map",\
indexed_map_enum_ptrs, indexed_map_reloc_ptrs, proc_data, values)
int lookup_indexed_map(const gs_indexed_params *, int, float *);
int alloc_indexed_map(gs_indexed_map ** ppmap, int num_values,
gs_memory_t * mem, client_name_t cname);
rc_free_proc(free_indexed_map);
#ifndef gs_pattern1_instance_t_DEFINED
#  define gs_pattern1_instance_t_DEFINED
typedef struct gs_pattern1_instance_s gs_pattern1_instance_t;
#endif
struct gs_pattern1_instance_s {
gs_pattern_instance_common;
gs_pattern1_template_t template;
gs_matrix step_matrix;
gs_rect bbox;
bool is_simple;
bool uses_mask;
gs_int_point size;
gx_bitmap_id id;
};
#define private_st_pattern1_instance() \
gs_private_st_composite(st_pattern1_instance, gs_pattern1_instance_t,\
"gs_pattern1_instance_t", pattern1_instance_enum_ptrs,\
pattern1_instance_reloc_ptrs)
#endif