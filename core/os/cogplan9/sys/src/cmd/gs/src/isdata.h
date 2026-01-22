#ifndef isdata_INCLUDED
#  define isdata_INCLUDED
typedef ref *s_ptr;
typedef const ref *const_s_ptr;
#ifndef gs_ref_memory_DEFINED
#  define gs_ref_memory_DEFINED
typedef struct gs_ref_memory_s gs_ref_memory_t;
#endif
#ifndef ref_stack_DEFINED
typedef struct ref_stack_s ref_stack_t;
#  define ref_stack_DEFINED
#endif
typedef struct ref_stack_params_s ref_stack_params_t;
struct ref_stack_s {
s_ptr p;
s_ptr bot;
s_ptr top;
ref current;
uint extension_size;
uint extension_used;
ref max_stack;
uint requested;
uint margin;
uint body_size;
ref_stack_params_t *params;
gs_ref_memory_t *memory;
};
#define public_st_ref_stack()	\
gs_public_st_complex_only(st_ref_stack, ref_stack_t, "ref_stack_t",\
ref_stack_clear_marks, ref_stack_enum_ptrs, ref_stack_reloc_ptrs, 0)
#define st_ref_stack_num_ptrs 2
#endif