#ifndef idebug_INCLUDED
#  define idebug_INCLUDED
void debug_print_name(const gs_memory_t *mem, const ref *);
void debug_print_name_index(const gs_memory_t *mem, uint );
void debug_print_ref(const gs_memory_t *mem, const ref *);
void debug_print_ref_packed(const gs_memory_t *mem, const ref_packed *);
void debug_dump_one_ref(const gs_memory_t *mem, const ref *);
void debug_dump_refs(const gs_memory_t *mem,
const ref * from, uint size, const char *msg);
void debug_dump_array(const gs_memory_t *mem, const ref * array);
#ifndef ref_stack_DEFINED
typedef struct ref_stack_s ref_stack_t;
#  define ref_stack_DEFINED
#endif
void debug_dump_stack(const gs_memory_t *mem,
const ref_stack_t * pstack, const char *msg);
#endif