#ifndef istack_INCLUDED
# define istack_INCLUDED
#include "isdata.h"
typedef struct ref_stack_block_s {
ref next;
ref used;
} ref_stack_block;
#define stack_block_refs (sizeof(ref_stack_block) / sizeof(ref))
int ref_stack_init(ref_stack_t *pstack, const ref *pblock_array,
uint bot_guard, uint top_guard,
const ref *pguard_value, gs_ref_memory_t *mem,
ref_stack_params_t *params);
void ref_stack_allow_expansion(ref_stack_t *pstack, bool expand);
void ref_stack_set_error_codes(ref_stack_t *pstack, int underflow_error,
int overflow_error);
int ref_stack_set_max_count(ref_stack_t *pstack, long nmax);
int ref_stack_set_margin(ref_stack_t *pstack, uint margin);
uint ref_stack_count(const ref_stack_t *pstack);
#define ref_stack_count_inline(pstk)\
((pstk)->p + 1 - (pstk)->bot + (pstk)->extension_used)
#define ref_stack_max_count(pstk) (uint)((pstk)->max_stack.value.intval)
ref *ref_stack_index(const ref_stack_t *pstack, long index);
uint ref_stack_counttomark(const ref_stack_t *pstack);
int ref_stack_store_check(const ref_stack_t *pstack, ref *parray,
uint count, uint skip);
#ifndef gs_dual_memory_DEFINED
# define gs_dual_memory_DEFINED
typedef struct gs_dual_memory_s gs_dual_memory_t;
#endif
int ref_stack_store(const ref_stack_t *pstack, ref *parray, uint count,
uint skip, int age, bool check,
gs_dual_memory_t *idmem, client_name_t cname);
void ref_stack_pop(ref_stack_t *pstack, uint count);
#define ref_stack_clear(pstk) ref_stack_pop(pstk, ref_stack_count(pstk))
#define ref_stack_pop_to(pstk, depth)\
ref_stack_pop(pstk, ref_stack_count(pstk) - (depth))
int ref_stack_pop_block(ref_stack_t *pstack);
int ref_stack_extend(ref_stack_t *pstack, uint request);
int ref_stack_push(ref_stack_t *pstack, uint count);
typedef struct ref_stack_enum_s {
ref_stack_block *block;
ref *ptr;
uint size;
} ref_stack_enum_t;
void ref_stack_enum_begin(ref_stack_enum_t *prse, const ref_stack_t *pstack);
bool ref_stack_enum_next(ref_stack_enum_t *prse);
void ref_stack_cleanup(ref_stack_t *pstack);
void ref_stack_release(ref_stack_t *pstack);
void ref_stack_free(ref_stack_t *pstack);
#endif