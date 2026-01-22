#ifndef idsdata_INCLUDED
# define idsdata_INCLUDED
#include "isdata.h"
#ifndef dict_stack_DEFINED
# define dict_stack_DEFINED
typedef struct dict_stack_s dict_stack_t;
#endif
struct dict_stack_s {
ref_stack_t stack;
uint min_size;
int userdict_index;
int def_space;
const ref_packed *top_keys;
uint top_npairs;
ref *top_values;
ref system_dict;
};
#define public_st_dict_stack() \
gs_public_st_suffix_add0(st_dict_stack, dict_stack_t, "dict_stack_t",\
dict_stack_enum_ptrs, dict_stack_reloc_ptrs, st_ref_stack)
#define st_dict_stack_num_ptrs st_ref_stack_num_ptrs
#endif