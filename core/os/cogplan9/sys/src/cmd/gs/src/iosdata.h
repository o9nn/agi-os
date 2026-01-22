#ifndef iosdata_INCLUDED
#  define iosdata_INCLUDED
#include "isdata.h"
typedef struct op_stack_s {
ref_stack_t stack;
} op_stack_t;
#define public_st_op_stack()	\
gs_public_st_suffix_add0(st_op_stack, op_stack_t, "op_stack_t",\
op_stack_enum_ptrs, op_stack_reloc_ptrs, st_ref_stack)
#define st_op_stack_num_ptrs st_ref_stack_num_ptrs
#endif