#ifndef iesdata_INCLUDED
#  define iesdata_INCLUDED
#include "isdata.h"
typedef struct exec_stack_s {
ref_stack_t stack;
ref *current_file;
} exec_stack_t;
#define public_st_exec_stack()	\
gs_public_st_suffix_add0(st_exec_stack, exec_stack_t, "exec_stack_t",\
exec_stack_enum_ptrs, exec_stack_reloc_ptrs, st_ref_stack)
#define st_exec_stack_num_ptrs st_ref_stack_num_ptrs
#endif