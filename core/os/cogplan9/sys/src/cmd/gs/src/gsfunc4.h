#ifndef gsfunc4_INCLUDED
#  define gsfunc4_INCLUDED
#include "gsfunc.h"
#define function_type_PostScript_Calculator 4
typedef enum {
PtCr_abs, PtCr_add, PtCr_and, PtCr_atan, PtCr_bitshift,
PtCr_ceiling, PtCr_cos, PtCr_cvi, PtCr_cvr, PtCr_div, PtCr_exp,
PtCr_floor, PtCr_idiv, PtCr_ln, PtCr_log, PtCr_mod, PtCr_mul,
PtCr_neg, PtCr_not, PtCr_or, PtCr_round,
PtCr_sin, PtCr_sqrt, PtCr_sub, PtCr_truncate, PtCr_xor,
PtCr_eq, PtCr_ge, PtCr_gt, PtCr_le, PtCr_lt, PtCr_ne,
PtCr_copy, PtCr_dup, PtCr_exch, PtCr_index, PtCr_pop, PtCr_roll,
PtCr_byte, PtCr_int , PtCr_float ,
PtCr_true, PtCr_false,
PtCr_if, PtCr_else, PtCr_return
} gs_PtCr_opcode_t;
#define PtCr_NUM_OPS ((int)PtCr_byte)
#define PtCr_NUM_OPCODES ((int)PtCr_return + 1)
typedef struct gs_function_PtCr_params_s {
gs_function_params_common;
gs_const_string ops;
} gs_function_PtCr_params_t;
#define private_st_function_PtCr()	\
gs_private_st_suffix_add_strings1(st_function_PtCr, gs_function_PtCr_t,\
"gs_function_PtCr_t", function_PtCr_enum_ptrs, function_PtCr_reloc_ptrs,\
st_function, params.ops)
int gs_function_PtCr_init(gs_function_t ** ppfn,
const gs_function_PtCr_params_t * params,
gs_memory_t * mem);
void gs_function_PtCr_free_params(gs_function_PtCr_params_t * params,
gs_memory_t * mem);
#endif