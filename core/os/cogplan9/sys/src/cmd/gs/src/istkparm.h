#ifndef istkparm_INCLUDED
#  define istkparm_INCLUDED
struct ref_stack_params_s {
uint bot_guard;
uint top_guard;
uint block_size;
uint data_size;
ref guard_value;
int underflow_error;
int overflow_error;
bool allow_expansion;
};
#define private_st_ref_stack_params() \
gs_private_st_simple(st_ref_stack_params, ref_stack_params_t,\
"ref_stack_params_t")
#endif