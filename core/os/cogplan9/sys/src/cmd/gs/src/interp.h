#ifndef interp_INCLUDED
#  define interp_INCLUDED
int i_initial_enter_name(i_ctx_t *, const char *, const ref *);
#define initial_enter_name(nstr, pvalue)\
i_initial_enter_name(i_ctx_p, nstr, pvalue)
void i_initial_remove_name(i_ctx_t *, const char *);
#define initial_remove_name(nstr)\
i_initial_remove_name(i_ctx_p, nstr)
extern const int gs_interp_max_op_num_args;
extern const int gs_interp_num_special_ops;
void gs_interp_make_oper(ref * opref, op_proc_t, int index);
int interp_reclaim(i_ctx_t **pi_ctx_p, int space);
int gs_errorname(i_ctx_t *, int, ref *);
int gs_errorinfo_put_string(i_ctx_t *, const char *);
int gs_interp_init(i_ctx_t **pi_ctx_p, const ref *psystem_dict,
gs_dual_memory_t *dmem);
#ifndef gs_context_state_t_DEFINED
#  define gs_context_state_t_DEFINED
typedef struct gs_context_state_s gs_context_state_t;
#endif
int gs_interp_alloc_stacks(gs_ref_memory_t * smem,
gs_context_state_t * pcst);
void gs_interp_free_stacks(gs_ref_memory_t * smem,
gs_context_state_t * pcst);
void gs_interp_reset(i_ctx_t *i_ctx_p);
int gs_interpret(i_ctx_t **pi_ctx_p, ref * pref, int user_errors,
int *pexit_code, ref * perror_object);
#endif