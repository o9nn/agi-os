#ifndef iht_INCLUDED
# define iht_INCLUDED
int zscreen_params(os_ptr op, gs_screen_halftone * phs);
int zscreen_enum_init(i_ctx_t *i_ctx_p, const gx_ht_order * porder,
gs_screen_halftone * phs, ref * pproc, int npop,
op_proc_t finish_proc, int space_index);
#endif