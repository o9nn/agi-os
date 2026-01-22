#ifndef icolor_INCLUDED
#  define icolor_INCLUDED
extern const int zcolor_remap_one_ostack;
extern const int zcolor_remap_one_estack;
int zcolor_remap_one(i_ctx_t *, const ref *, gx_transfer_map *,
const gs_state *, op_proc_t);
int zcolor_remap_one_finish(i_ctx_t *);
int zcolor_remap_one_signed_finish(i_ctx_t *);
int zcolor_reset_transfer(i_ctx_t *);
int zcolor_remap_color(i_ctx_t *);
#endif