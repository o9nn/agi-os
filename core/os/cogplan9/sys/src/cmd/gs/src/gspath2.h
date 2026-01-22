#ifndef gspath2_INCLUDED
#  define gspath2_INCLUDED
int gs_setbbox(gs_state *, floatp, floatp, floatp, floatp);
int gs_rectappend(gs_state *, const gs_rect *, uint);
int gs_rectclip(gs_state *, const gs_rect *, uint);
int gs_rectfill(gs_state *, const gs_rect *, uint);
int gs_rectstroke(gs_state *, const gs_rect *, uint, const gs_matrix *);
#endif