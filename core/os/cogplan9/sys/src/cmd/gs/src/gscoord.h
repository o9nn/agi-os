#ifndef gscoord_INCLUDED
#  define gscoord_INCLUDED
int gs_initmatrix(gs_state *),
gs_defaultmatrix(const gs_state *, gs_matrix *),
gs_currentmatrix(const gs_state *, gs_matrix *),
gs_setmatrix(gs_state *, const gs_matrix *),
gs_translate(gs_state *, floatp, floatp),
gs_scale(gs_state *, floatp, floatp),
gs_rotate(gs_state *, floatp),
gs_concat(gs_state *, const gs_matrix *);
int gs_setdefaultmatrix(gs_state *, const gs_matrix *),
gs_currentcharmatrix(gs_state *, gs_matrix *, bool),
gs_setcharmatrix(gs_state *, const gs_matrix *),
gs_settocharmatrix(gs_state *);
int gs_transform(gs_state *, floatp, floatp, gs_point *),
gs_dtransform(gs_state *, floatp, floatp, gs_point *),
gs_itransform(gs_state *, floatp, floatp, gs_point *),
gs_idtransform(gs_state *, floatp, floatp, gs_point *);
#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
int gs_imager_setmatrix(gs_imager_state *, const gs_matrix *);
int gs_imager_idtransform(const gs_imager_state *, floatp, floatp, gs_point *);
#endif