#ifndef gscolor3_INCLUDED
# define gscolor3_INCLUDED
#ifndef gs_shading_t_DEFINED
# define gs_shading_t_DEFINED
typedef struct gs_shading_s gs_shading_t;
#endif
int gs_setsmoothness(gs_state *, floatp);
float gs_currentsmoothness(const gs_state *);
int gs_shfill(gs_state *, const gs_shading_t *);
#endif