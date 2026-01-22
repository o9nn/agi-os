#ifndef gscolor2_INCLUDED
# define gscolor2_INCLUDED
#include "gscindex.h"
#include "gsptype1.h"
const gs_color_space *gs_currentcolorspace(const gs_state *);
int gs_setcolorspace(gs_state *, const gs_color_space *);
const gs_client_color *gs_currentcolor(const gs_state *);
int gs_setcolor(gs_state *, const gs_client_color *);
#ifndef gs_cie_render_DEFINED
# define gs_cie_render_DEFINED
typedef struct gs_cie_render_s gs_cie_render;
#endif
const gs_cie_render *gs_currentcolorrendering(const gs_state *);
int gs_setcolorrendering(gs_state *, gs_cie_render *);
int gs_includecolorspace(gs_state * pgs, const byte *res_name, int name_length);
#endif