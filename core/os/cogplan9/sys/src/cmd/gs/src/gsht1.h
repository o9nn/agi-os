#ifndef gsht1_INCLUDED
# define gsht1_INCLUDED
#include "gsht.h"
int gs_setcolorscreen(gs_state *, gs_colorscreen_halftone *);
int gs_currentcolorscreen(gs_state *, gs_colorscreen_halftone *);
#ifndef gs_halftone_DEFINED
# define gs_halftone_DEFINED
typedef struct gs_halftone_s gs_halftone;
#endif
int gs_sethalftone(gs_state *, gs_halftone *);
int gs_sethalftone_allocated(gs_state *, gs_halftone *);
int gs_currenthalftone(gs_state *, gs_halftone *);
#endif