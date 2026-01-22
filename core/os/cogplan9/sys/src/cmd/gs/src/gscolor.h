#ifndef gscolor_INCLUDED
#  define gscolor_INCLUDED
#include "gxtmap.h"
int gs_setgray(gs_state *, floatp);
int gs_currentgray(const gs_state *, float *);
int gs_setrgbcolor(gs_state *, floatp, floatp, floatp);
int gs_currentrgbcolor(const gs_state *, float[3]);
int gs_setnullcolor(gs_state *);
int gs_settransfer(gs_state *, gs_mapping_proc);
int gs_settransfer_remap(gs_state *, gs_mapping_proc, bool);
gs_mapping_proc gs_currenttransfer(const gs_state *);
#endif