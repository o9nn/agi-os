#ifndef gscolor1_INCLUDED
#  define gscolor1_INCLUDED
int gs_setcmykcolor(gs_state *, floatp, floatp, floatp, floatp),
gs_currentcmykcolor(const gs_state *, float[4]),
gs_setblackgeneration(gs_state *, gs_mapping_proc),
gs_setblackgeneration_remap(gs_state *, gs_mapping_proc, bool);
gs_mapping_proc gs_currentblackgeneration(const gs_state *);
int gs_setundercolorremoval(gs_state *, gs_mapping_proc),
gs_setundercolorremoval_remap(gs_state *, gs_mapping_proc, bool);
gs_mapping_proc gs_currentundercolorremoval(const gs_state *);
int gs_setcolortransfer(gs_state *, gs_mapping_proc  ,
gs_mapping_proc  ,
gs_mapping_proc  ,
gs_mapping_proc  ),
gs_setcolortransfer_remap(gs_state *, gs_mapping_proc  ,
gs_mapping_proc  ,
gs_mapping_proc  ,
gs_mapping_proc  , bool);
void gs_currentcolortransfer(const gs_state *, gs_mapping_proc[4]);
#endif