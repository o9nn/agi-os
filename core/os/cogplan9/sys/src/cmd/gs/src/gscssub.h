#ifndef gscssub_INCLUDED
# define gscssub_INCLUDED
#include "gscspace.h"
int gs_setsubstitutecolorspace(gs_state *pgs, gs_color_space_index csi,
const gs_color_space *pcs);
const gs_color_space *
gs_currentsubstitutecolorspace(const gs_state *pgs,
gs_color_space_index csi);
const gs_color_space *gs_current_DeviceGray_space(const gs_state *pgs);
const gs_color_space *gs_current_DeviceRGB_space(const gs_state *pgs);
const gs_color_space *gs_current_DeviceCMYK_space(const gs_state *pgs);
#endif