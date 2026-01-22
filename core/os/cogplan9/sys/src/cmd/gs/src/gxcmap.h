#ifndef gxcmap_INCLUDED
# define gxcmap_INCLUDED
#include "gscsel.h"
#include "gxfmap.h"
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#ifndef gx_device_color_DEFINED
# define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
#define cmap_proc_gray(proc)\
void proc(frac, gx_device_color *, const gs_imager_state *,\
gx_device *, gs_color_select_t)
#define cmap_proc_rgb(proc)\
void proc(frac, frac, frac, gx_device_color *, const gs_imager_state *,\
gx_device *, gs_color_select_t)
#define cmap_proc_cmyk(proc)\
void proc(frac, frac, frac, frac, gx_device_color *,\
const gs_imager_state *, gx_device *, gs_color_select_t)
#define cmap_proc_rgb_alpha(proc)\
void proc(frac, frac, frac, frac, gx_device_color *,\
const gs_imager_state *, gx_device *, gs_color_select_t)
#define cmap_proc_separation(proc)\
void proc(frac, gx_device_color *, const gs_imager_state *,\
gx_device *, gs_color_select_t)
#define cmap_proc_devicen(proc)\
void proc(const frac *, gx_device_color *, const gs_imager_state *, \
gx_device *, gs_color_select_t)
#define cmap_proc_is_halftoned(proc)\
bool proc(const gs_imager_state *, gx_device *)
#define cm_map_proc_gray(proc) \
void proc (gx_device * dev, frac gray, \
frac * out)
#define cm_map_proc_rgb(proc) \
void proc (gx_device * dev, \
const gs_imager_state *pis, \
frac r, frac g, frac b, \
frac * out)
#define cm_map_proc_cmyk(proc) \
void proc (gx_device * dev, \
frac c, frac m, frac y, frac k, \
frac * out)
struct gx_cm_color_map_procs_s {
cm_map_proc_gray((*map_gray));
cm_map_proc_rgb((*map_rgb));
cm_map_proc_cmyk((*map_cmyk));
};
typedef struct gx_cm_color_map_procs_s gx_cm_color_map_procs;
cm_map_proc_gray(gray_cs_to_gray_cm);
cm_map_proc_rgb(rgb_cs_to_rgb_cm);
cm_map_proc_cmyk(cmyk_cs_to_cmyk_cm);
struct gx_color_map_procs_s {
cmap_proc_gray((*map_gray));
cmap_proc_rgb((*map_rgb));
cmap_proc_cmyk((*map_cmyk));
cmap_proc_rgb_alpha((*map_rgb_alpha));
cmap_proc_separation((*map_separation));
cmap_proc_devicen((*map_devicen));
cmap_proc_is_halftoned((*is_halftoned));
};
typedef struct gx_color_map_procs_s gx_color_map_procs;
const gx_color_map_procs *
gx_get_cmap_procs(const gs_imager_state *, const gx_device *);
const gx_color_map_procs *
gx_default_get_cmap_procs(const gs_imager_state *, const gx_device *);
void gx_set_cmap_procs(gs_imager_state *, const gx_device *);
#define gx_remap_concrete_gray(cgray, pdc, pis, dev, select)\
((pis)->cmap_procs->map_gray)(cgray, pdc, pis, dev, select)
#define gx_remap_concrete_rgb(cr, cg, cb, pdc, pis, dev, select)\
((pis)->cmap_procs->map_rgb)(cr, cg, cb, pdc, pis, dev, select)
#define gx_remap_concrete_cmyk(cc, cm, cy, ck, pdc, pis, dev, select)\
((pis)->cmap_procs->map_cmyk)(cc, cm, cy, ck, pdc, pis, dev, select)
#define gx_remap_concrete_rgb_alpha(cr, cg, cb, ca, pdc, pis, dev, select)\
((pis)->cmap_procs->map_rgb_alpha)(cr, cg, cb, ca, pdc, pis, dev, select)
#define gx_remap_concrete_separation(pcc, pdc, pis, dev, select)\
((pis)->cmap_procs->map_separation)(pcc, pdc, pis, dev, select)
#define gx_remap_concrete_devicen(pcc, pdc, pis, dev, select)\
((pis)->cmap_procs->map_devicen)(pcc, pdc, pis, dev, select)
#include "gxcindex.h"
#include "gxcvalue.h"
extern cm_map_proc_gray(gx_default_gray_cs_to_gray_cm);
extern cm_map_proc_rgb(gx_default_rgb_cs_to_gray_cm);
extern cm_map_proc_cmyk(gx_default_cmyk_cs_to_gray_cm);
extern cm_map_proc_gray(gx_default_gray_cs_to_rgb_cm);
extern cm_map_proc_rgb(gx_default_rgb_cs_to_rgb_cm);
extern cm_map_proc_cmyk(gx_default_cmyk_cs_to_rgb_cm);
extern cm_map_proc_gray(gx_default_gray_cs_to_cmyk_cm);
extern cm_map_proc_rgb(gx_default_rgb_cs_to_cmyk_cm);
extern cm_map_proc_cmyk(gx_default_cmyk_cs_to_cmyk_cm);
extern cm_map_proc_gray(gx_default_gray_cs_to_cmyk_cm);
extern cm_map_proc_rgb(gx_default_rgb_cs_to_cmyk_cm);
extern cm_map_proc_cmyk(gx_default_cmyk_cs_to_cmyk_cm);
extern cm_map_proc_gray(gx_error_gray_cs_to_cmyk_cm);
extern cm_map_proc_rgb(gx_error_rgb_cs_to_cmyk_cm);
extern cm_map_proc_cmyk(gx_error_cmyk_cs_to_cmyk_cm);
#define dev_t_proc_get_color_mapping_procs(proc, dev_t) \
const gx_cm_color_map_procs * (proc)(const dev_t * dev)
#define dev_proc_get_color_mapping_procs(proc) \
dev_t_proc_get_color_mapping_procs(proc, gx_device)
#define NO_COMP_NAME_TYPE 0
#define SEPARATION_NAME 1
#define dev_t_proc_get_color_comp_index(proc, dev_t) \
int (proc)(dev_t * dev, const char * pname, int name_size, int component_type)
#define dev_proc_get_color_comp_index(proc) \
dev_t_proc_get_color_comp_index(proc, gx_device)
#define dev_t_proc_encode_color(proc, dev_t) \
gx_color_index (proc)(dev_t * dev, const gx_color_value colors[])
#define dev_proc_encode_color(proc) \
dev_t_proc_encode_color(proc, gx_device)
#define dev_t_proc_decode_color(proc, dev_t) \
int (proc)(dev_t * dev, gx_color_index cindex, gx_color_value colors[])
#define dev_proc_decode_color(proc) \
dev_t_proc_decode_color(proc, gx_device)
dev_proc_get_color_comp_index(gx_error_get_color_comp_index);
dev_proc_get_color_comp_index(gx_default_DevGray_get_color_comp_index);
dev_proc_get_color_comp_index(gx_default_DevRGB_get_color_comp_index);
dev_proc_get_color_comp_index(gx_default_DevCMYK_get_color_comp_index);
dev_proc_get_color_comp_index(gx_default_DevRGBK_get_color_comp_index);
dev_proc_get_color_mapping_procs(gx_error_get_color_mapping_procs);
dev_proc_get_color_mapping_procs(gx_default_DevGray_get_color_mapping_procs);
dev_proc_get_color_mapping_procs(gx_default_DevRGB_get_color_mapping_procs);
dev_proc_get_color_mapping_procs(gx_default_DevCMYK_get_color_mapping_procs);
dev_proc_get_color_mapping_procs(gx_default_DevRGBK_get_color_mapping_procs);
dev_proc_encode_color(gx_error_encode_color);
dev_proc_encode_color(gx_default_encode_color);
dev_proc_encode_color(gx_default_gray_fast_encode);
dev_proc_encode_color(gx_default_gray_encode);
dev_proc_encode_color(gx_backwards_compatible_gray_encode);
dev_proc_decode_color(gx_error_decode_color);
dev_proc_decode_color(gx_default_decode_color);
#define unit_frac(v, ftemp)\
(ftemp = (v),\
(is_fneg(ftemp) ? frac_0 : is_fge1(ftemp) ? frac_1 : float2frac(ftemp)))
#endif