#ifndef gxpaint_INCLUDED
#  define gxpaint_INCLUDED
#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
#ifndef gs_state_DEFINED
#  define gs_state_DEFINED
typedef struct gs_state_s gs_state;
#endif
#ifndef gx_device_DEFINED
#  define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#ifndef gx_device_color_DEFINED
#  define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
int gx_fill_path(gx_path * ppath, gx_device_color * pdevc, gs_state * pgs,
int rule, fixed adjust_x, fixed adjust_y);
int gx_stroke_fill(gx_path * ppath, gs_state * pgs);
int gx_stroke_add(gx_path *ppath, gx_path *to_path, const gs_state * pgs);
int gx_imager_stroke_add(gx_path *ppath, gx_path *to_path,
gx_device *dev, const gs_imager_state *pis);
void gx_adjust_if_empty(const gs_fixed_rect *, gs_fixed_point *);
int gx_stroke_path_expansion(const gs_imager_state *pis,
const gx_path *ppath, gs_fixed_point *ppt);
#define gx_stroke_expansion(pis, ppt)\
gx_stroke_path_expansion(pis, (const gx_path *)0, ppt)
#ifndef gx_fill_params_DEFINED
#  define gx_fill_params_DEFINED
typedef struct gx_fill_params_s gx_fill_params;
#endif
struct gx_fill_params_s {
int rule;
gs_fixed_point adjust;
float flatness;
bool fill_zero_width;
};
#define gx_fill_path_only(ppath, dev, pis, params, pdevc, pcpath)\
(*dev_proc(dev, fill_path))(dev, pis, ppath, params, pdevc, pcpath)
#ifndef gx_stroke_params_DEFINED
#  define gx_stroke_params_DEFINED
typedef struct gx_stroke_params_s gx_stroke_params;
#endif
struct gx_stroke_params_s {
float flatness;
};
int gx_stroke_path_only(gx_path * ppath, gx_path * to_path, gx_device * dev,
const gs_imager_state * pis,
const gx_stroke_params * params,
const gx_device_color * pdevc,
const gx_clip_path * pcpath);
#endif