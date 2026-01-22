#ifndef gximage3_INCLUDED
# define gximage3_INCLUDED
#include "gsiparm3.h"
#include "gxiparam.h"
#define IMAGE3_MAKE_MID_PROC(proc)\
int proc(gx_device **pmidev, gx_device *dev, int width, int height,\
gs_memory_t *mem)
typedef IMAGE3_MAKE_MID_PROC((*image3_make_mid_proc_t));
#define IMAGE3_MAKE_MCDE_PROC(proc)\
int proc(\
gx_device *dev,\
const gs_imager_state *pis,\
const gs_matrix *pmat,\
const gs_image_common_t *pic,\
const gs_int_rect *prect,\
const gx_drawing_color *pdcolor,\
const gx_clip_path *pcpath, gs_memory_t *mem,\
gx_image_enum_common_t **pinfo,\
\
gx_device **pmcdev, gx_device *midev,\
gx_image_enum_common_t *pminfo,\
const gs_int_point *origin)
typedef IMAGE3_MAKE_MCDE_PROC((*image3_make_mcde_proc_t));
int gx_begin_image3_generic(gx_device * dev,
const gs_imager_state *pis,
const gs_matrix *pmat,
const gs_image_common_t *pic,
const gs_int_rect *prect,
const gx_drawing_color *pdcolor,
const gx_clip_path *pcpath, gs_memory_t *mem,
IMAGE3_MAKE_MID_PROC((*make_mid)),
IMAGE3_MAKE_MCDE_PROC((*make_mcde)),
gx_image_enum_common_t **pinfo);
#endif