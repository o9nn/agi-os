#ifndef gximag3x_INCLUDED
# define gximag3x_INCLUDED
#include "gsipar3x.h"
#include "gxiparam.h"
#define IMAGE3X_MAKE_MID_PROC(proc)\
int proc(gx_device **pmidev, gx_device *dev, int width, int height,\
int depth, gs_memory_t *mem)
typedef IMAGE3X_MAKE_MID_PROC((*image3x_make_mid_proc_t));
#define IMAGE3X_MAKE_MCDE_PROC(proc)\
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
gx_device **pmcdev, gx_device *midev[2],\
gx_image_enum_common_t *pminfo[2],\
const gs_int_point origin[2],\
const gs_image3x_t *pim)
typedef IMAGE3X_MAKE_MCDE_PROC((*image3x_make_mcde_proc_t));
int gx_begin_image3x_generic(gx_device * dev,
const gs_imager_state *pis,
const gs_matrix *pmat,
const gs_image_common_t *pic,
const gs_int_rect *prect,
const gx_drawing_color *pdcolor,
const gx_clip_path *pcpath, gs_memory_t *mem,
IMAGE3X_MAKE_MID_PROC((*make_mid)),
IMAGE3X_MAKE_MCDE_PROC((*make_mcde)),
gx_image_enum_common_t **pinfo);
#endif