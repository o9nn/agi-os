#ifndef gxiparam_INCLUDED
# define gxiparam_INCLUDED
#include "gsstype.h"
#include "gxdevcli.h"
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
#ifndef gx_image_type_DEFINED
# define gx_image_type_DEFINED
typedef struct gx_image_type_s gx_image_type_t;
#endif
struct gx_image_type_s {
gs_memory_type_ptr_t stype;
dev_proc_begin_typed_image((*begin_typed_image));
#define image_proc_source_size(proc)\
int proc(const gs_imager_state *pis, const gs_image_common_t *pic,\
gs_int_point *psize)
image_proc_source_size((*source_size));
#define image_proc_sput(proc)\
int proc(const gs_image_common_t *pic, stream *s,\
const gs_color_space **ppcs)
image_proc_sput((*sput));
#define image_proc_sget(proc)\
int proc(gs_image_common_t *pic, stream *s, const gs_color_space *pcs)
image_proc_sget((*sget));
#define image_proc_release(proc)\
void proc(gs_image_common_t *pic, gs_memory_t *mem)
image_proc_release((*release));
int index;
};
image_proc_source_size(gx_data_image_source_size);
image_proc_sput(gx_image_no_sput);
image_proc_sget(gx_image_no_sget);
image_proc_release(gx_image_default_release);
int gx_pixel_image_sput(const gs_pixel_image_t *pic, stream *s,
const gs_color_space **ppcs, int extra);
int gx_pixel_image_sget(gs_pixel_image_t *pic, stream *s,
const gs_color_space *pcs);
void gx_pixel_image_release(gs_pixel_image_t *pic, gs_memory_t *mem);
bool gx_image_matrix_is_default(const gs_data_image_t *pid);
void gx_image_matrix_set_default(gs_data_image_t *pid);
void sput_variable_uint(stream *s, uint w);
int sget_variable_uint(stream *s, uint *pw);
#define DECODE_DEFAULT(i, dd1)\
((i) == 1 ? dd1 : (i) & 1)
#ifndef gx_image_enum_common_t_DEFINED
# define gx_image_enum_common_t_DEFINED
typedef struct gx_image_enum_common_s gx_image_enum_common_t;
#endif
typedef struct gx_image_enum_procs_s {
#define image_enum_proc_plane_data(proc)\
int proc(gx_image_enum_common_t *info, const gx_image_plane_t *planes,\
int height, int *rows_used)
image_enum_proc_plane_data((*plane_data));
#define image_enum_proc_end_image(proc)\
int proc(gx_image_enum_common_t *info, bool draw_last)
image_enum_proc_end_image((*end_image));
#define image_enum_proc_flush(proc)\
int proc(gx_image_enum_common_t *info)
image_enum_proc_flush((*flush));
#define image_enum_proc_planes_wanted(proc)\
bool proc(const gx_image_enum_common_t *info, byte *wanted)
image_enum_proc_planes_wanted((*planes_wanted));
} gx_image_enum_procs_t;
#define gx_image_enum_common\
const gx_image_type_t *image_type;\
const gx_image_enum_procs_t *procs;\
gx_device *dev;\
gs_id id;\
int num_planes;\
int plane_depths[gs_image_max_planes]; \
int plane_widths[gs_image_max_planes]
struct gx_image_enum_common_s {
gx_image_enum_common;
};
extern_st(st_gx_image_enum_common);
#define public_st_gx_image_enum_common() \
gs_public_st_composite(st_gx_image_enum_common, gx_image_enum_common_t,\
"gx_image_enum_common_t",\
image_enum_common_enum_ptrs, image_enum_common_reloc_ptrs)
int gx_image_enum_common_init(gx_image_enum_common_t * piec,
const gs_data_image_t * pic,
const gx_image_enum_procs_t * piep,
gx_device * dev, int num_components,
gs_image_format_t format);
image_enum_proc_plane_data(gx_no_plane_data);
image_enum_proc_end_image(gx_ignore_end_image);
dev_proc_begin_typed_image(gx_begin_image1);
image_enum_proc_plane_data(gx_image1_plane_data);
image_enum_proc_end_image(gx_image1_end_image);
image_enum_proc_flush(gx_image1_flush);
#endif