#ifndef gximage_INCLUDED
#  define gximage_INCLUDED
#include "gsiparam.h"
#include "gxcspace.h"
#include "strimpl.h"
#include "sisparam.h"
#include "gxdda.h"
#include "gxiclass.h"
#include "gxiparam.h"
#include "gxsample.h"
typedef enum {
sd_none,
sd_lookup,
sd_compute
} sample_decoding;
struct sample_map_s {
sample_lookup_t table;
float decode_lookup[16];
#define decode_base decode_lookup[0]
#define decode_max decode_lookup[15]
double decode_factor;
sample_decoding decoding;
bool inverted;
};
#ifndef sample_map_DEFINED
#define sample_map_DEFINED
typedef struct sample_map_s sample_map;
#endif
#define decode_sample(sample_value, cc, i)\
switch ( penum->map[i].decoding )\
{\
case sd_none:\
cc.paint.values[i] = (sample_value) * (1.0 / 255.0);  \
break;\
case sd_lookup:	\
cc.paint.values[i] =\
penum->map[i].decode_lookup[(sample_value) >> 4];\
break;\
case sd_compute:\
cc.paint.values[i] =\
penum->map[i].decode_base + (sample_value) * penum->map[i].decode_factor;\
}
#define decode_frac(frac_value, cc, i)\
cc.paint.values[i] =\
penum->map[i].decode_base + (frac_value) * penum->map[i].decode_factor
extern const sample_unpack_proc_t sample_unpack_12_proc;
extern const sample_unpack_proc_t sample_unpack_16_proc;
typedef enum {
image_portrait = 0,
image_landscape,
image_skewed
} image_posture;
typedef struct gx_image_clue_s {
gx_device_color dev_color;
bits32 key;
} gx_image_clue;
#ifndef gx_device_clip_DEFINED
#  define gx_device_clip_DEFINED
typedef struct gx_device_clip_s gx_device_clip;
#endif
#ifndef gx_device_rop_texture_DEFINED
#  define gx_device_rop_texture_DEFINED
typedef struct gx_device_rop_texture_s gx_device_rop_texture;
#endif
struct gx_image_enum_s {
gx_image_enum_common;
byte bps;
byte unpack_bps;
byte log2_xbytes;
byte spp;
gs_image_alpha_t alpha;
struct mc_ {
uint values[GS_IMAGE_MAX_COMPONENTS * 2];
bits32 mask, test;
bool exact;
} mask_color;
byte use_mask_color;
byte spread;
byte masked;
byte interpolate;
gs_matrix matrix;
struct r_ {
int x, y, w, h;
} rect;
gs_fixed_point x_extent, y_extent;
SAMPLE_UNPACK_PROC((*unpack));
irender_proc((*render));
const gs_imager_state *pis;
const gs_color_space *pcs;
gs_memory_t *memory;
byte *buffer;
uint buffer_size;
byte *line;
uint line_size;
uint line_width;
image_posture posture;
byte use_rop;
byte clip_image;
#define image_clip_xmin 1
#define image_clip_xmax 2
#define image_clip_ymin 4
#define image_clip_ymax 8
#define image_clip_region 0x10
byte slow_loop;
byte device_color;
gs_fixed_rect clip_outer;
gs_fixed_rect clip_inner;
gs_logical_operation_t log_op;
fixed adjust;
fixed dxx, dxy;
gx_device_clip *clip_dev;
gx_device_rop_texture *rop_dev;
stream_image_scale_state *scaler;
int y;
gs_int_point used;
gs_fixed_point cur, prev;
struct dd_ {
gx_dda_fixed_point row;
gx_dda_fixed_point strip;
gx_dda_fixed_point pixel0;
} dda;
int line_xy;
int xi_next;
gs_int_point xyi;
int yci, hci;
int xci, wci;
sample_map map[GS_IMAGE_MAX_COMPONENTS];
gx_image_clue clues[256];
#define icolor0 clues[0].dev_color
#define icolor1 clues[255].dev_color
};
#define gx_image_enum_do_ptrs(m)\
m(0,pis) m(1,pcs) m(2,dev) m(3,buffer) m(4,line)\
m(5,clip_dev) m(6,rop_dev) m(7,scaler)
#define gx_image_enum_num_ptrs 8
#define private_st_gx_image_enum() \
gs_private_st_composite(st_gx_image_enum, gx_image_enum, "gx_image_enum",\
image_enum_enum_ptrs, image_enum_reloc_ptrs)
#define dev_color_eq(devc1, devc2)\
gx_device_color_equal(&(devc1), &(devc2))
void gx_image_scale_mask_colors(gx_image_enum *penum,
int component_index);
int
gx_image_enum_alloc(const gs_image_common_t * pic,
const gs_int_rect * prect,
gs_memory_t * mem, gx_image_enum **ppenum);
int
gx_image_enum_begin(gx_device * dev, const gs_imager_state * pis,
const gs_matrix *pmat, const gs_image_common_t * pic,
const gx_drawing_color * pdcolor,
const gx_clip_path * pcpath,
gs_memory_t * mem, gx_image_enum *penum);
void
image_init_clues(gx_image_enum * penum, int bps, int spp);
#endif