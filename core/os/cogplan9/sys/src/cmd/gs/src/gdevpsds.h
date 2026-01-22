#ifndef gdevpsds_INCLUDED
# define gdevpsds_INCLUDED
#include "strimpl.h"
#include "gsiparam.h"
typedef struct stream_1248_state_s {
stream_state_common;
uint samples_per_row;
int bits_per_sample;
uint left;
} stream_1248_state;
extern const stream_template s_1_8_template;
extern const stream_template s_2_8_template;
extern const stream_template s_4_8_template;
extern const stream_template s_12_8_template;
extern const stream_template s_8_1_template;
extern const stream_template s_8_2_template;
extern const stream_template s_8_4_template;
int s_1248_init(stream_1248_state *ss, int Columns, int samples_per_pixel);
typedef struct stream_C2R_state_s {
stream_state_common;
const gs_imager_state *pis;
} stream_C2R_state;
#define private_st_C2R_state() \
gs_private_st_ptrs1(st_C2R_state, stream_C2R_state, "stream_C2R_state",\
c2r_enum_ptrs, c2r_reloc_ptrs, pis)
extern const stream_template s_C2R_template;
int s_C2R_init(stream_C2R_state *ss, const gs_imager_state *pis);
typedef struct stream_IE_state_s {
stream_state_common;
int BitsPerComponent;
int NumComponents;
int Width;
int BitsPerIndex;
const float *Decode;
gs_bytestring Table;
int hash_table[400];
int next_index;
uint byte_in;
int in_bits_left;
int next_component;
uint byte_out;
int x;
} stream_IE_state;
#define private_st_IE_state() \
gs_public_st_composite(st_IE_state, stream_IE_state, "stream_IE_state",\
ie_state_enum_ptrs, ie_state_reloc_ptrs)
extern const stream_template s_IE_template;
#define stream_Downsample_state_common\
stream_state_common;\
\
int Colors;\
int WidthIn, HeightIn;\
int XFactor, YFactor;\
bool AntiAlias;\
bool padX, padY; \
\
int x, y
#define s_Downsample_set_defaults_inline(ss)\
((ss)->AntiAlias = (ss)->padX = (ss)->padY = false)
typedef struct stream_Downsample_state_s {
stream_Downsample_state_common;
} stream_Downsample_state;
int s_Downsample_size_out(int size_in, int factor, bool pad);
typedef struct stream_Subsample_state_s {
stream_Downsample_state_common;
} stream_Subsample_state;
extern const stream_template s_Subsample_template;
typedef struct stream_Average_state_s {
stream_Downsample_state_common;
uint sum_size;
uint copy_size;
uint *sums;
} stream_Average_state;
#define private_st_Average_state() \
gs_private_st_ptrs1(st_Average_state, stream_Average_state,\
"stream_Average_state", avg_enum_ptrs, avg_reloc_ptrs, sums)
extern const stream_template s_Average_template;
typedef struct stream_compr_chooser_state_s {
stream_state_common;
uint choice;
uint width, height, depth, bits_per_sample;
uint samples_count, bits_left;
ulong packed_data;
byte *sample;
ulong upper_plateaus, lower_plateaus;
ulong gradients;
} stream_compr_chooser_state;
#define private_st_compr_chooser_state() \
gs_private_st_ptrs1(st_compr_chooser_state, stream_compr_chooser_state, \
"stream_compr_chooser_state",\
compr_chooser_enum_ptrs, compr_chooser_reloc_ptrs, sample)
extern const stream_template s_compr_chooser_template;
int
s_compr_chooser_set_dimensions(stream_compr_chooser_state * st, int width,
int height, int depth, int bits_per_sample);
uint s_compr_chooser__get_choice(stream_compr_chooser_state *st, bool force);
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
typedef struct stream_image_colors_state_s stream_image_colors_state;
struct stream_image_colors_state_s {
stream_state_common;
uint width, height, depth, bits_per_sample;
byte output_bits_buffer;
uint output_bits_buffered;
uint output_component_bits_written;
uint output_component_index;
uint output_depth, output_bits_per_sample;
uint raster;
uint row_bits;
uint row_bits_passed;
uint row_alignment_bytes;
uint row_alignment_bytes_left;
uint input_component_index;
uint input_bits_buffer;
uint input_bits_buffered;
uint input_color[GS_IMAGE_MAX_COLOR_COMPONENTS];
uint output_color[GS_IMAGE_MAX_COLOR_COMPONENTS];
uint MaskColor[GS_IMAGE_MAX_COLOR_COMPONENTS * 2];
float Decode[GS_IMAGE_MAX_COLOR_COMPONENTS * 2];
const gs_color_space *pcs;
gx_device *pdev;
const gs_imager_state *pis;
int (*convert_color)(stream_image_colors_state *);
};
#define private_st_image_colors_state() \
gs_private_st_ptrs3(st_stream_image_colors_state, stream_image_colors_state,\
"stream_image_colors_state", stream_image_colors_enum_ptrs,\
stream_image_colors_reloc_ptrs, pcs, pdev, pis)
extern const stream_template s_image_colors_template;
void s_image_colors_set_dimensions(stream_image_colors_state * st,
int width, int height, int depth, int bits_per_sample);
void s_image_colors_set_mask_colors(stream_image_colors_state * ss, uint *MaskColor);
void s_image_colors_set_color_space(stream_image_colors_state * ss, gx_device *pdev,
const gs_color_space *pcs, const gs_imager_state *pis,
float *Decode);
extern const stream_template s__image_colors_template;
#endif