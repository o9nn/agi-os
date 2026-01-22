#ifndef gdevpsdf_INCLUDED
# define gdevpsdf_INCLUDED
#include "gdevvec.h"
#include "gsparam.h"
#include "strimpl.h"
#include "sa85x.h"
#include "scfx.h"
#include "spsdf.h"
extern const stream_template s_DCTE_template;
typedef struct psdf_image_params_s {
gs_c_param_list *ACSDict;
bool AntiAlias;
bool AutoFilter;
int Depth;
gs_c_param_list *Dict;
bool Downsample;
float DownsampleThreshold;
enum psdf_downsample_type {
ds_Average,
ds_Bicubic,
ds_Subsample
} DownsampleType;
#define psdf_ds_names\
"Average", "Bicubic", "Subsample"
bool Encode;
const char *Filter;
int Resolution;
const stream_template *filter_template;
} psdf_image_params;
#define psdf_image_param_defaults(af, res, dst, f, ft)\
NULL, 0, af, -1, NULL, 1,\
dst, ds_Subsample, 1, f, res, ft
extern const stream_template s_CFE_template;
typedef struct psdf_distiller_params_s {
bool ASCII85EncodePages;
enum psdf_auto_rotate_pages {
arp_None,
arp_All,
arp_PageByPage
} AutoRotatePages;
#define psdf_arp_names\
"None", "All", "PageByPage"
enum psdf_binding {
binding_Left,
binding_Right
} Binding;
#define psdf_binding_names\
"Left", "Right"
bool CompressPages;
enum psdf_default_rendering_intent {
ri_Default,
ri_Perceptual,
ri_Saturation,
ri_RelativeColorimetric,
ri_AbsoluteColorimetric
} DefaultRenderingIntent;
#define psdf_ri_names\
"Default", "Perceptual", "Saturation", "RelativeColorimetric",\
"AbsoluteColorimetric"
bool DetectBlends;
bool DoThumbnails;
long ImageMemory;
bool LockDistillerParams;
bool LZWEncodePages;
int OPM;
bool PreserveOPIComments;
bool UseFlateCompression;
gs_const_string CalCMYKProfile;
gs_const_string CalGrayProfile;
gs_const_string CalRGBProfile;
gs_const_string sRGBProfile;
enum psdf_color_conversion_strategy {
ccs_LeaveColorUnchanged,
ccs_UseDeviceDependentColor,
ccs_UseDeviceIndependentColor,
ccs_UseDeviceIndependentColorForImages,
ccs_sRGB
} ColorConversionStrategy;
#define psdf_ccs_names\
"LeaveColorUnchanged", "UseDeviceDependentColor",\
"UseDeviceIndependentColor", "UseDeviceIndependentColorForImages",\
"sRGB"
bool PreserveHalftoneInfo;
bool PreserveOverprintSettings;
enum psdf_transfer_function_info {
tfi_Preserve,
tfi_Apply,
tfi_Remove
} TransferFunctionInfo;
#define psdf_tfi_names\
"Preserve", "Apply", "Remove"
enum psdf_ucr_and_bg_info {
ucrbg_Preserve,
ucrbg_Remove
} UCRandBGInfo;
#define psdf_ucrbg_names\
"Preserve", "Remove"
#define psdf_general_param_defaults(ascii)\
ascii, arp_None, binding_Left, 1,\
ri_Default, 1 , 0 ,\
500000, 0 , 0, 1,\
0 , 1 ,\
\
{0}, {0}, {0}, {0},\
ccs_LeaveColorUnchanged, 0, 0, tfi_Preserve, ucrbg_Remove
psdf_image_params ColorImage;
bool ConvertCMYKImagesToRGB;
bool ConvertImagesToIndexed;
#define psdf_color_image_param_defaults\
{ psdf_image_param_defaults(1, 72, 1.5, 0, 0) },\
0, 1
psdf_image_params GrayImage;
#define psdf_gray_image_param_defaults\
{ psdf_image_param_defaults(1, 72, 1.5, 0, 0) }
psdf_image_params MonoImage;
#define psdf_mono_image_param_defaults\
{ psdf_image_param_defaults(0, 300, 2.0, "CCITTFaxEncode", &s_CFE_template) }
gs_param_string_array AlwaysEmbed;
gs_param_string_array NeverEmbed;
enum psdf_cannot_embed_font_policy {
cefp_OK,
cefp_Warning,
cefp_Error
} CannotEmbedFontPolicy;
#define psdf_cefp_names\
"OK", "Warning", "Error"
bool EmbedAllFonts;
int MaxSubsetPct;
bool SubsetFonts;
#define psdf_font_param_defaults\
{0}, {0}, cefp_Warning, 1, 100, 1
} psdf_distiller_params;
typedef enum {
psdf_version_level1 = 1000,
psdf_version_level1_color = 1100,
psdf_version_level2 = 2000,
psdf_version_level2_with_TT = 2010,
psdf_version_level2_plus = 2017,
psdf_version_ll3 = 3010
} psdf_version;
#define gx_device_psdf_common\
gx_device_vector_common;\
psdf_version version;\
bool binary_ok; \
bool HaveCFF;\
bool HaveTrueTypes;\
bool HaveCIDSystem;\
psdf_distiller_params params
typedef struct gx_device_psdf_s {
gx_device_psdf_common;
} gx_device_psdf;
#define psdf_initial_values(version, ascii)\
vector_initial_values,\
version,\
!(ascii),\
true,\
true,\
false,\
{ psdf_general_param_defaults(ascii),\
psdf_color_image_param_defaults,\
psdf_gray_image_param_defaults,\
psdf_mono_image_param_defaults,\
psdf_font_param_defaults\
}
extern_st(st_device_psdf);
#define public_st_device_psdf() \
BASIC_PTRS(device_psdf_ptrs) {\
GC_OBJ_ELT2(gx_device_psdf, params.ColorImage.ACSDict,\
params.ColorImage.Dict),\
GC_CONST_STRING_ELT(gx_device_psdf, params.CalCMYKProfile),\
GC_CONST_STRING_ELT(gx_device_psdf, params.CalGrayProfile),\
GC_CONST_STRING_ELT(gx_device_psdf, params.CalRGBProfile),\
GC_CONST_STRING_ELT(gx_device_psdf, params.sRGBProfile),\
GC_OBJ_ELT2(gx_device_psdf, params.GrayImage.ACSDict,\
params.GrayImage.Dict),\
GC_OBJ_ELT2(gx_device_psdf, params.MonoImage.ACSDict,\
params.MonoImage.Dict),\
GC_OBJ_ELT2(gx_device_psdf, params.AlwaysEmbed.data,\
params.NeverEmbed.data)\
};\
gs_public_st_basic_super_final(st_device_psdf, gx_device_psdf,\
"gx_device_psdf", device_psdf_ptrs, device_psdf_data,\
&st_device_vector, 0, gx_device_finalize)
#define st_device_psdf_max_ptrs (st_device_vector_max_ptrs + 12)
dev_proc_get_params(gdev_psdf_get_params);
dev_proc_put_params(gdev_psdf_put_params);
int psdf_setlinewidth(gx_device_vector * vdev, floatp width);
int psdf_setlinecap(gx_device_vector * vdev, gs_line_cap cap);
int psdf_setlinejoin(gx_device_vector * vdev, gs_line_join join);
int psdf_setmiterlimit(gx_device_vector * vdev, floatp limit);
int psdf_setdash(gx_device_vector * vdev, const float *pattern,
uint count, floatp offset);
int psdf_setflat(gx_device_vector * vdev, floatp flatness);
int psdf_setlogop(gx_device_vector * vdev, gs_logical_operation_t lop,
gs_logical_operation_t diff);
#define psdf_dopath gdev_vector_dopath
int psdf_dorect(gx_device_vector * vdev, fixed x0, fixed y0, fixed x1,
fixed y1, gx_path_type_t type);
int psdf_beginpath(gx_device_vector * vdev, gx_path_type_t type);
int psdf_moveto(gx_device_vector * vdev, floatp x0, floatp y0,
floatp x, floatp y, gx_path_type_t type);
int psdf_lineto(gx_device_vector * vdev, floatp x0, floatp y0,
floatp x, floatp y, gx_path_type_t type);
int psdf_curveto(gx_device_vector * vdev, floatp x0, floatp y0,
floatp x1, floatp y1, floatp x2,
floatp y2, floatp x3, floatp y3, gx_path_type_t type);
int psdf_closepath(gx_device_vector * vdev, floatp x0, floatp y0,
floatp x_start, floatp y_start, gx_path_type_t type);
typedef struct psdf_binary_writer_s {
gs_memory_t *memory;
stream *target;
stream *strm;
gx_device_psdf *dev;
} psdf_binary_writer;
extern_st(st_psdf_binary_writer);
#define public_st_psdf_binary_writer() \
gs_public_st_ptrs3(st_psdf_binary_writer, psdf_binary_writer,\
"psdf_binary_writer", psdf_binary_writer_enum_ptrs,\
psdf_binary_writer_reloc_ptrs, target, strm, dev)
#define psdf_binary_writer_max_ptrs 3
int psdf_begin_binary(gx_device_psdf * pdev, psdf_binary_writer * pbw);
int psdf_encode_binary(psdf_binary_writer * pbw,
const stream_template * template, stream_state * ss);
int psdf_CFE_binary(psdf_binary_writer * pbw, int w, int h, bool invert);
int psdf_DCT_filter(gs_param_list *plist ,
stream_state *st,
int Columns, int Rows, int Colors,
psdf_binary_writer *pbw );
bool psdf_is_converting_image_to_RGB(const gx_device_psdf * pdev,
const gs_imager_state * pis, const gs_pixel_image_t * pim);
int psdf_setup_image_filters(gx_device_psdf *pdev, psdf_binary_writer *pbw,
gs_pixel_image_t *pim, const gs_matrix *pctm,
const gs_imager_state * pis, bool lossless);
int psdf_setup_lossless_filters(gx_device_psdf *pdev, psdf_binary_writer *pbw,
gs_pixel_image_t *pim);
int psdf_end_binary(psdf_binary_writer * pbw);
int psdf_setup_compression_chooser(psdf_binary_writer *pbw,
gx_device_psdf *pdev,
int width, int height, int depth,
int bits_per_sample);
int psdf_setup_image_to_mask_filter(psdf_binary_writer *pbw, gx_device_psdf *pdev,
int width, int height, int depth, int bits_per_sample, uint *MaskColor);
int psdf_setup_image_colors_filter(psdf_binary_writer *pbw,
gx_device_psdf *pdev, gs_pixel_image_t * pim,
const gs_imager_state *pis,
gs_color_space_index output_cspace_index);
#define psdf_write_string(s, str, size, print_ok)\
s_write_ps_string(s, str, size, print_ok)
#define psdf_alloc_position_stream(ps, mem)\
s_alloc_position_stream(ps, mem)
#define psdf_alloc_param_printer(pplist, ppp, s, mem)\
s_alloc_param_printer(pplist, ppp, s, mem)
#define psdf_free_param_printer(plist)\
s_free_param_printer(plist)
typedef struct psdf_set_color_commands_s {
const char *setgray;
const char *setrgbcolor;
const char *setcmykcolor;
const char *setcolorspace;
const char *setcolor;
const char *setcolorn;
} psdf_set_color_commands_t;
extern const psdf_set_color_commands_t
psdf_set_fill_color_commands, psdf_set_stroke_color_commands;
gx_color_index psdf_adjust_color_index(gx_device_vector *vdev,
gx_color_index color);
int psdf_set_color(gx_device_vector *vdev, const gx_drawing_color *pdc,
const psdf_set_color_commands_t *ppscc);
double psdf_round(double v, int precision, int radix);
dev_proc_get_bits(psdf_get_bits);
dev_proc_get_bits_rectangle(psdf_get_bits_rectangle);
dev_proc_create_compositor(psdf_create_compositor);
#endif