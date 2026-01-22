#ifndef gdevpdfg_INCLUDED
#  define gdevpdfg_INCLUDED
#include "gscspace.h"
typedef struct pdf_color_space_names_s {
const char *DeviceCMYK;
const char *DeviceGray;
const char *DeviceRGB;
const char *Indexed;
} pdf_color_space_names_t;
#define PDF_COLOR_SPACE_NAMES\
"/DeviceCMYK", "/DeviceGray", "/DeviceRGB", "/Indexed"
#define PDF_COLOR_SPACE_NAMES_SHORT\
"/CMYK", "/G", "/RGB", "/I"
extern const pdf_color_space_names_t
pdf_color_space_names,
pdf_color_space_names_short;
#ifndef gs_color_space_DEFINED
#  define gs_color_space_DEFINED
typedef struct gs_color_space_s gs_color_space;
#endif
typedef struct pdf_color_space_s pdf_color_space_t;
struct pdf_color_space_s {
pdf_resource_common(pdf_color_space_t);
const gs_range_t *ranges;
uint serialized_size;
byte *serialized;
};
#define public_st_pdf_color_space()  \
gs_public_st_suffix_add2(st_pdf_color_space, pdf_color_space_t,\
"pdf_color_space_t", pdf_color_space_enum_ptrs,\
pdf_color_space_reloc_ptrs, st_pdf_resource, ranges, serialized)
int pdf_cspace_init_Device(const gs_memory_t *mem, gs_color_space *pcs, int num_components);
int pdf_color_space(gx_device_pdf *pdev, cos_value_t *pvalue,
const gs_range_t **ppranges,
const gs_color_space *pcs,
const pdf_color_space_names_t *pcsn,
bool by_name);
int pdf_color_space_named(gx_device_pdf *pdev, cos_value_t *pvalue,
const gs_range_t **ppranges,
const gs_color_space *pcs,
const pdf_color_space_names_t *pcsn,
bool by_name, const byte *res_name, int name_length);
int pdf_cs_Pattern_colored(gx_device_pdf *pdev, cos_value_t *pvalue);
int pdf_cs_Pattern_uncolored(gx_device_pdf *pdev, cos_value_t *pvalue);
int pdf_cs_Pattern_uncolored_hl(gx_device_pdf *pdev,
const gs_color_space *pcs, cos_value_t *pvalue);
void pdf_color_space_procsets(gx_device_pdf *pdev,
const gs_color_space *pcs);
void pdf_viewer_state_from_imager_state(gx_device_pdf * pdev,
const gs_imager_state *pis, const gx_device_color *pdevc);
void pdf_prepare_initial_viewer_state(gx_device_pdf * pdev, const gs_imager_state *pis);
void pdf_reset_graphics(gx_device_pdf *pdev);
void pdf_set_initial_color(gx_device_pdf * pdev, gx_hl_saved_color *saved_fill_color,
gx_hl_saved_color *saved_stroke_color,
bool *fill_used_process_color, bool *stroke_used_process_color);
int pdf_set_pure_color(gx_device_pdf * pdev, gx_color_index color,
gx_hl_saved_color * psc,
bool *used_process_color,
const psdf_set_color_commands_t *ppscc);
int pdf_set_drawing_color(gx_device_pdf * pdev, const gs_imager_state * pis,
const gx_drawing_color *pdc,
gx_hl_saved_color * psc,
bool *used_process_color,
const psdf_set_color_commands_t *ppscc);
int pdf_try_prepare_fill(gx_device_pdf *pdev, const gs_imager_state *pis);
int pdf_prepare_drawing(gx_device_pdf *pdev, const gs_imager_state *pis, pdf_resource_t **ppres);
int pdf_prepare_fill(gx_device_pdf *pdev, const gs_imager_state *pis);
int pdf_prepare_stroke(gx_device_pdf *pdev, const gs_imager_state *pis);
int pdf_prepare_image(gx_device_pdf *pdev, const gs_imager_state *pis);
int pdf_prepare_imagemask(gx_device_pdf *pdev, const gs_imager_state *pis,
const gx_drawing_color *pdcolor);
int pdf_save_viewer_state(gx_device_pdf *pdev, stream *s);
int pdf_restore_viewer_state(gx_device_pdf *pdev, stream *s);
int pdf_end_gstate(gx_device_pdf *pdev, pdf_resource_t *pres);
int pdf_string_to_cos_name(gx_device_pdf *pdev, const byte *str, uint len,
cos_value_t *pvalue);
typedef struct pdf_pattern_s pdf_pattern_t;
struct pdf_pattern_s {
pdf_resource_common(pdf_pattern_t);
pdf_pattern_t *substitute;
};
#define private_st_pdf_pattern()  \
gs_private_st_suffix_add1(st_pdf_pattern, pdf_pattern_t,\
"pdf_pattern_t", pdf_pattern_enum_ptrs,\
pdf_pattern_reloc_ptrs, st_pdf_resource, substitute)
pdf_resource_t *pdf_substitute_pattern(pdf_resource_t *pres);
typedef struct pdf_image_names_s {
pdf_color_space_names_t color_spaces;
pdf_filter_names_t filter_names;
const char *BitsPerComponent;
const char *ColorSpace;
const char *Decode;
const char *Height;
const char *ImageMask;
const char *Interpolate;
const char *Width;
} pdf_image_names_t;
#define PDF_IMAGE_PARAM_NAMES\
"/BitsPerComponent", "/ColorSpace", "/Decode",\
"/Height", "/ImageMask", "/Interpolate", "/Width"
#define PDF_IMAGE_PARAM_NAMES_SHORT\
"/BPC", "/CS", "/D", "/H", "/IM", "/I", "/W"
extern const pdf_image_names_t pdf_image_names_full, pdf_image_names_short;
int pdf_put_image_values(cos_dict_t *pcd, gx_device_pdf *pdev,
const gs_pixel_image_t *pic,
const pdf_image_names_t *pin,
const cos_value_t *pcsvalue);
int pdf_put_image_filters(cos_dict_t *pcd, gx_device_pdf *pdev,
const psdf_binary_writer * pbw,
const pdf_image_names_t *pin);
void pdf_make_bitmap_matrix(gs_matrix * pmat, int x, int y, int w, int h,
int h_actual);
void pdf_put_image_matrix(gx_device_pdf * pdev, const gs_matrix * pmat,
floatp y_scale);
int pdf_do_image_by_id(gx_device_pdf * pdev, double scale,
const gs_matrix * pimat, bool in_contents, gs_id id);
int pdf_do_image(gx_device_pdf * pdev, const pdf_resource_t * pres,
const gs_matrix * pimat, bool in_contents);
#define pdf_image_writer_num_alt_streams 4
typedef struct pdf_image_writer_s {
psdf_binary_writer binary[pdf_image_writer_num_alt_streams];
int alt_writer_count;
const pdf_image_names_t *pin;
pdf_resource_t *pres;
int height;
cos_stream_t *data;
const char *end_string;
cos_dict_t *named;
pdf_resource_t *pres_mask;
} pdf_image_writer;
extern_st(st_pdf_image_writer);
#define public_st_pdf_image_writer() \
gs_public_st_composite(st_pdf_image_writer, pdf_image_writer,\
"pdf_image_writer", pdf_image_writer_enum_ptrs, pdf_image_writer_reloc_ptrs)
#define pdf_image_writer_max_ptrs (psdf_binary_writer_max_ptrs * pdf_image_writer_num_alt_streams + 4)
void pdf_image_writer_init(pdf_image_writer * piw);
int pdf_begin_write_image(gx_device_pdf * pdev, pdf_image_writer * piw,
gx_bitmap_id id, int w, int h,
cos_dict_t *pnamed, bool in_line);
int pdf_begin_image_data(gx_device_pdf * pdev, pdf_image_writer * piw,
const gs_pixel_image_t * pim,
const cos_value_t *pcsvalue,
int alt_writer_index);
int pdf_copy_mask_bits(stream *s, const byte *base, int sourcex,
int raster, int w, int h, byte invert);
int pdf_copy_color_bits(stream *s, const byte *base, int sourcex,
int raster, int w, int h, int bytes_per_pixel);
int
pdf_complete_image_data(gx_device_pdf *pdev, pdf_image_writer *piw, int data_h,
int width, int bits_per_pixel);
int pdf_end_image_binary(gx_device_pdf *pdev, pdf_image_writer *piw,
int data_h);
int pdf_end_write_image(gx_device_pdf * pdev, pdf_image_writer * piw);
int pdf_make_alt_stream(gx_device_pdf * pdev, psdf_binary_writer * piw);
int pdf_choose_compression(pdf_image_writer * piw, bool end_binary);
int pdf_register_charproc_resource(gx_device_pdf *pdev, gs_id id, pdf_resource_type_t type);
int pdf_store_pattern1_params(gx_device_pdf *pdev, pdf_resource_t *pres,
gs_pattern1_instance_t *pinst);
int pdf_put_colored_pattern(gx_device_pdf *pdev, const gx_drawing_color *pdc,
const gs_color_space *pcs,
const psdf_set_color_commands_t *ppscc,
bool have_pattern_streams, pdf_resource_t **ppres);
int pdf_put_uncolored_pattern(gx_device_pdf *pdev, const gx_drawing_color *pdc,
const gs_color_space *pcs,
const psdf_set_color_commands_t *ppscc,
bool have_pattern_streams, pdf_resource_t **ppres);
int pdf_put_pattern2(gx_device_pdf *pdev, const gx_drawing_color *pdc,
const psdf_set_color_commands_t *ppscc,
pdf_resource_t **ppres);
int pdf_copy_color_data(gx_device_pdf * pdev, const byte * base, int sourcex,
int raster, gx_bitmap_id id, int x, int y, int w, int h,
gs_image_t *pim, pdf_image_writer *piw,
int for_pattern);
#endif