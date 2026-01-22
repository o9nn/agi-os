#ifndef gdevpdfx_INCLUDED
#  define gdevpdfx_INCLUDED
#include "gsparam.h"
#include "gsuid.h"
#include "gxdevice.h"
#include "gxfont.h"
#include "gxline.h"
#include "stream.h"
#include "spprint.h"
#include "gdevpsdf.h"
#include "gxdevmem.h"
#define FINE_GLYPH_USAGE 1
#ifndef stream_arcfour_state_DEFINED
#define stream_arcfour_state_DEFINED
typedef struct stream_arcfour_state_s stream_arcfour_state;
#endif
#define MAX_USER_COORD 16300
#define MAX_OUTLINE_DEPTH 32
#define MAX_DEST_STRING 80
typedef enum {
PDF_IN_NONE,
PDF_IN_STREAM,
PDF_IN_TEXT,
PDF_IN_STRING
} pdf_context_t;
typedef struct cos_object_s cos_object_t;
typedef struct cos_stream_s cos_stream_t;
typedef struct cos_dict_s cos_dict_t;
typedef struct cos_array_s cos_array_t;
typedef struct cos_value_s cos_value_t;
typedef struct cos_object_procs_s cos_object_procs_t;
typedef const cos_object_procs_t *cos_type_t;
#define cos_types_DEFINED
#ifndef pdf_text_state_DEFINED
#  define pdf_text_state_DEFINED
typedef struct pdf_text_state_s pdf_text_state_t;
#endif
#ifndef pdf_char_glyph_pairs_DEFINED
#  define pdf_char_glyph_pairs_DEFINED
typedef struct pdf_char_glyph_pairs_s pdf_char_glyph_pairs_t;
#endif
typedef enum {
resourceColorSpace,
resourceExtGState,
resourcePattern,
resourceShading,
resourceXObject,
resourceOther,
resourceFont,
resourceCharProc,
resourceCIDFont,
resourceCMap,
resourceFontDescriptor,
resourceGroup,
resourceSoftMaskDict,
resourceFunction,
resourcePage,
NUM_RESOURCE_TYPES
} pdf_resource_type_t;
#define PDF_RESOURCE_TYPE_NAMES\
"/ColorSpace", "/ExtGState", "/Pattern", "/Shading", "/XObject", 0, "/Font",\
0, "/Font", "/CMap", "/FontDescriptor", "/Group", "/Mask", 0, 0
#define PDF_RESOURCE_TYPE_STRUCTS\
&st_pdf_color_space,		\
&st_pdf_resource,		\
&st_pdf_pattern,\
&st_pdf_resource,\
&st_pdf_x_object,		\
&st_pdf_resource,\
&st_pdf_font_resource,	\
&st_pdf_char_proc,		\
&st_pdf_font_resource,	\
&st_pdf_resource,\
&st_pdf_font_descriptor,	\
&st_pdf_resource,\
&st_pdf_resource,\
&st_pdf_resource,\
&st_pdf_resource
#define pdf_resource_common(typ)\
typ *next;			\
pdf_resource_t *prev;	\
gs_id rid;			\
bool named;\
bool global;                \
char rname[1 + (sizeof(long) * 8 / 3 + 1) + 1];\
ulong where_used;		\
cos_object_t *object
typedef struct pdf_resource_s pdf_resource_t;
struct pdf_resource_s {
pdf_resource_common(pdf_resource_t);
};
extern_st(st_pdf_resource);
#define public_st_pdf_resource()  \
gs_public_st_ptrs3(st_pdf_resource, pdf_resource_t, "pdf_resource_t",\
pdf_resource_enum_ptrs, pdf_resource_reloc_ptrs, next, prev, object)
typedef struct pdf_x_object_s pdf_x_object_t;
struct pdf_x_object_s {
pdf_resource_common(pdf_x_object_t);
int width, height;
int data_height;
};
#define private_st_pdf_x_object()  \
gs_private_st_suffix_add0(st_pdf_x_object, pdf_x_object_t,\
"pdf_x_object_t", pdf_x_object_enum_ptrs, pdf_x_object_reloc_ptrs,\
st_pdf_resource)
typedef enum {
NoMarks = 0,
ImageB = 1,
ImageC = 2,
ImageI = 4,
Text = 8
} pdf_procset_t;
typedef struct pdf_char_proc_s pdf_char_proc_t;
typedef struct pdf_font_s pdf_font_t;
typedef struct pdf_text_data_s pdf_text_data_t;
typedef struct pdf_outline_node_s {
long id, parent_id, prev_id, first_id, last_id;
int count;
cos_dict_t *action;
} pdf_outline_node_t;
typedef struct pdf_outline_level_s {
pdf_outline_node_t first;
pdf_outline_node_t last;
int left;
} pdf_outline_level_t;
typedef struct pdf_bead_s {
long id, article_id, prev_id, next_id, page_id;
gs_rect rect;
} pdf_bead_t;
typedef struct pdf_article_s pdf_article_t;
struct pdf_article_s {
pdf_article_t *next;
cos_dict_t *contents;
pdf_bead_t first;
pdf_bead_t last;
};
#define private_st_pdf_article()\
gs_private_st_ptrs2(st_pdf_article, pdf_article_t,\
"pdf_article_t", pdf_article_enum_ptrs, pdf_article_reloc_ptrs,\
next, contents)
#define NUM_RESOURCE_CHAINS 16
typedef struct pdf_resource_list_s {
pdf_resource_t *chains[NUM_RESOURCE_CHAINS];
} pdf_resource_list_t;
#define gs_id_hash(rid) ((rid) + ((rid) / NUM_RESOURCE_CHAINS))
#define PDF_RESOURCE_CHAIN(pdev, type, rid)\
(&(pdev)->resources[type].chains[gs_id_hash(rid) % NUM_RESOURCE_CHAINS])
typedef struct pdf_stream_position_s {
long length_id;
long start_pos;
} pdf_stream_position_t;
typedef struct pdf_text_rotation_s {
long counts[5];
int Rotate;
} pdf_text_rotation_t;
#define pdf_text_rotation_angle_values 0, 90, 180, 270, -1
typedef struct pdf_page_dsc_info_s {
int orientation;
int viewing_orientation;
gs_rect bounding_box;
} pdf_page_dsc_info_t;
typedef struct pdf_page_s {
cos_dict_t *Page;
gs_point MediaBox;
pdf_procset_t procsets;
long contents_id;
long resource_ids[resourceFont + 1];
long group_id;
cos_array_t *Annots;
pdf_text_rotation_t text_rotation;
pdf_page_dsc_info_t dsc_info;
bool NumCopies_set;
int NumCopies;
} pdf_page_t;
#define private_st_pdf_page()	\
gs_private_st_ptrs2(st_pdf_page, pdf_page_t, "pdf_page_t",\
pdf_page_enum_ptrs, pdf_page_reloc_ptrs, Page, Annots)
typedef struct pdf_temp_file_s {
char file_name[gp_file_name_sizeof];
FILE *file;
stream *strm;
byte *strm_buf;
stream *save_strm;
} pdf_temp_file_t;
#ifndef gx_device_pdf_DEFINED
#  define gx_device_pdf_DEFINED
typedef struct gx_device_pdf_s gx_device_pdf;
#endif
typedef struct pdf_font_cache_elem_s pdf_font_cache_elem_t;
struct pdf_font_cache_elem_s {
pdf_font_cache_elem_t *next;
gs_id font_id;
int num_chars;
int num_widths;
struct pdf_font_resource_s *pdfont;
byte *glyph_usage;
double *real_widths;
gx_device_pdf *pdev;
};
#define private_st_pdf_font_cache_elem()\
gs_private_st_ptrs5(st_pdf_font_cache_elem, pdf_font_cache_elem_t,\
"pdf_font_cache_elem_t", pdf_font_cache_elem_enum,\
pdf_font_cache_elem_reloc, next, pdfont,\
glyph_usage, real_widths, pdev)
typedef struct pdf_viewer_state_s {
int transfer_not_identity;
gs_id transfer_ids[4];
float opacity_alpha;
float shape_alpha;
gs_blend_mode_t blend_mode;
gs_id halftone_id;
gs_id black_generation_id;
gs_id undercolor_removal_id;
int overprint_mode;
float smoothness;
float flatness;
bool text_knockout;
bool fill_overprint;
bool stroke_overprint;
bool stroke_adjust;
bool fill_used_process_color;
bool stroke_used_process_color;
gx_hl_saved_color saved_fill_color;
gx_hl_saved_color saved_stroke_color;
gx_line_params line_params;
float dash_pattern[max_dash];
gs_id soft_mask_id;
} pdf_viewer_state;
typedef struct pdf_substream_save_s {
pdf_context_t	context;
pdf_text_state_t	*text_state;
gx_path		*clip_path;
gs_id		clip_path_id;
int			vgstack_bottom;
stream		*strm;
cos_dict_t		*substream_Resources;
pdf_procset_t	procsets;
bool		skip_colors;
pdf_resource_t      *font3;
pdf_resource_t	*accumulating_substream_resource;
bool		charproc_just_accumulated;
bool		accumulating_a_global_object;
pdf_resource_t      *pres_soft_mask_dict;
gs_const_string		objname;
} pdf_substream_save;
#define private_st_pdf_substream_save()\
gs_private_st_strings1_ptrs7(st_pdf_substream_save, pdf_substream_save,\
"pdf_substream_save", pdf_substream_save_enum,\
pdf_substream_save_reloc, objname, text_state, clip_path, strm, \
substream_Resources, font3, accumulating_substream_resource, pres_soft_mask_dict)
#define private_st_pdf_substream_save_element()\
gs_private_st_element(st_pdf_substream_save_element, pdf_substream_save,\
"pdf_substream_save[]", pdf_substream_save_elt_enum_ptrs,\
pdf_substream_save_elt_reloc_ptrs, st_pdf_substream_save)
typedef enum {
pdf_compress_none,
pdf_compress_LZW,
pdf_compress_Flate
} pdf_compression_type;
struct gx_device_pdf_s {
gx_device_psdf_common;
double CompatibilityLevel;
int EndPage;
int StartPage;
bool Optimize;
bool ParseDSCCommentsForDocInfo;
bool ParseDSCComments;
bool EmitDSCWarnings;
bool CreateJobTicket;
bool PreserveEPSInfo;
bool AutoPositionEPSFiles;
bool PreserveCopyPage;
bool UsePrologue;
int OffOptimizations;
bool ReAssignCharacters;
bool ReEncodeCharacters;
long FirstObjectNumber;
bool CompressFonts;
bool PrintStatistics;
bool ForOPDFRead;
gs_param_string OPDFReadProcsetPath;
bool CompressEntireFile;
bool ResourcesBeforeUsage;
bool HavePDFWidths;
bool HaveStrokeColor;
bool HaveTransparency;
bool PatternImagemask;
bool PDFX;
long MaxClipPathSize;
long MaxViewerMemorySize;
long MaxShadingBitmapSize;
long MaxInlineImageSize;
gs_param_string OwnerPassword;
gs_param_string UserPassword;
uint KeyLength;
uint Permissions;
uint EncryptionR;
byte EncryptionO[32];
byte EncryptionU[32];
byte EncryptionKey[16];
uint EncryptionV;
bool EncryptMetadata;
gs_param_string NoEncrypt;
bool is_EPS;
pdf_page_dsc_info_t doc_dsc_info;
pdf_page_dsc_info_t page_dsc_info;
bool fill_overprint, stroke_overprint;
int overprint_mode;
gs_id halftone_id;
gs_id transfer_ids[4];
int transfer_not_identity;
gs_id black_generation_id, undercolor_removal_id;
pdf_compression_type compression;
pdf_compression_type compression_at_page_start;
#define pdf_memory v_memory
pdf_temp_file_t xref;
pdf_temp_file_t asides;
pdf_temp_file_t streams;
pdf_temp_file_t pictures;
long next_id;
cos_dict_t *Catalog;
cos_dict_t *Info;
cos_dict_t *Pages;
#define pdf_num_initial_ids 3
long outlines_id;
int next_page;
int max_referred_page;
long contents_id;
pdf_context_t context;
long contents_length_id;
long contents_pos;
pdf_procset_t procsets;
pdf_text_data_t *text;
pdf_text_rotation_t text_rotation;
#define initial_num_pages 50
pdf_page_t *pages;
int num_pages;
ulong used_mask;
pdf_resource_list_t resources[NUM_RESOURCE_TYPES];
pdf_resource_t *cs_Patterns[5];
pdf_resource_t *Identity_ToUnicode_CMaps[2];
pdf_resource_t *last_resource;
pdf_outline_level_t outline_levels[MAX_OUTLINE_DEPTH];
int outline_depth;
int closed_outline_depth;
int outlines_open;
pdf_article_t *articles;
cos_dict_t *Dests;
byte fileID[16];
cos_dict_t *global_named_objects;
cos_dict_t *local_named_objects;
cos_array_t *NI_stack;
cos_array_t *Namespace_stack;
pdf_font_cache_elem_t *font_cache;
gs_point char_width;
gx_path *clip_path;
cos_array_t *PageLabels;
int PageLabels_current_page;
cos_dict_t *PageLabels_current_label;
gs_text_enum_t *pte;
pdf_viewer_state vgstack[11];
int vgstack_depth;
int vgstack_bottom;
pdf_viewer_state vg_initial;
bool vg_initial_set;
int sbstack_size;
int sbstack_depth;
pdf_substream_save *sbstack;
cos_dict_t *substream_Resources;
gs_color_space_index pcm_color_info_index;
bool skip_colors;
bool AR4_save_bug;
pdf_resource_t *font3;
pdf_resource_t *accumulating_substream_resource;
gs_matrix_fixed charproc_ctm;
bool charproc_just_accumulated;
bool accumulating_a_global_object;
const pdf_char_glyph_pairs_t *cgp;
int substituted_pattern_count;
int substituted_pattern_drop_page;
gs_id     image_mask_id;
bool      image_mask_is_SMask;
bool      image_mask_skip;
gs_matrix converting_image_matrix;
double    image_mask_scale;
pdf_resource_t *pres_soft_mask_dict;
gs_const_string objname;
};
#define is_in_page(pdev)\
((pdev)->contents_id != 0)
#define is_in_document(pdev)\
(is_in_page(pdev) || (pdev)->last_resource != 0)
#define gx_device_pdf_do_ptrs(m)\
m(0,asides.strm) m(1,asides.strm_buf) m(2,asides.save_strm)\
m(3,streams.strm) m(4,streams.strm_buf)\
m(5,pictures.strm) m(6,pictures.strm_buf) m(7,pictures.save_strm)\
m(8,Catalog) m(9,Info) m(10,Pages)\
m(11,text) m(12,pages)\
m(13,cs_Patterns[0])\
m(14,cs_Patterns[1]) m(15,cs_Patterns[3]) m(16,cs_Patterns[4])\
m(17,last_resource)\
m(18,articles) m(19,Dests) m(20,global_named_objects)\
m(21, local_named_objects) m(22,NI_stack) m(23,Namespace_stack)\
m(24,font_cache) m(25,clip_path)\
m(26,PageLabels) m(27,PageLabels_current_label)\
m(28,sbstack) m(29,substream_Resources) m(30,font3)\
m(31,accumulating_substream_resource) \
m(32,pres_soft_mask_dict)
#define gx_device_pdf_num_ptrs 33
#define gx_device_pdf_do_param_strings(m)\
m(0, OPDFReadProcsetPath) m(1, OwnerPassword) m(2, UserPassword) m(3, NoEncrypt)
#define gx_device_pdf_num_param_strings 4
#define gx_device_pdf_do_const_strings(m)\
m(0, objname)
#define gx_device_pdf_num_const_strings 1
#define st_device_pdf_max_ptrs\
(st_device_psdf_max_ptrs + gx_device_pdf_num_ptrs +\
gx_device_pdf_num_param_strings + gx_device_pdf_num_const_strings +\
NUM_RESOURCE_TYPES * NUM_RESOURCE_CHAINS  +\
MAX_OUTLINE_DEPTH * 2
#define private_st_device_pdfwrite()	\
gs_private_st_composite_final(st_device_pdfwrite, gx_device_pdf,\
"gx_device_pdf", device_pdfwrite_enum_ptrs, device_pdfwrite_reloc_ptrs,\
device_pdfwrite_finalize)
dev_proc_copy_mono(gdev_pdf_copy_mono);
dev_proc_copy_color(gdev_pdf_copy_color);
dev_proc_fill_mask(gdev_pdf_fill_mask);
dev_proc_strip_tile_rectangle(gdev_pdf_strip_tile_rectangle);
extern const gx_device_vector_procs pdf_vector_procs;
dev_proc_fill_rectangle(gdev_pdf_fill_rectangle);
dev_proc_fill_path(gdev_pdf_fill_path);
dev_proc_stroke_path(gdev_pdf_stroke_path);
dev_proc_begin_typed_image(gdev_pdf_begin_typed_image);
dev_proc_get_params(gdev_pdf_get_params);
dev_proc_put_params(gdev_pdf_put_params);
dev_proc_text_begin(gdev_pdf_text_begin);
dev_proc_pattern_manage(gdev_pdf_pattern_manage);
dev_proc_fill_rectangle_hl_color(gdev_pdf_fill_rectangle_hl_color);
dev_proc_include_color_space(gdev_pdf_include_color_space);
dev_proc_create_compositor(gdev_pdf_create_compositor);
dev_proc_begin_transparency_group(gdev_pdf_begin_transparency_group);
dev_proc_end_transparency_group(gdev_pdf_end_transparency_group);
dev_proc_begin_transparency_mask(gdev_pdf_begin_transparency_mask);
dev_proc_end_transparency_mask(gdev_pdf_end_transparency_mask);
dev_proc_discard_transparency_layer(gdev_pdf_discard_transparency_layer);
void pdf_initialize_ids(gx_device_pdf * pdev);
void pdf_set_process_color_model(gx_device_pdf * pdev, int index);
void pdf_reset_text(gx_device_pdf *pdev);
int pdf_open_document(gx_device_pdf * pdev);
long pdf_obj_ref(gx_device_pdf * pdev);
long pdf_stell(gx_device_pdf * pdev);
long pdf_open_obj(gx_device_pdf * pdev, long id);
long pdf_begin_obj(gx_device_pdf * pdev);
int pdf_end_obj(gx_device_pdf * pdev);
int pdf_open_contents(gx_device_pdf * pdev, pdf_context_t context);
int pdf_close_contents(gx_device_pdf * pdev, bool last);
extern const char *const pdf_resource_type_names[];
extern const gs_memory_struct_type_t *const pdf_resource_type_structs[];
#define ASIDES_BASE_POSITION min_long
long pdf_open_separate(gx_device_pdf * pdev, long id);
long pdf_begin_separate(gx_device_pdf * pdev);
void pdf_reserve_object_id(gx_device_pdf * pdev, pdf_resource_t *ppres, long id);
int pdf_alloc_aside(gx_device_pdf * pdev, pdf_resource_t ** plist,
const gs_memory_struct_type_t * pst, pdf_resource_t **ppres,
long id);
int pdf_begin_aside(gx_device_pdf * pdev, pdf_resource_t **plist,
const gs_memory_struct_type_t * pst,
pdf_resource_t **ppres);
int pdf_begin_resource(gx_device_pdf * pdev, pdf_resource_type_t rtype,
gs_id rid, pdf_resource_t **ppres);
int pdf_begin_resource_body(gx_device_pdf * pdev, pdf_resource_type_t rtype,
gs_id rid, pdf_resource_t **ppres);
int pdf_alloc_resource(gx_device_pdf * pdev, pdf_resource_type_t rtype,
gs_id rid, pdf_resource_t **ppres, long id);
int pdf_find_same_resource(gx_device_pdf * pdev,
pdf_resource_type_t rtype, pdf_resource_t **ppres,
int (*eq)(gx_device_pdf * pdev, pdf_resource_t *pres0, pdf_resource_t *pres1));
pdf_resource_t *pdf_find_resource_by_resource_id(gx_device_pdf * pdev,
pdf_resource_type_t rtype, gs_id id);
pdf_resource_t *pdf_find_resource_by_gs_id(gx_device_pdf * pdev,
pdf_resource_type_t rtype,
gs_id rid);
void pdf_drop_resources(gx_device_pdf * pdev, pdf_resource_type_t rtype,
int (*cond)(gx_device_pdf * pdev, pdf_resource_t *pres));
void pdf_print_resource_statistics(gx_device_pdf * pdev);
int pdf_cancel_resource(gx_device_pdf * pdev, pdf_resource_t *pres,
pdf_resource_type_t rtype);
void pdf_forget_resource(gx_device_pdf * pdev, pdf_resource_t *pres1,
pdf_resource_type_t rtype);
int pdf_substitute_resource(gx_device_pdf *pdev, pdf_resource_t **ppres,
pdf_resource_type_t rtype,
int (*eq)(gx_device_pdf *pdev, pdf_resource_t *pres0, pdf_resource_t *pres1),
bool write);
long pdf_resource_id(const pdf_resource_t *pres);
int pdf_end_separate(gx_device_pdf * pdev);
int pdf_end_aside(gx_device_pdf * pdev);
int pdf_end_resource(gx_device_pdf * pdev);
int pdf_write_resource_objects(gx_device_pdf *pdev, pdf_resource_type_t rtype);
void pdf_reverse_resource_chain(gx_device_pdf *pdev, pdf_resource_type_t rtype);
int pdf_free_resource_objects(gx_device_pdf *pdev, pdf_resource_type_t rtype);
int pdf_write_and_free_all_resource_objects(gx_device_pdf *pdev);
int pdf_store_page_resources(gx_device_pdf *pdev, pdf_page_t *page);
void pdf_copy_data(stream *s, FILE *file, long count, stream_arcfour_state *ss);
void pdf_copy_data_safe(stream *s, FILE *file, long position, long count);
int pdf_begin_encrypt(gx_device_pdf * pdev, stream **s, gs_id object_id);
void pdf_end_encrypt(gx_device_pdf * pdev);
int pdf_encrypt_init(const gx_device_pdf * pdev, gs_id object_id, stream_arcfour_state *psarc4);
long pdf_page_id(gx_device_pdf * pdev, int page_num);
pdf_page_t *pdf_current_page(gx_device_pdf *pdev);
cos_dict_t *pdf_current_page_dict(gx_device_pdf *pdev);
int pdf_open_page(gx_device_pdf * pdev, pdf_context_t context);
int pdf_unclip(gx_device_pdf * pdev);
int pdf_write_saved_string(gx_device_pdf * pdev, gs_string * pstr);
int pdf_remember_clip_path(gx_device_pdf * pdev, const gx_clip_path * pcpath);
bool pdf_must_put_clip_path(gx_device_pdf * pdev, const gx_clip_path * pcpath);
int pdf_put_clip_path(gx_device_pdf * pdev, const gx_clip_path * pcpath);
typedef struct pdf_lcvd_s {
gx_device_memory mdev;
gx_device_memory *mask;
gx_device_pdf *pdev;
dev_t_proc_fill_rectangle((*std_fill_rectangle), gx_device);
dev_t_proc_close_device((*std_close_device), gx_device);
bool mask_is_empty;
bool path_is_empty;
bool mask_is_clean;
bool write_matrix;
bool has_background;
gs_matrix m;
gs_point path_offset;
} pdf_lcvd_t;
#define public_st_pdf_lcvd_t()\
gs_public_st_suffix_add2(st_pdf_lcvd_t, pdf_lcvd_t,\
"pdf_lcvd_t", pdf_lcvd_t_enum_ptrs,\
pdf_lcvd_t_reloc_ptrs, st_device_memory, mask, pdev)
#define pdf_lcvd_t_max_ptrs (gx_device_memory_max_ptrs + 2)
int pdf_setup_masked_image_converter(gx_device_pdf *pdev, gs_memory_t *mem, const gs_matrix *m, pdf_lcvd_t **pcvd,
bool need_mask, int x, int y, int w, int h, bool write_on_close);
int pdf_dump_converted_image(gx_device_pdf *pdev, pdf_lcvd_t *cvd);
void pdf_remove_masked_image_converter(gx_device_pdf *pdev, pdf_lcvd_t *cvd, bool need_mask);
#define PDF_MAX_PRODUCER 200
void pdf_store_default_Producer(char buf[PDF_MAX_PRODUCER]);
typedef struct pdf_filter_names_s {
const char *ASCII85Decode;
const char *ASCIIHexDecode;
const char *CCITTFaxDecode;
const char *DCTDecode;
const char *DecodeParms;
const char *Filter;
const char *FlateDecode;
const char *LZWDecode;
const char *RunLengthDecode;
} pdf_filter_names_t;
#define PDF_FILTER_NAMES\
"/ASCII85Decode", "/ASCIIHexDecode", "/CCITTFaxDecode",\
"/DCTDecode",  "/DecodeParms", "/Filter", "/FlateDecode",\
"/LZWDecode", "/RunLengthDecode"
#define PDF_FILTER_NAMES_SHORT\
"/A85", "/AHx", "/CCF", "/DCT", "/DP", "/F", "/Fl", "/LZW", "/RL"
void pdf_put_matrix(gx_device_pdf *pdev, const char *before,
const gs_matrix *pmat, const char *after);
typedef int (*pdf_put_name_chars_proc_t)(stream *, const byte *, uint);
pdf_put_name_chars_proc_t
pdf_put_name_chars_proc(const gx_device_pdf *pdev);
int pdf_put_name_chars(const gx_device_pdf *pdev, const byte *nstr,
uint size);
int pdf_put_name(const gx_device_pdf *pdev, const byte *nstr, uint size);
int pdf_put_string(const gx_device_pdf *pdev, const byte *str, uint size);
int pdf_write_value(const gx_device_pdf *pdev, const byte *vstr, uint size, gs_id object_id);
int pdf_put_filters(cos_dict_t *pcd, gx_device_pdf *pdev, stream *s,
const pdf_filter_names_t *pfn);
typedef struct pdf_data_writer_s {
psdf_binary_writer binary;
long start;
long length_pos;
pdf_resource_t *pres;
gx_device_pdf *pdev;
long length_id;
bool encrypted;
} pdf_data_writer_t;
#define DATA_STREAM_NOT_BINARY 0
#define DATA_STREAM_BINARY 1
#define DATA_STREAM_COMPRESS 2
#define DATA_STREAM_NOLENGTH 4
#define DATA_STREAM_ENCRYPT  8
int pdf_begin_data_stream(gx_device_pdf *pdev, pdf_data_writer_t *pdw,
int options, gs_id object_id);
int pdf_append_data_stream_filters(gx_device_pdf *pdev, pdf_data_writer_t *pdw,
int orig_options, gs_id object_id);
int pdf_begin_data(gx_device_pdf *pdev, pdf_data_writer_t *pdw);
int pdf_end_data(pdf_data_writer_t *pdw);
#define MAX_REF_CHARS ((sizeof(long) * 8 + 2) / 3)
#ifndef gs_function_DEFINED
typedef struct gs_function_s gs_function_t;
#  define gs_function_DEFINED
#endif
int pdf_function(gx_device_pdf *pdev, const gs_function_t *pfn,
cos_value_t *pvalue);
int pdf_function_scaled(gx_device_pdf *pdev, const gs_function_t *pfn,
const gs_range_t *pranges, cos_value_t *pvalue);
int pdf_write_function(gx_device_pdf *pdev, const gs_function_t *pfn,
long *pid);
int pdf_write_font_bbox(gx_device_pdf *pdev, const gs_int_rect *pbox);
#define pdfmark_proc(proc)\
int proc(gx_device_pdf *pdev, gs_param_string *pairs, uint count,\
const gs_matrix *pctm, const gs_param_string *objname)
bool pdf_key_eq(const gs_param_string * pcs, const char *str);
int pdfmark_scan_int(const gs_param_string * pstr, int *pvalue);
int pdfmark_process(gx_device_pdf * pdev, const gs_param_string_array * pma);
int pdfmark_close_outline(gx_device_pdf * pdev);
int pdfmark_end_pagelabels(gx_device_pdf * pdev);
int pdfmark_write_article(gx_device_pdf * pdev, const pdf_article_t * part);
bool pdf_objname_is_valid(const byte *data, uint size);
int pdf_find_named(gx_device_pdf * pdev, const gs_param_string * pname,
cos_object_t **ppco);
int pdf_create_named(gx_device_pdf *pdev, const gs_param_string *pname,
cos_type_t cotype, cos_object_t **ppco, long id);
int pdf_create_named_dict(gx_device_pdf *pdev, const gs_param_string *pname,
cos_dict_t **ppcd, long id);
int pdf_refer_named(gx_device_pdf *pdev, const gs_param_string *pname,
cos_object_t **ppco);
int pdf_make_named(gx_device_pdf * pdev, const gs_param_string * pname,
cos_type_t cotype, cos_object_t **ppco, bool assign_id);
int pdf_make_named_dict(gx_device_pdf * pdev, const gs_param_string * pname,
cos_dict_t **ppcd, bool assign_id);
int pdf_get_named(gx_device_pdf * pdev, const gs_param_string * pname,
cos_type_t cotype, cos_object_t **ppco);
int pdf_push_namespace(gx_device_pdf *pdev);
int pdf_pop_namespace(gx_device_pdf *pdev);
int pdf_scan_token(const byte **pscan, const byte * end, const byte **ptoken);
int pdf_scan_token_composite(const byte **pscan, const byte * end,
const byte **ptoken);
int pdf_replace_names(gx_device_pdf *pdev, const gs_param_string *from,
gs_param_string *to);
int pdf_close_text_document(gx_device_pdf *pdev);
pdf_text_data_t *pdf_text_data_alloc(gs_memory_t *mem);
void pdf_set_text_state_default(pdf_text_state_t *pts);
void pdf_text_state_copy(pdf_text_state_t *pts_to, pdf_text_state_t *pts_from);
void pdf_reset_text_page(pdf_text_data_t *ptd);
void pdf_reset_text_state(pdf_text_data_t *ptd);
void pdf_close_text_page(gx_device_pdf *pdev);
int pdf_char_image_y_offset(const gx_device_pdf *pdev, int x, int y, int h);
int pdf_begin_char_proc(gx_device_pdf * pdev, int w, int h, int x_width,
int y_offset, gs_id id, pdf_char_proc_t **ppcp,
pdf_stream_position_t * ppos);
int pdf_end_char_proc(gx_device_pdf * pdev, pdf_stream_position_t * ppos);
int pdf_do_char_image(gx_device_pdf * pdev, const pdf_char_proc_t * pcp,
const gs_matrix * pimat);
int pdf_start_charproc_accum(gx_device_pdf *pdev);
int pdf_set_charproc_attrs(gx_device_pdf *pdev, gs_font *font, const double *pw, int narg,
gs_text_cache_control_t control, gs_char ch, gs_const_string *gnstr);
int pdf_end_charproc_accum(gx_device_pdf *pdev, gs_font *font,
const pdf_char_glyph_pairs_t *cgp);
int pdf_open_aside(gx_device_pdf *pdev, pdf_resource_type_t rtype,
gs_id id, pdf_resource_t **ppres, bool reserve_object_id, int options);
int pdf_close_aside(gx_device_pdf *pdev);
int pdf_enter_substream(gx_device_pdf *pdev, pdf_resource_type_t rtype,
gs_id id, pdf_resource_t **ppres, bool reserve_object_id, bool compress);
int pdf_exit_substream(gx_device_pdf *pdev);
int pdf_add_procsets(cos_dict_t *pcd, pdf_procset_t procsets);
int pdf_add_resource(gx_device_pdf *pdev, cos_dict_t *pcd, const char *key, pdf_resource_t *pres);
int pdf_from_stream_to_text(gx_device_pdf *pdev);
int pdf_from_string_to_text(gx_device_pdf *pdev);
void pdf_close_text_contents(gx_device_pdf *pdev);
#endif