#ifndef gxfont_INCLUDED
#  define gxfont_INCLUDED
#include "gsccode.h"
#include "gsfont.h"
#include "gsgdata.h"
#include "gsmatrix.h"
#include "gsnotify.h"
#include "gsuid.h"
#include "gsstype.h"
#include "gxftype.h"
#ifndef gs_text_enum_DEFINED
#  define gs_text_enum_DEFINED
typedef struct gs_text_enum_s gs_text_enum_t;
#endif
#ifndef gx_path_DEFINED
#  define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
#define FONT_IS_FIXED_WIDTH (1<<0)
typedef struct gs_font_info_s {
int members;
#define FONT_INFO_ASCENT 0x0001
int Ascent;
#define FONT_INFO_AVG_WIDTH 0x0002
int AvgWidth;
#define FONT_INFO_BBOX 0x0004
gs_int_rect BBox;
#define FONT_INFO_CAP_HEIGHT 0x0008
int CapHeight;
#define FONT_INFO_DESCENT 0x0010
int Descent;
#define FONT_INFO_FLAGS 0x0020
uint Flags;
uint Flags_requested;
uint Flags_returned;
#define FONT_INFO_ITALIC_ANGLE 0x0100
float ItalicAngle;
#define FONT_INFO_LEADING 0x0200
int Leading;
#define FONT_INFO_MAX_WIDTH 0x0400
int MaxWidth;
#define FONT_INFO_MISSING_WIDTH 0x0800
int MissingWidth;
#define FONT_INFO_STEM_H 0x00010000
int StemH;
#define FONT_INFO_STEM_V 0x00020000
int StemV;
#define FONT_INFO_UNDERLINE_POSITION 0x00040000
int UnderlinePosition;
#define FONT_INFO_UNDERLINE_THICKNESS 0x00080000
int UnderlineThickness;
#define FONT_INFO_X_HEIGHT 0x00100000
int XHeight;
#define FONT_INFO_COPYRIGHT 0x0040
gs_const_string Copyright;
#define FONT_INFO_NOTICE 0x0080
gs_const_string Notice;
#define FONT_INFO_FAMILY_NAME 0x1000
gs_const_string FamilyName;
#define FONT_INFO_FULL_NAME 0x2000
gs_const_string FullName;
} gs_font_info_t;
#define public_st_gs_font_info() \
BASIC_PTRS(gs_font_info_ptrs) {\
GC_CONST_STRING_ELT(gs_font_info_t, Copyright),\
GC_CONST_STRING_ELT(gs_font_info_t, Notice),\
GC_CONST_STRING_ELT(gs_font_info_t, FamilyName),\
GC_CONST_STRING_ELT(gs_font_info_t, FullName)\
};\
gs_public_st_basic(st_gs_font_info, gs_font_info_t, "gs_font_info_t",\
gs_font_info_ptrs, gs_font_info_data)
typedef struct gs_glyph_info_s {
int members;
#define GLYPH_INFO_WIDTH0 1
#define GLYPH_INFO_WIDTH GLYPH_INFO_WIDTH0
#define GLYPH_INFO_WIDTH1 2
#define GLYPH_INFO_WIDTHS (GLYPH_INFO_WIDTH0 | GLYPH_INFO_WIDTH1)
gs_point width[2];
gs_point v;
#define GLYPH_INFO_BBOX 4
gs_rect bbox;
#define GLYPH_INFO_NUM_PIECES 8
int num_pieces;
#define GLYPH_INFO_PIECES 16
gs_glyph *pieces;
#define GLYPH_INFO_OUTLINE_WIDTHS 32
#define GLYPH_INFO_VVECTOR0 64
#define GLYPH_INFO_VVECTOR1 128
#define GLYPH_INFO_CDEVPROC 256
} gs_glyph_info_t;
typedef struct gs_font_procs_s {
#define font_proc_define_font(proc)\
int proc(gs_font_dir *, gs_font *)
font_proc_define_font((*define_font));
#define font_proc_make_font(proc)\
int proc(gs_font_dir *, const gs_font *, const gs_matrix *, gs_font **)
font_proc_make_font((*make_font));
#define font_proc_font_info(proc)\
int proc(gs_font *font, const gs_point *pscale, int members,\
gs_font_info_t *info)
font_proc_font_info((*font_info));
#define FONT_SAME_OUTLINES 1
#define FONT_SAME_METRICS 2
#define FONT_SAME_ENCODING 4
#define font_proc_same_font(proc)\
int proc(const gs_font *font, const gs_font *ofont, int mask)
font_proc_same_font((*same_font));
#define font_proc_encode_char(proc)\
gs_glyph proc(gs_font *, gs_char, gs_glyph_space_t)
font_proc_encode_char((*encode_char));
#define font_proc_decode_glyph(proc)\
gs_char proc(gs_font *, gs_glyph)
font_proc_decode_glyph((*decode_glyph));
#define font_proc_enumerate_glyph(proc)\
int proc(gs_font *font, int *pindex, gs_glyph_space_t glyph_space,\
gs_glyph *pglyph)
font_proc_enumerate_glyph((*enumerate_glyph));
#define font_proc_glyph_info(proc)\
int proc(gs_font *font, gs_glyph glyph, const gs_matrix *pmat,\
int members, gs_glyph_info_t *info)
font_proc_glyph_info((*glyph_info));
#define font_proc_glyph_outline(proc)\
int proc(gs_font *font, int WMode, gs_glyph glyph, const gs_matrix *pmat,\
gx_path *ppath, double sbw[4])
font_proc_glyph_outline((*glyph_outline));
#define font_proc_glyph_name(proc)\
int proc(gs_font *font, gs_glyph glyph, gs_const_string *pstr)
font_proc_glyph_name((*glyph_name));
#define font_proc_init_fstack(proc)\
int proc(gs_text_enum_t *, gs_font *)
font_proc_init_fstack((*init_fstack));
#define font_proc_next_char_glyph(proc)\
int proc(gs_text_enum_t *pte, gs_char *pchar, gs_glyph *pglyph)
font_proc_next_char_glyph((*next_char_glyph));
#define font_proc_build_char(proc)\
int proc(gs_text_enum_t *, gs_state *, gs_font *, gs_char, gs_glyph)
font_proc_build_char((*build_char));
} gs_font_procs;
font_proc_define_font(gs_no_define_font);
font_proc_make_font(gs_no_make_font);
font_proc_make_font(gs_base_make_font);
font_proc_font_info(gs_default_font_info);
font_proc_same_font(gs_default_same_font);
font_proc_same_font(gs_base_same_font);
font_proc_encode_char(gs_no_encode_char);
font_proc_decode_glyph(gs_no_decode_glyph);
font_proc_enumerate_glyph(gs_no_enumerate_glyph);
font_proc_glyph_info(gs_default_glyph_info);
font_proc_glyph_outline(gs_no_glyph_outline);
font_proc_glyph_name(gs_no_glyph_name);
font_proc_init_fstack(gs_default_init_fstack);
font_proc_next_char_glyph(gs_default_next_char_glyph);
font_proc_build_char(gs_no_build_char);
extern const gs_font_procs gs_font_procs_default;
typedef struct gs_font_name_s {
#define gs_font_name_max 47
byte chars[gs_font_name_max + 1];
uint size;
} gs_font_name;
#define gs_font_common\
gs_font *next, *prev;		\
\
gs_memory_t *memory;		\
gs_font_dir *dir;		\
bool is_resource;\
gs_notify_list_t notify_list;	\
gs_id id;			\
gs_font *base;			\
void *client_data;		\
gs_matrix FontMatrix;\
gs_matrix orig_FontMatrix;      \
font_type FontType;\
bool BitmapWidths;\
fbit_type ExactSize, InBetweenSize, TransformedChar;\
int WMode;			\
int PaintType;			\
\
float StrokeWidth;		\
\
gs_font_procs procs;\
\
\
gs_font_name key_name, font_name
struct gs_font_s {
gs_font_common;
};
extern_st(st_gs_font);
struct_proc_finalize(gs_font_finalize);
#define public_st_gs_font()	\
gs_public_st_complex_only(st_gs_font, gs_font, "gs_font",\
0, font_enum_ptrs, font_reloc_ptrs, gs_font_finalize)
#define st_gs_font_max_ptrs (st_gs_notify_list_max_ptrs + 5)
#define private_st_gs_font_ptr()	\
gs_private_st_ptr(st_gs_font_ptr, gs_font *, "gs_font *",\
font_ptr_enum_ptrs, font_ptr_reloc_ptrs)
#define st_gs_font_ptr_max_ptrs 1
extern_st(st_gs_font_ptr_element);
#define public_st_gs_font_ptr_element()	\
gs_public_st_element(st_gs_font_ptr_element, gs_font *, "gs_font *[]",\
font_ptr_element_enum_ptrs, font_ptr_element_reloc_ptrs, st_gs_font_ptr)
gs_font *
gs_font_alloc(gs_memory_t *mem, gs_memory_type_ptr_t pstype,
const gs_font_procs *procs, gs_font_dir *dir,
client_name_t cname);
void gs_font_notify_init(gs_font *font);
int gs_font_notify_register(gs_font *font, gs_notify_proc_t proc,
void *proc_data);
int gs_font_notify_unregister(gs_font *font, gs_notify_proc_t proc,
void *proc_data);
#ifndef FAPI_server_DEFINED
#define FAPI_server_DEFINED
typedef struct FAPI_server_s FAPI_server;
#endif
#define gs_font_base_common\
gs_font_common;\
gs_rect FontBBox;\
gs_uid UID;\
FAPI_server *FAPI; \
void *FAPI_font_data; \
gs_encoding_index_t encoding_index;\
gs_encoding_index_t nearest_encoding_index  \
#ifndef gs_font_base_DEFINED
#  define gs_font_base_DEFINED
typedef struct gs_font_base_s gs_font_base;
#endif
struct gs_font_base_s {
gs_font_base_common;
};
extern_st(st_gs_font_base);
#define public_st_gs_font_base()	\
gs_public_st_suffix_add1_final(st_gs_font_base, gs_font_base,\
"gs_font_base", font_base_enum_ptrs, font_base_reloc_ptrs,\
gs_font_finalize, st_gs_font, UID.xvalues)
#define st_gs_font_base_max_ptrs (st_gs_font_max_ptrs + 1)
gs_font_base *
gs_font_base_alloc(gs_memory_t *mem, gs_memory_type_ptr_t pstype,
const gs_font_procs *procs, gs_font_dir *dir,
client_name_t cname);
extern const char gx_extendeg_glyph_name_separator[];
bool gs_font_glyph_is_notdef(gs_font_base *bfont, gs_glyph glyph);
const gs_font_base *gs_font_parent(const gs_font_base *pbfont);
#endif