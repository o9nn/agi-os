#ifndef ifapi_INCLUDED
# define ifapi_INCLUDED
#include "iplugin.h"
typedef int FracInt;
typedef int FAPI_retcode;
typedef enum {
FAPI_FONT_FEATURE_FontMatrix,
FAPI_FONT_FEATURE_UniqueID,
FAPI_FONT_FEATURE_BlueScale,
FAPI_FONT_FEATURE_Weight,
FAPI_FONT_FEATURE_ItalicAngle,
FAPI_FONT_FEATURE_IsFixedPitch,
FAPI_FONT_FEATURE_UnderLinePosition,
FAPI_FONT_FEATURE_UnderlineThickness,
FAPI_FONT_FEATURE_FontType,
FAPI_FONT_FEATURE_FontBBox,
FAPI_FONT_FEATURE_BlueValues_count,
FAPI_FONT_FEATURE_BlueValues,
FAPI_FONT_FEATURE_OtherBlues_count,
FAPI_FONT_FEATURE_OtherBlues,
FAPI_FONT_FEATURE_FamilyBlues_count,
FAPI_FONT_FEATURE_FamilyBlues,
FAPI_FONT_FEATURE_FamilyOtherBlues_count,
FAPI_FONT_FEATURE_FamilyOtherBlues,
FAPI_FONT_FEATURE_BlueShift,
FAPI_FONT_FEATURE_BlueFuzz,
FAPI_FONT_FEATURE_StdHW,
FAPI_FONT_FEATURE_StdVW,
FAPI_FONT_FEATURE_StemSnapH_count,
FAPI_FONT_FEATURE_StemSnapH,
FAPI_FONT_FEATURE_StemSnapV_count,
FAPI_FONT_FEATURE_StemSnapV,
FAPI_FONT_FEATURE_ForceBold,
FAPI_FONT_FEATURE_LanguageGroup,
FAPI_FONT_FEATURE_lenIV,
FAPI_FONT_FEATURE_Subrs_count,
FAPI_FONT_FEATURE_Subrs_total_size,
FAPI_FONT_FEATURE_TT_size
} fapi_font_feature;
typedef enum {
FAPI_METRICS_NOTDEF,
FAPI_METRICS_ADD,
FAPI_METRICS_REPLACE_WIDTH,
FAPI_METRICS_REPLACE
} FAPI_metrics_type;
typedef struct {
int char_code;
bool is_glyph_index;
const unsigned char *char_name;
unsigned int char_name_length;
FAPI_metrics_type metrics_type;
FracInt sb_x, sb_y, aw_x, aw_y;
int metrics_scale;
} FAPI_char_ref;
typedef struct FAPI_font_s FAPI_font;
struct FAPI_font_s {
void *server_font_data;
bool need_decrypt;
const gs_memory_t *memory;
const char *font_file_path;
int subfont;
bool is_type1;
bool is_cid;
bool is_mtx_skipped;
void *client_ctx_p;
void *client_font_data;
void *client_font_data2;
const void *char_data;
int char_data_len;
unsigned short (*get_word )(FAPI_font *ff, fapi_font_feature var_id, int index);
unsigned long (*get_long )(FAPI_font *ff, fapi_font_feature var_id, int index);
float (*get_float)(FAPI_font *ff, fapi_font_feature var_id, int index);
unsigned short (*get_subr) (FAPI_font *ff, int index, byte *buf, ushort buf_length);
unsigned short (*get_glyph)(FAPI_font *ff, int char_code, byte *buf, ushort buf_length);
unsigned short (*serialize_tt_font)(FAPI_font *ff, void *buf, int buf_size);
};
typedef struct FAPI_path_s FAPI_path;
struct FAPI_path_s {
void *olh;
int shift;
int (*moveto )(FAPI_path *, FracInt, FracInt);
int (*lineto )(FAPI_path *, FracInt, FracInt);
int (*curveto )(FAPI_path *, FracInt, FracInt, FracInt, FracInt, FracInt, FracInt);
int (*closepath)(FAPI_path *);
};
typedef struct FAPI_font_scale_s {
FracInt matrix[6];
FracInt HWResolution[2];
int subpixels[2];
bool align_to_pixels;
} FAPI_font_scale;
typedef struct FAPI_metrics_s {
int bbox_x0, bbox_y0, bbox_x1, bbox_y1;
int escapement;
int em_x, em_y;
} FAPI_metrics;
typedef struct {
void *p;
int width, height, line_step;
int orig_x, orig_y;
} FAPI_raster;
#ifndef FAPI_server_DEFINED
#define FAPI_server_DEFINED
typedef struct FAPI_server_s FAPI_server;
#endif
typedef int FAPI_descendant_code;
#define FAPI_DESCENDANT_PREPARED -1
#define FAPI_TOPLEVEL_PREPARED -2
#define FAPI_TOPLEVEL_BEGIN -3
#define FAPI_TOPLEVEL_COMPLETE -4
struct FAPI_server_s {
i_plugin_instance ig;
int frac_shift;
FAPI_retcode (*ensure_open)(FAPI_server *server);
FAPI_retcode (*get_scaled_font)(FAPI_server *server, FAPI_font *ff, int subfont, const FAPI_font_scale *scale, const char *xlatmap, bool bVertical, FAPI_descendant_code dc);
FAPI_retcode (*get_decodingID)(FAPI_server *server, FAPI_font *ff, const char **decodingID);
FAPI_retcode (*get_font_bbox)(FAPI_server *server, FAPI_font *ff, int BBox[4]);
FAPI_retcode (*get_font_proportional_feature)(FAPI_server *server, FAPI_font *ff, int subfont, bool *bProportional);
FAPI_retcode (*can_retrieve_char_by_name)(FAPI_server *server, FAPI_font *ff, FAPI_char_ref *c, int *result);
FAPI_retcode (*can_replace_metrics)(FAPI_server *server, FAPI_font *ff, FAPI_char_ref *c, int *result);
FAPI_retcode (*get_char_width)(FAPI_server *server, FAPI_font *ff, FAPI_char_ref *c, FAPI_metrics *metrics);
FAPI_retcode (*get_char_raster_metrics)(FAPI_server *server, FAPI_font *ff, FAPI_char_ref *c, FAPI_metrics *metrics);
FAPI_retcode (*get_char_raster)(FAPI_server *server, FAPI_raster *r);
FAPI_retcode (*get_char_outline_metrics)(FAPI_server *server, FAPI_font *ff, FAPI_char_ref *c, FAPI_metrics *metrics);
FAPI_retcode (*get_char_outline)(FAPI_server *server, FAPI_path *p);
FAPI_retcode (*release_char_data)(FAPI_server *server);
FAPI_retcode (*release_typeface)(FAPI_server *server, void *server_font_data);
};
#endif