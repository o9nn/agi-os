#include "stdio_.h"
#include "ierrors.h"
#include "ifapi.h"
#include "write_t1.h"
#include "write_t2.h"
#include "math_.h"
#include "freetype/freetype.h"
#include "freetype/ftincrem.h"
#include "freetype/ftglyph.h"
#include "freetype/ftoutln.h"
#include "freetype/fttrigon.h"
#include <assert.h>
typedef struct FF_server_
{
FAPI_server m_fapi_server;
FT_Library m_freetype_library;
FT_OutlineGlyph m_outline_glyph;
FT_BitmapGlyph m_bitmap_glyph;
} FF_server;
typedef struct FF_face_
{
FT_Face m_ft_face;
FT_Incremental_InterfaceRec* m_ft_inc_int;
unsigned char* m_font_data;
} FF_face;
typedef struct FT_IncrementalRec_
{
FAPI_font* m_fapi_font;
unsigned char* m_glyph_data;
size_t m_glyph_data_length;
bool m_glyph_data_in_use;
FT_Incremental_MetricsRec m_glyph_metrics;
unsigned long m_glyph_metrics_index;
FAPI_metrics_type m_metrics_type;
} FT_IncrementalRec;
static FF_face* new_face(FT_Face a_ft_face,FT_Incremental_InterfaceRec* a_ft_inc_int,
unsigned char* a_font_data)
{
FF_face* face = (FF_face *)malloc(sizeof(FF_face));
if (face)
{
face->m_ft_face = a_ft_face;
face->m_ft_inc_int = a_ft_inc_int;
face->m_font_data = a_font_data;
}
return face;
}
static void delete_face(FF_face* a_face)
{
if (a_face)
{
FT_Done_Face(a_face->m_ft_face);
free(a_face->m_ft_inc_int);
free(a_face->m_font_data);
free(a_face);
}
}
static FT_IncrementalRec* new_inc_int_info(FAPI_font* a_fapi_font)
{
FT_IncrementalRec* info = (FT_IncrementalRec*)malloc(sizeof(FT_IncrementalRec));
if (info)
{
info->m_fapi_font = a_fapi_font;
info->m_glyph_data = NULL;
info->m_glyph_data_length = 0;
info->m_glyph_data_in_use = false;
info->m_glyph_metrics_index = 0xFFFFFFFF;
info->m_metrics_type = FAPI_METRICS_NOTDEF;
}
return info;
}
static void delete_inc_int_info(FT_IncrementalRec* a_inc_int_info)
{
if (a_inc_int_info)
{
free(a_inc_int_info->m_glyph_data);
free(a_inc_int_info);
}
}
static FT_Error get_fapi_glyph_data(FT_Incremental a_info,FT_UInt a_index,FT_Data* a_data)
{
FAPI_font* ff = a_info->m_fapi_font;
ushort length = 0;
ff->need_decrypt = true;
if (a_info->m_glyph_data_in_use)
{
unsigned char* buffer = NULL;
length = ff->get_glyph(ff,a_index,NULL,0);
buffer = malloc(length);
if (!buffer)
return FT_Err_Out_Of_Memory;
ff->get_glyph(ff,a_index,buffer,length);
a_data->pointer = buffer;
}
else
{
const void* saved_char_data = ff->char_data;
length = ff->get_glyph(ff,a_index,a_info->m_glyph_data,(ushort)a_info->m_glyph_data_length);
if (length > a_info->m_glyph_data_length)
{
a_info->m_glyph_data = realloc(a_info->m_glyph_data,length);
if (!a_info->m_glyph_data)
{
a_info->m_glyph_data_length = 0;
return FT_Err_Out_Of_Memory;
}
a_info->m_glyph_data_length = length;
ff->char_data = saved_char_data;
ff->get_glyph(ff,a_index,a_info->m_glyph_data,length);
}
a_data->pointer = a_info->m_glyph_data;
a_info->m_glyph_data_in_use = true;
}
a_data->length = length;
return 0;
}
static void free_fapi_glyph_data(FT_Incremental a_info,FT_Data* a_data)
{
if (a_data->pointer == (const FT_Byte*)a_info->m_glyph_data)
a_info->m_glyph_data_in_use = false;
else
free((FT_Byte*)a_data->pointer);
}
static FT_Error get_fapi_glyph_metrics(FT_Incremental a_info,FT_UInt a_glyph_index,
FT_Bool a_vertical,FT_Incremental_MetricsRec* a_metrics)
{
if (a_info->m_glyph_metrics_index == a_glyph_index)
{
switch (a_info->m_metrics_type)
{
case FAPI_METRICS_ADD:
a_metrics->advance += a_info->m_glyph_metrics.advance;
break;
case FAPI_METRICS_REPLACE_WIDTH:
a_metrics->advance = a_info->m_glyph_metrics.advance;
break;
case FAPI_METRICS_REPLACE:
*a_metrics = a_info->m_glyph_metrics;
break;
default:
assert(false);
break;
}
}
return 0;
}
static const FT_Incremental_FuncsRec TheFAPIIncrementalInterfaceFuncs =
{
get_fapi_glyph_data,
free_fapi_glyph_data,
get_fapi_glyph_metrics
};
static FT_Incremental_InterfaceRec* new_inc_int(FAPI_font* a_fapi_font)
{
FT_Incremental_InterfaceRec* i = (FT_Incremental_InterfaceRec*)malloc(sizeof(FT_Incremental_InterfaceRec));
if (i)
{
i->object = (FT_Incremental)new_inc_int_info(a_fapi_font);
i->funcs = &TheFAPIIncrementalInterfaceFuncs;
}
if (!i->object)
{
free(i);
i = NULL;
}
return i;
}
static void delete_inc_int(FT_Incremental_InterfaceRec* a_inc_int)
{
if (a_inc_int)
{
delete_inc_int_info(a_inc_int->object);
free(a_inc_int);
}
}
static int ft_to_gs_error(FT_Error a_error)
{
if (a_error)
{
if (a_error == FT_Err_Out_Of_Memory)
return e_VMerror;
else
return e_unknownerror;
}
return 0;
}
static FAPI_retcode load_glyph(FAPI_font* a_fapi_font,const FAPI_char_ref *a_char_ref,
FAPI_metrics* a_metrics,FT_Glyph* a_glyph,bool a_bitmap)
{
FT_Error ft_error = 0;
FF_face* face = (FF_face*)a_fapi_font->server_font_data;
FT_Face ft_face = face->m_ft_face;
int index = a_char_ref->char_code;
const void* saved_char_data = a_fapi_font->char_data;
if (!a_char_ref->is_glyph_index)
{
if (ft_face->num_charmaps)
index = FT_Get_Char_Index(ft_face,index);
else
{
if (a_fapi_font->is_type1)
index = 0;
else
index = a_char_ref->char_code;
}
}
if (face->m_ft_inc_int)
face->m_ft_inc_int->object->m_fapi_font = a_fapi_font;
if (face->m_ft_inc_int && a_char_ref->metrics_type != FAPI_METRICS_NOTDEF)
{
FT_Incremental_MetricsRec* m = &face->m_ft_inc_int->object->m_glyph_metrics;
m->bearing_x = a_char_ref->sb_x >> 16;
m->bearing_y = a_char_ref->sb_y >> 16;
m->advance = a_char_ref->aw_x >> 16;
face->m_ft_inc_int->object->m_glyph_metrics_index = index;
face->m_ft_inc_int->object->m_metrics_type = a_char_ref->metrics_type;
}
ft_error = FT_Load_Glyph(ft_face,index,FT_LOAD_MONOCHROME | FT_LOAD_NO_SCALE);
if (!ft_error && a_metrics)
{
a_metrics->bbox_x0 = ft_face->glyph->metrics.horiBearingX;
a_metrics->bbox_y0 = ft_face->glyph->metrics.horiBearingY - ft_face->glyph->metrics.height;
a_metrics->bbox_x1 = a_metrics->bbox_x0 + ft_face->glyph->metrics.width;
a_metrics->bbox_y1 = a_metrics->bbox_y0 + ft_face->glyph->metrics.height;
a_metrics->escapement = ft_face->glyph->metrics.horiAdvance;
a_metrics->em_x = ft_face->units_per_EM;
a_metrics->em_y = ft_face->units_per_EM;
}
if (!ft_error)
{
a_fapi_font->char_data = saved_char_data;
ft_error = FT_Load_Glyph(ft_face,index,a_bitmap ? FT_LOAD_MONOCHROME | FT_LOAD_RENDER: FT_LOAD_MONOCHROME);
}
if (!ft_error && a_glyph)
ft_error = FT_Get_Glyph(ft_face->glyph,a_glyph);
return ft_to_gs_error(ft_error);
}
static FAPI_retcode ensure_open(FAPI_server* a_server)
{
FF_server* s = (FF_server*)a_server;
if (!s->m_freetype_library)
{
FT_Error ft_error = FT_Init_FreeType(&s->m_freetype_library);
if (ft_error)
return ft_to_gs_error(ft_error);
}
return 0;
}
static void transform_concat(FT_Matrix* a_A,const FT_Matrix* a_B)
{
FT_Matrix result = *a_B;
FT_Matrix_Multiply(a_A,&result);
*a_A = result;
}
static void make_rotation(FT_Matrix* a_transform,const FT_Vector* a_vector)
{
FT_Fixed length, cos, sin;
if (a_vector->x >= 0 && a_vector->y == 0)
{
a_transform->xx = a_transform->yy = 65536;
a_transform->xy = a_transform->yx = 0;
return;
}
length = FT_Vector_Length((FT_Vector*)a_vector);
cos = FT_DivFix(a_vector->x,length);
sin = FT_DivFix(a_vector->y,length);
a_transform->xx = a_transform->yy = cos;
a_transform->xy = -sin;
a_transform->yx = sin;
}
static void transform_decompose(FT_Matrix* a_transform,
FT_Fixed* a_x_scale,FT_Fixed* a_y_scale)
{
FT_Matrix rotation;
bool have_rotation = false;
FT_Vector v;
v.x = a_transform->xx;
v.y = -a_transform->yx;
if (v.y || v.x < 0)
{
have_rotation = true;
make_rotation(&rotation,&v);
transform_concat(a_transform,&rotation);
}
*a_x_scale = a_transform->xx;
if (*a_x_scale < 0)
{
*a_x_scale = -*a_x_scale;
a_transform->xx = -65536;
}
else
a_transform->xx = 65536;
*a_y_scale = a_transform->yy;
if (*a_y_scale < 0)
{
*a_y_scale = -*a_y_scale;
a_transform->yy = -65536;
}
else
a_transform->yy = 65536;
a_transform->yx = FT_DivFix(a_transform->yx,*a_x_scale);
a_transform->xy = FT_DivFix(a_transform->xy,*a_y_scale);
if (have_rotation)
{
rotation.xy = -rotation.xy;
rotation.yx = -rotation.yx;
transform_concat(a_transform,&rotation);
}
}
static FAPI_retcode get_scaled_font(FAPI_server* a_server,FAPI_font* a_font,int a_subfont,
const FAPI_font_scale* a_font_scale,
const char* a_map,bool a_vertical,
FAPI_descendant_code a_descendant_code)
{
FF_server* s = (FF_server*)a_server;
FF_face* face = (FF_face*)a_font->server_font_data;
FT_Error ft_error = 0;
if (a_font->is_cid && a_font->is_type1 && a_font->font_file_path == NULL &&
(a_descendant_code == FAPI_TOPLEVEL_BEGIN ||
a_descendant_code == FAPI_TOPLEVEL_COMPLETE))
{
return 0;
}
if (!face)
{
FT_Face ft_face = NULL;
FT_Parameter ft_param;
FT_Incremental_InterfaceRec* ft_inc_int = NULL;
unsigned char* own_font_data = NULL;
if (a_font->font_file_path)
{
ft_error = FT_New_Face(s->m_freetype_library,a_font->font_file_path,a_subfont,&ft_face);
if (!ft_error && ft_face)
ft_error = FT_Select_Charmap(ft_face,ft_encoding_unicode);
}
else
{
FT_Open_Args open_args;
open_args.flags = FT_OPEN_MEMORY;
if (a_font->is_type1)
{
long length;
int type = a_font->get_word(a_font,FAPI_FONT_FEATURE_FontType,0);
a_font->need_decrypt = true;
if (type == 1)
length = FF_serialize_type1_font(a_font,NULL,0);
else
length = FF_serialize_type2_font(a_font,NULL,0);
open_args.memory_base = own_font_data = malloc(length);
if (!open_args.memory_base)
return e_VMerror;
if (type == 1)
open_args.memory_size = FF_serialize_type1_font(a_font,own_font_data,length);
else
open_args.memory_size = FF_serialize_type2_font(a_font,own_font_data,length);
assert(open_args.memory_size == length);
ft_inc_int = new_inc_int(a_font);
if (!ft_inc_int)
{
free(own_font_data);
return e_VMerror;
}
}
else
{
open_args.memory_size = a_font->get_long(a_font,FAPI_FONT_FEATURE_TT_size,0);
if (open_args.memory_size == 0)
return e_invalidfont;
open_args.memory_base = own_font_data = malloc(open_args.memory_size);
if (!own_font_data)
return e_VMerror;
if (a_font->serialize_tt_font(a_font,own_font_data,open_args.memory_size))
return e_invalidfont;
ft_inc_int = new_inc_int(a_font);
if (!ft_inc_int)
{
free(own_font_data);
return e_VMerror;
}
}
if (ft_inc_int)
{
open_args.flags = (FT_UInt)(open_args.flags | FT_OPEN_PARAMS);
ft_param.tag = FT_PARAM_TAG_INCREMENTAL;
ft_param.data = ft_inc_int;
open_args.num_params = 1;
open_args.params = &ft_param;
}
ft_error = FT_Open_Face(s->m_freetype_library,&open_args,a_subfont,&ft_face);
}
if (ft_face)
{
face = new_face(ft_face,ft_inc_int,own_font_data);
if (!face)
{
free(own_font_data);
FT_Done_Face(ft_face);
delete_inc_int(ft_inc_int);
return e_VMerror;
}
a_font->server_font_data = face;
}
else
a_font->server_font_data = NULL;
}
if (face)
{
static const FT_Matrix ft_reflection = { 65536, 0, 0, -65536 };
FT_Matrix ft_transform;
FT_F26Dot6 width, height;
ft_transform.xx = a_font_scale->matrix[0];
ft_transform.xy = a_font_scale->matrix[1];
ft_transform.yx = -a_font_scale->matrix[2];
ft_transform.yy = -a_font_scale->matrix[3];
transform_decompose(&ft_transform,&width,&height);
width >>= 10;
height >>= 10;
ft_error = FT_Set_Char_Size(face->m_ft_face,width,height,
a_font_scale->HWResolution[0] >> 16,
a_font_scale->HWResolution[1] >> 16);
if (ft_error)
{
delete_face(face);
a_font->server_font_data = NULL;
return ft_to_gs_error(ft_error);
}
FT_Matrix_Multiply(&ft_reflection,&ft_transform);
FT_Set_Transform(face->m_ft_face,&ft_transform,NULL);
}
return a_font->server_font_data ? 0 : -1;
}
static FAPI_retcode get_decodingID(FAPI_server* a_server,FAPI_font* a_font,const char** a_decoding_id)
{
*a_decoding_id = "Unicode";
return 0;
}
static FAPI_retcode get_font_bbox(FAPI_server* a_server,FAPI_font* a_font,int a_box[4])
{
FF_face* face = (FF_face*)a_font->server_font_data;
a_box[0] = face->m_ft_face->bbox.xMin;
a_box[1] = face->m_ft_face->bbox.yMin;
a_box[2] = face->m_ft_face->bbox.xMax;
a_box[3] = face->m_ft_face->bbox.yMax;
return 0;
}
static FAPI_retcode get_font_proportional_feature(FAPI_server* a_server,FAPI_font* a_font,int a_subfont,
bool* a_proportional)
{
*a_proportional = true;
return 0;
}
static FAPI_retcode can_retrieve_char_by_name(FAPI_server* a_server,FAPI_font* a_font,FAPI_char_ref* a_char_ref,
bool* a_result)
{
FF_face* face = (FF_face*)a_font->server_font_data;
char name[128];
if (FT_HAS_GLYPH_NAMES(face->m_ft_face) && a_char_ref->char_name_length < sizeof(name))
{
memcpy(name,a_char_ref->char_name,a_char_ref->char_name_length);
name[a_char_ref->char_name_length] = 0;
a_char_ref->char_code = FT_Get_Name_Index(face->m_ft_face,name);
*a_result = a_char_ref->char_code != 0;
if (*a_result)
a_char_ref->is_glyph_index = true;
}
else
*a_result = false;
return 0;
}
static FAPI_retcode can_replace_metrics(FAPI_server *a_server,FAPI_font *a_font,FAPI_char_ref *a_char_ref,int *a_result)
{
*a_result = a_char_ref->metrics_scale == 0;
return 0;
}
static FAPI_retcode get_char_width(FAPI_server *a_server,FAPI_font *a_font,FAPI_char_ref *a_char_ref,
FAPI_metrics *a_metrics)
{
return load_glyph(a_font,a_char_ref,a_metrics,NULL,false);
}
static FAPI_retcode get_char_raster_metrics(FAPI_server* a_server,FAPI_font* a_font,FAPI_char_ref* a_char_ref,
FAPI_metrics* a_metrics)
{
FF_server* s = (FF_server*)a_server;
FAPI_retcode error = load_glyph(a_font,a_char_ref,a_metrics,(FT_Glyph*)&s->m_bitmap_glyph,true);
return error;
}
static FAPI_retcode get_char_raster(FAPI_server *a_server,FAPI_raster *a_raster)
{
FF_server* s = (FF_server*)a_server;
assert(s->m_bitmap_glyph);
a_raster->p = s->m_bitmap_glyph->bitmap.buffer;
a_raster->width = s->m_bitmap_glyph->bitmap.width;
a_raster->height = s->m_bitmap_glyph->bitmap.rows;
a_raster->line_step = s->m_bitmap_glyph->bitmap.pitch;
a_raster->orig_x = s->m_bitmap_glyph->left * 16;
a_raster->orig_y = s->m_bitmap_glyph->top * 16;
return 0;
}
static FAPI_retcode get_char_outline_metrics(FAPI_server *a_server,FAPI_font *a_font,FAPI_char_ref *a_char_ref,
FAPI_metrics *a_metrics)
{
FF_server* s = (FF_server*)a_server;
return load_glyph(a_font,a_char_ref,a_metrics,(FT_Glyph*)&s->m_outline_glyph,false);
}
typedef struct FF_path_info_
{
FAPI_path* m_path;
FracInt m_x;
FracInt m_y;
} FF_path_info;
static int move_to(FT_Vector* aTo,void* aObject)
{
FF_path_info* p = (FF_path_info*)aObject;
p->m_x = aTo->x;
p->m_y = aTo->y;
return p->m_path->moveto(p->m_path,aTo->x,aTo->y) ? -1 : 0;
}
static int line_to(FT_Vector* aTo,void* aObject)
{
FF_path_info* p = (FF_path_info*)aObject;
p->m_x = aTo->x;
p->m_y = aTo->y;
return p->m_path->lineto(p->m_path,aTo->x,aTo->y) ? -1 : 0;
}
static int conic_to(FT_Vector* aControl,FT_Vector* aTo,void* aObject)
{
FF_path_info* p = (FF_path_info*)aObject;
p->m_x = aTo->x;
p->m_y = aTo->y;
return p->m_path->curveto(p->m_path,(p->m_x + aControl->x * 2) / 3,
(p->m_y + aControl->y * 2) / 3,
(aTo->x + aControl->x * 2) / 3,
(aTo->y + aControl->y * 2) / 3,
aTo->x,aTo->y) ? -1 : 0;
}
static int cubic_to(FT_Vector* aControl1,FT_Vector* aControl2,FT_Vector* aTo,void* aObject)
{
FF_path_info* p = (FF_path_info*)aObject;
p->m_x = aTo->x;
p->m_y = aTo->y;
return p->m_path->curveto(p->m_path,aControl1->x,aControl1->y,aControl2->x,aControl2->y,aTo->x,aTo->y) ? -1 : 0;
}
static const FT_Outline_Funcs TheFtOutlineFuncs =
{
move_to,
line_to,
conic_to,
cubic_to,
0,
0
};
static FAPI_retcode get_char_outline(FAPI_server *a_server,FAPI_path *a_path)
{
FF_server* s = (FF_server*)a_server;
FF_path_info p;
FT_Error ft_error = 0;
p.m_path = a_path;
p.m_x = 0;
p.m_y = 0;
ft_error = FT_Outline_Decompose(&s->m_outline_glyph->outline,&TheFtOutlineFuncs,&p);
a_path->closepath(a_path);
return ft_to_gs_error(ft_error);
}
static FAPI_retcode release_char_data(FAPI_server *a_server)
{
FF_server* s = (FF_server*)a_server;
FT_Done_Glyph(&s->m_outline_glyph->root);
FT_Done_Glyph(&s->m_bitmap_glyph->root);
s->m_outline_glyph = NULL;
s->m_bitmap_glyph = NULL;
return 0;
}
static FAPI_retcode release_typeface(FAPI_server* a_server,void* a_server_font_data)
{
FF_face* face = (FF_face*)a_server_font_data;
delete_face(face);
return 0;
}
static void gs_freetype_destroy(i_plugin_instance* a_instance,i_plugin_client_memory* a_memory);
static const i_plugin_descriptor TheFreeTypeDescriptor =
{
"FAPI",
"FreeType",
gs_freetype_destroy
};
static const FAPI_server TheFreeTypeServer =
{
{ &TheFreeTypeDescriptor },
16,
ensure_open,
get_scaled_font,
get_decodingID,
get_font_bbox,
get_font_proportional_feature,
can_retrieve_char_by_name,
can_replace_metrics,
get_char_width,
get_char_raster_metrics,
get_char_raster,
get_char_outline_metrics,
get_char_outline,
release_char_data,
release_typeface
};
plugin_instantiation_proc(gs_fapi_ft_instantiate);
int gs_fapi_ft_instantiate(i_ctx_t *a_context,
i_plugin_client_memory *a_memory,
i_plugin_instance **a_plugin_instance)
{
FF_server *server = (FF_server *)a_memory->alloc(a_memory,
sizeof(FF_server),"FF_server");
if (!server)
return e_VMerror;
memset(server,0,sizeof(*server));
server->m_fapi_server = TheFreeTypeServer;
*a_plugin_instance = &server->m_fapi_server.ig;
return 0;
}
static void gs_freetype_destroy(i_plugin_instance *a_plugin_instance,i_plugin_client_memory *a_memory)
{
FF_server *server = (FF_server *)a_plugin_instance;
assert(server->m_fapi_server.ig.d == &TheFreeTypeDescriptor);
FT_Done_Glyph(&server->m_outline_glyph->root);
FT_Done_Glyph(&server->m_bitmap_glyph->root);
FT_Done_FreeType(server->m_freetype_library);
a_memory->free(a_memory,server,"FF_server");
}