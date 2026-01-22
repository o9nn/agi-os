#ifndef FREETYPE_H
#define FREETYPE_H
#ifdef __cplusplus
extern "C" {
#endif
#if ARCH_LOG2_SIZEOF_LONG == 2
typedef signed long TT_Fixed;
#elif ARCH_LOG2_SIZEOF_INT == 2
typedef signed int TT_Fixed;
#else
#error "No appropriate type for Fixed 16.16 Floats"
#endif
typedef signed short TT_FWord;
typedef unsigned short TT_UFWord;
typedef signed short TT_Short;
typedef unsigned short TT_UShort;
typedef signed long TT_Long;
typedef unsigned long TT_ULong;
typedef signed short TT_F2Dot14;
#if ARCH_LOG2_SIZEOF_LONG == 2
typedef signed long TT_F26Dot6;
#elif ARCH_LOG2_SIZEOF_INT == 2
typedef signed int TT_F26Dot6;
#else
#error "No appropriate type for Fixed 26.6 Floats"
#endif
#if ARCH_LOG2_SIZEOF_LONG == 2
typedef signed long TT_Pos;
#elif ARCH_LOG2_SIZEOF_INT == 2
typedef signed int TT_Pos;
#else
#error "No appropriate type for point position"
#endif
struct _TT_UnitVector
{
TT_F2Dot14 x;
TT_F2Dot14 y;
};
typedef struct _TT_UnitVector TT_UnitVector;
struct _TT_Vector
{
TT_F26Dot6 x;
TT_F26Dot6 y;
};
typedef struct _TT_Vector TT_Vector;
struct _TT_Matrix
{
TT_Fixed xx, xy;
TT_Fixed yx, yy;
};
typedef struct _TT_Matrix TT_Matrix;
struct _TT_Outline
{
unsigned int contours;
unsigned int points;
unsigned short* conEnds;
TT_Pos* xCoord;
TT_Pos* yCoord;
unsigned char* flag;
int owner;
int high_precision;
int second_pass;
char dropout_mode;
};
typedef struct _TT_Outline TT_Outline;
struct _TT_BBox
{
TT_Pos xMin;
TT_Pos yMin;
TT_Pos xMax;
TT_Pos yMax;
};
typedef struct _TT_BBox TT_BBox;
struct _TT_Glyph_Metrics
{
TT_BBox bbox;
TT_Pos bearingX;
TT_Pos bearingY;
TT_Pos advance;
};
struct _TT_Big_Glyph_Metrics
{
TT_BBox bbox;
TT_Pos horiBearingX;
TT_Pos horiBearingY;
TT_Pos vertBearingX;
TT_Pos vertBearingY;
TT_Pos horiAdvance;
TT_Pos vertAdvance;
};
typedef struct _TT_Glyph_Metrics TT_Glyph_Metrics;
typedef struct _TT_Big_Glyph_Metrics TT_Big_Glyph_Metrics;
struct _TT_Instance_Metrics
{
int pointSize;
int x_ppem;
int y_ppem;
TT_Fixed x_scale;
TT_Fixed y_scale;
int x_resolution;
int y_resolution;
};
typedef struct _TT_Instance_Metrics TT_Instance_Metrics;
#define TT_Flow_Down -1
#define TT_Flow_Up 1
#define TT_Flow_Error 0
struct _TT_Raster_Map
{
int rows;
int cols;
int width;
int flow;
void* bitmap;
long size;
};
typedef struct _TT_Raster_Map TT_Raster_Map;
struct _TT_Header
{
TT_Fixed Table_Version;
TT_Fixed Font_Revision;
TT_Long CheckSum_Adjust;
TT_Long Magic_Number;
TT_UShort Flags;
TT_UShort Units_Per_EM;
TT_Long Created [2];
TT_Long Modified[2];
TT_FWord xMin;
TT_FWord yMin;
TT_FWord xMax;
TT_FWord yMax;
TT_UShort Mac_Style;
TT_UShort Lowest_Rec_PPEM;
TT_Short Font_Direction;
TT_Short Index_To_Loc_Format;
TT_Short Glyph_Data_Format;
};
typedef struct _TT_Header TT_Header;
struct _TT_Horizontal_Header
{
TT_Fixed Version;
TT_FWord Ascender;
TT_FWord Descender;
TT_FWord Line_Gap;
TT_UFWord advance_Width_Max;
TT_FWord min_Left_Side_Bearing;
TT_FWord min_Right_Side_Bearing;
TT_FWord xMax_Extent;
TT_FWord caret_Slope_Rise;
TT_FWord caret_Slope_Run;
TT_Short Reserved[5];
TT_Short metric_Data_Format;
TT_UShort number_Of_HMetrics;
};
typedef struct _TT_Horizontal_Header TT_Horizontal_Header;
struct _TT_OS2
{
TT_UShort version;
TT_FWord xAvgCharWidth;
TT_UShort usWeightClass;
TT_UShort usWidthClass;
TT_Short fsType;
TT_FWord ySubscriptXSize;
TT_FWord ySubscriptYSize;
TT_FWord ySubscriptXOffset;
TT_FWord ySubscriptYOffset;
TT_FWord ySuperscriptXSize;
TT_FWord ySuperscriptYSize;
TT_FWord ySuperscriptXOffset;
TT_FWord ySuperscriptYOffset;
TT_FWord yStrikeoutSize;
TT_FWord yStrikeoutPosition;
TT_Short sFamilyClass;
char panose[10];
TT_ULong ulUnicodeRange1;
TT_ULong ulUnicodeRange2;
TT_ULong ulUnicodeRange3;
TT_ULong ulUnicodeRange4;
char achVendID[4];
TT_UShort fsSelection;
TT_UShort usFirstCharIndex;
TT_UShort usLastCharIndex;
TT_UShort sTypoAscender;
TT_UShort sTypoDescender;
TT_UShort sTypoLineGap;
TT_UShort usWinAscent;
TT_UShort usWinDescent;
TT_ULong ulCodePageRange1;
TT_ULong ulCodePageRange2;
};
typedef struct _TT_OS2 TT_OS2;
struct _TT_Postscript
{
TT_Fixed FormatType;
TT_Fixed italicAngle;
TT_FWord underlinePosition;
TT_FWord underlineThickness;
TT_ULong isFixedPitch;
TT_ULong minMemType42;
TT_ULong maxMemType42;
TT_ULong minMemType1;
TT_ULong maxMemType1;
};
typedef struct _TT_Postscript TT_Postscript;
struct _TT_Hdmx_Record
{
unsigned char ppem;
unsigned char max_width;
unsigned char* widths;
};
typedef struct _TT_Hdmx_Record TT_Hdmx_Record;
struct _TT_Hdmx
{
TT_UShort version;
TT_Short num_records;
TT_Hdmx_Record* records;
};
typedef struct _TT_Hdmx TT_Hdmx;
struct _TT_Face_Properties
{
int num_Glyphs;
int max_Points;
int max_Contours;
int num_Faces;
TT_Header* header;
TT_Horizontal_Header* horizontal;
TT_OS2* os2;
TT_Postscript* postscript;
TT_Hdmx* hdmx;
};
typedef struct _TT_Face_Properties TT_Face_Properties;
struct _TT_Engine { void* z; };
struct _TT_Stream { void* z; };
struct _TT_Face { void* z; };
struct _TT_Instance { void* z; };
struct _TT_Glyph { void* z; };
struct _TT_CharMap { void* z; };
typedef struct _TT_Engine TT_Engine;
typedef struct _TT_Stream TT_Stream;
typedef struct _TT_Face TT_Face;
typedef struct _TT_Instance TT_Instance;
typedef struct _TT_Glyph TT_Glyph;
typedef struct _TT_CharMap TT_CharMap;
typedef int TT_Error;
extern const TT_Instance TT_Null_Instance;
TT_Error TT_Init_FreeType( TT_Engine* engine );
TT_Error TT_Done_FreeType( TT_Engine engine );
TT_Error TT_Set_Raster_Gray_Palette( TT_Engine engine, char* palette );
TT_Error TT_Open_Face( TT_Engine engine,
const char* fontpathname,
TT_Face* face );
TT_Error TT_Open_Collection( TT_Engine engine,
const char* collectionpathname,
int fontIndex,
TT_Face* face );
TT_Error TT_Get_Face_Properties( TT_Face face,
TT_Face_Properties* properties );
TT_Error TT_Set_Face_Pointer( TT_Face face,
void* data );
void* TT_Get_Face_Pointer( TT_Face face );
TT_Error TT_Flush_Face( TT_Face face );
TT_Error TT_Close_Face( TT_Face face );
TT_Error TT_Get_Font_Data( TT_Face face,
long tag,
long offset,
void* buffer,
long* length );
# define MAKE_TT_TAG( _x1, _x2, _x3, _x4 ) \
(_x1 << 24 | _x2 << 16 | _x3 << 8 | _x4)
TT_Error TT_New_Instance( TT_Face face,
TT_Instance* instance );
TT_Error TT_Set_Instance_Resolutions( TT_Instance instance,
int x_resolution,
int y_resolution );
TT_Error TT_Set_Instance_CharSize( TT_Instance instance,
TT_F26Dot6 charSize );
TT_Error TT_Set_Instance_CharSizes( TT_Instance instance,
TT_F26Dot6 charWidth,
TT_F26Dot6 charHeight );
#define TT_Set_Instance_PointSize( ins, ptsize ) \
TT_Set_Instance_CharSize( ins, ptsize*64 )
TT_Error TT_Set_Instance_PixelSizes( TT_Instance instance,
int pixelWidth,
int pixelHeight,
TT_F26Dot6 pointSize );
TT_Error TT_Set_Instance_Transform_Flags( TT_Instance instance,
int rotated,
int stretched );
TT_Error TT_Get_Instance_Metrics( TT_Instance instance,
TT_Instance_Metrics* metrics );
TT_Error TT_Set_Instance_Pointer( TT_Instance instance,
void* data );
void* TT_Get_Instance_Pointer( TT_Instance instance );
TT_Error TT_Done_Instance( TT_Instance instance );
TT_Error TT_New_Glyph( TT_Face face,
TT_Glyph* glyph );
TT_Error TT_Done_Glyph( TT_Glyph glyph );
#define TTLOAD_SCALE_GLYPH 1
#define TTLOAD_HINT_GLYPH 2
#define TTLOAD_DEFAULT (TTLOAD_SCALE_GLYPH | TTLOAD_HINT_GLYPH)
TT_Error TT_Load_Glyph( TT_Instance instance,
TT_Glyph glyph,
int glyph_index,
int load_flags );
TT_Error TT_Get_Glyph_Outline( TT_Glyph glyph,
TT_Outline* outline );
TT_Error TT_Get_Glyph_Metrics( TT_Glyph glyph,
TT_Glyph_Metrics* metrics );
TT_Error TT_Get_Glyph_Bitmap( TT_Glyph glyph,
TT_Raster_Map* raster_map,
TT_F26Dot6 x_offset,
TT_F26Dot6 y_offset );
TT_Error TT_Get_Glyph_Pixmap( TT_Glyph glyph,
TT_Raster_Map* raster_map,
TT_F26Dot6 x_offset,
TT_F26Dot6 y_offset );
TT_Error TT_New_Outline( int num_points,
int num_contours,
TT_Outline* outline );
TT_Error TT_Done_Outline( TT_Outline* outline );
TT_Error TT_Copy_Outline( TT_Outline* source,
TT_Outline* target );
TT_Error TT_Get_Outline_Bitmap( TT_Engine engine,
TT_Outline* outline,
TT_Raster_Map* raster_map );
TT_Error TT_Get_Outline_Pixmap( TT_Engine engine,
TT_Outline* outline,
TT_Raster_Map* raster_map );
TT_Error TT_Get_Outline_BBox( TT_Outline* outline,
TT_BBox* bbox );
void TT_Transform_Outline( TT_Outline* outline,
TT_Matrix* matrix );
# define TT_Appy_Outline_Matrix TT_Transform_Matrix;
void TT_Translate_Outline( TT_Outline* outline,
TT_F26Dot6 x_offset,
TT_F26Dot6 y_offset );
# define TT_Apply_Outline_Translation TT_Translate_Outline
void TT_Transform_Vector( TT_F26Dot6* x,
TT_F26Dot6* y,
TT_Matrix* matrix );
# define TT_Apply_Vector_Matrix( x, y, m ) \
TT_Transform_Vector( x, y, m )
void TT_Matrix_Multiply( TT_Matrix* a,
TT_Matrix* b );
TT_Error TT_Matrix_Invert( TT_Matrix* matrix );
int TT_Get_CharMap_Count( TT_Face face );
TT_Error TT_Get_CharMap_ID( TT_Face face,
int charmapIndex,
short* platformID,
short* encodingID );
TT_Error TT_Get_CharMap( TT_Face face,
int charmapIndex,
TT_CharMap* charMap );
int TT_Char_Index( TT_CharMap charMap,
unsigned short charCode );
int TT_Get_Name_Count( TT_Face face );
TT_Error TT_Get_Name_ID( TT_Face face,
int nameIndex,
short* platformID,
short* encodingID,
short* languageID,
short* nameID );
TT_Error TT_Get_Name_String( TT_Face face,
int nameIndex,
char** stringPtr,
int* length );
#define TT_Callback_Glyph_Outline_Load 0
typedef int (*TT_Glyph_Loader_Callback)( void* instance_ptr,
int glyph_index,
TT_Outline* outline,
TT_F26Dot6* lsb,
TT_F26Dot6* aw );
TT_Error TT_Register_Callback( TT_Engine engine,
int callback_id,
void* callback_ptr );
#define TT_Err_Ok 0
#define TT_Err_Invalid_Face_Handle 0x001
#define TT_Err_Invalid_Instance_Handle 0x002
#define TT_Err_Invalid_Glyph_Handle 0x003
#define TT_Err_Invalid_CharMap_Handle 0x004
#define TT_Err_Invalid_Result_Address 0x005
#define TT_Err_Invalid_Glyph_Index 0x006
#define TT_Err_Invalid_Argument 0x007
#define TT_Err_Could_Not_Open_File 0x008
#define TT_Err_File_Is_Not_Collection 0x009
#define TT_Err_Table_Missing 0x00A
#define TT_Err_Invalid_Horiz_Metrics 0x00B
#define TT_Err_Invalid_CharMap_Format 0x00C
#define TT_Err_Invalid_PPem 0x00D
#define TT_Err_Invalid_File_Format 0x010
#define TT_Err_Invalid_Engine 0x020
#define TT_Err_Too_Many_Extensions 0x021
#define TT_Err_Extensions_Unsupported 0x022
#define TT_Err_Invalid_Extension_Id 0x023
#define TT_Err_Max_Profile_Missing 0x080
#define TT_Err_Header_Table_Missing 0x081
#define TT_Err_Horiz_Header_Missing 0x082
#define TT_Err_Locations_Missing 0x083
#define TT_Err_Name_Table_Missing 0x084
#define TT_Err_CMap_Table_Missing 0x085
#define TT_Err_Hmtx_Table_Missing 0x086
#define TT_Err_OS2_Table_Missing 0x087
#define TT_Err_Post_Table_Missing 0x088
#define TT_Err_Out_Of_Memory 0x100
#define TT_Err_Invalid_File_Offset 0x200
#define TT_Err_Invalid_File_Read 0x201
#define TT_Err_Invalid_Frame_Access 0x202
#define TT_Err_Too_Many_Points 0x300
#define TT_Err_Too_Many_Contours 0x301
#define TT_Err_Invalid_Composite 0x302
#define TT_Err_Too_Many_Ins 0x303
#define TT_Err_Invalid_Opcode 0x400
#define TT_Err_Too_Few_Arguments 0x401
#define TT_Err_Stack_Overflow 0x402
#define TT_Err_Code_Overflow 0x403
#define TT_Err_Bad_Argument 0x404
#define TT_Err_Divide_By_Zero 0x405
#define TT_Err_Storage_Overflow 0x406
#define TT_Err_Cvt_Overflow 0x407
#define TT_Err_Invalid_Reference 0x408
#define TT_Err_Invalid_Distance 0x409
#define TT_Err_Interpolate_Twilight 0x40A
#define TT_Err_Debug_OpCode 0x40B
#define TT_Err_ENDF_In_Exec_Stream 0x40C
#define TT_Err_Out_Of_CodeRanges 0x40D
#define TT_Err_Nested_DEFS 0x40E
#define TT_Err_Invalid_CodeRange 0x40F
#define TT_Err_Invalid_Displacement 0x410
#define TT_Err_Nested_Frame_Access 0x500
#define TT_Err_Invalid_Cache_List 0x501
#define TT_Err_Could_Not_Find_Context 0x502
#define TT_Err_Unlisted_Object 0x503
#define TT_Err_Raster_Pool_Overflow 0x600
#define TT_Err_Raster_Negative_Height 0x601
#define TT_Err_Raster_Invalid_Value 0x602
#define TT_Err_Raster_Not_Initialized 0x603
#ifdef __cplusplus
}
#endif
#endif