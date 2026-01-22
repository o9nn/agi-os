#ifndef gxbitfmt_INCLUDED
# define gxbitfmt_INCLUDED
typedef ulong gx_bitmap_format_t;
#define GB_COLORS_NATIVE (1L<<0)
#define GB_COLORS_GRAY (1L<<1)
#define GB_COLORS_RGB (1L<<2)
#define GB_COLORS_CMYK (1L<<3)
#define GB_COLORS_STANDARD_ALL\
(GB_COLORS_GRAY | GB_COLORS_RGB | GB_COLORS_CMYK)
#define GB_COLORS_ALL\
(GB_COLORS_NATIVE | GB_COLORS_STANDARD_ALL)
#define GB_COLORS_NAMES\
"colors_native", "colors_Gray", "colors_RGB", "colors_CMYK"
#define GB_ALPHA_NONE (1L<<4)
#define GB_ALPHA_FIRST (1L<<5)
#define GB_ALPHA_LAST (1L<<6)
#define GB_ALPHA_ALL\
(GB_ALPHA_NONE | GB_ALPHA_FIRST | GB_ALPHA_LAST)
#define GB_ALPHA_NAMES\
"alpha_none", "alpha_first", "alpha_last", "?alpha_unused?"
#define GB_DEPTH_1 (1L<<8)
#define GB_DEPTH_2 (1L<<9)
#define GB_DEPTH_4 (1L<<10)
#define GB_DEPTH_8 (1L<<11)
#define GB_DEPTH_12 (1L<<12)
#define GB_DEPTH_16 (1L<<13)
#define GB_DEPTH_ALL\
(GB_DEPTH_1 | GB_DEPTH_2 | GB_DEPTH_4 | GB_DEPTH_8 |\
GB_DEPTH_12 | GB_DEPTH_16)
#define GB_DEPTH_NAMES\
"depth_1", "depth_2", "depth_4", "depth_8",\
"depth_12", "depth_16", "?depth_unused1?", "?depth_unused2?"
#define GB_OPTIONS_MAX_DEPTH(opt)\
"\
\000\001\002\002\004\004\004\004\010\010\010\010\010\010\010\010\
\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\
\020\020\020\020\020\020\020\020\020\020\020\020\020\020\020\020\
\020\020\020\020\020\020\020\020\020\020\020\020\020\020\020\020\
"[((opt) >> 8) & 0x3f]
#define GB_OPTIONS_DEPTH(opt)\
((((opt) >> 8) & 0xf) |\
"\000\000\014\020"[((opt) >> 12) & 3])
#define GB_PACKING_CHUNKY (1L<<16)
#define GB_PACKING_PLANAR (1L<<17)
#define GB_PACKING_BIT_PLANAR (1L<<18)
#define GB_PACKING_ALL\
(GB_PACKING_CHUNKY | GB_PACKING_PLANAR | GB_PACKING_BIT_PLANAR)
#define GB_PACKING_NAMES\
"packing_chunky", "packing_planar", "packing_bit_planar"
#define GB_SELECT_PLANES (1L<<19)
#define GB_SELECT_ALL\
(GB_SELECT_PLANES)
#define GB_SELECT_NAMES\
"select_planes"
#define GB_RETURN_COPY (1L<<20)
#define GB_RETURN_POINTER (1L<<21)
#define GB_RETURN_ALL\
(GB_RETURN_COPY | GB_RETURN_POINTER)
#define GB_RETURN_NAMES\
"return_copy", "return_pointer"
#define GB_ALIGN_STANDARD (1L<<22)
#define GB_ALIGN_ANY (1L<<23)
#define GB_ALIGN_ALL\
(GB_ALIGN_ANY | GB_ALIGN_STANDARD)
#define GB_ALIGN_NAMES\
"align_standard", "align_any"
#define GB_OFFSET_0 (1L<<24)
#define GB_OFFSET_SPECIFIED (1L<<25)
#define GB_OFFSET_ANY (1L<<26)
#define GB_OFFSET_ALL\
(GB_OFFSET_0 | GB_OFFSET_SPECIFIED | GB_OFFSET_ANY)
#define GB_OFFSET_NAMES\
"offset_0", "offset_specified", "offset_any", "?offset_unused?"
#define GB_RASTER_STANDARD (1L<<28)
#define GB_RASTER_SPECIFIED (1L<<29)
#define GB_RASTER_ANY (1L<<30)
#define GB_RASTER_ALL\
(GB_RASTER_STANDARD | GB_RASTER_SPECIFIED | GB_RASTER_ANY)
#define GB_RASTER_NAMES\
"raster_standard", "raster_specified", "raster_any"
#define GX_BITMAP_FORMAT_NAMES\
GB_COLORS_NAMES, GB_ALPHA_NAMES, GB_DEPTH_NAMES, GB_PACKING_NAMES,\
GB_SELECT_NAMES, GB_RETURN_NAMES, GB_ALIGN_NAMES, GB_OFFSET_NAMES,\
GB_RASTER_NAMES
#endif