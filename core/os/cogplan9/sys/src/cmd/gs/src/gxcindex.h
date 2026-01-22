#ifndef gxcindex_INCLUDED
# define gxcindex_INCLUDED
#include "gsbitops.h"
#define GX_DEVICE_COLOR_MAX_COMPONENTS 16
#ifdef TEST_CINDEX_STRUCT
typedef struct { ulong value[2]; } gx_color_index_data;
#else
#ifdef GX_COLOR_INDEX_TYPE
typedef GX_COLOR_INDEX_TYPE gx_color_index_data;
#else
typedef ulong gx_color_index_data;
#endif
#endif
#ifdef TEST_CINDEX_POINTER
typedef gx_color_index_data * gx_color_index;
#define arch_sizeof_color_index arch_sizeof_ptr
extern const gx_color_index_data gx_no_color_index_data;
#define gx_no_color_index_values (&gx_no_color_index_data)
#define gx_no_color_index (&gx_no_color_index_data)
#else
#define arch_sizeof_color_index sizeof(gx_color_index_data)
typedef gx_color_index_data gx_color_index;
#define gx_no_color_index_value (~0)
#define gx_no_color_index ((gx_color_index)gx_no_color_index_value)
#endif
#define DECLARE_LINE_ACCUM(line, bpp, xo)\
sample_store_declare_setup(l_dptr, l_dbit, l_dbyte, line, 0, bpp)
#define LINE_ACCUM(color, bpp)\
sample_store_next_any(color, l_dptr, l_dbit, bpp, l_dbyte)
#define LINE_ACCUM_SKIP(bpp)\
sample_store_skip_next(l_dptr, l_dbit, bpp, l_dbyte)
#define LINE_ACCUM_STORE(bpp)\
sample_store_flush(l_dptr, l_dbit, bpp, l_dbyte)
#define DECLARE_LINE_ACCUM_COPY(line, bpp, xo)\
DECLARE_LINE_ACCUM(line, bpp, xo);\
int l_xprev = (xo)
#define LINE_ACCUM_COPY(dev, line, bpp, xo, xe, raster, y)\
if ( (xe) > l_xprev ) {\
int code;\
LINE_ACCUM_STORE(bpp);\
code = (*dev_proc(dev, copy_color))\
(dev, line, l_xprev - (xo), raster,\
gx_no_bitmap_id, l_xprev, y, (xe) - l_xprev, 1);\
if ( code < 0 )\
return code;\
}
#endif