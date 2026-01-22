#ifndef gstypes_INCLUDED
# define gstypes_INCLUDED
typedef ulong gs_id;
#define gs_no_id 0L
#define GS_STRING_COMMON\
byte *data;\
uint size
typedef struct gs_string_s {
GS_STRING_COMMON;
} gs_string;
#define GS_CONST_STRING_COMMON\
const byte *data;\
uint size
typedef struct gs_const_string_s {
GS_CONST_STRING_COMMON;
} gs_const_string;
typedef struct gs_param_string_s {
GS_CONST_STRING_COMMON;
bool persistent;
} gs_param_string;
typedef struct gs_bytestring_s {
GS_STRING_COMMON;
byte *bytes;
} gs_bytestring;
typedef struct gs_const_bytestring_s {
GS_CONST_STRING_COMMON;
const byte *bytes;
} gs_const_bytestring;
#define gs_bytestring_from_string(pbs, dat, siz)\
((pbs)->data = (dat), (pbs)->size = (siz), (pbs)->bytes = 0)
#define gs_bytestring_from_bytes(pbs, byts, offset, siz)\
((pbs)->data = ((pbs)->bytes = (byts)) + (offset), (pbs)->size = (siz))
typedef struct gs_point_s {
double x, y;
} gs_point;
typedef struct gs_int_point_s {
int x, y;
} gs_int_point;
typedef struct gs_log2_scale_point_s {
int x, y;
} gs_log2_scale_point;
typedef struct gs_rect_s {
gs_point p, q;
} gs_rect;
typedef struct gs_int_rect_s {
gs_int_point p, q;
} gs_int_rect;
typedef struct gs_range_s {
float rmin, rmax;
} gs_range_t;
#endif