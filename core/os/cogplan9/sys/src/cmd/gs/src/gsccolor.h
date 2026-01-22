#ifndef gsccolor_INCLUDED
# define gsccolor_INCLUDED
#include "gsstype.h"
#ifndef gs_pattern_instance_DEFINED
# define gs_pattern_instance_DEFINED
typedef struct gs_pattern_instance_s gs_pattern_instance_t;
#endif
#define GS_CLIENT_COLOR_MAX_COMPONENTS 16
typedef struct gs_paint_color_s {
float values[GS_CLIENT_COLOR_MAX_COMPONENTS];
} gs_paint_color;
#ifndef gs_client_color_DEFINED
# define gs_client_color_DEFINED
typedef struct gs_client_color_s gs_client_color;
#endif
struct gs_client_color_s {
gs_paint_color paint;
gs_pattern_instance_t *pattern;
};
extern_st(st_client_color);
#define public_st_client_color() \
gs_public_st_ptrs1(st_client_color, gs_client_color, "gs_client_color",\
client_color_enum_ptrs, client_color_reloc_ptrs, pattern)
#define st_client_color_max_ptrs 1
#endif