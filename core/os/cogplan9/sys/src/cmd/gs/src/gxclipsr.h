#ifndef gxclipsr_INCLUDED
#  define gxclipsr_INCLUDED
#include "gsrefct.h"
#ifndef gx_clip_path_DEFINED
#  define gx_clip_path_DEFINED
typedef struct gx_clip_path_s gx_clip_path;
#endif
#ifndef gx_clip_stack_DEFINED
#  define gx_clip_stack_DEFINED
typedef struct gx_clip_stack_s gx_clip_stack_t;
#endif
struct gx_clip_stack_s {
rc_header rc;
gx_clip_path *clip_path;
gx_clip_stack_t *next;
};
#define private_st_clip_stack()	\
gs_private_st_ptrs2(st_clip_stack, gx_clip_stack_t,\
"gx_clip_stack_t", clip_stack_enum_ptrs, clip_stack_reloc_ptrs,\
clip_path, next)
#endif