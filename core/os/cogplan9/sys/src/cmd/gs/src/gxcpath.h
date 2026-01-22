#ifndef gxcpath_INCLUDED
#  define gxcpath_INCLUDED
typedef struct gx_clip_rect_s gx_clip_rect;
struct gx_clip_rect_s {
gx_clip_rect *next, *prev;
int ymin, ymax;
int xmin, xmax;
byte to_visit;
};
extern_st(st_clip_rect);
#define public_st_clip_rect()	\
gs_public_st_ptrs2(st_clip_rect, gx_clip_rect, "clip_rect",\
clip_rect_enum_ptrs, clip_rect_reloc_ptrs, next, prev)
#define st_clip_rect_max_ptrs 2
#ifndef gx_clip_list_DEFINED
#  define gx_clip_list_DEFINED
typedef struct gx_clip_list_s gx_clip_list;
#endif
struct gx_clip_list_s {
gx_clip_rect single;
gx_clip_rect *head;
gx_clip_rect *tail;
int xmin, xmax;
int count;
};
#define private_st_clip_list()	\
gs_private_st_ptrs2(st_clip_list, gx_clip_list, "clip_list",\
clip_list_enum_ptrs, clip_list_reloc_ptrs, head, tail)
#define st_clip_list_max_ptrs 2
#define clip_list_is_rectangle(clp) ((clp)->count <= 1)
#ifndef gx_device_clip_DEFINED
#  define gx_device_clip_DEFINED
typedef struct gx_device_clip_s gx_device_clip;
#endif
struct gx_device_clip_s {
gx_device_forward_common;
gx_clip_list list;
gx_clip_rect *current;
gs_int_point translation;
gs_fixed_rect clipping_box;
bool clipping_box_set;
};
extern_st(st_device_clip);
#define public_st_device_clip()	\
gs_public_st_composite_use_final(st_device_clip, gx_device_clip,\
"gx_device_clip", device_clip_enum_ptrs, device_clip_reloc_ptrs,\
gx_device_finalize)
void gx_make_clip_translate_device(gx_device_clip * dev,
const gx_clip_list * list,
int tx, int ty, gs_memory_t *mem);
#define gx_make_clip_device(dev, list)\
gx_make_clip_translate_device(dev, list, 0, 0, NULL)
void gx_make_clip_path_device(gx_device_clip *, const gx_clip_path *);
#define clip_rect_print(ch, str, ar)\
if_debug7(ch, "[%c]%s 0x%lx: (%d,%d),(%d,%d)\n", ch, str, (ulong)ar,\
(ar)->xmin, (ar)->ymin, (ar)->xmax, (ar)->ymax)
void gx_clip_list_init(gx_clip_list *);
void gx_clip_list_free(gx_clip_list *, gs_memory_t *);
void gx_cpath_set_outer_box(gx_clip_path *);
const gx_clip_list *gx_cpath_list(const gx_clip_path *pcpath);
#endif