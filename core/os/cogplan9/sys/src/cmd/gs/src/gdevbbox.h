#ifndef gdevbbox_INCLUDED
# define gdevbbox_INCLUDED
typedef struct gx_device_bbox_procs_s {
#define dev_bbox_proc_init_box(proc)\
bool proc(void *proc_data)
dev_bbox_proc_init_box((*init_box));
#define dev_bbox_proc_get_box(proc)\
void proc(const void *proc_data, gs_fixed_rect *pbox)
dev_bbox_proc_get_box((*get_box));
#define dev_bbox_proc_add_rect(proc)\
void proc(void *proc_data, fixed x0, fixed y0, fixed x1, fixed y1)
dev_bbox_proc_add_rect((*add_rect));
#define dev_bbox_proc_in_rect(proc)\
bool proc(const void *proc_data, const gs_fixed_rect *pbox)
dev_bbox_proc_in_rect((*in_rect));
} gx_device_bbox_procs_t;
dev_bbox_proc_init_box(bbox_default_init_box);
dev_bbox_proc_get_box(bbox_default_get_box);
dev_bbox_proc_add_rect(bbox_default_add_rect);
dev_bbox_proc_in_rect(bbox_default_in_rect);
#define gx_device_bbox_common\
gx_device_forward_common;\
bool free_standing;\
bool forward_open_close;\
gx_device_bbox_procs_t box_procs;\
void *box_proc_data;\
bool white_is_opaque;\
\
gs_fixed_rect bbox;\
gx_color_index black, white;\
gx_color_index transparent
typedef struct gx_device_bbox_s gx_device_bbox;
#define gx_device_bbox_common_initial(fs, foc, wio)\
0 ,\
fs, foc, {0}, 0, wio,\
{{0, 0}, {0, 0}}, gx_no_color_index, gx_no_color_index, gx_no_color_index
struct gx_device_bbox_s {
gx_device_bbox_common;
};
extern_st(st_device_bbox);
#define public_st_device_bbox() \
gs_public_st_suffix_add1_final(st_device_bbox, gx_device_bbox,\
"gx_device_bbox", device_bbox_enum_ptrs, device_bbox_reloc_ptrs,\
gx_device_finalize, st_device_forward, box_proc_data)
void gx_device_bbox_init(gx_device_bbox * dev, gx_device * target, gs_memory_t *mem);
void gx_device_bbox_fwd_open_close(gx_device_bbox * dev,
bool forward_open_close);
void gx_device_bbox_set_white_opaque(gx_device_bbox *dev,
bool white_is_opaque);
void gx_device_bbox_bbox(gx_device_bbox * dev, gs_rect * pbbox);
void gx_device_bbox_release(gx_device_bbox *dev);
#endif