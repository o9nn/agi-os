#ifndef gzacpath_INCLUDED
# define gzacpath_INCLUDED
typedef struct gx_device_cpath_accum_s {
gx_device_common;
gs_memory_t *list_memory;
gs_int_rect clip_box;
gs_int_rect bbox;
gx_clip_list list;
} gx_device_cpath_accum;
void gx_cpath_accum_begin(gx_device_cpath_accum * padev, gs_memory_t * mem);
void gx_cpath_accum_set_cbox(gx_device_cpath_accum * padev,
const gs_fixed_rect * pbox);
int gx_cpath_accum_end(const gx_device_cpath_accum * padev,
gx_clip_path * pcpath);
void gx_cpath_accum_discard(gx_device_cpath_accum * padev);
int gx_cpath_intersect_path_slow(gx_clip_path *, gx_path *, int,
gs_imager_state *);
#endif