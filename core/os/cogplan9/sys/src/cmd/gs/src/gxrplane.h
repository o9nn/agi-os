#ifndef gxrplane_INCLUDED
# define gxrplane_INCLUDED
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
typedef struct gx_render_plane_s {
int depth;
int shift;
int index;
} gx_render_plane_t;
int gx_render_plane_init(gx_render_plane_t *render_plane,
const gx_device *dev, int index);
#endif