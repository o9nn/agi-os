#ifndef gxdevbuf_INCLUDED
#  define gxdevbuf_INCLUDED
#include "gxrplane.h"
#ifndef gx_device_DEFINED
#  define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
typedef struct gx_device_buf_space_s {
ulong bits;
ulong line_ptrs;
uint raster;
} gx_device_buf_space_t;
typedef struct gx_device_buf_procs_s {
#define dev_proc_create_buf_device(proc)\
int proc(gx_device **pbdev, gx_device *target,\
const gx_render_plane_t *render_plane, gs_memory_t *mem,\
bool for_band)
dev_proc_create_buf_device((*create_buf_device));
#define dev_proc_size_buf_device(proc)\
int proc(gx_device_buf_space_t *space, gx_device *target,\
const gx_render_plane_t *render_plane,\
int height, bool for_band)
dev_proc_size_buf_device((*size_buf_device));
#define dev_proc_setup_buf_device(proc)\
int proc(gx_device *bdev, byte *buffer, int bytes_per_line,\
byte **line_ptrs , int y, int setup_height,\
int full_height)
dev_proc_setup_buf_device((*setup_buf_device));
#define dev_proc_destroy_buf_device(proc)\
void proc(gx_device *bdev)
dev_proc_destroy_buf_device((*destroy_buf_device));
} gx_device_buf_procs_t;
dev_proc_create_buf_device(gx_default_create_buf_device);
dev_proc_size_buf_device(gx_default_size_buf_device);
dev_proc_setup_buf_device(gx_default_setup_buf_device);
dev_proc_destroy_buf_device(gx_default_destroy_buf_device);
#endif