#ifndef gdevmrun_INCLUDED
#  define gdevmrun_INCLUDED
#include "gxdevmem.h"
typedef struct gx_device_run_s {
gx_device_memory md;
uint runs_per_line;
int umin, umax1;
int smin, smax1;
struct sp_ {
dev_proc_copy_mono((*copy_mono));
dev_proc_copy_color((*copy_color));
dev_proc_fill_rectangle((*fill_rectangle));
dev_proc_copy_alpha((*copy_alpha));
dev_proc_strip_tile_rectangle((*strip_tile_rectangle));
dev_proc_strip_copy_rop((*strip_copy_rop));
dev_proc_get_bits_rectangle((*get_bits_rectangle));
} save_procs;
} gx_device_run;
int gdev_run_from_mem(gx_device_run *rdev, gx_device_memory *mdev);
#endif