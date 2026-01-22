#ifndef gxiclass_INCLUDED
# define gxiclass_INCLUDED
typedef struct gx_image_enum_s gx_image_enum;
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#define irender_proc(proc)\
int proc(gx_image_enum *penum, const byte *buffer, int data_x,\
uint w, int h, gx_device *dev)
typedef irender_proc((*irender_proc_t));
#define iclass_proc(proc)\
irender_proc_t proc(gx_image_enum *penum)
typedef iclass_proc((*gx_image_class_t));
#endif