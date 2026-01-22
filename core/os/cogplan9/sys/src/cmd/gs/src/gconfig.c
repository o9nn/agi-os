#include "memory_.h"
#include "gx.h"
#include "gscdefs.h"
#include "gconf.h"
#include "gxdevice.h"
#include "gxdhtres.h"
#include "gxiclass.h"
#include "gxiodev.h"
#include "gxiparam.h"
#include "gxcomp.h"
#define compositor_(comp_type) extern gs_composite_type_t comp_type;
#define device_(dev) extern gx_device dev;
#define device2_(dev) extern const gx_device dev;
#define halftone_(dht) extern DEVICE_HALFTONE_RESOURCE_PROC(dht);
#define image_class_(cls) extern iclass_proc(cls);
#define image_type_(i,type) extern const gx_image_type_t type;
#define init_(proc) extern init_proc(proc);
#define io_device_(iodev) extern const gx_io_device iodev;
#include "gconf.h"
#undef io_device_
#undef init_
#undef image_type_
#undef image_class_
#undef halftone_
#undef device2_
#undef device_
#undef compositor_
#define compositor_(comp_type) &comp_type,
private const gs_composite_type_t *const gx_compositor_list[] = {
#include "gconf.h"
0
};
#undef compositor_
#define device_(dev) (const gx_device *)&dev,
#define device2_(dev) &dev,
private const gx_device *const gx_device_list[] = {
#include "gconf.h"
0
};
#undef device2_
#undef device_
extern_gx_device_halftone_list();
#define halftone_(dht) dht,
const gx_dht_proc gx_device_halftone_list[] = {
#include "gconf.h"
0
};
#undef halftone_
extern_gx_image_class_table();
#define image_class_(cls) cls,
const gx_image_class_t gx_image_class_table[] = {
#include "gconf.h"
0
};
#undef image_class_
const unsigned gx_image_class_table_count = countof(gx_image_class_table) - 1;
extern_gx_image_type_table();
#define image_type_(i,type) &type,
const gx_image_type_t *const gx_image_type_table[] = {
#include "gconf.h"
0
};
#undef image_type_
const unsigned gx_image_type_table_count = countof(gx_image_type_table) - 1;
extern_gx_init_table();
#define init_(proc) proc,
const gx_init_proc gx_init_table[] = {
#include "gconf.h"
0
};
#undef init_
extern_gx_io_device_table();
extern gx_io_device gs_iodev_os;
#define io_device_(iodev) &iodev,
const gx_io_device *const gx_io_device_table[] = {
&gs_iodev_os,
#include "gconf.h"
0
};
#undef io_device_
const unsigned gx_io_device_table_count = countof(gx_io_device_table) - 1;
extern_gs_find_compositor();
const gs_composite_type_t *
gs_find_compositor(int comp_id)
{
const gs_composite_type_t *const * ppcomp = gx_compositor_list;
const gs_composite_type_t * pcomp;
while ((pcomp = *ppcomp++) != 0 && pcomp->comp_id != comp_id)
;
return pcomp;
}
extern_gs_lib_device_list();
int
gs_lib_device_list(const gx_device * const **plist,
gs_memory_struct_type_t ** pst)
{
if (plist != 0)
*plist = gx_device_list;
if (pst != 0)
*pst = NULL;
return countof(gx_device_list) - 1;
}