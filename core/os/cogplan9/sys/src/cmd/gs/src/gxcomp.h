#ifndef gxcomp_INCLUDED
#  define gxcomp_INCLUDED
#include "gscompt.h"
#include "gsrefct.h"
#include "gxbitfmt.h"
#define GX_COMPOSITOR_ALPHA        0x01
#define GX_COMPOSITOR_OVERPRINT    0x02
#define GX_COMPOSITOR_PDF14_TRANS  0x03
#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
#ifndef gx_device_DEFINED
#  define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
typedef struct gs_composite_type_procs_s {
#define composite_create_default_compositor_proc(proc)\
int proc(const gs_composite_t *pcte, gx_device **pcdev,\
gx_device *dev, gs_imager_state *pis, gs_memory_t *mem)
composite_create_default_compositor_proc((*create_default_compositor));
#define composite_equal_proc(proc)\
bool proc(const gs_composite_t *pcte, const gs_composite_t *pcte2)
composite_equal_proc((*equal));
#define composite_write_proc(proc)\
int proc(const gs_composite_t *pcte, byte *data, uint *psize)
composite_write_proc((*write));
#define composite_read_proc(proc)\
int proc(gs_composite_t **ppcte, const byte *data, uint size,\
gs_memory_t *mem)
composite_read_proc((*read));
#define composite_clist_write_update(proc)\
int proc(const gs_composite_t * pcte, gx_device * dev, gx_device ** pcdev,\
gs_imager_state * pis, gs_memory_t * mem)
composite_clist_write_update((*clist_compositor_write_update));
#define composite_clist_read_update(proc)\
int proc(gs_composite_t * pcte, gx_device * cdev, gx_device * tdev,\
gs_imager_state * pis, gs_memory_t * mem)
composite_clist_read_update((*clist_compositor_read_update));
} gs_composite_type_procs_t;
typedef struct gs_composite_type_s {
byte comp_id;
gs_composite_type_procs_t procs;
} gs_composite_type_t;
composite_clist_write_update(gx_default_composite_clist_write_update);
composite_clist_read_update(gx_default_composite_clist_read_update);
#define gs_composite_common\
const gs_composite_type_t *type;\
gs_id id;		\
rc_header rc
struct gs_composite_s {
gs_composite_common;
};
#define gs_composite_id(pcte) ((pcte)->id)
#endif