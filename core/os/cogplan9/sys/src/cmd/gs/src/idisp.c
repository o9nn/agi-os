#include "stdio_.h"
#include "stdpre.h"
#include "iapi.h"
#include "ghost.h"
#include "gp.h"
#include "gscdefs.h"
#include "gsmemory.h"
#include "gstypes.h"
#include "gsdevice.h"
#include "iref.h"
#include "imain.h"
#include "iminst.h"
#include "oper.h"
#include "ostack.h"
#include "gx.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "idisp.h"
#include "gdevdevn.h"
#include "gsequivc.h"
#include "gdevdsp.h"
#include "gdevdsp2.h"
int
display_set_callback(gs_main_instance *minst, display_callback *callback)
{
i_ctx_t *i_ctx_p = minst->i_ctx_p;
bool was_open;
int code;
int exit_code = 0;
os_ptr op = osp;
gx_device *dev;
gx_device_display *ddev;
const char getdisplay[] =
"devicedict /display known dup { /display finddevice exch } if";
code = gs_main_run_string(minst, getdisplay, 0, &exit_code,
&minst->error_object);
if (code < 0)
return code;
op = osp;
check_type(*op, t_boolean);
if (op->value.boolval) {
check_read_type(op[-1], t_device);
dev = op[-1].value.pdevice;
was_open = dev->is_open;
if (was_open) {
code = gs_closedevice(dev);
if (code < 0)
return_error(code);
}
ddev = (gx_device_display *) dev;
ddev->callback = callback;
if (was_open) {
code = gs_opendevice(dev);
if (code < 0) {
dprintf("**** Unable to open the display device, quitting.\n");
return_error(code);
}
}
pop(1);
}
pop(1);
return 0;
}