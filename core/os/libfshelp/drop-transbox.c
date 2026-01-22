#include "fshelp.h"
void
fshelp_drop_transbox (struct transbox *box)
{
if (box->active != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), box->active);
}